#!/usr/bin/env node
// Spec-parity driver for the mcpserver everything-demo.
//
// Usage:
//   node run-parity.mjs --url http://127.0.0.1:3001/mcp [--report report.json]
//
// Connects via the official @modelcontextprotocol/sdk Streamable HTTP
// client, declares sampling / elicitation / roots client capabilities,
// installs handlers for the matching server-to-client requests, and walks
// every tool, resource, and prompt the server advertises. A JSON report
// with per-check pass/fail entries is written to --report (or stdout
// when --report is omitted). The process exits 0 when every check passed,
// non-zero otherwise.

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
import {
  CreateMessageRequestSchema,
  ElicitRequestSchema,
  ListRootsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { writeFileSync } from "node:fs";
import process from "node:process";

function parseArgs(argv) {
  const out = { url: "http://127.0.0.1:3001/mcp", report: null };
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--url" && i + 1 < argv.length) {
      out.url = argv[++i];
    } else if (argv[i] === "--report" && i + 1 < argv.length) {
      out.report = argv[++i];
    }
  }
  return out;
}

const { url, report } = parseArgs(process.argv.slice(2));

const results = [];
function check(name, ok, detail) {
  results.push({ name, ok, detail: detail ?? null });
  if (!ok) {
    process.stderr.write(`FAIL  ${name}: ${detail ?? ""}\n`);
  } else {
    process.stderr.write(`pass  ${name}\n`);
  }
}

async function safe(name, fn) {
  try {
    const v = await fn();
    check(name, true, v);
    return v;
  } catch (err) {
    check(name, false, String(err?.message ?? err));
    return null;
  }
}

async function main() {
  const client = new Client(
    { name: "mcpserver-parity", version: "0.1.0" },
    {
      capabilities: {
        sampling: {},
        elicitation: {},
        roots: { listChanged: true },
      },
    }
  );

  // Server-to-client request handlers. We respond with deterministic
  // mock data so the parity test is reproducible.
  client.setRequestHandler(CreateMessageRequestSchema, async (req) => {
    return {
      role: "assistant",
      content: {
        type: "text",
        text: `mock-sample: ${
          req.params?.messages?.[0]?.content?.text ?? ""
        }`,
      },
      model: "mock-model",
      stopReason: "endTurn",
    };
  });
  client.setRequestHandler(ElicitRequestSchema, async (req) => {
    return { action: "accept", content: { answer: "mock-answer" } };
  });
  client.setRequestHandler(ListRootsRequestSchema, async () => {
    return { roots: [{ uri: "file:///tmp/mock-root", name: "mock-root" }] };
  });

  const transport = new StreamableHTTPClientTransport(new URL(url), {
    // Our server defaults to validating the Origin header to defend
    // against DNS rebinding; supply a loopback Origin so the request
    // passes that check.
    requestInit: {
      headers: { Origin: "http://127.0.0.1" },
    },
  });

  await safe("connect", async () => {
    await client.connect(transport);
    return { server: client.getServerVersion()?.name };
  });

  const tools = await safe("tools/list", async () => {
    const r = await client.listTools();
    return { count: r.tools.length, names: r.tools.map((t) => t.name) };
  });
  const toolNames = tools?.names ?? [];

  // Helper to build sane arguments per tool from the server's schema.
  const sampleArgs = {
    echo: { message: "hello" },
    add: { a: 2, b: 40 },
    printEnv: {},
    longRunningOperation: { duration: 2 },
    getTinyImage: {},
    annotatedMessage: { includeImage: false },
    getResourceReference: { resourceId: "42" },
    getResourceLinks: { count: 2 },
    setLogLevel: { level: "info" },
    toggleSimulatedLogging: { bursts: 1 },
    toggleSubscriberUpdates: { count: 2 },
    gzipFileAsResource: { content: "hello world", name: "hi.gz" },
    sampleLLM: { prompt: "What is 2+2?" },
    startElicitation: { message: "Pick a colour" },
    listRoots: {},
  };

  for (const name of toolNames) {
    const args = sampleArgs[name] ?? {};
    await safe(`tools/call ${name}`, async () => {
      const res = await client.callTool({ name, arguments: args });
      if (res.isError) {
        throw new Error(
          `tool returned isError=true: ${
            res.content?.[0]?.text ?? "(no text)"
          }`
        );
      }
      return {
        contentTypes: (res.content ?? []).map((c) => c.type),
      };
    });
  }

  // Resources -------------------------------------------------------------
  const resources = await safe("resources/list", async () => {
    const r = await client.listResources();
    return { count: r.resources.length, uris: r.resources.map((x) => x.uri) };
  });
  for (const uri of resources?.uris ?? []) {
    await safe(`resources/read ${uri}`, async () => {
      const r = await client.readResource({ uri });
      return { mime: r.contents?.[0]?.mimeType, hasBody:
        Boolean(r.contents?.[0]?.text || r.contents?.[0]?.blob) };
    });
  }

  const templates = await safe("resources/templates/list", async () => {
    const r = await client.listResourceTemplates();
    return {
      count: r.resourceTemplates.length,
      templates: r.resourceTemplates.map((t) => t.uriTemplate),
    };
  });
  for (const tmpl of templates?.templates ?? []) {
    const uri = tmpl.replace(/\{[^}]+\}/g, "7");
    await safe(`resources/read template ${uri}`, async () => {
      const r = await client.readResource({ uri });
      return { mime: r.contents?.[0]?.mimeType };
    });
  }

  // Prompts ---------------------------------------------------------------
  const prompts = await safe("prompts/list", async () => {
    const r = await client.listPrompts();
    return { count: r.prompts.length, names: r.prompts.map((p) => p.name) };
  });

  const promptArgs = {
    simple: {},
    "args-prompt": { city: "Boston", style: "casual" },
    "completions-prompt": { department: "Engineering", name: "Alice" },
    "embedded-resource-prompt": { id: "12" },
  };
  for (const name of prompts?.names ?? []) {
    await safe(`prompts/get ${name}`, async () => {
      const r = await client.getPrompt({ name, arguments: promptArgs[name] ?? {} });
      return { messageCount: r.messages?.length ?? 0 };
    });
  }

  // Completions -----------------------------------------------------------
  await safe("completion/complete department", async () => {
    const r = await client.complete({
      ref: { type: "ref/prompt", name: "completions-prompt" },
      argument: { name: "department", value: "Eng" },
    });
    return { values: r.completion?.values };
  });
  await safe("completion/complete name (cascading)", async () => {
    const r = await client.complete({
      ref: { type: "ref/prompt", name: "completions-prompt" },
      argument: { name: "name", value: "A" },
      context: { arguments: { department: "Engineering" } },
    });
    return { values: r.completion?.values };
  });

  // Logging level switch --------------------------------------------------
  await safe("logging/setLevel info", async () => {
    await client.setLoggingLevel("info");
    return { ok: true };
  });

  // Done — close.
  await safe("close", async () => {
    await client.close();
    return true;
  });

  const passed = results.filter((r) => r.ok).length;
  const failed = results.length - passed;
  const summary = { passed, failed, total: results.length, results };

  const json = JSON.stringify(summary, null, 2);
  if (report) writeFileSync(report, json);
  process.stdout.write(json + "\n");
  process.stderr.write(`\n=== parity: ${passed}/${results.length} passed ===\n`);
  process.exit(failed === 0 ? 0 : 1);
}

main().catch((err) => {
  process.stderr.write(`fatal: ${err?.stack ?? err}\n`);
  process.exit(2);
});
