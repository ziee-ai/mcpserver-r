#!/usr/bin/env node
// Spec-parity driver for the mcpserver everything-demo.
//
// Usage:
//   node run-parity.mjs --url http://127.0.0.1:3001/mcp [--report report.json]
//
// Connects via the official @modelcontextprotocol/sdk Streamable HTTP
// client, declares sampling / elicitation / roots client capabilities,
// installs handlers for the matching server-to-client requests, and walks
// every tool, resource, and prompt the server advertises using the
// TypeScript everything-server's exact names. A JSON report with
// per-check pass/fail entries is written to --report (or stdout when
// --report is omitted). The process exits 0 when every check passed,
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
    return {
      action: "accept",
      content: { answer: "mock-answer", confidence: 0.9 },
    };
  });
  client.setRequestHandler(ListRootsRequestSchema, async () => {
    return {
      roots: [
        { uri: "file:///tmp/mock-root", name: "mock-root" },
        { uri: "file:///workspace", name: "workspace" },
      ],
    };
  });

  const transport = new StreamableHTTPClientTransport(new URL(url), {
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

  // Match the TypeScript everything-server's tool argument shapes.
  const sampleArgs = {
    "echo": { message: "hello" },
    "get-sum": { a: 2, b: 40 },
    "get-structured-content": { location: "Boston" },
    "get-env": {},
    "trigger-long-running-operation": { duration: 0.5, steps: 3 },
    "get-tiny-image": {},
    "get-annotated-message": { messageType: "success", includeImage: false },
    "get-resource-reference": { resourceType: "Text", resourceId: "42" },
    "get-resource-links": { count: 3 },
    "gzip-file-as-resource": {
      content: "hello world",
      name: "hi.gz",
      outputType: "resource",
    },
    "toggle-simulated-logging": { enable: true },
    "toggle-subscriber-updates": { enable: true },
    "simulate-research-query": { topic: "vaccines", steps: 2 },
    "trigger-sampling-request": {
      prompt: "What is 2+2?",
      maxTokens: 10,
      temperature: 0.5,
    },
    "trigger-elicitation-request": { message: "Pick a colour" },
    "get-roots-list": {},
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
      return { contentTypes: (res.content ?? []).map((c) => c.type) };
    });
  }

  // Toggle off after the smoke pass so we don't leak intervals.
  if (toolNames.includes("toggle-simulated-logging")) {
    await safe("tools/call toggle-simulated-logging off", async () => {
      const res = await client.callTool({
        name: "toggle-simulated-logging",
        arguments: { enable: false },
      });
      return { ok: !res.isError };
    });
  }
  if (toolNames.includes("toggle-subscriber-updates")) {
    await safe("tools/call toggle-subscriber-updates off", async () => {
      const res = await client.callTool({
        name: "toggle-subscriber-updates",
        arguments: { enable: false },
      });
      return { ok: !res.isError };
    });
  }

  // Resources ----------------------------------------------------------
  const resources = await safe("resources/list", async () => {
    const r = await client.listResources();
    return { count: r.resources.length, uris: r.resources.map((x) => x.uri) };
  });
  for (const uri of resources?.uris ?? []) {
    await safe(`resources/read ${uri}`, async () => {
      const r = await client.readResource({ uri });
      return {
        mime: r.contents?.[0]?.mimeType,
        hasBody: Boolean(r.contents?.[0]?.text || r.contents?.[0]?.blob),
      };
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

  // Resource subscriptions ---------------------------------------------
  const subscriptionUri = "demo://resource/dynamic/text/1";
  await safe(`resources/subscribe ${subscriptionUri}`, async () => {
    await client.subscribeResource({ uri: subscriptionUri });
    return { ok: true };
  });
  await safe(`resources/unsubscribe ${subscriptionUri}`, async () => {
    await client.unsubscribeResource({ uri: subscriptionUri });
    return { ok: true };
  });

  // Prompts ------------------------------------------------------------
  const prompts = await safe("prompts/list", async () => {
    const r = await client.listPrompts();
    return { count: r.prompts.length, names: r.prompts.map((p) => p.name) };
  });

  const promptArgs = {
    "simple-prompt": {},
    "args-prompt": { city: "Boston", state: "MA" },
    "completable-prompt": { department: "Engineering", name: "Alice" },
    "resource-prompt": { resourceType: "Text", resourceId: "12" },
  };
  for (const name of prompts?.names ?? []) {
    await safe(`prompts/get ${name}`, async () => {
      const r = await client.getPrompt({
        name,
        arguments: promptArgs[name] ?? {},
      });
      return { messageCount: r.messages?.length ?? 0 };
    });
  }

  // Completions --------------------------------------------------------
  await safe("completion/complete department", async () => {
    const r = await client.complete({
      ref: { type: "ref/prompt", name: "completable-prompt" },
      argument: { name: "department", value: "Eng" },
    });
    return { values: r.completion?.values };
  });
  await safe("completion/complete name (cascading)", async () => {
    const r = await client.complete({
      ref: { type: "ref/prompt", name: "completable-prompt" },
      argument: { name: "name", value: "A" },
      context: { arguments: { department: "Engineering" } },
    });
    return { values: r.completion?.values };
  });

  // Logging level switch ----------------------------------------------
  await safe("logging/setLevel info", async () => {
    await client.setLoggingLevel("info");
    return { ok: true };
  });

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
