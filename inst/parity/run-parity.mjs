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
import { z } from "zod";
import { randomUUID } from "node:crypto";
import { writeFileSync } from "node:fs";
import process from "node:process";

// SEP-1686 task-augmented request schemas. The SDK does not expose
// `tasks/get` / `tasks/result` schemas yet, so we declare minimal
// zod stubs here to register handlers via `setRequestHandler`.
const TasksGetRequestSchema = z.object({
  method: z.literal("tasks/get"),
  params: z.object({ taskId: z.string() }).passthrough(),
});
const TasksResultRequestSchema = z.object({
  method: z.literal("tasks/result"),
  params: z.object({ taskId: z.string() }).passthrough(),
});

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
        tasks: {
          requests: {
            sampling: { createMessage: {} },
            elicitation: { create: {} },
          },
        },
      },
    }
  );

  // SEP-1686 client-side task store. Each task is "working" for a
  // short delay, then transitions to "completed" and surfaces a
  // canned result that mirrors the synchronous path's output.
  const tasks = new Map();

  function startTask(parkedResult) {
    const taskId = randomUUID();
    const createdAt = new Date().toISOString();
    const task = {
      taskId,
      status: "working",
      createdAt,
      lastUpdatedAt: createdAt,
      ttl: 30000,
      pollInterval: 100,
    };
    tasks.set(taskId, { task, result: null });
    setTimeout(() => {
      const entry = tasks.get(taskId);
      if (!entry) return;
      entry.task.status = "completed";
      entry.task.lastUpdatedAt = new Date().toISOString();
      entry.result = parkedResult;
    }, 150);
    return task;
  }

  client.setRequestHandler(CreateMessageRequestSchema, async (req) => {
    const result = {
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
    // Task-augmented variant: client returns CreateTaskResult and
    // parks the real result for later retrieval via tasks/result.
    if (req.params?.task) {
      return { task: startTask(result) };
    }
    return result;
  });
  client.setRequestHandler(ElicitRequestSchema, async (req) => {
    // The everything-server's `trigger-elicitation-request` sends a
    // 13-field schema; the simpler/async variants send 2-field
    // schemas. Inspect `requestedSchema.properties` to decide what to
    // return. The R server applies schema-declared defaults so we
    // only need to supply `name` (the sole required field) for the
    // 13-field variant.
    const props = req.params?.requestedSchema?.properties ?? {};
    const content =
      "name" in props
        ? { name: "Mock User" }
        : "interpretation" in props
        ? { interpretation: "programming" }
        : { answer: "mock-answer", confidence: 0.9 };
    const result = { action: "accept", content };
    if (req.params?.task) {
      return { task: startTask(result) };
    }
    return result;
  });
  client.setRequestHandler(ListRootsRequestSchema, async () => {
    return {
      roots: [
        { uri: "file:///tmp/mock-root", name: "mock-root" },
        { uri: "file:///workspace", name: "workspace" },
      ],
    };
  });
  client.setRequestHandler(TasksGetRequestSchema, async (req) => {
    const entry = tasks.get(req.params.taskId);
    if (!entry) {
      throw new Error(`unknown taskId: ${req.params.taskId}`);
    }
    return { task: entry.task };
  });
  client.setRequestHandler(TasksResultRequestSchema, async (req) => {
    const entry = tasks.get(req.params.taskId);
    if (!entry) {
      throw new Error(`unknown taskId: ${req.params.taskId}`);
    }
    if (entry.task.status !== "completed") {
      throw new Error(
        `task ${req.params.taskId} not yet completed (status=${entry.task.status})`
      );
    }
    return entry.result;
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
      data: "data:text/plain;base64,aGVsbG8gd29ybGQ=",
      name: "hi.gz",
      outputType: "resource_link",
    },
    "toggle-simulated-logging": { enable: true },
    "toggle-subscriber-updates": { enable: true },
    "simulate-research-query": { topic: "python", steps: 2,
                                 ambiguous: true },
    "trigger-sampling-request": {
      prompt: "What is 2+2?",
      maxTokens: 10,
      temperature: 0.5,
    },
    "trigger-elicitation-request": { message: "Pick a colour" },
    "trigger-sampling-request-async": {
      prompt: "What is 5+5?",
      maxTokens: 10,
      ttl: 5,
      pollInterval: 0.1,
    },
    "trigger-elicitation-request-async": {
      message: "Pick a fruit",
      ttl: 5,
      pollInterval: 0.1,
    },
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
