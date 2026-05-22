import { spawn } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { randomBytes } from "node:crypto";

const PORT = parseInt(process.env.MCP_PORT ?? "47301", 10);
const BASE = `http://127.0.0.1:${PORT}`;

async function waitFor(url: string, token: string, ms = 30_000) {
  const deadline = Date.now() + ms;
  while (Date.now() < deadline) {
    try {
      const r = await fetch(url, {
        headers: { Authorization: `Bearer ${token}`, Origin: "http://127.0.0.1" },
      });
      if (r.status === 200) return;
    } catch {
      // ignore
    }
    await new Promise((res) => setTimeout(res, 250));
  }
  throw new Error(`server at ${url} did not become ready`);
}

export default async function globalSetup() {
  const dir = mkdtempSync(join(tmpdir(), "mcpserver-e2e-"));
  const dbPath = join(dir, "state.db");
  const token = randomBytes(16).toString("hex");

  const runner = join(dir, "runner.R");
  writeFileSync(
    runner,
    [
      "suppressPackageStartupMessages(library(mcpserver))",
      `store <- new_mcp_store('sqlite', path = '${dbPath}')`,
      `oauth_as <- oauth_server_config(issuer = 'http://127.0.0.1:${PORT}', audience = 'mcp', store = store)`,
      "mcp <- new_server('e2e', version = '0.1.0')",
      "add_capability(mcp, new_tool('whoami', 'whoami', schema(list()),",
      "  handler = function(args, ctx) response_text(",
      "    sprintf('user_id=%s name=%s admin=%s', ctx$user_id %||% 'none', ctx$user_name %||% 'none', ctx$is_admin))))",
      `Sys.setenv(MCPSERVER_ADMIN_TOKEN='${token}')`,
      `serve_http(mcp, port = ${PORT}L, oauth_as = oauth_as,`,
      "             admin = list(bootstrap_token = Sys.getenv('MCPSERVER_ADMIN_TOKEN'),",
      "                          ui = TRUE),",
      "             allowed_origins = c('http://127.0.0.1'))",
    ].join("\n"),
  );

  // Resolve Rscript via $MCP_RSCRIPT (e.g. conda env binary) or fall
  // back to the one on PATH.
  const rscript = process.env.MCP_RSCRIPT?.trim() || "Rscript";
  const child = spawn(rscript, [runner], {
    stdio: ["ignore", "pipe", "pipe"],
    env: { ...process.env },
  });
  let serverLog = "";
  child.stdout.on("data", (d) => (serverLog += d.toString()));
  child.stderr.on("data", (d) => (serverLog += d.toString()));

  (globalThis as any).__mcpserver_pid__ = child.pid;
  (globalThis as any).__mcpserver_tmp__ = dir;

  process.env.MCP_BASE_URL = BASE;
  process.env.MCP_ADMIN_TOKEN = token;

  try {
    await waitFor(`${BASE}/admin/healthz`, token);
  } catch (e) {
    console.error("server log:\n" + serverLog);
    throw e;
  }
}
