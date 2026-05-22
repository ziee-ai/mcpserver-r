import { rmSync } from "node:fs";

export default async function globalTeardown() {
  const pid = (globalThis as any).__mcpserver_pid__ as number | undefined;
  if (pid) {
    try {
      process.kill(pid, "SIGTERM");
    } catch {}
  }
  const dir = (globalThis as any).__mcpserver_tmp__ as string | undefined;
  if (dir) {
    try {
      rmSync(dir, { recursive: true, force: true });
    } catch {}
  }
}
