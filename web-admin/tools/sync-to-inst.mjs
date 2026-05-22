#!/usr/bin/env node
// Mirror the Vite `dist/` output into `inst/admin-ui/dist/`, which is
// served by the R process at /admin/ui/*. Run after `npm run build`.
import { rm, cp, stat } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const SRC = resolve(here, "..", "dist");
const DEST = resolve(here, "..", "..", "inst", "admin-ui", "dist");

try {
  await stat(SRC);
} catch {
  console.error(`No build output at ${SRC}. Run "npm run build" first.`);
  process.exit(1);
}

await rm(DEST, { recursive: true, force: true });
await cp(SRC, DEST, { recursive: true });
console.log(`Synced ${SRC} -> ${DEST}`);
