import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: 1,
  reporter: process.env.CI ? "github" : "list",
  timeout: 30_000,
  expect: { timeout: 5_000 },
  use: {
    baseURL: process.env.MCP_BASE_URL ?? "http://127.0.0.1:47301",
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
    permissions: ["clipboard-read", "clipboard-write"],
    extraHTTPHeaders: { Origin: "http://127.0.0.1" },
  },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
  ],
  globalSetup: "./global-setup.ts",
  globalTeardown: "./global-teardown.ts",
});
