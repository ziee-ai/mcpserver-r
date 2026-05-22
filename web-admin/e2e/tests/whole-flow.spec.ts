import { test, expect, Page, request } from "@playwright/test";

async function login(page: Page) {
  await page.goto("/admin/ui/login");
  await page.getByTestId("login-token").fill(process.env.MCP_ADMIN_TOKEN!);
  await page.getByTestId("login-submit").click();
  await expect(page).toHaveURL(/\/admin\/ui\/users$/);
}

test("end-to-end: create user -> mint -> use via fetch -> revoke -> 401", async ({
  page,
}) => {
  await login(page);

  // 1. Create user
  const username = `dave-${Date.now()}`;
  await page.getByTestId("users-new").click();
  await page.getByTestId("new-user-username").fill(username);
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByRole("cell", { name: username, exact: true }))
    .toBeVisible();

  // 2. Open Tokens page
  const row = page.getByRole("row", { name: new RegExp(username) });
  await row.getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  // 3. Mint a token; grab the JWT
  await page.getByTestId("tokens-mint").click();
  await page.getByTestId("mint-name").fill("smoke");
  await page.getByRole("button", { name: "Mint", exact: true }).click();
  const jwt = (await page.getByTestId("minted-value").textContent()) ?? "";
  expect(jwt).toMatch(/^eyJ/);
  await page.getByTestId("minted-close").click();

  // 4. Use the JWT against /mcp tools/list — should succeed
  const ctx = await request.newContext({
    baseURL: process.env.MCP_BASE_URL,
    extraHTTPHeaders: {
      Origin: "http://127.0.0.1",
      Accept: "application/json, text/event-stream",
      "Content-Type": "application/json",
      Authorization: `Bearer ${jwt}`,
    },
  });
  const r1 = await ctx.post("/mcp", {
    data: {
      jsonrpc: "2.0",
      id: 1,
      method: "initialize",
      params: { protocolVersion: "2025-06-18", capabilities: {} },
    },
  });
  expect(r1.status()).toBe(200);

  // 5. Revoke the token from the UI
  await page.reload();
  const tokenRow = page.getByRole("row", { name: /smoke/ });
  await tokenRow.getByRole("button", { name: "Revoke" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await expect(tokenRow).toHaveCount(0); // hidden by default

  // 6. Re-use the same JWT -> 401
  const r2 = await ctx.post("/mcp", {
    data: {
      jsonrpc: "2.0",
      id: 2,
      method: "tools/list",
      params: {},
    },
  });
  expect(r2.status()).toBe(401);
});
