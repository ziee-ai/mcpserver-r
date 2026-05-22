import { test, expect, Page, request } from "@playwright/test";

async function login(page: Page) {
  await page.goto("/admin/ui/login");
  await page.getByTestId("login-token").fill(process.env.MCP_ADMIN_TOKEN!);
  await page.getByTestId("login-submit").click();
  await expect(page).toHaveURL(/\/admin\/ui\/users$/);
}

async function createUser(page: Page, username: string) {
  await page.getByTestId("users-new").click();
  await page.getByTestId("new-user-username").fill(username);
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByRole("cell", { name: username, exact: true }))
    .toBeVisible();
}

async function mintToken(page: Page, name: string) {
  await page.getByTestId("tokens-mint").click();
  await page.getByTestId("mint-name").fill(name);
  await page.getByRole("button", { name: "Mint", exact: true }).click();
  const jwt = (await page.getByTestId("minted-value").innerText()) ?? "";
  expect(jwt).toMatch(/^eyJ/);
  await page.getByTestId("minted-close").click();
  return jwt;
}

test("revoke flips the row status and disappears from the default list", async ({
  page,
}) => {
  await login(page);
  const u = `erin-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  await mintToken(page, "to-revoke");

  // Active row is visible
  const row = page.getByRole("row", { name: /to-revoke/ });
  await expect(row).toBeVisible();

  // Revoke (confirm dialog) -> row gone from default list
  await row.getByRole("button", { name: "Revoke" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await expect(row).toHaveCount(0);

  // Toggle "show revoked" -> the row reappears with status=revoked
  await page.getByTestId("tokens-include-revoked").click();
  const revoked = page.getByRole("row", { name: /to-revoke/ });
  await expect(revoked).toBeVisible();
  await expect(revoked).toContainText("revoked");
});

test("revoked JWT is rejected by /mcp with 401 invalid_token", async ({
  page,
}) => {
  await login(page);
  const u = `frank-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  const jwt = await mintToken(page, "live-then-dead");

  // Pre-revoke: the JWT works against /mcp
  const ctx = await request.newContext({
    baseURL: process.env.MCP_BASE_URL,
    extraHTTPHeaders: {
      Origin: "http://127.0.0.1",
      Accept: "application/json, text/event-stream",
      "Content-Type": "application/json",
      Authorization: `Bearer ${jwt}`,
    },
  });
  const ok = await ctx.post("/mcp", {
    data: {
      jsonrpc: "2.0",
      id: 1,
      method: "initialize",
      params: { protocolVersion: "2025-06-18", capabilities: {} },
    },
  });
  expect(ok.status()).toBe(200);

  // Revoke from the UI
  const row = page.getByRole("row", { name: /live-then-dead/ });
  await row.getByRole("button", { name: "Revoke" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await expect(row).toHaveCount(0);

  // Same JWT now 401s + carries WWW-Authenticate
  const dead = await ctx.post("/mcp", {
    data: { jsonrpc: "2.0", id: 2, method: "tools/list", params: {} },
  });
  expect(dead.status()).toBe(401);
  expect(dead.headers()["www-authenticate"] ?? "").toMatch(/Bearer/);
});
