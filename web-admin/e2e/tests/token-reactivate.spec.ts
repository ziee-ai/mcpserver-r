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

test("reactivate restores an active row and re-enables the JWT", async ({
  page,
}) => {
  await login(page);
  const u = `gina-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  const jwt = await mintToken(page, "revive-me");

  const ctx = await request.newContext({
    baseURL: process.env.MCP_BASE_URL,
    extraHTTPHeaders: {
      Origin: "http://127.0.0.1",
      Accept: "application/json, text/event-stream",
      "Content-Type": "application/json",
      Authorization: `Bearer ${jwt}`,
    },
  });
  const probe = () =>
    ctx.post("/mcp", {
      data: {
        jsonrpc: "2.0",
        id: 1,
        method: "initialize",
        params: { protocolVersion: "2025-06-18", capabilities: {} },
      },
    });

  // Pre-revoke: JWT works
  expect((await probe()).status()).toBe(200);

  // Revoke from the UI -> JWT 401, row gone from default list
  let row = page.getByRole("row", { name: /revive-me/ });
  await row.getByRole("button", { name: "Revoke" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await expect(row).toHaveCount(0);
  expect((await probe()).status()).toBe(401);

  // Show revoked, then Reactivate that row
  await page.getByTestId("tokens-include-revoked").click();
  row = page.getByRole("row", { name: /revive-me/ });
  await expect(row).toContainText("revoked");
  await row.getByTestId("token-reactivate").click();
  await page.getByRole("button", { name: "OK", exact: true }).click();

  // Row is active again and the original JWT is accepted again
  await expect(row).toContainText("active");
  await expect.poll(async () => (await probe()).status()).toBe(200);

  // And it shows up in the default (active-only) list
  await page.getByTestId("tokens-include-revoked").click();
  await expect(page.getByRole("row", { name: /revive-me/ })).toBeVisible();
});
