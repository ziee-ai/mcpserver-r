import { test, expect, Page } from "@playwright/test";

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

async function mintToken(page: Page, name: string, ttl: number) {
  await page.getByTestId("tokens-mint").click();
  await page.getByTestId("mint-name").fill(name);
  // antd InputNumber forwards data-testid onto the inner <input>, so fill it directly.
  await page.getByTestId("mint-ttl").fill(String(ttl));
  await page.getByRole("button", { name: "Mint", exact: true }).click();
  await expect(page.getByTestId("minted-value")).toBeVisible();
  await page.getByTestId("minted-close").click();
  await expect(page.getByTestId("minted-value")).toBeHidden();
}

test("a past-expiry token shows 'expired'; a live one stays 'active'", async ({
  page,
}) => {
  await login(page);
  const u = `jane-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  await mintToken(page, "long-lived", 3600);
  await mintToken(page, "short-lived", 1); // 1s TTL -> expires almost immediately

  // Wait past the short TTL, then reload so the table re-renders and the
  // client recomputes status from expires_at vs now.
  await page.waitForTimeout(1500);
  await page.reload();

  // short-lived -> orange "expired" tag (not "active")
  const shortRow = page.getByRole("row", { name: /short-lived/ });
  await expect(shortRow.getByTestId("status-expired")).toBeVisible();
  await expect(shortRow).toContainText("expired");

  // long-lived -> still "active", no expired tag
  const longRow = page.getByRole("row", { name: /long-lived/ });
  await expect(longRow).toContainText("active");
  await expect(longRow.getByTestId("status-expired")).toHaveCount(0);
});
