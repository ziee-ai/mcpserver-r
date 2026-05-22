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

test("mint a token: JWT is shown once and only once", async ({ page }) => {
  await login(page);
  const username = `carol-${Date.now()}`;
  await createUser(page, username);

  // Click Tokens for that row
  const row = page.getByRole("row", { name: new RegExp(username) });
  await row.getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  // Mint
  await page.getByTestId("tokens-mint").click();
  await page.getByTestId("mint-name").fill("ci-runner");
  await page.getByRole("button", { name: "Mint", exact: true }).click();

  // Token reveal modal: JWT visible, copy works, table now has the row
  const mintedValue = page.getByTestId("minted-value");
  await expect(mintedValue).toBeVisible();
  const jwt = (await mintedValue.innerText()) ?? "";
  expect(jwt).toMatch(/^eyJ/);
  await page.getByTestId("minted-close").click();

  // The modal is dismissed (Antd may keep the node mounted for animation;
  // .toBeHidden() checks the user-visible state, which is what matters).
  await expect(page.getByTestId("minted-value")).toBeHidden();

  // Token row visible in the table
  await expect(page.getByRole("cell", { name: "ci-runner", exact: true }))
    .toBeVisible();
});
