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

async function mintToken(page: Page, name: string) {
  await page.getByTestId("tokens-mint").click();
  await page.getByTestId("mint-name").fill(name);
  await page.getByRole("button", { name: "Mint", exact: true }).click();
  const jwt = (await page.getByTestId("minted-value").innerText()) ?? "";
  expect(jwt).toMatch(/^eyJ/);
  await page.getByTestId("minted-close").click();
  return jwt;
}

// The confirm dialog's OK button reads "Delete" too, so scope to the dialog
// to avoid matching the row's Delete button.
async function confirmDelete(page: Page) {
  await page.getByRole("dialog").getByRole("button", { name: "Delete" })
    .click();
}

test("delete an active token frees its name for re-minting", async ({
  page,
}) => {
  await login(page);
  const u = `hank-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  await mintToken(page, "key-a");
  const row = page.getByRole("row", { name: /key-a/ });
  await expect(row).toBeVisible();

  // Delete -> gone even with "show revoked" on (hard delete, not soft)
  await row.getByTestId("token-delete").click();
  await confirmDelete(page);
  await expect(row).toHaveCount(0);
  await page.getByTestId("tokens-include-revoked").click();
  await expect(page.getByRole("row", { name: /key-a/ })).toHaveCount(0);

  // The freed name mints cleanly again (this is the reported bug, fixed)
  await mintToken(page, "key-a");
  await expect(page.getByRole("row", { name: /key-a/ })).toBeVisible();
});

test("delete a revoked token removes it for good", async ({ page }) => {
  await login(page);
  const u = `ivy-${Date.now()}`;
  await createUser(page, u);
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Tokens" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+\/tokens$/);

  await mintToken(page, "doomed");

  // Revoke, reveal it, then delete it
  let row = page.getByRole("row", { name: /doomed/ });
  await row.getByRole("button", { name: "Revoke" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await page.getByTestId("tokens-include-revoked").click();

  row = page.getByRole("row", { name: /doomed/ });
  await expect(row).toContainText("revoked");
  await row.getByTestId("token-delete").click();
  await confirmDelete(page);

  // Gone from the include-revoked view entirely
  await expect(page.getByRole("row", { name: /doomed/ })).toHaveCount(0);
});
