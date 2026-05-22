import { test, expect, Page } from "@playwright/test";

async function login(page: Page) {
  await page.goto("/admin/ui/login");
  await page.getByTestId("login-token").fill(process.env.MCP_ADMIN_TOKEN!);
  await page.getByTestId("login-submit").click();
  await expect(page).toHaveURL(/\/admin\/ui\/users$/);
}

test("create user appears in the table", async ({ page }) => {
  await login(page);
  await page.getByTestId("users-new").click();
  const u = `alice-${Date.now()}`;
  await page.getByTestId("new-user-username").fill(u);
  await page.getByTestId("new-user-email").fill(`${u}@x.com`);
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByRole("cell", { name: u, exact: true }))
    .toBeVisible();
});

test("edit user persists across reload", async ({ page }) => {
  await login(page);
  await page.getByTestId("users-new").click();
  const u = `edit-${Date.now()}`;
  await page.getByTestId("new-user-username").fill(u);
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByRole("cell", { name: u, exact: true }))
    .toBeVisible();

  // Navigate into edit page
  await page.getByRole("row", { name: new RegExp(u) })
    .getByRole("button", { name: "Edit" }).click();
  await expect(page).toHaveURL(/\/admin\/ui\/users\/.+$/);

  // Fill email and save
  await page.getByTestId("edit-user-email").fill("edited@x.com");
  await page.getByTestId("edit-user-save").click();

  // Reload — server should now return the updated email
  await page.reload();
  await expect(page.getByTestId("edit-user-email")).toHaveValue(
    "edited@x.com",
  );
});

test("delete user removes the row", async ({ page }) => {
  await login(page);
  await page.getByTestId("users-new").click();
  const u = `bob-${Date.now()}`;
  await page.getByTestId("new-user-username").fill(u);
  await page.getByRole("button", { name: "Create" }).click();
  // Click Delete on the row we just created
  const row = page.getByRole("row", { name: new RegExp(u) });
  await row.getByRole("button", { name: "Delete" }).click();
  await page.getByRole("button", { name: "OK", exact: true }).click();
  await expect(row).toHaveCount(0);
});
