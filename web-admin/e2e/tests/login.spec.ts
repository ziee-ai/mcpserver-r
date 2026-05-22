import { test, expect } from "@playwright/test";

const TOKEN = () => process.env.MCP_ADMIN_TOKEN!;

test("a good token logs in and lands on /users", async ({ page }) => {
  await page.goto("/admin/ui/login");
  await page.getByTestId("login-token").fill(TOKEN());
  await page.getByTestId("login-submit").click();
  await expect(page).toHaveURL(/\/admin\/ui\/users$/);
  await expect(page.getByTestId("users-table")).toBeVisible();
});

test("a bad token is rejected and stays on /login", async ({ page }) => {
  await page.goto("/admin/ui/login");
  await page.getByTestId("login-token").fill("definitely-not-the-token");
  await page.getByTestId("login-submit").click();
  await expect(page).toHaveURL(/\/admin\/ui\/login$/);
});
