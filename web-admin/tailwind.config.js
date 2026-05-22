/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./src/**/*.{ts,tsx}"],
  // Disable Tailwind's preflight reset so it doesn't clobber Ant Design's
  // baseline component styles.
  corePlugins: { preflight: false },
  theme: { extend: {} },
  plugins: [],
};
