import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import compression from "vite-plugin-compression";

// Served from /admin/ui/ by the R process (R/admin-static.R). The base
// must match so hashed asset URLs (e.g. /admin/ui/assets/<hash>.js)
// resolve correctly.
export default defineConfig({
  base: "/admin/ui/",
  plugins: [
    react(),
    // Emit .gz siblings so the static handler can serve pre-compressed
    // assets without compressing per-request.
    compression({
      algorithm: "gzip",
      ext: ".gz",
      threshold: 512,
      deleteOriginFile: false,
    }),
  ],
  build: {
    target: "es2020",
    sourcemap: false,
    rollupOptions: {
      output: {
        manualChunks: {
          // Split antd into its own chunk so it caches separately
          // from the app code; helps both bundle hygiene and reload UX.
          antd: ["antd", "@ant-design/icons"],
          react: ["react", "react-dom", "react-router-dom"],
        },
      },
    },
  },
  server: {
    port: 5173,
    proxy: {
      // Local dev: forward API + MCP requests to the R server.
      "/admin/healthz": "http://127.0.0.1:3000",
      "/admin/users": "http://127.0.0.1:3000",
      "/admin/tokens": "http://127.0.0.1:3000",
      "/mcp": "http://127.0.0.1:3000",
    },
  },
});
