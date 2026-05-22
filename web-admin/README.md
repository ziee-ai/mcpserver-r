# mcpserver admin SPA

React + Vite + Ant Design + Tailwind. Built artifacts live in
`../inst/admin-ui/dist/`, where the R package's static handler serves
them at `/admin/ui/*` at runtime.

## Develop

```bash
npm ci

# Run the R server on :3000 in another terminal:
#   Rscript -e 'mcpserver::serve_http(...)'

npm run dev   # Vite dev server on :5173 with HMR; proxies /admin/* and /mcp to :3000
```

## Build + sync (committed bundle)

```bash
npm run build-and-sync
git status -- ../inst/admin-ui/dist
```

CI verifies that `inst/admin-ui/dist/` matches the source by re-running
the same command and `git diff --exit-code`.

## End-to-end (Playwright)

```bash
npm run e2e:setup   # one-time: install chromium
npm run e2e
```

The Playwright config starts an R process against a temp SQLite
database, runs the suite, and tears it down. See `e2e/playwright.config.ts`.

## Layout

- `src/api/client.ts` — axios + token storage in `sessionStorage`.
- `src/pages/Login.tsx` — bootstrap-token entry; verifies via `/admin/healthz`.
- `src/pages/UsersList.tsx` — Ant `Table` with create/edit/delete/tokens.
- `src/pages/UserEdit.tsx` — single-user form.
- `src/pages/UserTokens.tsx` — mint + revoke; minted JWT shown once in a modal.
- `tools/sync-to-inst.mjs` — copies `dist/` into `../inst/admin-ui/dist/`.
