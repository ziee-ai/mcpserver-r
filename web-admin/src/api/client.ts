import axios, { AxiosInstance } from "axios";

const STORAGE_KEY = "mcpserver.admin.token";

export function getToken(): string | null {
  return sessionStorage.getItem(STORAGE_KEY);
}

export function setToken(token: string) {
  sessionStorage.setItem(STORAGE_KEY, token);
}

export function clearToken() {
  sessionStorage.removeItem(STORAGE_KEY);
}

const api: AxiosInstance = axios.create({
  baseURL: "/",
  timeout: 10_000,
});

api.interceptors.request.use((cfg) => {
  const t = getToken();
  if (t) cfg.headers.set("Authorization", `Bearer ${t}`);
  return cfg;
});

api.interceptors.response.use(
  (r) => r,
  (err) => {
    // Surface 401s as a thrown error the caller can catch + display.
    return Promise.reject(err);
  },
);

export type User = {
  id: string;
  username: string;
  email?: string | null;
  is_admin: boolean;
  groups?: string[];
  metadata?: Record<string, unknown>;
  created_at: string;
  updated_at: string;
};

export type Token = {
  jti: string;
  user_id: string;
  name: string;
  scopes: string[];
  created_at: string;
  expires_at: string;
  last_used_at?: string | null;
  revoked: boolean;
};

export const adminApi = {
  healthz: () => api.get("/admin/healthz"),
  listUsers: () =>
    api.get<{ users: User[] }>("/admin/users").then((r) => r.data.users),
  getUser: (id: string) => api.get<User>(`/admin/users/${id}`).then((r) => r.data),
  createUser: (body: Partial<User>) =>
    api.post<User>("/admin/users", body).then((r) => r.data),
  updateUser: (id: string, body: Partial<User>) =>
    api.patch<User>(`/admin/users/${id}`, body).then((r) => r.data),
  deleteUser: (id: string) => api.delete(`/admin/users/${id}`),
  listTokens: (userId: string, includeRevoked = false) =>
    api
      .get<{ tokens: Token[] }>(
        `/admin/users/${userId}/tokens${includeRevoked ? "?include_revoked=true" : ""}`,
      )
      .then((r) => r.data.tokens),
  mintToken: (body: {
    user_id: string;
    name: string;
    scopes: string[];
    ttl: number;
  }) =>
    api
      .post<{ jti: string; token: string; expires_at: string }>(
        "/admin/tokens/mint",
        body,
      )
      .then((r) => r.data),
  revokeToken: (jti: string) => api.post(`/admin/tokens/${jti}/revoke`),
  reactivateToken: (jti: string) =>
    api.post(`/admin/tokens/${jti}/reactivate`),
  deleteToken: (jti: string) => api.delete(`/admin/tokens/${jti}`),
};

export default api;
