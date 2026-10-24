import type {
  Bookmark,
  BookmarkInput,
  IntranetGroup,
  Meta,
  SessionInfo,
} from "./types";

// 生产环境页面挂在 /homepage/ 下，接口就是 /homepage/api/*；开发环境是 /api/*。
// Vite 的 BASE_URL 在 build 时由 vite.config.ts 的 base 决定。
const apiBase = import.meta.env.BASE_URL.replace(/\/+$/, "");

export class ApiError extends Error {
  status: number;

  constructor(message: string, status: number) {
    super(message);
    this.name = "ApiError";
    this.status = status;
  }
}

async function request<T>(path: string, init: RequestInit = {}): Promise<T> {
  const headers = new Headers(init.headers);
  if (init.body !== undefined) {
    headers.set("Content-Type", "application/json");
  }
  const response = await fetch(path, {
    credentials: "same-origin",
    ...init,
    headers,
  });
  if (!response.ok) {
    let message = "请求失败 (" + response.status + ")";
    try {
      const data = (await response.json()) as { error?: string };
      if (data.error) {
        message = data.error;
      }
    } catch {
      // 非 JSON 错误响应，保留默认提示
    }
    throw new ApiError(message, response.status);
  }
  if (response.status === 204) {
    return undefined as T;
  }
  return (await response.json()) as T;
}

export const api = {
  meta: () => request<Meta>(apiBase + "/api/meta"),
  session: () => request<SessionInfo>(apiBase + "/api/session"),
  login: (username: string, password: string) =>
    request<{ username: string }>(apiBase + "/api/auth/login", {
      method: "POST",
      body: JSON.stringify({ username, password }),
    }),
  logout: () => request<void>(apiBase + "/api/auth/logout", { method: "POST" }),
  bookmarks: () => request<Bookmark[]>(apiBase + "/api/bookmarks"),
  intranet: () => request<IntranetGroup[]>(apiBase + "/api/intranet"),
  createBookmark: (input: BookmarkInput) =>
    request<Bookmark>(apiBase + "/api/bookmarks", {
      method: "POST",
      body: JSON.stringify(input),
    }),
  updateBookmark: (id: number, input: BookmarkInput) =>
    request<Bookmark>(apiBase + "/api/bookmarks/" + id, {
      method: "PUT",
      body: JSON.stringify(input),
    }),
  deleteBookmark: (id: number) =>
    request<void>(apiBase + "/api/bookmarks/" + id, { method: "DELETE" }),
};
