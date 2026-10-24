import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";

export default defineConfig(({ command }) => ({
  // 生产构建挂在 https://<host>/homepage/ 下（Caddy strip_prefix 后交给后端），
  // 本地 dev server 仍从站点根开始，方便直接打开 http://localhost:5173/。
  base: command === "build" ? "/homepage/" : "/",
  plugins: [vue()],
  server: {
    // 本地开发：cargo run 起的后端默认监听 18081，前端只代理 /api。
    proxy: {
      "/api": "http://127.0.0.1:18081",
    },
  },
  build: {
    outDir: "dist",
    emptyOutDir: true,
  },
}));
