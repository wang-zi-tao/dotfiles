<script setup lang="ts">
import { onMounted, ref } from "vue";
import LoginView from "./components/LoginView.vue";
import HomeView from "./components/HomeView.vue";
import { api } from "./api";
import type { Meta, SessionInfo } from "./types";

const meta = ref<Meta | null>(null);
const session = ref<SessionInfo>({ authenticated: false });
const loading = ref(true);
const error = ref("");

// SSO 回跳：Caddy forward_auth 把未登录请求重定向到
// /homepage/?next=<原始地址>，登录成功后回到原来要访问的服务。
const nextUrl = safeNext();

function safeNext(): string | null {
  const raw = new URLSearchParams(window.location.search).get("next");
  if (!raw) {
    return null;
  }
  try {
    const target = new URL(raw, window.location.href);
    if (
      (target.protocol === "http:" || target.protocol === "https:") &&
      target.hostname === window.location.hostname
    ) {
      return target.href;
    }
  } catch {
    // 非法 next 直接忽略
  }
  return null;
}

async function bootstrap() {
  loading.value = true;
  error.value = "";
  try {
    const [metaInfo, sessionInfo] = await Promise.all([api.meta(), api.session()]);
    meta.value = metaInfo;
    session.value = sessionInfo;
    document.title = metaInfo.title;
    if (sessionInfo.authenticated && nextUrl) {
      window.location.replace(nextUrl);
      return;
    }
  } catch (err) {
    error.value = err instanceof Error ? err.message : "加载失败";
  } finally {
    loading.value = false;
  }
}

function handleLoggedIn(info: SessionInfo) {
  session.value = info;
  if (nextUrl) {
    window.location.replace(nextUrl);
  }
}

async function handleLogout() {
  try {
    await api.logout();
  } catch {
    // 退出失败也回到登录页
  }
  session.value = { authenticated: false };
}

function handleUnauthorized() {
  session.value = { authenticated: false };
}

onMounted(bootstrap);
</script>

<template>
  <div v-if="loading" class="boot">正在加载…</div>
  <div v-else-if="error" class="boot">
    <p>{{ error }}</p>
    <button class="button" type="button" @click="bootstrap">重试</button>
  </div>
  <LoginView
    v-else-if="meta && !session.authenticated"
    :title="meta.title"
    @logged-in="handleLoggedIn"
  />
  <HomeView
    v-else-if="meta && session.authenticated"
    :meta="meta"
    :username="session.username ?? ''"
    @logout="handleLogout"
    @unauthorized="handleUnauthorized"
  />
</template>
