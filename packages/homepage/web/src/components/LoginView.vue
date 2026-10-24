<script setup lang="ts">
import { onMounted, ref } from "vue";
import { api } from "../api";

defineProps<{ title: string }>();
const emit = defineEmits<{ loggedIn: [{ authenticated: boolean; username: string }] }>();

const username = ref("");
const password = ref("");
const busy = ref(false);
const error = ref("");
const usernameInput = ref<HTMLInputElement | null>(null);

async function submit() {
  if (busy.value) {
    return;
  }
  error.value = "";
  if (!username.value.trim() || !password.value) {
    error.value = "请输入用户名和密码";
    return;
  }
  busy.value = true;
  try {
    const result = await api.login(username.value.trim(), password.value);
    emit("loggedIn", { authenticated: true, username: result.username });
  } catch (err) {
    error.value = err instanceof Error ? err.message : "登录失败";
  } finally {
    busy.value = false;
  }
}

onMounted(() => usernameInput.value?.focus());
</script>

<template>
  <div class="login-page">
    <div class="login-card glass">
      <div class="login-badge">W</div>
      <h1>{{ title }}</h1>
      <p class="login-sub">登录后查看收藏与内网入口</p>
      <form class="login-form" @submit.prevent="submit">
        <label>
          <span>用户名</span>
          <input
            ref="usernameInput"
            v-model="username"
            type="text"
            autocomplete="username"
            spellcheck="false"
          />
        </label>
        <label>
          <span>密码</span>
          <input v-model="password" type="password" autocomplete="current-password" />
        </label>
        <p v-if="error" class="form-error">{{ error }}</p>
        <button class="button primary" type="submit" :disabled="busy">
          {{ busy ? "登录中…" : "登录" }}
        </button>
      </form>
      <p class="login-foot">密码由 sops 管理，仅本人可见</p>
    </div>
  </div>
</template>

<style scoped>
.login-page {
  min-height: 100vh;
  display: grid;
  place-items: center;
  padding: 24px;
}

.login-card {
  width: min(400px, 100%);
  padding: 40px 36px 28px;
  text-align: center;
}

.login-badge {
  width: 56px;
  height: 56px;
  margin: 0 auto 18px;
  display: grid;
  place-items: center;
  font-size: 26px;
  font-weight: 700;
  border-radius: 18px;
  color: #bcd0ff;
  background: linear-gradient(145deg, rgba(122, 162, 255, 0.35), rgba(94, 234, 212, 0.12));
  border: 1px solid rgba(122, 162, 255, 0.35);
}

h1 {
  margin: 0;
  font-size: 24px;
  letter-spacing: 0.02em;
}

.login-sub {
  margin: 8px 0 26px;
  color: var(--muted);
  font-size: 13px;
}

.login-form {
  display: grid;
  gap: 16px;
  text-align: left;
}

.login-form label {
  display: grid;
  gap: 6px;
  font-size: 13px;
  color: var(--muted);
}

.login-form input {
  padding: 11px 13px;
  border-radius: 10px;
  border: 1px solid var(--panel-border);
  background: rgba(9, 13, 26, 0.55);
  outline: none;
  transition: border-color 0.15s ease, box-shadow 0.15s ease;
}

.login-form input:focus {
  border-color: rgba(122, 162, 255, 0.7);
  box-shadow: 0 0 0 3px rgba(122, 162, 255, 0.15);
}

.login-form .button {
  margin-top: 4px;
  width: 100%;
}

.login-foot {
  margin: 20px 0 0;
  font-size: 12px;
  color: rgba(152, 163, 189, 0.7);
}
</style>
