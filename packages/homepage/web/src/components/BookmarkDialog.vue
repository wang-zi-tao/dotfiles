<script setup lang="ts">
import { computed, reactive, ref, watch } from "vue";
import { api, ApiError } from "../api";
import type { Bookmark } from "../types";

const props = defineProps<{
  open: boolean;
  bookmark: Bookmark | null;
  categories: string[];
}>();

const emit = defineEmits<{
  close: [];
  saved: [];
  unauthorized: [];
}>();

const form = reactive({ name: "", url: "", category: "", icon: "" });
const busy = ref(false);
const error = ref("");
const title = computed(() => (props.bookmark ? "编辑收藏" : "添加收藏"));

watch(
  () => props.open,
  (open) => {
    if (!open) {
      return;
    }
    error.value = "";
    form.name = props.bookmark?.name ?? "";
    form.url = props.bookmark?.url ?? "";
    form.category = props.bookmark?.category ?? props.categories[0] ?? "常用";
    form.icon = props.bookmark?.icon ?? "";
  },
  { immediate: true },
);

async function submit() {
  if (busy.value) {
    return;
  }
  error.value = "";
  if (!form.url.trim()) {
    error.value = "请填写网址";
    return;
  }
  busy.value = true;
  try {
    const payload = {
      name: form.name.trim(),
      url: form.url.trim(),
      category: form.category.trim(),
      icon: form.icon.trim() ? form.icon.trim() : null,
    };
    if (props.bookmark) {
      await api.updateBookmark(props.bookmark.id, payload);
    } else {
      await api.createBookmark(payload);
    }
    emit("saved");
    emit("close");
  } catch (err) {
    if (err instanceof ApiError && err.status === 401) {
      emit("unauthorized");
      emit("close");
      return;
    }
    error.value = err instanceof Error ? err.message : "保存失败";
  } finally {
    busy.value = false;
  }
}
</script>

<template>
  <Teleport to="body">
    <div v-if="open" class="overlay" @click.self="emit('close')" @keydown.esc="emit('close')">
      <form class="dialog glass" @submit.prevent="submit">
        <header>
          <h2>{{ title }}</h2>
          <button class="button small ghost" type="button" aria-label="关闭" @click="emit('close')">×</button>
        </header>
        <label>
          <span>名称</span>
          <input v-model="form.name" type="text" placeholder="留空则使用域名" />
        </label>
        <label>
          <span>网址</span>
          <input v-model="form.url" type="text" placeholder="https://example.com" />
        </label>
        <label>
          <span>分类</span>
          <input v-model="form.category" type="text" list="homepage-categories" placeholder="常用" />
          <datalist id="homepage-categories">
            <option v-for="item in categories" :key="item" :value="item" />
          </datalist>
        </label>
        <label>
          <span>图标地址（可选）</span>
          <input v-model="form.icon" type="text" placeholder="https://example.com/favicon.ico" />
        </label>
        <p v-if="error" class="form-error">{{ error }}</p>
        <footer>
          <button class="button" type="button" @click="emit('close')">取消</button>
          <button class="button primary" type="submit" :disabled="busy">
            {{ busy ? "保存中…" : "保存" }}
          </button>
        </footer>
      </form>
    </div>
  </Teleport>
</template>

<style scoped>
.overlay {
  position: fixed;
  inset: 0;
  z-index: 50;
  display: grid;
  place-items: center;
  padding: 20px;
  background: rgba(4, 7, 15, 0.6);
  backdrop-filter: blur(4px);
}

.dialog {
  width: min(440px, 100%);
  padding: 22px 24px;
  background: var(--panel-strong);
  display: grid;
  gap: 14px;
}

header {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

header h2 {
  margin: 0;
  font-size: 17px;
}

label {
  display: grid;
  gap: 6px;
  font-size: 12.5px;
  color: var(--muted);
}

label input {
  padding: 10px 12px;
  border-radius: 9px;
  border: 1px solid var(--panel-border);
  background: rgba(9, 13, 26, 0.6);
}

label input:focus {
  border-color: rgba(122, 162, 255, 0.65);
  box-shadow: 0 0 0 3px rgba(122, 162, 255, 0.12);
}

footer {
  display: flex;
  justify-content: flex-end;
  gap: 10px;
  margin-top: 4px;
}
</style>
