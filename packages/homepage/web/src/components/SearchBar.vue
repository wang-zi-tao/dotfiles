<script setup lang="ts">
import { computed, ref, watch } from "vue";
import type { Meta } from "../types";

const props = defineProps<{ meta: Meta; modelValue: string }>();
const emit = defineEmits<{ "update:modelValue": [string] }>();

const input = ref<HTMLInputElement | null>(null);
const engines = computed(() => props.meta.searchEngines);
const engineId = ref(
  localStorage.getItem("homepage.engine") ?? props.meta.defaultSearchEngine ?? engines.value[0]?.id ?? "",
);
const engine = computed(() => engines.value.find((item) => item.id === engineId.value) ?? engines.value[0]);

watch(engineId, (value) => {
  if (value) {
    localStorage.setItem("homepage.engine", value);
  }
});

function looksLikeUrl(query: string) {
  if (/\s/.test(query)) {
    return false;
  }
  return /^https?:\/\//i.test(query) || /^[\w-]+(\.[\w-]+)+([/?#].*)?$/.test(query);
}

function submit() {
  const query = props.modelValue.trim();
  if (!query) {
    return;
  }
  if (looksLikeUrl(query)) {
    window.location.href = /^https?:\/\//i.test(query) ? query : "https://" + query;
    return;
  }
  const template = engine.value?.url ?? "https://www.bing.com/search?q={query}";
  window.location.href = template.replace("{query}", encodeURIComponent(query));
}

function focus() {
  input.value?.focus();
  input.value?.select();
}

defineExpose({ focus });
</script>

<template>
  <form class="search glass" @submit.prevent="submit">
    <svg class="search-icon" viewBox="0 0 24 24" aria-hidden="true">
      <circle cx="11" cy="11" r="6.5" fill="none" stroke="currentColor" stroke-width="1.8" />
      <path d="M16 16l4.5 4.5" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" />
    </svg>
    <input
      ref="input"
      :value="modelValue"
      type="search"
      placeholder="搜索，或输入网址直接打开（按 / 聚焦）"
      autocomplete="off"
      spellcheck="false"
      @input="emit('update:modelValue', ($event.target as HTMLInputElement).value)"
    />
    <select v-if="engines.length" v-model="engineId" aria-label="搜索引擎">
      <option v-for="item in engines" :key="item.id" :value="item.id">{{ item.name }}</option>
    </select>
  </form>
</template>

<style scoped>
.search {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 8px 14px;
  border-radius: 14px;
  transition: border-color 0.15s ease, box-shadow 0.15s ease;
}

.search:focus-within {
  border-color: rgba(122, 162, 255, 0.6);
  box-shadow: 0 0 0 4px rgba(122, 162, 255, 0.12);
}

.search-icon {
  width: 20px;
  height: 20px;
  flex: none;
  color: var(--muted);
}

input {
  flex: 1;
  min-width: 0;
  padding: 10px 0;
  border: 0;
  background: transparent;
  font-size: 15px;
}

input::-webkit-search-cancel-button {
  display: none;
}

select {
  flex: none;
  max-width: 130px;
  padding: 7px 10px;
  border-radius: 9px;
  border: 1px solid var(--panel-border);
  background: rgba(9, 13, 26, 0.6);
  color: var(--muted);
  font-size: 12px;
  cursor: pointer;
}
</style>
