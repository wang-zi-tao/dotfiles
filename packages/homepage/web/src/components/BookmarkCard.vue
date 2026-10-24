<script setup lang="ts">
import { computed, ref } from "vue";
import type { Bookmark } from "../types";

const props = defineProps<{ bookmark: Bookmark }>();
const emit = defineEmits<{ edit: [Bookmark]; remove: [Bookmark] }>();

const failed = ref(false);

function favicon(url: string) {
  try {
    return new URL(url).origin + "/favicon.ico";
  } catch {
    return "";
  }
}

const iconSrc = computed(() => (failed.value ? "" : props.bookmark.icon ?? favicon(props.bookmark.url)));
const letter = computed(() => (props.bookmark.name.trim().charAt(0) || "?").toUpperCase());
const host = computed(() => {
  try {
    return new URL(props.bookmark.url).host;
  } catch {
    return props.bookmark.url;
  }
});

function open() {
  window.location.href = props.bookmark.url;
}
</script>

<template>
  <div class="card glass" role="link" tabindex="0" @click="open" @keydown.enter.prevent="open">
    <span class="icon">
      <img v-if="iconSrc" :src="iconSrc" alt="" loading="lazy" referrerpolicy="no-referrer" @error="failed = true" />
      <span v-else class="letter">{{ letter }}</span>
    </span>
    <span class="text">
      <strong>{{ bookmark.name }}</strong>
      <small>{{ host }}</small>
    </span>
    <span class="actions" @click.stop>
      <button class="button small ghost" type="button" title="编辑" @click="emit('edit', bookmark)">编辑</button>
      <button class="button small ghost danger" type="button" title="删除" @click="emit('remove', bookmark)">删除</button>
    </span>
  </div>
</template>

<style scoped>
.card {
  position: relative;
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 14px;
  border-radius: 13px;
  cursor: pointer;
  transition: transform 0.15s ease, border-color 0.15s ease, background 0.15s ease;
  min-width: 0;
}

.card:hover,
.card:focus-visible {
  transform: translateY(-2px);
  border-color: rgba(122, 162, 255, 0.45);
  background: rgba(30, 40, 64, 0.7);
  outline: none;
}

.icon {
  flex: none;
  width: 36px;
  height: 36px;
  display: grid;
  place-items: center;
  border-radius: 10px;
  background: rgba(255, 255, 255, 0.06);
  overflow: hidden;
}

.icon img {
  width: 22px;
  height: 22px;
  object-fit: contain;
}

.letter {
  font-size: 15px;
  font-weight: 600;
  color: #b9c8ff;
}

.text {
  display: grid;
  min-width: 0;
  line-height: 1.35;
}

.text strong {
  font-size: 14px;
  font-weight: 550;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.text small {
  font-size: 11.5px;
  color: var(--muted);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.actions {
  position: absolute;
  top: 6px;
  right: 6px;
  display: flex;
  gap: 2px;
  opacity: 0;
  pointer-events: none;
  background: var(--panel-strong);
  border-radius: 9px;
  padding: 2px;
  transition: opacity 0.15s ease;
}

.card:hover .actions,
.card:focus-within .actions {
  opacity: 1;
  pointer-events: auto;
}
</style>
