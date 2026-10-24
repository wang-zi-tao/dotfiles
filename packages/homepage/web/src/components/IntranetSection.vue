<script setup lang="ts">
import { reactive } from "vue";
import type { IntranetGroup } from "../types";

defineProps<{ groups: IntranetGroup[] }>();

// 图标来自公开 CDN，加载失败时退回原来的小圆点，避免出现破图。
const failedIcons = reactive<Record<string, boolean>>({});

function hostLabel(url: string) {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}
</script>

<template>
  <section class="intranet">
    <header class="section-head">
      <h2>内网入口</h2>
      <span class="section-note">由 nix 生成</span>
    </header>
    <div class="groups">
      <article v-for="group in groups" :key="group.name" class="group glass">
        <h3>{{ group.name }}</h3>
        <div class="entries">
          <a
            v-for="entry in group.entries"
            :key="entry.url"
            class="entry"
            :href="entry.url"
            :title="entry.url"
            target="_blank"
            rel="noreferrer"
          >
            <img
              v-if="entry.icon && !failedIcons[entry.url]"
              class="icon"
              :src="entry.icon"
              alt=""
              loading="lazy"
              referrerpolicy="no-referrer"
              @error="failedIcons[entry.url] = true"
            />
            <span v-else class="dot" aria-hidden="true"></span>
            <span class="entry-text">
              <strong>{{ entry.name }}</strong>
              <small>{{ entry.description ?? hostLabel(entry.url) }}</small>
            </span>
          </a>
        </div>
      </article>
    </div>
  </section>
</template>

<style scoped>
.intranet {
  display: grid;
  gap: 12px;
}

.section-head {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
}

.section-head h2 {
  margin: 0;
  font-size: 16px;
  font-weight: 600;
}

.section-note {
  font-size: 12px;
  color: var(--muted);
}

/* 服务器之间纵向排列，一台服务器一行 */
.groups {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

/* 左侧固定服务器名，右侧网站按横向表格铺开 */
.group {
  padding: 10px 14px;
  display: grid;
  grid-template-columns: 96px minmax(0, 1fr);
  gap: 6px 14px;
  align-items: start;
}

.group h3 {
  margin: 0;
  padding-top: 7px;
  font-size: 12px;
  font-weight: 600;
  letter-spacing: 0.06em;
  color: var(--muted);
  overflow-wrap: anywhere;
}

/* 同一服务器内的网站横向排成表格 */
.entries {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
  gap: 6px;
  min-width: 0;
}

.entry {
  display: flex;
  align-items: center;
  gap: 7px;
  padding: 5px 9px;
  border-radius: 9px;
  border: 1px solid transparent;
  background: rgba(255, 255, 255, 0.035);
  transition:
    background 0.15s ease,
    border-color 0.15s ease;
  min-width: 0;
}

.entry:hover {
  background: rgba(255, 255, 255, 0.08);
  border-color: rgba(122, 162, 255, 0.25);
}

.icon {
  flex: none;
  width: 36px;
  height: 36px;
  object-fit: contain;
  border-radius: 4px;
}

.dot {
  flex: none;
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: var(--accent-2);
  box-shadow: 0 0 8px rgba(94, 234, 212, 0.65);
}

.entry-text {
  display: flex;
  flex-direction: column;
  line-height: 1.25;
  min-width: 0;
}

.entry-text strong {
  font-size: 13px;
  font-weight: 500;
}

.entry-text small {
  max-width: 100%;
  font-size: 10.5px;
  color: var(--muted);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

/* 窄屏下服务器名单独一行，避免挤占表格宽度 */
@media (max-width: 640px) {
  .group {
    grid-template-columns: minmax(0, 1fr);
    gap: 6px;
  }

  .group h3 {
    padding-top: 0;
  }
}
</style>
