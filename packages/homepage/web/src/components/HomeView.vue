<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from "vue";
import { api, ApiError } from "../api";
import type { Bookmark, IntranetGroup, Meta } from "../types";
import BookmarkCard from "./BookmarkCard.vue";
import BookmarkDialog from "./BookmarkDialog.vue";
import IntranetSection from "./IntranetSection.vue";
import SearchBar from "./SearchBar.vue";

const props = defineProps<{ meta: Meta; username: string }>();
const emit = defineEmits<{ logout: []; unauthorized: [] }>();

const bookmarks = ref<Bookmark[]>([]);
const intranet = ref<IntranetGroup[]>([]);
const loading = ref(true);
const loadError = ref("");
const notice = ref("");
const filter = ref("");
const activeCategory = ref("全部");
const dialogOpen = ref(false);
const editing = ref<Bookmark | null>(null);
const searchBar = ref<{ focus: () => void } | null>(null);
const now = ref(new Date());
let ticker = 0;
let noticeTimer = 0;

const categories = computed(() => {
  const order: string[] = [];
  const map = new Map<string, Bookmark[]>();
  for (const bookmark of bookmarks.value) {
    if (!map.has(bookmark.category)) {
      map.set(bookmark.category, []);
      order.push(bookmark.category);
    }
    map.get(bookmark.category)!.push(bookmark);
  }
  return order.map((name) => ({ name, bookmarks: map.get(name)! }));
});

const categoryNames = computed(() => categories.value.map((item) => item.name));

const visibleCategories = computed(() => {
  const needle = filter.value.trim().toLowerCase();
  const source =
    activeCategory.value === "全部"
      ? categories.value
      : categories.value.filter((item) => item.name === activeCategory.value);
  if (!needle) {
    return source;
  }
  return source
    .map((item) => ({
      name: item.name,
      bookmarks: item.bookmarks.filter((bookmark) => matches(bookmark, needle)),
    }))
    .filter((item) => item.bookmarks.length > 0);
});

const totalVisible = computed(() =>
  visibleCategories.value.reduce((sum, item) => sum + item.bookmarks.length, 0),
);

const clock = computed(() =>
  now.value.toLocaleTimeString("zh-CN", { hour: "2-digit", minute: "2-digit", hour12: false }),
);
const dateText = computed(() =>
  now.value.toLocaleDateString("zh-CN", { month: "long", day: "numeric", weekday: "long" }),
);
const greeting = computed(() => {
  const hour = now.value.getHours();
  if (hour < 6) return "夜深了";
  if (hour < 11) return "早上好";
  if (hour < 14) return "中午好";
  if (hour < 18) return "下午好";
  return "晚上好";
});

function matches(bookmark: Bookmark, needle: string) {
  return (
    bookmark.name.toLowerCase().includes(needle) ||
    bookmark.url.toLowerCase().includes(needle) ||
    bookmark.category.toLowerCase().includes(needle)
  );
}

async function load() {
  loading.value = true;
  loadError.value = "";
  try {
    const [items, groups] = await Promise.all([api.bookmarks(), api.intranet()]);
    bookmarks.value = items;
    intranet.value = groups;
  } catch (err) {
    if (err instanceof ApiError && err.status === 401) {
      emit("unauthorized");
      return;
    }
    loadError.value = err instanceof Error ? err.message : "加载失败";
  } finally {
    loading.value = false;
  }
}

function flash(message: string) {
  notice.value = message;
  window.clearTimeout(noticeTimer);
  noticeTimer = window.setTimeout(() => {
    notice.value = "";
  }, 4000);
}

function openAdd() {
  editing.value = null;
  dialogOpen.value = true;
}

function openEdit(bookmark: Bookmark) {
  editing.value = bookmark;
  dialogOpen.value = true;
}

async function removeBookmark(bookmark: Bookmark) {
  if (!window.confirm("删除收藏「" + bookmark.name + "」？")) {
    return;
  }
  try {
    await api.deleteBookmark(bookmark.id);
    flash("已删除「" + bookmark.name + "」");
    await load();
  } catch (err) {
    if (err instanceof ApiError && err.status === 401) {
      emit("unauthorized");
      return;
    }
    flash(err instanceof Error ? err.message : "删除失败");
  }
}

async function handleSaved() {
  await load();
  flash("收藏已保存");
}

function onKeydown(event: KeyboardEvent) {
  const target = event.target as HTMLElement | null;
  const typing =
    target?.tagName === "INPUT" || target?.tagName === "TEXTAREA" || target?.tagName === "SELECT";
  if (event.key === "/" && !typing) {
    event.preventDefault();
    searchBar.value?.focus();
  } else if (event.key === "Escape") {
    if (dialogOpen.value) {
      dialogOpen.value = false;
    } else if (filter.value) {
      filter.value = "";
    }
  }
}

watch(categories, (items) => {
  if (activeCategory.value !== "全部" && !items.some((item) => item.name === activeCategory.value)) {
    activeCategory.value = "全部";
  }
});

onMounted(() => {
  ticker = window.setInterval(() => {
    now.value = new Date();
  }, 1000);
  window.addEventListener("keydown", onKeydown);
  load();
});

onUnmounted(() => {
  window.clearInterval(ticker);
  window.clearTimeout(noticeTimer);
  window.removeEventListener("keydown", onKeydown);
});
</script>

<template>
  <div class="page">
    <header class="topbar">
      <div class="brand">
        <span class="logo">W</span>
        <div class="brand-text">
          <h1>{{ meta.title }}</h1>
          <p>{{ dateText }}</p>
        </div>
      </div>
      <div class="top-right">
        <span class="clock">{{ clock }}</span>
        <span class="user">{{ username }}</span>
        <button class="button small ghost" type="button" @click="emit('logout')">退出</button>
      </div>
    </header>

    <main>
      <section class="hero">
        <p class="greeting">{{ greeting }}，{{ username }}</p>
        <SearchBar ref="searchBar" v-model="filter" :meta="meta" />
        <p class="hero-hint">输入关键字即时筛选收藏，按 <kbd>/</kbd> 聚焦搜索框</p>
      </section>

      <IntranetSection v-if="intranet.length" :groups="intranet" />

      <section class="bookmarks">
        <header class="section-head">
          <h2>收藏</h2>
          <div class="chips">
            <button
              class="chip"
              :class="{ active: activeCategory === '全部' }"
              type="button"
              @click="activeCategory = '全部'"
            >
              全部
            </button>
            <button
              v-for="item in categories"
              :key="item.name"
              class="chip"
              :class="{ active: activeCategory === item.name }"
              type="button"
              @click="activeCategory = item.name"
            >
              {{ item.name }}
              <small>{{ item.bookmarks.length }}</small>
            </button>
          </div>
          <button class="button small primary" type="button" @click="openAdd">+ 添加收藏</button>
        </header>

        <p v-if="loading" class="state">加载中…</p>
        <p v-else-if="loadError" class="state error">
          {{ loadError }}
          <button class="button small" type="button" @click="load">重试</button>
        </p>
        <template v-else>
          <article v-for="item in visibleCategories" :key="item.name" class="category">
            <h3>
              {{ item.name }}
              <small>{{ item.bookmarks.length }}</small>
            </h3>
            <div class="grid">
              <BookmarkCard
                v-for="bookmark in item.bookmarks"
                :key="bookmark.id"
                :bookmark="bookmark"
                @edit="openEdit"
                @remove="removeBookmark"
              />
            </div>
          </article>
          <p v-if="!visibleCategories.length" class="state">
            {{ filter ? "没有匹配「" + filter + "」的收藏" : "还没有收藏，点击右上角添加" }}
          </p>
        </template>
      </section>
    </main>

    <footer class="foot">
      <span>{{ totalVisible }} 个收藏</span>
      <span>·</span>
      <span>homepage</span>
    </footer>

    <Transition name="toast">
      <div v-if="notice" class="toast glass">{{ notice }}</div>
    </Transition>

    <BookmarkDialog
      :open="dialogOpen"
      :bookmark="editing"
      :categories="categoryNames"
      @close="dialogOpen = false"
      @saved="handleSaved"
      @unauthorized="emit('unauthorized')"
    />
  </div>
</template>

<style scoped>
.page {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  width: min(1180px, 100%);
  margin: 0 auto;
  padding: 24px clamp(16px, 4vw, 40px) 40px;
  gap: 26px;
}

.topbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
}

.brand {
  display: flex;
  align-items: center;
  gap: 12px;
}

.logo {
  width: 42px;
  height: 42px;
  display: grid;
  place-items: center;
  border-radius: 13px;
  font-weight: 700;
  font-size: 20px;
  color: #cdd9ff;
  background: linear-gradient(145deg, rgba(122, 162, 255, 0.4), rgba(94, 234, 212, 0.12));
  border: 1px solid rgba(122, 162, 255, 0.35);
}

.brand-text h1 {
  margin: 0;
  font-size: 18px;
  letter-spacing: 0.02em;
}

.brand-text p {
  margin: 2px 0 0;
  font-size: 12px;
  color: var(--muted);
}

.top-right {
  display: flex;
  align-items: center;
  gap: 14px;
}

.clock {
  font-size: 22px;
  font-variant-numeric: tabular-nums;
  letter-spacing: 0.04em;
}

.user {
  font-size: 13px;
  color: var(--muted);
}

main {
  display: grid;
  gap: 30px;
}

.hero {
  display: grid;
  gap: 12px;
  padding: 28px 26px;
  border-radius: 20px;
  background: linear-gradient(160deg, rgba(122, 162, 255, 0.13), rgba(94, 234, 212, 0.05));
  border: 1px solid rgba(122, 162, 255, 0.18);
}

.greeting {
  margin: 0;
  font-size: 22px;
  font-weight: 600;
  letter-spacing: 0.02em;
}

.hero-hint {
  margin: 0;
  font-size: 12px;
  color: var(--muted);
}

kbd {
  padding: 1px 6px;
  border-radius: 5px;
  border: 1px solid var(--panel-border);
  background: rgba(9, 13, 26, 0.6);
  font-size: 11px;
}

.section-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 14px;
  flex-wrap: wrap;
}

.section-head h2 {
  margin: 0;
  font-size: 16px;
  font-weight: 600;
}

.chips {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  flex: 1;
  justify-content: center;
}

.chip {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 5px 11px;
  border-radius: 999px;
  border: 1px solid var(--panel-border);
  background: rgba(255, 255, 255, 0.03);
  color: var(--muted);
  font-size: 12px;
  cursor: pointer;
  transition: background 0.15s ease, color 0.15s ease, border-color 0.15s ease;
}

.chip small {
  font-size: 10.5px;
  opacity: 0.7;
}

.chip:hover {
  color: var(--text);
  background: rgba(255, 255, 255, 0.07);
}

.chip.active {
  color: #0b1020;
  background: var(--accent);
  border-color: var(--accent);
}

.bookmarks {
  display: grid;
  gap: 18px;
}

.category {
  display: grid;
  gap: 10px;
}

.category h3 {
  margin: 0;
  font-size: 13px;
  font-weight: 600;
  color: var(--muted);
  letter-spacing: 0.06em;
}

.category h3 small {
  margin-left: 4px;
  opacity: 0.6;
}

.grid {
  display: grid;
  gap: 10px;
  grid-template-columns: repeat(auto-fill, minmax(210px, 1fr));
}

.state {
  margin: 0;
  padding: 18px 0;
  color: var(--muted);
  font-size: 13.5px;
  display: flex;
  align-items: center;
  gap: 12px;
}

.state.error {
  color: var(--danger);
}

.foot {
  margin-top: auto;
  padding-top: 8px;
  display: flex;
  gap: 8px;
  justify-content: center;
  font-size: 11.5px;
  color: rgba(152, 163, 189, 0.6);
}

.toast {
  position: fixed;
  bottom: 26px;
  left: 50%;
  transform: translateX(-50%);
  padding: 10px 18px;
  border-radius: 999px;
  font-size: 13px;
  z-index: 60;
}

.toast-enter-active,
.toast-leave-active {
  transition: opacity 0.2s ease, transform 0.2s ease;
}

.toast-enter-from,
.toast-leave-to {
  opacity: 0;
  transform: translate(-50%, 8px);
}

@media (max-width: 640px) {
  .top-right .user {
    display: none;
  }

  .clock {
    font-size: 18px;
  }

  .hero {
    padding: 20px 18px;
  }

  .greeting {
    font-size: 19px;
  }
}
</style>
