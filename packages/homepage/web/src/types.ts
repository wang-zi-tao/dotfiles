export interface Bookmark {
  id: number;
  name: string;
  url: string;
  icon: string | null;
  category: string;
}

export interface BookmarkInput {
  name: string;
  url: string;
  category: string;
  icon: string | null;
}

export interface SearchEngine {
  id: string;
  name: string;
  url: string;
}

export interface Meta {
  title: string;
  searchEngines: SearchEngine[];
  defaultSearchEngine: string | null;
}

export interface SessionInfo {
  authenticated: boolean;
  username?: string;
}

export interface IntranetEntry {
  name: string;
  url: string;
  description?: string | null;
  icon?: string | null;
}

export interface IntranetGroup {
  name: string;
  entries: IntranetEntry[];
}
