/**
 * Minimal ambient declarations for the Node built-ins used by this package.
 * The Nix build compiles with nixpkgs TypeScript only; these declarations
 * keep the package free of @types/node.
 */

declare module 'node:fs' {
  export function readFileSync(path: string, encoding: 'utf8'): string
}

declare module 'node:path' {
  export function resolve(...segments: string[]): string
}

interface ProcessEnv {
  [key: string]: string | undefined
}

interface Process {
  env: ProcessEnv
}

declare const process: Process
