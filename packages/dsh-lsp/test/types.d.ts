declare module 'node:test' {
  const test: (name: string, fn: () => void | Promise<void>) => void
  export default test
}

declare module 'node:assert/strict' {
  export function ok(value: unknown, message?: string): asserts value
  export function equal(actual: unknown, expected: unknown, message?: string): void
  export function deepEqual(actual: unknown, expected: unknown, message?: string): void
  export function match(value: string, pattern: RegExp, message?: string): void
  export function throws(fn: () => void, expected?: RegExp | Error, message?: string): void
  export function rejects(fn: () => Promise<unknown>, expected?: RegExp | ((error: unknown) => boolean), message?: string): Promise<void>
}
