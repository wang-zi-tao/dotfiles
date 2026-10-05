// @ts-nocheck —— 构建期脚本：js-yaml / Node 的类型只在 Nix 构建里解析，编辑器里没有。
// 把 YAML 文本里「被引号包住的 !!js 表达式」提升成真正的 YAML 标签。
//
//   input : config: { apiKey: "!!js fs.readFileSync(…).trim()" }
//   output: config: { apiKey: !!js fs.readFileSync(…).trim() }
//
// 标签与节点形状跟 dsh 的 JsExpr（packages/boot/app-boot/lib/index.js）逐字同构，
// 所以结果能被 dsh 的 entryListSchema 原样吃下，并在条目激活时求值。
//
// 用法: node js-tag.mjs <input.yml> <output.yml>

import fs from 'node:fs'
import yaml from 'js-yaml'

const MARK = '!!js'

const isJsExpr = (value) => !!value && typeof value === 'object' && typeof value.__jsExpr === 'string'

// 与 dsh 的 JsExpr 完全一致：读时 construct 成 { __jsExpr }，写时 represent 回裸标签。
const JsExpr = new yaml.Type('tag:yaml.org,2002:js', {
  kind: 'scalar',
  resolve: (data) => typeof data === 'string',
  construct: (data) => ({ __jsExpr: data }),
  predicate: isJsExpr,
  represent: (data) => data.__jsExpr,
})

// extend() 把 JsExpr 放进 explicit 类型表，load/dump 双向都认它。
const schema = yaml.JSON_SCHEMA.extend([JsExpr])

/** 深度遍历，把 "!!js <expr>" 字符串换成 JsExpr 节点；其余原样保留（幂等）。 */
function promote(node) {
  if (typeof node === 'string') {
    if (!node.startsWith(MARK)) return node
    const rest = node.slice(MARK.length)
    // 后面必须跟空白或直接结束，免得误伤 "!!json…" / "!!jsx…"
    if (rest !== '' && !/^\s/.test(rest)) return node
    return { __jsExpr: rest.trim() }
  }
  if (Array.isArray(node)) return node.map(promote)
  if (node !== null && typeof node === 'object') {
    return Object.fromEntries(Object.entries(node).map(([key, value]) => [key, promote(value)]))
  }
  return node
}

const [input, output] = process.argv.slice(2)
if (!input || !output) {
  console.error('usage: js-tag.mjs <input.yml> <output.yml>')
  process.exit(2)
}

const doc = yaml.load(fs.readFileSync(input, 'utf8'), { schema })

fs.writeFileSync(
  output,
  yaml.dump(promote(doc), {
    schema,
    lineWidth: -1, // js-yaml 里 -1 = 不折行，长表达式保持一行
    noRefs: true, // 重复子对象直接展开，不生成 &anchor / *alias
  }),
)
