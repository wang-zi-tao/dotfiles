{
  stdenvNoCC,
  fetchurl,
  runCommand,
  nodejs,
}:

# 把 pkgs.formats.yaml 生成的 cordis.patch.yml 里「被引号包住的 !!js 表达式」
# 还原成 dsh 真正认的 YAML 标签：
#
#   apiKey: "!!js require('fs')…"      ← formats.yaml 的产物（普通字符串）
#   apiKey: !!js require('fs')…        ← 转换后（标签，dsh 会在 entry 激活时求值）
#
# 为什么需要这一步：`!!js` 是 dsh 注册的自定义 YAML 标签
# （tag:yaml.org,2002:js，见 dsh 的 packages/boot/app-boot/lib/index.js 里的
#  JsExpr = new yaml.Type(...)）。只有作为**标签**出现时，Loader 才会把它
# construct 成 { __jsExpr } 并在条目激活时求值。而 formats.yaml 走的是
# remarshal/json2yaml，会给每个字符串加引号（长字符串还会折成 block scalar），
# 标签于是退化成普通字符串，表达式永远不会执行。
#
# 用法（`yaml` 是 formats.yaml 生成出来的文件路径，直接喂给脚本读，
# 不走 builtins.readFile，因此不引入 IFD）：
#   pkgs.dsh-cordis-patch {
#     yaml = (pkgs.formats.yaml { }).generate "cordis.raw.yml" value;
#   }
#   pkgs.dsh-cordis-patch { name = "x.yml"; yaml = …; }
#
# 写 !!js 表达式时注意它跑在 Loader 的沙箱里：
#   new Function("ctx", "expr", "with (ctx) { return eval(expr) }")
# 只有全局作用域 + loader provide 的成员（dsh 只 provide 了 dshHomePath），
# 所以 **没有 require**：写 require('fs') 会 ReferenceError，该 entry 直接
# 激活失败、插件不加载。要读文件用 process.getBuiltinModule('fs')，
# 并且 readFileSync 必须带 'utf8'（不给编码返回 Buffer，而没有 Buffer.trim()）。
#
# 转换是幂等的：已经是裸标签的输入（yaml 解析后是 { __jsExpr } 节点）
# 再跑一次输出不变。

let
  # js-yaml 4.2.0 —— 与 dsh 自带的同版本。dsh 的 JsExpr 就是
  # `new yaml.Type(...)` + `JSON_SCHEMA.extend(...)`，这是 js-yaml 的 API，
  # 只有它能按同样的 construct/represent 约定把自定义标签读写回去。
  # 主库不 require argparse（只有 bin/ 用），所以不需要额外取那个依赖。
  jsYaml = stdenvNoCC.mkDerivation {
    pname = "dsh-cordis-patch-js-yaml";
    version = "4.2.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/js-yaml/-/js-yaml-4.2.0.tgz";
      hash = "sha256-ULr8SqTLJjstOz9DBTXtjUkpTrEHeW52fXalykdaymY=";
    };
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      runHook preInstall
      mkdir -p "$out/lib/node_modules/js-yaml"
      cp -r ./* "$out/lib/node_modules/js-yaml/"
      runHook postInstall
    '';
  };
in
{
  yaml,
  name ? "cordis.patch.yml",
}:
runCommand name
  {
    nativeBuildInputs = [ nodejs ];
    meta = {
      description = "Restore bare !!js YAML tags in a generated cordis.patch.yml";
    };
  }
  ''
    # Node 的原生解析按目录逐级往上找 node_modules，把 js-yaml 摆在脚本旁边即可。
    mkdir -p node_modules
    ln -s ${jsYaml}/lib/node_modules/js-yaml node_modules/js-yaml
    cp ${./js-tag.mjs} js-tag.mjs
    node js-tag.mjs "${yaml}" "$out"
  ''
