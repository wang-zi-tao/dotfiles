"""
PlantUML 语法检查脚本。

遍历 markdown 文件中的 ```plantuml fenced code blocks，
使用 plantuml.jar 检查每个 diagram 的语法正确性。

用法:
    python check_plantuml.py <file1.md> [file2.md ...]
    python check_plantuml.py --dir <directory>           # 递归扫描目录下所有 .md

exit code:
    0 = 所有 diagram 语法正确
    1 = 存在语法错误
    2 = plantuml.jar 未找到 或 Java 不可用
"""

import sys
import os
import re
import subprocess
import tempfile
import argparse
from pathlib import Path

PLANTUML_JAR = os.environ["DOTFILE_WINDOWS"] + "\\lib\\plantuml.jar"

# ```plantuml ... ``` fenced code block
PLANTUML_BLOCK_RE = re.compile(
    r"```plantuml\s*\n(.*?)```",
    re.DOTALL,
)

# @startuml ... @enduml（兼容不含 startuml 前缀的块）
STARTUML_RE = re.compile(r"@startuml\b")


def check_plantuml_block(content: str) -> tuple[bool, str]:
    """检查单个 plantuml 块的语法。

    返回 (is_valid, error_message)。
    """
    # 确保有 @startuml/@enduml 包裹
    if not STARTUML_RE.search(content):
        content = f"@startuml\n{content}\n@enduml"

    # 写入临时文件，避免 cmd 转义问题
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".puml", delete=False, encoding="utf-8"
    ) as f:
        f.write(content)
        tmp_path = f.name

    try:
        result = subprocess.run(
            ["java", "-jar", PLANTUML_JAR, "-pipe", "-syntax"],
            input=content,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=30,
        )
        output = result.stdout.strip()
        stderr = result.stderr.strip()
        if stderr:
            output += f"\n{stderr}"

        if output.startswith("ERROR"):
            return False, output
        return True, output
    except subprocess.TimeoutExpired:
        return False, "Timeout: plantuml 语法检查超时"
    except FileNotFoundError:
        return False, "Java 未找到，请确认 JDK 已安装"
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def check_file(filepath: str) -> list[dict]:
    """检查单个 markdown 文件中所有 plantuml 块。

    返回错误列表，每项为 {file, block_index, line, msg, preview}。
    """
    errors = []
    try:
        with open(filepath, encoding="utf-8") as f:
            content = f.read()
    except Exception as e:
        return [{"file": filepath, "block_index": 0, "line": 0, "msg": str(e), "preview": ""}]

    blocks = PLANTUML_BLOCK_RE.findall(content)

    if not blocks:
        return []

    for i, block in enumerate(blocks):
        valid, output = check_plantuml_block(block)
        if not valid:
            preview = block[:120].replace("\n", " ") + ("..." if len(block) > 120 else "")
            errors.append({
                "file": filepath,
                "block_index": i + 1,
                "line": _find_block_line(content, block),
                "msg": output,
                "preview": preview,
            })

    return errors


def _find_block_line(content: str, block: str) -> int:
    """在 content 中查找 block 首次出现位置的行号（1-based）。"""
    idx = content.find(block)
    if idx == -1:
        return 0
    return content[:idx].count("\n") + 1


def find_md_files(directory: str) -> list[str]:
    """递归查找目录下所有 .md 文件。"""
    return [str(p) for p in Path(directory).rglob("*.md")]


def main():
    global PLANTUML_JAR

    parser = argparse.ArgumentParser(description="PlantUML 语法检查")
    parser.add_argument("files", nargs="*", help="要检查的 .md 文件")
    parser.add_argument("--dir", help="递归扫描目录下所有 .md 文件")
    parser.add_argument("--plantuml", default=PLANTUML_JAR, help="plantuml.jar 路径")
    args = parser.parse_args()

    PLANTUML_JAR = args.plantuml

    # 检查 plantuml.jar 是否存在
    if not os.path.exists(PLANTUML_JAR):
        print(f"ERROR: plantuml.jar 未找到: {PLANTUML_JAR}")
        sys.exit(2)

    # 收集文件列表
    files: list[str] = []
    if args.dir:
        files = find_md_files(args.dir)
    files.extend(args.files)

    if not files:
        print("INFO: 未指定任何文件，使用 --dir <directory> 或直接传入文件路径")
        sys.exit(0)

    total_blocks = 0
    all_errors: list[dict] = []

    for filepath in files:
        if not os.path.isfile(filepath):
            print(f"WARN: 文件不存在，跳过: {filepath}")
            continue

        errors = check_file(filepath)
        total_blocks += _count_blocks(filepath)
        all_errors.extend(errors)

        if errors:
            for e in errors:
                print(f"FAIL  {e['file']}  block #{e['block_index']}  (line ~{e['line']})")
                print(f"      preview: {e['preview']}")
                print(f"      error:   {e['msg'].replace(chr(10), chr(10) + '               ')}")
        else:
            n = _count_blocks(filepath)
            if n > 0:
                print(f"OK    {filepath}  ({n} diagram{'s' if n > 1 else ''})")
            else:
                print(f"SKIP  {filepath}  (no plantuml blocks)")

    # 摘要
    failed = len(all_errors)
    if failed > 0:
        print(f"\nSUMMARY: {failed} error(s) in {len(set(e['file'] for e in all_errors))} file(s)  ({total_blocks} total blocks)")
        sys.exit(1)
    else:
        print(f"\nSUMMARY: all {total_blocks} diagram(s) valid in {len(files)} file(s)")
        sys.exit(0)


def _count_blocks(path: str) -> int:
    try:
        with open(path, encoding="utf-8") as f:
            return len(PLANTUML_BLOCK_RE.findall(f.read()))
    except Exception:
        return 0


if __name__ == "__main__":
    main()
