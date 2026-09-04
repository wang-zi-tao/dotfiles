
$file = 'packages/dsh-neovim/src/index.ts'
$content = [System.IO.File]::ReadAllText($file, [System.Text.Encoding]::UTF8)

$old = @'
  async function dapStep(action: string, luaFn: string, exec: ToolRunContext): Promise<string> {
    markSession(exec)
    const ret = await dapCall(`require("${config.luaModule}").${luaFn}()`)
    if (ret.status === 'terminated') return '调试目标已退出'
    let out = `## 调试器已暂停 (${action})\n`
    out += `**原因:** ${ret.reason || '?'}\n`
    out += fmtStack({ frames: ret.frames || [], thread_id: ret.thread_id })
    return out
  }
'@

$new = @'
  async function dapStep(action: string, luaFn: string, args: any, exec: ToolRunContext): Promise<string> {
    markSession(exec)
    const ret = await dapCall(`require("${config.luaModule}").${luaFn}(args)`, { args })
    if (ret.status === 'terminated') return '调试目标已退出'
    let out = `## 调试器已暂停 (${action})\n`
    out += `**原因:** ${ret.reason || '?'}\n`
    out += fmtStack({ frames: ret.frames || [], thread_id: ret.thread_id })
    return out
  }
'@

$content = $content.Replace($old, $new)
[System.IO.File]::WriteAllText($file, $content, [System.Text.Encoding]::UTF8)
Write-Output "Edit 1 (dapStep): OK - replaced $(if ($content.Contains($old)) {'0'} else {'1'})"
