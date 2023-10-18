$trigger = New-ScheduledTaskTrigger -Daily -At 1am
$action = New-ScheduledTaskAction -Execute 'powershell' -Argument "G:\task.ps1"
Register-ScheduledTask -TaskName "AutoCompile" -Action $action -Trigger $trigger

& 'C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\Common7\Tools\Launch-VsDevShell.ps1'

Get-ChildItem "G:\" -Directory | 
Foreach-Object {
    $sln = $_.FullName + '/debug/WPSOffice.sln'
    if (Test-Path -Path $sln -PathType Leaf){
        msbuild $sln -t:wpsmain
        msbuild $sln -t:etmain
        msbuild $sln -t:wppmain
        msbuild $sln -t:pdfmain
        msbuild $sln
        msbuild $sln
    }
}

