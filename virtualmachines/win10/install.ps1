set-DnsClientServerAddress -InterfaceIndex 14 -ServerAddresses ("8.8.8.8","192.168.122.1")

# choco 
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
choco install -y .\packages.conf

# powershell
$key = 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Console\TrueTypeFont'
Set-ItemProperty -Path $key -Name '000' -Value 'CaskaydiaCove Nerd Font'
Install-Module posh-git -Force
Install-Module Terminal-Icons -Force 
Install-Module devtoolbox -Force
Install-Module PSReadLine -Force
Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope LocalMachine -Force

# virtiofs
cmd /c "sc create VirtioFsSvc binpath=`"C:\ProgramData\chocolatey\lib\virtio-drivers\tools\virtio\viofs\w10\amd64\virtiofs.exe`" start=auto depend=`"WinFsp.Launcher/VirtioFsDrv`" DisplayName=`"Virtio FS Service`""
cmd /c "sc start VirtioFsSvc"

Enable-WindowsOptionalFeature -FeatureName ServicesForNFS-ClientOnly, ClientForNFS-Infrastructure -Online -NoRestart

# neovim
mkdir ~/AppData/Local/nvim
cp ../../packages/wangzi-neovim/* ~/AppData/Local/nvim -Force -Recurse
pip3 install --user neovim-remote
npm i -g pyright

#rdp
Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server' -name "fDenyTSConnections" -value 0
(Get-WmiObject -class "Win32_TSGeneralSetting" -Namespace root\cimv2\terminalservices -ComputerName $ComputerName -Filter "TerminalName='RDP-tcp'").SetUserAuthenticationRequired(0)
Enable-NetFirewallRule -DisplayGroup "Remote Desktop"

# sshd
Get-WindowsCapability -Online | Where-Object Name -like 'OpenSSH.Server*' | Add-WindowsCapability –Online 
New-NetFirewallRule -Name sshd -DisplayName 'OpenSSH Server (sshd)' -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22
Set-Service -Name sshd -StartupType 'Automatic'
Start-Service sshd


# vcpkg
git clone "https://github.com/microsoft/vcpkg" "C:\vcpkg"
C:\vcpkg\bootstrap-vcpkg.bat
[Environment]::SetEnvironmentVariable(
    "Path",
    [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::Machine) + ";C:\vcpkg",
    [EnvironmentVariableTarget]::Machine)
C:\vcpkg\vcpkg.exe install v8 libffi qt5

# keys
$hexified = "00,00,00,00,00,00,00,00,02,00,00,00,1d,00,3a,00,00,00,00,00".Split(',') | % { "0x$_"};
$kbLayout = 'HKLM:\System\CurrentControlSet\Control\Keyboard Layout';
New-ItemProperty -Path $kbLayout -Name "Scancode Map" -PropertyType Binary -Value ([byte[]]$hexified);

# vs
&devenv /ResetSettings ./vsconfig.vsconfig
$DestPath="C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build"
cp "$DestPath\14.24\Microsoft.VCToolsVersion.14.24.props" "$DestPath\Microsoft.VCToolsVersion.default.props" -Force
cp "$DestPath\14.24\Microsoft.VCToolsVersion.14.24.props" "$DestPath\Microsoft.VCToolsVersion.v142.default.props" -Force
cp "$DestPath\14.24\Microsoft.VCToolsVersion.14.24.txt" "$DestPath\Microsoft.VCToolsVersion.default.txt" -Force
cp "$DestPath\14.24\Microsoft.VCToolsVersion.14.24.txt" "$DestPath\Microsoft.VCToolsVersion.v142.default.txt" -Force
$DestPath="C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC\"
cd $DestPath
pip3 install kso-repo-tool -i https://mirrors.wps.cn/pypi/dev/ --no-cache --user
for /f "delims=" %%i in ('dir /b/a:d %DestPath%') do if "14.24.28314" NEQ "%%i" rmdir /s %%i

#task
$trigger = New-ScheduledTaskTrigger -Daily -At 1am
$action = New-ScheduledTaskAction -Execute 'powershell' -Argument "G:\task.ps1"
Set-ScheduledTask -Trigger $trigger -Action $action -TaskPath "G:\" -TaskName "auto-compile"

&'C:\Program Files\Mozilla Firefox\firefox.exe'

#uninstall
$appsToRemove = "Microsoft.YourPhone","Microsoft.BingWeather","Microsoft.MicrosoftEdge.Stable","Microsoft.WindowsCamera","Microsoft.WindowsAlarms","Microsoft.People","Microsoft.Store","Microsoft.Office.OneNote","Microsoft.SkypeApp"
foreach ($app in $appsToRemove) {
    echo $app
    Get-AppxPackage $app -AllUsers | Remove-AppxPackage
}
$sevicesToStop = "TrkWks","DPS","iphlpsvc","WSearch","WerSvc","SEMgrSvc","DusmSvc","WinHttpAutoProxySvc","Wcmsvc","WbioSrvc","TabletInputService","SSDPSRV","SstpSvc","PimIndexMaintenanceSvc_5573c","DoSvc","DevicePickerUserSvc_5573c","DevicesFlowUserSvc_5573c","WdiServiceHost","WdiSystemHost","MSDTC","fdPHost","lfsvc","Power","NcbService","InstallService","DisplayEnhancementService","RmSvc","OneSyncSvc_5573c"
foreach ($service in $sevicesToStop) {
    echo $service
    Stop-Service -Name $service
    Set-Service -Name $service -StartupType Manual
}
$servicesToDisable = "SysMain","CertPropSvc","DiagTrack","LicenseManager","WpnService","RmSvc"
foreach ($service in $servicesToDisable) {
    echo $service
    Stop-Service -Name $service
    Set-Service -Name $service -StartupType Disabled 
}


#end

Read-Host -Prompt "Press Enter to exit"
