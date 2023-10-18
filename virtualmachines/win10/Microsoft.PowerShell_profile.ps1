Import-Module posh-git
Import-Module Terminal-Icons
Import-Module devtoolbox
Import-Module PSReadLine
Import-Module Appx
Invoke-Expression (& starship init powershell)
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Windows
Set-PSReadLineOption -EditMode
