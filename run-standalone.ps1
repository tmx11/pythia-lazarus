# Quick Test - Build and Run Standalone App

Write-Host "Compiling PythiaApp..." -ForegroundColor Cyan

$DelphiPath = "C:\Program Files (x86)\Embarcadero\Studio\23.0"
$DCC32 = "$DelphiPath\bin\dcc32.exe"

$env:BDS = $DelphiPath
& $DCC32 -B PythiaApp.dpr -USour ce -EWin32\Debug -NWin32\Debug

if ($LASTEXITCODE -eq 0) {
    Write-Host "Success! Launching..." -ForegroundColor Green
    Start-Process "Win32\Debug\PythiaApp.exe"
} else {
    Write-Host "Build failed. Open PythiaApp.dproj directly in IDE instead." -ForegroundColor Red
}
