# Pythia Clean Install Script
# Completely removes old installations and installs fresh

param(
    [switch]$NoRestart
)

$ErrorActionPreference = "Stop"

# Paths
$ProjectDir = "d:\dev\delphi\pythia2"
$SystemBplDir = "C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl"
$SystemBpl = "$SystemBplDir\pythia.bpl"
$ProjectBpl = "$ProjectDir\Win32\Debug\pythia.bpl"  # Desired location (not used by compiler yet)
$DelphiExe = "d:\program files (x86)\embarcadero\studio\23.0\bin\bds.exe"
$ProjectFile = "$ProjectDir\pythia.dproj"
$RegistryPath = "HKCU:\Software\Embarcadero\BDS\23.0\Known Packages"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Pythia Clean Install" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Step 1: Kill Delphi
Write-Host "[1/6] Stopping Delphi IDE..." -ForegroundColor Yellow
$delphiProcesses = Get-Process -Name "bds" -ErrorAction SilentlyContinue
if ($delphiProcesses) {
    $delphiProcesses | Stop-Process -Force
    Start-Sleep -Seconds 3
    Write-Host "  Delphi IDE stopped" -ForegroundColor Green
} else {
    Write-Host "  No Delphi instances running" -ForegroundColor Green
}

# Step 2: Remove ALL registry entries
Write-Host "`n[2/6] Cleaning registry..." -ForegroundColor Yellow
if (Test-Path $RegistryPath) {
    $props = Get-ItemProperty $RegistryPath
    $removed = 0
    foreach ($prop in $props.PSObject.Properties) {
        if ($prop.Name -notlike "PS*" -and $prop.Value -like "*pythia*") {
            Remove-ItemProperty -Path $RegistryPath -Name $prop.Name -ErrorAction SilentlyContinue
            Write-Host "  Removed: $($prop.Name)" -ForegroundColor Gray
            $removed++
        }
    }
    if ($removed -eq 0) {
        Write-Host "  No registry entries found" -ForegroundColor Green
    } else {
        Write-Host "  Removed $removed registry entries" -ForegroundColor Green
    }
} else {
    Write-Host "  Registry path not found (clean)" -ForegroundColor Green
}

# Step 3: Delete old BPL files
Write-Host "`n[3/6] Removing old BPL files..." -ForegroundColor Yellow
$removed = 0
if (Test-Path $SystemBpl) {
    Remove-Item $SystemBpl -Force
    Write-Host "  Removed: $SystemBpl" -ForegroundColor Gray
    $removed++
}
if (Test-Path $ProjectBpl) {
    Remove-Item $ProjectBpl -Force
    Write-Host "  Removed: $ProjectBpl" -ForegroundColor Gray
    $removed++
}
if ($removed -eq 0) {
    Write-Host "  No old BPL files found" -ForegroundColor Green
} else {
    Write-Host "  Removed $removed BPL files" -ForegroundColor Green
}

# Step 4: Clean build
Write-Host "`n[4/6] Building package (clean)..." -ForegroundColor Yellow
Push-Location $ProjectDir
$buildProcess = Start-Process -FilePath "$DelphiExe" -ArgumentList "-ns","-b","$ProjectFile" -Wait -PassThru -NoNewWindow
Pop-Location
Start-Sleep -Seconds 1

# Compiler outputs to system BPL directory by default
if ($buildProcess.ExitCode -eq 0 -and (Test-Path $SystemBpl)) {
    Write-Host "  Build successful" -ForegroundColor Green
    $bplInfo = Get-Item $SystemBpl
    $sizeMB = [math]::Round($bplInfo.Length / 1MB, 2)
    Write-Host "  Size: $sizeMB MB" -ForegroundColor Gray
    Write-Host "  Location: $SystemBpl" -ForegroundColor Gray
} else {
    Write-Host "  Build failed!" -ForegroundColor Red
    Write-Host ""
    Write-Host "========== Compilation Errors (pythia.err) ==========" -ForegroundColor Red
    if (Test-Path "pythia.err") {
        Get-Content "pythia.err" | Select-String -Pattern "Error|Fatal" -Context 2,2
        Write-Host ""
        Write-Host "Full error log:" -ForegroundColor Yellow
        Get-Content "pythia.err"
    } else {
        Write-Host "Error file not found." -ForegroundColor Red
    }
    Write-Host "========== End Compilation Errors ==========" -ForegroundColor Red
    exit 1
}

# Step 5: Register in IDE (no copy needed - already in system location)
Write-Host "`n[5/6] Registering in IDE..." -ForegroundColor Yellow
$packageName = [System.IO.Path]::GetFileName($SystemBpl)
$description = "Pythia - AI Chat Assistant for Delphi"
New-ItemProperty -Path $RegistryPath -Name $SystemBpl -Value $description -PropertyType String -Force | Out-Null
Write-Host "  Registered in IDE" -ForegroundColor Green

# Step 6: Launch Delphi with project
if (-not $NoRestart) {
    Write-Host "`n[6/6] Starting Delphi IDE..." -ForegroundColor Yellow
    Start-Process $DelphiExe -ArgumentList "$ProjectDir\pythia.dproj"
    Start-Sleep -Seconds 2
    Write-Host "  Delphi IDE launched with pythia.dproj" -ForegroundColor Green
    Write-Host "  Opening source file..." -ForegroundColor Gray
    # The IDE will auto-open the project and last opened files
} else {
    Write-Host "`n[6/6] Skipping IDE restart" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Clean Install Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Package Location:" -ForegroundColor Yellow
Write-Host "  $SystemBpl" -ForegroundColor Gray
Write-Host ""
Write-Host "Access Pythia via:" -ForegroundColor Yellow
Write-Host "  - Tools > Pythia AI Chat..." -ForegroundColor Gray
Write-Host "  - Keyboard: Ctrl+Shift+P" -ForegroundColor Gray
Write-Host ""
Write-Host "Debug Log:" -ForegroundColor Yellow
Write-Host "  " -NoNewline
Write-Host "$env:TEMP\pythia_debug.log" -ForegroundColor Gray
Write-Host ""
Write-Host "If you see duplicate menu items:" -ForegroundColor Yellow
Write-Host "  1. Close Delphi" -ForegroundColor Gray
Write-Host "  2. Run: .\clean-install.ps1" -ForegroundColor Gray
Write-Host "  3. Delphi will reopen cleanly" -ForegroundColor Gray
