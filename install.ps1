# Pythia Package Automated Build and Install Script
# This script automates the uninstall-build-install cycle for Delphi packages

param(
    [switch]$SkipBuild,
    [switch]$NoRestart
)

$ErrorActionPreference = "Stop"

# Paths
$BplPath = "C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\pythia.bpl"
$DelphiExe = "d:\program files (x86)\embarcadero\studio\23.0\bin\bds.exe"
$ProjectFile = "d:\dev\delphi\pythia2\pythia.dproj"
$RegistryPath = "HKCU:\Software\Embarcadero\BDS\23.0\Known Packages"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Pythia Package Installer" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Step 1: Check if Delphi is running
Write-Host "[1/5] Checking for running Delphi instances..." -ForegroundColor Yellow
$delphiProcesses = Get-Process -Name "bds" -ErrorAction SilentlyContinue

if ($delphiProcesses) {
    Write-Host "  Delphi IDE is running. Closing it..." -ForegroundColor Yellow
    $delphiProcesses | Stop-Process -Force
    Start-Sleep -Seconds 2
    Write-Host "  Delphi IDE closed" -ForegroundColor Green
} else {
    Write-Host "  No Delphi instances running" -ForegroundColor Green
}

# Step 2: Uninstall existing package from registry
Write-Host "`n[2/5] Uninstalling existing package..." -ForegroundColor Yellow
try {
    if (Test-Path $RegistryPath) {
        $packages = Get-ItemProperty -Path $RegistryPath -ErrorAction SilentlyContinue
        $pythiaKeys = $packages.PSObject.Properties | Where-Object { $_.Value -like "*Pythia*" }
        
        foreach ($key in $pythiaKeys) {
            Remove-ItemProperty -Path $RegistryPath -Name $key.Name -ErrorAction SilentlyContinue
            Write-Host "  Removed package from registry" -ForegroundColor Green
        }
        
        if (-not $pythiaKeys) {
            Write-Host "  Package not found in registry (may be first install)" -ForegroundColor Gray
        }
    }
} catch {
    Write-Host "  Could not modify registry: $_" -ForegroundColor Yellow
}

# Step 3: Build the package
if (-not $SkipBuild) {
    Write-Host "`n[3/5] Building package..." -ForegroundColor Yellow
    
    if (Test-Path $BplPath) {
        Remove-Item $BplPath -Force -ErrorAction SilentlyContinue
    }
    
    Push-Location "d:\dev\delphi\pythia2"
    
    $buildProcess = Start-Process -FilePath "$DelphiExe" -ArgumentList "-ns","-b","$ProjectFile" -Wait -PassThru -NoNewWindow
    
    Pop-Location
    
    Start-Sleep -Seconds 1
    
    if ($buildProcess.ExitCode -eq 0 -and (Test-Path $BplPath)) {
        Write-Host "  Package built successfully" -ForegroundColor Green
        $bplInfo = Get-Item $BplPath
        $sizeMB = [math]::Round($bplInfo.Length / 1MB, 2)
        Write-Host "  Size: $sizeMB MB" -ForegroundColor Gray
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
            Write-Host "Error file not found. Build process may have failed to start." -ForegroundColor Red
        }
        Write-Host "========== End Compilation Errors ==========" -ForegroundColor Red
        Write-Host ""
        exit 1
    }
} else {
    Write-Host "`n[3/5] Skipping build (using existing BPL)..." -ForegroundColor Yellow
}

# Step 4: Install package in registry
Write-Host "`n[4/5] Installing package..." -ForegroundColor Yellow
try {
    if (Test-Path $BplPath) {
        # Add to Known Packages registry
        if (-not (Test-Path $RegistryPath)) {
            New-Item -Path $RegistryPath -Force | Out-Null
        }
        
        $packageDesc = "Pythia - AI Copilot Chat for Delphi IDE"
        New-ItemProperty -Path $RegistryPath -Name $BplPath -Value $packageDesc -PropertyType String -Force | Out-Null
        Write-Host "  Package registered in IDE" -ForegroundColor Green
    } else {
        Write-Host "  BPL file not found at: $BplPath" -ForegroundColor Red
        exit 1
    }
} catch {
    Write-Host "  Failed to register package: $_" -ForegroundColor Red
    exit 1
}

# Step 5: Restart Delphi
if (-not $NoRestart) {
    Write-Host "`n[5/5] Starting Delphi IDE..." -ForegroundColor Yellow
    Start-Process $DelphiExe
    Write-Host "  Delphi IDE launched" -ForegroundColor Green
} else {
    Write-Host "`n[5/5] Skipping IDE restart..." -ForegroundColor Yellow
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "Installation Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Access Pythia via:" -ForegroundColor White
Write-Host "  - Tools menu: Pythia AI Chat..." -ForegroundColor White
Write-Host "  - Keyboard: Ctrl+Alt+P" -ForegroundColor White
Write-Host ""
Write-Host "Next steps:" -ForegroundColor White
Write-Host "  1. Open Tools menu and select Pythia AI Chat (or press Ctrl+Alt+P)" -ForegroundColor White
Write-Host "  2. Sign in with GitHub Copilot (free with GitHub account)" -ForegroundColor White
Write-Host "  3. Or click Settings to enter OpenAI/Anthropic API key" -ForegroundColor White
Write-Host ""
Write-Host "Recommended: Use GitHub Copilot (no API key needed!)" -ForegroundColor Green
Write-Host ""
