# Pythia Package Uninstaller
# Removes the package from Delphi IDE

$ErrorActionPreference = "Stop"

$BplPath = "C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\pythia.bpl"
$RegistryPath = "HKCU:\Software\Embarcadero\BDS\23.0\Known Packages"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Pythia Package Uninstaller" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Close Delphi
Write-Host "Checking for running Delphi instances..." -ForegroundColor Yellow
$delphiProcesses = Get-Process -Name "bds" -ErrorAction SilentlyContinue

if ($delphiProcesses) {
    Write-Host "  Closing Delphi IDE..." -ForegroundColor Yellow
    $delphiProcesses | Stop-Process -Force
    Start-Sleep -Seconds 2
    Write-Host "  ✓ Delphi IDE closed" -ForegroundColor Green
}

# Remove from registry
Write-Host "Removing package from registry..." -ForegroundColor Yellow
try {
    if (Test-Path $RegistryPath) {
        $packages = Get-ItemProperty -Path $RegistryPath -ErrorAction SilentlyContinue
        $pythiaKey = $packages.PSObject.Properties | Where-Object { $_.Value -like "*Pythia*" }
        
        if ($pythiaKey) {
            Remove-ItemProperty -Path $RegistryPath -Name $pythiaKey.Name -ErrorAction SilentlyContinue
            Write-Host "  ✓ Removed from registry" -ForegroundColor Green
        } else {
            Write-Host "  ℹ Package not found in registry" -ForegroundColor Gray
        }
    }
} catch {
    Write-Host "  ⚠ Could not modify registry: $_" -ForegroundColor Yellow
}

# Delete BPL file
Write-Host "Removing BPL file..." -ForegroundColor Yellow
if (Test-Path $BplPath) {
    Remove-Item $BplPath -Force -ErrorAction SilentlyContinue
    Write-Host "  ✓ BPL file deleted" -ForegroundColor Green
} else {
    Write-Host "  ℹ BPL file not found" -ForegroundColor Gray
}

Write-Host "`n✓ Uninstallation complete!" -ForegroundColor Green
