# PYTHIA LAZARUS - AUTOMATED CLEAN INSTALL
# Fully automated build, package, and IDE rebuild

Write-Host "=== PYTHIA LAZARUS - CLEAN INSTALL ===" -ForegroundColor Cyan
Write-Host "Automated: Build → Package → IDE Rebuild → Launch" -ForegroundColor Gray
Write-Host ""

# Step 1: Close Lazarus
Write-Host "[1/5] Closing Lazarus IDE..." -ForegroundColor Yellow
$lazarusProcess = Get-Process -Name "lazarus" -ErrorAction SilentlyContinue
if ($lazarusProcess) {
    $lazarusProcess | Stop-Process -Force
    Write-Host "  Lazarus stopped" -ForegroundColor Green
} else {
    Write-Host "  No Lazarus instances running" -ForegroundColor Green
}
Start-Sleep -Seconds 2

# Step 2: Clean old package registry
Write-Host "`n[2/5] Cleaning package registry..." -ForegroundColor Yellow
$removed = 0
$paths = @(
    "C:\Users\$env:USERNAME\AppData\Local\lazarus\packagefiles.xml",
    "C:\Users\$env:USERNAME\AppData\Local\lazarus\packagelinks.xml",
    "C:\Users\$env:USERNAME\AppData\Local\lazarus\lib\pythia"
)
foreach ($path in $paths) {
    if (Test-Path $path) {
        Remove-Item $path -Recurse -Force -ErrorAction SilentlyContinue
        $removed++
    }
}
Write-Host "  Removed $removed old registry entries" -ForegroundColor Green

# Step 3: Clean build artifacts
Write-Host "`n[3/5] Cleaning build artifacts..." -ForegroundColor Yellow
if (Test-Path "lib") {
    Remove-Item "lib" -Recurse -Force
    Write-Host "  Cleaned lib/ directory" -ForegroundColor Green
} else {
    Write-Host "  No lib/ directory to clean" -ForegroundColor Green
}

# Step 4: Build package
Write-Host "`n[4/5] Building package..." -ForegroundColor Yellow
$buildOutput = C:\lazarus\lazbuild.exe --build-all pythia.lpk 2>&1
if ($LASTEXITCODE -eq 0) {
    $lines = ($buildOutput | Select-String "lines compiled").Line
    Write-Host "  $lines" -ForegroundColor Green
} else {
    Write-Host "  [ERROR] Package build failed!" -ForegroundColor Red
    Write-Host "  $buildOutput" -ForegroundColor Red
    exit 1
}

# Step 5: Rebuild IDE with package
Write-Host "`n[5/5] Rebuilding Lazarus IDE with Pythia..." -ForegroundColor Yellow
Write-Host "  This takes 1-2 minutes, please wait..." -ForegroundColor Gray
$rebuildOutput = C:\lazarus\lazbuild.exe --add-package pythia.lpk --build-ide= 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Host "  IDE rebuilt successfully!" -ForegroundColor Green
} else {
    Write-Host "  [ERROR] IDE rebuild failed!" -ForegroundColor Red
    Write-Host "  $rebuildOutput" -ForegroundColor Red
    exit 1
}

# Step 6: Launch Lazarus
Write-Host "`n[6/6] Starting Lazarus IDE..." -ForegroundColor Yellow
Start-Process "C:\lazarus\lazarus.exe"
Start-Sleep -Seconds 2
Write-Host "  Lazarus launched" -ForegroundColor Green

Write-Host ""
Write-Host "=== INSTALLATION COMPLETE ===" -ForegroundColor Green
Write-Host "Access Pythia: View → IDE Internals Windows → PythiaChatWindow" -ForegroundColor Cyan
Write-Host ""  
Write-Host "Troubleshooting:" -ForegroundColor Yellow
Write-Host "  - Check Messages panel in Lazarus for any warnings" -ForegroundColor Gray
Write-Host "  - Build log: .\lib\x86_64-win64\pythia.compiled" -ForegroundColor Gray
Write-Host "  - If menu missing: Tools → Configure IDE → Environment → IDE Internals" -ForegroundColor Gray
