# Clean Install Script for Pythia Lazarus Package

Write-Host "=== PYTHIA LAZARUS - CLEAN INSTALL ===" -ForegroundColor Cyan
Write-Host ""

# Step 1: Close Lazarus
Write-Host "Step 1: Closing Lazarus..." -ForegroundColor Yellow
taskkill /F /IM lazarus.exe 2>&1 | Out-Null
Start-Sleep -Seconds 2

# Step 2: Clean old package files
Write-Host "Step 2: Removing old package registry..." -ForegroundColor Yellow
Remove-Item "C:\Users\$env:USERNAME\AppData\Local\lazarus\packagefiles.xml" -Force -ErrorAction SilentlyContinue
Remove-Item "C:\Users\$env:USERNAME\AppData\Local\lazarus\packagelinks.xml" -Force -ErrorAction SilentlyContinue
Remove-Item "C:\Users\$env:USERNAME\AppData\Local\lazarus\lib\pythia" -Recurse -Force -ErrorAction SilentlyContinue

# Step 3: Clean build artifacts
Write-Host "Step 3: Cleaning build artifacts..." -ForegroundColor Yellow
Remove-Item "lib" -Recurse -Force -ErrorAction SilentlyContinue

# Step 4: Rebuild package
Write-Host "Step 4: Rebuilding package..." -ForegroundColor Yellow
C:\lazarus\lazbuild.exe --build-all pythia.lpk 2>&1 | Out-Null
if ($LASTEXITCODE -eq 0) {
    Write-Host "[OK] Package compiled successfully" -ForegroundColor Green
} else {
    Write-Host "[ERROR] Package compilation failed!" -ForegroundColor Red
    exit 1
}

# Step 5: Install package and rebuild IDE
Write-Host "Step 5: Installing package into IDE and rebuilding..." -ForegroundColor Yellow
Write-Host "This will take 1-2 minutes, please wait..."
C:\lazarus\lazbuild.exe --add-package "$PWD\pythia.lpk" --build-ide= 2>&1 | Out-Null
if ($LASTEXITCODE -eq 0) {
    Write-Host "[OK] IDE rebuilt with package installed" -ForegroundColor Green
} else {
    Write-Host "[ERROR] IDE rebuild failed - check errors above" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "=== INSTALLATION COMPLETE ===" -ForegroundColor Green
Write-Host "The package has been installed and IDE rebuilt automatically."
Write-Host "After Lazarus starts, access via: View -> IDE Internals Windows -> Pythia AI Chat"
Write-Host ""
