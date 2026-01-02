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

# Step 5: Start Lazarus
Write-Host ""
Write-Host "Step 5: Starting Lazarus with clean state..." -ForegroundColor Yellow
Start-Process "C:\lazarus\lazarus.exe"

Write-Host ""
Write-Host "=== MANUAL STEPS ===" -ForegroundColor Cyan
Write-Host "1. Wait for Lazarus to fully start"
Write-Host "2. Package -> Open Package File"
Write-Host "3. Browse to: $PWD\pythia.lpk"
Write-Host "4. Click 'Compile' button (check for errors in Messages)"
Write-Host "5. If no errors, click 'Use' then 'Install'"
Write-Host "6. Wait for IDE to rebuild (1-2 minutes)"
Write-Host "7. After restart, go to: View -> IDE Internals Windows -> PythiaChatWindow"
Write-Host ""
Write-Host "If access violation still occurs, the problem is in ChatForm.pas initialization." -ForegroundColor Yellow
