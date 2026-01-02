Write-Host "========================================"
Write-Host "Building Pythia Standalone App (Silent)"
Write-Host "========================================"

Set-Location "d:\dev\delphi\pythia2"

# Clean up old logs
Remove-Item "build-app.log" -ErrorAction SilentlyContinue

Write-Host "Building PythiaApp.dproj..."
Write-Host ""

$BDS = "D:\Program Files (x86)\Embarcadero\Studio\23.0\bin\bds.exe"
if (!(Test-Path $BDS)) {
    Write-Host "ERROR: BDS not found at $BDS" -ForegroundColor Red
    exit 1
}

# Record existing BDS processes to avoid touching them
$ExistingBDS = @(Get-Process bds -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Id)
Write-Host "Existing BDS PIDs: $($ExistingBDS -join ', ')" -ForegroundColor Gray

# Kill any existing PythiaApp.exe to release the file
Get-Process PythiaApp -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue

$ExePath = "Win32\Debug\PythiaApp.exe"
$BeforeTime = $null
if (Test-Path $ExePath) {
    $BeforeTime = (Get-Item $ExePath).LastWriteTime
}

Write-Host "Starting build process..."

# Start NEW BDS process in background with output redirection
$Process = Start-Process -FilePath $BDS `
    -ArgumentList "-ns","-b","PythiaApp.dproj" `
    -PassThru `
    -RedirectStandardOutput "build-stdout.log" `
    -RedirectStandardError "build-stderr.log" `
    -WindowStyle Hidden
$OurPID = $Process.Id
Write-Host "Started BDS with PID: $OurPID" -ForegroundColor Cyan

# Wait for build to complete (check every 1 second, max 30 seconds)
$Counter = 0
$MaxWait = 30
$BuildSuccess = $false

Write-Host "Monitoring build progress..."
while ($Counter -lt $MaxWait) {
    Start-Sleep -Seconds 1
    $Counter += 1
    
    # Check if our BDS process exited
    if ($Process.HasExited) {
        Write-Host "BDS process exited" -ForegroundColor Yellow
        break
    }
    
    # Check if output file was updated
    if (Test-Path $ExePath) {
        $AfterTime = (Get-Item $ExePath).LastWriteTime
        if ($BeforeTime -eq $null -or $AfterTime -gt $BeforeTime) {
            Write-Host "Build completed in $Counter seconds" -ForegroundColor Green
            $BuildSuccess = $true
            break
        }
    }
    
    # Show progress every 5 seconds
    if ($Counter % 5 -eq 0) {
        Write-Host "  Waiting... ($Counter seconds)" -ForegroundColor Gray
    }
}

# Kill ONLY our BDS process, never touch existing ones
if (!$Process.HasExited) {
    Write-Host "Closing our BDS (PID: $OurPID)..."
    Stop-Process -Id $OurPID -Force -ErrorAction SilentlyContinue
    Start-Sleep -Seconds 2  # Wait for files to be released
}

# Verify we didn't kill existing BDS
foreach ($ExistingPID in $ExistingBDS) {
    if (Get-Process -Id $ExistingPID -ErrorAction SilentlyContinue) {
        Write-Host "OK Existing BDS PID $ExistingPID still running" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "========== Build Output =========="
if (Test-Path "build-stdout.log") {
    Get-Content "build-stdout.log"
}
if (Test-Path "build-stderr.log") {
    $stderr = Get-Content "build-stderr.log"
    if ($stderr) {
        Write-Host ""
        Write-Host "========== Errors/Warnings =========="
        $stderr
    }
}
if (Test-Path "PythiaApp.err") {
    Write-Host ""
    Write-Host "========== Compilation Errors (PythiaApp.err) =========="
    Get-Content "PythiaApp.err"
}
Write-Host "========== End Build Output =========="
Write-Host ""

if ($BuildSuccess -and (Test-Path $ExePath)) {
    Write-Host "SUCCESS! Application built successfully" -ForegroundColor Green
    Get-Item $ExePath | Format-Table Name, Length, LastWriteTime -AutoSize
    Write-Host ""
    Write-Host "Starting PythiaApp.exe..."
    Start-Process -FilePath $ExePath
    exit 0
} else {
    if (!$BuildSuccess) {
        Write-Host "ERROR: Build timeout after $MaxWait seconds" -ForegroundColor Red
    } else {
        Write-Host "ERROR: PythiaApp.exe not found!" -ForegroundColor Red
    }
    exit 1
}
