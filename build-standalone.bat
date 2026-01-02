@echo off
echo ========================================
echo Building Pythia Standalone App (Silent)
echo ========================================
cd /d "d:\dev\delphi\pythia2"

del build-app.log 2>nul

echo Building PythiaApp.dproj...
echo.

REM Use bds.exe with silent flags - it will still briefly show/hide but no manual clicks needed
REM Flags: -ns = no splash, -np = no component palette
set BDS="D:\Program Files (x86)\Embarcadero\Studio\23.0\bin\bds.exe"
if not exist %BDS% (
    echo ERROR: BDS not found at %BDS%
    exit /b 1
)

REM Redirect to log, no interactive dialogs
%BDS% -ns -np -b "PythiaApp.dproj" > build-app.log 2>&1

echo Exit code: %ERRORLEVEL%
echo.

if exist build-app.log (
    echo ========== Build Log ==========
    type build-app.log
    echo ========== End Log ==========
    echo.
)

set EXE_PATH=Win32\Debug\PythiaApp.exe
if exist "%EXE_PATH%" (
    echo SUCCESS! Application built successfully
    dir "%EXE_PATH%"
    echo.
    echo Starting PythiaApp.exe...
    start "" "%EXE_PATH%"
) else (
    echo ERROR: PythiaApp.exe not found!
    echo Expected location: %EXE_PATH%
    echo Check build log above for errors
    exit /b 1
)
