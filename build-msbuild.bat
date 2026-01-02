@echo off
echo ========================================
echo Building Pythia Standalone App with MSBuild
echo ========================================
cd /d "d:\dev\delphi\pythia2"

REM Kill any running instance to unlock the exe
taskkill /F /IM PythiaApp.exe >nul 2>&1
timeout /t 1 /nobreak >nul

del build-app.log 2>nul

REM Set up RAD Studio environment (required for MSBuild to find Delphi compiler)
call "d:\program files (x86)\embarcadero\studio\23.0\bin\rsvars.bat" >nul 2>&1

echo Building PythiaApp.dproj with MSBuild...
echo.

REM MSBuild works in Community Edition per Reddit
msbuild PythiaApp.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /verbosity:minimal /nologo > build-app.log 2>&1

set BUILD_EXIT=%ERRORLEVEL%
echo Exit code: %BUILD_EXIT%
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
    if "%1"=="--run" (
        echo Starting PythiaApp.exe...
        start "" "%EXE_PATH%"
    )
) else (
    echo ERROR: PythiaApp.exe not found!
    echo Build failed - check log above
    exit /b 1
)
