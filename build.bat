@echo off
echo ========================================
echo Building Pythia Package with Delphi IDE
echo ========================================
cd /d "d:\dev\delphi\pythia2"

del build.log 2>nul

echo Building project...
"d:\program files (x86)\embarcadero\studio\23.0\bin\bds.exe" -ns -b "pythia.dproj" -obuild.log

timeout /t 3 >nul

echo.
echo Exit code: %ERRORLEVEL%
echo.

if exist build.log (
    echo ========== Build Log ==========
    type build.log
    echo ========== End Log ==========
    echo.
)

set BPL_PATH=C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\pythia.bpl
if exist "%BPL_PATH%" (
    echo SUCCESS! Package built successfully
    dir "%BPL_PATH%"
    echo.
    echo ========================================
    echo Installation Instructions:
    echo ========================================
    echo 1. Open Delphi IDE
    echo 2. Component ^> Install Packages
    echo 3. Add ^> Browse to: %BPL_PATH%
    echo 4. Click OK and restart IDE
    echo 5. Access via Tools ^> Pythia AI Chat (Ctrl+Shift+P)
    echo.
    echo Package location: %BPL_PATH%
) else (
    echo ERROR: pythia.bpl not found!
    echo Expected location: %BPL_PATH%
    echo Check build log above for errors
    exit /b 1
)
