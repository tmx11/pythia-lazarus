@echo off
echo ========================================
echo Building with DCC32 (Command Line Compiler)
echo ========================================
cd /d "d:\dev\delphi\pythia2"

del dcc32-build.log 2>nul

set DCC32="d:\program files (x86)\embarcadero\studio\23.0\bin\dcc32.exe"

echo Compiling PythiaApp.dpr...
echo.

%DCC32% -B PythiaApp.dpr -USSource -EWin32\Debug -NWin32\Debug > dcc32-build.log 2>&1

echo Exit code: %ERRORLEVEL%
echo.

type dcc32-build.log

if exist Win32\Debug\PythiaApp.exe (
    echo.
    echo SUCCESS! PythiaApp.exe built
    dir Win32\Debug\PythiaApp.exe
) else (
    echo.
    echo BUILD FAILED - see errors above
    exit /b 1
)
