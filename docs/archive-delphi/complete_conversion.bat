@echo off
echo === Pythia Lazarus Conversion - Final Phase ===
echo.

cd Source

REM Test Context compilation
echo Testing Pythia.Context.pas...
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -Mdelphi -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -Fu. Pythia.Context.pas 2>&1 | findstr /C:"compiled" /C:"Error" /C:"Fatal"
if errorlevel 1 goto error

cd ..

REM Now build the full package
echo.
echo Building full package...
C:\lazarus\lazbuild.exe --skip-dependencies --build-mode=default pythia.lpk 2>&1

if errorlevel 1 (
  echo.
  echo Build FAILED - checking errors...
  goto error
) else (
  echo.
  echo === BUILD SUCCESSFUL! ===
  echo Package compiled: pythia.lpk
  echo Output: lib\x86_64-win64\
  dir /B lib\x86_64-win64\ 2>nul
  goto success
)

:error
echo.
echo Conversion incomplete - see errors above
exit /b 1

:success
echo.
echo Lazarus conversion complete!
exit /b 0
