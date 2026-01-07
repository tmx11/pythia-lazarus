@echo off
echo Testing individual unit compilation...
cd Source

echo.
echo === Testing Pythia.Config.pas ===
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -Mdelphi -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 Pythia.Config.pas 2>&1 | findstr /C:"Error" /C:"Fatal" /C:"compiled"

cd ..
pause
