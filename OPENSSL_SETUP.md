# OpenSSL DLLs Required

Pythia requires OpenSSL 1.1.1 DLLs to make HTTPS API calls.

## Option 1: Download Pre-built DLLs (Recommended)

Download from the official Indy Sockets repository:
https://github.com/IndySockets/OpenSSL-Binaries/tree/master/OpenSSL1.1.1w/Win64

Required files:
- `libcrypto-1_1-x64.dll` (2.7 MB)
- `libssl-1_1-x64.dll` (686 KB)

Place these DLLs in:
- Same directory as lazarus.exe: `C:\lazarus\`
- OR in the package directory: `D:\dev\delphi\pythia-lazarus\`

## Option 2: Install OpenSSL for Windows

Download installer from: https://slproweb.com/products/Win32OpenSSL.html
- Choose "Win64 OpenSSL v1.1.1" (Light version is sufficient)
- Install to default location
- DLLs will be in `C:\Program Files\OpenSSL-Win64\bin\`

## Verification

After installing, open Lazarus IDE and test the Pythia chat window with an API message.
The error "Could not initialize OpenSSL library" should disappear.
