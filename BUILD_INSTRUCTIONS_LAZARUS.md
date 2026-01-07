# Build Instructions - Pythia for Lazarus IDE

## Quick Start (Recommended)

**One-command automated installation:**
```powershell
.\CLEAN_INSTALL.ps1
```

This script performs a **complete automated installation**:
1. Stops Lazarus IDE (if running)
2. Cleans old package registrations and build artifacts
3. Builds the package: `lazbuild --build-all pythia.lpk`
4. Rebuilds IDE with package: `lazbuild --add-package pythia.lpk --build-ide=`
5. Launches Lazarus with Pythia installed

**No manual steps required!**

---

## Manual Build Instructions

If you prefer to build manually or need to troubleshoot:

### Prerequisites
- Lazarus IDE 3.0+ (tested on 3.2)
- Free Pascal Compiler (FPC) 3.2.2+
- Windows (currently supported platform)

### Step 1: Build the Package
```powershell
C:\lazarus\lazbuild.exe --build-all pythia.lpk
```

Check for compilation success:
```powershell
# Should show "X lines compiled" with no errors
C:\lazarus\lazbuild.exe --build-all pythia.lpk 2>&1 | Select-String "Error|lines compiled"
```

### Step 2: Rebuild IDE with Package
```powershell
C:\lazarus\lazbuild.exe --add-package pythia.lpk --build-ide=
```

**Note**: This takes 1-2 minutes as it rebuilds the entire Lazarus IDE.

### Step 3: Launch Lazarus
```powershell
C:\lazarus\lazarus.exe
```

### Step 4: Verify Installation
1. Open Lazarus IDE
2. Go to **View → IDE Internals Windows → PythiaChatWindow**
3. Chat window should open

---

## Configuration

### GitHub Copilot (Recommended - FREE)
1. Open Pythia chat window
2. Click **Settings** button
3. Click **Sign in with GitHub**
4. Enter device code at https://github.com/login/device
5. Ready to use - no API keys needed!

### API Keys (Optional)
If using OpenAI or Anthropic directly:
- OpenAI: https://platform.openai.com/api-keys
- Anthropic: https://console.anthropic.com/settings/keys

Configuration stored in: `%APPDATA%\Pythia\pythia.ini`

---

## Troubleshooting

### Package Build Fails
```powershell
# View full build output
C:\lazarus\lazbuild.exe --build-all pythia.lpk
```

Common issues:
- **Missing units**: Ensure Lazarus installed with all packages
- **OpenSSL errors**: Install OpenSSL (see OPENSSL_SETUP.md)
- **Permission denied**: Run PowerShell as Administrator

### IDE Rebuild Fails
```powershell
# Check IDE rebuild output
C:\lazarus\lazbuild.exe --add-package pythia.lpk --build-ide= 2>&1 | Select-String "Error"
```

If rebuild hangs:
1. Kill Lazarus process: `taskkill /F /IM lazarus.exe`
2. Try again with clean install: `.\CLEAN_INSTALL.ps1`

### Chat Window Not Appearing
1. Check **View → IDE Internals Windows** menu
2. If missing, check Messages panel for package load errors
3. Try reinstalling: `.\CLEAN_INSTALL.ps1`

### OpenSSL Missing DLLs
Download OpenSSL for Windows:
- https://slproweb.com/products/Win32OpenSSL.html
- Install "Win64 OpenSSL v3.x.x Light"
- Or copy DLLs to `C:\Windows\System32\`

---

## Project Structure

```
pythia-lazarus/
├── Source/                  # Pascal source files
│   ├── Pythia.Register.pas  # IDE plugin registration
│   ├── Pythia.ChatForm.pas  # Main chat UI
│   ├── Pythia.AI.Client.pas # API client
│   ├── Pythia.Config.pas    # Configuration
│   └── ...
├── pythia.lpk               # Lazarus package file
├── PythiaApp.dpr            # Standalone app (for testing)
├── lib/                     # Build output
└── CLEAN_INSTALL.ps1        # Automated installer

**Note**: Both `pythia.lpk` (IDE plugin) and `PythiaApp.dpr` (standalone) 
share the same Source/*.pas files - changes work in both!
```

---

## Development Workflow

### Quick Testing Without IDE Reinstall
1. Open **PythiaApp.dpr** in Lazarus
2. Press **F9** to compile and run
3. Test changes in standalone window
4. When ready, run `CLEAN_INSTALL.ps1` to update IDE plugin

### Incremental Package Build
```powershell
# Build package only (no IDE rebuild)
C:\lazarus\lazbuild.exe pythia.lpk

# Check for errors/warnings
C:\lazarus\lazbuild.exe pythia.lpk 2>&1 | Select-String "Error|Warning"
```

---

## Command-Line Reference

```powershell
# Clean build
C:\lazarus\lazbuild.exe --build-all pythia.lpk

# Quick rebuild (only changed files)
C:\lazarus\lazbuild.exe pythia.lpk

# Add package and rebuild IDE
C:\lazarus\lazbuild.exe --add-package pythia.lpk --build-ide=

# Check build mode
C:\lazarus\lazbuild.exe --build-mode=Debug pythia.lpk

# Verbose output
C:\lazarus\lazbuild.exe --verbose pythia.lpk
```

---

## Additional Resources

- **README.md** - Feature overview and usage
- **AGENTS.md** - Developer workflow and guidelines
- **LAZARUS_BUILD_SUCCESS.md** - Detailed port information
- **QUICKSTART.md** - Quick reference guide

For issues, check: https://github.com/tmx11/pythia-lazarus/issues
