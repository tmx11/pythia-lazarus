# GetIt Package Creation for Pythia

## What is GetIt?
GetIt is Embarcadero's package manager for Delphi/C++ Builder that allows one-click installation of libraries and IDE plugins through the IDE's **Tools > GetIt Package Manager**.

## Current Status
The automated installer scripts (`install.ps1`) provide a better developer experience than GetIt for local development. GetIt packages are primarily useful for:
- Public distribution to other developers
- Publishing to Embarcadero's official GetIt repository
- Enterprise-wide deployment

## Package Structure Created
- `getit/package.json` - GetIt package manifest

## To Distribute via GetIt

### 1. Create Release Package
```powershell
# Build the package
.\build.bat

# Create GetIt distribution zip
Compress-Archive -Path `
  "Win32\Debug\pythia.bpl", `
  "getit\package.json", `
  "README.md" `
  -DestinationPath "pythia-getit.zip"
```

### 2. Host the Package
Upload `pythia-getit.zip` to:
- GitHub Releases
- Your own web server
- Embarcadero's GetIt repository (requires approval)

### 3. Users Can Install Via
- **IDE**: Tools > GetIt Package Manager > Search "Pythia"
- **Command Line**: `GetItCmd.exe -install:Pythia.AIChat`

## For Now: Use install.ps1
The automated installation script is more practical for active development:
```powershell
# One command to rebuild and install
.\install.ps1

# Uninstall when needed
.\uninstall.ps1
```

This avoids the overhead of creating zip packages and hosting them for every code change.
