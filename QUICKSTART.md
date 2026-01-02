# Pythia Development - Quick Reference

## Dual Build System

### 🚀 Standalone App (Fast Testing)

**In Delphi IDE:**
1. File > Close All (close any open projects)
2. File > Open Project > `PythiaApp.dproj`
3. Press F9 to run

**If you get "host application required" error:**
- Project > Options > Application: Target type = **Application**
- Project > Options > Debugger: Host Application = `$(OutputPath)\$(ProjectName).exe`

**Output:** `Win32\Debug\PythiaApp.exe`

### 📦 IDE Plugin (Final Deployment)
```powershell
# Build package
pythia.dproj (in Delphi IDE: Shift+F9)

# Install
.\install.ps1

# Restart Delphi IDE
```

**Key**: Both projects use **same Source/*.pas files** - edit once, works everywhere!

## Current Sprint (Phase 0: GitHub Copilot)

**Completed** ✅:
- `pythia2-q1v`: GitHub OAuth device flow (Pythia.GitHub.Auth.pas)

**Ready to Start**:
1. `pythia2-r8s` (P1): Update AI.Client for Copilot API endpoint
2. `pythia2-h5k` (P2): Settings form GitHub signin button
3. `pythia2-4u0` (P2): ComboBox with Copilot models

## Beads Workflow

```bash
bd ready                  # Show available work
bd update <id> --status in_progress
bd close <id> -r "Done"   # Mark complete
bd sync                   # Sync with git
```

## File Structure
```
Source/
  Pythia.ChatForm.pas         - Main UI
  Pythia.AI.Client.pas        - API client (OpenAI, Anthropic, + GitHub Copilot)
  Pythia.Config.pas           - INI file config
  Pythia.GitHub.Auth.pas      - OAuth device flow ✨ NEW
  Pythia.SettingsForm.pas     - Settings dialog
  Pythia.Register.pas         - IDE integration

PythiaApp.dpr/.dproj          - Standalone app project
pythia.dpk/.dproj             - IDE package project
```

## Next Actions
1. Update AI.Client to call `api.githubcopilot.com/chat/completions`
2. Add GitHub auth UI to SettingsForm
3. Test in standalone app
4. Install as plugin once verified
