# Agent Instructions

This project uses **bd** (beads) for issue tracking. Issues stored in `.beads/issues.jsonl`.

## Project Context

**Platform**: Lazarus IDE (Free Pascal) - Successfully ported from Delphi 12
**Status**: Functional IDE plugin with AI chat integration
**Current Branch**: `feature/synedit-chat-renderer`
**Build System**: `lazbuild` (Lazarus command-line compiler)

## Build & Install Workflow

**ALWAYS use this fully automated script:**
```powershell
.\CLEAN_INSTALL.ps1
```

This script performs a **complete automated installation**:
1. Stops Lazarus IDE (if running)
2. Removes old package registrations from AppData
3. Cleans `lib/` build artifacts
4. Builds package: `lazbuild --build-all pythia.lpk`
5. **Automatically rebuilds IDE**: `lazbuild --add-package pythia.lpk --build-ide=`
6. Starts Lazarus IDE with Pythia installed

**No manual steps required** - script handles everything including IDE rebuild.

**Access Pythia**: View → IDE Internals Windows → PythiaChatWindow

## Key Implementation Notes

**Architecture**:
- **Pythia.Register.pas**: IDE integration via `IDEWindowIntf` (not ToolsAPI)
- **Pythia.ChatForm.pas**: Main UI using LCL (TSynEdit for chat display)
- **Pythia.AI.Client.pas**: API client using `fphttpclient` (not THttpClient)
- **Pythia.Config.pas**: INI file config in `%APPDATA%\Pythia\`
- **Pythia.Context.pas**: IDE context extraction via `SourceEditorIntf`

**Recent Features**:
- ✅ Terminal pane for command execution
- ✅ Markdown rendering with toggle
- ✅ Git branch and conversation stats
- ✅ Word wrap and visual message markers
- ✅ IDE context (file, selection, cursor position)

## Beads Quick Reference

```powershell
# View open issues (PowerShell)
Get-Content .beads\issues.jsonl | ConvertFrom-Json | Where-Object { $_.status -eq 'open' } | Select-Object id, title, priority

# Check specific issue details
Get-Content .beads\issues.jsonl | ConvertFrom-Json | Where-Object { $_.id -eq 'pythia-lazarus-xxx' }

# Close issue (update JSON line, set status="closed", add closed_at timestamp)
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

