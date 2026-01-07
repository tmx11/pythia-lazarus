# Agent Instructions

This project uses **bd** (beads) for issue tracking. Issues stored in `.beads/issues.jsonl`.

## Build & Install Workflow

**ALWAYS use this automated script for package installation:**
```powershell
.\CLEAN_INSTALL.ps1
```

This script:
1. Stops Lazarus IDE
2. Removes old package registrations from AppData
3. Cleans `lib/` build artifacts
4. Compiles package: `lazbuild --build-all pythia.lpk`
5. Rebuilds IDE: `lazbuild --add-package pythia.lpk --build-ide=`
6. Starts Lazarus IDE

**NEVER** use manual Component > Install Packages - CLEAN_INSTALL.ps1 is fully automated.

## Beads Quick Reference

```bash
# Check .beads/issues.jsonl for status directly
cat .beads/issues.jsonl | grep '"status":"open"'

# Manual issue management
# Create: Add line to .beads/issues.jsonl
# Update: Edit status field in JSON line
# Close: Set status="closed", add closed_at timestamp
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

