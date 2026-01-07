# Clean Install Instructions

## Current Situation

You have:
- Old Pythia installations with duplicate menu items
- Menu shortcut not working (Ctrl+Alt+P instead of Ctrl+Shift+P)
- Showing "No IDE context" message
- Package registered but possibly stale version

## Solution: Clean Install

### Step 1: Close Delphi
Make sure Delphi IDE is completely closed.

### Step 2: Run Clean Install
```powershell
.\clean-install.ps1
```

This script will:
1. **Kill any running Delphi processes**
2. **Remove ALL Pythia registry entries** (cleans duplicates)
3. **Delete old BPL files** from both project and system locations
4. **Clean build** the package from scratch
5. **Install fresh** to the IDE
6. **Launch Delphi** with clean installation

### Step 3: Verify Installation
After Delphi opens:
1. Check **Tools** menu - should see **ONE** "Pythia AI Chat..." entry
2. Press **Ctrl+Shift+P** - should open Pythia window
3. Open a .pas file in the editor
4. Open Pythia chat window
5. Check context panel - should show file information

### Debugging

If you still see issues:

**Check Debug Log:**
```powershell
notepad $env:TEMP\pythia_debug.log
```

This shows:
- Which menus were found
- Whether duplicate detection worked
- Menu item registration details

**Check Package Location:**
```powershell
Get-Item "Win32\Debug\pythia.bpl" | Select-Object FullName, LastWriteTime, Length
Get-Item "C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\pythia.bpl" | Select-Object FullName, LastWriteTime, Length
```

Both should exist and have matching timestamps.

**Check Registry:**
```powershell
Get-ItemProperty "HKCU:\Software\Embarcadero\BDS\23.0\Known Packages" | Select-Object *pythia*
```

Should show ONE entry for `C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\pythia.bpl`.

## Manual Installation (Alternative)

If you prefer to add the package manually:

1. Run clean build only:
   ```powershell
   .\clean-install.ps1 -NoRestart
   ```

2. In Delphi IDE:
   - Go to **Component > Install Packages**
   - Click **Add...**
   - Browse to `d:\dev\delphi\pythia2\Win32\Debug\pythia.bpl`
   - Click **OK**
   - Restart Delphi

## Key Changes Made

1. **Shortcut**: Changed from Ctrl+Alt+P to **Ctrl+Shift+P** (matches docs)
2. **Duplicate Detection**: Now checks Tools menu for existing "Pythia" items
3. **Build Output**: Package builds to `Win32\Debug\pythia.bpl` in project directory
4. **Error Reporting**: install.ps1 now shows compilation errors on failure
5. **Clean Install Script**: New script that completely removes old installations

## Context Provider Issue

The "No IDE context" message suggests the IDE context provider isn't detecting open files. After clean install, if this persists:

1. Check the debug log to see which provider is being used
2. Open a .pas file and click "Refresh Context"
3. Check if `BorlandIDEServices` is available at runtime
4. May need to add more diagnostics to `TIDEContextProvider.IsAvailable`
