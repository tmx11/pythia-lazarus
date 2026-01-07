# Terminal Threading Testing Guide

## Implementation Status

✅ **COMPLETE** - Threading implementation successfully compiled and installed

## What Was Implemented

### New Unit: Pythia.Tools.Terminal.Async.pas

**TTerminalOutputQueue**: Thread-safe output buffer
- Uses `SyncObjs.TCriticalSection` for synchronization
- `Add()`: Thread posts output lines
- `ExtractAll()`: Main thread retrieves accumulated output

**TTerminalThread**: Background execution thread
- Extends `TThread` with `FreeOnTerminate := True`
- Runs PowerShell via `TProcess` in `Execute()` method
- Uses `Synchronize(@NotifyComplete)` for completion callback
- 30-second timeout for command execution

**TTerminalExecutor**: Coordinator class
- `TTimer` polls queue every 100ms
- `ExecuteAsync()`: Starts background thread
- `Cancel()`: Terminates thread if still running
- Callbacks: `OnOutput`, `OnComplete`

### Modified: Pythia.Tools.Terminal.pas

**ExecuteCommandSync**: Synchronous wrapper
- Calls `FExecutor.ExecuteAsync(Command)`
- Waits with `Application.ProcessMessages` loop
- 35-second timeout (5 seconds more than thread)
- Keeps UI responsive during execution

**Callbacks**:
- `OnOutputReceived`: Accumulates text as it arrives
- `OnCommandComplete`: Sets final output + exit code

## Testing Checklist

### Phase 1: Basic Functionality ✅

1. **Open Pythia Chat**
   - View → IDE Internals Windows → PythiaChatWindow
   - Verify window opens without errors

2. **Simple Command**
   ```
   Can you run: Get-Location
   ```
   - Should show current directory
   - IDE should remain responsive

3. **Quick Command**
   ```
   Run this: Get-ChildItem -Path . | Select-Object -First 5
   ```
   - Should list first 5 files
   - Output should appear quickly

### Phase 2: Long-Running Commands ⏳

4. **10-Second Sleep Test**
   ```
   Execute this command: Start-Sleep -Seconds 10; Write-Output "Done sleeping"
   ```
   - **CRITICAL TEST**: IDE should NOT freeze
   - Lazarus should remain clickable/responsive
   - After 10 seconds, should show "Done sleeping"

5. **Progress Test**
   ```
   Run: 1..5 | ForEach-Object { Start-Sleep -Seconds 2; Write-Output "Step $_" }
   ```
   - Should output "Step 1" through "Step 5" over 10 seconds
   - IDE should stay responsive throughout

6. **Git Command (Real-World)**
   ```
   Run git log --oneline -10
   ```
   - Should show last 10 commits
   - Test with real I/O operations

### Phase 3: Edge Cases ⚠️

7. **Timeout Test (30 seconds)**
   ```
   Execute: Start-Sleep -Seconds 40
   ```
   - Should timeout after 30 seconds
   - Error message should say "timed out"
   - Thread should be cancelled properly

8. **Error Handling**
   ```
   Run this: Get-NonExistentCommand
   ```
   - Should show PowerShell error message
   - Non-zero exit code should be displayed

9. **Multiple Commands Sequentially**
   - Send 3 commands one after another
   - Each should complete independently
   - No thread leaks or resource issues

### Phase 4: Stress Testing 💪

10. **Large Output**
    ```
    Run: Get-ChildItem -Path C:\Windows\System32 -Recurse | Select-Object -First 100
    ```
    - Should handle large text output
    - No memory leaks

11. **Fast Repeated Commands**
    - Send 5 quick commands in a row (each takes <1 second)
    - All should complete successfully
    - No race conditions

12. **Command While Thread Running**
    - Start long command (10+ seconds)
    - Immediately try another command
    - Should queue or handle gracefully

## Expected Behavior

### Success Indicators ✅
- IDE remains clickable during command execution
- Messages panel shows no errors
- Output appears in chat after command completes
- Exit codes displayed for errors
- Timeouts work correctly (30 seconds)

### Problem Indicators ❌
- IDE freezes/becomes unresponsive
- Commands never complete (hung thread)
- Access violations or memory errors
- Output garbled or missing
- Multiple threads not cleaned up

## VS Code Comparison

| Feature | VS Code | Pythia (This Implementation) |
|---------|---------|------------------------------|
| Process isolation | Separate pty host | Same process, background thread |
| UI responsiveness | Event-driven IPC | Application.ProcessMessages loop |
| Output streaming | Real-time | Polled every 100ms |
| Cancellation | Direct IPC message | Thread.Terminate + timeout |
| Concurrent commands | Full multiplexing | Sequential (one at a time) |

## Known Limitations

1. **No real-time streaming**: Output appears every 100ms (timer interval)
2. **Sequential execution**: Only one command at a time
3. **No cancellation UI**: User can't manually cancel (only timeout)
4. **Process overhead**: Creates new TProcess for each command

## Future Enhancements

### Phase 5: UI Improvements (Not Yet Implemented)
- [ ] Progress indicator during execution
- [ ] Cancel button for long commands
- [ ] Real-time output streaming (not batched)
- [ ] Status bar showing command state

### Phase 6: Advanced Features (Not Yet Implemented)
- [ ] Persistent PowerShell session (keep state between commands)
- [ ] Multiple concurrent commands
- [ ] Command history/autocomplete
- [ ] Terminal pane docking

## How to Report Issues

If you encounter problems:

1. Check Lazarus Messages panel for errors
2. Note exact command that failed
3. Check if IDE was responsive or frozen
4. Look for error messages in chat output
5. Check beads issue: `pythia-lazarus-bg1`

## Implementation Files

- **Source/Pythia.Tools.Terminal.Async.pas** (321 lines)
  - Threading infrastructure
  - TTerminalOutputQueue, TTerminalThread, TTerminalExecutor

- **Source/Pythia.Tools.Terminal.pas** (132 lines)
  - Modified to use async executor
  - ExecuteCommandSync wrapper method

- **pythia.lpk**
  - Package updated with new unit

## Build/Install

```powershell
.\CLEAN_INSTALL.ps1
```

This automatically:
1. Stops Lazarus
2. Cleans registry
3. Builds package
4. Rebuilds IDE
5. Launches Lazarus

## Git Branch

```bash
feature/terminal-threading
```

Created from: `feature/synedit-chat-renderer`

## Success Criteria

✅ This implementation is successful if:
- IDE never freezes during terminal commands
- 10-second sleep test passes with responsive IDE
- Timeout works correctly (30 seconds)
- No memory leaks after multiple commands
- Exit codes displayed correctly

The primary goal is **UI responsiveness** - VS Code-level polish can come later.
