# Terminal Output Testing Guide

## Issue
User reports: "chat can't see the output of terminal commands"

## Current Flow (VERIFIED AS CORRECT)

```pascal
// 1. Tool execution (Pythia.Tools.Terminal.pas)
function TTerminalTool.Execute(Arguments: TJSONObject): string;
begin
  Result := ExecuteCommandSync(Command);  // Returns accumulated output
end;

// 2. ChatForm captures result (Pythia.ChatForm.pas:484)
ToolResults[I] := ToolRegistry.ExecuteToolCall(Response.ToolCalls[I]);

// 3. ChatForm sends to AI (Pythia.ChatForm.pas:496)
Response := TPythiaAIClient.SendMessageWithToolResults(FMessages, ModelName, ToolResults, ContextText);

// 4. AI Client formats tool results (Pythia.AI.Client.pas:260-269)
for ToolResult in ToolResults do
begin
  MsgObj := TJSONObject.Create;
  MsgObj.Add('role', 'tool');
  MsgObj.Add('tool_call_id', ToolResult.CallId);
  MsgObj.Add('content', ToolResult.Output);  // ← Output IS sent to AI
  MsgArray.Add(MsgObj);
end;
```

## Test Scenarios

### Test 1: Simple Command
**Command to AI**: "Run this command: `Get-Location`"

**Expected Result**:
- Terminal pane shows: `D:\dev\delphi\pythia-lazarus`
- AI should respond: "The current directory is `D:\dev\delphi\pythia-lazarus`"

**If AI doesn't mention output**: The model is not processing tool results properly

### Test 2: Command with Error
**Command to AI**: "Run this command: `Get-NonExistentCommand`"

**Expected Result**:
- Terminal pane shows error
- AI should respond acknowledging the error

### Test 3: Verbose Output
**Command to AI**: "Run: `Get-ChildItem | Select-Object -First 5`"

**Expected Result**:
- Terminal shows file list
- AI should summarize the files

## Diagnosis

**If terminal pane shows output BUT AI doesn't mention it**:
→ Problem: Model not processing tool results (API issue, not code issue)

**If terminal pane is empty**:
→ Problem: Tool execution issue (check `TTerminalExecutor`)

**If terminal shows output AND AI mentions it**:
→ Working correctly! User expectation was wrong.

## Quick Verification Script

```pascal
// Add temporary debug logging to Pythia.ChatForm.pas:
procedure TChatWindow.SendMessageToAI;
begin
  // ... existing code ...
  
  if Length(ToolResults) > 0 then
  begin
    AddMessage('system', '[DEBUG] Tool Results Count: ' + IntToStr(Length(ToolResults)));
    for I := 0 to High(ToolResults) do
      AddMessage('system', '[DEBUG] Output Length: ' + IntToStr(Length(ToolResults[I].Output)));
  end;
  
  // Send tool results back to AI
  Response := TPythiaAIClient.SendMessageWithToolResults(...);
end;
```

## VS Code Reference (from research)

VS Code uses **streaming output** with `Pseudoterminal.onDidWrite` events, but Lazarus uses **batch collection** (accumulate → return full output). Both approaches are valid - batch is simpler and works fine for short commands.

For streaming (future enhancement):
- Add `OnOutputReceived` event in ChatForm
- Display output incrementally in terminal pane as it arrives
- Still send full output to AI after completion

## Recommendation

**Before making changes**:
1. Test with simple command: "Run: Get-Location"
2. Check if AI's response mentions the directory
3. If yes → System works correctly
4. If no → Model configuration issue (not code issue)
