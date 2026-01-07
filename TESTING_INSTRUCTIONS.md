# 🧪 Pythia Function Calling - Testing Instructions

## ✅ Installation Complete!

**Status**: Pythia has been built and installed in Lazarus IDE with full function calling support!

## How to Test

### 1. Open Pythia Chat Window

In Lazarus IDE:
- **View** → **IDE Internals Windows** → **PythiaChatWindow**

### 2. Configure API Key

Before testing, you need an API key. Choose one:

**Option A: GitHub Copilot (FREE - Recommended!)**
- Model: "GitHub Copilot: GPT-4" or "GitHub Copilot: GPT-3.5 Turbo"
- Requires GitHub Copilot subscription
- Uses OpenAI-compatible format

**Option B: OpenAI**
- Model: "GPT-4" or "GPT-3.5 Turbo"
- Requires OpenAI API key
- Get key from: https://platform.openai.com/api-keys

**Option C: Anthropic**
- Model: "Claude 3.5 Sonnet" or "Claude 3 Opus"
- Requires Anthropic API key
- Get key from: https://console.anthropic.com/

### 3. Test Terminal Commands

**Terminal commands now work via function calling!** Try these:

#### Test 1: Current Directory
```
What directory are we in?
```

**Expected behavior**:
1. AI makes tool call: `run_terminal_command` with `Get-Location`
2. You see: "⚙️ Executing: run_terminal_command..."
3. Terminal pane appears with output: `D:\dev\delphi\pythia-lazarus`
4. AI responds: "You are currently in the directory: D:\dev\delphi\pythia-lazarus"

#### Test 2: List Files
```
What files are in the Source directory?
```

**Expected behavior**:
1. Tool call: `Get-ChildItem Source`
2. Terminal shows file listing
3. AI summarizes the files

#### Test 3: Git Status
```
What's the current git status?
```

**Expected behavior**:
1. Tool call: `git status`
2. Terminal shows git output
3. AI explains the status

#### Test 4: Multiple Commands
```
What's my current branch and how many commits ahead am I?
```

**Expected behavior**:
1. Multiple tool calls (git branch, git status)
2. Each shown in terminal
3. AI summarizes results

### 4. Verify Function Calling

Look for these indicators that function calling is working:

✅ **Tool execution messages**: "⚙️ Executing: run_terminal_command..."
✅ **Terminal pane appears**: Shows command output
✅ **No code blocks in response**: AI doesn't show \`\`\`powershell blocks anymore
✅ **Structured flow**: Request → Execute → Results → Final answer

### 5. Debug if Not Working

If terminal commands don't work:

**Check 1: API Key Set**
- Pythia Settings: View → Pythia Settings
- Enter your API key for chosen model

**Check 2: Model Selected**
- Dropdown shows: "GitHub Copilot: GPT-4" (or your chosen model)

**Check 3: Error Messages**
- Look for red error messages in chat
- Check if it says "401 Unauthorized" → API key wrong
- Check if it says "429 Rate Limit" → Too many requests

**Check 4: Tool Registration**
- Terminal command should be available
- If not working, check FormCreate in Pythia.ChatForm.pas
- Should have: `ToolRegistry.RegisterTool(TTerminalTool.Create);`

### 6. Compare Old vs New Behavior

**OLD (Code Block Parsing - Removed)**:
```
User: "What directory are we in?"
AI: "Let me check:
```powershell
cd
```
[Output shown in terminal]"
```

**NEW (Function Calling - Active Now)**:
```
User: "What directory are we in?"
⚙️ Executing: run_terminal_command...
[Terminal shows: D:\dev\delphi\pythia-lazarus]
AI: "You are currently in the directory: D:\dev\delphi\pythia-lazarus"
```

## What Changed Under the Hood

### The Magic

When you ask "What directory are we in?", this happens:

1. **Request includes tools**:
   ```json
   {
     "model": "gpt-4",
     "messages": [...],
     "tools": [{
       "type": "function",
       "function": {
         "name": "run_terminal_command",
         "description": "Execute a PowerShell command in Windows terminal",
         "parameters": {
           "type": "object",
           "properties": {
             "command": {"type": "string", "description": "PowerShell command"}
           },
           "required": ["command"]
         }
       }
     }]
   }
   ```

2. **AI responds with tool call**:
   ```json
   {
     "tool_calls": [{
       "id": "call_abc123",
       "type": "function",
       "function": {
         "name": "run_terminal_command",
         "arguments": "{\"command\": \"Get-Location\"}"
       }
     }]
   }
   ```

3. **Pythia executes**:
   ```pascal
   TProcess → powershell.exe -NoProfile -Command "Get-Location"
   Output: "D:\dev\delphi\pythia-lazarus"
   ```

4. **Results sent back**:
   ```json
   {
     "role": "tool",
     "tool_call_id": "call_abc123",
     "content": "D:\\dev\\delphi\\pythia-lazarus"
   }
   ```

5. **AI final response**:
   ```json
   {
     "content": "You are currently in the directory: D:\\dev\\delphi\\pythia-lazarus"
   }
   ```

## Known Limitations

1. **Anthropic tools not fully implemented**: OpenAI and GitHub Copilot work, Anthropic needs different tool schema
2. **Max 10 iterations**: Prevents infinite tool call loops
3. **PowerShell only**: Terminal tool currently uses PowerShell (Windows)
4. **30-second timeout**: Commands that take longer will be killed

## Next Steps

Once terminal commands work:

1. **Add more tools**: File read, file search, git operations
2. **Anthropic support**: Implement Anthropic tool schema
3. **Cross-platform**: Add bash support for Linux/Mac
4. **Tool results in history**: Store tool calls in conversation history
5. **Tool permissions**: Ask user before executing commands

## Troubleshooting

### "Error: OpenAI API key not configured"
→ Set API key in Pythia Settings

### "Error: GitHub Copilot not authenticated"
→ Need GitHub Copilot subscription and authentication

### Terminal pane doesn't appear
→ Check if tool call message appears ("⚙️ Executing...")
→ If not, tool calling might not be working - check API response

### AI still uses code blocks
→ Old API models might not support function calling
→ Try GPT-4 or GPT-3.5-turbo (not text-davinci-003)

### Commands fail with "Access Denied"
→ PowerShell execution policy - run as admin: `Set-ExecutionPolicy RemoteSigned`

---

**You are now testing the FIRST Lazarus IDE plugin with native LLM function calling! 🚀**
