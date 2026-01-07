# Function Calling Implementation - Pioneer Status

## 🎯 Mission
Implement OpenAI/Anthropic-style function calling in Lazarus IDE - **first of its kind for Lazarus!**

## ✅ Phase 1: COMPLETE - Core Infrastructure

### Files Created
1. **Pythia.Tools.pas** (169 lines)
   - Tool system interface (`IPythiaTool`)
   - Tool registry (`TPythiaToolRegistry`)
   - Data structures: `TToolDefinition`, `TToolCall`, `TToolResult`
   - Global registry instance
   
2. **Pythia.Tools.Terminal.pas** (129 lines)
   - Implements `run_terminal_command` tool
   - Uses TProcess for PowerShell execution
   - 30-second timeout protection
   - Returns command output as string

3. **Pythia.AI.Response.pas** (135 lines)
   - `TAIResponse` record type (text + tool_calls)
   - `ParseOpenAIResponse()` - handles tool_calls in response
   - `ParseAnthropicResponse()` - handles tool_use blocks
   
4. **Pythia.AI.Client.pas** (PARTIAL - in progress)
   - ✅ Updated interface with new methods
   - ✅ Added `BuildToolDefinitionsJSON()` method
   - ✅ Changed return types to `TAIResponse`
   - ✅ Added `SendMessageWithToolResults()` method
   - ⏳ Need to update `BuildOpenAIRequest()` to add tools array
   - ⏳ Need to update `BuildAnthropicRequest()` to add tools array
   - ⏳ Need to handle tool result messages in conversation

## 🚧 Phase 2: IN PROGRESS - Request Building

### What Needs To Happen

**BuildOpenAIRequest() needs**:
1. Add `tools` array to JSON (from `BuildToolDefinitionsJSON()`)
2. Handle `ToolResults` parameter - add tool result messages to conversation
3. Remove old terminal prompt text (now using real tools)

**BuildAnthropicRequest() needs**:
1. Add `tools` array to JSON (Anthropic format differs from OpenAI)
2. Handle `ToolResults` parameter - different format than OpenAI
3. Remove old terminal prompt text

### Current Challenge
The request building methods are very large (~100 lines each) with complex system prompts. Need to:
1. Keep file edit prompt (that still uses JSON format)
2. Remove terminal code block prompt (replaced by function calling)
3. Add tools array
4. Add tool result handling

## ⏳ Phase 3: TODO - ChatForm Integration

### Changes Needed in Pythia.ChatForm.pas

1. **Register tools on startup**:
   ```pascal
   procedure TChatWindow.FormCreate(Sender: TObject);
   begin
     // ... existing code ...
     
     // Register terminal tool
     ToolRegistry.RegisterTool(TTerminalTool.Create);
   end;
   ```

2. **Tool call loop in SendMessageToAI**:
   ```pascal
   // Send message
   AIResponse := TPythiaAIClient.SendMessageWithContext(FMessages, ModelName, ContextText);
   
   // Handle tool calls
   MaxIterations := 10;  // Prevent infinite loops
   while AIResponse.HasToolCalls and (MaxIterations > 0) do
   begin
     Dec(MaxIterations);
     
     // Execute tools
     SetLength(ToolResults, Length(AIResponse.ToolCalls));
     for I := 0 to High(AIResponse.ToolCalls) do
     begin
       AddMessage('system', '⚙️ Executing: ' + AIResponse.ToolCalls[I].ToolName + '...');
       ToolResults[I] := ToolRegistry.ExecuteToolCall(AIResponse.ToolCalls[I]);
       
       // Show terminal output
       if AIResponse.ToolCalls[I].ToolName = 'run_terminal_command' then
       begin
         AddTerminalOutput(ToolResults[I].Output);
         PanelTerminal.Visible := True;
       end;
     end;
     
     // Send results back to AI
     AIResponse := TPythiaAIClient.SendMessageWithToolResults(FMessages, ModelName, ToolResults, ContextText);
   end;
   
   // Display final response
   if AIResponse.Text <> '' then
     AddMessage('assistant', AIResponse.Text);
   ```

## 📋 Implementation Strategy

### Option A: Complete AI Client First (Recommended)
1. Finish updating `BuildOpenAIRequest()`
2. Finish updating `BuildAnthropicRequest()`
3. Test with simple request/response
4. Then integrate into ChatForm

### Option B: Minimal Viable Product
1. Only implement OpenAI function calling
2. Skip Anthropic for now
3. Get terminal commands working
4. Add Anthropic support later

### Option C: Incremental Testing
1. Add tools array to requests (no tool results yet)
2. Test that AI tries to call tools
3. Add tool result handling
4. Test full loop

## 🎓 Learning Points

### What We Discovered
- VS Code Copilot doesn't use prompting - it uses function calling
- LLMs return structured `tool_calls` in responses
- Tool execution is a multi-turn conversation
- Need to handle:
  - Tool definitions in request
  - Tool calls in response
  - Tool results in next request
  - Final text response

### API Differences
**OpenAI**:
- Tools in `tools` array
- Tool calls in `message.tool_calls`
- Tool results as `role: "tool"` messages

**Anthropic**:
- Tools in `tools` array (different schema)
- Tool use in `content` array as `type: "tool_use"`
- Tool results as `type: "tool_result"` in user message content

## 🐛 Risks & Mitigations

### Infinite Loops
**Risk**: AI keeps calling tools forever
**Mitigation**: Max iteration counter (10 iterations)

### Token Limits
**Risk**: Tool calls + results consume lots of tokens
**Mitigation**: Keep tool descriptions concise, limit output size

### Error Handling
**Risk**: Tool execution fails, breaks conversation
**Mitigation**: Return error as tool result, let AI handle it

### JSON Complexity
**Risk**: Nested JSON in tool arguments needs careful escaping
**Mitigation**: Use fpjson for all JSON generation

## 📊 Estimated Completion

- **Phase 2 (Request Building)**: 2-3 hours
- **Phase 3 (ChatForm Integration)**: 1-2 hours  
- **Testing & Debugging**: 2-4 hours
- **Total**: 5-9 hours of focused work

## 🚀 When Complete

Pythia will be the **first Lazarus IDE plugin with real LLM function calling!**

Users will be able to:
- Ask "What directory are we in?" → AI calls terminal → Shows result
- Ask "List files" → AI calls `Get-ChildItem` → Shows output
- Ask "What's my git status?" → AI calls `git status` → Shows status
- Chain multiple commands naturally in conversation

This is VS Code Copilot-level functionality in Lazarus!
