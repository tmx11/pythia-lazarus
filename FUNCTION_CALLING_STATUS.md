# Function Calling Implementation Status

## ✅ Completed

### 1. Core Infrastructure
- **Pythia.Tools.pas**: Tool system interface
  - `IPythiaTool` interface for tool implementations
  - `TPythiaToolRegistry` for managing tools
  - `TToolCall`, `TToolResult` record types
  - Tool parameter definitions with JSON schema

- **Pythia.Tools.Terminal.pas**: Terminal tool implementation
  - Implements `run_terminal_command` tool
  - Executes PowerShell commands via TProcess
  - Returns command output
  - 30-second timeout protection

- **Pythia.AI.Response.pas**: Response parsing
  - `TAIResponse` record type
  - Parses both text and tool call responses
  - Supports OpenAI and Anthropic formats

## 🚧 In Progress

### 2. AI Client Integration
Need to modify **Pythia.AI.Client.pas**:

1. **Add tool definitions to requests**:
   - OpenAI: Add `tools` array to JSON
   - Anthropic: Add `tools` array to JSON
   - Format: See OpenAI function calling spec

2. **Change return type**:
   - `SendMessage()` → returns `TAIResponse` instead of `string`
   - `SendMessageWithContext()` → returns `TAIResponse`

3. **Support tool result messages**:
   - Add `SendMessageWithToolResults()` method
   - Pass tool results back to AI in next turn
   - Format tool results per API spec

### 3. ChatForm Integration
Need to modify **Pythia.ChatForm.pas**:

1. **Tool call loop**:
   ```pascal
   // Send initial message
   Response := AI.SendMessage(Messages, Model);
   
   while Response.HasToolCalls do
   begin
     // Execute all tool calls
     for ToolCall in Response.ToolCalls do
       Results.Add(ToolRegistry.ExecuteToolCall(ToolCall));
     
     // Send results back to AI
     Response := AI.SendMessageWithToolResults(Messages, Model, Results);
   end;
   
   // Display final text response
   AddMessage('assistant', Response.Text);
   ```

2. **Initialize tool registry**:
   - Register terminal tool on form create
   - Could add more tools later (file tree, search, etc.)

3. **Display tool execution**:
   - Show "⚙️ Executing: run_terminal_command..."
   - Show terminal output in terminal pane
   - Add tool results to conversation history

## 📋 OpenAI Function Calling Format

### Request with Tools
```json
{
  "model": "gpt-4",
  "messages": [...],
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "run_terminal_command",
        "description": "Execute a PowerShell command...",
        "parameters": {
          "type": "object",
          "properties": {
            "command": {
              "type": "string",
              "description": "The PowerShell command to execute"
            }
          },
          "required": ["command"]
        }
      }
    }
  ]
}
```

### Response with Tool Calls
```json
{
  "choices": [{
    "message": {
      "role": "assistant",
      "content": null,
      "tool_calls": [{
        "id": "call_abc123",
        "type": "function",
        "function": {
          "name": "run_terminal_command",
          "arguments": "{\"command\": \"Get-Location\"}"
        }
      }]
    },
    "finish_reason": "tool_calls"
  }]
}
```

### Next Request with Tool Results
```json
{
  "model": "gpt-4",
  "messages": [
    {"role": "user", "content": "What directory are we in?"},
    {
      "role": "assistant",
      "content": null,
      "tool_calls": [...]
    },
    {
      "role": "tool",
      "tool_call_id": "call_abc123",
      "content": "D:\\dev\\delphi\\pythia-lazarus"
    }
  ],
  "tools": [...]
}
```

## 📋 Anthropic Tool Use Format

### Request with Tools
```json
{
  "model": "claude-3-5-sonnet-20241022",
  "messages": [...],
  "tools": [
    {
      "name": "run_terminal_command",
      "description": "Execute a PowerShell command...",
      "input_schema": {
        "type": "object",
        "properties": {
          "command": {
            "type": "string",
            "description": "The PowerShell command to execute"
          }
        },
        "required": ["command"]
      }
    }
  ]
}
```

### Response with Tool Use
```json
{
  "content": [{
    "type": "tool_use",
    "id": "toolu_abc123",
    "name": "run_terminal_command",
    "input": {
      "command": "Get-Location"
    }
  }],
  "stop_reason": "tool_use"
}
```

### Next Request with Tool Results
```json
{
  "model": "claude-3-5-sonnet-20241022",
  "messages": [
    {"role": "user", "content": "What directory are we in?"},
    {
      "role": "assistant",
      "content": [{
        "type": "tool_use",
        "id": "toolu_abc123",
        "name": "run_terminal_command",
        "input": {"command": "Get-Location"}
      }]
    },
    {
      "role": "user",
      "content": [{
        "type": "tool_result",
        "tool_use_id": "toolu_abc123",
        "content": "D:\\dev\\delphi\\pythia-lazarus"
      }]
    }
  ],
  "tools": [...]
}
```

## 🎯 Next Steps

1. **Update Pythia.AI.Client.pas**:
   - Add `BuildToolDefinitionsJSON()` method
   - Modify `BuildOpenAIRequest()` to include tools
   - Modify `BuildAnthropicRequest()` to include tools
   - Change return type to `TAIResponse`
   - Add `SendMessageWithToolResults()` method

2. **Update Pythia.ChatForm.pas**:
   - Register terminal tool on FormCreate
   - Implement tool call loop in `SendMessageToAI`
   - Display tool execution status
   - Show results in terminal pane

3. **Update pythia.lpk**:
   - Add Pythia.Tools.pas
   - Add Pythia.Tools.Terminal.pas
   - Add Pythia.AI.Response.pas

4. **Test**:
   - Build and install
   - Ask: "What directory are we in?"
   - Should see: AI requests tool → Tool executes → AI responds with result

## 🐛 Known Issues to Watch For

1. **JSON escaping**: Tool arguments contain JSON strings that need proper escaping
2. **Infinite loops**: AI might keep calling tools - add max iteration limit (5-10)
3. **Error handling**: Tool execution failures need graceful degradation
4. **Context size**: Tool calls + results consume tokens - watch limits

## 📚 References

- [OpenAI Function Calling](https://platform.openai.com/docs/guides/function-calling)
- [Anthropic Tool Use](https://docs.anthropic.com/en/docs/build-with-claude/tool-use)
- [VS Code Language Model Tools API](https://code.visualstudio.com/api/extension-guides/ai/tools)
