# Terminal Command Protocol for Pythia

## Problem Analysis

**Current Issue:** Terminal pane opens but shows no output. Commands don't execute.

**Root Cause:** We're trying to use prompting instead of structured protocols.

## How VS Code Actually Works

### File Edits (Working ✅)
```json
{
  "edits": [
    {
      "file": "unit1.pas",
      "startLine": 1,
      "endLine": 1,
      "newText": "// Edited by Pythia\nunit Unit1;"
    }
  ]
}
```
- AI outputs **structured JSON**
- `ParseFileEdits()` extracts JSON from response
- `ApplyFileEdits()` calls IDE APIs
- System shows: "Applied 1 file edit(s)"

### Terminal Commands (Should Work The Same Way)
```json
{
  "commands": [
    {
      "command": "cd",
      "description": "Show current directory"
    }
  ]
}
```
- AI outputs **structured JSON**
- `ParseTerminalCommands()` extracts JSON from response
- `ExecuteTerminalCommands()` uses TProcess to run
- System shows output in terminal pane

## VS Code's Actual Implementation

VS Code uses **Language Model Tools** (function calling):

1. **Tool Definition** (`package.json`):
   ```json
   {
     "languageModelTools": [{
       "name": "run_terminal_command",
       "description": "Execute a command in the terminal",
       "inputSchema": {
         "type": "object",
         "properties": {
           "command": { "type": "string", "description": "Command to execute" }
         },
         "required": ["command"]
       }
     }]
   }
   ```

2. **Tool Implementation**: Extension registers handler
3. **LLM Invocation**: LLM generates tool call with structured params
4. **Execution**: VS Code invokes tool, captures output, returns to LLM
5. **Response**: LLM continues conversation with tool results

## What We Need To Implement

### Option 1: JSON Protocol (Like File Edits)
**Pros**: 
- Works TODAY with current AI APIs
- No VS Code-specific APIs needed
- Consistent with file edits

**Cons**:
- AI must be taught the JSON format
- Less integrated than VS Code's tool calling

### Option 2: Function Calling (OpenAI/Anthropic)
**Pros**:
- Native LLM feature
- More reliable than JSON parsing

**Cons**:
- Requires function calling API support
- More complex implementation

## Recommended Approach

**Start with JSON Protocol** (matches file edits):

1. Update system prompt with JSON examples
2. Implement `ParseTerminalCommands()` - extract JSON
3. Implement `ExecuteTerminalCommands()` - run via TProcess
4. Show results in terminal pane
5. Feed output back to AI in next message

## Implementation Plan

### 1. System Prompt Update
```
TERMINAL ACCESS:

When you need to run terminal commands, output them as JSON:

```json
{
  "commands": [
    {
      "command": "Get-Location",
      "description": "Get current directory"
    },
    {
      "command": "Get-ChildItem -Name",
      "description": "List files in current directory"
    }
  ]
}
```

Commands will execute automatically and output will be shown below the chat.
```

### 2. Create Pythia.Terminal.pas
```pascal
unit Pythia.Terminal;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, Process;

type
  TTerminalCommand = record
    Command: string;
    Description: string;
  end;

  TTerminalCommandArray = array of TTerminalCommand;

function ParseTerminalCommands(const Response: string): TTerminalCommandArray;
function ExecuteTerminalCommands(const Commands: TTerminalCommandArray): TStringList;

implementation

function ParseTerminalCommands(const Response: string): TTerminalCommandArray;
// Extract JSON {"commands": [...]} from AI response

function ExecuteTerminalCommands(const Commands: TTerminalCommandArray): TStringList;
// Execute each command via TProcess, return combined output

end.
```

### 3. Update ChatForm Integration
```pascal
// In SendMessageToAI, after file edits:

// Check if response contains terminal commands
Commands := ParseTerminalCommands(Response);
if Length(Commands) > 0 then
begin
  Output := ExecuteTerminalCommands(Commands);
  AddTerminalOutput(Output.Text);
  PanelTerminal.Visible := True;  // Auto-show terminal
  
  // Add output to next AI message context
  AddMessage('system', 'Command output:' + #13#10 + Output.Text);
end;
```

## Testing

1. Build and install
2. Ask: "What directory are we in?"
3. AI should respond with:
   ```json
   {
     "commands": [{"command": "cd", "description": "Show current directory"}]
   }
   ```
4. Terminal pane shows: `D:\dev\delphi\pythia-lazarus`
5. AI sees output and continues conversation

## Future Enhancement: Function Calling

Once JSON protocol works, migrate to native function calling:
- OpenAI: `tools` parameter with `function` type
- Anthropic: `tools` parameter with `tool_choice`
- GitHub Copilot: Language Model Tools API

This requires updating `Pythia.AI.Client.pas` to use function calling APIs instead of simple chat completion.
