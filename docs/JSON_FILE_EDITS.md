# JSON-Based File Editing Implementation

## Overview

Pythia now uses **structured JSON file edits** based on VS Code's WorkspaceEdit architecture, replacing the previous simple text-based format.

## Architecture Changes

### Before (Simple Text Format)
```
EDIT_FILE: filepath
entire file content here
END_EDIT
```

**Problems:**
- Sent entire file content (large payloads)
- Lost IDE markers/diagnostics on replacement
- No support for multiple edits
- No precision in edit locations

### After (JSON WorkspaceEdit Format)
```json
{
  "edits": [
    {
      "file": "Source/MyUnit.pas",
      "startLine": 10,
      "endLine": 15,
      "newText": "  // Updated implementation\n  Result := True;"
    }
  ]
}
```

**Benefits:**
- ✅ Precise line-range edits (like VS Code TextEdit)
- ✅ Preserves IDE markers and diagnostics
- ✅ Small payloads (only changed lines)
- ✅ Multiple edits in one response
- ✅ Works with IOTAEditWriter for proper IDE integration

## Implementation Details

### 1. AI System Prompts Updated

All three model providers now request JSON format:

**Files Modified:**
- `Pythia.AI.Client.pas` - BuildOpenAIRequest
- `Pythia.AI.Client.pas` - BuildAnthropicRequest  
- `Pythia.AI.Client.pas` - BuildGitHubCopilotRequest

**New Prompt Instructions:**
```
When editing files, return ONLY the changed lines with precise line ranges. Use this JSON format:
```json
{
  "edits": [
    {
      "file": "Source/MyUnit.pas",
      "startLine": 10,
      "endLine": 15,
      "newText": "  // Updated code\n  Result := True;"
    }
  ]
}
```
Lines are 1-indexed. Include only changed code, not entire file. Multiple edits allowed.
```

### 2. JSON Parser Implementation

**File:** `Pythia.ChatForm.pas` - ParseAndExecuteFileEdits method

**Key Features:**
- Extracts JSON from markdown code blocks (```json ... ```)
- Parses using System.JSON (standard Delphi)
- Iterates through edits array
- Calls `IContextProvider.ReplaceLines` for each edit
- Shows summary with ✓/✗ for success/failure

**Flow:**
1. Search for ````json` block in AI response
2. Extract JSON text
3. Parse with `TJSONObject.ParseJSONValue`
4. Iterate `edits` array
5. For each edit:
   - Extract file, startLine, endLine, newText
   - Call `FContextProvider.ReplaceLines()`
   - Build success/failure summary
6. Replace JSON block with summary in chat

### 3. IOTAEditWriter Integration

**File:** `Pythia.Context.pas` - TIDEContextProvider.ReplaceLines

**Already implemented** - no changes needed!

```pascal
function TIDEContextProvider.ReplaceLines(const FilePath: string; 
  StartLine, EndLine: Integer; const NewText: string): Boolean;
begin
  // Uses IOTAEditWriter to:
  // 1. Open file in editor (or get existing)
  // 2. Calculate start/end positions
  // 3. Replace range with new text
  // 4. Preserves markers/bookmarks
  Result := True;
end;
```

This method already does **exactly what VS Code's TextEdit does**!

## VS Code Comparison

| Feature | VS Code | Pythia |
|---------|---------|--------|
| Edit Format | WorkspaceEdit + TextEdit | JSON edits array |
| Precision | Line + character position | Line range (1-indexed) |
| API | workspace.applyEdit() | IOTAEditWriter |
| Multiple Files | ✅ Yes | ✅ Yes (array support) |
| Markers Preserved | ✅ Yes | ✅ Yes (OTA handles) |
| Multiple Edits | ✅ Yes | ✅ Yes |

## Example Usage

### AI Request:
```
User: "Fix the memory leak in TMyClass.Create, it's in Source/MyClass.pas lines 45-52"
```

### AI Response:
```markdown
I've fixed the memory leak by adding try-finally:

```json
{
  "edits": [
    {
      "file": "Source/MyClass.pas",
      "startLine": 45,
      "endLine": 52,
      "newText": "constructor TMyClass.Create;\nbegin\n  FList := TStringList.Create;\n  try\n    FList.LoadFromFile('data.txt');\n  except\n    FList.Free;\n    raise;\n  end;\nend;"
    }
  ]
}
```

The try-except ensures FList is freed if LoadFromFile raises an exception.
```

### Plugin Display:
```
**File Edits Applied:**
✓ Updated MyClass.pas (lines 45-52)

The try-except ensures FList is freed if LoadFromFile raises an exception.
```

## Benefits Over Previous Approach

1. **Smaller Token Usage**: Only changed lines, not entire file
2. **Faster Responses**: Less data to generate/parse
3. **Better Context**: AI can see surrounding code via GetCurrentFile
4. **IDE Integration**: Markers, breakpoints, bookmarks preserved
5. **Multiple Edits**: Can fix multiple methods in one response
6. **Error Recovery**: JSON parsing fails gracefully

## Future Enhancements

Potential improvements (not yet implemented):

1. **Character-level precision**: Add `startCharacter` / `endCharacter`
   ```json
   "range": {
     "start": {"line": 10, "character": 5},
     "end": {"line": 10, "character": 20}
   }
   ```

2. **Multi-file edits**: Already supported in JSON format, just needs AI to use it

3. **Edit validation**: Verify line numbers exist before applying

4. **Undo support**: Track edits for rollback

5. **Diff preview**: Show before/after in UI before applying

## Testing

To test the new format:

1. Open Delphi IDE
2. Tools > Pythia AI Chat (Ctrl+Shift+P)
3. Configure API keys in Settings
4. Ask: "Add error handling to procedure X in lines Y-Z"
5. Verify AI returns JSON format
6. Verify file is edited correctly
7. Check IDE markers still work

## References

- VS Code API: [WorkspaceEdit](https://code.visualstudio.com/api/references/vscode-api#WorkspaceEdit)
- VS Code API: [TextEdit](https://code.visualstudio.com/api/references/vscode-api#TextEdit)
- Delphi ToolsAPI: IOTAEditWriter
- JSON Standard: RFC 8259
