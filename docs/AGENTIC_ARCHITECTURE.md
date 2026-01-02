# Pythia Agentic Architecture - Technical Summary

**Updated**: December 24, 2025  
**Status**: Phase 0 Complete (GitHub Copilot Auth), Phase 1 In Progress (Workspace Context)

**Goal**: Add intelligent workspace context injection to Pythia, enabling it to "see" and understand the codebase like VS Code Copilot.

---

## 0. COMPLETED: GitHub Copilot Chat Integration ✅

Successfully implemented December 2025. See git history for implementation details.

---

## 1. Workspace Context Injection (CURRENT FOCUS)

### 1.1 How GitHub Copilot Does It

Research from https://docs.github.com/copilot and https://code.visualstudio.com/docs/copilot shows:

**Chat Variables (#-mentions)**:
- `#file` - Reference specific file: "#file:Pythia.ChatForm.pas explain this"
- `#selection` - Include selected text from editor
- `#codebase` - Search/reference across entire project  
- `#terminalSelection` - Include terminal output

**Reference Tracking**:
- Responses show "Used n references" dropdown
- Lists files/context injected into request
- Transparent about what the AI "saw"

**Token Management**:
- GitHub Copilot uses ~8k-32k token context window
- Intelligent prioritization: selection > current file > related files > project structure
- Automatic truncation of less relevant context when over limit

**Model Context Protocol (MCP)**:
- Extensible system for adding custom context sources
- Standard protocol for AI tools to access external data
- Can integrate databases, APIs, issue trackers, etc.

### 1.2 Delphi-Specific Context Strategy

**Three-Tier Context System**:

1. **Immediate Context** (Always Included - ~500 tokens):
   - Current active file name & path
   - Current cursor position/selection (if any)
   - Nearby code (±50 lines around cursor)

2. **Extended Context** (Included if space - ~2000 tokens):
   - Full current file content
   - Related files (interface/implementation pairs)
   - Recently edited files (from IDE history)

3. **Project Context** (On-demand or with #codebase - ~5000 tokens):
   - Project file list (.dproj parsing)
   - Unit dependency graph
   - Types/functions index (future: beads database?)

### 1.3 Why Not Just Spam Everything?

**Problems with naive "send all files" approach**:
- ❌ Wastes tokens on irrelevant code
- ❌ Dilutes important context with noise
- ❌ Slows AI processing (more to parse)
- ❌ Hits token limits quickly on large projects
- ❌ Costs more $ per request (if using paid APIs)

**Smart context gathering advantages**:
- ✅ Faster responses (less processing)
- ✅ More relevant answers (focused context)
- ✅ Scales to large projects (10k+ files)
- ✅ Cheaper API costs
- ✅ Better AI understanding (signal vs noise)

### 1.4 Beads Integration Consideration

**User's insight**: "maybe we should take notes from beads or just make beads a direct requirement"

**Beads architecture** (from .beads/ directory):
- SQLite database with graph structure
- JSONL for portability
- Issue linking and dependencies
- Rich metadata and relationships

**Potential uses for context**:
1. **Code Relationship Graph**: Track which units depend on which
2. **Symbol Index**: Fast lookup of classes/functions across project
3. **Change History**: What files change together (git blame style)
4. **Issue-Code Links**: "This code relates to issue pythia2-5j4"

**Decision**: 
- **Phase 1**: Implement basic context (current file, selection, project list)
- **Phase 2 (Future)**: Optional beads integration for advanced graph-based context
- **Rationale**: Don't over-engineer. Prove basic context works first, then add intelligence.

---

## 2. Implementation Plan

### 2.1 New Unit: Pythia.Context.pas

**Purpose**: Gather and format workspace context for AI requests

**Key Types**:
```pascal
type
  TContextItem = record
    ItemType: (ctCurrentFile, ctSelection, ctProjectFile, ctRelatedFile);
    FilePath: string;
    Content: string;
    LineStart, LineEnd: Integer;  // For selections
    TokenCount: Integer;
  end;

  IContextProvider = interface
    function GetCurrentFile: TContextItem;
    function GetSelection: TContextItem;
    function GetProjectFiles: TArray<string>;
    function GetRelatedFiles(const FileName: string): TArray<string>;
    function EstimateTokens(const Text: string): Integer;
  end;

  TIDEContextProvider = class(TInterfacedObject, IContextProvider)
    // Uses ToolsAPI - for BPL plugin
  end;

  TStandaloneContextProvider = class(TInterfacedObject, IContextProvider)
    // Uses filesystem - for standalone app
  end;
```

**Token Estimation**:
```pascal
function TContextProvider.EstimateTokens(const Text: string): Integer;
begin
  // Rough estimate: 1 token ≈ 4 characters for English text
  // Code is denser, use 1 token ≈ 3 characters
  Result := Length(Text) div 3;
end;
```

### 2.2 Modified Unit: Pythia.AI.Client.pas

**Add Context Parameter**:
```pascal
class function TPythiaAIClient.SendMessage(
  const AModel: string;
  const AMessages: TArray<TChatMessage>;
  const AContext: TArray<TContextItem>  // NEW!
): string;
begin
  // 1. Calculate total context tokens
  TokenBudget := GetModelTokenLimit(AModel);  // 8192 for GPT-4
  UsedTokens := EstimateMessageTokens(AMessages);
  
  // 2. Prioritize context items
  //    - ctSelection: Always include (highest priority)
  //    - ctCurrentFile: Include if fits
  //    - ctRelatedFile: Include if space remains
  //    - ctProjectFile: Only file names, not content
  
  // 3. Build context prefix
  ContextStr := FormatContextForAI(FilteredContext);
  
  // 4. Inject as system message or user prefix
  ModifiedMessages := InjectContext(AMessages, ContextStr);
  
  // 5. Send to API
  Result := CallActualAPI(AModel, ModifiedMessages);
end;
```

**Context Formatting**:
```pascal
function FormatContextForAI(const Context: TArray<TContextItem>): string;
begin
  Result := '';
  for Item in Context do
    case Item.ItemType of
      ctCurrentFile:
        Result := Result + Format('## Current File: %s'#13#10'```pascal'#13#10'%s'#13#10'```'#13#10,
                                 [Item.FilePath, Item.Content]);
      ctSelection:
        Result := Result + Format('## Selected Code (Lines %d-%d):'#13#10'```pascal'#13#10'%s'#13#10'```'#13#10,
                                 [Item.LineStart, Item.LineEnd, Item.Content]);
      ctProjectFile:
        Result := Result + Format('- %s'#13#10, [Item.FilePath]);
    end;
end;
```

### 2.3 Modified Unit: Pythia.ChatForm.pas

**Add Context UI**:
```pascal
private
  FContextProvider: IContextProvider;
  FAutoContext: Boolean;  // Setting: automatically include context
  
  procedure UpdateContextStatus;  // Show "Context: 3 files (2.1k tokens)"
  procedure RefreshContext;
  function GatherContext: TArray<TContextItem>;
```

**UI Elements**:
```
┌─────────────────────────────────────────────────┐
│ Pythia AI Chat                                  │
├─────────────────────────────────────────────────┤
│ [Chat Display]                                  │
│                                                 │
│ Response uses context:                          │
│   📄 Pythia.ChatForm.pas (lines 1-150)          │
│   📄 Pythia.AI.Client.pas                       │
│   📁 Project: 12 files                          │
├─────────────────────────────────────────────────┤
│ Context: Auto ✓ | 3 files | 2.1k tokens [↻]   │
├─────────────────────────────────────────────────┤
│ [Input]                                         │
│   Tip: Use #file:name or #selection            │
└─────────────────────────────────────────────────┘
```

**Send Message Flow**:
```pascal
procedure TChatWindow.SendMessageToAI;
var
  Context: TArray<TContextItem>;
begin
  // 1. Gather context
  if FAutoContext then
    Context := GatherContext
  else
    Context := ParseManualContext(MemoInput.Text);  // #file:xyz
  
  // 2. Send with context
  Response := TPythiaAIClient.SendMessage(
    ComboModel.Text,
    FMessages,
    Context  // NEW!
  );
  
  // 3. Show what context was used
  DisplayContextReferences(Context);
end;
```

### 2.4 IDE vs Standalone Context

**IDE Plugin (pythia.bpl)**:
```pascal
TIDEContextProvider.GetCurrentFile: TContextItem;
var
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  Reader: IOTAEditReader;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(Module) then
  begin
    Editor := Module.GetModuleFileEditor(0) as IOTASourceEditor;
    Reader := Editor.CreateReader;
    Result.Content := Reader.GetText;
    Result.FilePath := Editor.FileName;
  end;
end;
```

**Standalone App (PythiaApp.exe)**:
```pascal
TStandaloneContextProvider.GetCurrentFile: TContextItem;
begin
  // Option 1: Command-line arg: --project "path\to\project.dproj"
  // Option 2: Settings UI: Browse for project
  // Option 3: Detect if launched from IDE (parent process = bds.exe)
  
  if FProjectPath <> '' then
  begin
    ParseDprojFile(FProjectPath);  // Get source file list
    // Return most recently modified .pas file as "current"
  end;
end;
```

---

## 3. Testing Strategy

**Phase 1 Tests** (Manual IDE Testing):
1. Open Delphi project with Pythia plugin installed
2. Open a .pas file in editor
3. Select a block of code
4. Ask Pythia: "explain this code"
5. Verify response includes: "Based on the selected code from Pythia.ChatForm.pas..."
6. Check "Used references" shows the correct file

**Phase 2 Tests** (Automated):
1. Token counting accuracy (compare with tiktoken library estimates)
2. Context truncation (verify least important items dropped first)
3. Context formatting (valid markdown, code fences)
4. IDE vs Standalone parity (both gather equivalent context when possible)

**Phase 3 Tests** (Real-world scenarios):
1. Large project (1000+ files) - does context gathering remain fast?
2. Binary files in project - properly excluded from context?
3. Very long files (10k+ lines) - intelligent snippet extraction?

---

## 4. Future Enhancements (Post-MVP)

### 4.1 Smart Context Selection
- Semantic search: "Find code related to user authentication"
- Type inference: Automatically include interface definitions for used classes
- Call graph: Include called/calling functions

### 4.2 Beads Integration
```pascal
// Pythia.Context.Beads.pas - optional dependency
function GetRelatedIssues(const FileName: string): TArray<string>;
begin
  // Query .beads/*.db for issues mentioning this file
  // Include issue titles in context: "Working on: pythia2-5j4 (context injection)"
end;
```

### 4.3 Custom Context Plugins
- MCP server integration (like VS Code)
- .pythia-context.json file in project root
- User-defined context gathering scripts

---

## 0 (Historical). GitHub Copilot Chat Integration (PRIMARY API)

**Why Copilot Chat > API Keys**:
- ✅ FREE tier available (GitHub Copilot Individual - $10/mo includes chat)
- ✅ No per-request costs like OpenAI/Anthropic
- ✅ Same models as VS Code (GPT-4, Claude 3.5 Sonnet)
- ✅ OAuth authentication (no manual API key management)

**Implementation**:

### 0.1 Authentication Flow
```pascal
// Pythia.GitHub.Auth.pas (NEW)
class function TGitHubCopilotAuth.GetAuthToken: string;
begin
  // 1. Device flow: Request code from GitHub
  //    POST https://github.com/login/device/code
  //    client_id=Iv1.b507a08c87ecfe98 (GitHub Copilot's client ID)
  
  // 2. Show user code in dialog: "Enter code ABCD-1234 at github.com/login/device"
  
  // 3. Poll for token:
  //    POST https://github.com/login/oauth/access_token
  //    Returns: { "access_token": "gho_..." }
  
  // 4. Cache token in registry (encrypted)
  Result := GetCachedToken;
end;
```

### 0.2 Chat API Endpoint
```pascal
// Pythia.AI.Client.pas - Add GitHub Copilot method
class function TPythiaAIClient.CallGitHubCopilot(
  const AMessages: TArray<TChatMessage>
): string;
begin
  // Endpoint: https://api.githubcopilot.com/chat/completions
  // OR: https://api.github.com/copilot_internal/v2/chat/completions
  
  // Headers:
  //   Authorization: Bearer {github_token}
  //   Editor-Version: Delphi-12.0
  //   Editor-Plugin-Version: Pythia-1.0
  
  // Body (OpenAI-compatible):
  {
    "model": "gpt-4",  // or "claude-3.5-sonnet"
    "messages": [
      {"role": "system", "content": "You are Pythia..."},
      {"role": "user", "content": "{user_message}"}
    ],
    "stream": false
  }
end;
```

### 0.3 Model Selection
Update ComboBox in ChatForm:
```pascal
- GitHub Copilot: GPT-4  (FREE with subscription) ← DEFAULT
- GitHub Copilot: Claude 3.5 Sonnet
---
- OpenAI: GPT-4 (requires API key)
- Anthropic: Claude 3.5 (requires API key)
```

### 0.4 Settings UI Changes
```pascal
// Pythia.SettingsForm - Add GitHub section
[GitHub Copilot]
☑ Use GitHub Copilot (recommended)
   Status: ✓ Authenticated as tmx@kinecis.com
   [Sign Out] [Reconnect]

[API Keys] (optional - for direct API access)
   OpenAI Key: [......]
   Anthropic Key: [......]
```

**Fallback**: If GitHub auth fails, fall back to API keys.

---

## 1. Core Concept: How Copilot Works

**Key Components**:
1. **Context Gathering**: Read current file + open tabs + project structure
2. **Prompt Engineering**: Inject context into AI request (6K character window)
3. **File Manipulation**: `ITextFileService.write()`, `WorkspaceEdit` (line/column ranges), diff engine
4. **Undo System**: Built-in transaction support

---

## 2. Delphi ToolsAPI Equivalent

### 2.1 Available Interfaces

Delphi's **ToolsAPI** (in `ToolsAPI.pas`) provides comprehensive IDE automation. Key interfaces:

#### **File Access**
```pascal
IOTAModuleServices      // Access to open modules (units/forms)
  - GetModuleCount
  - GetModule(Index)
  - FindModule(FileName)
  - OpenFile(FileName): IOTAModule

IOTAModule              // Represents an open file
  - GetFileName: string
  - GetModuleFileCount: Integer
  - GetModuleFileEditor(Index): IOTAEditor
  - Close
  
IOTASourceEditor        // Access to source code editor
  - GetEditViewCount: Integer
  - GetEditView(Index): IOTAEditView
  - SetBlockIndent(Value: Integer)  // Formatting
  
IOTAEditView            // Visual editor window
**Critical Interfaces**:
```pascal
IOTAModuleServices.CurrentModule.GetModuleFileEditor(0)
  → IOTASourceEditor.GetEditView(0)
    → IOTAEditView.Buffer  // IOTAEditBuffer
      → CreateReader().GetText()  // Read
      → CreateUndoableWriter()    // Write

IOTAEditorServices.BeginUndo/EndUndo  // Group operations
IOTAProject.GetModuleFileCount        // Project files
```

**Capabilities vs VS Code**:pas)                            │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│            Agent Command Parser (NEW)                        │
│  Detects intent: "edit X", "read Y", "create Z"             │
│  → Pythia.Agent.Parser.pas                                   │
└────────────┬────────────────────────────────────────────────┘
             │
       ┌─────┴──────┬──────────────┬──────────────┐
       ▼            ▼              ▼              ▼
┌──────────┐ ┌─────────────┐ ┌────────────┐ ┌──────────────┐
│  Read    │ │   Edit      │ │  Create/   │ │   Search     │
│  Agent   │ │   Agent     │ │  Delete    │ │   Agent      │
│          │ │             │ │  Agent     │ │              │
└────┬─────┘ └──────┬──────┘ └─────┬──────┘ └──────┬───────┘
     │              │              │               │
     └──────┬───────┴──────┬───────┴───────┬───────┘
            ▼              ▼               ▼
┌─────────────────────────────────────────────────────────────┐
│           ToolsAPI Wrapper (NEW)                             │
│  Pythia.IDE.Wrapper.pas - Abstracts ToolsAPI complexity      │
│  - GetOpenFiles(), ReadFile(), ApplyDiff(), CreateFile()     │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│           Delphi ToolsAPI (Built-in)                         │
│  IOTAModuleServices, IOTAEditBuffer, IOTAEditWriter          │
└─────────────────────────────────────────────────────────────┘
```

### 3.2 New Units Required

#### **Pythia.Agent.Parser.pas**
- Parse AI responses for action directives
- Detect code blocks with file paths: ` ```pascal:src/Unit1.pas `
- Extract diff markers: `<<<<<<< ORIGINAL`, `=======`, `>>>>>>> MODIFIED`
- Convert to structured commands: `TAgentCommand` record

#### **Pythia.Agent.Executor.pas**  
- Execute parsed commands
- Handle errors (file not found, read-only, etc.)
- Provide feedback to chat window
- Manage undo grouping for multi-step operations

#### **Pythia.IDE.Wrapper.pas**
- Abstract ToolsAPI complexity
- Key methods:
  ```pascal
  function GetOpenFiles: TArray<string>;
  function ReadFileContent(const FileName: string): string;
  function WriteFileContent(const FileName, Content: string): Boolean;
  function ApplyDiff(const FileName: string; const Diff: TDiffOperation): Boolean;
  function CreateNewFile(const FileName, Content, Template: string): Boolean;
  function DeleteFil| Delphi | Gap |
|---------|---------|--------|-----|
| Read file | ✅ | ✅ IOTAEditBuffer | None |
| Write file | ✅ | ✅ IOTAEditWriter | More manual |
| Apply diff | ✅ WorkspaceEdit | ❌ | **Must build** |
| Undo/Redo | ✅ | ✅ UndoManager | Must group manually |
| Multi-file atomic | ✅ | ❌ | Sequential only |

**Bottom Line**: All core features possible, but need custom diff engine
- Collect context before each AI request
- Methods:
  ```pascal
  function GetCurrentFileContext: string;           // Active editor content
  function GetOpenTabsContext: TArray<string>;      // All open files
  function GetProjectStructure: TProjectTree;       // File tree
  function GetCursorContext: TCursorInfo;           // Surrounding code at cursor
  ```
**New Units** (6 total):
1. **Pythia.GitHub.Auth.pas** - GitHub OAuth device flow
2. Pythia.IDE.Wrapper.pas
3. Pythia.Diff.Engine.pas
4. Pythia.Agent.Parser.pas
5. Pythia.Context.Gatherer.pas
6. Pythia.Agent.Executor.pas

### Example Flow: "Refactor Method"

**User Request**: `"Extract this code into a separate method"`

**Flow**:
1. Pythia.ChatForm.SendMessageToAI
   - Checks: GitHub Copilot authenticated?
   - If yes → CallGitHubCopilot()
   - If no → Prompt for GitHub login OR use API key

2. Pythia.Context.Gatherer:
   - Reads current file via IOTAEditBuffer
   - Gets cursor position, selected text
   - Builds prompt: "Current file: Unit1.pas\nSelected code: {...}\nUser: Extract method"

3. GitHub Copilot API responds with diff:
   ```pascal:Unit1.pas
   <<<<<<< ORIGINAL
   [old code]
   =======
   [refactored code]
   >>>>>>> MODIFIED
   ```

4. Pythia.Agent.Parser extracts operations
5. Pythia.Agent.Executor:
   - Confirms with user: "Apply refactoring to MyUnit.pas?"
   - Calls Pythia.IDE.Wrapper.ApplyDiff()
   
6. Pythia.Agent.Executor:
   - Confirms with user: "Apply refactoring to MyUnit.pas?"
   - Calls Pythia.IDE.Wrapper.ApplyDiff()
   
7. Pythia.IDE.Wrapper:
   - Finds IOTAModule for MyUnit.pas
   - Gets IOTAEditBuffer
   - Uses Pythia.Diff.Engine to calculate exact positions
   - Calls IOTAEditWriter.CopyTo/DeleteTo/Insert
   - Refreshes editor view
   
8. Shows success message in chat
```

### 4.2 Example: "Add logging to all methods"

**User**: `"Add try/except logging to every method in this unit"`

**Flow** (Multi-step):
```
1. Context gathering:
   - Read entire unit via IOTAEditBuffer.GetText()
   - Parse method declarations (simple regex for "procedure|function")

2. AI generates multiple diff blocks:
   ```pascal:MyUnit.pas:Line45
   [diff for Method1]
   ```
   ```pascal:MyUnit.pas:Line120  
   [diff for Method2]
   ```

3. Agent.Executor:
   - Groups all edits into single undo transaction
   - Applies in reverse order (bottom to top) to preserve line numbers
   - If any fail, rolls back entire operation

4. Reports: "Added logging to 5 methods"
```
1. **Pythia.IDE.Wrapper.pas** - ToolsAPI abstraction
   ```pascal
   function ReadFile(FileName: string): string;
   function ApplyDiff(FileName: string; Diff: TDiffOp): Boolean;
   function GetOpenFiles: TArray<string>;
   ```

2. **Pythia.Diff.Engine.pas** - Calculate edit operations
   ```pascal
   function ComputeDiff(OldText, NewText: string): TArray<TEditOp>;
   // TEditOp = record LineNum, DeleteCount: Integer; InsertText: string; end
   ```

3. **Pythia.Agent.Parser.pas** - Parse AI responses for file operations
   ```pascal
   // Detect: ```pascal:Unit1.pas\n<<<<<<< ORIGINAL\n...\n=======\n...\n>>>>>>>
   ```

4. **Pythia.Context.Gatherer.pas** - Build AI prompts with context
   ```pascal
   function BuildPrompt(UserMsg: string): string;  // Injects current file + open files
   ```

5. **Pythia.Agent.Executor.pas** - Execute operations safely
   ```pascal
   procedure ExecuteEdit(FileOp: TFileOperation);  // With confirmation + undo grouping
 type
  TAPIProvider = (apOpenAI, apAnthropic, apGitHubModels);
  
  TPythiaConfig = class
    class function GetGitHubToken: string;
    class procedure SetGitHubToken(const Value: string);
    class function GetPreferredProvider: TAPIProvider;
  end;
```

**Pythia.AI.Client.pas**:
```pascal
class function TPythiaAIClient.SendMessage(
  const AMessages: TArray<TChatMessage>; 
  const AModel: string
): string;
begin
  if AModel.StartsWith('GitHub:') then
    ReExample Flow: "Add Error Handling"

**User**: `"Add try/except to this method"`

1. **Context.Gatherer** reads current file via IOTAEditBuffer
2. **AI.Client** sends prompt: `"Current code: [method]... Add try/except..."`
3. **AI responds**:
   ````pascal:Unit1.pas
   <<<<<<< ORIGINAL
   procedure TForm1.ButtonClick;
   begin
     ShowMessage('Hi');
   end;
   =======
   procedure TForm1.ButtonClick;
   begin
     try
       ShowMessage('Hi');
     except
       on E: Exception do LogError(E);
     end;
   end;
   >>>>>>> MODIFIED
   ````
4. **Agent.Parser** extracts TDiffOperation(OriginalText, ModifiedText)
5. **Diff.Engine** calculates: `Line 10, Delete 3 lines, Insert 7 lines`
6. **IDE.Wrapper** applies:
   ```pascal
   Writer := Buffer.CreateUndoableWriter;
   Writer.CopyTo(LineToPosition(10));
   Writer.DeleteTo(LineToPosition(13));
   Writer.Insert(PAnsiChar(AnsiString(NewCode)));
   ```
7. Show: `"✓ Applied to Unit1.pas"  {
      "type": "edit",
      "file": "src/MyUnit.pas",
      "changes": [
        {
          "start_line": 10,
          "end_line": 13,
          "original": "...",
          "modified": "..."
        }
      ]
    }
  ]
}
```

**Recommendation**: Use **XML markers in fenced code blocks** (easier to parse with Delphi's native TXMLDocument, regex, or simple Pos() calls). Instruct AI via system prompt:

```
"When editing files, respond with:
```pascal:filename.pas
<<<<<<< ORIGINAL
[existing code to replace]
=======
[new code]
>>>>>>> MODIFIED
```
Always include file path and enough context (3-5 lines) around changes."
```

### 6.2 Context Injection Strategy

**Prompt Template**:
```
SYSTEM: You are Pythia, expert Delphi/Object Pascal programming assistant integrated into Embarcadero RAD Studio IDE.

WORKSPACE CONTEXT:
- Current File: {current_file_path}
- Cursor Position: Line {line}, Col {col}
- Open Files: {open_files_list}
- Project: {project_name} ({file_count} files)

CURRENT FILE CONTENT (lines {start}-{end}):
```pascal
{file_content_snippet}
```

USER REQUEST: {user_message}

INSTRUCTIONS:
- For code edits, respond with edit blocks showing ORIGINAL and MODIFIED code
- Include line numbers or surrounding context for precise matching  
- For multi-file changes, provide separate blocks per file
- Explain your changes briefly
```

---

## 7. Implementation Phases

### Phase 1: GitHub Copilot Integration (1 week) **PRIORITY**
**Goal**: Replace API keys with GitHub Copilot authentication

**Deliverables**:
1. `Pythia.GitHub.Auth.pas` with OAuth device flow
2. Update `Pythia.AI.Client.pas` to call GitHub Copilot API
3. Settings form: "Sign in with GitHub" button
4. Model dropdown: "GitHub Copilot: GPT-4" as default
5. Token caching in encrypted registry storage

**Test**: Send chat message using free GitHub Copilot tier

---

### Phase 2: Read-Only Context (2-3 weeks)
**Goal**: AI can see code, give advice, but not edit yet

**Deliverables**:
1. `Pythia.IDE.Wrapper.pas` with read-only methods: (FREE AI)

**Add as 3rd API provider** alongside OpenAI/Anthropic:

```pascal
// Pythia.AI.Client.pas
if Model.StartsWith('GitHub:') then
  CallGitHubModels()  // https://models.inference.ai.azure.com/chat/completions
```

**Why**: Same models as Copilot (Claude 3.5 Sonnet, GPT-4), FREE tier, no credit card.

**Model Dropdown**:
```
☑ GitHub: Claude 3.5 Sonnet  ← Default (FREE)
  GitHub: GPT-4o
  GitHub: Llama 3.3
  ---
  OpenAI: GPT-4 ($$)
  Anthropic: Claude 3.5 ($$$)    - Form2.pas: 5 edits
        ...
        
        [Show Details] [Apply All] [Cancel]
```

**After user clicks "Apply All"**:
```
Applying changes... ✓ Form1.pas
                    ✓ Form2.pas  
                    ✗ Form3.pas (read-only)
                    
14/14 files processed (1 skipped)
All changes can be undone via Edit > Undo.
```

---

## 9. Performance Considerations

### 9.1 Context Size Limits

- GitHub Models: 128K token context (Claude 3.5), ~96K characters
- Must truncate large files: Send only ±500 lines around cursor
- For full-file analysis: Process in chunks, summarize

### 9.2 IDE Responsiveness

- **Problem**: IOTAEditBuffer.GetText() blocks UI thread
- **Solution**: 
  ```pascal
  TTask.Run(procedure
  begin
    var Content := ReadFileContent(FileName);
    TThread.Synchronize(nil, procedure
    begin
      SendToAI(Content);
    end);
  end);
  ```

### 9.3 Caching Strategy

Cache recent file contents to avoid re-reading:
```pascal
type
  TFileCache = class
    FCache: TDictionary<string, record Content: string; Timestamp: TDateTime; end>;
    function GetContent(const FileName: string): string; // Check cache first
    procedure Invalidate(const FileName: string);        // On file change
  end;
```

---AI Protocol

**System Prompt** (instruct AI to use diff format):
```
You are Pythia. When editing code, use:

```pascal:Unit1.pas
<<<<<<< ORIGINAL
[code to replace]
=======
[new code]
>>>>>>> MODIFIED
```

**Context Injection**:
```
Current File: Unit1.pas (250 lines)
Open Files: MainForm.pas, DataModule.pas
Cursor: Line 45

FILE CONTENT (lines 40-50):
[code snippet]

USER: {user message}
```

---

## 13. Implementation Roadmap

### Phase 0: GitHub Copilot Integration (Week 1) ✨ **START HERE**
- [ ] Create Pythia.GitHub.Auth.pas with OAuth device flow
- [ ] Update Pythia.AI.Client.pas to call Copilot Chat API
- [ ] Add "Sign in with GitHub" to settings form
- [ ] Test: Chat works without API keys
- **Checkpoint**: `git tag checkpoint-github-copilot`

### Phase 1-2: Context (Weeks 2-4)
   - [ ] Implement file reading (Pythia.IDE.Wrapper)
   - [ ] Inject current file content into AI prompts
   - [ ] Test: "Explain this code" with context

### Month 1 Goal:
   - [ ] Complete Phase 0-2: GitHub Copilot working + AI can see code
   - [ ] User can ask questions with full file context

### Month 2-3 Goal:
   - [ ] Phase 3: AI can edit files (Diff.Engine, Agent.Parser, Agent.Executor)
   - [ ] "Add logging", "Refactor method" work end-to-end

---

## 14. Open Questions

1. **Diff Algorithm**: Use line-based (simpler) or character-based (more precise)?
   - **Recommendation**: Start with line-based (Myers), add fuzzy matching later

2. **Encoding**: How to handle UTF-8, UTF-16, ANSI?
   - **Recommendation**: Detect BOM, default to UTF-8 for new files

3. **Streaming Responses**: Should AI responses stream token-by-token?
   - **Recommendation**: Phase 1 = no streaming (simpler), Phase 5 = add streaming for better UX

4. **Error Recovery**: If edit fails mid-operation, how to rollback?
   - **Recommendation**: Wrap in IOTAEditServices.BeginUndo...EndUndo group

5. **Testing**: Can we automate IDE interaction tests?
   - **Recommendation**: Manual for MVP, investigate IDE automation tools (e.g., TestComplete) later

---

## 15. Conclusion

**Feasibility**: ✅ **100% possible** with Delphi's ToolsAPI

**Effort**: 3-4 months for full agentic capabilities (MVP in 6-8 weeks)

**Key Innovation**: Using **GitHub Models API** solves the cost/accessibility problem - users get Claude 3.5 Sonnet for free, just like in VS Code Copilot.

**Competitive Advantage**: First true AI coding assistant for Delphi/RAD Studio. No competitors exist.

**User Value**: Transform Pythia from "chat bot" → "AI pair programmer" that actively helps write code.

---

## Appendix A: Example Prompts for Testing

### Simple Read
```
User: "What does this code do?"
Expected: AI summarizes current file's logic
```

### Simple Edit  
```
User: "Add error handling to this method"
Expected: AI responds with try/except diff block
```

### Multi-Step
```
User: "Create a CRUD interface for TCustomer"
Expected: AI generates:
1. CustomerData.pas (model)
2. CustomerForm.pas + .dfm (UI)
3. CustomerManager.paRoadmap

| Phase | Goal | Effort | Key Deliverables |
|-------|------|--------|------------------|
| 1 | Read files | 2-3 weeks | IDE.Wrapper read methods, Context.Gatherer |
| 2 | GitHub Models | 1 week | CallGitHubModels, settings UI |
| 3 | Edit files | 3-4 weeks | Diff.Engine, Agent.Parser, apply edits |
| 4 | Multi-file | 2-3 weeks | Create/delete, undo transactions |
| 5 | Advanced | 3-4 weeks | Smart context, conversation memory |

**MVP (Phases 1-3)**: 6-8 weeks → AI can read and edit files

---

## 8. Safety & Testing

**Safety**:
- Confirmation dialog before all edits
- Show diff preview
- Integrated undo (BeginUndo/EndUndo groups)

**Testing**:
```pascal
// DUnit example
Assert.AreEqual('try...except added', TestDiffEngine);
```

---

## 9. ToolsAPI Quick Reference

```pascal
// Read file
ModuleServices.CurrentModule.GetModuleFileEditor(0)
  .GetEditView(0).Buffer.CreateReader().GetText()

// Write file  
Buffer.CreateUndoableWriter()
  .CopyTo(Pos)
  .DeleteTo(EndPos)
  .Insert(PAnsiChar(NewText))

// Undo grouping
EditorServices.BeginUndo('AI Edit');
// ... edits ...
EditorServices.EndUndo;
```

---

## Conclusion

**Feasibility**: ✅ 100% possible with ToolsAPI  
**Timeline**: 6-8 weeks for MVP (read + edit files)  
**Key Innovation**: GitHub Models API = FREE Claude 3.5 Sonnet  
**Competitive Edge**: First true AI assistant for Delphi