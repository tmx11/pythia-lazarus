# Pythia - AI Copilot Instructions

Pythia is a **Delphi 12 IDE plugin** that provides VS Code Copilot-style AI chat functionality. This is a **design-time package** that integrates with the IDE via ToolsAPI.

## Project Type & Build

- **Language**: Object Pascal (Delphi)
- **IDE**: Embarcadero Delphi 12 (Athens)
- **Package Type**: Design-time only (`{$DESIGNONLY}` + `{$RUNONLY}`)
- **Build**: Must be built through Delphi IDE (Shift+F9) - Community Edition does NOT support command-line compilation
- **Output**: `Win32\Debug\pythia.bpl` (binary package library)

### Build Workflow
1. Open `pythia.dproj` in Delphi 12 IDE
2. Project > Build pythia (Shift+F9)
3. Component > Install Packages > Add > select `Win32\Debug\pythia.bpl`
4. Restart IDE to activate plugin

**Critical**: Never attempt MSBuild/command-line compilation with Community Edition.

## Architecture

### Four Core Units

1. **[Pythia.Register.pas](Source/Pythia.Register.pas)** - IDE integration entry point
   - Uses `ToolsAPI` to hook into IDE's menu system
   - Registers "Pythia AI Chat..." in Tools menu with `Ctrl+Shift+P` shortcut
   - Creates singleton `TChatWindow` instance via `TPythiaMenuHandler.ShowChatWindow`

2. **[Pythia.ChatForm.pas](Source/Pythia.ChatForm.pas)** + `.dfm` - Main UI
   - `TChatWindow` form with split layout: chat display (TRichEdit) + input area (TMemo)
   - Maintains conversation history in `TArray<TChatMessage>` records
   - Model selection via ComboBox: GPT-4, GPT-3.5 Turbo, Claude 3.5 Sonnet, Claude 3 Opus
   - Key method: `SendMessageToAI` orchestrates the API call flow

3. **[Pythia.AI.Client.pas](Source/Pythia.AI.Client.pas)** - HTTP API client
   - Static class `TPythiaAIClient.SendMessage` routes to OpenAI or Anthropic based on model name
   - Builds JSON requests with system prompt: "You are Pythia, an expert Delphi programming assistant..."
   - Uses `System.Net.HttpClient` for REST calls
   - Parses JSON responses to extract assistant's text reply

4. **[Pythia.Config.pas](Source/Pythia.Config.pas)** - Configuration manager
   - Static class using `TIniFile` to persist API keys
   - Storage location: `%APPDATA%\Roaming\Pythia\pythia.ini`
   - Keys: `[API]` section with `OpenAIKey` and `AnthropicKey`

### Data Flow
```
User types in MemoInput 
→ TChatWindow.ButtonSendClick 
→ TChatWindow.SendMessageToAI 
→ TPythiaAIClient.SendMessage (routes by model name)
→ CallOpenAI/CallAnthropic (HTTP POST with THttpClient)
→ Parse JSON response
→ TChatWindow.AddMessage(assistant, response)
→ TChatWindow.DisplayMessage (updates MemoChat with formatting)
```

## Delphi-Specific Patterns

### CRITICAL RULES
- **NO CONDITIONAL COMPILATION**: Never use `{$IFDEF}`, `{$IFNDEF}`, `{$DEFINE}`, or any conditional directives
- **NO BRANCHING MACROS**: Code must be the same for all build targets (IDE plugin and standalone app)
- **Runtime Detection Only**: Use `Assigned(BorlandIDEServices)` to detect IDE vs standalone at runtime
- Both `pythia.dpk` (plugin) and `PythiaApp.dpr` (standalone) reference the **exact same source files** in `Source/`
- Any changes must work for both projects without modification

### Unit Structure
- Every file starts with `unit UnitName;` matching filename (e.g., `unit Pythia.Register;` in `Pythia.Register.pas`)
- Standard sections: `interface` (public declarations) → `implementation` (code) → optional `initialization`/`finalization`

### Memory Management
- **Manual**: Use `try...finally` with `.Free` for all object instances
- Example from [Pythia.AI.Client.pas](Source/Pythia.AI.Client.pas#L60-L70):
  ```pascal
  JSON := TJSONObject.Create;
  try
    // work with JSON
  finally
    JSON.Free;
  end;
  ```
- **Forms**: `Application.CreateForm` auto-manages, but plugin uses manual creation in [Pythia.Register.pas](Source/Pythia.Register.pas#L29)

### Naming Conventions
- Classes: `T` prefix (e.g., `TChatWindow`, `TPythiaConfig`)
- Fields: `F` prefix (e.g., `FMessages`, `FIsProcessing`)
- Parameters: `A` prefix (e.g., `ARole`, `AContent`)
- All identifiers use PascalCase

### Package Requirements
From [pythia.dpk](pythia.dpk#L33-L37):
```pascal
requires
  rtl,         // Runtime library
  designide,   // IDE integration (ToolsAPI)
  vcl,         // Visual Component Library
  vclx;        // VCL extensions
```

### IDE Integration via ToolsAPI
- Access IDE services: `BorlandIDEServices as INTAServices`
- Get main menu: `NTAServices.MainMenu`
- Find Tools menu by name: `MainMenu.Items[I].Name = 'ToolsMenu'`
- Add items with shortcuts: `MenuItemChat.ShortCut := TextToShortCut('Ctrl+Shift+P')`

## Testing & Debugging

- **No unit tests**: Manual testing via IDE after package installation
- **Debug mode**: Build in Debug configuration (default), check Messages window in IDE
- **API testing**: Open Tools > Pythia AI Chat, send test message to verify keys work
- **Log errors**: Check exception messages in chat window (API client wraps errors in `try...except`)

## Common Tasks

### Adding a new API model
1. Update model dropdown in [Pythia.ChatForm.pas](Source/Pythia.ChatForm.pas#L83-L87) `FormCreate`
2. Add model name mapping in [Pythia.AI.Client.pas](Source/Pythia.AI.Client.pas) `BuildOpenAIRequest` or `BuildAnthropicRequest`

### Changing system prompt
Edit both `BuildOpenAIRequest` and `BuildAnthropicRequest` in [Pythia.AI.Client.pas](Source/Pythia.AI.Client.pas#L92-L93)

### Modifying UI layout
1. Open [Pythia.ChatForm.dfm](Source/Pythia.ChatForm.dfm) in Delphi Form Designer
2. Visual editing creates `.dfm` binary format
3. Text version viewable via "View as Text" in IDE

## External Dependencies

- **OpenAI API**: `https://api.openai.com/v1/chat/completions` (models: gpt-4, gpt-3.5-turbo)
- **Anthropic API**: `https://api.anthropic.com/v1/messages` (models: claude-3-5-sonnet-20241022, claude-3-opus-20240229)
- **No third-party libraries**: Uses only built-in Delphi RTL/VCL units

## Key Constraints

- **Community Edition limitation**: No command-line builds - IDE only
- **Windows only**: VCL is Windows-specific
- **32-bit default**: Output in `Win32\Debug\` (can be changed in project options)
- **Single instance**: `ChatWindow` variable is global singleton in [Pythia.ChatForm.pas](Source/Pythia.ChatForm.pas#L47)
