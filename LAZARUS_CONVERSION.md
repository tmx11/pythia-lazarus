# Converting Pythia from Delphi to Lazarus

## Conversion Method

Lazarus has a built-in **Convert Delphi Project** tool that handles:
- Converting .dpr/.dpk to .lpr/.lpk
- Converting .dfm forms to LCL-compatible format
- Adjusting unit dependencies
- Converting VCL to LCL components

## Steps Performed

1. **Copied project** from `D:\dev\delphi\pythia2` to `D:\dev\delphi\pythia-lazarus`
2. **Launched Lazarus** with the Delphi project file
3. **Use Tools > Convert Delphi Project to Lazarus Package**

## Lazarus IDE Converter

When Lazarus opens the .dproj file, it should offer to convert it automatically. Alternatively:

**Menu Path**: `Tools > Convert Delphi Project to Lazarus Package`

The converter will:
1. Create .lpk (Lazarus package) from .dpk (Delphi package)
2. Convert form files (.dfm) to LFM format
3. Replace VCL units with LCL equivalents
4. Create IDE integration using IDEIntf package

## Manual Changes Needed After Conversion

### 1. IDE Integration (Pythia.Register.pas)

Replace Delphi ToolsAPI with Lazarus IDEIntf:

```pascal
// OLD (Delphi):
uses
  ToolsAPI, Menus;

// NEW (Lazarus):
uses
  IDEWindowIntf, IDECommands, MenuIntf, LazIDEIntf;

// Replace registration:
// OLD:
procedure RegisterMenus;
var
  NTAServices: INTAServices;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  // ... menu code
end;

// NEW:
procedure Register;
begin
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'PythiaChat', 
    'Pythia AI Chat', nil, @ShowChatWindow, nil, 'Ctrl+Shift+P');
end;
```

### 2. Dockable Window Registration

```pascal
// Create window descriptor
var
  ViewDescriptor: TIDEWindowDescriptor;
begin
  ViewDescriptor := IDEWindowCreators.Add('PythiaChatWindow');
  ViewDescriptor.Caption := 'Pythia AI Chat';
  ViewDescriptor.Hint := 'AI-powered chat assistant';
  ViewDescriptor.CreateFormProc := @CreateChatWindow;
  ViewDescriptor.OnGetLayout := @GetLayoutEvent;
  ViewDescriptor.OnSetLayout := @SetLayoutEvent;
end;
```

### 3. HTTP Client (Pythia.AI.Client.pas)

Replace System.Net.HttpClient with fphttpclient:

```pascal
// OLD (Delphi):
uses
  System.Net.HttpClient;

var
  Client: THttpClient;
begin
  Client := THttpClient.Create;
  try
    Response := Client.Post(URL, Stream);
  finally
    Client.Free;
  end;
end;

// NEW (Lazarus):
uses
  fphttpclient, opensslsockets; // opensslsockets for HTTPS

var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    Client.RequestBody := TStringStream.Create(JSONPayload);
    Response := Client.Post(URL);
  finally
    Client.Free;
  end;
end;
```

### 4. RichEdit Control

Lazarus doesn't have TRichEdit in standard LCL. Options:
- Use TMemo (loses formatting)
- Use TSynEdit from SynEdit package (syntax highlighting)
- Use HTML control (THTMLControl)

Recommended: TMemo for simplicity, or TSynEdit for better chat display.

### 5. JSON Parsing

```pascal
// OLD (Delphi):
uses
  System.JSON;

var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
  Value := JSON.GetValue('key').Value;
end;

// NEW (Lazarus):
uses
  fpjson, jsonparser;

var
  JSON: TJSONObject;
begin
  JSON := GetJSON(Text) as TJSONObject;
  Value := JSON.Strings['key'];
end;
```

## Build Process

Once converted:

```bash
# Command-line build:
cd D:\dev\delphi\pythia-lazarus
C:\lazarus\lazbuild.exe pythia.lpk

# Or build in Lazarus IDE:
# Package > Open Package File > pythia.lpk
# Package > Compile
# Package > Install
```

## Expected Changes

The converter will create:
- `pythia.lpk` - Lazarus package file (replaces .dpk)
- `*.lfm` - Lazarus form files (replaces .dfm)
- Updated `uses` clauses with LCL units

## Testing Checklist

After conversion and manual fixes:
- [ ] Package compiles without errors
- [ ] Forms open in designer
- [ ] Menu item appears in IDE
- [ ] Window can dock in IDE
- [ ] API calls work (HTTP client)
- [ ] JSON parsing works
- [ ] Chat interface displays correctly
- [ ] Settings dialog works
- [ ] GitHub OAuth flow works

## Benefits of Lazarus Version

1. **Full docking support** - No edition restrictions
2. **Cross-platform** - Can run on Linux/macOS
3. **Open source** - No licensing issues
4. **Free** - No cost for any features
5. **Anchor Docking** - Built-in docking manager

## Known Limitations

1. **TRichEdit** not available - must use alternative
2. **Different API** - Learning curve for IDEIntf
3. **Smaller ecosystem** - Fewer examples/plugins
4. **Different debugger** - GDB instead of IDE debugger

## Next Steps

1. Wait for Lazarus converter to finish
2. Fix compilation errors (IDE integration, HTTP, JSON)
3. Test basic functionality
4. Test docking behavior
5. Compare with Delphi version
6. Decide which to maintain long-term
