unit Pythia.AI.Client;

{$mode delphi}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, fphttpclient, openssl, opensslsockets,
  Pythia.Tools, Pythia.AI.Response;

type
  TChatMessage = record
    Role: string;
    Content: string;
    Timestamp: TDateTime;
  end;

  TPythiaAIClient = class
  private
    class function BuildToolDefinitionsJSON: TJSONArray;
    class function BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model, Context: string; 
      const ToolResults: TToolResultArray = nil): string;
    class function BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model, Context: string;
      const ToolResults: TToolResultArray = nil): string;
    class function BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string; 
      const Context: string = ''; const ToolResults: TToolResultArray = nil): string;
    class function CallOpenAI(const RequestBody: string): string;
    class function CallAnthropic(const RequestBody: string): string;
    class function CallGitHubCopilot(const RequestBody: string): string;
  public
    class function SendMessage(const Messages: TArray<TChatMessage>; const Model: string): TAIResponse;
    class function SendMessageWithContext(const Messages: TArray<TChatMessage>; 
      const Model: string; const Context: string): TAIResponse;
    class function SendMessageWithToolResults(const Messages: TArray<TChatMessage>; 
      const Model: string; const ToolResults: TToolResultArray; const Context: string = ''): TAIResponse;
  end;

implementation

uses
  Pythia.Config, Pythia.GitHub.Auth;

var
  SSLInitialized: Boolean = False;

procedure InitializeSSL;
begin
  if not SSLInitialized then
  begin
    // Configure SSL library DLL names for Windows (use OpenSSL 1.1 or 3.x)
    DLLSSLName := 'libssl-1_1-x64.dll';
    DLLSSLName2 := 'libssl-3-x64.dll';
    DLLSSLName3 := 'libssl-3.dll';
    DLLUtilName := 'libcrypto-1_1-x64.dll';
    DLLUtilName2 := 'libcrypto-3-x64.dll';
    SSLInitialized := True;
  end;
end;

{ TPythiaAIClient }

class function TPythiaAIClient.BuildToolDefinitionsJSON: TJSONArray;
var
  ToolDefs: TToolDefinitionArray;
  I, J: Integer;
  ToolObj, FuncObj, ParamsObj, PropsObj, PropObj: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONArray.Create;
  
  // Get tool definitions from registry
  ToolDefs := ToolRegistry.GetToolDefinitions;
  
  // Convert to OpenAI format
  for I := 0 to High(ToolDefs) do
  begin
    ToolObj := TJSONObject.Create;
    ToolObj.Add('type', 'function');
    
    FuncObj := TJSONObject.Create;
    FuncObj.Add('name', ToolDefs[I].Name);
    FuncObj.Add('description', ToolDefs[I].Description);
    
    // Build parameters schema
    ParamsObj := TJSONObject.Create;
    ParamsObj.Add('type', 'object');
    
    PropsObj := TJSONObject.Create;
    Required := TJSONArray.Create;
    
    for J := 0 to High(ToolDefs[I].Parameters) do
    begin
      PropObj := TJSONObject.Create;
      PropObj.Add('type', ToolDefs[I].Parameters[J].ParamType);
      PropObj.Add('description', ToolDefs[I].Parameters[J].Description);
      PropsObj.Add(ToolDefs[I].Parameters[J].Name, PropObj);
      
      if ToolDefs[I].Parameters[J].Required then
        Required.Add(ToolDefs[I].Parameters[J].Name);
    end;
    
    ParamsObj.Add('properties', PropsObj);
    if Required.Count > 0 then
      ParamsObj.Add('required', Required)
    else
      Required.Free;
      
    FuncObj.Add('parameters', ParamsObj);
    ToolObj.Add('function', FuncObj);
    Result.Add(ToolObj);
  end;
end;

class function TPythiaAIClient.SendMessage(const Messages: TArray<TChatMessage>;
  const Model: string): TAIResponse;
begin
  Result := SendMessageWithContext(Messages, Model, '');
end;

class function TPythiaAIClient.SendMessageWithContext(const Messages: TArray<TChatMessage>;
  const Model: string; const Context: string): TAIResponse;
begin
  Result := SendMessageWithToolResults(Messages, Model, nil, Context);
end;

class function TPythiaAIClient.SendMessageWithToolResults(const Messages: TArray<TChatMessage>;
  const Model: string; const ToolResults: TToolResultArray; const Context: string = ''): TAIResponse;
var
  RequestBody: string;
  Response: string;
begin
  // Initialize SSL on first use
  InitializeSSL;
  
  try
    // Determine which API to use based on model name
    if Pos('COPILOT', UpperCase(Model)) > 0 then
    begin
      // GitHub Copilot API (FREE!)
      RequestBody := BuildGitHubCopilotRequest(Messages, Model, Context, ToolResults);
      Response := CallGitHubCopilot(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('GPT', UpperCase(Model)) > 0 then
    begin
      // OpenAI API
      RequestBody := BuildOpenAIRequest(Messages, Model, Context, ToolResults);
      Response := CallOpenAI(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('CLAUDE', UpperCase(Model)) > 0 then
    begin
      // Anthropic API
      RequestBody := BuildAnthropicRequest(Messages, Model, Context, ToolResults);
      Response := CallAnthropic(RequestBody);
      Result := ParseAnthropicResponse(Response);
    end;
  except
    on E: Exception do
    begin
      // Provide user-friendly error messages
      Result.HasToolCalls := False;
      SetLength(Result.ToolCalls, 0);
      
      if Pos('429', E.Message) > 0 then
        Result.Text := 'Error: Rate limit exceeded. Please wait a moment and try again. ' +
                  'You may have exceeded your API quota or made too many requests.'
      else if Pos('401', E.Message) > 0 then
        Result.Text := 'Error: Invalid API key. Please check your settings.'
      else if Pos('403', E.Message) > 0 then
        Result.Text := 'Error: Access forbidden. Check your API key permissions.'
      else
        Result.Text := 'Error: ' + E.Message;
    end;
  end;
end;

class function TPythiaAIClient.BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model, Context: string; const ToolResults: TToolResultArray): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
  ToolResult: TToolResult;
  ModelName: string;
begin
  // Map display name to API model name
  if Pos('GPT-4', Model) > 0 then
    ModelName := 'gpt-4'
  else if Pos('GPT-3.5', Model) > 0 then
    ModelName := 'gpt-3.5-turbo'
  else
    ModelName := 'gpt-4';
    
  JSON := TJSONObject.Create;
  try
    JSON.Add('model', ModelName);
    JSON.Add('temperature', 0.7);
    JSON.Add('max_tokens', 2000);
    
    // Add tools if available
    if ToolRegistry.GetToolDefinitions <> nil then
      JSON.Add('tools', BuildToolDefinitionsJSON());
    
    MsgArray := TJSONArray.Create;
    
    // Add system message
    MsgObj := TJSONObject.Create;
    MsgObj.Add('role', 'system');
    if Context <> '' then
      MsgObj.Add('content', 'You are Pythia, an expert Free Pascal and Lazarus IDE programming assistant. ' +
        'Help users with Object Pascal code in Lazarus IDE, explain concepts, debug issues, and provide best practices for Free Pascal Compiler (FPC) 3.2.2 and Lazarus 3.2.0.' + #13#10#13#10 +
        'When editing files, use this JSON format to REPLACE specific line ranges:' + #13#10 +
        '```json' + #13#10 +
        '{' + #13#10 +
        '  "edits": [' + #13#10 +
        '    {' + #13#10 +
        '      "file": "Source/MyUnit.pas",' + #13#10 +
        '      "startLine": 10,' + #13#10 +
        '      "endLine": 12,' + #13#10 +
        '      "newText": "  // Comment\n  Line11Code;\n  Line12Code;"' + #13#10 +
        '    }' + #13#10 +
        '  ]' + #13#10 +
        '}' + #13#10 +
        '```' + #13#10 +
        'CRITICAL: Lines startLine through endLine are COMPLETELY REPLACED with newText. ' +
        'If adding comments, you MUST include the original code + comment in newText. ' +
        'Lines are 1-indexed. Multiple edits allowed.' + #13#10#13#10 +
        'CURRENT IDE CONTEXT:' + #13#10 + Context)
    else
      MsgObj.Add('content', 'You are Pythia, an expert Free Pascal and Lazarus IDE programming assistant. ' +
        'Help users with Object Pascal code in Lazarus IDE, explain concepts, debug issues, and provide best practices for Free Pascal Compiler (FPC) 3.2.2 and Lazarus 3.2.0.' + #13#10#13#10 +
        'When editing files, use this JSON format to REPLACE specific line ranges:' + #13#10 +
        '```json' + #13#10 +
        '{' + #13#10 +
        '  "edits": [' + #13#10 +
        '    {' + #13#10 +
        '      "file": "Source/MyUnit.pas",' + #13#10 +
        '      "startLine": 10,' + #13#10 +
        '      "endLine": 12,' + #13#10 +
        '      "newText": "  // Comment\n  Line11Code;\n  Line12Code;"' + #13#10 +
        '    }' + #13#10 +
        '  ]' + #13#10 +
        '}' + #13#10 +
        '```' + #13#10 +
        'CRITICAL: Lines startLine through endLine are COMPLETELY REPLACED with newText. ' +
        'If adding comments, you MUST include the original code + comment in newText. ' +
        'Lines are 1-indexed. Multiple edits allowed.');
    MsgArray.Add(MsgObj);
    
    // Add conversation messages
    for Msg in Messages do
    begin
      MsgObj := TJSONObject.Create;
      MsgObj.Add('role', Msg.Role);
      MsgObj.Add('content', Msg.Content);
      MsgArray.Add(MsgObj);
    end;
    
    // Add tool result messages if any
    if Length(ToolResults) > 0 then
    begin
      for ToolResult in ToolResults do
      begin
        MsgObj := TJSONObject.Create;
        MsgObj.Add('role', 'tool');
        MsgObj.Add('tool_call_id', ToolResult.CallId);
        MsgObj.Add('content', ToolResult.Output);
        MsgArray.Add(MsgObj);
      end;
    end;
    
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model, Context: string; const ToolResults: TToolResultArray): string;
var
  JSON, MsgObj, ToolResultContent: TJSONObject;
  MsgArray, ContentArray: TJSONArray;
  Msg: TChatMessage;
  ToolResult: TToolResult;
  ModelName: string;
begin
  // Map display name to API model name
  if Pos('3.5 SONNET', UpperCase(Model)) > 0 then
    ModelName := 'claude-3-5-sonnet-20241022'
  else if Pos('OPUS', UpperCase(Model)) > 0 then
    ModelName := 'claude-3-opus-20240229'
  else
    ModelName := 'claude-3-5-sonnet-20241022';
    
  JSON := TJSONObject.Create;
  try
    JSON.Add('model', ModelName);
    JSON.Add('max_tokens', 4096);
    
    // Add tools if available (Anthropic format differs from OpenAI)
    if ToolRegistry.GetToolDefinitions <> nil then
    begin
      // TODO: Build Anthropic tool format (different schema than OpenAI)
      // For now, skip - we'll implement Anthropic tools after OpenAI works
    end;
    if Context <> '' then
      JSON.Add('system', 'You are Pythia, an expert Free Pascal and Lazarus IDE programming assistant. ' +
        'Help users with Object Pascal code in Lazarus IDE, explain concepts, debug issues, and provide best practices for Free Pascal Compiler (FPC) 3.2.2 and Lazarus 3.2.0.' + #13#10 +
        'When editing files, use this JSON format to REPLACE specific line ranges:' + #13#10 +
        '```json' + #13#10 +
        '{' + #13#10 +
        '  "edits": [' + #13#10 +
        '    {' + #13#10 +
        '      "file": "Source/MyUnit.pas",' + #13#10 +
        '      "startLine": 1,' + #13#10 +
        '      "endLine": 1,' + #13#10 +
        '      "newText": "// Header comment\nunit MyUnit;"' + #13#10 +
        '    }' + #13#10 +
        '  ]' + #13#10 +
        '}' + #13#10 +
        '```' + #13#10 +
        'CRITICAL: Lines startLine through endLine are COMPLETELY REPLACED with newText. ' +
        'Never duplicate lines - if line 1 is "unit X;", your newText should contain it ONCE. ' +
        'If adding comments, include original code + comment in newText. Lines are 1-indexed.' + #13#10#13#10 +
        'CURRENT IDE CONTEXT:' + #13#10 + Context)
    else
      JSON.Add('system', 'You are Pythia, an expert Free Pascal and Lazarus IDE programming assistant. ' +
        'Help users with Object Pascal code in Lazarus IDE, explain concepts, debug issues, and provide best practices for Free Pascal Compiler (FPC) 3.2.2 and Lazarus 3.2.0.' + #13#10 +
        'When editing files, use this JSON format to REPLACE specific line ranges:' + #13#10 +
        '```json' + #13#10 +
        '{' + #13#10 +
        '  "edits": [' + #13#10 +
        '    {' + #13#10 +
        '      "file": "Source/MyUnit.pas",' + #13#10 +
        '      "startLine": 1,' + #13#10 +
        '      "endLine": 1,' + #13#10 +
        '      "newText": "// Header comment\nunit MyUnit;"' + #13#10 +
        '    }' + #13#10 +
        '  ]' + #13#10 +
        '}' + #13#10 +
        '```' + #13#10 +
        'CRITICAL: Lines startLine through endLine are COMPLETELY REPLACED with newText. ' +
        'Never duplicate lines - if line 1 is "unit X;", your newText should contain it ONCE. ' +
        'If adding comments, include original code + comment in newText. Lines are 1-indexed.');
    
    MsgArray := TJSONArray.Create;
    
    // Add conversation messages (skip system message for Anthropic)
    for Msg in Messages do
    begin
      if Msg.Role <> 'system' then  // Skip system messages, we use separate 'system' field
      begin
        MsgObj := TJSONObject.Create;
        MsgObj.Add('role', Msg.Role);
        MsgObj.Add('content', Msg.Content);
        MsgArray.Add(MsgObj);
      end;
    end;
    
    // Add tool results if any (Anthropic uses content blocks, not role="tool")
    if Length(ToolResults) > 0 then
    begin
      // Build a user message with tool_result content blocks
      MsgObj := TJSONObject.Create;
      MsgObj.Add('role', 'user');
      
      ContentArray := TJSONArray.Create;
      for ToolResult in ToolResults do
      begin
        ToolResultContent := TJSONObject.Create;
        ToolResultContent.Add('type', 'tool_result');
        ToolResultContent.Add('tool_use_id', ToolResult.CallId);
        ToolResultContent.Add('content', ToolResult.Output);
        ContentArray.Add(ToolResultContent);
      end;
      
      MsgObj.Add('content', ContentArray);
      MsgArray.Add(MsgObj);
    end;
    
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string; const Context: string = ''; const ToolResults: TToolResultArray = nil): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
  ToolResult: TToolResult;
  ModelName: string;
  SystemPrompt: string;
begin
  // Map display name to API model name
  if Pos('GPT-4', UpperCase(Model)) > 0 then
    ModelName := 'gpt-4'
  else if Pos('GPT-3.5', UpperCase(Model)) > 0 then
    ModelName := 'gpt-3.5-turbo'
  else
    ModelName := 'gpt-4'; // Default to GPT-4
    
  JSON := TJSONObject.Create;
  try
    JSON.Add('model', ModelName);
    JSON.Add('temperature', 0.7);
    JSON.Add('max_tokens', 4096);
    
    // Add tools if available (GitHub Copilot uses OpenAI format)
    if ToolRegistry.GetToolDefinitions <> nil then
      JSON.Add('tools', BuildToolDefinitionsJSON());
    
    MsgArray := TJSONArray.Create;
    
    // Build system prompt with context if provided
    SystemPrompt := 'You are Pythia, an expert Delphi programming assistant. ' +
      'Help users with Delphi code, explain concepts, debug issues, and provide best practices.' + #13#10 +
      'When editing files, use this JSON format to REPLACE specific line ranges:' + #13#10 +
      '```json' + #13#10 +
      '{' + #13#10 +
      '  "edits": [' + #13#10 +
      '    {' + #13#10 +
      '      "file": "Source/MyUnit.pas",' + #13#10 +
      '      "startLine": 1,' + #13#10 +
      '      "endLine": 1,' + #13#10 +
      '      "newText": "// Header comment\nunit MyUnit;"' + #13#10 +
      '    }' + #13#10 +
      '  ]' + #13#10 +
      '}' + #13#10 +
      '```' + #13#10 +
      'CRITICAL: Lines startLine through endLine are COMPLETELY REPLACED with newText. ' +
      'Never duplicate lines - if line 1 is "unit X;", your newText should contain it ONCE. ' +
      'If adding comments, include original code + comment in newText. Lines are 1-indexed.';
    
    // Add context if provided
    if Context <> '' then
      SystemPrompt := SystemPrompt + #13#10#13#10 + 'CURRENT FILE CONTEXT:' + #13#10 + Context;
    
    // Add system message
    MsgObj := TJSONObject.Create;
    MsgObj.Add('role', 'system');
    MsgObj.Add('content', SystemPrompt);
    MsgArray.Add(MsgObj);
    
    // Add conversation messages
    for Msg in Messages do
    begin
      MsgObj := TJSONObject.Create;
      MsgObj.Add('role', Msg.Role);
      MsgObj.Add('content', Msg.Content);
      MsgArray.Add(MsgObj);
    end;
    
    // Add tool result messages if any (GitHub Copilot uses OpenAI format)
    if Length(ToolResults) > 0 then
    begin
      for ToolResult in ToolResults do
      begin
        MsgObj := TJSONObject.Create;
        MsgObj.Add('role', 'tool');
        MsgObj.Add('tool_call_id', ToolResult.CallId);
        MsgObj.Add('content', ToolResult.Output);
        MsgArray.Add(MsgObj);
      end;
    end;
    
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.CallOpenAI(const RequestBody: string): string;
var
  HttpClient: TFPHTTPClient;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  APIKey: string;
begin
  APIKey := TPythiaConfig.GetOpenAIKey;
  if APIKey = '' then
    raise Exception.Create('OpenAI API key not configured. Please set it in Settings.');
    
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Content-Type', 'application/json');
    HttpClient.AddHeader('Authorization', 'Bearer ' + APIKey);
    
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      try
        HttpClient.RequestBody := RequestStream;
        HttpClient.Post('https://api.openai.com/v1/chat/completions', ResponseStream);
        Result := ResponseStream.DataString;
      except
        on E: Exception do
        begin
          if Pos('401', E.Message) > 0 then
            raise Exception.Create('HTTP 401: Invalid API key. Check your OpenAI key in Settings. Response: ' + ResponseStream.DataString)
          else if Pos('429', E.Message) > 0 then
            raise Exception.Create('HTTP 429: Rate limit exceeded. Response: ' + ResponseStream.DataString)
          else
            raise;
        end;
      end;
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class function TPythiaAIClient.CallAnthropic(const RequestBody: string): string;
var
  HttpClient: TFPHTTPClient;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  APIKey: string;
begin
  APIKey := TPythiaConfig.GetAnthropicKey;
  if APIKey = '' then
    raise Exception.Create('Anthropic API key not configured');
    
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Content-Type', 'application/json');
    HttpClient.AddHeader('x-api-key', APIKey);
    HttpClient.AddHeader('anthropic-version', '2023-06-01');
    
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      HttpClient.RequestBody := RequestStream;
      HttpClient.Post('https://api.anthropic.com/v1/messages', ResponseStream);
      Result := ResponseStream.DataString;
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class function TPythiaAIClient.CallGitHubCopilot(const RequestBody: string): string;
var
  HttpClient: TFPHTTPClient;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  Token: string;
begin
  Token := TGitHubCopilotAuth.GetAuthToken;
  if Token = '' then
    raise Exception.Create('GitHub Copilot not authenticated. Please sign in with GitHub in Settings.');
    
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Content-Type', 'application/json');
    HttpClient.AddHeader('Authorization', 'Bearer ' + Token);
    HttpClient.AddHeader('Editor-Version', 'vscode/1.85.0');
    HttpClient.AddHeader('Editor-Plugin-Version', 'copilot-chat/0.11.0');
    HttpClient.AddHeader('User-Agent', 'GithubCopilot/1.0 (Lazarus/4.4.0)');
    
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      try
        HttpClient.RequestBody := RequestStream;
        HttpClient.Post('https://api.githubcopilot.com/chat/completions', ResponseStream);
        Result := ResponseStream.DataString;
      except
        on E: Exception do
        begin
          if Pos('401', E.Message) > 0 then
            raise Exception.Create('HTTP 401: GitHub authentication expired. Please sign in again in Settings.')
          else if Pos('429', E.Message) > 0 then
            raise Exception.Create('HTTP 429: Rate limit exceeded. Response: ' + ResponseStream.DataString)
          else
            raise;
        end;
      end;
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

end.
