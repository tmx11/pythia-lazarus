unit Pythia.AI.Client;

{$mode delphi}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, fphttpclient, openssl, opensslsockets;

type
  TChatMessage = record
    Role: string;
    Content: string;
    Timestamp: TDateTime;
  end;

  TPythiaAIClient = class
  private
    class function BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model, Context: string): string;
    class function BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model, Context: string): string;
    class function BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string; const Context: string = ''): string;
    class function CallOpenAI(const RequestBody: string): string;
    class function CallAnthropic(const RequestBody: string): string;
    class function CallGitHubCopilot(const RequestBody: string): string;
    class function ParseOpenAIResponse(const Response: string): string;
    class function ParseAnthropicResponse(const Response: string): string;
  public
    class function SendMessage(const Messages: TArray<TChatMessage>; const Model: string): string;
    class function SendMessageWithContext(const Messages: TArray<TChatMessage>; 
      const Model: string; const Context: string): string;
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

class function TPythiaAIClient.SendMessage(const Messages: TArray<TChatMessage>;
  const Model: string): string;
var
  RequestBody: string;
  Response: string;
begin
  Result := '';
  
  // Initialize SSL on first use
  InitializeSSL;
  
  try
    // Determine which API to use based on model name
    if Pos('COPILOT', UpperCase(Model)) > 0 then
    begin
      // GitHub Copilot API (FREE!)
      RequestBody := BuildGitHubCopilotRequest(Messages, Model);
      Response := CallGitHubCopilot(RequestBody);
      Result := ParseOpenAIResponse(Response); // Same format as OpenAI
    end
    else if Pos('GPT', UpperCase(Model)) > 0 then
    begin
      // OpenAI API
      RequestBody := BuildOpenAIRequest(Messages, Model, '');
      Response := CallOpenAI(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('CLAUDE', UpperCase(Model)) > 0 then
    begin
      // Anthropic API
      RequestBody := BuildAnthropicRequest(Messages, Model, '');
      Response := CallAnthropic(RequestBody);
      Result := ParseAnthropicResponse(Response);
    end;
  except
    on E: Exception do
    begin
      // Provide user-friendly error messages
      if Pos('429', E.Message) > 0 then
        Result := 'Error: Rate limit exceeded. Please wait a moment and try again. ' +
                  'You may have exceeded your API quota or made too many requests.'
      else if Pos('401', E.Message) > 0 then
        Result := 'Error: Invalid API key. Please check your settings.'
      else if Pos('403', E.Message) > 0 then
        Result := 'Error: Access forbidden. Check your API key permissions.'
      else
        Result := 'Error: ' + E.Message;
    end;
  end;
end;

class function TPythiaAIClient.BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model, Context: string): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
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
    
    MsgArray := TJSONArray.Create;
    
    // Add system message with file editing instructions
    MsgObj := TJSONObject.Create;
    MsgObj.Add('role', 'system');
    if Context <> '' then
      MsgObj.Add('content', 'You are Pythia, an expert Delphi programming assistant. ' +
        'Help users with Delphi code, explain concepts, debug issues, and provide best practices.' + #13#10 +
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
      MsgObj.Add('content', 'You are Pythia, an expert Delphi programming assistant. ' +
        'Help users with Delphi code, explain concepts, debug issues, and provide best practices.' + #13#10 +
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
    
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model, Context: string): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
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
    if Context <> '' then
      JSON.Add('system', 'You are Pythia, an expert Delphi programming assistant. ' +
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
        'If adding comments, include original code + comment in newText. Lines are 1-indexed.' + #13#10#13#10 +
        'CURRENT IDE CONTEXT:' + #13#10 + Context)
    else
      JSON.Add('system', 'You are Pythia, an expert Delphi programming assistant. ' +
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
    
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string; const Context: string = ''): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
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

class function TPythiaAIClient.ParseOpenAIResponse(const Response: string): string;
var
  JSON: TJSONData;
  ChoicesArray: TJSONArray;
  Choice, Message: TJSONObject;
  ErrMsg: string;
begin
  Result := '';
  try
    JSON := GetJSON(Response);
    try
      if JSON is TJSONObject then
      begin
        ChoicesArray := TJSONObject(JSON).Get('choices', TJSONArray(nil));
        if Assigned(ChoicesArray) and (ChoicesArray.Count > 0) then
        begin
          Choice := ChoicesArray.Objects[0];
          Message := Choice.Get('message', TJSONObject(nil));
          if Assigned(Message) then
            Result := Message.Get('content', '');
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      ErrMsg := 'JSON Parse Error: ' + E.Message + #13#10 + 'Response: ';
      if Length(Response) > 400 then
        ErrMsg := ErrMsg + Copy(Response, 1, 400) + '...'
      else
        ErrMsg := ErrMsg + Response;
      raise Exception.Create(ErrMsg);
    end;
  end;
end;

class function TPythiaAIClient.ParseAnthropicResponse(const Response: string): string;
var
  JSON: TJSONData;
  ContentArray: TJSONArray;
  ContentItem: TJSONObject;
  ErrMsg: string;
begin
  Result := '';
  try
    JSON := GetJSON(Response);
    try
      if JSON is TJSONObject then
      begin
        ContentArray := TJSONObject(JSON).Get('content', TJSONArray(nil));
        if Assigned(ContentArray) and (ContentArray.Count > 0) then
        begin
          ContentItem := ContentArray.Objects[0];
          Result := ContentItem.Get('text', '');
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      ErrMsg := 'JSON Parse Error: ' + E.Message + #13#10 + 'Response: ';
      if Length(Response) > 400 then
        ErrMsg := ErrMsg + Copy(Response, 1, 400) + '...'
      else
        ErrMsg := ErrMsg + Response;
      raise Exception.Create(ErrMsg);
    end;
  end;
end;

class function TPythiaAIClient.SendMessageWithContext(const Messages: TArray<TChatMessage>;
  const Model: string; const Context: string): string;
var
  RequestBody: string;
  Response: string;
begin
  Result := '';

  // Initialize SSL on first use
  InitializeSSL;

  try
    // All models now support context injection
    if Pos('COPILOT', UpperCase(Model)) > 0 then
    begin
      RequestBody := BuildGitHubCopilotRequest(Messages, Model, Context);
      Response := CallGitHubCopilot(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('GPT', UpperCase(Model)) > 0 then
    begin
      RequestBody := BuildOpenAIRequest(Messages, Model, Context);
      Response := CallOpenAI(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('CLAUDE', UpperCase(Model)) > 0 then
    begin
      RequestBody := BuildAnthropicRequest(Messages, Model, Context);
      Response := CallAnthropic(RequestBody);
      Result := ParseAnthropicResponse(Response);
    end
    else
    begin
      // Fallback for unknown models
      RequestBody := BuildOpenAIRequest(Messages, Model, Context);
      Response := CallOpenAI(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end;
  except
    on E: Exception do
      Result := 'Error: ' + E.Message;
  end;
end;

end.
