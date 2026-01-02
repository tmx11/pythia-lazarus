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
    class function BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
    class function BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
    class function BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
    class function CallOpenAI(const RequestBody: string): string;
    class function CallAnthropic(const RequestBody: string): string;
    class function CallGitHubCopilot(const RequestBody: string): string;
    class function ParseOpenAIResponse(const Response: string): string;
    class function ParseAnthropicResponse(const Response: string): string;
  public
    class function SendMessage(const SystemPrompt, UserMessage, ConversationHistory, ModelName: string): string;
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

class function TPythiaAIClient.SendMessage(const SystemPrompt, UserMessage, ConversationHistory, ModelName: string): string;
var
  Messages: TArray<TChatMessage>;
  RequestBody, Response: string;
begin
  Result := '';
  
  // Initialize SSL on first use
  InitializeSSL;
  
  try
    // Build message array
    SetLength(Messages, 2);
    Messages[0].Role := 'system';
    Messages[0].Content := SystemPrompt;
    Messages[0].Timestamp := Now;
    Messages[1].Role := 'user';
    Messages[1].Content := UserMessage;
    Messages[1].Timestamp := Now;
    
    // Route to appropriate API
    if Pos('COPILOT', UpperCase(ModelName)) > 0 then
    begin
      RequestBody := BuildGitHubCopilotRequest(Messages, ModelName);
      Response := CallGitHubCopilot(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('GPT', UpperCase(ModelName)) > 0 then
    begin
      RequestBody := BuildOpenAIRequest(Messages, ModelName);
      Response := CallOpenAI(RequestBody);
      Result := ParseOpenAIResponse(Response);
    end
    else if Pos('CLAUDE', UpperCase(ModelName)) > 0 then
    begin
      RequestBody := BuildAnthropicRequest(Messages, ModelName);
      Response := CallAnthropic(RequestBody);
      Result := ParseAnthropicResponse(Response);
    end
    else
      Result := 'Unknown model: ' + ModelName;
  except
    on E: Exception do
      Result := 'Error: ' + E.Message;
  end;
end;

class function TPythiaAIClient.BuildOpenAIRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
  ModelName: string;
begin
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

class function TPythiaAIClient.BuildAnthropicRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
var
  JSON, MsgObj: TJSONObject;
  MsgArray: TJSONArray;
  Msg: TChatMessage;
  ModelName, SystemPrompt: string;
begin
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
    
    // Anthropic uses separate system field
    SystemPrompt := '';
    MsgArray := TJSONArray.Create;
    for Msg in Messages do
    begin
      if Msg.Role = 'system' then
        SystemPrompt := Msg.Content
      else
      begin
        MsgObj := TJSONObject.Create;
        MsgObj.Add('role', Msg.Role);
        MsgObj.Add('content', Msg.Content);
        MsgArray.Add(MsgObj);
      end;
    end;
    
    if SystemPrompt <> '' then
      JSON.Add('system', SystemPrompt);
    JSON.Add('messages', MsgArray);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

class function TPythiaAIClient.BuildGitHubCopilotRequest(const Messages: TArray<TChatMessage>; const Model: string): string;
begin
  // GitHub Copilot uses same format as OpenAI
  Result := BuildOpenAIRequest(Messages, 'gpt-4');
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
    raise Exception.Create('OpenAI API key not configured');
    
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Content-Type', 'application/json');
    HttpClient.AddHeader('Authorization', 'Bearer ' + APIKey);
    
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      HttpClient.RequestBody := RequestStream;
      HttpClient.Post('https://api.openai.com/v1/chat/completions', ResponseStream);
      Result := ResponseStream.DataString;
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
    raise Exception.Create('GitHub Copilot not authenticated. Please authenticate in Settings.');
    
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Content-Type', 'application/json');
    HttpClient.AddHeader('Authorization', 'Bearer ' + Token);
    
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      HttpClient.RequestBody := RequestStream;
      HttpClient.Post('https://api.githubcopilot.com/v1/chat/completions', ResponseStream);
      Result := ResponseStream.DataString;
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
begin
  Result := '';
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
end;

class function TPythiaAIClient.ParseAnthropicResponse(const Response: string): string;
var
  JSON: TJSONData;
  ContentArray: TJSONArray;
  ContentItem: TJSONObject;
begin
  Result := '';
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
end;

end.
