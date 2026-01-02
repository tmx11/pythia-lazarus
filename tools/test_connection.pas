program test_connection;

{$mode delphi}

uses
  SysUtils, Classes,
  Pythia.AI.Client, Pythia.Config, Pythia.GitHub.Auth;

var
  Messages: TArray<TChatMessage>;
  Response: string;
  OpenAIKey, AnthropicKey: string;
  IsGitHubAuth: Boolean;
  TestPassed: Integer;

procedure TestAPI(const Model, DisplayName: string);
begin
  Write('Testing ', DisplayName, '... ');
  SetLength(Messages, 1);
  Messages[0].Role := 'user';
  Messages[0].Content := 'Reply with only: OK';
  Messages[0].Timestamp := Now;
  
  try
    Response := TPythiaAIClient.SendMessage(Messages, Model);
    if Pos('OK', Response) > 0 then
    begin
      WriteLn('✓ PASS');
      Inc(TestPassed);
    end
    else
      WriteLn('✗ FAIL - Unexpected response: ', Copy(Response, 1, 50));
  except
    on E: Exception do
      WriteLn('✗ FAIL - ', E.Message);
  end;
end;

begin
  WriteLn('=== Pythia API Connection Test ===');
  WriteLn;
  
  TestPassed := 0;
  
  // Check configured APIs
  OpenAIKey := TPythiaConfig.GetOpenAIKey;
  AnthropicKey := TPythiaConfig.GetAnthropicKey;
  IsGitHubAuth := TGitHubCopilotAuth.IsAuthenticated;
  
  WriteLn('Configured APIs:');
  WriteLn('  OpenAI:    ', OpenAIKey <> '');
  WriteLn('  Anthropic: ', AnthropicKey <> '');
  WriteLn('  GitHub:    ', IsGitHubAuth);
  WriteLn;
  
  WriteLn('Running tests...');
  WriteLn;
  
  // Test GitHub Copilot (if authenticated)
  if IsGitHubAuth then
  begin
    TestAPI('GitHub Copilot: GPT-4', 'GitHub Copilot GPT-4');
    TestAPI('GitHub Copilot: GPT-3.5 Turbo', 'GitHub Copilot GPT-3.5');
  end
  else
    WriteLn('Skipping GitHub Copilot (not authenticated)');
  
  // Test OpenAI (if configured)
  if OpenAIKey <> '' then
  begin
    TestAPI('GPT-4', 'OpenAI GPT-4');
    TestAPI('GPT-3.5 Turbo', 'OpenAI GPT-3.5 Turbo');
  end
  else
    WriteLn('Skipping OpenAI (no API key)');
  
  // Test Anthropic (if configured)
  if AnthropicKey <> '' then
  begin
    TestAPI('Claude 3.5 Sonnet', 'Anthropic Claude 3.5 Sonnet');
  end
  else
    WriteLn('Skipping Anthropic (no API key)');
  
  WriteLn;
  WriteLn('=== Results ===');
  WriteLn('Tests passed: ', TestPassed);
  WriteLn;
  
  if TestPassed > 0 then
    ExitCode := 0
  else
    ExitCode := 1;
end.
