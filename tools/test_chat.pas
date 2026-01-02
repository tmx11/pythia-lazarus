program test_chat;

{$mode delphi}

uses
  SysUtils, Classes,
  Pythia.AI.Client, Pythia.Config, Pythia.GitHub.Auth;

var
  Messages: TArray<TChatMessage>;
  Response: string;
  Model: string;

procedure ShowUsage;
begin
  WriteLn('Usage: test_chat <model> <message>');
  WriteLn;
  WriteLn('Models:');
  WriteLn('  copilot-gpt4     - GitHub Copilot: GPT-4 (FREE)');
  WriteLn('  copilot-gpt35    - GitHub Copilot: GPT-3.5 Turbo');
  WriteLn('  gpt4             - OpenAI GPT-4');
  WriteLn('  gpt35            - OpenAI GPT-3.5 Turbo');
  WriteLn('  claude-sonnet    - Claude 3.5 Sonnet');
  WriteLn('  claude-opus      - Claude 3 Opus');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  test_chat copilot-gpt4 "What is Delphi?"');
  WriteLn('  test_chat gpt4 "Explain Free Pascal"');
  Halt(1);
end;

begin
  WriteLn('=== Pythia AI Chat Test ===');
  WriteLn;
  
  if ParamCount < 2 then
    ShowUsage;
  
  // Map friendly names to API model names
  case LowerCase(ParamStr(1)) of
    'copilot-gpt4': Model := 'GitHub Copilot: GPT-4';
    'copilot-gpt35': Model := 'GitHub Copilot: GPT-3.5 Turbo';
    'gpt4': Model := 'GPT-4';
    'gpt35': Model := 'GPT-3.5 Turbo';
    'claude-sonnet': Model := 'Claude 3.5 Sonnet';
    'claude-opus': Model := 'Claude 3 Opus';
  else
    WriteLn('Unknown model: ', ParamStr(1));
    ShowUsage;
  end;
  
  WriteLn('Model: ', Model);
  WriteLn('Message: ', ParamStr(2));
  WriteLn;
  
  // Build message array
  SetLength(Messages, 1);
  Messages[0].Role := 'user';
  Messages[0].Content := ParamStr(2);
  Messages[0].Timestamp := Now;
  
  WriteLn('Sending request...');
  WriteLn;
  
  try
    Response := TPythiaAIClient.SendMessage(Messages, Model);
    
    WriteLn('=== Response ===');
    WriteLn(Response);
    WriteLn;
    
    ExitCode := 0;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
