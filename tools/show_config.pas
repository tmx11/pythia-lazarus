program show_config;

{$mode delphi}

uses
  SysUtils, Classes,
  Pythia.Config, Pythia.GitHub.Auth;

var
  ConfigPath: string;
  OpenAIKey, AnthropicKey: string;
  IsGitHubAuth: Boolean;
  ConfigFile: TextFile;
  Line: string;

begin
  WriteLn('=== Pythia Configuration ===');
  WriteLn;
  
  ConfigPath := TPythiaConfig.GetConfigPath;
  WriteLn('Config file: ', ConfigPath);
  WriteLn('File exists: ', FileExists(ConfigPath));
  WriteLn;
  
  // Check API keys
  OpenAIKey := TPythiaConfig.GetOpenAIKey;
  AnthropicKey := TPythiaConfig.GetAnthropicKey;
  IsGitHubAuth := TGitHubCopilotAuth.IsAuthenticated;
  
  WriteLn('=== API Keys Status ===');
  if OpenAIKey <> '' then
    WriteLn('OpenAI:    ', Copy(OpenAIKey, 1, 10), '...', Copy(OpenAIKey, Length(OpenAIKey) - 9, 10))
  else
    WriteLn('OpenAI:    [Not configured]');
    
  if AnthropicKey <> '' then
    WriteLn('Anthropic: ', Copy(AnthropicKey, 1, 10), '...', Copy(AnthropicKey, Length(AnthropicKey) - 9, 10))
  else
    WriteLn('Anthropic: [Not configured]');
    
  WriteLn('GitHub:    ', IsGitHubAuth);
  WriteLn;
  
  // Show config file contents
  if FileExists(ConfigPath) then
  begin
    WriteLn('=== Config File Contents ===');
    AssignFile(ConfigFile, ConfigPath);
    try
      Reset(ConfigFile);
      while not Eof(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        // Mask sensitive data
        if Pos('Key=', Line) > 0 then
          WriteLn(Copy(Line, 1, Pos('=', Line)), '***masked***')
        else if Pos('Token=', Line) > 0 then
          WriteLn(Copy(Line, 1, Pos('=', Line)), '***masked***')
        else
          WriteLn(Line);
      end;
      CloseFile(ConfigFile);
    except
      on E: Exception do
        WriteLn('Error reading config: ', E.Message);
    end;
  end;
  
  WriteLn;
end.
