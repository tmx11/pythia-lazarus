program test_github_auth;

{$mode delphi}

uses
  SysUtils, Classes,
  Pythia.GitHub.Auth, Pythia.Config;

var
  Token: string;
  IsAuth: Boolean;
begin
  WriteLn('=== GitHub Copilot Authentication Test ===');
  WriteLn;
  
  // Check if already authenticated
  IsAuth := TGitHubCopilotAuth.IsAuthenticated;
  WriteLn('Authenticated: ', IsAuth);
  
  if IsAuth then
  begin
    Token := TGitHubCopilotAuth.GetAuthToken;
    if Length(Token) > 20 then
      WriteLn('Token: ', Copy(Token, 1, 10), '...', Copy(Token, Length(Token) - 9, 10))
    else
      WriteLn('Token: ', Token);
  end
  else
  begin
    WriteLn('Not authenticated. Run interactive auth with:');
    WriteLn('  fpc test_github_signin.pas && test_github_signin');
  end;
  
  WriteLn;
  WriteLn('Config file: ', TPythiaConfig.GetConfigPath);
  WriteLn;
  
  if IsAuth then
    ExitCode := 0
  else
    ExitCode := 1;
end.
