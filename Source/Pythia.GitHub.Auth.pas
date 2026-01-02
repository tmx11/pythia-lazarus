unit Pythia.GitHub.Auth;

{$mode delphi}

interface

uses
  SysUtils, Classes, IniFiles, Dialogs, Forms;

type
  TGitHubAuthStatus = (asNotAuthenticated, asAuthenticating, asAuthenticated, asError);

  TGitHubAuthResult = record
    Success: Boolean;
    Token: string;
    Username: string;
    ErrorMessage: string;
  end;

  TGitHubCopilotAuth = class
  private
    class var FAuthToken: string;
    class var FCopilotToken: string;
    class var FUsername: string;
    class var FStatus: TGitHubAuthStatus;
    class var FTokenExpiresAt: TDateTime;
  public
    class function StartDeviceFlow(out DeviceCode: string; out UserCode: string; out VerificationUri: string): Boolean;
    class function PollForToken(const DeviceCode: string): TGitHubAuthResult;
    class function GetAuthToken: string;
    class function GetUsername: string;
    class function IsAuthenticated: Boolean;
    class function GetStatus: TGitHubAuthStatus;
    class procedure ClearAuth;
    class procedure LoadCachedToken;
    class procedure SaveToken(const AToken, AUsername: string);
  end;

implementation

uses
  Pythia.Config, fpjson, jsonparser, fphttpclient, opensslsockets, LCLIntf;

const
  GITHUB_CLIENT_ID = 'Iv1.b507a08c87ecfe98';
  GITHUB_DEVICE_CODE_URL = 'https://github.com/login/device/code';
  GITHUB_TOKEN_URL = 'https://github.com/login/oauth/access_token';
  GITHUB_USER_URL = 'https://api.github.com/user';
  GITHUB_COPILOT_TOKEN_URL = 'https://api.github.com/copilot_internal/v2/token';

{ TGitHubCopilotAuth }

class function TGitHubCopilotAuth.StartDeviceFlow(out DeviceCode, UserCode, VerificationUri: string): Boolean;
var
  HttpClient: TFPHTTPClient;
  RequestBody: string;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  JSON: TJSONData;
begin
  Result := False;
  FStatus := asAuthenticating;
  
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Accept', 'application/json');
    HttpClient.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    
    RequestBody := 'client_id=' + GITHUB_CLIENT_ID + '&scope=';
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      HttpClient.RequestBody := RequestStream;
      HttpClient.Post(GITHUB_DEVICE_CODE_URL, ResponseStream);
      
      JSON := GetJSON(ResponseStream.DataString);
      try
        if JSON is TJSONObject then
        begin
          DeviceCode := TJSONObject(JSON).Get('device_code', '');
          UserCode := TJSONObject(JSON).Get('user_code', '');
          VerificationUri := TJSONObject(JSON).Get('verification_uri', '');
          Result := (DeviceCode <> '') and (UserCode <> '');
          
          if Result then
            OpenURL(VerificationUri);
        end;
      finally
        JSON.Free;
      end;
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class function TGitHubCopilotAuth.PollForToken(const DeviceCode: string): TGitHubAuthResult;
var
  HttpClient: TFPHTTPClient;
  RequestBody: string;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  JSON: TJSONData;
  AccessToken: string;
begin
  Result.Success := False;
  Result.Token := '';
  Result.Username := '';
  Result.ErrorMessage := '';
  
  HttpClient := TFPHTTPClient.Create(nil);
  try
    HttpClient.AddHeader('Accept', 'application/json');
    HttpClient.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    
    RequestBody := 'client_id=' + GITHUB_CLIENT_ID + 
                   '&device_code=' + DeviceCode + 
                   '&grant_type=urn:ietf:params:oauth:grant-type:device_code';
    RequestStream := TStringStream.Create(RequestBody, TEncoding.UTF8);
    ResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      HttpClient.RequestBody := RequestStream;
      try
        HttpClient.Post(GITHUB_TOKEN_URL, ResponseStream);
        
        JSON := GetJSON(ResponseStream.DataString);
        try
          if JSON is TJSONObject then
          begin
            AccessToken := TJSONObject(JSON).Get('access_token', '');
            if AccessToken <> '' then
            begin
              Result.Token := AccessToken;
              Result.Success := True;
              FAuthToken := AccessToken;
              FStatus := asAuthenticated;
            end
            else
            begin
              Result.ErrorMessage := TJSONObject(JSON).Get('error', 'authorization_pending');
            end;
          end;
        finally
          JSON.Free;
        end;
      except
        on E: Exception do
          Result.ErrorMessage := E.Message;
      end;
    finally
      RequestStream.Free;
      ResponseStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class function TGitHubCopilotAuth.GetAuthToken: string;
begin
  if FAuthToken = '' then
    LoadCachedToken;
  Result := FAuthToken;
end;

class function TGitHubCopilotAuth.GetUsername: string;
begin
  Result := FUsername;
end;

class function TGitHubCopilotAuth.IsAuthenticated: Boolean;
begin
  Result := (FAuthToken <> '') and (FStatus = asAuthenticated);
end;

class function TGitHubCopilotAuth.GetStatus: TGitHubAuthStatus;
begin
  Result := FStatus;
end;

class procedure TGitHubCopilotAuth.ClearAuth;
begin
  FAuthToken := '';
  FCopilotToken := '';
  FUsername := '';
  FStatus := asNotAuthenticated;
  FTokenExpiresAt := 0;
end;

class procedure TGitHubCopilotAuth.LoadCachedToken;
var
  Config: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := TPythiaConfig.GetConfigPath;
  if FileExists(ConfigPath) then
  begin
    Config := TIniFile.Create(ConfigPath);
    try
      FAuthToken := Config.ReadString('GitHub', 'Token', '');
      FUsername := Config.ReadString('GitHub', 'Username', '');
      if FAuthToken <> '' then
        FStatus := asAuthenticated
      else
        FStatus := asNotAuthenticated;
    finally
      Config.Free;
    end;
  end;
end;

class procedure TGitHubCopilotAuth.SaveToken(const AToken, AUsername: string);
var
  Config: TIniFile;
  ConfigPath: string;
  ConfigDir: string;
begin
  ConfigPath := TPythiaConfig.GetConfigPath;
  ConfigDir := ExtractFileDir(ConfigPath);
  
  if not DirectoryExists(ConfigDir) then
    ForceDirectories(ConfigDir);
    
  Config := TIniFile.Create(ConfigPath);
  try
    Config.WriteString('GitHub', 'Token', AToken);
    Config.WriteString('GitHub', 'Username', AUsername);
  finally
    Config.Free;
  end;
  
  FAuthToken := AToken;
  FUsername := AUsername;
  FStatus := asAuthenticated;
end;

end.

