unit Pythia.SettingsForm;

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TSettingsForm = class(TForm)
    PageControl: TPageControl;
    TabGitHub: TTabSheet;
    LabelGitHub: TLabel;
    LabelGitHubStatus: TLabel;
    LabelGitHubInfo: TLabel;
    ButtonGitHubSignIn: TButton;
    ButtonGitHubSignOut: TButton;
    TabAPI: TTabSheet;
    LabelOpenAI: TLabel;
    LabelAnthropic: TLabel;
    EditOpenAIKey: TEdit;
    EditAnthropicKey: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelInfo: TLabel;
    PanelButtons: TPanel;
    LabelConfigPath: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure LabelConfigPathClick(Sender: TObject);
    procedure ButtonGitHubSignInClick(Sender: TObject);
    procedure ButtonGitHubSignOutClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateGitHubStatus;
  public
    class function Execute: Boolean;
  end;

implementation

uses
  Pythia.Config, Pythia.GitHub.Auth;

{$R *.dfm}

class function TSettingsForm.Execute: Boolean;
var
  Form: TSettingsForm;
begin
  Form := TSettingsForm.Create(nil);
  try
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  ConfigPath: string;
begin
  Caption := 'Pythia Settings';
  ClientWidth := 720;
  ClientHeight := 520;
  Position := poOwnerFormCenter;
  BorderStyle := bsSizeable;  // Allow resizing
  
  // Setup clickable config path label
  ConfigPath := TPythiaConfig.GetConfigPath;
  LabelConfigPath.Caption := 'Config file: ' + ConfigPath;
  LabelConfigPath.Cursor := crHandPoint;
  LabelConfigPath.Font.Color := clBlue;
  LabelConfigPath.Font.Style := [fsUnderline];
  LabelConfigPath.Hint := 'Click to open config file';
  LabelConfigPath.ShowHint := True;
  
  UpdateGitHubStatus;
  LoadSettings;
end;

procedure TSettingsForm.LoadSettings;
begin
  EditOpenAIKey.Text := TPythiaConfig.GetOpenAIKey;
  EditAnthropicKey.Text := TPythiaConfig.GetAnthropicKey;
  
  // Show keys as plain text
  EditOpenAIKey.PasswordChar := #0;
  EditAnthropicKey.PasswordChar := #0;
end;

procedure TSettingsForm.SaveSettings;
begin
  TPythiaConfig.SetOpenAIKey(EditOpenAIKey.Text);
  TPythiaConfig.SetAnthropicKey(EditAnthropicKey.Text);
end;

procedure TSettingsForm.ButtonOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TSettingsForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSettingsForm.LabelConfigPathClick(Sender: TObject);
var
  ConfigPath: string;
begin
  ConfigPath := TPythiaConfig.GetConfigPath;
  if FileExists(ConfigPath) then
    OpenURL(ConfigPath)
  else
    ShowMessage('Config file not found: ' + ConfigPath);
end;

procedure TSettingsForm.UpdateGitHubStatus;
var
  Token: string;
begin
  Token := TGitHubCopilotAuth.GetAuthToken;
  
  if Token <> '' then
  begin
    LabelGitHubStatus.Caption := 'Status: Authenticated';
    LabelGitHubStatus.Font.Color := clGreen;
    LabelGitHubStatus.Font.Style := [fsBold];
    ButtonGitHubSignIn.Enabled := False;
    ButtonGitHubSignOut.Enabled := True;
  end
  else
  begin
    LabelGitHubStatus.Caption := 'Status: Not authenticated';
    LabelGitHubStatus.Font.Color := clGray;
    LabelGitHubStatus.Font.Style := [];
    ButtonGitHubSignIn.Enabled := True;
    ButtonGitHubSignOut.Enabled := False;
  end;
end;

procedure TSettingsForm.ButtonGitHubSignInClick(Sender: TObject);
var
  DeviceCode, UserCode, VerificationUri: string;
  AuthResult: TGitHubAuthResult;
begin
  try
    // Start OAuth device flow
    if not TGitHubCopilotAuth.StartDeviceFlow(DeviceCode, UserCode, VerificationUri) then
    begin
      ShowMessage('Failed to start GitHub authentication. Please check your internet connection.');
      Exit;
    end;
    
    // Show user code and open browser
    ShowMessage(Format('Please visit: %s'#13#10#13#10'And enter code: %s'#13#10#13#10 +
      'Click OK after authorizing in your browser.', [VerificationUri, UserCode]));
    
    // Open browser
    OpenURL(VerificationUri);
    
    // Poll for token
    AuthResult := TGitHubCopilotAuth.PollForToken(DeviceCode);
    
    if AuthResult.Success then
    begin
      ShowMessage('Successfully authenticated with GitHub!');
      UpdateGitHubStatus;
    end
    else
      ShowMessage('Authentication failed: ' + AuthResult.ErrorMessage);
      
  except
    on E: Exception do
      ShowMessage('Error during GitHub sign-in: ' + E.Message);
  end;
end;

procedure TSettingsForm.ButtonGitHubSignOutClick(Sender: TObject);
begin
  TGitHubCopilotAuth.ClearAuth;
  UpdateGitHubStatus;
  ShowMessage('Signed out from GitHub');
end;

end.





