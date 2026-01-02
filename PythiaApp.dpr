program PythiaApp;

uses
  Vcl.Forms,
  Pythia.ChatForm in 'Source\Pythia.ChatForm.pas' {ChatWindow},
  Pythia.AI.Client in 'Source\Pythia.AI.Client.pas',
  Pythia.Config in 'Source\Pythia.Config.pas',
  Pythia.GitHub.Auth in 'Source\Pythia.GitHub.Auth.pas',
  Pythia.SettingsForm in 'Source\Pythia.SettingsForm.pas' {SettingsForm},
  Pythia.Context in 'Source\Pythia.Context.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TChatWindow, ChatWindow);
  Application.Run;
end.
