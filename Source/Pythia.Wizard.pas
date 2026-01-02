unit Pythia.Wizard;

interface

uses
  LazIDEIntf, IDEWindowIntf, IDECommands, MenuIntf, Vcl.Menus;

type
  TPythiaWizard = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  private
    FMenuItem: TMenuItem;
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTAMenuWizard  
    function GetMenuText: string;
  end;

procedure Register;

implementation

uses
  SysUtils, Pythia.ChatForm;

var
  WizardIndex: Integer = -1;

{ TPythiaWizard }

function TPythiaWizard.GetIDString: string;
begin
  Result := 'Pythia.AIChat.Wizard';
end;

function TPythiaWizard.GetName: string;
begin
  Result := 'Pythia AI Chat';
end;

function TPythiaWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TPythiaWizard.GetMenuText: string;
begin
  Result := 'Pythia AI Chat...';
end;

procedure TPythiaWizard.Execute;
begin
  if not Assigned(ChatWindow) then
    ChatWindow := TChatWindow.Create(nil);
  ChatWindow.Show;
end;

procedure Register;
begin
  // Register the wizard with the IDE
  WizardIndex := (BorlandIDEServices as IOTAWizardServices).AddWizard(TPythiaWizard.Create);
end;

initialization

finalization
  // Unregister the wizard
  if WizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(WizardIndex);

end.
