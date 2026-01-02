unit Pythia.Register;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, MenuIntf, LazIDEIntf, IDEWindowIntf, IDECommands;

procedure Register;

implementation

uses
  Pythia.ChatForm;

const
  PythiaChatWindowName = 'PythiaChatWindow';

var
  PythiaChatCreator: TIDEWindowCreator;

procedure CreatePythiaChatWindow(Sender: TObject; aFormName: string; 
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if AForm = nil then
  begin
    AForm := TChatWindow.Create(nil);  // Use nil instead of Application
    AForm.Name := aFormName;
  end;
end;

procedure Register;
begin
  // Register as dockable IDE window - minimal registration
  if IDEWindowCreators <> nil then
  begin
    PythiaChatCreator := IDEWindowCreators.Add(PythiaChatWindowName);
    if PythiaChatCreator <> nil then
    begin
      PythiaChatCreator.OnCreateFormProc := @CreatePythiaChatWindow;
      PythiaChatCreator.CreateSimpleLayout;
    end;
  end;
end;

end.


