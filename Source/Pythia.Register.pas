unit Pythia.Register;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, IDEWindowIntf;

procedure Register;

implementation

uses
  Pythia.ChatForm;

var
  PythiaChatCreator: TIDEWindowCreator;

procedure CreatePythiaChatWindow(Sender: TObject; aFormName: string; 
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // Only create if not already exists
  if AForm <> nil then Exit;
  
  // Create form - DO NOT assign parent/owner yet
  AForm := TChatWindow.Create(nil);
  AForm.Name := aFormName;
end;

procedure Register;
begin
  // Simple registration - no menu, just window creator
  PythiaChatCreator := IDEWindowCreators.Add('PythiaChatWindow');
  PythiaChatCreator.OnCreateFormProc := @CreatePythiaChatWindow;
  PythiaChatCreator.CreateSimpleLayout;
end;

end.


