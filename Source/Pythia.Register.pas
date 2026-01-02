unit Pythia.Register;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, IDEWindowIntf, MenuIntf;

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
  
  // Create form
  AForm := TChatWindow.Create(nil);
  AForm.Name := aFormName;
  
  // Handle auto-sizing - critical for docking persistence
  if DoDisableAutoSizing then
    AForm.DisableAutoSizing;
end;

procedure ShowPythiaChatWindow(Sender: TObject);
begin
  IDEWindowCreators.ShowForm('PythiaChatWindow', true);
end;

procedure Register;
begin
  // Register dockable window
  PythiaChatCreator := IDEWindowCreators.Add('PythiaChatWindow');
  PythiaChatCreator.OnCreateFormProc := @CreatePythiaChatWindow;
  PythiaChatCreator.CreateSimpleLayout;
  
  // Add to View -> IDE Internals menu
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'mnuPythiaChat', 
    'Pythia AI Chat', nil, @ShowPythiaChatWindow);
end;

end.


