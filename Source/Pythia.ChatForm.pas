unit Pythia.ChatForm;

{$mode delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Pythia.AI.Client;

type
  TChatWindow = class(TForm)
    MemoChat: TMemo;
    PanelInput: TPanel;
    MemoInput: TMemo;
    ButtonSend: TButton;
    ButtonSettings: TButton;
    ComboModel: TComboBox;
    LabelModel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure MemoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FMessages: TArray<TChatMessage>;
    FIsProcessing: Boolean;
    procedure AddMessage(const Role, Content: string);
    procedure SendMessageToAI;
  public
  end;

var
  ChatWindow: TChatWindow;

implementation

uses
  Pythia.Config, Pythia.SettingsForm;

{$R *.lfm}

procedure TChatWindow.FormCreate(Sender: TObject);
begin
  Caption := 'Pythia AI Chat';
  Width := 600;
  Height := 500;
  
  // Initialize message history
  SetLength(FMessages, 0);
  
  // Setup chat display
  MemoChat.Align := alClient;
  MemoChat.ReadOnly := True;
  MemoChat.ScrollBars := ssAutoBoth;
  MemoChat.Font.Name := 'Consolas';
  MemoChat.Font.Size := 10;
  
  // Setup input panel
  PanelInput.Align := alBottom;
  PanelInput.Height := 120;
  
  // Wire up Shift+Enter handler
  MemoInput.OnKeyDown := MemoInputKeyDown;
  
  // Setup model selector
  ComboModel.Items.Clear;
  ComboModel.Items.Add('GitHub Copilot: GPT-4');
  ComboModel.Items.Add('GitHub Copilot: GPT-3.5 Turbo');
  ComboModel.Items.Add('GPT-4');
  ComboModel.Items.Add('GPT-3.5 Turbo');
  ComboModel.Items.Add('Claude 3.5 Sonnet');
  ComboModel.Items.Add('Claude 3 Opus');
  ComboModel.ItemIndex := 0;
  
  FIsProcessing := False;
  
  AddMessage('assistant', 'Hello! I''m Pythia, your AI coding assistant for Delphi. How can I help you today?');
end;

procedure TChatWindow.ButtonSettingsClick(Sender: TObject);
begin
  TSettingsForm.Execute;
end;

procedure TChatWindow.ButtonSendClick(Sender: TObject);
begin
  if FIsProcessing or (Trim(MemoInput.Text) = '') then
    Exit;
    
  SendMessageToAI;
end;

procedure TChatWindow.MemoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Enter without Shift = Send message
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0; // Consume the key
    ButtonSendClick(nil);
  end;
  // Shift+Enter = New line (default behavior, don't intercept)
end;

procedure TChatWindow.AddMessage(const Role, Content: string);
var
  Prefix: string;
  Msg: TChatMessage;
begin
  // Add to message history
  Msg.Role := Role;
  Msg.Content := Content;
  Msg.Timestamp := Now;
  SetLength(FMessages, Length(FMessages) + 1);
  FMessages[High(FMessages)] := Msg;
  
  // Display in chat
  if Role = 'user' then
    Prefix := '>>> YOU: '
  else if Role = 'assistant' then
    Prefix := '<<< AI: '
  else if Role = 'system' then
    Prefix := '=== '
  else
    Prefix := '';
  
  MemoChat.Lines.Add('');
  MemoChat.Lines.Add(Prefix + Content);
  MemoChat.Lines.Add('');
end;

procedure TChatWindow.SendMessageToAI;
var
  UserMessage: string;
  Response: string;
  ModelName: string;
begin
  FIsProcessing := True;
  ButtonSend.Enabled := False;
  try
    UserMessage := Trim(MemoInput.Text);
    ModelName := ComboModel.Text;
    
    AddMessage('user', UserMessage);
    MemoInput.Clear;
    
    Application.ProcessMessages;
    
    // Call AI client with message history
    try
      Response := TPythiaAIClient.SendMessage(FMessages, ModelName);
      
      if Response <> '' then
        AddMessage('assistant', Response)
      else
        AddMessage('assistant', 'Error: No response from AI');
    except
      on E: Exception do
        AddMessage('assistant', 'Error: ' + E.Message);
    end;
  finally
    FIsProcessing := False;
    ButtonSend.Enabled := True;
    MemoInput.SetFocus;
  end;
end;

end.

