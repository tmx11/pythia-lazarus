unit Pythia.ChatForm;

{$mode delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TChatWindow = class(TForm)
    MemoChat: TMemo;
    PanelInput: TPanel;
    MemoInput: TMemo;
    ButtonSend: TButton;
    ComboModel: TComboBox;
    LabelModel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure MemoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FIsProcessing: Boolean;
    procedure AddMessage(const Role, Content: string);
    procedure SendMessageToAI;
  public
  end;

var
  ChatWindow: TChatWindow;

implementation

uses
  Pythia.AI.Client, Pythia.Config;

{$R *.lfm}

procedure TChatWindow.FormCreate(Sender: TObject);
begin
  Caption := 'Pythia AI Chat';
  Width := 600;
  Height := 500;
  
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
  ComboModel.Items.Add('GPT-4');
  ComboModel.Items.Add('GPT-3.5 Turbo');
  ComboModel.Items.Add('Claude 3.5 Sonnet');
  ComboModel.Items.Add('Claude 3 Opus');
  ComboModel.ItemIndex := 0;
  
  FIsProcessing := False;
  
  AddMessage('system', 'Pythia AI Chat - Ready! (Shift+Enter to send)');
end;

procedure TChatWindow.ButtonSendClick(Sender: TObject);
begin
  if FIsProcessing or (Trim(MemoInput.Text) = '') then
    Exit;
    
  SendMessageToAI;
end;

procedure TChatWindow.MemoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Shift+Enter sends the message
  if (Key = VK_RETURN) and (ssShift in Shift) then
  begin
    Key := 0; // Consume the key
    ButtonSendClick(nil);
  end;
end;

procedure TChatWindow.AddMessage(const Role, Content: string);
var
  Prefix: string;
begin
  if Role = 'user' then Prefix := '>>> YOU: ' else if Role = 'assistant' then Prefix := '<<< AI: ' else if Role = 'system' then Prefix := '=== ' else Prefix := '';
  
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
    
    // Call AI client (currently stubbed)
    Response := TPythiaAIClient.SendMessage(
      'You are Pythia, an expert Delphi programming assistant.',
      UserMessage,
      '',
      ModelName
    );
    
    AddMessage('assistant', Response);
  finally
    FIsProcessing := False;
    ButtonSend.Enabled := True;
    MemoInput.SetFocus;
  end;
end;

end.

