unit Pythia.ChatForm;

{$mode delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Process, Pythia.AI.Client;

const
  PYTHIA_VERSION = 'v1.0.3-20260102';

type
  TChatWindow = class(TForm)
    MemoChat: TMemo;
    PanelStatus: TPanel;
    LabelVersion: TLabel;
    LabelGitBranch: TLabel;
    LabelCurrentFile: TLabel;
    LabelStats: TLabel;
    ButtonRefreshContext: TButton;
    PanelInput: TPanel;
    MemoInput: TMemo;
    ButtonSend: TButton;
    ButtonSettings: TButton;
    ComboModel: TComboBox;
    LabelModel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure ButtonRefreshContextClick(Sender: TObject);
    procedure MemoInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FMessages: TArray<TChatMessage>;
    FIsProcessing: Boolean;
    FTotalTokens: Integer;
    procedure AddMessage(const Role, Content: string);
    procedure SendMessageToAI;
    procedure UpdateStatusBar;
    procedure UpdateGitBranch;
    procedure UpdateCurrentFile;
    procedure UpdateStats;
    function GetGitBranch: string;
    function EstimateTokens(const Text: string): Integer;
    function RenderMarkdown(const Text: string): string;
  public
  end;

var
  ChatWindow: TChatWindow;

implementation

uses
  Pythia.Config, Pythia.SettingsForm, Pythia.Context, Pythia.FileEdit;

{$R *.lfm}

procedure TChatWindow.FormCreate(Sender: TObject);
begin
  Caption := 'Pythia AI Chat ' + PYTHIA_VERSION;
  Width := 600;
  Height := 500;
  
  // Initialize state
  SetLength(FMessages, 0);
  FTotalTokens := 0;
  
  // Setup status panel
  PanelStatus.Align := alTop;
  PanelStatus.Height := 50;
  
  // Update all status elements
  UpdateStatusBar;
  
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
  UpdateStats;
end;

procedure TChatWindow.UpdateStatusBar;
begin
  LabelVersion.Caption := 'Pythia ' + PYTHIA_VERSION;
  UpdateGitBranch;
  UpdateCurrentFile;
  UpdateStats;
end;

function TChatWindow.GetGitBranch: string;
var
  Process: TProcess;
  Output: string;
  BytesRead: Integer;
  Buffer: array[0..2047] of Byte;
begin
  Result := '(unknown)';
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'git';
    Process.Parameters.Add('rev-parse');
    Process.Parameters.Add('--abbrev-ref');
    Process.Parameters.Add('HEAD');
    Process.Options := [poUsePipes, poNoConsole];
    try
      Process.Execute;
      if Process.Running then
        Process.WaitOnExit;
      
      if Process.ExitStatus = 0 then
      begin
        BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          SetString(Output, PChar(@Buffer[0]), BytesRead);
          Result := Trim(Output);
        end;
      end;
    except
      // Git not available or not a git repo
    end;
  finally
    Process.Free;
  end;
end;

procedure TChatWindow.UpdateGitBranch;
var
  Branch: string;
begin
  Branch := GetGitBranch;
  LabelGitBranch.Caption := 'Branch: ' + Branch;
end;

procedure TChatWindow.UpdateCurrentFile;
var
  ContextProvider: IContextProvider;
  Context: TContextItem;
begin
  ContextProvider := GetContextProvider;
  if Assigned(ContextProvider) and ContextProvider.IsAvailable then
  begin
    Context := ContextProvider.GetCurrentFile;
    if Context.FilePath <> '' then
      LabelCurrentFile.Caption := 'File: ' + ExtractFileName(Context.FilePath)
    else
      LabelCurrentFile.Caption := 'File: (none)';
  end
  else
    LabelCurrentFile.Caption := 'File: (none)';
end;

function TChatWindow.EstimateTokens(const Text: string): Integer;
begin
  // Rough estimate: ~4 characters per token for English
  Result := Length(Text) div 4;
end;

procedure TChatWindow.UpdateStats;
var
  MessageCount: Integer;
  TotalChars: Integer;
  I: Integer;
begin
  MessageCount := Length(FMessages);
  TotalChars := 0;
  
  for I := 0 to MessageCount - 1 do
    TotalChars := TotalChars + Length(FMessages[I].Content);
  
  FTotalTokens := EstimateTokens(IntToStr(TotalChars));
  
  LabelStats.Caption := Format('Messages: %d | Est. Tokens: ~%d | Chars: %d', 
    [MessageCount, FTotalTokens, TotalChars]);
end;

function TChatWindow.RenderMarkdown(const Text: string): string;
var
  I: Integer;
  InCodeBlock: Boolean;
  Line, Output: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    Output := '';
    InCodeBlock := False;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      
      // Code blocks
      if (Length(Line) >= 3) and (Copy(Line, 1, 3) = '```') then
      begin
        InCodeBlock := not InCodeBlock;
        if InCodeBlock then
          Output := Output + '  --- CODE ---' + #13#10
        else
          Output := Output + '  -----------' + #13#10;
        Continue;
      end;
      
      if InCodeBlock then
      begin
        Output := Output + '  ' + Line + #13#10;
        Continue;
      end;
      
      // Headers
      if (Length(Line) > 0) and (Line[1] = '#') then
      begin
        Line := StringReplace(Line, '#', '', [rfReplaceAll]);
        Line := '*** ' + Trim(Line) + ' ***';
      end;
      
      // Bold **text**
      Line := StringReplace(Line, '**', '', [rfReplaceAll]);
      
      // Inline code `text`
      Line := StringReplace(Line, '`', '''', [rfReplaceAll]);
      
      // Bullets
      if (Length(Line) > 2) and (Copy(Line, 1, 2) = '- ') then
        Line := '  * ' + Copy(Line, 3, Length(Line));
      
      Output := Output + Line + #13#10;
    end;
    
    Result := Output;
  finally
    Lines.Free;
  end;
end;

procedure TChatWindow.ButtonRefreshContextClick(Sender: TObject);
begin
  UpdateStatusBar;
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
  DisplayContent: string;
begin
  // Add to message history
  Msg.Role := Role;
  Msg.Content := Content;
  Msg.Timestamp := Now;
  SetLength(FMessages, Length(FMessages) + 1);
  FMessages[High(FMessages)] := Msg;
  
  // Display in chat
  if Role = 'user' then
  begin
    Prefix := '>>> YOU: ';
    DisplayContent := Content;
  end
  else if Role = 'assistant' then
  begin
    Prefix := '<<< AI: ';
    DisplayContent := RenderMarkdown(Content);  // Render markdown for AI responses
  end
  else if Role = 'system' then
  begin
    Prefix := '=== ';
    DisplayContent := Content;
  end
  else
  begin
    Prefix := '';
    DisplayContent := Content;
  end;
  
  MemoChat.Lines.Add('');
  MemoChat.Lines.Add(Prefix + DisplayContent);
  MemoChat.Lines.Add('');
  
  // Update stats after adding message
  UpdateStats;
end;

procedure TChatWindow.SendMessageToAI;
var
  UserMessage: string;
  Response: string;
  ModelName: string;
  ContextProvider: IContextProvider;
  CurrentContext: TContextItem;
  ContextText: string;
  Edits: TFileEditArray;
begin
  FIsProcessing := True;
  ButtonSend.Enabled := False;
  try
    UserMessage := Trim(MemoInput.Text);
    ModelName := ComboModel.Text;

    AddMessage('user', UserMessage);
    MemoInput.Clear;

    Application.ProcessMessages;

    // Get current file context if available
    ContextText := '';
    try
      ContextProvider := GetContextProvider;
      if ContextProvider.IsAvailable then
      begin
        CurrentContext := ContextProvider.GetCurrentFile;
        if CurrentContext.Content <> '' then
        begin
          ContextText := 'File: ' + CurrentContext.FilePath + #13#10 +
                        'Cursor: Line ' + IntToStr(CurrentContext.CursorLine) + 
                        ', Column ' + IntToStr(CurrentContext.CursorColumn) + #13#10;
          
          if CurrentContext.Selection <> '' then
            ContextText := ContextText + 'SELECTED TEXT (lines ' + 
              IntToStr(CurrentContext.LineStart) + '-' + 
              IntToStr(CurrentContext.LineEnd) + '):' + #13#10 + 
              CurrentContext.Selection
          else
            ContextText := ContextText + 'FILE CONTENT:' + #13#10 + CurrentContext.Content;
        end;
      end;
    except
      // Context gathering failed - continue without context
    end;

    // Call AI client with message history and context
    try
      if ContextText <> '' then
        Response := TPythiaAIClient.SendMessageWithContext(FMessages, ModelName, ContextText)
      else
        Response := TPythiaAIClient.SendMessage(FMessages, ModelName);

      if Response <> '' then
      begin
        AddMessage('assistant', Response);
        
        // Check if response contains file edit instructions
        Edits := ParseFileEdits(Response);
        if Length(Edits) > 0 then
        begin
          if ApplyFileEdits(Edits) then
            AddMessage('system', 'Applied ' + IntToStr(Length(Edits)) + ' file edit(s)')
          else
            AddMessage('system', 'Warning: Some file edits could not be applied');
        end;
      end;
    except
      on E: Exception do
        AddMessage('error', 'Error calling AI: ' + E.Message);
    end;
  finally
    FIsProcessing := False;
    ButtonSend.Enabled := True;
    MemoInput.SetFocus;
  end;
end;

end.

