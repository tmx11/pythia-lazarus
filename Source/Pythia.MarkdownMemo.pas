unit Pythia.MarkdownMemo;

{$mode delphi}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, LCLType, Controls;

type
  TMarkdownMemo = class(TMemo)
  private
    FRenderMarkdown: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddMarkdownText(const Text: string);
  published
    property RenderMarkdown: Boolean read FRenderMarkdown write FRenderMarkdown;
  end;

implementation

uses
  StrUtils;

constructor TMarkdownMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderMarkdown := False;
  Font.Name := 'Consolas';
  Font.Size := 10;
end;

procedure TMarkdownMemo.AddMarkdownText(const Text: string);
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  InCodeBlock: Boolean;
  Output: TStringList;
  P1, P2: Integer;
  BoldText: string;
begin
  if not FRenderMarkdown then
  begin
    // Raw markdown - just add as-is
    Self.Lines.Add(Text);
    Exit;
  end;
  
  // Parse and render markdown with text decorations
  Lines := TStringList.Create;
  Output := TStringList.Create;
  try
    Lines.Text := Text;
    InCodeBlock := False;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      
      // Code blocks
      if (Length(Line) >= 3) and (Copy(Line, 1, 3) = '```') then
      begin
        InCodeBlock := not InCodeBlock;
        Output.Add('```');
        Continue;
      end;
      
      if InCodeBlock then
      begin
        // Indent code
        Output.Add('    ' + Line);
        Continue;
      end;
      
      // Headers - make uppercase and add separators
      if (Length(Line) > 0) and (Line[1] = '#') then
      begin
        Line := StringReplace(Line, '#', '', [rfReplaceAll]);
        Line := UpperCase(Trim(Line));
        Output.Add('');
        Output.Add('━━ ' + Line + ' ━━');
        Output.Add('');
        Continue;
      end;
      
      // Bold - use uppercase for emphasis (since we can't bold in TMemo)
      while Pos('**', Line) > 0 do
      begin
        P1 := Pos('**', Line);
        P2 := PosEx('**', Line, P1 + 2);
        if P2 > P1 then
        begin
          BoldText := Copy(Line, P1 + 2, P2 - P1 - 2);
          Line := Copy(Line, 1, P1 - 1) + UpperCase(BoldText) + Copy(Line, P2 + 2, Length(Line));
        end
        else
          Break;
      end;
      
      // Inline code - wrap in quotes
      Line := StringReplace(Line, '`', '''', [rfReplaceAll]);
      
      // Bullets - convert to unicode bullet
      if (Length(Line) > 2) and (Copy(Line, 1, 2) = '- ') then
        Line := '  • ' + Copy(Line, 3, Length(Line));
      
      Output.Add(Line);
    end;
    
    // Add to memo
    Self.Lines.AddStrings(Output);
    
  finally
    Lines.Free;
    Output.Free;
  end;
end;

end.
