unit Pythia.FileEdit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, SrcEditorIntf;

type
  TFileEdit = record
    FileName: string;
    StartLine: Integer;
    EndLine: Integer;
    NewText: string;
  end;

  TFileEditArray = array of TFileEdit;

function ParseFileEdits(const Response: string): TFileEditArray;
function ApplyFileEdits(const Edits: TFileEditArray): Boolean;

implementation

uses
  LazIDEIntf;

function ParseFileEdits(const Response: string): TFileEditArray;
var
  JSON, EditObj: TJSONObject;
  EditsArray: TJSONArray;
  I: Integer;
  StartPos, EndPos: Integer;
begin
  SetLength(Result, 0);
  
  // Look for JSON block in response (between ```json and ```)
  StartPos := Pos('```json', Response);
  if StartPos = 0 then
    StartPos := Pos('{', Response);
  
  if StartPos = 0 then
    Exit;
    
  EndPos := Pos('```', Response, StartPos + 7);
  if EndPos = 0 then
    EndPos := Length(Response) + 1;
    
  try
    JSON := GetJSON(Copy(Response, StartPos + 7, EndPos - StartPos - 7)) as TJSONObject;
    try
      // Check if this is an edits response
      if not JSON.Find('edits', EditsArray) then
        Exit;
        
      SetLength(Result, EditsArray.Count);
      
      for I := 0 to EditsArray.Count - 1 do
      begin
        EditObj := EditsArray.Objects[I];
        Result[I].FileName := EditObj.Get('file', '');
        Result[I].StartLine := EditObj.Get('startLine', 1);
        Result[I].EndLine := EditObj.Get('endLine', 1);
        Result[I].NewText := EditObj.Get('newText', '');
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      // JSON parsing failed - no edits to apply
      SetLength(Result, 0);
    end;
  end;
end;

function ApplyFileEdits(const Edits: TFileEditArray): Boolean;
var
  Edit: TFileEdit;
  Editor: TSourceEditorInterface;
  I: Integer;
  Lines: TStrings;
begin
  Result := True;
  
  if SourceEditorManagerIntf = nil then
    Exit(False);
    
  for Edit in Edits do
  begin
    // Find the editor for this file
    Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(Edit.FileName);
    
    if Editor = nil then
    begin
      // File not open - skip (could open it, but safer to require it's open)
      Result := False;
      Continue;
    end;
    
    try
      // Replace lines StartLine through EndLine with NewText
      Lines := TStringList.Create;
      try
        Lines.Text := Edit.NewText;
        
        // Delete old lines
        for I := Edit.EndLine downto Edit.StartLine do
        begin
          if I <= Editor.LineCount then
            Editor.Lines.Delete(I - 1);  // Lines are 0-indexed in API
        end;
        
        // Insert new lines
        for I := Lines.Count - 1 downto 0 do
        begin
          Editor.Lines.Insert(Edit.StartLine - 1, Lines[I]);
        end;
      finally
        Lines.Free;
      end;
    except
      on E: Exception do
        Result := False;
    end;
  end;
end;

end.
