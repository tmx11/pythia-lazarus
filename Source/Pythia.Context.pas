unit Pythia.Context;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SrcEditorIntf, LazIDEIntf;

type
  TContextItemType = (ctCurrentFile, ctSelection, ctProjectFile, ctRelatedFile);
  
  TContextItem = record
    ItemType: TContextItemType;
    FilePath: string;
    Content: string;
    LineStart: Integer;
    LineEnd: Integer;
    CursorLine: Integer;
    CursorColumn: Integer;
    Selection: string;
    TokenCount: Integer;
  end;

  { IContextProvider }
  IContextProvider = interface
    ['{B8E9F7A1-2C3D-4E5F-8A9B-1C2D3E4F5A6B}']
    function GetCurrentFile: TContextItem;
    function IsAvailable: Boolean;
  end;

  { TLazarusContextProvider }
  TLazarusContextProvider = class(TInterfacedObject, IContextProvider)
  public
    function GetCurrentFile: TContextItem;
    function IsAvailable: Boolean;
  end;

function GetContextProvider: IContextProvider;
function CreateContextItem(AType: TContextItemType; const AFilePath, AContent: string;
  ALineStart: Integer = 0; ALineEnd: Integer = 0): TContextItem;

implementation

var
  GlobalContextProvider: IContextProvider = nil;

function GetContextProvider: IContextProvider;
begin
  if GlobalContextProvider = nil then
    GlobalContextProvider := TLazarusContextProvider.Create;
  Result := GlobalContextProvider;
end;

function CreateContextItem(AType: TContextItemType; const AFilePath, AContent: string;
  ALineStart, ALineEnd: Integer): TContextItem;
begin
  Result.ItemType := AType;
  Result.FilePath := AFilePath;
  Result.Content := AContent;
  Result.LineStart := ALineStart;
  Result.LineEnd := ALineEnd;
  Result.CursorLine := 0;
  Result.CursorColumn := 0;
  Result.Selection := '';
  Result.TokenCount := 0;
end;

{ TLazarusContextProvider }

function TLazarusContextProvider.GetCurrentFile: TContextItem;
var
  Editor: TSourceEditorInterface;
  Selected: string;
begin
  Result := CreateContextItem(ctCurrentFile, '', '');
  
  // Check if SourceEditorManagerIntf is available
  if SourceEditorManagerIntf = nil then
    Exit;
    
  // Get the active source editor
  Editor := SourceEditorManagerIntf.ActiveEditor;
  if Editor = nil then
    Exit;
    
  // Get file information
  Result.FilePath := Editor.FileName;
  Result.CursorLine := 0;  // TODO: Find correct API for cursor position in Lazarus
  Result.CursorColumn := 0;
  
  // Get selected text or full content
  Selected := Editor.Selection;
  if Selected <> '' then
  begin
    // User has a selection
    Result.ItemType := ctSelection;
    Result.Content := Selected;
    Result.Selection := Selected;
    Result.LineStart := Editor.BlockBegin.Y;
    Result.LineEnd := Editor.BlockEnd.Y;
  end
  else
  begin
    // No selection - get entire file content
    Result.ItemType := ctCurrentFile;
    Result.Content := Editor.Lines.Text;
    Result.LineStart := 1;
    Result.LineEnd := Editor.LineCount;
  end;
end;

function TLazarusContextProvider.IsAvailable: Boolean;
begin
  Result := (SourceEditorManagerIntf <> nil) and 
            (SourceEditorManagerIntf.ActiveEditor <> nil);
end;

end.
