unit Pythia.Context;

interface

uses SysUtils, Classes, Generics.Collections;

type TContextItemType = (ctCurrentFile, ctSelection, ctProjectFile, ctRelatedFile);
  TContextItem = record ItemType: TContextItemType; FilePath: string; Content: string; LineStart: Integer; LineEnd: Integer; TokenCount: Integer; class function Create(AType: TContextItemType; const AFilePath, AContent: string; ALineStart: Integer = 0; ALineEnd: Integer = 0): TContextItem; static; end;
  IContextProvider = interface ['{B8E9F7A1-2C3D-4E5F-8A9B-1C2D3E4F5A6B}'] function GetCurrentFile: TContextItem; function IsAvailable: Boolean; end;

implementation

class function TContextItem.Create(AType: TContextItemType; const AFilePath, AContent: string; ALineStart, ALineEnd: Integer): TContextItem; begin Result.ItemType := AType; Result.FilePath := AFilePath; Result.Content := AContent; Result.LineStart := ALineStart; Result.LineEnd := ALineEnd; Result.TokenCount := 0; end;

end.