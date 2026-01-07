unit Pythia.Tools;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson;

type
  // Tool parameter definition
  TToolParameter = record
    Name: string;
    ParamType: string;  // 'string', 'number', 'boolean', 'array', 'object'
    Description: string;
    Required: Boolean;
  end;
  TToolParameterArray = array of TToolParameter;

  // Tool definition
  TToolDefinition = record
    Name: string;
    Description: string;
    Parameters: TToolParameterArray;
  end;
  TToolDefinitionArray = array of TToolDefinition;

  // Tool call from AI
  TToolCall = record
    CallId: string;       // Unique ID for this call (from AI)
    ToolName: string;     // Which tool to invoke
    Arguments: string;    // JSON string of arguments
  end;
  TToolCallArray = array of TToolCall;

  // Tool result to return to AI
  TToolResult = record
    CallId: string;       // Match the call ID
    ToolName: string;
    Output: string;       // Result as string (can be JSON)
    IsError: Boolean;
  end;
  TToolResultArray = array of TToolResult;

  // Abstract tool interface
  IPythiaTool = interface
    ['{B5E3C2D1-9F4A-4E2B-8D6C-7A1B2C3D4E5F}']
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TToolParameterArray;
    function Execute(const Arguments: TJSONObject): string;
  end;

  // Tool registry
  TPythiaToolRegistry = class
  private
    FTools: array of IPythiaTool;
  public
    procedure RegisterTool(const Tool: IPythiaTool);
    function GetToolDefinitions: TToolDefinitionArray;
    function GetTool(const Name: string): IPythiaTool;
    function ExecuteToolCall(const ToolCall: TToolCall): TToolResult;
    function Count: Integer;
  end;

// Global tool registry
function ToolRegistry: TPythiaToolRegistry;

implementation

var
  GlobalToolRegistry: TPythiaToolRegistry = nil;

function ToolRegistry: TPythiaToolRegistry;
begin
  if GlobalToolRegistry = nil then
    GlobalToolRegistry := TPythiaToolRegistry.Create;
  Result := GlobalToolRegistry;
end;

{ TPythiaToolRegistry }

procedure TPythiaToolRegistry.RegisterTool(const Tool: IPythiaTool);
begin
  SetLength(FTools, Length(FTools) + 1);
  FTools[High(FTools)] := Tool;
end;

function TPythiaToolRegistry.GetToolDefinitions: TToolDefinitionArray;
var
  I: Integer;
  Tool: IPythiaTool;
begin
  SetLength(Result, Length(FTools));
  for I := 0 to High(FTools) do
  begin
    Tool := FTools[I];
    Result[I].Name := Tool.GetName;
    Result[I].Description := Tool.GetDescription;
    Result[I].Parameters := Tool.GetParameters;
  end;
end;

function TPythiaToolRegistry.GetTool(const Name: string): IPythiaTool;
var
  Tool: IPythiaTool;
begin
  Result := nil;
  for Tool in FTools do
  begin
    if SameText(Tool.GetName, Name) then
      Exit(Tool);
  end;
end;

function TPythiaToolRegistry.ExecuteToolCall(const ToolCall: TToolCall): TToolResult;
var
  Tool: IPythiaTool;
  Args: TJSONObject;
begin
  Result.CallId := ToolCall.CallId;
  Result.ToolName := ToolCall.ToolName;
  Result.IsError := False;
  
  try
    Tool := GetTool(ToolCall.ToolName);
    if Tool = nil then
    begin
      Result.Output := 'Error: Tool "' + ToolCall.ToolName + '" not found';
      Result.IsError := True;
      Exit;
    end;
    
    // Parse arguments JSON
    try
      Args := GetJSON(ToolCall.Arguments) as TJSONObject;
      try
        Result.Output := Tool.Execute(Args);
      finally
        Args.Free;
      end;
    except
      on E: Exception do
      begin
        Result.Output := 'Error parsing arguments: ' + E.Message;
        Result.IsError := True;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Output := 'Error executing tool: ' + E.Message;
      Result.IsError := True;
    end;
  end;
end;

function TPythiaToolRegistry.Count: Integer;
begin
  Result := Length(FTools);
end;

initialization
  GlobalToolRegistry := nil;

finalization
  if GlobalToolRegistry <> nil then
    GlobalToolRegistry.Free;

end.
