unit Pythia.Tools.Terminal;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, fpjson, Forms, LCLIntf, LCLType,
  Pythia.Tools, Pythia.Tools.Terminal.Async;

type
  TTerminalTool = class(TInterfacedObject, IPythiaTool)
  private
    FExecutor: TTerminalExecutor;
    FPendingCommand: string;
    FCommandOutput: string;
    FIsExecuting: Boolean;
    procedure OnOutputReceived(const Text: string);
    procedure OnCommandComplete(const ExitCode: Integer; const FullOutput: string);
    function ExecuteCommandSync(const Command: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TToolParameterArray;
    function Execute(const Arguments: TJSONObject): string;
  end;

implementation

{ TTerminalTool }

constructor TTerminalTool.Create;
begin
  inherited Create;
  FExecutor := TTerminalExecutor.Create;
  FExecutor.OnOutput := @OnOutputReceived;
  FExecutor.OnComplete := @OnCommandComplete;
  FIsExecuting := False;
end;

destructor TTerminalTool.Destroy;
begin
  FExecutor.Free;
  inherited Destroy;
end;

{ TTerminalTool }

function TTerminalTool.GetName: string;
begin
  Result := 'run_terminal_command';
end;

function TTerminalTool.GetDescription: string;
begin
  Result := 'Execute a PowerShell command in the Windows terminal and return the output. ' +
            'Use this to run commands like Get-Location, Get-ChildItem, git status, etc. ' +
            'The command will be executed in the project directory.';
end;

function TTerminalTool.GetParameters: TToolParameterArray;
begin
  SetLength(Result, 1);
  Result[0].Name := 'command';
  Result[0].ParamType := 'string';
  Result[0].Description := 'The PowerShell command to execute (e.g., "Get-Location", "git status")';
  Result[0].Required := True;
end;

function TTerminalTool.Execute(const Arguments: TJSONObject): string;
var
  Command: string;
begin
  Command := Arguments.Get('command', '');
  if Command = '' then
    Exit('Error: command parameter is required');
    
  Result := ExecuteCommandSync(Command);
end;

procedure TTerminalTool.OnOutputReceived(const Text: string);
begin
  // Accumulate output as it arrives
  if FCommandOutput <> '' then
    FCommandOutput := FCommandOutput + #13#10;
  FCommandOutput := FCommandOutput + Text;
end;

procedure TTerminalTool.OnCommandComplete(const ExitCode: Integer; const FullOutput: string);
begin
  // Command finished - set final output and flag
  FCommandOutput := FullOutput;
  if ExitCode <> 0 then
    FCommandOutput := FCommandOutput + #13#10 + '[Exit code: ' + IntToStr(ExitCode) + ']';
  FIsExecuting := False;
end;

function TTerminalTool.ExecuteCommandSync(const Command: string): string;
var
  StartTime: TDateTime;
  TimeoutSeconds: Double;
begin
  // Start async execution
  FCommandOutput := '';
  FIsExecuting := True;
  FPendingCommand := Command;
  
  FExecutor.ExecuteAsync(Command);
  
  // Wait for completion (with timeout)
  StartTime := Now;
  TimeoutSeconds := 35.0; // 35 seconds (5 seconds more than thread timeout)
  
  while FIsExecuting do
  begin
    // Check for timeout
    if (Now - StartTime) * 86400 > TimeoutSeconds then
    begin
      FExecutor.Cancel;
      FIsExecuting := False;
      Exit('Error: Command execution timed out');
    end;
    
    // Process messages to allow timer to fire and update output
    Application.ProcessMessages;
    Sleep(50);
  end;
  
  Result := FCommandOutput;
end;

end.
