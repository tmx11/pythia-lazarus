unit Pythia.Tools.Terminal;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, fpjson, Pythia.Tools;

type
  TTerminalTool = class(TInterfacedObject, IPythiaTool)
  private
    function ExecuteCommand(const Command: string): string;
  public
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TToolParameterArray;
    function Execute(const Arguments: TJSONObject): string;
  end;

implementation

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
    
  Result := ExecuteCommand(Command);
end;

function TTerminalTool.ExecuteCommand(const Command: string): string;
var
  AProcess: TProcess;
  OutputLines: TStringList;
  Buffer: array[0..2047] of Byte;
  BytesRead: Integer;
  StartTime: TDateTime;
  TimeoutSeconds: Double;
begin
  Result := '';
  OutputLines := TStringList.Create;
  AProcess := TProcess.Create(nil);
  try
    // Configure process for PowerShell
    AProcess.Executable := 'powershell.exe';
    AProcess.Parameters.Add('-NoProfile');
    AProcess.Parameters.Add('-NonInteractive');
    AProcess.Parameters.Add('-Command');
    AProcess.Parameters.Add(Command);
    
    // Set working directory to project root
    AProcess.CurrentDirectory := GetCurrentDir;
    
    // Configure pipes for output
    AProcess.Options := [poUsePipes, poStderrToOutPut];
    
    try
      AProcess.Execute;
      
      StartTime := Now;
      TimeoutSeconds := 30.0; // 30 second timeout
      
      // Read output
      while AProcess.Running do
      begin
        // Check for timeout
        if (Now - StartTime) * 86400 > TimeoutSeconds then
        begin
          AProcess.Terminate(1);
          OutputLines.Add('[Command timed out after ' + IntToStr(Round(TimeoutSeconds)) + ' seconds]');
          Break;
        end;
        
        // Read available output
        if AProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            OutputLines.Add(Copy(TEncoding.ANSI.GetString(Buffer), 1, BytesRead));
        end
        else
          Sleep(10); // Brief pause if no output available
      end;
      
      // Read any remaining output
      repeat
        BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          OutputLines.Add(Copy(TEncoding.ANSI.GetString(Buffer), 1, BytesRead));
      until BytesRead <= 0;
      
      Result := Trim(OutputLines.Text);
      
      // Add exit code if non-zero
      if AProcess.ExitCode <> 0 then
        Result := Result + #13#10 + '[Exit code: ' + IntToStr(AProcess.ExitCode) + ']';
        
    except
      on E: Exception do
        Result := 'Error executing command: ' + E.Message;
    end;
  finally
    OutputLines.Free;
    AProcess.Free;
  end;
end;

end.
