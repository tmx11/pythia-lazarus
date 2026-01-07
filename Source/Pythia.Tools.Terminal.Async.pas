unit Pythia.Tools.Terminal.Async;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, ExtCtrls, SyncObjs;

type
  // Forward declaration
  TTerminalThread = class;
  
  // Callback types
  TTerminalOutputCallback = procedure(const Text: string) of object;
  TTerminalCompleteCallback = procedure(const ExitCode: Integer; const FullOutput: string) of object;

  // Thread-safe output queue using critical section
  TTerminalOutputQueue = class
  private
    FLines: TStringList;
    FLock: SyncObjs.TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Text: string);
    function ExtractAll: TStringList;
    function HasData: Boolean;
  end;

  // Background thread for command execution
  TTerminalThread = class(TThread)
  private
    FCommand: string;
    FQueue: TTerminalOutputQueue;
    FExitCode: Integer;
    FFullOutput: TStringList;
    FOnComplete: TTerminalCompleteCallback;
    procedure ExecutePowerShell;
    procedure NotifyComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACommand: string; AQueue: TTerminalOutputQueue; 
      AOnComplete: TTerminalCompleteCallback);
    destructor Destroy; override;
    property ExitCode: Integer read FExitCode;
  end;

  // Async terminal executor
  TTerminalExecutor = class
  private
    FOutputQueue: TTerminalOutputQueue;
    FCurrentThread: TTerminalThread;
    FTimer: TTimer;
    FOnOutput: TTerminalOutputCallback;
    FOnComplete: TTerminalCompleteCallback;
    procedure OnTimerTick(Sender: TObject);
    procedure ThreadComplete(const ExitCode: Integer; const FullOutput: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ExecuteAsync(const Command: string);
    procedure Cancel;
    function IsRunning: Boolean;
    
    property OnOutput: TTerminalOutputCallback read FOnOutput write FOnOutput;
    property OnComplete: TTerminalCompleteCallback read FOnComplete write FOnComplete;
  end;

implementation

uses
  LCLIntf, LCLType;

{ TTerminalOutputQueue }

constructor TTerminalOutputQueue.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
  FLock := SyncObjs.TCriticalSection.Create;
end;

destructor TTerminalOutputQueue.Destroy;
begin
  FLines.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TTerminalOutputQueue.Add(const Text: string);
begin
  FLock.Enter;
  try
    FLines.Add(Text);
  finally
    FLock.Leave;
  end;
end;

function TTerminalOutputQueue.ExtractAll: TStringList;
begin
  Result := TStringList.Create;
  FLock.Enter;
  try
    Result.Assign(FLines);
    FLines.Clear;
  finally
    FLock.Leave;
  end;
end;

function TTerminalOutputQueue.HasData: Boolean;
begin
  FLock.Enter;
  try
    Result := FLines.Count > 0;
  finally
    FLock.Leave;
  end;
end;

{ TTerminalThread }

constructor TTerminalThread.Create(const ACommand: string; 
  AQueue: TTerminalOutputQueue; AOnComplete: TTerminalCompleteCallback);
begin
  inherited Create(False); // Start immediately
  FreeOnTerminate := True;
  FCommand := ACommand;
  FQueue := AQueue;
  FExitCode := -1;
  FFullOutput := TStringList.Create;
  FOnComplete := AOnComplete;
end;

destructor TTerminalThread.Destroy;
begin
  FFullOutput.Free;
  inherited Destroy;
end;

procedure TTerminalThread.Execute;
begin
  try
    ExecutePowerShell;
  except
    on E: Exception do
    begin
      FQueue.Add('[Thread Error: ' + E.Message + ']');
      FFullOutput.Add('[Thread Error: ' + E.Message + ']');
    end;
  end;
  
  // Notify completion (will be marshaled to main thread)
  Synchronize(@NotifyComplete);
end;

procedure TTerminalThread.NotifyComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(FExitCode, FFullOutput.Text);
end;

procedure TTerminalThread.ExecutePowerShell;
var
  AProcess: TProcess;
  Buffer: array[0..2047] of Byte;
  BytesRead: Integer;
  StartTime: TDateTime;
  Line: string;
begin
  FQueue.Add('> ' + FCommand);
  FFullOutput.Add('> ' + FCommand);
  
  AProcess := TProcess.Create(nil);
  try
    // Configure process for PowerShell
    AProcess.Executable := 'powershell.exe';
    AProcess.Parameters.Add('-NoProfile');
    AProcess.Parameters.Add('-NonInteractive');
    AProcess.Parameters.Add('-Command');
    AProcess.Parameters.Add(FCommand);
    AProcess.CurrentDirectory := GetCurrentDir;
    AProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    AProcess.ShowWindow := swoHIDE;
    
    AProcess.Execute;
    StartTime := Now;
    
    // Read output in loop (THIS IS NOW IN BACKGROUND THREAD)
    while AProcess.Running and not Terminated do
    begin
      // Timeout check (30 seconds)
      if (Now - StartTime) * 86400 > 30.0 then
      begin
        AProcess.Terminate(1);
        Line := '[Command timed out after 30 seconds]';
        FQueue.Add(Line);
        FFullOutput.Add(Line);
        Break;
      end;
      
      // Read available output
      if AProcess.Output.NumBytesAvailable > 0 then
      begin
        BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          SetLength(Line, BytesRead);
          Move(Buffer[0], Line[1], BytesRead);
          FQueue.Add(Line);
          FFullOutput.Add(Line);
        end;
      end
      else
        Sleep(10);
    end;
    
    // Read final output
    repeat
      BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetLength(Line, BytesRead);
        Move(Buffer[0], Line[1], BytesRead);
        FQueue.Add(Line);
        FFullOutput.Add(Line);
      end;
    until BytesRead <= 0;
    
    FExitCode := AProcess.ExitCode;
    if FExitCode <> 0 then
    begin
      Line := '[Exit code: ' + IntToStr(FExitCode) + ']';
      FQueue.Add(Line);
      FFullOutput.Add(Line);
    end;
      
  finally
    AProcess.Free;
  end;
end;

{ TTerminalExecutor }

constructor TTerminalExecutor.Create;
begin
  inherited Create;
  FOutputQueue := TTerminalOutputQueue.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100; // Check queue every 100ms
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := True;
end;

destructor TTerminalExecutor.Destroy;
begin
  Cancel;
  FTimer.Free;
  FOutputQueue.Free;
  inherited Destroy;
end;

procedure TTerminalExecutor.OnTimerTick(Sender: TObject);
var
  Lines: TStringList;
  I: Integer;
begin
  if not FOutputQueue.HasData then
    Exit;
    
  Lines := FOutputQueue.ExtractAll;
  try
    if Lines.Count > 0 then
    begin
      // Call callback for each line
      if Assigned(FOnOutput) then
        for I := 0 to Lines.Count - 1 do
          FOnOutput(Lines[I]);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTerminalExecutor.ThreadComplete(const ExitCode: Integer; const FullOutput: string);
begin
  FCurrentThread := nil; // Thread freed itself
  
  if Assigned(FOnComplete) then
    FOnComplete(ExitCode, FullOutput);
end;

procedure TTerminalExecutor.ExecuteAsync(const Command: string);
begin
  // Cancel any running command
  Cancel;
  
  // Start new thread
  FCurrentThread := TTerminalThread.Create(Command, FOutputQueue, @ThreadComplete);
end;

procedure TTerminalExecutor.Cancel;
begin
  if Assigned(FCurrentThread) then
  begin
    FCurrentThread.Terminate;
    FCurrentThread := nil;
  end;
end;

function TTerminalExecutor.IsRunning: Boolean;
begin
  Result := Assigned(FCurrentThread);
end;

end.
