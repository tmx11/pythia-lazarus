# Terminal Threading Plan - Non-Blocking Execution

## Problem
Terminal commands currently execute synchronously on the main thread using `TProcess`, causing the Lazarus IDE to freeze until the command completes.

```pascal
// CURRENT: Blocks UI thread
while AProcess.Running do
begin
  // Polls in main thread - freezes IDE!
  if AProcess.Output.NumBytesAvailable > 0 then
    BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
  Sleep(10);
end;
```

## VS Code Terminal Architecture (Research)

Based on VS Code source code analysis:

### 1. **Pty Host Process (Separate Process)**
- VS Code runs `ptyHost` as a separate Node.js process
- Communicates via IPC (Inter-Process Communication)
- Terminal backend is completely isolated from renderer

### 2. **Terminal Service (Main Process)**
- `IPtyService` interface for all pty operations
- Event-driven: `onProcessData`, `onProcessReady`, `onProcessExit`
- Manages multiple terminal instances
- Non-blocking API using Promises/Events

### 3. **Renderer (UI Thread)**
- Subscribes to events from terminal service
- Updates xterm.js component asynchronously
- Never blocks on process I/O

### 4. **Key Design Patterns**
```typescript
// Event-driven, not polling
readonly onProcessData: Event<{ id: number; event: IProcessDataEvent }>;

// Async operations
attachToProcess(id: number): Promise<void>;

// Flow control to prevent flooding
acknowledgeDataEvent(id: number, charCount: number): Promise<void>;
```

## Lazarus/Free Pascal Threading Approach

### Option 1: TThread with Message Queue (RECOMMENDED)
**Pros**:
- Native Free Pascal threading
- Clean separation of concerns
- Proven pattern in Lazarus applications

**Cons**:
- More complex than direct execution
- Need thread-safe queue implementation

**Architecture**:
```
┌─────────────────┐
│ Main UI Thread  │
│  (ChatForm)     │
└────────┬────────┘
         │ 1. User sends message
         │
         ▼
┌─────────────────┐
│  ExecuteCommand │
└────────┬────────┘
         │ 2. Create TTerminalThread
         │
         ▼
┌─────────────────────────────┐
│    TTerminalThread          │
│  (Background Thread)        │
│                             │
│  • TProcess.Execute         │
│  • Read output in loop      │
│  • Add to thread-safe queue │
└──────────┬──────────────────┘
           │ 3. Output available
           │
           ▼
┌─────────────────────────────┐
│ Application.QueueAsyncCall  │
│  (Marshal to main thread)   │
└──────────┬──────────────────┘
           │ 4. Update UI safely
           │
           ▼
┌─────────────────┐
│ AddTerminalOutput│
│  (Main thread)  │
└─────────────────┘
```

### Option 2: TAsyncProcess (Simplified)
**Pros**:
- Built-in async support in Free Pascal
- Less code than manual threading

**Cons**:
- Less control over threading
- Still needs careful synchronization

### Option 3: External Process + Polling Timer
**Pros**:
- Very simple
- No threading complexity

**Cons**:
- Timer overhead
- Less responsive
- Doesn't fully solve blocking issue

## Recommended Implementation: TThread Approach

### Phase 1: Core Threading Infrastructure

#### 1.1 Create Thread-Safe Output Queue
```pascal
type
  TTerminalOutputQueue = class
  private
    FQueue: TThreadList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Text: string);
    function Extract: TStringList; // Get all pending output
  end;
```

#### 1.2 Create Terminal Thread Class
```pascal
type
  TTerminalThread = class(TThread)
  private
    FCommand: string;
    FOutputQueue: TTerminalOutputQueue;
    FExitCode: Integer;
    FErrorMessage: string;
    procedure OutputCallback(const Text: string); // Called in thread
  protected
    procedure Execute; override;
  public
    constructor Create(const ACommand: string; AQueue: TTerminalOutputQueue);
    property ExitCode: Integer read FExitCode;
    property ErrorMessage: string read FErrorMessage;
  end;
```

#### 1.3 Update TTerminalTool
```pascal
type
  TTerminalTool = class
  private
    FOutputQueue: TTerminalOutputQueue;
    FCurrentThread: TTerminalThread;
    FTimer: TTimer; // Poll queue in main thread
    procedure OnTimerTick(Sender: TObject);
    procedure ProcessQueuedOutput;
  public
    procedure ExecuteAsync(const Command: string); // Non-blocking
    procedure Cancel; // Terminate running command
  end;
```

### Phase 2: UI Integration

#### 2.1 Update ChatForm
```pascal
procedure TChatWindow.SendMessageToAI;
begin
  // ... existing code ...
  
  // Execute tools asynchronously
  for I := 0 to High(Response.ToolCalls) do
  begin
    if Response.ToolCalls[I].ToolName = 'run_terminal_command' then
    begin
      AddMessage('system', '⚙️ Executing: run_terminal_command...');
      TerminalTool.ExecuteAsync(Command);
      
      // Don't wait for completion - will update via callback
    end;
  end;
end;
```

#### 2.2 Add Callback Mechanism
```pascal
type
  TTerminalOutputCallback = procedure(const Text: string) of object;
  TTerminalCompleteCallback = procedure(const ExitCode: Integer; const Output: string) of object;

type
  TTerminalTool = class
  private
    FOnOutput: TTerminalOutputCallback;
    FOnComplete: TTerminalCompleteCallback;
  public
    property OnOutput: TTerminalOutputCallback write FOnOutput;
    property OnComplete: TTerminalCompleteCallback write FOnComplete;
  end;
```

### Phase 3: Flow Control (Advanced)

#### 3.1 Prevent Output Flooding
```pascal
const
  MAX_QUEUED_LINES = 1000; // Drop old output if queue grows too large

procedure TTerminalOutputQueue.Add(const Text: string);
begin
  FQueue.LockList;
  try
    if FQueue.Count > MAX_QUEUED_LINES then
      FQueue.Delete(0); // Drop oldest
    FQueue.Add(Text);
  finally
    FQueue.UnlockList;
  end;
end;
```

#### 3.2 Cancellation Support
```pascal
procedure TTerminalThread.Execute;
begin
  // Check for termination frequently
  while AProcess.Running and not Terminated do
  begin
    if AProcess.Output.NumBytesAvailable > 0 then
      // ... read output ...
    Sleep(10);
  end;
  
  if Terminated then
    AProcess.Terminate(1); // Kill process if thread cancelled
end;
```

## Implementation Steps

### Step 1: Create Infrastructure (1-2 hours)
- [ ] Implement `TTerminalOutputQueue` with TThreadList
- [ ] Create `TTerminalThread` class
- [ ] Add thread management to `TTerminalTool`

### Step 2: Update ChatForm Integration (1 hour)
- [ ] Add timer for queue polling (100ms interval)
- [ ] Update `SendMessageToAI` for async execution
- [ ] Add callback for terminal output

### Step 3: Testing (30 min)
- [ ] Test long-running commands (e.g., `Start-Sleep -Seconds 10`)
- [ ] Verify IDE stays responsive during execution
- [ ] Test cancellation (close window during command)
- [ ] Test multiple concurrent commands

### Step 4: Polish (30 min)
- [ ] Add progress indicator (spinner?) during execution
- [ ] Show "Command running..." message
- [ ] Add cancel button for long commands

## Code Example: Complete Implementation

```pascal
unit Pythia.Tools.Terminal.Async;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, fpjson, Pythia.Tools, ExtCtrls;

type
  // Thread-safe output queue
  TTerminalOutputQueue = class
  private
    FQueue: TThreadList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Text: string);
    function ExtractAll: TStringList;
  end;

  // Background thread for command execution
  TTerminalThread = class(TThread)
  private
    FCommand: string;
    FQueue: TTerminalOutputQueue;
    FExitCode: Integer;
    procedure ExecutePowerShell;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACommand: string; AQueue: TTerminalOutputQueue);
    property ExitCode: Integer read FExitCode;
  end;

  // Async terminal tool
  TTerminalToolAsync = class(TInterfacedObject, IPythiaTool)
  private
    FOutputQueue: TTerminalOutputQueue;
    FCurrentThread: TTerminalThread;
    FTimer: TTimer;
    FOnOutput: TNotifyEvent;
    procedure OnTimerTick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TToolParameterArray;
    function Execute(const Arguments: TJSONObject): string;
    
    procedure ExecuteAsync(const Command: string);
    procedure Cancel;
    
    property OnOutput: TNotifyEvent read FOnOutput write FOnOutput;
  end;

implementation

{ TTerminalOutputQueue }

constructor TTerminalOutputQueue.Create;
begin
  inherited Create;
  FQueue := TThreadList.Create;
end;

destructor TTerminalOutputQueue.Destroy;
begin
  FQueue.Free;
  inherited Destroy;
end;

procedure TTerminalOutputQueue.Add(const Text: string);
var
  List: TList;
begin
  List := FQueue.LockList;
  try
    List.Add(StrNew(PChar(Text)));
  finally
    FQueue.UnlockList;
  end;
end;

function TTerminalOutputQueue.ExtractAll: TStringList;
var
  List: TList;
  I: Integer;
begin
  Result := TStringList.Create;
  List := FQueue.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Result.Add(StrPas(PChar(List[I])));
      StrDispose(PChar(List[I]));
    end;
    List.Clear;
  finally
    FQueue.UnlockList;
  end;
end;

{ TTerminalThread }

constructor TTerminalThread.Create(const ACommand: string; 
  AQueue: TTerminalOutputQueue);
begin
  inherited Create(False); // Start immediately
  FreeOnTerminate := True;
  FCommand := ACommand;
  FQueue := AQueue;
  FExitCode := -1;
end;

procedure TTerminalThread.Execute;
begin
  try
    ExecutePowerShell;
  except
    on E: Exception do
      FQueue.Add('[Thread Error: ' + E.Message + ']');
  end;
end;

procedure TTerminalThread.ExecutePowerShell;
var
  AProcess: TProcess;
  Buffer: array[0..2047] of Byte;
  BytesRead: Integer;
  StartTime: TDateTime;
begin
  FQueue.Add('> ' + FCommand);
  
  AProcess := TProcess.Create(nil);
  try
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
      // Timeout check
      if (Now - StartTime) * 86400 > 30.0 then
      begin
        AProcess.Terminate(1);
        FQueue.Add('[Command timed out after 30 seconds]');
        Break;
      end;
      
      // Read available output
      if AProcess.Output.NumBytesAvailable > 0 then
      begin
        BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          FQueue.Add(Copy(TEncoding.ANSI.GetString(Buffer), 1, BytesRead));
      end
      else
        Sleep(10);
    end;
    
    // Read final output
    repeat
      BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        FQueue.Add(Copy(TEncoding.ANSI.GetString(Buffer), 1, BytesRead));
    until BytesRead <= 0;
    
    FExitCode := AProcess.ExitCode;
    if FExitCode <> 0 then
      FQueue.Add('[Exit code: ' + IntToStr(FExitCode) + ']');
      
  finally
    AProcess.Free;
  end;
end;

{ TTerminalToolAsync }

constructor TTerminalToolAsync.Create;
begin
  inherited Create;
  FOutputQueue := TTerminalOutputQueue.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100; // Check queue every 100ms
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := True;
end;

destructor TTerminalToolAsync.Destroy;
begin
  Cancel;
  FTimer.Free;
  FOutputQueue.Free;
  inherited Destroy;
end;

procedure TTerminalToolAsync.OnTimerTick(Sender: TObject);
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := FOutputQueue.ExtractAll;
  try
    if Lines.Count > 0 then
    begin
      // Process all queued output
      for I := 0 to Lines.Count - 1 do
      begin
        // Notify ChatForm via callback
        if Assigned(FOnOutput) then
          FOnOutput(Self);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTerminalToolAsync.ExecuteAsync(const Command: string);
begin
  // Cancel any running command
  Cancel;
  
  // Start new thread
  FCurrentThread := TTerminalThread.Create(Command, FOutputQueue);
end;

procedure TTerminalToolAsync.Cancel;
begin
  if Assigned(FCurrentThread) then
  begin
    FCurrentThread.Terminate;
    FCurrentThread := nil;
  end;
end;

function TTerminalToolAsync.GetName: string;
begin
  Result := 'run_terminal_command';
end;

function TTerminalToolAsync.GetDescription: string;
begin
  Result := 'Execute a PowerShell command asynchronously in the background';
end;

function TTerminalToolAsync.GetParameters: TToolParameterArray;
begin
  SetLength(Result, 1);
  Result[0].Name := 'command';
  Result[0].ParamType := 'string';
  Result[0].Description := 'The PowerShell command to execute';
  Result[0].Required := True;
end;

function TTerminalToolAsync.Execute(const Arguments: TJSONObject): string;
var
  Command: string;
begin
  Command := Arguments.Get('command', '');
  if Command = '' then
    Exit('Error: command parameter is required');
  
  ExecuteAsync(Command);
  Result := 'Command started: ' + Command;
end;

end.
```

## Alternative: Simpler Approach Using TAsyncProcess

If full threading is too complex, Free Pascal's `TAsyncProcess` provides a middle ground:

```pascal
type
  TTerminalTool = class
  private
    FProcess: TAsyncProcess;
    procedure OnReadData(Sender: TObject);
    procedure OnTerminate(Sender: TObject);
  public
    procedure ExecuteAsync(const Command: string);
  end;

procedure TTerminalTool.ExecuteAsync(const Command: string);
begin
  FProcess := TAsyncProcess.Create(nil);
  FProcess.Executable := 'powershell.exe';
  FProcess.Parameters.Add('-NoProfile');
  FProcess.Parameters.Add('-Command');
  FProcess.Parameters.Add(Command);
  FProcess.Options := [poUsePipes, poNoConsole];
  FProcess.OnReadData := @OnReadData;
  FProcess.OnTerminate := @OnTerminate;
  FProcess.Execute;
end;

procedure TTerminalTool.OnReadData(Sender: TObject);
var
  Buffer: string;
begin
  // Called automatically when data available
  SetLength(Buffer, FProcess.Output.NumBytesAvailable);
  FProcess.Output.Read(Buffer[1], Length(Buffer));
  
  // Update UI (already in main thread!)
  AddTerminalOutput(Buffer);
end;
```

## Conclusion

**Recommended: TThread approach with TThreadList**
- Most control over execution
- Clean separation of concerns
- Proven pattern for Lazarus

**Alternative: TAsyncProcess**
- Simpler implementation
- Less flexible
- Good enough for basic use case

Both approaches solve the blocking issue and keep the IDE responsive during command execution.
