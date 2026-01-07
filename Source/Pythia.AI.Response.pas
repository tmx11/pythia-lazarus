unit Pythia.AI.Response;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, Pythia.Tools;

type
  // AI response can contain text AND/OR tool calls
  TAIResponse = record
    Text: string;                    // Regular text response
    ToolCalls: TToolCallArray;       // Tool calls requested by AI
    HasToolCalls: Boolean;           // Whether tool calls are present
    FinishReason: string;            // 'stop', 'tool_calls', 'length', etc.
  end;

// Parse OpenAI response (supports both regular and tool-calling responses)
function ParseOpenAIResponse(const JSONResponse: string): TAIResponse;

// Parse Anthropic response (supports both regular and tool-calling responses)
function ParseAnthropicResponse(const JSONResponse: string): TAIResponse;

implementation

function ParseOpenAIResponse(const JSONResponse: string): TAIResponse;
var
  JSON, Choice, Message, ToolCall: TJSONObject;
  Choices, ToolCalls: TJSONArray;
  I: Integer;
begin
  Result.Text := '';
  SetLength(Result.ToolCalls, 0);
  Result.HasToolCalls := False;
  Result.FinishReason := '';
  
  try
    JSON := GetJSON(JSONResponse) as TJSONObject;
    try
      if not JSON.Find('choices', Choices) then
        Exit;
      if Choices.Count = 0 then
        Exit;
        
      Choice := Choices.Objects[0];
      Message := Choice.Objects['message'];
      
      // Get finish reason
      Result.FinishReason := Choice.Get('finish_reason', 'stop');
      
      // Check for regular text content
      if Message.IndexOfName('content') >= 0 then
        Result.Text := Message.Get('content', '');
      
      // Check for tool calls
      if Message.Find('tool_calls', ToolCalls) then
      begin
        Result.HasToolCalls := True;
        SetLength(Result.ToolCalls, ToolCalls.Count);
        
        for I := 0 to ToolCalls.Count - 1 do
        begin
          ToolCall := ToolCalls.Objects[I];
          Result.ToolCalls[I].CallId := ToolCall.Get('id', '');
          Result.ToolCalls[I].ToolName := ToolCall.Objects['function'].Get('name', '');
          Result.ToolCalls[I].Arguments := ToolCall.Objects['function'].Get('arguments', '{}');
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      Result.Text := 'Error parsing response: ' + E.Message;
  end;
end;

function ParseAnthropicResponse(const JSONResponse: string): TAIResponse;
var
  JSON, ContentItem: TJSONObject;
  Content: TJSONArray;
  I: Integer;
  ItemType: string;
begin
  Result.Text := '';
  SetLength(Result.ToolCalls, 0);
  Result.HasToolCalls := False;
  Result.FinishReason := '';
  
  try
    JSON := GetJSON(JSONResponse) as TJSONObject;
    try
      // Get finish reason (stop_reason in Anthropic)
      Result.FinishReason := JSON.Get('stop_reason', 'end_turn');
      
      if not JSON.Find('content', Content) then
        Exit;
        
      // Anthropic returns array of content blocks
      for I := 0 to Content.Count - 1 do
      begin
        ContentItem := Content.Objects[I];
        ItemType := ContentItem.Get('type', '');
        
        if ItemType = 'text' then
        begin
          // Regular text response
          if Result.Text <> '' then
            Result.Text := Result.Text + #13#10;
          Result.Text := Result.Text + ContentItem.Get('text', '');
        end
        else if ItemType = 'tool_use' then
        begin
          // Tool call
          Result.HasToolCalls := True;
          SetLength(Result.ToolCalls, Length(Result.ToolCalls) + 1);
          with Result.ToolCalls[High(Result.ToolCalls)] do
          begin
            CallId := ContentItem.Get('id', '');
            ToolName := ContentItem.Get('name', '');
            // Anthropic's input is already a JSON object
            Arguments := ContentItem.Objects['input'].AsJSON;
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      Result.Text := 'Error parsing response: ' + E.Message;
  end;
end;

end.
