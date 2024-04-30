unit FreeLogger;

{$mode delphi}

interface

uses
  Classes, SysUtils, FreeExceptionDlg
  {$ifdef LCL}, LazLogger{$endif};

const
  LOG_NONE = 0;
  LOG_ERROR = 1;
  LOG_WARNING = 2;
  LOG_INFO = 3;
  LOG_DEBUG = 4;
Type

  { TLogger }

  TLogger = class(TLazLoggerFile)
    private
      FLogLevel : integer;
    public
      constructor Create();
      procedure Info(S:String);
      procedure Warning(S:String);
      procedure Error(S:String);
      procedure Debug(S:String);
      procedure SetLogLevel(L:integer);
      procedure IncreaseIndent; override;
      procedure DecreaseIndent; override;
      function GetExceptionCallStack(E: Exception): string;
      procedure ShowExceptionCallStack(E: Exception);
      procedure LogExceptionCallStack(E: Exception);
      property LogLevel : integer read FLogLevel write SetLogLevel;
  end;

var Logger : TLogger;
var ExceptionDlg : TExceptionDlg;

implementation

constructor TLogger.Create();
begin
  inherited Create;
  FLogLevel := LOG_NONE;
  CloseLogFileBetweenWrites:=true;
end;

procedure TLogger.Info(S:String);
begin
  if FLogLevel >= LOG_INFO then
  begin
    {$ifdef LCL}
    debugln(S);
    {$endif}
  end;
end;


procedure TLogger.Warning(S:String);
begin
  if FLogLevel >= LOG_WARNING then
  begin
    {$ifdef LCL}
    debugln(S);
    {$endif}
  end;
end;

procedure TLogger.Error(S:String);
begin
  if FLogLevel >= LOG_ERROR then
  begin
    {$ifdef LCL}
    debugln(S);
    {$endif}
  end;
end;

procedure TLogger.Debug(S:String);
begin
  if FLogLevel >= LOG_DEBUG then
  begin
    {$ifdef LCL}
    debugln(S);
    {$endif}
  end;
end;

procedure TLogger.SetLogLevel(L:integer);
begin
  if (L < LOG_NONE) or (L > LOG_DEBUG) then
    raise Exception.Create('Log level is out of range');
  FLogLevel := L;
end;

procedure TLogger.IncreaseIndent;
begin
 inherited IncreaseIndent;
end;

procedure TLogger.DecreaseIndent;
begin
 inherited DecreaseIndent;
end;

function TLogger.GetExceptionCallStack(E: Exception):string;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Result := Result + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Result := Result + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + LineEnding + BackTraceStrFunc(Frames[I]);
end;

procedure TLogger.ShowExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := GetExceptionCallStack(E);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  logger.Error(Report);
  if ExceptionDlg<>nil then
  begin
    ExceptionDlg.Message.Text := Report;
    ExceptionDlg.Showmodal;
  end;
  //Halt; // End of program execution
end;

procedure TLogger.LogExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := GetExceptionCallStack(E);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  logger.Error(Report);
end;

initialization
  Logger := TLogger.Create;
  ExceptionDlg := TExceptionDlg.Create(nil);
finalization
  ExceptionDlg.Free;
  Logger.Free;
end.

