unit FreeLogger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

const
  LOG_NONE = 0;
  LOG_ERROR = 1;
  LOG_INFO = 2;
  LOG_DEBUG = 3;
Type
  TLogger = class
    private
      FLogLevel : integer;
    public
      constructor Create();
      procedure Info(S:String);
      procedure Error(S:String);
      procedure Debug(S:String);
      procedure SetLogLevel(L:integer);
      property LogLevel : integer read FLogLevel write SetLogLevel;
  end;

var Logger : TLogger;

implementation

constructor TLogger.Create();
begin
  FLogLevel := LOG_NONE;
end;

procedure TLogger.Info(S:String);
begin
  if FLogLevel >= LOG_INFO then
    writeln(S);
end;

procedure TLogger.Error(S:String);
begin
  if FLogLevel >= LOG_ERROR then
    writeln(S);
end;

procedure TLogger.Debug(S:String);
begin
  if FLogLevel >= LOG_DEBUG then
    writeln(S);
end;

procedure TLogger.SetLogLevel(L:integer);
begin
  if (L < LOG_NONE) or (L > LOG_DEBUG) then
    raise Exception.Create('Log level is out of range');
  FLogLevel := L;
end;

begin
  Logger := TLogger.Create;
end.

