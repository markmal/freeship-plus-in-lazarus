unit FreePrinter;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface
uses
{$IFDEF FPC}
  LCLIntf,
  LCLType,
  //LMessages,
  LResources,
  //PrintersDlgs,
  Printer4Lazarus,
{$ENDIF}
Classes,
SysUtils,
Printers;

{$ifdef FPC}
procedure AssignPrn( var f:TextFile);
{$endif}

implementation

{$ifdef FPC}
procedure AssignPrn( var f:TextFile);
begin
   Assign(f, Printer.FileName );
end;
{$endif}


end.

