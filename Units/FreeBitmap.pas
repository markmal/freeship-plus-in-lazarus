{
This unit allows to work with bitmap in memory as with BGR 24-bit bitmap using ScanLine
}
unit FreeBitmap;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, FreeTypes;

type

  TFreeBitmap = class (TBitmap)
  private
      //pData : pRGBTripleArray;
      function  GetScanline(ARow: Integer): pRGBTripleArray;
  public
      constructor Create; override;
      property ScanLine[Row: Integer]: pRGBTripleArray read GetScanLine; // Use only when wrpped by a begin/endupdate
  end;

implementation

  constructor TFreeBitmap.Create;
  begin
    inherited Create;
    Self.SetPixelFormat(pf24bit);
    RawImage.Init;
    RawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(10,10);
    RawImage.CreateData(false);
  end;

  function  TFreeBitmap.GetScanline(ARow: Integer): pRGBTripleArray;
  var P:pointer;
  begin
    P := RawImage.Data;
    inc(P, RawImage.Description.BytesPerLine * Arow);
    result := pRGBTripleArray(P);
  end;

end.

