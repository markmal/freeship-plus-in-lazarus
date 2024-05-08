unit FreeBGRbuffer;

{$mode delphi}

interface

uses
  Classes, SysUtils, FreeTypes, Graphics;

type

  TFreeBGRbuffer = class
  private
      pData : pRGBTripleArray;
      FWidth : integer;
      FHeight : integer;
      FCanvas : TCanvas;
      FImage  : TRasterImage;
      function  GetScanline(ARow: Integer): pRGBTripleArray;
      procedure ReallocateData;
      procedure setWidth(AWidth : integer);
      procedure setHeight(AHeight : integer);
      procedure SetCanvas(ACanvas : TCanvas);
  public
      constructor Create;
      procedure BeginUpdate;
      procedure EndUpdate;
      procedure SaveToFile(AFileName : String);
      function CreateBitmap:TBitmap;
      procedure SetSize(AWidth, AHeight : integer);
      procedure Fill(AColor : TColor);

      property ScanLine[Row: Integer]: pRGBTripleArray read GetScanLine; // Use only when wrpped by a begin/endupdate
      property Width : integer read FWidth write SetWidth;
      property Height : integer read FHeight write SetHeight;
      property Canvas : TCanvas read FCanvas write SetCanvas;

  end;



implementation

  constructor TFreeBGRbuffer.Create;
  begin
    inherited Create;
    //FCanvas := TCanvas.Create;
    FImage:= TRasterImage.Create;
    //FCanvas := TBitmapCanvas.Create(FImage);
  end;

  function  TFreeBGRbuffer.GetScanline(ARow: Integer): pRGBTripleArray;
  var P:pointer;
  begin
    P := pData;
    inc(P, 3 * Arow);
    result := pRGBTripleArray(P);
  end;

  procedure TFreeBGRbuffer.BeginUpdate;
  begin
  end;

  procedure TFreeBGRbuffer.EndUpdate;
  begin
  end;

  procedure TFreeBGRbuffer.ReallocateData;
  var L:integer;
  begin
    L := FWidth * FHeight*3;
    //SetLength(pData, L);
    ReallocMem(pData,L);
    // TODO make smart with copying existing image over new data
  end;

  procedure TFreeBGRbuffer.SetSize(AWidth, AHeight : integer);
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    ReallocateData;
    //FCanvas.Width := FWidth;
    //FCanvas.Height := FHeight;
  end;

  procedure TFreeBGRbuffer.SetWidth(AWidth : integer);
  begin
    FWidth := AWidth;
    ReallocateData;
    //FCanvas.Width := FWidth;
  end;

  procedure TFreeBGRbuffer.SetHeight(AHeight : integer);
  begin
    FHeight := AHeight;
    ReallocateData;
    //FCanvas.Height := FHeight;
  end;

  procedure TFreeBGRbuffer.SetCanvas(ACanvas : TCanvas);
  begin
    FCanvas := ACanvas;
  end;


  function TFreeBGRbuffer.CreateBitmap:TBitmap;
  var BM : TBitmap;
  begin
    BM := TBitmap.Create;
    BM.PixelFormat:=pf24bit;
    BM.RawImage.Init;
    BM.RawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(FWidth,FHeight);
    BM.SetSize(FWidth,FHeight);
    BM.RawImage.Data:=pByte(pData);
    result := BM;
  end;

  procedure TFreeBGRbuffer.SaveToFile(AFileName : String);
  var BM : TBitmap;
  begin
    BM := CreateBitmap;
    try
      BM.SaveToFile(AFileName);
    finally
      FreeAndNil(BM);
    end;
  end;

  procedure TFreeBGRbuffer.Fill(AColor : TColor);
  var rgb : TRGBTriple; i: integer;
  begin
    rgb.rgbtBlue := Blue(AColor);
    rgb.rgbtGreen := Green(AColor);
    rgb.rgbtRed := Red(AColor);
    for i:=0 to FWidth * FHeight do
     pData^[i] := rgb;
  end;

end.

