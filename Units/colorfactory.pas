{ ColorFactory prodices TColors that can be used to set chart colors in multiple series, palettes and so.
Methods supported:
Random - produces colors that color tone (Hue) is based on Phi (Golden Ratio) equi-distribution.
Random2 - produces colors that color tone (Hue) is based on Phi (Golden Ratio) equi-distribution
                 and Brightness (Value) is based on Eulers equi-distribution.
Rainbow - produces colors that tone (Hue) is growing in N-color "rainbow"
System - produces colors from standard Lazarus System palette.
}

unit ColorFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, GraphUtil, LCLType;

const
  StdColors : array[0..15] of TColor = (
    clBlack   ,
    clMaroon  ,
    clGreen   ,
    clOlive   ,
    clNavy    ,
    clPurple  ,
    clTeal    ,
    clGray    ,
    clSilver  ,
    clRed     ,
    clLime    ,
    clYellow  ,
    clBlue    ,
    clFuchsia ,
    clAqua    ,
    clWhite
    );

type
  TColorFactoryMethod = (cfmRandom, cfmRandom2, cfmRainbow, cfmStandard);

  TColorFactory = class
  private
    FStartHue : Double; //Start Hue
    FStartValue: Double;
    FHue : Double;
    FValue : Double;
    FSaturation: Double;
    FValueSpaceStart : double;
    FValueSpaceWidth : double;
    FMethod : TColorFactoryMethod;
    FColIndex : integer;
    FRainbowSize : integer;
    function HSVtoColor(H, S, V: Double): TColor;
  public
    constructor Create;
    constructor Create(StartHue:Double; Saturation:Double; Value:Double);

    function getRandomColor( I : integer):TColor;
    function getNextRandomColor:TColor;
    function getRandom2Color( I : integer):TColor; // Phi and Euler -based pseudo random Color in Hue and SubValue space both
    function getNextRandom2Color:TColor;
    function getRainbowColor(I:integer):TColor; // get I-th color out of N hue sectors, rainbow way
    function getNextRainbowColor:TColor; // get next color out of N hue sectors, rainbow way
    function getStandardColor(I:integer):TColor; // get standard color by its position
    function getNextStandardColor:TColor; // get next standard color
    function getColor( I : integer):TColor;
    function getNextColor:TColor;

    function getColorDistance(A,B:TColor):integer;
    function getNextColorFarFrom(F:TColor; minDistance:integer):TColor;

    property Value:Double read FValue write FValue;
    property Saturation:Double read FSaturation write FSaturation;
    property Hue:Double read FHue write FHue;
    property ValueSpaceStart:Double read FValueSpaceStart write FValueSpaceStart;
    property ValueSpaceWidth:Double read FValueSpaceWidth write FValueSpaceWidth;
    property RainbowSize:integer read FRainbowSize write FRainbowSize;
  end;

implementation
const PHI = (1 + sqrt(5)) / 2; //Golden Ratio
      EULER = 2.7182818284590452353602874713527; // Math E constant

constructor TColorFactory.Create;
begin
  Create(0.0, 1/PHI, 1/PHI);
end;

constructor TColorFactory.Create(StartHue:Double; Saturation:Double; Value:Double);
begin
 FHue := StartHue;
 FStartHue:=StartHue;
 FSaturation:=Saturation;
 FValue:=Value;
 FColIndex := -1;
 FMethod := cfmRandom2;
 FRainbowSize := 8;
 FValueSpaceWidth := 0.7;
 FValueSpaceStart := 0.3;
 FStartValue:=1/PHI;
end;

function TColorFactory.HSVtoColor(H, S, V: Double): TColor;
var
  i: Integer;
  f, p, q, t: Double;

  function RGBFP(R, G, B: Double): TColor;
  const
    RGBmax = 255;
  begin
    Result := RGBtoColor(Round(RGBmax * R), Round(RGBmax * G), Round(RGBmax * B));
  end;

begin
  Assert(InRange(H, 0.0, 1.0));
  Assert(InRange(S, 0.0, 1.0));
  Assert(InRange(V, 0.0, 1.0));

  if S = 0.0 then
  begin
    // achromatic (grey)
    Result := RGBFP(V, V, V);
    exit;
  end;

  H := H * 6.0; // sector 0 to 5
  i := floor(H);
  f := H - i; // fractional part of H
  p := V * (1.0 - S);
  q := V * (1.0 - S * f);
  t := V * (1.0 - S * (1.0 - f));
  case i of
  0: Result := RGBFP(V, t, p);
  1: Result := RGBFP(q, V, p);
  2: Result := RGBFP(p, V, t);
  3: Result := RGBFP(p, q, V);
  4: Result := RGBFP(t, p, V);
  else
    Result := RGBFP(V, p, q);
  end;
end;


// Phi-based pseudo random Color in Hue space
function TColorFactory.getRandomColor( I : integer):TColor;
var H: Double; C: TColor;
begin
  H := FStartHue + I/PHI; // Hue(i)
  H := H - trunc(H); // fractional part of Hue
  C := HSVtoColor(H,FSaturation,FValue);
  FHue := H;
  result := C;
end;

function TColorFactory.getNextRandomColor:TColor;
begin
  inc(FColIndex);
  result := getRandomColor(FColIndex);
end;

function TColorFactory.getNextRandom2Color:TColor;
begin
  inc(FColIndex);
  result := getRandom2Color(FColIndex);
end;

// pseudo random Color that varies in Hue (Phi-based) and SubValue (Euler-based) space both
// We use different bases to prevent producing close Value for close Hue
function TColorFactory.getRandom2Color( I : integer):TColor;
var H,V: Double; C: TColor;
begin
  H := FStartHue + I/PHI; // Hue(i)
  H := H - trunc(H); // fractional part of Hue
  // get value in SubValue space. SubValue space is ValueSpaceWidth started from ValueSpaceStart
  // so, if ValueSpaceWidth = 0.5 and ValueSpaceStart is 0.25 then it spans from 0.25 to 0.75
  // doing that we can eliminate too dark and too light values
  V := (FValue-FValueSpaceStart)/FValueSpaceWidth; // expand value from subspace
  V := FStartValue + (I*EULER/5); // get I-th golden ratio value
  V := V - trunc(V); // fractional part of Value
  V := FValueSpaceStart+V*FValueSpaceWidth; // compress value into subSpace
  C := HSVtoColor(H,FSaturation,V);
  writeln('H:',H,' V:',V);
  FHue := H;
  FValue := V;
  result := C;
end;

function TColorFactory.getRainbowColor(I:integer):TColor; // get I-th color out of N, rainbow way
var H: Double; C: TColor;
begin
  H := 1.0*I/FRainbowSize; // Hue(i)
  H := H - trunc(H); // fractional part of Hue, just in case I>N
  C := HSVtoColor(H,FSaturation,FValue);
  result := C;
end;

function TColorFactory.getNextRainbowColor:TColor; // get I-th color out of N, rainbow way
begin
  inc(FColIndex);
  if FColIndex>=FRainbowSize then FColIndex := 0;
  result := getRainbowColor(FColIndex);
end;

function TColorFactory.getStandardColor(I:integer):TColor; // get standard color by its position
begin
  result := StdColors[i mod 16];
end;

function TColorFactory.getNextStandardColor:TColor; // get standard color by its position
begin
  inc(FColIndex);
  if FColIndex>=16 then FColIndex := 0;
  result := getStandardColor(FColIndex);
end;

function TColorFactory.getColor( I : integer):TColor;
begin
  case FMethod of
    cfmRandom : result := getRandomColor(I);
    cfmRandom2 : result := getRandom2Color(I);
    cfmRainbow : result := getRainbowColor(I);
    cfmStandard : result := getStandardColor(I);
  end;
end;

function TColorFactory.getNextColor:TColor;
begin
  case FMethod of
    cfmRandom : result := getNextRandomColor;
    cfmRandom2 : result := getNextRandom2Color;
    cfmRainbow : result := getNextRainbowColor;
    cfmStandard : result := getNextStandardColor;
  end;
end;

function TColorFactory.getColorDistance(A,B:TColor):integer;
var Ha,La,Sa, Hb,Lb,Sb:byte; Ar,Br:TColorRef; rgb:longint;
begin
  rgb:=ColorToRGB(A);
  Ar := TColorRef(rgb);
  rgb:=ColorToRGB(B);
  Br := TColorRef(rgb);
  ColorToHLS(Ar,Ha,La,Sa);
  ColorToHLS(Br,Hb,Lb,Sb);
  result:=round(sqrt( (Ha-Hb)**2 + (La-Lb)**2 + (Sa-Sb)**2 ));
end;

function TColorFactory.getNextColorFarFrom(F:TColor; minDistance:integer):TColor;
var C:TColor; I:integer = 0;
begin
  C:=getNextColor;
  while (getColorDistance(C,F) > minDistance) and (I<255) do
   begin
   C:=getNextColor;
   inc(I);
   end;
  result:=C;
end;

end.

