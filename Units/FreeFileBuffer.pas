unit FreeFileBuffer;

{ $mode objfpc}
{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  StrUtils,
  LConvEncoding,
  FreeVersionUnit,
  FreeTypes,
  FreeLanguageSupport;

const
  FileBufferBlockSize = 4096;
// used for reading and writing files using TFilebuffer

type
  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeFileBuffer                                         }

  { Binary stream used to store file info                                                             }
  {---------------------------------------------------------------------------------------------------}
  TFreeFileBuffer = class
  private
    FCapacity: integer;
    // Amount of bytes allocated
    FCount: integer;
    // The amount of bytes actually used
    FPosition: integer;
    // current position when reading information from the buffer
    FVersion: TFreeFileVersion;
    FData: array of byte;
    FFileName: string;
    FFile: file;
    FEncoding: string;
    procedure FGrow(size: integer);
    procedure FSetCapacity(val: integer); virtual;
    function FGetCapacity: integer; virtual;
  public
    procedure Add(IntegerValue: integer);      overload; virtual;
    procedure Add(Text: string);               overload; virtual;
    procedure Add(BooleanValue: boolean);      overload; virtual;
    //procedure Add2(BooleanValue:Boolean);        overload;virtual;
    procedure Add(FloatValue: TFloatType);     overload; virtual;
    //procedure Add(WordValue:word);               overload;virtual;
    procedure Add(Version: TFreeFileVersion);  overload; virtual;
    procedure Add(Coordinate: T3DCoordinate);      overload; virtual;
    procedure Add(Plane: T3DPlane);                overload; virtual;
    procedure Add(Data: TFreeDelftSeriesResistanceData);      overload; virtual;
    procedure Add(Data: TFreeKAPERResistanceData);            overload; virtual;
    procedure Add(Data: TFreeHoltrSeriesResistanceData);      overload; virtual;
    procedure Add(Data: TFreeOSTSeriesResistanceData);        overload; virtual;
    procedure Add(Data: TFreeTask1PropellerData);             overload; virtual;
    procedure Add(Data: TFreeTask2PropellerData);             overload; virtual;
    procedure Add(Data: TFreeTask3PropellerData);             overload; virtual;
    procedure Add(Data: TFreePlaningResistanceData);      overload; virtual;
    procedure Add(Data: TFreeRvrsPropellerData);          overload; virtual;
    procedure Add(Data: TFreeHollenSeriesResistanceData);      overload; virtual;
    procedure Add(Data: TFreeTask4PropellerData);              overload; virtual;
    procedure Add(Data: TFreeTask5PropellerData);              overload; virtual;
    procedure Add(Data: TFreeOortmerSeriesResistanceData);      overload; virtual;
    procedure Add(Data: TFreeFungSeriesResistanceData);       overload; virtual;
    procedure Add(Data: TFreeHydrodynManeuvData);      overload; virtual;
    procedure Add(Data: TFreeHydrodynTask1Data);       overload; virtual;
    procedure Add(Data: TFreeRBHSSeriesResistanceData);      overload; virtual;
    procedure Add(Data: TFreeMHSeriesResistanceData);        overload; virtual;
    //procedure Add(const source;Size:Integer);    overload;virtual;
    procedure Add(JPegImage: TJPEGImage);      overload; virtual;

    procedure Load(var Output: integer);    virtual;overload;
    procedure Load(var Output: string);      overload; virtual;
    //procedure Load(var Output:Word);           overload;virtual;
    procedure Load(var Output: TFreeFileVersion);      overload; virtual;
    procedure Load(var Output: boolean);      overload; virtual;
    //procedure Load2(var Output:Boolean);          overload;virtual;
    procedure Load(var Output: TFloatType);      overload; virtual;
    //procedure Load(var Output: TColor);      overload; virtual;
    procedure Load(var Output: T3DCoordinate);      overload; virtual;
    procedure Load(var Output: T3DPlane);      overload; virtual;
    procedure Load(var JPegImage: TJPEGImage);      overload; virtual;
    procedure Load(var Data: TFreeKAPERResistanceData);      overload; virtual;
    procedure Load(var Data: TFreeDelftSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeHoltrSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeOSTSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeTask1PropellerData);      overload; virtual;
    procedure Load(var Data: TFreeTask2PropellerData);      overload; virtual;
    procedure Load(var Data: TFreeTask3PropellerData);      overload; virtual;
    procedure Load(var Data: TFreePlaningResistanceData); overload; virtual;
    procedure Load(var Data: TFreeRvrsPropellerData);      overload; virtual;
    procedure Load(var Data: TFreeHollenSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeTask4PropellerData);      overload; virtual;
    procedure Load(var Data: TFreeTask5PropellerData);      overload; virtual;
    procedure Load(var Data: TFreeOortmerSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeFungSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeHydrodynManeuvData);overload; virtual;
    procedure Load(var Data: TFreeHydrodynTask1Data);      overload; virtual;
    procedure Load(var Data: TFreeRBHSSeriesResistanceData); overload; virtual;
    procedure Load(var Data: TFreeMHSeriesResistanceData); overload; virtual;

    //procedure Load(var Dest;Size:Integer);       overload;virtual;
    constructor Create;
    procedure Clear;         virtual;
    procedure LoadFromFile(Filename: string);      virtual;
    procedure Reset; virtual;
    // reset the data before reading
    procedure SaveToFile(Filename: string); virtual;
    function GetPosition:integer; virtual;
    destructor Destroy; override;
    property Capacity: integer read FGetCapacity write FSetCapacity;
    property Count: integer read FCount;
    property Version: TFreeFileVersion read FVersion write FVersion;
    property Position: integer read GetPosition;
    property Encoding: string read FEncoding write FEncoding;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeTextBuffer                                         }

  { Text file used to store file info                                                                 }
  {---------------------------------------------------------------------------------------------------}
  TFreeTextBuffer = class(TFreeFileBuffer)
  private
    FLines: TStringList;
    FPosition: integer; // this is our position
    procedure FSetCapacity(val: integer); override;
    function FGetCapacity: integer; override;
  public
    constructor Create;
    procedure Add(IntegerValue: integer); override; overload;
    procedure Add(Text: string); override; overload;
    procedure Add(BooleanValue: boolean); override; overload;
    procedure Add(FloatValue: TFloatType); override; overload;
    procedure Add(PVersion: TFreeFileVersion); override; overload;
    procedure Add(Coordinate: T3DCoordinate); override; overload;
    procedure Add(Plane: T3DPlane); override; overload;
    //procedure Add(const source;Size:Integer);     override;
    procedure Add(JPegImage: TJPEGImage); override; overload;

    procedure Load(var Output: integer); override; overload;
    procedure Load(var Output: string); override; overload;
    procedure Load(var Output: TFreeFileVersion); override; overload;
    procedure Load(var Output: boolean); override; overload;
    procedure Load(var Output: TFloatType); override; overload;
    //procedure Load(var Output: TColor); override; overload;
    procedure Load(var Output: T3DCoordinate); override; overload;
    procedure Load(var Output: T3DPlane); override; overload;
    procedure Load(var JPegImage: TJPEGImage); override; overload;
    //procedure Load(var Dest;Size:Integer);        override;

    procedure Clear; override;
    procedure LoadFromFile(Filename: string); override;
    procedure Reset; override;
    // reset the data before reading
    procedure SaveToFile(Filename: string); override;
    function GetPosition:integer; override;
    destructor Destroy; override;
    property Capacity: integer read FGetCapacity write FSetCapacity;
    property Position: integer read GetPosition;
  end;



implementation


{---------------------------------------------------------------------------------------------------}
{                                           TFreeFileBuffer                                         }

{ Binary stream used to store file info                                                             }
{---------------------------------------------------------------------------------------------------}
procedure TFreeFileBuffer.FGrow(size: integer);
var
  AmountToGrow: integer;
begin
  AmountToGrow := 1024;
  if Size > AmountToGrow then
    AmountToGrow := Size;
  Capacity := Capacity + AmountToGrow;
end;{TFreeFileBuffer.FGrow}

function TFreeFileBuffer.FGetCapacity: integer;
begin
  Result := FCapacity;
end;{TFreeFileBuffer.FSetCapacity}

procedure TFreeFileBuffer.FSetCapacity(val: integer);
var
  I: integer;
begin
  Setlength(FData, Val);
  for I := FCapacity + 1 to Val do
    FData[I - 1] := 255;
  FCapacity := Val;
end;{TFreeFileBuffer.FSetCapacity}

{
procedure TFreeFileBuffer.Add(WordValue:word);
var Size:Integer;
begin
   Size:=2;//SizeOf(Word);
   if Count+Size>Capacity then FGrow(Size);
   Move(WordValue,FData[FCount],Size);
   Inc(FCount,Size);
end;}{TFreeFileBuffer.Add}


procedure TFreeFileBuffer.Add(IntegerValue: integer);
var
  Size: integer;
begin
  Size := 4;//SizeOf(Integer);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(NtoLE(IntegerValue), FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Add(Version: TFreeFileVersion);
var
  Size: integer;
begin
  FVersion := Version;
  Size := SizeOf(Version);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(Version, FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Add(Coordinate: T3DCoordinate);
var
  Size: integer;
begin
  Size := SizeOf(Coordinate);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(Coordinate, FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Add(Plane: T3DPlane);
var
  Size: integer;
begin
  Size := SizeOf(Plane);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(Plane, FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

{
// MM: This may be not endiness safe. Will replace it typed
procedure TFreeFileBuffer.Add(const source;Size:Integer);
var S: PChar;
    I: Integer;
begin
   S:=PChar(@Source);
   if Count+Size>Capacity then FGrow(Size);
   for I := 0 to size-1 do
   begin
      FData[FCount]:=Byte(S[I]);
      Inc(FCount);
   end;
end;}
{TFreeFileBuffer.Add}


procedure TFreeFileBuffer.Add(JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  Size: integer;
begin
  Add(JPEGImage.Width);
  Add(JPEGImage.Height);
  Stream := TMemoryStream.Create;
  JPEGImage.SaveToStream(Stream);
  Size := Stream.Size;
  Stream.Position := 0;
  Add(Size);
  if Count + Size + 20 > Capacity then
    FGrow(Size + 20);
  Stream.Read(FData[FCount], Size);
  Inc(FCount, Size);
  Stream.Destroy;
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Load(var JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  W, H, Size: integer;
begin
  Load(W);
  Load(H);
  Load(Size);
  Stream := TMemoryStream.Create;
  Stream.SetSize(Size);
  Stream.Write(FData[FPosition], Size);
  Inc(FPosition, Size);
  Stream.Position := 0;
  JPEGImage.LoadFromStream(Stream);
  Stream.Destroy;
end;{TFreeFileBuffer.Add}

{
procedure TFreeFileBuffer.Load(var Dest;Size:Integer);
var D : PChar;
    I : Integer;
begin
   D := PChar(@Dest);
   if FPosition+Size>FCount then
     begin
       raise Exception.Create(UserString(192)+'_0 ! Load data'+EOL
       +'Position+Size:'+IntToStr(FPosition+Size)
       +' > '+IntToStr(FCount)
       +EOL+FFileName);
       //MessageDlg(UserString(192)+'_0 !',mtError,[mbOk],0);
       exit;
     end;
   for I:=0 to size-1 do
   begin
      D[I]:=Char(FData[FPosition]);
      Inc(FPosition);
   end;
end;}{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeDelftSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(WlArea);
    Load(EstimateWetSurf);
    // Structures are aligned to 2 bytes, so load Boolean as Word
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeKAPERResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Draft);
    Load(Lwl);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(LCB);
    Load(WettedSurface);
    Load(At_Ax);
    Load(EntranceAngle);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeHoltrSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(WlArea);
    Load(Ke);
    Load(BA);
    Load(KBulb);
    Load(ZBulb);
    Load(Cstrn);
    Load(Np);
    Load(Dp);
    Load(Ks);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeOSTSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(WlArea);
    Load(Ke);
    Load(Ks);
    Load(Nser);
    Load(Na);
    Load(Nf);
    Load(Np);
    Load(Dp);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(Dat17_1);
    Load(Dat17_2);
    Load(Dat17_3);
    Load(Dat17_4);
    Load(Dat17_5);
    Load(Dat18_1);
    Load(Dat18_2);
    Load(Dat18_3);
    Load(Dat18_4);
    Load(Dat18_5);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeTask1PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeTask2PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Dat17_1);
    Load(Dat17_2);
    Load(Dat17_3);
    Load(Dat17_4);
    Load(Dat17_5);
    Load(Dat18_1);
    Load(Dat18_2);
    Load(Dat18_3);
    Load(Dat18_4);
    Load(Dat18_5);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeTask3PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Dat17);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreePlaningResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Draft);
    Load(Lwl);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(LCB);
    Load(WettedSurface);
    Load(At_Ax);
    Load(EntranceAngle);
    Load(Sa);
    Load(Caa);
    Load(Angle);
    Load(K);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeRvrsPropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Dat17);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeHollenSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(Los);
    Load(Ke);
    Load(BA);
    Load(KBulb);
    Load(ZBulb);
    Load(Cstrn);
    Load(Np);
    Load(Dp);
    Load(Ks);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeTask4PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeTask5PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeOortmerSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(Los);
    Load(Ke);
    Load(BA);
    Load(KBulb);
    Load(ZBulb);
    Load(Cstrn);
    Load(Np);
    Load(Dp);
    Load(Ks);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeFungSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(Los);
    Load(Ke);
    Load(BA);
    Load(KBulb);
    Load(ZBulb);
    Load(Cstrn);
    Load(Np);
    Load(Dp);
    Load(Ks);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeHydrodynManeuvData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Dat13);
    Load(Dat14);
    Load(Dat15);
    Load(Dat16);
    Load(Dat17);
    Load(Dat18);
    Load(Dat19);
    Load(Dat20);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeHydrodynTask1Data);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(Dat2);
    Load(Dat3);
    Load(Dat4);
    Load(Dat5);
    Load(Dat6);
    Load(Dat7);
    Load(Dat8);
    Load(Dat9);
    Load(Dat10);
    Load(Dat11);
    Load(Dat12);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeRBHSSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(WlArea);
    Load(Ke);
    Load(Ks);
    Load(Nser);
    Load(Na);
    Load(Nf);
    Load(Np);
    Load(Dp);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(Dat17_1);
    Load(Dat17_2);
    Load(Dat17_3);
    Load(Dat17_4);
    Load(Dat17_5);
    Load(Dat18_1);
    Load(Dat18_2);
    Load(Dat18_3);
    Load(Dat18_4);
    Load(Dat18_5);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Data: TFreeMHSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    Load(StartSpeed);
    Load(EndSpeed);
    Load(StepSpeed);
    Load(Bwl);
    Load(Cp);
    Load(Displacement);
    Load(Draft);
    Load(DraftTotal);
    Load(KeelChordLength);
    Load(KeelArea);
    Load(LCB);
    Load(Lwl);
    Load(RudderChordLength);
    Load(RudderArea);
    Load(Viscosity);
    Load(WettedSurface);
    Load(WlArea);
    Load(Ke);
    Load(Ks);
    Load(Nser);
    Load(Na);
    Load(Nf);
    Load(Np);
    Load(Dp);
    Load(K1);
    Load(K2);
    Load(K3);
    Load(K4);
    Load(K5);
    Load(K6);
    Load(K7);
    Load(A1);
    Load(A2);
    Load(A3);
    Load(A4);
    Load(A5);
    Load(A6);
    Load(A7);
    Load(A8);
    Load(A9);
    Load(A10);
    Load(A11);
    Load(Dat17_1);
    Load(Dat17_2);
    Load(Dat17_3);
    Load(Dat17_4);
    Load(Dat17_5);
    Load(Dat18_1);
    Load(Dat18_2);
    Load(Dat18_3);
    Load(Dat18_4);
    Load(Dat18_5);
    Load(EstimateWetSurf);
    Load(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.Load}


////////// Add
procedure TFreeFileBuffer.Add(Data: TFreeDelftSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(WlArea);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeKAPERResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Draft);
    Add(Lwl);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(LCB);
    Add(WettedSurface);
    Add(At_Ax);
    Add(EntranceAngle);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Add(Data: TFreeHoltrSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(WlArea);
    Add(Ke);
    Add(BA);
    Add(KBulb);
    Add(ZBulb);
    Add(Cstrn);
    Add(Np);
    Add(Dp);
    Add(Ks);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeOSTSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(WlArea);
    Add(Ke);
    Add(Ks);
    Add(Nser);
    Add(Na);
    Add(Nf);
    Add(Np);
    Add(Dp);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(Dat17_1);
    Add(Dat17_2);
    Add(Dat17_3);
    Add(Dat17_4);
    Add(Dat17_5);
    Add(Dat18_1);
    Add(Dat18_2);
    Add(Dat18_3);
    Add(Dat18_4);
    Add(Dat18_5);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeTask1PropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeTask2PropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Dat17_1);
    Add(Dat17_2);
    Add(Dat17_3);
    Add(Dat17_4);
    Add(Dat17_5);
    Add(Dat18_1);
    Add(Dat18_2);
    Add(Dat18_3);
    Add(Dat18_4);
    Add(Dat18_5);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeTask3PropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Dat17);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreePlaningResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Draft);
    Add(Lwl);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(LCB);
    Add(WettedSurface);
    Add(At_Ax);
    Add(EntranceAngle);
    Add(Sa);
    Add(Caa);
    Add(Angle);
    Add(K);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeRvrsPropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Dat17);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeHollenSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(Los);
    Add(Ke);
    Add(BA);
    Add(KBulb);
    Add(ZBulb);
    Add(Cstrn);
    Add(Np);
    Add(Dp);
    Add(Ks);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeTask4PropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeTask5PropellerData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeOortmerSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(Los);
    Add(Ke);
    Add(BA);
    Add(KBulb);
    Add(ZBulb);
    Add(Cstrn);
    Add(Np);
    Add(Dp);
    Add(Ks);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeFungSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(Los);
    Add(Ke);
    Add(BA);
    Add(KBulb);
    Add(ZBulb);
    Add(Cstrn);
    Add(Np);
    Add(Dp);
    Add(Ks);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeHydrodynManeuvData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Dat13);
    Add(Dat14);
    Add(Dat15);
    Add(Dat16);
    Add(Dat17);
    Add(Dat18);
    Add(Dat19);
    Add(Dat20);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeHydrodynTask1Data);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(Dat2);
    Add(Dat3);
    Add(Dat4);
    Add(Dat5);
    Add(Dat6);
    Add(Dat7);
    Add(Dat8);
    Add(Dat9);
    Add(Dat10);
    Add(Dat11);
    Add(Dat12);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeRBHSSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(WlArea);
    Add(Ke);
    Add(Ks);
    Add(Nser);
    Add(Na);
    Add(Nf);
    Add(Np);
    Add(Dp);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(Dat17_1);
    Add(Dat17_2);
    Add(Dat17_3);
    Add(Dat17_4);
    Add(Dat17_5);
    Add(Dat18_1);
    Add(Dat18_2);
    Add(Dat18_3);
    Add(Dat18_4);
    Add(Dat18_5);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Data: TFreeMHSeriesResistanceData);
var
  Size: integer = sizeof(Data);
  bp: integer;
begin
  bp := FCount;
  if Count + Size > Capacity then
    FGrow(Size);
  with Data do
  begin
    Add(StartSpeed);
    Add(EndSpeed);
    Add(StepSpeed);
    Add(Bwl);
    Add(Cp);
    Add(Displacement);
    Add(Draft);
    Add(DraftTotal);
    Add(KeelChordLength);
    Add(KeelArea);
    Add(LCB);
    Add(Lwl);
    Add(RudderChordLength);
    Add(RudderArea);
    Add(Viscosity);
    Add(WettedSurface);
    Add(WlArea);
    Add(Ke);
    Add(Ks);
    Add(Nser);
    Add(Na);
    Add(Nf);
    Add(Np);
    Add(Dp);
    Add(K1);
    Add(K2);
    Add(K3);
    Add(K4);
    Add(K5);
    Add(K6);
    Add(K7);
    Add(A1);
    Add(A2);
    Add(A3);
    Add(A4);
    Add(A5);
    Add(A6);
    Add(A7);
    Add(A8);
    Add(A9);
    Add(A10);
    Add(A11);
    Add(Dat17_1);
    Add(Dat17_2);
    Add(Dat17_3);
    Add(Dat17_4);
    Add(Dat17_5);
    Add(Dat18_1);
    Add(Dat18_2);
    Add(Dat18_3);
    Add(Dat18_4);
    Add(Dat18_5);
    Add(EstimateWetSurf);
    Add(Extract);
  end;
  FCount := bp + Size;
end;{TFreeFileBuffer.Load}


//////////


procedure TFreeFileBuffer.Load(var Output: string);
var
  I, Size: integer;
  Ch: char;
begin
  Load(Size);
  Output := '';
  if FPosition + Size <= FCount then
  begin
    for I := 1 to Size do
    begin
      Ch := char(FData[FPosition]);
      Inc(FPosition);
      Output := Output + Ch;
    end;
    Output := ConvertEncoding(Output,FEncoding,'utf8');
  end
  else
    raise Exception.Create(UserString(192) + '_0 ! Load String' + EOL
      + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' + IntToStr(
      FCount) + EOL + FFileName);
  //MessageDlg(UserString(192)+'_0 !',mtError,[mbOk],0);
end;{TFreeFileBuffer.Load}

{
procedure TFreeFileBuffer.Load(var Output:Word);
var Size : Integer;
begin
   Size:=2;
   Output:=0;
   if FPosition+Size<=FCount then
   begin
      Move(FData[FPosition],Output,Size);
      Inc(FPosition,Size);
   end
   else
   raise Exception.Create(UserString(192)+'_1 ! Load Integer'+EOL
      +'Position+Size:'+IntToStr(FPosition+Size)
      +' > '+IntToStr(FCount)
      +EOL+FFileName);
   //MessageDlg(UserString(192)+'_1 !',mtError,[mbOk],0);
end;}{TFreeFileBuffer.Load}


procedure TFreeFileBuffer.Load(var Output: integer);
var
  Size: integer;
begin
  Size := 4;
  Output := 0;
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Output := LEtoN(Output);
    Inc(FPosition, Size);
  end
  else
    raise Exception.Create(UserString(192) + '_1 ! Load Integer' + EOL
      + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' + IntToStr(
      FCount) + EOL + FFileName);
  //MessageDlg(UserString(192)+'_1 !',mtError,[mbOk],0);
end; {TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Output: TFreeFileVersion);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else
    raise Exception.Create(UserString(192) + '_2 ! Load TFreeFileVersion' +
      EOL + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' +
      IntToStr(FCount) + EOL + FFileName);
  //MessageDlg(UserString(192)+'_2 !',mtError,[mbOk],0);
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Output: boolean);
var
  Size: integer;
begin
  Size := 1;
  Output := False;
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else //MessageDlg(UserString(192)+'_3 !',mtError,[mbOk],0);
    raise Exception.Create(UserString(192) + '_3 ! Load Boolean' + EOL
      + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' + IntToStr(
      FCount) + EOL + FFileName);
end;{TFreeFileBuffer.Load}

{
procedure TFreeFileBuffer.Load2(var Output:Boolean);
var Size : Integer; b2: word;
begin
   Size:=2;
   Output:=False;
   if FPosition+Size<=FCount then
   begin
      Move(FData[FPosition],b2,Size);
      Output := b2 <> 0;
      Inc(FPosition,Size);
   end
   else //MessageDlg(UserString(192)+'_3 !',mtError,[mbOk],0);
   raise Exception.Create(UserString(192)+'_3 ! Load Boolean'+EOL
      +'Position+Size:'+IntToStr(FPosition+Size)
      +' > '+IntToStr(FCount)
      +EOL+FFileName);
end;}{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Output: TFloatType);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  Output := 0.0;
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else //MessageDlg(UserString(192)+'_4 !',mtError,[mbOk],0);
    raise Exception.Create(UserString(192) + '_4 ! Load TFloatType' +
      EOL + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' +
      IntToStr(FCount) + EOL + FFileName);
end;{TFreeFileBuffer.Load}

{
procedure TFreeFileBuffer.Load(var Output: TColor);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  Output := clBlack;
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else //MessageDlg(UserString(192)+'_5 !',mtError,[mbOk],0);
    raise Exception.Create(UserString(192) + '_5 ! Load TColor' + EOL
      + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' + IntToStr(
      FCount) + EOL + FFileName);
end;}{TFreeFileBuffer.Load}


procedure TFreeFileBuffer.Load(var Output: T3DCoordinate);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  Output := ZERO;
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else //MessageDlg(UserString(192)+'_6 !',mtError,[mbOk],0);
    raise Exception.Create(UserString(192) + '_6 Load T3DCoordinate!' +
      EOL + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' +
      IntToStr(FCount) + EOL + FFileName);

end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Load(var Output: T3DPlane);
var
  Size: integer;
  PLE: T3DPlane;
begin
  Size := SizeOf(Output);
  if FPosition + Size <= FCount then
  begin
    Move(FData[FPosition], Output, Size);
    Inc(FPosition, Size);
  end
  else //MessageDlg(UserString(192)+'_7 !',mtError,[mbOk],0);
    raise Exception.Create(UserString(192) + '_7 ! Load T3DPlane' + EOL
      + 'Position+Size:' + IntToStr(FPosition + Size) + ' > ' + IntToStr(
      FCount) + EOL + FFileName);
end;{TFreeFileBuffer.Load}

procedure TFreeFileBuffer.Add(Text: string);
var
  Size: integer;
begin
  // convert text from UTF8 to Windows ANSI
  Text:=ConvertEncoding(Text,'utf8',FEncoding);
  Size := Length(Text);
  Add(Size);
  if Size = 0 then
    exit;
  if Count + Size > Capacity then
    FGrow(Size);
  Move(Text[1], FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

procedure TFreeFileBuffer.Add(BooleanValue: boolean);
var
  Size: integer;
begin
  Size := 1;//SizeOf(BooleanValue);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(BooleanValue, FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

{
procedure TFreeFileBuffer.Add2(BooleanValue:Boolean);
var Size:Integer; b2: word;
begin
   Size:=2;//SizeOf(BooleanValue);
   if Count+Size>Capacity then FGrow(Size);
   Move(b2,FData[FCount],Size);
   BooleanValue := b2 <> 0;
   Inc(FCount,Size);
end;}{TFreeFileBuffer.Add}


procedure TFreeFileBuffer.Add(FloatValue: TFloatType);
var
  Size: integer;
begin
  Size := SizeOf(FloatValue);
  if Count + Size > Capacity then
    FGrow(Size);
  Move(FloatValue, FData[FCount], Size);
  Inc(FCount, Size);
end;{TFreeFileBuffer.Add}

constructor TFreeFileBuffer.Create;
begin
  inherited Create;
  Clear;
end;{TFreeFileBuffer.Create}

procedure TFreeFileBuffer.Clear;
begin
  FCapacity := 0;
  FCount := 0;
  FPosition := 0;
  Setlength(FData, 0);
  FFileName := '';
  FEncoding := 'cp1252';
end;{TFreeFileBuffer.Clear}

destructor TFreeFileBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;{TFreeFileBuffer.Destroy}

procedure TFreeFileBuffer.LoadFromFile(Filename: string);
var
  DataLeft: integer;
  Tmp: integer;
  Size: integer;
begin
  FFileName := Filename;
  AssignFile(FFile, Filename);
  system.Reset(FFile, 1);
  FCount := 0;
  DataLeft := FileSize(FFile);
  Capacity := DataLeft;
  FPosition := 0;
  if DataLeft < 0 then
    exit;
  while DataLeft > 0 do
  begin
    if DataLeft < FileBufferBlockSize then
      Size := DataLeft
    else
      Size := FileBufferBlockSize;
    BlockRead(FFile, FData[FCount], Size, Tmp);
    Dec(DataLeft, Tmp);
    Inc(FCount, Tmp);
  end;
  Closefile(FFile);
  FFileName := '';
end;{TFreeFileBuffer.LoadFromFile}

// reset the data before reading
procedure TFreeFileBuffer.Reset;
begin
  FPosition := 0;
end;{TFreeFileBuffer.Reset}

procedure TFreeFileBuffer.SaveToFile(Filename: string);
var
  DataWritten: integer;
  DataLeft: integer;
  Tmp: integer;
  Size: integer;
begin
  FFileName := Filename;
  AssignFile(FFile, Filename);
  Rewrite(FFile, 1);
  DataWritten := 0;
  DataLeft := Count;
  while DataWritten < Count do
  begin
    if DataLeft < FileBufferBlockSize then
      Size := DataLeft
    else
      Size := FileBufferBlockSize;
    BlockWrite(FFile, FData[DataWritten], Size, Tmp);
    Dec(DataLeft, Tmp);
    Inc(DataWritten, Tmp);
  end;
  Closefile(FFile);
  FFileName := '';
end;{TFreeFileBuffer.SaveToFile}

function TFreeFileBuffer.GetPosition:integer;
begin
  Result:=FPosition;
end;{TFreeFileBuffer.GetPosition}



{---------------------------------------------------------------------------------------------------}
{                                           TFreeTextBuffer                                         }

{ Text file used to store file info                                                             }
{---------------------------------------------------------------------------------------------------}
constructor TFreeTextBuffer.Create;
begin
  FLines := TStringList.Create;
  inherited Create;
end;{TFreeTextBuffer.Create}

function TFreeTextBuffer.FGetCapacity: integer;
begin
  Result := FLines.Capacity;
end;{TFreeTextBuffer.FSetCapacity}

procedure TFreeTextBuffer.FSetCapacity(val: integer);
var
  I: integer;
begin
  FLines.Capacity := val;
end;{TFreeTextBuffer.FSetCapacity}

procedure TFreeTextBuffer.Clear;
var
  Size: integer;
  S: string;
begin
  if FLines <> nil then
    FLines.Clear;
  inherited Clear;
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(Text: string);
var
  Size: integer;
  S: string;
begin
  S := ReplaceStr(Text, '\', '\\');
  S := ReplaceStr(S, EOL, '\n');
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(BooleanValue: boolean);
var
  Size: integer;
  S: string;
begin
  //if BooleanValue then S:='True' else S:='False';
  S := BoolToStr(BooleanValue);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(FloatValue: TFloatType);
var
  Size: integer;
  S: string;
begin
  S := FloatToStr(FloatValue);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(IntegerValue: integer);
var
  Size: integer;
  S: string;
begin
  S := IntToStr(IntegerValue);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}


procedure TFreeTextBuffer.Add(PVersion: TFreeFileVersion);
var
  Size: integer;
  S: string;
begin
  FVersion := PVersion;
  S := VersionString(PVersion);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}


procedure TFreeTextBuffer.Add(Coordinate: T3DCoordinate);
var
  Size: integer;
  S: string;
begin
  S := FloatToStr(Coordinate.X) + ' ' + FloatToStr(Coordinate.Y) + ' ' + FloatToStr(Coordinate.Z);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(Plane: T3DPlane);
var
  Size: integer;
  S: string;
begin
  S := FloatToStr(Plane.a) + ' ' + FloatToStr(Plane.b) + ' '
     + FloatToStr(Plane.c) + ' ' + FloatToStr(Plane.d);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  Size: integer;
  S: PChar;
  P: PChar;
  L: string;
begin
  Add(JPEGImage.Width);
  Add(JPEGImage.Height);

  Stream := TMemoryStream.Create;
  JPEGImage.SaveToStream(Stream);
  Size := Stream.Size;
  Stream.Position := 0;
  Add(Size);

  S := StrAlloc(Size * 2 + 1);
  S[Size * 2] := #0;
  S[Size * 2 + 1] := #0;
  P := Stream.Memory;
  BinToHex(P, S, size);
  L := StrPas(S);
  StrDispose(S);
  FLines.Add(L);
  Stream.Destroy;
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Load(var JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  W, H, Size: integer;
  PData: PChar;
begin
  Load(W);
  Load(H);
  Load(Size);
  PData := StrAlloc(Size);
  Stream := TMemoryStream.Create;
  Stream.SetSize(Size);
  HexToBin(PChar(FLines[FPosition]), Stream.Memory, Size);
  //Stream.Write(PData, Size);
  StrDispose(PData);
  Stream.Position := 0;
  JPEGImage.LoadFromStream(Stream);
  Stream.Destroy;
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Load(var Output: integer);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToInt(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: string);
var
  S: string;
begin
  S := FLines[FPosition];
  S := ReplaceStr(S, '\n', EOL);
  S := ReplaceStr(S, '\\', '\');
  Output := S;
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: TFreeFileVersion);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := VersionBinary(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: boolean);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToBool(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: TFloatType);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToFloat(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

{
procedure TFreeTextBuffer.Load(var Output: TColor);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToInt(S);
  Inc(FPosition);
end;}{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: T3DCoordinate);
var
  S: string;
begin
  S := FLines[FPosition];
  Output.X := StrToFloat(ExtractWord(1, S, [' ']));
  Output.Y := StrToFloat(ExtractWord(2, S, [' ']));
  Output.Z := StrToFloat(ExtractWord(3, S, [' ']));
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

procedure TFreeTextBuffer.Load(var Output: T3DPlane);
var
  S: string;
begin
  S := FLines[FPosition];
  Output.a := StrToFloat(ExtractWord(1, S, [' ']));
  Output.b := StrToFloat(ExtractWord(2, S, [' ']));
  Output.c := StrToFloat(ExtractWord(3, S, [' ']));
  Output.d := StrToFloat(ExtractWord(4, S, [' ']));
  Inc(FPosition);
end;{TFreeTextBuffer.Load}

destructor TFreeTextBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;{TFreeTextBuffer.Destroy}

procedure TFreeTextBuffer.LoadFromFile(Filename: string);
begin
  FFileName := Filename;
  FLines.LoadFromFile(Filename);
  FPosition := 0;
end;{TFreeTextBuffer.LoadFromFile}

// reset the data before reading
procedure TFreeTextBuffer.Reset;
begin
  FPosition := 0;
end;{TFreeTextBuffer.Reset}

procedure TFreeTextBuffer.SaveToFile(Filename: string);
var
  DataWritten: integer;
  DataLeft: integer;
  Tmp: integer;
  Size: integer;
begin
  FFileName := Filename;
  FLines.SaveToFile(Filename);
end;{TFreeTextBuffer.SaveToFile}

function TFreeTextBuffer.GetPosition:integer;
begin
  Result:=FPosition;
end;{TFreeFileBuffer.GetPosition}

end.
