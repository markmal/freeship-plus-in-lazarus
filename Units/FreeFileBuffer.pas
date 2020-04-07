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
  TNameData = record N:integer; Name:String; end;
  TLinearConstraintData = record N, LinearConstraintPointA, LinearConstraintPointB:integer; end;
  TAnchorData = record N, AnchorPoint:integer; IsAnchorHard:boolean; end;

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
    procedure Add(words: TStrings);            overload; virtual;
    //procedure Add(WordValue:word);               overload;virtual;
    procedure Add(Version: TFreeFileVersion);  overload; virtual;
    procedure Add(Coordinate: T3DCoordinate);      overload; virtual;
    procedure Add(NameData: TNameData);                overload; virtual;
    procedure Add(AnchorData: TAnchorData);                overload; virtual;
    procedure Add(LCData: TLinearConstraintData);      overload; virtual;
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

    procedure LoadInteger(var Output: integer);    virtual;
    procedure LoadString(var Output: string);      virtual;
    //procedure Load(var Output:Word);           virtual;
    procedure LoadTStrings(var Output: TStrings);        virtual;
    procedure LoadTFreeFileVersion(var Output: TFreeFileVersion);      virtual;
    procedure LoadBoolean(var Output: boolean);      virtual;
    procedure LoadTColor(var Output: TColor); virtual;
    procedure LoadTNameData(var NameData: TNameData); virtual;
    procedure LoadTAnchorData(var AnchorData: TAnchorData);  virtual;
    procedure LoadTLinearConstraintData(var LCData: TLinearConstraintData);  virtual;
    procedure LoadTFloatType(var Output: TFloatType);       virtual;
    procedure LoadT3DCoordinate(var Output: T3DCoordinate);       virtual;
    procedure LoadT3DPlane(var Output: T3DPlane);       virtual;
    procedure LoadTJPEGImage(var JPegImage: TJPEGImage);       virtual;
    procedure LoadTFreeKAPERResistanceData(var Data: TFreeKAPERResistanceData);       virtual;
    procedure LoadTFreeDelftSeriesResistanceData(var Data: TFreeDelftSeriesResistanceData);  virtual;
    procedure LoadTFreeHoltrSeriesResistanceData(var Data: TFreeHoltrSeriesResistanceData);  virtual;
    procedure LoadTFreeOSTSeriesResistanceData(var Data: TFreeOSTSeriesResistanceData);  virtual;
    procedure LoadTFreeTask1PropellerData(var Data: TFreeTask1PropellerData);       virtual;
    procedure LoadTFreeTask2PropellerData(var Data: TFreeTask2PropellerData);       virtual;
    procedure LoadTFreeTask3PropellerData(var Data: TFreeTask3PropellerData);       virtual;
    procedure LoadTFreePlaningResistanceData(var Data: TFreePlaningResistanceData);  virtual;
    procedure LoadTFreeRvrsPropellerData(var Data: TFreeRvrsPropellerData);       virtual;
    procedure LoadTFreeHollenSeriesResistanceData(var Data: TFreeHollenSeriesResistanceData);  virtual;
    procedure LoadTFreeTask4PropellerData(var Data: TFreeTask4PropellerData);       virtual;
    procedure LoadTFreeTask5PropellerData(var Data: TFreeTask5PropellerData);       virtual;
    procedure LoadTFreeOortmerSeriesResistanceData(var Data: TFreeOortmerSeriesResistanceData);  virtual;
    procedure LoadTFreeFungSeriesResistanceData(var Data: TFreeFungSeriesResistanceData);  virtual;
    procedure LoadTFreeHydrodynManeuvData(var Data: TFreeHydrodynManeuvData); virtual;
    procedure LoadTFreeHydrodynTask1Data(var Data: TFreeHydrodynTask1Data);       virtual;
    procedure LoadTFreeRBHSSeriesResistanceData(var Data: TFreeRBHSSeriesResistanceData);  virtual;
    procedure LoadTFreeMHSeriesResistanceData(var Data: TFreeMHSeriesResistanceData);  virtual;

    //procedure Load(var Dest;Size:Integer);       overload;virtual;
    constructor Create;
    procedure Clear;         virtual;
    procedure LoadFromFile(Filename: string);      virtual;
    procedure Reset; virtual;
    // reset the data before reading
    function SaveToFile(Filename: string):boolean; virtual;
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
    procedure Add(words: TStrings); override; overload;
    procedure Add(PVersion: TFreeFileVersion); override; overload;
    procedure Add(NameData: TNameData); override; overload;
    procedure Add(LCData: TLinearConstraintData); override; overload;
    procedure Add(Coordinate: T3DCoordinate); override; overload;
    procedure Add(Plane: T3DPlane); override; overload;
    //procedure Add(const source;Size:Integer);     override;
    procedure Add(JPegImage: TJPEGImage); override; overload;

    procedure LoadInteger(var Output: integer); override;
    procedure LoadString(var Output: string); override;
    procedure LoadTFreeFileVersion(var Output: TFreeFileVersion); override;
    procedure LoadBoolean(var Output: boolean); override;
    procedure LoadTColor(var Output: TColor); override;
    procedure LoadTFloatType(var Output: TFloatType); override;
    procedure LoadTStrings(var Output: TStrings); override;
    procedure LoadTNameData(var Output: TNameData); override;
    procedure LoadTLinearConstraintData(var Output: TLinearConstraintData); override;
        //procedure LoadTFreeMHSeriesResistanceData(var Output: TColor); override;
    procedure LoadT3DCoordinate(var Output: T3DCoordinate); override;
    procedure LoadT3DPlane(var Output: T3DPlane); override;
    procedure LoadTJPEGImage(var JPegImage: TJPEGImage); override;
    //procedure LoadTFreeMHSeriesResistanceData(var Dest;Size:Integer);        override;

    procedure Clear; override;
    procedure LoadFromFile(Filename: string); override;
    procedure Reset; override;
    // reset the data before reading
    function SaveToFile(Filename: string):boolean; override;
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

procedure TFreeFileBuffer.Add(NameData: TNameData);
begin
  Add(NameData.N);
  Add(NameData.Name);
end;

procedure TFreeFileBuffer.Add(AnchorData: TAnchorData);
begin
  Add(AnchorData.N);
  Add(AnchorData.AnchorPoint);
  Add(AnchorData.IsAnchorHard);
end;

procedure TFreeFileBuffer.Add(LCData: TLinearConstraintData);
begin
  Add(LCData.N);
  Add(LCData.LinearConstraintPointA);
  Add(LCData.LinearConstraintPointB);
end;


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

procedure TFreeFileBuffer.LoadTJPEGImage(var JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  W, H, Size: integer;
begin
  LoadInteger(W);
  LoadInteger(H);
  LoadInteger(Size);
  Stream := TMemoryStream.Create;
  Stream.SetSize(Size);
  Stream.Write(FData[FPosition], Size);
  Inc(FPosition, Size);
  Stream.Position := 0;
  JPEGImage.LoadFromStream(Stream);
  Stream.Destroy;
end;{TFreeFileBuffer.Add}

{
procedure TFreeFileBuffer.LoadTFreeMHSeriesResistanceData(var Dest;Size:Integer);
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
end;}{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeDelftSeriesResistanceData(var Data: TFreeDelftSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(WlArea);
    LoadBoolean(EstimateWetSurf);
    // Structures are aligned to 2 bytes, so LoadTFreeMHSeriesResistanceData Boolean as Word
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeKAPERResistanceData(var Data: TFreeKAPERResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Draft);
    LoadTFloatType(Lwl);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(LCB);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(At_Ax);
    LoadTFloatType(EntranceAngle);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeHoltrSeriesResistanceData(var Data: TFreeHoltrSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(WlArea);
    LoadTFloatType(Ke);
    LoadTFloatType(BA);
    LoadTFloatType(KBulb);
    LoadTFloatType(ZBulb);
    LoadTFloatType(Cstrn);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(Ks);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeOSTSeriesResistanceData(var Data: TFreeOSTSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(WlArea);
    LoadTFloatType(Ke);
    LoadTFloatType(Ks);
    LoadTFloatType(Nser);
    LoadTFloatType(Na);
    LoadTFloatType(Nf);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadTFloatType(Dat17_1);
    LoadTFloatType(Dat17_2);
    LoadTFloatType(Dat17_3);
    LoadTFloatType(Dat17_4);
    LoadTFloatType(Dat17_5);
    LoadTFloatType(Dat18_1);
    LoadTFloatType(Dat18_2);
    LoadTFloatType(Dat18_3);
    LoadTFloatType(Dat18_4);
    LoadTFloatType(Dat18_5);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeTask1PropellerData(var Data: TFreeTask1PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeTask2PropellerData(var Data: TFreeTask2PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadTFloatType(Dat17_1);
    LoadTFloatType(Dat17_2);
    LoadTFloatType(Dat17_3);
    LoadTFloatType(Dat17_4);
    LoadTFloatType(Dat17_5);
    LoadTFloatType(Dat18_1);
    LoadTFloatType(Dat18_2);
    LoadTFloatType(Dat18_3);
    LoadTFloatType(Dat18_4);
    LoadTFloatType(Dat18_5);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeTask3PropellerData(var Data: TFreeTask3PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadTFloatType(Dat17);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreePlaningResistanceData(var Data: TFreePlaningResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Draft);
    LoadTFloatType(Lwl);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(LCB);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(At_Ax);
    LoadTFloatType(EntranceAngle);
    LoadTFloatType(Sa);
    LoadTFloatType(Caa);
    LoadTFloatType(Angle);
    LoadTFloatType(K);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeRvrsPropellerData(var Data: TFreeRvrsPropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadTFloatType(Dat17);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeHollenSeriesResistanceData(var Data: TFreeHollenSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(Los);
    LoadTFloatType(Ke);
    LoadTFloatType(BA);
    LoadTFloatType(KBulb);
    LoadTFloatType(ZBulb);
    LoadTFloatType(Cstrn);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(Ks);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeTask4PropellerData(var Data: TFreeTask4PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeTask5PropellerData(var Data: TFreeTask5PropellerData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeOortmerSeriesResistanceData(var Data: TFreeOortmerSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(Los);
    LoadTFloatType(Ke);
    LoadTFloatType(BA);
    LoadTFloatType(KBulb);
    LoadTFloatType(ZBulb);
    LoadTFloatType(Cstrn);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(Ks);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeFungSeriesResistanceData(var Data: TFreeFungSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(Los);
    LoadTFloatType(Ke);
    LoadTFloatType(BA);
    LoadTFloatType(KBulb);
    LoadTFloatType(ZBulb);
    LoadTFloatType(Cstrn);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(Ks);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeHydrodynManeuvData(var Data: TFreeHydrodynManeuvData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadTFloatType(Dat13);
    LoadTFloatType(Dat14);
    LoadTFloatType(Dat15);
    LoadTFloatType(Dat16);
    LoadTFloatType(Dat17);
    LoadTFloatType(Dat18);
    LoadTFloatType(Dat19);
    LoadTFloatType(Dat20);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeHydrodynTask1Data(var Data: TFreeHydrodynTask1Data);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(Dat2);
    LoadTFloatType(Dat3);
    LoadTFloatType(Dat4);
    LoadTFloatType(Dat5);
    LoadTFloatType(Dat6);
    LoadTFloatType(Dat7);
    LoadTFloatType(Dat8);
    LoadTFloatType(Dat9);
    LoadTFloatType(Dat10);
    LoadTFloatType(Dat11);
    LoadTFloatType(Dat12);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeRBHSSeriesResistanceData(var Data: TFreeRBHSSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(WlArea);
    LoadTFloatType(Ke);
    LoadTFloatType(Ks);
    LoadTFloatType(Nser);
    LoadTFloatType(Na);
    LoadTFloatType(Nf);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadTFloatType(Dat17_1);
    LoadTFloatType(Dat17_2);
    LoadTFloatType(Dat17_3);
    LoadTFloatType(Dat17_4);
    LoadTFloatType(Dat17_5);
    LoadTFloatType(Dat18_1);
    LoadTFloatType(Dat18_2);
    LoadTFloatType(Dat18_3);
    LoadTFloatType(Dat18_4);
    LoadTFloatType(Dat18_5);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTFreeMHSeriesResistanceData(var Data: TFreeMHSeriesResistanceData);
var
  bp: integer;
begin
  bp := FPosition;
  with Data do
  begin
    LoadTFloatType(StartSpeed);
    LoadTFloatType(EndSpeed);
    LoadTFloatType(StepSpeed);
    LoadTFloatType(Bwl);
    LoadTFloatType(Cp);
    LoadTFloatType(Displacement);
    LoadTFloatType(Draft);
    LoadTFloatType(DraftTotal);
    LoadTFloatType(KeelChordLength);
    LoadTFloatType(KeelArea);
    LoadTFloatType(LCB);
    LoadTFloatType(Lwl);
    LoadTFloatType(RudderChordLength);
    LoadTFloatType(RudderArea);
    LoadTFloatType(Viscosity);
    LoadTFloatType(WettedSurface);
    LoadTFloatType(WlArea);
    LoadTFloatType(Ke);
    LoadTFloatType(Ks);
    LoadTFloatType(Nser);
    LoadTFloatType(Na);
    LoadTFloatType(Nf);
    LoadTFloatType(Np);
    LoadTFloatType(Dp);
    LoadTFloatType(K1);
    LoadTFloatType(K2);
    LoadTFloatType(K3);
    LoadTFloatType(K4);
    LoadTFloatType(K5);
    LoadTFloatType(K6);
    LoadTFloatType(K7);
    LoadTFloatType(A1);
    LoadTFloatType(A2);
    LoadTFloatType(A3);
    LoadTFloatType(A4);
    LoadTFloatType(A5);
    LoadTFloatType(A6);
    LoadTFloatType(A7);
    LoadTFloatType(A8);
    LoadTFloatType(A9);
    LoadTFloatType(A10);
    LoadTFloatType(A11);
    LoadTFloatType(Dat17_1);
    LoadTFloatType(Dat17_2);
    LoadTFloatType(Dat17_3);
    LoadTFloatType(Dat17_4);
    LoadTFloatType(Dat17_5);
    LoadTFloatType(Dat18_1);
    LoadTFloatType(Dat18_2);
    LoadTFloatType(Dat18_3);
    LoadTFloatType(Dat18_4);
    LoadTFloatType(Dat18_5);
    LoadBoolean(EstimateWetSurf);
    LoadBoolean(Extract);
  end;
  FPosition := bp + sizeof(Data); //record data can be aligned
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}


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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}


//////////
resourcestring
rsParsingErrorOutOfFileSize =
  'Error parsing of file "%s". Reading %s of size %d at position %d that is out of file size %d.';


procedure TFreeFileBuffer.LoadString(var Output: string);
var
  I, Size: integer;
  Ch: char;   S: string;
begin
  LoadInteger(Size);
  Output := '';

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'String',Size,FPosition,FCount]) );

  for I := 1 to Size do
  begin
    Ch := char(FData[FPosition]);
    Inc(FPosition);
    Output := Output + Ch;
  end;
  S:=Output;
  Output := ConvertEncoding(S,FEncoding,'utf8');
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

{
procedure TFreeFileBuffer.LoadTFreeMHSeriesResistanceData(var Output:Word);
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
end;}{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadInteger(var Output: integer);
var
  Size: integer;
begin
  Size := 4;
  Output := 0;

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'Integer',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Output := LEtoN(Output);
  Inc(FPosition, Size);
end; {TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTColor(var Output: TColor);
var
  Size: integer;
begin
  Size := 4;
  Output := 0;

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'TColor',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Output := LEtoN(Output);
  Inc(FPosition, Size);
end; {TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}


procedure TFreeFileBuffer.LoadTStrings(var Output: TStrings);
var i,c: integer; S:String;
begin
  LoadInteger(c);
  for i:=1 to c do
    begin
    LoadString(S);
    Output.Add(S);
    end;
end;{TFreeFileBuffer.Add}


procedure TFreeFileBuffer.LoadTFreeFileVersion(var Output: TFreeFileVersion);
var
  Size: integer;
begin
  Size := SizeOf(Output);

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'TFreeFileVersion',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Inc(FPosition, Size);
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadBoolean(var Output: boolean);
var
  Size: integer;
begin
  Size := 1;
  Output := False;

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'Boolean',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Inc(FPosition, Size);
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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
end;}{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadTNameData(var NameData: TNameData);
begin
  LoadInteger(NameData.N);
  LoadString(NameData.Name);
end;

procedure TFreeFileBuffer.LoadTAnchorData(var AnchorData: TAnchorData);
begin
  LoadInteger(AnchorData.N);
  LoadInteger(AnchorData.AnchorPoint);
  LoadBoolean(AnchorData.IsAnchorHard);
end;

procedure TFreeFileBuffer.LoadTLinearConstraintData(var LCData: TLinearConstraintData);
begin
  LoadInteger(LCData.N);
  LoadInteger(LCData.LinearConstraintPointA);
  LoadInteger(LCData.LinearConstraintPointB);
end;

procedure TFreeFileBuffer.LoadTFloatType(var Output: TFloatType);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  Output := 0.0;

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'TFloatType',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Inc(FPosition, Size);
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

{
procedure TFreeFileBuffer.LoadTFreeMHSeriesResistanceData(var Output: TColor);
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
end;}{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}


procedure TFreeFileBuffer.LoadT3DCoordinate(var Output: T3DCoordinate);
var
  Size: integer;
begin
  Size := SizeOf(Output);
  Output := ZERO;

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'TFloatType',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Inc(FPosition, Size);
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

procedure TFreeFileBuffer.LoadT3DPlane(var Output: T3DPlane);
var
  Size: integer;
begin
  Size := SizeOf(Output);

  if FPosition + Size > FCount then
    raise Exception.Create(format(rsParsingErrorOutOfFileSize,
       [FFileName,'TFloatType',Size,FPosition,FCount]) );

  Move(FData[FPosition], Output, Size);
  Inc(FPosition, Size);
end;{TFreeFileBuffer.LoadTFreeMHSeriesResistanceData}

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

procedure TFreeFileBuffer.Add(words: TStrings);
var i: integer;
begin
  Add(words.Count);
  for i:=0 to words.Count-1 do
    Add(words[i]);
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
  //TheStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    system.FileMode := fmOpenRead;
    system.Reset(FFile,1);
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
  finally
    Closefile(FFile);
  end;
  FFileName := '';
end;{TFreeFileBuffer.LoadFromFile}

// reset the data before reading
procedure TFreeFileBuffer.Reset;
begin
  FPosition := 0;
end;{TFreeFileBuffer.Reset}

function TFreeFileBuffer.SaveToFile(Filename: string):boolean;
var
  DataWritten: integer;
  DataLeft: integer;
  Tmp: integer;
  Size: integer;
begin
  result:=false;
  FFileName := Filename;
  try
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
  finally
    Closefile(FFile);
  end;
  FFileName := '';
  result:=true;
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
begin
  FLines.Capacity := val;
end;{TFreeTextBuffer.FSetCapacity}

procedure TFreeTextBuffer.Clear;
begin
  if FLines <> nil then
    FLines.Clear;
  inherited Clear;
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(Text: string);
var
  S: string;
begin
  S := ReplaceStr(Text, '\', '\\');
  S := ReplaceStr(S, EOL, '\n');
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(BooleanValue: boolean);
var
  S: string;
begin
  //if BooleanValue then S:='True' else S:='False';
  S := BoolToStr(BooleanValue,'1','0');
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(FloatValue: TFloatType);
var
  S: string;
begin
  S := FloatToStr(FloatValue);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(IntegerValue: integer);
var
  S: string;
begin
  S := IntToStr(IntegerValue);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(words: TStrings);
var
  i: integer;
  S: string;
begin
  S:='';
  if words.Count > 0 then
    S := words[0];
  for i:=1 to words.Count-1 do
    S := S + ' ' + words[i];
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}


procedure TFreeTextBuffer.Add(PVersion: TFreeFileVersion);
var
  S: string;
begin
  FVersion := PVersion;
  S := VersionString(PVersion);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(NameData: TNameData);
var S: string;
begin
  S := IntToStr(NameData.N) + ' ' + NameData.Name;
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(LCData: TLinearConstraintData);
var
  S: string;
begin
  S := IntToStr(LCData.N)
    + ' ' + IntToStr(LCData.LinearConstraintPointA)
    + ' ' + IntToStr(LCData.LinearConstraintPointB);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}


procedure TFreeTextBuffer.Add(Coordinate: T3DCoordinate);
var
  S: string;
begin
  S := FloatToStr(Coordinate.X) + ' ' + FloatToStr(Coordinate.Y) + ' ' + FloatToStr(Coordinate.Z);
  FLines.Add(S);
  Inc(FPosition);
end;{TFreeTextBuffer.Add}

procedure TFreeTextBuffer.Add(Plane: T3DPlane);
var
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

  S := StrAlloc(Size * 2 + 2);
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

procedure TFreeTextBuffer.LoadTJPEGImage(var JPegImage: TJPEGImage);
var
  Stream: TMemoryStream;
  W, H, Size: integer;
  PData: PChar;
begin
  LoadInteger(W);
  LoadInteger(H);
  LoadInteger(Size);
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

procedure TFreeTextBuffer.LoadInteger(var Output: integer);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToInt(S);
  Inc(FPosition);
end;

procedure TFreeTextBuffer.LoadTColor(var Output: TColor);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToInt(S);
  Inc(FPosition);
end;

procedure TFreeTextBuffer.LoadString(var Output: string);
var
  S: string;
begin
  S := FLines[FPosition];
  S := ReplaceStr(S, '\n', EOL);
  S := ReplaceStr(S, '\\', '\');
  Output := S;
  //Output := ConvertEncoding(S,FEncoding,'utf8');
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadTFreeFileVersion(var Output: TFreeFileVersion);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := VersionBinary(S);
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadBoolean(var Output: boolean);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToBool(S);
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadTFloatType(var Output: TFloatType);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToFloat(S);
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

{
procedure TFreeTextBuffer.LoadTJPEGImage(var Output: TColor);
var
  S: string;
begin
  S := FLines[FPosition];
  Output := StrToInt(S);
  Inc(FPosition);
end;}{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadTNameData(var Output: TNameData);
var  p:integer; S: string;
begin
  S := FLines[FPosition];
  p := pos(' ',S);
  Output.N := StrToInt(copy(S,1,p-1));
  Output.Name := copy(S,p+1,length(S));
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadTLinearConstraintData(var Output: TLinearConstraintData);
var  S: string;
begin
  S := FLines[FPosition];
  Output.N := StrToInt(ExtractWord(1, S, [' ']));
  Output.LinearConstraintPointA := StrToInt(ExtractWord(2, S, [' ']));
  Output.LinearConstraintPointB := StrToInt(ExtractWord(3, S, [' ']));
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadT3DCoordinate(var Output: T3DCoordinate);
var
  S: string;
begin
  S := FLines[FPosition];
  Output.X := StrToFloat(ExtractWord(1, S, [' ']));
  Output.Y := StrToFloat(ExtractWord(2, S, [' ']));
  Output.Z := StrToFloat(ExtractWord(3, S, [' ']));
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

procedure TFreeTextBuffer.LoadT3DPlane(var Output: T3DPlane);
var
  S: string;
begin
  S := FLines[FPosition];
  Output.a := StrToFloat(ExtractWord(1, S, [' ']));
  Output.b := StrToFloat(ExtractWord(2, S, [' ']));
  Output.c := StrToFloat(ExtractWord(3, S, [' ']));
  Output.d := StrToFloat(ExtractWord(4, S, [' ']));
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

// load string of words separated by spaces
procedure TFreeTextBuffer.LoadTStrings(var Output: TStrings);
var  i:integer; S,V: string; //SS:TStrings;
begin
  S := FLines[FPosition];
  i:=1;
  while true do
  begin
    V:=ExtractWord(i, S, [' ']);
    if V='' then break;
    Output.Add(V);
    inc(i);
  end;
  Inc(FPosition);
end;{TFreeTextBuffer.LoadTJPEGImage}

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

function TFreeTextBuffer.SaveToFile(Filename: string):boolean;
begin
  result:=false;
  FFileName := Filename;
  FLines.SaveToFile(Filename);
  result:=true;
end;{TFreeTextBuffer.SaveToFile}

function TFreeTextBuffer.GetPosition:integer;
begin
  Result:=FPosition;
end;{TFreeFileBuffer.GetPosition}

end.
