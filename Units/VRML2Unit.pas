unit VRML2Unit;

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
  Windows,
    {$ENDIF}
    {$IFDEF LCL}
         {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

    {$ENDIF}
  Classes,
  SysUtils,
  Dialogs,
  Graphics,
  FreeTypes,
  FreeGeometry,
  FasterList,
  Math,
  VRMLUnit;

// VRML 2

type

  TVRML2Scene = class;
  TVRML2object = class;
  TVRML2IndexedFaceSet = class;
  TVRML2Coordinates = class;

  TFasterListTVRML2Object = specialize TFasterList<TVRML2Object>;
  TFasterListTVRML2Coordinates = specialize TFasterList<TVRML2Coordinates>;
  TFasterListTVRML2IndexedFaceSet = specialize TFasterList<TVRML2IndexedFaceSet>;

TVRML2object = class(TVRMLObject)
private
  FName: String;
  FType: String;
  FScene: TVRML2Scene;
  FParent: TVRML2object;
public
  constructor Create(Scene: TVRML2Scene; Parent: TVRML2object); virtual;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; virtual;
end;

TVRML2Group = class(TVRML2object)
private
  FChildren: TFasterListTVRML2Object;
  function FGetCount: integer;
  function FGetChild(Index: integer): TVRMLObject;
public
  procedure AddChild(VRMLObject: TVRML2Object);
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; override;
  property Count: integer read FGetCount;
  property Children[index: integer]: TVRMLObject read FGetChild;
end;

TFloatArray3 = array[0..2] of Float;
TFloatArray4 = array[0..3] of Float;

TVRML2Transform = class(TVRML2Group)
private
  FScale: TFloatArray3;
  FScaleOrientation: array[0..3] of Float;
  FCenter: array[0..2] of Float;
  FRotation: array[0..3] of Float;
  FTranslation: array[0..2] of Float;
  FBBoxCenter: array[0..2] of Float;
  FBBoxSize: array[0..2] of Float;
public
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; override;
end;

TVRML2Material = class(TVRML2object)
private
  FAmbientIntensity: Float;
  FDiffuseColor: TColor;
  FEmissiveColor: TColor;
  FShininess: Float;
  FSpecularColor: TColor;
  FTransparency: Float;
public
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; override;
  property AmbientIntensity: Float read FAmbientIntensity;
  property DiffuseColor: TColor read FDiffuseColor;
  property EmissiveColor: TColor read FEmissiveColor;
  property Shininess: Float read FShininess;
  property SpecularColor: TColor read FSpecularColor;
  property Transparency: Float read FTransparency;
end;

TVRML2Appearance = class(TVRML2object)
private
  FMaterial: TVRML2Material;
  FTexture: TVRMLobject; // not implemented
  FTextureTransform: TVRMLobject; // not implemented
public
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; override;
  property Material: TVRML2Material read FMaterial;
end;

TVRML2Shape = class(TVRML2object)
private
  FAppearance: TVRML2Appearance;
  FGeometry: TVRML2object;
public
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  procedure Clear; override;
  destructor Destroy; override;
  procedure Load; override;
  property Appearance: TVRML2Appearance read FAppearance;
  property Geometry: TVRML2object read FGeometry;
end;

TVRML2Coordinates = class(TVRML2object)
private
  FCapacity: integer;
  FCount: integer;
  FFaceSets: TFasterListTVRML2IndexedFaceSet;
  FCoordinates: array of T3DCoordinate;
  FPrecision: double;
  function FGetNumberOfFacesets: integer;
  function FGetPoint(Index: integer): T3DCoordinate;
  procedure FSetCapacity(val: integer);
public
  procedure Add(P: T3DCoordinate);
  procedure AddFaceSet(FaceSet: TVRML2IndexedFaceSet);
  procedure Clear; override;
  constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object); override;
  destructor Destroy; override;
  procedure Load; override;
  property Count: integer read FCount;
  property Capacity: integer
    read FCapacity write FSetCapacity;
  property NumberOfFaceSets: integer
    read FGetNumberOfFacesets;
  property Point[index: integer]: T3DCoordinate
    read FGetPoint;
  property Precision: double read FPrecision;
end;



  TVRML2IndexedFaceSet = class(TVRML2object)
  private
    FFaces: specialize TFasterList<TIntArray>;
    FCoordinates: TVRML2Coordinates;
    function FGetFace(Index: integer): TIntArray;
    procedure FSetCapacity(val: integer);
    function FGetCapacity: integer;
    function FGetCount: integer;
  private
    FColor: TColor;
    FNormals: array of TVRMLobject;
    FTexCoordinates: TVRML2Coordinates;
    Fccw: boolean;
    FColorIndex: TIntArray;
    FColorPerVertex: boolean;
    FConvex: boolean;
    FCreaseAngle: Float;
    FNormalIndex: TIntArray;
    FnormalPerVertex: boolean;
    FSolid: boolean;
    FTexCoordIndex: TIntArray;
  public
    property Coordinates: TVRML2Coordinates
      read FCoordinates;
    property Count: integer read FGetCount;
    property Capacity: integer read FGetCapacity write FSetCapacity;
    property Face[index: integer]: TIntArray read FGetFace;
  public
    constructor Create(Scene: TVRML2Scene; Parent:TVRML2Object);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Load; override;
  end;


  TToken = class
    FString: String;
    FLine: integer;
    FPosition: integer;
    constructor Create(val:String; ln,pos:integer);
  end;

  TFasterListTToken = specialize TFasterList<TToken>;

  TVRML2Scene = class (TVRMLList)
    FObjects: TFasterListTVRML2Object;
    FFaceSets: TFasterListTVRML2IndexedFaceSet;
    FLastAddedCoordinates: TVRML2Coordinates;
    FTokens: TFasterListTToken;
    FFileType: TVRMLFileType;
    FCurrentToken: Integer;
    FLevel: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Tokenize(Str:String; ln:integer):TFasterListTToken;
    function LoadId(var token:TToken):String;
    function LoadFloat(var token:TToken):Float;
    function LoadInteger(var token:TToken):Integer;
    function LoadBoolean(var token:TToken): boolean;
    function LoadColor(var token:TToken):TColor;
    procedure SkipObject;
    procedure LoadFromFile(Filename: string); override;
    procedure LoadVrml2;
    procedure Import(Filename:String; SubdivisionSurface: TFreeSubdivisionSurface); override;
    procedure Add(VRMLObject: TVRML2Object);
    function ExtractFaceSetData: TFasterListTVRML2IndexedFaceSet;
  end;

implementation

uses FreeStringsUnit;

{$modeSwitch class+}
{$modeSwitch exceptions+}
type
	VRML2ParserException = class end;

procedure RaiseParserError(found:TToken; message:String);
begin
    MessageDlg(String.Format('Parser error: %s at %d,%d',
      [message, found.FLine, found.FPosition]),
      mtError, [mbOK], 0);
    raise VRML2ParserException.create at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure Assert(found:TToken; expected:String);
  begin
    if found.FString.ToUpper <> expected.ToUpper then
      begin
      MessageDlg(String.Format('Found "%s", "%s" was expected at %d,%d',
        [found.FString, expected, found.FLine, found.FPosition]),
        mtError, [mbOK], 0);
      raise VRML2ParserException.create at get_caller_addr(get_frame), get_caller_frame(get_frame);
      end;
  end;

procedure RaiseSyntaxError(found:TToken; expected: String);
  begin
    MessageDlg(String.Format('Found "%s", %s was expected at %d,%d',
      [found.FString, expected, found.FLine, found.FPosition]),
      mtError, [mbOK], 0);
    raise VRML2ParserException.create at get_caller_addr(get_frame), get_caller_frame(get_frame);
  end;

constructor TToken.Create(val:String; ln,pos:integer);
begin
  FString := val;
  FLine := ln;
  FPosition := pos;
end;


constructor TVRML2Object.Create(Scene: TVRML2Scene; Parent: TVRML2object);
    begin
      FScene := Scene;
      Parent := Parent;
      FName := '';
    end;{TVRML2object.Create}

    procedure TVRML2Object.Clear;
    begin
      FName := '';
    end;{TVRML2object.Clear}

    destructor TVRML2Object.Destroy;
    begin
      //Clear;
      inherited Destroy;
    end;{TVRML2object.Destroy}

    procedure TVRML2Object.Load;
    begin
    end;{TVRML2object.Load}


    constructor TVRML2Scene.Create;
    begin
      inherited Create;
      FTokens := TFasterListTToken.Create;
      FObjects:= TFasterListTVRML2Object.Create;
      FFaceSets:= TFasterListTVRML2IndexedFaceSet.Create;
      FCurrentToken := -1;
      FLevel := 0;
    end;

    destructor TVRML2Scene.Destroy;
    begin
      Clear;
      FreeAndNil(FTokens);
      FreeAndNil(FObjects);
      FreeAndNil(FFaceSets);
      inherited Destroy;
    end;

    procedure TVRML2Scene.Clear;
    begin
      FTokens.Clear;
      FObjects.Clear;
      FFaceSets.Clear;
      FCurrentToken:=-1;
      FLevel := 0;
    end;

    procedure TVRML2Scene.LoadVrml2;
    var
      LineNr: integer;
      I, Index, NObj: integer;
      Str: string;
      word, ObjectName, ObjectType: string;
      VRMLObject: TVRML2Object;
      token: TToken;
      ValidFile: boolean;
    begin
      while FCurrentToken < FTokens.Count-1 do
      begin
        VRMLObject := nil; ObjectName:=''; ObjectType:='';
        token := nil;
        word := Self.LoadId(token);

        if word.toUpper = 'DEF' then
        begin
          ObjectName := Self.LoadId(token);
          word := Self.LoadId(token);
        end;

        ObjectType := word;

        if word.ToUpper = 'GROUP' then
          VRMLObject := TVRML2Group.Create(self, nil)
        else
        if word.ToUpper = 'SHAPE' then
          VRMLObject := TVRML2Shape.Create(self,nil)
        else
        if word.ToUpper = 'TRANSFORM' then
          VRMLObject := TVRML2Transform.Create(self,nil)
        else if word.ToUpper <> '}' then
          Self.SkipObject;

        if VRMLObject <> nil then
        begin
          Index := 0;
          VRMLObject.FName := ObjectName;
          VRMLObject.FType := ObjectType;
          VRMLObject.Load;
          Add(VRMLObject);
        end;
      end;
    end;{TVRML2Scene.LoadVrml2}

    function TVRML2Scene.Tokenize(Str:String; ln:integer):TFasterListTToken;
    var i,b: Integer; C: char; W: String; InSQString, InDQString: boolean;
      token: TToken;
    begin
      Result := TFasterListTToken.Create;
      W := ''; InSQString:=false; InDQString:=false; b:=1;
      for i:=1 to Length(Str) do
      begin
        C := Str[i];
        if (C in [' ',#9,#10,#13])
          and not InSQString and not InDQString then
           begin
             if W > '' then
             begin
               Result.Add(TToken.Create(W,ln,b));
             end;
             b:=i+1;
             W := '';
           end
        else if C = '''' then
           begin
             if not InSQString and not InDQString then
             begin
               if W > '' then Result.Add(TToken.Create(W,ln,i));
               W := C; b:=i;
               InSQString:=true;
             end;
             if InSQString and not InDQString then
             begin
               W := W + C;
               InSQString:=false;
             end;
             if not InSQString and InDQString then
             begin
               W := W + C;
             end;
           end
        else if C = '"' then
           begin
             if not InSQString and not InDQString then
             begin
               if W > '' then Result.Add(TToken.Create(W,ln,b));
               W := C; b:=i;
               InDQString:=true;
             end;
             if not InSQString and InDQString then
             begin
               W := W + C;
               InSQString:=false;
             end;
             if InSQString and not InDQString then
             begin
               W := W + C;
             end;
           end
        else if C in [',',':','[',']','{','}'] then
           begin
             if W > '' then Result.Add(TToken.Create(W,ln,b));
             W := ''; b:=i;
             Result.Add(TToken.Create(C,ln,b));
           end
        else
           W := W + C;
      end;
      if W > '' then Result.Add(TToken.Create(W,ln,i));
    end;{TVRML2Scene.Tokenize}

    function TVRML2Scene.LoadId(var token:TToken):String;
    begin
     inc(FCurrentToken);
     token := FTokens[FCurrentToken];
     if token.FString = '{' then inc(FLevel)
     else if token.FString = '}'
       then dec(FLevel)
     else if token.FString = '[' then inc(FLevel)
     else if token.FString = ']' then dec(FLevel);
     Result := token.FString;
    end;

    function TVRML2Scene.LoadFloat(var token:TToken):Float;
    begin
     inc(FCurrentToken);
     token := FTokens[FCurrentToken];
     if not Float.TryParse(token.FString, Result) then
       RaiseParserError(token, String.Format('Expected Decimal, found %s',[token.FString]));
    end;

    function TVRML2Scene.LoadInteger(var token:TToken):Integer;
    begin
     inc(FCurrentToken);
     token := FTokens[FCurrentToken];
     if not Integer.TryParse(token.FString, Result) then
       RaiseParserError(token, String.Format('Expected Integer, found %s',[token.FString]));
    end;

    function TVRML2Scene.LoadBoolean(var token:TToken): boolean;
    begin
     inc(FCurrentToken);
     token := FTokens[FCurrentToken];
     try
       Result := Boolean.Parse(token.FString);
     except on Exception do
       RaiseParserError(token, String.Format('Expected Boolean, found %s',[token.FString]));
     end;
    end;

    function TVRML2Scene.LoadColor(var token:TToken):TColor;
    var
      fR,fG,fB: Float;
      R,G,B: byte;
      tokenG,tokenB: TToken;
    begin
      fR := LoadFloat(token);
      fG := LoadFloat(tokenG);
      fB := LoadFloat(tokenB);
      R := round(255 * fR);
      G := round(255 * fG);
      B := round(255 * fB);
      Result := RGBToColor(R, G, B);
    end;

    procedure TVRML2Scene.SkipObject;
    var curLevel: integer;  token: TToken; word: String;
    begin
     curLevel := FLevel;
     while (FCurrentToken < FTokens.Count)
       and (word<>'{')and (word<>'[') do
       word := LoadId(token);
     repeat
       word := LoadId(token);
     until (FCurrentToken = FTokens.Count) or (curLevel = FLevel);
     //dec(FCurrentToken);
    end;

    procedure TVRML2Scene.LoadFromFile(Filename: string);
    var
      Strings: TStringList;
      LineNr: integer;
      I,L, Index, NObj: integer;
      Str: string;
      token:TToken;
      tokens: TFasterListTToken;
      ObjectName: string;
      FFile: TextFile;
      VRMLObject: TVRMLObject;
      ValidFile: boolean;
    begin
      if FileExistsUTF8(Filename) { *Converted from FileExists* } then
      begin
        Clear;
        Strings := TStringList.Create;
        AssignFile(FFile, FileName);
        Reset(FFile);
        ValidFile := False;
        if not EOF(FFile) then
        begin
          Readln(FFile, Str); // First line, must contain the string: "#VRML V1.0 ascii"
          Str := Uppercase(Str);
          if pos('#VRML V2.0 UTF8', Str) <> 0 then
          begin
            FFileType := ftVRML2;
            ValidFile := True;
          end;
        end;
        if not ValidFile then
        begin
          MessageDlg(rs_This_is_not_a_valid_VRML_2_0_file {UserString[208]} + '!', mtError, [mbOK], 0);
          CloseFile(FFile);
          FreeAndNil(Strings);
          exit;
        end;
        if FTokens = nil then FTokens := specialize TFasterList<TToken>.Create;
        L:=1;
        while not EOF(FFile) do
        begin
          Readln(FFile, Str);
          I := 1;
          while I <= Length(Str) do
          begin
            if Str[I] in [#9, #10, #13] then
              Str[I] := #32
            else
            if Str[I] = '#' then
            begin
              // remove comments
              Str := copy(Str, 1, I - 1);
              break;
            end;
            Inc(I);
          end;
          {repeat
            // replace double spaces by only 1
            I := Pos('  ', Str);
            if I <> 0 then
            begin
              System.Delete(Str, I, 1);
            end;
          until I = 0;}
          // remove leading and trailing spaces
          //Str := Trim(Str);
          if Trim(Str) <> '' then
          begin
            tokens := Tokenize(Str, L);
            for I:=0 to tokens.Count-1 do
            begin
              FTokens.Add(tokens[I]);
            end;
            FreeAndNil(tokens);
            //Strings.Add(Uppercase(Str));
          end;
          inc(L);
        end;
        CloseFile(FFile);

        if FTokens.Count > 0 then
        begin
          LoadVrml2;
        end
        else
          MessageDlg(rs_This_is_not_a_valid_VRML_1_0_file {UserString[208]} + '!', mtError, [mbOK], 0);
        FreeAndNil(Strings);
      end;
    end;{TVRML2Scene.LoadFromFile}

procedure TVRML2Scene.Add(VRMLObject: TVRML2Object);
begin
  FObjects.Add(VRMLObject);
  if VRMLObject is TVRML2Coordinates then
  begin
    FLastAddedCoordinates := VRMLObject as TVRML2Coordinates;
  end
  else if VRMLObject is TVRML2IndexedFaceSet then
  begin
    FFaceSets.Add(VRMLObject as TVRML2IndexedFaceSet);
    if FLastAddedCoordinates <> nil then
      FLastAddedCoordinates.AddFaceSet(VRMLObject as TVRML2IndexedFaceSet);
  end;
end;{TVRMLList.Add}

function TVRML2Scene.ExtractFaceSetData: TFasterListTVRML2IndexedFaceSet;
var
  I: integer;
  FaceSet: TVRML2IndexedFaceSet;
begin
  Result := nil;
  if FFaceSets.Count > 0 then
  begin
    Result := TFasterListTVRML2IndexedFaceSet.Create;
    Result.Capacity := FFaceSets.Count;
    for I := 1 to FFaceSets.Count do
    begin
      FaceSet := FFacesets[I - 1];
      if Faceset.Coordinates <> nil then
        Result.Add(FaceSet);
    end;
    if Result.Count = 0 then
    begin
      FreeAndNil(Result);
      Result := nil;
    end;
  end;
end;{TVRMLList.ExtractFaceSetData}


// ##################################### VRML2 Group #####################################
function TVRML2Group.FGetCount: integer;
begin
  Result := FChildren.Count;
end;{TVRMLSeparator.FGetCount}

function TVRML2Group.FGetChild(Index: integer): TVRMLObject;
begin
  Result := FChildren[index];
end;{TVRMLSeparator.FGetItems}

procedure TVRML2Group.AddChild(VRMLObject: TVRML2Object);
begin
  FChildren.Add(VRMLObject);
  {if VRMLObject is TVRMLCoordinate3 then
  begin
    FScene.FLastAddedCoordinates := VRMLObject as TVRMLCoordinate3;
  end
  else
  begin
    if VRMLObject is TVRMLIndexedFaceSet then
    begin
      FScene.FFaceSets.Add(VRMLObject as TVRMLIndexedFaceSet);
      if FScene.FLastAddedCoordinates <> nil then
        FScene.FLastAddedCoordinates.AddFaceSet(VRMLObject as TVRMLIndexedFaceSet);
    end;
  end;}
end;{FChildren.Add}

constructor TVRML2Group.Create(Scene: TVRML2Scene; Parent: TVRML2object);
begin
  inherited Create(Scene, Parent);
  FChildren :=  TFasterListTVRML2Object.Create;
end;{TVRML2Group.Create}

procedure TVRML2Group.Clear;
var
  I: integer;
begin
  for I := 0 to Count-1 do
    FChildren[I].Destroy;
  FChildren.Clear;
  inherited Clear;
end;{TVRML2Group.Clear}

destructor TVRML2Group.Destroy;
begin
  Clear;
  FreeAndNil(FChildren);
  inherited Destroy;
end;{TVRML2Group.Destroy}

procedure TVRML2Group.Load;
var
  token:TToken;
  word,ObjectName: string;
  coord: T3DCoordinate;
  child: TVRML2Object;
begin
  word := FScene.LoadId(token);
  if word <> '{' then
    begin
      Self.FName := word;
      word := FScene.LoadId(token);
      Assert(token, '{');
    end;
  word := FScene.LoadId(token);
  Assert(token, 'CHILDREN');
  word := FScene.LoadId(token);
  Assert(token, '[');
  word := FScene.LoadId(token);
  while word <> ']' do
  begin
    if word.ToUpper = 'SHAPE' then
    begin
      child := TVRML2Shape.Create(FScene, Self);
      child.Load;
      FChildren.Add(child);
    end;
    word := FScene.LoadId(token);
  end;
  Assert(token, ']');
  word := FScene.LoadId(token);
  Assert(token, '}');
end;{TVRML2Group.Load}


// TVRML2Transform
constructor TVRML2Transform.Create(Scene: TVRML2Scene; Parent: TVRML2object);
begin
  inherited Create(Scene, Parent);
  Clear;
end;{TVRML2Transform.Create}

procedure TVRML2Transform.Clear;
var
  I: integer;
begin
  inherited Clear;
  FScale[0]:=1.0; FScale[1]:=1.0; FScale[0]:=1.0;
  FScaleOrientation[0]:= 0; FScaleOrientation[1]:= 0;
  FScaleOrientation[2]:= 1; FScaleOrientation[3]:= 0;
  FCenter[0]:=0; FCenter[1]:=0; FCenter[2]:=0;
  FRotation[0]:=0; FRotation[1]:=0; FRotation[2]:=1; FRotation[3]:=0;
  FTranslation[0]:=0; FTranslation[1]:=0; FTranslation[2]:=0;
  FBBoxCenter[0]:=0; FBBoxCenter[1]:=0; FBBoxCenter[2]:=0;
  FBBoxSize[0]:=-1; FBBoxSize[2]:=-1; FBBoxSize[2]:=-1;
end;{TVRML2Transform.Clear}

destructor TVRML2Transform.Destroy;
begin
  inherited Destroy;
end;{TVRML2Transform.Destroy}

procedure TVRML2Transform.Load;
var
  token:TToken;
  word,ObjectName: string;
  coord: T3DCoordinate;
  child: TVRML2Object;
begin
  word := FScene.LoadId(token);
  if word <> '{' then
    begin
      Self.FName := word;
      word := FScene.LoadId(token);
      Assert(token, '{');
    end;
  word := FScene.LoadId(token);
  while word <> '}' do
  begin
    if word.ToUpper = 'CENTER' then
    begin
      FCenter[0] := FScene.LoadFloat(token);
      FCenter[1] := FScene.LoadFloat(token);
      FCenter[2] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'ROTATION' then
    begin
      FRotation[0] := FScene.LoadFloat(token);
      FRotation[1] := FScene.LoadFloat(token);
      FRotation[2] := FScene.LoadFloat(token);
      FRotation[3] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'SCALE' then
    begin
      FScale[0] := FScene.LoadFloat(token);
      FScale[1] := FScene.LoadFloat(token);
      FScale[2] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'SCALEORIENTATION' then
    begin
      FScaleOrientation[0] := FScene.LoadFloat(token);
      FScaleOrientation[1] := FScene.LoadFloat(token);
      FScaleOrientation[2] := FScene.LoadFloat(token);
      FScaleOrientation[3] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'TRANSLATION' then
    begin
      FTranslation[0] := FScene.LoadFloat(token);
      FTranslation[1] := FScene.LoadFloat(token);
      FTranslation[2] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'BBOXCENTER' then
    begin
      FBBoxCenter[0] := FScene.LoadFloat(token);
      FBBoxCenter[1] := FScene.LoadFloat(token);
      FBBoxCenter[2] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'BBOXSIZE' then
    begin
      FBBoxSize[0] := FScene.LoadFloat(token);
      FBBoxSize[1] := FScene.LoadFloat(token);
      FBBoxSize[2] := FScene.LoadFloat(token);
    end
    else if word.ToUpper = 'CHILDREN' then
    begin
      word := FScene.LoadId(token);
      Assert(token, '[');
      word := FScene.LoadId(token);
      while word <> ']' do
      begin
        if word.ToUpper = 'SHAPE' then
        begin
          child := TVRML2Shape.Create(FScene, Self);
          child.Load;
          FChildren.Add(child);
        end;
        word := FScene.LoadId(token);
      end;
      Assert(token, ']');
    end
    else RaiseParserError(token, 'center|children|rotation|scale|scaleOrientation'
         +'|translation|bboxCenter|bboxSize');

    word := FScene.LoadId(token);
  end; //while Transform
end;{TVRML2Transform.Load}

//   TVRML2Material

constructor TVRML2Material.Create(Scene: TVRML2Scene; Parent:TVRML2Object);
begin
  inherited Create(Scene, Parent);
  Clear;
end;{TVRML2Material.Create}

procedure TVRML2Material.Clear;
begin
  FAmbientIntensity := 0.2;
  FDiffuseColor := clLtGray;
  FEmissiveColor := clBlack;
  FShininess := 0.2;
  FSpecularColor := clBlack;
  FTransparency := 0.0;
  inherited Clear;
end;{TVRML2Material.Clear}

destructor TVRML2Material.Destroy;
begin
  inherited Destroy;
end;{TVRML2Material.Destroy}

procedure TVRML2Material.Load;
var
  token: TToken; word:string;
begin
    word := FScene.LoadId(token);
    if word <> '{' then
    begin
      Self.FName := word;
      word := FScene.LoadId(token);
      Assert(token, '{');
    end;
    word := FScene.LoadId(token);
    while word <> '}' do
    begin
      if word.toUpper = 'DIFFUSECOLOR' then FDiffuseColor := FScene.LoadColor(token)
      else
      if word.toUpper = 'DIFFUSECOLOR' then FDiffuseColor := FScene.LoadColor(token)
      else
      if word.toUpper = 'EMISSIVECOLOR' then FEmissiveColor := FScene.LoadColor(token)
      else
      if word.toUpper = 'SPECULARCOLOR' then FSpecularColor := FScene.LoadColor(token)
      else
      if word.toUpper = 'AMBIENTINTENSITY' then FAmbientIntensity := FScene.LoadFloat(token)
      else
      if word.toUpper = 'SHININESS' then FShininess := FScene.LoadFloat(token)
      else
      if word.toUpper = 'TRANSPARENCY' then FTransparency := FScene.LoadFloat(token)
      else RaiseParserError(token, 'ambientIntensity|diffuseColor|emissiveColor'
           +'|shininess|specularColor|transparency');

      word := FScene.LoadId(token);
    end;
end;{TVRML2Material.Load}


// TVRML2Appearance
  constructor TVRML2Appearance.Create(Scene: TVRML2Scene; Parent:TVRML2Object);
  begin
    inherited Create(Scene, Parent);
    Clear;
  end;{TVRML2Appearance.Create}

  procedure TVRML2Appearance.Clear;
  begin
    FreeAndNil(FMaterial);
    FreeAndNil(FTexture);
    FreeAndNil(FTextureTransform);
    inherited Clear;
  end;{TVRML2Appearance.Clear}

  destructor TVRML2Appearance.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;{TVRML2Appearance.Destroy}

  procedure TVRML2Appearance.Load;
  var
    token: TToken;
    word: string;
  begin
      word := FScene.LoadId(token);
      if word <> '{' then
      begin
        if word.toUpper = 'DEF' then
        begin
          word := FScene.LoadId(token);
          Self.FName := word; // nodeNameId
          word := FScene.LoadId(token);
        end;
        Self.FType := word;
        word := FScene.LoadId(token);
        Assert(token, '{');
      end;
      word := FScene.LoadId(token);
      while word <> '}' do
      begin
        if word.toUpper = 'MATERIAL' then
        begin
          FMaterial := TVRML2Material.Create(FScene, Self);
          FMaterial.Load;
        end
        else if word.toUpper = 'TEXTURE' then FScene.SkipObject
        else if word.toUpper = 'TEXTURETRANSFORM' then FScene.SkipObject
        else RaiseSyntaxError(token, 'material|texture|textureTransform');
        word := FScene.LoadId(token);
      end;
  end;{TVRML2Appearance.Load}

// TVRML2Shape
  constructor TVRML2Shape.Create(Scene: TVRML2Scene; Parent:TVRML2Object);
  begin
    inherited Create(Scene, Parent);
    Clear;
  end;{TVRML2Shape.Create}

  procedure TVRML2Shape.Clear;
  begin
    FreeAndNil(FAppearance);
    FreeAndNil(FGeometry);
    inherited Clear;
  end;{TVRML2Shape.Clear}

  destructor TVRML2Shape.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;{TVRML2Shape.Destroy}

  procedure TVRML2Shape.Load;
  var
    token: TToken;
    word, ObjectName: string;
    coord: T3DCoordinate;
    VRMLObject: TVRML2Object;
  begin
    word := FScene.LoadId(token);
    if word <> '{' then
      begin
        if word.toUpper = 'DEF' then
        begin
          word := FScene.LoadId(token);
          Self.FName := word; // nodeNameId
          word := FScene.LoadId(token);
        end;
        Self.FType := word;
        word := FScene.LoadId(token);
        Assert(token, '{');
      end;
    word := FScene.LoadId(token);
    while word <> '}' do
    begin
      if word.ToUpper = 'APPEARANCE' then
      begin
        FAppearance := TVRML2Appearance.Create(FScene, Self);
        FAppearance.Load;
        //word := FScene.LoadId(token);
      end
      else if word.ToUpper = 'GEOMETRY' then
      begin
        word := FScene.LoadId(token);
        if word.ToUpper = 'INDEXEDFACESET' then
        begin
          FGeometry := TVRML2IndexedFaceSet.Create(FScene, Self);
          FGeometry.Load;
        end
        else FScene.SkipObject;
      end
      else RaiseParserError(token, 'appearance|geometry');
      word := FScene.LoadId(token);
    end;
  end;{TVRML2Shape.Load}

// TVRML2Coordinates
  function TVRML2Coordinates.FGetNumberOfFacesets: integer;
  begin
    Result := FFaceSets.Count;
  end;{TVRMLCoordinate3.FGetNumberOfFacesets}

function TVRML2Coordinates.FGetPoint(Index: integer): T3DCoordinate;
  begin
    if (Index >= 0) and (Index < FCount) then
      Result := FCoordinates[index]
    else
    begin
      Result := ZERO;
      MessageDlg('Point index out of bounds in TVRML2Coordinates.FGetPoint',
        mtError, [mbOK], 0);
    end;
  end;{TVRML2Coordinates.FGetPoint}

  procedure TVRML2Coordinates.FSetCapacity(val: integer);
  begin
    FCapacity := val;
    Setlength(FCoordinates, FCapacity);
    if FCapacity < FCount then
      FCount := FCapacity;
  end;{TVRML2Coordinates.FSetCapacity}

  procedure TVRML2Coordinates.Add(P: T3DCoordinate);
  begin
    if FCount >= FCapacity then
      Capacity := Count + 25;
    Inc(FCount);
    FCoordinates[FCount - 1] := P;
  end;{TVRML2Coordinates.Add}

  procedure TVRML2Coordinates.AddFaceSet(FaceSet: TVRML2IndexedFaceSet);
  begin
    if FFaceSets.IndexOf(FaceSet) = -1 then
    begin
      FFaceSets.Add(FaceSet);
      FaceSet.FCoordinates := self;
    end;
  end;{TVRML2Coordinates.AddFaceSet}

  procedure TVRML2Coordinates.Clear;
  begin
    Capacity := 0;
    FCount := 0;
    FFaceSets.Clear;
  end;{TVRML2Coordinates.Clear}

  constructor TVRML2Coordinates.Create(Scene: TVRML2Scene; Parent:TVRML2Object);
  begin
    inherited Create(Scene, Parent);
    FFaceSets := TFasterListTVRML2IndexedFaceSet.Create;
  end;{TVRML2Coordinates.Create}

  destructor TVRML2Coordinates.Destroy;
  begin
    FreeAndNil(FFaceSets);
    inherited Destroy;
  end;{TVRML2Coordinates.Destroy}

  procedure TVRML2Coordinates.Load;
  var
    token, tokenX, tokenY, tokenZ:TToken;
    word, ObjectName: string;
    coord: T3DCoordinate;

    procedure detectPrecision(D:string);
    var i:integer; dc, code:integer; ss:string;
    begin
      if D.IndexOf('e') < 0 then
      begin
        i:=D.IndexOf('.');
        if i > -1 then
          dc:=D.length - i - 1;
        if FPrecision > power(10,-dc) then
          FPrecision := power(10,-dc);
      end
      else
      begin //scientific
        i:=D.IndexOf('e');
        ss:=D.Substring(i+1,32);
        val(ss,dc,code);
        if code = 0 then
          if FPrecision > power(10,dc) then
            FPrecision := power(10,dc);
      end
    end;

  begin
    word := FScene.LoadId(token);
    if word <> '{' then
      begin
        Self.FName := word;
        word := FScene.LoadId(token);
        Assert(token, '{');
      end;
    SetLength(FCoordinates,10);
    word := FScene.LoadId(token);
    while word <> '}' do
    begin
      if word.ToUpper = 'POINT' then
      begin
        word := FScene.LoadId(token);
        Assert(token, '[');
        while word <> ']' do
        begin
          coord.X := FScene.LoadFloat(tokenX);
          coord.Y := FScene.LoadFloat(tokenY);
          coord.Z := FScene.LoadFloat(tokenZ);
          detectPrecision(tokenX.FString);
          detectPrecision(tokenY.FString);
          detectPrecision(tokenZ.FString);
          Add(coord);
          word := FScene.LoadId(token);
        end;
      end
      else RaiseParserError(token, 'point');
      word := FScene.LoadId(token);
    end;
    Assert(token, '}');
  end;{TVRML2Coordinates.Load}

// TVRML2IndexedFaceSet
  constructor TVRML2IndexedFaceSet.Create(Scene: TVRML2Scene; Parent:TVRML2Object);
  begin
    inherited Create(Scene, Parent);
    FFaces := specialize TFasterList<TIntArray>.Create;
    FCoordinates := TVRML2Coordinates.Create(Scene, Parent);
    Clear;
  end;

  destructor TVRML2IndexedFaceSet.Destroy;
  begin
    Clear;
    FreeAndNil(FFaces);
    FreeAndNil(FCoordinates);
    inherited Destroy;
  end;{TVRML2Shape.Destroy}


  function TVRML2IndexedFaceSet.FGetFace(Index: integer): TIntArray;
  begin
    if (Index >= 0) and (Index < FGetCount) then
      Result := FFaces[index]
    else
    begin
      Result := nil;
      MessageDlg('Face index out of bounds in TVRMLIndexedFaceSet.FGetFace',
        mtError, [mbOK], 0);
    end;
  end;{TVRMLIndexedFaceSet.FGetFace}

  procedure TVRML2IndexedFaceSet.FSetCapacity(val: integer);
  begin
    FFaces.Capacity := val;
  end;{TVRMLIndexedFaceSet.FSetCapacity}

  function TVRML2IndexedFaceSet.FGetCapacity: integer;
  begin
    Result := FFaces.Capacity;
  end;

  function TVRML2IndexedFaceSet.FGetCount: integer;
  begin
    Result := FFaces.Count;
  end;

  procedure TVRML2IndexedFaceSet.Clear;
    begin
      FFaces.Clear;
      FCoordinates.Clear;

      FColor:= clBlack;
      FreeAndNil(FNormals);
      FreeAndNil(FTexCoordinates);
      Fccw:= true;
      FreeAndNil(FColorIndex);
      FColorPerVertex:= true;
      FConvex:= true;
      FCreaseAngle:= 0;
      FreeAndNil(FNormalIndex);
      FnormalPerVertex:= true;
      FSolid:= true;
      FreeAndNil(FTexCoordIndex);
      inherited Clear;
    end;{TVRML2IndexedFaceSet.Clear}

    procedure TVRML2IndexedFaceSet.Load;
    var
      token:TToken;
      word, ObjectName: string;
      VRMLObject: TVRMLObject;
      FaceCoords:TStringList;
      fc: ^TIntArray;
      i, index: integer;
    begin
      word := FScene.LoadId(token);
      Assert(token,'{');
      word := FScene.LoadId(token);
      while word <> '}' do
      begin
        if word.ToUpper = 'CCW' then
          FCCW := FScene.LoadBoolean(token)
        else
        if word.ToUpper = 'COLORPERVERTEX' then
          FColorPerVertex := FScene.LoadBoolean(token)
        else
        if word.ToUpper = 'CONVEX' then
          FConvex := FScene.LoadBoolean(token)
        else
        if word.ToUpper = 'NORMALPERVERTEX' then
          FNormalPerVertex := FScene.LoadBoolean(token)
        else
        if word.ToUpper = 'SOLID' then
          FSolid := FScene.LoadBoolean(token)
        else
        if word.ToUpper = 'CREASEANGLE' then
          FCreaseAngle := FScene.LoadFloat(token)
        else
        if word.ToUpper = 'COORD' then
        begin
          //FCoordinates := TVRML2Coordinates.Create(FScene, Self);
          FCoordinates.Load;
        end
        else
        if (word.ToUpper='COLOR')or(word.ToUpper='NORMAL')or(word.ToUpper='TEXCOORD') then
        begin
          FScene.SkipObject;
        end
        else
        if (word.ToUpper='COLORINDEX')or(word.ToUpper='NORMALINDEX')or(word.ToUpper='TEXCOORDINDEX') then
        begin
          FScene.SkipObject;
        end
        else
        if word.ToUpper = 'COORDINDEX' then
        begin
          word := FScene.LoadId(token);
          Assert(token,'[');
          FaceCoords := TStringList.Create;
          word := FScene.LoadId(token);
          while word <> ']' do
          begin
            while word <> '-1' do
            begin
              if (word <> '-1') and (word <> ',') and (word <> ']') then
                FaceCoords.Add(word);
              word := FScene.LoadId(token); // comma or ]
              if word = ',' then
                word := FScene.LoadId(token); // number
            end;
            if FaceCoords.Count>0 then
            begin
              new(fc);
              SetLength(fc^, FaceCoords.Count);
              for i:=0 to FaceCoords.Count-1 do
                 fc^[i] := Integer.Parse(FaceCoords[i]);
              FFaces.Add(fc^);
              FaceCoords.Clear;
            end;
            word := FScene.LoadId(token);
          end;
        end
        else RaiseParserError(token,'color|coord|normal|texCoord|ccw|colorIndex'
             +'|colorPerVertex|convex|coordIndex|creaseAngle|normalIndex'
             +'|normalPerVertex|solid|texCoordIndex');
        word := FScene.LoadId(token);
      end;
      FScene.Add(Self);
    end;{TVRML2IndexedFaceSet.Load}



    procedure TVRML2Scene.Import(Filename:String; SubdivisionSurface: TFreeSubdivisionSurface);
    var
      VRMLScene: TVRML2Scene;
      I, J, K, N, FS: integer;
      Index: integer;
      IndexedFaceSets: TFasterListTVRML2IndexedFaceSet;
      CoordInfo: TVRML2Coordinates;
      FaceInfo: TVRML2IndexedFaceSet;
      V3Point: T3DCoordinate;
      Face: TIntArray;
      Layer: TFreeSubdivisionLayer;
      FacePoints: TFasterListTFreeSubdivisionControlPoint;
      AddedCtrlPts: TFasterListTVRML2Coordinates;
      CtrPoint: TFreeSubdivisionControlPoint;
      Points: TFasterListTFreeSubdivisionControlPoint;
      tolerance:double;
      VRMLVersion: String;

      function AddedCtrlPtsIndexOf(V3Point: T3DCoordinate): Integer;
      var i: integer;
      begin
       Result := -1;
       for i := 0 to AddedCtrlPts.Count-1 do
         if V3Point = AddedCtrlPts[i].Point[i] then
         begin
           Result := i;
           break;
         end;
      end;

    begin
      if FileExists(Filename) then
      begin
        SubdivisionSurface.IsLoading := true;
        VRMLVersion := TVRMLList.CheckVRMLFileVersion(Filename);
        if VRMLVersion <> 'VRML V2.0' then
        begin
          MessageDlg('Not a VRML V2.0 format', mtError, [mbOK], 0);
          exit;
        end;
        try
        Self.LoadFromFile(Filename);
        IndexedFaceSets := Self.ExtractFaceSetData;
        if IndexedFaceSets <> nil then
        begin
          Clear;
          try
            AddedCtrlPts := TFasterListTVRML2Coordinates.Create(true,false);
            AddedCtrlPts.Capacity := IndexedFaceSets.Count;
            // Assemble coordinate sets
            for I := 0 to IndexedFaceSets.Count-1 do
            begin
              FaceInfo := IndexedFaceSets[I];
              if AddedCtrlPts.SortedIndexOf(FaceInfo.Coordinates) = -1 then
                AddedCtrlPts.Add(FaceInfo.Coordinates);
            end;

            // now add actual controlPoints
            for I := 0 to AddedCtrlPts.Count-1 do
            begin
              CoordInfo := AddedCtrlPts[I];
              Points := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
              Points.Capacity := CoordInfo.Count;
              AddedCtrlPts.Objects[I] := Points;
              for J := 0 to CoordInfo.Count-1 do
              begin
                V3Point := CoordInfo.Point[J];
                CtrPoint := SubdivisionSurface.AddControlPoint(V3Point,CoordInfo.Precision);
                Points.Add(CtrPoint);
              end;
            end;


            // Add controlfaces
            FacePoints := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
            for I := 0 to IndexedFaceSets.Count-1 do
            begin
              FaceInfo := IndexedFaceSets[I];
              Index := AddedCtrlPts.IndexOf(FaceInfo.Coordinates);
              if Index <> -1 then
              begin
                Points := TFasterListTFreeSubdivisionControlPoint(AddedCtrlPts.Objects[Index]);
                Layer := SubdivisionSurface.AddNewLayer;
                for J := 0 to FaceInfo.Count-1 do
                begin
                  Face := FaceInfo.Face[J];
                  if Face <> nil then
                  begin
                    N := length(Face);
                    FacePoints.Clear;
                    for K := 1 to N do
                    begin
                      Index := Face[K - 1];
                      if (Index >= 0) and (Index < Points.Count) then
                      begin
                        CtrPoint := Points[Index] as TFreeSubdivisionControlpoint;
                        if FacePoints.IndexOf(CtrPoint) = -1 then
                          FacePoints.Add(CtrPoint);
                      end;
                    end;
                    if FacePoints.Count > 2 then
                      SubdivisionSurface.AddControlFaceN(FacePoints, True, Layer);
                  end;
                end;
              end;
            end;
            FreeAndNil(FacePoints);

            for I := 1 to AddedCtrlPts.Count do
            begin
              Points := TFasterListTFreeSubdivisionControlPoint(AddedCtrlPts.Objects[I - 1]);
              FreeAndNil(Points);
            end;
            FreeAndNil(AddedCtrlPts);


            // delete empty layers
            for I := SubdivisionSurface.NumberOfLayers downto 1 do
            begin
              if (SubdivisionSurface.Layer[I - 1].Count = 0)
                and (SubdivisionSurface.NumberOfLayers > 1)
              then
                SubdivisionSurface.Layer[I - 1].Delete;
            end;
            SubdivisionSurface.ActiveLayer :=
              SubdivisionSurface.Layer[SubdivisionSurface.NumberOfLayers - 1];
          finally
            SubdivisionSurface.Built := False;
            FreeAndNil(IndexedFaceSets);
          end;
        end
        else
          MessageDlg(rs_No_meshdata_could_be_imported {UserString[203]} + '.', mtError, [mbOK], 0);
        except
          on E:Exception do
             MessageDlg('Exception in VRML Load:'+#10+E.Message, mtError, [mbOK], 0);
        end;
        //FreeAndNil(VRMLScene);
        SubdivisionSurface.IsLoading := false;
      end;
    end;

end.

