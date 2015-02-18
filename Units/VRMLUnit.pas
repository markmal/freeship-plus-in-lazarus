unit VRMLUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// Skip translation
interface

uses
    {$IFDEF Windows}
    Windows,
    {$ELSE}
    FileUtil,
    {$ENDIF}
     Classes,
     SysUtils,
     Dialogs,
     Graphics,
     FreeTypes,
     FreeGeometry,
     FasterList;

type TIntArray           = array of integer;
     TVRMLFileType       = (ftVRML1,ftVRML2);
     TVRMLList           = class;
     TVRMLCoordinate3    = class;
     TVRMLIndexedFaceSet = class;
     TVRMLobject         = class
                              private
                                 FOwner:TVRMLList;
                              public
                                 constructor Create(Owner:TVRMLList);     virtual;
                                 procedure Clear;        virtual;
                                 destructor Destroy;override;
                                 procedure Load(var LineNr:Integer;Strings:TStringList);virtual;
                           end;
     TVRMLSeparator      = class(TVRMLobject)
                              private
                                 FObjects             : TFasterList;
                                 function FGetCount:Integer;
                                 function FGetItems(Index:Integer):TVRMLObject;
                              public
                                 procedure Add(VRMLObject:TVRMLObject);
                                 constructor Create(Owner:TVRMLList);override;
                                 procedure Clear;override;
                                 destructor Destroy;override;
                                 procedure Load(var LineNr:Integer;Strings:TStringList);override;
                                 property Count : Integer read FGetCount;
                                 property Items[index:Integer]    : TVRMLObject read FGetItems;
                           end;
     TVRMLCoordinate3    = class(TVRMLobject)
                              private
                                 FCapacity      : Integer;
                                 FCount         : Integer;
                                 FFaceSets      : TFasterList;
                                 FCoordinates   : array of T3DCoordinate;
                                 function FGetNumberOfFacesets:Integer;
                                 function FGetPoint(Index:INteger):T3DCoordinate;
                                 procedure FSetCapacity(val:Integer);
                              public
                                 procedure Add(P:T3DCoordinate);
                                 procedure AddFaceSet(FaceSet:TVRMLIndexedFaceSet);
                                 procedure Clear;override;
                                 constructor Create(Owner:TVRMLList);override;
                                 destructor Destroy;override;
                                 procedure Load(var LineNr:Integer;Strings:TStringList);override;
                                 property Count                : Integer read FCount;
                                 property Capacity             : Integer read FCapacity write FSetCapacity;
                                 property NumberOfFaceSets     : Integer read FGetNumberOfFacesets;
                                 property Point[index:Integer] : T3DCoordinate read FGetPoint;
                           end;
     TVRMLIndexedFaceSet = class(TVRMLobject)
                              private
                                 FCapacity      : Integer;
                                 FCount         : Integer;
                                 FFaces         : array of TIntArray;
                                 FCoordinates   : TVRMLCoordinate3;
                                 function FGetFace(Index:Integer):TIntArray;
                                 procedure FSetCapacity(val:Integer);
                              public
                                 procedure Clear;override;
                                 procedure Load(var LineNr:Integer;Strings:TStringList);override;
                                 property Coordinates          : TVRMLCoordinate3 read FCoordinates;
                                 property Count                : Integer read FCount;
                                 property Capacity             : Integer read FCapacity write FSetCapacity;
                                 property Face[index:Integer]  : TIntArray read FGetFace;
                           end;
     TVRMLList           = class
                              private
                                 FObjects             : TFasterList;
                                 FFaceSets            : TFasterList;
                                 FFileType            : TVRMLFileType;
                                 FLastAddedCoordinates: TVRMLCoordinate3;
                                 function FGetCount:integer;
                                 function FGetItems(Index:Integer):TVRMLObject;
                              public
                                 procedure Add(VRMLObject:TVRMLObject);
                                 procedure Clear;
                                 constructor Create;
                                 destructor Destroy;override;
                                 function ExtractFaceSetData:TFasterList;
                                 procedure LoadFromFile(Filename:String);
                                 property Count                   : Integer read FGetCount;
                                 property Items[index:Integer]    : TVRMLObject read FGetItems;
                           end;

implementation

uses FreeLanguageSupport;

function Trim(input:Ansistring):AnsiString;
var I,N :Integer;
begin
   N:=0;
   for I:=1 to Length(input) do
   begin
      if Input[I]=#32 then inc(N) else break;
   end;
   if N<>0 then
   begin
      Delete(Input,1,N);
   end;
   N:=0;
   for I:=Length(input) downto 1 do
   begin
      if Input[I]=#32 then inc(N) else break;
   end;
   if N<>0 then
   begin
      Delete(Input,Length(Input)-N+1,N);
   end;
   Result:=input;
end;

procedure ProcessString(Input:Ansistring;var output:TStringList);
var Index   : Integer;
    Tmp     : Ansistring;
begin
   // split a string value into multiple words
   Output.Clear;
   if Input='' then exit;
   repeat
      Index:=Pos(#10,Input);
      if Index<>0 then Input[index]:=#32;
   until index=0;
   repeat
      Index:=Pos(#13,Input);
      if Index<>0 then Input[index]:=#32;
   until index=0;

   repeat
      Index:=Pos(#32,Input);
      if Index<>0 then
      begin
         Tmp:=Copy(Input,1,Index-1);
         if Tmp<>'' then Output.Add(Tmp);
         Delete(Input,1,index);
      end else
      begin
         trim(Input);
         if Input<>'' then Output.Add(Input);
         Input:='';
      end;
   until Input='';
   if Output.Count=0 then Output.Clear;
end;{ProcessString}

procedure LoadNextObject(strings:TStringList;var LineNr,NumberOfObjects:Integer;var Objectname:string;Dest:TStringList);
var Level,L : Integer;
    Tmp,Str : string;
    Index   : Integer;
    Ch      : char;
    Done    : Boolean;
begin
   Str := '';
   Dest.Clear;
   Level:=0;
   Done:=False;
   Objectname:='';
   NumberOfObjects:=0;
   while (LineNr<Strings.Count) and (not Done) do
   begin
      Tmp:=Strings[LineNr];
      index:=1;
      L:=Length(Tmp);
      while Index<=L do
      begin
         Ch:=Tmp[index];

         if (Level=0) and (Ch<>'{') then
         begin
            if Ch=#32 then
            begin
               if Objectname<>'' then ObjectName:=ObjectName+ch;
            end else Objectname:=Objectname+Ch;
            //and (Ch<>#32) then Objectname:=Objectname+Ch;
         end;

         if ch='[' then
         begin
            if level<>0 then Str:=Str+Ch;
            inc(level);

         end else if Ch=']' then
         begin
            dec(Level);
            if Level=0 then
            begin
               Done:=True;
               break;
            end else Str:=Str+Ch;
         end else

         if Ch='{' then
         begin
            inc(NumberOfObjects);
            if Level<>0 then Str:=Str+Ch;
            inc(Level);
         end else if Ch='}' then
         begin
            dec(Level);
            if Level=0 then
            begin
               Done:=True;
               break;
            end else Str:=Str+Ch;
         end else if Level<>0 then Str:=Str+Ch;
         inc(Index);
      end;
      if Str<>'' then
      begin
         Dest.Add(Str);
         Str:='';
      end;
      if Done then
      begin
         Delete(Tmp,1,Index);
         if Tmp='' then
         begin
            inc(LineNr);
         end else
         begin
            Strings[LineNr]:=Tmp;
         end;
      end else
      begin
         if level=0 then
         begin
            if objectname<>'' then
               if Objectname[length(Objectname)]<>#32 then
                  objectname:=objectname+#32;
         end;
         inc(LineNr);
      end;
   end;
   Objectname:=Trim(Objectname);
   if not Done then
   begin
      Done:=(Objectname='') and (Dest.Count=0);
      if NumberOfObjects=0 then Done:=True;
      if not done then MessageDlg(Userstring(115),mtError,[mbOk],0);
   end;
end;{LoadNextObject}

procedure GetEmbeddedObjects(Source:TStringlist;Dest:TStringList);
var ToDo    : TList;
    I       : Integer;
    Line    : Integer;
    NObj    : Integer;
    Current : TStringList;
    OutPut  : TStringList;
    ObjName : string;

begin
   ToDo:=TList.Create;
   ToDo.Add(Source);
   I:=1;
   while I<=ToDo.Count do
   begin
      Current:=ToDo[I-1];
      Line:=0;
      While Line<Current.Count do
      begin
         OutPut:=TStringlist.Create;
         LoadNextObject(Current,Line,NObj,ObjName,Output);
         if NObj>1 then ToDo.Add(Output)
                   else Dest.AddObject(ObjName,Output);
      end;
      inc(I);
   end;
   ToDo.Destroy;
end;{GetEmbeddedObjects}

// ##################################### VRML base object #####################################
constructor TVRMLobject.Create(Owner:TVRMLList);
begin
   inherited Create;
   FOwner:=Owner;
   clear;
end;{TVRMLobject.Create}

procedure TVRMLobject.Clear;
begin
end;{TVRMLobject.Clear}

destructor TVRMLobject.Destroy;
begin
   Clear;
   inherited Destroy;
end;{TVRMLobject.Destroy}

procedure TVRMLobject.Load(var LineNr:Integer;Strings:TStringList);
begin
end;{TVRMLobject.Load}

// ##################################### VRML Separator #####################################
function TVRMLSeparator.FGetCount:Integer;
begin
   Result:=FObjects.Count;
end;{TVRMLSeparator.FGetCount}

function TVRMLSeparator.FGetItems(Index:Integer):TVRMLObject;
begin
   Result:=FObjects[index];
end;{TVRMLSeparator.FGetItems}

procedure TVRMLSeparator.Add(VRMLObject:TVRMLObject);
begin
   FObjects.Add(VRMLObject);
   if VRMLObject is TVRMLCoordinate3 then
   begin
      FOwner.FLastAddedCoordinates:=VRMLObject as TVRMLCoordinate3;
   end else
   begin
      if VRMLObject is TVRMLIndexedFaceSet then
      begin
         FOwner.FFaceSets.Add(VRMLObject);
         if FOwner.FLastAddedCoordinates<>nil then FOwner.FLastAddedCoordinates.AddFaceSet(VRMLObject as TVRMLIndexedFaceSet);
      end;
   end;
end;{TVRMLSeparator.Add}

constructor TVRMLSeparator.Create(Owner:TVRMLList);
begin
   FObjects:=TFasterList.Create;
   Inherited Create(Owner);
end;{TVRMLSeparator.Create}

procedure TVRMLSeparator.Clear;
var I:Integer;
begin
   for I:=1 to Count do Items[I-1].Destroy;
   FObjects.Clear;
   inherited clear;
end;{TVRMLSeparator.Clear}

destructor TVRMLSeparator.Destroy;
begin
   Inherited Destroy;
   FObjects.Destroy;
end;{TVRMLSeparator.Destroy}

procedure TVRMLSeparator.Load(var LineNr:Integer;Strings:TStringList);
var Words      : TStringList;
    ObjectName : string;
    VRMLObject : TVRMLObject;
    Index,NObj : Integer;
begin
   Words:=TStringList.Create;
   While LineNr<Strings.Count do
   begin
      LoadNextObject(Strings,LineNr,NObj,Objectname,Words);
      VRMLObject:=nil;
      if Pos('SEPARATOR',Objectname)<>0 then VRMLObject:=TVRMLSeparator.create(FOwner) else
         if Pos('COORDINATE3',Objectname)<>0 then VRMLObject:=TVRMLCoordinate3.create(FOwner) else
            if Pos('INDEXEDFACESET',Objectname)<>0 then VRMLObject:=TVRMLIndexedFaceSet.Create(FOwner) else
      begin
         {
         if NObj>1 then
         begin
            Objects:=TStringList.Create;
            GetEmbeddedObjects(Words,Objects);
            for I:=1 to Objects.Count do
            begin
               ObjectName:=Objects[I-1];
               if Pos('INDEXEDFACESET',Objectname)<>0 then
               begin
               end;
               words:=Objects.Objects[I-1] as TStringList;
               Words.destroy;
            end;
            //Showmessage(Objects.Text);
            Objects.Destroy;
         end;
         }
      end;
      if VRMLObject<>nil then
      begin
         Index:=0;
         VRMLObject.Load(Index,Words);
         Add(VRMLObject);
      end;
   end;
   words.Destroy;
end;{TVRMLSeparator.Load}

// ##################################### VRML Material #####################################
function TVRMLCoordinate3.FGetNumberOfFacesets:Integer;
begin
   Result:=FFaceSets.Count;
end;{TVRMLCoordinate3.FGetNumberOfFacesets}

function TVRMLCoordinate3.FGetPoint(Index:Integer):T3DCoordinate;
begin
   if (Index>=0) and (Index<FCount) then Result:=FCoordinates[index] else
   begin
      Result:=ZERO;
      MessageDlg('Point index out of bounds in TVRMLCoordinate3.FGetPoint',mtError,[mbOk],0);
   end;
end;{TVRMLCoordinate3.FGetPoint}

procedure TVRMLCoordinate3.FSetCapacity(val:Integer);
begin
   FCapacity:=val;
   Setlength(FCoordinates,FCapacity);
   if FCapacity<FCount then FCount:=FCapacity;
end;{TVRMLCoordinate3.FSetCapacity}

procedure TVRMLCoordinate3.Add(P:T3DCoordinate);
begin
   if FCount>=FCapacity then Capacity:=Count+25;
   inc(FCount);
   FCoordinates[FCount-1]:=P;
end;{TVRMLCoordinate3.Add}

procedure TVRMLCoordinate3.AddFaceSet(FaceSet:TVRMLIndexedFaceSet);
begin
   if FFaceSets.IndexOf(FaceSet)=-1 then
   begin
      FFaceSets.Add(FaceSet);
      FaceSet.FCoordinates:=self;
   end;
end;{TVRMLCoordinate3.AddFaceSet}

procedure TVRMLCoordinate3.Clear;
begin
   Capacity:=0;
   FCount:=0;
   FFaceSets.Clear;
end;{TVRMLCoordinate3.Clear}

constructor TVRMLCoordinate3.Create(Owner:TVRMLList);
begin
   FFaceSets:=TFasterList.Create;
   Inherited Create(Owner);
end;{TVRMLCoordinate3.Create}

destructor TVRMLCoordinate3.Destroy;
begin
   Inherited Destroy;
   FFaceSets.Destroy;
end;{TVRMLCoordinate3.Destroy}

procedure TVRMLCoordinate3.Load(var LineNr:Integer;Strings:TStringList);
var Data    : AnsiString;
    Index   : Integer;
    S,F,I,L : Integer;
    Flag    : Integer;
    Ch      : char;
    OK      : boolean;
    Points  : TStringList;
    P       : T3DCoordinate;

begin
   Data:=Strings.Text;
   Index:=Pos('POINT',Data);
   if Index<>0 then
   begin
      S:=-1;
      F:=-1;
      L:=Length(Data);
      I:=Index+1;
      While I<=L do
      begin
         Ch:=Data[I];
         if (Ch='[') and (S=-1) then S:=I;
         if (Ch=']') and (F=-1) then F:=I;
         if (S<>-1) and (F<>-1) then break;
         inc(I);
      end;
      OK:=(S<>-1) and (F<>-1);
      if OK then
      begin
         Points:=TStringList.Create;
         ProcessString(Copy(Data,S+1,F-S-2),Points);
         if Points.Count mod 3=0 then
         begin
            Capacity:=Points.Count div 3;
            I:=1;
            while I<=Points.Count do
            begin
               P:=ZERO;
               OK:=True;
               Val(Points[I-1],P.X,Flag);
               if Flag<>0 then OK:=False;

               Val(Points[I],P.Y,Flag);
               if Flag<>0 then OK:=False;

               Val(Points[I+1],P.Z,Flag);
               if Flag<>0 then OK:=False;

               if OK then Add(P);
               inc(I,3);
            end;
         end;
         Points.Destroy;
      end;
   end;
end;{TVRMLCoordinate3.Load}

// ##################################### VRML Indexed face set #####################################
function TVRMLIndexedFaceSet.FGetFace(Index:Integer):TIntArray;
begin
   if (Index>=0) and (Index<FCount) then Result:=FFaces[index] else
   begin
      Result:=nil;
      MessageDlg('Face index out of bounds in TVRMLIndexedFaceSet.FGetFace',mtError,[mbOk],0);
   end;
end;{TVRMLIndexedFaceSet.FGetFace}

procedure TVRMLIndexedFaceSet.FSetCapacity(val:Integer);
begin
   FCapacity:=val;
   Setlength(FFaces,FCapacity);
   if FCapacity<FCount then FCount:=FCapacity;
end;{TVRMLIndexedFaceSet.FSetCapacity}

procedure TVRMLIndexedFaceSet.Clear;
begin
   Capacity:=0;
   FCount:=0;
   FCoordinates:=nil;
end;{TVRMLIndexedFaceSet.Clear}

procedure TVRMLIndexedFaceSet.Load(var LineNr:Integer;Strings:TStringList);
var Data    : AnsiString;
    Index   : Integer;
    S,F,I,L,N : Integer;
    Flag    : Integer;
    Ch      : char;
    OK      : boolean;
    Faces   : TStringList;
    Tmp     : array of integer;
begin
   Data:=Strings.Text;
   Index:=Pos('COORDINDEX',Data);
   if Index<>0 then
   begin
      S:=-1;
      F:=-1;
      L:=Length(Data);
      I:=Index+1;
      While I<=L do
      begin
         Ch:=Data[I];
         if (Ch='[') and (S=-1) then S:=I;
         if (Ch=']') and (F=-1) then F:=I;
         if (S<>-1) and (F<>-1) then break;
         inc(I);
      end;
      OK:=(S<>-1) and (F<>-1);
      if OK then
      begin
         Data:=Copy(Data,S+1,F-S-2);
         Faces:=TStringList.Create;
         ProcessString(Data,Faces);
         Setlength(Tmp,Faces.Count);
         N:=0;
         for I:=1 to faces.Count do
         begin
            Val(Faces[I-1],Index,Flag);
            if Flag=0 then
            begin
               if Index=-1 then
               begin
                  if N>2 then
                  begin
                     if FCount>=Capacity then Capacity:=FCount+25;
                     setlength(FFaces[FCount],N);
                     Move(Tmp[0],FFaces[FCount][0],N*SizeOf(Integer));
                     inc(FCount);
                  end;
                  N:=0;
               end else
               begin
                  Tmp[N]:=Index;
                  inc(N);
               end;

            end;
         end;
         Faces.Destroy;
      end;
   end;
end;{TVRMLIndexedFaceSet.Load}

// ##################################### VRML list #####################################
function TVRMLList.FGetCount:integer;
begin
   Result:=FObjects.Count;
end;{TVRMLList.FGetCount}

function TVRMLList.FGetItems(Index:Integer):TVRMLObject;
begin
   Result:=FObjects[index];
end;{TVRMLList.FGetItems}

procedure TVRMLList.Add(VRMLObject:TVRMLObject);
begin
   FObjects.Add(VRMLObject);
   if VRMLObject is TVRMLCoordinate3 then
   begin
      FLastAddedCoordinates:=VRMLObject as TVRMLCoordinate3
   end else if VRMLObject is TVRMLIndexedFaceSet then
   begin
      FFaceSets.Add(VRMLObject);
      if FLastAddedCoordinates<>nil then FLastAddedCoordinates.AddFaceSet(VRMLObject as TVRMLIndexedFaceSet);
   end;
end;{TVRMLList.Add}

procedure TVRMLList.Clear;
var I:Integer;
begin
   for I:=1 to count do Items[I-1].Destroy;
   FObjects.Clear;
   FFaceSets.Clear;
   FLastAddedCoordinates:=nil;
   FFileType:=ftVRML1;
end;{TVRMLList.Clear}

constructor TVRMLList.Create;
begin
   FObjects:=TFasterList.Create;
   FFaceSets:=TFasterList.Create;
   clear;
end;{TVRMLList.Create}

destructor TVRMLList.destroy;
begin
   Clear;
   FObjects.Destroy;
   FFaceSets.Destroy;
   inherited Destroy;
end;{TVRMLList.destroy}

function TVRMLList.ExtractFaceSetData:TFasterList;
var I       : Integer;
    FaceSet : TVRMLIndexedFaceSet;
begin
   Result:=nil;
   if FFaceSets.Count>0 then
   begin
      Result:=TFasterList.Create;
      Result.Capacity:=FFaceSets.Count;
      for I:=1 to FFaceSets.Count do
      begin
         FaceSet:=FFacesets[I-1];
         if Faceset.Coordinates<>nil then Result.Add(FaceSet);
      end;
      if Result.Count=0 then
      begin
         Result.Destroy;
         result:=nil;
      end;
   end;
end;{TVRMLList.ExtractFaceSetData}

procedure TVRMLList.LoadFromFile(Filename:String);
var Strings       : TStringList;
    LineNr        : Integer;
    I,Index,NObj  : Integer;
    Str           : String;
    ObjectName    : string;
    FFile         : TextFile;
    VRMLObject    : TVRMLObject;
    Words         : TStringList;
    ValidFile     : Boolean;
begin
   if FileExistsUTF8(Filename) { *Converted from FileExists* } then
   begin
      Clear;
      Strings:=TStringList.Create;
      AssignFile(FFile,FileName);
      Reset(FFile);
      ValidFile:=False;
      if not EOF(FFile) then
      begin
         Readln(FFile,Str); // First line, must contain the string: "#VRML V1.0 ascii"
         Str:=Uppercase(Str);
         if pos('#VRML V1.0 ASCII',Str)<>0 then
         begin
            FFileType:=ftVRML1;
            ValidFile:=true;
         end else if pos('#VRML V2.0 UTF8',Str)<>0 then
         begin
            FFileType:=ftVRML2;
            ValidFile:=true; // Закоментировать если не работает
         end;
      end;
      if not ValidFile then
      begin
         MessageDlg(Userstring(208)+'!',mtError,[mbOk],0);
         CloseFile(FFile);
         Strings.Destroy;
         exit;
      end;
      while not EOF(FFile) do
      begin
         Readln(FFile,Str);
//         Read(FFile,Str);
         I:=1;
         while I<=Length(Str) do
         begin
            if Str[I] in [#9,','] then Str[I]:=#32 else
               if Str[I]='#' then
               begin
                  // remove comments
                  Str:=copy(Str,1,I-1);
                  break;
               end;
            inc(I);
         end;
         repeat
            // replace double spaces by only 1
            I:=Pos('  ',Str);
            if I<>0 then
            begin
               System.Delete(Str,I,1);
            end;
         until I=0;
         // remove leading and trailing spaces
         Str:=Trim(Str);
         if Str<>'' then Strings.Add(Uppercase(Str));
      end;
      CloseFile(FFile);
      if Strings.Count>0 then
      begin
         LineNr:=0;
         Words:=TStringList.Create;
         While LineNr<Strings.Count do
         begin
            LoadNextObject(Strings,LineNr,NObj,Objectname,Words);
//            showmessage(words.Text);  
            VRMLObject:=nil;

            if Pos('SEPARATOR',Objectname)<>0 then VRMLObject:=TVRMLSeparator.create(self) else
               if Pos('COORDINATE3',Objectname)<>0 then VRMLObject:=TVRMLCoordinate3.create(self) else
                  if pos('INDEXEDFACESET',Objectname)<>0 then VRMLObject:=TVRMLIndexedFaceSet.Create(self);
            if VRMLObject<>nil then
            begin
               Index:=0;
               VRMLObject.Load(Index,Words);
               Add(VRMLObject);
            end;
         end;
         words.Destroy;
      end else MessageDlg(Userstring(208)+'!',mtError,[mbOK],0);
      Strings.destroy;
   end;
end;{TVRMLList.LoadFromFile}

end.