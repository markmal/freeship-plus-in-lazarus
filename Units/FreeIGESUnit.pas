{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
{                                                                                             }
{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }
{                                                                                             }
{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }
{                                                                                             }
{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }
{                                                                                             }
{#############################################################################################}

unit FreeIGESUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
     {$ifdef Windows}
      Windows,
     {$else}
     LCLIntf, LCLType, //
     {$endif}

     classes,
     SysUtils,
     DateUtils,
     Forms,
     Dialogs,
     Graphics,
     FreeTypes,
     FreeGeometry;

const ParameterDelimiter = ',';
      RecordDelimiter    = ';';
      LastColumn         = 72;
      LastParameterColumn= 64;          

type TFreeIgesString     = string[80];
     TFreeIGESList       = class
                              private
                                 FStartSection              : TStringList;
                                 FGlobalSection             : TStringList;
                                 FDirectorySection          : TStringList;
                                 FParameterSection          : TStringList;
                                 FTerminateSection          : TStringList;
                                 FNumberOfSurfaces          : Integer;
                                 FIGESunits                 : TFreeUnitType;
                                 FMaxCoordinate             : TFloatType;
                                 FSystemID                  : TFreeIgesString;
                                 FFileCreatedBy             : TFreeIgesString;
                                 FFileName                  : String;
                                 procedure FProcessParameterData(Str:AnsiString;ParamData:TStringList);
                              public
                                 procedure Add_Entity_128(NURB:TFreeNURBSurface;ColorIndex:Integer);
                                 function  Add_Entity_314(Color:TColor):Integer;
                                 procedure Clear;
                                 constructor Create;
                                 destructor Destroy; override;
                                 procedure SaveToFile(Filename:String);
                                 property FileCreatedBy  : TFreeIgesString read FFileCreatedBy write FFileCreatedBy;
                                 property FileName       : String read FFileName write FFileName;
                                 property IGESUnits      : TFreeUnitType read FIGESunits write FIGESunits;
                                 property SystemID       : TFreeIgesString read FSystemID write FSystemID;
                           end;

implementation

function ConvertString(Input:AnsiString):AnsiString;
begin
   if Input='' then Result:=''
               else Result:=IntToStr(Length(Input))+'H'+Input;
end;{ConvertString}

function CheckString(Str:AnsiString;MaxLength:integer;SectionCharacter:Char;index:Integer):string;
const Spaces ='                                                                                ';
var L,C:Integer;
begin
   L:=Length(Str);
   if L>Maxlength then Str:=Copy(Str,1,MaxLength)
   else if L<Maxlength then
   begin
      C:=Maxlength-L;
      Str:=Str+Copy(Spaces,1,C);
   end;
   Result:=Str+SectionCharacter;
   L:=Length(Result)+Length(IntToStr(Index));
   C:=80-L;
   Result:=Result+Copy(Spaces,1,C)+IntToStr(Index);
end;{CheckString}

function IndexStr(Index,MaxLength:Integer):String;
begin
   Result:=IntToStr(Index);
   while length(Result)<MaxLength do Result:=#32+result;
end;{IndexStr}

// Takes one string containing all the parameter data and splits it up into
// lines of 64 characters
procedure TFreeIGESList.FProcessParameterData(Str:AnsiString;ParamData:TStringList);
var LastCol : Integer;
    Tmp     : AnsiString;
begin
   // split up into lines of 64 characters
   if Length(Str)>0 then if Str[Length(Str)]<>RecordDelimiter then Str:=Str+RecordDelimiter;
   while length(Str)>LastParameterColumn do
   begin
      Tmp:=Copy(Str,1,LastParameterColumn);
      LastCol:=LastParameterColumn;
      while (Tmp<>'') and (Tmp[LastCol]<>ParameterDelimiter) do
      begin
         Delete(Tmp,LastCol,1);
         Dec(LastCol);
      end;
      while length(Tmp)<LastParameterColumn+1 do Tmp:=Tmp+#32;
      Tmp:=Tmp+IndexStr(FDirectorySection.Count+1,7)+'P'+IndexStr(FParameterSection.Count+ParamData.Count+1,7);
      ParamData.Add(Tmp);
      Delete(Str,1,LastCol);
   end;
   if Str<>'' then
   begin
      while length(Str)<65 do Str:=Str+#32;
      Str:=Str+IndexStr(FDirectorySection.Count+1,7)+'P'+IndexStr(FParameterSection.Count+ParamData.Count+1,7);
      ParamData.Add(Str);
   end;
end;{TFreeIGESList.FProcessParameterData}

procedure TFreeIGESList.Add_Entity_128(NURB:TFreeNURBSurface;ColorIndex:Integer);
var K1,K2,M1,M2   : Integer;
    I,J           : Integer;
    C             : Integer;
    Param         : TStringList;
    PROP          : array[1..10] of integer;
    EntityStr     : AnsiString;
    P             : T3DCoordinate;
    U0,U1,V0,V1   : TFloatType;

    function CreateKnotvector(N,Degree:Integer;knots:TFloatArray) :AnsiString;
    var I:Integer;
        Tmp:single;
    begin
       Result:='';
       for I:=1 to Length(Knots) do
       begin
         Tmp:=Knots[I-1];
         if Tmp<0 then Tmp:=0;
         if Tmp>1 then Tmp:=1;
         Result:=Result+ParameterDelimiter+Truncate(Tmp,6);
       end;
    end;{CreateKnotvector}

    function CreateWeightVector(N:Integer):AnsiString;
    var I:Integer;
    begin
       Result:='';
       for I:=1 to N do Result:=Result+ParameterDelimiter+'1.0';
    end;{CreateWeightVector}

begin
   inc(FNumberOfSurfaces);
   Param:=TStringList.Create;

   // Create parametersection
   K1:=Nurb.ColCount-1;
   K2:=Nurb.Rowcount-1;
   M1:=Nurb.ColDegree;
   M2:=Nurb.RowDegree;
   Prop[1]:=0; // not closed in u-dir
   Prop[2]:=0; // not closed in v-dir
   Prop[3]:=1; // rational
   Prop[4]:=0; // not periodic in u-dir
   Prop[5]:=0; // not periodic in v-dir
   C:=(1+K1)*(1+K2); // number of weights
   U0:=0.0; U1:=1.0; // Start and end first parameter values
   V0:=0.0; V1:=1.0; // Start and end second parameter values

   EntityStr:=IntToStr(128)+ParameterDelimiter+               // Rational B-Spline Surface (Type 128)
              IntToStr(K1)+ParameterDelimiter+                // Upper index of first sum
              IntToStr(K2)+ParameterDelimiter+                // Upper index of second sum
              IntToStr(M1)+ParameterDelimiter+                // Degree of first basis functions
              IntToStr(M2)+ParameterDelimiter+                // Degree of second basis functions
              IntToStr(Prop[1])+ParameterDelimiter+           // 0=closed in first direction, 1=not closed
              IntToStr(Prop[2])+ParameterDelimiter+           // 0=closed in second direction, 1=not closed
              IntToStr(Prop[3])+ParameterDelimiter+           // 0=rational, 1=polynomial
              IntToStr(Prop[4])+ParameterDelimiter+           // 0=nonperiodic in first direction , 1=periodic
              IntToStr(Prop[5])+                              // 0=nonperiodic in second direction , 1=periodic
              CreateKnotvector(K1,M1,Nurb.ColKnotVector)+     // knot sequence
              CreateKnotvector(K2,M2,Nurb.RowKnotVector)+     // second knot sequence
              CreateWeightVector(C);                          // weight sequence

   for I:=1 to Nurb.Rowcount do
   begin
      for J:=1 to Nurb.ColCount do
      begin
         P:=Nurb.Point[J-1,I-1];
         if abs(P.X)>FMaxCoordinate then FMaxCoordinate:=abs(P.X);
         if abs(P.Y)>FMaxCoordinate then FMaxCoordinate:=abs(P.Y);
         if abs(P.Z)>FMaxCoordinate then FMaxCoordinate:=abs(P.Z);
         EntityStr:=EntityStr+ParameterDelimiter+Truncate(P.X,5)+ParameterDelimiter+
                                                 Truncate(P.Y,5)+ParameterDelimiter+
                                                 Truncate(P.Z,5);
      end;
   end;

   EntityStr:=EntityStr     +ParameterDelimiter+
              Truncate(U0,6)+ParameterDelimiter+      // Start first parameter value
              Truncate(U1,6)+ParameterDelimiter+      // End first parameter value
              Truncate(V0,6)+ParameterDelimiter+      // Start second parameter value
              Truncate(V1,6);                         // End second parameter value


   FProcessParameterData(EntityStr,Param);
   // First line
   EntityStr:=IndexStr(128,8)+                        // entity nr
              IndexStr(FParameterSection.Count+1,8)+  // pointer to parametersection start
              IndexStr(0,8)+                          // structure
              IndexStr(1,8)+                          // line font pattern
              IndexStr(0,8)+                          // level
              IndexStr(0,8)+                          // view
              IndexStr(0,8)+                          // Transformation matrix
              IndexStr(0,8)+                          // Label display associativity
              '00000000';                             // Visible,independent,geometry,topdown
   EntityStr:=CheckString(EntityStr,LastColumn,'D',FDirectorySection.Count+1);
   FDirectorySection.Add(EntityStr);
   // Second line
   EntityStr:=IndexStr(128,8)+                        // entity nr
              IndexStr(0,8)+                          // Lineweight
              IndexStr(ColorIndex,8)+                 // color
              IndexStr(Param.Count,8)+                // number of lines in parameter section
              IndexStr(0,8)+                          // form number
              IndexStr(0,8)+                          // reserved for future use
              IndexStr(0,8)+                          // reserved for future use
              '        '+                             // entity label
              IndexStr(0,8);                          // Entity subscript number
   EntityStr:=CheckString(EntityStr,LastColumn,'D',FDirectorySection.Count+1);
   FDirectorySection.Add(EntityStr);
   FParametersection.AddStrings(Param);
   Param.Destroy;
end;{TFreeIGESList.Add_Entity_128}

function TFreeIGESList.Add_Entity_314(Color:TColor):Integer;
var R,G,B         : Integer;
    Param         : TStringList;
    EntityStr     : AnsiString;
begin
   Param:=TStringList.Create;
   // Create parametersection
   R:=GetRValue(Color);
   G:=GetGValue(Color);
   B:=GetBValue(Color);

   EntityStr:=IntToStr(314)+ParameterDelimiter+
              Truncate(100*R/255,3)+ParameterDelimiter+
              Truncate(100*G/255,3)+ParameterDelimiter+
              Truncate(100*B/255,3)+ParameterDelimiter+
              ConvertString('');
   FProcessParameterData(EntityStr,Param);
   // First line
   EntityStr:=IndexStr(314,8)+                        // entity nr
              IndexStr(FParameterSection.Count+1,8)+  // pointer to parametersection start
              IndexStr(0,8)+                          // structure
              IndexStr(1,8)+                          // line font pattern
              IndexStr(0,8)+                          // level
              IndexStr(0,8)+                          // view
              IndexStr(0,8)+                          // Transformation matrix
              IndexStr(0,8)+                          // Label display associativity
              '00000000';                             // Visible,independent,geometry,topdown
   EntityStr:=CheckString(EntityStr,LastColumn,'D',FDirectorySection.Count+1);
   FDirectorySection.Add(EntityStr);
   Result:=-FDirectorySection.Count;
   // Second line
   EntityStr:=IndexStr(314,8)+                        // entity nr
              IndexStr(0,8)+                          // Lineweight
              IndexStr(0,8)+                          // color
              IndexStr(Param.Count,8)+                // number of lines in parameter section
              IndexStr(0,8)+                          // form number
              IndexStr(0,8)+                          // reserved for future use
              IndexStr(0,8)+                          // reserved for future use
              '        '+                             // entity label
              IndexStr(0,8);                          // Entity subscript number
   EntityStr:=CheckString(EntityStr,LastColumn,'D',FDirectorySection.Count+1);
   FDirectorySection.Add(EntityStr);
   FParametersection.AddStrings(Param);
   Param.Destroy;
end;{TFreeIGESList.Add_Entity_314}

procedure TFreeIGESList.Clear;
begin
   FStartSection.Clear;
   FGlobalSection.Clear;
   FDirectorySection.Clear;
   FParameterSection.Clear;
   FTerminateSection.Clear;
   FNumberOfSurfaces:=0;
   FMaxCoordinate:=0.0;
   FSystemID:='';
   FFileCreatedBy:='';
   FFilename:='';
end;{TFreeIGESList.Clear}

constructor TFreeIGESList.Create;
begin
   Inherited Create;
   FStartSection:=TStringList.Create;
   FGlobalSection:=TStringList.Create;
   FDirectorySection:=TStringList.Create;
   FParameterSection:=TStringList.Create;
   FTerminateSection:=TStringList.Create;
   Clear;
end;{TFreeIGESList.Create}

destructor TFreeIGESList.Destroy;
begin
   Clear;
   FreeAndNil(FStartSection);
   FreeAndNil(FGlobalSection);
   FreeAndNil(FDirectorySection);
   FreeAndNil(FParameterSection);
   FreeAndNil(FTerminateSection);
   inherited Destroy;
end;{TFreeIGESList.Destroy}

procedure TFreeIGESList.SaveToFile(Filename:String);
var Str     : AnsiString;
    TimeStr : string;
    Tmp     : AnsiString;
    Strings : TStringList;
    Index   : Integer;
    LastCol : Integer;

    function CreateTimeStamp:string;
    var Time    : TDateTime;
    begin
       Time:=now;
       // year
       Result:=IntToStr(YearOf(Time));
       // month
       Tmp:=IntToStr(MonthOf(Time));
       if length(Tmp)<2 then Tmp:='0'+Tmp;
       Result:=Result+Tmp;
       // day
       Tmp:=IntToStr(DayOf(Time));
       if length(Tmp)<2 then Tmp:='0'+Tmp;
       Result:=Result+Tmp;
       // separator
       Result:=Result+'.';
       // Hours
       Tmp:=IntToStr(HourOf(Time));
       if length(Tmp)<2 then Tmp:='0'+Tmp;
       Result:=Result+Tmp;
       // minutes
       Tmp:=IntToStr(MinuteOf(Time));
       if length(Tmp)<2 then Tmp:='0'+Tmp;
       Result:=Result+Tmp;
       // seconds
       Tmp:=IntToStr(SecondOf(Time));
       if length(Tmp)<2 then Tmp:='0'+Tmp;
       Result:=Result+Tmp;
       Result:=IntToStr(length(Result))+'H'+Result;

    end;{CreateTimeStamp}

begin
   // Create time string
   TimeStr:=CreateTimeStamp;
   // Create the start section
   FStartSection.Clear;
   FStartSection.Add(CheckString('FREE!ship IGES file. (www.freeship.org)',LastColumn,'S',1));
   // Create the global section
   Str:=ConvertString(ParameterDelimiter)+ParameterDelimiter+
        ConvertString(RecordDelimiter)+ParameterDelimiter+
        ConvertString(ChangeFileExt(ExtractFilename(Application.ExeName),''))+ParameterDelimiter+                    // ProductNameSender
        ConvertString(FFilename)+ParameterDelimiter+                                                                 // Original filenameh
        ConvertString(SystemID)+ParameterDelimiter+                                                                  // Preprocessor
        ConvertString(SystemID)+ParameterDelimiter+                                                                  // Preprocessor version
        IntToStr(32)+ParameterDelimiter+                                                                             // Integer bits
        IntToStr(38)+ParameterDelimiter+                                                                             // Single precision
        IntToStr(6)+ParameterDelimiter+                                                                              // Single sign. bits
        IntToStr(308)+ParameterDelimiter+                                                                            // Double precision
        IntToStr(15)+ParameterDelimiter+                                                                             // Double sign. bits
        ConvertString('')+ParameterDelimiter+                                                                        // Post processor
        FloatToStrF(1.0,ffFixed,7,4)+ParameterDelimiter;                                                             // Model scale
   if IGESUnits=fuImperial then Str:=Str+IntToStr(4)+ParameterDelimiter+ConvertString('Feet')+ParameterDelimiter
                           else Str:=Str+IntToStr(6)+ParameterDelimiter+ConvertString('Meters')+ParameterDelimiter;
   Str:=Str+IntToStr(1)+ParameterDelimiter+                                                                          // Lineweight graduations
        FloatToStrF(1.0,ffFixed,7,4)+ParameterDelimiter+                                                             // max lineweight
        TimeStr+ParameterDelimiter+                                                                                  // Time
        FloatToStrF(0.01,ffFixed,7,6)+ParameterDelimiter+                                                            // Minimum resolution
        FloatToStrF(FMaxCoordinate,ffFixed,7,6)+ParameterDelimiter+                                                  // Maximum coordinate
        ConvertString(FileCreatedBy)+ParameterDelimiter+                                                             // Author
        ''+ParameterDelimiter+                                                                                       // Author organisation
        IntToStr(9)+ParameterDelimiter+                                                                              // IGES version 5.1
        IntToStr(1)+ParameterDelimiter+                                                                              // draftingstandard=NONE
        TimeStr+RecordDelimiter;

   // split up into lines of max. 72 characters
   Index:=1;
   while length(Str)>LastColumn do
   begin
      //
      Tmp:=Copy(Str,1,LastColumn);
      LastCol:=LastColumn;
      while (Tmp<>'') and (Tmp[LastCol]<>ParameterDelimiter) do
      begin
         if pos(ParameterDelimiter,Tmp)=0 then
         begin
            // This must be a stringvalue which spans multiple lines
            Tmp:=Uppercase(Tmp);
            break;
         end else
         begin
            Delete(Tmp,LastCol,1);
            Dec(LastCol);
         end;
      end;
      Tmp:=CheckString(Tmp,LastColumn,'G',Index);
      FGlobalSection.Add(Tmp);
      Inc(Index);
      Delete(Str,1,LastCol);
   end;
   if Str<>'' then
   begin
      Tmp:=CheckString(Str,LastColumn,'G',Index);
      FGlobalSection.Add(Tmp);
   end;

   // create the terminate section
   Str:='S'+IndexStr(FStartsection.Count,7)+
        'G'+IndexStr(FGlobalsection.Count,7)+
        'D'+IndexStr(FDirectorysection.Count,7)+
        'P'+IndexStr(FParametersection.Count,7);
   Str:=CheckString(Str,LastColumn,'T',1);
   FTerminateSection.Add(Str);

   Strings:=TStringList.Create;
   Strings.AddStrings(FStartSection);
   Strings.AddStrings(FGlobalSection);
   Strings.AddStrings(FDirectorySection);
   Strings.AddStrings(FParameterSection);
   Strings.AddStrings(FTerminateSection);
   Strings.SaveToFile(ChangeFileExt(Filename,'.igs'));
   Strings.Destroy;
end;{TFreeIGESList.SaveToFile}


end.
