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

// Based upon IniLang v 0.9
// Freeware unit for Delphi 4 projects
// by Frйdйric Sigonneau <aFas member> 24/04/1999
// e-mail : frederic.sigonneau@wanadoo.fr
// Modified to suit FREEship and adapted to new components

unit FreeLanguageSupport;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
  LCLProc,
  LMessages,
{$ENDIF}
  ActnList,
     SysUtils,
     Classes,
     Forms,
     stdCtrls,
     typInfo,
     extCtrls,
     iniFiles,
     Dialogs,
     Menus,
     ComCtrls,
     FreeStringsUnit,
     FreeStringUtils,
     Controls,
     FileUtil,
     StrUtils;

// Skip translation


type TCompInfo = record
                    CompName: String;
                    Action  : string;
                    Caption : string;
                    Hint    : string;
                 end;

var CurrentLanguage : TMemIniFile; 	//Global variable for current language

//user procs
function LoadLanguage(Name:string):TMemIniFile;overload;
procedure ShowTranslatedValues(Component:TComponent);
function UserString(Index:Integer):String;
function tl8(Key:String):String;

//utilities
function GetCompInfo(Component:TComponent):TCompInfo;
function GetProperty(comp:TComponent;prop:string):string;
procedure setProp(comp:TComponent;{const }prop,value:string);
function HasProperty(comp:TComponent;prop:string):boolean;

implementation

function LoadLanguage(Name:string):TMemIniFile;
var Filename:string;    
begin
   if Name='' then exit; // leave with default English language
   //Filename:=ChangeFileExt(extractFileDir(application.exeName)+'/Languages/'+Name,'.ini');
   Filename:=Name;
   if FileExistsUTF8(Filename) { *Converted from FileExists* } then
   begin
      if CurrentLanguage<>nil then CurrentLanguage.Free;
      CurrentLanguage:=TMemIniFile.create(Filename);
   end else if uppercase(Name)<>'ENGLISH'
      then MessageDlg('Could not load language file '+Filename,mtError,[mbOk],0);
   Result:=CurrentLanguage;
end;{LoadLanguage}

// Takse a string value and detrmines if it is a stringvalue that should
// be translated into another langage
// Values not to be translated are: numbers, strings containing no valid characters
function ValidString(Input:String):Boolean;
var Value   : single;
    IsNumber: Boolean;
    Flag,I,N: Integer;
    Up      : string;
begin
   Result:=True;
   Up:=Uppercase(trim(Input));
   if Input='' then Result:=False;
   if Result then
   begin
      if (Up='.FBM') or
         (Up='[MM]') or
         (Up='[M]') or
         (Up='[M2]') or
         (Up='[M3]') or
         (Up='[M4]') or
         (Up='[FT]') or
         (Up='[FT2]') or
         (Up='[FT3]') or
         (Up='[FT4]') or
         (Up='[T/M3]') or
         (Up='[LBS/FT3]') or
         (Up='[DEGR]') or
         (Up='[DEGR.]') or
         (Up='[%]') or
         (Up='[KN]') or
         (Up='[INCH]') then result:=False;
   end;
   if Result then
   begin
      Val(Input,Value,flag);
      IsNumber:=Flag=0;
      if IsNumber then Result:=False;
   end;
   if result then
   begin
      // check for valid characters
      N:=0;
      for I:=1 to Length(Up) do if Up[I] in ['A'..'Z']then
      begin
         inc(N);
      end;
      Result:=N>1;
   end;
end;{ValidString}

function UserString(Index:Integer):String;
var Str     : string;
    Section : String;
    val     : string;
    I,N     : Integer;
    Key     : string = 'User0000';
begin
   Result:='';
   if CurrentLanguage<>nil then
   begin
      Section:='User';
       {Val:=IntToStr(Index);
       while length(val)<4 do val:='0'+val;
       Val:='User'+Val;
       Str:=CurrentLanguage.readString(Section,Val,'');
       }
      Val:=IntToStr(Index);
      Key := Copy(Key,1,8-len(Val))+Val;
      Str:=CurrentLanguage.readString(Section,Key,'');
      if Str<>'' then Result:=Str;
   end;
   if Result='' then
   begin
      N:=Length(UserStrings);
      for I:=1 to N do if UserStrings[I-1].ID=index then
      begin
         Result:=UserStrings[I-1].Value;
         break;
      end;
   end;
end;{UserString}

{ Direct translation - key is a word or a phrase in Latin chars, it can be a whole phase.
 Example:
Hello world!=Здравствуй, мир!
It can be multiline too, EOLs in key and value will be replaced by \n.
}
function tl8(Key:String):String;
var Str     : string;
    Section : String;
    val     : string;
    idx     : string;
    I,N     : Integer;
begin
   Result:=Key;
   if CurrentLanguage<>nil then
   begin
      Section:='Direct';
      idx:=ReplaceStr(key,'\','\\');
      idx:=ReplaceStr(idx,'=','\q');
      idx:=ReplaceStr(idx,LineEnding,'\n');
      Str:=CurrentLanguage.readString(Section,idx,idx);
      Str:=ReplaceStr(Str,'\n',LineEnding);
      Str:=ReplaceStr(Str,'\q','=');
      Str:=ReplaceStr(Str,'\\','\');
      Result:=Str;
   end;
end;{UserString}


{Translates the forms you choose in the language called in ini.
Only created forms are translated with ShowTranslatedValues. Call it in the onShow
event of your main form whith names of all automatically created forms
at the start-up of your application in the TC parameter.
In runtime, call it when you create dynamically a form.
See demo for a sample}
procedure ShowTranslatedValues(Component:TComponent);
var I,J     : integer;
    Index   : Integer;
    Comp    : TComponent;
    Str,Tmp : string;
begin
   if (CurrentLanguage=nil) or (Component=nil) then exit;
   with CurrentLanguage do
   begin
      Str:=readString(Component.Classname,Component.Classname+'.Caption','');
      if Str<>'' then TForm(Component).caption:=Str;
      for i:=0 to Component.componentCount-1 do
      begin
   		comp:=Component.Components[i];

         Str:=readString(Component.Classname,Component.Classname+'.'+comp.name+'.Caption','');
         if Str<>'' then setProp(comp,'Caption',Str);

         Str:=readString(Component.ClassName,Component.Classname+'.'+comp.name+'.Hint','');
         if Str<>'' then setProp(comp,'Hint',Str);

         if comp is TCustomMemo then
         begin
         	for J:=0 to TCustomMemo(comp).lines.count-1 do
            begin
            	Str:=readString(Component.classname,
                  Component.classname+'.'+comp.name+'.lines['+intToStr(J)+']','fsdef');
               //in TMemo or TRichEdit, you may have to leave some lines empty
               if Str<>'fsdef' then TCustomMemo(comp).lines[J]:=Str;
            end;
         end else if comp is TRadioGroup then
         begin
         	for J:=0 to TRadioGroup(comp).items.count-1 do
            begin
            	Str:=readString(Component.classname,
                  Component.classname+'.'+comp.name+'.items['+intToStr(J)+']','');
               if Str<>'' then TRadioGroup(comp).items[J]:=Str;
            end;
         end else if comp is TComboBox then
         begin
            Index:=TComboBox(comp).ItemIndex;
            TComboBox(comp).Items.BeginUpdate;
            try
            	for J:=0 to TComboBox(comp).items.count-1 do
               begin
                  Tmp:=Component.classname+'.'+comp.name+'.items['+IntToStr(J)+']';
               	Str:=readString(Component.classname,Tmp,'');
               	if Str<>'' then TComboBox(comp).items[J]:=Str;
               end;
            finally
               TComboBox(comp).Items.EndUpdate;
               TComboBox(comp).ItemIndex:=Index;
            end;
         end;
   	end;
   end;
end;{ShowTranslatedValues}

//Backs up the prop property value of the comp component
function GetProperty(comp:TComponent;prop:string):string;
var ppi:PPropInfo;
    P:TPropInfo;
begin
	ppi:=getPropInfo(comp.classInfo,prop);
   if ppi<>nil then P:=ppi^;
   if ppi<>nil then result:=getStrProp(comp,ppi)
               else result:='';
end;

//Assign the value value to prop property of comp component
procedure setProp(comp:TComponent;{const }prop,value:string);
var ppi:PPropInfo;
begin
	if value<>'' then
   begin
   	ppi:=getPropInfo(comp.classInfo,prop);
      if ppi<>nil then setStrProp(comp,ppi,value);
   end;
end;

//True if prop property exists for comp component
function HasProperty(comp:TComponent;prop:string):boolean;
begin
	result:=(getPropInfo(comp.classInfo,prop)<>nil) and (comp.name<>'');
end;

function GetCompInfo(Component:TComponent):TCompInfo;
var ActionComp : TAction;
    Obj        : TObject;
    Index      : Integer;
begin
   Result.CompName:=Component.Name;
   if HasProperty(Component,'Action') then
   begin
      Obj:=GetObjectProp(Component,'Action');
      if Obj<>nil then
      begin
         ActionComp:=Obj as TAction;
         Result.Action:=ActionComp.Name;
      end else Result.Action:='';

   end else Result.Action:='';
   if HasProperty(Component,'Caption') then
   begin
      Result.Caption:=GetProperty(Component,'Caption');
      Index:=pos('&',Result.Caption);
      if Index<>0 then Delete(result.Caption,Index,1);
   end else Result.Caption:='';
   if HasProperty(Component,'Hint') then Result.Hint:=GetProperty(Component,'Hint')
                                    else Result.Hint:='';
end;{GetCompInfo}

initialization
   CurrentLanguage:=nil;

finalization
   if CurrentLanguage<>nil then CurrentLanguage.Free;

end.
