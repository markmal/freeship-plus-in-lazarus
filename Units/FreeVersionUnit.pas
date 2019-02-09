{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2010 by Timoshenko V.F.                                                 }
{    e-mail                  : tvf@rambler.ru                                                 }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }
{    Copyright © 2015, by Mark Malakanov                                                      }
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

unit FreeVersionUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// Unit to keep track of fileversions, bux fixes and release dates

interface

uses Classes,SysUtils,Dialogs;

const
 COMPILE_DATE:string={$I %DATE%};
 COMPILE_TIME:string={$I %TIME%};
 TARGET_CPU:string={$I %FPCTARGET%};
 TARGET_OS:string={$I %FPCTARGETOS%};

 FREESHIP_MAJOR_VERSION='3.6';   //Major version
 FREESHIP_VERSION='3.6.1.0';   //Major full version

 //SUBVERSION_REVISION:integer={$I SVNLastChangeRevision.inc};
 GITVERSION_REVISION={$I GITLastChangeRevision.inc};//number of git commits
 FPCVERSION:string={$I %FPCVERSION%};

type TFreeFileVersion     = (fv100,fv110,fv120,fv130,fv140,fv150,fv160,fv165,fv170,fv180,fv190,fv191,fv195,fv198,fv200,
                             fv201,fv210,fv220,fv230,fv240,fv250,fv260,fv270,fv280,fv290,fv295,fv296,fv297,fv298,fv300,
                             fv302,fv303,fv305,fv309,fv310,fv313,fv314,fv317,fv327,fv332,fv335);
const CurrentVersion      = fv335;   // Current (latest) version of the FREE!ship project.
                                     // All new created models are initialized to this version
      ReleasedDate        = 'Apr 14, 2016';

function VersionString(Version:TFreeFileVersion):String;
function VersionBinary(Version:String):TFreeFileVersion;
function ResourceVersionInfo: String;



implementation

uses FreeLanguageSupport,
     resource, versionresource, versiontypes,
     resreader, coffreader, elfreader, winpeimagereader, elfconsts,
     FreeLogger;

function VersionString(Version:TFreeFileVersion):String;
begin
   Case Version of
      fv100  : Result:='1.0';
      fv110  : Result:='1.1';
      fv120  : Result:='1.2';
      fv130  : Result:='1.3';
      fv140  : Result:='1.4';
      fv150  : Result:='1.5';
      fv160  : Result:='1.6';
      fv165  : Result:='1.65';
      fv170  : Result:='1.7';
      fv180  : Result:='1.8';
      fv190  : Result:='1.9';
      fv191  : Result:='1.91';
      fv195  : Result:='1.95';
      fv198  : Result:='1.98';
      fv200  : Result:='2.0';
      fv201  : Result:='2.01';
      fv210  : Result:='2.1';
      fv220  : Result:='2.2';
      fv230  : Result:='2.3';
      fv240  : Result:='2.4';
      fv250  : Result:='2.5';
      fv260  : Result:='2.6';
      fv270  : Result:='2.7+';
      fv280  : Result:='2.8+';	
      fv290  : Result:='2.94+';	
      fv295  : Result:='2.95+';	
      fv296  : Result:='2.96+';	
      fv297  : Result:='2.97+';	
      fv298  : Result:='2.98+';	
      fv300  : Result:='3.0+';	
      fv302  : Result:='3.02+';
      fv303  : Result:='3.03+';
      fv305  : Result:='3.08+';
      fv309  : Result:='3.09+';
      fv310  : Result:='3.12+';
      fv313  : Result:='3.13+';
      fv314  : Result:='3.16+';
      fv317  : Result:='3.27+';
      fv327  : Result:='3.3+';
      fv332  : Result:='3.34+';
      fv335  : Result:='3.4';	  
      else MessageDlg(Userstring(204)+'!',mtError,[mbok],0);
   end
end;{VersionString}

function VersionBinary(Version:String):TFreeFileVersion;
begin

  if Version =	'1.1'	 then Result:=	      fv110	 else
  if Version =	'1.2'	 then Result:=	      fv120	 else
  if Version =	'1.3'	 then Result:=	      fv130	 else
  if Version =	'1.4'	 then Result:=	      fv140	 else
  if Version =	'1.5'	 then Result:=	      fv150	 else
  if Version =	'1.6'	 then Result:=	      fv160	 else
  if Version =	'1.65'	 then Result:=	      fv165	 else
  if Version =	'1.7'	 then Result:=	      fv170	 else
  if Version =	'1.8'	 then Result:=	      fv180	 else
  if Version =	'1.9'	 then Result:=	      fv190	 else
  if Version =	'1.91'	 then Result:=	      fv191	 else
  if Version =	'1.95'	 then Result:=	      fv195	 else
  if Version =	'1.98'	 then Result:=	      fv198	 else
  if Version =	'2.0'	 then Result:=	      fv200	 else
  if Version =	'2.01'	 then Result:=	      fv201	 else
  if Version =	'2.1'	 then Result:=	      fv210	 else
  if Version =	'2.2'	 then Result:=	      fv220	 else
  if Version =	'2.3'	 then Result:=	      fv230	 else
  if Version =	'2.4'	 then Result:=	      fv240	 else
  if Version =	'2.5'	 then Result:=	      fv250	 else
  if Version =	'2.6'	 then Result:=	      fv260	 else
  if Version =	'2.7+'	 then Result:=	      fv270	 else
  if Version =	'2.8+'	 then Result:=	      fv280	 else
  if Version =	'2.94+'	 then Result:=	      fv290	 else
  if Version =	'2.95+'	 then Result:=	      fv295	 else
  if Version =	'2.96+'	 then Result:=	      fv296	 else
  if Version =	'2.97+'	 then Result:=	      fv297	 else
  if Version =	'2.98+'	 then Result:=	      fv298	 else
  if Version =	'3.0+'	 then Result:=	      fv300	 else
  if Version =	'3.02+'	 then Result:=	      fv302	 else
  if Version =	'3.03+'	 then Result:=	      fv303	 else
  if Version =	'3.08+'	 then Result:=	      fv305	 else
  if Version =	'3.09+'	 then Result:=	      fv309	 else
  if Version =	'3.12+'	 then Result:=	      fv310	 else
  if Version =	'3.13+'	 then Result:=	      fv313	 else
  if Version =	'3.16+'	 then Result:=	      fv314	 else
  if Version =	'3.27+'	 then Result:=	      fv317	 else
  if Version =	'3.3+'	 then Result:=	      fv327	 else
  if Version =	'3.34+'	 then Result:=	      fv332	 else
  if Version =	'3.4'	 then Result:=	      fv335	 else
  raise Exception.Create(Userstring(204)+'! '+Version);
end;{VersionString}


FUNCTION resourceVersionInfo: STRING;

(* Unlike most of AboutText (below), this takes significant activity at run-    *)
(* time to extract version/release/build numbers from resource information      *)
(* appended to the binary.                                                      *)

VAR     Stream: TResourceStream;
        vr: TVersionResource;
        fi: TVersionFixedInfo;

BEGIN
  RESULT:= '';
  TRY

(* This raises an exception if version info has not been incorporated into the  *)
(* binary (Lazarus Project -> Project Options -> Version Info -> Version        *)
(* numbering).                                                                  *)

    Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    TRY
      vr:= TVersionResource.Create;
      TRY
        vr.SetCustomRawDataStream(Stream);
        fi:= vr.FixedInfo;
        RESULT := IntToStr(fi.FileVersion[0]) + '.' + IntToStr(fi.FileVersion[1]) +
               '.' + IntToStr(fi.FileVersion[2]) + '.' + IntToStr(fi.FileVersion[3]);
        vr.SetCustomRawDataStream(nil)
      FINALLY
        vr.Free
      END
    FINALLY
      Stream.Free
    END
  EXCEPT
    Logger.error('Failed to get version info from HINSTANCE');
  END
END { resourceVersionInfo } ;

end.
