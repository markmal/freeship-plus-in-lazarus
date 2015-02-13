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

uses SysUtils,
     Dialogs;

type TFreeFileVersion     = (fv100,fv110,fv120,fv130,fv140,fv150,fv160,fv165,fv170,fv180,fv190,fv191,fv195,fv198,fv200,
                             fv201,fv210,fv220,fv230,fv240,fv250,fv260,fv270,fv280,fv290,fv295,fv296,fv297,fv298,fv300,
                             fv302,fv303,fv305,fv309,fv310,fv313,fv314,fv317,fv327,fv332,fv335);
const CurrentVersion      = fv335;   // Current (latest) version of the FREE!ship project.
                                     // All new created models are initialized to this version
      ReleasedDate        = 'March 31, 2013';
function VersionString(Version:TFreeFileVersion):String;

implementation

uses FreeLanguageSupport;

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

end.
