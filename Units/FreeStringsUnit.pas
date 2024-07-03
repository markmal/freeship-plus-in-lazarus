{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2006, by Mark Malakanov                                                      }
{    FREE!ship project page  : https://github.com/markmal/freeship-plus-in-lazarus            }
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

unit FreeStringsUnit;

{$IFDEF FPC}
  { $MODE Delphi}
  {$modeswitch result+}
{$ENDIF}

interface

resourcestring rs_translation_author = 'Translation: <Your name>';

{ Get rid of UserStrings
Type TUserstring   = record
                        ID     : Integer;
                        Value  : string;
                     end;

const UserStrings:array[0..8] of TUserstring =(
   (ID:0275; Value:'according to John Winters'),
   (ID:0131; Value:'Add cylinder'),
   (ID:0130; Value:'Add flowline'),
   (ID:0227; Value:'Add multiple buttocks'),
   (ID:0233; Value:'Add multiple diagonals'),
   (ID:0224; Value:'Add multiple stations'),
   (ID:0230; Value:'Add multiple waterlines'),
   (ID:0226; Value:'Add one buttock'),
   (ID:0232; Value:'Add one diagonal')
);
}

{$I FreeResourceStrings.inc}

// this is a workaround for code where UserString called in loops with tricky computed indexes
{$I FreeUserStringsArray.inc}
function UserString(Index:Integer):String;
function LanguageCode(Language:string): string;
function LanguageName(code:string): string;

implementation

function UserString(Index:Integer):String;
begin
   Result := UserStrings[Index - 1];
end;{UserString}

// Returns ISO 639 codes from a Language name
function LanguageCode(Language:string): string;
begin
  case Language of
   'Chinese': Result := 'cn';
   'Czech': Result := 'cs';
   'German','Deutsch': Result := 'de';
   'English': Result := 'en';
   'French','Francais': Result := 'fr';
   'Italian','Italiano': Result := 'it';
   'Dutch','Nederlands': Result := 'nl';
   'Norwegian','Norsk': Result := 'no';
   'Russian': Result := 'ru';
   'Spanish': Result := 'es';
   'Finnish','Suomi': Result := 'fi';
   'Ukrainian': Result := 'uk';
   'Vietnamese': Result := 'vi';
  end;
end;

// Returns English Language name from an ISO 639 code
function LanguageName(code:string): string;
begin
  case code of
   'cn': Result := 'Chinese';
   'cs': Result := 'Czech';
   'de': Result := 'German';
   'en': Result := 'English';
   'fr': Result := 'French';
   'it': Result := 'Italian';
   'nl': Result := 'Dutch';
   'no': Result := 'Norwegian';
   'ru': Result := 'Russian';
   'es': Result := 'Spanish';
   'fi': Result := 'Finnish';
   'uk': Result := 'Ukrainian';
   'vi': Result := 'Vietnamese';
  end;
end;

end.
