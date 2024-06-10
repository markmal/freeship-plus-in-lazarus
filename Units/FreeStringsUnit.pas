unit FreeStringsUnit;

{$IFDEF FPC}
  { $MODE Delphi}
  {$modeswitch result+}
{$ENDIF}

interface

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
