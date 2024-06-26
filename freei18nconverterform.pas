unit FreeI18nConverterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fgl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

const languages: array of String = (
'Chinese',
'Czech',
'Deutsch',
'English',
'Francais',
'Italiano',
'Nederlands',
'Norsk',
'Russian',
'Spanish',
'Suomi',
'Ukrainian',
'Vietnamese' );

// ISO 639 codes
const langs: array of String = (
'cn',
'cs',
'de',
'en',
'fr',
'it',
'nl',
'no',
'ru',
'es',
'fi',
'uk',
'vi' );

procedure runConversion;

implementation

{$R *.lfm}

uses
  Translations, FileUtil,
  FreeStringUtils, FreeLanguageSupport;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  runConversion;
end;

function genSRname(str:string):string;
var s:string; i:integer;
begin
  Result := 'rs_';
  s := str;
  for i:=1 to s.Length do
    begin
    if ((s[i]>='0') and (s[i]<='9'))
     or((s[i]>='A') and (s[i]<='Z'))
     or((s[i]>='a') and (s[i]<='z'))
    then Result += s[i]
    else if s[i]='+' then Result += 't' // replace text table frames
    else if s[i]='|' then Result += 'l'
    else if s[i]='*' then Result += 'o'
    else Result += '_';
    end;
  if Result.Length > 127 then
    Result := Result.Substring(1,120)+'_'+Result.GetHashCode.ToHexString(4);
end;

var EnglishLanguageIniFile: TLanguageIniFile;

type EnglishString = record
   strResId: string;
   englishStr: string;
end;

const UserStringCount = 1670;

var EnglishStrings: array [1..UserStringCount] of EnglishString;


procedure replaceStringInFile(filename:string; findString:String; replaceString:String);
  var
    lines: TStringList;
    i: integer; found:boolean=false; findString_lc:string;
  begin
    lines := TStringList.Create;
    try
      lines.LoadFromFile(filename);
      findString_lc := findstring.ToLower;
      for i := 0 to lines.Count-1 do
        if lines[i].ToLower.Contains(findString_lc)
        then
          begin
            found := true;
            break;
          end;
      if found then
      begin
        for i := 0 to lines.Count-1 do
          lines[i] := StringReplace(lines[i], findstring, replaceString, [rfReplaceAll,rfIgnoreCase]);
        lines.SaveToFile(filename);
      end;
    finally
      lines.Free;
    end;
  end;

procedure replaceStringInFiles(dir:string; findString:String; replaceString:String);
var PascalFiles: TStringList; pasfile: string; i: integer;
begin
  PascalFiles := FindAllFiles(dir, '*.pas;*.pp;*.p;*.inc', false); //find e.g. all pascal sourcefiles
  try
    //ShowMessage(Format('Found %d Pascal source files', [PascalFiles.Count]));

    for pasfile in PascalFiles do
      begin
      Writeln(pasfile);
      replaceStringInFile(pasfile, findString, replaceString);
      end;

  finally
    PascalFiles.Free;
  end;
end;

procedure replaceUserStrings;
var es, us, rsid, oldstr, newstr: String;
  i: integer;
  resourceStrings: TStringList;
begin
  writeln('replaceUserStrings');

  // replace UserString(i) calls in all Pascal files
  for i:=1 to UserStringCount do
  begin
    es := EnglishStrings[i].englishStr;
    rsid := EnglishStrings[i].strResId;
    if es>'' then
    begin
      oldstr := 'UserString('+IntToStr(i)+')';
      newstr := rsid+' {UserString['+IntToStr(i)+']}';
      replaceStringInFiles('Forms', oldstr, newstr);
      replaceStringInFiles('Units', oldstr, newstr);
    end;
  end;

end;

procedure generateResourceStrings;
var es, us, rsid, oldstr, newstr: String;
  i,j: integer;
  resourceStrings: TStringList;
  dup: boolean;
begin
  writeln('replaceUserStrings');

  // create .inc file with resourcestrings
  resourceStrings := TStringList.create;
  resourceStrings.Add('// resourcestrings that replace legacy UserString calls');
  resourceStrings.Add('resourcestring');

  for i:=1 to UserStringCount do
  begin
    es := EnglishStrings[i].englishStr;
    rsid := EnglishStrings[i].strResId;
    if es>'' then
    begin
      resourceStrings.Add(' //UserString('+IntToStr(i)+')');
      dup := false;
      for j:=1 to i-1 do begin
        dup := EnglishStrings[j].strResId.ToLower = rsid.ToLower;
        if dup then break;
      end;
      if dup
      then resourceStrings.Add(' //'+rsid+' = '''+es.Replace('''','''''')+''';')
      else resourceStrings.Add('  '+rsid+' = '''+es.Replace('''','''''')+''';');
      resourceStrings.Add('');
    end;
  end;
  resourceStrings.Add('////////////////////');
  resourceStrings.SaveToFile('Units'+DirectorySeparator+'FreeResourceStrings.inc');
  resourceStrings.free;
end;

procedure generateUserStringsArray;
var es, us, rsid, sep: String;
  i,j: integer;
  resourceStrings: TStringList;
  dup: boolean;
begin
  writeln('generateUserStringsArray');

  // create .inc file with UserString as array of resourcestrings
  resourceStrings := TStringList.create;
  resourceStrings.Add('// resourcestrings that replace legacy UserString calls');
  resourceStrings.Add('');
  resourceStrings.Add('resourcestring rs_ = '''';');
  resourceStrings.Add('');
  resourceStrings.Add('const  UserStrings: array of String = (');

  for i:=1 to UserStringCount do
  begin
    es := EnglishStrings[i].englishStr;
    rsid := EnglishStrings[i].strResId;
    if i<UserStringCount then sep:=',' else sep:='';
    resourceStrings.Add('  {'+IntToStr(i).PadLeft(4)+'} '+rsid+sep);
  end;

  resourceStrings.Add(');');
  resourceStrings.Add('');
  resourceStrings.SaveToFile('Units'+DirectorySeparator+'FreeUserStringsArray.inc');
  resourceStrings.free;
end;


procedure convertUserStringsForLang(lang:string; ln:string; poFile:TPOFile);
var es, us, rsid, cmt: String;
  i: integer;
  CurrentItem: TPOFileItem;
begin
  writeln('convertUserStrings');
  for i:=1 to UserStringCount do
  begin
    es := EnglishStrings[i].englishStr;
    rsid := 'freestringsunit.'+EnglishStrings[i].strResId;
    if es>'' then
    begin
      us := UserString(i);
      cmt := 'UserString('+IntToStr(i)+')';
      CurrentItem := poFile.FindPoItem(rsid);
      if CurrentItem<>nil then
      begin
        CurrentItem.Translation := us;
        CurrentItem.Comments := cmt;
      end;
      poFile.FillItem(CurrentItem,
          {Identifier} rsid, {Original} es, {Translation} us,
          {Comments} cmt,
          {Context} '', {Flags} '', {PreviousID} '');
    end;
  end;
end;

procedure convertUserStrings;
var sr, lang, ln, poFileName: string;
  l: integer;
  poFile: TPOFile;
begin
  for l:=0 to length(languages)-1 do
  begin
    lang := languages[l];
    ln := langs[l];
    poFileName := 'locale'+DirectorySeparator+'FreeShip.'+ln+'.po';
    if FileExists(poFileName) then
      poFile := TPOFile.Create(poFileName, true)
    else
      poFile := TPOFile.Create('locale'+DirectorySeparator+'FreeShip.pot', true);

    FreeLanguageSupport.LoadLanguage(lang,'Languages/'+lang+'.ini');

    convertUserStringsForLang(lang, ln, poFile);

    poFile.SaveToFile(poFileName);

    //FreeLanguageSupport.CurrentLanguage.Free;
    poFile.Free;
  end;
end;


var EnglishMap: specialize TFPGMap<String, String>;


procedure convertGUIforLang(lang:string; ln:string; poFile:TPOFile);
var es, tl, rsid, sec: String;
  i: integer;
  Sections, SectionKeys: TStrings;
  CurrentItem: TPOFileItem;
begin
  writeln('convertUserStrings');
  Sections := TStringList.Create;
  SectionKeys := TStringList.Create;

  CurrentLanguage.ReadSections(Sections);

  for sec in Sections do
  begin
    SectionKeys.Clear;
    CurrentLanguage.ReadSection(sec, SectionKeys);
    for rsid in SectionKeys do
    if EnglishMap.IndexOf(rsid)>=0 then
    begin
      es := EnglishMap[rsid];
      if es>'' then
      begin
        tl := CurrentLanguage.ReadString(sec,rsid,''); ;
        CurrentItem := poFile.FindPoItem(rsid);
        if CurrentItem <> nil then
        begin
          CurrentItem.Translation := tl;
          poFile.FillItem(CurrentItem,
            {Identifier} rsid, {Original} es, {Translation} tl,
            {Comments} '',
            {Context} '', {Flags} '', {PreviousID} '');
        end;
      end;
    end;

  end;
  SectionKeys.Free;
  Sections.Free;
end;


procedure LoadEnglishGUI;
var es, us, rsid, sec, val: String;
  i: integer;
  Sections, SectionKeys, SectionValues: TStrings;
  CurrentItem: TPOFileItem;
begin
  writeln('LoadEnglishGUI');
  EnglishMap := specialize TFPGMap<String,String>.Create;
  FreeLanguageSupport.LoadLanguage('English','Languages/English.ini');
  Sections := TStringList.Create;
  SectionKeys := TStringList.Create;
  SectionValues := TStringList.Create;
  CurrentLanguage.ReadSections(Sections);
  for sec in Sections do
  begin
    SectionKeys.Clear;
    CurrentLanguage.ReadSection(sec, SectionKeys);
    for rsid in SectionKeys do
    begin
      val := CurrentLanguage.ReadString(sec,rsid,'');
      EnglishMap[rsid] := val;
    end;
  end;
  SectionValues.Free;
  Sections.Free;
end;


procedure convertGUI;
var sr, lang, ln, poFileName: string;
  l: integer;
  poFile: TPOFile;
begin
  loadEnglishGUI;

  for l:=0 to length(languages)-1 do
  begin
    lang := languages[l];
    ln := langs[l];
    poFileName := 'locale'+DirectorySeparator+'FreeShip.'+ln+'.po';
    if FileExists(poFileName) then
      poFile := TPOFile.Create(poFileName, true)
    else
      poFile := TPOFile.Create('locale'+DirectorySeparator+'FreeShip.pot', true);

    FreeLanguageSupport.LoadLanguage(lang,'Languages/'+lang+'.ini');

    convertGUIforLang(lang, ln, poFile);

    poFile.SaveToFile(poFileName);

    //FreeLanguageSupport.CurrentLanguage.Free;
    poFile.Free;
  end;
end;


procedure loadEnglishUserStrings;
  var us: String;
    i: integer;
  begin
    writeln('loadEnglishUserStrings');
    for i:=1 to UserStringCount do
      begin
      us := UserString(i);
      EnglishStrings[i].strResId := genSRname(us);
      EnglishStrings[i].englishStr := UserString(i);
      end;
  end;

procedure runConversion;
  var lang: String;
begin
  EnglishLanguageIniFile :=  FreeLanguageSupport.LoadLanguage('English','Languages/English.ini');
  loadEnglishUserStrings; // load English UserStrings from Languages/English.ini
  convertUserStrings;  // create/update PO files from Languages/*.ini
  //generateResourceStrings; // creates FreeResourceStrings.inc
  //generateUserStringsArray; // creates FreeUserStringsArray.inc
  //replaceUserStrings; // !!! replaces legacy UserString(N) calls with ResourceStrings in Forms/ and Units/ folders
  convertGUI;
  writeln('Done!');
end;

end.

