{---------------------------------------------------------------------------------------------------}
{                                       TFreePreferences                                            }
{   Container class for all program settings                                                        }
{---------------------------------------------------------------------------------------------------}
{ Due to migration into multiplatform environment config files and directories will be stored
in diffrent locations for different config areas.

  For per-machine and per-user installation in Linux all pathes are predefined. See below.

  Per-machine installation in Windows is done into so called FreeShipHome
  that by default is CIDL_PROGRAM_FILES\FreeShip

  Per-user installation in Windows is done into so called FreeShipHome
  that is always CIDL_LOCAL_APPDATA\Programs\FreeShip

  ref: https://docs.microsoft.com/en-us/windows/win32/msi/single-package-authoring

  Global Config: the default config for all users. Users have read-only privileges.
    In Linux: /etc/FreeShip/FreeShip.ini
    In Windows: [FreeShipHome]\FreeShip.ini

  Because per-machine installation in Windows can be other than CIDL_PROGRAM_FILES
  Global Config is found from the app start location.

  User Config: config for current user. User have full privileges.
    In Linux: $HOME/.config/FreeShip/FreeShip.ini
    In Windows: [FreeShipHome]\FreeShip.ini

  First is loaded Global Config, then User Config that may override all or some settings.
  Global and User config is same file for per-User installation.

  Executables:
    In Linux: /usr/local/bin/FreeShip
    In Windows: [FreeShipHome]\FreeShip.exe

  External Executables:
    In Linux: /usr/local/bin/FreeShip/Exec/*
    In Windows: [FreeShipHome]\Exec\*.exe

  All data and resource files (such as Languages, Manuals) can be stored globally
  and locally if you want customization
  Globally:
    In Linux: /usr/share/FreeShip/
    In Windows: defined by [FreeShipHome]\

  Locally:
    In Linux: $HOME/FreeShip/
    In Windows: defined by [FreeShipHome]\

  Note: Paths from Config have piority over default paths.
  Default Local config and data paths have piority over global ones.

  Application defines default paths following way:
  1. Try to get from local config, if exists
  2. Try to get from global config, if exists
  3. Try to get from default local location, if exists
  4. Try to get from default global location, if exists
  5. Get from the app start location. Deprecated. Left for compatibility with FreeShip for Windows

}

function TFreePreferences.FGetExportDirectory: string;
begin
  if DirectoryExistsUTF8(FExportDirectory) { *Converted from DirectoryExists* } then
    Result := FExportDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getUserDataDirectory + DirectorySeparator+'Export';
end;{TFreePreferences.FGetExportDirectory}

function TFreePreferences.FGetImportDirectory: string;
begin
  if DirectoryExistsUTF8(FImportDirectory) { *Converted from DirectoryExists* } then
    Result := FImportDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getUserDataDirectory + DirectorySeparator+'Import';
end;{TFreePreferences.FGetImportDirectory}

function TFreePreferences.FGetGlobalImportDirectory: string;
begin
  if DirectoryExistsUTF8(FGlobalImportDirectory)
  { *Converted from DirectoryExists* } then
    Result := FGlobalImportDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getGlobalAppDataDirectory + DirectorySeparator+'Import';
end;{TFreePreferences.FGetGlobalImportDirectory}

function TFreePreferences.FGetLastDirectory: string;
begin
  if DirectoryExistsUTF8(FLastDirectory) then
    Result := FLastDirectory
  else
    Result := self.getUserDataDirectory + DirectorySeparator+'Ships';
end;{TFreePreferences.FGetOpenDirectory}

function TFreePreferences.FGetOpenDirectory: string;
begin
  if DirectoryExistsUTF8(FOpenDirectory) { *Converted from DirectoryExists* } then
    Result := FOpenDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getUserDataDirectory + DirectorySeparator+'Ships';
end;{TFreePreferences.FGetOpenDirectory}

procedure TFreePreferences.FSetOpenDirectory(val: string);
begin
  FOpenDirectory := val;
end;{TFreePreferences.FSetOpenDirectory}

procedure TFreePreferences.FSetLastDirectory(val: string);
begin
  FLastDirectory := val;
end;{TFreePreferences.FSetOpenDirectory}

function TFreePreferences.FGetGlobalOpenDirectory: string;
begin
  if DirectoryExistsUTF8(FGlobalOpenDirectory)
  { *Converted from DirectoryExists* } then
    Result := FGlobalOpenDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getGlobalAppDataDirectory + DirectorySeparator+'Ships';
end;{TFreePreferences.FGetGlobalOpenDirectory}

function TFreePreferences.FGetSaveDirectory: string;
begin
  if DirectoryExistsUTF8(FSaveDirectory) { *Converted from DirectoryExists* } then
    Result := FSaveDirectory
  //else Result:=ExtractFilePath(Application.ExeName);
  else
    Result := self.getUserDataDirectory + DirectorySeparator+'Ships';
end;{TFreePreferences.FGetSaveDirectory}

function TFreePreferences.FGetInitDirectory: string;
begin
  if DirectoryExistsUTF8(FInitDirectory) { *Converted from DirectoryExists* } then
    Result := FInitDirectory
  else
    Result := ExtractFilePath(Application.ExeName);
end;{TFreePreferences.FGetInitDirectory}

procedure TFreePreferences.FSetViewportColor(Val: TColor);
var
  I: integer;
begin
  FViewportColor := Val;
  for I := 1 to Owner.NumberOfViewports do
    Owner.Viewport[I - 1].Color := FViewportColor;
end;{TFreePreferences.FSetViewportColor}

procedure TFreePreferences.Clear;
begin
  ResetColors;
  FPointSize := 2;
  FLastDirectory := '';
  FOpenDirectory := '';
  FGlobalOpenDirectory := '';
  FSaveDirectory := '';
  FImportDirectory := '';
  FGlobalImportDirectory := '';
  FExportDirectory := '';
  FExecDirectory := '';
  FManualsDirectory := '';
  FTempDirectory := '';
  FLanguage := 'English';
  FLanguageFile := '';
  FMaxUndoMemory := 20;// Max 20Mb undomemory
  FFbmEncoding := 'cp1252';
end;{TFreePreferences.Clear}

constructor TFreePreferences.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := Owner;
  Clear;
end;{TFreePreferences.Create}

procedure TFreePreferences.Edit;
var
  Dialog: TFreePreferencesDialog;
  Lang: string;
  I: integer;
  Dir: string;
  StrList: TStringList;
const DS = DirectorySeparator;

  procedure Browse(Dir: string);
  var
    SearchRec: TSearchRec;

    procedure Add(FileName: string);
    var
      Tmp: string;
      I: integer;
      Found: boolean;
    begin
      //Tmp := ChangeFileExt(ExtractFilename(Filename), '');
      Tmp := Filename.Split('.')[1];
      Tmp := LanguageName(Tmp);
      if Tmp = '' then exit;
      found := False;
      for I := 1 to Dialog.ComboBox1.Items.Count do
        if Uppercase(Tmp) = Uppercase(Dialog.Combobox1.Items[I - 1]) then
        begin
          Found := True;
          break;
        end;
      if not found then
        Dialog.Combobox1.Items.Add(Tmp);
    end;{Add}

  begin
    if Dir[Length(Dir)] <> DirectorySeparator then
      Dir := Dir + DirectorySeparator;
    if FindFirstUTF8(Dir + '*.po', faAnyfile, SearchRec)
      { *Converted from FindFirst* } = 0 then
    begin
      if Uppercase(ExtractFileExt(SearchRec.Name)) = '.PO' then
        Add(Searchrec.Name);
      while FindNextUTF8(SearchRec) { *Converted from FindNext* } = 0 do
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          if Uppercase(ExtractFileExt(SearchRec.Name)) = '.PO' then
            Add(Searchrec.Name);
      FindCloseUTF8(SearchRec); { *Converted from FindClose* }
    end;
  end;{Browse}

begin
  Dialog := TFreePreferencesDialog.Create(Owner);
  //ShowTranslatedValues(Dialog);
  //Dir:=ExtractFileDir(Application.ExeName)+'/Languages/';
  Dir := FLanguagesDirectory;
  Dialog.ComboBox1.Items.Clear;
  Dialog.ComboBox1.Items.Add('English');
  Browse(Dir);
  Dialog.ComboBox1.ItemIndex := 0;
  for I := 1 to Dialog.ComboBox1.Items.Count do
    if Uppercase(Dialog.ComboBox1.Items[I - 1]) = Uppercase(FLanguage) then
    begin
      Dialog.ComboBox1.ItemIndex := I - 1;
      break;
    end;

  Dialog.ComboBoxThemes.Text := FThemeName;
  StrList := TStringList.Create;
  StrList.Sorted := True;
  StrList.Duplicates := dupIgnore;

  getAllThemes(StrList);

  Dialog.ComboBoxThemes.Items.Clear;
  Dialog.ComboBoxThemes.Items.AddStrings(StrList);
  StrList.Free;

  if Dialog.Execute(Owner) then
  begin
    PointSize := Dialog.SpinEdit1.Value;
    FontSize := Dialog.seFontSize.Value;
    LayerColor := Dialog.Panel2.Color;
    UnderWaterColor := Dialog.Panel5.Color;
    UnderWaterColorAlpha := Dialog.seSubmergedSurfaceOpacity.Value * 255 div 100;
    EdgeColor := Dialog.Panel6.Color;
    CreaseEdgeColor := Dialog.Panel7.Color;
    CreaseColor := Dialog.Panel8.Color;
    RegularPointColor := Dialog.Panel9.Color;
    CreasePointColor := Dialog.Panel10.Color;
    CornerPointColor := Dialog.Panel11.Color;
    DartPointColor := Dialog.Panel12.Color;
    SelectColor := Dialog.Panel13.Color;
    GridColor := Dialog.Panel14.Color;
    GridFontColor := Dialog.Panel15.Color;
    StationColor := Dialog.Panel16.Color;
    ButtockColor := Dialog.Panel17.Color;
    WaterlineColor := Dialog.Panel18.Color;
    NormalColor := Dialog.Panel19.Color;
    DiagonalColor := Dialog.Panel20.Color;
    LeakPointColor := Dialog.Panel21.Color;
    MarkerColor := Dialog.Panel22.Color;
    CurvaturePlotColor := Dialog.Panel23.Color;
    ControlCurveColor := Dialog.Panel24.Color;
    HydrostaticsFontColor := Dialog.Panel25.Color;
    ZebraStripeColor := Dialog.Panel26.Color;
    ViewportColor := Dialog.Panel4.Color;
    // Set viewportcolor last, because it forces a repaint
    Lang := Dialog.Combobox1.Text;

    FLanguagesDirectory := Dialog.EditLanguagesDir.Text;
    FManualsDirectory := Dialog.EditManualsDir.Text;
    FExecDirectory := Dialog.EditExecDir.Text;
    FTempDirectory := Dialog.EditTempDir.Text;
    FOpenDirectory := Dialog.EditOpenDir.Text;
    FSaveDirectory := Dialog.EditSaveDir.Text;
    FImportDirectory := Dialog.EditImportDir.Text;
    FExportDirectory := Dialog.EditExportDir.Text;
    //FToolIconDirectory:=Dialog.EditToolIconsDir.Text;
    FToolIconSize := StrToInt(Dialog.SelectToolIconSize.Text);
    FGlobalOpenDirectory := Dialog.EditGlobalOpenDir.Text;
    FGlobalImportDirectory := Dialog.EditGlobalImportDir.Text;
    FThemeName := Dialog.ComboBoxThemes.Text;

    if Uppercase(Lang) <> Uppercase(FLanguage) then
      if FileExistsUTF8(FLanguagesDirectory + DS + 'FreeShip.'+LanguageCode(Lang) + '.po')
      then
      begin
        (*LoadLanguage(Lang, FLanguagesDirectory + DS + Lang + '.ini');
        FLanguageFile := FLanguagesDirectory + DS + Lang + '.ini';*)
        SetDefaultLang(LanguageCode(Lang), FLanguagesDirectory, 'FreeShip', true);
        FLanguage := Lang;
        (*
        for I := 1 to Application.ComponentCount do
          if Application.Components[I - 1] is TCustomForm then
            //ShowTranslatedValues(Application.Components[I - 1]);
        for I := 1 to MainForm.MDIChildCount do
          if MainForm.MDIChildren[I - 1] is TFreeLinesplanForm then
            //ShowTranslatedValues(TFreeLinesplanForm(
              MainForm.MDIChildren[I - 1]).LinesplanFrame)
          else
            //ShowTranslatedValues(MainForm.MDIChildren[I - 1]);
         *)
      end//else FLanguage:=Lang;
    ;

    FFbmEncoding := string(Dialog.ComboBoxEncoding.Items.Objects[
      Dialog.ComboBoxEncoding.ItemIndex]);
    FMaxUndoMemory := Dialog.FreeNumInput1.Value;

    if assigned(Owner.FOnFileChanged) then
      Owner.FOnFileChanged(Owner);
    if assigned(Owner.FOnUpdateUndoData) then
      Owner.FOnUpdateUndoData(Owner);
    if assigned(Owner.FOnUpdateRecentFileList) then
      Owner.FOnUpdateRecentFileList(Owner);
    if assigned(Owner.FOnChangeCursorIncrement) then
      Owner.FOnChangeCursorIncrement(Owner);
    if assigned(Owner.FOnUpdateGeometryInfo) then
      Owner.FOnUpdateGeometryInfo(owner);
    Owner.Redraw;

    if Dialog.IsThemeChanged then
      if IsThemeCustom(Dialog.ComboBoxThemes.Text) then
        begin
        SaveCustomTheme;
        end
      else
        SaveThemeAsCustom(Dialog);

    if Dialog.IsConfigChanged then
      Save;

  end;
  FreeAndNil(Dialog);
end;{TFreePreferences.Edit}



procedure TFreePreferences.SaveCustomTheme;
begin
  SaveTheme(FThemeName, FParentThemeName);
end;

procedure TFreePreferences.SaveThemeAsCustom(Dialog: TForm);
var
  d: TFreePreferencesDialog;
  t: TEnterThemeNameDlg;
  r: integer;
begin
  d := TFreePreferencesDialog(Dialog);
  t := TEnterThemeNameDlg.Create(d.Owner);
  t.EditCustomSchemeName.Text := 'New Custom Theme';
  t.Message.Caption := 'Some properties of current scheme "' +
    d.ComboBoxThemes.Text + '" were changed.' +
    ' Please enter a name to save it as a new custom scheme.';
  r := t.ShowModal;
  if r = mrOk then
  begin
    FParentThemeName := FThemeName;
    FThemeName := t.EditCustomSchemeName.Text;
    SaveTheme(FThemeName, FParentThemeName);
    Save;
  end;
  t.Free;
end;

procedure TFreePreferences.detectApplicationScope;
var appPath:string;
begin
  appPath := ExtractFilePath(Application.Exename);
  {$ifdef UNIX}
  if appPath.StartsWith(GetUserDir)
  then FApplicationScope := asUser
  else FApplicationScope := asMachine;
  {$endif}
  {$ifdef Windows}
  if appPath.StartsWith(windirs.GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA))
     or appPath.StartsWith(windirs.GetWindowsSpecialDir(CSIDL_PROFILE))
  then FApplicationScope := asUser
  else FApplicationScope := asMachine;
  {$endif}
end;

function TFreePreferences.getGlobalConfigDirectory: string;
var
  D,GACD,LACD,UD,TD: string;
begin
  GACD:=GetAppConfigDir(true);
  LACD:=GetAppConfigDir(false);
  UD:=GetUserDir;
  TD:=GetTempDir;
  {$ifdef UNIX}
  if FApplicationScope = asUser then
  begin
    Result := GetUserDir + '.local/share/FreeShip';
  end
  else
  begin
    Result := GetAppConfigDir(true);
  end;
  {$endif}
  {$ifdef Windows}
  if FApplicationScope = asUser then
  begin
    Result := windirs.GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA)+'Programs\FreeShip';
  end
  else
  begin
    Result := GetAppConfigDir(true);
    Result := windirs.GetWindowsSpecialDir(CSIDL_PROGRAM_FILES)+'FreeShip';
  end;
  {$endif}
end;

function TFreePreferences.getUserConfigDirectory: string;
var
  D: string;
begin
  Result := ExcludeTrailingPathDelimiter(GetAppConfigDir(False));
end;

function TFreePreferences.getGlobalAppDataDirectory: string;
begin
  {$ifdef UNIX}
  if FApplicationScope = asUser then
  begin
    Result := GetUserDir + '.local/share/FreeShip';
  end
  else
  begin
    Result := '/usr/share/FreeShip';
  end;
  {$endif}
  {$ifdef Windows}
  if FApplicationScope = asUser then
  begin
    Result := windirs.GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA)+'Programs\FreeShip';
  end
  else
  begin
    Result := windirs.GetWindowsSpecialDir(CSIDL_PROGRAM_FILES)+'FreeShip';
  end;
  {$endif}
end;

function TFreePreferences.getUserDataDirectory: string;
begin
  {$ifdef UNIX}
  //according to /etc/xdg/user-dirs.defaults
  Result := SysUtils.GetEnvironmentVariable('HOME') + '/Documents/FreeShip';
  {$endif}

  {$ifdef Windows}
  Result := windirs.GetWindowsSpecialDir(CSIDL_PERSONAL) + 'FreeShip';
  {$endif}
end;

function TFreePreferences.getUserAppDataDirectory: string;
begin
  Result := getGlobalAppDataDirectory;
end;


function TFreePreferences.getTempDirectory: string;
begin
  {$ifdef UNIX}
  // we will use XDG_RUNTIME_DIR first
  Result:=GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if DirectoryExists(Result) then
     Result := ExcludeTrailingPathDelimiter(Result)+'/FreeShip'
  else
  begin
    Result := GetTempDir(False);
    if Result = '' then Result:=GetEnvironmentVariable('TMPDIR'); //POSIX
    if Result = '' then Result:=GetEnvironmentVariable('TEMP');
    if Result = '' then Result:=GetEnvironmentVariable('TMP');
    if Result = '' then Result := '/tmp';
    Result := ExcludeTrailingPathDelimiter(Result)+'/'+GetEnvironmentVariable('USER')+'/FreeShip';
  end;
  {$endif}
  {$ifdef Windows}
  Result := SysUtils.GetEnvironmentVariable('TEMP') + '\FreeShip';
  if Result = '' then
     Result := GetTempDir;
     //Result := windirs.GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA)+'Temp\FreeShip';
  {$endif}
end;


procedure TFreePreferences.LoadFromDta(Filename: string);
var
  FFile: TextFile;
  I, N: integer;
  T, L, W, H, S: integer;
begin
  //Filename:=ChangeFileExt(Application.ExeName,'.dta');
  if FileExists(Filename) then
  begin
    AssignFile(FFile, Filename);
    try
      Clear;
      Reset(FFile);
      Readln(FFile, FPointSize);
      Readln(FFile, FButtockColor);
      Readln(FFile, FWaterlineColor);
      Readln(FFile, FStationColor);
      Readln(FFile, FCreaseColor);
      Readln(FFile, FCreaseEdgeColor);
      Readln(FFile, FGridColor);
      Readln(FFile, FGridFontColor);
      Readln(FFile, FEdgeColor);
      Readln(FFile, FCreasePointColor);
      Readln(FFile, FRegularPointColor);
      Readln(FFile, FCornerPointColor);
      Readln(FFile, FDartPointColor);
      Readln(FFile, FSelectColor);
      Readln(FFile, FLayerColor);
      Readln(FFile, FUnderWaterColor);
      Readln(FFile, FNormalColor);
      Readln(FFile, FViewportColor);
      if not EOF(FFile) then
        readln(FFile, FOpenDirectory);
      if not EOF(FFile) then
        readln(FFile, FSaveDirectory);
      if not EOF(FFile) then
        readln(FFile, FImportDirectory);
      if not EOF(FFile) then
        readln(FFile, FExportDirectory);
      if not EOF(FFile) then
        readln(FFile, FDiagonalColor);
      if not EOF(FFile) then
      begin
        // load recent files
        Readln(FFile, N);
        Owner.Edit.FRecentFiles.Clear;
        Owner.Edit.FRecentFiles.Capacity := N;
        for I := 1 to N do
        begin
          Readln(FFile, Filename);
          // only add the file to the list if it is a valid filename
          //if FileExists(Filename+'.fbm') then
          Owner.Edit.FRecentFiles.Add(Filename);
        end;
        if assigned(Owner.FOnUpdateRecentFileList) then
          Owner.FOnUpdateRecentFileList(self);
      end;
      if not EOF(FFile) then
        Readln(FFile, FLeakPointColor);
      if not EOF(FFile) then
        readln(FFile, FMarkerColor);
      if not EOF(FFile) then
        Readln(FFile, FCurvaturePlotColor);
      if not EOF(FFile) then
        Readln(FFile, FControlCurveColor);
      if not EOF(FFile) then
        readln(FFile, FHydrostaticsFontColor);
      if not EOF(FFile) then
        readln(FFile, FZebraStripeColor);
      if not EOF(FFile) then
      begin
        Readln(FFile, T, L, H, W, S);
        if MainForm <> nil then
        begin
          if L > Screen.Width then
            L := 0;
          if T > Screen.Height then
            T := 0;
          case TWindowState(S) of
            wsNormal: MainForm.SetBounds(L, T, W, H);
            wsMinimized:
            begin
              MainForm.WindowState := wsNormal;
              MainForm.SetBounds(L, T, W, H);
            end;
            wsMaximized: MainForm.WindowState := wsMaximized;
          end;
        end;
      end;
      if not EOF(FFile) then
        Readln(FFile, FLanguage)
      else
        FLanguage := 'English';
      FLanguageFile := ExtractFilePath(Application.Exename) +
        'Languages'+DirectorySeparator+ Flanguage + '.ini';
      if not FileExists(FLanguageFile) then
      begin
        FLanguage := 'English';
        FLanguageFile := ExtractFilePath(Application.Exename) +
          'Languages'+DirectorySeparator + FLanguage + '.ini';
      end;
      if not EOF(FFile) then
        Readln(FFile, FMaxUndoMemory);
      CloseFile(FFile);
    except
      MessageDlg(rs_Could_not_load_program_preferences_from_the_following_location {UserString[176]} + ':' + EOL + Filename, mtError, [mbOK], 0);
    end;
  end;
end;{TFreePreferences.LoadFromDta}

// Only schemes that are saved in User Config are custom
function TFreePreferences.IsThemeCustom(ThemeName: string): boolean;
begin
  Result := FileExistsUTF8(Self.FUserConfigDirectory + DirectorySeparator+'Themes' +
    DirectorySeparator + ThemeName + DirectorySeparator+'theme.ini');
end;

function TFreePreferences.getThemeConfigFile(ThemeName: string): string;
const DS=DirectorySeparator;
var themePath:string;
begin
  themePath := DS+'Themes'+DS+ThemeName+DS+'theme.ini';
  if FileExistsUTF8(Self.FUserConfigDirectory + themePath) then
    Result := Self.FUserConfigDirectory + themePath
  else
  if FileExistsUTF8(Self.FGlobalConfigDirectory + themePath) then
    Result := Self.FGlobalConfigDirectory + themePath
  else
  if FileExistsUTF8(Self.FUserAppDataDirectory + themePath) then
    Result := Self.FUserAppDataDirectory + themePath
  else
  if FileExistsUTF8(Self.FGlobalAppDataDirectory + themePath) then
    Result := Self.FGlobalAppDataDirectory + themePath
  else
    Result := Self.FGlobalAppDataDirectory + themePath;
end;

procedure TFreePreferences.getAllThemes(ss: TStrings);
const DS=DirectorySeparator;
begin
  ss.Clear;
  getThemesInDir(Self.FUserConfigDirectory + DS+'Themes', ss);
  getThemesInDir(Self.FGlobalConfigDirectory + DS+'Themes', ss);
  getThemesInDir(Self.FUserAppDataDirectory + DS+'Themes', ss);
  getThemesInDir(Self.FGlobalAppDataDirectory + DS+'Themes', ss);
end;

procedure TFreePreferences.getThemesInDir(dir: string; ss: TStrings);
var
  sr: TSearchRec;
begin
  if not DirectoryExists(dir) then
    exit;
  if FindFirstUTF8(dir + DirectorySeparator+'*', faDirectory, sr) = 0 then
    repeat
      if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and
        (sr.Name <> '..') then
        ss.Add(sr.Name);
    until FindNextUTF8(sr) <> 0;
  FindCloseUTF8(sr);
end;


function TFreePreferences.getParentThemeName(ThemeName: string): string;
var
  params: TColorIniFile;
begin
  params := TColorIniFile.Create(getThemeConfigFile(FThemeName));
  Result := params.ReadString('Theme', 'ParentTheme', '');
  params.Free;
end;

{
function TFreePreferences.GetToolIconDirectory: string;
var S:string;
const DS=DirectorySeparator;
begin
 S:=DS+'Themes'+DS+FThemeName+DS+'icons'+DS+IntToStr(FToolIconSize);
 if DirectoryExistsUTF8(FUserAppDataDirectory + S) then
  result := FUserAppDataDirectory + S
  else result := FGlobalAppDataDirectory + S;
end;
}

// finds IconFileName recursively from the current theme and its parents
function TFreePreferences.GetIconFileName(ThemeName, IconName: string;
  IconSize: integer): string;
var
  subPath, ptn: string;
const DS=DirectorySeparator;
begin
  Result:='';
  subPath := DS+'Themes'+DS+ThemeName+DS+'icons'+DS + IntToStr(IconSize) +
    DS + IconName + '.png';
  if FileExistsUTF8(GetCurrentDirUTF8 + subPath) then
    Result := GetCurrentDirUTF8 + subPath
  else
  if FileExistsUTF8(Self.FUserConfigDirectory + subPath) then
    Result := Self.FUserConfigDirectory + subPath
  else
  if FileExistsUTF8(Self.FGlobalConfigDirectory + subPath) then
    Result := Self.FGlobalConfigDirectory + subPath
  else
  if FileExistsUTF8(Self.FUserAppDataDirectory + subPath) then
    Result := Self.FUserAppDataDirectory + subPath
  else
  if FileExistsUTF8(Self.FGlobalAppDataDirectory + subPath) then
    Result := Self.FGlobalAppDataDirectory + subPath
  else
  begin
    ptn := getParentThemeName(ThemeName);
    if (ptn > '') and (ptn <> ThemeName) then
      Result := GetIconFileName(ptn, IconName, IconSize);
    //else  Result := '';
  end;
end;


{ this is used just once to dump menu/toolbutton icons }
procedure TFreePreferences.dumpIcons(ImageList: TImageList; ActionList: TActionList);
const
  transpix: TRGBQuad = (rgbBlue: $FF; rgbGreen: $99; rgbRed: $33; rgbReserved: $00);
var
  i, II, sz, ilcnt, x, y: integer;
  A: TAction;
  AName, IName, IPath: string;
  cil: TCustomImageList;
  it: TImageType;
  bmp, bmp2: TBitmap;
  bkcl: TColor;
  png: TPortableNetworkGraphic;
  img: TLazIntfImage;
  rimg: TRawImage;
  //cimg: TFPCustomImage;
  aFlags: TRawImageQueryFlags;
  tb: TToolButton;
  ico: TIcon;
  ppix: PRGBQuad;
  pix: TRGBQuad;
  col, txcol: TFPColor;
  pngWriter: TLazWriterPNG;
begin
  sz := ImageList.Width;
  cil := TCustomImageList(ImageList);
  ilcnt := cil.Count;
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.RawImage.Description.AlphaPrec.Parse('8');
  bmp.Transparent := True;
  bmp.SetSize(sz, sz);
  {
  bmp2:=TBitmap.Create;

  png:=TPortableNetworkGraphic.Create;
  png.PixelFormat:=pf32bit;
  png.SetSize(sz,sz);

  img:=TLazIntfImage.Create(sz,sz);
  img.DataDescription.BitsPerPixel:=32;
  img.DataDescription.AlphaPrec:=8;
  img.DataDescription.Format:=ricfRGBA;
  //cimg:= TImage.Create(nil);
  //cimg.Picture.Bitmap:=bmp;
  }
  pngWriter := TLazWriterPNG.Create;
  pngWriter.UseAlpha := True;


  for i := 0 to ActionList.ActionCount - 1 do
  begin
    A := TAction(ActionList.Actions[i]);
    AName := A.Name;
    II := A.ImageIndex;
    if (II > -1) and (II < ilcnt) then
    begin
      IPath := 'icons' + DirectorySeparator + IntToStr(sz) + DirectorySeparator + AName;
      //cil.GetBitmap(II, bmp);
      //bmp.LoadFromIntfImage(img);
      bmp.Canvas.Brush.Color :=
        RGB(transpix.rgbRed, transpix.rgbGreen, transpix.rgbBlue);
      bmp.Canvas.FillRect(0, 0, sz, sz);
      cil.Draw(bmp.Canvas, 0, 0, II, dsTransparent, itImage);
      {
      for y:=bmp.Height-1 downto 0 do
      begin
        ppix:=bmp.ScanLine[y];
        for x:=bmp.Width-1 downto 0 do
        begin
          if TColor(ppix^) = TColor(transpix)
          then
            ppix.rgbReserved:=$00
          else
            ppix.rgbReserved:=$FF;
          inc(ppix);
        end;
      end;
      }

      img := bmp.CreateIntfImage;
      txcol := TColorToFPColor(RGB(transpix.rgbRed, transpix.rgbGreen,
        transpix.rgbBlue));
      for y := img.Height - 1 downto 0 do
        for x := img.Width - 1 downto 0 do
        begin
          col := img.Colors[x, y];
          if (col.Blue = txcol.Blue) and (col.Green = txcol.Green) and
            (col.Red = txcol.Red) then
            col.alpha := $0000
          else
            col.alpha := $FFFF;
          //col.Red:=0; col.Green:=0; col.Blue:=0;
          img.Colors[x, y] := col;
        end//ppix:=img.GetDataLineStart(y);
      ;

      img.SaveToFile(IPath + '.png', pngWriter);
    end;
  end;
  bmp.Free;
  img.Free;
  pngWriter.Free;
end;


// loads icons from FMenuIconDirectory that is set according to theme and icon size
procedure TFreePreferences.LoadImageListByActions(ImageList: TImageList;
  ActionList: TActionList);
var
  i, II, sz, ilcnt: integer;
  A: TAction;
  AName, IName, IPath, IconFile: string;
  cil: TCustomImageList;
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  img: TLazIntfImage;
begin
  sz := ToolIconSize;
  cil := TCustomImageList(ImageList);
  ilcnt := cil.Count;
  ImageList.Width := sz;
  ImageList.Height := sz;
  IPath := ToolIconDirectory;
  bmp := TBitmap.Create;
  bmp.SetSize(sz, sz);
  png := TPortableNetworkGraphic.Create;
  //img:= TLazIntfImage.Create(sz,sz);

  // insert empties, just for count
  //for i:=0 to ActionList.ActionCount-1 do
  //  if TAction(ActionList.Actions[i]).ImageIndex > -1 then
  //    cil.Add(bmp,nil);

  for i := 0 to ilcnt - 1 do
    cil.Add(bmp, nil);
  ilcnt := cil.Count;

  for i := 0 to ActionList.ActionCount - 1 do
  begin
    A := TAction(ActionList.Actions[i]);
    AName := A.Name;
    if Aname = 'LayerVisibilityDialog' then
      II:=0;
    II := A.ImageIndex;
    if (II > -1) then
    begin
      IconFile := GetIconFileName(Theme, AName, ToolIconSize);
      if (IconFile > '') and FileExistsUTF8(IconFile) then
      begin
        ilcnt:=cil.Count;
        while cil.Count<=II do
          cil.Add(bmp, nil);
        ilcnt:=cil.Count;
        if (II < cil.Count) then
        begin
          png.LoadFromFile(IconFile);
          img := png.CreateIntfImage;
          //img.LoadFromFile(IPath+DirectorySeparator+AName+'.png');
          bmp.LoadFromIntfImage(img);
          cil.Replace(II, bmp, nil);
          img.Free;
        end;
      end;
    end;
  end;
  bmp.Free;
  png.Free;
end;

// loads icon into Bitmap that is set according to theme and icon size
procedure TFreePreferences.LoadImageIntoBitmap(Bitmap: TBitmap; Name: string);
var
  sz: integer;
  IName, IPath, IconFile: string;
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  img: TLazIntfImage;
begin
  sz := ToolIconSize;
  IPath := ToolIconDirectory;
  bmp := TBitmap.Create;
  bmp.SetSize(sz, sz);
  png := TPortableNetworkGraphic.Create;
  img := TLazIntfImage.Create(sz, sz);

  IconFile := GetIconFileName(Theme, Name, ToolIconSize);

  if (IconFile > '') and FileExistsUTF8(IconFile) then
  begin
    png.LoadFromFile(IconFile);
    img := png.CreateIntfImage;
    bmp.LoadFromIntfImage(img);
    Bitmap.FreeImage;
    Bitmap.Assign(bmp);
    img.Free;
  end;
  bmp.Free;
  png.Free;
end;

// loads icons from FMenuIconDirectory that is set according to theme and icon size
procedure TFreePreferences.LoadImageIntoList(ImageList: TImageList;
  Item: integer; Name: string);
var
  i, II, sz, ilcnt: integer;
  A: TAction;
  IName, IPath, IconFile: string;
  cil: TCustomImageList;
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  img: TLazIntfImage;
begin
  sz := ToolIconSize;
  ImageList.Width := sz;
  ImageList.Height := sz;
  IPath := ToolIconDirectory;
  cil := TCustomImageList(ImageList);
  ilcnt := cil.Count;
  bmp := TBitmap.Create;
  bmp.SetSize(sz, sz);
  png := TPortableNetworkGraphic.Create;
  img := TLazIntfImage.Create(sz, sz);

  IconFile := GetIconFileName(Theme, Name, ToolIconSize);

  if (Item > ImageList.Count - 1) then
    for i := ImageList.Count to Item do
      cil.Add(bmp, nil);

  if (IconFile > '') and FileExistsUTF8(IconFile) then
  begin
    png.LoadFromFile(IconFile);
    img := png.CreateIntfImage;
    bmp.LoadFromIntfImage(img);
    cil.Replace(Item, bmp, nil);
    img.Free;
  end;
  bmp.Free;
  png.Free;
end;

procedure TFreePreferences.LoadTheme(ThemeName: string);
var
  IniFile, ParentIniFile, ParentThemeName: string;
  params: TColorIniFile;
begin
  IniFile := self.getThemeConfigFile(ThemeName);
  params := TColorIniFile.Create(IniFile, False);
  ParentThemeName := params.ReadString('Theme', 'ParentTheme', FParentThemeName);
  params.Free;
  ParentIniFile := self.getThemeConfigFile(ParentThemeName);
  LoadThemeIni(ParentIniFile);
  LoadThemeIni(IniFile);
end;

procedure TFreePreferences.LoadThemeIni(FileName: string);
var
  params: TColorIniFile;
  //RecentFileNames: TStringList;
begin
  if not FileExistsUTF8(Filename) then
    exit;

  params := TColorIniFile.Create(Filename, False);

  FThemeName := params.ReadString('Theme', 'Name', FThemeName);
  FParentThemeName := params.ReadString('Theme', 'ParentTheme', FParentThemeName);

  FPointSize := params.ReadInteger('Graphic', 'PointSize', FPointSize);
  FFontSize := params.ReadInteger('Graphic', 'FontSize', FFontSize);
  FButtockColor := params.ReadColor('Graphic', 'ButtockColor', FButtockColor);
  FWaterlineColor := params.ReadColor('Graphic', 'WaterlineColor', FWaterlineColor);
  FStationColor := params.ReadColor('Graphic', 'StationColor', FStationColor);
  FCreaseColor := params.ReadColor('Graphic', 'CreaseColor', FCreaseColor);
  FCreaseEdgeColor := params.ReadColor('Graphic', 'CreaseEdgeColor', FCreaseEdgeColor);
  FGridColor := params.ReadColor('Graphic', 'GridColor', FGridColor);
  FGridFontColor := params.ReadColor('Graphic', 'GridFontColor', FGridFontColor);
  FEdgeColor := params.ReadColor('Graphic', 'EdgeColor', FEdgeColor);
  FCreasePointColor := params.ReadColor('Graphic', 'CreasePointColor',
    FCreasePointColor);
  FRegularPointColor := params.ReadColor('Graphic', 'RegularPointColor',
    FRegularPointColor);
  FCornerPointColor := params.ReadColor('Graphic', 'CornerPointColor',
    FCornerPointColor);
  FDartPointColor := params.ReadColor('Graphic', 'DartPointColor', FDartPointColor);
  FSelectColor := params.ReadColor('Graphic', 'SelectColor', FSelectColor);
  FLayerColor := params.ReadColor('Graphic', 'LayerColor', FLayerColor);
  FUnderWaterColor := params.ReadColor('Graphic', 'UnderWaterColor', FUnderWaterColor);
  FUnderWaterColorAlpha := params.ReadInteger('Graphic', 'UnderWaterColorAlpha', FUnderWaterColorAlpha);
  FNormalColor := params.ReadColor('Graphic', 'NormalColor', FNormalColor);
  FViewportColor := params.ReadColor('Graphic', 'ViewportColor', FViewportColor);
  FDiagonalColor := params.ReadColor('Graphic', 'DiagonalColor', FDiagonalColor);
  FLeakPointColor := params.ReadColor('Graphic', 'LeakPointColor', FLeakPointColor);
  FMarkerColor := params.ReadColor('Graphic', 'MarkerColor', FMarkerColor);
  FCurvaturePlotColor := params.ReadColor('Graphic', 'CurvaturePlotColor',
    FCurvaturePlotColor);
  FControlCurveColor := params.ReadColor('Graphic', 'ControlCurveColor',
    FControlCurveColor);
  FHydrostaticsFontColor := params.ReadColor(
    'Graphic', 'HydrostaticsFontColor', FHydrostaticsFontColor);
  FZebraStripeColor := params.ReadColor('Graphic', 'ZebraStripeColor',
    FZebraStripeColor);

  FIntersectionLineWidth := params.ReadInteger('Graphic', 'IntersectionLineWidth', FIntersectionLineWidth);
  FControlEdgeLineWidth := params.ReadInteger('Graphic', 'ControlEdgeLineWidth', FControlEdgeLineWidth);
  FInteriorEdgeLineWidth := params.ReadInteger('Graphic', 'InteriorEdgeLineWidth', FInteriorEdgeLineWidth);
  FHydrostaticLineWidth := params.ReadInteger('Graphic', 'HydrostaticLineWidth', FHydrostaticLineWidth);
  FAuxEdgeLineWidth := params.ReadInteger('Graphic', 'AuxEdgeLineWidth', FAuxEdgeLineWidth);

  params.Free;
end;

procedure TFreePreferences.LoadFromIni(Filename: string);
var
  I, N: integer;
  T, L, W, H, S: integer;
  params: TColorIniFile;
  RecentFileNames: TStringList;
begin
  if not FileExistsUTF8(Filename) then
  begin
    if Mainform <> nil then
    begin
      MainForm.WindowState := wsNormal;
      MainForm.SetBounds(0, 0, Screen.WorkAreaWidth, Screen.WorkAreaHeight);
    end;
    exit;
  end;

  params := TColorIniFile.Create(Filename, False);

  FLastDirectory := params.ReadString('Directories', 'LastDirectory', FLastDirectory);
  FOpenDirectory := params.ReadString('Directories', 'OpenDirectory', FOpenDirectory);
  FGlobalOpenDirectory := params.ReadString('Directories',
    'GlobalOpenDirectory', FGlobalOpenDirectory);
  FSaveDirectory := params.ReadString('Directories', 'SaveDirectory', FSaveDirectory);
  FImportDirectory := params.ReadString('Directories', 'ImportDirectory',
    FImportDirectory);
  FGlobalImportDirectory := params.ReadString('Directories',
    'GlobalImportDirectory', FGlobalImportDirectory);
  FExportDirectory := params.ReadString('Directories', 'ExportDirectory',
    FExportDirectory);
  FLanguagesDirectory := params.ReadString('Directories', 'LanguagesDirectory',
    FLanguagesDirectory);
  FExecDirectory := params.ReadString('Directories', 'ExecDirectory', FExecDirectory);
  FManualsDirectory := params.ReadString('Directories', 'ManualsDirectory',
    FManualsDirectory);
  FTempDirectory := params.ReadString('Directories', 'TempDirectory', FTempDirectory);

  //FThemeDirectory := params.ReadString('Directories','ThemeDirectory',FMenuIconDirectory);
  //FThemeName := params.ReadString('Theme','Name',FThemeName);

  FMenuIconSize := params.ReadInteger('Graphic', 'MenuIconSize', FMenuIconSize);
  FToolIconSize := params.ReadInteger('Graphic', 'ToolIconSize', FToolIconSize);
  FThemeName := params.ReadString('Graphic', 'Theme', FThemeName);

  // made dynamic from theme and tool icon size
  //ToolIconDirectory := params.ReadString('Directories', 'ToolIconDirectory', ToolIconDirectory);

  RecentFileNames := TStringList.Create;
  params.ReadSectionValues('RecentFiles', RecentFileNames);

  Owner.Edit.FRecentFiles.Clear;
  Owner.Edit.FRecentFiles.Capacity := RecentFileNames.Count;
  for I := 0 to RecentFileNames.Count - 1 do
  begin
    Filename := RecentFileNames.ValueFromIndex[I];
    // only add the file to the list if it is a valid filename
    //if FileExistsUTF8(Filename) { *Converted from FileExists* } then
    // add any filename, TFreeEmptyModelChooserDialog will be invoked if the file does not exist
    Owner.Edit.FRecentFiles.Add(Filename);
  end;
  FreeAndNil(RecentFileNames);
  if assigned(Owner.FOnUpdateRecentFileList) then
    Owner.FOnUpdateRecentFileList(self);

  //Readln(FFile,T,L,H,W,S);
  T := params.ReadInteger('Window', 'Top', 0);
  L := params.ReadInteger('Window', 'Left', 0);
  H := params.ReadInteger('Window', 'Height', Screen.WorkAreaHeight);
  W := params.ReadInteger('Window', 'Width', Screen.WorkAreaWidth);
  S := params.ReadInteger('Window', 'State', integer(wsNormal));

  if MainForm <> nil then
  begin
    if L > Screen.Width then
      L := 0;
    if T > Screen.Height then
      T := 0;
    if W > Screen.Width then
      W := Screen.WorkAreaWidth;
    if H > Screen.Height then
      H := Screen.WorkAreaHeight;
    case TWindowState(S) of
      wsNormal: MainForm.SetBounds(L, T, W, H);
      wsMinimized:
      begin
        MainForm.WindowState := wsNormal;
        MainForm.SetBounds(L, T, W, H);
      end;
      wsMaximized: MainForm.WindowState := wsMaximized;
    end;
  end;

  FLanguage := params.ReadString('General', 'Language', FLanguage);
  FLanguageFile := FLanguagesDirectory+DirectorySeparator+'FreeShip.'+LanguageCode(Flanguage)+'.po';
  if not FileExistsUTF8(FLanguageFile) { *Converted from FileExists* } then
    FLanguage := 'English';
  FFbmEncoding := params.ReadString('General', 'FbmEncoding', FbmEncoding);
  FMaxUndoMemory := params.ReadInteger('General', 'MaxUndoMemory', FMaxUndoMemory);
  params.Free;
end;

procedure TFreePreferences.SetDefaults;
begin
  FPointSize := 2;
  FFontSize := 8;
  FLanguage := 'English';
  FFbmEncoding := 'cp1252';
  FMaxUndoMemory := 32;
  FMenuIconSize := 16;
  FToolIconSize := 24;
  FThemeName := 'Default';
  FParentThemeName := '';
  ResetDirectories;
  ResetColors;
end;

procedure TFreePreferences.ResetDirectories;
const DS = DirectorySeparator;
var
  AppDataDir, S: string;

  function chooseDirAppDataDir(GlobalAppDataDir, UserAppDataDir: string): string;
  begin
    if (UserAppDataDir <> '') and (DirectoryExistsUTF8(UserAppDataDir)) then
      Result := UserAppDataDir
    else
      Result := GlobalAppDataDir;
  end;

begin
  FUserConfigDirectory := getUserConfigDirectory;
  FGlobalConfigDirectory := getGlobalConfigDirectory;
  FUserDataDirectory := getUserDataDirectory;
  FUserAppDataDirectory := getUserAppDataDirectory;
  FGlobalAppDataDirectory := getGlobalAppDataDirectory;
  FTempDirectory := getTempDirectory;

  FOpenDirectory := FUserDataDirectory + DS + 'Ships';
  FSaveDirectory := FUserDataDirectory + DS + 'Ships';
  FImportDirectory := FUserDataDirectory + DS + 'Import';
  FExportDirectory := FUserDataDirectory + DS + 'Export';

  FLanguagesDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory +
    DS + 'Languages', FUserAppDataDirectory + DS + 'locale');
  FExecDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory +
    DS + 'Exec', FUserAppDataDirectory + DS + 'Exec');
  FManualsDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory +
    DS + 'Manuals', FUserAppDataDirectory + DS + 'Manuals');
  {FTempDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory +
    DS + 'Temp', FUserAppDataDirectory + DS + 'Temp');}
  //FTempDirectory := FUserAppDataDirectory + DS + 'Temp';

  FGlobalOpenDirectory:=FGlobalAppDataDirectory + DS + 'Ships';
  FGlobalImportDirectory:=FGlobalAppDataDirectory + DS + 'Import';

  S:=StringReplace('/Themes/Default/icons/16','/',DS,[rfReplaceAll]);
  FMenuIconDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory + S,
                        FUserAppDataDirectory + S );

  S:=StringReplace('/Themes/Default/icons/24','/',DS,[rfReplaceAll]);
  //FToolIconDirectory := chooseDirAppDataDir(FGlobalAppDataDirectory + S,
  //                      FUserAppDataDirectory + S);

end;

resourcestring
rsGlobalConfigLoadErrorMessage = 'Unable to load per-machine configuration. Skipping. There may be further problems with finding directories and files.'+#10+'Error:';
rsUserConfigLoadErrorMessage = 'Unable to load per-user configuration. There may be further problems with finding directories and files. Using defaults.'+#10+'Error:';
rsThemeLoadErrorMessage = 'Unable to load theme %s'+#10+'Error:';

procedure TFreePreferences.Load;
var
  GlobalConfigFileName, UserConfigFileName, DtaFilename: string;
  I, N: integer;
  T, L, W, H, S: integer;
  params: TColorIniFile;
  RecentFileNames: TStrings;
begin
  detectApplicationScope;
  setDefaults;
  {FGlobalConfigDirectory := self.getGlobalConfigDirectory;
  FGlobalAppDataDirectory := self.getGlobalAppDataDirectory;
  FUserConfigDirectory := self.getUserConfigDirectory;
  FUserAppDataDirectory := self.getUserAppDataDirectory;
  FUserDataDirectory := self.getUserDataDirectory;}

  GlobalConfigFileName := FGlobalConfigDirectory + DirectorySeparator+'FreeShip.ini';
  UserConfigFileName := FUserConfigDirectory + DirectorySeparator+'FreeShip.ini';

  // just for migration from .dta to .ini
  if not FileExistsUTF8(GlobalConfigFileName) and not
    FileExistsUTF8(UserConfigFileName) then
  begin
    DtaFilename := ChangeFileExt(Application.ExeName, '.dta');
    if FileExistsUTF8(DtaFilename) then
    begin
      LoadFromDta(DtaFilename);
      exit;
    end;
  end;

  try
    LoadFromIni(GlobalConfigFileName);
  except on E:Exception do
    ShowMessage(rsGlobalConfigLoadErrorMessage + E.Message);
  end;
  try
    LoadFromIni(UserConfigFileName);
  except on E:Exception do
    ShowMessage(rsUserConfigLoadErrorMessage + E.Message);
  end;

  try
    LoadTheme(FThemeName);
  except on E:Exception do
    ShowMessage(format(rsThemeLoadErrorMessage,[FThemeName]) + E.Message);
  end;
end;{TFreePreferences.Load}


procedure TFreePreferences.ResetColors;
begin
  FThemeName := 'Default';
  FParentThemeName := '';

  FButtockColor := $00808040;     // Kind of teal-blue
  FWaterlineColor := $00808040;   // Kind of teal-blue
  FStationColor := $00808040;     // Kind of teal-blue
  FDiagonalColor := $00808040;     // Kind of teal-blue
  FCreaseColor := clBlack;        // color of descendants from crease controledges
  FCreaseEdgeColor := clRed;      // Color of crease control edges
  FGridColor := clSilver;
  FGridFontColor := clWhite;
  FEdgeColor := $006F6F6F;
  FCreasePointColor := $00004080;
  FRegularPointColor := $00E1E1E1;
  FCornerPointColor := $00B95C00;
  FDartPointColor := clFuchsia;
  FSelectColor := clYellow;
  FLayerColor := RGB(0, 128, 0);       // Default color of each layer (green-ish)
  FViewportColor := $009F9F9F;
  FUnderwaterColor := RGB(240, 240, 240);
  FUnderwaterColorAlpha := 255;   //opaque
  FLeakPointColor := $0099FF00;
  FNormalColor := clWhite;
  FMarkerColor := $008000FF;
  FCurvaturePlotColor := clFuchsia;
  FControlCurveColor := 16711808;
  FHydrostaticsFontColor := clMaroon;
  FZebraStripeColor := RGB(230, 230, 230);

  FIntersectionLineWidth := 1;
  FControlEdgeLineWidth := 2;
  FInteriorEdgeLineWidth := 1;
  FAuxEdgeLineWidth := 1;
  FHydrostaticLineWidth := 1;

end;{TFreePreferences.ResetColors}

procedure TFreePreferences.SaveToDta; //deprecated
var
  FileName: string;
  FFile: TextFile;
  I: integer;
begin
  Filename := ChangeFileExt(Application.ExeName, '.dta');
  AssignFile(FFile, Filename);
  try
    Rewrite(FFile);
    Writeln(FFile, FPointSize);
    Writeln(FFile, FButtockColor);
    Writeln(FFile, FWaterlineColor);
    Writeln(FFile, FStationColor);
    Writeln(FFile, FCreaseColor);
    Writeln(FFile, FCreaseEdgeColor);
    Writeln(FFile, FGridColor);
    Writeln(FFile, FGridFontColor);
    Writeln(FFile, FEdgeColor);
    Writeln(FFile, FCreasePointColor);
    Writeln(FFile, FRegularPointColor);
    Writeln(FFile, FCornerPointColor);
    Writeln(FFile, FDartPointColor);
    Writeln(FFile, FSelectColor);
    Writeln(FFile, FLayerColor);
    Writeln(FFile, FUnderWaterColor);
    Writeln(FFile, FNormalColor);
    Writeln(FFile, FViewportColor);
    Writeln(FFile, FOpenDirectory);
    Writeln(FFile, FSaveDirectory);
    Writeln(FFile, FImportDirectory);
    Writeln(FFile, FExportDirectory);
    Writeln(FFile, FDiagonalColor);
    // save list with recently used files
    writeln(FFile, Owner.Edit.RecentFileCount);
    for I := 1 to Owner.Edit.RecentFileCount do
      Writeln(FFile, Owner.Edit.RecentFile[I - 1]);
    Writeln(FFile, FLeakPointColor);
    Writeln(FFile, FMarkerColor);
    Writeln(FFile, FCurvaturePlotColor);
    Writeln(FFile, FControlCurveColor);
    Writeln(FFile, FHydrostaticsFontColor);
    Writeln(FFile, FZebraStripeColor);
    Writeln(FFile, MainForm.Top, #32, MainForm.Left, #32, MainForm.Height, #32,
      MainForm.Width, #32, Ord(MainForm.WindowState));
    Writeln(FFile, FLanguageFile);
    Writeln(FFile, FMaxUndoMemory);
    CloseFile(FFile);
  except
    MessageDlg(rs_Could_not_save_program_preferences_to_the_following_location {UserString[177]} + ':' + EOL + Filename, mtError, [mbOK], 0);
  end;
end;{TFreePreferences.SaveToDta}

procedure TFreePreferences.Save;
var
  FileName: string;
  I, N: integer;
  T, L, W, H, S: integer;
  params: TColorIniFile;
begin
  FileName := self.getUserConfigDirectory + DirectorySeparator+'FreeShip.ini';
  if not FileExistsUTF8(Filename) then
    ForceDirectoriesUTF8(ExtractFilePath(Filename));

  params := TColorIniFile.Create(Filename, False);

  params.WriteString('Directories', 'LastDirectory', FLastDirectory);
  params.WriteString('Directories', 'OpenDirectory', FOpenDirectory);
  params.WriteString('Directories', 'GlobalOpenDirectory', FGlobalOpenDirectory);
  params.WriteString('Directories', 'SaveDirectory', FSaveDirectory);
  params.WriteString('Directories', 'ImportDirectory', FImportDirectory);
  params.WriteString('Directories', 'GlobalImportDirectory', FGlobalImportDirectory);
  params.WriteString('Directories', 'ExportDirectory', FExportDirectory);
  params.WriteString('Directories', 'LanguagesDirectory', FLanguagesDirectory);
  params.WriteString('Directories', 'ExecDirectory', FExecDirectory);
  params.WriteString('Directories', 'ManualsDirectory', FManualsDirectory);
  params.WriteString('Directories', 'TempDirectory', FTempDirectory);

  params.WriteString('Directories', 'MenuIconDirectory', FMenuIconDirectory);
  //params.WriteString('Directories', 'ToolIconDirectory', FToolIconDirectory);
  params.WriteInteger('Graphic', 'MenuIconSize', FMenuIconSize);
  params.WriteInteger('Graphic', 'ToolIconSize', FToolIconSize);
  params.WriteString('Graphic', 'Theme', FThemeName);

  params.EraseSection('RecentFiles');
  for I := 0 to Owner.Edit.FRecentFiles.Count - 1 do
  begin
    Filename := Owner.Edit.FRecentFiles[I];
    params.WriteString('RecentFiles', 'File' + IntToStr(I + 1), Filename);
  end;

  if assigned(Owner.FOnUpdateRecentFileList) then
    Owner.FOnUpdateRecentFileList(self);

  if MainForm <> nil then
  begin
    params.WriteInteger('Window', 'Top', MainForm.Top);
    params.WriteInteger('Window', 'Left', MainForm.Left);
    params.WriteInteger('Window', 'Height', MainForm.Height);
    params.WriteInteger('Window', 'Width', MainForm.Width);
    params.WriteInteger('Window', 'State', integer(MainForm.WindowState));
  end;

  params.WriteString('General', 'Language', FLanguage);
  params.WriteString('General', 'FbmEncoding', FbmEncoding);
  params.WriteInteger('General', 'MaxUndoMemory', FMaxUndoMemory);
end;

procedure TFreePreferences.SaveTheme(ThemeName, ParentThemeName: string);
var
  FileName, DirName: string;
  params: TColorIniFile;
const DS=DirectorySeparator;
begin
  DirName := self.getUserConfigDirectory + DS+'Themes'+DS + ThemeName;
  FileName := DirName + DS+'theme.ini';

  if not DirectoryExistsUTF8(DirName) then
    ForceDirectoriesUTF8(DirName);

  params := TColorIniFile.Create(Filename, False);

  params.WriteString('Theme', 'Name', ThemeName);
  params.WriteString('Theme', 'ParentTheme', ParentThemeName);

  params.WriteInteger('Graphic', 'PointSize', FPointSize);
  params.WriteInteger('Graphic', 'FontSize', FFontSize);
  params.WriteColor('Graphic', 'ButtockColor', FButtockColor);
  params.WriteColor('Graphic', 'WaterlineColor', FWaterlineColor);
  params.WriteColor('Graphic', 'StationColor', FStationColor);
  params.WriteColor('Graphic', 'CreaseColor', FCreaseColor);
  params.WriteColor('Graphic', 'CreaseEdgeColor', FCreaseEdgeColor);
  params.WriteColor('Graphic', 'GridColor', FGridColor);
  params.WriteColor('Graphic', 'GridFontColor', FGridFontColor);
  params.WriteColor('Graphic', 'EdgeColor', FEdgeColor);
  params.WriteColor('Graphic', 'CreasePointColor', FCreasePointColor);
  params.WriteColor('Graphic', 'RegularPointColor', FRegularPointColor);
  params.WriteColor('Graphic', 'CornerPointColor', FCornerPointColor);
  params.WriteColor('Graphic', 'DartPointColor', FDartPointColor);
  params.WriteColor('Graphic', 'SelectColor', FSelectColor);
  params.WriteColor('Graphic', 'LayerColor', FLayerColor);
  params.WriteColor('Graphic', 'UnderWaterColor', FUnderWaterColor);
  params.WriteInteger('Graphic', 'UnderWaterColorAlpha', FUnderWaterColorAlpha);
  params.WriteColor('Graphic', 'NormalColor', FNormalColor);
  params.WriteColor('Graphic', 'ViewportColor', FViewportColor);
  params.WriteColor('Graphic', 'DiagonalColor', FDiagonalColor);
  params.WriteColor('Graphic', 'LeakPointColor', FLeakPointColor);
  params.WriteColor('Graphic', 'MarkerColor', FMarkerColor);
  params.WriteColor('Graphic', 'CurvaturePlotColor', FCurvaturePlotColor);
  params.WriteColor('Graphic', 'ControlCurveColor', FControlCurveColor);
  params.WriteColor('Graphic', 'HydrostaticsFontColor', FHydrostaticsFontColor);
  params.WriteColor('Graphic', 'ZebraStripeColor', FZebraStripeColor);

  params.WriteInteger('Graphic', 'IntersectionLineWidth', FIntersectionLineWidth);
  params.WriteInteger('Graphic', 'ControlEdgeLineWidth', FControlEdgeLineWidth);
  params.WriteInteger('Graphic', 'InteriorEdgeLineWidth', FInteriorEdgeLineWidth);
  params.WriteInteger('Graphic', 'HydrostaticLineWidth', FHydrostaticLineWidth);
  params.WriteInteger('Graphic', 'AuxEdgeLineWidth', FAuxEdgeLineWidth);

end;  {SaveTheme}

