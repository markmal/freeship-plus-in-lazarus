unit FreeFilePreviewDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics,
  Dialogs, ComCtrls, EditBtn,
  //ShellCtrls,
  FreeShellCtrls,
  StdCtrls, Buttons,
  ExtCtrls, PairSplitter, FileCtrl, Menus, ActnList, ShellCtrls, Math,
  {$IFDEF WINDOWS}
  Windows, ShellApi
  {$ELSE}
  fileicon
  {$ENDIF}
  , Types
  ;

const
  ftDirectory = 0;
  ftText = 1;
  ftExecutable = 2;

resourcestring
  hintDblClickToFullSize = 'DblClick to full size';
  hintDblClickToFitSize = 'DblClick to fit size';

type

 TDir = class
    fDir: string;
    constructor Create(p:string);
 end;

 TSelectFileEvent = procedure(Sender: TObject; filename:string) of object;

 TFileDialogMode = ( fdmOpen, fdmSave );

  { TFreeFilePreviewDialog }

 TFreeFilePreviewDialog = class(TForm)
    ActionRefresh: TAction;
    ActionList: TActionList;
    BitBtnOpen: TBitBtn;
    BitBtnCancel: TBitBtn;
    ComboBoxDir: TComboBox;
    FullSize: TMenuItem;
    FitSize: TMenuItem;
    Properties: TMenuItem;
    PreviewPopupMenu: TPopupMenu;
    ShellPathPanel: TShellPathPanel;
    BottomPanel: TPanel;
    EditName: TEdit;
    FilterComboBox1: TFilterComboBox;
    LabelLocation: TLabel;
    LabelName: TLabel;
    AutoSearchListBox: TListBox;
    ListViewPlaces: TListView;
    ShellListViewItemNewFolder: TMenuItem;
    ShellListViewItemDelete: TMenuItem;
    ShellListViewItemRename: TMenuItem;
    ShellListViewPopupMenu: TPopupMenu;
    SavePanel: TPanel;
    SaveToolsPanel: TPanel;
    Panel5: TPanel;
    LocationEditorPanel: TPanel;
    Panel2: TPanel;
    NavigationPanel: TPanel;
    LocationSelectorPanel: TPanel;
    Panel4: TPanel;
    Panel7: TPanel;
    PreviewToolPanel: TPanel;
    PlacesMenuItemRename: TMenuItem;
    PlacesMenuItemDelete: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterFilesAndPreview: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSideFiles: TPairSplitterSide;
    PairSplitterSidePreview: TPairSplitterSide;
    Panel1: TPanel;
    PlacesPopupMenu: TPopupMenu;
    FPreviewImage: TImage;
    LeftPageControl: TPageControl;
    CenterPanel: TPanel;
    ScrollBoxPreview: TScrollBox;
    ShellTreeView: TShellTreeView;
    LocationPanel: TPanel;
    SmallImageList: TImageList;
    LargeImageList: TImageList;
    ShellListView: TShellListView;
    SpeedButtonEditLocation: TSpeedButton;
    //ShellListView: TFreeShellListView;
    SpeedButtonViewDetails: TSpeedButton;
    SpeedButtonViewListSimple: TSpeedButton;
    SpeedButtonViewListCompact: TSpeedButton;
    SpeedButtonViewListLarge: TSpeedButton;
    SpeedButtonViewListHidden: TSpeedButton;
    SpeedButtonNewDir: TSpeedButton;
    SpeedButtonGoUp: TSpeedButton;
    SpeedButtonGoBack: TSpeedButton;
    SpeedButtonAddPlace: TSpeedButton;
    StatusBar: TStatusBar;
    TabSheetPlaces: TTabSheet;
    TabSheetTree: TTabSheet;
    ToolBarOps: TToolBar;

    procedure ActionRefreshExecute(Sender: TObject);
    procedure AutoSearchListBoxClick(Sender: TObject);
    procedure AutoSearchListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AutoSearchListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AutoSearchListBoxMouseEnter(Sender: TObject);
    procedure AutoSearchListBoxMouseLeave(Sender: TObject);
    procedure AutoSearchListBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure AutoSearchListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure BitBtnOpenClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure ComboBoxDirChange(Sender: TObject);
    procedure ComboBoxDirEditingDone(Sender: TObject);
    procedure ComboBoxDirKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBoxDirKeyPress(Sender: TObject; var Key: char);
    procedure FilterComboBox1Change(Sender: TObject);
    procedure FilterComboBox1Select(Sender: TObject);
    procedure FitSizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelLocationResize(Sender: TObject);
    procedure LeftPageControlChange(Sender: TObject);
    procedure ListViewPlacesClick(Sender: TObject);
    procedure ListViewPlacesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListViewPlacesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListViewPlacesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewPlacesShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FullSizeClick(Sender: TObject);
    procedure PropertiesClick(Sender: TObject);
    procedure ShellListViewItemDeleteClick(Sender: TObject);
    procedure ShellListViewItemNewFolderClick(Sender: TObject);
    procedure NavigationPanelResize(Sender: TObject);
    procedure PlacesMenuItemDeleteClick(Sender: TObject);
    procedure PlacesMenuItemRenameClick(Sender: TObject);
    procedure ScrollBoxPreviewDblClick(Sender: TObject);
    procedure ScrollBoxPreviewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBoxPreviewResize(Sender: TObject);
    procedure ShellListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellListViewFileAdded(Sender: TObject; Item: TListItem);
    procedure ShellListViewItemRenameClick(Sender: TObject);
    procedure ShellListViewKeyPress(Sender: TObject; var Key: char);
    procedure ShellListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShellListViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShellListViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShellListViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewSelectionChanged(Sender: TObject);
    procedure SpeedButtonEditLocationClick(Sender: TObject);
    procedure SpeedButtonViewDetailsClick(Sender: TObject);
    procedure SpeedButtonViewListHiddenClick(Sender: TObject);
    procedure SpeedButtonViewListSimpleClick(Sender: TObject);
    procedure SpeedButtonViewListCompactClick(Sender: TObject);
    procedure SpeedButtonViewListLargeClick(Sender: TObject);
    procedure SpeedButtonGoBackClick(Sender: TObject);
    procedure SpeedButtonGoUpClick(Sender: TObject);
    procedure SpeedButtonAddPlaceClick(Sender: TObject);
    procedure SpeedButtonNewDirClick(Sender: TObject);
    procedure selectFile;
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
    function GetFilterIndex: integer;
    procedure SetFilterIndex(const AValue: integer);
    procedure ShellListViewEdited(Sender: TObject; Item: TListItem; var AValue: string);
    procedure ShellListViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ShellPathPanelOnSelectionChanged(Sender: TObject);
 private
    { private declarations }
    FRoot : String;
    FFileName : string;
    FIconNamesMap: TStringList;
    FMousePos: TPoint;
    FOnSelectFile: TSelectFileEvent;
    FOnPreview: TSelectFileEvent;
    FHistoryStack: TStringList;
    FFileDialogMode : TFileDialogMode;
    FFileMimeIcon:TFileMimeIcon;
    FMouseDownTime:TDateTime;
    FInAutoSearchListBox:boolean;
    FListBox1ControlledByKeys:boolean;
    FInShellPathPanelOnSelectionChanged:boolean;
    FInShellTreeViewChange:boolean;
    FInShellTreeViewSelectionChanged:boolean;
    FIconVIewCellWidth:integer;
    FIconVIewCellHeight:integer;
    FLocaleDir:string;
    FLang:string; // short name like 'ru', 'de'
    {$IFDEF WINDOWS}
    function GetWinIconForFile(filename:string; isLarge: boolean; imageList:TImageList): integer;
    {$ELSE}
    procedure addMagicIconsForFile(filename:string; Item:TListItem);
    procedure addMagicIconForFile(filename:string; Item:TTreeNode);
    {$ENDIF}

    procedure addIconsForFile(filename:string; item:TListItem);

    function  getSelectedFileName:string;
    procedure setSelectedFileName(fn:string);
    function  getCurrentPath:string;
    procedure setCurrentPath(p:string);
    procedure setTooltips;

    function  getPreviewText:string;
    procedure setPreviewText(s:string);
    function  getPreviewBitmap:Graphics.TBitmap;
    procedure setPreviewBitmap(bmp:Graphics.TBitmap);
    function  getPreviewPicture:TPicture;
    procedure setPreviewPicture(pic:TPicture);
    procedure DefaultPreview(fn:string);
    procedure FitImageIntoScrollBox;
    procedure ShellListViewSetWordWrap(slv: TCustomShellListView);
    procedure setSpeedButtonGoUpEnabled(path:String);
    procedure setFileDialogMode(const AValue: TFileDialogMode);
    procedure setLang(ALang:string);

    function CreateNewDir(aName:String):boolean;
    procedure CreateNewFolder;
    procedure AddToHistory(dir:string);
 public
    function Execute: boolean;
    procedure ChangeDir(dir:string);
    procedure addPlace(placeName:string; dirName:string);
    procedure setStatusText(AValue:String);
    procedure ReplaceLabels(aControl:TControl; fromText:String; toText:String); // search and replace a label for control and its children
    procedure Translate(ALang:string); // '' is default Lang from environment  published
    property FileName:string read getSelectedFileName write setSelectedFileName;
    property CurrentPath:string read getCurrentPath write setCurrentPath;
    property OnSelectFile: TSelectFileEvent read FOnSelectFile write FOnSelectFile;
    property OnPreview: TSelectFileEvent read FOnPreview write FOnPreview;
    property PreviewImage: TImage read FPreviewImage;
    property PreviewPicture: TPicture read getPreviewPicture write setPreviewPicture;
    property PreviewBitmap: Graphics.TBitmap read getPreviewBitmap write setPreviewBitmap;
    property PreviewText: String read getPreviewText write setPreviewText;
    property Filter: String read getFilter write setFilter;
    property FilterIndex: integer read getFilterIndex write setFilterIndex;
    property FileDialogMode: TFileDialogMode read FFileDialogMode write setFileDialogMode;
    property LocaleDir: String read FLocaleDir write FLocaleDir;
    property Lang: String read FLang write setLang;
  end;

implementation
uses LCLType,
  LResources, LCLTranslator, LazUTF8, // for translations
  qtwidgets
  ,qt4
//  ,qt5
  ;


{$R *.lfm}

const
  cSecunde = 1/24/60/60;
  cMinute  = 1/24/60;
  cHour    = 1/24;

constructor TDir.Create(p:string);
begin
  fDir:=p;
end;

{ TFreeFilePreviewDialog }

procedure TFreeFilePreviewDialog.ShellListViewSetWordWrap(slv:TCustomShellListView);
var   QtListWidget: TQtListWidget; gSz : TSize; ih,fh:integer;
    fd: TFontData;
begin
  QtListWidget := TQtListWidget(slv.Handle);
  if ShellListView.ViewStyle=vsSmallIcon then
     ih:=SmallImageList.Height;
  if ShellListView.ViewStyle=vsIcon then
     ih:=LargeImageList.Height;
  fd := GetFontData(ShellListView.Font.Handle);
  fh:=fd.Height;
  if fh=0 then fh:=16;
  if FIconViewCellWidth = 0 then FIconViewCellWidth := 128;
  if FIconVIewCellHeight = 0 then FIconVIewCellHeight := ih + fh + 4;
  gSz := Size(FIconVIewCellWidth, FIconVIewCellHeight);
  //gSz := Size(128, ih + fh + 4);
  QtListWidget.GridSize:=gSz;
  QtListWidget.setWrapping(true);
  QtListWidget.setWordWrap(true);
end;


procedure TFreeFilePreviewDialog.SpeedButtonViewDetailsClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsReport;
  setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListHiddenClick(Sender: TObject
  );
var ot: TObjectTypes;
begin
  ot:=ShellListView.ObjectTypes;
  if SpeedButtonViewListHidden.Down
    then include(ot, otHidden)
    else exclude(ot, otHidden);
  ShellListView.ObjectTypes:=ot;
  ShellListView.Reload;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListSimpleClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsList;
  setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListCompactClick(Sender: TObject);
begin
    ShellListView.ViewStyle:=vsSmallIcon;
    ShellListViewSetWordWrap(ShellListView);
    setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListLargeClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsIcon;
  ShellListViewSetWordWrap(ShellListView);
  setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonGoBackClick(Sender: TObject);
var bkDir: String;
begin
  if FHistoryStack.Count > 0 then
    begin
    bkDir := FHistoryStack[0];
    FHistoryStack.Delete(0);

    self.ChangeDir(bkDir);

    if FHistoryStack.Count > 0 then
       FHistoryStack.Delete(0);

    if FHistoryStack.Count > 0 then
      begin
       SpeedButtonGoBack.Hint := FHistoryStack[0];
       SpeedButtonGoBack.Enabled:=true;
      end
    else
      begin
       SpeedButtonGoBack.Hint := '';
       SpeedButtonGoBack.Enabled:=false;
      end;
    end;
end;

procedure TFreeFilePreviewDialog.SpeedButtonGoUpClick(Sender: TObject);
var parentDir, parentDir2 : string;
begin
    parentDir := ExpandFileName(IncludeTrailingPathDelimiter(ShellListView.Root) + '..');
    ChangeDir(parentDir);

    parentDir2 := ExpandFileName(IncludeTrailingPathDelimiter(ShellListView.Root) + '..');

    SpeedButtonGoUp.Enabled := (parentDir <> parentDir2);
end;


procedure TFreeFilePreviewDialog.ComboBoxDirEditingDone(Sender: TObject);
begin
  if not ComboBoxDir.Focused then exit;
  if FInAutoSearchListBox then exit;
  AutoSearchListBox.Visible:=false;
  ChangeDir( ComboBoxDir.Text );
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxSelectionChange(
  Sender: TObject; User: boolean);
var s:string;
var Key: Word; Shift: TShiftState;
begin
  if FListBox1ControlledByKeys then exit; // do only on Mouse selection
  s:=AutoSearchListBox.GetSelectedText;
  Key:=VK_RETURN; Shift:=[];
  AutoSearchListBoxKeyDown(Sender, Key, Shift);
  //Self.setStatusText(s);
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FListBox1ControlledByKeys:=true;
  if Key = VK_RETURN then
    begin
    FInAutoSearchListBox:=false;
    ComboBoxDir.Text := CleanAndExpandDirectory(ShellListView.Root
                     + DirectorySeparator + AutoSearchListBox.GetSelectedText);

    ComboBoxDir.SetFocus;
    AutoSearchListBox.Visible:=false;
    Key:=0;
    end;
  if Key = VK_ESCAPE then
    begin
    ComboBoxDir.SetFocus;
    FInAutoSearchListBox:=false;
    AutoSearchListBox.Visible:=false;
    Key:=0;
    end;
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s:string;
begin
  //s:=AutoSearchListBox.GetSelectedText;
  //Self.setStatusText(s);
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxMouseEnter(Sender: TObject);
begin
 FListBox1ControlledByKeys:=false;
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxMouseLeave(Sender: TObject);
begin
 FListBox1ControlledByKeys:=true;
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 FListBox1ControlledByKeys:=false;
end;

procedure TFreeFilePreviewDialog.ComboBoxDirKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (AutoSearchListBox.Items.Count > 0)
     then begin
     key:=0;
     FListBox1ControlledByKeys:=true;
     FInAutoSearchListBox := true;
     //AutoSearchListBox.Visible:=true;
     AutoSearchListBox.SetFocus;
     AutoSearchListBox.ItemIndex:=0;
     AutoSearchListBox.MultiSelect:=true;  // it is trick to workaround selection issues
     //Application.ProcessMessages;
     if AutoSearchListBox.Items.Count=1
        then AutoSearchListBox.SelectAll
        else AutoSearchListBox.Selected[0]:=true;
     AutoSearchListBox.MultiSelect:=false;
  end;
end;

procedure TFreeFilePreviewDialog.ComboBoxDirChange(Sender: TObject);
var nm,s,cd:string; sl,sel: TStringList;
    item:TListItem; i,h:integer;
    lb:TListBox; fi:TFileItem;
    P1:TPoint;
begin
 //if not ComboBoxDir.Focused then exit;

 sl := fileicon.Split(ComboBoxDir.Text, {System.}DirectorySeparator);
  nm := sl[sl.Count-1];
  cd := '';
  // reconstruct path to compare with current path
  for i:=0 to sl.Count-2 do cd := cd + {System.}DirectorySeparator + sl[i];
  cd:=CleanAndExpandDirectory(cd);
  if cd <> ShellListView.Root then exit;

  if nm = '' then
    begin
    AutoSearchListBox.Clear;
    AutoSearchListBox.Visible:=false;
    exit;
    end;

  i:=0;
  AutoSearchListBox.Clear;
  item:=ShellListView.FindCaption(i,nm,true,true,false,false);
  while assigned(item) do
  begin
    s:=item.Caption;
    fi:=TFileItem(item.Data);
    s:=(TFileItem(item.Data)).FileName;
    if assigned(item.Data)
      and (TFileItem(item.Data)).IsFolder then
        AutoSearchListBox.AddItem(item.Caption,nil);
    i := item.Index+1;
    item:=ShellListView.FindCaption(i,nm,true,true,false,false);
  end;
  if AutoSearchListBox.Items.Count = 0 then
    AutoSearchListBox.Visible:=false
  else
    begin
    P1:=ComboBoxDir.ClientToParent(
           Point(ComboBoxDir.Left,ComboBoxDir.Top+ComboBoxDir.Height), Self);
    AutoSearchListBox.Left:=P1.x;
    AutoSearchListBox.Top:=P1.y;
    AutoSearchListBox.Width := ShellListView.Columns[0].Width;
    AutoSearchListBox.Visible:=true;
    Application.ProcessMessages;
    AutoSearchListBox.Constraints.MaxHeight:=Self.Height - AutoSearchListBox.Top;
    h:=AutoSearchListBox.Items.Count * AutoSearchListBox.ItemHeight + 4;
    AutoSearchListBox.Height:=AutoSearchListBox.Items.Count * AutoSearchListBox.ItemHeight + 4;
    Application.ProcessMessages;
    end;
end;

procedure TFreeFilePreviewDialog.ComboBoxDirKeyPress(Sender: TObject;
  var Key: char);
var nm,s:string; sl,sel: TStringList;
    item:TListItem; i,h:integer;
    lb:TListBox; fi:TFileItem;
begin
  //inherited KeyPress(Key);
  sl := fileicon.Split(ComboBoxDir.Text, {System.}DirectorySeparator);
  nm := sl[sl.Count-1];
  if (Key>#31) and (Key<#127) then
     nm := nm + key;

  if nm = '' then exit;

  i:=0;
  AutoSearchListBox.Clear;
  item:=ShellListView.FindCaption(i,nm,true,true,false,false);
  while assigned(item) do
  begin
    s:=item.Caption;
    fi:=TFileItem(item.Data);
    s:=(TFileItem(item.Data)).FileName;
    if assigned(item.Data)
      and (TFileItem(item.Data)).IsFolder then
        AutoSearchListBox.AddItem(item.Caption,nil);
    i := item.Index+1;
    item:=ShellListView.FindCaption(i,nm,true,true,false,false);
  end;
  if AutoSearchListBox.Items.Count = 0 then
    AutoSearchListBox.Visible:=false
  else
    begin
    AutoSearchListBox.Width := ShellListView.Columns[0].Width;
    AutoSearchListBox.Visible:=true;
    Application.ProcessMessages;
    AutoSearchListBox.Constraints.MaxHeight:=Self.Height - AutoSearchListBox.Top;
    h:=AutoSearchListBox.Items.Count * AutoSearchListBox.ItemHeight + 4;
    AutoSearchListBox.Height:=AutoSearchListBox.Items.Count * AutoSearchListBox.ItemHeight + 4;
    Application.ProcessMessages;
    end;
end;

procedure TFreeFilePreviewDialog.FilterComboBox1Change(Sender: TObject);
begin

end;

function ExtractFileNameExt(const AFilename: string): string; // extracts extention with dot
var
  p: Integer;
begin
  Result:=AFilename;
  p:=length(Result);
  while (p>0) do begin
    case Result[p] of
      PathDelim: exit;
      {$ifdef windows}
      '/': if ('/' in AllowDirectorySeparators) then exit;
      {$endif}
      '.': exit(copy(Result, p, length(Result)-p+1));
    end;
    dec(p);
  end;
end;

procedure TFreeFilePreviewDialog.FilterComboBox1Select(Sender: TObject);
var ext, mask : string;
begin
  if self.FFileDialogMode <> fdmSave then exit;
  ext := FilterComboBox1.Items[FilterComboBox1.ItemIndex];
  mask:=FilterComboBox1.Mask;
  ext:=ExtractFileNameExt(mask);
  if (pos(';',mask)>0) or (pos(';',ext)>0) or (pos('*',ext)>0) or (pos('?',ext)>0) then exit;
  EditName.Text := ExtractFileNameWithoutExt(EditName.Text) + ext;
end;

procedure TFreeFilePreviewDialog.FitSizeClick(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  FPreviewImage.AutoSize := false;
  if not FPreviewImage.AutoSize then
    begin
    FitImageIntoScrollBox;
    FPreviewImage.Hint := hintDblClickToFullSize;
    ScrollBoxPreview.Hint := FPreviewImage.Hint;
    end
  else
    begin
    FPreviewImage.Hint := hintDblClickToFitSize;
    ScrollBoxPreview.Hint := FPreviewImage.Hint;
    end;
end;

function TFreeFilePreviewDialog.CreateNewDir(aName:String):boolean;
var curPath, newPath, newDir: String;  x: boolean;
begin
  newDir := aName;
  curPath := ShellTreeView.Path;
  newPath := AppendPathDelim(curPath) + newDir;
  result:=false;
  if CreateDirUTF8(newPath) then
    begin // refresh
    result:=true;
    x := ShellTreeView.Selected.Expanded;
    ShellTreeView.Selected.Collapse(false) ;
    if x then ShellTreeView.Selected.Expand(false);
    //ShellListView.Root:=AppendPathDelim(curPath)+'.';
    ShellListView.Reload;
    end;
end;

procedure TFreeFilePreviewDialog.BitBtnOpenClick(Sender: TObject);
begin
  Close;
end;

procedure TFreeFilePreviewDialog.ActionRefreshExecute(Sender: TObject);
var x : boolean;
begin
  ShellListView.Root:=AppendPathDelim(ShellTreeView.Path)+'.';
  x := ShellTreeView.Selected.Expanded;
  ShellTreeView.Selected.Collapse(false);
  if x then ShellTreeView.Selected.Expand(false);
end;

procedure TFreeFilePreviewDialog.AutoSearchListBoxClick(Sender: TObject);
begin
{ ComboBoxDir.Text := ShellListView.Root + DirectorySeparator
                  + AutoSearchListBox.GetSelectedText;
 ComboBoxDir.SetFocus;
 FInAutoSearchListBox:=false;
 AutoSearchListBox.Visible:=false;}
end;

procedure TFreeFilePreviewDialog.BitBtnCancelClick(Sender: TObject);
begin
  FFileName := '';
  Close;
end;


procedure TFreeFilePreviewDialog.FormCreate(Sender: TObject);
var GlobalTranslator: TAbstractTranslator; LocalTranslator: TPOTranslator;
begin
  FInShellTreeViewChange:=false;
  FInShellPathPanelOnSelectionChanged:=false;
  FInShellTreeViewSelectionChanged:=false;
  FIconViewCellWidth:=0;
  FIconVIewCellHeight:=0;

  FMouseDownTime := maxDouble;
  {$IFDEF WINDOWS}
  SmallImageList.Clear;
  LargeImageList.Clear;
  {$ELSE}
  FFileMimeIcon:=TFileMimeIcon.Create;
  {$ENDIF}
  FIconNamesMap:= TStringList.Create;

  FListBox1ControlledByKeys:=false;

  ShellListView.SortColumn := -1;
  ShellListView.OnFileAdded:=@ShellListViewFileAdded;
  ShellListView.OnEdited:=@ShellListViewEdited;
  ShellListView.OnShowHint:=@ShellListViewShowHint;
  FHistoryStack := TStringList.Create;

  SpeedButtonViewListHidden.GroupIndex := 2;
  SpeedButtonViewListHidden.Down:=false;

  ShellPathPanel := TShellPathPanel.Create(Self);
  with ShellPathPanel do
  begin
    Parent := LocationEditorPanel;
    Autosize := true;
    BorderSpacing.Left := 8;
    BorderSpacing.Right := 8;
    TabOrder := 0;
    ControlBorderSpacing.InnerBorder := 4;
    BevelOuter:=bvNone;
  end;
  ShellPathPanel.OnSelectionChanged := @ShellPathPanelOnSelectionChanged;
  ComboBoxDir.Visible:=false;

  //ChangeDir( CleanAndExpandDirectory('.') );
  Width := Screen.Width * 2 div 3;
  Height := Screen.Height *2 div 3;
  Resize;
  Translate('');
end;

procedure TFreeFilePreviewDialog.Translate(ALang:string); // '' is default Lang from environment
var GlobalTranslator: TAbstractTranslator; LocalTranslator: TPOTranslator;
    vLang, FallBackLang, LangShortID, POFileName, POFilePath, AppDir : string;
    i:integer;
  function makePath(ADir:string; AFile:string):string;
    begin
      Result := AppDir + ADir + DirectorySeparator + AFile;
      if FileExistsUTF8(Result) then
        exit;
      Result := '';
    end;

begin
  vLang := ALang;
  if vLang = '' then
    for i := 1 to Paramcount - 1 do
      if (ParamStrUTF8(i) = '--LANG') or (ParamStrUTF8(i) = '-l') or
        (ParamStrUTF8(i) = '--lang') then
        Lang := ParamStrUTF8(i + 1);

  //Win32 user may decide to override locale with LANG variable.
  if vLang = '' then
    vLang := GetEnvironmentVariableUTF8('LANG');

  if vLang = '' then
    LazGetLanguageIDs(vLang, FallBackLang);

  LangShortID := copy(vLang, 1, 2);

  AppDir := ExtractFilePath(ParamStrUTF8(0));
  POFileName := 'FreeFilePreviewDialog.'+LangShortID+'.po';
  POFilePath := makePath(FLocaleDir, POFileName);
  if POFilePath = '' then
    POFilePath := makePath('locale', POFileName);
  if POFilePath = '' then
    POFilePath := makePath('languages', POFileName);
  if POFilePath = '' then
    exit;

  LocalTranslator:=TPOTranslator.Create('locale/FreeFilePreviewDialog.'+LangShortID+'.po');
  if assigned(LocalTranslator) then
   begin
    GlobalTranslator := LRSTranslator;
    LRSTranslator := LocalTranslator;
    LocalTranslator.UpdateTranslation(Self);
    FLang := vLang;
    Self.Invalidate;
    //LRSTranslator := GlobalTranslator;
   end;
end;

procedure TFreeFilePreviewDialog.setLang(ALang:string);
begin
  Translate(ALang);
end;

procedure TFreeFilePreviewDialog.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Text := IntToStr(Self.ClientWidth)+' x '+IntToStr(Self.ClientHeight);
end;

procedure TFreeFilePreviewDialog.FormShow(Sender: TObject);
begin
  PairSplitterSideFiles.Width := Width * 4 div 5;
  SaveToolsPanel.Width := NavigationPanel.Width;
end;

procedure TFreeFilePreviewDialog.LabelLocationResize(Sender: TObject);
begin
  LabelName.Constraints.MinWidth := LabelLocation.Width;
end;


procedure TFreeFilePreviewDialog.LeftPageControlChange(Sender: TObject);
begin
end;

procedure TFreeFilePreviewDialog.ListViewPlacesClick(Sender: TObject);
begin
end;

procedure TFreeFilePreviewDialog.ListViewPlacesDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var n,p:string; l:TStringList;
begin
  if ((Sender=ListViewPlaces) and (Source = ShellListView )) then
    begin
      n := ShellListView.Selected.Caption;
      p := ShellListView.GetPathFromItem(ShellListView.Selected);
    end;
  if ((Sender=ListViewPlaces) and (Source = ComboBoxDir)) then
    begin
      p := ComboBoxDir.Text;
      n:='';
    end;

  if not DirPathExists(p) then exit;
  p:=CleanAndExpandDirectory(p);
  if n='' then
    begin
     l := fileicon.Split(p, {System.}DirectorySeparator);
     n := l[l.count-2];
    end;
  ListViewPlaces.AddItem(n, TDir.Create(p) );
end;

procedure TFreeFilePreviewDialog.ListViewPlacesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept :=
  ((Source = ShellListView)
    and DirPathExists(ShellListView.GetPathFromItem(ShellListView.Selected)))
  or
  ((Source = ComboBoxDir) and DirPathExists(ComboBoxDir.Text) )
  ;
end;

procedure TFreeFilePreviewDialog.ListViewPlacesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var dir:TDir;
begin
 if not Selected then exit;
 dir := TDir(Item.Data);
 ChangeDir(dir.fDir);
end;

procedure TFreeFilePreviewDialog.ListViewPlacesShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var dir : TDir; i: integer; item: TListItem;
begin
 if ListViewPlaces.Items.Count = 0 then exit;
 item := ListViewPlaces.GetItemAt(HintInfo^.CursorPos.x,HintInfo^.CursorPos.y);
 if not assigned(item) then exit;
 dir := TDir(item.Data);
 hintInfo^.HintStr:=dir.fDir;
end;

procedure TFreeFilePreviewDialog.FullSizeClick(Sender: TObject);
begin
 if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
 FPreviewImage.AutoSize := true;
 if not FPreviewImage.AutoSize then
   begin
   FitImageIntoScrollBox;
   FPreviewImage.Hint := hintDblClickToFullSize;
   ScrollBoxPreview.Hint := FPreviewImage.Hint;
   end
 else
   begin
   FPreviewImage.Hint := hintDblClickToFitSize;
   ScrollBoxPreview.Hint := FPreviewImage.Hint;
   end;
end;

procedure TFreeFilePreviewDialog.PropertiesClick(Sender: TObject);
var fn,dn,mime,iconName, msg:string;
begin
  fn:=ShellListView.Selected.Caption;
  dn:=ShellListView.Root;
  fn:=dn+fn;
  mime:=FFileMimeIcon.getMimeTypeForFile(fn);
  iconName:=FFileMimeIcon.getMimeToIconName(mime);
  msg:=Format('FileName:%s'+#10+'MIME:%s'+#10+'IconName:%s',[fn,mime,iconName]);
  if FileIsReadable(fn) then msg:=msg+#10+'Readable';
  if FileIsWritable(fn) then msg:=msg+#10+'Writable';
  if FileIsText(fn) then msg:=msg+#10+'Text';
  if FileIsExecutable(fn) then msg:=msg+#10+'Executable';
  if FileIsSymlink(fn) then msg:=msg+#10+'SymLink';
  if FileIsHardLink(fn) then msg:=msg+#10+'HardLink';
  ShowMessage(msg);
end;

procedure TFreeFilePreviewDialog.ShellListViewItemDeleteClick(Sender: TObject);
begin
  ShellListView.Selected.Delete;
end;

procedure TFreeFilePreviewDialog.ShellListViewItemNewFolderClick(Sender: TObject);
begin
  CreateNewFolder;
end;

procedure TFreeFilePreviewDialog.ShellListViewShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var dir : TDir; i: integer; item: TListItem;
begin
 if ShellListView.Items.Count = 0 then exit;
 item := ShellListView.GetItemAt(HintInfo^.CursorPos.x,HintInfo^.CursorPos.y);
 if not assigned(item) then exit;
 //dir := TDir(item.Data);
 hintInfo^.HintStr:=item.Caption;
end;

procedure TFreeFilePreviewDialog.NavigationPanelResize(Sender: TObject);
begin
  SaveToolsPanel.Constraints.MinWidth := NavigationPanel.Width;
end;

procedure TFreeFilePreviewDialog.PlacesMenuItemDeleteClick(Sender: TObject);
begin
  ListViewPlaces.Selected.Delete;
end;

procedure TFreeFilePreviewDialog.PlacesMenuItemRenameClick(Sender: TObject);
begin
  ListViewPlaces.Selected.EditCaption;
end;

procedure TFreeFilePreviewDialog.ShellListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var Name1, Name2: string; Name1IsDir, Name2IsDir: boolean;
  typ1,typ2:string;
  isz1,isz2: longint;
begin
  Name1 := ShellListView.GetPathFromItem(Item1);
  Name2 := ShellListView.GetPathFromItem(Item2);
  Name1IsDir:=DirPathExists(Name1);
  Name2IsDir:=DirPathExists(Name2);
  typ1:=Item1.SubItems[1];
  typ2:=Item2.SubItems[1];

  if ShellListView.SortColumn = 0 then
    begin
    if not Name1IsDir and not Name2IsDir then Compare:= CompareFilenames(Name1,Name2);
    if Name1IsDir and Name2IsDir then Compare:= CompareFilenames(Name1,Name2);
    if Name1IsDir and not Name2IsDir  then Compare:=-1;
    if not Name1IsDir and Name2IsDir  then Compare:=1;
    //Col := ShellListView.Column[ShellListView.SortColumn];
    end;
  if ShellListView.SortColumn = 1 then
    begin
    isz1:=FileSizeUtf8(Name1);
    isz2:=FileSizeUtf8(Name2);
    if not Name1IsDir and not Name2IsDir then Compare:= Math.Sign(isz1-isz2);
    if Name1IsDir and Name2IsDir then Compare:= Math.Sign(isz1-isz2);
    if Name1IsDir and not Name2IsDir  then Compare:=-1;
    if not Name1IsDir and Name2IsDir  then Compare:=1;
    end;
  if ShellListView.SortColumn = 2 then
    begin
    if not Name1IsDir and not Name2IsDir then Compare:= CompareFilenames(typ1,typ2);
    if Name1IsDir and Name2IsDir then Compare:= CompareFilenames(typ1,typ2);
    if Name1IsDir and not Name2IsDir  then Compare:=-1;
    if not Name1IsDir and Name2IsDir  then Compare:=1;
    end;

end;


procedure TFreeFilePreviewDialog.AddToHistory(dir:string);
begin
  if FHistoryStack.Count = 0
     then FHistoryStack.Add(dir)
     else if FHistoryStack[0] <> dir
        then FHistoryStack.Insert(0,dir);
  SpeedButtonGoBack.Hint := FHistoryStack[0];
  SpeedButtonGoBack.Enabled:=true;
end;


procedure TFreeFilePreviewDialog.ChangeDir(dir:string);
var curPath: String;
begin
  if DirPathExists(dir) then
     begin
     dir := CleanAndExpandDirectory(dir);
     curPath := ShellTreeView.Path;
     if curPath = dir then exit;

     if curPath <> '' then AddToHistory(curPath);

     FRoot := ExtractFileRoot(dir);


     if ShellTreeView.Root <> FRoot then ShellTreeView.Root := FRoot;

     Self.Cursor := crHourGlass;
     ShellListView.Cursor := crHourGlass;
     Application.ProcessMessages;

     //ShellListView.Root := dir;
     ShellTreeView.Path := dir;

     ShellListView.Cursor := crDefault;
     Self.Cursor := crDefault;

     ComboBoxDir.Text := dir;
     ShellPathPanel.Path:=dir;
     setSpeedButtonGoUpEnabled(dir);

     FFileName := '';
     if ComboBoxDir.Items.IndexOf(ComboBoxDir.Text) = -1
        then ComboBoxDir.AddItem(ComboBoxDir.Text, nil);

     BitBtnOpen.Enabled:=false;
     end;
end;

procedure TFreeFilePreviewDialog.selectFile;
var fullName : string;
begin
  if not Assigned(ShellListView.Selected) then
     begin
     FFileName := '';
     exit;
     end;
  fullName := ShellListView.GetPathFromItem(ShellListView.Selected);
  if DirPathExists(fullName)
  then
    begin
    //ShellTreeView.Path:=fullName;
    ChangeDir(fullName);
    FFileName := '';
    end
  else
    begin
      FFileName := fullName;
      Close;
    end;
end;

procedure TFreeFilePreviewDialog.ShellListViewDblClick(Sender: TObject);
begin
  selectFile;
end;

procedure TFreeFilePreviewDialog.ShellListViewKeyPress(Sender: TObject;
  var Key: char);
begin
  if (key = #13) or (key = #10) then
     selectFile;
end;

procedure TFreeFilePreviewDialog.ShellListViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var item:TListItem;
  QtTreeWidget: TQtTreeWidget;
  TWI : QTreeWidgetItemH;
  //QtListWidget: TQtListWidget;
  //LWI : QListWidgetItemH;
  wtooltip: widestring;
begin
  // for some reason tooltip is not assigned in setTooltips
  item := ShellListView.GetItemAt(x,y);
  if not assigned(item) then exit;
  wtooltip := item{%H-}.Caption;
  if (ShellListView.ViewStyle = vsReport) then
    begin
      QtTreeWidget:=TQtTreeWidget(ShellListView.Handle);
      TWI := QtTreeWidget.itemAt(x,y);
      QTreeWidgetItem_setToolTip(TWI, 0, @wtooltip);
    end
end;

procedure TFreeFilePreviewDialog.ShellListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (Shift = [ssLeft])
  then Self.FMouseDownTime:=Now
  else Self.FMouseDownTime := maxDouble;
end;

procedure TFreeFilePreviewDialog.ShellListViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (Shift = [])
     and ( Now - Self.FMouseDownTime > cSecunde * 2 )
     and not ShellListView.ReadOnly
  then
     ShellListView.Selected.EditCaption;
  Self.FMouseDownTime := maxDouble;
end;

procedure TFreeFilePreviewDialog.ShellListViewMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var d: integer;
begin
 if ShellListView.ViewStyle <> vsIcon then exit;
 if Shift = [ssCtrl] then
    begin
    if WheelDelta > 0 then d:=16 else d:=-16;
    FIconVIewCellWidth:=FIconVIewCellWidth + d;
    FIconVIewCellHeight:=FIconVIewCellWidth + d;
    Handled:=true;
    ShellListViewSetWordWrap(ShellListView);
    end;
end;

resourcestring
  statusWrongName = 'Wrong name "%s"';
  statusNameAlreadyExists = 'Name "%s" already exists';
  statusFailedToRename = 'Failed to rename "%s" to "%s"';

procedure TFreeFilePreviewDialog.ShellListViewEdited(Sender: TObject; Item: TListItem;
    var AValue: string);
var newName, oldPath, newPath, curPath:String; item2: TListItem;
begin
  newName := AValue;

  if (newName = '') or (newName = '.') or (newName = '..') then
    begin
    AValue:=Item.Caption;
    StatusBar.Panels[0].Text := Format(statusWrongName,[AValue]);
    exit;
    end;

  item2 := ShellListView.FindCaption(0,newName,false,true,true);
  if assigned(item2) then
    begin
    AValue:=Item.Caption;
    StatusBar.Panels[0].Text := Format(statusNameAlreadyExists,[AValue]);
    exit;
    end;

  //curPath := ShellTreeView.Path;
  curPath := ShellListView.Root;
  oldPath := AppendPathDelim(curPath) + Item.Caption;
  newPath := AppendPathDelim(curPath) + newName;
  if not RenameFileUTF8(oldPath, newPath) then
    begin
    AValue:=Item.Caption;
    StatusBar.Panels[0].Text := format(statusFailedToRename, [Item.Caption,Avalue]);
    exit;
    end;

  if ShellTreeView.Selected.Expanded then
    begin
    ShellTreeView.Selected.Collapse(false);
    ShellTreeView.Selected.Expand(false);
    end
  else
    begin
    ShellTreeView.Selected.Expand(false);
    ShellTreeView.Selected.Collapse(false);
    end;
end;

procedure TFreeFilePreviewDialog.setTooltips;
var
  i: integer;
  item:TListItem;
  QtTreeWidget: TQtTreeWidget;
  TWI : QTreeWidgetItemH;
  QtListWidget: TQtListWidget;
  LWI : QListWidgetItemH;
  wtooltip: widestring;
begin
  //ShellListView.Hint := '';
  //ShellListView.showHint := true;

  for i:=0 to ShellListView.Items.Count - 1 do
  begin

  item := ShellListView.Items[i];
  wtooltip := Item{%H-}.Caption;

  if (ShellListView.ViewStyle = vsReport) then
    begin
      ShellListView.ShowHint := false;
      QtTreeWidget:=TQtTreeWidget(ShellListView.Handle);
      TWI := QtTreeWidget.currentItem;
      QTreeWidgetItem_setToolTip(TWI, 0, @wtooltip);
    end
   else
    begin
      ShellListView.ShowHint := false;
      QtListWidget:=TQtListWidget(ShellListView.Handle);
      LWI := QtListWidget.getItem(Item.Index);
      QListWidgetItem_setToolTip(LWI, @wtooltip);
    end;
  end;
end;

procedure TFreeFilePreviewDialog.setSpeedButtonGoUpEnabled(path:String);
begin
  SpeedButtonGoUp.Enabled := (FRoot <> path);
end;

procedure TFreeFilePreviewDialog.ShellListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var isDir: boolean;
begin
  if not Selected then exit;
  FFileName := ShellListView.GetPathFromItem(Item);
  isDir:=DirectoryExistsUTF8(FFileName);

  if Assigned(FOnSelectFile) then FOnSelectFile(Sender, FFilename);
  if not isDir then
    if Assigned(FOnPreview)
      then FOnPreview(Self, FFilename)
      else DefaultPreview(FFilename);

  if not isDir then
    begin
    EditName.Text := Item.Caption;
    BitBtnOpen.Enabled:=true;
    end
  else
    BitBtnOpen.Enabled:=false;
 end;

procedure TFreeFilePreviewDialog.ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if FInShellTreeViewChange then exit;
  FInShellTreeViewChange:=true;
  if ComboBoxDir.Items.IndexOf(ComboBoxDir.Text) = -1
    then ComboBoxDir.AddItem(ComboBoxDir.Text, nil);
  ComboBoxDir.Text := ShellListView.Root;
  if ShellTreeView.Focused then ShellPathPanel.Path:=ShellListView.Root;
  setSpeedButtonGoUpEnabled(ShellListView.Root);
  FInShellTreeViewChange:=false;
end;

procedure TFreeFilePreviewDialog.ShellTreeViewSelectionChanged(Sender: TObject);
begin
  if FInShellTreeViewSelectionChanged then exit;
  FInShellTreeViewSelectionChanged:=true;
  ShellListView.ClearSelection;
  setSpeedButtonGoUpEnabled(ShellListView.Root);
  FInShellTreeViewSelectionChanged:=false;
end;

procedure TFreeFilePreviewDialog.SpeedButtonEditLocationClick(Sender: TObject);
begin
  self.ShellPathPanel.Visible:= not SpeedButtonEditLocation.Down;
  self.ComboBoxDir.Visible:= SpeedButtonEditLocation.Down;
end;

procedure TFreeFilePreviewDialog.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
var path:string; stNode:TShellTreeNode;
begin
 path:=TShellTreeView(Sender).GetPathFromNode(Node);
{$IFDEF WINDOWS}
 Node.ImageIndex := GetWinIconForFile(path, false, SmallImageList);
{$ELSE}
 stNode := nil;
 if Node is TShellTreeNode then
   stNode := TShellTreeNode(Node);

 if assigned(stNode) and stNode.IsDirectory and DirectoryExists(path)
 then addMagicIconForFile(path,Node);

 if assigned(stNode) and not stNode.IsDirectory and FileExists(path)
 then addMagicIconForFile(path,Node);

 //Node.ImageIndex := 0; //folder
 //addMagicIconsForFile(path, Node);
{$ENDIF}
end;


{$IFDEF WINDOWS}
function TFreeFilePreviewDialog.GetWinIconForFile(filename:string; isLarge: boolean;
  imageList:TImageList): integer;
  var
    mIcon: TIcon;
  myIconInfo: TIconInfo;
  FileInfo: SHFILEINFOW;
  pwc: array [0..1024] of WideChar;
  Flags: dword;
  r,i,iconIndex : integer;
  winBitmap: Windows.TBitmap;
  aBmp : Graphics.TBitmap;
  fTypeName: string;
begin
  StringToWideChar(fileName,pwc, length(fileName)*2);

  Flags := SHGFI_ICON or SHGFI_TYPENAME;
  if isLarge then Flags := Flags or SHGFI_LARGEICON else Flags := Flags or SHGFI_SMALLICON;
  r := SHGetFileInfo(@pwc, FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo), Flags);
  fTypeName := WideCharToString( FileInfo.szTypeName );

  // check if icon is registered in iconName to IconIndex map
  i := FIconNamesMap.IndexOf(fTypeName);
  if (i>-1) then
    begin
    iconIndex := PtrUInt(FIconNamesMap.Objects[i]);
    result := iconindex;
    end
  else
    begin
    iconIndex := -1;
    mIcon := TIcon.Create;
    if (assigned(mIcon)) and (FileInfo.hIcon <> 0)
       and (GetIconInfo(FileInfo.hIcon, myIconInfo)) then
      begin
          aBmp := Graphics.TBitmap.Create;
          aBmp.LoadFromBitmapHandles(myIconInfo.hbmColor, myIconInfo.hbmMask, nil); // @r
          mIcon.Assign(aBmp); // there is no other way for TIcons - you MUST use a Bitmap, as it has implemented "Handles"-handling (setHandles)
          // TIcons themselves cannot cope with handles
          aBmp.Free;
          iconIndex :=imageList.AddIcon(mIcon);
          FIconNamesMap.AddObject(fTypeName, TObject(PtrUint(iconIndex)));
          result := iconindex;
      end;
    end;

end;
{$ENDIF}

{$IFNDEF WINDOWS}
procedure TFreeFilePreviewDialog.addMagicIconsForFile(filename:string; Item:TListItem);
var mimeType:string; iconName:string; i,i1,i2,iconIndex:integer;
    smallIcon, largeIcon: TIcon;
begin
  mimeType:=FFileMimeIcon.getMimeTypeForFile(filename);
  iconName:=FFileMimeIcon.getMimeToIconName(mimeType);

  // check if icon is registered in iconName to IconIndex map
  i := FIconNamesMap.IndexOf(iconName);
  if (i>-1) then
    begin
    iconIndex := PtrUInt(FIconNamesMap.Objects[i]);
    Item.ImageIndex:=iconIndex;
    end
  else
    begin
    iconIndex := -1;
    smallIcon := FFileMimeIcon.getIconByName(iconName,16);
    largeIcon := FFileMimeIcon.getIconByName(iconName,32);
    if assigned(smallIcon) and assigned(largeIcon) then
      begin
      i1 :=SmallImageList.AddIcon(smallIcon);
      i2 :=LargeImageList.AddIcon(largeIcon);
      //if (i1<>i2)         then raise Exception.Create('icon indexes differ');
      iconIndex := i1;
      FIconNamesMap.AddObject(iconName, TObject(PtrUint(iconIndex)));
      Item.ImageIndex:=iconIndex;
      end;
    end;
end;

procedure TFreeFilePreviewDialog.addMagicIconForFile(filename:string; Item:TTreeNode);
var mimeType:string; iconName:string; i,i1,iconIndex:integer;
    smallIcon, largeIcon: TIcon;
begin
  try
    mimeType:=FFileMimeIcon.getMimeTypeForFile(filename);
    iconName:=FFileMimeIcon.getMimeToIconName(mimeType);
  finally
  end;
  // check if icon is registered in iconName to IconIndex map
  i := FIconNamesMap.IndexOf(iconName);
  if (i>-1) then
    begin
    iconIndex := PtrUInt(FIconNamesMap.Objects[i]);
    Item.ImageIndex:=iconIndex;
    end
  else
    begin
    iconIndex := -1;
    smallIcon := FFileMimeIcon.getIconByName(iconName,16);
    largeIcon := FFileMimeIcon.getIconByName(iconName,32);
    if assigned(smallIcon) and assigned(largeIcon) then
      begin
      i1 := SmallImageList.AddIcon(smallIcon);
      LargeImageList.AddIcon(largeIcon);
      //if (i1<>i2)         then raise Exception.Create('icon indexes differ');
      iconIndex := i1;
      FIconNamesMap.AddObject(iconName, TObject(PtrUint(iconIndex)));
      Item.ImageIndex:=iconIndex;
      end;
    end;
end;

{$ENDIF}

procedure TFreeFilePreviewDialog.addIconsForFile(filename:string; Item:TListItem);
begin
{$IFDEF WINDOWS}
 Item.ImageIndex := GetWinIconForFile(filename, isLarge, ImageList);
{$ELSE}
 addMagicIconsForFile(filename, Item);
{$ENDIF}
end;

procedure TFreeFilePreviewDialog.setStatusText(AValue:String);
begin
  StatusBar.Panels[0].Text:=AValue;
end;

procedure TFreeFilePreviewDialog.ShellListViewFileAdded(Sender: TObject;
  Item: TListItem);
var fullName: String;
begin
  Item.ImageIndex := ftText;
  fullName := IncludeTrailingPathDelimiter(ShellListView.root) + Item.Caption;

  if FileIsExecutable(fullName) then Item.ImageIndex := ftExecutable;
  if DirPathExists(fullName) then Item.ImageIndex := ftDirectory;
  setStatusText(Item.Caption);
  Application.ProcessMessages;
  addIconsForFile(fullName, Item);
end;

procedure TFreeFilePreviewDialog.ShellListViewItemRenameClick(Sender: TObject);
begin
  ShellListView.Selected.EditCaption;
end;


/// Places
procedure TFreeFilePreviewDialog.addPlace(placeName:string; dirName:string);
var absdir: string;
begin
  absdir := CleanAndExpandDirectory(dirName);
  ListViewPlaces.AddItem(placeName, TDir.Create(absdir));
end;

procedure TFreeFilePreviewDialog.SpeedButtonAddPlaceClick(Sender: TObject);
var absdir, plName: string;
begin
  absdir := CleanAndExpandDirectory(ComboBoxDir.Text);
  plName := ShellTreeView.Selected.Text;
  //PlacesList.AddItem(plName, TDir.Create(absdir));
  ListViewPlaces.AddItem(plName, TDir.Create(absdir));
end;

procedure TFreeFilePreviewDialog.SpeedButtonNewDirClick(Sender: TObject);
begin
  CreateNewFolder;
end;

procedure TFreeFilePreviewDialog.CreateNewFolder;
var curPath, newDir: String; item:TListItem; n:integer;
begin
 //curPath := ShellTreeView.Path;
 {
 EditNewDirName.Text:='New Directory';
 PanelNewDirName.Visible := true;
 EditNewDirName.Enabled := true;
 EditNewDirName.SetFocus;
 EditNewDirName.SelectAll;
 }

 ShellListView.Reload;
 newDir := 'New Folder'; n:=0;
 item:=ShellListView.FindCaption(0,newDir,false,true,true);
 while assigned(item) do
 begin
   n:=n+1;
   newDir := 'New Folder '+IntToStr(n);
   item:=ShellListView.FindCaption(0,newDir,false,true,true);
 end;

 if CreateNewDir(newDir) then
   begin
   ShellListView.Reload;
   item:=ShellListView.FindCaption(0,newDir,false,true,true);
   if assigned(item) then
     begin
     ShellListView.setFocus;
     item.Selected:=true;
     item.EditCaption;
     end;
   end;
end;

{
procedure TFreeFilePreviewDialog.PlacesListShowHint(Sender: TObject;
  HintInfo: PHintInfo);
 var dir : TDir; i: integer;
begin
  if PlacesList.Count = 0 then exit;
  i := PlacesList.ItemAtPos( HintInfo^.CursorPos, true);
  if i<0 then exit;
  dir := TDir(PlacesList.Items.Objects[i]);
  hintInfo^.HintStr:=dir.fDir;
end;
}

procedure TFreeFilePreviewDialog.FitImageIntoScrollBox;
var kI, kS : real;
begin
  kI := FPreviewImage.Picture.Width / FPreviewImage.Picture.Height;
  kS := ScrollBoxPreview.ClientWidth / ScrollBoxPreview.ClientHeight;
  if kI > kS
    then FPreviewImage.Width := ScrollBoxPreview.ClientWidth
    else FPreviewImage.Height := ScrollBoxPreview.ClientHeight;
end;

procedure TFreeFilePreviewDialog.ScrollBoxPreviewResize(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  if FPreviewImage.AutoSize = false then
     FitImageIntoScrollBox;
end;

procedure TFreeFilePreviewDialog.ScrollBoxPreviewDblClick(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  FPreviewImage.AutoSize := not FPreviewImage.AutoSize;
  if not FPreviewImage.AutoSize then
    begin
    FitImageIntoScrollBox;
    FPreviewImage.Hint := hintDblClickToFullSize;
    ScrollBoxPreview.Hint := FPreviewImage.Hint;
    end
  else
    begin
    FPreviewImage.Hint := hintDblClickToFitSize;
    ScrollBoxPreview.Hint := FPreviewImage.Hint;
    end;
end;

procedure TFreeFilePreviewDialog.ScrollBoxPreviewMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var w,h : integer; k: real; s : string;
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  if ScrollBoxPreview.MouseInClient then s:='In' else s:='Out';
  StatusBar.Panels[0].Text := s+' '+IntToStr(MousePos.x) + ' : ' + IntToStr(MousePos.y);

  if MousePos.x < 24 then exit;
  if MousePos.y < 24 then exit;

  k := 1.0;
  if WheelDelta > 0 then k:=1.1;
  if WheelDelta < 0 then k:=0.9;
  w := round(k * FPreviewImage.Width);
  h := round(k * FPreviewImage.Height);
  FPreviewImage.AutoSize := false;
  FPreviewImage.SetBounds(0,0,w,h);
  FPreviewImage.Invalidate;
  Handled := true;
end;


function  TFreeFilePreviewDialog.getSelectedFileName:string;
begin
  result := FFileName;
end;

procedure TFreeFilePreviewDialog.setSelectedFileName(fn:string);
var itm:TListItem;
begin
  FFileName := fn;
  EditName.Text := FFileName;

  if self.FFileDialogMode = fdmOpen then
    begin
    FFileName := '';
    itm:=ShellListView.FindCaption(0,fn, false,false,false,false);
    if assigned(itm) then
      begin
      FFileName := itm.Caption;
      EditName.Text := FFileName;
      itm.Selected:=true;
      ShellListView.Selected:=itm;
      end;
    end;
end;

function  TFreeFilePreviewDialog.getCurrentPath:string;
begin
  result := ShellListView.GetPathFromItem(ShellListView.Selected);
end;

procedure TFreeFilePreviewDialog.setCurrentPath(p:string);
begin
  //ComboBoxDir.Text:=p;
  ChangeDir(p);
end;

function TFreeFilePreviewDialog.getPreviewText:string;
begin
  result := PreviewToolPanel.Caption;
end;

procedure TFreeFilePreviewDialog.setPreviewText(s:string);
begin
  PreviewToolPanel.Caption := s;
end;

function TFreeFilePreviewDialog.getPreviewBitmap:Graphics.TBitmap;
begin
  result := FPreviewImage.Picture.Bitmap;
end;

procedure TFreeFilePreviewDialog.setPreviewBitmap(bmp:Graphics.TBitmap);
begin
  FPreviewImage.Picture.Bitmap.Assign(bmp);
end;

function TFreeFilePreviewDialog.getPreviewPicture:TPicture;
begin
  result := FPreviewImage.Picture;
end;

procedure TFreeFilePreviewDialog.setPreviewPicture(pic:TPicture);
begin
  FPreviewImage.Picture := pic;
end;

procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  ShowMessage(Report);
  Halt; // End of program execution
end;

resourcestring UnableOpenFileMsg = 'Unable open file "%s"';

procedure TFreeFilePreviewDialog.DefaultPreview(fn:string);
begin
  try
    try
       Cursor := crHourGlass;
       ShellListView.Cursor := crHourGlass;
       Application.ProcessMessages;

       FPreviewImage.Picture.LoadFromFile(fn);

       ShellListView.Cursor := crDefault;
       Cursor := crDefault;
       Application.ProcessMessages;

       {PreviewToolPanel.Caption:=IntToStr(FPreviewImage.Picture.Bitmap.Width)+' x '
         + IntToStr(FPreviewImage.Picture.Bitmap.Height);}

       FPreviewImage.AutoSize:=false;
       FitImageIntoScrollBox;
    except
       on EInvalidGraphic do
          begin
          PreviewToolPanel.Caption:='';
          FPreviewImage.Picture.Clear;
          end;
       on EFOpenError do
         ShowMessage(Format(UnableOpenFileMsg,[fn]));
       on E: Exception do
          DumpExceptionCallStack(E);
       else
         Raise;
    end;
  finally
        ShellListView.Cursor := crDefault;
        Cursor := crDefault;
  end;
end;

function TFreeFilePreviewDialog.GetFilter: string;
begin
  result := FilterComboBox1.Filter;
end;

procedure TFreeFilePreviewDialog.SetFilter(const AValue: string);
begin
  FilterComboBox1.Filter := AValue;
end;

function TFreeFilePreviewDialog.GetFilterIndex: integer;
begin
  result := FilterComboBox1.ItemIndex;
end;

procedure TFreeFilePreviewDialog.SetFilterIndex(const AValue: integer);
begin
  FilterComboBox1.ItemIndex := AValue;
end;

function TFreeFilePreviewDialog.Execute: boolean;
begin
  result := (self.ShowModal = mrOK);
end;

procedure TFreeFilePreviewDialog.setFileDialogMode(const AValue: TFileDialogMode);
begin
  FFileDialogMode := AValue;
  if AValue = fdmOpen then
    begin
    SpeedButtonNewDir.Enabled := false;
    EditName.Enabled := false;
    end
  else
  if AValue = fdmSave then
    begin
    SpeedButtonNewDir.Enabled := true;
    EditName.Enabled := true;
    end;
end;

var idnt:string;
// this can be used for internationalization
procedure TFreeFilePreviewDialog.ReplaceLabels(aControl:TControl; fromText:String; toText:String);
var WinControl: TWinControl;   i:integer; idnt0:string;
begin
  writeln(aControl.Name);
  if aControl.Caption = fromText then aControl.Caption := toText;
  if aControl.Hint = fromText then aControl.Hint := toText;
  if aControl is TWinControl
     then WinControl:=TWinControl(aControl)
     else exit;
  idnt0:=idnt; idnt:=idnt+'  ';
  for i:=0 to WinControl.ControlCount-1 do
    ReplaceLabels(WinControl.Controls[i], fromText, toText);
  idnt:=idnt0;
end;

procedure TFreeFilePreviewDialog.ShellPathPanelOnSelectionChanged(Sender: TObject);
var s:string;
begin
 if FInShellPathPanelOnSelectionChanged then exit;
 if not ShellPathPanel.IsSelectionInteractive then exit;
 FInShellPathPanelOnSelectionChanged := true;
 //ChangeDir(ShellPathPanel.SelectedPath); // this causes unknown errors
 //ShellListView.Root:=ShellPathPanel.SelectedPath;
 AddToHistory(ShellListView.Root);
 ShellTreeView.Path:=ShellPathPanel.SelectedPath;
 FInShellPathPanelOnSelectionChanged := false;
end;

end.

