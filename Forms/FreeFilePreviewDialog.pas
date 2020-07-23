unit FreeFilePreviewDialog;

{$mode objfpc}{$H+}
interface

//{$define Magic}

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics,
  Dialogs, ComCtrls, EditBtn,
  //ShellCtrls, FileCtrl,
  FreeShellCtrls,
  StdCtrls, Buttons,
  ExtCtrls, PairSplitter, Menus, ActnList, FileCtrl, Math,
  FileIcon,
  {$IFDEF WINDOWS}
  FileIconWin,
  {$ELSE}
    {$IFDEF Magic}
      FileIconMagic,
    {$ELSE}
     {$ifdef LCLGTK2}
      FileIconGtk,
     {$endif}
     {$ifdef LCLGTK3}
       FileIconGtk3,
     {$endif}
    {$ENDIF}
  {$ENDIF}
  Types
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
    Panel7: TPanel;
    Properties: TMenuItem;
    PreviewPopupMenu: TPopupMenu;
    ShellPathPanel: TShellPathPanel;
    BottomPanel: TPanel;
    EditName: TEdit;
    FilterComboBox1: FreeShellCtrls.TFilterComboBox;
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
    FileEditPanel: TPanel;
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
    ShellTreeView: FreeShellCtrls.TShellTreeView;
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
    ReloadTimer: TTimer;
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
    procedure ComboBoxDirChange(Sender: TObject);
    procedure ComboBoxDirEditingDone(Sender: TObject);
    procedure ComboBoxDirExit(Sender: TObject);
    procedure ComboBoxDirKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBoxDirKeyPress(Sender: TObject; var Key: char);
    procedure EditNameEditingDone(Sender: TObject);
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
    procedure LocationEditorPanelClick(Sender: TObject);
    procedure PropertiesClick(Sender: TObject);
    procedure ReloadTimerTimer(Sender: TObject);
    procedure ShellListViewClick(Sender: TObject);
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
    procedure ShellListViewLoaded(Sender: TObject);
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
    procedure ShellPathPanelOnClick(Sender: TObject);

//     self.ComboBoxDir.Visible:=false;
// self.ShellPathPanel.Visible:=true;

    //procedure Timer1StopTimer(Sender: TObject);
    //procedure Timer1Timer(Sender: TObject);
 private
    { private declarations }
    FAutoFit: boolean;
    FIsShowing: boolean;
    FRoot : String;
    FPath : String;
    FFileName : string;
    FIconNamesMap: TStringList;
    //FMousePos: TPoint;
    FOnSelectFile: TSelectFileEvent;
    FOnPreview: TSelectFileEvent;
    FHistoryStack: TStringList;
    FFileDialogMode : TFileDialogMode;
    FFileIcon:TFileIconAdapter;
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
    FListHidden:boolean;
    //FPendingDir:string;
    FNeedsReload:boolean;
    FIsInReload:boolean;
    //function addAnyIconsForFile(filename:string):integer;
    procedure addIconsForFile(filename:string; item:TListItem);

    function  getSelectedAbsoluteFileName:string;
    procedure setSelectedAbsoluteFileName(fn:string);
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
    procedure ShellListViewSetWordWrap(slv: TCustomShellListView; AWordWrap:boolean);
    procedure setSpeedButtonGoUpEnabled(path:String);
    procedure setFileDialogMode(const AValue: TFileDialogMode);
    procedure setLang(ALang:string);
    procedure setListHidden(val:boolean);
    procedure setLocaleDir(val: string);

    function CreateNewDir(aName:String):boolean;
    procedure CreateNewFolder;
    procedure AddToHistory(dir:string);
    procedure setAutoFit(val:boolean);
 public
    procedure DoAutoFit;
    procedure Activate; override;
    procedure DoShow; override;
    function ShowModal: Integer; override;
    function Execute: boolean;
    procedure AddPlace(placeName:string; dirName:string);
    procedure ChangeDir(dir:string);
    procedure FitPreviewImage;
    procedure ReplaceLabels(aControl:TControl; fromText:String; toText:String); // search and replace a label for control and its children
    procedure SetStatusText(AValue:String);
    procedure Translate(ALang:string); // '' is default Lang from environment  published
    procedure ScheduleReload(aDir:string); // schedules asynchronous reload
    procedure Reload; // reloads TreeView, ListView and ButtonPath. called from timer when scheduled

    property AutoFit: boolean read FAutoFit write setAutoFit;
    property AbsoluteFileName:string read getSelectedAbsoluteFileName write setSelectedAbsoluteFileName;
    property CurrentPath:string read getCurrentPath write setCurrentPath;
    property FileName:string read getSelectedFileName write setSelectedFileName;
    property Filter: String read getFilter write setFilter;
    property FilterIndex: integer read getFilterIndex write setFilterIndex;
    property FileDialogMode: TFileDialogMode read FFileDialogMode write setFileDialogMode;
    property LocaleDir: String read FLocaleDir write setLocaleDir;
    property Lang: String read FLang write setLang;
    property ListHidden: boolean read FListHidden write setListHidden;
    property OnSelectFile: TSelectFileEvent read FOnSelectFile write FOnSelectFile;
    property OnPreview: TSelectFileEvent read FOnPreview write FOnPreview;
    property PreviewImage: TImage read FPreviewImage;
    property PreviewPicture: TPicture read getPreviewPicture write setPreviewPicture;
    property PreviewBitmap: Graphics.TBitmap read getPreviewBitmap write setPreviewBitmap;
    property PreviewText: String read getPreviewText write setPreviewText;
  end;

implementation
uses LCLType,
  LResources, LCLTranslator, LazUTF8 // for translations
  {$IfDef QT}
  ,qtwidgets  ,qt4
  //  ,qt5
  {$EndIf}
  {$ifdef LCLGTK2}
  ,WSProc, Gtk2, GLib2, Pango, Gtk2WSComCtrls, Gtk2Def, Gtk2Proc
  {$endif}
  ,FreeLogger
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

{$ifdef LCLGTK2}

// this was not published in LCL. Copied here.
procedure GetCommonTreeViewWidgets(ATreeViewHandle: PGtkWidget;
  out TVWidgets: PTVWidgets);
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo(ATreeViewHandle);
  TVWidgets := PTVWidgets(WidgetInfo^.UserData);
end;

// this was not defined in LCL
function gtk_cell_layout_get_cells(cell_layout:PGtkCellLayout):PGlist;
          cdecl; external gtklib name 'gtk_cell_layout_get_cells';

procedure gtk_cell_renderer_set_alignment(cell: PGtkCellRenderer;
          xalign: gfloat; yalign: gfloat); cdecl; external;

procedure gtk_cell_renderer_set_padding(cell:PGtkCellRenderer; xpad:gint; ypad:gint);
          cdecl; external gtklib;

function Gtk2_GtkIconView_GetTextRenderer(icon_view:PGtkIconView):PGtkCellRendererText;
var renderers, renderers_list: PGList;
  renderer: PGtkCellRenderer;
begin
  renderers_list := gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(icon_view));
  renderers := renderers_list;
  while (renderers <> nil) and (renderers^.data <> nil) do
  begin
     renderer := renderers^.data;
     if GTK_IS_CELL_RENDERER_TEXT(renderer) then
     begin
       result := GTK_CELL_RENDERER_TEXT(renderer);
       break;
     end;
     if renderers^.next = nil then break;
     renderers := renderers^.next;
  end;
  g_list_free(renderers_list);
end;

function Gtk2_GtkIconView_GetPixRenderer(icon_view:PGtkIconView):PGtkCellRenderer;
var renderers, renderers_list: PGList;
  renderer: PGtkCellRenderer;
begin
  renderers_list := gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(icon_view));
  renderers := renderers_list;
  while (renderers <> nil) and (renderers^.data <> nil) do
  begin
     renderer := renderers^.data;
     if GTK_IS_CELL_RENDERER(renderer) then
     begin
       result := GTK_CELL_RENDERER(renderer);
       break;
     end;
     if renderers^.next = nil then break;
     renderers := renderers^.next;
  end;
  g_list_free(renderers_list);
end;

procedure Gtk2WSCustomListView_IconView_SetWordWrap(const ALV: TCustomListView;
  const AWordWrap: Boolean; AWrapWidth:integer );
var
  Widgets: PTVWidgets;
  icon_view: PGtkIconView;
  text_renderer : PGtkCellRendererText;
  pix_renderer : PGtkCellRenderer;
  //gfXAlign: Gfloat;
  iw:Gint;
  //giXPad:Gint;
begin
 if not WSCheckHandleAllocated(ALV, 'TListViewTestForm')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_ICON_VIEW(Widgets^.MainView)
  then Exit;

  icon_view := PGtkIconView(Widgets^.MainView);

  text_renderer := Gtk2_GtkIconView_GetTextRenderer(icon_view);
  pix_renderer  := Gtk2_GtkIconView_GetPixRenderer(icon_view);

  if text_renderer = nil then exit;

  if AWordWrap then
    begin
    gtk_icon_view_set_item_width(icon_view, AWrapWidth);
    gtk_cell_renderer_set_alignment(GTK_CELL_RENDERER(text_renderer), 0.5, 0.5);

    g_object_set(G_OBJECT(text_renderer),
      PChar('alignment'), [gpointer(ptrint(PANGO_ALIGN_CENTER)),
      PChar('wrap-mode'),  gpointer(ptrint(PANGO_WRAP_WORD_CHAR)),
      PChar('wrap-width'), gpointer(ptrint(AWrapWidth)), nil]);

    //g_object_set(G_OBJECT(text_renderer), PChar('background'), [PChar('lightgray'),nil]);
    //g_object_set(G_OBJECT(text_renderer), PChar('width'), [gpointer(ptrint(160)),nil]); // CAUSES Memory ERROR
    //gtk_cell_renderer_set_fixed_size(GTK_CELL_RENDERER(text_renderer), AWrapWidth, -1); // CAUSES Memory ERROR
    //gtk_cell_renderer_set_padding(GTK_CELL_RENDERER(text_renderer), 0,0);

    gtk_cell_renderer_set_alignment(pix_renderer, 0.5, 0.0);
    gtk_cell_renderer_set_fixed_size(pix_renderer, AWrapWidth, -1);
    gtk_cell_renderer_set_padding(pix_renderer, 0,0);
    end
  else
    begin
    iw:=gtk_icon_view_get_item_width(icon_view);
    gtk_icon_view_set_item_width(icon_view, -1);
    gtk_cell_renderer_set_alignment(GTK_CELL_RENDERER(text_renderer), 0.0, 0.0);
    g_object_set(G_OBJECT(text_renderer),
      PChar('alignment'), [gpointer(ptrint(PANGO_ALIGN_LEFT)),
      PChar('wrap-mode'),  gpointer(ptrint(PANGO_WRAP_CHAR)),
      PChar('wrap-width'), gpointer(ptrint(-1)), nil]);
    end;
end;

{$endif}

procedure TFreeFilePreviewDialog.ShellListViewSetWordWrap(slv:TCustomShellListView; AWordWrap:boolean);
var //gSz : TSize; ih,fh:integer; fd: TFontData;
  {$IfDef QT}  QtListWidget: TQtListWidget;  {$EndIf}
  AWrapWidth:integer = 200;
  c:TListColumn;
begin
  if AWordWrap then
    begin
    c:=slv.Column[0];
    c.Alignment:=taCenter;
    c.Width:=AWrapWidth;
    c.MaxWidth:=AWrapWidth;
    end
  else
    begin
    c:=slv.Column[0];
    c.Alignment:=taLeftJustify;
    c.Width:=0;
    c.MaxWidth:=0;
    end;

  {$IfDef LCLGTK2}
  Gtk2WSCustomListView_IconView_SetWordWrap(slv, AWordWrap, 160);
  {$EndIf}

  {$IfDef QT}
  QtListWidget := TQtListWidget(slv.Handle);
  if ShellListView.ViewStyle=vsSmallIcon then
     ih:=SmallImageList.Height;
  if ShellListView.ViewStyle=vsIcon then
     ih:=LargeImageList.Height;
  {$IFNDEF WINDOWS}
  fd := GetFontData(ShellListView.Font.Handle);
  fh:=fd.Height;
  {$ELSE}
  fh := 20;
  {$ENDIF}
  if fh=0 then fh:=16;
  if FIconViewCellWidth = 0 then FIconViewCellWidth := 160;
  if FIconVIewCellHeight = 0 then FIconVIewCellHeight := ih + fh + 4;
  gSz := Size(FIconVIewCellWidth, FIconVIewCellHeight);
  //gSz := Size(128, ih + fh + 4);
  QtListWidget.GridSize:=gSz;
  QtListWidget.setWrapping(true);
  QtListWidget.setWordWrap(true);
  {$EndIf}

  slv.Invalidate;
  slv.Refresh;
end;

procedure TFreeFilePreviewDialog.Reload;
var old,dir:string;
begin
  FIsInReload := true;
  FNeedsReload := false;
  dir:=FPath;
  old:=ShellTreeView.Path;
  ShellTreeView.Path:=FPath;
  ShellTreeView.Selected;
  sleep(100); // to prevent racing with ShellTreeView.TreeView.Timer
  {If there is hidden dir in path, the ShellTreeView.Path will be
  the parent of a hidden dir. It will be selected.
  The node needs to be collapsed and Path re-assigned }
  if ShellTreeView.Path <> dir then
  begin
   ShellTreeView.Selected.Collapse(false);
   ShellTreeView.Path := dir;
   sleep(100); // to prevent racing with ShellTreeView.TreeView.Timer
  end;

  //if FFileName <> '' then
     FileName := FFileName;

  FIsInReload := false;
end;

procedure TFreeFilePreviewDialog.ScheduleReload(aDir:string);
begin
  if ShellListView.IsLoading and (FPath <> aDir) then
  begin
     ShellListView.AbortLoading;
     ShellTreeView.AbortLoading;
  end;
  FNeedsReload := true;
  FPath := aDir;
end;

procedure TFreeFilePreviewDialog.ReloadTimerTimer(Sender: TObject);
begin
  if FNeedsReload then
    begin
    if not ShellListView.IsLoading then
        Reload;
    end;

  if ShellListView.IsLoading then
    begin
    //Screen.Cursor := crHourGlass;
    //Self.Cursor := crHourGlass;
    ShellListView.Cursor := crHourGlass;
    application.processmessages;
    end
  else
    begin
    //Screen.Cursor := crDefault;
    //Self.Cursor := crDefault;
    ShellListView.Cursor := crDefault;
    application.processmessages;
    end;

end;

procedure TFreeFilePreviewDialog.ShellListViewClick(Sender: TObject);
var i:integer;
begin
 i:=10;
end;


procedure TFreeFilePreviewDialog.SpeedButtonViewDetailsClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsReport;
  ShellListView.Column[0].MaxWidth:=1000;
  ShellListViewSetWordWrap(ShellListView, false);
  setTooltips;
end;

procedure TFreeFilePreviewDialog.setListHidden(val:boolean);
  var ottv,otlv: FreeShellCtrls.TObjectTypes;
    //P:string;
begin
  if FListHidden = val then exit;
  FListHidden := val;
  SpeedButtonViewListHidden.Down := val;

  ottv:=ShellTreeView.ObjectTypes;
  if val
    then include(ottv, otHidden)
    else exclude(ottv, otHidden);

  otlv:=ShellListView.ObjectTypes;
  if val
    then include(otlv, otHidden)
    else exclude(otlv, otHidden);

  ShellTreeView.ObjectTypes:=ottv;
  ShellListView.ObjectTypes:=otlv;
  //ShellListView.Reload;
  ScheduleReload(FPath);
end;

procedure TFreeFilePreviewDialog.setLocaleDir(val: string);
begin
  if FLocaleDir = val then exit;
  FLocaleDir := val;
  translate(FLang);
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListHiddenClick(Sender: TObject
  );
//var ot: TObjectTypes;
begin
  setListHidden(SpeedButtonViewListHidden.Down)
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListSimpleClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsList;
  ShellListView.Column[0].MaxWidth:=1000;
  ShellListViewSetWordWrap(ShellListView, false);
  setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListCompactClick(Sender: TObject);
begin
    ShellListView.ViewStyle:=vsSmallIcon;
    ShellListViewSetWordWrap(ShellListView, true);
    setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonViewListLargeClick(Sender: TObject);
begin
  ShellListView.ViewStyle:=vsIcon;
  //ShellListView.Column[0].MaxWidth:=100;
  ShellListViewSetWordWrap(ShellListView, true);
  setTooltips;
end;

procedure TFreeFilePreviewDialog.SpeedButtonGoBackClick(Sender: TObject);
var bkDir: String;
begin
  if FHistoryStack.Count > 0 then
    begin
    bkDir := FHistoryStack[0];
    FHistoryStack.Delete(0);

    //self.ChangeDir(bkDir);
    ScheduleReload(bkDir);

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
    //ChangeDir(parentDir);
    ScheduleReload(parentDir);

    parentDir2 := ExpandFileName(IncludeTrailingPathDelimiter(ShellListView.Root) + '..');

    SpeedButtonGoUp.Enabled := (parentDir <> parentDir2);
end;

resourcestring rsLocation_does_not_exist = 'Location does not exist';

procedure TFreeFilePreviewDialog.ComboBoxDirEditingDone(Sender: TObject);
var loc,dir,fil:string;
begin
  if not ComboBoxDir.Focused then exit;
  if FInAutoSearchListBox then exit;
  AutoSearchListBox.Visible:=false;
  //ChangeDir( ComboBoxDir.Text );
  //TODO clean and check path
  loc := ComboBoxDir.Text;
  dir := ExtractFilePath(loc);
  fil := ExtractFileName(loc);
  if (fil='') then
    if DirectoryExistsUTF8(dir) then
      begin
      dir := CleanAndExpandDirectory(dir);
      ScheduleReload(dir)
      end
    else ShowMessage(rsLocation_does_not_exist)
  else
    if FileExistsUTF8(loc) then
      begin
      dir := CleanAndExpandDirectory(dir);
      FFileName := fil;
      ScheduleReload(dir);
      //ComboBoxDir.Text := dir;
      //loc := CleanAndExpandFilename(loc);
      //EditName.Text:=fil;
      //FileName := fil;
      end
    else ShowMessage(rsLocation_does_not_exist);
end;

procedure TFreeFilePreviewDialog.ComboBoxDirExit(Sender: TObject);
begin
 if SpeedButtonEditLocation.Down then exit;
 self.ComboBoxDir.Visible:=false;
 self.ShellPathPanel.Visible:=true;
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
//var s:string;
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
  if (Key = VK_ESCAPE) then
    begin
    ComboBoxDirExit(Sender);
    Key := VK_UNKNOWN;
    end;
  if (Key = VK_RETURN) then
    begin
    ComboBoxDirEditingDone(sender);
    end;

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

function split(Input: string; const Delimiter: Char):TStringList;
var Strings:TStringList;
begin
   Strings:=TStringList.Create;
   Assert(Assigned(Strings)) ;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
   result:=Strings;
end;

procedure TFreeFilePreviewDialog.ComboBoxDirChange(Sender: TObject);
var nm,s,cd:string; sl: TStringList;
    item:TListItem; i,h:integer;
    //lb:TListBox;
    fi:TFileItem;
    P1:TPoint;
begin
  if FIsInReload then exit;
  //if not ComboBoxDir.Focused then exit;
  //if trim(ComboBoxDir.Text)='' then exit;
  sl := split(ComboBoxDir.Text, {System.}DirectorySeparator);
  if sl.count = 0 then
    begin
    AutoSearchListBox.Clear;
    AutoSearchListBox.Visible:=false;
    exit;
    end;

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
var nm,s:string; sl: TStringList;
    item:TListItem; i,h:integer;
    //lb:TListBox;
    fi:TFileItem;
begin
  //inherited KeyPress(Key);
  sl := split(ComboBoxDir.Text, {System.}DirectorySeparator);
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

procedure TFreeFilePreviewDialog.EditNameEditingDone(Sender: TObject);
begin
  FFileName := trim(EditName.Text);
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
  FileName := EditName.Text;
end;

procedure TFreeFilePreviewDialog.FitSizeClick(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  FPreviewImage.AutoSize := false;
  if not FPreviewImage.AutoSize then
    begin
    FitPreviewImage;
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

resourcestring
  rsCancelButtonCaptionCancel = 'Cancel';
  rsClickToEdit = 'Click to edit';

procedure TFreeFilePreviewDialog.FormCreate(Sender: TObject);
//var GlobalTranslator: TAbstractTranslator;  LocalTranslator: TPOTranslator;
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
    FFileIcon:=TFileIconWin.Create(SmallImageList, LargeImageList);
  {$ELSE}
    {$IFDEF Magic}
      FFileIcon:=TFileMimeIcon.Create(SmallImageList, LargeImageList);
    {$ELSE}
      FFileIcon:=TFileIconGtk.Create(SmallImageList, LargeImageList);
    {$ENDIF}
  {$ENDIF}

  FIconNamesMap:= TStringList.Create;

  FListBox1ControlledByKeys:=false;

  ShellTreeView.PopulateWithbaseFiles();
  ShellTreeView.FileSortType:=fstAlphabet; //, fstFoldersFirst);

  //ShellListView.AutoWidthLastColumn:=false;
  ShellListView.SortColumn := -1;
  ShellListView.OnFileAdded:=@ShellListViewFileAdded;
  ShellListView.OnLoaded:=@ShellListViewLoaded;
  ShellListView.OnEdited:=@ShellListViewEdited;
  ShellListView.OnShowHint:=@ShellListViewShowHint;
  FHistoryStack := TStringList.Create;

  SpeedButtonViewListHidden.GroupIndex := 2;
  SpeedButtonViewListHidden.Down:=false;

  BitBtnCancel.Caption:= rsCancelButtonCaptionCancel;

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
    ControlPanel.Hint := rsClickToEdit;
    ControlPanel.ShowHint:=true;
    //Color:=clLime;
  end;
  ShellPathPanel.OnSelectionChanged := @ShellPathPanelOnSelectionChanged;
  ShellPathPanel.ControlPanel.onClick := @ShellPathPanelOnClick;
  ComboBoxDir.Visible:=false;

  //ChangeDir( CleanAndExpandDirectory('.') );
  Width := Screen.Width * 2 div 3;
  Height := Screen.Height *2 div 3;
  Resize;
  Translate(FLang);
end;

procedure TFreeFilePreviewDialog.Translate(ALang:string); // '' is default Lang from environment
var GlobalTranslator: TAbstractTranslator; LocalTranslator: TPOTranslator;
    vLang, FallBackLang, LangShortID, POFileName, POFilePath, AppDir : string;
    i:integer;

    function makePath(ADir:string; AFile:string):string;
    begin
      Result := AFile;
      if ADir>'' then
        Result := ADir + DirectorySeparator + AFile;
      if FileExistsUTF8(Result) then
        exit;
      logger.Debug({$include %file%}+':'+{$include %line%}+': File not found: '+Result);
      Result := '';
    end;

begin
  vLang := ALang;
  if vLang = '' then
    for i := 1 to Paramcount - 1 do
      if (ParamStrUTF8(i) = '--LANG') or (ParamStrUTF8(i) = '-l') or
        (ParamStrUTF8(i) = '--lang') then
        FLang := ParamStrUTF8(i + 1);

  //Win32 user may decide to override locale with LANG variable.
  if vLang = '' then
    vLang := GetEnvironmentVariableUTF8('LANG');

  if vLang = '' then
    LazGetLanguageIDs(vLang, FallBackLang);

  LangShortID := copy(vLang, 1, 2);

  POFileName := 'FreeFilePreviewDialog.'+LangShortID+'.po';
  logger.Debug({$include %file%}+':'+{$include %line%}+': POFileName: '+POFileName);

  logger.Debug({$include %file%}+':'+{$include %line%}+': LocaleDir: '+FLocaleDir);
  POFilePath := makePath(FLocaleDir, POFileName);
  logger.Debug({$include %file%}+':'+{$include %line%}+': PO FilePath: '+POFilePath);

  if POFilePath = '' then
  begin
    AppDir := ExtractFilePath(ParamStrUTF8(0));
    logger.Debug({$include %file%}+':'+{$include %line%}+': AppDir: '+AppDir);
    POFilePath := makePath(AppDir+directorySeparator+'locale', POFileName);
  end;
  if POFilePath = '' then
    POFilePath := makePath(AppDir+directorySeparator+'languages', POFileName);
  if POFilePath = '' then
    exit;
  logger.Debug({$include %file%}+':'+{$include %line%}+' :PO FilePath: '+POFilePath);

  try
    LocalTranslator:=TPOTranslator.Create(POFilePath);
  except on E : Exception do
    begin
    LocalTranslator := nil;
    logger.Error(Format('Trying create translator from %s :\n Error: %s',[POFilePath, E.Message]));
    logger.LogExceptionCallStack(E);
    end;
  end;

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
  if FLang = ALang then exit;
  FLang := ALang;
  Translate(ALang);
end;

procedure TFreeFilePreviewDialog.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Text := IntToStr(Self.ClientWidth)+' x '+IntToStr(Self.ClientHeight);
  ShellListView.Constraints.MaxWidth:=Width - self.ShellTreeView.Width - 100;
  FitPreviewImage;
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
var //item:TListItem;
  i:integer;
begin
  {for i:=0 to ListViewPlaces.Items.Count-1 do
     begin
     ListViewPlaces.Items[i].Selected := false;
     ListViewPlaces.Items[i].Focused := false;
     end;
  ListViewPlaces.Selected := nil;}
  ListViewPlaces.ItemFocused := nil;
  ListViewPlaces.ClearSelection;
  LeftPageControl.ActivePage.SetFocus;
  Application.ProcessMessages;

if LeftPageControl.ActivePage = TabSheetPlaces then
 begin
 for i:=0 to ListViewPlaces.Items.Count-1 do
   if TDir(ListViewPlaces.Items[i].Data).fDir = FPath
   then
    begin
    ListViewPlaces.Selected := ListViewPlaces.Items[i];
    ListViewPlaces.Items[i].Selected := true;
    ListViewPlaces.Items[i].Focused := true;
    ListViewPlaces.ItemFocused := ListViewPlaces.Items[i];
    end;
 end;

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
     l := split(p, {System.}DirectorySeparator);
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

procedure TFreeFilePreviewDialog.ListViewPlacesClick(Sender: TObject);
var Item: TListItem; dir:TDir;
begin
  Item:=ListViewPlaces.Selected;
  if Item = nil then exit;
  dir := TDir(Item.Data);
  ScheduleReload(dir.fDir);
end;

procedure TFreeFilePreviewDialog.ListViewPlacesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var dir:TDir;
begin
 if not Selected then exit;
 dir := TDir(Item.Data);
 ScheduleReload(dir.fDir);
end;


procedure TFreeFilePreviewDialog.ListViewPlacesShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var dir : TDir; //i: integer;
  item: TListItem;
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
   FitPreviewImage;
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
var fn,dn, msg:string;
  {$ifdef USEMIME}iconName, mime:string;{$endif}
begin
  fn:=ShellListView.Selected.Caption;
  dn:=ShellListView.Root;
  fn:=dn+fn;
  msg:=Format('FileName:%s'+#10,[fn]);
  {$IFNDEF WINDOWS}
    {$ifdef USEMIME}
    mime:=FFileMimeIcon.getMimeTypeForFile(fn);
    iconName:=FFileMimeIcon.getMimeToIconName(mime);
    msg:=msg + Format('MIME:%s'+#10+'IconName:%s',[mime,iconName]);
    {$endif}
  {$ENDIF}
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
var item: TListItem;
begin
 if ShellListView.Items.Count = 0 then exit;
 item := ShellListView.GetItemAt(HintInfo^.CursorPos.x,HintInfo^.CursorPos.y);
 if not assigned(item) then exit;
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

{
procedure TFreeFilePreviewDialog.Timer1Timer(Sender: TObject);
begin
   if not ShellListView.IsLoading then
     Timer1.Enabled:=false;
end;

procedure TFreeFilePreviewDialog.Timer1StopTimer(Sender: TObject);
begin
   ChangeDir(FPendingDir);
end;
}

procedure TFreeFilePreviewDialog.ChangeDir(dir:string);
var curPath: String;
begin
  if not DirPathExists(dir) then exit;
  dir := CleanAndExpandDirectory(dir);
  curPath := ShellTreeView.Path;
  if curPath = dir then exit;

  { When current dir is loading and user changed it to an another dir
   the new dir is saved in FPendingDir and the current loading is set to be
   aborted.
   Then Timer1 is started and this flow exits.
   Timer1 is running until the current loading is actually aborted.
   Then the timer calls ChangeDir(FPendingDir)
  if ShellListView.IsLoading then
   begin
   FPendingDir:=dir;
   ShellListView.AbortLoading;
   Timer1.Enabled:=true;
   exit;
   end;
   }

  if curPath <> '' then AddToHistory(curPath);

  FPath := dir;
  FRoot := ExtractFileRoot(dir);

  if not FIsShowing
   then exit;

  ScheduleReload(dir);

  exit;


  //commented because it removes other drives in Windows, like D: etc
  //if ShellTreeView.Root <> FRoot
  //  then ShellTreeView.Root := FRoot;

  Self.Cursor := crHourGlass;
  ShellListView.Cursor := crHourGlass;
  Application.ProcessMessages;

  //ShellListView.Root := dir;
  ShellTreeView.Path := dir;
  {If there is hidden dir in path, the ShellTreeView.Path will be
  the parent of a hidden dir. It will be selected.
  The node needs to be collapsed and Path re-assigned }
  if ShellTreeView.Path <> dir then
  begin
   ShellTreeView.Selected.Collapse(false);
   ShellTreeView.Path := dir;
  end;

  //if otHidden in ShellTreeView.ObjectTypes
  //  then SpeedButtonViewListHidden.Down:=true

  ShellListView.Cursor := crDefault;
  Self.Cursor := crDefault;

  ComboBoxDir.Text := dir;
  ShellPathPanel.Path := dir;
  setSpeedButtonGoUpEnabled(dir);

  if ComboBoxDir.Items.IndexOf(ComboBoxDir.Text) = -1
    then ComboBoxDir.AddItem(ComboBoxDir.Text, nil);

  if (FileDialogMode = fdmOpen) then
   FFileName := '';
  BitBtnOpen.Enabled:=false;
  if (FileDialogMode = fdmSave) and (self.FFileName <> '') then
   BitBtnOpen.Enabled:=true;

end;

procedure TFreeFilePreviewDialog.selectFile;
var fullName: string; item:TListItem; fileItem:TFileItem;
begin
  if not Assigned(ShellListView.Selected) then
     begin
     FFileName := '';
     exit;
     end;
  item:=ShellListView.Selected;
  fileItem:=TFileItem(item.Data);
  fullName := ShellListView.GetPathFromItem(ShellListView.Selected);
  if not fileItem.isFolder
  then
     begin
     if not FileExistsUTF8(fullName) then exit;
     FPath := fileItem.BasePath;
     FFileName := fileItem.FileName;
     self.ModalResult := mrOk;
     //Close;
     end
  else
     begin
     if not DirectoryExistsUTF8(fullName) then exit;
     FFileName := '';
     ScheduleReload(fullName);
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
//var item:TListItem;
  {$IfDef QT}
  var
  QtTreeWidget: TQtTreeWidget;
  TWI : QTreeWidgetItemH;
  //QtListWidget: TQtListWidget;
  //LWI : QListWidgetItemH;
  {$EndIf}
  //wtooltip: widestring;
begin
  {$IfDef QT}
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
  {$EndIf}
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
  {$IfDef QT}
  QtTreeWidget: TQtTreeWidget;
  TWI : QTreeWidgetItemH;
  QtListWidget: TQtListWidget;
  LWI : QListWidgetItemH;
  {$EndIf}
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
      {$IfDef QT}
      ShellListView.ShowHint := false;
      QtTreeWidget:=TQtTreeWidget(ShellListView.Handle);
      TWI := QtTreeWidget.currentItem;
      QTreeWidgetItem_setToolTip(TWI, 0, @wtooltip);
      {$Else}
      ShellListView.ShowHint := true;
      ShellListView.Hint := Item.Caption;
      {$EndIf}
    end
   else
    begin
      {$IfDef QT}
      ShellListView.ShowHint := false;
      QtListWidget:=TQtListWidget(ShellListView.Handle);
      LWI := QtListWidget.getItem(Item.Index);
      QListWidgetItem_setToolTip(LWI, @wtooltip);
      {$Else}
      ShellListView.ShowHint := true;
      ShellListView.Hint := Item.Caption;
      {$EndIf}
    end;
  end;
end;

procedure TFreeFilePreviewDialog.setSpeedButtonGoUpEnabled(path:String);
begin
  SpeedButtonGoUp.Enabled := (FRoot <> path);
end;

procedure TFreeFilePreviewDialog.ShellListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var isFile: boolean; pat: string; fileItem:TFileItem;
begin
  if not Selected then exit;
  pat := ShellListView.GetPathFromItem(Item);
  if not FileExistsUTF8(pat) then exit;
  fileItem := TFileItem(Item.Data);
  isFile := (not fileItem.isFolder);

  if Assigned(FOnSelectFile) then FOnSelectFile(Sender, pat);

  if isFile then
    if Assigned(FOnPreview)
      then
        try
           FOnPreview(Self, pat);
        except
           on e: Exception do
              DefaultPreview(pat);
        end
      else DefaultPreview(pat);

  if isFile then
    begin
    FFileName := fileItem.FileName;
    EditName.Text := fileItem.FileName;
    BitBtnOpen.Enabled:=true;
    end
  else
    BitBtnOpen.Enabled:=false;

 end;

procedure TFreeFilePreviewDialog.ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
var SelNode: TTreeNode; t,p:string;
begin
  if FInShellTreeViewChange then exit;
  FInShellTreeViewChange:=true;
  if ShellTreeView.IsLoading then exit;
  if ComboBoxDir.Items.IndexOf(ComboBoxDir.Text) = -1
    then ComboBoxDir.AddItem(ComboBoxDir.Text, nil);
  //logger.Debug('FPathB:'+FPath);
  SelNode := ShellTreeView.Selected;
  if SelNode<>nil then t:=SelNode.text;
  p:=ShellTreeView.Path;
  if not FIsInReload and (SelNode<>nil) then
    begin
     //FPath := ShellTreeView.Path;
     ShellPathPanel.Path := FPath;
     ComboBoxDir.Text := ShellListView.Root;
     //if ShellTreeView.Focused then ShellPathPanel.Path:=ShellListView.Root;
     setSpeedButtonGoUpEnabled(FPath);
    end;
  FInShellTreeViewChange:=false;
end;

// this is called when user selects a node in ShellTreeView
procedure TFreeFilePreviewDialog.ShellTreeViewSelectionChanged(Sender: TObject);
var Node: TTreeNode;
begin
  if FInShellTreeViewSelectionChanged then exit;
  FInShellTreeViewSelectionChanged:=true;
  Node := ShellTreeView.Selected;
  if Node <> nil then
    begin
    ShellListView.ClearSelection;
    setSpeedButtonGoUpEnabled(ShellListView.Root);
    end;
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
 stNode := nil;
 if Node is TShellTreeNode then
   stNode := TShellTreeNode(Node);

 if assigned(stNode) then
    if stNode.IsDirectory and DirectoryExists(path)
    then Node.ImageIndex := FFileIcon.addIconsForFile(path)
    else
    if not stNode.IsDirectory and FileExists(path)
    then Node.ImageIndex :=  FFileIcon.addIconsForFile(path);
end;


{$IFDEF WINDOWS_DONT_USE}
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
  fTypeName := String(WideCharToString( FileInfo.szTypeName ));

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

//{$IFNDEF WINDOWS}
{$IFDEF useMagic}
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
//{$ENDIF}

{ // moved to FileIconAdapter
function TFreeFilePreviewDialog.addAnyIconsForFile(filename:string):integer;
var mimeType:string; iconName:string; i,i1,i2,iconIndex:integer;
    smallIcon, largeIcon: TIcon;
begin
  result:=-1;
  iconName:=FFileIcon.getIconNameForFile(filename,16);

  // check if icon is registered in iconName to IconIndex map
  i := FIconNamesMap.IndexOf(iconName);
  if (i>-1) then
    begin
    iconIndex := PtrUInt(FIconNamesMap.Objects[i]);
    result := iconIndex;
    end
  else
    begin
    iconIndex := -1;
    smallIcon := FFileIcon.getIconByName(iconName,16);
    largeIcon := FFileIcon.getIconByName(iconName,32);
    if assigned(smallIcon) and assigned(largeIcon) then
      begin
      i1 :=SmallImageList.AddIcon(smallIcon);
      i2 :=LargeImageList.AddIcon(largeIcon);
      iconIndex := i1;
      FIconNamesMap.AddObject(iconName, TObject(PtrUint(iconIndex)));
      result := iconIndex;
      end;
    end;
end;
}

procedure TFreeFilePreviewDialog.addIconsForFile(filename: string;
  item: TListItem);
begin
{$IFDEF WINDOWS_NOUSE}
 Item.ImageIndex := GetWinIconForFile(filename, true, Self.LargeImageList);
 Item.ImageIndex := GetWinIconForFile(filename, false, Self.SmallImageList);
{$ELSE}
{$IFDEF useGtk}
addMagicIconsForFile(filename, Item);
{$ENDIF}
{$IFDEF useMagic}
 addMagicIconsForFile(filename, Item);
{$ENDIF}
{$ENDIF}
 if item <> nil then
    Item.ImageIndex := FFileIcon.addIconsForFile(filename);
end;

procedure TFreeFilePreviewDialog.SetStatusText(AValue: String);
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

procedure TFreeFilePreviewDialog.ShellListViewLoaded(Sender: TObject);
begin
  setStatusText('');
end;

procedure TFreeFilePreviewDialog.ShellListViewItemRenameClick(Sender: TObject);
begin
  ShellListView.Selected.EditCaption;
end;

/// Places
procedure TFreeFilePreviewDialog.AddPlace(placeName: string; dirName: string);
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
var newDir: String; item:TListItem; n:integer;
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

procedure TFreeFilePreviewDialog.FitPreviewImage;
var kI, kS : real;
begin
 if FAutoFit then doAutoFit
 else begin
  if FPreviewImage.Picture = nil then exit;
  if FPreviewImage.Picture.Height = 0 then exit;
  if ScrollBoxPreview.ClientHeight = 0 then exit;
  kI := FPreviewImage.Picture.Width / FPreviewImage.Picture.Height;
  kS := ScrollBoxPreview.ClientWidth / ScrollBoxPreview.ClientHeight;
  if  kI > kS
    then FPreviewImage.Width := ScrollBoxPreview.ClientWidth
    else FPreviewImage.Height := ScrollBoxPreview.ClientHeight;
 end;
end;

procedure TFreeFilePreviewDialog.ScrollBoxPreviewResize(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  if FPreviewImage.AutoSize = false then
     FitPreviewImage;
end;

procedure TFreeFilePreviewDialog.ScrollBoxPreviewDblClick(Sender: TObject);
begin
  if not Assigned(FPreviewImage.Picture.Bitmap) then exit;
  FPreviewImage.AutoSize := not FPreviewImage.AutoSize;
  if not FPreviewImage.AutoSize then
    begin
    FitPreviewImage;
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

function  TFreeFilePreviewDialog.getSelectedAbsoluteFileName:string;
begin
  result := CreateAbsolutePath(FFileName,FPath);
  result := CleanAndExpandFilename(result);
end;

procedure TFreeFilePreviewDialog.setSelectedAbsoluteFileName(fn:string);
var CurPath, fName:String;
begin
  CurPath := ExtractFilePath(fn);
  fName := ExtractFileName(fn);
  setCurrentPath(CurPath);
  setSelectedFileName(fName);
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
      end
    else
      begin
      FFileName := '';
      EditName.Text := '';
      ShellListView.Selected:=nil;
      PreviewToolPanel.Caption:='';
      FPreviewImage.Picture.Clear;
      end

    end;
end;

function  TFreeFilePreviewDialog.getCurrentPath:string;
begin
  {if ShellListView.Selected <> nil then
    result := ShellListView.GetPathFromItem(ShellListView.Selected)
  else}
    result := ShellListView.Root;
end;

procedure TFreeFilePreviewDialog.setCurrentPath(p:string);
begin
  if getCurrentPath = p then exit;
  //ComboBoxDir.Text:=p;
  if TCustomShellTreeView.PathContainsHiddenDir(p) then
     setListHidden(true);
  FPath := CleanAndExpandDirectory(p);
  //if FIsShowing then
     //ChangeDir(FPath);
  ScheduleReload(FPath);
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

procedure TFreeFilePreviewDialog.setAutoFit(val:boolean);
begin
 if FAutoFit = val then exit;
 FAutoFit := val;
 FitPreviewImage;
end;

procedure TFreeFilePreviewDialog.DoAutoFit;
var
  //omd: TFreeFilePreviewDialog;
  bmw,bmh, cw, tvw, lvw, pvpw,pvph, ppw, psp: integer;
begin
    bmw:=PreviewPicture.Bitmap.Width;
    bmh:=PreviewPicture.Bitmap.Height;
    cw := ShellListView.ColumnsWidth;
    lvw := ShellListView.Width;
    tvw := ShellTreeView.Width;
    pvpw := ScrollBoxPreview.Width;
    pvph := ScrollBoxPreview.Height;
    ppw := PreviewPicture.Width;
    psp := PairSplitterFilesAndPreview.Position;


    if (cw + 10 < ShellListView.ClientWidth)
      and (pvpw < PreviewPicture.Width) then
      if (lvw - cw) < (ppw - pvpw) then
        psp := cw + 10
      else
        psp := psp - (ppw - pvpw) + 10;
    if psp < (tvw + cw + 60) then
      psp := tvw + cw + 60;
    PairSplitterFilesAndPreview.Position := psp;

    // Smart fit
    pvpw := ScrollBoxPreview.ClientWidth;
    pvph := ScrollBoxPreview.ClientHeight;
    if (pvpw < bmw) or (pvph < bmh) then
    begin
      PreviewImage.Center := True;
      PreviewImage.Stretch := True;
      PreviewImage.Align := alClient;
      PreviewImage.AutoSize := false;
    end
    else
    begin
      PreviewImage.Center := True;
      PreviewImage.Stretch := false;
      PreviewImage.Align := alClient;
      PreviewImage.AutoSize := false;
    end;

    if (ShellListView.ClientWidth > cw) and (lvw > self.ClientWidth)
      then ShellListView.ClientWidth := cw;
end;


resourcestring rsUnableOpenFileMsg = 'Unable open file "%s"';

procedure TFreeFilePreviewDialog.DefaultPreview(fn:string);
begin
  try
    try
       Cursor := crHourGlass;
       ShellListView.Cursor := crHourGlass;
       Application.ProcessMessages;

       PreviewToolPanel.Caption:='';
       FPreviewImage.Picture.Clear;

       FPreviewImage.Picture.LoadFromFile(fn);

       ShellListView.Cursor := crDefault;
       Cursor := crDefault;
       Application.ProcessMessages;

       {PreviewToolPanel.Caption:=IntToStr(FPreviewImage.Picture.Bitmap.Width)+' x '
         + IntToStr(FPreviewImage.Picture.Bitmap.Height);}

       FPreviewImage.AutoSize:=false;
       FitPreviewImage;
    except
       on EInvalidGraphic do
          begin
          PreviewToolPanel.Caption:='';
          FPreviewImage.Picture.Clear;
          end;
       on EFOpenError do
         ShowMessage(Format(rsUnableOpenFileMsg,[fn]));
       on E: Exception do
          DumpExceptionCallStack(E);
       else
         Raise;
    end;
  finally
        ShellListView.Cursor := crDefault;
        Cursor := crDefault;
        Screen.Cursor := crDefault;
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
  if ShellListView <> nil then
    ShellListView.Mask := FilterComboBox1.Mask;
end;

function TFreeFilePreviewDialog.ShowModal: Integer;
begin
  result := inherited;
  FIsShowing:=false;
end;

procedure TFreeFilePreviewDialog.Activate;
begin
  Application.ProcessMessages;
  FIsShowing:=true;
  //ShellTreeView.Path:=FPath;
  //self.ChangeDir(FPath);
  ScheduleReload(FPath);
  //self.ShellListView.Refresh;
  inherited;
end;

procedure TFreeFilePreviewDialog.DoShow;
begin
  inherited;
end;


function TFreeFilePreviewDialog.Execute: boolean;
var mr:integer;
begin
  mr := self.ShowModal;
  result := (mr = mrOK);
end;

resourcestring
  OkButtonCaptionOpen = 'Open';
  OkButtonCaptionSave = 'Save';

procedure TFreeFilePreviewDialog.setFileDialogMode(const AValue: TFileDialogMode);
begin
  FFileDialogMode := AValue;
  if AValue = fdmOpen then
    begin
    SpeedButtonNewDir.Enabled := false;
    EditName.Enabled := false;
    BitBtnOpen.Caption := OkButtonCaptionOpen;
    end
  else
  if AValue = fdmSave then
    begin
    SpeedButtonNewDir.Enabled := true;
    EditName.Enabled := true;
    BitBtnOpen.Enabled := true;
    BitBtnOpen.Caption := OkButtonCaptionSave;
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
begin
 if FInShellPathPanelOnSelectionChanged then exit;
 if not ShellPathPanel.IsSelectionInteractive then exit;
 FInShellPathPanelOnSelectionChanged := true;
 //ChangeDir(ShellPathPanel.SelectedPath); // this causes unknown errors
 //ShellListView.Root:=ShellPathPanel.SelectedPath;
 AddToHistory(ShellListView.Root);
 //ShellTreeView.Path:=ShellPathPanel.SelectedPath;
 FPath := ShellPathPanel.SelectedPath;
 ScheduleReload(FPath);
 FInShellPathPanelOnSelectionChanged := false;
end;

procedure TFreeFilePreviewDialog.ShellPathPanelOnClick(Sender: TObject);
begin
 self.ComboBoxDir.Visible:=true;
 self.ComboBoxDir.SetFocus;
 self.ShellPathPanel.Visible:=false;
end;

procedure TFreeFilePreviewDialog.LocationEditorPanelClick(Sender: TObject);
begin
 self.ShellPathPanel.Visible:=false;
 self.ComboBoxDir.Visible:=true;
 self.ComboBoxDir.SetFocus;
end;

end.

