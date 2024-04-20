{
 /***************************************************************************
                                   FreeShellCtrls.pas
                                   ------------


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FreeShellCtrls;

{$mode objfpc}{$H+}

{.$define debug_shellctrls}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  Forms, Graphics, ComCtrls, LCLProc, LCLStrConsts,
  StdCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8,
  ScrollPanel;

{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

resourcestring
  sShellCtrlsTime = 'Time';

type

  { TObjectTypes }

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  TFileSortType = (fstNone, fstAlphabet, fstFoldersFirst);

  { TFileItem : internal helper class used for temporarily storing info in an internal TStrings component}
  type
    { TFileItem }
    TFileItem = class(TObject)
    private
      FFileInfo: TSearchRec;
      FBasePath: String;
      function GetBasePath: String;
      function GetFileName: String;
      function GetSize: integer;
      function GetIsFolder: boolean;
      function GetIsHidden: boolean;
      function GetCreatedTime: TDateTime;
      function GetModifiedTime: TDateTime;
      function GetIsReadable: boolean;
      function GetIsWriteable: boolean;
      function GetIsExecutable: boolean;
      function GetOwner: String;
    public
      //more data to sort by size, date... etc
      constructor Create(const DirInfo: TSearchRec; ABasePath: String);
      property FileInfo: TSearchRec read FFileInfo write FFileInfo;
      property BasePath: String read GetBasePath;
      property FileName: String read GetFileName;
      property Size: integer read GetSize;
      property isFolder: Boolean read GetIsFolder;
      property isHidden: Boolean read GetIsHidden;
      property CreatedTime: TDateTime read GetCreatedTime;
      property ModifiedTime: TDateTime read GetModifiedTime;
      property IsReadable: Boolean read GetIsReadable;
      property IsWriteable: Boolean read GetIsWriteable;
      property IsExecutable: Boolean read GetIsExecutable;
      property Owner: String read GetOwner;
    end;


  { Forward declaration of the classes }

  TCustomShellTreeView = class;
  TCustomShellListView = class;

  { TCustomShellTreeView }

  TCustomShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellListView: TCustomShellListView;
    FFileSortType: TFileSortType;
    FInitialRoot: String;
    FIsLoading: boolean;
    FAbortLoading : boolean;
    { Setters and getters }
    function GetPath: string;
    procedure SetFileSortType(const AValue: TFileSortType);
    procedure SetObjectTypes(AValue: TObjectTypes);
    procedure SetPath(AValue: string);
    procedure SetRoot(const AValue: string);
    procedure SetShellListView(const Value: TCustomShellListView);
  protected
    procedure DoCreateNodeClass(var NewNodeClass: TTreeNodeClass); override;
    procedure Loaded; override;
    function CreateNode: TTreeNode; override;
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
    procedure DoSelectionChanged; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    procedure AbortLoading;
    class function  GetBasePath: string;
    function  GetRootPath: string;
    class function PathContainsHiddenDir(path: String): Boolean;
    class procedure GetFilesInDir(const ABaseDir: string;
      AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType = fstNone);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): string;
    procedure PopulateWithBaseFiles;
    procedure Refresh(ANode: TTreeNode); overload;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes;
    property ShellListView: TCustomShellListView read FShellListView write SetShellListView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType;
    property Root: string read FRoot write SetRoot;
    property Path: string read GetPath write SetPath;

    property IsLoading: boolean read FIsLoading;

    { Protected properties which users may want to access, see bug 15374 }
    property Items;
  end;

  { TShellTreeView }

  TShellTreeView = class(TCustomShellTreeView)
  published
    { TCustomTreeView properties }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExpandSignType;
    property Font;
    property FileSortType;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Root;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelectionChanged;
    property OnShowHint;
    property OnUTF8KeyPress;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomShellTreeView properties }
    property ObjectTypes;
    property ShellListView;
  end;

  { TCustomShellListView }

  TCSLVFileAddedEvent = procedure(Sender: TObject; Item: TListItem) of object;

  TCustomShellListView = class(TCustomListView)
  private
    FMask: string;
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellTreeView: TCustomShellTreeView;
    FOnFileAdded: TCSLVFileAddedEvent;
    FOnLoaded: TNotifyEvent;

    FNameColumn : TListColumn;
    FSizeColumn : TListColumn;
    FTypeColumn : TListColumn;
    FTimeColumn : TListColumn;

    FAbortLoading : boolean;
    FIsLoading : boolean;

    { Setters and getters }
    procedure SetMask(const AValue: string);
    procedure SetShellTreeView(const Value: TCustomShellTreeView);
    procedure SetRoot(const Value: string);
  protected
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
    procedure Resize; override;
    function GetAllColumnsWidth:integer;
    procedure AdjustColumnSizes();
    procedure defaultFileCompare(Sender: TObject; Item1, Item2: TListItem;
                                   Data: Integer; var Compare: Integer);
    property OnFileAdded: TCSLVFileAddedEvent read FOnFileAdded write FOnFileAdded;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Methods specific to Lazarus }
    function GetPathFromItem(ANode: TListItem): string;
    procedure AbortLoading;
    procedure Reload;
    { Properties }
    property IsLoading: boolean read FIsLoading;
    property Mask: string read FMask write SetMask; // Can be used to connect to other controls
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property Root: string read FRoot write SetRoot;
    property ShellTreeView: TCustomShellTreeView read FShellTreeView write SetShellTreeView;
    { Protected properties which users may want to access, see bug 15374 }
    property Items;
    property ColumnsWidth:integer read GetAllColumnsWidth;
    property AutoWidthLastColumn;
  end;

  { TShellListView }

  TShellListView = class(TCustomShellListView)
  public
    property Columns;
  published
    { TCustomListView properties
      The same as TListView excluding data properties }
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
//    property Checkboxes;
    property Color default clWindow;
//    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property Font;
//    property FullDrag;
//    property GridLines;
    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
    property LargeImages;
    property Mask;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle default vsReport;
//    property OnAdvancedCustomDraw;
//    property OnAdvancedCustomDrawItem;
//    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
//    property OnCustomDraw;
//    property OnCustomDrawItem;
//    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited; // added MM7 to rename files in place
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectItem;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnFileAdded;
    property OnLoaded;
    { TCustomShellListView properties }
    property ObjectTypes;
    property Root;
    property ShellTreeView;
  end;

  { TShellTreeNode }

  TShellTreeNode = class(TTreeNode)
  private
    FFileInfo: TSearchRec;
    FBasePath: String;
  protected
    procedure SetBasePath(ABasePath: String);
  public
    function ShortFilename: String;
    function FullFilename: String;
    function IsDirectory: Boolean;

    property BasePath: String read FBasePath;
  end;

  EShellCtrl = class(Exception);
  EInvalidPath = class(EShellCtrl);


  { TCustomFilterComboBox }

  TCustomFilterComboBox = class(TCustomComboBox)
  private
    FFilter: string;
    FShellListView: TShellListView;
    function GetMask: string;
    procedure SetFilter(const AValue: string);
    procedure SetShellListView(const AValue: TShellListView);
  protected
    procedure Select; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Base methods }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    { Externally available methods }
    class procedure ConvertFilterToStrings(AFilter: string;
      AStrings: TStrings; AClearStrings, AAddDescription, AAddFilter: Boolean);
    { properties }
    property Mask: string read GetMask; // Can be used to connect to other controls
    property ShellListView: TShellListView read FShellListView write SetShellListView;
  end;

  TFilterComboBox = class(TCustomFilterComboBox)
  published
    { properties }
    property Align;
    property Anchors;
    property AutoComplete;
    property AutoDropDown;
    property AutoSize;// Note: windows has a fixed height in some styles
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    // property FileList: TFileList
    property Filter: string read FFilter write SetFilter;
    property Font;
    property ItemIndex;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShellListView;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    { events }
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
  end;


function DbgS(OT: TObjectTypes): String; overload;


type TPathType = (ptUnixAbsolute, ptUnixRelative,
 ptWindowsAbsolute, ptWindowsRelative,  ptWindowsUNC);

type TShellPathPanel = class(TScrollPanel)
private
  FPathType: TPathType;
  FPath: string;
protected
  procedure setPath(aPath:string);
  function  getSelectedPath:string;
public
  property Path:string read FPath write setPath;
  property SelectedPath :string read getSelectedPath;
end;

procedure Register;

implementation

{$ifdef windows}
uses Windows;
{$endif}

const
  //no need to localize, it's a message for the programmer
  sShellTreeViewIncorrectNodeType = 'TShellTreeView: the newly created node is not a TShellTreeNode!';

function DbgS(OT: TObjectTypes): String; overload;
begin
  Result := '[';
  if (otFolders in OT) then Result := Result + 'otFolders,';
  if (otNonFolders in OT) then Result := Result + 'otNonFolders,';
  if (otHidden in OT) then Result := Result + 'otHidden';
  if Result[Length(Result)] = ',' then System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;



constructor TFileItem.Create(const DirInfo:TSearchRec; ABasePath: String);
begin
  FFileInfo := DirInfo;
  FBasePath:= ABasePath;
end;

function TFileItem.GetBasePath: String;
begin
  result := FBasePath;
end;

function TFileItem.GetFileName: String;
begin
  result := FFileInfo.Name ;
end;

function TFileItem.GetSize: integer;
begin
  result := FFileInfo.Size ;
end;

function TFileItem.GetIsFolder: boolean;
begin
  result := FFileInfo.Attr and FaDirectory > 0;
end;

function TFileItem.GetIsHidden: boolean;
begin
  result := FFileInfo.Attr and faHidden > 0;
end;

function TFileItem.GetCreatedTime: TDateTime;
begin
  result := FileDateToDateTime(FFileInfo.Time );
end;

function TFileItem.GetModifiedTime: TDateTime;
begin
  result := FileDateToDateTime(FFileInfo.Time );
end;

function TFileItem.GetIsReadable: boolean;
begin
  result := FFileInfo.Attr and faAnyFile > 0;
end;

function TFileItem.GetIsWriteable: boolean;
begin
  result := FFileInfo.Attr and faReadOnly = 0;
end;

function TFileItem.GetIsExecutable: boolean;
begin
  result := FFileInfo.Attr and faAnyFile > 0;
end;

function TFileItem.GetOwner: String;
begin
  result := '';
end;



{ TShellTreeNode }

procedure TShellTreeNode.SetBasePath(ABasePath: String);
begin
  FBasePath := ABasePath;
end;


function TShellTreeNode.ShortFilename: String;
begin
  Result := ExtractFileName(FFileInfo.Name);
  if (Result = '') then Result := FFileInfo.Name;
end;

function TShellTreeNode.FullFilename: String;
begin
  if (FBasePath <> '') then
    Result := AppendPathDelim(FBasePath) + FFileInfo.Name
  else
    //root nodes
    Result := FFileInfo.Name;
  {$if defined(windows) and not defined(wince)}
  if (Length(Result) = 2) and (Result[2] = DriveSeparator) then
    Result := Result + PathDelim;
  {$endif}
end;

function TShellTreeNode.IsDirectory: Boolean;
begin
  Result := ((FFileInfo.Attr and faDirectory) > 0);
end;


{ TCustomShellTreeView }

procedure TCustomShellTreeView.SetShellListView(
  const Value: TCustomShellListView);
var
  Tmp: TCustomShellListView;
begin
  if FShellListView = Value then Exit;

  if Assigned(FShellListView) then
  begin
    Tmp := FShellListView;
    FShellListView := nil;
    Tmp.ShellTreeView := nil;
  end;

  FShellListView := Value;

  // Update the pair, it will then update itself
  // in the setter of this property
  // Updates only if necessary to avoid circular calls of the setters
  if Assigned(Value) and (Value.ShellTreeView <> Self) then
    Value.ShellTreeView := Self;
end;


procedure TCustomShellTreeView.DoCreateNodeClass(
  var NewNodeClass: TTreeNodeClass);
begin
  NewNodeClass := TShellTreeNode;
  inherited DoCreateNodeClass(NewNodeClass);
end;

procedure TCustomShellTreeView.Loaded;
begin
  FIsLoading := true;
  inherited Loaded;
  if (FInitialRoot = '') then
    PopulateWithBaseFiles()
  else
    SetRoot(FInitialRoot);
  FIsLoading := false;
end;

function TCustomShellTreeView.CreateNode: TTreeNode;
begin
  Result := inherited CreateNode;
  //just in case someone attaches a new OnCreateNodeClass which does not return a TShellTreeNode (sub)class
  if not (Result is TShellTreeNode) then
    Raise EShellCtrl.Create(sShellTreeViewIncorrectNodeType);
end;

procedure TCustomShellTreeView.SetRoot(const AValue: string);
var
  RootNode: TTreeNode;
begin
  if FRoot=AValue then exit;
  if (csLoading in ComponentState) then
  begin
    FInitialRoot := AValue;
    Exit;
  end;
  //Delphi raises an exception in this case, but don't crash the IDE at designtime
  if not (csDesigning in ComponentState)
     and (AValue <> '')
     and not DirectoryExistsUtf8(ExpandFilenameUtf8(AValue)) then
     Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[ExpandFileNameUtf8(AValue)]);
  if (AValue = '') then
    FRoot := GetBasePath
  else
    FRoot:=AValue;
  Items.Clear;
  if FRoot = '' then
  begin
    PopulateWithBaseFiles()
  end
  else
  begin
    //Add a node for Root and expand it (issue #0024230)
    //Make FRoot contain fully qualified pathname, we need it later in GetPathFromNode()
    FRoot := ExpandFileNameUtf8(FRoot);
    //Set RootNode.Text to AValue so user can choose if text is fully qualified path or not
    RootNode := Items.AddChild(nil, AValue);
    TShellTreeNode(RootNode).FFileInfo.Attr := FileGetAttr(FRoot);
    TShellTreeNode(RootNode).FFileInfo.Name := FRoot;
    TShellTreeNode(RootNode).SetBasePath('');
    RootNode.HasChildren := True;
    RootNode.Expand(False);
  end;
  if Assigned(ShellListView) then
    ShellListView.Root := FRoot;
end;

// ToDo: Optimize, now the tree is populated in constructor, SetRoot and SetFileSortType.
// For some reason it does not show in performance really.
procedure TCustomShellTreeView.SetFileSortType(const AValue: TFileSortType);
var
  RootNode: TTreeNode;
  CurrPath: String;
begin
  if FFileSortType=AValue then exit;
  FFileSortType:=AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Items.Clear;
    if FRoot = '' then
      PopulateWithBaseFiles()
    else
    begin
      RootNode := Items.AddChild(nil, FRoot);
      RootNode.HasChildren := True;
      RootNode.Expand(False);
      try
       SetPath(CurrPath);
      except
        // CurrPath may have been removed in the mean time by another process, just ignore
        on E: EInvalidPath do ;//
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellTreeView.SetObjectTypes(AValue: TObjectTypes);
var
  CurrPath: String;
begin
  if FObjectTypes = AValue then Exit;
  FObjectTypes := AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Refresh(nil);
    try
       SetPath(CurrPath);
    except
      // CurrPath may have been removed in the mean time by another process, just ignore
      on E: EInvalidPath do ;//
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  OldAutoExpand: Boolean;
begin
  Result:=inherited CanExpand(Node);
  if not Result then exit;
  OldAutoExpand:=AutoExpand;
  AutoExpand:=False;
  Node.DeleteChildren;
  Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
  AutoExpand:=OldAutoExpand;
end;

constructor TCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialRoot := '';

  // Initial property values
  FObjectTypes:= [otFolders];

  // Populating the base dirs is done in Loaded
end;

destructor TCustomShellTreeView.Destroy;
begin
  FShellListView:=nil;
  inherited Destroy;
end;


function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.FileInfo.Name, f2.FileInfo.Name);
end;

function FilesSortFoldersFirst(p1,p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  if f1.isFolder=f2.isFolder then
    Result:=FilesSortAlphabet(p1,p2)
  else begin
    if f1.isFolder then Result:=-1
    else Result:=1;
  end;

end;

function STVCompareFiles(f1, f2: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(f1),AnsiString(f2));
end;

{ Helper routine.
  Finds all files/directories directly inside a directory.
  Does not recurse inside subdirectories.

  AResult will contain TFileItem objects upon return, make sure to free them in the calling routine

  AMask may contain multiple file masks separated by ;
  Don't add a final ; after the last mask.
}
class procedure TCustomShellTreeView.GetFilesInDir(const ABaseDir: string;
  AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType);
var
  DirInfo: TSearchRec;
  FindResult: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile: Boolean;
  SearchStr: string;
  MaskStr: string;
  Files: TList;
  FileItem: TFileItem;
  i: Integer;
  MaskStrings: TStringList;
  FileTree: TAvlTree;
  ShortFilename: AnsiString;
  j: Integer;
  {$if defined(windows) and not defined(wince)}
  ErrMode : LongWord;
  {$endif}
begin
  {$if defined(windows) and not defined(wince)}
  // disables the error dialog, while enumerating not-available drives
  // for example listing A: path, without diskette present.
  // WARNING: Since Application.ProcessMessages is called, it might effect some operations!
  ErrMode:=SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
  {$endif}

  if Trim(AMask) = '' then MaskStr := AllFilesMask
  else MaskStr := AMask;

  // The string list implements support for multiple masks separated
  // by semi-colon ";"
  MaskStrings := TStringList.Create;
  FileTree:=TAvlTree.Create(@STVCompareFiles);
  try
    {$ifdef NotLiteralFilenames}
    MaskStrings.CaseSensitive := False;
    {$else}
    MaskStrings.CaseSensitive := True;
    {$endif}

    MaskStrings.Delimiter := ';';
    MaskStrings.DelimitedText := MaskStr;

    if AFileSortType=fstNone then Files:=nil
    else Files:=TList.Create;

    // search files by masks
    j:=0;
    for i := 0 to MaskStrings.Count - 1 do
    begin
      if MaskStrings.IndexOf(MaskStrings[i]) < i then Continue; // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + MaskStrings.Strings[i];

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);

      while FindResult = 0 do
      begin
        inc(j);
        if j=100 then
        begin
          Application.ProcessMessages;
          j:=0;
        end;

        ShortFilename := DirInfo.Name;

        IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);

        IsValidDirectory := (ShortFilename <> '.') and (ShortFilename <> '..');

        IsHidden := (DirInfo.Attr and faHidden{%H-} = faHidden{%H-});

        // First check if we show hidden files
        if IsHidden then AddFile := (otHidden in AObjectTypes)
        else AddFile := True;

        // If it is a directory, check if it is a valid one
        if IsDirectory then
          AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
        else
          AddFile := AddFile and (otNonFolders in AObjectTypes);

        // AddFile identifies if the file is valid or not
        if AddFile then
        begin
          if not Assigned(Files) then begin
            if FileTree.Find(Pointer(ShortFilename))=nil then
            begin
              // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
              FileTree.Add(Pointer(ShortFilename));
              AResult.AddObject(ShortFilename, TFileItem.Create(DirInfo, ABaseDir));
            end;
          end else
            Files.Add ( TFileItem.Create(DirInfo, ABaseDir));
        end;

        FindResult := FindNextUTF8(DirInfo);
      end;

      FindCloseUTF8(DirInfo);
    end;
  finally
    FileTree.Free;
    MaskStrings.Free;
  end;

  if Assigned(Files) then begin

    case AFileSortType of
      fstAlphabet:     Files.Sort(@FilesSortAlphabet);
      fstFoldersFirst: Files.Sort(@FilesSortFoldersFirst);
    end;

    for i:=0 to Files.Count-1 do
    begin
      FileItem:=TFileItem(Files[i]);
      if (i < Files.Count - 1) and (TFileItem(Files[i]).FileInfo.Name = TFileItem(Files[i + 1]).FileInfo.Name) then
      begin
        FileItem.Free;
        Continue; // cause Files is sorted // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      end;
      AResult.AddObject(FileItem.FileInfo.Name, FileItem);
    end;
    //don't free the TFileItems here, they will freed by the calling routine
    Files.Free;
  end;

  {$if defined(windows) and not defined(wince)}
  finally
     SetErrorMode(ErrMode);
  end;
  {$endif}
end;

class function TCustomShellTreeView.GetBasePath: string;
begin
  {$if defined(windows) and not defined(wince)}
  Result := '';
  {$endif}
  {$ifdef wince}
  Result := '\';
  {$endif}
  {$ifdef unix}
  Result := '/';
  {$endif}
  {$ifdef HASAMIGA}
  Result := '';
  {$endif}
end;

function TCustomShellTreeView.GetRootPath: string;
begin
  if FRoot <> '' then
    Result := FRoot
  else
    Result := GetBasePath();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ Returns true if at least one item was added, false otherwise }
function TCustomShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: string): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;

   function HasSubDir(Const ADir: String): Boolean;
   var
     SR: TSearchRec;
     FindRes: LongInt;
     Attr: Longint;
     IsHidden: Boolean;
   begin
     Result:=False;
     try
       Attr := faDirectory;
       if (otHidden in fObjectTypes) then Attr := Attr or faHidden{%H-};
       FindRes := FindFirstUTF8(AppendPathDelim(ADir) + AllFilesMask, Attr , SR);
       while (FindRes = 0) do
       begin
         if ((SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and
            (SR.Name <> '..')) then
         begin
           IsHidden := ((Attr and faHidden{%H-}) > 0);
           if not (IsHidden and (not ((otHidden in fObjectTypes)))) then
           begin
             Result := True;
             Break;
           end;
         end;
         FindRes := FindNextUtf8(SR);
       end;
     finally
       FindCloseUTF8(SR);
     end; //try
   end;

begin
  Result := False;
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  Files := TStringList.Create;
  try
    Files.OwnsObjects := True;
    GetFilesInDir(ANodePath, AllFilesMask, FObjectTypes, Files, FFileSortType);
    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      NewNode := Items.AddChildObject(ANode, Files.Strings[i], nil);
      TShellTreeNode(NewNode).FFileInfo := TFileItem(Files.Objects[i]).FileInfo;
      TShellTreeNode(NewNode).SetBasePath(TFileItem(Files.Objects[i]).FBasePath);

      if (fObjectTypes * [otNonFolders] = []) then
        NewNode.HasChildren := (TShellTreeNode(NewNode).IsDirectory and
                               HasSubDir(AppendpathDelim(ANodePath)+Files[i]))
      else
        NewNode.HasChildren := TShellTreeNode(NewNode).IsDirectory;
    end;
  finally
    Files.Free;
  end;
end;

procedure TCustomShellTreeView.PopulateWithBaseFiles;
{$if defined(windows) and not defined(wince)}
var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: PChar;
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;
  Items.Clear;
  r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if r = 0 then Exit;
  if r > SizeOf(Drives) then Exit;
//    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
  pDrive := Drives;
  while pDrive^ <> #0 do
  begin
    NewNode := Items.AddChildObject(nil, ExcludeTrailingBackslash(pDrive), pDrive);
    //Yes, we want to remove the backslash,so don't use ChompPathDelim here
    TShellTreeNode(NewNode).FFileInfo.Name := ExcludeTrailingBackslash(pDrive);
    //On NT platforms drive-roots really have these attributes
    TShellTreeNode(NewNode).FFileInfo.Attr := faDirectory + faSysFile + faHidden;
    TShellTreeNode(NewNode).SetBasePath('');
    NewNode.HasChildren := True;
    Inc(pDrive, 4);
  end;
end;
{$else}
var
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  // also do not populate before loading is done
  if ([csDesigning, csLoading] * ComponentState <> []) then Exit;
  Items.Clear;

  // This allows showing "/" in Linux, but in Windows it makes no sense to show the base
  if GetBasePath() <> '' then
  begin
    NewNode := Items.AddChild(nil, GetBasePath());
    NewNode.HasChildren := True;
    PopulateTreeNodeWithFiles(NewNode, GetBasePath());
    NewNode.Expand(False);
  end
  else
    PopulateTreeNodeWithFiles(nil, GetBasePath());
end;
{$endif}

procedure TCustomShellTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  CurrentNodePath: String;
begin
  inherited DoSelectionChanged;
  ANode := Selected;
  if Assigned(FShellListView) and Assigned(ANode) then
  begin
    //You cannot rely on HasChildren here, because it can become FALSE when user
    //clicks the expand sign and folder is empty
    //Issue 0027571
    CurrentNodePath := ChompPathDelim(GetPathFromNode(ANode));
    if TShellTreeNode(ANode).IsDirectory then
    begin
      //Note: the folder may have been deleted in the mean time
      //an exception will be raised by the next line in that case
      FShellListView.Root := GetPathFromNode(ANode)
    end
    else
    begin
      if not FileExistsUtf8(CurrentNodePath) then
        Raise EShellCtrl.CreateFmt(sShellCtrlsSelectedItemDoesNotExists,[CurrentNodePath]);
      if Assigned(Anode.Parent) then
        FShellListView.Root := GetPathFromNode(ANode.Parent)
      else
        FShellListView.Root := '';
    end;
  end;
end;

function TCustomShellTreeView.GetPathFromNode(ANode: TTreeNode): string;
begin
  if Assigned(ANode) then
  begin
    Result := TShellTreeNode(ANode).FullFilename;
    if TShellTreeNode(ANode).IsDirectory then
      Result := AppendPathDelim(Result);
    if not FilenameIsAbsolute(Result) then
      Result := GetRootPath() + Result;    // Include root directory
  end
  else
    Result := '';
end;


procedure TCustomShellTreeView.Refresh(ANode: TTreeNode);
//nil will refresh root
var
  RootNodeText: String;
  IsRoot: Boolean;
begin
  if (Items.Count = 0) then Exit;
  {$ifdef debug_shellctrls}
  debugln(['TCustomShellTreeView.Refresh: GetFirstVisibleNode.Text = "',Items.GetFirstVisibleNode.Text,'"']);
  {$endif}

  IsRoot := (ANode = nil) or ((ANode = Items.GetFirstVisibleNode) and (GetRootPath <> ''));
  {$ifdef debug_shellctrls}
  debugln(['IsRoot = ',IsRoot]);
  {$endif}


  if (ANode = nil) and (GetRootPath <> '') then ANode := Items.GetFirstVisibleNode;
  if IsRoot then
  begin
    if Assigned(ANode) then
      RootNodeText := ANode.Text  //this may differ from FRoot, so don't use FRoot here
    else
      RootNodeText := GetRootPath;
    {$ifdef debug_shellctrls}
    debugln(['IsRoot = TRUE, RootNodeText = "',RootNodeText,'"']);
    {$endif}


    FRoot := #0; //invalidate FRoot
    SetRoot(RootNodeText); //re-initialize the entire tree
  end
  else
  begin
    ANode.Expand(False);
  end;
end;

function TCustomShellTreeView.GetPath: string;
begin
  Result := GetPathFromNode(Selected);
end;

class function TCustomShellTreeView.PathContainsHiddenDir(path: String): Boolean;
var
  i: Integer;
  Attr: LongInt;
  Dirs: TStringList;
  Fn, AbsPath, CurPath: String;
begin
  AbsPath:=TrimAndExpandFilename(path);

  begin
    Attr := FileGetAttrUtf8(AbsPath);
    Result := ((Attr and faHidden{%H-}) = faHidden{%H-});
    if not Result then
    begin
      //it also is not allowed that any folder above is hidden
      Fn := ChompPathDelim(AbsPath);
      Fn := ExtractFileDir(Fn);
      Dirs := TStringList.Create;
      try
        Dirs.StrictDelimiter := True;
        Dirs.Delimiter := PathDelim;
        Dirs.DelimitedText := Fn;
        CurPath := '';
        for i := 0 to Dirs.Count - 1 do
        begin
          if (i = 0) then
            CurPath := Dirs.Strings[i]
          else
            CurPath := CurPath + PathDelim + Dirs.Strings[i];
          if (CurPath = '') then Continue;
          //RelPath := CreateRelativePath(Fn, FQRootPath, False, True);
          //don't check if Fn now is "higher up the tree" than the current root
          {if (RelPath = '') or ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.')) then
          begin
            {$ifdef debug_shellctrls}
            debugln(['TCustomShellTreeView.SetPath.ContainsHidden: Fn is higher: ',Fn]);
            {$endif}
            Continue;
          end;}
          {$if defined(windows) and not defined(wince)}
          if (Length(CurPath) = 2) and (CurPath[2] = ':') then Continue;
          {$endif}
          Attr := FileGetAttrUtf8(CurPath);
          if (Attr <> -1) and ((Attr and faHidden{%H-}) > 0) then
          begin
            Result := True;
            {$ifdef debug_shellctrls}
            debugln(['TCustomShellTreeView.SetPath.Exists: a subdir is hidden: Result := False']);
            {$endif}
            Break;
          end;
        end;
      finally
        Dirs.Free;
      end;
    end;
  end;
end;

procedure TCustomShellTreeView.AbortLoading;
begin
  FAbortLoading:=true;
end;

{
SetPath: Path can be
- Absolute like '/usr/lib'
- Relative like 'foo/bar'
  This can be relative to:
  - Self.Root (which takes precedence over)
  - Current directory
}
procedure TCustomShellTreeView.SetPath(AValue: string);
var
  sl: TStringList;
  Node, LastNode: TTreeNode;
  i: integer;
  FQRootPath, RelPath: String;
  RootIsAbsolute: Boolean;
  IsRelPath: Boolean;

  function GetAdjustedNodeText(ANode: TTreeNode): String;
  begin
    if (ANode = Items.GetFirstVisibleNode) and (FQRootPath <> '') then
    begin
      if not RootIsAbsolute then
        Result := ''
      else
        Result := FQRootPath;
    end
    else Result := ANode.Text;
  end;

  function Exists(Fn: String): Boolean;
  //Fn should be fully qualified
  var
    Attr: LongInt;
    //Dirs: TStringList;
    //i: Integer;
  begin
    Result := False;
    Attr := FileGetAttrUtf8(Fn);
    {$ifdef debug_shellctrls}
    debugln(['TCustomShellTreeView.SetPath.Exists: Attr = ', Attr]);
    {$endif}
    if (Attr = -1) then Exit;
    if not (otNonFolders in FObjectTypes) then
      Result := ((Attr and faDirectory) > 0)
    else
      Result := True;
    {$ifdef debug_shellctrls}
    debugln(['TCustomShellTreeView.SetPath.Exists: Result = ',Result]);
    {$endif}
  end;

  function PathIsDriveRoot({%H-}Path: String): Boolean;  {$if not (defined(windows) and not defined(wince))}inline;{$endif}
  //WinNT filesystem reports faHidden on all physical drive-roots (e.g. C:\)
  begin
    {$if defined(windows) and not defined(wince)}
    Result := (Length(Path) = 3) and
              (Upcase(Path[1]) in ['A'..'Z']) and
              (Path[2] = DriveSeparator) and
              (Path[3] in AllowDirectorySeparators);
    {$else}
    Result := False;
    {$endif windows}
  end;

  function ContainsHiddenDir(Fn: String): Boolean;
  var
    i: Integer;
    Attr: LongInt;
    Dirs: TStringList;
    RelPath: String;
  begin
    //if fn=root then always return false
    if (CompareFileNames(Fn, FQRootPath) = 0) then
      Result := False
    else
    begin
      Attr := FileGetAttrUtf8(Fn);
      Result := ((Attr and faHidden{%H-}) = faHidden{%H-}) and not PathIsDriveRoot(Fn);
      if not Result then
      begin
        //it also is not allowed that any folder above is hidden
        Fn := ChompPathDelim(Fn);
        Fn := ExtractFileDir(Fn);
        Dirs := TStringList.Create;
        try
          Dirs.StrictDelimiter := True;
          Dirs.Delimiter := PathDelim;
          Dirs.DelimitedText := Fn;
          Fn := '';
          for i := 0 to Dirs.Count - 1 do
          begin
            if (i = 0) then
              Fn := Dirs.Strings[i]
            else
              Fn := Fn + PathDelim + Dirs.Strings[i];
            if (Fn = '') then Continue;
            RelPath := CreateRelativePath(Fn, FQRootPath, False, True);
            //don't check if Fn now is "higher up the tree" than the current root
            if (RelPath = '') or ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.')) then
            begin
              {$ifdef debug_shellctrls}
              debugln(['TCustomShellTreeView.SetPath.ContainsHidden: Fn is higher: ',Fn]);
              {$endif}
              Continue;
            end;
            {$if defined(windows) and not defined(wince)}
            if (Length(Fn) = 2) and (Fn[2] = ':') then Continue;
            {$endif}
            Attr := FileGetAttrUtf8(Fn);
            if (Attr <> -1) and ((Attr and faHidden{%H-}) > 0) and not PathIsDriveRoot(Fn) then
            begin
              Result := True;
              {$ifdef debug_shellctrls}
              debugln(['TCustomShellTreeView.SetPath.Exists: a subdir is hidden: Result := False']);
              {$endif}
              Break;
            end;
          end;
        finally
          Dirs.Free;
        end;
      end;
    end;
  end;

begin
  RelPath := '';

  LastNode := Selected;
  if LastNode <> nil then LastNode.Selected := false;
  LastNode := Selected;
  LastNode := nil;

  {$ifdef debug_shellctrls}
  debugln(['SetPath: GetRootPath = "',getrootpath,'"',' AValue=',AValue]);
  {$endif}
  if (GetRootPath <> '') then
    //FRoot is already Expanded in SetRoot, just add PathDelim if needed
    FQRootPath := AppendPathDelim(GetRootPath)
  else
    FQRootPath := '';
  RootIsAbsolute := (FQRootPath = '') or (FQRootPath = PathDelim)
                    or ((Length(FQRootPath) = 3) and (FQRootPath[2] = ':') and (FQRootPath[3] = PathDelim));

  {$ifdef debug_shellctrls}
  debugln(['SetPath: FQRootPath = ',fqrootpath]);
  debugln(['SetPath: RootIsAbsolute = ',RootIsAbsolute]);
  debugln(['SetPath: FilenameIsAbsolute = ',FileNameIsAbsolute(AValue)]);
  {$endif}

  if not FileNameIsAbsolute(AValue) then
  begin
    if Exists(FQRootPath + AValue) then
    begin
      //Expand it, since it may be in the form of ../../foo
      AValue := ExpandFileNameUtf8(FQRootPath + AValue);
    end
    else
    begin
      //don't expand Avalue yet, we may need it in error message
      if not Exists(ExpandFileNameUtf8(AValue)) then
        Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[ExpandFileNameUtf8(FQRootPath + AValue)]);
      //Directory (or file) exists
      //Make it fully qualified
      AValue := ExpandFileNameUtf8(AValue);
    end;
  end
  else
  begin
    //AValue is an absolute path to begin with
    //if not DirectoryExistsUtf8(AValue) then
    if not Exists(AValue) then
      Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue]);
  end;

  //AValue now is a fully qualified path and it exists
  //Now check if it is a subdirectory of FQRootPath
  //RelPath := CreateRelativePath(AValue, FQRootPath, False);
  IsRelPath := (FQRootPath = '') or TryCreateRelativePath(AValue, FQRootPath, False, True, RelPath);

  {$ifdef debug_shellctrls}
  debugln('TCustomShellTreeView.SetPath: ');
  debugln(['  IsRelPath = ',IsRelPath]);
  debugln(['  RelPath = "',RelPath,'"']);
  debugln(['  FQRootPath = "',FQRootPath,'"']);
  {$endif}

  if (not IsRelpath) or ((RelPath <> '') and ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.'))) then
  begin
    // CreateRelativePath returns a string beginning with ..
    // so AValue is not a subdirectory of FRoot
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPathRelative,[AValue, FQRootPath]);
  end;

  if (RelPath = '') and (FQRootPath = '') then
    RelPath := AValue;
  {$ifdef debug_shellctrls}
  debugln(['RelPath = ',RelPath]);
  {$endif}

  if not (otHidden in FObjectTypes) and ContainsHiddenDir(AValue) then
  ///  Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue, FQRootPath]);
    include(FObjectTypes, otHidden);

  if (RelPath = '') then
  begin
    {$ifdef debug_shellctrls}
    debugln('Root selected');
    {$endif}
    //Node := Items.GetFirstVisibleNode;
    if not(otHidden in FObjectTypes) // MM
    then Node := Items.GetFirstVisibleNode
    else Node := Items.GetFirstNode;
    if Assigned(Node) then
    begin
      Node.Expanded := True;
      Node.Selected := True;
    end;
    Exit;
  end;

  sl := TStringList.Create;
  sl.Delimiter := PathDelim;
  sl.StrictDelimiter := True;
  sl.DelimitedText := RelPath;
  if (sl.Count > 0) and (sl[0] = '') then  // This happens when root dir is empty
    sl[0] := PathDelim;                    //  and PathDelim was the first char
  if (sl.Count > 0) and (sl[sl.Count-1] = '') then sl.Delete(sl.Count-1); //remove last empty string
  if (sl.Count = 0) then
  begin
    sl.Free;
    Exit;
  end;

  {$ifdef debug_shellctrls}
  for i := 0 to sl.Count - 1 do debugln(['sl[',i,']="',sl[i],'"']);
  {$endif}



  BeginUpdate;
  try
    if not(otHidden in FObjectTypes) // MM
    then Node := Items.GetFirstVisibleNode
    else Node := Items.GetFirstNode;

    {$ifdef debug_shellctrls}
    if assigned(node) then debugln(['GetFirstVisibleNode = ',GetAdjustedNodeText(Node)]);
    {$endif}
    //Root node doesn't have Siblings in this case, we need one level down the tree
    if (GetRootPath <> '') and Assigned(Node) then
    begin
      {$ifdef debug_shellctrls}
      debugln('Root node doesn''t have Siblings');
      {$endif}
      if not(otHidden in FObjectTypes) // MM
      then Node := Node.GetFirstVisibleChild
      else Node := Node.GetFirstChild;
      {$ifdef debug_shellctrls}
      debugln(['Node = ',GetAdjustedNodeText(Node)]);
      {$endif}
      //I don't know why I wrote this in r44893, but it seems to be wrong so I comment it out
      //for the time being (2015-12-05: BB)
      //if RootIsAbsolute then sl.Delete(0);
    end;
    FAbortLoading := false;
    for i := 0 to sl.Count-1 do
    begin
      {$ifdef debug_shellctrls}
      DbgOut(['i=',i,' sl[',i,']=',sl[i],' ']);
      if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
      else  DbgOut('Node = NIL');
      debugln;
      {$endif}
      while (Node <> Nil) and
            {$IF defined(CaseInsensitiveFilenames) or defined(NotLiteralFilenames)}
            (Utf8LowerCase(GetAdjustedNodeText(Node)) <> Utf8LowerCase(sl[i]))
            {$ELSE}
            (GetAdjustedNodeText(Node) <> sl[i])
            {$ENDIF}
            do
            begin
              if FAbortLoading then break;
              {$ifdef debug_shellctrls}
              DbgOut(['  i=',i,' "',GetAdjustedNodeText(Node),' <> ',sl[i],' -> GetNextVisibleSibling -> ']);
              {$endif}
              if not(otHidden in FObjectTypes) // MM
              then Node := Node.GetNextVisibleSibling
              else Node := Node.GetNextSibling;
              {$ifdef debug_shellctrls}
              if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
              else DbgOut('Node = NIL');
              debugln;
              {$endif}
            end;
      if Node <> Nil then
      begin
        Node.Expanded := True;
        LastNode := Node;
        //Node.Selected := True;
        writeln(Node.Text);
        if not(otHidden in FObjectTypes) then // MM
        Node := Node.GetFirstVisibleChild
        else Node := Node.GetFirstChild
      end
      else
        Break;
    end;
    LastNode.Selected := true;
  finally
    sl.free;
    EndUpdate;
  end;
end;


{ TCustomShellListView }

procedure TCustomShellListView.SetShellTreeView(
  const Value: TCustomShellTreeView);
var
  Tmp: TCustomShellTreeView;
begin
  if FShellTreeView = Value then Exit;
  if FShellTreeView <> nil then
  begin
    Tmp := FShellTreeView;
    FShellTreeView := nil;
    Tmp.ShellListView := nil;
  end;

  FShellTreeView := Value;

  if not (csDestroying in ComponentState) then
    Clear;

  if Value <> nil then
  begin
    FRoot := Value.GetPathFromNode(Value.Selected);
    PopulateWithRoot();

    // Also update the pair, but only if necessary to avoid circular calls of the setters
    if Value.ShellListView <> Self then Value.ShellListView := Self;
  end;

end;

procedure TCustomShellListView.SetMask(const AValue: string);
begin
  if AValue <> FMask then
  begin
    FMask := AValue;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

procedure TCustomShellListView.SetRoot(const Value: string);
begin
  if FRoot <> Value then
  begin
    //Delphi raises an unspecified exception in this case, but don't crash the IDE at designtime
    if not (csDesigning in ComponentState)
       and (Value <> '')
       and not DirectoryExistsUtf8(ExpandFilenameUtf8(Value)) then
       Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[Value]);
    FRoot := Value;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

procedure TCustomShellListView.Reload;
begin
  Clear;
  Items.Clear;
  FAbortLoading := false;
  PopulateWithRoot();
end;

procedure TCustomShellListView.AbortLoading;
begin
  FAbortLoading:=true;
end;

constructor TCustomShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values
  ViewStyle := vsReport;
  ObjectTypes := [otFolders, otNonFolders, otHidden];

  FNameColumn := Self.Columns.Add(); // Name
  FSizeColumn := Self.Columns.Add; // Size
  FTypeColumn := Self.Columns.Add; // Type
  FTimeColumn := Self.Columns.Add; // Time

  FNameColumn.Caption := sShellCtrlsName;
  FSizeColumn.Caption := sShellCtrlsSize;
  FTypeColumn.Caption := sShellCtrlsType;
  FTimeColumn.Caption := sShellCtrlsTime;

  FNameColumn.MinWidth:=30;
  FNameColumn.AutoSize:=true;
  FSizeColumn.AutoSize:=true;
  FTypeColumn.AutoSize:=true;
  FTimeColumn.AutoSize:=true;
  FTimeColumn.MaxWidth:=100;

  AutoWidthLastColumn:=false;
  FAbortLoading := false;
  FIsLoading := false;

  Resize;

  if Self.OnCompare = nil then
     Self.OnCompare := @defaultFileCompare;
end;

destructor TCustomShellListView.Destroy;
begin
  FShellTreeView:=nil;
  inherited Destroy;
end;

procedure TCustomShellListView.defaultFileCompare(Sender: TObject;
          Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var type1,type2, colName:String;
begin
  Compare := 0;
  type1:=Item1.SubItems[1];
  type2:=Item2.SubItems[1];

  if (type1='folder') and (type2<>'folder') then
    begin
    Compare := -1;
    exit;
    end;
  if (type1<>'folder') and (type2='folder') then
    begin
    Compare := 1;
    exit;
    end;

  {if Self.SortDirection = sdDescending then
    begin // swap items
    Item0 := Item2;
    Item2 := Item1;
    Item1 := Item0;
    end;}

  colName := Columns[Self.SortColumn].Caption;
  if colName = sShellCtrlsName then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
  if colName = sShellCtrlsSize then
    Compare := TFileItem(Item1.Data).FileInfo.Size - TFileItem(Item2.Data).FileInfo.Size
  else
  if colName = sShellCtrlsType then
    Compare := CompareText(Item1.SubItems[1], Item2.SubItems[1])
  else
  if colName = sShellCtrlsTime then
    Compare := CompareText(Item1.SubItems[2], Item2.SubItems[2])
  ;

  if Self.SortDirection = sdDescending then
    Compare := Compare * -1;
end;

function TCustomShellListView.GetAllColumnsWidth:integer;
var w:integer;
begin
  // Initial sizes, necessary under Windows CE
  result := FNameColumn.Width;
  if FSizeColumn.Visible then result := result + FSizeColumn.Width;
  if FTypeColumn.Visible then result := result + FTypeColumn.Width;
  w:=FTimeColumn.Width;
  {$ifdef LCLGTK2}
  if w > 120 then w:=120; // GTK2 sometimes makes last column auto expanded. Limit the size.
  {$endif}
  if FTimeColumn.Visible then result := result + w;
end;

procedure TCustomShellListView.AdjustColumnSizes;
var //ncw:integer;
  cw:integer = 0;
begin
  // Initial sizes, necessary under Windows CE
  {cw := GetAllColumnsWidth;
  if Self.ClientWidth > 0 then
     ncw := Self.ClientWidth - cw
  else ncw := Self.Width - cw;
  if ncw > 0 then
     FNameColumn.Width := ncw;
  if FNameColumn.Width < FNameColumn.MinWidth
    then FNameColumn.Width := FNameColumn.MinWidth;}
  FNameColumn.AutoSize:=false;
  FNameColumn.AutoSize:=true;
end;

procedure TCustomShellListView.PopulateWithRoot();
//type PSearchRec = ^TSearchRec;
var
  i: Integer;
  Files: TStringList;
  NewItem: TListItem;
  CurFileName, CurFilePath, sDT: string;
  CurFileSize: Int64;
  DirObjectTypes, FileObjectTypes : TObjectTypes;
  Time : Longint;
  fileItem : TFileItem;
  obj: TObject;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  // Check inputs
  if Trim(FRoot) = '' then Exit;

  if FIsLoading then exit;
  FIsLoading := true;

  Files := TStringList.Create;
  try
    Files.OwnsObjects := True;

    // otFolders, otNonFolders, otHidden
    DirObjectTypes := FObjectTypes;
    exclude(DirObjectTypes, otNonFolders);
    FileObjectTypes := FObjectTypes;
    exclude(FileObjectTypes, otFolders);


    TCustomShellTreeView.GetFilesInDir(FRoot, '*',   DirObjectTypes, Files, fstAlphabet);
    TCustomShellTreeView.GetFilesInDir(FRoot, FMask, FileObjectTypes, Files, fstAlphabet);
    FAbortLoading := false;
    for i := 0 to Files.Count - 1 do
    begin
      if FAbortLoading then break;
      NewItem := Items.Add;
      CurFileName := Files.Strings[i];
      CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
      // First column - Name
      NewItem.Caption := CurFileName;
      // Second column - Size
      // The raw size in bytes is stored in the data part of the item
      CurFileSize := FileSize(CurFilePath); // in Bytes
      //NewItem.Data := Pointer(PtrInt(CurFileSize));
      if CurFileSize < 1024 then
        //NewItem.SubItems.Add(Format(sShellCtrlsBytes, [IntToStr(CurFileSize)]))
        NewItem.SubItems.Add(Format('%s', [IntToStr(CurFileSize)]))
      else if CurFileSize < 1024 * 1024 then
        NewItem.SubItems.Add(Format(sShellCtrlsKB, [IntToStr(CurFileSize div 1024)]))
      else
        NewItem.SubItems.Add(Format(sShellCtrlsMB, [IntToStr(CurFileSize div (1024 * 1024))]));

      obj := Files.Objects[i];
      fileItem := TFileItem.Create(TFileItem(obj).FileInfo, TFileItem(obj).BasePath);
      NewItem.Data := fileItem;

      // Third column - Type
      if fileItem.isFolder
      //if (DirectoryExistsUTF8(CurFilePath))
        then NewItem.SubItems.Add('folder')
        else NewItem.SubItems.Add(ExtractFileExt(CurFileName));

      Time := fileItem.FFileInfo.Time;
      //Time := FileAge(CurFilePath);
      // Forth column - Time
      sDT := FormatDateTime('YYYY-MM-DD hh:mm:ss',FileDateToDateTime(Time));
      NewItem.SubItems.Add(sDT);
      if Assigned(FOnFileAdded)
         then FOnFileAdded(Self,NewItem);
    end;
    Sort;
  finally
    Files.Free;
    FIsLoading := false;
    if Assigned(FOnLoaded)
       then FOnLoaded(Self);
  end;
end;

procedure TCustomShellListView.Resize;
begin
  inherited Resize;
  {$ifdef DEBUG_SHELLCTRLS}
    debugln(':>TCustomShellListView.HandleResize');
  {$endif}

  ///AdjustColumnSizes;

  {$ifdef DEBUG_SHELLCTRLS}
    debugln([':<TCustomShellListView.HandleResize C0.Width=',
     Column[0].Width, ' C1.Width=', Column[1].Width,
     ' C2.Width=', Column[2].Width]);
  {$endif}

end;



  { TCustomFilterComboBox }

  function TCustomFilterComboBox.GetMask: string;
  var
    FilterList: TStrings;
  begin
    Result := '';

    FilterList := TStringList.Create;
    try
      TCustomFilterComboBox.ConvertFilterToStrings(FFilter, FilterList, True, False, True);

      if (ItemIndex >= 0) and (ItemIndex < FilterList.Count) then
      begin
        Result := FilterList[ItemIndex];
      end;
    finally
      FilterList.Free;
    end;
  end;

  procedure TCustomFilterComboBox.SetFilter(const AValue: string);
  begin
    if AValue = FFilter then Exit;

    FFilter := AValue;

    TFilterComboBox.ConvertFilterToStrings(AValue, Items, True, True, False);

    ItemIndex := 0;
  end;

  procedure TCustomFilterComboBox.SetShellListView(const AValue: TShellListView);
  begin
    if FShellListView=AValue then exit;

    FShellListView:=AValue;

    if FShellListView <> nil then begin
      FShellListView.Mask := Mask;
      FreeNotification(FShellListView);
    end;
  end;

  procedure TCustomFilterComboBox.Select;
  begin
    if FShellListView <> nil then
      FShellListView.Mask := Mask;

    inherited Select;
  end;

  procedure TCustomFilterComboBox.Notification(AComponent: TComponent;
    Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if Operation=opRemove then
    begin
      if FShellListView=AComponent then
        FShellListView:=nil;
    end;
  end;

  {------------------------------------------------------------------------------
  This is a parser that converts LCL filter strings to a TStringList

  The parses states are:

  0 - Initial state, is reading a string to be displayed on the filter
  1 - Is reading the extensions

  A LCL filter string looks like this:

  Text files (*.txt *.pas)|*.txt;*.pas|Binaries (*.exe)|*.exe
  or
  Text files (*.txt *.pas)|*.txt;*.pas|Binaries (*.exe)|*.exe|

  The TStrings will contain the following strings if
  AAddDescription = True, AAddFilter = False

  Text files (*.txt *.pas)
  Binaries (*.exe)

  Adapted from the converter initially created for QtWSDialogs.pas
  ------------------------------------------------------------------------------}
  class procedure TCustomFilterComboBox.ConvertFilterToStrings(AFilter: string;
    AStrings: TStrings; AClearStrings, AAddDescription, AAddFilter: Boolean);
  var
    ParserState, Position, i: Integer;
  begin
    if AStrings = nil then Exit;

    if AClearStrings then AStrings.Clear;

    ParserState := 0;
    Position := 1;
    AFilter := AFilter + '|'; // to prevent ignoring of last filter

    for i := 1 to Length(AFilter) do
    begin
      if AFilter[i] = '|' then
      begin
        case ParserState of
        0:
        begin
          if AAddDescription then
            AStrings.Add(Copy(AFilter, Position, i - Position));
          ParserState := 1;
        end;
        1:
        begin
          if AAddFilter then
            AStrings.Add(Copy(AFilter, Position, i - Position));
          ParserState := 0;
        end;
        end;// case

        Position := i + 1;
      end;
    end;
  end;

  constructor TCustomFilterComboBox.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);

    Text := '';
  end;

  destructor TCustomFilterComboBox.Destroy;
  begin

    inherited Destroy;
  end;

  // ------------------------------------


function TCustomShellListView.GetPathFromItem(ANode: TListItem): string;
begin
  Result := IncludeTrailingPathDelimiter(FRoot) + ANode.Caption;
end;

procedure TShellPathPanel.setPath(aPath:string);
var i,n:integer; sl:TStringList; vPath:string;
begin
  if aPath = FPath then exit;
  vPath:=CleanAndExpandDirectory(aPath);

  sl:=TStringList.Create;
  sl.StrictDelimiter := true;
  sl.Delimiter:=DirectorySeparator;
  sl.DelimitedText:=vPath;

  if DirectorySeparator='/' then FPathType:=ptUnixRelative;
  if DirectorySeparator='\' then FPathType:=ptWindowsRelative;

  if FilenameIsUnixAbsolute(vPath) then
    begin
    FPathType:=ptUnixAbsolute;
    sl[0]:= ExtractFileRoot(vPath);
    end;
  if FilenameIsWinAbsolute(vPath) then
    begin
    FPathType:=ptWindowsAbsolute;
    sl[0]:= ExtractFileRoot(vPath);
    end;
  if IsUNCPath(vPath) then
    begin
    FPathType:=ptWindowsUNC;
    sl[0]:='\\'+ExtractUNCVolume(vPath);
    end;


  while ControlPanel.ControlCount > 0 do
    ControlPanel.Controls[0].Free;

  n:=0;
  for i:=0 to sl.Count-1 do
    if sl[i]>'' then
      begin
      insertSpeedButton(sl[i],n);
      inc(n);
      end;

  ControlSelected:=n-1;
  sl.Free;
end;

function TShellPathPanel.getSelectedPath:string;
var i:integer; vPath:string;
begin
  vPath:='';
  for i:=0 to ControlSelected do
    vPath:=vPath+ControlPanel.Controls[i].Caption+DirectorySeparator;

  result:=CleanAndExpandDirectory(vPath);
end;

procedure Register;
begin
  RegisterComponents('Misc',[TShellTreeView, TShellListView, TShellPathPanel]);
end;

end.
