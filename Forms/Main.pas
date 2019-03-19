{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2012, by Timoshenko V.F.                                                }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : http://freeship-plus.pisem.su                                  }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }
{                                                                                             }
{    Copyright © 2015-2015, Conversion to FPC/Lazarus by Mark Malakanov.                      }
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

//{$DEFINE MEMCHECK}

unit Main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF LCL}
  jpeg, ShellAPI, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}

{$IFDEF VER3}
 LazUTF8,
 LazFileUtils,
{$ELSE}
 FileUtil, //deprecated
{$ENDIF}

     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     IntfGraphics, GraphType, FPImage,
     Controls,
     Forms,
     Math,
     Dialogs,
     ExtCtrls,
     ActnList,
     StdCtrls,
     ImgList,
     ComCtrls,
     {$IFDEF MEMCHECK}
        MemCheck,     // Memcheck is used for memory-leak tracking (debugging all)
     {$ENDIF}
     FreeTypes,
     FreeGeometry,
     FreeShipUnit,
     FreeVersionUnit,
     FreeHullformWindow,
     FreeAboutDlg,
     Menus,
     ToolWin,
     Buttons, StdActns, Spin, ExtDlgs,
     DefaultTranslator
;

type

{ TMainForm }

 TMainForm         = class(TForm)
     AboutAction: TAction;
     FontDialog1: TFontDialog;
     HelpAction: THelpAction;
     HelpContents: TMenuItem;
     HelpAbout: TMenuItem;
     PanelActiveLayerColor: TPanel;
     PanelMain: TPanel;
     SelectLeakPoints: TAction;
    FreeShip                   : TFreeShip;
    ActionList1                : TActionList;
    LoadFile                   : TAction;
    SelectLeakPoints1: TMenuItem;
    MH: TMenuItem;
    OST: TMenuItem;
    PropellerRvrs: TMenuItem;
    RBHS: TMenuItem;
    SpinEditFontSize: TSpinEdit;
    StatusBar                  : TPanel;
    MenuImages                 : TImageList;
    ExitProgram                : TAction;
    ShowControlNet             : TAction;
    ShowInteriorEdges          : TAction;
    MainMenu1                  : TMainMenu;
    File1                      : TMenuItem;
    Open       : TMenuItem;
    ExitProgram1               : TMenuItem;
    ToolBarCurves: TToolBar;
    ToolBarFaces: TToolBar;
    ToolBarFile: TToolBar;
    ToolBarEdit: TToolBar;
    ToolBarEdges: TToolBar;
    ToolBarPoints: TToolBar;
    ToolBarVisibility: TToolBar;
    ToolBarLayers: TToolBar;
    ToolButtonSelect: TToolButton;
    ToolButtonRedo: TToolButton;
    ToolButtonUndo: TToolButton;
    Visibility1                : TMenuItem;
    ShowControlNet1            : TMenuItem;
    ShowInteriorEdges1         : TMenuItem;
    Window1                    : TMenuItem;
    Cascade1                   : TMenuItem;
    Tile1                       : TMenuItem;
    NewWindow1                 : TMenuItem;
    NewWindow                  : TAction;
    TileWindow                 : TAction;
    CascadeWindow              : TAction;
    N1                         : TMenuItem;
    BothSides                  : TAction;
    Showbothsides1             : TMenuItem;
    FileSaveas                 : TAction;
    Save1                      : TMenuItem;
    LayerAutoGroup             : TAction;
    Layer1                     : TMenuItem;
    Autogroup1                 : TMenuItem;
    NewLayer                   : TAction;
    New1                       : TMenuItem;
    Delete: TAction;
    ToolButtonOpenFile: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    LayerBox: TComboBox;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    Edit1: TMenuItem;
    Point1: TMenuItem;
    Edge1: TMenuItem;
    Face1: TMenuItem;
    EdgeCollapse: TAction;
    ToolButton13: TToolButton;
    Collapse1: TMenuItem;
    ToolButtonDelete: TToolButton;
    Delete1: TMenuItem;
    NewEdge: TAction;
    ToolButton16: TToolButton;
    New2: TMenuItem;
    ImportFEF: TAction;
    ImportOff1: TMenuItem;
    EdgeCrease: TAction;
    ToolButton17: TToolButton;
    Crease1: TMenuItem;
    DeselectAll: TAction;
    Selection1: TMenuItem;
    Clearselection1: TMenuItem;
    PointCollapse: TAction;
    ToolButton18: TToolButton;
    PointCollapse1: TMenuItem;
    ColorDialog: TColorDialog;
    ActiveLayerColor: TAction;
    Activelayercolor1: TMenuItem;
    DeleteEmptyLayers: TAction;
    Deleteempty1: TMenuItem;
    LayerDialog: TAction;
    Deleteempty2: TMenuItem;
    NewModel: TAction;
    ToolButton20: TToolButton;
    New3: TMenuItem;
    ShowStations: TAction;
    ToolButton21: TToolButton;
    Stations1: TMenuItem;
    ShowButtocks: TAction;
    ToolButton22: TToolButton;
    ShowWaterlines: TAction;
    ToolButton23: TToolButton;
    Buttocks1: TMenuItem;
    Waterlines1: TMenuItem;
    NewFace: TAction;
    ToolButton24: TToolButton;
    New4: TMenuItem;
    IntersectionDialog: TAction;
    ToolButton26: TToolButton;
    EdgeExtrude: TAction;
    ToolButton27: TToolButton;
    Extrude1: TMenuItem;
    Help1: TMenuItem;
    EdgeSplit: TAction;
    ToolButton29: TToolButton;
    Split1: TMenuItem;
    ExportFEF: TAction;
    ExportFEFfile1: TMenuItem;
    EditProjectSettings: TAction;
    Project1: TMenuItem;
    Projectsettings1: TMenuItem;
    CheckModel: TAction;
    ools1: TMenuItem;
    Analyzesurface1: TMenuItem;
    ToolButton30: TToolButton;
    ShowNormals: TAction;
    DesignHydrostatics: TAction;
    Calculations1: TMenuItem;
    Hydrostatics1: TMenuItem;
    Normals1: TMenuItem;
    ImportVRML: TAction;
    Export1: TMenuItem;
    VRML1: TMenuItem;
    Import1: TMenuItem;
    RemoveNegative: TAction;
    Removenegative1: TMenuItem;
    RotateModel: TAction;
    Rotatemodel1: TMenuItem;
    RotateModelM: TAction;
    Rotatemodel2: TMenuItem;
    ScaleModel: TAction;
    Scale3D1: TMenuItem;
    ShowGrid: TAction;
    ToolButton31: TToolButton;
    Analyzesurface2: TMenuItem;
    Undo: TAction;
    Undo1: TMenuItem;
    Panel2: TPanel;
    HydrostaticsDialog: TAction;
    Hydrostatics2: TMenuItem;
    ExportObj: TAction;
    WavefrontfileObj1: TMenuItem;
    InvertFace: TAction;
    Invert1: TMenuItem;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    Preferences: TAction;
    Preferences1: TMenuItem;
    N2: TMenuItem;
    ImportBodyplan: TAction;
    ExportDXF3DPolylines: TAction;
    ExportDXFPolylines1: TMenuItem;
    ExportDXFFaces: TAction;
    DXF3Dfaces1: TMenuItem;
    ImportHullFile: TAction;
    Carlssonhulfile1: TMenuItem;
    ExportOffsets: TAction;
    Offsets1: TMenuItem;
    MoveModel: TAction;
    Deselectall2: TMenuItem;
    AddPoint: TAction;
    Add1: TMenuItem;
    Intersections1: TMenuItem;
    DevelopLayers: TAction;
    Developplates1: TMenuItem;
    ExportArchimedes: TAction;
    N3: TMenuItem;
    ArchimedesMB1: TMenuItem;
    ToolButton34: TToolButton;
    ShowLinesplan: TAction;
    Linesplan1: TMenuItem;
    ShowDiagonals: TAction;
    ToolButton35: TToolButton;
    Diagonals1: TMenuItem;
    Recentfiles: TMenuItem;
    N4: TMenuItem;
    ImportCarene: TAction;
    Carenefile1: TMenuItem;
    ShowMarkers: TAction;
    ToolButton36: TToolButton;
    Markers1: TMenuItem;
    DeleteMarkers: TAction;
    Deletemarkers1: TMenuItem;
    ImportSurface: TAction;
    Surface1: TMenuItem;
    Showcurvature: TAction;
    ToolButton37: TToolButton;
    IncreaseCurvatureScale: TAction;
    DecreaseCurvatureScale: TAction;
    N5: TMenuItem;
    Decrcurvaturescale1: TMenuItem;
    Incrcurvaturescale1: TMenuItem;
    FileSave: TAction;
    ToolButton38: TToolButton;
    Save2: TMenuItem;
    Curvature1: TMenuItem;
    ImportChines: TAction;
    Chines1: TMenuItem;
    Curve1: TMenuItem;
    ShowControlCurves: TAction;
    Controlcurves1: TMenuItem;
    ToolButton2: TToolButton;
    NewCurve: TAction;
    AddCurve1: TMenuItem;
    ToolButton40: TToolButton;
    ExportCoordinates: TAction;
    Coordinates1: TMenuItem;
    InsertPlane: TAction;
    InsertPlane1: TMenuItem;
    ToolButton41: TToolButton;
    PointsLock: TAction;
    PointsLock1: TMenuItem;
    PointsUnlock: TAction;
    Unlockpoints1: TMenuItem;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    PointsUnlockAll: TAction;
    Unlockallpoints1: TMenuItem;
    ToolButton44: TToolButton;
    Markers2: TMenuItem;
    ImportMarkers: TAction;
    Import2: TMenuItem;
    ExportMichlet: TAction;
    MichletCFD1: TMenuItem;
    ExportAddMass: TAction;
    AddMassCFD1: TMenuItem;
    ResistanceKaper: TAction;
    ResistancePlaning: TAction;
    Resistance1: TMenuItem;
    Kaper1: TMenuItem;
    ResistanceDelft: TAction;
    Delftyachtseries1: TMenuItem;
    ResistanceHoltr: TAction;
    ResistanceOST: TAction;
    ResistanceRBHS: TAction;
    ResistanceMH: TAction;
    ResistanceHollen: TAction;
    ResistanceOortmer: TAction;
    ResistanceFungLeib: TAction;
    Holtr1: TMenuItem;
    Hollen: TMenuItem;
    Oortmer: TMenuItem;
    FungLeib: TMenuItem;
    Planing1: TMenuItem;
    PropellerTask1: TMenuItem;
    PropTask4_: TAction;
    PropTask5_: TAction;
    PropTask1_: TAction;
    PropTask2_: TAction;
    PropTask3_: TAction;
    PropTask4: TMenuItem;
    PropTask5: TMenuItem;
    PropTask1: TMenuItem;
    PropTask2: TMenuItem;
    PropTask3: TMenuItem;
    PropTaskRvrs_: TAction;
    PropTaskRvrs: TMenuItem;
    HydrodynManeuv_: TAction;
    HydrodynManeuv: TMenuItem;
    HydrodynTask: TMenuItem;
    HydrodynTask1_: TAction;
    HydrodynTask2_: TAction;
    HydrodynTask3_: TAction;
    HydrodynTask4_: TAction;
    HydrodynTask1: TMenuItem;
    HydrodynTask2: TMenuItem;
    HydrodynTask3: TMenuItem;
    HydrodynTask4: TMenuItem;
    Panel3: TPanel;
    PointAlign: TAction;
    ToolButton45: TToolButton;
    Projectline1: TMenuItem;
    ImportMichletWaves: TAction;
    N6: TMenuItem;
    ImportMichletWaves1: TMenuItem;
    ShowHydrostatics: TAction;
    ToolButton46: TToolButton;
    Hydrostaticdata1: TMenuItem;
    MirrorFace: TAction;
    ransform1: TMenuItem;
    MirrorFace1: TMenuItem;
    ExportDXF2DPolylines: TAction;
    DXF2DPolylines1: TMenuItem;
    Panel4: TPanel;
    TransformLackenby: TAction;
    Lackenby1: TMenuItem;
    ExportIGES: TAction;
    IGES1: TMenuItem;
    ExportPart: TAction;
    Part1: TMenuItem;
    ImportPart: TAction;
    Part2: TMenuItem;
    LayerIntersection: TAction;
    ToolButton47: TToolButton;
    Saveas1: TMenuItem;
    KeelRudderWizard: TAction;
    Deleteempty3: TMenuItem;
    Redo: TAction;
    Archimedes1: TMenuItem;
    ClearUndo: TAction;
    N7: TMenuItem;
    Undohistory1: TMenuItem;
    Clear1: TMenuItem;
    ShowUndoHistory: TAction;
    Show1: TMenuItem;
    ImportPolyCad: TAction;
    PolyCad1: TMenuItem;
    RemoveUnusedPoints: TAction;
    Removeunusedpoints1: TMenuItem;
    ExportGHS: TAction;
    GHS1: TMenuItem;
    ExportPAM: TAction;
    PAM1: TMenuItem;	
    ShowFlowlines: TAction;
    Flowlines1: TMenuItem;
    ToolButton48: TToolButton;
    AddCylinder: TAction;
    AddCylinder1: TMenuItem;
    SelectAll: TAction;
    Selectall1: TMenuItem;
    ExportSTL: TAction;
    STL1: TMenuItem;
    CrossCurves: TAction;
    Crosscurves1: TMenuItem;
    //SelectLeakPoints: TAction;
    SelectionSeparator1: TMenuItem;
    Select_LeakPoints: TMenuItem;

    FMDIChildList : TList;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);

    procedure LoadFileExecute(Sender   : TObject);
    procedure ExitProgramExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelMainResize(Sender: TObject);
    procedure ShowControlNetExecute(Sender: TObject);
    procedure ShowInteriorEdgesExecute(Sender: TObject);
    procedure NewWindowExecute(Sender: TObject);
    procedure SpinEditFontSizeChange(Sender: TObject);
    procedure TileWindowExecute(Sender: TObject);
    procedure CascadeWindowExecute(Sender: TObject);
    procedure BothSidesExecute(Sender: TObject);
    procedure FreeShipFileChanged(Sender: TObject);
    //procedure PrecisionBoxChange(Sender: TObject);
    procedure FileSaveasExecute(Sender: TObject);
    procedure LayerAutoGroupExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewLayerExecute(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure EdgeCollapseExecute(Sender: TObject);
    procedure NewEdgeExecute(Sender: TObject);
    procedure ImportFEFExecute(Sender: TObject);
    procedure EdgeCreaseExecute(Sender: TObject);
    procedure DeselectAllExecute(Sender: TObject);
    procedure PointCollapseExecute(Sender: TObject);
    procedure LayerBoxChange(Sender: TObject);
    procedure PanelActiveLayerColorClick(Sender: TObject);
    procedure ActiveLayerColorExecute(Sender: TObject);
    procedure DeleteEmptyLayersExecute(Sender: TObject);
    procedure LayerDialogExecute(Sender: TObject);
    procedure NewModelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowStationsExecute(Sender: TObject);
    procedure ShowButtocksExecute(Sender: TObject);
    procedure ShowWaterlinesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NewFaceExecute(Sender: TObject);
    procedure IntersectionDialogExecute(Sender: TObject);
    procedure EdgeExtrudeExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure EdgeSplitExecute(Sender: TObject);
    procedure ExportFEFExecute(Sender: TObject);
    procedure EditProjectSettingsExecute(Sender: TObject);
    procedure CheckModelExecute(Sender: TObject);
    procedure ShowNormalsExecute(Sender: TObject);
    procedure DesignHydrostaticsExecute(Sender: TObject);
    procedure ImportVRMLExecute(Sender: TObject);
    procedure RemoveNegativeExecute(Sender: TObject);
    procedure RotateModelExecute(Sender: TObject);
    procedure RotateModelMExecute(Sender: TObject);
    procedure ScaleModelExecute(Sender: TObject);
    procedure ShowGridExecute(Sender: TObject);
    procedure ToolButtonSelectClick(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure FreeShipUpdateUndoData(Sender: TObject);
    procedure HydrostaticsDialogExecute(Sender: TObject);
    procedure ExportObjExecute(Sender: TObject);
    procedure InvertFaceExecute(Sender: TObject);
    procedure PreferencesExecute(Sender: TObject);
    procedure ImportBodyplanExecute(Sender: TObject);
    procedure ExportDXF3DPolylinesExecute(Sender: TObject);
    procedure ExportDXFFacesExecute(Sender: TObject);
    procedure ImportHullFileExecute(Sender: TObject);
    procedure ExportOffsetsExecute(Sender: TObject);
    procedure MoveModelExecute(Sender: TObject);
    procedure AddPointExecute(Sender: TObject);
    procedure DevelopLayersExecute(Sender: TObject);
    procedure ExportArchimedesExecute(Sender: TObject);
    procedure ShowLinesplanExecute(Sender: TObject);
    procedure ShowDiagonalsExecute(Sender: TObject);
    procedure FreeShipUpdateRecentFileList(Sender: TObject);
    procedure ImportCareneExecute(Sender: TObject);
    procedure ShowMarkersExecute(Sender: TObject);
    procedure DeleteMarkersExecute(Sender: TObject);
    procedure ImportSurfaceExecute(Sender: TObject);
    procedure ShowcurvatureExecute(Sender: TObject);
    procedure IncreaseCurvatureScaleExecute(Sender: TObject);
    procedure DecreaseCurvatureScaleExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure ImportChinesExecute(Sender: TObject);
    procedure ShowControlCurvesExecute(Sender: TObject);
    procedure NewCurveExecute(Sender: TObject);
    procedure ExportCoordinatesExecute(Sender: TObject);
    procedure InsertPlaneExecute(Sender: TObject);
    procedure PointsLockExecute(Sender: TObject);
    procedure PointsUnlockExecute(Sender: TObject);
    procedure PointsUnlockAllExecute(Sender: TObject);
    procedure ImportMarkersExecute(Sender: TObject);
    procedure ExportMichletExecute(Sender: TObject);
    procedure ExportAddMassExecute(Sender: TObject);
    procedure ResistanceKaperExecute(Sender: TObject);
    procedure ResistanceDelftExecute(Sender: TObject);
    procedure ResistanceHoltrExecute(Sender: TObject);
    procedure ResistanceHollenExecute(Sender: TObject);
    procedure ResistanceOortmerExecute(Sender: TObject);
    procedure ResistanceFungLeibExecute(Sender: TObject);
    procedure ResistanceOSTExecute(Sender: TObject);
    procedure ResistanceRBHSExecute(Sender: TObject);
    procedure ResistanceMHExecute(Sender: TObject);
    procedure ResistancePlaningExecute(Sender: TObject);
    procedure PropTask1Execute(Sender: TObject);
    procedure PropTask2Execute(Sender: TObject);
    procedure PropTask3Execute(Sender: TObject);
    procedure PropTask4Execute(Sender: TObject);
    procedure PropTask5Execute(Sender: TObject);
    Procedure PropTaskRvrsExecute(Sender: TObject);
    Procedure HydrodynManeuvExecute(Sender: TObject);
    procedure HydrodynTask1Execute(Sender: TObject);
    procedure HydrodynTask2Execute(Sender: TObject);
    procedure HydrodynTask3Execute(Sender: TObject);
    procedure HydrodynTask4Execute(Sender: TObject);
    procedure FreeShipChangeCursorIncrement(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure PointAlignExecute(Sender: TObject);
    procedure ImportMichletWavesExecute(Sender: TObject);
    procedure ShowHydrostaticsExecute(Sender: TObject);
    procedure MirrorFaceExecute(Sender: TObject);
    procedure ExportDXF2DPolylinesExecute(Sender: TObject);
    procedure FreeShipUpdateGeometryInfo(Sender: TObject);
    procedure TransformLackenbyExecute(Sender: TObject);
    procedure ExportIGESExecute(Sender: TObject);
    procedure ExportPartExecute(Sender: TObject);
    procedure ImportPartExecute(Sender: TObject);
    procedure LayerIntersectionExecute(Sender: TObject);
    procedure KeelRudderWizardExecute(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure ClearUndoExecute(Sender: TObject);
    procedure ShowUndoHistoryExecute(Sender: TObject);
    procedure ImportPolyCadExecute(Sender: TObject);
    procedure RemoveUnusedPointsExecute(Sender: TObject);
    procedure ExportGHSExecute(Sender: TObject);
    procedure ExportPAMExecute(Sender: TObject);
    procedure ShowFlowlinesExecute(Sender: TObject);
    procedure AddCylinderExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure ExportSTLExecute(Sender: TObject);
    procedure CrossCurvesExecute(Sender: TObject);
    procedure SelectLeakPointsExecute(Sender: TObject);
    procedure LoadMostRecentFile;
    procedure LoadNamedFile(FileName:string);
    procedure dumpIcons;
    procedure LoadToolIcons;
    function  getToolbarControlsWidth(tb:TToolBar): integer;
    function  getAllToolbarsControlWidth: integer;
    procedure AlignAllToolbars;
    procedure SetAllActionsEnabled(val : boolean);
    procedure InitiallyLoadModel;
   private    { Private declarations }
      FAllToolbarsControlsWidth : integer;
      FToolBarFileControlsWidth : integer;
      FToolBarVisibilityControlsWidth : integer;
      FToolBarLayersControlsWidth : integer;
      FToolBarPointsControlsWidth : integer;
      FToolBarEditControlsWidth : integer;
      FToolBarEdgesControlsWidth : integer;
      FToolBarFacesControlsWidth : integer;
      FToolBarCurvesControlsWidth : integer;

      procedure FLoadRecentFile(sender:TObject);
      procedure FreeShipChangeLayerData(Sender: TObject);
      procedure FreeShipChangeActiveLayer(Sender: TObject;Layer: TFreeSubdivisionLayer);
      procedure FOnSelectItem(Sender:TObject);
      procedure FOpenHullWindows;   // Creates 4 different views on the hullform
   public     { Public declarations }
      FFileName : string;
      FModelInitallyLoaded : boolean;
      {$IFDEF FPC}
      function  MDIChildCount: Integer; override;
      function  GetMDIChildren(AIndex: Integer): TCustomForm; override;
      procedure AbandonMDIChildren(AIndex: Integer);
      procedure Tile;
      procedure Cascade;
      {$ENDIF}
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure SetCaption;
      procedure UpdateMenu;
      procedure ShowRecentFilesDialog;
  end;

var MainForm: TMainForm;

implementation

uses FreeSplashWndw,
     FreeLinesplanFrm,
     FreeControlPointFrm,
     FreeKeelWizardDlg,
     FreeLanguageSupport,
     FreeEmptyModelChooserDlg,
     RibbonToolBarMgr,
     TileDialog;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure SortControlsByXY(tb:TWinControl);
var c1,c2: TControl; i,j, t1,t2, l1,l2:integer;
begin
  {
  Writeln();
  Writeln(tb.Name);
  for i:=0 to tb.ControlCount-1 do
   with tb.Controls[i] do
    Writeln( Name, ' t:',Top, ' l:', Left);
   }
  i:=0;
  while i <= tb.ControlCount-2 do
   begin
   j:=1;
   while j <= tb.ControlCount-1 do
      begin
      c1 := tb.Controls[j-1];
      c2 := tb.Controls[j];
      t1:=c1.Top; t2:=c2.Top;
      l1:=c1.Left; l2:=c2.Left;
      if c1.Top > c2.Top then
        begin
          tb.RemoveControl(c2);
          tb.InsertControl(c2,j-1);
        end
      else
        if (c1.Top = c2.Top) and (c1.Left > c2.Left) then
          begin
            tb.RemoveControl(c2);
            tb.InsertControl(c2,j-1);
          end
        else
          inc(j);
      end;
    inc(i);
    end;
 {
  Writeln();
  Writeln(tb.Name);
  for i:=0 to tb.ControlCount-1 do
   with tb.Controls[i] do
    Writeln( Name, ' t:',Top, ' l:', Left);
    }
end;


constructor TMainForm.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FMDIChildList := TList.Create;
 FFileName:='';

 //FreeShip.Preferences.SetDefaults;

{ SortControlsByXY(PanelMain);
 SortControlsByXY(ToolBarFile);
 SortControlsByXY(ToolBarVisibility);
 SortControlsByXY(ToolBarLayers);
 SortControlsByXY(ToolBarPoints);
 SortControlsByXY(ToolBarEdit);
 SortControlsByXY(ToolBarEdges);
 SortControlsByXY(ToolBarFaces);
 SortControlsByXY(ToolBarCurves);
 }
end;

destructor TMainForm.Destroy;
var i: Integer;
begin
 for i:=0 to FMDIChildList.Count -1 do
   TFreeHullWindow(FMDIChildList.Items[i]).Destroy;
 FMDIChildList.Destroy;
 inherited Destroy;
end;

procedure TMainForm.FormChangeBounds(Sender: TObject);
begin
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  //if MainForm.IsResizing then exit;
  //PanelMain.Invalidate;
  //Application.ProcessMessages;
  ///AlignAllToolbars;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
 // this is just to kick toolbar to autoresize
{ Self.Height:=Self.Height+1;
 Self.Resize;
 Self.Height:=Self.Height-1;
 Self.Resize;
 Self.Invalidate;
 }
end;

// total width of all controls in toolbar
function TMainForm.getToolbarControlsWidth(tb:TToolBar): integer;
var i:integer; c: TToolButton;
begin
  result := 0;
  for i:=0 to tb.ButtonCount-1 do
    begin
      c := tb.Buttons[i];
      result := result + c.Width + c.BorderSpacing.Right+c.BorderSpacing.Left+c.BorderSpacing.Around;
    end;
end;

function TMainForm.getAllToolbarsControlWidth: integer;
begin
  FToolBarFileControlsWidth := getToolbarControlsWidth(ToolBarFile);
  FToolBarVisibilityControlsWidth := getToolbarControlsWidth(ToolBarVisibility);
  FToolBarLayersControlsWidth := getToolbarControlsWidth(ToolBarLayers);
  FToolBarPointsControlsWidth := getToolbarControlsWidth(ToolBarPoints);
  FToolBarEditControlsWidth := getToolbarControlsWidth(ToolBarEdit);
  FToolBarEdgesControlsWidth := getToolbarControlsWidth(ToolBarEdges);
  FToolBarFacesControlsWidth := getToolbarControlsWidth(ToolBarFaces);
  FToolBarCurvesControlsWidth := getToolbarControlsWidth(ToolBarCurves);

  FAllToolbarsControlsWidth :=
    FToolBarFileControlsWidth +
    FToolBarVisibilityControlsWidth +
    FToolBarLayersControlsWidth +
    FToolBarPointsControlsWidth +
    FToolBarEditControlsWidth +
    FToolBarEdgesControlsWidth +
    FToolBarFacesControlsWidth +
    FToolBarCurvesControlsWidth;

  result := FAllToolbarsControlsWidth;
end;

procedure TMainForm.AlignAllToolbars;
var AllW, lft, gap:integer;
    // max bottom Y of all controls in toolbar
    function getToolbarControlsBottom(tb:TToolBar): integer;
    var i:integer; c: TControl;
    begin
      result := 0;
      for i:=0 to tb.ControlCount-1 do
        begin
          c := tb.Controls[i];
          result := max(result, c.BoundsRect.Bottom + c.BorderSpacing.Bottom+c.BorderSpacing.Around);
        end;
    end;

    // max right X of all controls in toolbar
    function getToolbarControlsRight(tb:TToolBar): integer;
    var i:integer; c: TControl;
    begin
      result := 0;
      for i:=0 to tb.ControlCount-1 do
        begin
          c := tb.Controls[i];
          result := max(result, c.BoundsRect.Right + c.BorderSpacing.Right+c.BorderSpacing.Around);
        end;
    end;

    // max right of a control in toolbar smaller than X
    function getToolbarXtrunc(tb:TToolBar; X:integer): integer;
    var i,r:integer; c: TControl;
    begin
      result := 0; r:=0;
      i:=0;
      while (i<=tb.ControlCount-1) do
        begin
          c := tb.Controls[i];
          r := r + c.Width + c.BorderSpacing.Right+c.BorderSpacing.Around;
          if (r<=X)
            then result := max(result,r);
          inc(i);
        end;
    end;

    function alignToolbar(tb:TToolBar; tbcw, lft, wdth, cnt: integer): integer;
    var tbch, r,w,bw :integer; c: TControl; b:TToolButton;
    begin
      tb.Top:=0;
      tb.Parent.Left:=lft;
      //tbcw := getToolbarControlsWidth(tb);
      //if tb.Name='ToolBarFile'
      //   then c:=c;
      bw := tb.ButtonWidth;
      if tb.ButtonCount > 0
        then
          begin
          b := tb.Buttons[0];
          bw := b.Width;
          end;


      // leave some space (bw+4)*cnt
      w:=round((wdth) * ((tbcw - gap) / (FAllToolbarsControlsWidth)));

      //tb.Width := w;
      r:=getToolbarXtrunc(tb,w);
      if (lft+r)>(wdth-(bw+4)*cnt)
        then r:=getToolbarXtrunc(tb,r-bw);

      tb.Width := max(r+4, bw+4);
      //tb.Width := w;
      //MainForm.Caption := 'ToolBarMain.Width:' + IntToStr(ToolBarMain.Width);

      //tb.Height := 16;
      tb.ReAlign;
      tb.Invalidate;
      //tb.Width := getToolbarControlsRight(tb)+4;
      //tb.ReAlign;
      Application.ProcessMessages;
      tbch:=getToolbarControlsBottom(tb);
      tb.Height := tbch + 4;
      if PanelMain.Height < tb.Height+2 then PanelMain.Height := tb.Height + 2;
      //tb.Parent.Height := tb.Height;
      //tb.Parent.Width := tb.Width;
      //tb.InvalidatePreferredSize;
      //Application.ProcessMessages;
      result:=tb.Parent.BoundsRect.Right;
    end;
var pnlWidth: integer;
begin
  gap:=4;
  if FAllToolbarsControlsWidth=0
   then FAllToolbarsControlsWidth:=getAllToolbarsControlWidth;
  lft:=gap;

  //PanelMain.Width := ToolBarMain.Width - 2; // for some reason PanelMain does not autosize
  PanelMain.Height := 20;
  pnlWidth := PanelMain.Width;
  lft := alignToolbar(ToolBarFile, FToolBarFileControlsWidth, lft+gap, pnlWidth, 7);
  lft := alignToolbar(ToolBarEdit, FToolBarEditControlsWidth, lft+gap, pnlWidth, 6);
  lft := alignToolbar(ToolBarVisibility, FToolBarVisibilityControlsWidth, lft+gap, pnlWidth, 5);
  lft := alignToolbar(ToolBarLayers, FToolBarLayersControlsWidth, lft+gap, pnlWidth, 4);
  lft := alignToolbar(ToolBarPoints, FToolBarPointsControlsWidth, lft+gap, pnlWidth, 3);
  lft := alignToolbar(ToolBarEdges, FToolBarEdgesControlsWidth, lft+gap, pnlWidth, 2);
  lft := alignToolbar(ToolBarFaces, FToolBarFacesControlsWidth, lft+gap, pnlWidth, 1);
  lft := alignToolbar(ToolBarCurves, FToolBarCurvesControlsWidth, lft+gap, pnlWidth, 0);


  {
  //make all toolbars uniform height
  ToolBarFile.Height:=PanelMain.Height-2;
  ToolBarEdit.Height:=PanelMain.Height-2;
  ToolBarVisibility.Height:=PanelMain.Height-2;
  ToolBarLayers.Height:=PanelMain.Height-2;
  ToolBarPoints.Height:=PanelMain.Height-2;
  ToolBarEdges.Height:=PanelMain.Height-2;
  ToolBarFaces.Height:=PanelMain.Height-2;
  ToolBarCurves.Height:=PanelMain.Height-2;
   }
  //ToolBarMain.Height:=PanelMain.Height;
end;

procedure TMainForm.InitiallyLoadModel;
var FileExt: string;
begin
if FFileName = '' then
  LoadMostRecentFile;

if (FFileName = '') and not FreeShip.ModelLoaded then  //default behaviour if no recent file defined
  NewModelExecute(Self)
else
if (FFileName <> '') and not FreeShip.ModelLoaded then
begin
   // Skip translation
   FileExt := Uppercase(ExtractFileExt(FFileName));
   if (FileExists(FFileName) { *Converted from FileExists* })
     and ( (FileExt = '.FBM') or (FileExt = '.FTM') ) then
     begin
        FOpenHullWindows;
        FreeShip.Edit.File_Load(FFileName);
     end
   else
   begin
      //MessageDlg(Userstring(106)+' '+FFileName,mtError,[mbOk],0);
      FreeEmptyModelChooserDialog:=TFreeEmptyModelChooserDialog.Create(Self);
      ShowTranslatedValues(FreeEmptyModelChooserDialog);
      if FreeEmptyModelChooserDialog.Execute(FFileName)
      then
        begin
        if FreeEmptyModelChooserDialog.RbCreateNew.Checked
          then NewModelExecute(Self)
        else
        if FreeEmptyModelChooserDialog.RbLoadFile.Checked
          then LoadFileExecute(Self)
        end;
      FreeEmptyModelChooserDialog.Free;
   end
   // End Skip translation
end;
SetCaption;
LoadToolIcons;
UpdateMenu;
end;

var inActivation: boolean = false;

procedure TMainForm.FormActivate(Sender: TObject);
var i:integer;
begin
  if inActivation then exit;
  inActivation:=true;

  if not FModelInitallyLoaded then
    begin
       InitiallyLoadModel;
       FModelInitallyLoaded := true;
    end;

  for i:=0 to FMDIChildList.Count -1 do begin
    TFreeHullWindow(FMDIChildList.Items[i]).BringToFront;
    Application.ProcessMessages;
  end;
  BringToFront;
  Application.BringToFront;
  Application.ProcessMessages;
  inActivation:=false;
end;

procedure TMainForm.FOnselectItem(Sender:TObject);
var Face1 : TFreeSubdivisionControlFace;
    Face2 : TFreeSubdivisionControlFace;
    Diff  : Boolean;
    I     : Integer;

begin
   if (Sender is TFreeSubdivisionControlPoint)
     and (Sender=FreeShip.ActiveControlPoint)
     and (FreeShip.ActiveControlPoint.Selected=false)
   then
     begin
        // The active controlpoint was deselected, probably internally by the subdivision surface.
        // Set the FreeShip.ActiveControlPoint to nil (which also closes the controlpoint window)
        FreeShip.ActiveControlPoint:=nil;
     end;
   if FreeShip.NumberOfSelectedControlFaces>0 then
   begin
      // set the layerbox itemindex to the index of the layer of the selected controlfaces
      Face1:=FreeShip.SelectedControlFace[0];
      // check if all selected controlfaces belong to the same layer
      Diff:=False;
      for I:=1 to FreeShip.NumberOfSelectedControlFaces do
      begin
         Face2:=FreeShip.SelectedControlFace[I-1];
         if Face1.Layer<>Face2.Layer then
         begin
            Diff:=True;
            Break;
         end;
      end;
      if not Diff then
      begin
         FreeShipChangeActiveLayer(self,Face1.Layer);
      end else FreeShipChangeActiveLayer(self,nil);
   end else FreeShipChangeActiveLayer(self,FreeShip.ActiveLayer);
   UpdateMenu;
end;{TMainForm.FOnselectItem}


{$ifdef FPC}
// implement it in FPC
function TMainForm.MDIChildCount: Integer;
begin
  Result := FMDIChildList.Count;
end;
{$endif}

// Creates 4 different views on the hullform
procedure TMainForm.FOpenHullWindows;
var HullformWindow: TFreeHullWindow;
    I             : Integer;
begin
  Panel3.Destroy;
  Panel3 := TPanel.Create(Self);
  Panel3.Parent := StatusBar;
  Panel3.Color := clWhite;
  Panel3.Align := alLeft;
  Panel3.Caption := 'Distance:';

  Panel4.Destroy;
  Panel4 := TPanel.Create(Self);
  Panel4.Parent := StatusBar;
  Panel4.Color := clWhite;
  Panel4.Align := alLeft;

   if MDIChildCount=0 then
   begin
      for I:=0 to 3 do
      begin
         // open a new window
         HullformWindow:=TFreeHullWindow.Create(Self);
         FMDIChildList.Add(HullformWindow);
         ///HullformWindow:=TFreeHullWindow.CreateParented(self.Handle);
         //HullformWindow.ParentWindow := self.Handle ;
         // Connect viewport to freeship component
         //HullformWindow.FreeShip:=FreeShip;
         HullformWindow.Viewport.ViewType:=TFreeViewType(I);
         ShowTranslatedValues(HullformWindow);
         HullformWindow.SetCaption;
         HullformWindow.Left := I * 40;
         HullformWindow.Top := I * 40;
      end;
      {$ifndef LCL}
        TileMode := tbHorizontal;
      {$endif}
      Tile;
   end;
   SetCaption;
end;{TMainForm.FOpenHullWindows}

procedure TMainForm.SetCaption;
begin
   // Skip translation
   if FreeShip.FileChanged then Caption:='Free!Ship  : '+FreeShip.Filename+' ('+Userstring(280)+')'
                           else Caption:='Free!Ship  : '+FreeShip.Filename+' ('+Userstring(281)+')';
   // End Skip translation
end;{TMainForm.SetCaption}

procedure TMainForm.SetAllActionsEnabled(val : boolean);
var i: integer; A: TAction;
begin
  for i:=0 to ActionList1.ActionCount-1 do
  begin
    A := TAction(ActionList1.Actions[i]);
    A.Enabled:=val;
  end;
end;

procedure TMainForm.UpdateMenu;
var I       : Integer;
    NLayers : Integer;
    FExecDirectory     : string;
//    FileToFind         : string;
// In this procedure all actions are set to enabled/disabled according to the current state
// and selected items
begin
   NLayers:=0;
   For I:=1 to Freeship.NumberOfLayers do if Freeship.Layer[I-1].Count>0 then inc(NLayers);

   {
   // disable almost all actions if model was not loaded for some reason
   if not Freeship.ModelLoaded then
     begin
        FreeShip.Filename:='MODEL NOT LOADED!';
        FreeShip.FilenameSet:=false;

        SetAllActionsEnabled(false);

        NewModel.Enabled:=true;
        LoadFile.Enabled:=true;
        ExitProgram.Enabled:=true;
        HelpAction.Enabled:=true;
        AboutAction.Enabled:=true;

        //ImportFEF.Enabled:=true;
        //ImportCarene.Enabled:=true;
        //ImportVRML.Enabled:=true;
        //ImportHullFile.Enabled:=true;
     end;
    }

   // File menu
   FileSaveas.Enabled:=(FreeShip.Surface.NumberOfControlPoints>0) or (Freeship.FileChanged) or (Freeship.FilenameSet);
   FileSave.Enabled:=(FileSaveas.Enabled) and (Freeship.FilenameSet);

   ImportMichletWaves1.Enabled:=(MDIChildCount>0) and (Freeship.Surface.NumberOfControlFaces>1);
   ExportFEF.Enabled:=Freeship.Surface.NumberOfControlPoints>0;
   ExportObj.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   ExportSTL.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   ExportDXF3DPolylines.Enabled:=((FreeShip.NumberofStations>0) and (FreeShip.Visibility.ShowStations)) or
                                 ((Freeship.NumberofButtocks>0) and (Freeship.Visibility.ShowButtocks)) or
                                 ((Freeship.NumberofWaterlines>0) and (FreeShip.Visibility.ShowWaterlines)) or
                                 ((FreeShip.NumberofDiagonals>0) and (FreeShip.Visibility.ShowDiagonals)) or
                                 ((FreeShip.NumberofControlcurves>0) and (FreeShip.Visibility.ShowControlcurves));
   ExportDXF2DPolylines.Enabled:=((FreeShip.NumberofStations>0) and (FreeShip.Visibility.ShowStations)) or
                                 ((Freeship.NumberofButtocks>0) and (Freeship.Visibility.ShowButtocks)) or
                                 ((Freeship.NumberofWaterlines>0) and (FreeShip.Visibility.ShowWaterlines));
   ExportDXFFaces.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   ExportIGES.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   ExportOffsets.Enabled:=FreeShip.NumberofStations+Freeship.NumberofButtocks+Freeship.NumberofWaterlines+
                          Freeship.NumberofDiagonals+Freeship.NumberOfControlCurves>0;
   ExportArchimedes.Enabled:=FreeShip.NumberofStations>0;
   ExportGHS.Enabled:=FreeShip.NumberofStations>0;
   ExportPAM.Enabled:=FreeShip.NumberofStations>0;   
// Detect existance of external modules
   FExecDirectory:=Freeship.Preferences.ExecDirectory;
   ExportMichlet.Enabled:=(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);
   ExportAddMass.Enabled:=(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);   
   HydrodynTask1_.Enabled:=False;
   HydrodynTask2_.Enabled:=False;
   HydrodynTask3_.Enabled:=False;
   HydrodynTask4_.Enabled:=False;
   PropTask1_.Enabled:=False;
   PropTask2_.Enabled:=False;
   PropTask3_.Enabled:=False;
   PropTask4_.Enabled:=False;
   PropTask5_.Enabled:=False;
   ResistanceHollen.Enabled:=False;
   ResistanceOST.Enabled:=False;
   ResistanceOortmer.Enabled:=False;
   ResistanceFungLeib.Enabled:=False;
   HydrodynManeuv_.Enabled:=False;
   PropTaskRvrs_.Enabled:=False;
   ExportAddMass.Enabled:=False;
   ResistanceRBHS.Enabled:=False;
   ResistanceMH.Enabled:=False;   
   // Detect existance of external modules and include executables into menu
   if FileExists(FExecDirectory+'/ADD_MASS.EXE') { *Converted from FileExists* } then ExportAddMass.Enabled:=True;
   if FileExists(FExecDirectory+'/Ishercof.EXE') { *Converted from FileExists* } then HydrodynTask1_.Enabled:=True;
   if FileExists(FExecDirectory+'/Pos_Ship.EXE') { *Converted from FileExists* } then HydrodynTask2_.Enabled:=True;
   if FileExists(FExecDirectory+'/Rot_Ship.EXE') { *Converted from FileExists* } then HydrodynTask3_.Enabled:=True;
   if FileExists(FExecDirectory+'/HOLLENBH.EXE') { *Converted from FileExists* } then ResistanceHollen.Enabled:=True;
   if FileExists(FExecDirectory+'/OORTMERS.EXE') { *Converted from FileExists* } then ResistanceOortmer.Enabled:=True;
   if FileExists(FExecDirectory+'/fungleib.EXE') { *Converted from FileExists* } then ResistanceFungLeib.Enabled:=True;
   if FileExists(FExecDirectory+'/hship.EXE') { *Converted from FileExists* }    then ResistanceOST.Enabled:=True;
   if FileExists(FExecDirectory+'/hship.EXE') { *Converted from FileExists* }    then ResistanceRBHS.Enabled:=True;
   if FileExists(FExecDirectory+'/hship.EXE') { *Converted from FileExists* }    then ResistanceMH.Enabled:=True;
   if FileExists(FExecDirectory+'/ManeuvPP.EXE') { *Converted from FileExists* } then HydrodynManeuv_.Enabled:=True;
   if FileExists(FExecDirectory+'/CALCPROP.EXE') { *Converted from FileExists* } then begin
      PropTask1_.Enabled:=True;
      PropTask2_.Enabled:=True;
      PropTask3_.Enabled:=True;
   end;
   if FileExists(FExecDirectory+'/PropPred.EXE') { *Converted from FileExists* } then PropTask4_.Enabled:=True;
   if FileExists(FExecDirectory+'/PROPOL.EXE') { *Converted from FileExists* }   then PropTask5_.Enabled:=True;
   if FileExists(FExecDirectory+'/RVRSSHIP.EXE') { *Converted from FileExists* } then PropTaskRvrs_.Enabled:=True;
   if ExportAddMass.Enabled then ExportAddMass.Enabled:=(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);   
//   if HydrodynTask2_.Enabled then HydrodynTask2_.Enabled:=(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);   
//   HydrodynTask3_.Enabled:=False; //(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);   
//   HydrodynTask4_.Enabled:=False; //(Freeship.Surface.NumberOfControlFaces>0) and (Freeship.ProjectSettings.MainparticularsHasBeenset);   
   RecentFiles.Enabled:=RecentFiles.Count>0;
   ExportCoordinates.Enabled:=Freeship.Surface.NumberOfControlPoints>0;
   ExportPart.Enabled:=(Freeship.Surface.NumberOfControlFaces>0);
   ImportPart.Enabled:=(Freeship.Surface.NumberOfControlFaces>0) and (MDIChildCount>0);
   // Show controledges and controlpoints
   ShowControlNet.Enabled:=FreeShip.Surface.NumberOfControlPoints>0;
   ShowControlNet.Checked:=FreeShip.Visibility.ShowControlNet;
   // Show interior edges
   ShowInteriorEdges.Enabled:=FreeShip.Surface.NumberOfControlFaces>0;
   ShowInteriorEdges.Checked:=FreeShip.Visibility.ShowInteriorEdges;
   // Show both sides
   BothSides.Checked:=Freeship.Visibility.ModelView=mvBoth;
   BothSides.Enabled:=FreeShip.Surface.NumberOfControlFaces>0;
   // Delete
   Delete.Enabled:=Freeship.NumberOfSelectedControlPoints+FreeShip.NumberOfSelectedControlEdges+
                   Freeship.NumberOfSelectedControlFaces+Freeship.NumberOfSelectedControlCurves+
                   Freeship.NumberOfSelectedFlowLines+Freeship.NumberOfselectedMarkers>0;
   // Window menu actions
   TileWindow.Enabled:=MDIChildCount>0;
   CascadeWindow.Enabled:=MDIChildCount>0;
   // Precision
   //PrecisionBox.ItemIndex:=Ord(FreeShip.Precision);
   // Layers
   LayerAutoGroup.Enabled:=(Freeship.Surface.NumberOfControlFaces>1) and (FreeShip.Visibility.ShowInteriorEdges);
   // Tools
   CheckModel.Enabled:=FreeShip.Surface.NumberOfControlFaces>0;
   DevelopLayers.Enabled:=False;
   for I:=1 to FreeShip.NumberOfLayers do
     if (FreeShip.Layer[I-1].Developable) and (FreeShip.Layer[I-1].Count>0)
     then
       begin
          DevelopLayers.Enabled:=True;
          break;
       end;
   KeelRudderWizard.Enabled:=MDIChildCount>0;
   DeleteMarkers.Enabled:=Freeship.NumberofMarkers>0;
   // Calculations
   DesignHydrostatics.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   Hydrostaticsdialog.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   CrossCurves.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   // edit commands
   AddPoint.Enabled:=(MDIChildCount>0) and (FreeShip.Visibility.ShowControlNet);
   Insertplane.Enabled:=(Freeship.Surface.NumberOfControlEdges>0) and (Freeship.Visibility.ShowControlNet);
   LayerIntersection.Enabled:=NLayers>1;
   EdgeCollapse.Enabled:=FreeShip.NumberOfSelectedControlEdges>0;
   NewEdge.Enabled:= (FreeShip.NumberOfSelectedControlPoints>1) and FreeShip.Surface.CanInsertEdge;
   EdgeCrease.Enabled:=FreeShip.NumberOfSelectedControlEdges>0;
   DeselectAll.Enabled:=(Freeship.NumberOfSelectedControlPoints+FreeShip.NumberOfSelectedControlEdges+Freeship.NumberOfSelectedControlFaces+Freeship.NumberOfSelectedControlCurves>0) or
                        (Freeship.ActiveControlPoint<>nil);
   NewCurve.Enabled:=FreeShip.NumberOfSelectedControlEdges>0;
   PointCollapse.Enabled:=Freeship.NumberOfSelectedControlPoints>0;
   DeleteEmptyLayers.Enabled:=False;
   for I:=1 to Freeship.NumberOfLayers do
     if (FreeShip.ModelLoaded) and (FreeShip.Layer[I-1].Count=0) and (FreeShip.NumberOfLayers>0) then
       begin
          DeleteEmptyLayers.Enabled:=True;
          break;
       end;
   RemoveUnusedPoints.Enabled:=False;
   for I:=1 to Freeship.Surface.NumberOfControlPoints do if Freeship.Surface.ControlPoint[I-1].NumberOfFaces=0 then
   begin
      RemoveUnusedPoints.Enabled:=True;
      break;
   end;
   InvertFace.Enabled:=Freeship.NumberOfSelectedControlFaces>0;
   ShowStations.Enabled:=Freeship.NumberofStations>0;
   ShowStations.Checked:=FreeShip.Visibility.ShowStations;
   ShowButtocks.Enabled:=Freeship.NumberofButtocks>0;
   ShowButtocks.Checked:=FreeShip.Visibility.ShowButtocks;
   ShowWaterlines.Enabled:=Freeship.NumberofWaterlines>0;
   ShowWaterlines.Checked:=FreeShip.Visibility.ShowWaterlines;
   ShowDiagonals.Enabled:=Freeship.NumberofDiagonals>0;
   ShowDiagonals.Checked:=FreeShip.Visibility.ShowDiagonals;
   ShowNormals.Checked:=FreeShip.Visibility.ShowNormals;
   ShowNormals.Enabled:=FreeShip.NumberOfSelectedControlFaces>0;
   ShowGrid.Checked:=Freeship.Visibility.ShowGrid;
   ShowGrid.Enabled:=Freeship.NumberofStations+Freeship.NumberofButtocks+Freeship.NumberofWaterlines+Freeship.NumberofDiagonals>0;
   ShowMarkers.Checked:=FreeShip.Visibility.ShowMarkers;
   ShowMarkers.Enabled:=Freeship.NumberofMarkers>0;
   ShowCurvature.Checked:=FreeShip.Visibility.ShowCurvature;
   ShowCurvature.Enabled:=Freeship.NumberofStations+Freeship.NumberofButtocks+Freeship.NumberofWaterlines+Freeship.NumberofDiagonals+Freeship.NumberOfControlCurves>0;
   ShowControlCurves.Checked:=FreeShip.Visibility.ShowControlCurves;
   ShowControlCurves.Enabled:=Freeship.Surface.NumberOfControlCurves>0;
   ShowHydrostatics.Checked:=Freeship.Visibility.ShowHydrostaticData;
   ShowHydrostatics.Enabled:=(Freeship.Surface.NumberOfControlFaces>2) and (Freeship.ProjectSettings.MainparticularsHasBeenset);
   ShowFlowlines.Checked:=Freeship.Visibility.ShowFlowlines;
   ShowFlowlines.Enabled:=Freeship.NumberOfFlowLines>0; 

   NewFace.Enabled:=FreeShip.NumberOfSelectedControlPoints>2;
   IntersectionDialog.Enabled:=FreeShip.Surface.NumberOfControlFaces>0;
   EdgeExtrude.Enabled:=FreeShip.NumberOfSelectedControlEdges>0;
   EdgeSplit.Enabled:=FreeShip.NumberOfSelectedControlEdges>0;
   RotateModel.Enabled:=FreeShip.Surface.NumberOfControlPoints>0;
   RotateModelM.Enabled:=FreeShip.Surface.NumberOfControlPoints>0;
   ScaleModel.Enabled:=FreeShip.Surface.NumberOfControlPoints>0;
   MoveModel.Enabled:=FreeShip.Surface.NumberOfControlPoints>0;
   Mirrorface.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
   // Skip translation
   if (Freeship.Undoposition-1>=0) and (Freeship.Undoposition-1<Freeship.UndoCount) then
   begin
      Undo.Caption:=Userstring(290)+#32+Freeship.UndoObject[Freeship.Undoposition-1].Undotext;
   end else
   begin
      Undo.Caption:=Userstring(290);
   end;
   Undo.Enabled:=(FreeShip.UndoCount>0) and (Freeship.UndoPosition>0);
   //if Undo.Enabled then Undo.Caption:='Undo '+Freeship.UndoObject[Freeship.Undoposition-1].Undotext
   //                else Undo.Caption:='Undo';
   Redo.Enabled:=(FreeShip.UndoCount>0) and (Freeship.UndoPosition<Freeship.UndoCount);
//   if Redo.Enabled then Redo.Caption:='Redo '+Freeship.UndoObject[Freeship.Undoposition].Undotext
//                   else Undo.Caption:='Redo';
   // End Skip translation
   Undohistory1.Enabled:=Freeship.UndoCount>0;
   ClearUndo.Enabled:=Freeship.UndoCount>0;
   PointsLock.Enabled:=(Freeship.NumberOfSelectedControlPoints>0) and (Freeship.NumberOfSelectedLockedPoints<Freeship.NumberOfSelectedControlPoints);
   PointsUnlock.Enabled:=Freeship.NumberOfSelectedLockedPoints>0;
   PointsUnlockAll.Enabled:=Freeship.NumberOfLockedPoints>0;
   PointAlign.Enabled:=Freeship.NumberOfSelectedControlPoints>2;
   TransformLackenby.Enabled:=Freeship.Surface.NumberOfControlFaces>0;
end;{TMainForm.UpdateMenu}

procedure TMainForm.ShowRecentFilesDialog;
var dlg: TTileDialog; i:integer; vFileName,sTime:string; jpg:TJPEGImage;
    pic:TPicture;
begin
  dlg := TTileDialog.create(Self);
  MenuImages.GetBitmap(ToolButtonOpenFile.ImageIndex, dlg.ButtonOpenFile.Glyph);

  for i:=0 to FreeShip.Edit.RecentFileCount-1 do
    begin
      vFileName:=Freeship.Edit.RecentFile[i];
      if not FileExists(vFileName) then continue;
      sTime:=FormatDateTime('YYYY-MM-DD hh:mm:ss',FileDateToDateTime(FileAgeUTF8(vFileName)));
      jpg:= Freeship.Edit.getPreviewImage(vFileName);
      if assigned(jpg) then
        begin
        pic := TPicture.Create;
        pic.Bitmap.Assign(jpg);
        dlg.AddTile(pic, sTime+' - '+vFileName, vFileName);
        end;
    end;

  dlg.ShowModal;
  vFileName:=dlg.FileName;
  dlg.Free;
  if (vFileName<>'') and (vFileName<>'*') then
    FreeShip.Edit.File_Load(vFileName);
  if (vFileName = '*') then
    begin
    FreeShip.Edit.File_Load;
    FOpenHullWindows;
    SetCaption;
    UpdateMenu;
    end;
end;

procedure TMainForm.LoadFileExecute(Sender: TObject);
begin
   ShowRecentFilesDialog;
   //FreeShip.Edit.File_Load;
   FOpenHullWindows;
   SetCaption;
   UpdateMenu;
end;{TMainForm.LoadFileExecute}

procedure TMainForm.ExitProgramExecute(Sender: TObject);
begin
  UpdateMenu;
  if FileExists('Resist.dat')  then DeleteFile('Resist.dat');
  if FileExists('RESISTp.dat')  then DeleteFile('RESISTp.dat');
  if FileExists('Vint1.dat')  then DeleteFile('Vint1.dat');
  if FileExists('SAC.tmp')  then DeleteFile('SAC.tmp');
  if FileExists('SACs.txt')  then DeleteFile('SACs.txt');
  if FileExists('Bonjean.txt')  then DeleteFile('Bonjean.txt');
  if FileExists('Weights.txt')  then DeleteFile('Weights.txt');
  if FileExists('Sterns.txt')  then DeleteFile('Sterns.txt');
  Close;
end;{TMainForm.ExitProgramExecute}

procedure TMainForm.FormShow(Sender: TObject);
var FileExt: string;
begin
   self.WindowState:=wsNormal;
   self.Position:=poScreenCenter;

   // Initialize some data
   FreeShip.OnChangeActiveLayer:=FreeShipChangeActiveLayer;
   Freeship.OnChangeLayerData:=FreeShipChangeLayerData;
   FreeShip.OnSelectItem:=FOnSelectItem;
   FreeShip.Clear;

   SetCaption;
   LoadToolIcons;
   UpdateMenu;
end;{TMainForm.FormShow}

procedure TMainForm.PanelMainResize(Sender: TObject);
begin
  if PanelMain.IsResizing then exit;
  ArrangeRibbonPanel(PanelMain);
end;


procedure TMainForm.ShowControlNetExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowControlNet:=not FreeShip.Visibility.ShowControlNet;
   UpdateMenu;
end;{TMainForm.ShowControlNetExecute}

procedure TMainForm.ShowInteriorEdgesExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowInteriorEdges:=not FreeShip.Visibility.ShowInteriorEdges;
   UpdateMenu;
end;{TMainForm.ShowInteriorEdgesExecute}

procedure TMainForm.NewWindowExecute(Sender: TObject);
var HullformWindow : TFreeHullWindow;
begin
   // open a new window
   HullformWindow:=TFreeHullWindow.Create(self);
   FMDIChildList.Add(HullformWindow);
   // Connect viewport to freeship component
   HullformWindow.FreeShip:=FreeShip;
   HullformWindow.Viewport.ViewType:=fvPerspective;
   ShowTranslatedValues(HullformWindow);
   HullformWindow.SetCaption;
   UpdateMenu;
end;{TMainForm.NewWindowExecute}

procedure TMainForm.SpinEditFontSizeChange(Sender: TObject);
var w: integer;  vp:TFreeViewport;
begin
  FreeShip.FontSize := SpinEditFontSize.value;
  for w:=0 to FMDIChildList.Count-1 do
  begin
    vp := TFreeHullWindow(FMDIChildList.Items[w]).Viewport;
    vp.invalidate;
  end;
end;

{$IFDEF FPC}
function TMainForm.GetMDIChildren(AIndex: Integer): TCustomForm;
begin
  Result := nil;
  if not (FormStyle in [fsMDIForm, fsMDIChild]) then
    exit;
  if AIndex > FMDIChildList.Count -1 then exit;
  Result := TCustomForm(FMDIChildList.Items[AIndex]) ;
end;

procedure TMainForm.AbandonMDIChildren(AIndex: Integer);
begin
  FMDIChildList.Delete(AIndex);
end;
{$ENDIF}

procedure TMainForm.Tile;
var T,L,MW,MH, X,Y,W,H, i, MCC, HH,FW : Integer;
    HFW : TCustomForm; //TFreeHullWindow;
begin
 {$ifdef Windows}
  inherited Tile;
 {$endif}

 {$ifdef LCLQt}
  inherited Tile;
 {$else}
 // do manual tile for non MDI environments

 //HH := GetSystemMetrics(SM_CYCAPTION); //header height
 //FW := GetSystemMetrics(SM_CYDLGFRAME); //frame width

// Color:=clRed;

 HH:=24;
 FW:=2;
 MW := Self.Width + FW*2;
 MH := Self.Height + HH + FW*2;
 T := Self.Top;
 L := Self.Left;
 T := HH;
 L := FW;
 MCC := MdiChildCount;
 for i:= 0 to MCC - 1 do
   begin
   HFW := MDIChildren[i];
   if Assigned(HFW) then
     begin
     if i = 0 then HFW.SetBounds(L,                T+MH,
                                 MW div 2 -FW*2,   HFW.Height -FW*2);
     if i = 1 then HFW.SetBounds(L+MW div 2 +FW*2, T+MH,
                                 MW div 2 -FW*2,   HFW.Height -FW*2);
     if i = 2 then HFW.SetBounds(L,                T+MH+HFW.Height+HH+FW*2,
                                 MW div 2 -FW*2,   HFW.Height -FW*2);
     if i = 3 then HFW.SetBounds(L+MW div 2 +FW*2, T+MH+HFW.Height+HH+FW*2,
                                 MW div 2 -FW*2,   HFW.Height -FW*2);
     end;
   end;
 {$endif}
end;{TMainForm.Tile}

procedure TMainForm.Cascade;
var T,L,MW,MH, X,Y,W,H, i : Integer;
    HFW : TCustomForm; //TFreeHullWindow;
begin
  {$ifdef Windows}
   inherited Cascade;
  {$endif}

  {$ifdef LCLQt}
   inherited Cascade;
  {$else}
  // do manual cascade for non MDI environments
  MW := Self.Width;
  MH := Self.Height + 30;
  T := Self.Top;
  L := Self.Left;
  W := MW-40*MdiChildCount;
  if W<200 then W:=200;
  for i:= 0 to MdiChildCount - 1 do
   begin
   HFW := MDIChildren[i];
   HFW.SetBounds(L+i*40,T+MH+i*40,W,HFW.Height);
   end;
  {$endif}
end;{TMainForm.Cascade}

procedure TMainForm.TileWindowExecute(Sender: TObject);
begin
  {$ifndef LCL}{$ifndef CLX}
  TileMode := tbHorizontal;
  {$endif}{$endif}
  Tile;
end;{TMainForm.TileWindowExecute}

procedure TMainForm.CascadeWindowExecute(Sender: TObject);
begin
  {$ifndef LCL}{$ifndef CLX}
  TileMode := tbHorizontal;
  {$endif}{$endif}
  Cascade;
end;{TMainForm.CascadeWindowExecute}

procedure TMainForm.BothSidesExecute(Sender: TObject);
begin
   if FreeShip.Visibility.ModelView=mvBoth then FreeShip.Visibility.ModelView:=mvPort
                                           else FreeShip.Visibility.ModelView:=mvBoth;
   UpdateMenu;
end;{TMainForm.BothSidesExecute}

procedure TMainForm.FreeShipFileChanged(Sender: TObject);
begin
   SetCaption;
end;{TMainForm.FreeShipFileChanged}

{ //moved to TFREEProjectSettingsDialog
procedure TMainForm.PrecisionBoxChange(Sender: TObject);
begin
   FreeShip.Precision:=TFreePrecisionType(PrecisionBox.ItemIndex);
   UpdateMenu;
end;}{TMainForm.PrecisionBoxChange}


procedure TMainForm.FileSaveasExecute(Sender: TObject);
begin
   FreeShip.Edit.File_SaveAs;
   UpdateMenu;
   SetCaption;
end;{TMainForm.SaveFileExecute}

procedure TMainForm.LayerAutoGroupExecute(Sender: TObject);
begin
   FreeShip.Edit.Layer_AutoGroup;
   UpdateMenu;
end;{TMainForm.LayerAutoGroupExecute}

procedure TMainForm.FLoadRecentFile(sender:TObject);
var Menu    : TMenuItem;
    Filename: string;
    N       : Integer;
    Answer  : word;
begin
   if sender is TMenuItem then
   begin
      Menu:=sender as TMenuItem;
      // Skip translation
      Filename:=Menu.Caption;
      repeat
         N:=Pos('&',Filename);
         if N<>0 then system.Delete(Filename,N,1);
      until N=0;

   if Freeship.FileChanged then
   begin
      Answer:=MessageDlg(Userstring(103)+EOL+Userstring(104),mtConfirmation,[mbYes,mbNo,mbCancel],0);
      if Answer=mrCancel then exit;
      if Answer=mrYes then
      begin
         Freeship.Edit.File_SaveAs;
         if Freeship.FileChanged then exit; // Apparently saving was not successfull, abort
      end;
   end;

   // End Skip translation
   if FileExists(Filename)  then
      begin
         Freeship.Edit.File_Load(Filename);
         FOpenHullWindows;
         SetCaption;
         UpdateMenu;
      end
   else
      begin
         //MessageDlg(Userstring(106)+' '+FFileName,mtError,[mbOk],0);
         FreeEmptyModelChooserDialog:=TFreeEmptyModelChooserDialog.Create(Self);
         ShowTranslatedValues(FreeEmptyModelChooserDialog);
         if FreeEmptyModelChooserDialog.Execute(FileName)
         then
           begin
           if FreeEmptyModelChooserDialog.RbCreateNew.Checked
             then NewModelExecute(Self)
           else
           if FreeEmptyModelChooserDialog.RbLoadFile.Checked
             then LoadFileExecute(Self)
           end;
         FreeEmptyModelChooserDialog.Free;
      end
   end;
end;{TMainForm.FLoadRecentFile}

procedure TMainForm.LoadMostRecentFile;
var Menu    : TMenuItem;
    Filename: string;
    N       : Integer;
    Answer  : word;
begin
  if FreeShip.Edit.RecentFileCount = 0 then exit;
  Filename := Freeship.Edit.RecentFile[0];
  FFilename := Filename;
  if FileExists(Filename)  then
    begin
    Freeship.Edit.File_Load(Filename);
    FOpenHullWindows;
    SetCaption;
    UpdateMenu;
    end;
end;{TMainForm.LoadMostRecentFile}

procedure TMainForm.LoadNamedFile(FileName:string);
var Menu    : TMenuItem;
    N       : Integer;
    Answer  : word;
begin
  if FileExists(Filename)  then
    begin
    Freeship.Edit.File_Load(Filename);
    FOpenHullWindows;
    SetCaption;
    UpdateMenu;
    end
  else
    begin
       //MessageDlg(Userstring(106)+' '+FFileName,mtError,[mbOk],0);
       FreeEmptyModelChooserDialog:=TFreeEmptyModelChooserDialog.Create(Self);
       if FreeEmptyModelChooserDialog.Execute(FileName)
       then
         begin
         if FreeEmptyModelChooserDialog.RbCreateNew.Checked
           then NewModelExecute(Self)
         else
         if FreeEmptyModelChooserDialog.RbLoadFile.Checked
           then LoadFileExecute(Self)
         end;
       FreeEmptyModelChooserDialog.Free;
    end

end;{TMainForm.LoadMostRecentFile}

procedure TMainForm.FreeShipChangeLayerData(Sender: TObject);
var I : Integer;
begin
   // Fill the layerbox with the current layers
   LayerBox.Items.BeginUpdate;
   LayerBox.Items.Clear;
   try
      for I:=1 to Freeship.NumberOfLayers do
      begin
         Layerbox.Items.AddObject(FreeShip.Layer[I-1].Name,FreeShip.Layer[I-1]);
      end;
   finally
      LayerBox.Items.EndUpdate;
      I:=LayerBox.Items.IndexOfObject(FreeShip.ActiveLayer);
      Layerbox.ItemIndex:=I;
   end;
end;{TMainForm.FreeShipChangeLayerData}

procedure TMainForm.FreeShipChangeActiveLayer(Sender: TObject;Layer: TFreeSubdivisionLayer);
var Index : Integer;
begin
   if (FreeShip.NumberOfSelectedControlFaces<>0) and (FreeShip.ActiveLayer=Layer) then
   begin
      // do not switch to the active layer when controlfaces are selected
   end else
   begin
      if Layer=nil then
      begin
         Index:=-1;
         Layerbox.ItemIndex:=Index;
         PanelActiveLayerColor.Color:=clBtnface;
      end else
      begin
         Index:=Layerbox.Items.IndexOfObject(Layer);
         Layerbox.ItemIndex:=Index;
         PanelActiveLayerColor.Color:=Layer.Color;
      end;
   end;
end;{TMainForm.FreeShipChangeActiveLayer}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FreeShip.Preferences.Save;
   FreeShip.OnChangeActiveLayer:=nil;
   Freeship.OnChangeLayerData:=nil;
   FreeShip.OnSelectItem:=nil;
end;{TMainForm.FormClose}

procedure TMainForm.NewLayerExecute(Sender: TObject);
begin
   FreeShip.Edit.Layer_New;
   UpdateMenu;
end;{TMainForm.NewLayerExecute}

procedure TMainForm.DeleteExecute(Sender: TObject);
begin
   Freeship.Edit.Selection_Delete;
   UpdateMenu;
end;{TMainForm.DeleteExecute}

procedure TMainForm.EdgeCollapseExecute(Sender: TObject);
begin
   FreeShip.Edit.Edge_Collapse;
   UpdateMenu;
end;{TMainForm.EdgeCollapseExecute}

procedure TMainForm.NewEdgeExecute(Sender: TObject);
begin
   FreeShip.Edit.Edge_Connect;
   UpdateMenu;
end;{TMainForm.NewEdgeExecute}

procedure TMainForm.ImportFEFExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ImportFEF;
   FOpenHullWindows;
   SetCaption;
   UpdateMenu;
end;{TMainForm.ImportFEFExecute}

procedure TMainForm.EdgeCreaseExecute(Sender: TObject);
begin
    FreeShip.Edit.Edge_Crease;
    UpdateMenu;
end;{TMainForm.EdgeCreaseExecute}

procedure TMainForm.DeselectAllExecute(Sender: TObject);
begin
   FreeShip.Edit.Selection_Clear;
   UpdateMenu;
end;{TMainForm.ClearSelectionExecute}

procedure TMainForm.PointCollapseExecute(Sender: TObject);
begin
   Freeship.Edit.Point_Collapse;
   UpdateMenu;
end;{TMainForm.PointCollapseExecute}

procedure TMainForm.LayerBoxChange(Sender: TObject);
var Layer   : TFreeSubdivisionLayer;
    I,Index : Integer;
begin
   Index:=Layerbox.ItemIndex;
   if index=-1 then Index:=0;
   Layer:=Layerbox.Items.Objects[index] as TFreeSubdivisionLayer;
   if Freeship.NumberOfSelectedControlFaces=0 then
   begin
      // change the active layer
      if Layer<>FreeShip.ActiveLayer then FreeShip.ActiveLayer:=Layer;
   end else
   begin
      // Assign all selected controlfaces to the new layer
      for I:=FreeShip.NumberOfSelectedControlFaces downto 1 do
         FreeShip.SelectedControlFace[I-1].Layer:=Layer;
      FreeShip.FileChanged:=True;
      FreeShip.Redraw;
   end;
   UpdateMenu;
end;{TMainForm.LayerBoxChange}

procedure TMainForm.PanelActiveLayerColorClick(Sender: TObject);
begin
   ActiveLayerColorExecute(self);
end;{TMainForm.Panel1Click}

procedure TMainForm.ActiveLayerColorExecute(Sender: TObject);
begin
   // change the color of the currently active layer
   ColorDialog.Color:=FreeShip.ActiveLayer.Color;
   if ColorDialog.Execute then
   begin
      FreeShip.ActiveLayer.Color:=ColorDialog.Color;
      FreeShip.FileChanged:=True;
      FreeShip.Redraw;
      FreeShipChangeActiveLayer(self,Freeship.ActiveLayer);
      UpdateMenu;
   end;
end;{TMainForm.ActiveLayerColorExecute}

procedure TMainForm.DeleteEmptyLayersExecute(Sender: TObject);
begin
   Freeship.Edit.Layer_DeleteEmpty(False);
   UpdateMenu;
end;{TMainForm.DeleteEmptyLayersExecute}

procedure TMainForm.LayerDialogExecute(Sender: TObject);
begin
   FreeShip.Edit.Layer_Dialog;
   UpdateMenu;
end;{TMainForm.LayerDialogExecute}

procedure TMainForm.NewModelExecute(Sender: TObject);
begin
   if FreeShip.Edit.Model_New then FOpenHullWindows;
   Updatemenu;
end;{TMainForm.NewModelExecute}

procedure TMainForm.FormCreate(Sender: TObject);
begin
   {$IFDEF MEMCHECK}
      // Initialize memcheck, for memory-leak tracking
      MemChk;
   {$ENDIF}
   {$ifndef Windows}
   if self.Align = alTop
     then self.Top := 0;
   {$endif}

   // Removed from LFM, moved here
{ object FreeShip: TFreeShip
    FileChanged = True
    Filename = 'New model.fbm'
    FileVersion = fv140
    OnChangeCursorIncrement = FreeShipChangeCursorIncrement
    OnFileChanged = FreeShipFileChanged
    OnUpdateGeometryInfo = FreeShipUpdateGeometryInfo
    OnUpdateRecentFileList = FreeShipUpdateRecentFileList
    OnUpdateUndoData = FreeShipUpdateUndoData
    Precision = fpLow
    FontSize = 0
    left = 32
    top = 72
  end
  }
   FreeShip := TFreeShip.Create(self) ;
   FreeShip.FileChanged := true;
   FreeShip.Filename := 'New model.fbm';
   FreeShip.FileVersion := fv140;
   FreeShip.OnChangeCursorIncrement := FreeShipChangeCursorIncrement;
   FreeShip.OnFileChanged := FreeShipFileChanged;
   FreeShip.OnUpdateGeometryInfo := FreeShipUpdateGeometryInfo;
   FreeShip.OnUpdateRecentFileList := FreeShipUpdateRecentFileList;
   FreeShip.OnUpdateUndoData := FreeShipUpdateUndoData;
   FreeShip.Precision := fpLow;
   FreeShip.FontSize := 0;

   FAllToolbarsControlsWidth := 0;
   GlobalFreeship := Freeship;
   FModelInitallyLoaded := false;
   //dumpIcons;


end;{TMainForm.FormCreate}

procedure TMainForm.ShowStationsExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowStations:=not FreeShip.Visibility.ShowStations;
   UpdateMenu;
end;{TMainForm.ShowStationsExecute}

procedure TMainForm.ShowButtocksExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowButtocks:=not FreeShip.Visibility.ShowButtocks;
   UpdateMenu;
end;{TMainForm.ShowButtocksExecute}

procedure TMainForm.ShowWaterlinesExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowWaterlines:=not FreeShip.Visibility.ShowWaterlines;
   UpdateMenu;
end;{TMainForm.ShowWaterlinesExecute}

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Answer:word;
begin
   if Freeship.FileChanged then
   begin
      Answer:=MessageDlg(Userstring(103)+EOL+Userstring(282)+'?',mtWarning,[mbYes,mbNo],0);
      CanClose:=Answer=mrYes;
   end;
end;{TMainForm.FormCloseQuery}

procedure TMainForm.NewFaceExecute(Sender: TObject);
begin
   FreeShip.Edit.Face_New;
   UpdateMenu;
end;{TMainForm.NewFaceExecute}

procedure TMainForm.IntersectionDialogExecute(Sender: TObject);
begin
   FreeShip.Edit.Intersection_Dialog;
   UpdateMenu;
end;{TMainForm.IntersectionDialogExecute}

procedure TMainForm.EdgeExtrudeExecute(Sender: TObject);
begin
   FreeShip.Edit.Edge_Extrude;
   UpdateMenu;
end;{TMainForm.EdgeExtrudeExecute}

procedure TMainForm.About1Click(Sender: TObject);
var FreeAboutDlg : TFreeAboutDlg;
begin
   // Show splash screen again
   {FreeSplashWindow:=TFreeSplashWindow.Create(Application);
   ShowTranslatedValues(FreeSplashWindow);
   FreeSplashWindow.Show;
   FreeSplashWindow.Refresh;}

 FreeAboutDlg := TFreeAboutDlg.Create(Self);
 FreeAboutDlg.ShowModal;
 FreeAboutDlg.Destroy;
end;{TMainForm.About1Click}

// begin correction Victor T
procedure TMainForm.Help1Click(Sender: TObject);
var pathFile,FileToFind : string;
    PathFileOld        : string;
    //FInitDirectory     : string;
    FManDirectory     : string;
    FLang : string;
    man : string;
//    FFreeship          : TFreeship;
    command            : PAnsiChar;
    I,II               : integer;
    L                  : boolean;
    //label NewCatalogSearch;
begin
{
  ii:=1;
  if Userstring(279)='Версия'  then II:=10;
  if Userstring(279)='Версія'  then II:=20;
  if Userstring(279)='Versie'  then II:=30;
  if Userstring(279)='Versjon' then II:=40;
  if Userstring(279)='Versio'  then II:=50;
  if Userstring(279)='Phien ban' then II:=70;
  if Userstring(279)='Versiуn' then II:=62;
  if Userstring(279)='Version' then begin
    if Userstring(273)='Speed'     then II:=60;
    if Userstring(273)='Vitesse'   then II:=61;
    if Userstring(273)='Velocidad' then II:=62;
    if Userstring(273)='Geschwindigkeit' then II:=63;
  end;
  FInitDirectory:=Freeship.Preferences.ManualsDirectory;
  PathFile:=GetCurrentDirUTF8;
  PathFileOld:=GetCurrentDirUTF8;
  L:=SetCurrentDirUTF8(FInitDirectory);
  Case II of
    10 : command:='Manuals/ManualRU.pdf';
    20 : command:='Manuals/ManualUA.pdf';
    30 : command:='Manuals/ManualHO.pdf';
    40 : command:='Manuals/ManualNO.pdf';
    50 : command:='Manuals/ManualFI.pdf';
    60 : command:='Manuals/Manual.pdf';
    61 : command:='Manuals/ManualFR.pdf';
    62 : command:='Manuals/ManualCA.pdf';
    63 : command:='Manuals/ManualGE.pdf';
    70 : command:='Manuals/ManualVN.pdf';
    else command:='Manuals/Manual.pdf';
  end;
  FileToFind := FileSearchUTF8(command,FInitDirectory);
  }
  FLang := Freeship.Preferences.Language;
  FManDirectory := Freeship.Preferences.ManualsDirectory;
  man := FLang+'.pdf';
  FileToFind := FileSearch(FManDirectory+'/'+man,FManDirectory);
  if (FileToFind='') and (FLang<>'English') then begin
     MessageDlg('Manual file "'+man+'" not found in "'+FManDirectory+'" directory'+EOL
               +'English manual will be opened.',mtInformation,[mbOk],0);
     man := 'English.pdf'
  end;
  FileToFind := FileSearch(FManDirectory+'/'+man,FManDirectory);
  if FileToFind='' then begin
     MessageDlg('Manual file "'+man+'" not found in "'+FManDirectory+'" directory',mtInformation,[mbOk],0);
     exit;
  end;

  OpenDocument(FileToFind); { *Converted from ShellExecute* }

  //L:=SetCurrentDirUTF8(PathFileOld); { *Converted from SetCurrentDir* }
  //exit;
end;{TMainForm.Help1Click}
// end correction Victor T

procedure TMainForm.EdgeSplitExecute(Sender: TObject);
begin
   FreeShip.Edit.Edge_Split;
   UpdateMenu;
end;{TMainForm.EdgeSplitExecute}

procedure TMainForm.ExportFEFExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportFEF;
   UpdateMenu;
end;{TMainForm.ExportFEFExecute}

procedure TMainForm.EditProjectSettingsExecute(Sender: TObject);
begin
   FreeShip.ProjectSettings.Edit;
   UpdateMenu;
end;{TMainForm.EditProjectSettingsExecute}

procedure TMainForm.CheckModelExecute(Sender: TObject);
begin
   FreeShip.Edit.Model_Check(True);
   UpdateMenu;
end;{TMainForm.AnalyzeSurfaceExecute}

procedure TMainForm.ShowNormalsExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowNormals:=not FreeShip.Visibility.ShowNormals;
   UpdateMenu;
end;{TMainForm.ShowNormalsExecute}

procedure TMainForm.DesignHydrostaticsExecute(Sender: TObject);
var Calculation : TFreeHydrostaticCalc;
begin
   Calculation:=Freeship.Edit.Hydrostatics_Calculate(Freeship.ProjectSettings.ProjectDraft,0.0,0.0);
   if Calculation<>nil then
   begin
      Calculation.Destroy;
   end;
end;{TMainForm.DesignHydrostaticsExecute}

procedure TMainForm.ImportVRMLExecute(Sender: TObject);
var DateTime : TDateTime;
    str_1,str_2 : string;
begin
      DateTime := Time;  // store the current date and time
      str_1 := TimeToStr(DateTime); // convert the time into a string
   FreeShip.Edit.File_ImportVRML;
   FOpenHullWindows;
      DateTime := Time;  // store the current date and time
      str_2 := TimeToStr(DateTime); // convert the time into a string
      MessageDlg(('Time import='+str_2+' - '+str_1),mtInformation,[mbOK],0);
   UpdateMenu;
end;{TMainForm.ImportVRMLExecute}

procedure TMainForm.RemoveNegativeExecute(Sender: TObject);
begin
   Freeship.Edit.Face_DeleteNegative;
   UpdateMenu;
end;{TMainForm.RemoveNegativeExecute}

procedure TMainForm.RotateModelExecute(Sender: TObject);
begin
   FreeShip.Edit.Face_Rotate;
   UpdateMenu;
end;{TMainForm.RotateModelExecute}

procedure TMainForm.RotateModelMExecute(Sender: TObject);
begin
   FreeShip.Edit.Face_RotateM;
   UpdateMenu;
end;{TMainForm.RotateModelMExecute}

procedure TMainForm.ScaleModelExecute(Sender: TObject);
begin
   FreeShip.Edit.Face_Scale;
   UpdateMenu;
end;{TMainForm.ScaleModelExecute}

procedure TMainForm.ShowGridExecute(Sender: TObject);
begin
   Freeship.Visibility.ShowGrid:=not Freeship.Visibility.ShowGrid;
   UpdateMenu;
end;{TMainForm.ShowGridExecute}

procedure TMainForm.ToolButtonSelectClick(Sender: TObject);
begin
end;

procedure TMainForm.UndoExecute(Sender: TObject);
begin
   FreeShip.Edit.Undo;
   UpdateMenu;
   SetCaption;
end;{TMainForm.UndoExecute}

   // Update undo memory usage
procedure TMainForm.FreeShipUpdateUndoData(Sender: TObject);
var Memory : Integer;
begin
   Memory:=Trunc(Freeship.UndoMemory/1024);
   if Memory<1024 then Panel2.Caption:=Userstring(283)+' : '+IntToStr(Memory)+' Kb.'
                  else Panel2.Caption:=Userstring(283)+' : '+Truncate(Memory/1024,3)+' Mb.';
   Undo.Enabled:=FreeShip.UndoCount>0;
   SetCaption;
   UpdateMenu;
end;{TMainForm.FreeShipUpdateUndoData}

procedure TMainForm.HydrostaticsDialogExecute(Sender: TObject);
begin
   Freeship.Edit.Hydrostatics_Dialog;
   UpdateMenu;
end;{TMainForm.HydrostaticsDialogExecute}

procedure TMainForm.ExportObjExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportObj;
   UpdateMenu;
end;{TMainForm.ExportObjExecute}

procedure TMainForm.InvertFaceExecute(Sender: TObject);
begin
   Freeship.Edit.Face_Flip;
   UpdateMenu;
end;{TMainForm.InvertFaceExecute}

procedure TMainForm.PreferencesExecute(Sender: TObject);
begin
   FreeShip.Preferences.Edit;
   UpdateMenu;
   LoadToolIcons;
end;{TMainForm.PreferencesExecute}

procedure TMainForm.ImportBodyplanExecute(Sender: TObject);
begin
   FreeShip.Edit.ImportFrames;
   FOpenHullWindows;
   SetCaption;
   UpdateMenu;
end;{TMainForm.ImportBodyplanExecute}

procedure TMainForm.ExportDXF3DPolylinesExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportDXF_3DPolylines;
   UpdateMenu;
end;{TMainForm.ExportDXFPolylinesExecute}

procedure TMainForm.ExportDXFFacesExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportDXF_Faces;
   UpdateMenu;
end;{TMainForm.ExportDXFFacesExecute}

procedure TMainForm.ImportHullFileExecute(Sender: TObject);
begin
   Freeship.Edit.File_ImportHull;
   FOpenHullWindows;
   UpdateMenu;
end;{TMainForm.ImportHullFileExecute}

procedure TMainForm.ExportOffsetsExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportOffsets;
   UpdateMenu;
end;{TMainForm.ExportOffsetsExecute}

procedure TMainForm.MoveModelExecute(Sender: TObject);
begin
   FreeShip.Edit.Face_Move;
   UpdateMenu;
end;{TMainForm.MoveModelExecute}

procedure TMainForm.AddPointExecute(Sender: TObject);
begin
   Freeship.Edit.Point_New;
   UpdateMenu;
end;{TMainForm.AddPointExecute}

procedure TMainForm.DevelopLayersExecute(Sender: TObject);
begin
   FreeShip.Edit.Layer_Develop;
   UpdateMenu;
end;{TMainForm.DevelopLayersExecute}

procedure TMainForm.ExportArchimedesExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportArchimedes;
   UpdateMenu;
end;{TMainForm.ExportArchimedesExecute}

procedure TMainForm.ShowLinesplanExecute(Sender: TObject);
var I          : Integer;
    AlreadyOpen: Boolean;
    Form       : TFreeLinesplanForm;
begin
   if not Freeship.ProjectSettings.MainparticularsHasBeenset then
   begin
      MessageDlg(Userstring(96),mtWarning,[mbOk],0);
      exit;
   end;
   AlreadyOpen:=False;
   for I:=1 to MDIChildCount do if MDIChildren[I-1] is TFreeLinesplanForm then
   begin
      AlreadyOpen:=True;
      MDIChildren[I-1].BringToFront;
      break;
   end;
   if not AlreadyOpen then
   begin
      Form:=TFreeLinesplanForm.Create(self);
      ShowTranslatedValues(Form.LinesplanFrame);
      ShowTranslatedValues(Form);
      Form.LinesplanFrame.FreeShip:=FreeShip;
      Form.LinesplanFrame.Viewport.ZoomExtents;
   end;
end;{TMainForm.ShowLinesplanExecute}

procedure TMainForm.ShowDiagonalsExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowDiagonals:=not FreeShip.Visibility.ShowDiagonals;
   UpdateMenu;
end;{TMainForm.ShowDiagonalsExecute}

procedure TMainForm.FreeShipUpdateRecentFileList(Sender: TObject);
var I    : Integer;
    Item : TMenuItem;
begin
   // delete old menu items
   RecentFiles.Clear;
   // add the new data
   for I:=1 to FreeShip.Edit.RecentFileCount do
   begin
      Item:=TMenuItem.Create(self);
      Item.Caption:=Freeship.Edit.RecentFile[I-1];
      Item.OnClick:=FLoadRecentFile;
      RecentFiles.Add(Item);
   end;
   UpdateMenu;
end;{TMainForm.FreeShipUpdateRecentFileList}

procedure TMainForm.ImportCareneExecute(Sender: TObject);
begin
   Freeship.Edit.File_ImportCarene;
   FOpenHullWindows;
   UpdateMenu;
end;{TMainForm.ImportCareneExecute}

procedure TMainForm.ShowMarkersExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowMarkers:=not FreeShip.Visibility.ShowMarkers;
   UpdateMenu;
end;{TMainForm.ShowMarkersExecute}

procedure TMainForm.DeleteMarkersExecute(Sender: TObject);
begin
   Freeship.Edit.Marker_Delete;
   UpdateMenu;
end;{TMainForm.DeleteMarkersExecute}

procedure TMainForm.ImportSurfaceExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ImportSurface;
   FOpenHullWindows;
   SetCaption;
   UpdateMenu;
end;{TMainForm.ImportSurfaceExecute}

procedure TMainForm.ShowcurvatureExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowCurvature:=not FreeShip.Visibility.ShowCurvature;
   UpdateMenu;
end;{TMainForm.ShowcurvatureExecute}

procedure TMainForm.IncreaseCurvatureScaleExecute(Sender: TObject);
begin
   Freeship.Visibility.IncreaseCurvatureScale;
end;{TMainForm.IncreaseCurvatureScaleExecute}

procedure TMainForm.DecreaseCurvatureScaleExecute(Sender: TObject);
begin
   Freeship.Visibility.DecreaseCurvatureScale;
end;{TMainForm.DecreaseCurvatureScaleExecute}

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
   FreeShip.Edit.File_Save;
   UpdateMenu;
end;{TMainForm.FileSaveExecute}

procedure TMainForm.ImportChinesExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ImportChines;
   FOpenHullWindows;
   SetCaption;
   UpdateMenu;
end;{TMainForm.ImportChinesExecute}

procedure TMainForm.ShowControlCurvesExecute(Sender: TObject);
begin
   FreeShip.Visibility.ShowControlCurves:=not FreeShip.Visibility.ShowControlCurves;
   UpdateMenu;
end;{TMainForm.ShowControlCurvesExecute}

procedure TMainForm.NewCurveExecute(Sender: TObject);
begin
   Freeship.Edit.Curve_Add;
   UpdateMenu;
end;{TMainForm.AddCurveExecute}

procedure TMainForm.ExportCoordinatesExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportCoordinates;
   UpdateMenu;
end;{TMainForm.ExportCoordinatesExecute}

procedure TMainForm.InsertPlaneExecute(Sender: TObject);
begin
   Freeship.Edit.Point_InsertPlane;
   UpdateMenu;
end;{TMainForm.InsertPlaneExecute}

procedure TMainForm.PointsLockExecute(Sender: TObject);
begin
   Freeship.Edit.Point_Lock;
   UpdateMenu;
end;{TMainForm.PointsLockExecute}

procedure TMainForm.PointsUnlockExecute(Sender: TObject);
begin
   Freeship.Edit.Point_Unlock;
   UpdateMenu;
end;{TMainForm.UnlockPointsExecute}

procedure TMainForm.PointsUnlockAllExecute(Sender: TObject);
begin
   Freeship.Edit.Point_UnlockAll;
   UpdateMenu;
end;{TMainForm.PointsUnlockAllExecute}

procedure TMainForm.ImportMarkersExecute(Sender: TObject);
begin
   Freeship.Edit.Marker_Import;
   UpdateMenu;
end;{TMainForm.ImportMarkersExecute}

procedure TMainForm.ExportMichletExecute(Sender: TObject);
begin
   Freeship.Edit.File_Export_Michlet;
   UpdateMenu;
end;{TMainForm.ExportMichletExecute}

procedure TMainForm.ExportAddMassExecute(Sender: TObject);
begin
   Freeship.Edit.File_Export_AddMass;
   UpdateMenu;
end;{TMainForm.ExportAddMassExecute}

procedure TMainForm.PropTask1Execute(Sender: TObject);
begin
   Freeship.Edit.Propeller_Task1;
   Updatemenu;
end;{TMainForm.PropTask1Execute}

procedure TMainForm.PropTask2Execute(Sender: TObject);
begin
   Freeship.Edit.Propeller_Task2;
   Updatemenu;
end;{TMainForm.PropTask2Execute}

procedure TMainForm.PropTask3Execute(Sender: TObject);
begin
   Freeship.Edit.Propeller_Task3;
   Updatemenu;
end;{TMainForm.PropTask3Execute}

procedure TMainForm.PropTask4Execute(Sender: TObject);
begin
   Freeship.Edit.Propeller_Task4;
   Updatemenu;
end;{TMainForm.PropTask4Execute}

procedure TMainForm.PropTask5Execute(Sender: TObject);
begin
   Freeship.Edit.Propeller_Task5;
   Updatemenu;
end;{TMainForm.PropTask5Execute}

procedure TMainForm.PropTaskRvrsExecute(Sender: TObject);
begin
   Freeship.Edit.Hydrodyn_Rvrs;
   Updatemenu;
end;{TMainForm.PropTaskRvrsExecute}

procedure TMainForm.HydrodynManeuvExecute(Sender: TObject);
begin
   Freeship.Edit.Hydrodyn_Maneuv;
   Updatemenu;
end;{TMainForm.HydrodynManeuvExecute}

//procedure TMainForm.HydrodynTask0Execute(Sender: TObject);
//begin
//   Freeship.Edit.Hydrodyn_Task1; // added masses
//   Updatemenu;
//end;{TMainForm.HydrodynTask0Execute}

procedure TMainForm.HydrodynTask1Execute(Sender: TObject);
begin
   Freeship.Edit.Hydrodyn_Task1; // Aerodynamic characteristics
   Updatemenu;
end;{TMainForm.HydrodynTask1Execute}

procedure TMainForm.HydrodynTask2Execute(Sender: TObject);
begin
//   Freeship.Edit.Hydrodyn_Task2; // Positional characteristics
   Updatemenu;
end;{TMainForm.HydrodynTask2Execute}

procedure TMainForm.HydrodynTask3Execute(Sender: TObject);
begin
//   Freeship.Edit.Hydrodyn_Task3;  // Rotative characteristics
   Updatemenu;
end;{TMainForm.HydrodynTask3Execute}

procedure TMainForm.HydrodynTask4Execute(Sender: TObject);  //для расчета присоединенных масс
begin
   Updatemenu;
end;{TMainForm.HydrodynTask4Execute}

procedure TMainForm.ResistanceKaperExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Kaper;
   Updatemenu;
end;{TMainForm.ResistanceKaperExecute}

procedure TMainForm.ResistancePlaningExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Planing;
   Updatemenu;
end;{TMainForm.ResistancePlaningExecute}


procedure TMainForm.ResistanceDelftExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Delft;
   Updatemenu;
end;{TMainForm.ResistanceDelftExecute}

procedure TMainForm.ResistanceHoltrExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Holtr;
   Updatemenu;
end;{TMainForm.ResistanceHoltrExecute}

procedure TMainForm.ResistanceOSTExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_OST;
   Updatemenu;
end;{TMainForm.ResistanceOSTExecute}

procedure TMainForm.ResistanceRBHSExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_RBHS;
   Updatemenu;
end;{TMainForm.ResistanceRBHSExecute}

procedure TMainForm.ResistanceMHExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_MH;
   Updatemenu;
end;{TMainForm.ResistanceMHExecute}

procedure TMainForm.ResistanceHollenExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Hollen;
   Updatemenu;
end;{TMainForm.ResistanceHollenExecute}

procedure TMainForm.ResistanceOortmerExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_Oortmer;
   Updatemenu;
end;{TMainForm.ResistanceOortmerExecute}

procedure TMainForm.ResistanceFungLeibExecute(Sender: TObject);
begin
   Freeship.Edit.Resistance_FungLeib;
   Updatemenu;
end;{TMainForm.ResistanceFungLeibExecute}

procedure TMainForm.FreeShipChangeCursorIncrement(Sender: TObject);
begin
  if (csdestroying in componentstate) then exit;
  Panel3.Caption:=Userstring(284)+': '+Truncate(Freeship.Visibility.CursorIncrement,5);
end;{TMainForm.FreeShipChangeCursorIncrement}

procedure TMainForm.Panel3Click(Sender: TObject);
var Str  : Ansistring;
    I    : integer;
    Value: TFloatType;
begin
   if Freeship.Surface.NumberOfControlPoints=0 then exit;
   Str:=Truncate(Freeship.Visibility.CursorIncrement,5);
   if InputQuery('',Userstring(285)+':',Str) then
   begin
      Val(Str,Value,I);
      if I=0 then Freeship.Visibility.CursorIncrement:=Value;
   end;
end;{TMainForm.Panel3Click}

procedure TMainForm.PointAlignExecute(Sender: TObject);
begin
   Freeship.Edit.Point_ProjectStraightLine;
   UpdateMenu;
end;{TMainForm.PointProjectLineExecute}

procedure TMainForm.ImportMichletWavesExecute(Sender: TObject);
begin
   Freeship.Edit.File_Import_MichletWaves;
   UpdateMenu;
end;{TMainForm.ImportMichletWavesExecute}

procedure TMainForm.ShowHydrostaticsExecute(Sender: TObject);
begin
   Freeship.Visibility.ShowHydrostaticData:=not Freeship.Visibility.ShowHydrostaticData;
   updatemenu;
end;{TMainForm.ShowHydrostaticsExecute}

procedure TMainForm.MirrorFaceExecute(Sender: TObject);
begin
   Freeship.Edit.Face_MirrorPlane;
   UpdateMenu;
end;{TMainForm.MirrorFaceExecute}

procedure TMainForm.ExportDXF2DPolylinesExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportDXF_2DPolylines;
   UpdateMenu;
end;{TMainForm.ExportDXF2DPolylinesExecute}

procedure TMainForm.FreeShipUpdateGeometryInfo(Sender: TObject);
begin
   Panel4.Caption:=IntToStr(Freeship.Surface.NumberOfControlFaces)+#32+Userstring(286)+', '+
                   IntToStr(Freeship.Surface.NumberOfControlEdges)+#32+Userstring(287)+', '+
                   IntToStr(Freeship.Surface.NumberOfControlPoints)+#32+Userstring(288)+', '+
                   IntToStr(Freeship.Surface.NumberOfControlCurves)+#32+Userstring(289);
end;{TMainForm.FreeShipUpdateGeometryInfo}

procedure TMainForm.TransformLackenbyExecute(Sender: TObject);
begin
   Freeship.Edit.Model_LackenbyTransformation;
   UpdateMenu;
end;{TMainForm.TransformLackenbyExecute}

procedure TMainForm.ExportIGESExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportIGES;
   UpdateMenu;
end;{TMainForm.ExportIGESExecute}

procedure TMainForm.ExportPartExecute(Sender: TObject);
begin
   Freeship.Edit.File_ExportPart;
   UpdateMenu;
end;{TMainForm.ExportPartExecute}

procedure TMainForm.ImportPartExecute(Sender: TObject);
begin
   Freeship.Edit.File_ImportPart;
   UpdateMenu;
end;{TMainForm.ImportPartExecute}

procedure TMainForm.LayerIntersectionExecute(Sender: TObject);
begin
   Freeship.Edit.Point_IntersectLayer;
   UpdateMenu;
end;{TMainForm.LayerIntersectionExecute}

procedure TMainForm.KeelRudderWizardExecute(Sender: TObject);
begin
   if not Assigned(FreeKeelWizardDialog) then
     FreeKeelWizardDialog := TFreeKeelWizardDialog.Create(Self);
   ShowTranslatedValues(FreeKeelWizardDialog);
   FreeKeelWizardDialog.Execute(freeship);
   UpdateMenu;
end;{TMainForm.KeelRudderWizardExecute}

procedure TMainForm.RedoExecute(Sender: TObject);
begin
   FreeShip.Edit.Redo;
   UpdateMenu;
   SetCaption;
end;{TMainForm.RedoExecute}

procedure TMainForm.ClearUndoExecute(Sender: TObject);
begin
   Freeship.Edit.Undo_Clear;
   UpdateMenu;
end;{TMainForm.ClearUndoExecute}

procedure TMainForm.ShowUndoHistoryExecute(Sender: TObject);
begin
   Freeship.Edit.Undo_ShowHistory;
   UpdateMenu;
end;{TMainForm.ShowUndoHistoryExecute}

procedure TMainForm.ImportPolyCadExecute(Sender: TObject);
begin
   Freeship.Edit.File_ImportPolycad;
   FOpenHullWindows;
   UpdateMenu;
end;{TMainForm.ImportPolyCadExecute}

procedure TMainForm.RemoveUnusedPointsExecute(Sender: TObject);
begin
   Freeship.Edit.Point_RemoveUnused;
   Updatemenu;
end;{TMainForm.RemoveUnusedPointsExecute}

procedure TMainForm.ExportGHSExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportGHS;
   UpdateMenu;
end;{TMainForm.ExportGHSExecute}

procedure TMainForm.ExportPAMExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportPAM;
   UpdateMenu;
end;{TMainForm.ExportPAMExecute}

procedure TMainForm.ShowFlowlinesExecute(Sender: TObject);
begin
   Freeship.Visibility.ShowFlowlines:=not Freeship.Visibility.ShowFlowlines;
   updatemenu;
end;{TMainForm.ShowFlowlinesExecute}

procedure TMainForm.AddCylinderExecute(Sender: TObject);
begin
   Freeship.Edit.Geometry_AddCylinder;
   UpdateMenu;
end;{TMainForm.AddCylinderExecute}

procedure TMainForm.SelectAllExecute(Sender: TObject);
begin
   Freeship.Edit.Selection_SelectAll;
   UpdateMenu;
end;{TMainForm.SelectAllExecute}

procedure TMainForm.ExportSTLExecute(Sender: TObject);
begin
   FreeShip.Edit.File_ExportSTL;
   UpdateMenu;
end;{TMainForm.ExportSTLExecute}

procedure TMainForm.CrossCurvesExecute(Sender: TObject);
begin
   Freeship.Edit.Hydrostatics_Crosscurves;
   UpdateMenu;
end;{TMainForm.CrossCurvesExecute}

procedure TMainForm.SelectLeakPointsExecute(Sender: TObject);
begin
  Freeship.Edit.Selection_SelectLeakPoints;
  UpdateMenu;
end;

{ this is used just once to dump menu/toolbutton icons }
procedure TMainForm.dumpIcons;
begin
  Freeship.Preferences.dumpIcons(MenuImages,ActionList1);
end;

// loads icons from FMenuIconDirectory that is set according to theme and icon size
procedure TMainForm.LoadToolIcons;
var i, II, sz, ilcnt:integer;
    A: TAction; AName, IName, IPath, IconFile:String;
    cil: TCustomImageList;
    bmp:TBitmap; png: TPortableNetworkGraphic; img: TLazIntfImage;

    procedure setToolBarButtonSize(ToolBar: TToolBar; sz: integer);
    var i:integer;
    begin
      ToolBar.ButtonHeight:= sz + (sz div 16)*2;
      ToolBar.ButtonWidth := sz + (sz div 16)*2;
      for i:=0 to ToolBar.ButtonCount-1 do
        begin
          ToolBar.Buttons[i].Height := ToolBar.ButtonHeight;
          ToolBar.Buttons[i].Width := ToolBar.ButtonWidth;
        end;

      //ToolBar.Parent.Width := ToolBar.Width;
      //ToolBar.Parent.Height := ToolBar.Height;
    end;

begin
  FreeShip.preferences.LoadImageListByActions(MenuImages, ActionList1);

  sz := Freeship.Preferences.ToolIconSize;

  setToolBarButtonSize(ToolBarFile, sz);
  setToolBarButtonSize(ToolBarVisibility, sz);
  setToolBarButtonSize(ToolBarLayers, sz);
  setToolBarButtonSize(ToolBarPoints, sz);
  setToolBarButtonSize(ToolBarEdit, sz);
  setToolBarButtonSize(ToolBarEdges, sz);
  setToolBarButtonSize(ToolBarFaces, sz);
  setToolBarButtonSize(ToolBarCurves, sz);

  FAllToolbarsControlsWidth := getAllToolbarsControlWidth;
  //AlignAllToolbars;
  ArrangeRibbonPanel(PanelMain);

  {
  // this is just to kick toolbar to autoresize
  Self.Height:=Self.Height+1;
  Self.Resize;
  Self.Height:=Self.Height-1;
  Self.Resize;
  Self.Invalidate;
  }
end;


end.
