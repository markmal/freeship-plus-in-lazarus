{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2012  by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : http://freeship-plus.pisem.su                                  }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }

{    Copyright © 2015-2015, Conversion to FPC/Lazarus by Mark Malakanov.                      }

{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }

{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }

{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }

{#############################################################################################}

unit FreeShipUnit;

{$IFDEF FPC}
//  {$MODE Delphi}{$H+}
    {$mode objfpc}{$H+}
{$ENDIF}

interface

uses SysUtils,// this declaration must be at the start, before the FreeGeometry unit
     {$ifdef windows}
  Windows, Windirs, shellapi, shfolder,
     {$ifndef LCL} JPeg,{$endif}
     {$endif}
     {$ifdef LCL}
  Interfaces, LCLIntf, LCLType, LCLProc,
  LMessages, IntfGraphics,
  FPImage,
  GraphType, GraphMath, Graphics, Controls,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
     {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

     {$endif}
     {$IFDEF LCLGTK2}
  Gtk2WSDialogs, GTK2,
     {$ENDIF}
  Types,
  IniFiles,
  Forms,
  Dialogs,
  Classes,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  ColorIniFile,
  ActnList,
  ImgList,
  FreeTypes,
  FreeVersionUnit,
  FasterList,
  FreeFileBuffer,
  FreeGeometry,
  FreeMatrices,
  FreeLanguageSupport,
  FreeControlPointFrm,
  FreeLogger,
  //FreeOpenDialog
  FreeFilePreviewDialog;


// FREE!ship uses British imperial format, eg 1 long ton=2240 lbs


const
  FreeShipExtention = '.ftm';
  // Default extention for hull model files
  SelectDistance = 3;
  // Max. distance in pixels between an item and the cursor in order to be selected
  Threshold = 3;
  // The distance that the cursor has to be moved before a controlpoint starts moving
  FontheightFactor = 140;
// used for calculating fontheight

resourcestring
  rsPointMove = 'point move';
  rsPointLinearConstraintChanged = 'ControlPoint Linear Constraint Changed';
  rsPointAnchorConstraintChanged = 'ControlPoint Anchor Constraint Changed';

type
  TFreeShip = class;                                                               // to be declared later
  TFreeIntersection = class;


  TFreeHydrostaticsData = record
    ModelMin, ModelMax: T3DCoordinate;
    // Min/max coordinates under given heelingangle and trim
    WlMin, WlMax: T3DCoordinate;
    // Min/max coordinates of the waterline
    SubMin, SubMax: T3DCoordinate;
    // Min/max extents of the submerged body
    WaterlinePlane: T3DPlane;
    AbsoluteDraft: TFloatType;
    // Depth of the lowest point of the hull beneath the waterplane
    // The following properties are always calculated
    Volume: TFloatType;
    // Displaced volume of the hull
    Displacement: TFloatType;
    // Displacement
    CenterOfBuoyancy: T3DCoordinate;
    // Center of gravity of displaced volume
    LCBPerc: TFloatType;
    LengthWaterline: TFloatType;
    BeamWaterline: TFloatType;
    BlockCoefficient: TFloatType;
    // BlockCoefficient
    WettedSurface: TFloatType;
    Leak: T3DCoordinate;
    // Coordinate encountered where the ship is making water
    // Mainframe properties
    Mainframearea: TFloatType;
    MainFrameCOG: T3DCoordinate;
    MainframeCoeff: TFloatType;
    // Waterplane properties
    Waterplanearea: TFloatType;
    WaterplaneCOG: T3DCoordinate;
    WaterplaneEntranceAngle: TFloatType;
    WaterplaneCoeff: TFloatType;
    WaterplaneMomInertia: T2DCoordinate;
    // Stability data
    KMtransverse: TFloatType;
    KMlongitudinal: TFloatType;
    // Lateral area and center
    LateralArea: TFloatType;
    LateralCOG: T3DCoordinate;

    // Prismatic coefficient
    PrismCoefficient: TFloatType;
    // Prismatic coefficient
    VertPrismCoefficient: TFloatType;
    // Sectional areas
    SAC: array of T2DCoordinate;
    // Weight and coordinates of CoG
    Weight_: TFloatType;
    CenterOfGravity_: T3DCoordinate;
    // BulbSection properties
    BulbSectionarea: TFloatType;
    BulbSectionCOG: T3DCoordinate;
    BulbSectionCoeff: TFloatType;
    // LateralArea above DWL and coordinates Z of CoG above DWL
    SDP: TFloatType;
    SDPCOG: T3DCoordinate;
    Zsdp: TFloatType;
    Xsdp: TFloatType;
    YWindAreaMax: TFloatType;
    // TransverseArea above DWL and coordinates Z of CoG
    SM: TFloatType;
    Zsm: TFloatType;
    XWindAreaMax: TFloatType;
    // Angle between BL and QBB
    QBBAngle: TFloatType;
    Z_min_board: TFloatType;
    // Y coordinate of waterplane area for left hull half
    Y_c_half: TFloatType;
    // Results of resistance calculation
    Wt: TFloatType;
    t0: TFloatType;
    EtaR: TFloatType;
    Dp: TFloatType;
    Tb: TFloatType;
    Vs1: TFloatType;
    Vs2: TFloatType;
    Vs3: TFloatType;
    Vs4: TFloatType;
    Vs5: TFloatType;
    Rt1: TFloatType;
    Rt2: TFloatType;
    Rt3: TFloatType;
    Rt4: TFloatType;
    Rt5: TFloatType;
    // Results of propeller calculation
    Kt: TFloatType;
    Kq: TFloatType;
    Eta0: TFloatType;
    Jp: TFloatType;
    Dpr: TFloatType;
    n_nom: TFloatType;
    Nprop: TFloatType;
    Ndiag: TFloatType;
    Zp: TFloatType;
    Teta: TFloatType;
    P_D: TFloatType;
    Peng: TFloatType;
    EtaG: TFloatType;
    EtaS: TFloatType;
  end;

  TFreeCrosscurvesData = record
    WaterlinePlane: T3DPlane;
    AbsoluteDraft: TFloatType;
    // Depth of the lowest point of the hull beneath the waterplane
    // The following properties are always calculated
    Volume: TFloatType;
    // Displaced volume of the hull
    Displacement: TFloatType;
    // Displacement
    CenterOfBuoyancy: T3DCoordinate;
    // Center of gravity of displaced volume
    // Stability data
    KNSinPhi: TFloatType;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeUndoObject                                             }

  {   TFreeUndoObject is an object class for undoing actions.                                         }
  {   It's function is very basic, just before each modification the file is saved to a the           }
  {   undo object rather then to a file. When the undo is called, the previous state will be          }
  {   read from the undo object and restored                                                          }
  {---------------------------------------------------------------------------------------------------}
  TFreeUndoObject = class
  private
    FOwner: TFreeShip;
    FUndoText: string;
    FUndoData: TFreeFileBuffer;
    // some other data to be stored
    FFileChanged: boolean;
    FFilenameSet: boolean;
    FFilename: string;
    FEditMode: TFreeEditMode;
    FTime: TDateTime;
    FIsTempRedoObject: boolean;
    function FGetMemory: integer;
    // calculates the amount of bytes used for each undo object
    function FGetTime: string;
    function FGetUndoText: string;
  public
    procedure Accept;
    constructor Create(Owner: TFreeShip);
    procedure Delete;
    destructor Destroy; override;
    procedure Restore;
    property Memory: integer read FGetMemory;
    // calculates the amount of bytes used for each undo object
    property Owner: TFreeShip read FOwner;
    property Time: string read FGetTime;
    property UndoData: TFreeFileBuffer read FUndoData;
    property UndoText: string read FGetUndoText;

  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeBackgroundImageData                                    }

  {   Freeship can import a max. of three different background images that may be coupled either      }
  {   to the bodyplan, profile or planview. These images can be used to trace the lines of an         }
  {   hullform and are stored within the FREE!ship file.                                              }
  {---------------------------------------------------------------------------------------------------}
  TFreeBackgroundImageData = class
  private
    FOwner: TFreeship;
    FAssignedView: TFreeViewType;
    FImageData: TJPEGImage;
    FQuality: integer;
    FOrigin: TPoint;
    FScale: TFloatType;
    FTransparent: boolean;
    FBlendingValue: integer;
    FTransparentColor: TColor;
    FVisible: boolean;
    FTolerance: integer;
  public
    procedure Clear;
    constructor Create(Owner: TFreeship);
    destructor Destroy; override;
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure UpdateData(Viewport: TFreeViewport);
    procedure UpdateViews;
    property AssignedView: TFreeViewType read FAssignedView;
    property BlendingValue: integer read FBlendingValue;
    property Image: TJPEGImage read FImageData;
    property Origin: TPoint read FOrigin;
    property Quality: integer read FQuality;
    property Scale: TFloatType read FScale;
    property Tolerance: integer read FTolerance;
    property Transparent: boolean read FTransparent;
    property TransparentColor: TColor read FTransparentColor;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeHydrostaticCalc                                        }

  {   TFreeHydrostaticCalc is an object class for hydrostatic calculations.                           }
  {   Each calculation has it's own draft, trim and angle of heel.                                    }
  {   Multiple calculations can be stored and then send to a report.                                  }
  {---------------------------------------------------------------------------------------------------}
  TFreeHydrostaticCalc = class
  private
    FOwner: TFreeShip;
    // Input data for each calculation
    FHeelingAngle: TFloatType;
    FTrim: TFloatType;
    FDraft: TFloatType;
    // Calculation flags
    FCalculated: boolean;
    FErrors: TFreeHydrostaticErrors;
    FHydrostaticType: TFreeHydrostaticType;
    // Determines how calculations are performed: short, extensive etc.
    // The following data is calculated
    FData: TFreeHydrostaticsData;
    FCalculations: TFreeHydrostaticsCalculate;
    FMainFrame: TFreeIntersection;
    FBulbSection: TFreeIntersection;
    function FGetErrorString: string;
    function FGetTrimAngle: TFloatType;
    function FGetWlPlane: T3DPlane;
    procedure FSetCalculated(val: boolean);
    procedure FSetDraft(Val: TFloatType);
    procedure FSetErrors(val: TFreeHydrostaticErrors);
    procedure FSetHeelingAngle(Val: TFloatType);
    procedure FSetHydrostaticType(val: TFreeHydrostaticType);
    procedure FSetTrim(Val: TFloatType);
  public
    procedure AddData(Strings: TStringList; Mode: TFreeHydrostaticsMode;
      Separator: char);
    // Add calculated data to a stringlist to either show in a report or save to disc
    procedure AddHeader(Strings: TStringList);
    procedure AddFooter(Strings: TStringList; Mode: TFreeHydrostaticsMode);
    function Balance(Displacement: TFloatType; FreeToTrim: boolean;
      var Output: TFreeCrosscurvesData): boolean;
    procedure Calculate;
    // The actual calculation of the hydrostatics finds place in this procedure
    procedure CalculateGravity;
    procedure CalculateVolume(WaterlinePlane: T3DPlane);
    procedure Clear;
    procedure Face_MoveZAuto;
    constructor Create(Owner: TFreeShip);
      virtual;
    destructor Destroy;
      override;
    procedure ShowData(Mode: TFreeHydrostaticsMode);
    property Calculated: boolean read FCalculated write FSetCalculated;
    property Calculations: TFreeHydrostaticsCalculate
      read FCalculations write FCalculations;
    property Data: TFreeHydrostaticsData read FData;
    property Draft: TFloatType read FDraft write FSetDraft;
    property Errors: TFreeHydrostaticErrors read FErrors write FSetErrors;
    property ErrorString: string read FGetErrorString;
    property HeelingAngle: TFloatType read FHeelingAngle write FSetHeelingAngle;
    property HydrostaticType: TFreeHydrostaticType
      read FHydrostaticType write FSetHydrostaticType;
    // Determines how calculations are performed: short, extensive etc.
    property Owner: TFreeShip read FOwner;
    property Trim: TFloatType read FTrim write FSetTrim;
    property TrimAngle: TFloatType read FGetTrimAngle;
    property WaterlinePlane: T3DPLane read FGetWlPlane;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeIntersection                                           }

  {   TFreeIntersection is a list of curves calculated from the intersection of a                     }
  {   ship hull (represented by a subdivision surface) and a plane.                                   }
  {   This plane can be a orthogonal plane (eg. stations, waterlines, buttocks) or a freely oriented  }
  {   3D plane (sent)                                                                                 }
  {---------------------------------------------------------------------------------------------------}
  TFreeIntersection = class
  private
    FOwner: TFreeShip;
    FItems: TFasterListTFreeSpline;
    FIntersectionType: TFreeIntersectionType;
    FPlane: T3DPlane;
    FBuild: boolean;
    FShowCurvature: boolean;
    FUseHydrostaticsSurfacesOnly: boolean;
    // used for lateral area, mainframe and waterplane properties
    function FGetColor: TColor;
    function FGetPlane: T3DPlane;
    function FGetCount: integer;
    function FGetDescription: string;
    function FGetItem(Index: integer): TFreeSpline;
    procedure FSetBuild(Val: boolean);
  public
    procedure Add(Item: TFreeSpline);
    procedure CalculateArea(Plane: T3DPlane; var Area: TFloatType;
      var COG: T3DCoordinate; var MomentOfInertia: T2DCoordinate);
    procedure Clear;
    constructor Create(Owner: TFreeShip);
    procedure CreateStarboardPart;
    // Create the starboardhalf of the ship, for use in hydrostatic calculations
    procedure Delete(Redraw: boolean);
    procedure DeleteItem(Item: TFreeSpline);
    destructor Destroy;
      override;
    procedure Draw(Viewport: TFreeViewport);
    procedure DrawAll;
    procedure Extents(var Min, Max: T3DCoordinate);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure Rebuild;
    procedure SaveToDXF(Strings: TStringList);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    property Build: boolean read FBuild write FSetBuild;
    property Color: TColor read FGetColor;
    property Count: integer read FGetCount;
    property Description: string read FGetDescription;
    property IntersectionType: TFreeIntersectionType
      read FIntersectionType write FIntersectionType;
    property Items[index: integer]: TFreeSpline read FGetItem;
    property Owner: TFreeShip read FOwner;
    property Plane: T3DPlane read FGetPlane write FPlane;
    property ShowCurvature: boolean read FShowCurvature write FShowCurvature;
    property UseHydrostaticsSurfacesOnly: boolean
      read FUseHydrostaticsSurfacesOnly write FUseHydrostaticsSurfacesOnly;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeMarker                                             }
  {---------------------------------------------------------------------------------------------------}
  TFreeMarker = class(TFreeSpline)
  private
    FVisible: boolean;
    FOwner: TFreeShip;
    function FGetSelected: boolean;
    procedure FSetSelected(val: boolean);
  public
    procedure Clear;
      override;
    function DistanceToCursor(X, Y: integer; Viewport: TFreeViewport): integer;
      override;
    procedure Delete;
    procedure Draw(Viewport: TFreeViewport);
      override;
    procedure LoadBinary(Source: TFreeFileBuffer);
      override;
    procedure SaveBinary(Destination: TFreeFileBuffer);
      override;
    property Owner: TFreeShip read FOwner;
    property Selected: boolean read FGetSelected write FSetSelected;
    property Visible: boolean read FVisible write FVisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeFlowline                                           }
  {---------------------------------------------------------------------------------------------------}
  TFreeFlowline = class
  private
    FProjectionPoint: T2DCoordinate;
    FProjectionView: TFreeViewType;
    FFlowLine: TFreeSpline;
    FBuild: boolean;
    FOwner: TFreeShip;
    FMethodNew: boolean;
    function FGetColor: TColor;
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    procedure FSetBuild(val: boolean);
    procedure FSetSelected(val: boolean);
  public
    procedure Clear;
    constructor Create(Owner: TFreeShip);
    procedure Delete;
    destructor Destroy;
      override;
    function DistanceToCursor(X, Y: integer; Viewport: TFreeViewport): integer;
    procedure Draw(Viewport: TFreeViewport);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure Rebuild;
    procedure SaveBinary(Destination: TFreeFileBuffer);
    property Build: boolean read FBuild write FSetBuild;
    property Color: TColor read FGetColor;
    property Owner: TFreeShip read FOwner;
    property Selected: boolean read FGetSelected write FSetSelected;
    property Visible: boolean read FGetvisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeVisibility                                             }

  {   This object stores all visibility options for the hull                                          }
  {---------------------------------------------------------------------------------------------------}
  TFreeVisibility = class(TPersistent)
  private
    FOwner: TFreeShip;
    FShowControlNet: boolean;
    FShowInteriorEdges: boolean;
    // Show the surface edges
    FShowStations: boolean;
    // Show the calculated stations
    FShowButtocks: boolean;
    // Show the calculated Buttocks
    FShowWaterlines: boolean;
    // Show the calculated Waterlines
    FShowDiagonals: boolean;
    // Show the calculated Diagonals
    FModelView: TFreeModelView;
    // Show half or entire ship
    FShowNormals: boolean;
    // Show normals of selected surface patches
    FShowGrid: boolean;
    // Show the grid of intersections in the plan,profile and bodyplan view
    FShowMarkers: boolean;
    FShowControlCurves: boolean;
    FShowCurvature: boolean;
    FShowHydrostaticData: boolean;
    FShowHydrostDisplacement: boolean;
    FShowHydrostLateralArea: boolean;
    FShowHydrostSectionalAreas: boolean;
    FShowHydrostMetacentricHeight: boolean;
    FShowHydrostLCF: boolean;
    FShowFlowlines: boolean;
    FCurvatureScale: TFloatType;
    // Scalefactor used to increase or decrease the size of the curvature plot
    FCursorIncrement: TFloatType;
    // Distance added when the active controlpoint is moved withe the arrow keys
    procedure FSetCursorIncrement(val: TFloatType);
    procedure FSetCurvatureScale(Val: TFloatType);
    procedure FSetShowButtocks(Val: boolean);
    procedure FSetShowControlNet(Val: boolean);
    procedure FSetShowCurvature(Val: boolean);
    procedure FSetShowDiagonals(Val: boolean);
    procedure FSetShowFlowlines(Val: boolean);
    procedure FSetShowGrid(Val: boolean);
    procedure FSetModelView(Val: TFreeModelView);
    procedure FSetShowInteriorEdges(Val: boolean);
    procedure FSetShowMarkers(Val: boolean);
    procedure FSetShowNormals(Val: boolean);
    procedure FSetShowStations(Val: boolean);
    procedure FSetShowWaterlines(Val: boolean);
    procedure FSetShowControlCurves(Val: boolean);
    procedure FSetShowHydrostaticData(Val: boolean);
  public
    constructor Create(Owner: TFreeShip);
    procedure Clear;
    procedure DecreaseCurvatureScale;
    procedure IncreaseCurvatureScale;
    procedure LoadBinary(Source: TFreeFilebuffer);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    property Owner: TFreeShip read FOwner write FOwner;
  published
    property CursorIncrement: TFloatType read FCursorIncrement
      write FSetCursorIncrement;
    property CurvatureScale: TFloatType read FCurvatureScale write FSetCurvatureScale;
    property ModelView: TFreeModelView read FModelView write FSetModelView;
    property ShowButtocks: boolean read FShowButtocks write FSetShowButtocks;
    property ShowControlCurves: boolean read FShowControlCurves
      write FSetShowControlCurves;
    property ShowControlNet: boolean read FShowControlNet write FSetShowControlNet;
    property ShowCurvature: boolean read FShowCurvature write FSetShowCurvature;
    property ShowDiagonals: boolean read FShowDiagonals write FSetShowDiagonals;
    property ShowFlowlines: boolean read FShowFlowlines write FSetShowFlowlines;
    property ShowGrid: boolean read FShowGrid write FSetShowGrid;
    property ShowHydrostaticData: boolean read FShowHydrostaticData
      write FSetShowHydrostaticData;
    property ShowInteriorEdges: boolean read FShowInteriorEdges
      write FSetShowInteriorEdges;
    property ShowMarkers: boolean read FShowMarkers write FSetShowMarkers;
    property ShowNormals: boolean read FShowNormals write FSetShowNormals;
    property ShowStations: boolean read FShowStations write FSetShowStations;
    property ShowWaterlines: boolean read FShowWaterlines write FSetShowWaterlines;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeEdit                                                   }

  {   Container class for all editing commandsns for the hull                                         }
  {---------------------------------------------------------------------------------------------------}
  TFreeEdit = class
  private
    FFreeShip: TFreeShip;
    FRecentFiles: TStringList;

    PreviewFrm: TForm;
    PreviewImg: TImage;
    FProgressBar: TProgressBar;
    function FGetRecentFile(Index: integer): string;
    function FGetRecentFileCount: integer;
    procedure SaveDialogTypeChange(Sender: TObject);
  public
    procedure AddToRecentFiles(Filename: string);
    // Takes a filename and adds it to the list with recent files
    procedure BackgroundImage_Delete(Viewport: TFreeViewport);
    // Delete the backgrundimage associated with this view
    procedure BackgroundImage_Open(Viewport: TFreeViewport);
    // browse for and open a backgroundimage
    constructor Create(FreeShip: TFreeShip);
    function CreateRedoObject: TFreeUndoObject;
    // Creates redo data before an undo is done
    function CreateUndoObject(UndoText: string; Accept: boolean): TFreeUndoObject;
    // Creates undodata just prior to modifications
    procedure Curve_Add;
    // Add a new controlcurve
    destructor Destroy; override;
    procedure Edge_Collapse;
    // Remove an edge by replacing the two connected faces by one controlface
    procedure Edge_Connect;
    // Create a new edge by connection two controlpoints belonging to the same controlface
    procedure Edge_Crease;
    // Switch selected edges between normal or crease edges (knuckle lines)
    procedure Edge_Extrude;
    // Create new controlfaces by extruding selected boundary edges (eg edges with only 1 controlface connected to it)
    procedure Edge_Split;
    // Create new controlpoints by splitting an controledge into two.
    procedure Face_Assemble;
    procedure Face_DeleteNegative;
    // Deletes all faces on the starboardside of the hull
    procedure Face_Flip;
    // Inverts the normal-direction of all selected controlfaces
    procedure Face_MirrorPlane;
    // Mirrors all selected faces in a 3D plane
    procedure Face_New;
    // Creates a new controlface from the currently selected controlpoints
    procedure Face_Rotate;
    // Rotate selected faces around the X,Y and/or Z axis
    procedure Face_RotateM;
    // Rotate all model around the X,Y axis
    procedure Face_Scale;
    // Scale selected faces
    procedure Face_Move;
    // Move selected faces in X,Y and Z direction
    procedure File_ExportArchimedes;
    // Exports stations to Archimedes or ArchimedesMB
    procedure File_ExportCoordinates;
    // export the coordinates of all controlpoints to a textfile
    procedure File_ExportDXF_2DPolylines;
    // Export all intersections to an individual DXF file as 2D polylines
    procedure File_ExportDXF_3DPolylines;
    // Export all lines to a 3D DXF model as polylines
    procedure File_ExportDXF_Faces;
    // Export all faces to a 3D DXF model
    procedure File_ExportFEF;
    // Save to a Freeship Exchange Format (FEF) file
    procedure File_ExportGHS;
    // Save ordinates to the GHS file format
    procedure File_ExportPAM;
    // Save ordinates to the PAM file format for Added masses calcs
    procedure File_Export_AddMass;
    // Creates a file to be read by the CFD program Added masses
    procedure File_Execute_AddMass;
    //(Component:TComponent);               // Output in Added masses program
    procedure File_ExportPart;
    // Save part of the geometry to a file
    procedure File_ExportIGES;
    // Save NURBS patches to an IGES file
    procedure File_Export_Michlet;
    // Creates a file to be read by the CFD program Michlet
    procedure File_Import_MichletWaves;
    procedure File_ExportObj;
    // Saves the model as a wavefront .Obj file
    procedure File_ExportOffsets;
    // Exports all intersections to a textfile as 3D points
    procedure File_ExportSTL;
    // Export the surface to a STL file
    procedure File_ImportCarene;
    // imports a Carene XYZ file and creates a multichine boat with developable surfaces
    procedure File_ImportChines;
    // Import chines from a textfile and fit a surface through them
    procedure File_ImportFEF;
    // Import a Freeship Exchange Format (FEF) file
    procedure File_ImportHull; overload; virtual;
    // Imports a file created with Carlssons's Hulls program
    procedure File_ImportHull(Filename: string; Quiet: boolean); reintroduce; overload;
    // Imports a file created with Carlssons's Hulls program
    procedure File_ImportPart;
    // Import a partfile and add it to the current geometry
    procedure File_ImportPolycad;
    // Imports a PolyCad file
    procedure File_ImportSurface;
    // Imports a number of curves and fits a surface
    procedure File_ImportVRML;
    // Import a VRML 1.0 file
    procedure File_Load; overload; virtual;
    // Load a FREE!ship file by showing an opendialog
    procedure File_Load(filename: string); reintroduce; overload;
    // Loads the given filename quietly
    function File_Save:boolean;
    // save as FREE!ship file without prompting for a filename (must already been set)
    function File_SaveAs:boolean;
    // Ask for filename and save as FREE!ship file
    procedure Flowline_Add(Source: T2DCoordinate; View: TFreeviewType);
    procedure Geometry_AddCylinder;
    function Hydrostatics_Calculate(Draft, AngleOfHeel, Trim: TFloatType):
      TFreeHydrostaticCalc;// Creates and calculates a hydrostatics calculation
    procedure Hydrostatics_Crosscurves;
    // Opens the dialog to calculate crosscurves
    procedure Hydrostatics_Dialog;
    // Opens the hydrostatics dialog and calculates hydrostatic data for a range of inputdata
    procedure ImportFrames;
    // Loads a bodyplane and tries to fit a surface to it
    function Intersection_Add(IntType: TFreeIntersectionType;
      Distance: TFloatType): TFreeIntersection;
    // Add a new intersection at the specified location
    procedure Intersection_AddToList(Intersection: TFreeIntersection);
    // Adds an intersection to the appropriate list
    procedure Intersection_Dialog;
    // Pops up the dialog in whcih to add or delete stations, buttocks and waterlines
    procedure Layer_AutoGroup;
    // All connected patches surrounded by crease edges are grouped together into a new layer
    procedure Layer_Develop;
    // Developes all developable layers
    procedure Layer_Dialog;
    // Show layer dialog window
    procedure Layer_DeleteEmpty(Quiet: boolean);
    // Delete all layers that are empty from the model
    function Layer_New: TFreeSubdivisionLayer;
    // Add a new empty layer
    procedure Marker_Add(Marker: TFreeMarker);
    // Adds a marker to the list with markers
    procedure Marker_Delete;
    // Delete all markers from the model
    procedure Marker_Import;
    // Import markers from a textfile
    procedure Model_Check(ShowResult: boolean);
    // Checks the surface for inconsistent normal directions and leaks
    function Model_New: boolean;
    // Start a new model (with a predefined surface)
    procedure Model_LackenbyTransformation;
    // Affine hullform transformation according to Lackenby
    procedure Model_Scale(ScaleVector: T3DCoordinate;
      OverrideLock, AdjustMarkers: boolean);
    // Scale the entire model and all equivalent data such as stations etc.
    procedure Point_Collapse;
    // Merge two selected edges by removing their common controlpoint.
    procedure Point_RemoveUnused;
    // removes any unused points from the model
    procedure Point_InsertPlane;
    // Finds all intersection of VISIBLE edges and a 3D plane, and inserts a point on each of these edges
    procedure Point_IntersectLayer;
    // Calculates the intersection points of two layers
    procedure Point_Lock;
    // Locks all selected points
    function Point_New: TFreeSubdivisionControlPoint;
    // Add a new point to the model with no edges/faces attached
    procedure Point_AnchorToPoint;
    procedure Point_ProjectStraightLine;
    // Project all selected points onto a straight line through the first and last selected points
    procedure Point_ProjectStraightLinePermanentConstraint;
      // Project all selected points onto a straight line through the first and last selected points
      // setting first and last selected points as linear constraint for all other selected points.
    procedure Point_Unlock;
    // Unlocks all selected locked points
    procedure Point_UnlockAll;
    // Unlocks all points
    function ProceedWhenLockedPoints: boolean;
    // Function that shows a warning when certain edit commands are invoked and the model contains locked points
    procedure Redo;
    // Restores the state of the model as it was after the previous undone
    procedure Resistance_Delft;
    // Calculate resistance of yachts according to Delft systematic yacht series
    procedure Resistance_Kaper;
    // Calculate resistance of slender hulls (canoes) according to John Winters
    procedure Resistance_Planing;
    // Calculate resistance of planing ships
    procedure Resistance_Holtr;
    // Calculate resistance of ships
    procedure Resistance_Hollen;
    // Calculate resistance of ships by Hollenbach method
    procedure Resistance_Oortmer;
    // Calculate resistance of ships by Oortmerssen method
    procedure Resistance_FungLeib;
    // Calculate resistance of naval ships by Fung-Leibman method
    procedure Resistance_OST;
    // Calculate resistance of ships by OST
    procedure Resistance_RBHS;
    // Calculate resistance of high speed round bilge ships
    procedure Resistance_MH;
    // Calculate resistance of multihull ships
    procedure Propeller_Task1;
    // Calculate propeller task1
    procedure Propeller_Task2;
    // Calculate propeller task2
    procedure Propeller_Task3;
    // Calculate propeller task3
    procedure Propeller_Task4;
    // Calculate propeller task4
    procedure Propeller_Task5;
    // Calculate propeller task5
    procedure Hydrodyn_Rvrs;
    // Calculate revers of ship
    procedure Hydrodyn_Maneuv;
    // Calculate maneuver of ship
    procedure Hydrodyn_Task1;
    // Calculate aero characteristics task1
    procedure Selection_Clear;
    // Deselect all selected items at once
    procedure Selection_Delete;
    // Delete all selected items
    procedure Selection_SelectAll;
    // Select all visible items
    procedure Selection_SelectAllControlPoints;
    procedure Selection_SelectLeakPoints;
    // Select all leakpoints
    procedure Undo;
    // Restores the state of the model as it was before the last modification
    procedure Undo_Clear;
    // Clear the undo history
    procedure Undo_ShowHistory; //Show the undo history
    procedure OnFilePreview(Sender: TObject; filename: string);
    procedure OnFaceRebuilt (Sender: TObject; current:integer; total:integer);
    property FreeShip: TFreeShip read FFreeShip write FFreeShip;
    property ProgressBar:TProgressBar read FProgressBar write FProgressBar;
    property RecentFiles: TStringList read FRecentFiles write FRecentFiles;
    property RecentFile[index: integer]: string read FGetRecentFile;
    // retrieve a filename from the recently used file list
    property RecentFileCount: integer read FGetRecentFileCount;
    // The number of files in the recently used file list
    function getPreviewImage(aFileName: string): TJPegImage;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreePreferences                                            }

  {   Container class for all program settings                                                        }
  {---------------------------------------------------------------------------------------------------}

  TApplicationScope = (asMachine,asUser);

  TFreePreferences = class(TPersistent)
  private
    FOwner: TFreeShip;
    FMainForm: TForm;
    FPointSize: integer;
    // Half width of controlpoints in pixels when drawn on screen
    // Colors
    FButtockColor: TColor;
    FWaterlineColor: TColor;
    FStationColor: TColor;
    FDiagonalColor: TColor;
    FEdgeColor: TColor;   // Color of normal edges
    FCreaseColor: TColor;   // color of crease edges
    FCreaseEdgeColor: TColor;   // color of crease control-edges
    FGridColor: TColor;   // Color of gridlines
    FGridFontColor: TColor;   // Color of font with gridlines
    FCreasePointColor: TColor;   // Color of crease vertices
    FRegularPointColor: TColor;
    FCornerPointColor: TColor;
    // Color of cornerpoints and points with at least 3 crease edges
    FDartPointColor: TColor;
    FSelectColor: TColor;   // Color of selected items
    FLayerColor: TColor;   // Default color for new layers
    FNormalColor: TColor;   // color of surface normals
    FUnderWaterColor: TColor;
    // Default color used for shading underwaterpart of the vessel
    FViewportColor: TColor;
    FLeakPointColor: TColor;
    FMarkerColor: TColor;
    FCurvaturePlotColor: TColor;
    FControlCurveColor: TColor;
    FHydrostaticsFontColor: TColor;
    FZebraStripeColor: TColor;

    FGlobalConfigDirectory: string;
    // Default directory where FreeShip.ini file is stored
    FGlobalAppDataDirectory: string;
    // Default directory where FreeShip programs, r/o data and resource files stored
    FUserConfigDirectory: string;
    // Default directory where users FreeShip.ini file is stored
    FUserAppDataDirectory: string;
    // Default directory where users FreeShip programs and r/o resource files stored
    FUserDataDirectory: string;
    // Default directory where users FreeShip r/w data (projects etc) stored

    FExecDirectory: string;
    // directory where users FreeShip 3-rd party executables located
    FManualsDirectory: string;   // Manuals directory
    FTempDirectory: string;
    // 3-rd party executables write/read temp files
    FMenuIconDirectory: string;
    // Directory where menu icons are 'Themes/Default/icons/16';
    //FToolIconDirectory: string;
    // Directory where toolbar icons are 'Themes/Default/icons/24';

    FThemeName: string;
    FParentThemeName: string;
    FMenuIconSize: integer;
    FToolIconSize: integer;

    FLastDirectory: string;   // directory of last Open/Save
    FInitDirectory: string;
    // Default directory where freeship.exe started
    FOpenDirectory: string;
    // Default directory to open existing files
    FSaveDirectory: string;   // Default directory to save files
    FImportDirectory: string; // Default directory to import files
    FExportDirectory: string; // Default directory to export files

    FGlobalOpenDirectory: string;
    // Default Global directory to open existing files
    FGlobalImportDirectory: string;
    // Default Global directory to import files

    FLanguagesDirectory: string;
    // Default directory where Language files stored. Default FGlobalAppDataDirectory/Languages
    FLanguageFile: string;
    FLanguage: string;

    FMaxUndoMemory: integer;
    // Max. amount of allowable undo memory in megabytes
    FFbmEncoding: string;
    //encoding that is used to convert national strings from/to FBM files
    FApplicationScope : TApplicationScope ; // can be 'machine' or 'user'

    procedure detectApplicationScope;
    function getGlobalConfigDirectory: string;
    function getGlobalAppDataDirectory: string; // dir for per-machine install
    function getUserConfigDirectory: string;
    function getUserAppDataDirectory: string; // dir for per-user install
    function getUserDataDirectory: string; // dir for your projects
    function getTempDirectory: string;

    function FGetExportDirectory: string;
    function FGetImportDirectory: string;
    function FGetOpenDirectory: string;
    function FGetSaveDirectory: string;
    function FGetInitDirectory: string;

    function FGetGlobalImportDirectory: string;
    function FGetGlobalOpenDirectory: string;

    function FGetLastDirectory: string;

    procedure FSetOpenDirectory(val: string);
    procedure FSetLastDirectory(val: string);

    procedure FSetViewportColor(Val: TColor);
  public
    procedure Clear;
    constructor Create(Owner: TFreeShip);
    procedure Edit;
    procedure Load;
    procedure LoadFromIni(Filename: string);
    procedure LoadFromDta(Filename: string);
    procedure ResetColors;
    procedure ResetDirectories;
    procedure SetDefaults;
    procedure Save;
    procedure SaveToDta;

    function getThemeConfigFile(ThemeName: string): string;
    function getParentThemeName(ThemeName: string): string;
    procedure getAllThemes(ss: TStrings);
    procedure getThemesInDir(dir: string; ss: TStrings);
    function GetIconFileName(ThemeName, IconName: string;
      IconSize: integer): string;
    function GetToolIconDirectory: string;
    procedure dumpIcons(ImageList: TImageList; ActionList: TActionList);
    procedure LoadImageIntoBitmap(Bitmap: TBitmap; Name: string);
    procedure LoadImageListByActions(ImageList: TImageList;
      ActionList: TActionList);
    procedure LoadImageIntoList(ImageList: TImageList; Item: integer; Name: string);
    function IsThemeCustom(ThemeName: string): boolean;
    procedure SaveCustomTheme(Dialog: TForm);
    procedure SaveThemeAsCustom(Dialog: TForm);
    procedure LoadTheme(ThemeName: string);
    procedure LoadThemeIni(FileName: string);
    procedure SaveTheme(ThemeName, ParentThemeName: string);
    property Owner: TFreeShip read FOwner write FOwner;
    property MainForm: TForm read FMainForm write FMainForm;
  published
    // General options
    property PointSize: integer read FPointSize write FPointSize;
    // Color settings
    property ButtockColor: TColor read FButtockColor write FButtockColor;
    property ControlCurveColor: TColor read FControlCurveColor write FControlCurveColor;
    property CornerPointColor: TColor read FCornerPointColor write FCornerPointColor;
    property CreaseColor: TColor read FCreaseColor write FCreaseColor;
    property CreaseEdgeColor: TColor read FCreaseEdgeColor write FCreaseEdgeColor;
    property CurvaturePlotColor: TColor read FCurvaturePlotColor
      write FCurvaturePlotColor;
    property DiagonalColor: TColor read FDiagonalColor write FDiagonalColor;
    property GridColor: TColor read FGridColor write FGridColor;
    property GridFontColor: TColor read FGridFontColor write FGridFontColor;
    property HydrostaticsFontColor: TColor
      read FHydrostaticsFontColor write FHydrostaticsFontColor;
    property EdgeColor: TColor read FEdgecolor write FEdgeColor;
    property ExportDirectory: string read FGetExportDirectory write FExportDirectory;
    property CreasePointColor: TColor read FCreasePointColor write FCreasePointColor;
    property Language: string read FLanguage write FLanguage;
    property LanguageFile: string read FLanguageFile write FLanguageFile;
    property LayerColor: TColor read FLayerColor write FLayerColor;
    property LeakPointColor: TColor read FLeakPointColor write FLeakPointColor;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property MaxUndoMemory: integer read FMaxUndoMemory write FMaxUndoMemory;
    property NormalColor: TColor read FNormalColor write FNormalColor;
    property InitDirectory: string read FGetInitDirectory write FInitDirectory;
    property ImportDirectory: string read FGetImportDirectory write FImportDirectory;
    property GlobalImportDirectory: string
      read FGetGlobalImportDirectory write FGlobalImportDirectory;
    property LastDirectory: string read FGetLastDirectory write FSetLastDirectory;
    property OpenDirectory: string read FGetOpenDirectory write FSetOpenDirectory;
    property GlobalOpenDirectory: string read FGetGlobalOpenDirectory
      write FGlobalOpenDirectory;
    property SaveDirectory: string read FGetSaveDirectory write FSaveDirectory;
    property LanguagesDirectory: string read FLanguagesDirectory
      write FLanguagesDirectory;
    property ExecDirectory: string read FExecDirectory write FExecDirectory;
    property ManualsDirectory: string read FManualsDirectory write FManualsDirectory;
    property TempDirectory: string read FTempDirectory write FTempDirectory;
    property StationColor: TColor read FStationColor write FStationColor;
    property UnderWaterColor: TColor read FUnderWaterColor write FUnderWaterColor;
    property RegularPointColor: TColor read FRegularPointColor write FRegularPointColor;
    property DartPointColor: TColor read FDartPointColor write FDartPointColor;
    property SelectColor: TColor read FSelectColor write FSelectColor;
    property ViewportColor: TColor read FViewportColor write FSetViewportColor;
    property WaterlineColor: TColor read FWaterlineColor write FWaterlineColor;
    property ZebraStripeColor: TColor read FZebraStripeColor write FZebraStripeColor;

    property MenuIconDirectory: string read FMenuIconDirectory write FMenuIconDirectory;
    property ToolIconDirectory: string read GetToolIconDirectory;
    property MenuIconSize: integer read FMenuIconSize write FMenuIconSize;
    property ToolIconSize: integer read FToolIconSize write FToolIconSize;
    property Theme: string read FThemeName;
    property FbmEncoding: string read FFbmEncoding write FFbmEncoding;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeProjectSettings                                        }

  {   Container class for project settings for each projecttl                                         }
  {---------------------------------------------------------------------------------------------------}
  TFreeProjectSettings = class
  private
    FOwner: TFreeShip;
    FMainparticularsHasBeenset: boolean;
    // Flag to check if the main particulars have been set before hydrostatic calculationss are being performed
    FDisableModelCheck: boolean;
    // Disable the automatic checking of the surface
    FEnableModelAutoMove: boolean;
    // Unable the automatic moving model along Z
    FEnableBonjeanSAC: boolean;
    // Unable calculation and save in file Bonjean scale and SAC
    FProjectAppendageCoefficient: TFloatType;
    FProjectBeam: TFloatType;
    FProjectDraft: TFloatType;
    FProjectLength: TFloatType;
    FProjectWaterDensity: TFloatType;
    FProjectWaterTemper: TFloatType;
    FProjectMainframeLocation: TFloatType;
    FUseDefaultMainframeLocation: boolean;
    // If set to true, the mainframe location is set to 0.5*project length, if false then value in FProjectMainframeLocation is used
    FProjectName: string;
    FProjectDesigner: string;
    FProjectComment: string;
    FProjectFileCreatedBy: string;
    FProjectShadeUnderwaterShip: boolean;
    FSavePreview: boolean;
    FProjectUnderWaterColor: TColor;
    FProjectUnits: TFreeUnitType;
    FProjectPrecision: TFreePrecisionType;
    FProjectSimplifyIntersections: boolean;
    FFreeHydrostaticCoefficients: TFreeHydrostaticCoeff;
    // General hydrostatics calculation settings
    FStartDraft: TFloatType;
    FEndDraft: TFloatType;
    FDraftStep: TFloatType;
    FTrim: TFloatType;
    // crosscurves settings
    FDisplacements: TFloatArray;
    FNoDisplacements: integer;
    FMinimumDisplacement: TFloatType;
    FMaximumDisplacement: TFloatType;
    FDisplIncrement: TFloatType;
    FUseDisplIncrements: boolean;
    FNoAngles: integer;
    FAngles: TFloatArray;
    FNoStabTrims: integer;
    FStabTrims: TFloatArray;
    FFreeTrim: boolean;
    FVCG: TFloatType;

    procedure FSetFreeHydrostaticCoefficients(val: TFreeHydrostaticCoeff);
    procedure FSetDisableModelCheck(Val: boolean);
    procedure FSetEnableModelAutoMove(Val: boolean);
    procedure FSetEnableBonjeanSAC(Val: boolean);
    function FGetProjectMainframeLocation: TFloatType;
    procedure FSetProjectAppendageCoefficient(Val: TFloatType);
    procedure FSetProjectBeam(Val: TFloatType);
    procedure FSetProjectComment(Val: string);
    procedure FSetProjectDraft(Val: TFloatType);
    procedure FSetProjectFileCreatedBy(Val: string);
    procedure FSetProjectLength(Val: TFloatType);
    procedure FSetProjectMainframeLocation(val: TFloatType);
    procedure FSetProjectName(Val: string);
    procedure FSetProjectDesigner(Val: string);
    procedure FSetProjectShadeUnderwaterShip(Val: boolean);
    procedure FSetProjectSimplifyIntersections(val: boolean);
    procedure FSetProjectUnderWaterColor(Val: TColor);
    procedure FSetProjectUnits(Val: TFreeUnitType);
    procedure FSetProjectWaterDensity(Val: TFloatType);
    procedure FSetProjectWaterTemper(Val: TFloatType);
    procedure FSetSavePreview(val: boolean);
    procedure FSetStartDraft(Val: TFloatType);
    procedure FSetEndDraft(Val: TFloatType);
    procedure FSetDraftStep(Val: TFloatType);
    procedure FSetTrim(Val: TFloatType);
    procedure FSetUseDefaultMainframeLocation(Val: boolean);
  public
    procedure Clear;
    constructor Create(Owner: TFreeShip);
    procedure Edit;
    // User input of mainparticulars and project setting
    procedure LoadBinary(Source: TFreeFilebuffer; Image: TJPegImage);
      overload; virtual;
    procedure SaveBinary(Destination: TFreeFileBuffer);
    property DisableModelCheck: boolean read FDisableModelCheck
      write FSetDisableModelCheck;
    property EnableModelAutoMove: boolean read FEnableModelAutoMove
      write FSetEnableModelAutoMove;
    property EnableBonjeanSAC: boolean read FEnableBonjeanSAC
      write FSetEnableBonjeanSAC;
    property Hydrostatics_Startdraft: TFloatType read FStartDraft write FSetStartDraft;
    property Hydrostatics_EndDraft: TFloatType read FEndDraft write FSetEndDraft;
    property Hydrostatics_DraftStep: TFloatType read FDraftStep write FSetDraftStep;
    property Hydrostatics_Trim: TFloatType read FTrim write FSetTrim;
    property MainparticularsHasBeenset: boolean read FMainparticularsHasBeenset;
    property Owner: TFreeShip read FOwner write FOwner;
    property ProjectAppendageCoefficient: TFloatType
      read FProjectAppendageCoefficient write FSetProjectAppendageCoefficient;
    property ProjectBeam: TFloatType read FProjectBeam write FSetProjectBeam;
    property ProjectCoefficients: TFreeHydrostaticCoeff
      read FFreeHydrostaticCoefficients write FSetFreeHydrostaticCoefficients;
    property ProjectComment: string read FProjectComment write FSetProjectComment;
    property ProjectDraft: TFloatType read FProjectDraft write FSetProjectDraft;
    property ProjectFileCreatedBy: string read FProjectFileCreatedBy
      write FSetProjectFileCreatedBy;
    property ProjectLength: TFloatType read FProjectLength write FSetProjectLength;
    property ProjectMainframeLocation: TFloatType
      read FGetProjectMainframeLocation write FSetProjectMainframeLocation;
    property ProjectName: string read FProjectName write FSetProjectName;
    property ProjectDesigner: string read FProjectDesigner write FSetProjectDesigner;
    property ProjectShadeUnderwaterShip: boolean
      read FProjectShadeUnderwaterShip write FSetProjectShadeUnderwaterShip;
    property ProjectSimplifyIntersections: boolean
      read FProjectSimplifyIntersections write FSetProjectSimplifyIntersections;
    property ProjectUnderWaterColor: TColor
      read FProjectUnderWaterColor write FSetProjectUnderWaterColor;
    property ProjectUnits: TFreeUnitType read FProjectUnits write FSetProjectUnits;
    property ProjectPrecision: TFreePrecisionType
      read FProjectPrecision write FProjectPrecision;
    property ProjectWaterDensity: TFloatType
      read FProjectWaterDensity write FSetProjectWaterDensity;
    property ProjectWaterTemper: TFloatType
      read FProjectWaterTemper write FSetProjectWaterTemper;
    property SavePreview: boolean read FSavePreview write FSetSavePreview;
    property UseDefaultMainframeLocation: boolean
      read FUseDefaultMainframeLocation write FSetUseDefaultMainframeLocation;
  end;

  TFasterListTFreeViewPort = specialize TFasterList<TFreeViewPort>;
  TFasterListTFreeMarker = specialize TFasterList<TFreeMarker>;
  TFasterListTFreeIntersection = specialize TFasterList<TFreeIntersection>;
  TFasterListTFreebackgroundImagedata = specialize TFasterList<TFreebackgroundImagedata>;
  TFasterListTFreeFlowLine = specialize TFasterList<TFreeFlowLine>;
  TFasterListTFreeHydrostaticCalc = specialize TFasterList<TFreeHydrostaticCalc>;
  TFasterListTFreeUndoObject = specialize TFasterList<TFreeUndoObject>;

  {---------------------------------------------------------------------------------------------------}
  {                                       TFreeShip                                                   }

  {   TFreeShip is the actual component used for modelling and representing the ship                  }
  {---------------------------------------------------------------------------------------------------}
  TFreeShip = class(TComponent)
  private     { Private declarations }
    FMainForm: TForm;
    FViewports: TFasterListTFreeViewPort;
    // List containing all viewports associated with the hullform
    FPrecision: TFreePrecisionType;
    FFileVersion: TFreeFileVersion;
    FEditMode: TFreeEditMode;
    // The component has different edit-modes which determine how the program responds to mouse-events
    FPreferences: TFreePreferences;
    FActiveControlPoint: TFreeSubdivisionControlPoint;
    // The last selected controlpoint (still selected)
    FFileChanged: boolean;
    // Flag to keep track of modifications to the file
    FSurface: TFreeSubdivisionSurface;
    FFilename: string;
    // Filename of the current project;
    FEdit: TFreeEdit;
    // Containerclass for all editing commands
    FStations: TFasterListTFreeIntersection;
    FButtocks: TFasterListTFreeIntersection;
    FWaterlines: TFasterListTFreeIntersection;
    FDiagonals: TFasterListTFreeIntersection;
    FMarkers: TFasterListTFreeMarker;
    FBackgroundImages: TFasterListTFreebackgroundImagedata;
    FFlowLines: TFasterListTFreeFlowLine;
    FSelectedFlowlines: TFasterListTFreeFlowLine;
    FSelectedMarkers: TFasterListTFreeMarker;
    FVisibility: TFreeVisibility;
    FOnFileChanged: TNotifyEvent;
    FOnUpdateUndoData: TNotifyEvent;
    FOnUpdateRecentFileList: TNotifyEvent;
    FOnChangeCursorIncrement: TNotifyEvent;
    FOnUpdateGeometryInfo: TNotifyEvent;
    // This event is raised whenever items are added or deleted from the surface
    FFreeLinesplanFrme: TFrame;
    FFilenameSet: boolean; // Flag to determine if the filename already has been set
    FModelIsLoaded: boolean; // Flag to determine if the model is created new or loaded.
    FFileIsReadOnly: boolean;
    // The folowing private variables are for moving controlpoints with the mouse
    FCurrentlyMoving: boolean;
    FPointHasBeenMoved: boolean;
    FStopAskingForFileVersion: boolean;
    FPrevCursorPosition: TPoint;
    FControlpointForm: TFreeControlPointForm;
    // form for manual adjustment of controlpoints
    FIntersectionDialog: TForm;
    // Dialog containing intersectionlines
    FProjectSettings: TFreeProjectSettings;
    FHydrostaticCalculations: TFasterListTFreeHydrostaticCalc;
    // List containing all hydrostatic calculations
    FUndoObjects: TFasterListTFreeUndoObject;
    FUndoPosition: integer;
    // Index of the current undo object
    FPreviousUndoPosition: integer;
    FPropellerTask1Data: TFreeTask1PropellerData;
    FPropellerTask2Data: TFreeTask2PropellerData;
    FPropellerTask3Data: TFreeTask3PropellerData;
    FPropellerTask4Data: TFreeTask4PropellerData;
    FPropellerTask5Data: TFreeTask5PropellerData;
    FPropellerRvrsData: TFreeRvrsPropellerData;

    FHydrodynManeuvData: TFreeHydrodynManeuvData;

    FHydrodynTask1Data: TFreeHydrodynTask1Data;
    //                                    FHydrodynTask2Data         : TFreeHydrodynTask2Data;
    //                                    FHydrodynTask3Data         : TFreeHydrodynTask3Data;
    //                                    FHydrodynTask4Data         : TFreeHydrodynTask4Data;
    FResistanceDelftData: TFreeDelftSeriesResistanceData;
    FResistanceKaperData: TFreeKAPERResistanceData;
    FResistancePlaningData: TFreePlaningResistanceData;
    FResistanceHoltrData: TFreeHoltrSeriesResistanceData;
    FResistanceHollenData: TFreeHollenSeriesResistanceData;
    FResistanceOortmerData: TFreeOortmerSeriesResistanceData;
    FResistanceFungData: TFreeFungSeriesResistanceData;
    FResistanceOSTData: TFreeOSTSeriesResistanceData;
    FResistanceRBHSData: TFreeRBHSSeriesResistanceData;
    FResistanceMHData: TFreeMHSeriesResistanceData;
    FDesignHydrostatics: TFreeHydrostaticCalc;
    // This object calculates hydrostatic data to draw in the viewports
    FFontSize: integer;
    procedure FBuildValidFrameTable(
      Destination: TFasterListTFreeSpline;
      CloseAtDeck: boolean);
    // Assembles all stations and builds a 2D bodyplan for export to other calculating programs
    function FGetActiveLayer: TFreeSubdivisionlayer;
    function FGetBackgroundImage(Index: integer): TFreeBackgroundImageData;
    function FGetBuild: boolean;
    function FGetButtock(Index: integer): TFreeIntersection;
    function FGetControlCurve(Index: integer): TFreeSubdivisionControlCurve;
    function FGetDiagonal(Index: integer): TFreeIntersection;
    function FGetFlowline(Index: integer): TFreeFlowline;
    function FGetFilename: string;
    function FGetHydrostaticCalculation(Index: integer): TFreeHydrostaticCalc;
    function FGetNumberOfLayers: integer;
    function FGetLayer(Index: integer): TFreeSubdivisionLayer;
    function FGetMarker(Index: integer): TFreeMarker;
    function FGetNumberofBackgroundImages: integer;
    function FGetNumberOfButtocks: integer;
    function FGetNumberOfControlCurves: integer;
    function FGetNumberOfDiagonals: integer;
    function FGetNumberOfFlowLines: integer;
    function FGetNumberOfHydrostaticCalculations: integer;
    function FGetNumberOfLockedPoints: integer;
    function FGetNumberOfMarkers: integer;
    function FGetNumberOfStations: integer;
    function FGetNumberOfViewports: integer;
    function FGetNumberOfWaterlines: integer;
    function FGetOnChangeActiveLayer: TChangeActiveLayerEvent;
    function FGetOnChangeLayerData: TNotifyEvent;
    function FGetOnSelectItem: TNotifyEvent;
    function FGetSelectedControlPoint(Index: integer): TFreeSubdivisionControlPoint;
    function FGetSelectedControlPointGroup(Index: integer): TFreeSubdivisionControlPointGroup;
    function FGetSelectedControlEdge(Index: integer): TFreeSubdivisionControlEdge;
    function FGetSelectedControlCurve(Index: integer): TFreeSubdivisionControlCurve;
    function FGetSelectedControlFace(Index: integer): TFreeSubdivisionControlFace;
    function FGetSelectedFlowline(index: integer): TFreeFlowline;
    function FGetSelectedMarker(index: integer): TFreeMarker;
    function FGetStation(Index: integer): TFreeIntersection;
    function FGetUndoCount: integer;
    function FGetUndoMemory: integer;
    function FGetUndoObject(Index: integer): TFreeUndoObject;
    function FGetViewport(Index: integer): TFreeViewport;
    function FGetWaterline(Index: integer): TFreeIntersection;
    procedure FSetActiveControlPoint(Val: TFreeSubdivisionControlPoint);
    procedure FSetActiveLayer(Val: TFreeSubdivisionLayer);
    procedure FSetBuild(Val: boolean);
    procedure FSetEditMode(Val: TFreeEditMode);
    procedure FSetFileChanged(Val: boolean);
    procedure FSetFileName(Val: string);
    procedure FSetFileVersion(Val: TFreeFileVersion);
    function FGetNumberOfSelectedControlCurves: integer;
    function FGetNumberOfSelectedControlEdges: integer;
    function FGetNumberOfSelectedControlFaces: integer;
    function FGetNumberOfSelectedControlPoints: integer;
    function FGetNumberOfSelectedControlPointGroups: integer;
    function FGetNumberOfselectedFlowlines: integer;
    function FGetNumberOfSelectedLockedPoints: integer;
    function FGetNumberOfselectedMarkers: integer;
    procedure FSetOnChangeActiveLayer(val: TChangeActiveLayerEvent);
    procedure FSetOnChangeLayerData(Val: TNotifyEvent);
    procedure FSetOnSelectItem(Val: TNotifyEvent);
    procedure FSetPrecision(Val: TFreePrecisionType);
    function FGetPreview: TJPEGImage;
  protected   { Protected declarations }
    procedure ViewportRequestExtents(Sender: TObject; var Min, Max: T3DCoordinate);
  public      { Public declarations }
    function GetFocusedViewport: TFreeViewport;
    procedure AddViewport(Viewport: TFreeViewport);
    // Add a viewport to the list of viewports connected to the model
    function AdjustMarkers: boolean;
    procedure Clear;
    procedure ClearUndo;
    constructor Create(AOwner: TComponent);
      override;
    procedure CreateOutputHeader(CalcHeader: string; Strings: TStrings);
    // Creates a header with all relevant project data
    procedure DeleteViewport(Viewport: TFreeViewport);
    // Delete a viewport from the list of viewports connected to the model
    destructor Destroy;
      override;
    procedure Draw;
    procedure ZoomFitAllViewports;
    procedure DrawToViewport(Viewport: TFreeViewport);
    procedure Extents(var Min, Max: T3DCoordinate);
    // calculate the bounding box coordinates of the model
    function FindLowestHydrostaticsPoint: TFloatType;
    function FindByName(aName:String):TFreeNamedObject;
    function GetAllNamedPoints:TStringList;
    procedure ImportChines(Np: integer; Chines: TFasterListTFreeSpline);
    // imports a number of longitudinally lines and creates developable surfaces between each two subsequent chines
    procedure LoadProject(Source: TFreeFileBuffer);
    procedure LoadControlPointGroups(Source:TFreeFileBuffer);
    procedure LoadControlPointNames(Source:TFreeFileBuffer);
    procedure LoadControlEdgeNames(Source:TFreeFileBuffer);
    procedure LoadControlFaceNames(Source:TFreeFileBuffer);
    procedure LoadControlCurveNames(Source:TFreeFileBuffer);
    procedure LoadControlPointLinearConstraints(Source:TFreeFileBuffer);
    procedure LoadPreview(Filename: string; Image: TJPegImage); // loads the preview image from a file
    procedure RebuildModel; // Force to rebuild the entire ship and recalculate all data
    procedure Redraw;  // Redraws the model on all viewports
    procedure SaveProject(Destination: TFreeFileBuffer);
    procedure SaveControlPointGroups(Destination:TFreeFileBuffer);
    procedure SaveControlPointNames(Destination:TFreeFileBuffer);
    procedure SaveControlEdgeNames(Destination:TFreeFileBuffer);
    procedure SaveControlFaceNames(Destination:TFreeFileBuffer);
    procedure SaveControlCurveNames(Destination:TFreeFileBuffer);
    procedure SaveControlPointLinearConstraints(Destination:TFreeFileBuffer);
    procedure SavePart(Faces: TFasterListTFreeSubdivisionControlFace);
    procedure SelectPointsInFrame(Viewport: TfreeViewport; rect:TRect);
    procedure SubmergedHullExtents(Wlplane: T3DPlane; var Min, Max: T3DCoordinate);
    procedure KeyUp(Viewport: TfreeViewport; var Key: word; Shift: TShiftState);
    procedure MouseDown(Viewport: TFreeViewport; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer; var ItemSelected: boolean);
    procedure MouseMove(Viewport: TFreeViewport; Shift: TShiftState; X, Y: integer);
    procedure AdjustRelatedPointsToLinearConstraint(
              Viewport: TFreeViewport; Point: TFreeSubdivisionControlPoint);
    procedure MouseUp(Viewport: TFreeViewport; Shift: TShiftState; X, Y: integer);

    property MainForm: TForm read FMainForm write FMainForm;
    property ActiveControlPoint: TFreeSubdivisionControlPoint
      read FActiveControlPoint write FSetActiveControlPoint;
    property ActiveLayer: TFreeSubdivisionLayer
      read FGetActiveLayer write FSetActiveLayer;
    property BackgroundImage[index: integer]: TFreeBackgroundImageData
      read FGetBackgroundImage;
    property Build: boolean read FGetBuild write FSetBuild;
    property Buttock[index: integer]: TFreeIntersection read FGetButtock;
    property ControlCurve[index: integer]: TFreeSubdivisionControlCurve
      read FGetControlCurve;
    property ControlpointForm: TFreeControlPointForm read FControlpointForm;
    // Pointer to form for manual adjustment of controlpoints
    property Diagonal[index: integer]: TFreeIntersection read FGetDiagonal;
    property Edit: TFreeEdit read FEdit;
    // Containerclass for all editing commands
    property EditMode: TFreeEditMode read FEditMode write FSetEditMode;
    property FilenameSet: boolean read FFilenameSet write FFilenameSet;
    property ModelIsLoaded: boolean read FModelIsLoaded write FModelIsLoaded;
    property FileIsReadOnly: boolean read FFileIsReadOnly write FFileIsReadOnly;
    property Flowline[index: integer]: TFreeFlowline read FGetFlowline;
    property HydrostaticCalculation[index: integer]: TFreeHydrostaticCalc
      read FGetHydrostaticCalculation;
    property Layer[index: integer]: TFreeSubdivisionLayer read FGetLayer;
    property Marker[index: integer]: TFreeMarker read FGetMarker;
    property NumberofBackgroundImages: integer read FGetNumberofBackgroundImages;
    property NumberofButtocks: integer read FGetNumberOfButtocks;
    property NumberOfControlCurves: integer read FGetNumberOfControlCurves;
    property NumberofDiagonals: integer read FGetNumberOfDiagonals;
    property NumberOfHydrostaticCalculations: integer
      read FGetNumberOfHydrostaticCalculations;
    property NumberOfLayers: integer read FGetNumberOfLayers;
    property NumberOfLockedPoints: integer read FGetNumberOfLockedPoints;
    property NumberofMarkers: integer read FGetNumberOfMarkers;
    property NumberOfFlowLines: integer read FGetNumberOfFlowLines;
    property NumberOfSelectedControlCurves: integer
      read FGetNumberOfSelectedControlCurves;
    property NumberOfSelectedControlEdges: integer
      read FGetNumberOfSelectedControlEdges;
    property NumberOfSelectedControlFaces: integer
      read FGetNumberOfSelectedControlFaces;
    property NumberOfSelectedControlPoints: integer
      read FGetNumberOfSelectedControlPoints;
    property NumberOfSelectedControlPointGroups: integer
      read FGetNumberOfSelectedControlPointGroups;
    property NumberOfselectedFlowlines: integer read FGetNumberOfselectedFlowlines;
    property NumberOfSelectedLockedPoints: integer
      read FGetNumberOfSelectedLockedPoints;
    property NumberOfselectedMarkers: integer read FGetNumberOfselectedMarkers;
    property NumberofStations: integer read FGetNumberOfStations;
    property NumberOfViewports: integer read FGetNumberOfViewports;
    property NumberofWaterlines: integer read FGetNumberOfWaterlines;
    property OnChangeActiveLayer: TChangeActiveLayerEvent
      read FGetOnChangeActiveLayer write FSetOnChangeActiveLayer;
    property OnChangeLayerData: TNotifyEvent
      read FGetOnChangeLayerData write FSetOnChangeLayerData;
    property OnSelectItem: TNotifyEvent read FGetOnSelectItem write FSetOnSelectItem;
    property SelectedControlCurve[index: integer]: TFreeSubdivisionControlCurve
      read FGetSelectedControlCurve;
    property SelectedControlPoint[index: integer]: TFreeSubdivisionControlPoint
      read FGetSelectedControlPoint;
    property SelectedControlPointGroup[index: integer]: TFreeSubdivisionControlPointGroup
      read FGetSelectedControlPointGroup;
    property SelectedControlEdge[index: integer]: TFreeSubdivisionControlEdge
      read FGetSelectedControlEdge;
    property SelectedControlFace[index: integer]: TFreeSubdivisionControlFace
      read FGetSelectedControlFace;
    property SelectedFlowline[index: integer]: TFreeFlowline read FGetSelectedFlowline;
    property SelectedMarker[index: integer]: TFreeMarker read FGetSelectedMarker;
    property Station[index: integer]: TFreeIntersection read FGetStation;
    property StopAskingForFileVersion: boolean
      read FStopAskingForFileVersion write FStopAskingForFileVersion;
    property UndoCount: integer read FGetUndoCount;
    property UndoMemory: integer read FGetUndoMemory;
    // amount of memory used by all undoobjects
    property UndoObject[index: integer]: TFreeUndoObject read FGetUndoObject;
    property UndoPosition: integer read FUndoPosition;
    property Viewport[index: integer]: TFreeViewport read FGetViewport;
    property Waterline[index: integer]: TFreeIntersection read FGetWaterline;
    property Surface: TFreeSubdivisionSurface read FSurface;
  published   { Published declarations }
    property FileChanged: boolean read FFileChanged write FSetFileChanged;
    property Filename: string read FGetFilename write FSetFileName;
    property FileVersion: TFreeFileVersion read FFileVersion write FSetFileVersion;
    property LinesplanFrame: TFrame read FFreeLinesplanFrme write FFreeLinesplanFrme;
    property OnChangeCursorIncrement: TNotifyEvent
      read FOnChangeCursorIncrement write FOnChangeCursorIncrement;
    property OnFileChanged: TNotifyEvent read FOnFileChanged write FOnFileChanged;
    property OnUpdateGeometryInfo: TNotifyEvent
      read FOnUpdateGeometryInfo write FOnUpdateGeometryInfo;
    property OnUpdateRecentFileList: TNotifyEvent
      read FOnUpdateRecentFileList write FOnUpdateRecentFileList;
    property OnUpdateUndoData: TNotifyEvent read FOnUpdateUndoData
      write FOnUpdateUndoData;
    property Precision: TFreePrecisionType read FPrecision write FSetPrecision;
    property Preferences: TFreePreferences read FPreferences;
    property ProjectSettings: TFreeProjectSettings read FProjectSettings;
    property Visibility: TFreeVisibility read FVisibility;
    property FontSize: integer read FFontSize write FFontSize;
  end;
// function to find the corresponding water viscosity based on the density
function FindWaterViscosity(Temper: TFloatType; Units: TFreeUnitType): TFloatType;
procedure INEXTR(XX: single; N: integer; Xs, Ws: array of single; var YY: single);
procedure SFINEX1(N: integer; X, Y: array of single; X0: single; var YY: single);

procedure Register;

var
  GlobalFreeShip: TFreeShip;

implementation

uses Math,
  FreeIGESUnit,
  FreeHydrostaticsDlg,
  FreeIntersectionDlg,
  FreeNewModelDlg,
  FreeExtrudeDlg,
  FreeProjectSettingsDlg,
  FreeRotateDlg,
  FreeRotateDlgM,
  FreeHydrostaticsFrm,
  FreePreferencesDlg,
  FreeExpanedPlatesDlg,
  FreeLinesplanFrm,
  FreeLinesplanFrme,
  FreeInsertPlaneDlg,
  FreeMichletOutputDlg,
  FreeAddMassOutputDlg,
  FreeResistance_KaperDlg,
  FreeResistance_DelftDlg,
  FreeResistance_HoltrDlg,
  FreeResistance_HollenDlg,
  FreeResistance_OortmerDlg,
  FreeResistance_FungLeibDlg,
  FreeResistance_OSTDlg,
  FreeResistance_RBHSDlg,
  FreeResistance_MHDlg,
  FreeResistance_PlaningDlg,
  FreePropeller_Task1Dlg,
  FreePropeller_Task2Dlg,
  FreePropeller_Task3Dlg,
  FreePropeller_Task4Dlg,
  FreePropeller_Task5Dlg,
  FreeHydrodyn_RVRSDlg,
  FreeHydrodyn_ManeuvDlg,
  FreeHydrodyn_Task1Dlg,
  //     FreeHydrodyn_Task2Dlg,
  //     FreeHydrodyn_Task3Dlg,
  //     FreeHydrodyn_Task4Dlg,
  FreeSelectLayersDlg,
  FreeMirrorPlaneDlg,
  Free2DDXFExportDlg,
  FreeLackenbyDlg,
  FreeIntersectLayerDlg,
  FreeUndoHistoryDlg,
  FreeCylinderDlg,
  FreeCrosscurvesDlg,
  FreeLayerDlg,
  EnterThemeNameDlg,
  Main,
  freehullformwindow_panel;

{$I FreeShipUnit_Functions.inc}
{$I FreeUndoObject.inc}
{$I FreeBackgroundImageData.inc}
{$I FreeHydrostaticCalc.inc}
{$I FreeIntersection.inc}
{$I FreeMarker.inc}
{$I FreeFlowline.inc}
{$I FreeVisibility.inc}
{$I FreeEdit.inc}
{$I FreePreferences.inc}
{$I FreeProjectSettings.inc}
{$I FreeShip.inc}

procedure Register;
begin
  RegisterComponents('FreeShip', [TFreeShip]);
end;{Register}

end.
