{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2006-2011, by Timoshenko V.F.                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : www.freeship-plus.pisem.su                                     }

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

unit FreeGeometry;

{$IFDEF FPC}
  {$MODE Delphi} {$H+}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  JPeg, Windows,
{$ELSE}
  LCLIntf, LCLType, LCLProc, LMessages,
{$ENDIF}
  FasterList,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  GraphType,
  IntfGraphics,
  Controls,
  Forms,
  Math,
  Dialogs,
  Buttons,
  StdCtrls,
  Printers,
  StrUtils,
  FreeTypes,
  FreeVersionUnit,
  FreeFileBuffer,
  ExtCtrls,
  ExtDlgs,
{$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
{$ELSE}
  FileUtil, //deprecated
{$ENDIF}
  FreeBitmapFormatHelper;

const
  Foot = 0.3048;
  Lbs = 0.44642857;
  WeightConversionFactor = (1000 / Lbs) / ((1 / Foot) * (1 / Foot) * (1 / Foot));
  IncrementSize = 25;
  // amount of points which is automaticly allocated extra memory for
  Decimals = 4;
  // When weilding points together this is the accuracy for comparing points
  PixelCountMax = 32768;
  // used for faster pixel acces when shading to viewport
  ZBufferScaleFactor = 1.004;
  // Offset for hidden-line drawing when drawing ontop of shaded triangles
  Zoomfactor = 1.02;

  DXFLayerColors: array[1..255] of TColor =
    ($0000FF, $00FFFF, $00FF00, $FFFF00, $FF0000, $FF00FF, $000000,
    $808080, $C0C0C0, $0000FF,
    $7F7FFF,
    $0000A5, $5252A5, $00007F, $3F3F7F, $00004C, $26264C, $000026, $131326, $003FFF,
    $7F9FFF,
    $0029A5, $5267A5, $001F7F, $3F4F7F, $00134C, $262F4C, $000926, $131726, $007FFF,
    $7FBFFF,
    $0052A5, $527CA5, $003F7F, $3F5F7F, $00264C, $26394C, $001326, $131C26, $00BFFF,
    $7FDFFF,
    $007CA5, $5291A5, $005F7F, $3F6F7F, $00394C, $26424C, $001C26, $132126, $00FFFF,
    $7FFFFF,
    $00A5A5, $52A5A5, $007F7F, $3F7F7F, $004C4C, $264C4C, $002626, $132626, $00FFBF,
    $7FFFDF,
    $00A57C, $52A591, $007F5F, $3F7F6F, $004C39, $264C42, $00261C, $132621, $00FF7F,
    $7FFFBF,
    $00A552, $52A57C, $007F3F, $3F7F5F, $004C26, $264C39, $002613, $13261C, $00FF3F,
    $7FFF9F,
    $00A529, $52A567, $007F1F, $3F7F4F, $004C13, $264C2F, $002609, $132617, $00FF00,
    $7FFF7F,
    $00A500, $52A552, $007F00, $3F7F3F, $004C00, $264C26, $002600, $132613, $3FFF00,
    $9FFF7F,
    $29A500, $67A552, $1F7F00, $4F7F3F, $134C00, $2F4C26, $092600, $172613, $7FFF00,
    $BFFF7F,
    $52A500, $7CA552, $3F7F00, $5F7F3F, $264C00, $394C26, $132600, $1C2613, $BFFF00,
    $DFFF7F,
    $7CA500, $91A552, $5F7F00, $6F7F3F, $394C00, $424C26, $1C2600, $212613, $FFFF00,
    $FFFF7F,
    $A5A500, $A5A552, $7F7F00, $7F7F3F, $4C4C00, $4C4C26, $262600, $262613, $FFBF00,
    $FFDF7F,
    $A57C00, $A59152, $7F5F00, $7F6F3F, $4C3900, $4C4226, $261C00, $262113, $FF7F00,
    $FFBF7F,
    $A55200, $A57C52, $7F3F00, $7F5F3F, $4C2600, $4C3926, $261300, $261C13, $FF3F00,
    $FF9F7F,
    $A52900, $A56752, $7F1F00, $7F4F3F, $4C1300, $4C2F26, $260900, $261713, $FF0000,
    $FF7F7F,
    $A50000, $A55252, $7F0000, $7F3F3F, $4C0000, $4C2626, $260000, $261313, $FF003F,
    $FF7F9F,
    $A50029, $A55267, $7F001F, $7F3F4F, $4C0013, $4C262F, $260009, $261317, $FF007F,
    $FF7FBF,
    $A50052, $A5527C, $7F003F, $7F3F5F, $4C0026, $4C2639, $260013, $26131C, $FF00BF,
    $FF7FDF,
    $A5007C, $A55291, $7F005F, $7F3F6F, $4C0039, $4C2642, $26001C, $261321, $FF00FF,
    $FF7FFF,
    $A500A5, $A552A5, $7F007F, $7F3F7F, $4C004C, $4C264C, $260026, $261326, $BF00FF,
    $DF7FFF,
    $7C00A5, $9152A5, $5F007F, $6F3F7F, $39004C, $42264C, $1C0026, $211326, $7F00FF,
    $BF7FFF,
    $5200A5, $7C52A5, $3F007F, $5F3F7F, $26004C, $39264C, $130026, $1C1326, $3F00FF,
    $9F7FFF,
    $2900A5, $6752A5, $1F007F, $4F3F7F, $13004C, $2F264C, $090026, $171326, $000000,
    $2D2D2D,
    $5B5B5B, $898989, $B7B7B7, $B3B3B3);



type

  TShadePoint =
    record                         // Used for drawing to the Z-buffer
    X, Y: integer;
    Z: TFloatType;
    R, G, B: integer;
  end;

  TLayerProperties = record
    SurfaceArea: TFloatType;
    Weight: TFloatType;
    SurfaceCenterOfGravity: T3DCoordinate;
  end;
  TFreeVertexType = (svRegular, svCrease, svDart, svCorner);
  // Different types of subdivisionvertices
  TFreeCameraType =
    (ftWide, ftStandard, ftShortTele, ftMediumTele, ftFarTele);
  // Different types of camera lenses, corresponding to focalpoints 20mm, 50mm, 90mm, 130mm, 200mm
  TFreeViewType = (fvBodyplan, fvProfile, fvPlan, fvPerspective);
  TFreeUnitType = (fuMetric, fuImperial);
  // Switch between metric and imperial units
  TFreeViewportMode =
    (vmWireFrame, vmShade, vmShadeGauss, vmShadeDevelopable, vmShadeZebra);
  TFreeSubdivisionMode = (fmQuadTriangle, fmCatmullClark);
  TFreeAssembleMode = (amRegular, amNURBS);
  TFreeViewportBackgroundMode =
    (emNormal, emSetOrigin, emSetScale, emSetTransparentColor);

  TFreeLight = record
    Position: T3DCoordinate;
    // position of light in world
    Luminance: byte;           // brightness
    Ambient: byte
  end;

type
  TFreeSubdivisionBase = class;
  TFreeSubdivisionSurface = class;
  TFreesubdivisionPoint = class;
  TFreeSubdivisionEdge = class;
  TFreeSubdivisionFace = class;
  TFreeSubdivisionControlPoint = class;
  TFreeSubdivisionControlPointGroup = class;
  TFreeSubdivisionControlEdge = class;
  TFreeSubdivisionControlFace = class;
  TFreeSubdivisionControlCurve = class;
  TFreeSubdivisionLayer = class;
  TFreeViewport = class;
  TFreeSpline = class;
  TFreeBackgroundImage = class;
  TFreeDevelopedPatch = class;

  {   Reference integrity diagram

  ControlPoint 1---* ControlEdge
  ControlPoint 1---* ControlFace
  ControlEdge  1---2  ControlPoint (StartPoint, EndPoint)
  ControlEdge  1---+  ControlFace
  ControlFace  1---2+ ControlPoint (usually 4, sometimes 3)

  Point 1---* Edge
  Point 1---* Face
  Edge  1---2  Point (StartPoint, EndPoint)
  Edge  1---+  Face
  Face  1---2+ Point

  }

  {---------------------------------------------------------------------------------------------------}
  {                                           FasterList specializations                                     }
  {---------------------------------------------------------------------------------------------------}

  TFasterListTFreeSpline = TFasterList<TFreeSpline>;

  TFasterListTFreeSubdivisionPoint = TFasterList<TFreeSubdivisionPoint>;
  TFasterListTFreeSubdivisionEdge = TFasterList<TFreeSubdivisionEdge>;
  TFasterListTFreeSubdivisionFace = TFasterList<TFreeSubdivisionFace>;
  //TFasterListTFreeSubdivisionCurve = TFasterList<TFreeSubdivisionCurve>;

  TFasterListTFreeSubdivisionControlPoint = TFasterList<TFreeSubdivisionControlPoint>;
  TFasterListTFreeSubdivisionControlPointGroup = TFasterList<TFreeSubdivisionControlPointGroup>;
  TFasterListTFreeSubdivisionControlEdge = TFasterList<TFreeSubdivisionControlEdge>;
  TFasterListTFreeSubdivisionControlFace = TFasterList<TFreeSubdivisionControlFace>;
  TFasterListTFreeSubdivisionControlCurve = TFasterList<TFreeSubdivisionControlCurve>;

  TFasterListTFreeSubdivisionLayer = TFasterList<TFreeSubdivisionLayer>;
  TFasterListTFreeDevelopedPatch = TFasterList<TFreeDevelopedPatch>;
  {---------------------------------------------------------------------------------------------------}


  TFreeFaceGrid = record
    Faces: array of array of
    TFreeSubdivisionControlFace;
    NCols: integer;
    NRows: integer
  end;
  TFreeFaceArray = array of TFreeFaceGrid;
  TFreeSubdivisionGrid = array of array of TFreeSubdivisionPoint;
  TFreeZBufferRow = TFloatArray;
  TFreeCoordinateArray = array of T3DCoordinate;
  TFreeCoordinateGrid = array of array of T3DCoordinate;

  TFreeIntersectionData = record // intersections of a spline with a plane
    NumberOfIntersections: integer;
    Points: TFreeCoordinateArray;
    Parameters: TFloatArray;
  end;

  TUnrolledPoint =
    record // points with extra information, used for unrolling plates
    Coordinate: T2DCoordinate;
  end;
  TOnRequestExtentsEvent = procedure(Sender: TObject;
    var Min, Max: T3DCoordinate) of object;
  // Event from TFreeviewport, which is raised when the viewport initializes and needs the
  // bounding box of the min/max coordinates of the 3D model
  TChangeActiveLayerEvent = procedure(Sender: TObject;
    Layer: TFreeSubdivisionLayer) of object;
  // Event raised when the active lyers has been changed

  TAlphaBlendData = record
    R, G, B: byte;
    Alpha: byte;
    zvalue: single;
  end;

  TAlphaBlendPixelArray = record
    Number: byte;
    Capacity: byte;
    Data: array of TAlphaBlendData;
  end;

  TAlphaBlendArray = record
    First, Last: integer;
    Pixels: array of TAlphaBlendPixelArray;
  end;


  TProgressEvent = procedure(Sender: TObject; current:integer; total:integer) of object;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeAlphaBuffer                                        }

  { Alpha-buffer class used in the shading algorithm                                                  }
  {---------------------------------------------------------------------------------------------------}
  TFreeAlphaBuffer = class
  private
    FViewport: TFreeViewport;
    FBuffer: array of TAlphaBlendArray;
    FWidth: integer;
    FHeight: integer;
    FFirstRow: integer;
    FLastRow: integer;
  public
    procedure AddPixelData(X, Y: integer;
      R, G, B, Alpha: byte; Z: single);
    procedure Initialize;
    procedure Draw;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeZBuffer                                            }

  { Z-buffer class used in the shading algorithm                                                      }
  {---------------------------------------------------------------------------------------------------}
  TFreeZBuffer = class
  private
    FViewport: TFreeViewport;
    FBuffer: array of TFreeZBufferRow;
    FWidth: integer;
    FHeight: integer;
  public
    procedure Initialize;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeBackgroundImage                                    }

  { Background image properties for use in a viewport                                                 }

  {---------------------------------------------------------------------------------------------------}
  TFreeBackgroundImage = class(TPersistent)
  private
    FOwner: TFreeViewport;
    FBitmap: TBitmap;
    FOrigin: TPoint;
    FScale: TFloatType;
    FTransparent: boolean;
    FTransparentColor: TColor;
    FVisible: boolean;
    FShowInView: TFreeViewType;
    FQuality: byte;
    FAlpha: byte;
    FTolerance: byte;
    FCachedBmp: TBitmap;
    procedure FSetAlpha(val: byte);
    procedure FSetOrigin(val: TPoint);
    procedure FSetTolerance(val: byte);
    procedure FSetTransparent(val: boolean);
    procedure FSetTransparentColor(val: TColor);
    procedure FSetVisible(val: boolean);
  public
    procedure AssignData(Image: TGraphic;
      View: TFreeViewType; Origin: TPoint; Scale: TFloatType; Transp: boolean;
      TranspCol: TColor; Alpha, Quality, Tolerance: byte; Quiet: boolean);
    procedure Clear;
    constructor Create(Viewport: TFreeViewport);
    destructor Destroy; override;
    procedure Invalidate;
    procedure Draw;
    function ImageCoordinate(X, Y: integer): TPoint;
    function TargetRect: TRect;
    procedure Open(InitialDir: string);
    procedure Save;
    procedure SetBlendingValue;
    procedure SetToleranceValue;
    property Origin: TPoint
      read FOrigin write FSetOrigin;
  published
    property Alpha: byte
      read FAlpha write FSetAlpha;
    property Bitmap: TBitmap
      read FBitmap write FBitmap;
    property Owner: TFreeViewport
      read FOwner write FOwner;
    property Quality: byte
      read FQuality write FQuality;
    property Scale: TFloatType
      read FScale write FScale;
    property ShowInView: TFreeViewType
      read FShowInView write FShowInView;
    property Tolerance: byte
      read FTolerance write FSetTolerance;
    property Transparent: boolean
      read FTransparent write FSetTransparent;
    property TransparentColor: TColor
      read FTransparentColor write FSetTransparentColor;
    property Visible: boolean
      read FVisible write FSetVisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeViewport                                           }

  { This is a 3D drawingcanvas used for viewing and drawing                                           }
  { It also is the userinterface for editing the hullform.                                            }
  {---------------------------------------------------------------------------------------------------}
  TFreeViewport = class(TCustomPanel)
  private
    FAngle: TFloatType;
    FDistance: TFloatType;
    // The distance from the model to the camera, determined by the field of view
    FElevation: TFloatType;
    FFieldOfView: TFloatType;
    // The field of view in degrees, default=50 degr. which corresponds with the human eye
    FDoubleBuffer: boolean;
    // Double buffering prevents flickering when redrawing the viewport
    FPrinting: boolean;
    // Switch to determine if the viewport is drawing to the screen, or to the printer (or bitmap)
    FPrintResolution: integer;
    // horizontal reolution of the printer
    FDestinationWidth: integer;
    // Destinationwidth of the canvas when not drawing to the screen
    FDestinationHeight: integer;
    // DestinationHeight of the canvas when not drawing to the screen
    FMin3D, FMax3D: T3DCoordinate;
    FMidPoint: T3DCoordinate;
    // Midpoint of the boundarybox determined by FMin3D and FMax3D. This point is used as centerpoint for rotating the 3D model
    FMargin: TFloatType;
    // margin around to viewport to keep clear;
    // and it also is the direction at which the camera looks
    FBackgroundMode: TFreeViewportBackgroundMode;
    FViewType: TFreeViewType;
    // Switch to sideview, frontview, topview or perspective view
    FCameraLocation: T3DCoordinate;
    // Position of the camera, following from the field of view and the distance of the camera
    FCameraType: TFreeCameraType;
    // Determines the focalpoint of the camera
    FCosAngle, FSinAngle: TFloatType;
    // Pre calculated values to speed-up the rotating of point in the perspective-projection
    FCosElevation, FSinElevation: TFloatType;
    // Pre calculated values to speed-up the rotating of point in the perspective-projection
    FScale: TFloatType;
    // Scale for projecting the 2D coordinates to the viewport
    FPrintScaleFactor: TFloatType;
    // Scale factor to adapt penwith depending on printsize and printresolution
    FZoom: TFloatType;
    FViewportMode: TFreeViewportmode;
    // Switch between wireframe mode or differentypes of shading
    FDrawingCanvas: TCanvas;
    FDrawingBuffer: TBitmap;
    // Drawingbuffer to prevent flickering. Everything is drawn on this bitmap, and then copied to the screen
    FBitmapFormatHelper: TFreeBitmapFormatHelper;
    FOnChangeBackgroundImage: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnRedraw: TNotifyEvent;
    FOnChangeViewType: TNotifyEvent;
    FOnRequestBackgroundImage: TNotifyEvent;
    FOnRequestExtents: TOnRequestExtentsEvent;
    FScreencenter: TPoint;
    FPan: TPoint;
    FPreviousPosition: TPoint;
    FBackgroundOrigin: TPoint;
    FBackgroundImage: TFreeBackgroundImage;
    // shade data
    FZBuffer: TFreeZBuffer;
    FAlphaBuffer: TFreeAlphaBuffer;
    FLight: TFreeLight;
    FHorScrollbar: TScrollBar;
    FVertScrollbar: TScrollBar;

    FSelectionFrameRect: TRect;
    FSelectionFrameActive: boolean;

    FUpdating: boolean;

    FLastResizeWidth:integer;
    FLastResizeHeight:integer;
    FLastResizeClientWidth:integer;
    FLastResizeClientHeight:integer;

    function FGetBrushColor: TColor;
    function FGetBrushStyle: TBrushStyle;
    function FGetFontColor: TColor;
    function FGetFontName: string;
    function FGetFontSize: integer;
    function FGetFontHeight: integer;
    function FGetPenColor: TColor;
    function FGetPenStyle: TPenStyle;
    function FGetPenWidth: integer;
    function FGetPrinting: boolean;
    procedure FSetAngle(Val: TFloatType);
    procedure FSetBackgroundMode(val: TFreeViewportBackgroundMode);
    procedure FSetBrushColor(Val: TColor);
    procedure FSetBrushStyle(Val: TBrushStyle);
    procedure FSetCameraType(Val: TFreeCameraType);
    procedure FSetElevation(Val: TFloatType);
    procedure FSetLight(val: TFreeLight);
    procedure FSetFontColor(Val: TColor);
    procedure FSetFontName(val: string);
    procedure FSetFontSize(val: integer);
    procedure FSetFontHeight(val: integer);
    procedure FSetHorScrollbar(val: TScrollbar);
    procedure FSetVertScrollbar(val: TScrollbar);
    procedure FSetMargin(Val: TFloatType);
    procedure FSetPan(Val: TPoint);
    procedure FSetPenColor(Val: TColor);
    procedure FSetPenStyle(Val: TPenStyle);
    procedure FSetPenWidth(Val: integer);
    function FGetPrintScaleFactor: TFloatType;
    procedure SetSelectionFrameRect(Rect: TRect);
    procedure SetSelectionFrameActive(Val: boolean);
    procedure FSetViewType(Val: TFreeViewType);
    procedure FSetViewportMode(Val: TFreeViewportMode);
    procedure FHorScrollbarChange(Sender: TObject);
    procedure FVertScrollbarChange(Sender: TObject);
    procedure WMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure KeyPress(var Key: char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;
    procedure GetPixel(X, Y: integer; out R, G, B: byte);
    procedure SetPixel(X, Y: integer; R, G, B: byte);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DetachEventHandlers;
    procedure DrawLineToZBuffer(Point1, Point2: T3DCoordinate; R, G, B: byte);
      virtual;
    procedure InitializeViewport(Min, Max: T3DCoordinate); virtual;
    procedure Print(Units: TFreeUnitType; AskPrintScale: boolean; Jobname: string);
      virtual;
    function Project(P: T3DCoordinate): TPoint;
    function ProjectBack(P: TPoint; Input: T3DCoordinate): T3DCoordinate;
    function ProjectBackTo2D(P: TPoint): T2DCoordinate;
    // Takes the cursor position and projects it to 2D object space
    function ProjectToZBuffer(P: T3DCoordinate): TShadePoint;
      overload; virtual;
    // Projects a 3D point to the screen and calculate it's Z-value for the Z-buffer
    function ProjectToZBuffer(Scale: TFloatType; P: T3DCoordinate): TShadePoint;
      reintroduce; overload;
    // Projects a 3D point with a certain z-buffer offset to the screen, used for drawing lines on top of shaded surfaces
    function RotatedPoint(P: T3DCoordinate): T3DCoordinate;
    function RotatedPointBack(P: T3DCoordinate): T3DCoordinate;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure LineTo(x, y: integer); virtual;
    procedure MoveTo(x, y: integer); virtual;
    procedure Line(x1, y1, x2, y2: integer); virtual;
    procedure Rectangle(x1, y1, x2, y2: integer); virtual; overload;
    procedure Rectangle(rect: TRect); virtual; overload;
    procedure Ellipse(x1, y1, x2, y2: integer); virtual;
    procedure Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
      StartX, StartY, EndX, EndY: integer); virtual;

    procedure Polyline(const Points: array of TPoint);
      virtual;
    procedure Polygon(const Points: array of TPoint);
      virtual;

    function GetDrawingBuffer:TBitmap;

    procedure SaveAsBitmap(Filename: string;
      const ShowDialog: boolean = True); virtual;

    procedure SetFocus; override;

    procedure SetPenWidth(Width: integer); virtual;
    procedure StretchDraw(DestRect: TRect; bmp: TBitmap); virtual;
    procedure ShadedColor(Dp: single; R, G, B: byte;
      var ROut, GOut, BOut: byte); virtual;
    procedure ShadeTriangle(P_1, P_2, P_3: T3DCoordinate; R, G, B: byte; Alpha: byte);
      overload; virtual;
    procedure ShadeTriangle(P_1, P_2, P_3: T3DCoordinate; C1, C2, C3: extended);
      virtual;//reintroduce;overload;
    procedure ShadeTriangle(P_1, P_2, P_3: T3DCoordinate;
      R1, G1, B1, R2, G2, B2, R3, G3, B3: byte); virtual;//reintroduce;overload;
    function TextWidth(val: string): integer; virtual;
    function TextHeight(val: string): integer; virtual;
    procedure TextOut(x, y: integer; val: string); virtual;
    procedure ZoomIn; virtual;
    procedure ZoomExtents; virtual;
    procedure ZoomOut; virtual;
    procedure DrawSelectionFrame; virtual;

    property AlphaBuffer: TFreeAlphaBuffer read FAlphaBuffer;
    property BackgroundMode: TFreeViewportBackgroundMode
      read FBackgroundMode write FSetBackgroundMode;
    property BrushColor: TColor
      read FGetBrushColor write FSetBrushColor;
    property BrushStyle: TBrushStyle
      read FGetBrushStyle write FSetBrushStyle;
    property CameraLocation: T3DCoordinate read FCameraLocation;
    property DrawingCanvas: TCanvas
      read FDrawingCanvas write FDrawingCanvas;
    property FieldOfView: TFloatType read FFieldOfView;
    property FontColor: TColor
      read FGetFontColor write FSetFontColor;
    property FontName: string
      read FGetFontname write FSetFontName;
    property FontSize: integer
      read FGetFontSize write FSetFontSize;
    property FontHeight: integer
      read FGetFontHeight write FSetFontHeight;

    property Light: TFreeLight read FLight write FSetLight;
    property Max3D: T3DCoordinate read FMax3D;
    property Min3D: T3DCoordinate read FMin3D;
    property PenColor: TColor
      read FGetPenColor write FSetPenColor;
    property PenStyle: TPenStyle
      read FGetPenStyle write FSetPenStyle;
    property PenWidth: integer
      read FGetPenWidth write FSetPenWidth;
    property Printing: boolean read FGetPrinting;
    property PrintResolution: integer read FPrintResolution;
    property PrintScaleFactor: TFloatType read FGetPrintScaleFactor;
    property Scale: TFloatType read FScale;
    property ZBuffer: TFreeZBuffer read FZBuffer;
    property Zoom: TFloatType read FZoom;
    property Pan: TPoint read FPan write FSetPan;
    property SelectionFrameRect: TRect read FSelectionFrameRect write SetSelectionFrameRect;
    property SelectionFrameActive: boolean read FSelectionFrameActive write SetSelectionFrameActive;
  published
    property Angle: TFloatType read FAngle write FSetAngle;
    property Align;
    property BackgroundImage: TFreeBackgroundImage
      read FBackgroundImage write FBackgroundImage;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property CameraType: TFreeCameraType
      read FCameraType write FSetCameraType;
    property Color;
    property DestinationWidth: integer
      read FDestinationWidth write FDestinationWidth;
    property DestinationHeight: integer
      read FDestinationHeight write FDestinationHeight;
    property DoubleBuffer: boolean
      read FDoubleBuffer write FDoubleBuffer;
    property Elevation: TFloatType
      read FElevation write FSetElevation;
    property HorScrollbar: TScrollBar
      read FHorScrollbar write FSetHorScrollbar;
    property Margin: TFloatType read FMargin write FSetMargin;
    property PopupMenu;
    property VertScrollbar: TScrollBar
      read FVertScrollbar write FSetVertScrollbar;
    property Visible;
    property ViewType: TFreeViewtype
      read FViewType write FSetViewType;
    property ViewportMode: TFreeViewportmode
      read FViewportMode write FSetViewportMode;
    // Switch between wireframe mode or differentypes of shading
    property OnChangeBackground: TNotifyEvent
      read FOnChangeBackgroundImage write FOnChangeBackgroundImage;
    property OnChangeViewType: TNotifyEvent
      read FOnChangeViewType write FOnChangeViewType;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown: TMouseEvent
      read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent
      read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent
      read FOnMouseMove write FOnMouseMove;
    property OnMouseEnter: TNotifyEvent
      read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent
      read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel;
    property OnResize;
    property OnRedraw: TNotifyEvent
      read FOnRedraw write FOnRedraw;
    property OnRequestBackgroundImage: TNotifyEvent
      read FOnRequestBackgroundImage write FOnRequestBackgroundImage;
    property OnRequestExtents: TOnRequestExtentsEvent
      read FOnRequestExtents write FOnRequestExtents;
  end;


  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeDevelopedPatch                                     }

  { Unrolled Subdivision control face                                                                 }
  {---------------------------------------------------------------------------------------------------}
  TFreeDevelopedPatch = class
  private
    FOwner: TFreeSubdivisionLayer;
    FName: string;
    FConnectedMirror: TFreeDevelopedPatch;
    FPoints: TFasterListTFreeSubdivisionPoint;
    // All original 3D points
    FEdges: TFasterListTFreeSubdivisionEdge;// All original edges
    ///FDoneList: TFasterListTFreeSubdivisionPoint;
    FDoneList: TFasterListTFreeSubdivisionFace;
    // List containing developed faces in chronological order
    FBoundaryEdges: TFasterListTFreeSubdivisionEdge;
    // All edges forming the boundary edges of the developed plate, sorted in the correct order
    FStations: TFasterListTFreeSpline;
    FWaterlines: TFasterListTFreeSpline;
    FButtocks: TFasterListTFreeSpline;
    FDiagonals: TFasterListTFreeSpline;
    FCorners: TFasterListTFreeSubdivisionPoint;
    // contains all cornerpoints for dimensioning reasons
    FRotation: TFloatType;
    FMirrorPlane: T3DPlane;
    FMirror: boolean;
    FMin2D: T2DCoordinate;
    FMax2D: T2DCoordinate;
    FTranslation: T2DCoordinate;
    FMaxAreaError: extended;
    FTotalAreaError: extended;
    FXGrid: TFloatType;
    FYGrid: TFloatType;
    FCos, FSin: TFloatType;
    F2DCoordinates: array of TUnrolledPoint;
    FEdgeErrors: array of double;
    // Visibility options
    FVisible: boolean;
    FShowSolid: boolean;
    // Fills the surface with the layer color
    FShowPartName: boolean;
    // draws the name of the surface at the center
    FShowBoundingBox: boolean;
    // Draws a boundary box around the surface
    FShowInteriorEdges: boolean;
    // Draw the interior edges (none crease edges)
    FShowStations: boolean;
    FShowButtocks: boolean;
    FShowDiagonals: boolean;
    FShowWaterlines: boolean;
    FShowErrorEdges: boolean;
    FShowDimensions: boolean;
    FMirrorOnScreen: boolean;
    FShadeSubmerged: boolean;
    FNoIterations: integer;
    FUnits: TFreeUnitType;
    FDimFontColor: TColor;
    FDimFontName: string;
    FDimFontSize: integer;

    function FGetShowErrorEdges: boolean;
    function FGetMidPoint: T2DCoordinate;
    function FGetMinError: extended;
    function FGetMaxError: extended;
    function FGetMirrorPoint(
      index: integer): T3DCoordinate;
    function FGetPoint(index: integer): T3DCoordinate;
    procedure FSetRotation(Val: TFloatType);
    procedure FSetTranslation(Val: T2DCoordinate);
    procedure FSetMirrorOnScreen(val: boolean);
  public
    procedure Assign(Org: TFreeDevelopedPatch;
      Mirror: boolean);
    procedure Clear;
    function ConvertTo3D(P: T2DCoordinate): T3DCoordinate;
    constructor Create(Owner: TFreeSubdivisionLayer);
    destructor Destroy; override;
    function DistanceToCursor(X, Y: integer;
      Viewport: TFreeViewport): integer;
    procedure Draw(Viewport: TFreeViewport);
    procedure Extents(var Min, Max: T3DCoordinate);
    procedure IntersectPlane(Plane: T3DPlane;
      Color: TColor);
    procedure SaveToDXF(Strings: TStringList);
    procedure SaveToTextFile(Strings: TStringList);
    procedure Unroll(ControlFaces: TFasterListTFreeSubdivisionControlFace);
    property MaxAreaError: extended
      read FMaxAreaError;
    property MaxError: extended
      read FGetMaxError;
    property MidPoint: T2DCoordinate
      read FGetMidPoint;
    property MinError: extended
      read FGetMinError;
    property MirrorOnScreen: boolean
      read FMirrorOnScreen write FSetMirrorOnScreen;
    property Name: string
      read FName write FName;
    property NumberOfIterations: integer
      read FNoIterations;
    property Owner:
      TFreeSubdivisionLayer read FOwner;
    property MirrorPoint[index: integer]: T3DCoordinate
      read FGetMirrorPoint;
    property Point[index: integer]: T3DCoordinate
      read FGetPoint;
    property Rotation: TFloatType
      read FRotation write FSetRotation;
    property ShadeSubmerged: boolean
      read FShadeSubmerged write FShadeSubmerged;
    property ShowBoundingBox: boolean
      read FShowBoundingBox write FShowBoundingBox;
    property ShowButtocks: boolean
      read FShowButtocks write FShowButtocks;
    property ShowDiagonals: boolean
      read FShowDiagonals write FShowDiagonals;
    property ShowDimensions: boolean
      read FShowDimensions write FShowDimensions;
    property ShowErrorEdges: boolean
      read FGetShowErrorEdges write FShowErrorEdges;
    property ShowInteriorEdges: boolean
      read FShowInteriorEdges write FShowInteriorEdges;
    property ShowPartName: boolean
      read FShowPartName write FShowPartName;
    property ShowSolid: boolean
      read FShowSolid write FShowSolid;
    property ShowStations: boolean
      read FShowStations write FShowStations;
    property ShowWaterlines: boolean
      read FShowWaterlines write FShowWaterlines;
    property DimFontColor: TColor
      read FDimFontColor write FDimFontColor;
    property DimFontName: string
      read FDimFontName write FDimFontName;
    property DimFontSize: integer
      read FDimFontSize write FDimFontSize;
    property TotalAreaError: extended
      read FTotalAreaError;
    property Translation: T2DCoordinate
      read FTranslation write FSetTranslation;
    property Units: TFreeUnitType
      read FUnits write FUnits;
    property Visible: boolean
      read FVisible write FVisible;
    property XGrid: TFloatType
      read FXGrid write FXGrid;
    property YGrid: TFloatType
      read FYGrid write FYGrid;
  end;


  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeNamedObject                                             }
  { This is the base class of all named objects in the project                                        }
  {---------------------------------------------------------------------------------------------------}
  TFreeNamedObject = class
  private
    FId:integer;
    FName:string;
    FOwner: TFreeSubdivisionSurface;
  public
    constructor Create(Owner: TFreeSubdivisionSurface); virtual;
    property Owner: TFreeSubdivisionSurface read FOwner write FOwner;
    procedure PrintDebug; virtual;
    property Id: Integer read FId;
    property Name: String read FName write FName;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeEntity                                             }

  { This is the base class of all 3D entities in the project                                          }
  {---------------------------------------------------------------------------------------------------}
  TFreeEntity = class(TFreeNamedObject)
  private
    FBuild: boolean; // Flag to check if the entity has already been built
    FIsBuilding: boolean; // Flag to check if the entity structure is building to exclude double entrance to building
    FMin, FMax: T3DCoordinate;
    // The min/max boundary coordinates of the entity after it has been build
    FPenWidth: byte;
    // Pen thickness to use when drawing
    FColor: TColor; // Color when drawing
    //FName:string;
    FPenstyle: TPenStyle;
    // Pen style for drawing the line
    function FGetMin: T3DCoordinate;
      virtual;
    function FGetMax: T3DCoordinate;
      virtual;
    procedure FSetBuild(Val: boolean);
      virtual;
  public
    constructor Create(Owner: TFreeSubdivisionSurface); override;
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure Extents(var Min, Max: T3DCoordinate); virtual;
    procedure Draw(Viewport: TFreeViewport);
      virtual;
    procedure Rebuild;
      virtual;
    property Build: boolean
      read FBuild write FSetBuild;
    property Color: TColor
      read FColor write FColor;
    property IsBuilding: boolean read FIsBuilding;
    property Min: T3DCoordinate
      read FGetMin;
    property Max: T3DCoordinate
      read FGetMax;
    property PenStyle: TPenStyle
      read FPenStyle write FPenStyle;        // Pen style
    property PenWidth: byte
      read FPenWidth write FPenWidth;             // Pen thickness when drawing on screen
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSpline                                             }
  { 3D CSpline                                                                                        }
  { Copied from page 107 of the book: "Numerical recipes in fortan 77"                                }
  { Url: http://www.library.cornell.edu/nr/bookfpdf/f3-3.pdf                                          }
  { Modified to use centripetal parametrisation for smoother interpolation and to accept              }
  { knuckles in the controlpoints                                                                     }
  {---------------------------------------------------------------------------------------------------}
  TFreeSpline = class(TFreeEntity)
    FCapacity: integer;
    // Number of points for which memory has been allocated
    FNoPoints: integer;
    // Actual number of points present
    FFragments: integer;
    // Number of straight-line segments used when drawing the curve
    FShowCurvature: boolean;
    FShowPoints: boolean;
    FCurvatureScale: TFloatType;
    // scale factor used to increase or decrease the scale of the curvature plot
    FCurvatureColor: TColor;
    // Color used for draing the curvature plot
    FTotalLength: TFloatType;
    FPoints: TFreeCoordinateArray;
    // Array containing all controlpoints
    FKnuckles: array of boolean;
    FParameters: array of TFloatType;
    FDerivatives: TFreeCoordinateArray;
    function FGetFragments: integer;
    function FGetKnuckle(Index: integer): boolean;
    function FGetParameter(Index: integer): TFloatType;
    function FGetPoint(Index: integer): T3DCoordinate;
    procedure FSetBuild(val: boolean);
      override;
    procedure FSetCapacity(Val: integer);
    procedure FSetFragments(Val: integer);
    procedure FSetKnuckle(Index: integer; Value: boolean);
    procedure FSetPoint(Index: integer; P: T3DCoordinate);

  public
    procedure Add(P: T3DCoordinate);
    procedure AddKnuckle(P: T3DCoordinate);
    // add a new point to the curve
    procedure Assign(Spline: TFreeSpline);
    // Copy all data from another spline
    function CoordLength(T1, T2: TFloatType): TFloatType;
    function ChordlengthApproximation(
      Percentage: TFloatType): extended;
    procedure Clear;
      override;
    constructor Create(Owner: TFreeSubdivisionSurface); override;
    function Curvature(Parameter: TFloatType;
      var Value, Normal: T3DCoordinate): TFloatType;
    procedure DeletePoint(Index: integer);
    function DistanceToCursor(X, Y: integer;
      Viewport: TFreeViewport): integer; virtual;
    procedure Draw(Viewport: TFreeViewport); override;
    function FirstDerive(Parameter: TFloatType): T3DCoordinate;
    procedure Insert(Index: integer; P: T3DCoordinate);
    procedure InsertSpline(Index: integer;
      Invert, DuplicatePoint: boolean; Source: TFreeSpline);
    function IntersectPlane(Plane: T3DPlane;
      var Output: TFreeIntersectionData): boolean;
    procedure InvertDirection;
    // invert the direction of the controlpoints and knuckles
    procedure LoadBinary(Source: TFreeFileBuffer);
      virtual;
    procedure Rebuild;
      override;
    procedure SaveBinary(Destination: TFreeFileBuffer);
      virtual;
    procedure SaveToDXF(Strings: TStringList;
      Layername: string; SendMirror: boolean);
    function SecondDerive(Parameter: TFloatType): T3DCoordinate;
    function Simplify(Criterium: TFloatType): boolean;
    // Remove points that do not contribute significantly to the shape
    function Value(Parameter: extended): T3DCoordinate;
    property Capacity: integer
      read FCapacity write FSetCapacity;
    property CurvatureColor: TColor
      read FCurvatureColor write FCurvatureColor;
    property CurvatureScale: TFloatType
      read FCurvatureScale write FCurvatureScale;
    property Fragments: integer
      read FGetFragments write FSetFragments;
    property Knuckle[Index: integer]: boolean
      read FGetKnuckle write FSetKnuckle;
    property NumberOfPoints: integer
      read FNoPoints;
    property Parameter[Index: integer]: TFloatType
      read FGetParameter;
    property Point[Index: integer]: T3DCoordinate
      read FGetPoint write FSetPoint;
    property ShowCurvature: boolean
      read FShowCurvature write FShowCurvature;
    property ShowPoints: boolean
      read FShowPoints write FShowPoints;
    property TotalLength: TFloatType
      read FTotalLength;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeNURBSurface                                       }
  {--------------------------------------------------------------------------------------------------}
  TFreeNURBSurface = class(TFreeEntity)
  private
    FColCount: integer;
    FRowCount: integer;
    FColCapacity: integer;
    FRowCapacity: integer;
    FColDegree: integer;
    FRowDegree: integer;
    FColKnots: TFloatArray;
    FRowKnots: TFloatArray;
    FControlPoints: TFreeCoordinateGrid;
    function FGetpoint(Col, Row: integer): T3DCoordinate;
    procedure FSetColDegree(Val: integer);
    procedure FSetRowDegree(Val: integer);
    procedure FSetPoint(Col, Row: integer;
      Val: T3DCoordinate);
    procedure FSetColCapacity(Val: integer);
    procedure FSetRowCapacity(Val: integer);
  protected
  public
    procedure Clear;
      override;
    procedure DeleteColumn(Col: integer);
    procedure DeleteRow(Row: integer);
    procedure InsertColKnot(U: TFloatType);
    procedure InsertRowKnot(V: TFloatType);
    procedure NormalizeKnotVectors;
    procedure Rebuild;
      override;
    procedure SetCapacity(Col, Row: integer);
    procedure SetDefaultColKnotvector;
    procedure SetDefaultRowKnotvector;
    procedure SetUniformColKnotvector;
    procedure SetUniformRowKnotvector;
    property ColCapacity: integer
      read FColCapacity write FSetColCapacity;
    property ColCount: integer
      read FColCount write FColCount;
    property ColDegree: integer
      read FColDegree write FSetColDegree;
    property ColKnotVector: TFloatArray
      read FColknots;
    property Point[Col, Row: integer]: T3DCoordinate
      read FGetpoint write FSetPoint;
    property RowCapacity: integer
      read FRowCapacity write FSetRowCapacity;
    property RowCount: integer
      read FRowCount write FRowCount;
    property RowDegree: integer
      read FRowDegree write FSetRowDegree;
    property RowKnotVector: TFloatArray
      read FRowknots;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionBase                                    }

  { TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
  {---------------------------------------------------------------------------------------------------}
  TFreeSubdivisionBase = class(TFreeNamedObject)
  private
    //FOwner: TFreeSubdivisionSurface;
    InUnreference:boolean;
    IsUnreferenceEnabled:boolean; // TODO - remove
    SubdivisionLevel:integer; // for investigation
  public
    constructor Create(Owner: TFreeSubdivisionSurface); override;
    //property Owner: TFreeSubdivisionSurface read FOwner write FOwner;
    procedure PrintDebug; override;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionControlCurve                            }

  { Controlcurves are curves that can be added to the controlnet an are subdivide with the surface.   }
  { The resulting curve therefore lies on the surface, and can be used in the fairing process         }
  {---------------------------------------------------------------------------------------------------}
  TFreeSubdivisionControlCurve = class(TFreeSubdivisionBase)
  private
    FVisible: boolean;
    FControlPoints: TFasterListTFreeSubdivisionControlPoint;
    FSubdividedPoints: TFasterListTFreeSubdivisionPoint;
    FCurve: TFreeSpline;
    FBuild: boolean;
    function FGetColor: TColor;
    function FGetNumberOfControlPoints: integer;
    function FGetControlPoint(
      Index: integer): TFreeSubdivisionControlPoint;
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    procedure FSetBuild(Val: boolean);
    procedure FSetSelected(val: boolean);
  public
    procedure AddPoint(P: TFreeSubdivisionControlPoint);
    function CheckIntegrity: boolean;
    procedure Clear;
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    procedure Delete;
    procedure DeleteEdge(
      Edge: TFreeSubdivisionControlEdge);
    destructor Destroy;
      override;
    function DistanceToCursor(X, Y: integer;
      Viewport: TFreeViewport): integer;
    procedure Draw(Viewport: TFreeViewport);
    procedure InsertControlPoint(
      P1, P2, New: TFreeSubdivisionControlPoint);
    procedure InsertEdgePoint(
      P1, P2, New: TFreeSubdivisionPoint);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure ReplaceVertexPoint(Old, New: TFreeSubdivisionPoint);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToDXF(Strings: TStringList);
    property Build: boolean
      read FBuild write FSetBuild;
    property Color: TColor
      read FGetColor;
    property Curve: TFreeSpline
      read FCurve;
    property NumberOfControlPoints: integer
      read FGetNumberOfControlPoints;
    property ControlPoint[index: integer]:
      TFreeSubdivisionControlPoint read FGetControlPoint;
    property Selected: boolean
      read FGetSelected write FSetSelected;
    // Property to see if this edge has been selected by the user
    property Visible: boolean
      read FGetVisible write FVisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionLayer                                   }

  { TFreeSubdivisionLayer is a layer-type class                                                       }

  { All individual controlfaces can be assigned to a layer. Properties such as color,                 }
  { visibility etc. are common for all controlfaces belonging the the same layer                      }
  {---------------------------------------------------------------------------------------------------}
  TFreeSubdivisionLayer = class
    // color, visibility, symmetric, calc intersections/part of hull
  private
    FOwner: TFreeSubdivisionSurface;
    // Pointer to the subdivisionsurface
    FLayerID: integer;
    // Unique identification number for internal references
    FColor: TColor;
    // Color of this layer
    FVisible: boolean;
    // Visibility switch
    FDescription: string;
    // Description of the layer, used as user identification
    FSymmetric: boolean;
    // Symmetric patches are mirrored in the centerplane when both halves of the ship are drawn
    FDevelopable: boolean;
    // Developable layers are shaded with Gauss curvature
    FUseForIntersections: boolean;
    // If set to true, stations, waterlines, buttocks and diagonals are calculated
    FUseInHydrostatics: boolean;
    // If set to true, the panels of this layer will be used for hydrostatic calculations
    FShowInLinesplan: boolean;
    // Flag to hide or show this layer in the linesplan
    FMaterialDensity: TFloatType;
    // Density of material used to calculate the weight of the surface
    FThickness: TFloatType;
    // Also used for weight calculation
    FPatches: TFasterListTFreeSubdivisionControlFace;
    // List containing all controlpatches
    FAlphaBlend: byte;
    function FGetColor: TColor;
    function FGetCount: integer;
    function FGetDXFLayername: string;
    function FGetName: string;
    function FGetItems(Index: integer): TFreeSubdivisionControlFace;
    function FGetLayerIndex: integer;
    function FGetSurfaceProperties: TLayerProperties;
    procedure FSetFDevelopable(Val: boolean);
    procedure FSetName(Val: string);
    procedure FSetColor(Val: TColor);
    procedure FSetShowInLinesplan(val: boolean);
    procedure FSetUseInHydrostatics(val: boolean);
    procedure FSetUseForIntersections(val: boolean);
    procedure FSetVisible(Val: boolean);
    procedure FSetSymmetric(Val: boolean);
  public
    procedure AddControlFace(
      ControlFace: TFreeSubdivisionControlFace);
    procedure AssignProperties(
      Source: TFreeSubdivisionLayer);
    procedure FindEdgeFaceIntersectionPoints(
      Layer: TFreeSubdivisionLayer; var NewEdge: TFreeSubdivisionControlEdge;
      var NewPoints: TFasterListTFreeSubdivisionControlPoint);
    procedure FindIntersectionPoints(
      Layer: TFreeSubdivisionLayer; var NewPoints: TFasterListTFreeSubdivisionControlPoint);
    procedure FindIntersectionControlPoints(
      Layer: TFreeSubdivisionLayer; var NewPoints: TFasterListTFreeSubdivisionControlPoint);
    function CalculateIntersectionPoints(
      Layer: TFreeSubdivisionLayer): boolean;
    constructor Create(Owner: TFreeSubdivisionSurface);
    procedure Clear;
    function CheckIntegrity: boolean;
    function Delete: boolean;
    procedure DeleteControlFace(
      ControlFace: TFreeSubdivisionControlFace);
    destructor Destroy;
      override;
    procedure Draw(Viewport: TFreeViewport);
    procedure Extents(var Min, Max: T3DCoordinate);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(
      var LineNr: integer; Strings: TStringList);
    procedure MoveDown;
    procedure MoveUp;
    procedure SaveToDXF(Strings: TStringList);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToStream(Strings: TStringList);
    procedure SelectAll;
    procedure Unroll(Destination: TFasterListTFreeDevelopedPatch);
    property AlphaBlend: byte
      read FAlphaBlend write FAlphaBlend;
    property Color: TColor
      read FGetColor write FSetColor;
    property Count: integer
      read FGetCount;
    property Developable: boolean
      read FDevelopable write FSetFDevelopable;
    property DXFLayername: string
      read FGetDXFLayername;
    property Items[Index: integer]
      : TFreeSubdivisionControlFace read FGetItems;
    property LayerID: integer
      read FLayerID write FLayerID;
    property LayerIndex: integer
      read FGetLayerIndex;
    property MaterialDensity: TFloatType
      read FMaterialDensity write FMaterialDensity;
    property Name: string
      read FGetName write FSetName;
    property Owner:
      TFreeSubdivisionSurface read FOwner write FOwner;
    property ShowInLinesplan: boolean
      read FShowInLinesplan write FSetShowInLinesplan;
    property SurfaceProperties:
      TLayerProperties read FGetSurfaceProperties;
    property Symmetric: boolean
      read FSymmetric write FSetSymmetric;
    property Thickness: TFloatType
      read FThickness write FThickness;
    property UseInHydrostatics: boolean
      read FUseInHydrostatics write FSetUseInHydrostatics;
    property UseForIntersections: boolean
      read FUseForIntersections write FSetUseForIntersections;
    property Visible: boolean
      read FVisible write FSetVisible;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionPoint                                  }
  {--------------------------------------------------------------------------------------------------}
  TFreeSubdivisionPoint = class(TFreeSubdivisionBase)
  private
    FFaces: TFasterListTFreeSubdivisionFace;
    FEdges: TFasterListTFreeSubdivisionEdge;
    FCoordinate: T3DCoordinate;
    FVertexType: TFreeVertexType;
    function FGetEdge(Index: integer):TFreeSubdivisionEdge;
    function FGetCoordinate: T3DCoordinate;
    function FGetCurvature: extended;
    function FGetFace(Index: integer):TFreeSubdivisionFace;
    function FGetIndex: integer;virtual;
    function FGetIsBoundaryVertex: boolean;
    function FGetNormal: T3DCoordinate;
    function FGetNumberOfCurves: integer;
    function FGetNumberOfEdges: integer;
    function FGetNumberOfFaces: integer;
    function FGetRegularPoint: boolean;
    function FGetLimitPoint: T3DCoordinate;
    procedure FSetCoordinate(Val: T3DCoordinate);virtual;
  public
    procedure AddEdge(Edge: TFreeSubdivisionEdge);
    procedure AddFace(Face: TFreeSubdivisionFace);
    function Averaging: T3DCoordinate;
    function CalculateVertexPoint:TFreeSubdivisionPoint;virtual;
    function CheckIntegrity: boolean;
    procedure Clear;
    constructor Create(Owner: TFreeSubdivisionSurface);override;
    procedure Delete; virtual;
    procedure UnreferenceEdge(Edge: TFreeSubdivisionEdge);
    procedure UnreferenceFace(Face: TFreeSubdivisionFace);
    procedure Unreference; virtual;
    destructor Destroy;override;
    function IndexOfFace(Face: TFreeSubdivisionFace): integer;
    function IsRegularNURBSPoint(Faces: TFasterListTFreeSubdivisionFace): boolean;
    property Coordinate:T3DCoordinate read FGetCoordinate write FSetCoordinate;
    property Curvature: extended read FGetCurvature;
    property Edge[index: integer]: TFreeSubdivisionEdge read FGetEdge;
    property Face[index: integer]: TFreeSubdivisionFace read FGetFace;
    property IsBoundaryVertex: boolean
      read FGetIsBoundaryVertex;
    property LimitPoint:
      T3DCoordinate read FGetLimitPoint;
    property Normal:
      T3DCoordinate read FGetNormal;
    property NumberOfCurves: integer
      read FGetNumberOfCurves;
    property NumberOfEdges: integer
      read FGetNumberOfEdges;
    property NumberOfFaces: integer
      read FGetNumberOfFaces;
    property RegularPoint: boolean
      read FGetRegularPoint;
    property VertexIndex: integer
      read FGetIndex;
    property VertexType:
      TFreeVertexType read FVertexType write FVertexType;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionControlPoint                           }
  {--------------------------------------------------------------------------------------------------}
  TFreeSubdivisionControlPoint = class(TFreeSubdivisionPoint)
  private
    FLinearConstraintPointA: TFreeSubdivisionControlPoint; // if defined, the point can be locate on the line between these A and B points only
    FLinearConstraintPointB: TFreeSubdivisionControlPoint;
    FLinearConstrainedPoints: TFasterListTFreeSubdivisionControlPoint; // backward link list
    FAnchorPoint: TFreeSubdivisionControlPoint;  // if defined, the point will be moved parallel to FAnchorConstraintPoint when it moved
    FAnchoredPoints: TFasterListTFreeSubdivisionControlPoint; // backward link list
    FLocked: boolean;
    FIsAnchorHard:boolean;
    FInSetCoordinate: boolean;
    function FGetColor: TColor;
    function FGetIndex: integer;
      override;
    function FGetIsLeak: boolean;
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    procedure FSetLocked(val: boolean);
    procedure FSetSelected(val: boolean);
    procedure FSetCoordinate(Val: T3DCoordinate); override;
  public
    procedure Collapse;
    constructor Create(Owner: TFreeSubdivisionSurface); override;
    destructor Destroy; override;
    procedure AdjustToLinearConstraint(Viewport: TFreeViewport);
    function DistanceToCursor(X, Y: integer;
      Viewport: TFreeViewport): integer;
    procedure Delete; override;
    procedure Draw(Viewport: TFreeViewport);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(var LineNr: integer; Strings: TStringList);
    procedure MoveBy( Viewport:TFreeViewPort; dX,dY,dZ: TFloatType;
                      vAnchor:TFreeSubdivisionControlPoint);
    procedure SetCoordinate( ViewPort:TFreeViewPort; Val: T3DCoordinate;
                             vAnchor:TFreeSubdivisionControlPoint);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToStream(Strings: TStringList);
    procedure SetLinearConstraint(pointA, pointB: TFreeSubdivisionControlPoint);
    procedure SetAnchorPoint(pointA: TFreeSubdivisionControlPoint);
    procedure SetIsAnchorHard(val: boolean);
    procedure Unreference; override;
    property Color: TColor read FGetColor;
    property IsLeak: boolean read FGetIsLeak;
    property LinearConstraintPointA: TFreeSubdivisionControlPoint read FLinearConstraintPointA;
    property LinearConstraintPointB: TFreeSubdivisionControlPoint read FLinearConstraintPointB;
    property AnchorPoint: TFreeSubdivisionControlPoint read FAnchorPoint write SetAnchorPoint;
    property Locked: boolean read FLocked write FSetLocked;
    property IsAnchorHard:boolean read FIsAnchorHard write SetIsAnchorHard;
    property Selected: boolean read FGetSelected write FSetSelected;
    // Property to see if this point has been selected by the user
    property Visible: boolean
      read FGetVisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionControlPointGroup                       }
  { TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
  {---------------------------------------------------------------------------------------------------}
  TFreeSubdivisionControlPointGroup = class(TFreeNamedObject)
  private
    //FOwner: TFreeSubdivisionSurface;
    FControlPoints: TFasterListTFreeSubdivisionControlPoint;
    FLocked: boolean;
    function CalculateCenterPoint: T3DCoordinate;
    function FGetIndex: integer;
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    procedure FSetLocked(val: boolean);
    procedure FSetSelected(val: boolean);
  public
    procedure AddControlPoint(cp:TFreeSubdivisionControlPoint);
    procedure RemoveControlPoint(cp:TFreeSubdivisionControlPoint);
    procedure BreakGroup; virtual;
    constructor Create(Owner: TFreeSubdivisionSurface); override;
    procedure Delete;
    procedure Draw(Viewport: TFreeViewport);
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(var LineNr: integer; Strings: TStringList);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToStream(Strings: TStringList);

    property ControlPoints:TFasterListTFreeSubdivisionControlPoint read FControlPoints;
    property Owner: TFreeSubdivisionSurface read FOwner write FOwner;
    //procedure PrintDebug; virtual;
    property Locked: boolean read FLocked write FSetLocked;
    property Selected: boolean read FGetSelected write FSetSelected;
    property Visible: boolean read FGetVisible;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionEdge                                   }
  {--------------------------------------------------------------------------------------------------}
  TFreeSubdivisionEdge = class(TFreeSubdivisionBase)
  private
    FStartpoint: TFreeSubdivisionPoint;
    FEndpoint: TFreeSubdivisionPoint;
    FFaces: TFasterListTFreeSubdivisionFace;
    FCrease: boolean;
    FControlEdge: boolean;
    FCurve: TFreeSubdivisionControlCurve;
    function FGetIndex: integer;
      virtual;
    function FGetIsBoundaryEdge: boolean;
      virtual;
    function FGetFace(Index: integer):
      TFreeSubdivisionFace;
    function FGetNumberOfFaces: integer;
    procedure FSetCrease(Val: boolean);
      virtual;
    function FGetPreviousEdge: TFreeSubdivisionEdge;
    function FGetNextEdge: TFreeSubdivisionEdge;
    procedure PrintDebug; override;
  public
    procedure AddFace(Face: TFreeSubdivisionFace);
    procedure Assign(Edge: TFreeSubdivisionEdge);
      virtual;
    function CalculateEdgeCenterPoint: TFreeSubdivisionPoint;
    function CheckIntegrity: boolean;
    procedure Clear;
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    procedure Delete; virtual;
    procedure UnreferenceFace(Face: TFreeSubdivisionFace);
    procedure Unreference; virtual;
    destructor Destroy;
      override;
    function DistanceToCursor(X, Y: integer;
      var P: T3DCoordinate; Viewport: TFreeViewport): integer; virtual;
    procedure Draw(DrawMirror: boolean;
      Viewport: TFreeViewport); virtual;
    procedure SwapData;
    //function getPoints:TFasterListTFreeSubdivisionPoint;
    property Crease: boolean
      read FCrease write FSetCrease;
    property Curve:
      TFreeSubdivisionControlCurve read FCurve write FCurve;
    property EdgeIndex: integer
      read FGetIndex;
    property EndPoint:
      TFreeSubdivisionPoint read FEndPoint write FEndPoint;
    property Face[index: integer]
      : TFreeSubdivisionFace read FGetFace;
    property IsBoundaryEdge: boolean
      read FGetIsBoundaryEdge;
    property NextEdge:
      TFreeSubdivisionEdge read FGetNextEdge;
    property NumberOfFaces: integer
      read FGetNumberOfFaces;
    property PreviousEdge:
      TFreeSubdivisionEdge read FGetPreviousEdge;
    property StartPoint:
      TFreeSubdivisionPoint read FStartPoint write FStartPoint;
    //property Points:TFasterListTFreeSubdivisionPoint read getPoints;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                         TFreesubdivisionControlEdge                              }
  {--------------------------------------------------------------------------------------------------}
  TFreesubdivisionControlEdge = class(TFreeSubdivisionEdge)
  private
    IsDeleting : boolean;
    function FGetColor: TColor;
    function FGetIndex: integer; override;
    function FGetIsBoundaryEdge: boolean; override;
    procedure FSetSelected(val: boolean);
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    function GetStartPoint:TFreeSubdivisionControlPoint;
    procedure SetStartPoint(val:TFreeSubdivisionControlPoint);
    function GetEndPoint:TFreeSubdivisionControlPoint;
    procedure SetEndPoint(val:TFreeSubdivisionControlPoint);
  public
    procedure Collapse;
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    procedure Delete; override;
    function DistanceToCursor(X, Y: integer;
      var P: T3DCoordinate; Viewport: TFreeViewport): integer; override;
    procedure Draw(DrawMirror: boolean;
      Viewport: TFreeViewport); override;
    function InsertControlPoint(
      P: T3DCoordinate): TFreeSubdivisionControlpoint;
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(
      var LineNr: integer; Strings: TStringList);
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToStream(Strings: TStringList);
    property EndPoint: TFreeSubdivisionControlPoint read GetEndPoint write SetEndPoint;
    property StartPoint: TFreeSubdivisionControlPoint read GetStartPoint write SetStartPoint;
    procedure Trace;
    function ConvertToFreeSubdivisionEdge:TFreeSubdivisionEdge;

    property Color: TColor
      read FGetColor;
    property Selected: boolean
      read FGetSelected write FSetSelected;
    // Property to see if this edge has been selected by the user
    property Visible: boolean
      read FGetVisible;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionFace                                   }
  {--------------------------------------------------------------------------------------------------}
  TFreeSubdivisionFace = class(TFreeSubdivisionBase)
  private
    FPoints: TFasterListTFreeSubdivisionPoint;
    function FGetArea: TFloatType;
    function FGetFaceCenter: T3DCoordinate;
    function FGetFaceNormal: T3DCoordinate;
    function FGetNumberOfPoints: integer;
    function FGetPoint(Index: integer): TFreeSubdivisionPoint;
  public
    procedure AddPoint(Point: TFreeSubdivisionPoint);
    function CalculateFaceCenterPoint: TFreeSubdivisionPoint;
    function CheckIntegrity: boolean;
    procedure Clear;
      virtual;
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    procedure Delete; virtual;
    procedure Unreference; virtual;
    destructor Destroy;
      override;
    procedure FlipNormal;
    // Inverts the point ordering of the face
    function IndexOfPoint(
      P: TFreeSubdivisionPoint): integer;
    procedure PrintDebug; override;
    procedure Subdivide(
      aOwner: TFreeSubdivisionSurface;
      aIsControlFace: boolean;
      aRefVertices:TFasterListTFreeSubdivisionPoint;
      aRefEdges:TFasterListTFreeSubdivisionEdge;
      aRefFaces:TFasterListTFreeSubdivisionFace;
      aInteriorEdges:TFasterListTFreeSubdivisionEdge;
      aControlDescendandEdges:TFasterListTFreeSubdivisionEdge;
      aNewFaces: TFasterListTFreeSubdivisionFace);
    property Area: TFloatType
      read FGetArea;
    property FaceCenter:
      T3DCoordinate read FGetFaceCenter;
    property FaceNormal:
      T3DCoordinate read FGetFaceNormal;
    property NumberOfpoints: integer
      read FGetNumberOfPoints;
    property Point[index: integer]
      : TFreeSubdivisionPoint read FGetPoint;
    property Points:TFasterListTFreeSubdivisionPoint read FPoints;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionControlFace                            }
  {--------------------------------------------------------------------------------------------------}
  TFreeSubdivisionControlFace = class(TFreeSubdivisionFace)
  private
    FLayer: TFreeSubdivisionLayer;
    FChildren: TFasterListTFreeSubdivisionFace;
    FMin, FMax: T3DCoordinate;
    FEdges: TFasterListTFreeSubdivisionEdge;
    FControlDescendantEdges: TFasterListTFreeSubdivisionEdge; // here can be real ControlEdges and divided "children" of them. Do not why.
    function FGetChild(Index: integer):
      TFreeSubdivisionFace;
    function FGetChildCount: integer;
    function FGetColor: TColor;
    function FGetControlDescendantEdge(Index: integer): TFreeSubdivisionEdge;
    function FGetControlDescendantEdgeCount: integer;
    function FGetEdge(Index: integer): TFreeSubdivisionEdge;
    function FGetEdgeCount: integer;
    function FGetIndex: integer;
    function FGetPoint(Index: integer): TFreeSubdivisionControlPoint;
    function FGetSelected: boolean;
    function FGetVisible: boolean;
    procedure FSetLayer(Val: TFreeSubdivisionLayer);
    procedure FSetSelected(val: boolean);
  public
    procedure CalcExtents;
    function CheckIntegrity: boolean;
    procedure Clear;
      override;
    procedure ClearChildren;
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    function DistanceToCursor(X, Y: integer;
      var P: T3DCoordinate; Viewport: TFreeViewport): integer;
    procedure Delete; override;
    procedure Unreference; override;
    destructor Destroy;  override;
    procedure Draw(Viewport: TFreeViewport); overload; virtual;
    procedure Draw(Viewport: TFreeViewport;
      MinCurvature, MaxCurvature: TFloatType); reintroduce; overload;
    function InsertEdge(
      P1, P2: TFreeSubdivisionControlPoint): TFreesubdivisionControlEdge;
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(
      var LineNr: integer; Strings: TStringList);
    procedure RemoveReferences;
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToDXF(Strings: TStringList);
    procedure SaveToStream(Strings: TStringList);
      virtual;
    procedure Subdivide(
      aOwner: TFreeSubdivisionSurface;
      aControlFace: boolean;
      aVertexPoints:TFasterListTFreeSubdivisionPoint;
      aEdgePoints:TFasterListTFreeSubdivisionEdge;
      aFacePoints:TFasterListTFreeSubdivisionFace;
      aInteriorEdges:TFasterListTFreeSubdivisionEdge;
      aControlEdges:TFasterListTFreeSubdivisionEdge;
      aDest: TFasterListTFreeSubdivisionFace);
    procedure Trace;
    // select all controlfaces connected to the current one that belong to the same layer and are not separated by a crease edge
    property Color: TColor
      read FGetColor;
    property ControlDescendantEdge[index: integer]
      : TFreeSubdivisionEdge read FGetControlDescendantEdge;
    property ControlDescendantEdgeCount: integer
      read FGetControlDescendantEdgeCount;
    property Child[index: integer]
      : TFreeSubdivisionFace read FGetChild;
    property ChildCount: integer
      read FGetChildCount;
    property Edge[index: integer]
      : TFreeSubdivisionEdge read FGetEdge;
    property EdgeCount: integer
      read FGetEdgeCount;
    property FaceIndex: integer
      read FGetIndex;
    property Layer:
      TFreeSubdivisionLayer read FLayer write FSetLayer;
    property Max:
      T3DCoordinate read FMax;
    property Min:
      T3DCoordinate read FMin;
    property Point[index: integer]: TFreeSubdivisionControlPoint read FGetPoint;

    property Selected: boolean
      read FGetSelected write FSetSelected;
    // Property to see if this controlface has been selected by the user
    property Visible: boolean
      read FGetVisible;
  end;

  {---------------------------------------------------------------------------------------------------}
  {                                           TFreeSubdivisionSurface                                 }
  { This is the subdivision surface used for modelling the hull.                                      }
  { This is actually a quad-triangle subdivision surface as published in the articles:               }
  {   "Quad/triangle subdivision" by J. Stam & C. Loop http://research.microsoft.com/~cloop/qtEG.pdf  }
  {   "On C2 triangle/quad subdivision" by Scott Schaeffer & Joe Warren                               }
  {---------------------------------------------------------------------------------------------------}
  TFreeSubdivisionSurface = class(TFreeEntity)
  private
    FControlPoints: TFasterListTFreeSubdivisionControlPoint;
    // List with controlpoints, which can be changed by the user
    FControlPointGroups: TFasterListTFreeSubdivisionControlPointGroup;
    FControlEdges: TFasterListTFreeSubdivisionControlEdge;
    // List with controledges, which can be changed by the user
    FControlFaces: TFasterListTFreeSubdivisionControlFace;
    // List with controlfaces, which can be changed by the user
    FControlCurves: TFasterListTFreeSubdivisionControlCurve;          // list with mastercurves
    FPoints: TFasterListTFreeSubdivisionPoint;
    // List with points obtained by subdividing the surface
    FEdges: TFasterListTFreeSubdivisionEdge;
    // this list edges obtained by subdividing the controledges
    FIdSequence:integer;
    FLayers: TFasterListTFreeSubdivisionLayer;
    // All layers are stored in this list
    FSelectedControlPoints: TFasterListTFreeSubdivisionControlPoint;
    // Controlpoints which are selected by the user are put in this list
    FSelectedControlPointGroups: TFasterListTFreeSubdivisionControlPointGroup;
    FSelectedControlEdges: TFasterListTFreeSubdivisionControlEdge;
    // List with currently selected controledges
    FSelectedControlCurves: TFasterListTFreeSubdivisionControlCurve;
    // List with currently selected controlcurves
    FSelectedControlFaces: TFasterListTFreeSubdivisionControlFace;
    // List with currently selected controlfaces
    FActiveLayer: TFreeSubdivisionLayer;
    // Currently active layer, may not be nil!
    FShowControlNet: boolean;
    // Flag to switch controlpoints and control-edges visibility
    FInitialized: boolean; // Flag to check if the surface has been initialised.
    FIsAveraged: boolean; // Flag to check if the surface has been averaged (smoothed).
    FShowInteriorEdges: boolean;
    // Switch to turn on drawing off all interior edges as well.
    FDrawMirror: boolean;
    // If this is set tot true, the other imaginary half (starboard side) will be drawn aswell
    FSubdivisionMode: TFreeSubdivisionMode;
    // Varaiable to switch between quad-triangle and Catmull Clark subdivision
    FDesiredSubdivisionLevel: byte;
    FCurrentSubdivisionLevel: byte;
    FCreaseColor: TColor;
    // color of descendants from creaseedges
    FCreaseEdgeColor: TColor;
    // Color of crease controledges
    FLastusedLayerID: integer;
    FOnChangeLayerData: TNotifyEvent;
    // Event which is raised when layer-data has been changed
    FOnChangeActiveLayer: TChangeActiveLayerEvent;
    // Event raised when the active layer is changed
    FOnSelectItem: TNotifyEvent;
    // This event is raised whenever an item (such as controlpoint, controledge or controlface) is selected or deselected

    FUnderWaterColor: TColor;
    // Color used for shading the underwater part
    FWaterlinePlane: T3DPlane;
    // This plane is used to clip the hull, and shade the underwatership in a different color
    FShadeUnderWater: boolean;
    // Switch to turn under water shading on or off
    FShowNormals: boolean;
    // show normals of selected controlfaces
    FControlPointSize: integer;
    FEdgeColor: TColor;
    // Color of normal edges (no crease)
    FSelectedcolor: TColor;
    // Default color for selected items
    FCreasePointColor: TColor;
    // Color for vertices connected to two creaseedges
    FRegularPointColor: TColor;
    // Color of regular controlpoints
    FCornerPointColor: TColor;               // color of cornerpoints
    FDartPointColor: TColor;
    FLayerColor: TColor;               // Default color for layers;
    FNormalColor: TColor;               // color of surface normals
    FLeakColor: TColor;               // color of leak points
    FCurvatureColor: TColor;
    // color of the curvature plot of controlcurves
    FControlCurveColor: TColor;
    FZebraColor: TColor;
    FShowCurvature: boolean;
    FShowControlCurves: boolean;
    FCurvatureScale: TFloatType;
    FGausCurvature: TFloatArray;
    // list with precalculated values of gauss. curvature in each point, used for shading
    FMinGaussCurvature: TFloatType;
    FMaxGaussCurvature: TFloatType;
    FMainframeLocation: single;
    FOnFaceRebuilt: TProgressEvent;
    InUnreferenceControlFace:boolean;
    InUnreferenceControlEdge:boolean;
    InUnreferenceControlPoint:boolean;
    InUnreferenceFace:boolean;
    InUnreferenceEdge:boolean;
    InUnreferencePoint:boolean;
    function FGetControlPoint(Index: integer): TFreeSubdivisionControlPoint;
    function FGetControlPointGroup(Index: integer): TFreeSubdivisionControlPointGroup;
    function FGetControlCurve(Index: integer): TFreesubdivisionControlCurve;
    function FGetControlEdge(Index: integer): TFreesubdivisionControlEdge;
    function FGetControlFace(Index: integer): TFreeSubdivisionControlFace;
    function FGetGaussCurvatureCalculated: boolean;
    function FGetLayer(Index: integer): TFreeSubdivisionLayer;
    function FGetNumberOfControlPoints: integer;
    function FGetNumberOfControlPointGroups: integer;
    function FGetNumberOfControlEdges: integer;
    function FGetNumberOfControlCurves: integer;
    function FGetNumberOfControlFaces: integer;
    function FGetNumberOfFaces: integer;
    function FGetNumberOfLayers: integer;
    function FGetNumberOfLockedPoints: integer;
    function FGetPoint(Index: integer): TFreeSubdivisionPoint;
    function FGetEdge(Index: integer): TFreeSubdivisionEdge;
    function FGetNumberOfPoints: integer;
    function FGetNumberOfSelectedControlCurves: integer;
    function FGetNumberOfSelectedControlEdges: integer;
    function FGetNumberOfSelectedControlFaces: integer;
    function FGetNumberOfSelectedControlPoints: integer;
    function FGetNumberOfSelectedControlPointGroups: integer;
    function FGetNumberOfSelectedLockedPoints: integer;
    function FGetNumberOfEdges: integer;
    function FGetSelectedControlCurve(Index: integer): TFreeSubdivisionControlCurve;
    function FGetSelectedControlEdge(Index: integer): TFreeSubdivisionControlEdge;
    function FGetSelectedControlFace(Index: integer): TFreeSubdivisionControlFace;
    function FGetSelectedControlPoint(Index: integer): TFreeSubdivisionControlPoint;
    function FGetSelectedControlPointGroup(Index: integer): TFreeSubdivisionControlPointGroup;
    function FRequestNewLayerID: integer;
    procedure FSetActiveLayer(Val: TFreeSubdivisionLayer);
    procedure FSetBuild(Val: boolean);
      override;
    procedure FSetDesiredSubdivisionLevel(val: byte);
    procedure FSetFShowControlNet(Val: boolean);
    procedure FSetSubdivisionMode(val: TFreeSubdivisionMode);
    procedure FSetUnderwaterColor(Val: TColor);
  protected
  public
    procedure AddControlCurve(Curve: TFreesubdivisionControlCurve);
    function AddControlEdge(P1, P2: TFreeSubdivisionControlPoint):
      TFreesubdivisionControlEdge; overload; virtual;
    function AddControlFace(Points: array of T3DCoordinate;
      NoPoints: integer): TFreeSubdivisionControlFace; overload; virtual;
    function AddControlFace(Points: TFasterListTFreeSubdivisionControlPoint;
      CheckEdges: boolean): TFreeSubdivisionControlFace; reintroduce; overload;
    function AddControlFace(Points: TList;
      CheckEdges: boolean): TFreeSubdivisionControlFace;
      reintroduce; overload;
    function AddControlFace(Points: TFasterListTFreeSubdivisionControlPoint; CheckEdges: boolean;
      Layer: TFreeSubdivisionLayer): TFreeSubdivisionControlFace; reintroduce; overload;
    function AddControlFaceN(Points: TFasterListTFreeSubdivisionControlPoint; CheckEdges: boolean;
      Layer: TFreeSubdivisionLayer): TFreeSubdivisionControlFace; reintroduce; overload;
    function AddControlPoint(P: T3DCoordinate): TFreeSubdivisionControlPoint;
      overload; virtual;
    procedure AddControlPoint(P: TFreeSubdivisionControlPoint);
      reintroduce; overload;
    function AddControlPoint: TFreeSubdivisionControlPoint;
      reintroduce; overload;
    // Adds a new controlpoint at 0,0,0 without checking other points

    // Delete all object's references from related objects
    procedure UnreferenceControlPoint(P: TFreeSubdivisionControlPoint);
    procedure UnreferenceControlEdge(E: TFreeSubdivisionControlEdge);
    procedure UnreferenceControlFace(F: TFreeSubdivisionControlFace);

    procedure UnreferencePoint(P: TFreeSubdivisionPoint);
    procedure UnreferenceEdge(E: TFreeSubdivisionEdge);
    procedure UnreferenceFace(F: TFreeSubdivisionFace);

    function AddNewLayer: TFreeSubdivisionLayer;
    procedure AssembleFacesToPatches(Layers: TFasterListTFreeSubdivisionLayer; Mode: TFreeAssembleMode;
      var AssembledPatches: TFreeFaceArray; var NAssembled: integer);
    procedure CalculateGaussCurvature;
    // Calculate Gauss. curvature in each point of the mesh and store it in a array

    function CheckIntegrity:boolean;

    procedure Clear; override;
    procedure ClearFaces;
    procedure ClearSelection;
    procedure ConvertToGrid(Input: TFreeFaceGrid; var Cols, Rows: integer;
      var Grid: TFreeSubdivisionGrid);
    procedure Edge_Connect;
    function CanInsertEdge: boolean;
    procedure ExportFeFFile(Strings: TStringList);
    procedure ExportObjFile(ExportControlNet: boolean; Strings: TStringList);
    procedure Extents(var Min, Max: T3DCoordinate);
      override;
    procedure ExtrudeEdges(Edges: TFasterListTFreeSubdivisionEdge; Direction: T3DCoordinate);
      reintroduce; overload;
    procedure CalculateIntersections(Plane: T3DPlane;
      Faces: TFasterListTFreeSubdivisionFace; Destination: TFasterListTFreeSpline);
    constructor Create(Owner: TFreeSubdivisionSurface);
      override;
    destructor Destroy;
      override;
    procedure Draw(Viewport: TFreeViewport);
      override;
    function EdgeExists(P1, P2: TFreeSubdivisionPoint): TFreeSubdivisionEdge;
    procedure ExtractAllEdgeLoops(var Destination: TFasterListTFreeSubdivisionEdge);
    procedure ExtractPointsFromFaces(  SelectedFaces : TFasterListTFreeSubdivisionFace;
               Points: TFasterListTFreeSubdivisionPoint; var LockedPoints: integer);
    // extracts all points that are used by the faces in the selectedfaces list
    procedure ExtractPointsFromSelection(SelectedPoints: TFasterListTFreeSubdivisionControlPoint;
      var LockedPoints: integer);
    procedure ImportFEFFile(Strings: TStringList; var LineNr: integer);
    procedure ImportGrid(Points: TFreeCoordinateGrid; Cols, Rows: integer;
      Layer: TFreesubdivisionLayer);
    procedure Initialize(PointStartIndex, EdgeStartIndex, FaceStartIndex: integer);
    function IntersectPlane(Plane: T3DPlane; HydrostaticsLayersOnly: boolean;
      List: TFasterListTFreeSpline): boolean;
    procedure InsertPlane(Plane: T3DPlane; AddCurves: boolean);
    // inserts points on edges (visible edges only) that intersect the input plane
    procedure IsolateEdges(Source:TFasterListTFreeSubdivisionEdge;
              Destination: TFasterListTFreeSubdivisionFace); overload; virtual;
    procedure LoadBinary(Source: TFreeFileBuffer);
    procedure LoadFromStream(var LineNr: integer; Strings: TStringList);
    procedure LoadVRMLFile(Filename: string);
    function PointExists(P: TFreeSubdivisionControlPoint): boolean;
    procedure Average;
    procedure Rebuild; override;
    procedure SaveBinary(Destination: TFreeFileBuffer);
    procedure SaveToStream(Strings: TStringList);
    procedure Selection_Delete;
    procedure SortEdges(Edges: TFasterListTFreeSubdivisionEdge);
      overload; virtual;
    procedure SortEdges(Edges: TFasterListTFreeSubdivisionEdge;
      var Points: TFasterListTFreeSubdivisionPoint); reintroduce; overload;
    procedure SubDivide;
    property ActiveLayer: TFreeSubdivisionLayer
      read FActiveLayer write FSetActiveLayer;
    property ControlPoint[index: integer]: TFreeSubdivisionControlPoint
      read FGetControlpoint;
    property ControlPointGroup[index: integer]: TFreeSubdivisionControlPointGroup
      read FGetControlpointGroup;
    property ControlPoints: TFasterListTFreeSubdivisionControlPoint read FControlPoints;
    property ControlPointGroups: TFasterListTFreeSubdivisionControlPointGroup read FControlPointGroups;
    property ControlPointSize: integer
      read FControlPointSize write FControlPointSize;
    property ControlCurve[index: integer]: TFreesubdivisionControlCurve
      read FGetControlCurve;
    property ControlCurveColor: TColor
      read FControlCurveColor write FControlCurveColor;
    property ControlEdge[index: integer]: TFreesubdivisionControlEdge
      read FGetControlEdge;
    property ControlEdges: TFasterListTFreeSubdivisionControlEdge
      read FCOntrolEdges;
    property ControlFace[index: integer]: TFreeSubdivisionControlFace
      read FGetControlFace;
    property ControlFaces: TFasterListTFreeSubdivisionControlFace
      read FCOntrolFaces;
    property CurrentSubdivisionLevel: byte
      read FCurrentSubdivisionLevel;
    property CurvatureColor: TColor
      read FCurvatureColor write FCurvatureColor;
    property CurvatureScale: TFloatType
      read FCurvatureScale write FCurvatureScale;
    property CreaseColor: TColor
      read FCreaseColor write FCreaseColor;
    property CreaseEdgeColor: TColor
      read FCreaseEdgeColor write FCreaseEdgeColor;
    property CornerPointColor: TColor
      read FCornerPointColor write FCornerPointColor;
    property DartPointColor: TColor
      read FDartPointColor write FDartPointColor;
    property DesiredSubdivisionLevel: byte
      read FDesiredSubdivisionLevel write FSetDesiredSubdivisionLevel;
    property DrawMirror: boolean
      read FDrawMirror write FDrawMirror;
    property GaussCurvatureCalculated: boolean
      read FGetGaussCurvatureCalculated;
    property CreasePointColor: TColor
      read FCreasePointColor write FCreasePointColor;
    property RegularPointColor: TColor
      read FRegularPointColor write FRegularPointColor;
    property Layer[index: integer]: TFreeSubdivisionLayer
      read FGetLayer;
    property LayerColor: TColor
      read FLayerColor write FLayerColor;
    property LeakColor: TColor
      read FLeakColor write FLeakColor;
    property MainframeLocation: TFloatType
      read FMainframeLocation write FMainframeLocation;
    property MaxGaussCurvature: TFloatType
      read FMaxGaussCurvature;
    property MinGaussCurvature: TFloatType
      read FMinGaussCurvature;
    property NumberOfControlFaces: integer
      read FGetNumberOfControlFaces;
    property NumberOfControlEdges: integer
      read FGetNumberOfControlEdges;
    property NumberOfControlCurves: integer
      read FGetNumberOfControlCurves;
    property NumberOfControlPoints: integer
      read FGetNumberOfControlPoints;
    property NumberOfControlPointGroups: integer
      read FGetNumberOfControlPointGroups;
    property NumberOfFaces: integer
      read FGetNumberOfFaces;
    property NumberOfLayers: integer
      read FGetNumberOfLayers;
    property NumberOfLockedPoints: integer
      read FGetNumberOfLockedPoints;
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
    property NumberOfSelectedLockedPoints: integer
      read FGetNumberOfSelectedLockedPoints;
    property OnChangeActiveLayer: TChangeActiveLayerEvent
      read FOnChangeActiveLayer write FOnChangeActiveLayer;
    property OnChangeLayerData: TNotifyEvent
      read FOnChangeLayerData write FOnChangeLayerData;
    property OnSelectItem: TNotifyEvent
      read FOnSelectItem write FOnSelectItem;
    property OnFaceRebuilt: TProgressEvent read FOnFaceRebuilt write FOnFaceRebuilt;
    property Point[index: integer]: TFreeSubdivisionPoint
      read FGetpoint;
    property Edge[index: integer]: TFreeSubdivisionEdge
      read FGetEdge;
    property EdgeColor: TColor
      read FEdgeColor write FEdgeColor;
    property NormalColor: TColor
      read FNormalColor write FNormalColor;
    property NumberOfEdges: integer
      read FGetNumberOfEdges;
    property NumberOfPoints: integer
      read FGetNumberOfPoints;
    property ShadeUnderWater: boolean
      read FShadeUnderWater write FShadeUnderWater;
    property Selectedcolor: TColor
      read FSelectedcolor write FSelectedcolor;
    property SelectedControlCurve[index: integer]: TFreeSubdivisionControlCurve
      read FGetSelectedControlCurve;
    property SelectedControlEdge[index: integer]: TFreeSubdivisionControlEdge
      read FGetSelectedControlEdge;
    property SelectedControlFace[index: integer]: TFreeSubdivisionControlFace
      read FGetSelectedControlFace;
    property SelectedControlPoint[index: integer]: TFreeSubdivisionControlPoint
      read FGetSelectedControlPoint;
    property SelectedControlPoints: TFasterListTFreeSubdivisionControlPoint read FSelectedControlPoints;
    property SelectedControlPointGroup[index: integer]: TFreeSubdivisionControlPointGroup
      read FGetSelectedControlPointGroup;
    property ShowControlCurves: boolean
      read FShowControlCurves write FShowControlCurves;
    property ShowControlNet: boolean
      read FShowControlNet write FSetFShowControlNet;
    property ShowCurvature: boolean
      read FShowCurvature write FShowCurvature;
    property ShowInteriorEdges: boolean
      read FShowInteriorEdges write FShowInteriorEdges;
    property ShowNormals: boolean
      read FShowNormals write FShowNormals;
    property SubdivisionMode: TFreeSubdivisionMode
      read FSubdivisionMode write FSetSubdivisionMode;
    property UnderWaterColor: TColor
      read FUnderWaterColor write FSetUnderWaterColor;
    property WaterlinePlane: T3DPlane
      read FWaterlinePlane write FWaterlinePlane;
    property ZebraColor: TColor
      read FZebraColor write FZebraColor;
  end;

  {--------------------------------------------------------------------------------------------------}
  {                                         TFreeDestroyList                                         }
  {  Some objects can be split to number of new ones and destroyed after it.                         }
  {  However they cannot be destroyed right away because they are referred in a processing cycle     }
  {  They should be added to this list and destroyed later.                                          }
  {--------------------------------------------------------------------------------------------------}

  TFreeDestroyList = class(TFasterList<TFreeSubdivisionBase>)
  public
    constructor Create;
    procedure DestroyAll;
    // destroy all objects in the list, but not the list itself. To destroy the list call Destroy;
  end;


function AddPoint(P1, P2: T3DCoordinate): T3DCoordinate;
// Add two vectors
function AddPointSymm(P1, P2: T3DCoordinate): T3DCoordinate;
// Add two vectors for symmetric layers
function AreaStr(Units: TFreeUnitType): string;
// Returns a string value with the area units
function BoolToStr(Val: boolean): string;
// In contrast to delphis own BoolToStrF this procedure returns '0' when false and '1' when true
procedure ClipTriangle(P1, P2, P3: T3DCoordinate; s1, s2, s3: TFloatType;
  var Nf, Nb: integer; var Front, Back: TFreeCoordinateArray); overload;
procedure ClipTriangle(P1, P2, P3: T3DCoordinate; Plane: T3DPlane;
  var Nf, Nb: integer; var Front, Back: TFreeCoordinateArray); overload;
function ConvertDimension(Value: TFloatType; Units: TFreeUnitType): string;
// Converts a dimesnion to a string
function ConvertCoordinate(Coord: string; OldCoord: TFloatType): TFloatType;
// converts a string to a floatingpoint value, possibly using imperial units
function CrossProduct(U, V: T3DCoordinate): T3DCoordinate;
function DisplacementToVolume(Displ, Density, AppCoeff: TFloatType;
  Units: TFreeUnitType): TFloatType; // Converts a displacement to volume
function DensityStr(Units: TFreeUnitType): string;
// Returns a string value with the density units
function DistPP3D(P1, P2: T3DCoordinate): TFloatType;
// Calculates the distance between two points
function DistanceToLine(P1, P2: TPoint; X, Y: integer; var Parameter: TFloatType): TFloatType;
function DistancePointToPlane(P: T3DCoordinate; Plane: T3DPlane): TFloatType;
function DotProduct(U, V: T3DCoordinate): TFloatType;
function FindDXFColorIndex(Color: TColor): integer;
// find nearest DXF color corresponding to a windows color
procedure FillColor(Parameter: TFloatType; var R, G, B: byte);
function InertiaStr(Units: TFreeUnitType): string;
// Returns a string value with the moment of inertia units
function Interpolate(P1, P2: T3DCoordinate; Param: TFloatType): T3DCoordinate;
// perform linear interpolation between two 3D points
procedure JoinSplineSegments(JoinError: TFloatType; ForceToOneSegment: boolean;
  List: TFasterListTFreeSpline);// Takes multiple splines and tries to connect them to as few as possible
function Lines3DIntersect(P1, P2, P3, P4: T3DCoordinate; var Param: double;
  var Int: T3DCoordinate): boolean;
function LengthStr(Units: TFreeUnitType): string;
// Returns a string value with the length units
function MakeLength(Value: TFloatType; Decimals, DesLength: integer): string; overload;
function MakeLength(Value: string; DesLength: integer): string; overload;
procedure MinMax(P: T3DCoordinate; var Min, Max: T3DCoordinate);
function Midpoint(P1, P2: T3DCoordinate): T3DCoordinate;
// Calculate the mid-point between P1 and P2
function MirrorPlane(P: T3DCoordinate; Plane: T3DPLane): T3DCoordinate;
// mirror a point in a plane
function Normalize(P: T3DCoordinate): T3DCoordinate;
function NumberOfDecimals(Value: TFloatType): integer;
// Finds out with how many decimals a number should be presented
function PlaneIntersectsBox(Min, Max: T3DCoordinate; Plane: T3DPlane): boolean;
// Function to determine if a plane intersects a bounding box
function PlanePointNormal(P, Normal: T3DCoordinate): T3DPlane;
// Calculates the plane with a given normal N through point P
function PlanePPP(P1, P2, P3: T3DCoordinate): T3DPlane;
// Create a plane defined by three points
function PointInTriangle(Int, P0, P1, P2: T3DCoordinate): boolean;
// This function calculates if a point lies inside a triangle assuming it lies on the plane determined by the triangle
function PoundsToNewton(InpLbs: TFloatType): TFloatType;
// converts pounds to Newton
function ProjectPointOnLine(P, P1, P2: T3DCoordinate): T3DCoordinate;
// Projects point  P on the linesegment through P1 and P2
function ProjectPointOnPlane(P: T3DCoordinate; Plane: T3DPlane): T3DCoordinate;
// Projects a point on to a plane
function RandomColor: TColor;
// create a random color
function ReadBoolFromStr(LineNr: integer; var Source: string): boolean;
// Read a single boolean value from a string
function ReadFloatFromStr(LineNr: integer;
  var Source: string): TFloatType;   // Read a single floatingpoint value from a string
function ReadIntFromStr(LineNr: integer; var Source: string): integer;
// Read an integer value from a string
function RotateAroundPoint(P: T3DCoordinate; Center: T3DCoordinate;
  sinhx, coshx, sinhy, coshy, sinhz, coshz: TFloatType): T3DCoordinate;
function RotatePointAroundVector(Point, StartPoint, Endpoint: T3DCoordinate): T3DCoordinate;
// Function to rotate a point around a vector
function RotateVector(P0: T3DCoordinate;
  sinx, cosx, siny, cosy, sinz, cosz: TFloatType): T3DCoordinate;
// Rotates a vector around the origin
function ScalePoint(Scale: TFloatType; P: T3DCoordinate): T3DCoordinate;
// Scales a vector
function SetPlane(a, b, c, d: TFloatType): T3DPlane;
function SetPoint(X, Y, Z: TFloatType): T3DCoordinate;
procedure SortFloatArray(var FloatArray: TFloatArray;
  var N: integer);        // sorts an array with floatingpoint values and removes double entries
function Space(Index: integer): string;
// Outputs a string with a number of spaces
function SquaredDistPP(P1, P2: T3DCoordinate): TFloatType;
// calculates the squared distance between two points
function Subtract(AVec1, AVec2: T3DCoordinate): T3DCoordinate;
// subtract two vectors
function Truncate(Value: TFloatType; Maxlength: integer): string;
// Convert a floatingpoint to a string value with a max. number of specified decimals All trailing zeros will be removed
function UnifiedNormal(P1, P2, P3: T3DCoordinate): T3DCoordinate;
// calculate the normal of a plane defined by points P1,P2,P3 and scale to unit-length
function UnitVector(P: T3DCoordinate): T3DCoordinate;
// Scale a vector sucht that it's length is 1.0
function VectorLength(Normal: T3DCoordinate): TFloatType;
// Calculate the length of a vector
function VolStr(Units: TFreeUnitType): string;
// Returns a string value with the volume units
function DensStr(Units: TFreeUnitType): string;
// Returns a string value with the density units
function ViscStr(Units: TFreeUnitType): string;
// Returns a string value with the viscosity units
function VolumeToDisplacement(Volume, Density, AppCoeff: TFloatType;
  Units: TFreeUnitType): TFloatType; // Converts a volume to displacement
function WeightStr(Units: TFreeUnitType): string;
// Returns a string value with the weight units
function DegrStr(Units: TFreeUnitType): string;
// Returns a string value with the degr units
function LenMMStr(Units: TFreeUnitType): string;
// Returns a string value with the length (mm or inch) units

var
  DelayedDestroyList: TFreeDestroyList;

procedure Register;

implementation

{$R ViewportCursors.res}


uses FreeLanguageSupport,
  VRMLUnit,
  FreeBackgroundBlendingDlg,
  FreeBackgroundToleranceDlg,
  FreeSaveImageDlg,
  FreeLogger;

const
  crRotate = 1; // Rotation cursor
  crPan = 2; // Pan cursor
  crSetOrigin = 3; // Cursor used when setting the origin of a background image
  crSetScale = 4; // Cursor used when setting the scale of a background image
  crTranspCol = 5;
// Cursor used when setting the transparent color of a background image

{$I FreeGometry_Functions.inc}
{$I FreeAlphaBuffer.inc}
{$I FreeBackgroundImage.inc}
{$I FreeViewport.inc}
{$I FreeDevelopedPatch.inc}
{$I FreeEntity.inc}
{$I FreeSpline.inc}
{$I FreeNamedObject.inc}
{$I FreeNURBSurface.inc}
{$I FreeSubdivisionBase.inc}
{$I FreeSubdivisionControlCurve.inc}
{$I FreeSubdivisionLayer.inc}
{$I FreeSubdivisionPoint.inc}
{$I FreeSubdivisionControlPoint.inc}
{$I FreeSubdivisionControlPointGroup.inc}
{$I FreeSubdivisionEdge.inc}
{$I FreesubdivisionControlEdge.inc}
{$I FreeSubdivisionFace.inc}
{$I FreeSubdivisionControlFace.inc}
{$I FreeSubdivisionSurface.inc}

// -----------------------------------------------------------------------------
constructor TFreeDestroyList.Create;
begin
  inherited Create(true,false);
end;

procedure TFreeDestroyList.DestroyAll;
var
  O: TObject;
  I: integer;
begin
  for I:=0 to Count-1 do
  if Assigned(Items[I]) then
    try
       Items[I].Free;

    {  while Count > 0 do
      begin
        I:=Count-1;
        O := TObject(Items[I]);
        if Assigned(O) then
        begin
          O.Destroy;
          Delete(I);
        end;
      end; }
    finally
    end;

  Clear;
end;
// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('FreeShip', [TFreeViewport]);
end;

initialization
  Randomize;
  DelayedDestroyList := TFreeDestroyList.Create;
end.
