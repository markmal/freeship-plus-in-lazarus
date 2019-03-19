{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2006-2011, by Timoshenko V.F.                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : www.freeship-plus.pisem.su                                     }
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
  buttons,
  StdCtrls,
  Printers,
  StrUtils,
  FreeTypes,
  FreeVersionUnit,
  FreeFileBuffer,
  ExtCtrls,
{$IFDEF VER3}
 LazUTF8,
 LazFileUtils,
{$ELSE}
 FileUtil, //deprecated
{$ENDIF}
  FreeBitmapFormatHelper;

const Foot                          = 0.3048;
      Lbs                           = 0.44642857;
      WeightConversionFactor        = (1000/Lbs)/((1/Foot)*(1/Foot)*(1/Foot));
      IncrementSize                 = 25;                            // amount of points which is automaticly allocated extra memory for
      Decimals                      = 4;                             // When weilding points together this is the accuracy for comparing points
      PixelCountMax                 = 32768;                         // used for faster pixel acces when shading to viewport
      ZBufferScaleFactor            = 1.004;                         // Offset for hidden-line drawing when drawing ontop of shaded triangles
      Zoomfactor                    = 1.02;

      DXFLayerColors           : array[1..255] of TColor = ($0000FF, $00FFFF, $00FF00, $FFFF00, $FF0000, $FF00FF, $000000, $808080, $C0C0C0, $0000FF,
                                                            $7F7FFF, $0000A5, $5252A5, $00007F, $3F3F7F, $00004C, $26264C, $000026, $131326, $003FFF,
                                                            $7F9FFF, $0029A5, $5267A5, $001F7F, $3F4F7F, $00134C, $262F4C, $000926, $131726, $007FFF,
                                                            $7FBFFF, $0052A5, $527CA5, $003F7F, $3F5F7F, $00264C, $26394C, $001326, $131C26, $00BFFF,
                                                            $7FDFFF, $007CA5, $5291A5, $005F7F, $3F6F7F, $00394C, $26424C, $001C26, $132126, $00FFFF,
                                                            $7FFFFF, $00A5A5, $52A5A5, $007F7F, $3F7F7F, $004C4C, $264C4C, $002626, $132626, $00FFBF,
                                                            $7FFFDF, $00A57C, $52A591, $007F5F, $3F7F6F, $004C39, $264C42, $00261C, $132621, $00FF7F,
                                                            $7FFFBF, $00A552, $52A57C, $007F3F, $3F7F5F, $004C26, $264C39, $002613, $13261C, $00FF3F,
                                                            $7FFF9F, $00A529, $52A567, $007F1F, $3F7F4F, $004C13, $264C2F, $002609, $132617, $00FF00,
                                                            $7FFF7F, $00A500, $52A552, $007F00, $3F7F3F, $004C00, $264C26, $002600, $132613, $3FFF00,
                                                            $9FFF7F, $29A500, $67A552, $1F7F00, $4F7F3F, $134C00, $2F4C26, $092600, $172613, $7FFF00,
                                                            $BFFF7F, $52A500, $7CA552, $3F7F00, $5F7F3F, $264C00, $394C26, $132600, $1C2613, $BFFF00,
                                                            $DFFF7F, $7CA500, $91A552, $5F7F00, $6F7F3F, $394C00, $424C26, $1C2600, $212613, $FFFF00,
                                                            $FFFF7F, $A5A500, $A5A552, $7F7F00, $7F7F3F, $4C4C00, $4C4C26, $262600, $262613, $FFBF00,
                                                            $FFDF7F, $A57C00, $A59152, $7F5F00, $7F6F3F, $4C3900, $4C4226, $261C00, $262113, $FF7F00,
                                                            $FFBF7F, $A55200, $A57C52, $7F3F00, $7F5F3F, $4C2600, $4C3926, $261300, $261C13, $FF3F00,
                                                            $FF9F7F, $A52900, $A56752, $7F1F00, $7F4F3F, $4C1300, $4C2F26, $260900, $261713, $FF0000,
                                                            $FF7F7F, $A50000, $A55252, $7F0000, $7F3F3F, $4C0000, $4C2626, $260000, $261313, $FF003F,
                                                            $FF7F9F, $A50029, $A55267, $7F001F, $7F3F4F, $4C0013, $4C262F, $260009, $261317, $FF007F,
                                                            $FF7FBF, $A50052, $A5527C, $7F003F, $7F3F5F, $4C0026, $4C2639, $260013, $26131C, $FF00BF,
                                                            $FF7FDF, $A5007C, $A55291, $7F005F, $7F3F6F, $4C0039, $4C2642, $26001C, $261321, $FF00FF,
                                                            $FF7FFF, $A500A5, $A552A5, $7F007F, $7F3F7F, $4C004C, $4C264C, $260026, $261326, $BF00FF,
                                                            $DF7FFF, $7C00A5, $9152A5, $5F007F, $6F3F7F, $39004C, $42264C, $1C0026, $211326, $7F00FF,
                                                            $BF7FFF, $5200A5, $7C52A5, $3F007F, $5F3F7F, $26004C, $39264C, $130026, $1C1326, $3F00FF,
                                                            $9F7FFF, $2900A5, $6752A5, $1F007F, $4F3F7F, $13004C, $2F264C, $090026, $171326, $000000,
                                                            $2D2D2D, $5B5B5B, $898989, $B7B7B7, $B3B3B3);



type

     TShadePoint                   = record                         // Used for drawing to the Z-buffer
                                         X,Y  : Integer;
                                         Z    : TFloatType;
                                         R,G,B: integer;
                                      end;
      TLayerProperties              = record
                                         SurfaceArea           : TFloatType;
                                         Weight                : TFloatType;
                                         SurfaceCenterOfGravity: T3DCoordinate;
                                      end;
      TFreeVertexType               = (svRegular,svCrease,svDart,svCorner);   // Different types of subdivisionvertices
      TFreeCameraType               = (ftWide,ftStandard,ftShortTele,ftMediumTele,ftFarTele); // Different types of camera lenses, corresponding to focalpoints 20mm, 50mm, 90mm, 130mm, 200mm
      TFreeViewType                 = (fvBodyplan,fvProfile,fvPlan,fvPerspective);
      TFreeUnitType                 = (fuMetric,fuImperial);                                               // Switch between metric and imperial units
      TFreeViewportmode             = (vmWireFrame,vmShade,vmShadeGauss,vmShadeDevelopable,vmShadeZebra);
      TFreeSubdivisionMode          = (fmQuadTriangle,fmCatmullClark);
      TFreeAssembleMode             = (amRegular,amNURBS);
      TFreeViewportBackgroundMode   = (emNormal,emSetOrigin,emSetScale,emSetTransparentColor);
      TFreeLight                    = record
                                         Position  : T3DCoordinate;  // position of light in world
                                         Luminance : byte;           // brightness
                                         Ambient   : byte
                                      end;
type TFreeSubdivisionBase           = class;
     TFreeSubdivisionSurface        = class;
     TFreesubdivisionPoint          = class;
     TFreeSubdivisionEdge           = class;
     TFreeSubdivisionFace           = class;
     TFreeSubdivisionControlPoint   = class;
     TFreeSubdivisionControlEdge    = class;
     TFreeSubdivisionControlFace    = class;
     TFreeSubdivisionLayer          = class;
     TFreeViewport                  = class;
     TFreeSpline                    = class;
     TFreeBackgroundImage           = class;
     TFreeFaceGrid                  = record
                                         Faces : array of array of TFreeSubdivisionControlFace;
                                         NCols : Integer;
                                         NRows : Integer
                                      end;
     TFreeFaceArray                 = array of TFreeFaceGrid;
     TFreeSubdivisionGrid           = array of array of TFreeSubdivisionPoint;
     TFreeZBufferRow                = TFloatArray;
     TFreeCoordinateArray           = array of T3DCoordinate;
     TFreeCoordinateGrid            = array of array of T3DCoordinate;
     TFreeIntersectionData          = record // intersections of a spline with a plane
                                         NumberOfIntersections : Integer;
                                         Points                : TFreeCoordinateArray;
                                         Parameters            : TFloatArray;
                                      end;
     TUnrolledPoint                 = record // points with extra information, used for unrolling plates
                                         Coordinate   : T2DCoordinate;
                                      end;
     TOnRequestExtentsEvent         = procedure(Sender: TObject;var Min,Max:T3DCoordinate) of object; // Event from TFreeviewport, which is raised when the viewport initializes and needs the
                                                                                                      // bounding box of the min/max coordinates of the 3D model
     TChangeActiveLayerEvent        = procedure(Sender: TObject;Layer:TFreeSubdivisionLayer) of Object;   // Event raised when the active lyers has been changed

     TAlphaBlendData              = record
                                         R,G,B  : Byte;
                                         Alpha  : byte;
                                         zvalue : single;
                                      end;
     TAlphaBlendPixelArray        = record
                                         Number    : byte;
                                         Capacity  : byte;
                                         Data      : array of TAlphaBlendData;
                                      end;
     TAlphaBlendArray             = record
                                         First,Last:Integer;
                                         Pixels    : array of TAlphaBlendPixelArray;
                                      end;

     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeAlphaBuffer                                        }
     {                                                                                                   }
     { Alpha-buffer class used in the shading algorithm                                                  }
     {---------------------------------------------------------------------------------------------------}
     TFreeAlphaBuffer             = class
                                         private
                                            FViewport : TFreeViewport;
                                            FBuffer   : array of TAlphaBlendArray;
                                            FWidth    : Integer;
                                            FHeight   : Integer;
                                            FFirstRow : Integer;
                                            FLastRow  : Integer;
                                         public
                                            procedure AddPixelData(X,Y:Integer;R,G,B,Alpha:Byte;Z:Single);
                                            procedure Initialize;
                                            procedure Draw;
                                      end;

     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeZBuffer                                            }
     {                                                                                                   }
     { Z-buffer class used in the shading algorithm                                                      }
     {---------------------------------------------------------------------------------------------------}
     TFreeZBuffer                   = class
                                         private
                                            FViewport : TFreeViewport;
                                            FBuffer   : array of TFreeZBufferRow;
                                            FWidth    : Integer;
                                            FHeight   : Integer;
                                         public
                                            procedure Initialize;
                                      end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeBackgroundImage                                    }
     {                                                                                                   }
     { Background image properties for use in a viewport                                                 }
     {                                                                                                   }
     {---------------------------------------------------------------------------------------------------}
     TFreeBackgroundImage   = class(TPersistent)
                                 private
                                    FOwner                     : TFreeViewport;
                                    FBitmap                    : TBitmap;
                                    FOrigin                    : TPoint;
                                    FScale                     : TFloatType;
                                    FTransparent               : Boolean;
                                    FTransparentColor          : TColor;
                                    FVisible                   : Boolean;
                                    FShowInView                : TFreeViewType;
                                    FQuality                   : Byte;
                                    FAlpha                     : Byte;
                                    FTolerance                 : Byte;
                                    procedure FSetAlpha(val:Byte);
                                    procedure FSetOrigin(val:TPoint);
                                    procedure FSetTolerance(val:Byte);
                                    procedure FSetTransparent(val:Boolean);
                                    procedure FSetTransparentColor(val:TColor);
                                    procedure FSetVisible(val:Boolean);
                                 public
                                    procedure AssignData(Image:TGraphic;View:TFreeViewType;Origin:TPoint;Scale:TFloatType;Transp:boolean;TranspCol:TColor;Alpha,Quality,Tolerance:Byte;Quiet:Boolean);
                                    procedure Clear;
                                    constructor Create(Viewport:TFreeViewport);
                                    destructor Destroy;                        override;
                                    procedure Draw;
                                    function ImageCoordinate(X,Y:Integer):TPoint;
                                    function TargetRect:TRect;
                                    procedure Open(InitialDir:String);
                                    procedure Save;
                                    procedure SetBlendingValue;
                                    property Origin            : TPoint read FOrigin write FSetOrigin;
                                 published
                                    property Alpha             : byte read FAlpha write FSetAlpha;
                                    property Bitmap            : TBitmap read FBitmap write FBitmap;
                                    property Owner             : TFreeViewport read FOwner write FOwner;
                                    property Quality           : byte read FQuality write FQuality;
                                    property Scale             : TFloatType read FScale write FScale;
                                    property ShowInView        : TFreeViewType read FShowInView write FShowInView;
                                    property Tolerance         : Byte read FTolerance write FSetTolerance;
                                    property Transparent       : Boolean read FTransparent write FSetTransparent;
                                    property TransparentColor  : TColor read FTransparentColor write FSetTransparentColor;
                                    property Visible           : Boolean read FVisible write FSetVisible;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeViewport                                           }
     {                                                                                                   }
     { This is a 3D drawingcanvas used for viewing and drawing                                           }
     { It also is the userinterface for editing the hullform.                                            }
     {---------------------------------------------------------------------------------------------------}
     TFreeViewport          = class(TCustomPanel)
                                 private
                                    FAngle                     : TFloatType;
                                    FDistance                  : TFloatType;              // The distance from the model to the camera, determined by the field of view
                                    FElevation                 : TFloatType;
                                    FFieldOfView               : TFloatType;              // The field of view in degrees, default=50 degr. which corresponds with the human eye
                                    FDoubleBuffer              : Boolean;                 // Double buffering prevents flickering when redrawing the viewport
                                    FPrinting                  : Boolean;                 // Switch to determine if the viewport is drawing to the screen, or to the printer (or bitmap)
                                    FPrintResolution           : Integer;                 // horizontal reolution of the printer
                                    FDestinationWidth          : Integer;                 // Destinationwidth of the canvas when not drawing to the screen
                                    FDestinationHeight         : Integer;                 // DestinationHeight of the canvas when not drawing to the screen
                                    FMin3D,FMax3D              : T3DCoordinate;
                                    FMidPoint                  : T3DCoordinate;           // Midpoint of the boundarybox determined by FMin3D and FMax3D. This point is used as centerpoint for rotating the 3D model
                                    FMargin                    : TFloatType;              // margin around to viewport to keep clear;
                                                                                          // and it also is the direction at which the camera looks
                                    FBackgroundMode            : TFreeViewportBackgroundMode;
                                    FViewType                  : TFreeViewType;           // Switch to sideview, frontview, topview or perspective view
                                    FCameraLocation            : T3DCoordinate;           // Position of the camera, following from the field of view and the distance of the camera
                                    FCameraType                : TFreeCameraType;         // Determines the focalpoint of the camera
                                    FCosAngle,FSinAngle        : TFloatType;              // Pre calculated values to speed-up the rotating of point in the perspective-projection
                                    FCosElevation,FSinElevation: TFloatType;              // Pre calculated values to speed-up the rotating of point in the perspective-projection
                                    FScale                     : TFloatType;              // Scale for projecting the 2D coordinates to the viewport
                                    FPrintScaleFactor          : TFloatType;              // Scale factor to adapt penwith depending on printsize and printresolution
                                    FZoom                      : TFloatType;
                                    FViewportMode              : TFreeViewportmode;       // Switch between wireframe mode or differentypes of shading
                                    FDrawingCanvas             : TCanvas;
                                    FDrawingBuffer             : TBitmap;                 // Drawingbuffer to prevent flickering. Everything is drawn on this bitmap, and then copied to the screen
                                    FBitmapFormatHelper        : TFreeBitmapFormatHelper;
                                    FOnMouseDown               : TMouseEvent;
                                    FOnMouseUp                 : TMouseEvent;
                                    FOnMouseEnter              : TNotifyEvent;
                                    FOnMouseMove               : TMouseMoveEvent;
                                    FOnMouseLeave              : TNotifyEvent;
                                    FOnRedraw                  : TNotifyEvent;
                                    FOnChangeViewType          : TNotifyEvent;
                                    FOnRequestBackgroundImage  : TNotifyEvent;
                                    FOnRequestExtents          : TOnRequestExtentsEvent;
                                    FScreencenter              : TPoint;
                                    FPan                       : TPoint;
                                    FPreviousPosition          : TPoint;
                                    FBackgroundOrigin          : TPoint;
                                    FBackgroundImage           : TFreeBackgroundImage;
                                    // shade data
                                    FZBuffer                   : TFreeZBuffer;
                                    FAlphaBuffer               : TFreeAlphaBuffer;
                                    FLight                     : TFreeLight;
                                    FHorScrollbar              : TScrollBar;
                                    FVertScrollbar             : TScrollBar;
                                    FOnChangeBackgroundImage   : TNotifyEvent;
                                    FUpdating                  : boolean;
                                    function  FGetBrushColor:TColor;
                                    function  FGetBrushStyle:TBrushStyle;
                                    function  FGetFontColor:TColor;
                                    function  FGetFontName:string;
                                    function  FGetFontSize:Integer;
                                    function  FGetFontHeight:integer;
                                    function  FGetPenColor:TColor;
                                    function  FGetPenStyle:TPenStyle;
                                    function  FGetPenWidth:integer;
                                    function  FGetPrinting:Boolean;
                                    procedure FSetAngle(Val:TFloatType);
                                    procedure FSetBackgroundMode(val:TFreeViewportBackgroundMode);
                                    procedure FSetBrushColor(Val:TColor);
                                    procedure FSetBrushStyle(Val:TBrushStyle);
                                    procedure FSetCameraType(Val:TFreeCameraType);
                                    procedure FSetElevation(Val:TFloatType);
                                    procedure FSetFontColor(Val:TColor);
                                    procedure FSetFontName(val:string);
                                    procedure FSetFontSize(val:integer);
                                    procedure FSetFontHeight(val:integer);
                                    procedure FSetHorScrollbar(val:TScrollbar);
                                    procedure FSetVertScrollbar(val:TScrollbar);
                                    procedure FSetMargin(Val:TFloatType);
                                    procedure FSetPan(Val:TPoint);
                                    procedure FSetPenColor(Val:TColor);
                                    procedure FSetPenStyle(Val:TPenStyle);
                                    procedure FSetPenWidth(Val:integer);
                                    function  FGetPrintScaleFactor:TFloatType;
                                    procedure FSetViewType(Val:TFreeViewType);
                                    procedure FSetViewportMode(Val:TFreeViewportMode);
                                    procedure FHorScrollbarChange(sender:TObject);
                                    procedure FVertScrollbarChange(sender:TObject);
                                    procedure WMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
                                    procedure WMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
                                 protected
                                    procedure Paint;                                                        override;
                                    procedure Resize;                                                       override;
                                    procedure KeyPress(var Key: Char);                                      override;
                                    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
                                    procedure MouseMove(Shift:TShiftState;X,Y:Integer);                     override;
                                    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);   override;
                                    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;     MousePos: TPoint): Boolean; override;
                                    procedure GetPixel(X,Y:Integer; out R,G,B:byte);
                                    procedure SetPixel(X,Y:Integer; R,G,B:byte);
                                 public
                                    constructor Create(AOwner:TComponent);                                  override;
                                    destructor Destroy;                                                     override;
                                    procedure DrawLineToZBuffer(Point1,Point2:T3DCoordinate;R,G,B:byte); virtual;
                                    procedure InitializeViewport(Min,Max:T3DCoordinate);                    virtual;
                                    procedure Print(Units:TFreeUnitType;AskPrintScale:Boolean;Jobname:string); virtual;
                                    function Project(P:T3DCoordinate):TPoint;
                                    function ProjectBack(P:TPoint;Input:T3DCoordinate):T3DCoordinate;
                                    function ProjectBackTo2D(P:TPoint):T2DCoordinate;        // Takes the cursor position and projects it to 2D object space
                                    function ProjectToZBuffer(P:T3DCoordinate):TShadePoint;  overload;virtual;// Projects a 3D point to the screen and calculate it's Z-value for the Z-buffer
                                    function ProjectToZBuffer(Scale:TFloatType;P:T3DCoordinate):TShadePoint;  reintroduce;overload;// Projects a 3D point with a certain z-buffer offset to the screen, used for drawing lines on top of shaded surfaces
                                    function RotatedPoint(P:T3DCoordinate):T3DCoordinate;
                                    function RotatedPointBack(P:T3DCoordinate):T3DCoordinate;

                                    procedure BeginUpdate;
                                    procedure EndUpdate;

                                    procedure LineTo(x,y:integer); virtual;
                                    procedure MoveTo(x,y:integer); virtual;
                                    procedure Line(x1,y1, x2,y2:integer); virtual;
                                    procedure Rectangle(x1,y1, x2,y2:integer); virtual; overload;
                                    procedure Rectangle(rect:TRect); virtual; overload;
                                    procedure Ellipse(x1,y1, x2,y2:integer); virtual;
                                    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer); virtual;

                                    procedure Polyline(const Points: array of TPoint);                      virtual;
                                    procedure Polygon(const Points: array of TPoint);                      virtual;
                                    procedure SaveAsBitmap(Filename:string;const ShowDialog:boolean=true);   virtual;
                                    procedure SetPenWidth(Width:integer);                                    virtual;
                                    procedure StretchDraw(DestRect:TRect; bmp:TBitmap);                      virtual;
                                    Procedure ShadedColor(Dp:single;R,G,B:byte;var ROut,GOut,BOut:byte);     virtual;
                                    procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R,G,B:byte;Alpha:byte);overload;virtual;
                                    procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;C1,C2,C3:Extended); virtual;//reintroduce;overload;
                                    procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R1,G1,B1,R2,G2,B2,R3,G3,B3:byte); virtual;//reintroduce;overload;
                                    function  TextWidth(val:string):integer; virtual;
                                    function  TextHeight(val:string):integer; virtual;
                                    procedure TextOut(x,y:integer; val:string); virtual;
                                    procedure ZoomIn; virtual;
                                    procedure ZoomExtents; virtual;
                                    procedure ZoomOut; virtual;

                                    property AlphaBuffer          : TFreeAlphaBuffer read FAlphaBuffer;
                                    property BackgroundMode       : TFreeViewportBackgroundMode read FBackgroundMode write FSetBackgroundMode;
                                    property BrushColor           : TColor read FGetBrushColor write FSetBrushColor;
                                    property BrushStyle           : TBrushStyle read FGetBrushStyle write FSetBrushStyle;
                                    property CameraLocation       : T3DCoordinate read FCameraLocation;
                                    property DrawingCanvas        : TCanvas read FDrawingCanvas write FDrawingCanvas;
                                    property FieldOfView          : TFloatType read FFieldOfView;
                                    property FontColor            : TColor read FGetFontColor write FSetFontColor;
                                    property FontName             : string read FGetFontname write FSetFontName;
                                    property FontSize             : integer read FGetFontSize write FSetFontSize;
                                    property FontHeight           : integer read FGetFontHeight write FSetFontHeight;

                                    property Light                : TFreeLight read FLight;
                                    property Max3D                : T3DCoordinate read FMax3D;
                                    property Min3D                : T3DCoordinate read FMin3D;
                                    property PenColor             : TColor read FGetPenColor write FSetPenColor;
                                    property PenStyle             : TPenStyle read FGetPenStyle write FSetPenStyle;
                                    property PenWidth             : integer read FGetPenWidth write FSetPenWidth;
                                    property Printing             : Boolean read FGetPrinting;
                                    property PrintResolution      : integer read FPrintResolution;
                                    property PrintScaleFactor     : TFloatType read FGetPrintScaleFactor;
                                    property Scale                : TFloatType read FScale;
                                    property ZBuffer              : TFreeZBuffer read FZBuffer;
                                    property Zoom                 : TFloatType read FZoom;
                                    property Pan                  : TPoint read FPan write FSetPan;
                                 published
                                    property Angle                : TFloatType read FAngle write FSetAngle;
                                    property Align;
                                    property BackgroundImage      : TFreeBackgroundImage read FBackgroundImage write FBackgroundImage;
                                    property BevelInner;
                                    property BevelOuter;
                                    property BorderStyle;
                                    property CameraType           : TFreeCameraType read FCameraType write FSetCameraType;
                                    property Color;
                                    property DestinationWidth     : Integer read FDestinationWidth write FDestinationWidth;
                                    property DestinationHeight    : Integer read FDestinationHeight write FDestinationHeight;
                                    property DoubleBuffer         : boolean read FDoubleBuffer write FDoubleBuffer;
                                    property Elevation            : TFloatType read FElevation write FSetElevation;
                                    property HorScrollbar         : TScrollBar read FHorScrollbar write FSetHorScrollbar;
                                    property Margin               : TFloatType read FMargin write FSetMargin;
                                    property PopupMenu;
                                    property VertScrollbar        : TScrollBar read FVertScrollbar write FSetVertScrollbar;
                                    property Visible;
                                    property ViewType             : TFreeViewtype read FViewType write FSetViewType;
                                    property ViewportMode         : TFreeViewportmode read FViewportMode write FSetViewportMode;       // Switch between wireframe mode or differentypes of shading
                                    property OnChangeBackground   : TNotifyEvent read FOnChangeBackgroundImage write FOnChangeBackgroundImage;
                                    property OnChangeViewType     : TNotifyEvent read FOnChangeViewType write FOnChangeViewType;
                                    property OnKeyDown;
                                    property OnKeyPress;
                                    property OnKeyUp;
                                    property OnMouseDown          : TMouseEvent read FOnMouseDown write FOnMouseDown;
                                    property OnMouseUp            : TMouseEvent read FOnMouseUp   write FOnMouseUp;
                                    property OnMouseMove          : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
                                    property OnMouseEnter         : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
                                    property OnMouseLeave         : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
                                    property OnMouseWheel;
                                    Property OnResize;
                                    property OnRedraw             : TNotifyEvent read FOnRedraw write FOnRedraw;
                                    property OnRequestBackgroundImage  : TNotifyEvent read FOnRequestBackgroundImage write FOnRequestBackgroundImage;
                                    property OnRequestExtents     : TOnRequestExtentsEvent read FOnRequestExtents write FOnRequestExtents;
                              end;


     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeDevelopedPatch                                     }
     {                                                                                                   }
     { Unrolled Subdivision control face                                                                 }
     {---------------------------------------------------------------------------------------------------}
     TFreeDevelopedPatch    = class
                                 Private
                                    FOwner            : TFreeSubdivisionLayer;
                                    FName             : string;
                                    FConnectedMirror  : TFreeDevelopedPatch;
                                    FPoints           : TFasterList;// All original 3D points
                                    FEdges            : TFasterList;// All original edges
                                    FDoneList         : TFasterList;// List containing developed faces in chronological order
                                    FBoundaryEdges    : TFasterList;// All edges forming the boundary edges of the developed plate, sorted in the correct order
                                    FStations         : TFasterList;
                                    FWaterlines       : TFasterList;
                                    FButtocks         : TFasterList;
                                    FDiagonals        : TFasterList;
                                    FCorners          : TFasterList;// contains all cornerpoints for dimensioning reasons
                                    FRotation         : TFloatType;
                                    FMirrorPlane      : T3DPlane;
                                    FMirror           : Boolean;
                                    FMin2D            : T2DCoordinate;
                                    FMax2D            : T2DCoordinate;
                                    FTranslation      : T2DCoordinate;
                                    FMaxAreaError     : extended;
                                    FTotalAreaError   : extended;
                                    FXGrid            : TFloatType;
                                    FYGrid            : TFloatType;
                                    FCos,FSin         : TFloatType;
                                    F2DCoordinates    : array of TUnrolledPoint;
                                    FEdgeErrors       : array of double;
                                    // Visibility options
                                    FVisible          : boolean;
                                    FShowSolid        : Boolean;     // Fills the surface with the layer color
                                    FShowPartName     : Boolean;     // draws the name of the surface at the center
                                    FShowBoundingBox  : Boolean;     // Draws a boundary box around the surface
                                    FShowInteriorEdges: boolean;     // Draw the interior edges (none crease edges)
                                    FShowStations     : Boolean;
                                    FShowButtocks     : Boolean;
                                    FShowDiagonals    : Boolean;
                                    FShowWaterlines   : Boolean;
                                    FShowErrorEdges   : Boolean;
                                    FShowDimensions   : Boolean;
                                    FMirrorOnScreen   : Boolean;
                                    FShadeSubmerged   : Boolean;
                                    FNoIterations     : Integer;
                                    FUnits            : TFreeUnitType;
                                    FDimFontColor     : TColor;
                                    FDimFontName      : String;
                                    FDimFontSize    : Integer;

                                    function FGetShowErrorEdges:Boolean;
                                    function FGetMidPoint:T2DCoordinate;
                                    function FGetMinError:Extended;
                                    function FGetMaxError:Extended;
                                    function FGetMirrorPoint(index:Integer):T3DCoordinate;
                                    function FGetPoint(index:Integer):T3DCoordinate;
                                    procedure FSetRotation(Val:TFloatType);
                                    procedure FSetTranslation(Val:T2DCoordinate);
                                    procedure FSetMirrorOnScreen(val:Boolean);
                                 public
                                    procedure Assign(Org:TFreeDevelopedPatch;Mirror:Boolean);
                                    procedure Clear;
                                    function ConvertTo3D(P:T2DCoordinate):T3DCoordinate;
                                    constructor Create(Owner:TFreeSubdivisionLayer);
                                    destructor Destroy; override;
                                    function DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
                                    procedure Draw(Viewport:TFreeViewport);
                                    procedure Extents(var Min,Max:T3DCoordinate);
                                    procedure IntersectPlane(Plane:T3DPlane;Color:TColor);
                                    procedure SaveToDXF(Strings:TStringList);
                                    procedure SaveToTextFile(Strings:TStringList);
                                    procedure Unroll(ControlFaces:TFasterList);
                                    property MaxAreaError         : extended read FMaxAreaError;
                                    property MaxError             : extended read FGetMaxError;
                                    property MidPoint             : T2DCoordinate read FGetMidPoint;
                                    property MinError             : extended read FGetMinError;
                                    property MirrorOnScreen       : boolean read FMirrorOnScreen write FSetMirrorOnScreen;
                                    property Name                 : string read FName write FName;
                                    property NumberOfIterations   : Integer read FNoIterations;
                                    property Owner                : TFreeSubdivisionLayer read FOwner;
                                    property MirrorPoint[index:Integer] : T3DCoordinate read FGetMirrorPoint;
                                    property Point[index:Integer] : T3DCoordinate read FGetPoint;
                                    property Rotation             : TFloatType read FRotation write FSetRotation;
                                    property ShadeSubmerged       : Boolean read FShadeSubmerged write FShadeSubmerged;
                                    property ShowBoundingBox      : boolean read FShowBoundingBox write FShowBoundingBox;
                                    property ShowButtocks         : Boolean read FShowButtocks write FShowButtocks;
                                    property ShowDiagonals        : Boolean read FShowDiagonals write FShowDiagonals;
                                    property ShowDimensions       : boolean read FShowDimensions write FShowDimensions;
                                    property ShowErrorEdges       : boolean read FGetShowErrorEdges write FShowErrorEdges;
                                    property ShowInteriorEdges    : Boolean read FShowInteriorEdges write FShowInteriorEdges;
                                    property ShowPartName         : boolean read FShowPartName write FShowPartName;
                                    property ShowSolid            : boolean read FShowSolid write FShowSolid;
                                    property ShowStations         : Boolean read FShowStations write FShowStations;
                                    property ShowWaterlines       : Boolean read FShowWaterlines write FShowWaterlines;
                                    property DimFontColor         : TColor  read FDimFontColor write FDimFontColor;
                                    property DimFontName          : String  read FDimFontName write FDimFontName;
                                    property DimFontSize          : integer  read FDimFontSize write FDimFontSize;
                                    property TotalAreaError       : extended read FTotalAreaError;
                                    property Translation          : T2DCoordinate read FTranslation write FSetTranslation;
                                    property Units                : TFreeUnitType read FUnits write FUnits;
                                    property Visible              : Boolean read FVisible write FVisible;
                                    property XGrid                : TFloatType read FXGrid write FXGrid;
                                    property YGrid                : TFloatType read FYGrid write FYGrid;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeEntity                                             }
     {                                                                                                   }
     { This is the base class of all 3D entities in the project                                          }
     {---------------------------------------------------------------------------------------------------}
     TFreeEntity            = class
                                 private
                                    FBuild                     : Boolean;                                                  // Flag to check if the entity has already been build
                                    FMin,FMax                  : T3DCoordinate;                                            // The min/max boundary coordinates of the entity after it has been build
                                    FPenWidth                  : byte;                                                     // Pen thickness to use when drawing
                                    FColor                     : TColor;                                                   // Color when drawing
                                    FPenstyle                  : TPenStyle;                                                // Pen style for drawing the line
                                    function  FGetMin:T3DCoordinate;                                        virtual;
                                    function  FGetMax:T3DCoordinate;                                        virtual;
                                    procedure FSetBuild(Val:Boolean);                                       virtual;
                                 public
                                    constructor Create;                                                     virtual;
                                    procedure   Clear;                                                      virtual;
                                    destructor  Destroy;                                                    override;
                                    procedure   Extents(Var Min,Max : T3DCoordinate);                       virtual;
                                    procedure   Draw(Viewport:TFreeViewport);                               virtual;
                                    procedure   Rebuild;                                                    virtual;
                                    property    Build                   : Boolean read FBuild write FSetBuild;
                                    property    Color                   : TColor read FColor write FColor;
                                    property    Min                     : T3DCoordinate read FGetMin;
                                    property    Max                     : T3DCoordinate read FGetMax;
                                    property    PenStyle                : TPenStyle read FPenStyle write FPenStyle;        // Pen style
                                    property    PenWidth                : byte read FPenWidth write FPenWidth;             // Pen thickness when drawing on screen
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeSpline                                             }
     {                                                                                                   }
     { 3D CSpline                                                                                        }
     {                                                                                                   }
     { Copied from page 107 of the book: "Numerical recipes in fortan 77"                                }
     { Url: http://www.library.cornell.edu/nr/bookfpdf/f3-3.pdf                                          }
     { Modified to use centripetal parametrisation for smoother interpolation and to accept              }
     { knuckles in the controlpoints                                                                     }
     {---------------------------------------------------------------------------------------------------}
     TFreeSpline            = class(TFreeEntity)
                                    FCapacity      : Integer;                                                           // Number of points for which memory has been allocated
                                    FNoPoints      : Integer;                                                           // Actual number of points present
                                    FFragments     : Integer;                                                           // Number of straight-line segments used when drawing the curve
                                    FShowCurvature : Boolean;
                                    FShowPoints    : Boolean;
                                    FCurvatureScale: TFloatType;                                                        // scale factor used to increase or decrease the scale of the curvature plot
                                    FCurvatureColor: TColor;                                                            // Color used for draing the curvature plot
                                    FTotalLength   : TFloatType;
                                    FPoints        : TFreeCoordinateArray;                                              // Array containing all controlpoints
                                    FKnuckles      : array of boolean;
                                    FParameters    : array of TFloatType;
                                    FDerivatives   : TFreeCoordinateArray;
                                    function  FGetFragments:Integer;
                                    function  FGetKnuckle(Index:integer):Boolean;
                                    function  FGetParameter(Index:integer):TFloatType;
                                    function  FGetPoint(Index:Integer):T3DCoordinate;
                                    procedure FSetBuild(val:boolean);                                       override;
                                    procedure FSetCapacity(Val:Integer);
                                    procedure FSetFragments(Val:Integer);
                                    procedure FSetKnuckle(Index:integer;Value:Boolean);
                                    procedure FSetPoint(Index:Integer;P:T3DCoordinate);
                                 public
                                    procedure   Add(P:T3DCoordinate);                                       // add a new point to the curve
                                    procedure   Assign(Spline:TFreeSpline);                                 // Copy all data from another spline
                                    function    CoordLength(T1,T2:TFloatType):TFloatType;
                                    function    ChordlengthApproximation(Percentage:TFloatType):extended;
                                    procedure   Clear;                                                      override;
                                    constructor Create;                                                     override;
                                    function    Curvature(Parameter:TFloatType;var Value,Normal:T3DCoordinate):TFloatType;
                                    procedure   DeletePoint(Index:Integer);
                                    function    DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;virtual;
                                    procedure   Draw(Viewport:TFreeViewport);                               override;
                                    function    FirstDerive(Parameter:TFloatType):T3DCoordinate;
                                    procedure   Insert(Index:Integer;P:T3DCoordinate);
                                    procedure   InsertSpline(Index:Integer;Invert,DuplicatePoint:Boolean;Source:TFreeSpline);
                                    function    IntersectPlane(Plane:T3DPlane;var Output:TFreeIntersectionData):Boolean;
                                    procedure   InvertDirection;                                            // invert the direction of the controlpoints and knuckles
                                    procedure   LoadBinary(Source:TFreeFileBuffer);                         virtual;
                                    procedure   Rebuild;                                                    override;
                                    procedure   SaveBinary(Destination:TFreeFileBuffer);                    virtual;
                                    procedure   SaveToDXF(Strings:TStringList;Layername:string;SendMirror:Boolean);
                                    function    SecondDerive(Parameter:TFloatType):T3DCoordinate;
                                    function    Simplify(Criterium:TFloatType):Boolean;                     // Remove points that do not contribute significantly to the shape
                                    function    Value(Parameter:extended):T3DCoordinate;
                                    property    Capacity                : Integer read FCapacity write FSetCapacity;
                                    property    CurvatureColor          : TColor read FCurvatureColor write FCurvatureColor;
                                    property    CurvatureScale          : TFloatType read FCurvatureScale write FCurvatureScale;
                                    property    Fragments               : integer read FGetFragments write FSetFragments;
                                    property    Knuckle[Index:integer]  : Boolean  read FGetKnuckle write FSetKnuckle;
                                    property    NumberOfPoints          : Integer read FNoPoints;
                                    property    Parameter[Index:integer]: TFloatType read FGetParameter;
                                    property    Point [Index:Integer]   : T3DCoordinate read FGetPoint write FSetPoint;
                                    property    ShowCurvature           : Boolean read FShowCurvature write FShowCurvature;
                                    property    ShowPoints              : boolean read FShowPoints write FShowPoints;
                                    property    TotalLength             : TFloatType read FTotalLength;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeNURBSurface                                       }
     {--------------------------------------------------------------------------------------------------}
     TFreeNURBSurface         = class(TFreeEntity)
                                 private
                                    FColCount            : integer;
                                    FRowCount            : integer;
                                    FColCapacity         : integer;
                                    FRowCapacity         : integer;
                                    FColDegree           : Integer;
                                    FRowDegree           : Integer;
                                    FColKnots            : TFloatArray;
                                    FRowKnots            : TFloatArray;
                                    FControlPoints       : TFreeCoordinateGrid;
                                    function  FGetpoint(Col,Row:Integer):T3DCoordinate;
                                    procedure FSetColDegree(Val:Integer);
                                    procedure FSetRowDegree(Val:Integer);
                                    procedure FSetPoint(Col,Row:Integer;Val:T3DCoordinate);
                                    procedure FSetColCapacity(Val:integer);
                                    procedure FSetRowCapacity(Val:integer);
                                 protected
                                 public
                                    procedure Clear;                                                     override;
                                    procedure DeleteColumn(Col:Integer);
                                    procedure DeleteRow(Row:Integer);
                                    procedure InsertColKnot(U:TFloatType);
                                    procedure InsertRowKnot(V:TFloatType);
                                    procedure NormalizeKnotVectors;
                                    procedure Rebuild;                                                   override;
                                    procedure SetCapacity(Col,Row:integer);
                                    procedure SetDefaultColKnotvector;
                                    procedure SetDefaultRowKnotvector;
                                    procedure SetUniformColKnotvector;
                                    procedure SetUniformRowKnotvector;
                                    property  ColCapacity            : integer read FColCapacity write FSetColCapacity;
                                    property  ColCount               : integer read FColCount write FColCount;
                                    property  ColDegree              : integer read FColDegree write FSetColDegree;
                                    property  ColKnotVector          : TFloatArray read FColknots;
                                    property  Point[Col,Row:Integer] : T3DCoordinate read FGetpoint write FSetPoint;
                                    property  RowCapacity            : integer read FRowCapacity write FSetRowCapacity;
                                    property  RowCount               : Integer read FRowCount write FRowCount;
                                    property  RowDegree              : integer read FRowDegree write FSetRowDegree;
                                    property  RowKnotVector          : TFloatArray read FRowknots;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionBase                                    }
     {                                                                                                   }
     { TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
     {---------------------------------------------------------------------------------------------------}
     TFreeSubdivisionBase   = class
                                 private
                                    FOwner               : TFreeSubdivisionSurface;
                                 public
                                    constructor Create(Owner:TFreeSubdivisionSurface);                                  virtual;
                                    property    Owner                         : TFreeSubdivisionSurface read FOwner write FOwner;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionControlCurve                            }
     {                                                                                                   }
     { Controlcurves are curves that can be added to the controlnet an are subdivide with the surface.   }
     { The resulting curve therefore lies on the surface, and can be used in the fairing process         }
     {---------------------------------------------------------------------------------------------------}
     TFreeSubdivisionControlCurve = class(TFreeSubdivisionBase)
                                 private
                                    FVisible          : Boolean;
                                    FControlPoints    : TFasterList;
                                    FSubdividedPoints : TFasterList;
                                    FCurve            : TFreeSpline;
                                    FBuild            : Boolean;
                                    function FGetColor:TColor;
                                    function FGetNumberOfControlPoints:Integer;
                                    function FGetControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
                                    function FGetSelected:Boolean;
                                    function FGetVisible:Boolean;
                                    procedure FSetBuild(Val:Boolean);
                                    procedure FSetSelected(val:Boolean);
                                 public
                                    procedure AddPoint(P:TFreesubdivisionPoint);
                                    procedure Clear;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                      override;
                                    procedure Delete;
                                    procedure DeleteEdge(Edge:TFreeSubdivisionControlEdge);
                                    destructor Destroy;                                                     override;
                                    function DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
                                    procedure Draw(Viewport:TFreeViewport);
                                    procedure InsertControlPoint(P1,P2,New:TFreeSubdivisionControlPoint);
                                    procedure InsertEdgePoint(P1,P2,New:TFreeSubdivisionPoint);
                                    procedure LoadBinary(Source:TFreeFileBuffer);
                                    procedure ReplaceVertexPoint(Old,New:TFreeSubdivisionPoint);
                                    procedure SaveBinary(Destination:TFreeFileBuffer);
                                    procedure SaveToDXF(Strings:TStringList);
                                    property Build                      : Boolean read FBuild write FSetBuild;
                                    property Color                      : TColor read FGetColor;
                                    property Curve                      : TFreeSpline read FCurve;
                                    property NumberOfControlPoints      : Integer read FGetNumberOfControlPoints;
                                    property ControlPoint[index:Integer]: TFreeSubdivisionControlPoint read FGetControlPoint;
                                    property Selected                   : boolean read FGetSelected write FSetSelected;       // Property to see if this edge has been selected by the user
                                    property Visible                    : Boolean read FGetVisible write FVisible;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionLayer                                   }
     {                                                                                                   }
     { TFreeSubdivisionLayer is a layer-type class                                                       }
     {                                                                                                   }
     { All individual controlfaces can be assigned to a leyer. Properties such as color,                 }
     { visibility etc. are common for all controlfaces belonging the the same layer                      }
     {---------------------------------------------------------------------------------------------------}
     TFreeSubdivisionLayer  = class
                                    // color, visibility, symmetric, calc intersections/part of hull
                                 private
                                    FOwner               : TFreeSubdivisionSurface; // Pointer to the subdivisionsurface
                                    FLayerID             : integer;              // Unique identification number for internal references
                                    FColor               : TColor;               // Color of this layer
                                    FVisible             : boolean;              // Visibility switch
                                    FDescription         : string;               // Description of the layer, used as user identification
                                    FSymmetric           : Boolean;              // Symmetric patches are mirrored in the centerplane when both halves of the ship are drawn
                                    FDevelopable         : Boolean;              // Developable layers are shaded with Gauss curvature
                                    FUseForIntersections : Boolean;              // If set to true, stations, waterlines, buttocks and diagonals are calculated
                                    FUseInHydrostatics   : boolean;              // If set to true, the panels of this layer will be used for hydrostatic calculations
                                    FShowInLinesplan     : boolean;              // Flag to hide or show this layer in the linesplan
                                    FMaterialDensity     : TFloatType;           // Density of material used to calculate the weight of the surface
                                    FThickness           : TFloatType;           // Also used for weight calculation
                                    FPatches             : TFasterList;          // List containing all controlpatches
                                    FAlphaBlend          : Byte;
                                    function  FGetColor:TColor;
                                    function  FGetCount:Integer;
                                    function  FGetDXFLayername:string;
                                    function  FGetName:string;
                                    function  FGetItems(Index:Integer):TFreeSubdivisionControlFace;
                                    function  FGetLayerIndex:Integer;
                                    function  FGetSurfaceProperties:TLayerProperties;
                                    procedure FSetFDevelopable(Val:Boolean);
                                    procedure FSetName(Val:String);
                                    procedure FSetColor(Val:TColor);
                                    procedure FSetShowInLinesplan(val:boolean);
                                    procedure FSetUseInHydrostatics(val:boolean);
                                    procedure FSetUseForIntersections(val:Boolean);
                                    procedure FSetVisible(Val:Boolean);
                                    procedure FSetSymmetric(Val:Boolean);
                                 public
                                    procedure AddControlFace(ControlFace:TFreeSubdivisionControlFace);
                                    procedure AssignProperties(Source:TFreeSubdivisionLayer);
                                    procedure FindEdgeFaceIntersectionPoints(Layer:TFreeSubdivisionLayer; var NewEdge : TFreeSubdivisionControlEdge; var NewPoints: TFasterList);
                                    procedure FindIntersectionPoints(Layer:TFreeSubdivisionLayer; var NewPoints: TFasterList);
                                    procedure FindIntersectionControlPoints(Layer:TFreeSubdivisionLayer; var NewPoints: TFasterList);
                                    function CalculateIntersectionPoints(Layer:TFreeSubdivisionLayer):Boolean;
                                    constructor Create(Owner:TFreeSubdivisionSurface);
                                    procedure Clear;
                                    function  Delete:Boolean;
                                    procedure DeleteControlFace(ControlFace:TFreeSubdivisionControlFace);
                                    destructor Destroy;                                                                    override;
                                    procedure Draw(Viewport:TFreeViewport);
                                    procedure Extents(var Min,Max:T3DCoordinate);
                                    procedure LoadBinary(Source:TFreeFileBuffer);
                                    procedure LoadFromStream(var LineNr:Integer;Strings:TStringList);
                                    procedure MoveDown;
                                    procedure MoveUp;
                                    procedure SaveToDXF(Strings:TStringList);
                                    procedure SaveBinary(Destination:TFreeFileBuffer);
                                    procedure SaveToStream(Strings:TStringList);
                                    procedure SelectAll;
                                    procedure Unroll(Destination:TFasterList);
                                    property  AlphaBlend                : Byte read FAlphaBlend write FAlphaBlend;
                                    property  Color                     : TColor read FGetColor write FSetColor;
                                    property  Count                     : Integer read FGetCount;
                                    property  Developable               : boolean read FDevelopable write FSetFDevelopable;
                                    property  DXFLayername              : string read FGetDXFLayername;
                                    property  Items[Index:Integer]      : TFreeSubdivisionControlFace read FGetItems;
                                    property  LayerID                   : Integer read FLayerID write FLayerID;
                                    property  LayerIndex                : integer read FGetLayerIndex;
                                    property  MaterialDensity           : TFloatType read FMaterialDensity write FMaterialDensity;
                                    property  Name                      : string read FGetName write FSetName;
                                    property  Owner                     : TFreeSubdivisionSurface read FOwner write FOwner;
                                    property  ShowInLinesplan           : Boolean read FShowInLinesplan write FSetShowInLinesplan;
                                    property  SurfaceProperties         : TLayerProperties read FGetSurfaceProperties;
                                    property  Symmetric                 : Boolean read FSymmetric write FSetSymmetric;
                                    property  Thickness                 : TFloatType read FThickness write FThickness;
                                    property  UseInHydrostatics         : boolean read FUseInHydrostatics write FSetUseInHydrostatics;
                                    property  UseForIntersections       : boolean read FUseForIntersections write FSetUseForIntersections;
                                    property  Visible                   : boolean read FVisible write FSetVisible;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionPoint                                  }
     {--------------------------------------------------------------------------------------------------}
     TFreeSubdivisionPoint    = class(TFreeSubdivisionBase)
                                 private
                                    FFaces                     : TFasterList;
                                    FEdges                     : TFasterList;
                                    FCoordinate                : T3DCoordinate;
                                    FVertexType                : TFreeVertexType;
                                    function FGetEdge(Index:Integer):TFreeSubdivisionEdge;
                                    function FGetCoordinate:T3DCoordinate;
                                    function FGetCurvature:extended;
                                    function FGetFace(Index:Integer):TFreeSubdivisionFace;
                                    function FGetIndex:Integer;                                          virtual;
                                    function FGetIsBoundaryVertex:Boolean;
                                    function FGetNormal:T3DCoordinate;
                                    function FGetNumberOfCurves:Integer;
                                    function FGetNumberOfEdges:integer;
                                    function FGetNumberOfFaces:integer;
                                    function FGetRegularPoint:Boolean;
                                    function FGetLimitPoint:T3DCoordinate;
                                    procedure FSetCoordinate(Val:T3DCoordinate);                         virtual;
                                 public
                                    procedure   AddEdge(Edge:TFreeSubdivisionEdge);
                                    procedure   AddFace(Face:TFreeSubdivisionFace);
                                    function    Averaging:T3DCoordinate;
                                    function    CalculateVertexPoint:TFreeSubdivisionPoint;              virtual;
                                    procedure   Clear;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                   override;
                                    procedure   DeleteEdge(Edge:TFreeSubdivisionEdge);
                                    procedure   DeleteFace(Face:TFreeSubdivisionFace);
                                    destructor  Destroy;                                                 override;
                                    function    IndexOfFace(Face:TFreeSubdivisionFace):Integer;
                                    function    IsRegularNURBSPoint(Faces:TFasterList):Boolean;
                                    property    Coordinate                    : T3DCoordinate read FGetCoordinate write FSetCoordinate;
                                    property    Curvature                     : extended read FGetCurvature;
                                    property    Edge[index:Integer]           : TFreeSubdivisionEdge read FGetEdge;
                                    property    Face[index:Integer]           : TFreeSubdivisionFace read FGetFace;
                                    property    IsBoundaryVertex              : boolean read FGetIsBoundaryVertex;
                                    property    LimitPoint                    : T3DCoordinate read FGetLimitPoint;
                                    property    Normal                        : T3DCoordinate read FGetNormal;
                                    property    NumberOfCurves                : Integer read FGetNumberOfCurves;
                                    property    NumberOfEdges                 : integer read FGetNumberOfEdges;
                                    property    NumberOfFaces                 : integer read FGetNumberOfFaces;
                                    property    RegularPoint                  : Boolean read FGetRegularPoint;
                                    property    VertexIndex                   : integer Read FGetIndex;
                                    property    VertexType                    : TFreeVertexType read FVertexType write FVertexType;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionControlPoint                           }
     {--------------------------------------------------------------------------------------------------}
     TFreeSubdivisionControlPoint = class(TFreeSubdivisionPoint)
                                 private
                                    FLocked  : Boolean;
                                    function FGetColor:TColor;
                                    function FGetIndex:Integer;                                          override;
                                    function FGetIsLeak:boolean;
                                    function FGetSelected:Boolean;
                                    function FGetVisible:Boolean;
                                    procedure FSetLocked(val:Boolean);
                                    procedure FSetSelected(val:Boolean);
                                    procedure FSetCoordinate(Val:T3DCoordinate);                         override;
                                 public
                                    procedure   Collapse;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                     override;
                                    function    DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
                                    procedure   Delete;
                                    procedure   Draw(Viewport:TFreeViewport);
                                    procedure   LoadBinary(Source:TFreeFileBuffer);
                                    procedure   LoadFromStream(var LineNr:Integer;Strings:TStringList);
                                    procedure   SaveBinary(Destination:TFreeFileBuffer);
                                    procedure   SaveToStream(Strings:TStringlist);
                                    property    Color                         : TColor read FGetColor;
                                    property    IsLeak                        : Boolean read FGetIsLeak;
                                    property    Locked                        : Boolean read FLocked write FSetLocked;
                                    property    Selected                      : boolean read FGetSelected write FSetSelected;       // Property to see if this point has been selected by the user
                                    property    Visible                       : Boolean read FGetVisible;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionEdge                                   }
     {--------------------------------------------------------------------------------------------------}
     TFreeSubdivisionEdge     = class(TFreeSubdivisionBase)
                                 private
                                    FStartpoint                : TFreeSubdivisionPoint;
                                    FEndpoint                  : TFreeSubdivisionPoint;
                                    FFaces                     : TFasterList;
                                    FCrease                    : Boolean;
                                    FControlEdge               : Boolean;
                                    FCurve                     : TFreeSubdivisionControlCurve;
                                    function  FGetIndex:Integer;                                         virtual;
                                    function  FGetIsBoundaryEdge:Boolean;                                virtual;
                                    function  FGetFace(Index:Integer):TFreeSubdivisionFace;
                                    function  FGetNumberOfFaces:Integer;
                                    procedure FSetCrease(Val:Boolean);                                   virtual;
                                    function  FGetPreviousEdge:TFreeSubdivisionEdge;
                                    function  FGetNextEdge:TFreeSubdivisionEdge;
                                 public
                                    procedure   AddFace(Face:TFreeSubdivisionFace);
                                    procedure   Assign(Edge:TFreeSubdivisionEdge);                       virtual;
                                    function    CalculateEdgePoint:TFreeSubdivisionPoint;
                                    procedure   Clear;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                   override;
                                    procedure   DeleteFace(Face:TFreeSubdivisionFace);
                                    destructor  Destroy;                                                 override;
                                    function    DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;virtual;
                                    procedure   Draw(DrawMirror:Boolean;Viewport:TFreeViewport);         virtual;
                                    procedure   SwapData;
                                    property    Crease                        : Boolean read FCrease write FSetCrease;
                                    property    Curve                         : TFreeSubdivisionControlCurve read FCurve write FCurve;
                                    property    EdgeIndex                     : integer read FGetIndex;
                                    property    EndPoint                      : TFreeSubdivisionPoint read FEndPoint write FEndPoint;
                                    property    Face[index:integer]           : TFreeSubdivisionFace read FGetFace;
                                    property    IsBoundaryEdge                : Boolean read FGetIsBoundaryEdge;
                                    property    NextEdge                      : TFreeSubdivisionEdge read FGetNextEdge;
                                    property    NumberOfFaces                 : integer read FGetNumberOfFaces;
                                    property    PreviousEdge                  : TFreeSubdivisionEdge read FGetPreviousEdge;
                                    property    StartPoint                    : TFreeSubdivisionPoint read FStartPoint write FStartPoint;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                         TFreesubdivisionControlEdge                              }
     {--------------------------------------------------------------------------------------------------}
     TFreesubdivisionControlEdge= class(TFreeSubdivisionEdge)
                                 private
                                    function  FGetColor:TColor;
                                    function  FGetIndex:Integer;                                         override;
                                    function  FGetIsBoundaryEdge:Boolean;                                override;
                                    procedure FSetSelected(val:Boolean);
                                    function  FGetSelected:Boolean;
                                    function  FGetVisible:Boolean;
                                 public
                                    procedure   Collapse;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                   override;
                                    procedure   Delete;
                                    function    DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;override;
                                    procedure   Draw(DrawMirror:Boolean;Viewport:TFreeViewport);         override;
                                    function    InsertControlPoint(P:T3DCoordinate):TFreeSubdivisionControlpoint;
                                    procedure   LoadBinary(Source:TFreeFileBuffer);
                                    procedure   LoadFromStream(var LineNr:Integer;Strings:TStringList);
                                    procedure   SaveBinary(Destination:TFreeFileBuffer);
                                    procedure   SaveToStream(Strings:TStringlist);
                                    procedure   Trace;
                                    property    Color                         : TColor read FGetColor;
                                    property    Selected                      : boolean read FGetSelected write FSetSelected;       // Property to see if this edge has been selected by the user
                                    property    Visible                       : Boolean read FGetVisible;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionFace                                   }
     {--------------------------------------------------------------------------------------------------}
     TFreeSubdivisionFace     = class(TFreeSubdivisionBase)
                                 private
                                    FPoints     : TFasterlist;
                                    function FGetArea:TFloatType;
                                    function FGetFaceCenter:T3DCoordinate;
                                    function FGetFaceNormal:T3DCoordinate;
                                    function FGetNumberOfPoints:Integer;
                                    function FGetPoint(Index:Integer):TFreeSubdivisionPoint;
                                 public
                                    procedure   AddPoint(Point:TFreeSubdivisionPoint);
                                    function    CalculateFacePoint:TFreeSubdivisionPoint;
                                    procedure   Clear;                                                   virtual;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                   override;
                                    destructor  Destroy;                                                 override;
                                    procedure   FlipNormal; // Inverts the point ordering of the face
                                    function    IndexOfPoint(P:TFreeSubdivisionPoint):Integer;
                                    procedure   Subdivide(Owner:TFreeSubdivisionSurface;ControlFace:Boolean;VertexPoints,EdgePoints,FacePoints,InteriorEdges,ControlEdges,Dest:TFasterList);virtual;
                                    property    Area                       : TFloatType read FGetArea;
                                    property    FaceCenter                 : T3DCoordinate read FGetFaceCenter;
                                    property    FaceNormal                 : T3DCoordinate read FGetFaceNormal;
                                    property    NumberOfpoints             : Integer read FGetNumberOfPoints;
                                    property    Point[index:Integer]       : TFreeSubdivisionPoint read FGetPoint;
                              end;
     {--------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionControlFace                            }
     {--------------------------------------------------------------------------------------------------}
     TFreeSubdivisionControlFace = class(TFreeSubdivisionFace)
                                 private
                                    FLayer            : TFreeSubdivisionLayer;
                                    FChildren         : TFasterList;
                                    FMin,FMax         : T3DCoordinate;
                                    FEdges            : TFasterList;
                                    FControlEdges     : TFasterList;
                                    function FGetChild(Index:Integer):TFreeSubdivisionFace;
                                    function FGetChildCount:Integer;
                                    function FGetColor:TColor;
                                    function FGetControlEdge(Index:Integer):TFreeSubdivisionEdge;
                                    function FGetControlEdgeCount:Integer;
                                    function FGetEdge(Index:Integer):TFreeSubdivisionEdge;
                                    function FGetEdgeCount:Integer;
                                    function FGetIndex:Integer;
                                    function FGetSelected:Boolean;
                                    function FGetVisible:Boolean;
                                    procedure FSetLayer(Val:TFreeSubdivisionLayer);
                                    procedure FSetSelected(val:Boolean);
                                 public
                                    procedure   CalcExtents;
                                    procedure   Clear;                                                   override;
                                    procedure   ClearChildren;
                                    constructor Create(Owner:TFreeSubdivisionSurface);                   override;
                                    function    DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;
                                    procedure   Delete;
                                    destructor  Destroy;                                                 override;
                                    procedure   Draw(Viewport:TFreeViewport);                            overload;virtual;
                                    procedure   Draw(Viewport:TFreeViewport;MinCurvature,MaxCurvature:TFloatType); reintroduce;overload;
                                    function    InsertEdge(P1,P2:TFreeSubdivisionControlPoint):TFreesubdivisionControlEdge;
                                    procedure   LoadBinary(Source:TFreeFileBuffer);
                                    procedure   LoadFromStream(var LineNr:Integer;Strings:TStringList);
                                    procedure   RemoveReferences;
                                    procedure   SaveBinary(Destination:TFreeFileBuffer);
                                    procedure   SaveToDXF(Strings:TStringList);
                                    procedure   SaveToStream(Strings:TStringlist);                       virtual;
                                    procedure   Subdivide(Owner:TFreeSubdivisionSurface;ControlFace:Boolean;VertexPoints,EdgePoints,FacePoints,InteriorEdges,ControlEdges,Dest:TFasterList);override;
                                    procedure   Trace;                                       // select all controlfaces connected to the current one that belong to the same layer and are not separated by a crease edge
                                    property    Color                         : TColor read FGetColor;
                                    property    ControlEdge[index:Integer]    : TFreeSubdivisionEdge read FGetControlEdge;
                                    property    ControlEdgeCount              : Integer read FGetControlEdgeCount;
                                    property    Child[index:Integer]          : TFreeSubdivisionFace read FGetChild;
                                    property    ChildCount                    : integer read FGetChildCount;
                                    property    Edge[index:Integer]           : TFreeSubdivisionEdge read FGetEdge;
                                    property    EdgeCount                     : Integer read FGetEdgeCount;
                                    property    FaceIndex                     : integer read FGetIndex;
                                    property    Layer                         : TFreeSubdivisionLayer read FLayer write FSetLayer;
                                    property    Max                           : T3DCoordinate read FMax;
                                    property    Min                           : T3DCoordinate read FMin;
                                    property    Selected                      : boolean read FGetSelected write FSetSelected;       // Property to see if this controlface has been selected by the user
                                    property    Visible                       : Boolean read FGetVisible;
                              end;
     {---------------------------------------------------------------------------------------------------}
     {                                           TFreeSubdivisionSurface                                 }
     {                                                                                                   }
     { This is the subdivision surface used for modelling the hull.                                      }
     { This is actually a quad-triangle subdivision surface as publisehed in the articles:               }
     {                                                                                                   }
     {   "Quad/triangle subdivision" by J. Stam & C. Loop http://research.microsoft.com/~cloop/qtEG.pdf  }
     {   "On C2 triangle/quad subdivision" by Scott Schaeffer & Joe Warren                               }
     {                                                                                                   }
     {---------------------------------------------------------------------------------------------------}
     TFreeSubdivisionSurface  = class(TFreeEntity)
                                 private
                                    FControlPoints             : TFasterList;          // List with controlpoints, which can be changed by the user
                                    FControlEdges              : TFasterList;          // List with controledges, which can be changed by the user
                                    FControlFaces              : TFasterList;          // List with controlfaces, which can be changed by the user
                                    FControlCurves             : TFasterList;          // list with mastercurves
                                    FPoints                    : TFasterList;          // List with points obtained by subdividing the surface
                                    FEdges                     : TFasterList;          // this list edges obtained by subdividing the controledges
                                    FLayers                    : TFasterList;          // All layers are stored in this list
                                    FSelectedControlPoints     : TFasterList;          // Controlpoints which are selected by the user are put in this list
                                    FSelectedControlEdges      : TFasterList;          // List with currently selected controledges
                                    FSelectedControlCurves     : TFasterList;          // List with currently selected controlcurves
                                    FSelectedControlFaces      : TFasterList;          // List with currently selected controlfaces
                                    FActiveLayer               : TFreeSubdivisionLayer;// Currently active layer, may not be nil!
                                    FShowControlNet            : Boolean;              // Flag to switch controlpoints and control-edges visibility
                                    FInitialized               : Boolean;              // Flag to check if the surface has been initialised.
                                    FShowInteriorEdges         : Boolean;              // Switch to turn on drawing off all interior edges as well.
                                    FDrawMirror                : Boolean;              // If this is set tot true, the other imaginary half (starboard side) will be drawn aswell
                                    FSubdivisionMode           : TFreeSubdivisionMode; // Varaiable to switch between quad-triangle and Catmull Clark subdivision
                                    FDesiredSubdivisionLevel   : byte;
                                    FCurrentSubdivisionLevel   : byte;
                                    FCreaseColor               : TColor;               // color of descendants from creaseedges
                                    FCreaseEdgeColor           : TColor;               // Color of crease controledges
                                    FLastusedLayerID           : integer;
                                    FOnChangeLayerData         : TNotifyEvent;         // Event which is raised when layer-data has been changed
                                    FOnChangeActiveLayer       : TChangeActiveLayerEvent; // Event raised when the active layer is changed
                                    FOnSelectItem              : TNotifyEvent;         // This event is raised whenever an item (such as controlpoint, controledge or controlface) is selected or deselected

                                    FUnderWaterColor           : TColor;               // Color used for shading the underwater part
                                    FWaterlinePlane            : T3DPlane;             // This plane is used to clip the hull, and shade the underwatership in a different color
                                    FShadeUnderWater           : Boolean;              // Switch to turn under water shading on or off
                                    FShowNormals               : boolean;              // show normals of selected controlfaces
                                    FControlPointSize          : Integer;
                                    FEdgeColor                 : TColor;               // Color of normal edges (no crease)
                                    FSelectedcolor             : TColor;               // Default color for selected items
                                    FCreasePointColor          : TColor;               // Color for vertices connected to two creaseedges
                                    FRegularPointColor         : TColor;               // Color of regular controlpoints
                                    FCornerPointColor          : TColor;               // color of cornerpoints
                                    FDartPointColor            : TColor;
                                    FLayerColor                : TColor;               // Default color for layers;
                                    FNormalColor               : TColor;               // color of surface normals
                                    FLeakColor                 : TColor;               // color of leak points
                                    FCurvatureColor            : TColor;               // color of the curvature plot of controlcurves
                                    FControlCurveColor         : TColor;
                                    FZebraColor                : TColor;
                                    FShowCurvature             : Boolean;
                                    FShowControlCurves         : Boolean;
                                    FCurvatureScale            : TFloatType;
                                    FGausCurvature             : TFloatArray;          // list with precalculated values of gauss. curvature in each point, used for shading
                                    FMinGaussCurvature         : TFloatType;
                                    FMaxGaussCurvature         : TFloatType;
                                    FMainframeLocation         : single;
                                    function FGetControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
                                    function FGetControlCurve(Index:Integer):TFreesubdivisionControlCurve;
                                    function FGetControlEdge(Index:Integer):TFreesubdivisionControlEdge;
                                    function FGetControlFace(Index:Integer):TFreeSubdivisionControlFace;
                                    function FGetGaussCurvatureCalculated:boolean;
                                    function FGetLayer(Index:Integer):TFreeSubdivisionLayer;
                                    function FGetNumberOfControlPoints:Integer;
                                    function FGetNumberOfControlEdges:Integer;
                                    function FGetNumberOfControlCurves:Integer;
                                    function FGetNumberOfControlFaces:Integer;
                                    function FGetNumberOfFaces:Integer;
                                    function FGetNumberOfLayers:Integer;
                                    function FGetNumberOfLockedPoints:Integer;
                                    function FGetPoint(Index:Integer):TFreeSubdivisionPoint;
                                    function FGetEdge(Index:Integer):TFreeSubdivisionEdge;
                                    function FGetNumberOfPoints:Integer;
                                    function FGetNumberOfSelectedControlCurves:Integer;
                                    function FGetNumberOfSelectedControlEdges:Integer;
                                    function FGetNumberOfSelectedControlFaces:Integer;
                                    function FGetNumberOfSelectedControlPoints:Integer;
                                    function FGetNumberOfSelectedLockedPoints:Integer;
                                    function FGetNumberOfEdges:Integer;
                                    function FGetSelectedControlCurve(Index:Integer):TFreeSubdivisionControlCurve;
                                    function FGetSelectedControlEdge(Index:Integer):TFreeSubdivisionControlEdge;
                                    function FGetSelectedControlFace(Index:Integer):TFreeSubdivisionControlFace;
                                    function FGetSelectedControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
                                    function FRequestNewLayerID:Integer;
                                    procedure FSetActiveLayer(Val:TFreeSubdivisionLayer);
                                    procedure FSetBuild(Val:Boolean);                                                                     override;
                                    procedure FSetDesiredSubdivisionLevel(val:byte);
                                    procedure FSetFShowControlNet(Val:Boolean);
                                    procedure FSetSubdivisionMode(val:TFreeSubdivisionMode);
                                 protected
                                 public
                                    procedure   AddControlCurve(Curve:TFreesubdivisionControlCurve);
                                    function    AddControlEdge(P1,P2:TFreeSubdivisionPoint):TFreesubdivisionControlEdge;                         overload;virtual;
                                    function    AddControlFace(Points:array of T3DCoordinate;NoPoints:Integer):TFreeSubdivisionControlFace;      overload;virtual;
                                    function    AddControlFace(Points:TFasterList;CheckEdges:Boolean):TFreeSubdivisionControlFace;               reintroduce;overload;
                                    function    AddControlFace(Points:TList;CheckEdges:Boolean):TFreeSubdivisionControlFace;                     reintroduce;overload;
                                    function    AddControlFace(Points:TFasterList;CheckEdges:Boolean;Layer:TFreeSubdivisionLayer):TFreeSubdivisionControlFace; reintroduce;overload;
                                    function    AddControlFaceN(Points:TFasterList;CheckEdges:Boolean;Layer:TFreeSubdivisionLayer):TFreeSubdivisionControlFace; reintroduce;overload;
                                    function    AddControlPoint(P:T3DCoordinate):TFreeSubdivisionControlPoint; overload;virtual;
                                    procedure   AddControlPoint(P:TFreeSubdivisionControlPoint);               reintroduce;overload;
                                    function    AddControlPoint:TFreeSubdivisionControlPoint;                  reintroduce;overload; // Adds a new controlpoint at 0,0,0 without checking other points
                                    function    AddNewLayer:TFreeSubdivisionLayer;
                                    procedure   AssembleFacesToPatches(Layers:TFasterList;Mode:TFreeAssembleMode;var AssembledPatches:TFreeFaceArray;var NAssembled:Integer);
                                    procedure   CalculateGaussCurvature;                                       // Calculate Gauss. curvature in each point of the mesh and store it in a array
                                    procedure   Clear;                                                         override;
                                    procedure   ClearFaces;
                                    procedure   Clearselection;
                                    procedure   ConvertToGrid(Input:TFreeFaceGrid;var Cols,Rows:Integer;var Grid:TFreeSubdivisionGrid);
                                    procedure   Edge_Connect;
                                    function    CanInsertEdge: boolean;
                                    procedure   ExportFeFFile(Strings:TStringList);
                                    procedure   ExportObjFile(ExportControlNet:Boolean;Strings:TStringList);
                                    procedure   Extents(Var Min,Max : T3DCoordinate);                          override;
                                    procedure   ExtrudeEdges(Edges:TFasterList;Direction:T3DCoordinate);       reintroduce;overload;
                                    procedure   CalculateIntersections(Plane:T3DPlane;Faces,Destination:TFasterList);
                                    constructor Create;                                                        override;
                                    destructor  Destroy;                                                       override;
                                    procedure   Draw(Viewport:TFreeViewport);                                  override;
                                    function    EdgeExists(P1,P2:TFreeSubdivisionPoint):TFreeSubdivisionEdge;
                                    procedure   ExtractAllEdgeLoops(var Destination:TFasterList);
                                    procedure   ExtractPointsFromFaces(SelectedFaces,Points:TFasterList;var LockedPoints:Integer);   // extracts all points that are used by the faces in the selectedfaces list
                                    procedure   ExtractPointsFromSelection(SelectedPoints:TFasterList;var LockedPoints:Integer);
                                    procedure   ImportFEFFile(Strings:TStringList;var LineNr:Integer);
                                    procedure   ImportGrid(Points:TFreeCoordinateGrid;Cols,Rows:Integer;Layer:TFreesubdivisionLayer);
                                    procedure   Initialize(PointStartIndex,EdgeStartIndex,FaceStartIndex:Integer);
                                    function    IntersectPlane(Plane:T3DPlane;HydrostaticsLayersOnly:Boolean;List:TFasterList):Boolean;
                                    procedure   InsertPlane(Plane:T3DPlane;AddCurves:Boolean);  // inserts points on edges (visible edges only) that intersect the input plane
                                    procedure   IsolateEdges(Source,Destination:TFasterList);overload;virtual;
                                    procedure   LoadBinary(Source:TFreeFileBuffer);
                                    procedure   LoadFromStream(var LineNr:Integer;Strings:TStringList);
                                    procedure   LoadVRMLFile(Filename:string);
                                    function    PointExists(P:TFreeSubdivisionControlPoint):Boolean;
                                    procedure   Rebuild;                                                       override;
                                    procedure   SaveBinary(Destination:TFreeFileBuffer);
                                    procedure   SaveToStream(Strings:TStringlist);
                                    procedure   Selection_Delete;
                                    procedure   SortEdges(Edges:TFasterList);                                  overload;virtual;
                                    procedure   SortEdges(Edges:TFasterList;var Points:TFasterList);           reintroduce;overload;
                                    procedure   SubDivide;
                                    property    ActiveLayer                         : TFreeSubdivisionLayer read FActiveLayer write FSetActiveLayer;
                                    property    ControlPoint[index:Integer]         : TFreeSubdivisionControlPoint read FGetControlpoint;
                                    property    ControlPointSize                    : Integer read FControlPointSize write FControlPointSize;
                                    property    ControlCurve[index:Integer]         : TFreesubdivisionControlCurve read FGetControlCurve;
                                    property    ControlCurveColor                   : TColor read FControlCurveColor write FControlCurveColor;
                                    property    ControlEdge[index:Integer]          : TFreesubdivisionControlEdge read FGetControlEdge;
                                    property    ControlEdges                        : TFasterlist read FCOntrolEdges;
                                    property    ControlFace[index:Integer]          : TFreeSubdivisionControlFace read FGetControlFace;
                                    property    CurrentSubdivisionLevel             : byte read FCurrentSubdivisionLevel;
                                    property    CurvatureColor                      : TColor read FCurvatureColor write FCurvatureColor;
                                    property    CurvatureScale                      : TFloatType read FCurvatureScale write FCurvatureScale;
                                    property    CreaseColor                         : TColor read FCreaseColor write FCreaseColor;
                                    property    CreaseEdgeColor                     : TColor read FCreaseEdgeColor write FCreaseEdgeColor;
                                    property    CornerPointColor                    : TColor read FCornerPointColor write FCornerPointColor;
                                    property    DartPointColor                      : TColor read FDartPointColor write FDartPointColor;
                                    property    DesiredSubdivisionLevel             : byte read FDesiredSubdivisionLevel write FSetDesiredSubdivisionLevel;
                                    property    DrawMirror                          : boolean read FDrawMirror write FDrawMirror;
                                    property    GaussCurvatureCalculated            : boolean read FGetGaussCurvatureCalculated;
                                    property    CreasePointColor                    : TColor read FCreasePointColor write FCreasePointColor;
                                    property    RegularPointColor                   : TColor read FRegularPointColor write FRegularPointColor;
                                    property    Layer[index:integer]                : TFreeSubdivisionLayer read FGetLayer;
                                    property    LayerColor                          : TColor read FLayerColor write FLayerColor;
                                    property    LeakColor                           : TColor read FLeakColor write FLeakColor;
                                    property    MainframeLocation                   : TFloatType read FMainframeLocation write FMainframeLocation;
                                    property    MaxGaussCurvature                   : TFloatType read FMaxGaussCurvature;
                                    property    MinGaussCurvature                   : TFloatType read FMinGaussCurvature;
                                    property    NumberOfControlFaces                : Integer read FGetNumberOfControlFaces;
                                    property    NumberOfControlEdges                : Integer read FGetNumberOfControlEdges;
                                    property    NumberOfControlCurves               : Integer read FGetNumberOfControlCurves;
                                    property    NumberOfControlPoints               : Integer read FGetNumberOfControlPoints;
                                    property    NumberOfFaces                       : Integer read FGetNumberOfFaces;
                                    property    NumberOfLayers                      : Integer read FGetNumberOfLayers;
                                    property    NumberOfLockedPoints                : Integer read FGetNumberOfLockedPoints;
                                    property    NumberOfSelectedControlCurves       : Integer read FGetNumberOfSelectedControlCurves;
                                    property    NumberOfSelectedControlEdges        : Integer read FGetNumberOfSelectedControlEdges;
                                    property    NumberOfSelectedControlFaces        : Integer read FGetNumberOfSelectedControlFaces;
                                    property    NumberOfSelectedControlPoints       : Integer read FGetNumberOfSelectedControlPoints;
                                    property    NumberOfSelectedLockedPoints        : Integer read FGetNumberOfSelectedLockedPoints;
                                    property    OnChangeActiveLayer                 : TChangeActiveLayerEvent read FOnChangeActiveLayer write FOnChangeActiveLayer;
                                    property    OnChangeLayerData                   : TNotifyEvent read FOnChangeLayerData write FOnChangeLayerData;
                                    property    OnSelectItem                        : TNotifyEvent read FOnSelectItem write FOnSelectItem;
                                    property    Point[index:Integer]                : TFreeSubdivisionPoint read FGetpoint;
                                    property    Edge[index:Integer]                 : TFreeSubdivisionEdge read FGetEdge;
                                    property    EdgeColor                           : TColor read FEdgeColor write FEdgeColor;
                                    property    NormalColor                         : TColor read FNormalColor write FNormalColor;
                                    property    NumberOfEdges                       : Integer read FGetNumberOfEdges;
                                    property    NumberOfPoints                      : Integer read FGetNumberOfPoints;
                                    property    ShadeUnderWater                     : boolean read FShadeUnderWater write FShadeUnderWater;
                                    property    Selectedcolor                       : TColor read FSelectedcolor write FSelectedcolor;
                                    property    SelectedControlCurve[index:Integer] : TFreeSubdivisionControlCurve read FGetSelectedControlCurve;
                                    property    SelectedControlEdge[index:Integer]  : TFreeSubdivisionControlEdge read FGetSelectedControlEdge;
                                    property    SelectedControlFace[index:Integer]  : TFreeSubdivisionControlFace read FGetSelectedControlFace;
                                    property    SelectedControlPoint[index:Integer] : TFreeSubdivisionControlPoint read FGetSelectedControlPoint;
                                    property    ShowControlCurves                   : boolean read FShowControlCurves write FShowControlCurves;
                                    property    ShowControlNet                      : boolean read FShowControlNet write FSetFShowControlNet;
                                    property    ShowCurvature                       : Boolean read FShowCurvature write FShowCurvature;
                                    property    ShowInteriorEdges                   : Boolean read FShowInteriorEdges write FShowInteriorEdges;
                                    property    ShowNormals                         : Boolean read FShowNormals write FShowNormals;
                                    property    SubdivisionMode                     : TFreeSubdivisionMode read FSubdivisionMode write FSetSubdivisionMode;
                                    property    UnderWaterColor                     : TColor read FUnderWaterColor write FUnderWaterColor;
                                    property    WaterlinePlane                      : T3DPlane read FWaterlinePlane write FWaterlinePlane;
                                    property    ZebraColor                          : TColor read FZebraColor write FZebraColor;
                              end;

   {--------------------------------------------------------------------------------------------------}
   {                                         TFreeDestroyList                                         }
   {  Some objects can be split to number of new ones and destroyed after it.                         }
   {  However they cannot be destroyed right away because they are referred in a processing cycle     }
   {  They should be added to this list and destroyed later.                                          }
   {--------------------------------------------------------------------------------------------------}

   TFreeDestroyList = class (TFasterList)
   public
     constructor Create;
     procedure DestroyAll; // destroy all objects in the list, but not the list itself. To destroy the list call Destroy;
   end;


function  AddPoint(P1,P2:T3DCoordinate):T3DCoordinate;                     // Add two vectors
function  AddPointSymm(P1,P2:T3DCoordinate):T3DCoordinate;                 // Add two vectors for symmetric layers 
function  AreaStr(Units:TFreeUnitType):String;                             // Returns a string value with the area units
function  BoolToStr(Val:Boolean):String;                                   // In contrast to delphis own BoolToStrF this procedure returns '0' when false and '1' when true
procedure ClipTriangle(P1,P2,P3:T3DCoordinate;s1,s2,s3:TFloatType;var Nf,Nb:INteger;var Front,Back:TFreeCoordinateArray);overload;
procedure ClipTriangle(P1,P2,P3:T3DCoordinate;Plane:T3DPlane;var Nf,Nb:INteger;var Front,Back:TFreeCoordinateArray);overload;
function  ConvertDimension(Value:TFloatType;Units:TFreeUnitType):String;   // Converts a dimesnion to a string
function  ConvertCoordinate(Coord: String; OldCoord: TFloatType):TFloatType;// converts a string to a floatingpoint value, possibly using imperial units
function  CrossProduct(U,V:T3DCoordinate):T3DCoordinate;
function  DisplacementToVolume(Displ,Density,AppCoeff:TFloatType;Units:TFreeUnitType):TFloatType; // Converts a displacement to volume
function  DensityStr(Units:TFreeUnitType):String;                          // Returns a string value with the density units
function  DistPP3D(P1,P2:T3DCoordinate):TFloatType;                        // Calculates the distance between two points
Function  DistanceToLine(P1,P2:TPoint;X,Y:Integer;var Parameter:TFloatType):TFloatType;
Function  DistancePointToPlane(P:T3DCoordinate;Plane:T3DPlane):TFloatType;
function  DotProduct(U, V : T3DCoordinate) : TFloatType;
function  FindDXFColorIndex(Color:TColor):integer;                         // find nearest DXF color corresponding to a windows color
procedure FillColor(Parameter:TFloatType;var R,G,B:Byte);
function  InertiaStr(Units:TFreeUnitType):String;                          // Returns a string value with the moment of inertia units
function  Interpolate(P1,P2:T3DCoordinate;Param:TFloatType):T3DCoordinate; // perform linear interpolation between two 3D points
procedure JoinSplineSegments(JoinError:TFloatType;ForceToOneSegment:Boolean;List:TFasterList);// Takes multiple splines and tries to connect them to as few as possible
Function  Lines3DIntersect(P1,P2,P3,P4:T3DCoordinate;var Param:double;var Int:T3DCoordinate):Boolean;
function  LengthStr(Units:TFreeUnitType):String;                           // Returns a string value with the length units
function  MakeLength(value:TFloatType;Decimals,DesLength:integer):string;overload;
function  MakeLength(value:String;DesLength:integer):string;overload;
procedure MinMax(P:T3DCoordinate;var Min,Max:T3DCoordinate);               //
function  Midpoint(P1,P2:T3DCoordinate):T3DCoordinate;                     // Calculate the mid-point between P1 and P2
function  MirrorPlane(P:T3DCoordinate;Plane:T3DPLane):T3DCoordinate;       // mirror a point in a plane
function  Normalize(P:T3DCoordinate):T3DCoordinate;
function  NumberOfDecimals(Value:TFloatType):Integer;                      // Finds out with how many decimals a number should be presented
function  PlaneIntersectsBox(Min,Max:T3DCoordinate;Plane:T3DPlane):Boolean;// Function to determine if a plane intersects a bounding box
function  PlanePointNormal(P,Normal:T3DCoordinate):T3DPlane;               // Calculates the plane with a given normal N through point P
function  PlanePPP(P1,P2,P3:T3DCoordinate):T3DPlane;                       // Create a plane defined by three points
function  PointInTriangle(Int,P0,P1,P2:T3DCoordinate):Boolean;             // This function calculates if a point lies inside a triangle assuming it lies on the plane determined by the triangle
function  PoundsToNewton(InpLbs:TFloatType):TFloatType;                    // converts pounds to Newton
Function  ProjectPointOnLine(P,P1,P2:T3DCoordinate):T3DCoordinate;         // Projects point  P on the linesegment through P1 and P2
function  ProjectPointOnPlane(P:T3DCoordinate;Plane:T3DPlane):T3DCoordinate;// Projects a point on to a plane
function  RandomColor:TColor;                                              // create a random color
function  ReadBoolFromStr(LineNr:Integer;Var source:String):Boolean;       // Read a single boolean value from a string
function  ReadFloatFromStr(LineNr:Integer;Var source:String):TFloatType;   // Read a single floatingpoint value from a string
function  ReadIntFromStr(LineNr:Integer;Var source:String):Integer;        // Read an integer value from a string
Function  RotateAroundPoint(P:T3DCoordinate;Center:T3DCoordinate;sinhx,coshx,sinhy,coshy,sinhz,coshz:TFloatType):T3DCoordinate;
function  RotatePointAroundVector(Point,StartPoint,Endpoint:T3DCoordinate):T3DCoordinate; // Function to rotate a point around a vector
function  RotateVector(P0:T3DCoordinate;sinx,cosx,siny,cosy,sinz,cosz:TFloatType):T3DCoordinate; // Rotates a vector around the origin
function  ScalePoint(Scale:TFloatType;P:T3DCoordinate):T3DCoordinate;      // Scales a vector
function  SetPlane(a,b,c,d:TFloatType):T3DPlane;
function  SetPoint(X,Y,Z:TFloatType):T3DCoordinate;
procedure SortFloatArray(var FloatArray:TFloatArray;var N:Integer);        // sorts an array with floatingpoint values and removes double entries
function  Space(Index:Integer):String;                                     // Outputs a string with a number of spaces
function  SquaredDistPP(P1,P2:T3DCoordinate):TFloatType;                   // calculates the squared distance between two points
function  Subtract(AVec1, AVec2 : T3DCoordinate) : T3DCoordinate;          // subtract two vectors
function  Truncate(Value:TFloatType;Maxlength:integer):String;             // Convert a floatingpoint to a string value with a max. number of specified decimals All trailing zeros will be removed
Function  UnifiedNormal(P1,P2,P3:T3DCoordinate):T3DCoordinate;             // calculate the normal of a plane defined by points P1,P2,P3 and scale to unit-length
function  UnitVector(P:T3DCoordinate):T3DCoordinate;                       // Scale a vector sucht that it's length is 1.0
function  VectorLength(Normal:T3DCoordinate) : TFloatType;                 // Calculate the length of a vector
function  VolStr(Units:TFreeUnitType):String;                              // Returns a string value with the volume units
function  DensStr(Units:TFreeUnitType):String;                             // Returns a string value with the density units
function  ViscStr(Units:TFreeUnitType):String;                             // Returns a string value with the viscosity units
function  VolumeToDisplacement(Volume,Density,AppCoeff:TFloatType;Units:TFreeUnitType):TFloatType; // Converts a volume to displacement
function  WeightStr(Units:TFreeUnitType):String;                           // Returns a string value with the weight units
function  DegrStr(Units:TFreeUnitType):String;                             // Returns a string value with the degr units
function  LenMMStr(Units:TFreeUnitType):String;                            // Returns a string value with the length (mm or inch) units

var DelayedDestroyList : TFreeDestroyList;

procedure Register;

implementation

{$R ViewportCursors.res}

uses FreeLanguageSupport,
     VRMLUnit,
     FreeBackgroundBlendingDlg,
     FreeSaveImageDlg,
     FreeLogger;

const crRotate    = 1; // Rotation cursor
      crPan       = 2; // Pan cursor
      crSetOrigin = 3; // Cursor used when setting the origin of a background image
      crSetScale  = 4; // Cursor used when setting the scale of a background image
      crTranspCol = 5; // Cursor used when setting the transparent color of a background image


function  AddPoint(P1,P2:T3DCoordinate):T3DCoordinate;
// Add two vectors
begin
   Result.X:=P1.X+P2.X;
   Result.Y:=P1.Y+P2.Y;
   if Result.Y<0 then Result.Y:=0.0;
   Result.Z:=P1.Z+P2.Z;
end;{AddPoint}

function  AddPointSymm(P1,P2:T3DCoordinate):T3DCoordinate;
// Add two vectors
begin
   Result.X:=P1.X+P2.X;
   Result.Y:=P1.Y+P2.Y;
   if Result.Y<0 then Result.Y:=0.0;
   Result.Z:=P1.Z+P2.Z;
end;{AddPointSymm}

// Returns a string value with the area units
function AreaStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(465)
                       else result:=Userstring(452);
end;{AreaStr}

// Returns a string value with the length units
function LengthStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(464)
                       else result:=Userstring(451);
end;{LengthStr}

// Returns a string value with the moment of inertia units
function InertiaStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(467)
                       else result:=Userstring(470);
end;{InertiaStr}

function VolStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(466)
                       else result:=Userstring(453);
end;{VolStr}

function DensStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(468)
                       else result:=Userstring(469);
end;{DensStr}

function ViscStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(471)
                       else result:=Userstring(472);
end;{ViscStr}

function DegrStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(455)
                       else result:=Userstring(455);
end;{DegrStr}

function LenMMStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(755)
                       else result:=Userstring(756);
end;{LenMMStr}

// Converts a volume to displacement
function VolumeToDisplacement(Volume,Density,AppCoeff:TFloatType;Units:TFreeUnitType):TFloatType;
begin
   if Units=fuImperial then Result:=Volume*AppCoeff*Density/2240 // convert to British imperial tons (1 imperial LONG ton=2240 lb)
                       else Result:=Volume*AppCoeff*Density;
end;{VolumeToDisplacement}

function DensityStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(468)
                       else result:=Userstring(469);
end;{DensityStr}

function WeightStr(Units:TFreeUnitType):String;
begin
   if Units=fuImperial then result:=Userstring(473)
                       else result:=Userstring(474);
end;{WeightStr}

function BoolToStr(Val:Boolean):String;
// In contrast to delphi's own BoolToStrF this procedure
// returns '0' when false and '1' when true
begin
   if val then result:='1'
          else Result:='0';
end;{BoolToStr}

procedure ClipTriangle(P1,P2,P3:T3DCoordinate;s1,s2,s3:TFloatType;var Nf,Nb:INteger;var Front,Back:TFreeCoordinateArray);overload;
var T :Double;
    P : T3DCoordinate;
begin
   Nf:=0;
   Nb:=0;
   if (S1<=0) and (S2<=0) and (S3<=0) then
   begin
      // all at the back of the plane
      Nb:=3;
      Setlength(Back,Nb);
      Back[0]:=P1;
      Back[1]:=P2;
      Back[2]:=P3;
   end else if (S1>0) and (S2>0) and (S3>0) then
   begin
      // all at the front of the plane
      Nf:=3;
      Setlength(Front,Nf);
      Front[0]:=P1;
      Front[1]:=P2;
      Front[2]:=P3;
   end else
   begin
      // Triangle spans the plane, calculate the intersections
      Setlength(Back,6);
      Setlength(Front,6);
      if S1<=0 then
      begin
         Inc(Nb);
         Back[Nb-1]:=P1;
      end;
      if S1>=0 then
      begin
         Inc(Nf);
         Front[Nf-1]:=P1;
      end;

      if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
      begin
         if S1=S2 then
         begin
            T:=0.5;
         end else T:=-s1/(s2-s1);
         P.X:=P1.X+T*(P2.X-P1.X);
         P.Y:=P1.Y+T*(P2.Y-P1.Y);
         P.Z:=P1.Z+T*(P2.Z-P1.Z);
         inc(Nb);
         Back[Nb-1]:=P;
         Inc(Nf);
         Front[Nf-1]:=P;
      end;
      if S2<=0 then
      begin
         Inc(Nb);
         Back[Nb-1]:=P2;
      end;
      if S2>=0 then
      begin
         Inc(Nf);
         Front[Nf-1]:=P2;
      end;

      if ((S2<0) and (S3>0)) or ((S2>0) and (S3<0)) then
      begin
         if S2=S3 then
         begin
            T:=0.5;
         end else T:=-S2/(S3-S2);
         P.X:=P2.X+T*(P3.X-P2.X);
         P.Y:=P2.Y+T*(P3.Y-P2.Y);
         P.Z:=P2.Z+T*(P3.Z-P2.Z);
         inc(Nb);
         Back[Nb-1]:=P;
         inc(Nf);
         Front[Nf-1]:=P;
      end;
      if S3<=0 then
      begin
         Inc(Nb);
         Back[Nb-1]:=P3;
      end;
      if S3>=0 then
      begin
         Inc(Nf);
         Front[Nf-1]:=P3;
      end;
      if ((S3<0) and (S1>0)) or ((S3>0) and (S1<0)) then
      begin
         if S3=S1 then
         begin
            T:=0.5;
         end else T:=-S3/(S1-S3);
         P.X:=P3.X+T*(P1.X-P3.X);
         P.Y:=P3.Y+T*(P1.Y-P3.Y);
         P.Z:=P3.Z+T*(P1.Z-P3.Z);
         inc(Nb);
         Back[Nb-1]:=P;
         inc(Nf);
         Front[Nf-1]:=P;
      end;
   end;
end;{ClipTriangle}

procedure ClipTriangle(P1,P2,P3:T3DCoordinate;Plane:T3DPlane;var Nf,Nb:INteger;var Front,Back:TFreeCoordinateArray);
var S1,S2,S3: double;
begin
   S1:=Plane.a*P1.x+Plane.b*P1.y+Plane.c*P1.z+Plane.d;
   S2:=Plane.a*P2.x+Plane.b*P2.y+Plane.c*P2.z+Plane.d;
   S3:=Plane.a*P3.x+Plane.b*P3.y+Plane.c*P3.z+Plane.d;
   ClipTriangle(P1,P2,P3,s1,s2,s3,Nf,Nb,Front,Back);
end;{ClipTriangle}

function ConvertDimension(Value:TFloatType;Units:TFreeUnitType):String;
var Feet    : integer;
    Inches  : TFloatType;
begin
   if Units=fuImperial then
   begin
      if Value<0 then Feet:=-abs(Trunc(Value))
                 else Feet:=Trunc(Value);
      Inches:=abs(Value-Feet)*12;
      Result:=IntToStr(Feet);
      if (Feet=0) and (Value<0) then
      begin
         Result:='-'+Result;
      end;
      Result:=Result+''''+#32+FloatToStrF(Inches,ffFixed,7,1)+'''''';
   end else Result:=IntToStr(Round(1000*Value));
end;{ConvertDimension}

function ConvertCoordinate(Coord: String; OldCoord: TFloatType):TFloatType;
{ parses input "Coord" and and returns the new value of a coordinate    }
{ if first character of input is (RelIdentifier - currently '@')        }
{    then the input value is added to the old value "OldCoord".         }
{ if the first character of the value is '-',                           }
{    then the remainder is treated as a negative value                  }
{ if the remainding value contains ONE '-'                              }
{    then the part after that '-' is considered as a fraction of a unit }
{    The size of the fraction is const. "InchFractions"                 }
var myString:    String;
    myFactor:    TFloatType;
    myOldValue:  TFloatType;
    myWholeFeet: TFloatType;
    myWholeInch: TFloatType;
    myFracInch:  TFloatType;
    myFracPos:   Integer;
const RelIdentifier: String = '@';
      InchFractions: TFloatType = 8;
begin
  myString:= Coord;
  myFactor:=    1.0;
  myOldValue:=  OldCoord;
  myWholeFeet:= 0.0;
  myWholeInch:= 0.0;
  myFracInch:=  0.0;

  if LeftStr(myString, 1) = RelIdentifier then  // get rid of the '@'
  begin
    myString:= MidStr(myString,2,255);
    if Pos(RelIdentifier, myString) > 0 then myString:= '0'; // make sure not to have another '@' (and a 0 doesn't hurt)
  end else myOldValue:= 0;  // we have a new value now, so don't add the old one!

  if LeftStr(myString, 1)='-' then       // get rid of the minus
  begin
    myFactor:=-1.0;
    myString:= MidStr(myString,2,255);
  end;

  myFracPos:= Pos('-', myString);
  if myFracPos >0 then          // check whether we have imperial input format
  begin
    if myFracPos > 1 then   // check whether there is an whole feet value
    begin
      myWholeFeet:= StrToFloat(LeftStr(myString, myFracPos-1));
      myString:= MidStr(myString,myFracPos+1,255);
    end;

    myFracPos:= Pos('-', myString); // is there a second "-", i.e. do we have also fractional inches?
    if myFracPos >0 then
    begin
      if myFracPos > 1 then   // check whether there is an whole inch value
      begin
        myWholeInch:= StrToFloat(LeftStr(myString, myFracPos-1));
        myString:= MidStr(myString,myFracPos,255);
      end;
      // new: check whether there is a + or a - to add or subtract a half fraction
      myString:= MidStr(myString,2,255);
      if (RightStr(myString,1) = '-') then
      begin
        myFracInch :=  StrToFloat(LeftStr(myString,Pos('-', myString)-1)) - 0.5;
      end
      else if (RightStr(myString,1) = '+') then
           begin
             myFracInch :=  StrToFloat(LeftStr(myString,Pos('+', myString)-1)) + 0.5;
           end
           else myFracInch :=  StrToFloat(myString);
      // end new
    end
    else // no fractional inches
    begin
      myWholeInch:=  StrToFloat(myString);
    end;

  end else myWholeFeet:= StrToFloat(myString); // no imperial input format

  Result:= myOldValue + myFactor*(myWholeFeet + myWholeInch/12 + myFracInch/(12*InchFractions));
end;{ConvertCoordinate}

function DistPP3D(P1,P2:T3DCoordinate):TFloatType;
// Calculates the distance between two points
begin
   Result:=Sqrt(Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y)+Sqr(P2.Z-P1.Z));
end;{DistPP3D}

Function DistancePointToLine(P,P1,P2:T3DCoordinate):TFloatType;
var V1,V2,N    : T3DCoordinate;
    Len,a,b,c  : TFloatType;
begin
   a:=P1.x-P2.x;
   b:=P1.y-P2.y;
   c:=P1.z-P2.z;
   Len:=Sqrt(a*a+b*b+c*c);
   if abs(Len)>1e-6 then begin
    V1.X:=P.X-P1.X;
    V1.Y:=P.Y-P1.Y;
    V1.Z:=P.Z-P1.Z;
    V2.x:=a;
    V2.y:=b;
    V2.z:=c;
    N.x:=V1.y*V2.z-V1.z*V2.y;
    N.y:=V1.z*V2.x-V1.x*V2.z;
    N.z:=V1.x*V2.y-V1.y*V2.x;
    Result:=Sqrt(N.x*N.x+N.y*N.y+N.z*N.z)/Len;
   end 
   else Result:=Sqrt(Sqr(P.X-P1.X)+Sqr(P.Y-P1.Y)+Sqr(P.Z-P1.Z));
end;{DistancePointToLine}

Function DistancePointToPlane(P:T3DCoordinate;Plane:T3DPlane):TFloatType;
begin
   Result:=Plane.a*P.x+Plane.b*P.y+Plane.c*P.z+Plane.d;
end;{DistancePointToPlane}

Function DistanceToLine(P1,P2:TPoint;X,Y:Integer;var Parameter:TFloatType):TFloatType;
var Pt  : T2DCoordinate;
begin
   if (P1.X<0) and (P2.X<0) then Result:=1e6 else
      if (P1.X>Screen.Width) and (P2.X>Screen.width) then Result:=1e6 else
         if (P1.Y<0) and (P2.Y<0) then Result:=1e6 else
            if (P1.Y>Screen.Height) and (P2.Y>Screen.Height) then Result:=1e6 else
   if (P2.X=P1.X) and (P1.Y=P2.Y) then
   begin
      Result:=Sqrt(Sqr(P1.X-X)+Sqr(P1.Y-Y));
   end else
   begin
      Parameter:=((X-P1.X)*(P2.X-P1.X)+(Y-P1.Y)*(P2.Y-P1.Y))/
         (sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y));
      if (Parameter>=0) and (Parameter<=1) then
      begin
         Pt.X:=P1.X+Parameter*(P2.X-P1.X);
         Pt.Y:=P1.Y+Parameter*(P2.Y-P1.Y);
         if (Pt.X>=0) and (Pt.Y>=0) and (Pt.X<=Screen.Width) and (Pt.Y<=Screen.Height)
            then Result:=Sqrt(Sqr(Pt.X-X)+Sqr(Pt.Y-Y))
            //else Result:=1e10;
            else Result:=1e8;
      end
      //else Result:=1e10;
      else Result:=1e8;
   end;
end;{DistanceToLine}

function CrossProduct(U,V:T3DCoordinate):T3DCoordinate;
begin
   Result.X :=  (V.Y * U.Z - V.Z * U.Y);
   Result.Y := -(V.X * U.Z - V.Z * U.X);
   Result.Z :=  (V.X * U.Y - V.Y * U.X);
end;{CrossProduct}

// Converts a displacement to volume
function DisplacementToVolume(Displ,Density,AppCoeff:TFloatType;Units:TFreeUnitType):TFloatType;
begin
   if Units=fuImperial then Result:=Displ*2240/Density // convert to British imperial tons (1 imperial LONG ton=2240 lb)
                       else Result:=Displ/Density;
   Result:=Result/AppCoeff;
end;{DisplacementToVolume}

function DotProduct(U, V : T3DCoordinate) : TFloatType;
begin
  Result:=(U.X * V.X)+(U.Y * V.Y)+(U.Z * V.Z);
end;{DotProduct}

// find nearest DXF color corresponding to a windows color
function FindDXFColorIndex(Color:TColor):integer;
var R,G,B:Byte;
    R1,G1,B1:Byte;
    I:Integer;
    Dist,Tmp:single;
begin
   Result:=1;
   R:=GetRValue(Color);
   G:=GetGValue(Color);
   B:=GetBValue(Color);
   Dist:=1e30;
   for I:=1 to 255 do
   begin
      R1:=GetRValue(DXFLayerColors[I]);
      G1:=GetGValue(DXFLayerColors[I]);
      B1:=GetBValue(DXFLayerColors[I]);
      Tmp:=Sqrt(Sqr(R1-R)+Sqr(G1-G)+Sqr(B1-B));
      if Tmp<Dist then
      begin
         Dist:=Tmp;
         Result:=I;
      end;
   end;
end;{FindDXFColorIndex}

procedure FillColor(Parameter:TFloatType;var R,G,B:Byte);
// Calculate color on basis of wavelength (0.0 - 1.0)
var Blue   :  single;
    Green  :  single;
    Red    :  single;
    wavelength:single;

   function Adjust(Color:single):integer;
   begin
      if color<=0.0 then result:=0 else
         if Color>=1.0 then Result:=255 else
      begin
         Result:=Round(255*Power(Color,0.8));
         //if result<0 then result:=0 else if result>255 then result:=255;
      end;
   end;{Adjust}

begin
   Parameter:=1-Parameter;
   wavelength:=420+Parameter*280;
  CASE TRUNC(Wavelength) OF
    380..439:
      begin
        Red   := -(Wavelength - 440) / (440 - 380);
        Green := 0.0;
        Blue  := 1.0
      end;
    440..489:
      begin
        Red   := 0.0;
        Green := (Wavelength - 440) / (490 - 440);
        Blue  := 1.0
      end;
    490..509:
      begin
        Red   := 0.0;
        Green := 1.0;
        Blue  := -(Wavelength - 510) / (510 - 490)
      end;
    510..579:
      begin
        Red   := (Wavelength - 510) / (580 - 510);
        Green := 1.0;
        Blue  := 0.0
      end;
    580..644:
      begin
        Red   := 1.0;
        Green := -(Wavelength - 645) / (645 - 580);
        Blue  := 0.0
      end;
    645..780:
      begin
        Red   := 1.0;
        Green := 0.0;
        Blue  := 0.0
      end;
    ELSE
      Red   := 0.0;
      Green := 0.0;
      Blue  := 0.0
  end;
  R:=Adjust(Red);
  G:=Adjust(Green);
  B:=Adjust(Blue);
end;{FillColor}

function Interpolate(P1,P2:T3DCoordinate;Param:TFloatType):T3DCoordinate;
begin
   Result.X:=P1.X+Param*(P2.X-P1.X);
   Result.Y:=P1.Y+Param*(P2.Y-P1.Y);
   Result.Z:=P1.Z+Param*(P2.Z-P1.Z);
end;{Interpolate}

// This procedure takes a lot of linesegments and tries to connect them into as few as possible splines
procedure JoinSplineSegments(JoinError:TFloatType;ForceToOneSegment:Boolean;List:TFasterList);

type TSegmentRecord = record
                         Length  : TFloatType;
                         COG     : T3DCoordinate;
                      end;
     TSegmentdata   = array[0..0] of TSegmentRecord;
     PSegmentdata   = ^TSegmentdata;

var I,J           : Integer;
    Fixed         : TFreeSpline;
    Match,Nearest : TFreeSpline;
    NearestDist   : TFloatType;
    Min           : TFloatType;
    MatchIndex    : Integer;
    FixedClosed   : Boolean;
    MatchClosed   : Boolean;
    D1,D2,D3,D4   : TFloatType;

    function Minimum(D1,D2,D3,D4:TFloatType):TFloatType;
    begin
       Result:=D1;
       if D2<Result then Result:=D2;
       if D3<Result then Result:=D3;
       if D4<Result then Result:=D4;
    end;{Minimum}

begin
   // First remove any single linesegments;
   I:=1;
   while I<List.Count do
   begin
      Fixed:=List.Items[I-1];
      if Fixed.NumberOfPoints>1 then FixedClosed:=DistPP3D(Fixed.Point[0],Fixed.Point[Fixed.NumberOfPoints-1])<1e-5
                                else FixedClosed:=False;
      if (Fixed.NumberOfPoints>1) and (Not FixedClosed) then
      begin
         NearestDist:=1e30;
         Nearest:=nil;
         MatchIndex:=-1;
         for J:=1 to List.Count do if I<>J then
         begin
            Match:=List.Items[J-1];
            if Match.NumberOfPoints>1 then MatchClosed:=DistPP3D(Match.Point[0],Match.Point[Match.NumberOfPoints-1])<1e-5
                                      else MatchClosed:=False;
            if (Match.NumberOfPoints>1) and (Not MatchClosed) then
            begin
               D1:=SquaredDistPP(Fixed.Point[0],Match.Point[0]);
               D2:=SquaredDistPP(Fixed.Point[0],Match.Point[Match.NumberOfPoints-1]);
               D3:=SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints-1],Match.Point[0]);
               D4:=SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints-1],Match.Point[Match.NumberOfPoints-1]);
               Min:=Minimum(D1,D2,D3,D4);
               if Min<NearestDist then
               begin
                  NearestDist:=Min;
                  Nearest:=Match;
                  MatchIndex:=J;
                  if Min<1e-5 then break;
               end;
            end;
         end;
         if Nearest<>nil then
         begin
            Match:=Nearest;
            D1:=SquaredDistPP(Fixed.Point[0],Match.Point[0]);
            D2:=SquaredDistPP(Fixed.Point[0],Match.Point[Match.NumberOfPoints-1]);
            D3:=SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints-1],Match.Point[0]);
            D4:=SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints-1],Match.Point[Match.NumberOfPoints-1]);
            Min:=Minimum(D1,D2,D3,D4);
            if (Min<JoinError) or (ForceToOneSegment) then
            begin // The splines do touch each other on one of their ends
               // Increase capacity of fixed spline
               Fixed.Capacity:=Fixed.Capacity+Match.NumberOfPoints;
               if Min=D1 then
               begin // Case 1
                  Fixed.InsertSpline(0,True,True,Match);
               end else if Min=D2 then
               begin // Case 2
                  Fixed.InsertSpline(0,False,True,Match);
               end else if Min=D3 then
               begin // Case 3
                  Fixed.InsertSpline(Fixed.NumberOfPoints,False,True,Match);
               end else if Min=D4 then
               begin // Case 4
                  Fixed.InsertSpline(Fixed.NumberOfPoints,True,True,Match);
               end else messagedlg('Error in comparing minimum values.',mterror,[mbok],0);
               // Destroy the matching spline
               List.Delete(MatchIndex-1);
               Match.Destroy;
               Match := nil;
               I:=0; // Reset match index to start searching for a new matching spline
            end;
         end;
      end;
      Inc(I);
   end;
end;{JoinSplineSegments}

Function Lines3DIntersect(P1,P2,P3,P4:T3DCoordinate;var Param:double;var Int:T3DCoordinate):Boolean;
//   Calculate the line segment PaPb that is the shortest route between
//   two lines P1P2 and P3P4. Calculate also the values of mua and mub where
//      Pa = P1 + mua (P2 - P1)
//      Pb = P3 + mub (P4 - P3)
//   Return FALSE if no solution exists.

const eps=1e-6;
      eps2=1e-12;
      eps3=3e-6;
var pa,pb,p13,p43,p21:T3DCoordinate;
    numer,denom,mua,mub:double;
    d1343,d4321,d1321,d4343,d2121:double;
begin
   Result:=False;
   p43.x:=p4.x - p3.x;
   p43.y:=p4.y - p3.y;
   p43.z:=p4.z - p3.z;
   d4343:=p43.x * p43.x + p43.y * p43.y + p43.z * p43.z;
/////   if (abs(p43.x)<eps) and (abs(p43.y)<eps) and (abs(p43.z)<eps) then exit;
   if (d4343<eps2) then exit;
   p21.x:=p2.x - p1.x;
   p21.y:=p2.y - p1.y;
   p21.z:=p2.z - p1.z;
   d2121:=p21.x * p21.x + p21.y * p21.y + p21.z * p21.z;
/////   if (abs(p21.x)<eps) and (abs(p21.y)<eps) and (abs(p21.z)<eps) then exit;
   if (d2121<eps2) then exit;
   p13.x:=p1.x - p3.x;
   p13.y:=p1.y - p3.y;
   p13.z:=p1.z - p3.z;
   d1343:=p13.x * p43.x + p13.y * p43.y + p13.z * p43.z;
   d4321:=p43.x * p21.x + p43.y * p21.y + p43.z * p21.z;
   d1321:=p13.x * p21.x + p13.y * p21.y + p13.z * p21.z;

   denom:=d2121*d4343 - d4321 * d4321;
   if (abs(denom) < eps) then exit;
   numer:=d1343 * d4321 - d1321 * d4343;
   mua:=numer / denom;
   mub:=(d1343 + d4321 * (mua)) / d4343;
   pa.x:=p1.x + mua * p21.x;
   pa.y:=p1.y + mua * p21.y;
   pa.z:=p1.z + mua * p21.z;
   pb.x:=p3.x + mub * p43.x;
   pb.y:=p3.y + mub * p43.y;
   pb.z:=p3.z + mub * p43.z;
   if (mua>-1e-6) and (mua<1) and (mub>-1e-6) and (mub<1) then
   begin
      Result:=true;
      Int:=Pa;
   end;
   {
   if not result then
   begin
      L1:=DistPP3D(P1,P2);
      L2:=DistPP3D(P3,P4);
      Result:=(DistPP3D(P1,Pa)<=L1) and (DistPP3D(P2,Pa)<=L1) and
              (DistPP3D(P3,Pb)<=L2) and (DistPP3D(P4,Pb)<=L2);
      if Result then
      begin
         Int:=Pa;
      end;

   end;
   }
end;{Lines3DIntersect}

// Calculate the mid-point between P1 and P2
function Midpoint(P1,P2:T3DCoordinate):T3DCoordinate;
begin
   Result.X:=0.5*(P1.X+P2.X);
   Result.Y:=0.5*(P1.Y+P2.Y);
   Result.Z:=0.5*(P1.Z+P2.Z);
end;{Midpoint}

function MirrorPlane(P:T3DCoordinate;Plane:T3DPLane):T3DCoordinate;
var P2 : T3DCoordinate;
begin
   P2:=ProjectPointOnPlane(P,Plane);
   Result.X:=P.X+2*(P2.X-P.X);
   Result.Y:=P.Y+2*(P2.Y-P.Y);
   Result.Z:=P.Z+2*(P2.Z-P.Z);
end;{MirrorPlane}

// Finds out with how many decimals a number should be presented
function NumberOfDecimals(Value:TFloatType):Integer;
var I   : TFloatType;
begin
   Value:=Abs(Value);
   if Value<1e-6 then Value:=1e-6;
   I:=Ln(Value)/2.30258;
   Result:=4-Trunc(I);
   if Result<0 then
   begin
      Result:=0;
   end;
   if Result>3 then Result:=3;
end;{NumberOfDecimals}

// Function to determine if a plane intersects a bounding box
function PlaneIntersectsBox(Min,Max:T3DCoordinate;Plane:T3DPlane):Boolean;
var P    : array[1..8] of T3DCoordinate;
    I    : Integer;
    S    : TFloatType;
    SMin : TFloatType;
    SMax : TFloatType;
begin
   SMin:=0;
   SMax:=0;
   P[1].X:=Min.X;
   P[1].Y:=Min.Y;
   P[1].Z:=Min.Z;
   P[2].X:=Max.X;
   P[2].Y:=Min.Y;
   P[2].Z:=Min.Z;
   P[3].X:=Max.X;
   P[3].Y:=Max.Y;
   P[3].Z:=Min.Z;
   P[4].X:=Min.X;
   P[4].Y:=Max.Y;
   P[4].Z:=Min.Z;
   P[5].X:=Min.X;
   P[5].Y:=Min.Y;
   P[5].Z:=Max.Z;
   P[6].X:=Max.X;
   P[6].Y:=Min.Y;
   P[6].Z:=Max.Z;
   P[7].X:=Max.X;
   P[7].Y:=Max.Y;
   P[7].Z:=Max.Z;
   P[8].X:=Min.X;
   P[8].Y:=Max.Y;
   P[8].Z:=Max.Z;
   for I:=1 to 8 do
   begin
      S:=Plane.A*P[I].x+Plane.B*P[I].y+Plane.C*P[I].z+Plane.D;
      if I=1 then
      begin
         SMin:=S;
         SMax:=S;
      end;
      if S<SMin then SMin:=S;
      if S>SMax then SMax:=S;
   end;
   Result:=((Smin<=0.0) and (SMax>=0.0)) or
           ((SMax<=0.0) and (SMin>=0.0));
end;{PlaneIntersectsBox}

// Calculates the plane with a given normal N through point P
function PlanePointNormal(P,Normal:T3DCoordinate):T3DPlane;
begin
   Result.a:= Normal.x;
   Result.b:= Normal.y;
   Result.c:= Normal.z;
   Result.d:= -P.x * Result.a - P.y * Result.b - P.z * Result.c;
end;{PlanePointNormal}

function PlanePPP(P1,P2,P3:T3DCoordinate):T3DPlane;
var N : T3DCoordinate;
    L : TFloatType;
begin
   // Calculate normal
   N.x:=(P2.y-P1.y)*(P3.z-P1.z)-(P2.z-P1.z)*(P3.y-P1.y);
   N.y:=(P2.z-P1.z)*(P3.x-P1.x)-(P2.x-P1.x)*(P3.z-P1.z);
   N.z:=(P2.x-P1.x)*(P3.y-P1.y)-(P2.y-P1.y)*(P3.x-P1.x);
   // Make normal unit length
   L:=Sqrt((N.X*N.X)+(N.Y*N.Y)+(N.Z*N.Z));
   if L<=0 then L:=0.0000001;
   Result.a:=N.x/L;
   Result.b:=N.y/L;
   Result.c:=N.z/L;
   Result.d:=-P1.x*Result.a-P1.y*Result.b-P1.z*Result.c;
end;{PlanePPP}


function PointInBlock(P,Pmin,Pmax:T3DCoordinate):Boolean;
begin
  result := ( (P.X >= Pmin.X) and (P.Y >= Pmin.Y) and (P.Z >= Pmin.Z) )
        and ( (P.X <= Pmax.X) and (P.Y <= Pmax.Y) and (P.Z <= Pmax.Z) );
end;


function PointInTriangleBarycentric(P,A,B,C:T3DCoordinate):Boolean;
// This function calculates using Barycentric method
// if a point lies inside a triangle
// from: http://blogs.msdn.com/b/rezanour/archive/2011/08/07/barycentric-coordinates-and-point-in-triangle-tests.aspx
var s,t,r, denom: double;
    u,v,w,
    vCrossU, vCrossW, uCrossV, uCrossW :T3DCoordinate; //vectors
    i: double;
begin
  result := false;
  u := Subtract(B,A);
  v := Subtract(C,A);
  w := Subtract(P,A);
  vCrossW := CrossProduct(v,w);
  vCrossU := CrossProduct(v,w);
  r := DotProduct(vCrossW, vCrossU); // calc r just to know its sign
   if ( r < 0)
     then exit;
  uCrossW := CrossProduct(u,w);
  uCrossV := CrossProduct(u,v);
  t :=DotProduct(uCrossW, uCrossV);  // calc t just to know its sign
  if ( t < 0)
     then exit;
  // At this point, we know that r and t and both > 0.
  // Therefore, as long as their sum is <= 1, each must be less <= 1
  denom := VectorLength(uCrossV);
  r := VectorLength(vCrossW) / denom;
  t := VectorLength(uCrossW) / denom;
  i := (r + t);
  if abs(i)<1e-5 then i:=0.0
  else if abs(1.0 - i) <= 1e-5 then i:=1.0;
  result := (i <= 1);
end;

function PointInTriangle(Int,P0,P1,P2:T3DCoordinate):Boolean;
// This function calculates if a point lies inside a triangle
// assuming it lies on the plane determined by the triangle

   function SameSide(p1,p2,a,b:T3DCoordinate):Boolean;
   var Cp1,Cp2 : T3DCoordinate;
       P,Ba    : T3DCoordinate;
       Dp      : double;
   begin
      Ba.X:=B.X-A.X;
      Ba.Y:=B.Y-A.Y;
      Ba.Z:=B.Z-A.Z;
      P.X:=P1.X-A.X;
      P.Y:=P1.Y-A.Y;
      P.Z:=P1.Z-A.Z;
      //cp1:=CrossProduct(Ba,P);
      Cp1.X:= (P.Y*Ba.Z-P.Z*Ba.Y);
      Cp1.Y:=-(P.X*Ba.Z-P.Z*Ba.X);
      Cp1.Z:= (P.X*Ba.Y-P.Y*Ba.X);
      P.X:=P2.X-A.X;
      P.Y:=P2.Y-A.Y;
      P.Z:=P2.Z-A.Z;
      //cp2:=CrossProduct(Ba,P);
      Cp2.X:= (P.Y*Ba.Z-P.Z*Ba.Y);
      Cp2.Y:=-(P.X*Ba.Z-P.Z*Ba.X);
      Cp2.Z:= (P.X*Ba.Y-P.Y*Ba.X);
      //Result:=DotProduct(cp1, cp2) >= 0;
      Dp:=(Cp1.X * Cp2.X)+(Cp1.Y * Cp2.Y)+(Cp1.Z * Cp2.Z);

      if abs(Dp)<1e-5 then
      begin
         Dp:=0.0;
      end;
      Result:=Dp>=0;
   end;
begin
   Result:=SameSide(Int,P0,P1,P2) and SameSide(Int,P1,P0,P2) and SameSide(Int,P2,P0,P1);
end;{PointInTriangle}

// converts pounds to Newton
function PoundsToNewton(InpLbs:TFloatType):TFloatType;
begin
   Result:=InpLbs*Lbs*9.81;
end;{PoundsToNewton}

function ProjectPointOnPlane(P:T3DCoordinate;Plane:T3DPlane):T3DCoordinate;
// Projects a point on a plane
var q,r : TFloatType;
begin
///   q:=Plane.a*Plane.a+Plane.b*Plane.b+Plane.c*Plane.c;
   q:=Sqr(Plane.a)+Sqr(Plane.b)+Sqr(Plane.c);
   if Q<>0.0 then
   begin
      r:=(Plane.a*P.x+Plane.b*P.y+Plane.c*P.z+Plane.d)/q;
      Result.x:= P.x - Plane.a * r;
      Result.y:= P.y - Plane.b * r;
      Result.z:= P.z - Plane.c * r;
   end else Result:=ZERO;
end;{ProjectPointOnPlane}

// Projects point P on the linesegment through P1 and P2
Function ProjectPointOnLine(P,P1,P2:T3DCoordinate):T3DCoordinate;
var V1,V2   : T3DCoordinate;
    L,T     : TFloatType;
begin
   V1.X:=P.X-P1.X;
   V1.Y:=P.Y-P1.Y;
   V1.Z:=P.Z-P1.Z;
   V2.X:=P2.X-P1.X;
   V2.Y:=P2.Y-P1.Y;
   V2.Z:=P2.Z-P1.Z;
   L:=Sqr(V2.X)+Sqr(V2.Y)+Sqr(V2.Z);
////   if abs(L)>1e-6 then
   if L>1e-6 then
   begin
      T:=((P.X-P1.X)*(P2.X-P1.X)+(P.Y-P1.Y)*(P2.Y-P1.Y)+(P.Z-P1.Z)*(P2.Z-P1.Z))/L;
      Result.X:=P1.X+T*(P2.X-P1.X);
      Result.Y:=P1.Y+T*(P2.Y-P1.Y);
      Result.Z:=P1.Z+T*(P2.Z-P1.Z);
   end else Result:=P;
end;{ProjectPointOnLine}

function RandomColor:TColor;
const base = 40;
begin
   Result:=RGB(Base+Random(255-Base),Base+Random(255-Base),Base+Random(255-Base));
end;{RandomColor}

function ReadBoolFromStr(LineNr:Integer;Var source:String):Boolean;
var Str     : string;
    Index   : Integer;
begin
   // Delete spaces from start
   While Length(Source)>0 do if Source[1]=#32 then System.Delete(Source,1,1) else break;
   // Extract stringvalue until space is encountered or end of line
   Str:='';
   Index:=1;
   while Index<=Length(Source) do
   begin
      if Source[Index]<>#32 then
      begin
         Str:=Str+Upcase(Source[Index]);
         Inc(Index);
         if Index>Length(Source) then Source:='';
      end else
      begin
         Delete(Source,1,Index);
         break;
      end;
   end;
   Result:=(Str='1') or (Str='TRUE') or (Str='YES') ;
end;{ReadBoolFromStr}

function ReadFloatFromStr(LineNr:Integer;Var source:String):TFloatType;
var Str     : string;
    Index,J : Integer;
begin
   // Delete spaces from start
   While Length(Source)>0 do if Source[1]=#32 then System.Delete(Source,1,1) else break;
   // Extract stringvalue until space is encountered or end of line
   Str:='';
   // Replace comma by period
   Index:=Pos(',',Source);
   if Index<>0 then
   begin
      J:=Pos('.',Source);
      if J=0 then J:=Length(Source)+1;
      if Index<=J then Source[Index]:='.';
   end;
   Index:=1;
   while Index<=Length(Source) do
   begin
      if Source[Index]<>#32 then
      begin
         Str:=Str+Source[Index];
         Inc(Index);
         if Index>Length(Source) then Source:='';
      end else
      begin
         Delete(Source,1,Index);
         break;
      end;
   end;
   Val(Str,Result,Index);
   if Index<>0 then Raise Exception.Create('Invalid floating point value in linenr. '+IntToStr(LineNr+1))
end;{ReadFloatFromStr}

function ReadIntFromStr(LineNr:Integer;Var source:String):Integer;
var Str     : string;
    Index   : Integer;
begin
   // Delete spaces from start
   While Length(Source)>0 do if Source[1]=#32 then System.Delete(Source,1,1) else break;
   // Extract stringvalue until space is encountered or end of line
   Str:='';
   Index:=1;
   while Index<=Length(Source) do
   begin
      if Source[Index]<>#32 then
      begin
         Str:=Str+Source[Index];
         Inc(Index);
         if Index>Length(Source) then Source:='';
      end else
      begin
         Delete(Source,1,Index);
         break;
      end;
   end;
   Val(Str,Result,Index);
   if Index<>0 then
   begin
      Raise Exception.Create('Invalid integer value in linenr. '+IntToStr(LineNr+1))
   end;
end;{ReadIntFromStr}

Function RotateAroundPoint(P:T3DCoordinate;Center:T3DCoordinate;sinhx,coshx,sinhy,coshy,sinhz,coshz:TFloatType):T3DCoordinate;
var P1:T3DCoordinate;
    r11,r12,r13,r21,r22,r23,r31,r32,r33:TFloatType;
begin
   // Translate back to origin
   P1.x:=P.x-Center.x;
   P1.y:=P.y-Center.y;
   P1.z:=P.z-Center.z;

   // Do rotation
   r11:= coshy * coshz;
   r12:= coshy * sinhz;
   r13:= -sinhy;
   r21:=  sinhx * sinhy * coshz  - coshx * sinhz;
   r22:=  sinhx * sinhy * sinhz  + coshx * coshz;
   r23:=  sinhx * coshy;
   r31:=  coshx * sinhy * coshz  + sinhx * sinhz;
   r32:=  coshx * sinhy * sinhz  - sinhx * coshz;
   r33:=  coshx * coshy;
   Result.x:= P1.x * r11 + P1.y * r12 + P1.z * r13;
   Result.y:= P1.x * r21 + P1.y * r22 + P1.z * r23;
   Result.z:= P1.x * r31 + P1.y * r32 + P1.z * r33;

   // Translate back to centre again
   Result.x:=result.x+Center.x;
   Result.y:=result.y+Center.y;
   Result.z:=result.z+Center.z;
end;{RotateAroundPoint}

// Function to rotate a point around a vector
function RotatePointAroundVector(Point,StartPoint,Endpoint:T3DCoordinate):T3DCoordinate;
VAR sinhx,coshx : TFloatType;
    sinhy,coshy : TFloatType;
    yz,xyz      : TFloatType;
    P1,P2       : T3DCoordinate;
begin
   Fillchar(Result,SizeOf(Result),0);
   yz:=sqrt(Endpoint.y*Endpoint.y+Endpoint.z*Endpoint.z);
   if (Endpoint.z=0) and (Endpoint.y = 0) then
   begin
      sinhx:= 0;
      coshx:= 1;
   end else
   begin
      sinhx:=-Endpoint.y/yz;
      coshx:=Endpoint.z/yz;
   end;
   xyz:=sqrt(Endpoint.x*Endpoint.x+Endpoint.y*Endpoint.y+Endpoint.z*Endpoint.z);
   if xyz = 0 then
   begin
      Result:=Point;
      exit;
   end else
   begin
      sinhy:=Endpoint.x/xyz;
      coshy:=yz/xyz;
   end;
   // Translate to origin
   P1.x:= Point.x-StartPoint.x;
   P1.y:= Point.y-StartPoint.y;
   P1.z:= Point.z-StartPoint.z;
   // Rotate around Y-axis
   P2.X:=P1.x*coshy+P1.z*sinhy;
   P2.Y:=P1.Y;
   P2.Z:=-P1.x*sinhy+P1.z*coshy;
   // Rotate around X-axis
   P1.x:=P2.x;
   P1.y:=P2.y*coshx-P2.z* sinhx;
   P1.z:=P2.y*sinhx+P2.z* coshx;
   // Translate back to centre again
   Result.x:=P1.x+StartPoint.x;
   Result.y:=P1.y+StartPoint.y;
   Result.z:=P1.z+StartPoint.z;
end;{RotatePointAroundVector}

// Rotates a vector around the origin
Function RotateVector(P0:T3DCoordinate;sinx,cosx,siny,cosy,sinz,cosz:TFloatType):T3DCoordinate;
var r11,r12,r13,r21,r22,r23,r31,r32,r33:TFloatType;
begin
   // Do rotation
   r11:= cosy * cosz;
   r12:= cosy * sinz;
   r13:= -siny;
   r21:=  sinx * siny * cosz  - cosx * sinz;
   r22:=  sinx * siny * sinz  + cosx * cosz;
   r23:=  sinx * cosy;
   r31:=  cosx * siny * cosz  + sinx * sinz;
   r32:=  cosx * siny * sinz  - sinx * cosz;
   r33:=  cosx * cosy;
   Result.x:= P0.x * r11 + P0.y * r12 + P0.z * r13;
   Result.y:= P0.x * r21 + P0.y * r22 + P0.z * r23;
   Result.z:= P0.x * r31 + P0.y * r32 + P0.z * r33;
end;{RotateVector}

// Scales a vector
function ScalePoint(Scale:TFloatType;P:T3DCoordinate):T3DCoordinate;
begin
   Result.X:=P.X*Scale;
   Result.Y:=P.Y*Scale;
   Result.Z:=P.Z*Scale;
end;{ScalePoint}

function SetPlane(a,b,c,d:TFloatType):T3DPlane;
begin
   Result.a:=a;
   Result.b:=b;
   Result.c:=c;
   Result.d:=d;
end;{SetPlane}

function SetPoint(X,Y,Z:TFloatType):T3DCoordinate;
begin
   Result.X:=X;
   Result.Y:=Y;
   Result.Z:=Z;
end;{SetPoint}

procedure SortFloatArray(var FloatArray:TFloatArray;var N:Integer);
var I : Integer;

   procedure QuickSort(L,R:Integer);
   var I, J : Integer;
       Val  : TFloatType;

       Procedure Swap(I,J:Integer);
       var Tmp : TFloatType;
       begin
          Tmp:=FloatArray[I];
          FloatArray[I]:=FloatArray[J];
          FloatArray[J]:=Tmp;
       end;

   begin
      I:=L;
      J:=R;
      Val:=FloatArray[(L+R) div 2];
      repeat
	      While FloatArray[I]<Val do Inc(I);
	      while Val<FloatArray[J] do Dec(J);
         if I<=J then
         begin
            Swap(I,J);
            Inc(I);
            Dec(J);
         end;
      Until I>J;
      if L<J then QuickSort(L,J);
      if I<R then QuickSort(I,R);
   end;{QuickSort}

begin                 //  begin procedure SortFloatarray
   if N<2 then exit;
   QuickSort(0,N-1);
   I:=2;
   // Remove duplicates
   While I<=N do
   begin
      if abs(FloatArray[I-2]-FloatArray[I-1])<1e-4 then
      begin // remove duplicate values
         Move(FloatArray[I-1],FloatArray[I-2],(N-I+1)*SizeOf(TFloatType));
         Dec(N);
      end else Inc(I);
   end;
end; {SortFloatArray}

// Outputs a string with a number of spaces
function Space(Index:Integer):String;
var I : Integer;
begin
   Result:='';
   for I:=1 to Index do Result:=Result+' ';
end;{Space}

function SquaredDistPP(P1,P2:T3DCoordinate):TFloatType;
// calculates the squared distance between two points
begin
   Result:=Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y)+Sqr(P2.Z-P1.Z);
end;{SquaredDistPP}

function Truncate(Value:TFloatType;Maxlength:integer):String;
// Convert a floatingpoint to a string value with a max. number of specified decimals
// All trailing zeros will be removed
begin
   Result:=FloatToStrF(Value,ffFixed,10,Maxlength);
   while Result[Length(Result)]='0' do Delete(Result,Length(Result),1);
   if Length(Result)<MaxLength then Result:=Result+'0' else
      if Result[Length(Result)] in ['.',','] then Result:=Result+'0';
end;{Truncate}

function MakeLength(value:TFloatType;Decimals,DesLength:integer):string;
var Input:String;
begin
   if Decimals=-1 then Decimals:=NumberOfDecimals(Value);
   Input:=FloatToStrF(Value,ffFixed,10,Decimals);
   While Length(Input)<DesLength do Input:=' '+Input;
   Result:=Input;
end;{MakeLength}

function MakeLength(value:String;DesLength:integer):string;
begin
   While Length(Value)<DesLength do Value:=value+' ';
   Result:=Value;
end;{MakeLength}

procedure MinMax(P:T3DCoordinate;var Min,Max:T3DCoordinate);
begin
   if P.X<Min.X then Min.X:=P.X;
   if P.Y<Min.Y then Min.Y:=P.Y;
   if P.Z<Min.Z then Min.Z:=P.Z;
   if P.X>Max.X then Max.X:=P.X;
   if P.Y>Max.Y then Max.Y:=P.Y;
   if P.Z>Max.Z then Max.Z:=P.Z;
end;{MinMax}

Function UnifiedNormal(P1,P2,P3:T3DCoordinate):T3DCoordinate;
var L:TFloatType;
begin
   Result.x:=(P2.y-P1.y)*(P3.z-P1.z)-(P2.z-P1.z)*(P3.y-P1.y);
   Result.y:=(P2.z-P1.z)*(P3.x-P1.x)-(P2.x-P1.x)*(P3.z-P1.z);
   Result.z:=(P2.x-P1.x)*(P3.y-P1.y)-(P2.y-P1.y)*(P3.x-P1.x);
   L:=VectorLength(Result);
   if L<1e-6 then L:=1e-6;
   Result.X:=Result.X/L;
   Result.y:=Result.y/L;
   Result.z:=Result.z/L;
end;{UnifiedNormal}

function UnitVector(P:T3DCoordinate):T3DCoordinate;
var L : TFloatType;
begin
   L:=Sqrt(Sqr(P.X*P.X)+Sqr(P.Y*P.Y)+Sqr(P.Z*P.Z));
   if L=0.0 then L:=1e-5;
   Result.X:=P.X/L;
   Result.Y:=P.Y/L;
   Result.Z:=P.Z/L;
end;{UnitVector}

function Subtract(AVec1, AVec2 : T3DCoordinate) : T3DCoordinate;
begin
  Result.X:=AVec1.X - AVec2.X;
  Result.Y:=AVec1.Y - AVec2.Y;
  Result.Z:=AVec1.Z - AVec2.Z;
end;{Subtract}

function Normalize(P:T3DCoordinate):T3DCoordinate;
// Normalize a vector to unit length
var L:TFloatType;
begin
   L:=Sqrt(P.X*P.X+P.Y*P.Y+P.Z*P.Z);
   if IsNan(L) then
   begin
      Result:=ZERO;
   end else
   begin
      if L<1e-6 then L:=1e-6;
      Result.X:=P.X/L;
      Result.Y:=P.Y/L;
      Result.Z:=P.Z/L;
   end;
end;{Normalize}

function VectorLength(Normal:T3DCoordinate) : TFloatType;
// calculate the length of a vector
var Tmp:TFloatType;
begin
   Tmp:=(Normal.X * Normal.X) + (Normal.Y * Normal.Y) + (Normal.Z * Normal.Z);
   if tmp<0 then Tmp:=0;
   Result := Sqrt(Tmp);
   if Result = 0 then Result := 0.0000001;
end;{VectorLength}




{---------------------------------------------------------------------------------------------------}
{                                           TFreeAlphaBuffer                                        }
{                                                                                                   }
{ Alpha-buffer class used in the shading algorithm                                                  }
{---------------------------------------------------------------------------------------------------}
procedure TFreeAlphaBuffer.AddPixelData(X,Y:Integer;R,G,B,Alpha:Byte;Z:Single);
var Data:TAlphaBlendData;
begin
   if (X>=0) and (X<FWidth) and (Y>=0) and (Y<FHeight) then
   begin

   if Y<FFirstRow then FFirstRow:=Y else if Y>FlastRow then FLastRow:=Y;
   if X<FBuffer[Y].First then FBuffer[Y].First:=X else if X>FBuffer[Y].Last then FBuffer[Y].Last:=X;

   if FBuffer[Y].Pixels[X].Number>=FBuffer[Y].Pixels[X].Capacity then
   begin
      inc(FBuffer[Y].Pixels[X].Capacity,4);
      setlength(FBuffer[Y].Pixels[X].Data,FBuffer[Y].Pixels[X].Capacity);
   end;
   Data.R:=R;
   Data.G:=G;
   Data.B:=B;
   Data.Alpha:=Alpha;
   Data.zvalue:=Z;
   try
   FBuffer[Y].Pixels[X].Data[FBuffer[Y].Pixels[X].Number]:=Data;
   except
   FBuffer[Y].Pixels[X].Data[FBuffer[Y].Pixels[X].Number]:=Data;
   end;
   if FBuffer[Y].Pixels[X].Number<250 then inc(FBuffer[Y].Pixels[X].Number);
   end;
end;{TFreeAlphaBuffer.AddPixelData}

procedure TFreeAlphaBuffer.Initialize;
var I,J : Integer;
begin
   if (FWidth<>FViewport.FDestinationWidth) or (FHeight<>FViewport.FDestinationheight) then
   begin
      FHeight:=FViewport.FDestinationheight;
      FWidth:=FViewport.FDestinationWidth;
      FFirstRow:=0;
      FLastRow:=-1;
      Setlength(FBuffer,FHeight);
      for I:=0 to FHeight-1 do
      begin
         Setlength(FBuffer[I].Pixels,FWidth);
         FBuffer[I].First:=0;
         FBuffer[I].Last:=-1;
         for J:=0 to FWidth-1 do
         begin
            Setlength(FBuffer[I].Pixels[J].Data,0);
            FBuffer[I].Pixels[J].Number:=0;
            FBuffer[I].Pixels[J].Capacity:=0;
         end;
      end;
   end;
end;{TFreeAlphaBuffer.Initialize}

procedure TFreeAlphaBuffer.Draw;
var I,J : Integer;


    procedure ProcessPixel(X,Y:Integer;PixData:TAlphaBlendPixelArray);

      procedure QuickSort(L,R:Integer);
      var I, J : Integer;
          Val  : TFloatType;

         Procedure Swap(I,J:Integer);
         var Tmp : TAlphaBlendData;
         begin
            Tmp:=PixData.Data[I];
            PixData.Data[I]:=PixData.Data[J];
            PixData.Data[J]:=Tmp;
         end;
      begin
         I:=L;
         J:=R;
         Val:=PixData.Data[(L+R) div 2].zvalue;
         repeat
	         While PixData.Data[I].zvalue<Val do Inc(I);
	         while Val<PixData.Data[J].zvalue do Dec(J);
            if I<=J then
            begin
               Swap(I,J);
               Inc(I);
               Dec(J);
            end;
         Until I>J;
         if L<J then QuickSort(L,J);
         if I<R then QuickSort(I,R);
      end;{QuickSort}


    var Data:TAlphaBlendData;
        //Row     : pRGBTripleArray;
        pRow,pPixel:pointer;
        Pixel : TRGBTriple;
        Clr : TColor;
        I:Byte;   R,G,B: byte; dR,dG,dB,dA:Smallint;
    begin
       if PixData.Number>1 then QuickSort(0,PixData.Number-1);

       FViewport.GetPixel(X,Y, R,G,B);

       for I:=0 to PixData.Number-1 do
       begin
         Data:=PixData.Data[I];
         if Data.zvalue>FViewport.ZBuffer.FBuffer[Y][X] then
         begin
         dA:=Data.Alpha;
         R := R + ((dA*(Data.R-R)) div 256);  // Use div because Negative shr 8 causes range error
         G := G + ((dA*(Data.G-G)) div 256);
         B := B + ((dA*(Data.B-B)) div 256);
         end;
       end;

       FViewport.SetPixel(X,Y, R,G,B);

    end; {ProcessPixel}
begin
   //FViewport.BeginUpdate; // doing that before working via Canvas causes black Canvas
   for I:=FFirstRow to FLastRow do
   begin
      for J:=FBuffer[I].First to FBuffer[I].Last do
      begin
         if FBuffer[I].Pixels[J].Number>0 then
         begin
            ProcessPixel(J,I,FBuffer[I].Pixels[J]);
            FBuffer[I].Pixels[J].Number:=0;
            FBuffer[I].Pixels[J].Capacity:=0;
            Setlength(FBuffer[I].Pixels[J].Data,0);
         end;
      end;
   end;
   FFirstRow:=0;
   FLastRow:=-1;
   //FViewport.EndUpdate; // doing that after working via Canvas causes black Canvas
end;{TFreeAlphaBuffer.Draw}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeZBuffer                                            }
{                                                                                                   }
{ Z-buffer class used in the shading algorithm                                                      }
{---------------------------------------------------------------------------------------------------}
procedure TFreeZBuffer.Initialize;
var I : Integer;
begin
   if (FWidth<>FViewport.FDestinationWidth) or (FHeight<>FViewport.FDestinationheight) then
   begin
      FHeight:=FViewport.FDestinationheight;
      FWidth:=FViewport.FDestinationWidth;
      Setlength(FBuffer,FHeight);
      for i:=0 to FHeight-1 do Setlength(FBuffer[I],FWidth);
   end;
   // initalize all pixel cells to an initial value
   for I:=0 to FWidth-1 do FBuffer[0][I]:=-1e10;
   for I:=1 to FHeight-1 do Move(FBuffer[0][0],FBuffer[I][0],FWidth*SizeOf(TFloatType));
end;{TFreeZBuffer.Initialize}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeBackgroundImage                                    }
{                                                                                                   }
{ Background image properties for use in a viewport                                                 }
{                                                                                                   }
{---------------------------------------------------------------------------------------------------}
procedure TFreeBackgroundImage.AssignData(Image:TGraphic;View:TFreeViewType;Origin:TPoint;Scale:TFloatType;Transp:boolean;TranspCol:TColor;Alpha,Quality,Tolerance:Byte;Quiet:Boolean);
var Changed:Boolean;
begin
   Changed:=False;
   if (Image<>nil) and (FBitmap=nil) then
   begin
      FBitmap:=TBitmap.Create;
      FBitmap.Assign(Image);
      Changed:=True;
   end else if (Image=nil) and (FBitmap<>nil) then
   begin
      FBitmap.Free;
      FBitmap:=nil;
      Changed:=true;
   end else if (Image<>nil) and (FBitmap<>nil) then
   begin
      FBitmap.Assign(Image);
      Changed:=true;
   end;
   FShowInView:=View;
   FOrigin:=Origin;
   FScale:=Scale;
   FTransparent:=Transp;
   FTransparentColor:=TranspCol;
   Falpha:=Alpha;
   FQuality:=Quality;
   FTolerance:=Tolerance;
   if Changed and (not Quiet) then Owner.Refresh;
end;{TFreeBackgroundImage.AssignData}

procedure TFreeBackgroundImage.Clear;
begin
   if FBitmap<>nil then
   begin
      FBitmap.Destroy;
      FBitmap:=nil;
   end;
   FOrigin.X:=0;
   FOrigin.Y:=0;
   FScale:=1.0;
   FTransparent:=False;
   FTransparentColor:=ClBlack;
   FVisible:=True;
   if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   FShowInView:=fvBodyPlan;
   FQuality:=100;
   FAlpha:=255;
   FTolerance:=5;
end;{TFreeBackgroundImage.Clear}

procedure TFreeBackgroundImage.FSetAlpha(val:Byte);
begin
   if val<>FAlpha then
   begin
      FAlpha:=val;
      if (FVisible) and (FBitmap<>nil) then FOwner.Refresh;
   end;
end;{TFreeBackgroundImage.FSetAlpha}

procedure TFreeBackgroundImage.FSetOrigin(val:TPoint);
begin
   if (Val.X<>Forigin.X) or (Val.Y<>FOrigin.Y) then
   begin
      FOrigin:=Val;
      if (FVisible) and (FBitmap<>nil) then FOwner.Refresh;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
end;{TFreeBackgroundImage.FSetOrigin}

procedure TFreeBackgroundImage.FSetTolerance(val:Byte);
begin
   if Val<>FTolerance then
   begin
      FTolerance:=val;
      if (FVisible) and (FBitmap<>nil) and (FTransparent) then FOwner.Refresh;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
end;{TFreeBackgroundImage.FSetTolerance}

procedure TFreeBackgroundImage.FSetTransparent(val:Boolean);
begin
   if Val<>FTransparent then
   begin
      FTransparent:=val;
      if (FVisible) and (FBitmap<>nil) then FOwner.Refresh;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
end;{TFreeBackgroundImage.FSetTransparent}

procedure TFreeBackgroundImage.FSetTransparentColor(val:TColor);
begin
   if Val<>FTransparentColor then
   begin
      FTransparentColor:=val;
      if (FVisible) and (FBitmap<>nil) and (Transparent) then
      begin
         if FTransparentColor=Owner.Color then FTransparent:=False;
         FOwner.Refresh;
      end else if FTransparentColor=Owner.Color then FTransparent:=False;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
end;{TFreeBackgroundImage.FSetTransparentColor}

procedure TFreeBackgroundImage.FSetVisible(val:Boolean);
begin
   if val<>FVisible then
   begin
      FVisible:=val;
      if FBitmap<>nil then Owner.Refresh;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
end;{TFreeBackgroundImage.FSetVisible}

constructor TFreeBackgroundImage.Create(Viewport:TFreeViewport);
begin
   Inherited Create;
   FOwner:=Viewport;
   FBitmap:=nil;
   Clear;
end;{TFreeBackgroundImage.Create}

destructor TFreeBackgroundImage.Destroy;
begin
   Clear;
   Inherited Destroy;
end;{TFreeBackgroundImage.Destroy}

procedure TFreeBackgroundImage.Draw;
var DestRect    : TRect;
    I,J     : Integer;
    Scan    : PRGBTripleArray;
    Rt,Gt,Bt: Byte;
    TmpBmp     : TBitmap;
    DrawingToprinter:Boolean;
    Backgr  : TRGBTriple;
    TmpVal  : Byte;
    BitmapFormatHelper    : TFreeBitmapFormatHelper;
    pPixel, pRow : pointer;
    Pixel : TRGBTriple;
    C : TColor;
begin
   if (Visible) and (Owner.ViewType=FShowInView) then
   begin
      DestRect:=TargetRect;

      TmpBmp:=TBitmap.Create;
      TmpBmp.Assign(FBitmap);
      //if TmpBmp.PixelFormat<>pf24bit then TmpBmp.PixelFormat:=pf24bit;
      BitmapFormatHelper := TFreeBitmapFormatHelper.Create(TmpBmp);

      //DrawingToprinter:=False;
      //if Printer<>nil then DrawingToPrinter:=(FOwner.DrawingCanvas=Printer.Canvas) and (Fowner.FPrinting);
      DrawingToPrinter:=FOwner.FGetPrinting;

      if DrawingToPrinter then
      begin
         // Use white background for transparent images
         Backgr.rgbtRed:=255;
         Backgr.rgbtGreen:=255;
         Backgr.rgbtBlue:=255;
      end else
      begin
         Backgr.rgbtRed:=GetRValue(Owner.Color);
         Backgr.rgbtGreen:=GetGValue(Owner.Color);
         Backgr.rgbtBlue:=GetBValue(Owner.Color);
      end;

      Rt:=GetRValue(FTransparentColor);
      Gt:=GetGValue(FTransparentColor);
      Bt:=GetBValue(FTransparentColor);
      TmpVal:=255-FAlpha;
      TmpBmp.BeginUpdate(true);
      //if (Transparent) or (Alpha<>255) then
      for I:=0 to TmpBmp.Height-1 do
      begin
         //Scan:=TmpBmp.ScanLine[I];
         pRow := TmpBmp.RawImage.GetLineStart(I);

         for J:=0 to TmpBmp.Width-1 do
         begin
            pPixel := pRow + BitmapFormatHelper.BytesPerPixel * J;
            Pixel := BitmapFormatHelper.ToTRGBTriple(pPixel);
            {C := TmpBmp.Canvas.Pixels[J,I];
            Pixel.rgbtRed := Red(C);
            Pixel.rgbtGreen:=Green(C);
            Pixel.rgbtBlue:=Blue(C);}
            if Transparent then
            begin
               // Replace transparent pixels with the viewport color
               if (abs(Pixel.rgbtRed-Rt)<=FTolerance) and
                  (abs(Pixel.rgbtGreen-Gt)<=FTolerance) and
                  (abs(Pixel.rgbtBlue-Bt)<=FTolerance) then
               begin
                  Pixel:=Backgr;
               end else
               begin
                  // Blend non-transparent pixels with the viewport
                  Pixel.rgbtRed:=(Tmpval*Backgr.rgbtRed+FAlpha*Pixel.rgbtRed) shr 8;
                  Pixel.rgbtGreen:=(Tmpval*Backgr.rgbtGreen+FAlpha*Pixel.rgbtGreen) shr 8;
                  Pixel.rgbtBlue:=(Tmpval*Backgr.rgbtBlue+FAlpha*Pixel.rgbtBlue) shr 8;
               end;
            end else
            begin
               // Blend all pixels with the viewport
               Pixel.rgbtRed:=(Tmpval*Backgr.rgbtRed+FAlpha*Pixel.rgbtRed) shr 8;
               Pixel.rgbtGreen:=(Tmpval*Backgr.rgbtGreen+FAlpha*Pixel.rgbtGreen) shr 8;
               Pixel.rgbtBlue:=(Tmpval*Backgr.rgbtBlue+FAlpha*Pixel.rgbtBlue) shr 8;
            end;

            BitmapFormatHelper.FromTRGBTriple(Pixel,pPixel);
            //TmpBmp.Canvas.Pixels[J,I]:= RGBtoColor(Pixel.rgbtRed,Pixel.rgbtGreen,Pixel.rgbtBlue);
         end;
      end;  //for I
      TmpBmp.EndUpdate(true);

      //StretchBlt(FOwner.DrawingCanvas.Handle,DestRect.Left,DestRect.Top,DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,TmpBmp.Canvas.Handle,0,0,TmpBmp.Width,TmpBmp.Height,SRCCOPY);
      //FOwner.DrawingCanvas.StretchDraw(DestRect,TmpBmp);
      FOwner.StretchDraw(DestRect,TmpBmp);
      BitmapFormatHelper.Destroy;
      BitmapFormatHelper := nil;
      TmpBmp.Destroy;
   end;
end;{TFreeBackgroundImage.Draw}

function TFreeBackgroundImage.ImageCoordinate(X,Y:Integer):TPoint;
Var Dest:TRect;
begin
   Dest:=TargetRect;
   Result.X:=round(((X-Dest.Left)/(Dest.Right-Dest.Left))*FBitmap.Width);
   Result.Y:=round(((Y-Dest.Top)/(Dest.Bottom-Dest.Top))*FBitmap.Height);
end;{TFreeBackgroundImage.ImageCoordinate}

function TFreeBackgroundImage.TargetRect:TRect;
var Pt : TPoint;
begin
   Pt:=FOwner.Project(ZERO);
   Result.Left:=Pt.X-round(Owner.Scale*Owner.Zoom*FScale*FOrigin.X);
   Result.Top:=Pt.Y-round(Owner.Scale*Owner.Zoom*FScale*FOrigin.Y);
   Result.Right:=Result.Left+Round(Owner.Scale*Owner.Zoom*FScale*FBitmap.Width);
   Result.Bottom:=Result.Top+Round(Owner.Scale*Owner.Zoom*FScale*FBitmap.Height);
end;{TFreeBackgroundImage.TargetRect}

procedure TFreeBackgroundImage.Open(InitialDir:String);
var Dialog     : TOpenDialog;
    Pt         : TPoint;
    P2D        : T2DCoordinate;
    JPEGImage  : TJPEGImage;
begin
   Dialog:=TOpenDialog.Create(Application);
   Dialog.InitialDir:=InitialDir;
   Dialog.Filter:='All files (*.jpg;*.bmp)|*.jpg; *.bmp|Jpeg images (*.jpg)|*.jpg|Bitmap files (*.bmp)|*.bmp|';
   Dialog.Options:=[ofHideReadOnly];
   if Dialog.Execute then
   begin
      Clear;
      FShowInView:=Owner.ViewType;

      FBitmap:=TBitmap.Create;
      if Uppercase(ExtractFileExt(Dialog.Filename))='.JPG' then
      begin
         JPEGImage:=TJPEGImage.Create;
         JPEGImage.LoadFromFile(Dialog.FileName);
         FBitmap.Assign(JPEGImage);
         JPEGImage.Destroy;
         FQuality:=JPEGImage.CompressionQuality;
      end else
      begin
         FBitmap.LoadFromFile(Dialog.Filename);
         FQuality:=100;
      end;
      // calculate scale
      Pt:=Owner.Project(ZERO);
      Pt.X:=Owner.ClientWidth;
      P2D:=Owner.ProjectBackTo2D(Pt);
      FOrigin.X:=0;
      FOrigin.Y:=FBitmap.Height;
      FScale:=P2D.X/FBitmap.Width;
      FOwner.Refresh;
      if (not (csdestroying in owner.componentstate)) then if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end;
   Dialog.Destroy;
end;{TFreeBackgroundImage.Open}

procedure TFreeBackgroundImage.Save;
var Image      : TJPEGImage;
    SaveDialog : TSaveDialog;
begin
   SaveDialog:=TSaveDialog.Create(Owner);
   SaveDialog.FileName:='image.jpg';
   SaveDialog.Filter:='Jpeg files (*.jpg)|*.jpg';
   Savedialog.Options:=[ofOverwritePrompt,ofHideReadOnly];
   if SaveDialog.Execute then
   begin
      Image:=TJPEGImage.Create;
      Image.Assign(FBitmap);
      Image.CompressionQuality:=FQuality;
      Image.SaveToFile(ChangeFileExt(SaveDialog.Filename,'.jpg'));
      Image.Destroy;
   end;
   SaveDialog.Destroy;
end;{TFreeBackgroundImage.Save}

procedure TFreeBackgroundImage.SetBlendingValue;
var Dialog  : TFreeBackgroundBlendDialog;
    Old     : Byte;
begin
   Dialog:=TFreeBackgroundBlendDialog.Create(Owner);
   ShowTranslatedValues(Dialog);
   Old:=FAlpha;
   if Dialog.Execute(Owner) then
   begin
      if assigned(Owner.FOnChangeBackgroundImage) then Owner.FOnChangeBackgroundImage(Owner);
   end else Alpha:=Old;
   Dialog.Destroy;
end;{TFreeBackgroundImage.SetBlendingValue}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeViewport                                           }
{                                                                                                   }
{ This is a 3D drawingcanvas.                                                                       }
{---------------------------------------------------------------------------------------------------}
procedure TFreeViewport.FSetAngle(Val:TFloatType);
begin
   if Val<>FAngle then
   begin
      FAngle:=Val;
      FCosAngle:=Cos(DegToRad(FAngle));
      FSinAngle:=sin(DegToRad(FAngle));
      FHorScrollbarChange(self);
      InitializeViewport(FMin3D,FMax3D);
   end;
end;{TFreeViewport.FSetAngle}

procedure TFreeViewport.FSetBackgroundMode(val:TFreeViewportBackgroundMode);
begin
   FBackgroundMode:=val;
   Case FBackGroundMode of
      emSetOrigin : Cursor:=crSetOrigin;
      emSetScale  : Cursor:=crSetScale;
      emSetTransparentColor:Cursor:=crTranspCol;
      else Cursor:=crCross;
   end;
end;{TFreeViewport.FSetBackgroundMode}

procedure TFreeViewport.FSetBrushStyle(Val:TBrushStyle);
begin
   if FDrawingcanvas.Brush.Style<>val then FDrawingcanvas.Brush.Style:=Val;
end;{TFreeViewport.FSetBrushStyle}

function TFreeViewport.FGetPenStyle:TPenStyle;
begin
   Result:=FDrawingCanvas.Pen.Style;
end;{TFreeViewport.FGetPenStyle}

function TFreeViewport.FGetPenWidth:integer;
begin
   Result:=FDrawingCanvas.Pen.Width;
end;{TFreeViewport.FGetPenWidth}

function TFreeViewport.FGetPrinting:Boolean;
begin
   if Printer=nil then Result:=False else Result:=FPrinting and (FDrawingcanvas=printer.Canvas);
end;{TFreeViewport.FGetPrinting}

function TFreeViewport.FGetBrushStyle:TBrushStyle;
begin
   Result:=FDrawingcanvas.Brush.Style;
end;{TFreeViewport.FGetBrushStyle}

function TFreeViewport.FGetPenColor:TColor;
begin
   Result:=FDrawingCanvas.Pen.Color;
end;{TFreeViewport.FGetPenColor}

function TFreeViewport.FGetBrushColor:TColor;
begin
   Result:=FDrawingCanvas.Brush.Color;
end;{TFreeViewport.FGetBrushColor}

function TFreeViewport.FGetFontColor:TColor;
begin
   Result:=FDrawingCanvas.Font.Color;
end;{TFreeViewport.FGetFontColor}

function TFreeViewport.FGetFontName:string;
begin
   Result:=FDrawingcanvas.Font.Name;
end;{TFreeViewport.FGetFontName}

function TFreeViewport.FGetFontSize:Integer;
begin
   Result:=FDrawingCanvas.Font.Size;
end;{TFreeViewport.FGetFontSize}

procedure TFreeViewport.StretchDraw(DestRect:TRect; bmp:TBitmap);
begin
   FDrawingCanvas.StretchDraw(DestRect, bmp);
end;


function TFreeViewport.TextWidth(val:string):integer;
begin
   result := FDrawingCanvas.TextWidth(val);
end;

function TFreeViewport.TextHeight(val:string):integer;
begin
   result := FDrawingCanvas.TextHeight(val);
end;

procedure TFreeViewport.TextOut(x,y:integer; val:string);
begin
   FDrawingCanvas.TextOut(x,y, val);
end;


procedure TFreeViewport.FSetCameraType(Val:TFreeCameraType);
var Film : TFloatType;
    Dist : TFloatType;
begin
   if Val<>FCameraType then
   begin
      Film:=35;   // standard 35mm. film
      Case Val of
         ftWide         : Dist:=20;
         ftStandard     : Dist:=50;
         ftShortTele    : Dist:=90;
         ftMediumTele   : Dist:=130;
         ftFarTele      : Dist:=200;
         else Dist:=50;
      end;
      FCameraType:=Val;
      FFieldOfView:=RadToDeg(ArcTan(Film/Dist));
      InitializeViewport(FMin3D,FMax3D);
   end;
end;{TFreeViewport.FSetCameraType}

procedure TFreeViewport.FSetElevation(Val:TFloatType);
begin
   if Val<>FElevation then
   begin
      FElevation:=Val;
      FCosElevation:=Cos(DegToRad(FElevation));
      FSinElevation:=sin(DegToRad(FElevation));
      FVertScrollbarChange(self);
      InitializeViewport(FMin3D,FMax3D);
   end;
end;{TFreeViewport.FSetElevation}

procedure TFreeViewport.FSetPan(Val:TPoint);
begin
   if (FPan.X<>Val.X) or (FPan.Y<>Val.Y) then
   begin
      FPan:=Val;
      Refresh;
   end;
end;{TFreeViewport.FSetPan}

procedure TFreeViewport.FSetPenColor(Val:TColor);
begin
   if FDrawingcanvas.Pen.Color<>val then FDrawingcanvas.Pen.Color:=Val;
end;{TFreeViewport.FSetPenColor}

procedure TFreeViewport.FSetBrushColor(Val:TColor);
begin
   if FDrawingcanvas.Brush.Color<>val then FDrawingcanvas.Brush.Color:=Val;
end;{TFreeViewport.FSetBrushColor}

procedure TFreeViewport.FSetFontColor(Val:TColor);
begin
   if FDrawingcanvas.Font.Color<>val then FDrawingcanvas.Font.Color:=Val;
end;{TFreeViewport.FSetFontColor}

procedure TFreeViewport.FSetFontName(val:string);
begin
   if Uppercase(FDrawingcanvas.Font.Name)<>uppercase(name) then FDrawingcanvas.Font.Name:=Name;
end;{TFreeViewport.FSetFontName}

procedure TFreeViewport.FSetFontSize(val:integer);
begin
   if FDrawingcanvas.Font.Size<>val then FDrawingcanvas.Font.Size:=val;
end;{TFreeViewport.FSetFontSize}

function TFreeViewport.FGetFontHeight:integer;
begin
   //Result:=FDrawingCanvas.TextHeight('Xy');
   Result:=FDrawingCanvas.Font.Height;
end;{TFreeViewport.FGetFontSize}

procedure TFreeViewport.FSetFontHeight(val:integer);
var Height         : TFloatType;
  CurrentHeight  : Integer;
begin
  FDrawingCanvas.Font.Height := val;
  // Sets the fontheight to a height in modelspace
  {Height:=val*Self.Scale*Self.Zoom;
  FSetFontSize(8);
  CurrentHeight:=FDrawingCanvas.TextHeight('Xy');
  while CurrentHeight>Height do
    begin
    FDrawingCanvas.Font.Size:=FDrawingCanvas.Font.Size-1;
    CurrentHeight:=FDrawingCanvas.TextHeight('Xy');
    if FDrawingCanvas.Font.Size<3 then break;
    end;}
end;{SetFontHeight}


procedure TFreeViewport.FSetHorScrollbar(val:TScrollbar);
begin
   if FHorScrollbar<>nil then FHorScrollbar.OnChange:=nil;
   FHorScrollbar:=Val;
   if FHorScrollbar<>nil then
   begin
      FHorScrollbar.OnChange:=FHorScrollbarChange;
      FHorScrollbar.Min:=-180;
      FHorScrollbar.Max:=180;
      if FHorScrollbar.Position<>round(angle) then FHorScrollbar.Position:=Round(Angle);
      FHorScrollbar.Visible:=ViewType=fvPerspective;
   end;
end;{TFreeViewport.FSetHorScrollbar}

procedure TFreeViewport.FSetVertScrollbar(val:TScrollbar);
begin
   if FVertScrollbar<>nil then FVertScrollbar.OnChange:=nil;
   FVertScrollbar:=Val;
   if FVertScrollbar<>nil then
   begin
      FVertScrollbar.OnChange:=FVertScrollbarChange;
      FVertScrollbar.Min:=-180;
      FVertScrollbar.Max:=180;
      if FVertScrollbar.Position<>round(Elevation) then FVertScrollbar.Position:=Round(Elevation);
      FVertScrollbar.Visible:=ViewType=fvPerspective;
   end;
end;{TFreeViewport.FSetVertScrollbar}

procedure TFreeViewport.FSetMargin(Val:TFloatType);
begin
   if Val<0 then Val:=0;
   if Val<>FMargin then
   begin
      FMargin:=Val;
      if not (csDesigning in ComponentState) then
      begin
         if Zoom<>1.0 then Refresh
                      else ZoomExtents;
      end;
   end;
end;{TFreeViewport.FSetMargin}

procedure TFreeViewport.FSetPenStyle(Val:TPenStyle);
begin
   if FDrawingcanvas.Pen.Style<>val then FDrawingcanvas.Pen.Style:=Val;
end;{TFreeViewport.FSetPenStyle}

procedure TFreeViewport.FSetPenWidth(Val:integer);
begin
   if FDrawingcanvas.Pen.Width<>val then FDrawingcanvas.Pen.Width:=Val;
end;{TFreeViewport.FSetPenWidth}

function TFreeViewport.FGetPrintScaleFactor:TFloatType;
begin
   Result:=FPrintScaleFactor;
end;{TFreeViewport.FGetPrintScaleFactor}

procedure TFreeViewport.FSetViewType(Val:TFreeViewType);
begin
   if Val<>FViewType then
   begin
      FViewType:=Val;
      FZoom:=1.0;
      FPan.X:=0;
      FPan.Y:=0;
      Case FViewtype of
         fvBodyplan :begin
                        FAngle:=0;
                        FElevation:=0;
                     end;
         fvProfile  :begin
                        FAngle:=90;
                        FElevation:=0;
                     end;
         fvPlan     :begin
                        FAngle:=90;
                        FElevation:=90;
                     end;
         fvPerspective:begin
                        FAngle:=20;
                        FElevation:=20;
                     end;
      end;
      if FHorScrollbar<>nil then
      begin
         FHorScrollbar.Position:=round(Angle);
         FHorScrollbar.Visible:=ViewType=fvPerspective;
         if FHorScrollbar.Position<>round(angle) then FHorScrollbar.Position:=Round(Angle);
      end;
      if FVertScrollbar<>nil then
      begin
         FVertScrollbar.Position:=round(Elevation);
         FVertScrollbar.Visible:=ViewType=fvPerspective;
         if FVertScrollbar.Position<>round(Elevation) then FVertScrollbar.Position:=Round(Elevation);
      end;
      if assigned(FOnRequestBackgroundImage) then FOnRequestBackgroundImage(self);
      if assigned(FOnRequestExtents) then FOnRequestExtents(self,FMin3D,FMax3D);
      InitializeViewport(FMin3D,FMax3D);
      if assigned(FOnChangeViewType) then FOnChangeViewType(self);
   end;
end;{TFreeViewport.FSetViewType}

procedure TFreeViewport.FSetViewportMode(Val:TFreeViewportMode);
begin
   if Val<>FViewportMode then
   begin
      FViewportmode:=Val;
      Refresh;
   end;
end;{TFreeViewport.FSetViewportMode}

procedure TFreeViewport.FHorScrollbarChange(sender:TObject);
begin
   if FHorScrollbar<>nil then
   begin
      if Round(Angle)<>FHorScrollbar.Position then Angle:=FHorScrollbar.Position;
   end;
end;{TFreeViewport.FHorScrollbarChange}

procedure TFreeViewport.FVertScrollbarChange(sender:TObject);
begin
   if FVertScrollbar<>nil then
   begin
      if Round(Elevation)<>FVertScrollbar.Position then Elevation:=FVertScrollbar.Position;
   end;
end;{TFreeViewport.FVertScrollbarChange}

procedure TFreeViewport.WMMouseEnter(var Message: TMessage);
begin
   inherited;
   if assigned(FOnMouseEnter) then FOnMouseEnter(self);
   //if not self.Focused then Setfocus;
end;{TFreeViewport.WMMouseEnter}

procedure TFreeViewport.WMMouseLeave(var Message: TMessage);
begin
   inherited;
   if self.Cursor<>crCross then Cursor:=crCross;
   if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;{TFreeViewport.WMMouseLeave}

constructor TFreeViewport.Create(AOwner:TComponent);
begin
   Inherited create(AOwner);
   FPrinting:=False;
   FBackgroundMode:=emNormal;
   FMargin:=0.0;
   FPrintResolution:=1;
   FPrintScaleFactor:=1.0;
   FAngle:=20;
   FElevation:=20;
   FDistance:=1e4;
   FZoom:=1.0;
   FPan.X:=0;
   FPan.Y:=0;
   FOnRequestBackgroundImage:=nil;
   FCameraType:=ftStandard; // Standard 50mm. lens, field of view= 35/50 =35 degrees.
   FFieldOfView:=RadToDeg(ArcTan(35/50));
   FViewType:=fvPerspective;
   FDoubleBuffer:=True;
   FBackgroundImage:=TFreeBackgroundImage.Create(Self);
   FHorScrollbar:=nil;
   FVertScrollbar:=nil;

   //Inherited create(AOwner);
   FDrawingBuffer:=TBitmap.Create ;
   // FDrawingBuffer.PixelFormat := pf24bit; // Use 24 bit for faster pixel-access when shading
   // We cannot use incompatible format in X11. It will cause desynchronization of RawImage with Canvas,
   // so Canvas operations will cause to "forget" previous RawImage changes.
   // We will create native buffer Bitmap and convert ScanLine pixels to/from TRGBTriple using TFreeBitmapFormatHelper
   // NO. It does not work. Currently all works via Canvas.Pixels[x,y]. Quite good.
   // NO2. It does work. Currently Canvas.Pixels[x,y] causes severe memory leak SetPixel creates Pen each call.
   //      TFreeBitmapFormatHelper works Quite good though requires native 24/32-bit formats RGB or BGR.
   FDrawingBuffer.Width:=10;
   FDrawingBuffer.Height:=10;
   //Canvas.Pen.Color:=clBlack;
   //Canvas.Brush.Color:=clWhite;
   //FDrawingBuffer.Canvas.FillRect(0,0,10,10); // to init handles
   FBitmapFormatHelper := TFreeBitmapFormatHelper.Create(FDrawingBuffer);
   Logger.Debug('FBitmapFormatHelper:'+FBitmapFormatHelper.AsString);
   FDrawingCanvas:=Canvas;

   FZBuffer:=TFreeZBuffer.Create;
   FZBuffer.FViewport:=self;
   FAlphaBuffer:=TFreeAlphaBuffer.Create;
   FAlphaBuffer.FViewport:=self;
   FLight.Position.X:=50;
   FLight.Position.Y:=20;
   FLight.Position.Z:=2;
   FLight.Ambient:=75;
   FLight.Luminance:=140;
   FViewportmode:=vmWireFrame;
   // Load cursors from resource file
   Screen.Cursors[crRotate]:=LoadCursor(hInstance,'ROTATEVIEWPORT');
   Screen.Cursors[crPan]:=LoadCursor(hInstance,'PANVIEWPORT');
   Screen.Cursors[crSetOrigin]:=LoadCursor(hInstance,'SETORIGIN');
   Screen.Cursors[crSetScale]:=LoadCursor(hInstance,'SETSCALE');
   Screen.Cursors[crTranspCol]:=LoadCursor(hInstance,'TRANSPARENTCOLOR');

   FontName := 'Courier';
   FontSize := 8;
   FontColor := clWhite;

   logger.debug('TFreeViewport.Create: done');

end;{TFreeViewport.Create}

destructor TFreeViewport.Destroy;
begin
   if Assigned(FDrawingBuffer)
      then FDrawingBuffer.Destroy;
   FDrawingBuffer := nil;
   if Assigned(FZBuffer)
      then FZBuffer.Destroy;
   FZBuffer := nil;
   if Assigned(FAlphaBuffer)
      then FAlphaBuffer.Destroy;
   FAlphaBuffer := nil;
   if Assigned(FBackgroundImage)
      then FBackgroundImage.Destroy;
   FBackgroundImage := nil;
   if Assigned(FBitmapFormatHelper)
      then FBitmapFormatHelper.Destroy;
   inherited Destroy;
end;{TFreeViewport.Destroy}

procedure TFreeViewport.BeginUpdate;
begin
   if not FUpdating then
   begin
     FDrawingBuffer.BeginUpdate(false);
     FUpdating := true;
   end;
end;

procedure TFreeViewport.EndUpdate;
begin
   if FUpdating then
   begin
     FDrawingBuffer.EndUpdate;
     FUpdating := false;
   end;
end;

procedure TFreeViewport.GetPixel(X,Y:Integer; out R,G,B:byte);
var pRow, pPixel : pointer;
    Pixel : TRGBTriple;
    A: byte;
    //Row           : pRGBTripleArray;
    //Clr:TColor;
begin
   // Scanline method. Came from Delphy
   //Row:=FDrawingBuffer.Scanline[P1.Y];

   // My method via FBitmapFormatHelper
   {pRow := FDrawingBuffer.RawImage.GetLineStart(Y);
   pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * X;
   Pixel := FBitmapFormatHelper.ToTRGBTriple(pPixel);
   R:=Pixel.rgbtRed;
   G:=Pixel.rgbtGreen;
   B:=Pixel.rgbtBlue;}
  FBitmapFormatHelper.GetPixel(FDrawingBuffer, X,Y, R,G,B,A);

   // Canvas.Pixels method.
   // temporary draw via canvas. Appeared it works well in QT.
  { //Clr:=FDrawingBuffer.Canvas.Pixels[P1.X,P1.Y];  // this causes mem leak in LibQt4 DCSetPixel (qtobject.inc) calling QPen_create5
      B:=Blue(Clr);
      G:=Green(Clr);
      R:=Red(Clr);
   }
end;

procedure TFreeViewport.SetPixel(X,Y:Integer; R,G,B:byte);
//var Row           : pRGBTripleArray;
begin
  //BeginUpdate;   //do it on higher level

  // Scanline method. Came from Delphy
  //Row:=FDrawingBuffer.Scanline[Y];
  //with TRGBTriple(Row^[X]) do begin rgbtBlue:=B; rgbtGreen:=G; rgbtRed:=R; end;

  // My method via FBitmapFormatHelper

  FBitmapFormatHelper.SetPixel(FDrawingBuffer, X,Y, R,G,B,255);

  //EndUpdate;    //do it on higher level

  // Canvas.Pixels method.
  // temporary draw via canvas. Appeared it works well in QT.
  //FDrawingBuffer.Canvas.Pixels[P1.X,P1.Y]:=RGBtoColor(R,G,B);  // this causes mem leak in LibQt4 DCSetPixel (qtobject.inc) calling QPen_create5
end;

procedure TFreeViewport.DrawLineToZBuffer(Point1,Point2:T3DCoordinate;R,G,B:byte);
var D           : Integer;
    P1,P2         : TShadePoint;
    ax,ay,sx,sy   : Integer;
    dx,dy,W,H     : Integer;
    dZ            : TFloatType;
    //Row           : pRGBTripleArray;
    pRow, pPixel : pointer;
    Pixel : TRGBTriple;
begin
   P1:=self.ProjectToZBuffer(ZBufferScaleFactor,Point1);
   W:=ClientWidth;
   H:=ClientHeight;
   P2:=self.ProjectToZBuffer(ZBufferScaleFactor,Point2);

   //BeginUpdate;  // do not do that here otherwise background jumps blask-and-gray on movements
   // do this outside cycle that draws lines

   dx:=P2.X-P1.X;
   dy:=P2.Y-P1.Y;
   ax:=Abs(dx) shl 1;
   ay:=Abs(dy) shl 1;
   if dx>=0 then sx:=1 else sx:=-1;
   if dy>=0 then sy:=1 else sy:=-1;
   if ax>ay then
   begin
      if dx=0 then dz:=0
              else dz:=(P2.Z-P1.Z)/abs(dx);
      d:=ay-ax shr 1;
      While P1.X<>P2.X do
      begin
         if (P1.Y>0) and (P1.Y<H) then
         begin
            if (P1.Y>-1) and (P1.Y<H) and (P1.X>0) and (P1.X<W) then
            begin
               if P1.Z>=FZBuffer.FBuffer[P1.Y][P1.X] then
               begin
                  SetPixel(P1.X, P1.Y, R,G,B);
                  FZBuffer.FBuffer[P1.Y][P1.X]:=P1.Z;
               end;
            end;
         end;
         if D>=0 then
         begin
            inc(P1.Y,sy);
            dec(d,ax);
         end;
         P1.Z:=P1.Z+dZ;
         inc(P1.X,sx);
         inc(d,ay);
      end;
   end else
   begin
      if dy=0 then dZ:=0
              else dz:=(P2.Z-P1.Z)/abs(dy);
      d:=ax-ay shr 1;
      while P1.Y<>P2.Y do
      begin
         if (P1.Y>0) and (P1.Y<H) then
         begin
            if (P1.Y>-1) and (P1.Y<H) and (P1.X>0) and (P1.X<W) then
            begin
               if P1.Z>=FZBuffer.FBuffer[P1.Y][P1.X] then
               begin
                 SetPixel(P1.X, P1.Y, R,G,B);
                 FZBuffer.FBuffer[P1.Y][P1.X]:=P1.Z;
               end;
            end;
         end;
         if d>=0 then
         begin
            inc(P1.X,sx);
            dec(d,ay);
         end;
         inc(P1.Y,sy);
         P1.Z:=P1.Z+dZ;
         inc(d,ax);
      end;
   end;
   //EndUpdate;
end;{DrawPolylineToZBuffer}

procedure TFreeViewport.InitializeViewport(Min,Max:T3DCoordinate);
// This procedure initializes the viewports and sets the scale in such a way
// that the model completely fills the viewport
var P             : array[1..8] of T3DCoordinate;
    Projected     : T2DCoordinate;
    Min2D,Max2D   : T2DCoordinate;
    P3D           : T3DCoordinate;
    I             : integer;
    VertCorr      : integer;
    HorCorr       : Integer;
    Width,Height  : TFloatType;
    Tmp           : TFloatType;
    XScale        : TFloatType;
    YScale        : TFloatType;
    Pt1,Pt2       : TPoint;
    Diff          : T3DCoordinate;

    procedure MinMax(P:T2DCoordinate);
    begin
       if P.X<Min2D.X then Min2D.X:=P.X;
       if P.Y<Min2D.Y then Min2D.Y:=P.Y;
       if P.X>Max2D.X then Max2D.X:=P.X;
       if P.Y>Max2D.Y then Max2D.Y:=P.Y;
    end;

begin
   // Add margin
   Diff:=ScalePoint(0.01*FMargin,Subtract(Max,Min));
   FMin3D:=Subtract(Min,Diff);
   Diff:=ScalePoint(-1.0,Diff);
   FMax3D:=Subtract(Max,diff);
   // Calculate the midpoint of the boundingbox, which is used as the center of the model for rotating the model
   FMidPoint:=MidPoint(FMin3D,FMax3D);
   // Calculate the distance of the camera to the center of the model, following from the field of view from the camera
   Tmp:=Sqrt(Sqr(FMax3D.Y-FMin3D.Y)+Sqr(FMax3D.Z-FMin3D.Z));
   if Tmp=0 then Tmp:=1e-2;
   if FViewtype=fvPerspective then
   begin
      if ArcTan(DegToRad(FFieldOfView))<>0 then
      begin
         FDistance:=1.5*Tmp/ArcTan(DegToRad(FFieldOfView));
         if FDistance>1e5 then FDistance:=1e5;
      end else FDistance:=1e5;
   end else FDistance:=1e8;
   FCameraLocation.X:=FMax3D.X+FDistance;
   FCameraLocation.Y:=FMidPoint.Y;
   FCameraLocation.Z:=FMidPoint.Z;

   FCosAngle:=Cos(DegToRad(FAngle));
   FSinAngle:=sin(DegToRad(FAngle));
   FCosElevation:=Cos(DegToRad(FElevation));
   FSinElevation:=sin(DegToRad(FElevation));

   // now project all 8 cornerpoints of the bounding box to 2D
   // in order to determin the min. and max. 2D coordinates of the 2D viewport.
   P[1].X:=FMin3D.X;
   P[1].Y:=FMin3D.Y;
   P[1].Z:=FMin3D.Z;
   P[2].X:=FMax3D.X;
   P[2].Y:=FMin3D.Y;
   P[2].Z:=FMin3D.Z;
   P[3].X:=FMax3D.X;
   P[3].Y:=FMax3D.Y;
   P[3].Z:=FMin3D.Z;
   P[4].X:=FMin3D.X;
   P[4].Y:=FMax3D.Y;
   P[4].Z:=FMin3D.Z;
   P[5].X:=FMin3D.X;
   P[5].Y:=FMin3D.Y;
   P[5].Z:=FMax3D.Z;
   P[6].X:=FMax3D.X;
   P[6].Y:=FMin3D.Y;
   P[6].Z:=FMax3D.Z;
   P[7].X:=FMax3D.X;
   P[7].Y:=FMax3D.Y;
   P[7].Z:=FMax3D.Z;
   P[8].X:=FMin3D.X;
   P[8].Y:=FMax3D.Y;
   P[8].Z:=FMax3D.Z;
   for I:=1 to 8 do
   begin
      P3D:=RotatedPoint(P[i]);
      // apply perspective projection
      Tmp:=FCameralocation.X-P3D.X;
      // apply perspective correction
      Projected.X:=FCameralocation.X*P3D.Y/Tmp;
      Projected.Y:=FCameralocation.X*P3D.Z/Tmp;
      if I=1 then
      begin
         Min2D:=Projected;
         Max2D:=Projected;
      end;
      MinMax(Projected);
   end;
   Width:=Max2D.X-Min2D.X;
   Height:=Max2D.Y-Min2D.Y;
   if (abs(Width)>1e-7) and (abs(Height)>1e-7) then
   begin
      XScale:=ClientWidth/Width;
      YScale:=ClientHeight/Height;
      if XScale<YScale then FScale:=XScale
                       else FScale:=YScale;
   end else FScale:=1;
   // Decrease Scale with 1% to keep the model free from the edges
   FScale:=0.99*FScale;
   FScreencenter.X:=ClientWidth div 2;
   FScreencenter.Y:=ClientHeight div 2;
   // Calculate correction (pan vector) to make sure
   // that the ship appears in the middle of the viewport
   Pt1.X:=FScreencenter.X+Round(FScale*Min2D.X);
   Pt1.Y:=FScreencenter.Y-Round(FScale*Min2D.Y);
   Pt2.X:=FScreencenter.X+Round(FScale*Max2D.X);
   Pt2.Y:=FScreencenter.Y-Round(FScale*Max2D.Y);
   HorCorr:=(Pt1.X+Pt2.X)div 2 - ClientWidth div 2;
   VertCorr:=(Pt1.Y+Pt2.Y)div 2 - ClientHeight div 2;
   FScreencenter.X:=FScreencenter.X-HorCorr;
   FScreencenter.Y:=FScreencenter.Y-vertCorr;
   // Remember the min/max values
   FMin3D:=Min;
   FMax3D:=Max;
   // Now force a complete repaint
   Invalidate;
end;{TFreeViewport.InitializeViewport}

procedure TFreeViewport.Print(Units:TFreeUnitType;AskPrintScale:Boolean;Jobname:string);
var CanvasWidth   : Integer;
    CanvasHeight  : Integer;
    Resolution    : Integer;
    I             : Integer;
    PrintScale    : extended;
    NewPrintScale : extended;
    XScale        : extended;
    OldZoom       : extended;
    YScale        : extended;
    Scale,Tmp     : extended;
    Width,Height  : extended;
    ModelSize     : extended;
    OldScale      : extended;
    OldPan        : TPoint;
    OldCanvas     : TCanvas;
    OldCenter     : TPoint;
    P             : array[1..8] of T3DCoordinate;
    P3D           : T3DCoordinate;
    Projected     : T2DCoordinate;
    Min2D,Max2D   : T2DCoordinate;
    ProjMidPoint  : TPoint;
    AbortPrinting : Boolean;
    Str           : AnsiString;

    procedure MinMax(P:T2DCoordinate);
    begin
       if P.X<Min2D.X then Min2D.X:=P.X;
       if P.Y<Min2D.Y then Min2D.Y:=P.Y;
       if P.X>Max2D.X then Max2D.X:=P.X;
       if P.Y>Max2D.Y then Max2D.Y:=P.Y;
    end;
begin
   if Printer<>nil then
   begin
      // Backup settings;
      OldScale:=FScale;               
      OldPan:=FPan;
      OldCanvas:=FDrawingCanvas;
      OldCenter:=FScreencenter;
      OldZoom:=FZoom;
      try
         CanvasWidth:=Printer.PageWidth;
         CanvasHeight:=Printer.PageHeight;
         Resolution:=Printer.XDPI; // GetDeviceCaps(Printer.Handle, LogPixelsX);
         FPrintResolution:=Resolution;
         FZoom:=1.0;
         FScale:=1.0;
         FPan.X:=0;
         FPan.Y:=0;
         FScreencenter.X:=Canvaswidth div 2;
         FScreencenter.Y:=Canvasheight div 2;
         // project all 8 cornerpoints of the bounding box to 2D
         // in order to determin the min. and max. 2D coordinates of the 2D viewport.
         P[1].X:=FMin3D.X;
         P[1].Y:=FMin3D.Y;
         P[1].Z:=FMin3D.Z;
         P[2].X:=FMax3D.X;
         P[2].Y:=FMin3D.Y;
         P[2].Z:=FMin3D.Z;
         P[3].X:=FMax3D.X;
         P[3].Y:=FMax3D.Y;
         P[3].Z:=FMin3D.Z;
         P[4].X:=FMin3D.X;
         P[4].Y:=FMax3D.Y;
         P[4].Z:=FMin3D.Z;
         P[5].X:=FMin3D.X;
         P[5].Y:=FMin3D.Y;
         P[5].Z:=FMax3D.Z;
         P[6].X:=FMax3D.X;
         P[6].Y:=FMin3D.Y;
         P[6].Z:=FMax3D.Z;
         P[7].X:=FMax3D.X;
         P[7].Y:=FMax3D.Y;
         P[7].Z:=FMax3D.Z;
         P[8].X:=FMin3D.X;
         P[8].Y:=FMax3D.Y;
         P[8].Z:=FMax3D.Z;
         for I:=1 to 8 do
         begin
            P3D:=RotatedPoint(P[i]);
            // apply perspective projection
            Tmp:=FCameralocation.X-P3D.X;
            // apply perspective correction
            Projected.X:=FScreenCenter.X+(FScale*FCameralocation.X*P3D.Y/Tmp);
            Projected.Y:=FScreenCenter.Y-(FScale*FCameralocation.X*P3D.Z/Tmp);
            if I=1 then
            begin
               Min2D:=Projected;
               Max2D:=Projected;
            end;
            MinMax(Projected);
         end;
         Width:=Max2D.X-Min2D.X;
         Height:=Max2D.Y-Min2D.Y;
         if (Width<>0) and (Height<>0) then
         begin
            XScale:=CanvasWidth/Width;
            YScale:=CanvasHeight/Height;
            if XScale<YScale then Scale:=XScale
                             else Scale:=YScale;
         end else Scale:=1;
         // Set max. scale
//         Scale:=0.99*Scale;
         FScale:=Scale;

         // Calculate the size at modelspace
         ModelSize:=Round(Width*FScale);  // Size in pixels
         ModelSize:=ModelSize/Resolution;    // size in inches
         if Units=fuMetric then ModelSize:=ModelSize*0.01*2.54 // size in meters
                           else ModelSize:=ModelSize/12;       // size in feet
         PrintScale:=Width/ModelSize;
         if AskPrintScale then
         begin
            Str:=IntToStr(Trunc(PrintScale)+1);
            if InputQuery(Userstring(193),Userstring(194)+' 1:',Str) then
            begin
               NewPrintScale:=StrToFloat(Str);
               if NewPrintScale<0 then
               begin
                  NewPrintScale:=1;
                  AbortPrinting:=True;
               end else AbortPrinting:=False;
            end else
            begin
               AbortPrinting:=True;
               NewPrintScale:=PrintScale;
            end;
         end else
         begin
            // set the new scale
            NewPrintScale:=PrintScale;
            AbortPrinting:=False;
         end;
         FScale:=FScale*(PrintScale/NewPrintscale);

         // check
         for I:=1 to 8 do
         begin
            Projected.X:=Project(P[i]).X;
            Projected.Y:=Project(P[i]).Y;
            if I=1 then
            begin
               Min2D:=Projected;
               Max2D:=Projected;
            end;
            MinMax(Projected);
         end;
         ProjMidPoint.X:=round(Min2D.X+Max2D.X) div 2;
         ProjMidPoint.Y:=round(Min2D.Y+Max2D.Y) div 2;
         FPan.X:=(Canvaswidth div 2)-ProjMidPoint.X;
         FPan.Y:=(CanvasHeight div 2)-ProjMidPoint.Y;
         FDrawingCanvas:=Printer.Canvas;
         FPrinting:=True;
         FDestinationWidth:=CanvasWidth;
         if not AbortPrinting then
         begin
            //  printscale factor on basis of resolutin and printsize
            if Max2D.X-Min2D.X>Max2D.Y-Min2D.Y then Tmp:=Max2D.X-Min2D.X
                                               else Tmp:=Max2D.Y-Min2D.Y;
            FPrintScalefactor:=(PrintResolution/200)*(Tmp/4000);

            FDestinationHeight:=CanvasHeight;
            Printer.Title:=JobName;
            Printer.BeginDoc;
            refresh;
            Printer.EndDoc;            
         end;
      finally
         // Restore settings;
         FPrinting:=false;
         FPrintResolution:=1;
         FPrintScaleFactor:=1.0;
         FDrawingCanvas:=OldCanvas;
         FScale:=OldScale;
         FPan:=OldPan;
         FZoom:=OldZoom;
         FScreencenter:=OldCenter;
         Refresh;
      end;
   end else MessageDlg(Userstring(195)+'!',mtError,[mbOk],0)
end;{TFreeViewport.Print}

function TFreeViewport.Project(P:T3DCoordinate):TPoint;
var P3D  : T3DCoordinate;
    P2D  : T2DCoordinate;
    Dist:TFloatType;
begin
   P3D:=RotatedPoint(P);
   Dist:=FCameralocation.X-P3D.X;
   // apply perspective correction
   P2D.X:=FCameralocation.X*P3D.Y/dist;
   P2D.Y:=FCameralocation.X*P3D.Z/Dist;
   Result.X:=FPan.X+FScreencenter.X+Round(FZoom*FScale*P2D.X);
   Result.Y:=FPan.Y+FScreencenter.Y-Round(FZoom*FScale*P2D.Y);
end;{TFreeViewport.Project}

function TFreeViewport.ProjectBack(P:TPoint;Input:T3DCoordinate):T3DCoordinate;
var P2D     : T2DCoordinate;
    P1,P2   : T3DCoordinate;
    P3D     : T3DCoordinate;
    Dist    : TFloatType;

   Function Dist_PL_3D(P,P1,P2:T3DCoordinate):T3DCoordinate;
   var t:TFloatType;
   begin
      t:=-((P1.X-P.X)*(P2.X-P1.X)+(P1.Y-P.Y)*(P2.Y-P1.Y)+(P2.Z-P.Z)*(P2.Z-P1.Z))/
          (sqr(P2.X-P1.X)+sqr(P2.Y-P1.Y)+sqr(P2.Z-P1.Z));
      Result.X:=P1.X+T*(P2.X-P1.X);
      Result.Y:=P1.Y+T*(P2.Y-P1.Y);
      Result.Z:=P1.Z+T*(P2.Z-P1.Z);
   end;{Dist_PL_3D}

begin
   // convert from screencoordinate to 2D world coordinate
   P2D.X:=(P.X-FPan.X-FScreencenter.X)/(FZoom*FScale);
   P2D.Y:=(P.Y-FPan.Y-FScreencenter.Y)/-(FZoom*FScale);
   // Now correct for perspective projection and create a 3D ray through the screen coordinate

   P3D.X:=FMin3D.X;
   Dist:=FCameralocation.X-P3D.X;
   P3D.Y:=P2D.X*dist/FCameralocation.X;
   P3D.Z:=P2D.Y*Dist/FCameralocation.X;
   P1:=RotatedPointBack(P3D);

   P3D.X:=FMax3D.X;
   Dist:=FCameralocation.X-P3D.X;
   P3D.Y:=P2D.X*dist/FCameralocation.X;
   P3D.Z:=P2D.Y*Dist/FCameralocation.X;
   P2:=RotatedPointBack(P3D);
   // Finally project point Input on the ray through P1 and P2
   Result:=Dist_PL_3D(Input,P1,P2);
end;{TFreeViewport.ProjectBack}

function TFreeViewport.ProjectBackTo2D(P:TPoint):T2DCoordinate;
var P2D     : T2DCoordinate;
    P1,P2   : T3DCoordinate;
    P3D     : T3DCoordinate;
    Dist    : TFloatType;

begin
   // convert from screencoordinate to 2D world coordinate
   P2D.X:=(P.X-FPan.X-FScreencenter.X)/(FZoom*FScale);
   P2D.Y:=(P.Y-FPan.Y-FScreencenter.Y)/-(FZoom*FScale);
   // convert from screencoordinate to 2D world coordinate
   P3D.X:=FMin3D.X;
   Dist:=FCameralocation.X-P3D.X;
   P3D.Y:=P2D.X*dist/FCameralocation.X;
   P3D.Z:=P2D.Y*Dist/FCameralocation.X;
   P1:=RotatedPointBack(P3D);
   P3D.X:=FMax3D.X;
   Dist:=FCameralocation.X-P3D.X;
   P3D.Y:=P2D.X*dist/FCameralocation.X;
   P3D.Z:=P2D.Y*Dist/FCameralocation.X;
   P2:=RotatedPointBack(P3D);
   Case ViewType of
      fvBodyplan   : begin
                        P2D.X:=P2.Y;
                        P2D.Y:=P2.Z;
                     end;
      fvPlan       : begin
                        P2D.X:=P2.X;
                        P2D.Y:=P2.Y;
                     end;
      fvProfile    : begin
                        P2D.X:=P2.X;
                        P2D.Y:=P2.Z;
                     end;
   end;
   Result:=P2D;
end;{TFreeViewport.ProjectBackTo2D}

// Projects a 3D point to the screen and calculate it's Z-value for the Z-buffer
function TFreeViewport.ProjectToZBuffer(P:T3DCoordinate):TShadePoint;
var P3D    : T3DCoordinate;
    P2D    : T2DCoordinate;
    Dist   : TFloatType;
begin
   P3D:=RotatedPoint(P);
   Dist:=FCameralocation.X-P3D.X;
   // apply perspective correction
   P2D.X:=FCameralocation.X*P3D.Y/dist;
   P2D.Y:=FCameralocation.X*P3D.Z/Dist;
   Result.X:=FPan.X+FScreencenter.X+Round(FZoom*FScale*P2D.X);
   Result.Y:=FPan.Y+FScreencenter.Y-Round(FZoom*FScale*P2D.Y);
   Result.Z:=FCameralocation.X*P3D.X/Dist;
end;{TFreeViewport.ProjectToZBuffer}

// Projects a 3D point with a certain z-buffer offset to the screen, used for drawing lines on top of shaded surfaces
function TFreeViewport.ProjectToZBuffer(Scale:TFloatType;P:T3DCoordinate):TShadePoint;
var P3D  : T3DCoordinate;
    P2D  : T2DCoordinate;
    Dist : TFloatType;
begin
   P3D:=RotatedPoint(P);
   Dist:=FCameralocation.X-P3D.X;
   // apply perspective correction
   P2D.X:=FCameralocation.X*P3D.Y/(Dist);
   P2D.Y:=FCameralocation.X*P3D.Z/(Dist);
   Result.X:=FPan.X+FScreencenter.X+Round(FZoom*FScale*P2D.X);
   Result.Y:=FPan.Y+FScreencenter.Y-Round(FZoom*FScale*P2D.Y);
   Result.Z:=FCameralocation.X*Scale*P3D.X/Dist;
end;{TFreeViewport.ProjectToZBuffer}

function TFreeViewport.RotatedPoint(P:T3DCoordinate):T3DCoordinate;
begin
   // This function takes a point from worldspace and rotates it around the midpoint
   // of the scene as specified by the viewing-parameters (angle/elevation)
   // translate midpoint back to origin
   P.X:=P.X-FMidpoint.X;
   P.Y:=P.Y-FMidpoint.Y;
   P.Z:=P.Z-FMidpoint.Z;
   // Rotate around the origin
   Result.x:=(P.x*FCosAngle-P.y*FSinAngle)*FCosElevation+P.z*FSinElevation;
   Result.y:=P.x*FSinAngle+P.y*FCosAngle;
   Result.z:=-(P.x*FCosAngle-P.y*FSinAngle)*FSinElevation+P.z*FCosElevation;
   // Translate origin back to midpoint
   Result.X:=Result.X+FMidPoint.X;
   Result.Y:=Result.Y+FMidPoint.Y;
   Result.Z:=Result.Z+FMidPoint.Z;
end;{TFreeViewport.RotatedPoint}

function TFreeViewport.RotatedPointBack(P:T3DCoordinate):T3DCoordinate;
var CosAngle,SinAngle        : TFloatType;
    CosElevation,SinElevation: TFloatType;
begin
   // This function takes a point from worldspace and rotates it around the midpoint
   // of the scene as specified by the viewing-parameters (angle/elevation)
   // translate midpoint back to origin
   CosAngle:=Cos(DegToRad(-FAngle));
   SinAngle:=sin(DegToRad(-FAngle));
   CosElevation:=Cos(DegToRad(-FElevation));
   SinElevation:=sin(DegToRad(-FElevation));
   P.X:=P.X-FMidpoint.X;
   P.Y:=P.Y-FMidpoint.Y;
   P.Z:=P.Z-FMidpoint.Z;
      // Rotate a point first around Y-axis then around the Z-axis
   Result.x:=(P.x*CosElevation+P.z*SinElevation)*CosAngle-P.y*SinAngle;
   Result.y:=(P.x*CosElevation+P.z*SinElevation)*SinAngle+P.y*CosAngle;
   Result.z:=(-P.x*SinElevation+P.z*CosElevation);
   // Translate origin back to midpoint
   Result.X:=Result.X+FMidPoint.X;
   Result.Y:=Result.Y+FMidPoint.Y;
   Result.Z:=Result.Z+FMidPoint.Z;
end;{TFreeViewport.RotatedPoint}

procedure TFreeViewport.Paint;
var OldCanvas        : TCanvas;
    DrawingToPrinter : Boolean;
    L                : TFloatType;
    N                : T3DCoordinate;
begin
   if (not (csDestroying in ComponentState)) and
      (not (csLoading in ComponentState)) and
      (not (csReading in ComponentState)) and
      (not (csWriting in ComponentState)) and
      (Parent<>nil) then
   begin
      // See if the contents is drawn to the printer
      if ViewportMode<>vmWireframe then
      begin
         L:=7.5*DistPP3D(FMin3D,FMax3D);
         N:=Normalize(FLight.Position);
         N.X:=N.X*L;
         N.Y:=N.Y*L;
         N.Z:=N.Z*L;
         FLight.Position:=N;
      end;
      if Printer=nil then DrawingToPrinter:=False
                     else DrawingToPrinter:=(FPrinting) and (FDrawingCanvas=Printer.Canvas);
      if DrawingToPrinter then
      begin
         FPrinting:=True;
         if BackgroundImage.Bitmap<>nil then BackgroundImage.Draw;
         if Assigned(OnRedraw) then OnRedraw(Self);
      end else
      begin
         OldCanvas:=FDrawingCanvas;
         if not FPrinting then
         begin
            FDestinationWidth:=ClientWidth;
            FDestinationHeight:=ClientHeight;
         end;
         if (FDoubleBuffer) or (ViewportMode<>vmWireframe) then
         begin
            if FDrawingBuffer.Width<>FDestinationWidth
               then FDrawingBuffer.Width:=FDestinationWidth;
            if FDrawingBuffer.Height<>FDestinationHeight
               then FDrawingBuffer.Height:=FDestinationHeight;
            FDrawingCanvas:=FDrawingBuffer.Canvas;
         end else FDrawingCanvas:=OldCanvas;
         // Clear buffer
         FDrawingCanvas.Brush.Color:=Color;
         FDrawingCanvas.Brush.Style:=bsSolid;
         FDrawingCanvas.Rectangle(-1,-1,FDestinationWidth+2,FDestinationHeight+2);

         if BackgroundImage.Bitmap<>nil then BackgroundImage.Draw;

         // Turn clipping back on
         if ViewportMode<>vmWireframe then
         begin
            ZBuffer.Initialize;
            AlphaBuffer.Initialize;
         end;

         if Assigned(OnRedraw) //and (ViewportMode=vmWireframe)
            then OnRedraw(Self);

         if ViewportMode<>vmWireframe
            then AlphaBuffer.Draw;

         if (FDoubleBuffer) or (ViewportMode<>vmWireframe) then
         begin
            // Copy buffer to screen
            //Bitblt(OldCanvas.Handle,0,0,FDestinationWidth,FDestinationHeight,FDrawingBuffer.Canvas.Handle,0,0,SRCCOPY);
            OldCanvas.Draw(0,0,FDrawingBuffer);
            FDrawingCanvas:=OldCanvas;
         end;
      end;
   end;
end;{TFreeViewport.Paint}

procedure TFreeViewport.MoveTo(x,y:integer);
begin
  FDrawingCanvas.MoveTo(x,y);
end;

procedure TFreeViewport.LineTo(x,y:integer);
begin
  FDrawingCanvas.LineTo(x,y);
end;

procedure TFreeViewport.Line(x1,y1, x2,y2:integer);
begin
  FDrawingCanvas.Line(x1,y1, x2,y2);
end;

procedure TFreeViewport.Rectangle(x1,y1, x2,y2:integer);
begin
  FDrawingCanvas.Rectangle(x1,y1, x2,y2);
end;

procedure TFreeViewport.Rectangle(rect:TRect);
begin
  FDrawingCanvas.Rectangle(rect);
end;

procedure TFreeViewport.Ellipse(x1,y1, x2,y2:integer);
begin
  FDrawingCanvas.Ellipse(x1,y1, x2,y2);
end;

procedure TFreeViewport.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer);
begin
  FDrawingCanvas.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY);
end;


procedure TFreeViewport.Polyline(const Points: array of TPoint);
begin
  FDrawingCanvas.Polyline(points);
end;

procedure TFreeViewport.Polygon(const Points: array of TPoint);
begin
  FDrawingCanvas.Polygon(points);
end;

procedure TFreeViewport.SaveAsBitmap(Filename:string;const ShowDialog:boolean=true);
var CanvasWidth   : Integer;
    CanvasHeight  : Integer;
    XScale        : TFloatType;
    YScale        : TFloatType;
    Scale         : TFloatType;
    OldScale      : TFloatType;
    OldPan        : TPoint;
    OldCanvas     : TCanvas;
    OldCenter     : TPoint;
    Bitmap        : TBitmap;
    Dialog        : TSaveImageDialog;
    OK            : Boolean;
begin
   if ShowDialog then
   begin
      Dialog:=TSaveImageDialog.Create(self);
      ShowTranslatedValues(Dialog);
      Dialog.ImageWidth:=ClientWidth;
      Dialog.ImageHeight:=ClientHeight;
      Dialog.Filename:=Filename;
      OK:=Dialog.Execute;
   end else
   begin
      OK:=True;
      Dialog:=nil;
   end;
   if OK then
   begin
      // Backup settings;
      OldScale:=FScale;
      OldPan:=FPan;
      OldCanvas:=FDrawingCanvas;
      OldCenter:=FScreencenter;
      try
         if ShowDialog then
         begin
            CanvasWidth:=Dialog.ImageWidth;
            CanvasHeight:=Dialog.ImageHeight;
         end else
         begin
            CanvasWidth:=ClientWidth;
            CanvasHeight:=ClientHeight;
         end;
         Bitmap:=TBitmap.Create;
         Bitmap.PixelFormat:=pf24bit;
         Bitmap.Width:=CanvasWidth;
         Bitmap.Height:=CanvasHeight;
         FDrawingCanvas:=Bitmap.Canvas;
         XScale:=CanvasWidth/ClientWidth;
         YScale:=CanvasHeight/ClientHeight;
         if XScale<YScale then Scale:=XScale
                          else Scale:=YScale;
         FScale:=FScale*Scale;
         FScreencenter.X:=round(XScale*FScreencenter.X);
         FScreencenter.Y:=round(YScale*FScreencenter.Y);
         FPrinting:=True;
         FDestinationWidth:=CanvasWidth;
         FDestinationHeight:=CanvasHeight;
         refresh;
         Bitmap.SaveToFile(ChangeFileExt(filename,'.bmp'));
         Bitmap.Destroy;
      finally
         // Restore settings;
         FPrinting:=false;
         FDrawingCanvas:=OldCanvas;
         FScale:=OldScale;
         FPan:=OldPan;
         FScreencenter:=OldCenter;
         Refresh;
      end;
   end;
   if ShowDialog then Dialog.Destroy;
end;{TFreeViewport.SaveAsBitmap}

procedure TFreeViewport.Resize;
begin
   Inherited Resize;
   InitializeViewport(FMin3D,FMax3D);
end;{TFreeViewport.Resize}

procedure TFreeViewport.KeyPress(var Key: Char);
begin
   inherited;
   if key in ['a','A'] then
   begin
      ZoomExtents;
   end else if key in ['i','I'] then
   begin
      ZoomIn;
   end else if key in ['o','O'] then
   begin
      ZoomOut;
   end;
end;{TFreeViewport.KeyPress}

procedure TFreeViewport.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var Pt   : TPoint;
    Diff : TPoint;
    str  : Ansistring;
    Tmp  : string;
    I,Ind: Integer;
    XVal : TFloattype;
    YVal : TFloatType;
    OK   : Boolean;
begin
   Inherited;
   FPreviousPosition.X:=X;
   FPreviousPosition.Y:=Y;
   if (BackgroundMode<>emNormal) and (ssRight in Shift) then
   begin
      BackgroundMode:=emNormal;
   end else
   if (BackgroundMode=emSetOrigin) and (ssLeft in Shift) then
   begin
      // Start moving the background image
      // store current origin
      FBackgroundOrigin:=BackgroundImage.Origin;
   end else if (BackgroundMode=emSetScale) and (ssLeft in Shift) then
   begin
      Pt:=Project(ZERO);
      if (Scale*Zoom*BackgroundImage.Scale)<>0 then
      begin
         Diff.X:=round((Pt.X-X)/(Scale*Zoom*BackgroundImage.Scale));
         Diff.Y:=round((Pt.Y-Y)/(Scale*Zoom*BackgroundImage.Scale));
      end else
      begin
         Diff.X:=Pt.X-X;
         Diff.Y:=Pt.Y-Y;
      end;
      Str:=Truncate(-BackgroundImage.Scale*Diff.X,4)+', '+Truncate(BackgroundImage.Scale*Diff.Y,4);
      if InputQuery(Userstring(91),Userstring(196)+':',str) then
      begin
         Ind:=Pos(',',Str);
         if Ind<>0 then
         begin
            OK:=True;
            Tmp:=trim(Copy(Str,1,Ind-1));
            if Tmp<>'' then
            begin
               Val(Tmp,XVal,I);
               if I<>0 then OK:=False;
            end else XVal:=0;
            Tmp:=trim(Copy(Str,Ind+1,Length(Str)-Ind));
            if Tmp<>'' then
            begin
               Val(Tmp,YVal,I);
               if I<>0 then OK:=False;
            end else YVal:=0;
            if OK then
            begin
               if abs(Diff.X)>abs(Diff.Y) then BackgroundImage.FScale:=XVal/-Diff.X
                                          else BackgroundImage.FScale:=YVal/Diff.Y;
               Refresh;
               BackgroundMode:=emNormal;
               if assigned(FOnChangeBackgroundImage) then FOnChangeBackgroundImage(Owner);
            end else MessageDlg(Userstring(197)+'!',mtError,[mbOk],0);
         end else MessageDlg(Userstring(197)+'!',mtError,[mbOk],0);
      end;
   end else if (BackgroundMode=emSetTransparentColor) and (ssLeft in Shift) then
   begin
      Pt:=Project(ZERO);
      if (Scale*Zoom*BackgroundImage.Scale)<>0 then
      begin
         Diff.X:=round((Pt.X-X)/(Scale*Zoom*BackgroundImage.Scale));
         Diff.Y:=round((Pt.Y-Y)/(Scale*Zoom*BackgroundImage.Scale));
      end else
      begin
         Diff.X:=Pt.X-X;
         Diff.Y:=Pt.Y-Y;
      end;
      Pt.X:=BackgroundImage.FOrigin.X-Diff.X;
      Pt.Y:=BackgroundImage.FOrigin.Y-Diff.Y;
      if (Pt.X>=0) and (Pt.X<BackgroundImage.FBitmap.Width) and
         (Pt.Y>=0) and (Pt.Y<=BackgroundImage.FBitmap.Height) then
      begin
         BackgroundImage.Transparent:=True;
         BackGroundImage.TransparentColor:=BackgroundImage.FBitmap.Canvas.Pixels[Pt.X,Pt.Y];
         Refresh;
         BackgroundMode:=emNormal;
         if assigned(FOnChangeBackgroundImage) then FOnChangeBackgroundImage(Owner);
      end else
      begin
         BackgroundImage.Transparent:=False;
         Refresh;
         BackgroundMode:=emNormal;
         if assigned(FOnChangeBackgroundImage) then FOnChangeBackgroundImage(Owner);
      end;
   end else
   begin
      if Assigned(FOnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);
      if not focused then setfocus;
      if (ssMiddle in shift) and (Viewtype=fvPerspective) then
      begin
         Cursor:=crRotate;
      end
   end;
end;{TFreeViewport.MouseDown}

procedure TFreeViewport.MouseMove(Shift:TShiftState;X,Y:Integer);
var Desired : TCursor;
    Pt,Prev : TPoint;
    Scale   : TFloatType;
begin
   Inherited;
   if (Shift=[ssLeft]) and (Backgroundmode=emSetOrigin) then
   begin
      // Background image moving procedure
      Scale:=1.0;
      if scale<>0 then
      begin
         Prev:=BackgroundImage.ImageCoordinate(FPreviousPosition.X,FPreviousPosition.Y);
         Pt:=BackgroundImage.ImageCoordinate(X,Y);
         Pt.X:=-round((Pt.X-Prev.X)/Scale);
         Pt.Y:=-round((Pt.Y-Prev.Y)/Scale);
         if (Pt.X<>0) or (Pt.Y<>0) then
         begin
            BackgroundImage.FOrigin.X:=BackgroundImage.FOrigin.X+Pt.X;
            BackgroundImage.FOrigin.Y:=BackgroundImage.FOrigin.Y+Pt.Y;
            if Pt.X<>0 then FPreviousPosition.X:=X;
            if Pt.Y<>0 then FPreviousPosition.Y:=Y;
            Refresh;
         end;
         exit;
      end;
   end else if (shift=[]) and (Backgroundmode<>emNormal) then
   begin
      Case Backgroundmode of
         emSetOrigin          : Desired:=crsetOrigin;
         emSetScale           : Desired:=crSetScale;
         emSetTransparentColor: Desired:=crTranspCol;
         else Desired:=cursor;
      end;
      if Cursor<>Desired then Cursor:=Desired;
   end else if (ssmiddle in shift) and (Viewtype=fvPerspective) then
   begin
      // rotation using middle mousebutton
      FAngle:=Self.FAngle+(X-FPreviousPosition.X)/4;
      while FAngle>180 do FAngle:=FAngle-360;
      while FAngle<-180 do FAngle:=FAngle+360;
      FCosAngle:=Cos(DegToRad(FAngle));
      FSinAngle:=sin(DegToRad(FAngle));

      FElevation:=FElevation+(Y-FPreviousPosition.Y)/4;
      while FElevation>180 do FElevation:=FElevation-360;
      while FElevation<-180 do FElevation:=FElevation+360;
      FCosElevation:=Cos(DegToRad(FElevation));
      FSinElevation:=sin(DegToRad(FElevation));
      if FHorScrollbar<>nil then
         if FHorScrollbar.Position<>round(angle) then FHorScrollbar.Position:=Round(Angle);
      if FVertScrollbar<>nil then
         if FVertScrollbar.Position<>round(Elevation) then FVertScrollbar.Position:=Round(Elevation);
      InitializeViewport(FMin3D,FMax3D);
   end else if ssRight in shift then Cursor:=crPan;
   FPreviousPosition.X:=X;
   FPreviousPosition.Y:=Y;
   if Assigned(FOnMouseMove) then OnMouseMove(Self,Shift,X,Y);
end;{TFreeViewport.MouseMove}

procedure TFreeViewport.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
   Inherited;
   if BackgroundMode=emSetOrigin then
   begin
      // Finished moving the background image
      // Check if origin has changed
      if (FBackgroundOrigin.X<>BackgroundImage.Origin.X) or
         (FBackgroundOrigin.Y<>BackgroundImage.Origin.Y) then
      begin
         BackgroundMode:=emNormal;
         if assigned(FOnChangeBackgroundImage) then FOnChangeBackgroundImage(Owner);
      end;
   end else
   begin
      if Assigned(FOnMouseUp) then OnMouseUp(Self,Button,Shift,X,Y);
      if Cursor<>crCross then Cursor:=crCross;
   end;
   if not focused then setfocus;
end;{TFreeViewport.MouseUp}

function TFreeViewport.DoMouseWheel(Shift:TShiftState;WheelDelta:Integer;MousePos: TPoint): Boolean;
var Factor:single;
    NewPan:TPoint;
    Mid:TPoint;
begin
   Result:=Inherited DoMousewheel(Shift,Wheeldelta,Mousepos);
   {$IFDEF WIN32} // We need to bring ScreenToClient in MDF mode, not a case for X11
   MousePos:=self.ScreenToClient(MousePos);
   {$ENDIF}
   if (MousePos.X>=0) and (MousePos.Y>=0) and (MousePos.X<Clientwidth) and (MousePos.Y<Clientheight) then
   begin
      // zoom by using mousewheel
      Factor:=1.1;
      Mid.X:=ClientWidth div 2;
      Mid.Y:=ClientHeight div 2;

      NewPan.X:=MousePos.X-Mid.X;
      NewPan.Y:=MousePos.Y-Mid.Y;

      if WheelDelta>0 then
      begin
         FZoom:=FZoom*Factor;
         FPan.X:=round(Factor*FPan.X);
         FPan.Y:=round(Factor*FPan.Y);
         Refresh;
      end else
      begin
         FZoom:=FZoom/Factor;
         FPan.X:=round(FPan.X/Factor);
         FPan.Y:=round(FPan.Y/Factor);
         Refresh;
      end;
   end;
end;{TFreeViewport.DoMouseWheel}

procedure TFreeViewport.SetPenWidth(Width:integer);
begin
   if FDrawingCanvas.Pen.Width<>Width then FDrawingCanvas.Pen.Width:=Width;
end;{TFreeViewport.SetPenWidth}

Procedure TFreeViewport.ShadedColor(Dp:single;R,G,B:byte;var ROut,GOut,BOut:byte);
const Ambient=0.20;
var C,Tmp:Single;
begin
   if Dp<0 then Dp:=-Dp else if Dp>1 then Dp:=1;
   if Dp>=0.80 then
   begin
      Tmp:=5-5*Dp;
      if Tmp<0 then C:=0
               else C:=Sqrt(Tmp);
      ROut:=Round(255-(255-Dp*R)*C);
      GOut:=Round(255-(255-Dp*G)*C);
      BOut:=Round(255-(255-Dp*B)*C);
   end else
   begin
      Dp:=Dp-Ambient;
      if Dp<0 then Dp:=0;
      C:=Dp/(0.8-Ambient);
      C:=Ambient+(0.8-Ambient)*C*C;
      ROut:=Round(C*R);
      GOut:=Round(C*G);
      BOut:=Round(C*B);
   end;
end;{TFreeViewport.ShadedColor}

procedure TFreeViewport.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R,G,B:byte;Alpha:Byte);
var Normal           : T3DCoordinate;
    Pt1,Pt2,Pt3,T    : TShadePoint;
    LSourceDirection : T3DCoordinate;
    Center           : T3DCoordinate;
    LIntensityRatio  : TFloatType;
    Y                : Integer;
    XLeft,dXLeft     : Integer;
    XRight,dXRight   : Integer;
    ZLeft,dZLeft     : TFloatType;
    ZRight,dZRight   : TFloatType;

    {$ifdef Windows32}
       procedure Swap(var A, B: Integer);assembler;
       asm
          MOV     ECX,[EAX]
          XCHG    ECX,[EDX]
          MOV     [EAX],ECX
       end;{Swap}
    {$else}
       procedure Swap(var A, B: Integer);
       var X:Integer;
       begin
          X:=A;
          A:=B;
          B:=X;
       end;{Swap}
    {$endif}


   procedure ShadeLine(X1,X2,Y: integer;Z1,Z2: TFloatType);
   var IncZ,T  : TFloatType;
       //Row     : pRGBTripleArray;
       pRow, pPixel : pointer;
       Pixel : TRGBTriple;
   begin
      if (Y<0) or (Y>FDestinationHeight-1) then exit;
      X1:=X1 div 256;
      X2:=X2 div 256;
      if X1>X2 then
      begin
         Swap(X1,X2);
         T:=Z1;
         Z1:=Z2;
         Z2:=T;
      end;
      if X1<>X2 then IncZ:=(Z2-Z1)/(X2-X1)
                else IncZ:=0;
      if (X1>FDestinationWidth-1) or (X2<0) then exit;
      if (X1<0) then
      begin
         Z1:=Z1+abs(X1)*IncZ;
         X1:=0;
      end;
      if X2>FDestinationWidth-1 then X2:=FDestinationWidth-1;
      if X1>=0 then
      begin
         // Use scanline property for faster pixel access
         //Row:=FDrawingBuffer.Scanline[y];
         pRow := FDrawingBuffer.RawImage.GetLineStart(Y);
         while X1<=X2 do
         begin
            if Z1>FZBuffer.FBuffer[Y][X1] then
            begin
               if Alpha=255 then
               begin
                 {
                 pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * X1;
                 Pixel := FBitmapFormatHelper.ToTRGBTriple(pPixel);
                 Pixel.rgbtRed:=R;
                 Pixel.rgbtGreen:=G;
                 Pixel.rgbtBlue:=B;
                 FBitmapFormatHelper.FromTRGBTriple(Pixel,pPixel);
                 }

                 // temporary draw via canvas
                 ///FDrawingBuffer.Canvas.Pixels[X1,Y]:=RGBtoColor(R,G,B);  // this causes mem leak in LibQt4 widget DCSetPixel (qtobject.inc) calling QPen_create5

                 SetPixel(X1,Y, R,G,B);

                 FZBuffer.FBuffer[Y][X1]:=Z1;
               end else AlphaBuffer.AddPixelData(X1,Y,R,G,B,Alpha,Z1);
            end;
            Z1:=Z1+IncZ;
            Inc(X1);
         end;
      end;
   end;{ShadeLine}

begin
   // Calculate data for the points
   Pt1:=ProjectToZBuffer(P_1);
   Pt2:=ProjectToZBuffer(P_2);
   Pt3:=ProjectToZBuffer(P_3);
   P_1:=RotatedPoint(P_1);
   P_2:=RotatedPoint(P_2);
   P_3:=RotatedPoint(P_3);
   // Calculate triangle normal and center
   Normal:=UnifiedNormal(P_1,P_2,P_3);
   Center.X:=(P_1.X+P_2.X+P_3.X)/3;
   Center.Y:=(P_1.Y+P_2.Y+P_3.Y)/3;
   Center.Z:=(P_1.Z+P_2.Z+P_3.Z)/3;
   // Calculate light vector
   LSourceDirection.X:=Center.X-FLight.Position.X;
   LSourceDirection.Y:=Center.Y-FLight.Position.Y;
   LSourceDirection.Z:=Center.Z-FLight.Position.Z;
   LSourceDirection:=Normalize(LSourceDirection);
   LIntensityRatio:=Dotproduct(Normal,LSourceDirection);
   ShadedColor(LIntensityRatio,R,G,B,R,G,B);

   // Sort points according to Y-value
   if Pt2.Y<Pt1.Y then
   begin
      T:=Pt1;
      Pt1:=Pt2;
      Pt2:=T;
   end;
   if Pt3.Y<Pt2.Y then
   begin
      T:=Pt2;
      Pt2:=Pt3;
      Pt3:=T;
      if Pt2.Y<Pt1.Y then
      begin
         T:=Pt1;
         Pt1:=Pt2;
         Pt2:=T;
      end;
   end;
   // check if min/max values are outside window
   if ((Pt1.Y>=FDestinationHeight) or (Pt3.Y<=0)) or
      ((Pt1.X<0) and (Pt2.X<0) and (Pt3.X<0)) or
      ((Pt1.X>FDestinationWidth) and (Pt2.X>FDestinationWidth) and (Pt3.X>FDestinationWidth)) then exit;
   Pt1.X:=Pt1.X shl 8;
   Pt2.X:=Pt2.X shl 8;
   Pt3.X:=Pt3.X shl 8;

   if Pt3.Y=Pt1.Y then
   begin
      if Pt2.X>=Pt1.X then ShadeLine(Pt1.X,Pt2.X,Pt1.Y,Pt1.Z,Pt2.Z)
                      else ShadeLine(Pt2.X,Pt1.X,Pt1.Y,Pt2.Z,Pt1.Z);
      if Pt3.X>=Pt2.X then ShadeLine(Pt2.X,Pt3.X,Pt1.Y,Pt2.Z,Pt3.Z)
                      else ShadeLine(Pt3.X,Pt2.X,Pt3.Y,Pt3.Z,Pt2.Z);
      Exit;
   end;
   XLeft:=Pt1.X;
   ZLeft:=Pt1.Z;
   if Pt3.Y<>Pt1.Y then
   begin
      dZLeft:=(Pt3.Z-Pt1.Z)/(Pt3.Y-Pt1.Y);
      dXLeft:=(Pt3.X-Pt1.X) div (Pt3.Y-Pt1.Y);
   end else
   begin
      dZLeft:=0;
      dXLeft:=0;
   end;
   XRight:=Pt1.X;
   ZRight:=Pt1.Z;
   if Pt2.Y<>Pt1.Y then
   begin
      dZRight:=(Pt2.Z-Pt1.Z)/(Pt2.Y-Pt1.Y);
      dXRight:=(Pt2.X-Pt1.X) div (Pt2.Y-Pt1.Y);
   end else
   begin
      dZRight:=0;
      dXRight:=0;
   end;
   Y:=Pt1.Y;
   while Y<Pt2.Y do
   begin
      if XRight>=XLeft then ShadeLine(XLeft,XRight,Y,ZLeft,ZRight)
                       else ShadeLine(XRight,XLeft,Y,ZRight,ZLeft);
      if Y<Pt2.Y then
      begin
         Inc(XLeft,dXLeft);
         ZLeft:=ZLeft+dZLeft;
         Inc(XRight,dXRight);
         ZRight:=ZRight+dZRight;
      end;
      Inc(Y);
   end;
   if Pt2.Y=Pt3.Y then Exit;
   XRight:=Pt2.X;
   ZRight:=Pt2.Z;
   if Pt3.Y<>Pt2.Y then
   begin
      dZRight:=(Pt3.Z-Pt2.Z)/(Pt3.Y-Pt2.Y);
      dXRight:=(Pt3.X-Pt2.X) div (Pt3.Y-Pt2.Y);
   end;
   for Y:=Pt2.Y to Pt3.Y do
   begin
      if XRight>=XLeft then ShadeLine(XLeft,XRight,Y,ZLeft,ZRight)
                       else ShadeLine(XRight,XLeft,Y,ZRight,ZLeft);
      Inc(XLeft,dXLeft);
      ZLeft:=ZLeft+dZLeft;
      Inc(XRight,dXRight);
      ZRight:=ZRight+dZRight;
   end;
end;{TFreeViewport.ShadeTriangle}

// Draw a smooth shaded triangle to the ZBuffer (used when all 3 corners of the triangle have
// different color, as for example when shading GAUSS curvature
procedure TFreeViewport.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R1,G1,B1,R2,G2,B2,R3,G3,B3:byte);
var Left,Right,DLeft,dRight:TShadePoint;
    Y,d             : integer;
    Tmp : TShadePoint;
    V1,V2,V3:TShadePoint;
    Normal,Center:T3DCoordinate;
    LSourceDirection:T3DCoordinate;
    LIntensityRatio:TFloatType;

    procedure SetColor(var P:TShadePoint;R,G,B:byte);
    // Set the color of each vertex based on it's curvature and triangle normal
    begin
      ShadedColor(LIntensityRatio,R,G,B,R,G,B);
      P.R:=R shl 8;
      P.G:=G shl 8;
      P.B:=B shl 8;
    end;{SetColor}

   procedure ShadeLine(Left,Right:TShadePoint;Y:Integer);
   var Delta,Tmp  : TShadePoint;
       d          : Integer;
       //Row        : pRGBTripleArray;
       pROw, pPixel : pointer;
       Pixel : TRGBTriple;
   begin
//      if (Y<0) or (Y>H-1) then exit;
      Left.X:=Left.X div 256;
      Right.X:=Right.X div 256;
      if Left.X>Right.X then
      begin
         Tmp:=Left;
         Left:=Right;
         Right:=Tmp;
      end;
      if (Left.X>ClientWidth-1) or (Right.X<0) or (Y<0) or (Y>ClientHeight-1) then exit;

      d:=Right.X-Left.X;
      if d<>0 then
      begin
         Delta.Z:=(Right.Z-Left.Z)/d;
         Delta.R:=(Right.R-Left.R) div d;
         Delta.G:=(Right.G-Left.G) div d;
         Delta.B:=(Right.B-Left.B) div d;
      end else
      begin
         Delta.Z:=0;
         Delta.R:=0;
         Delta.G:=0;
         Delta.B:=0;
      end;

//      if (Left.X>W) or (Right.X<0) then exit;
      if (Left.X<0) then
      begin
         Left.Z:=Left.Z+-Left.X*Delta.Z;
         Inc(Left.R,-Left.X*Delta.R);
         Inc(Left.G,-Left.X*Delta.G);
         Inc(Left.B,-Left.X*Delta.B);
         Left.X:=0;
      end;

      if Right.X>ClientWidth-1 then Right.X:=ClientWidth-1;
      if Left.X>=0 then
      begin
         //Row:=FDrawingBuffer.Scanline[y];
         //pRow := FDrawingBuffer.RawImage.GetLineStart(Y); // this is slow!

         while Left.X<=Right.X do
         begin
            if Left.Z>FZBuffer.FBuffer[Y][Left.X] then
            begin
               FZBuffer.FBuffer[Y][Left.X]:=Left.Z;

               {pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * Left.X;
               Pixel.rgbtRed:=Left.R shr 8;
               Pixel.rgbtGreen:=Left.G shr 8;
               Pixel.rgbtBlue:=Left.B shr 8;
               FBitmapFormatHelper.FromTRGBTriple(Pixel,pPixel);}

               // temporary draw via canvas
               //FDrawingBuffer.Canvas.Pixels[Left.X,Y]:=RGBtoColor(Left.R shr 8, Left.G shr 8, Left.B shr 8);
               SetPixel(Left.X,Y, Left.R shr 8, Left.G shr 8, Left.B shr 8);
            end;
            Left.Z:=Left.Z+Delta.Z;
            Inc(Left.R,Delta.R);
            Inc(Left.G,Delta.G);
            Inc(Left.B,Delta.B);
            Inc(Left.X);
         end;
      end;
   end;{ShadeLine}

begin
   // Calculate data for the points
   V1:=ProjectToZBuffer(P_1);
   V2:=ProjectToZBuffer(P_2);
   V3:=ProjectToZBuffer(P_3);
   P_1:=RotatedPoint(P_1);
   P_2:=RotatedPoint(P_2);
   P_3:=RotatedPoint(P_3);
   // Calculate triangle normal and center
   Normal:=UnifiedNormal(P_1,P_2,P_3);
   Center.X:=(P_1.X+P_2.X+P_3.X)/3;
   Center.Y:=(P_1.Y+P_2.Y+P_3.Y)/3;
   Center.Z:=(P_1.Z+P_2.Z+P_3.Z)/3;
   // Calculate light vector
   LSourceDirection.X:=Center.X-FLight.Position.X;
   LSourceDirection.Y:=Center.Y-FLight.Position.Y;
   LSourceDirection.Z:=Center.Z-FLight.Position.Z;
   LSourceDirection:=Normalize(LSourceDirection);
   LIntensityRatio:=Dotproduct(Normal,LSourceDirection);
   SetColor(V1,R1,G1,B1);
   SetColor(V2,R2,G2,B2);
   SetColor(V3,R3,G3,B3);

   if V2.Y<V1.Y then
   begin
      Tmp:=V1;
      V1:=V2;
      V2:=Tmp;
   end;
   if V3.Y<V2.Y then
   begin
      Tmp:=V2;
      V2:=V3;
      V3:=Tmp;
      if V2.Y<V1.Y then
      begin
         Tmp:=V1;
         V1:=V2;
         V2:=Tmp;
      end;
   end;
   V1.X:=V1.X shl 8;
   V2.X:=V2.X shl 8;
   V3.X:=V3.X shl 8;
   if V3.Y=V1.Y then
   begin
      if V2.X>=V1.X then ShadeLine(V1,V2,V1.Y)
                    else ShadeLine(V2,V1,V1.Y);
      if V3.X>=V2.X then ShadeLine(V2,V3,V1.Y)
                    else ShadeLine(V3,V2,V1.Y);
      Exit;
   end;

   Left:=V1;
   d:=(V3.Y-V1.Y);
   if d<>0 then
   begin
      dLeft.Z:=(V3.Z-V1.Z)/d;
      dLeft.X:=(V3.X-V1.X) div d;
      dLeft.R:=(V3.R-V1.R) div d;
      dLeft.G:=(V3.G-V1.G) div d;
      dLeft.B:=(V3.B-V1.B) div d;
   end;

   Right:=V1;
   d:=(V2.Y-V1.Y);
   if d<>0 then
   begin
      dRight.Z:=(V2.Z-V1.Z)/d;
      dRight.X:=(V2.X-V1.X) div d;
      dRight.R:=(V2.R-V1.R) div d;
      dRight.G:=(V2.G-V1.G) div d;
      dRight.B:=(V2.B-V1.B) div d;
   end;

   Y:=V1.Y;
   if (Y<0) and (V2.Y>0) then
   begin
      Inc(Left.X,-Y*dLeft.X);
      Left.Z:=Left.Z+-Y*dLeft.Z;
      Inc(Left.R,-Y*dLeft.R);
      Inc(Left.G,-Y*dLeft.G);
      Inc(Left.B,-Y*dLeft.B);

      Inc(Right.X,-Y*dRight.X);
      Right.Z:=Right.Z+-Y*dRight.Z;
      Inc(Right.R,-Y*dRight.R);
      Inc(Right.G,-Y*dRight.G);
      Inc(Right.B,-Y*dRight.B);
      Y:=0;
   end;

   while Y<V2.Y do
   begin
      if Right.X>=Left.X
         then ShadeLine(Left,Right,Y)
         else ShadeLine(Right,Left,Y);
      if Y<V2.Y then
      begin
         Inc(Left.X,dLeft.X);
         Left.Z:=Left.Z+dLeft.Z;
         Inc(Left.R,dLeft.R);
         Inc(Left.G,dLeft.G);
         Inc(Left.B,dLeft.B);

         Inc(Right.X,dRight.X);
         Right.Z:=Right.Z+dRight.Z;
         Inc(Right.R,dRight.R);
         Inc(Right.G,dRight.G);
         Inc(Right.B,dRight.B);
      end;
      Inc(Y);
   end;
   if V2.Y=V3.Y then Exit;

   Right:=V2;
   d:=V3.Y-V2.Y;
   if d<>0 then
   begin
      dRight.Z:=(V3.Z-V2.Z)/d;
      dRight.X:=(V3.X-V2.X) div d;
      dRight.R:=(V3.R-V2.R) div d;
      dRight.G:=(V3.G-V2.G) div d;
      dRight.B:=(V3.B-V2.B) div d;
   end
   else Fillchar(dRight,SizeOf(dRight),0);

   Y:=V2.Y;
   if (Y<0) and (V3.Y>0) then
   begin
      Inc(Left.X,-Y*dLeft.X);
      Left.Z:=Left.Z+-Y*dLeft.Z;
      Inc(Left.R,-Y*dLeft.R);
      Inc(Left.G,-Y*dLeft.G);
      Inc(Left.B,-Y*dLeft.B);

      Inc(Right.X,-Y*dRight.X);
      Right.Z:=Right.Z+-Y*dRight.Z;
      Inc(Right.R,-Y*dRight.R);
      Inc(Right.G,-Y*dRight.G);
      Inc(Right.B,-Y*dRight.B);
      Y:=0;
   end;
   while Y<=V3.Y do
   begin
      if Right.X>=Left.X
         then ShadeLine(Left,Right,Y)
         else ShadeLine(Right,Left,Y);
      if Y<=V3.Y then
      begin
         Inc(Left.X,dLeft.X);
         Left.Z:=Left.Z+dLeft.Z;
         Inc(Left.R,dLeft.R);
         Inc(Left.G,dLeft.G);
         Inc(Left.B,dLeft.B);

         Inc(Right.X,dRight.X);
         Right.Z:=Right.Z+dRight.Z;
         Inc(Right.R,dRight.R);
         Inc(Right.G,dRight.G);
         Inc(Right.B,dRight.B);
         Inc(Y);
      end;
   end;
end;{TFreeViewport.ShadeTriangle}

// Draw a smooth shaded triangle to the ZBuffer (used when all 3 corners of the triangle have
// different color, as for example when shading GAUSS curvature
procedure TFreeViewport.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;C1,C2,C3:Extended);
var Left,Right,DLeft,dRight:TShadePoint;
    Y,d             : integer;
    Tmp : TShadePoint;
    V1,V2,V3:TShadePoint;
    Normal,Center:T3DCoordinate;
    LSourceDirection:T3DCoordinate;
    LIntensityRatio:TFloatType;

    procedure SetColor(var P:TShadePoint;Curvature:extended);
    // Set the color of each vertex based on it's curvature and triangle normal
    var R,G,B:byte;
    begin
      if abs(Curvature)<1e-4 then ShadedColor(LIntensityRatio,0,255,0,R,G,B)
                             else ShadedColor(LIntensityRatio,255,0,0,R,G,B);
      P.R:=R shl 8;
      P.G:=G shl 8;
      P.B:=B shl 8;
    end;{SetColor}

   procedure ShadeLine(Left,Right:TShadePoint;Y:Integer);
   var Delta,Tmp  : TShadePoint;
       d          : Integer;
       //Row        : pRGBTripleArray;
       pROw, pPixel : pointer;
       Pixel : TRGBTriple;
   begin
//      if (Y<0) or (Y>H-1) then exit;
      Left.X:=Left.X div 256;
      Right.X:=Right.X div 256;
      if Left.X>Right.X then
      begin
         Tmp:=Left;
         Left:=Right;
         Right:=Tmp;
      end;
      if (Left.X>ClientWidth-1) or (Right.X<0) or (Y<0) or (Y>ClientHeight-1) then exit;

      d:=Right.X-Left.X;
      if d<>0 then
      begin
         Delta.Z:=(Right.Z-Left.Z)/d;
         Delta.R:=(Right.R-Left.R) div d;
         Delta.G:=(Right.G-Left.G) div d;
         Delta.B:=(Right.B-Left.B) div d;
      end else
      begin
         Delta.Z:=0;
         Delta.R:=0;
         Delta.G:=0;
         Delta.B:=0;
      end;

//      if (Left.X>W) or (Right.X<0) then exit;
      if (Left.X<0) then
      begin
         Left.Z:=Left.Z+-Left.X*Delta.Z;
         Inc(Left.R,-Left.X*Delta.R);
         Inc(Left.G,-Left.X*Delta.G);
         Inc(Left.B,-Left.X*Delta.B);
         Left.X:=0;
      end;

      if Right.X>ClientWidth-1 then Right.X:=ClientWidth-1;
      if Left.X>=0 then
      begin
         //Row:=FDrawingBuffer.Scanline[y];
         ///pRow := FDrawingBuffer.RawImage.GetLineStart(Y);

         while Left.X<=Right.X do
         begin
            if Left.Z>FZBuffer.FBuffer[Y][Left.X] then
            begin
               FZBuffer.FBuffer[Y][Left.X]:=Left.Z;
               {pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * Left.X;
               Pixel.rgbtRed:=Left.R shr 8;
               Pixel.rgbtGreen:=Left.G shr 8;
               Pixel.rgbtBlue:=Left.B shr 8;
               FBitmapFormatHelper.FromTRGBTriple(Pixel,pPixel); }

               // temporary draw via canvas
               //FDrawingBuffer.Canvas.Pixels[Left.X,Y]:=RGBtoColor(Left.R shr 8, Left.G shr 8, Left.B shr 8);

               SetPixel(Left.X,Y, Left.R shr 8, Left.G shr 8, Left.B shr 8);
            end;
            Left.Z:=Left.Z+Delta.Z;
            Inc(Left.R,Delta.R);
            Inc(Left.G,Delta.G);
            Inc(Left.B,Delta.B);
            Inc(Left.X);
         end;
      end;
   end;{ShadeLine}

begin
   // Calculate data for the points
   V1:=ProjectToZBuffer(P_1);
   V2:=ProjectToZBuffer(P_2);
   V3:=ProjectToZBuffer(P_3);
   P_1:=RotatedPoint(P_1);
   P_2:=RotatedPoint(P_2);
   P_3:=RotatedPoint(P_3);
   // Calculate triangle normal and center
   Normal:=UnifiedNormal(P_1,P_2,P_3);
   Center.X:=(P_1.X+P_2.X+P_3.X)/3;
   Center.Y:=(P_1.Y+P_2.Y+P_3.Y)/3;
   Center.Z:=(P_1.Z+P_2.Z+P_3.Z)/3;
   // Calculate light vector
   LSourceDirection.X:=Center.X-FLight.Position.X;
   LSourceDirection.Y:=Center.Y-FLight.Position.Y;
   LSourceDirection.Z:=Center.Z-FLight.Position.Z;
   LSourceDirection:=Normalize(LSourceDirection);
   LIntensityRatio:=Dotproduct(Normal,LSourceDirection);
   SetColor(V1,C1);
   SetColor(V2,C2);
   SetColor(V3,C3);

   if V2.Y<V1.Y then
   begin
      Tmp:=V1;
      V1:=V2;
      V2:=Tmp;
   end;
   if V3.Y<V2.Y then
   begin
      Tmp:=V2;
      V2:=V3;
      V3:=Tmp;
      if V2.Y<V1.Y then
      begin
         Tmp:=V1;
         V1:=V2;
         V2:=Tmp;
      end;
   end;
   V1.X:=V1.X shl 8;
   V2.X:=V2.X shl 8;
   V3.X:=V3.X shl 8;
   if V3.Y=V1.Y then
   begin
      if V2.X>=V1.X then ShadeLine(V1,V2,V1.Y)
                    else ShadeLine(V2,V1,V1.Y);
      if V3.X>=V2.X then ShadeLine(V2,V3,V1.Y)
                    else ShadeLine(V3,V2,V1.Y);
      Exit;
   end;

   Left:=V1;
   d:=(V3.Y-V1.Y);
   if d<>0 then
   begin
      dLeft.Z:=(V3.Z-V1.Z)/d;
      dLeft.X:=(V3.X-V1.X) div d;
      dLeft.R:=(V3.R-V1.R) div d;
      dLeft.G:=(V3.G-V1.G) div d;
      dLeft.B:=(V3.B-V1.B) div d;
   end;

   Right:=V1;
   d:=(V2.Y-V1.Y);
   if d<>0 then
   begin
      dRight.Z:=(V2.Z-V1.Z)/d;
      dRight.X:=(V2.X-V1.X) div d;
      dRight.R:=(V2.R-V1.R) div d;
      dRight.G:=(V2.G-V1.G) div d;
      dRight.B:=(V2.B-V1.B) div d;
   end;

   Y:=V1.Y;
   if (Y<0) and (V2.Y>0) then
   begin
      Inc(Left.X,-Y*dLeft.X);
      Left.Z:=Left.Z+-Y*dLeft.Z;
      Inc(Left.R,-Y*dLeft.R);
      Inc(Left.G,-Y*dLeft.G);
      Inc(Left.B,-Y*dLeft.B);

      Inc(Right.X,-Y*dRight.X);
      Right.Z:=Right.Z+-Y*dRight.Z;
      Inc(Right.R,-Y*dRight.R);
      Inc(Right.G,-Y*dRight.G);
      Inc(Right.B,-Y*dRight.B);
      Y:=0;
   end;

   while Y<V2.Y do
   begin
      if Right.X>=Left.X then ShadeLine(Left,Right,Y)
                         else ShadeLine(Right,Left,Y);
      if Y<V2.Y then
      begin
         Inc(Left.X,dLeft.X);
         Left.Z:=Left.Z+dLeft.Z;
         Inc(Left.R,dLeft.R);
         Inc(Left.G,dLeft.G);
         Inc(Left.B,dLeft.B);

         Inc(Right.X,dRight.X);
         Right.Z:=Right.Z+dRight.Z;
         Inc(Right.R,dRight.R);
         Inc(Right.G,dRight.G);
         Inc(Right.B,dRight.B);
      end;
      Inc(Y);
   end;
   if V2.Y=V3.Y then Exit;

   Right:=V2;
   d:=V3.Y-V2.Y;
   if d<>0 then
   begin
      dRight.Z:=(V3.Z-V2.Z)/d;
      dRight.X:=(V3.X-V2.X) div d;
      dRight.R:=(V3.R-V2.R) div d;
      dRight.G:=(V3.G-V2.G) div d;
      dRight.B:=(V3.B-V2.B) div d;
   end else Fillchar(dRight,SizeOf(dRight),0);

   Y:=V2.Y;
   if (Y<0) and (V3.Y>0) then
   begin
      Inc(Left.X,-Y*dLeft.X);
      Left.Z:=Left.Z+-Y*dLeft.Z;
      Inc(Left.R,-Y*dLeft.R);
      Inc(Left.G,-Y*dLeft.G);
      Inc(Left.B,-Y*dLeft.B);

      Inc(Right.X,-Y*dRight.X);
      Right.Z:=Right.Z+-Y*dRight.Z;
      Inc(Right.R,-Y*dRight.R);
      Inc(Right.G,-Y*dRight.G);
      Inc(Right.B,-Y*dRight.B);
      Y:=0;
   end;
   while Y<=V3.Y do
   begin
      if Right.X>=Left.X then ShadeLine(Left,Right,Y)
                         else ShadeLine(Right,Left,Y);
      if Y<=V3.Y then
      begin
         Inc(Left.X,dLeft.X);
         Left.Z:=Left.Z+dLeft.Z;
         Inc(Left.R,dLeft.R);
         Inc(Left.G,dLeft.G);
         Inc(Left.B,dLeft.B);

         Inc(Right.X,dRight.X);
         Right.Z:=Right.Z+dRight.Z;
         Inc(Right.R,dRight.R);
         Inc(Right.G,dRight.G);
         Inc(Right.B,dRight.B);
         Inc(Y);
      end;
   end;
end;{TFreeViewport.ShadeTriangle}

procedure TFreeViewport.ZoomExtents;
var Min,Max:T3DCoordinate;
begin
   if Assigned(FOnRequestExtents) then
   begin
      Min:=ZERO; Max:=ZERO;
      FOnRequestExtents(self,Min,Max);
      FZoom:=1.0;
      FPan.X:=0;
      FPan.Y:=0;
      InitializeViewport(Min,Max);
   end;
end;{TFreeViewport.ZoomExtents}

procedure TFreeViewport.ZoomIn;
begin
   FZoom:=FZoom*Zoomfactor;
   FPan.X:=round(Zoomfactor*FPan.X);
   FPan.Y:=round(Zoomfactor*FPan.Y);
   Refresh;
end;{TFreeViewport.ZoomIn}

procedure TFreeViewport.ZoomOut;
begin
   FZoom:=FZoom/Zoomfactor;
   FPan.X:=round(FPan.X/Zoomfactor);
   FPan.Y:=round(FPan.Y/Zoomfactor);
   Refresh;
end;{TFreeViewport.ZoomOut}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeDevelopedPatch                                     }
{                                                                                                   }
{ Unrolled Subdivision layer                                                                        }
{---------------------------------------------------------------------------------------------------}
function TFreeDevelopedPatch.FGetMaxError:Extended;
var I:Integer;
begin
   Result:=0.0;
   for I:=1 to FEdges.Count do
   begin
      if I=1 then Result:=FEdgeErrors[I-1] else
         if FEdgeErrors[I-1]>result then Result:=FEdgeErrors[I-1];
   end;
end;{TFreeDevelopedPatch.FGetMaxError}

function TFreeDevelopedPatch.FGetShowErrorEdges:Boolean;
begin
   Result:=ShowInteriorEdges and FShowErrorEdges;
end;{TFreeDevelopedPatch.FGetShowErrorEdges}

function TFreeDevelopedPatch.FGetMidPoint:T2DCoordinate;
begin
   Result.X:=0.5*(FMin2D.X+FMax2D.X);
   Result.Y:=0.5*(FMin2D.Y+FMax2D.Y);
end;{TFreeDevelopedPatch.FGetMidPoint}

function TFreeDevelopedPatch.FGetMinError:Extended;
var I:Integer;
begin
   Result:=0.0;
   for I:=1 to FEdges.Count do
   begin
      if I=1 then Result:=FEdgeErrors[I-1] else
         if FEdgeErrors[I-1]<result then Result:=FEdgeErrors[I-1];
   end;
end;{TFreeDevelopedPatch.FGetMinError}

function TFreeDevelopedPatch.FGetPoint(index:Integer):T3DCoordinate;
var P : T2DCoordinate;
begin
   P:=F2DCoordinates[index].Coordinate;
   if (FMirrorOnScreen) and not (FMirror) then P.Y:=-P.Y;
   Result:=ConvertTo3D(P);
end;{TFreeDevelopedPatch.FGetPoint}

function TFreeDevelopedPatch.FGetMirrorPoint(index:Integer):T3DCoordinate;
var P    : T2DCoordinate;
    Tmp  : T3DCoordinate;
begin
   P:=F2DCoordinates[index].Coordinate;

   Tmp:=SetPoint(P.X,P.Y,0.0);
   Tmp:=MirrorPlane(Tmp,FMirrorplane);
   P.X:=Tmp.X;
   P.Y:=Tmp.Y;
   if (FMirrorOnScreen) and not (FMirror) then P.Y:=-P.Y;
   Result:=ConvertTo3D(P);
end;{TFreeDevelopedPatch.FGetMirrorPoint}

procedure TFreeDevelopedPatch.FSetRotation(Val:TFloatType);
begin
   FRotation:=val;
   FCos:=Cos(DegTorad(FRotation));
   FSin:=Sin(DegTorad(FRotation));
end;{TFreeDevelopedPatch.FSetRotation}

procedure TFreeDevelopedPatch.FSetTranslation(Val:T2DCoordinate);
begin
   FTranslation:=Val;
end;{TFreeDevelopedPatch.FSetTranslation}

procedure TFreeDevelopedPatch.FSetMirrorOnScreen(val:Boolean);
var Value:TFloatType;
begin
   if (val<>FMirrorOnScreen) and (not FMirror) then
   begin
      FMirrorOnScreen:=Val;
      Value:=FMin2D.Y;
      FMin2D.Y:=-FMax2D.Y;
      FMax2D.Y:=-Value;
      Rotation:=-Rotation;
   end;
end;{TFreeDevelopedPatch.FSetMirrorOnScreen}

procedure TFreeDevelopedPatch.Assign(Org:TFreeDevelopedPatch;Mirror:Boolean);
var I : Integer;
begin
   FName:=Org.FName;
   FShowSolid:=org.FShowSolid;
   FShowPartName:=Org.FShowPartName;
   FShowBoundingBox:=Org.FShowBoundingBox;
   FShowInteriorEdges:=Org.FShowInteriorEdges;
   FShowStations:=Org.FShowStations;
   FShowButtocks:=Org.FShowButtocks;
   FShowWaterlines:=Org.FShowWaterlines;
   FShowDiagonals:=Org.FShowDiagonals;
   FShowErrorEdges:=Org.FShowErrorEdges;
   FShowDimensions:=Org.FShowDimensions;
   FShadeSubmerged:=Org.FShadeSubmerged;

   FDimFontColor:=Org.FDimFontColor;
   FDimFontName:=Org.FDimFontName;
   FDimFontSize:=Org.FDimFontSize;

   FBoundaryEdges.Clear;
   FBoundaryEdges.AddList(Org.FBoundaryEdges);
   FPoints.Clear;
   FPoints.AddList(Org.FPoints);
   FEdges.Clear;
   FEdges.AddList(Org.FEdges);
   FCorners.Clear;
   FCorners.AddList(Org.FCorners);
   FDoneList.Clear;
   FDoneList.AddList(Org.FDoneList);
   Rotation:=-Org.Rotation;
   Translation:=Org.Translation;
   FNoIterations:=Org.FNoIterations;
   FMirrorOnScreen:=Org.FMirrorOnScreen;
   if length(Org.F2DCoordinates)>0 then
   begin
      Setlength(F2DCoordinates,FPoints.Count);
      for I:=1 to FPoints.Count do
      begin
         F2DCoordinates[I-1]:=Org.F2DCoordinates[I-1];
         if Mirror then F2DCoordinates[I-1].Coordinate.Y:=-F2DCoordinates[I-1].Coordinate.Y;
         if I=1 then
         begin
            FMin2D:=F2DCoordinates[I-1].Coordinate;
            FMax2D:=FMin2D;
         end else
         begin
            if F2DCoordinates[I-1].Coordinate.X<FMin2D.X then FMin2D.X:=F2DCoordinates[I-1].Coordinate.X;
            if F2DCoordinates[I-1].Coordinate.Y<FMin2D.Y then FMin2D.Y:=F2DCoordinates[I-1].Coordinate.Y;
            if F2DCoordinates[I-1].Coordinate.X>FMax2D.X then FMax2D.X:=F2DCoordinates[I-1].Coordinate.X;
            if F2DCoordinates[I-1].Coordinate.Y>FMax2D.Y then FMax2D.Y:=F2DCoordinates[I-1].Coordinate.Y;
         end;
      end;
   end;
   Setlength(FEdgeErrors,Fedges.Count);
   Move(Org.FEdgeErrors[0],FEdgeErrors[0],FEdges.Count*SizeOf(Double));
   FMaxAreaError:=Org.FMaxAreaError;
   FTotalAreaError:=Org.FTotalAreaError;
end;{TFreeDevelopedPatch.Assign}

procedure TFreeDevelopedPatch.Clear;
var I       : Integer;
    Spline  : TFreeSpline;
begin
   FConnectedMirror:=nil;
   FXGrid:=1;
   FYGrid:=1;
   FNoIterations:=0;
   FName:='';
   FPoints.Clear;
   FEdges.Clear;
   FDoneList.Clear;
   FCorners.Clear;
   FBoundaryEdges.Clear;
   Setlength(F2DCoordinates,0);
   Setlength(FEdgeErrors,0);
   Rotation:=0.0;
   FMin2D.X:=0.0;
   FMin2D.Y:=0.0;
   FMax2D:=FMin2D;
   FTranslation:=FMin2D;
   FShowSolid:=True;
   FShowBoundingBox:=False;
   FShowInteriorEdges:=True;
   FShowStations:=True;
   FShowButtocks:=True;
   FShowWaterlines:=True;
   FShowDiagonals:=True;
   FShowErrorEdges:=True;
   FShowDimensions:=True;
   FShowPartName:=True;
   FShadeSubmerged:=True;
   FVisible:=True;
   FMirrorOnScreen:=False;

   FDimFontColor:=clBlack;
   FDimFontName:='Arial';
   FDimFontSize:=6;

   for I:=1 to FStations.Count do
   begin
      Spline:=FStations[I-1];
      Spline.Destroy;
   end;
   FStations.Clear;
   for I:=1 to FButtocks.Count do
   begin
      Spline:=FButtocks[I-1];
      Spline.Destroy;
   end;
   FButtocks.Clear;
   for I:=1 to FWaterlines.Count do
   begin
      Spline:=FWaterlines[I-1];
      Spline.Destroy;
   end;
   FWaterlines.Clear;
   for I:=1 to FDiagonals.Count do
   begin
      Spline:=FDiagonals[I-1];
      Spline.Destroy;
   end;
   FDiagonals.Clear;
end;{TFreeDevelopedPatch.Clear}

constructor TFreeDevelopedPatch.Create;
begin
   Inherited Create;
   FOwner:=Owner;
   FPoints:=TFasterList.Create;
   FEdges:=TFasterList.Create;
   FDoneList:=TFasterList.Create;
   FBoundaryEdges:=TFasterlist.Create;
   FStations:=TFasterList.Create;
   FWaterlines:=TFasterList.Create;
   FButtocks:=TFasterList.Create;
   FDiagonals:=TFasterList.Create;
   FCorners:=TFasterList.Create;
   Clear;
end;{TFreeDevelopedPatch.Create}

destructor TFreeDevelopedPatch.Destroy;
begin
   Clear;
   Fillchar(FMirrorplane,SizeOf(FMirrorplane),0);
   FMirror:=False;
   FPoints.Destroy;
   FPoints := nil;
   FEdges.Destroy;
   FEdges := nil;
   FDoneList.Destroy;
   FDoneList := nil;
   FBoundaryEdges.Destroy;
   FBoundaryEdges := nil;
   FStations.Destroy;
   FStations := nil;
   FWaterlines.Destroy;
   FWaterlines := nil;
   FButtocks.Destroy;
   FButtocks := nil;
   FDiagonals.Destroy;
   FDiagonals := nil;
   FCorners.Destroy;
   FCorners := nil;
   Inherited Destroy;
end;{TFreeDevelopedPatch.Destroy}

function TFreeDevelopedPatch.DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
var I,S,E   : Integer;
    Edge    : TFreeSubdivisionEdge;
    P1,P2   : T3DCoordinate;
    Dist    : integer;
    Param   : TFloattype;
begin
   Result:=1000000;
   // check distance to all interior edges
   for I:=1 to FEdges.Count do
   begin
      Edge:=FEdges[I-1];
      S:=FPoints.SortedIndexOf(Edge.StartPoint);
      E:=FPoints.SortedIndexOf(Edge.EndPoint);
      if (S<>-1) and (E<>-1) then
      begin
         P1:=Point[S];
         P2:=Point[E];
         Dist:=Round(DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param));
         if dist<Result then Result:=Dist;
         if FMirror then
         begin
            P1:=MirrorPoint[S];
            P2:=MirrorPoint[E];
            Dist:=Round(DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param));
            if dist<Result then Result:=Dist;
         end;
      end;
   end;
end;{TFreeDevelopedPatch.DistanceToCursor}

procedure TFreeDevelopedPatch.Draw(Viewport:TFreeViewport);
var I,J,K,S,E  : Integer;
    Index,Cap  : Integer;
    Face       : TFreeSubdivisionFace;
    Edge       : TFreeSubdivisionEdge;
    Point      : TFreeSubdivisionPoint;
    Pts        : array of TPoint;
    Pt         : TPoint;
    P1,P2,P3   : T3DCoordinate;
    Min,Max    : T3DCoordinate;
    r,g,b      : integer;
    PenWidthfactor:Integer;
    EdgeColor  : TColor;
    Str        : string;
    WlPlane    : T3Dplane;
    MinZ,MaxZ  : TFloatType;
    s1,s2,s3   : TFloatType;
    Above,Below   : TFreeCoordinateArray;
    Na,Nb         : Integer;

    procedure DrawSpline(Spline:TFreeSpline);
    var I   : Integer;
        P2D : T2DCoordinate;
        P3D : T3DCoordinate;
        Pts : array of TPoint;
    begin
       //MM: for some reason number of fragments is not set for Dev Patch splines, so we set it here
       if Spline.Fragments = 0
          then Spline.Fragments := round(sqrt(sqrt(Spline.NumberOfPoints)) * 100.0);

       Setlength(Pts,Spline.Fragments+1);
       for I:=0 to Spline.Fragments do
       begin
          P3D:=Spline.Value(I/Spline.Fragments);
          P2D.X:=P3D.X;
          P2D.Y:=P3D.Y;
          if (FMirrorOnScreen) and not (FMirror) then P2D.Y:=-P2D.Y;
          P3D:=self.ConvertTo3D(P2D);
          Pts[I]:=Viewport.Project(P3D);
       end;
       Viewport.SetPenWidth(PenwidthFactor);
       Viewport.PenColor:=Spline.Color;
       Viewport.Polyline(Pts);
    end;{DrawSpline}

   procedure Swap(var P1,P2:T3DCoordinate);
   var Tmp:T3DCoordinate;
   begin
      Tmp:=P1;
      P1:=P2;
      P2:=Tmp;
   end;{Swap}

   procedure DrawDimension(P1,P2:T3DCoordinate);
   var Tmp  : Integer;
       P    : T3DCoordinate;
       Pt   : TPoint;
       Str  : string;
   begin
      if P2.X<>P1.X then
      begin
         if P2.X<P1.X then Swap(P1,P2);
         Tmp:=Trunc(P1.X/XGrid)-1;
         P.X:=Tmp*XGrid;
         while P.X<=P2.X+XGrid do
         begin
            if (Abs(P.X-P1.X)<1e-4) or (Abs(P.X-P2.X)<1e-4) or ((P.X>=P1.X) and (P.X<=P2.X)) then
            begin
               P.Y:=P1.Y+((P.X-P1.X)/(P2.X-P1.X))*(P2.Y-P1.Y);
               P.Z:=0.0;
               Pt:=Viewport.Project(P);
               Str:=ConvertDimension(P.Y,Units);
               Viewport.TextOut(Pt.X-Viewport.TextWidth(Str) div 2,Pt.Y,Str);
            end;
            P.X:=P.X+XGrid;
         end;
      end;
      if abs(P2.Y-P1.Y)<>0.0 then
      begin
         if P2.Y<P1.Y then Swap(P1,P2);
         Tmp:=Trunc(P1.Y/YGrid)-1;
         P.Y:=Tmp*YGrid;
         while P.Y<=P2.Y+YGrid do
         begin
            if (Abs(P.Y-P1.Y)<1e-4) or (Abs(P.Y-P2.Y)<1e-4) or ((P.Y>=P1.Y) and (P.Y<=P2.Y)) then
            begin
               P.X:=P1.X+((P.Y-P1.Y)/(P2.Y-P1.Y))*(P2.X-P1.X);
               P.Z:=0.0;
               Pt:=Viewport.Project(P);
               Str:=ConvertDimension(P.X,Units);
               Viewport.TextOut(Pt.X-Viewport.TextWidth(Str) div 2,Pt.Y,Str);
            end;
            P.Y:=P.Y+YGrid;
         end;
      end;
   end;{DrawDimension}

   procedure DrawTriangle(P1,P2,P3:T3DCoordinate;Color:TColor);
   var Pts:array[0..2] of TPoint;
   begin
      Pts[0]:=Viewport.Project(P1);
      Pts[1]:=Viewport.Project(P2);
      Pts[2]:=Viewport.Project(P3);
      Viewport.PenColor:=Color;
      Viewport.BrushColor:=Color;
      Viewport.Polygon(Pts);
   end;{DrawTriangle}

begin
   r:=round(0.9*GetRValue(Owner.Color));
   g:=round(0.9*GetGValue(Owner.Color));
   b:=round(0.9*GetBValue(Owner.Color));
   EdgeColor:=RGB(r,g,b);
   if Viewport.Printing then PenWidthfactor:=Round(Viewport.PrintScaleFactor)
                        else PenwidthFactor:=1;

   r:=round(1.1*GetRValue(Owner.Color));
   if r>255 then r:=255;
   g:=round(1.1*GetGValue(Owner.Color));
   if g>255 then g:=255;
   b:=round(1.1*GetBValue(Owner.Color));
   if b>255 then b:=255;
   Viewport.BrushColor:=RGB(r,g,b);
   Viewport.PenWidth:=PenwidthFactor;
   if FShowInteriorEdges then Viewport.PenColor:=EdgeColor else
   if ShowSolid then Viewport.PenColor:=Viewport.BrushColor
                else Viewport.PenColor:=clWhite;
   if ShowSolid then Viewport.BrushStyle:=bsSolid
                else Viewport.BrushStyle:=bsClear;
   Viewport.PenStyle:=psSolid;

   Cap:=0;
   if (ShowSolid) or (ShowInteriorEdges) then
   begin
      Viewport.BrushColor:=RGB(r,g,b);
      if (not showSolid) then Viewport.BrushStyle:=bsClear;

      Wlplane:=Owner.Owner.WaterlinePlane;

      for I:=1 to FDoneList.Count do
      begin
         Face:=FDoneList[I-1];
         if cap<>Face.NumberOfpoints then
         begin
            Cap:=Face.NumberOfpoints;
            setlength(Pts,Cap);
         end;

         if FShadeSubmerged then
         begin
            for J:=3 to Face.NumberOfpoints do
            begin
               Index:=FPoints.SortedIndexOf(Face.Point[0]);
               if Index<>-1 then P1:=self.Point[index];
               Index:=FPoints.SortedIndexOf(Face.Point[J-2]);
               if Index<>-1 then P2:=self.Point[index];
               Index:=FPoints.SortedIndexOf(Face.Point[J-1]);
               if Index<>-1 then P3:=self.Point[index];

               // Check if clipping is required

               Point:=Face.Point[0];
               s1:=WlPlane.a*Point.Coordinate.x+WlPlane.b*Point.Coordinate.y+WlPlane.c*Point.Coordinate.z+WlPlane.d;
               MinZ:=s1;
               MaxZ:=MinZ;
               Point:=Face.Point[J-2];
               s2:=WlPlane.a*Point.Coordinate.x+WlPlane.b*Point.Coordinate.y+WlPlane.c*Point.Coordinate.z+WlPlane.d;
               if s2<MinZ then MinZ:=s2 else if s2>MaxZ then MaxZ:=s2;
               Point:=Face.Point[J-1];
               s3:=WlPlane.a*Point.Coordinate.x+WlPlane.b*Point.Coordinate.y+WlPlane.c*Point.Coordinate.z+WlPlane.d;
               if s3<MinZ then MinZ:=s3 else if s3>MaxZ then MaxZ:=s3;
               if MaxZ<=0.0 then
               begin
                  // entirely below the plane
                  DrawTriangle(P1,P2,P3,Owner.Owner.UnderWaterColor);
               end else if MinZ>=0.0 then
               begin
                  // entirely above the plane
                  DrawTriangle(P1,P2,P3,Owner.Color);
               end else
               begin
                  // pierces water, clip triangle
                  ClipTriangle(P1,P2,P3,s1,s2,s3,Na,Nb,Above,Below);
                  for K:=3 to Na do DrawTriangle(Above[0],Above[K-2],Above[K-1],Owner.Color);
                  for K:=3 to Nb do DrawTriangle(Below[0],Below[K-2],Below[K-1],Owner.Owner.UnderWaterColor);
               end;
            end;   
         end else
         begin
            for J:=1 to Face.NumberOfpoints do
            begin
               Index:=FPoints.SortedIndexOf(Face.Point[J-1]);
               if Index<>-1 then
               begin
                  P1:=self.Point[index];
                  Pts[J-1]:=Viewport.Project(P1);
               end else Raise Exception.Create('Unrolled point could not be found!');
            end;
            Viewport.Polygon(Pts);
         end;
         if FMirror then
         begin
            for J:=1 to Face.NumberOfpoints do
            begin
               Index:=FPoints.SortedIndexOf(Face.Point[J-1]);
               if Index<>-1 then
               begin
                  P1:=MirrorPoint[index];
                  Pts[J-1]:=Viewport.Project(P1);
               end else Raise Exception.Create('Unrolled point could not be found!');
            end;
            Viewport.Polygon(Pts);
         end;
      end;
   end;

   if not ShowInteriorEdges then
   begin
      // Draw only boundaryedges
      Viewport.PenColor:=EdgeColor;
      Viewport.PenStyle:=psSolid;
      Viewport.PenWidth:=1*PenwidthFactor;
      for I:=1 to FBoundaryEdges.Count do
      begin
         Edge:=FBoundaryEdges[I-1];
         S:=FPoints.SortedIndexOf(Edge.StartPoint);
         E:=FPoints.SortedIndexOf(Edge.EndPoint);
         if (S<>-1) and (E<>-1) then
         begin
            Pt:=Viewport.Project(self.Point[S]);
            Viewport.MoveTo(Pt.X,Pt.Y);
            Pt:=Viewport.Project(self.Point[E]);
            Viewport.LineTo(Pt.X,Pt.Y);
            if FMirror then
            begin
               Pt:=Viewport.Project(MirrorPoint[S]);
               Viewport.MoveTo(Pt.X,Pt.Y);
               Pt:=Viewport.Project(MirrorPoint[E]);
               Viewport.LineTo(Pt.X,Pt.Y);
            end;
         end;
      end;
   end;

   // draw dimensions
   if ShowDimensions then
   begin
      Viewport.PenColor:=clBlack;
      Viewport.BrushStyle:=bsClear;
      Viewport.PenWidth:=PenwidthFactor;

      // calculate and set fontheight
      //Viewport.FontName:='arial';
      //Viewport.FontHeight:=round(DistPP3D(Viewport.Min3D,Viewport.Max3D)/150);

      Viewport.FontColor:=FDimFontColor;
      Viewport.FontName:=FDimFontName;
      Viewport.FontHeight:=FDimFontSize;

      for I:=1 to FBoundaryEdges.Count do
      begin
         Edge:=FBoundaryEdges[I-1];
         if (not FMirror) or (FMirror and (abs(Edge.FStartPoint.Coordinate.Y)>1e-3) and (abs(Edge.FEndPoint.Coordinate.Y)>1e-3)) then
         begin
            S:=FPoints.SortedIndexOf(Edge.StartPoint);
            E:=FPoints.SortedIndexOf(Edge.EndPoint);
            if (S<>-1) and (E<>-1) then
            begin
               DrawDimension(self.Point[S],self.Point[E]);
               if FMirror then DrawDimension(MirrorPoint[S],MirrorPoint[E]);
            end;
         end;
      end;

      for I:=1 to FCorners.Count do
      begin
         Point:=FCorners[I-1];
         S:=FPoints.SortedIndexOf(Point);
         P1:=Self.Point[S];
         Str:='('+ConvertDimension(P1.X,Units)+' / '+ConvertDimension(P1.Y,Units)+')';
         Pt:=Viewport.Project(P1);
         Viewport.TextOut(Pt.X-Viewport.TextWidth(Str) div 2,Pt.Y,Str);
         if FMirror then
         begin
            P1:=Self.MirrorPoint[S];
            Str:='('+ConvertDimension(P1.X,Units)+' / '+ConvertDimension(P1.Y,Units)+')';
            Pt:=Viewport.Project(P1);
            Viewport.TextOut(Pt.X-Viewport.TextWidth(Str) div 2,Pt.Y,Str);
         end;
      end;
   end;
   // show edges with errors
   if ShowErrorEdges then
   begin
      Viewport.PenWidth:=2*PenwidthFactor;
      for I:=1 to FEdges.Count do
      begin
         if abs(FEdgeErrors[I-1])>1e-4 then
         begin
            if FEdgeErrors[I-1]>0 then Viewport.PenColor:=clRed
                                  else Viewport.PenColor:=clBlue;
            Edge:=Fedges[I-1];
            S:=FPoints.SortedIndexOf(Edge.StartPoint);
            E:=FPoints.SortedIndexOf(Edge.EndPoint);
            if (S<>-1) and (E<>-1) then
            begin
               Pt:=Viewport.Project(self.Point[S]);
               Viewport.MoveTo(Pt.X,Pt.Y);
               Pt:=Viewport.Project(self.Point[E]);
               Viewport.LineTo(Pt.X,Pt.Y);
               if FMirror then
               begin
                  Pt:=Viewport.Project(MirrorPoint[S]);
                  Viewport.MoveTo(Pt.X,Pt.Y);
                  Pt:=Viewport.Project(MirrorPoint[E]);
                  Viewport.LineTo(Pt.X,Pt.Y);
               end;

            end;
         end;
      end;
   end;   

   if ShowStations then for I:=1 to FStations.Count do DrawSpline(FStations[I-1]);
   if ShowButtocks then for I:=1 to FButtocks.Count do DrawSpline(FButtocks[I-1]);
   if ShowWaterlines then for I:=1 to FWaterlines.Count do DrawSpline(FWaterlines[I-1]);
   if ShowDiagonals then for I:=1 to FDiagonals.Count do DrawSpline(FDiagonals[I-1]);

   if ShowPartName then
   begin
      P1:=ConvertTo3D(MidPoint);
      Pt:=Viewport.Project(P1);
      Viewport.FontColor:=clBlack;
      Viewport.PenColor:=clBlack;
      Viewport.PenWidth:=1;
      Viewport.PenStyle:=psSolid;
      Viewport.BrushColor:=clWhite;
      Viewport.BrushStyle:=bsSolid;
      I:=Owner.Owner.ControlPointSize;
      Viewport.FontSize:=7;
      Viewport.Rectangle(Pt.X-I,Pt.Y-I,Pt.X+I,Pt.Y+I);
      Viewport.BrushStyle:=bsClear;
      Viewport.TextOut(Pt.X-Viewport.TextWidth(Name) div 2,Pt.Y,Name);
   end;
   if (ShowBoundingBox) and (not viewport.Printing) then
   begin
      Viewport.PenColor:=clBlack;
      Viewport.PenStyle:=psDot;
      Viewport.PenWidth:=1;
      Viewport.BrushStyle:=bsClear;
      Extents(Min,Max);
      Setlength(Pts,4);
      P1:=Min;
      Pts[0]:=Viewport.Project(P1);
      P1.X:=Max.X;
      Pts[1]:=Viewport.Project(P1);
      P1.Y:=Max.Y;
      Pts[2]:=Viewport.Project(P1);
      P1.X:=Min.X;
      Pts[3]:=Viewport.Project(P1);
      Viewport.Polygon(Pts);
   end;
   Viewport.BrushStyle:=bsClear;
   Viewport.FontColor:=clBlack;

end;{TFreeDevelopedPatch.Draw}

procedure TFreeDevelopedPatch.Extents(var Min,Max:T3DCoordinate);
var I : Integer;
    P : T3DCoordinate;
begin
   if FPoints.Count=0 then
   begin
      Min.X:=-1.0;
      Min.Y:=-1.0;
      Min.Z:= 0.0;
      Max.X:= 1.0;
      Max.Y:= 1.0;
      Max.Z:= 0.0;
   end else for I:=1 to FPoints.Count do
   begin
      P:=Point[I-1];
      if I=1 then
      begin
         Min:=P;
         Max:=Min;
      end;
      MinMax(P,Min,Max);
      if FMirror then
      begin
         P:=MirrorPoint[I-1];
         MinMax(P,Min,Max);
      end;
   end;
end;{TFreeDevelopedPatch.Extents}

procedure TFreeDevelopedPatch.IntersectPlane(Plane:T3DPlane;Color:TColor);
type IntersectionData = record
                           Point    : T3DCoordinate;
                           Knuckle  : Boolean;
                        end;

var I,J,K      : Integer;
    Index1     : Integer;
    Index2     : Integer;
    Edge       : TFreeSubdivisionEdge;
    P1,P2      : TFreeSubdivisionPoint;
    Side1      : TFloatType;
    Side2      : TFloatType;
    Parameter  : TFloatType;
    Output     : T3DCoordinate;
    Spline     : TFreeSpline;
    Copy       : TFreeSpline;
    Face       : TFreeSubdivisionface;
    IntArray   : array of IntersectionData;
    ArrayLength: Integer;
    NoPoints   : Integer;
    Dest       : TFasterList;
begin

   ArrayLength:=10;
   Setlength(IntArray,ArrayLength);
   Dest:=TFasterList.Create;

   for J:=1 to FDoneList.Count do
   begin
      Face:=FDoneList[J-1];
      NoPoints:=0;
      P1:=Face.Point[Face.NumberOfPoints-1];
      Side1:=Plane.A*P1.Coordinate.x+Plane.B*P1.Coordinate.y+Plane.C*P1.Coordinate.z+Plane.D;
      for K:=1 to Face.FPoints.Count do
      begin
         P2:=Face.FPoints[K-1];
         Side2:=Plane.A*P2.Coordinate.x+Plane.B*P2.Coordinate.y+Plane.C*P2.Coordinate.z+Plane.D;
         if ((Side1<-1e-5) and (Side2>1e-5)) or
            ((Side1>1e-5) and (Side2<-1e-5)) then
         begin
            // regular intersection of edge
            // add the edge to the list
            Parameter:=-side1/(side2-side1);
            Index1:=FPoints.SortedIndexOf(P1);
            Index2:=FPoints.SortedIndexOf(P2);
            Output.X:=F2DCoordinates[Index1].Coordinate.X+Parameter*(F2DCoordinates[Index2].Coordinate.X-F2DCoordinates[Index1].Coordinate.X);
            Output.Y:=F2DCoordinates[Index1].Coordinate.Y+Parameter*(F2DCoordinates[Index2].Coordinate.Y-F2DCoordinates[Index1].Coordinate.Y);
            Output.Z:=0.0;
            Inc(NoPoints);
            if NoPoints>ArrayLength then
            begin
               Inc(ArrayLength,10);
               Setlength(IntArray,ArrayLength);
            end;
            IntArray[NoPoints-1].Point:=Output;
            Edge:=Owner.FOwner.EdgeExists(P1,P2);
            if Edge<>nil then IntArray[NoPoints-1].Knuckle:=Edge.Crease
                         else IntArray[NoPoints-1].Knuckle:=False;
         end else
         begin
            // Does the edge lie entirely within the plane??
            if ((abs(side1)<=1e-5) and (abs(Side2)<=1e-5)) then
            begin
            end else if abs(Side2)<1e-5 then
            begin
               Inc(NoPoints);
               if NoPoints>ArrayLength then
               begin
                  Inc(ArrayLength,10);
                  Setlength(IntArray,ArrayLength);
               end;
               Index2:=FPoints.SortedIndexOf(P2);
               IntArray[NoPoints-1].Point.X:=F2DCoordinates[Index2].Coordinate.X;
               IntArray[NoPoints-1].Point.Y:=F2DCoordinates[Index2].Coordinate.Y;
               IntArray[NoPoints-1].Point.Z:=0.0;
               IntArray[NoPoints-1].Knuckle:=P2.VertexType<>svRegular;
            end;
         end;
         P1:=P2;
         Side1:=Side2;
      end;
      if NoPoints>1 then
      begin
         if DistPP3D(IntArray[0].Point,IntArray[NoPoints-1].Point)<1e-4 then
         begin
            dec(NoPoints);
         end;
         if NoPoints>1 then
         begin
            Spline:=TFreeSpline.Create;
            Spline.Color:=Color;
            Spline.Capacity:=NoPoints;
            for K:=1 to NoPoints do
            begin
               Spline.Add(IntArray[K-1].Point);
               Spline.Knuckle[Spline.NumberOfPoints-1]:=IntArray[K-1].Knuckle;
            end;
            Dest.Add(Spline);
         end;
      end;
   end;

   if Dest.Count>1 then
   begin
      Dest.Capacity:=Dest.Count;
      JoinSplineSegments(0.01,False,Dest);
      for I:=Dest.Count downto 1 do
      begin
         // Remove tiny fragments of very small length
         Spline:=Dest[I-1];
         if Spline.NumberOfPoints>1 then
         begin
            Parameter:=SquaredDistPP(Spline.Min,Spline.Max);
            if Parameter<1e-3 then
            begin
               Spline.Destroy;
               Dest.Delete(I-1);
            end;
         end;
      end;
   end;
   for I:=1 to Dest.Count do
   begin
      Spline:=Dest[I-1];
      if abs(Plane.a)>0.9999 then FStations.Add(Spline) else
         if abs(Plane.b)>0.9999 then FButtocks.Add(Spline) else
            if abs(Plane.c)>0.9999 then FWaterlines.Add(Spline) else
              if (abs(Plane.b)>0.5) and (abs(Plane.c)>0.5) then FDiagonals.Add(Spline);
      if FMirror then
      begin
         Copy:=TFreeSpline.Create;
         Copy.Assign(Spline);
         for J:=1 to Spline.NumberOfPoints do
         begin
            Copy.Point[J-1]:=MirrorPlane(Spline.Point[J-1],FMirrorPlane);
         end;
         if abs(Plane.a)>0.9999 then FStations.Add(Copy) else
            if abs(Plane.b)>0.9999 then FButtocks.Add(Copy) else
               if abs(Plane.c)>0.9999 then FWaterlines.Add(Copy) else
                  if (abs(Plane.b)>0.5) and (abs(Plane.c)>0.5) then FDiagonals.Add(Copy);
      end;
   end;
   Dest.Destroy;

end;{TFreeDevelopedPatch.IntersectPlane}

function TFreeDevelopedPatch.ConvertTo3D(P:T2DCoordinate):T3DCoordinate;
var Mid  : T2DCoordinate;
    P2 : T2DCoordinate;
begin
   Mid:=MidPoint;
   // translate to origin
   P2.X:=P.X-Mid.X;
   P2.Y:=P.Y-Mid.Y;
   // rotate around origin
   P.x:=P2.x*FCos-P2.y*FSin;
   P.y:=P2.x*FSin+P2.y*FCos;
   // Translate back again
   Result.X:=P.X+Mid.X+FTranslation.X;
   Result.Y:=P.Y+Mid.Y+FTranslation.Y;
   result.Z:=0.0;
end;{TFreeDevelopedPatch.ConvertTo3D}

procedure TFreeDevelopedPatch.SaveToDXF(Strings:TStringList);
var I,J  : Integer;
    Col  : Integer;
    Index: integer;
    P    : TFreeSubdivisionPoint;
    P3D  : T3DCoordinate;
    Layer: String;
    Source,Dest:TFasterList;

    procedure ExportSpline(Spline:TFreeSpline;Layername:string);
    var I,Col  : Integer;
        P2D    : T2DCoordinate;
        P3D    : T3DCoordinate;
    begin
       Col:=FindDXFColorIndex(Spline.Color);
       Strings.Add('0'+EOL+'POLYLINE');
       Strings.Add('8'+EOL+LayerName);   // layername
       Strings.Add('62'+EOL+IntToStr(Col));  // color by layer
       Strings.Add('66'+EOL+'1');    // vertices follow
       for I:=0 to Spline.Fragments do
       begin
          P3D:=Spline.Value(I/Spline.Fragments);
          P2D.X:=P3D.X;
          P2D.Y:=P3D.Y;

          if (FMirrorOnScreen) and not (FMirror) then P2D.Y:=-P2D.Y;

          P3D:=ConvertTo3D(P2D);
          Strings.Add('0'+EOL+'VERTEX');
          Strings.Add('8'+EOL+Layername);
          Strings.Add('10'+EOL+Truncate(P3D.X,4));
          Strings.Add('20'+EOL+Truncate(P3D.Y,4));
       end;
       Strings.Add('0'+EOL+'SEQEND');
    end;{ExportSpline}

begin
   // Extract edges as polylines
   Source:=TFasterList.Create;
   Source.AddList(FBoundaryEdges);
   Dest:=TFasterList.Create;
   Owner.Owner.IsolateEdges(Source,Dest);
   Source.Destroy;
   Col:=FindDXFColorIndex(Owner.Color);
   Layer:=Owner.Name;

   for I:=1 to Dest.Count do
   begin
      Source:=Dest[I-1];
      // Save data as 2D polyline
      Strings.Add('0'+EOL+'POLYLINE');
      Strings.Add('8'+EOL+Layer);               // layername
      Strings.Add('62'+EOL+IntToStr(Col));      // color by layer
      Strings.Add('66'+EOL+'1');                // vertices follow
      for J:=1 to Source.Count do
      begin
         P:=Source[J-1];
         Index:=FPoints.SortedIndexOf(P);
         P3D:=Point[Index];
         Strings.Add('0'+EOL+'VERTEX');
         Strings.Add('8'+EOL+Layer);
         Strings.Add('10'+EOL+Truncate(P3D.X,4));
         Strings.Add('20'+EOL+Truncate(P3D.Y,4));
      end;
      Strings.Add('0'+EOL+'SEQEND');
      if FMirror then
      begin
         Strings.Add('0'+EOL+'POLYLINE');
         Strings.Add('8'+EOL+Layer);            // layername
         Strings.Add('62'+EOL+IntToStr(Col));   // color by layer
         Strings.Add('66'+EOL+'1');             // vertices follow
         for J:=1 to Source.Count do
         begin
            P:=Source[J-1];
            Index:=FPoints.SortedIndexOf(P);
            P3D:=MirrorPoint[Index];
            Strings.Add('0'+EOL+'VERTEX');
            Strings.Add('8'+EOL+Layer);
            Strings.Add('10'+EOL+Truncate(P3D.X,4));
            Strings.Add('20'+EOL+Truncate(P3D.Y,4));
         end;
         Strings.Add('0'+EOL+'SEQEND');
      end;
      Source.Destroy;
   end;
   if ShowStations then for I:=1 to FStations.Count do ExportSpline(FStations[I-1],'stations');
   if ShowButtocks then for I:=1 to FButtocks.Count do ExportSpline(FButtocks[I-1],'buttocks');
   if ShowWaterlines then for I:=1 to FWaterlines.Count do ExportSpline(FWaterlines[I-1],'waterlines');
   if ShowDiagonals then for I:=1 to FDiagonals.Count do ExportSpline(FDiagonals[I-1],'diagonals');
   Dest.Destroy;
end;{TFreeDevelopedPatch.SaveToDXF}

procedure TFreeDevelopedPatch.SaveToTextFile(Strings:TStringList);
var I,J  : Integer;
    Index: integer;
    P    : TFreeSubdivisionPoint;
    P3D  : T3DCoordinate;
    Source,Dest:TFasterList;
    Min,Max:T3DCoordinate;
    First:Boolean;

begin
   // Extract edges as polylines
   Source:=TFasterList.Create;
   Source.AddList(FBoundaryEdges);
   Dest:=TFasterList.Create;
   Owner.Owner.IsolateEdges(Source,Dest);
   Source.Destroy;

   // Calculate min.,max extents of coundary
   // all measurements are referred to the min. coordinate
   First:=True;
   Strings.Add('');
   Strings.Add('Boundary coordinates for: '+Name);
   for I:=1 to Dest.Count do
   begin
      Source:=Dest[I-1];
      for J:=1 to Source.Count do
      begin
         P:=Source[J-1];
         Index:=FPoints.SortedIndexOf(P);
         P3D:=Point[Index];
         if First then
         begin
            Min:=P3D;
            Max:=min;
            First:=false;
         end else MinMax(P3D,Min,Max);
         if FMirror then
         begin
            P3D:=MirrorPoint[Index];
            MinMax(P3D,Min,Max);
         end;
      end;
   end;

   for I:=1 to Dest.Count do
   begin
      Source:=Dest[I-1];
      if I>1 then Strings.Add('');
      for J:=1 to Source.Count do
      begin
         P:=Source[J-1];
         Index:=FPoints.SortedIndexOf(P);
         P3D:=Point[Index];
         P3D.X:=P3D.X-Min.X;
         P3D.Y:=P3D.Y-Min.Y;
         P3D.Z:=P3D.Z-Min.Z;
         Strings.Add(FloatToStrF(P3D.X,ffFixed,7,3)+#32+FloatToStrF(P3D.Y,ffFixed,7,3));
      end;

      if FMirror then
      begin
         Strings.Add('');
         for J:=1 to Source.Count do
         begin
            P:=Source[J-1];
            Index:=FPoints.SortedIndexOf(P);
            P3D:=MirrorPoint[Index];
            P3D.X:=P3D.X-Min.X;
            P3D.Y:=P3D.Y-Min.Y;
            P3D.Z:=P3D.Z-Min.Z;
            Strings.Add(FloatToStrF(P3D.X,ffFixed,7,3)+#32+FloatToStrF(P3D.Y,ffFixed,7,3));
         end;
      end;
      Source.Destroy;
   end;
   Dest.Destroy;
end;{TFreeDevelopedPatch.SaveToDXF}

procedure TFreeDevelopedPatch.Unroll(ControlFaces:TFasterList);
type TPolygonOrientation = (poCCW,poCW);
var I,J,K,N       : Integer;
    BestIndex     : integer;
    ErrorIndex    : Integer;
    Error,Area    : extended;
    MaxError      : Extended;
    SeedArea      : extended;
    OptArea,Dist  : TFloatType;
    OptAngle      : TFloatType;
    Min,Max       : T3DCoordinate;
    Normal        : T3DCoordinate;
    P3D1,P3D2,P3D3: T3DCoordinate;
    Ctrlface      : TFreeSubdivisionControlFace;
    Child,Face    : TFreeSubdivisionface;
    Seedface      : TFreeSubdivisionface;
    Edge          : TFreeSubdivisionEdge;
    P1,P2,P3      : TFreeSubdivisionPoint;
    Processed     : array of boolean;
    FFaces        : TFasterList;
    SeedFaces     : TFasterList;
    Orientation   : TPolygonOrientation;
    Winding       : TPolygonOrientation;
    First         : Boolean;
    TmpEdges      : TFasterList;
    SortedEdges   : TFasterList;

    function Distance2D(P1,P2:T2DCoordinate):Extended;
    var dX,dY:Extended;
    begin
       dX := P2.X - P1.X;
       dY := P2.Y - P1.Y;
       Result:=sqrt(sqr(dX)+sqr(dY));
    end;{Distance2D}

    function Distance3D(P1,P2:T3DCoordinate):Extended;
    var dX,dY,dZ:Extended;
    begin
       dX := P2.X - P1.X;
       dY := P2.Y - P1.Y;
       dZ := P2.Z - P1.Z;
       Result:=sqrt(sqr(dX)+sqr(dY)+sqr(dZ));
    end;{Distance3D}

   // Crossproduct
   //
   // Computes the crossproduct of three points
   // Returns whether their internal angle is clockwise or counter-clockwise.
   function Crossproduct(P1,P2,P3:T2DCoordinate):TPolygonOrientation;
   var Tmp:extended;
   begin
      Result:=poCCW;
      Tmp:=(P2.X-P1.X)*(P3.Y-P2.Y)-(P2.Y-P1.Y)*(P3.X-P2.X);
      if Tmp>=0.0 then Result:=poCCW else
         if Tmp<0 then Result:=poCW;
   end;{Crossproduct}

   // Calculates the third point of a triangle when the length of its
   // three sides and two coordinates are known
   Function CalculateTriangle(a,b,c:extended;P1,P2:T2DCoordinate):T2DCoordinate;
   var Fie1,Fie2  : extended;
       Fie3,P     : extended;
   begin
      if abs(P2.X-P1.X)<=1e-6 then
      begin
         if P2.X=P1.X then
         begin
            if P2.Y>p1.Y then Fie1:=0.5*Pi
                         else Fie1:=-0.5*Pi;
         end else
         begin
            Fie1:=ArcTan((P2.Y-P1.Y)/(P2.X-P1.X));
         end;
      end else Fie1:=ArcTan((P2.Y-P1.Y)/(P2.X-P1.X));
      if (Fie1<0) and (P2.Y>P1.Y) then Fie1:=Fie1+Pi;
      if (Fie1>0) and (P2.Y<P1.Y) then Fie1:=Fie1+Pi;
      if b*c<>0 then P:=(b*b+c*c-a*a)/(2*b*c)
                else p:=1*Sign(b*b+c*c-a*a);
      if p>1 then p:=1 else if P<-1 then p:=-1;
      if P=0.0 then
      begin
         Fie2:=0.5*Pi;
      end else Fie2:=arcTan(Sqrt(1-p*p)/p);
      if p<0 then Fie2:=Pi-abs(Fie2);
      Fie3:=Fie1-Fie2;
      Result.X:=b*Cos(Fie3)+P1.X;
      Result.Y:=b*Sin(Fie3)+P1.Y;
   end;{CalculateTriangle}

   Function CalculateTriangle2(a,b,c:extended;P1,P2:T2DCoordinate):T2DCoordinate;
   var Fie1,Fie2  : extended;
       Beta       : extended;
       Tmp        : extended;
   begin
      if abs(P2.X-P1.X)<=1e-6 then
      begin
         if P2.X=P1.X then
         begin
            if P2.Y>p1.Y then Fie1:=0.5*Pi
                         else Fie1:=-0.5*Pi;
         end else
         begin
            Fie1:=ArcTan((P2.Y-P1.Y)/(P2.X-P1.X));
         end;
      end else Fie1:=ArcTan((P2.Y-P1.Y)/(P2.X-P1.X));
      if (Fie1<0) and (P2.Y>P1.Y) then Fie1:=Fie1+Pi;
      if (Fie1>0) and (P2.Y<P1.Y) then Fie1:=Fie1+Pi;
      if 2*a*c=0 then
      begin
         if a=0.0 then a:=1e-7;
         if c=0.0 then c:=1e-7;
      end;
      begin
         Tmp:=(a*a+c*c-b*b)/(2*a*c);
         if tmp>1 then
         begin
            tmp:=1;
         end else
         if tmp<-1 then
         begin
            tmp:=-1;
         end;
         Beta:=ArcCos(Tmp);
         Fie2:=Fie1+Beta;
         Result.X:=c*Cos(Fie2)+P1.X;
         Result.Y:=c*Sin(Fie2)+P1.Y;
      end;
   end;{CalculateTriangle}

   Procedure Unroll2D(Face:TFreeSubdivisionFace;FirstFace:boolean; var Error:Boolean);
   var I,S,E      : Integer;
       Index1     : Integer;
       Index2     : Integer;
       Index3     : Integer;
       P1,P2,P3   : TFreeSubdivisionPoint;
       Indices    : array of boolean;

       procedure ProcessTriangle(P1,P2,P3:TFreeSubdivisionPoint;Ind1,Ind2,Ind3:Integer);
       var P1_2D  : T2DCoordinate;
           P2_2D  : T2DCoordinate;
           P3_2D  : T2DCoordinate;
           a,b,c  : extended;
       begin
          a:=Distance3D(P1.Coordinate,P2.Coordinate);
          b:=Distance3D(P2.Coordinate,P3.Coordinate);
          c:=Distance3D(P3.Coordinate,P1.Coordinate);
          if (not Processed[Ind1]) and
             (not Processed[Ind2]) and
             (not Processed[Ind3]) then
          begin
             // first face, calculate P1, P2 and P3
             P1_2D.X:=P1.Coordinate.X;
             P1_2D.Y:=P1.Coordinate.Y;
             P2_2D.X:=P1_2D.X;
             P2_2D.Y:=P1_2D.Y+a;
             Processed[Ind1]:=True;
             F2DCoordinates[Ind1].Coordinate:=P1_2D;
             Processed[Ind2]:=True;
             F2DCoordinates[Ind2].Coordinate:=P2_2D;
          end;
          P1_2D:=F2DCoordinates[Ind1].Coordinate;
          P2_2D:=F2DCoordinates[Ind2].Coordinate;
          P3_2D:=F2DCoordinates[Ind3].Coordinate;
          if (Processed[Ind1]) and (Processed[Ind2]) and (not Processed[Ind3]) then
          begin
             // calculate position of P3
             a:=Distance2D(P1_2D,P2_2D);
             //P3_2D:=CalculateTriangle(b,c,a,P1_2D,P2_2D);
             P3_2D:=CalculateTriangle2(a,b,c,P1_2D,P2_2D);
             if First then
             begin
               Orientation:=Crossproduct(P1_2D,P2_2D,P3_2D);
               First:=False;
             end else
             begin
                Winding:=Crossproduct(P1_2D,P2_2D,P3_2D);
                if Winding<>Orientation then
                begin
                   if not Error then
                   begin
                      Error:=True;
                   end;
                end;
             end;
             F2DCoordinates[Ind3].Coordinate:=P3_2D;
             Processed[ind3]:=True;
          end;
       end;
   begin
      Error:=false;
      setlength(Indices,face.NumberOfpoints);
      for I:=1 to Face.NumberOfpoints do
      begin
         Index1:=FPoints.SortedIndexOf(Face.Point[I-1]);
         Indices[I-1]:=Processed[Index1];
      end;
      // find two succ. calculated points
      S:=-1;
      E:=-1;
      for I:=1 to Face.NumberOfpoints do
      begin
         if Indices[I-1] then
         begin
            Index1:=I mod face.NumberOfpoints;
            if Indices[Index1] then
            begin
               S:=I-1;
               E:=Index1;
               break;
            end;
         end;
      end;
      if (S<>-1) and (E<>-1) then
      begin
         for I:=3 to Face.NumberOfpoints do
         begin
            P1:=Face.Point[S];
            Index1:=FPoints.SortedIndexOf(P1);
            E:=(S+I-2) mod face.NumberOfpoints;
            P2:=Face.Point[E];
            Index2:=FPoints.SortedIndexOf(P2);
            E:=(S+I-1) mod face.NumberOfpoints;
            P3:=Face.Point[E];
            Index3:=FPoints.SortedIndexOf(P3);
            ProcessTriangle(P1,P2,P3,Index1,Index2,index3);
         end;
      end else
      begin
         for I:=3 to Face.NumberOfpoints do
         begin
            P1:=Face.Point[0];
            Index1:=FPoints.SortedIndexOf(P1);
            P2:=Face.Point[I-2];
            Index2:=FPoints.SortedIndexOf(P2);
            P3:=Face.Point[I-1];
            Index3:=FPoints.SortedIndexOf(P3);
            ProcessTriangle(P1,P2,P3,Index1,Index2,index3);
         end;
      end;
   end;{Unroll2D}

   function TriangleArea(P1,P2,P3:T2DCoordinate):extended;
   const half:Extended=0.5;
   var d12x,d12y,d23x,d23y,d31x,d31y:Extended;
   begin
      d12x := (P1.x-P2.x);
      d12y := (P1.y-P2.y);
      d23x := (P2.x-P3.x);
      d23y := (P2.y-P3.y);
      d31x := (P3.x-P1.x);
      d31y := (P3.y-P1.y);
      Result:=half*(d12x*d12y + d23x*d23y + d31x*d31y);
   end;{TriangleArea}

   procedure ProcessFaces(Seedface:TFreeSubdivisionFace;var MaxError:extended;var ErrorIndex:Integer);
   var I,J,k,S,E,P: Integer;
       Index      : Integer;
       ToDoList   : TFasterlist;
       Face       : TFreeSubdivisionFace;
       Child      : TFreeSubdivisionFace;
       Edge       : TFreeSubdivisionEdge;
       P1,P2      : TFreeSubdivisionPoint;
       L2D,L3D    : Extended;
       Error      : extended;
       TotalError : Extended;
       _3DArea    : Extended;
       _2DArea    : extended;
       Temp       : Boolean;
   begin
      MaxError:=0.0;
      TotalError:=0.0;
      FMaxAreaError:=0.0;
      FTotalAreaError:=0.0;

      ErrorIndex:=-1;
      Setlength(F2DCoordinates,FPoints.Count);
      Setlength(Processed,FPoints.Count);
      Setlength(FEdgeErrors,FEdges.Count);
      for I:=1 to FEdges.Count do FEdgeErrors[I-1]:=0.0;

      for i:=1 to FPoints.Count do
      begin
         Processed[I-1]:=False;
         F2DCoordinates[I-1].Coordinate.X:=0.0;
         F2DCoordinates[I-1].Coordinate.Y:=0.0;
      end;
      ToDoList:=TFasterList.Create;       // Assemble all faces to be developed in a list
      ToDoList.AddList(FFaces);
      ToDoList.Sort;
      FDoneList.Clear;
      FDoneList.Capacity:=ToDoList.Count;

      First:=True;
      while ToDoList.Count>0 do
      begin
         if SeedFace=nil then
         begin
            // Find a new seedface, this layer has multiple areas
            SeedFace:=ToDoList[0];
         end;
         FDoneList.Add(SeedFace);
         Index:=ToDoList.SortedIndexOf(Seedface);
         if Index<>-1 then ToDoList.Delete(Index);
         I:=1;
         while I<=FDoneList.Count do
         begin
            Face:=FDoneList[I-1];
            Unroll2D(Face,I=1,Temp);
            if (temp) and (ErrorIndex=-1) then ErrorIndex:=I-1;
            P1:=Face.Point[Face.NumberOfPoints-1];
            for J:=1 to Face.NumberOfpoints do
            begin
               P2:=Face.Point[J-1];
               Edge:=Owner.FOwner.EdgeExists(P1,P2);
               if Edge<>nil then
               begin
                  for K:=1 to Edge.NumberOfFaces do
                  begin
                     Child:=Edge.Face[K-1];
                     Index:=ToDoList.SortedIndexOf(Child);
                     if Index<>-1 then
                     begin
                        FDoneList.Add(Child);
                        ToDoList.Delete(Index);
                     end;
                  end;
               end;
               P1:=P2;
            end;
            inc(I);
         end;
         SeedFace:=nil;
      end;
      ToDoList.Destroy;

      // Calculate diff. in area of all faces
      FMaxAreaerror:=0.0;
      FTotalAreaError:=0.0;
      for I:=1 to FDoneList.Count do
      begin
         Face:=FDoneList[I-1];
         _2DArea:=0.0;
         _3DArea:=Face.Area;
         // calculate 2D area
         S:=FPoints.SortedIndexOf(Face.Point[0]);
         for J:=3 to Face.NumberOfpoints do
         begin
            E:=FPoints.SortedIndexOf(Face.Point[J-2]);
            P:=FPoints.SortedIndexOf(Face.Point[J-1]);
            _2DArea:=_2DArea+TriangleArea(F2DCoordinates[S].Coordinate,F2DCoordinates[E].Coordinate,F2DCoordinates[P].Coordinate);
         end;
         Error:=_2DArea-_3DArea;
         FTotalAreaError:=FTotalAreaError+Error;
         Error:=abs(Error);
         if Error>FMaxAreaError then FMaxAreaError:=Error;
      end;

      // calculate min/max errors of edges
      for I:=1 to FEdges.Count do
      begin
         Edge:=FEdges[I-1];
         S:=FPoints.SortedIndexOf(Edge.StartPoint);
         E:=FPoints.SortedIndexOf(Edge.EndPoint);
         if (S<>-1) and (E<>-1) then
         begin
            // original distance in 3D
            L3D:=sqr(Edge.Endpoint.Coordinate.X-Edge.Startpoint.Coordinate.X)+
                 sqr(Edge.Endpoint.Coordinate.Y-Edge.Startpoint.Coordinate.Y)+
                 sqr(Edge.Endpoint.Coordinate.Z-Edge.Startpoint.Coordinate.Z);
            L2D:=sqr(F2DCoordinates[E].Coordinate.X-F2DCoordinates[S].Coordinate.X)+
                 sqr(F2DCoordinates[E].Coordinate.Y-F2DCoordinates[S].Coordinate.Y);
            Error:=Sqrt(L2D)-Sqrt(L3D);
            TotalError:=TotalError+abs(Error);
            if abs(Error)>MaxError then MaxError:=abs(Error);
            FEdgeErrors[I-1]:=Error;
         end;
      end;
      MaxError:=MaxError+abs(FTotalAreaError);
   end;{ProcessFaces}

begin
   // first assemble all points, edges and faces used
   FFaces:=TFasterList.Create;
   for K:=1 to ControlFaces.Count do
   begin
      Ctrlface:=ControlFaces[K-1];
      FFaces.AddList(Ctrlface.FChildren);
      FFaces.Sort;
      for I:=1 to Ctrlface.ChildCount do
      begin
         Child:=Ctrlface.Child[I-1];
         P1:=Child.Point[Child.NumberOfPoints-1];
         for J:=1 to Child.NumberOfpoints do
         begin
            P2:=Child.Point[J-1];
            // Add this point
            if FPoints.SortedIndexOf(P2)=-1 then FPoints.AddSorted(P2);
            // add this edge
            Edge:=Owner.Owner.EdgeExists(P1,P2);
            if Edge<>nil then if FEdges.SortedIndexOf(Edge)=-1 then FEdges.AddSorted(Edge);
            P1:=P2;
         end;
      end;
   end;

   FPoints.Capacity:=FPoints.Count;
   FEdges.Capacity:=FEdges.Count;

   // Find the seed face, which is characterized by the face that it
   // has one cornerpoint with (possibly) multiple faces, but only 1
   // face is present in the list of faces to be unrolled.
   SeedFaces:=TFasterList.Create;
   SeedFace:=nil;
   SeedArea:=0;
   for I:=1 to FPoints.Count do
   begin
      P1:=FPoints[I-1];
      N:=0;
      for J:=1 to P1.FFaces.Count do
      begin
         if FFaces.SortedIndexOf(P1.FFaces[J-1])<>-1 then inc(N);
      end;
      if N=1 then
      begin
         for J:=1 to P1.FFaces.Count do
         begin
            if FFaces.SortedIndexOf(P1.FFaces[J-1])<>-1 then
            begin
               Area:=P1.Face[J-1].Area;
               if (Area>SeedArea) or (SeedFace=nil) then
               begin
                  SeedFace:=P1.FFaces[J-1];
                  SeedArea:=Area;
               end;
               Seedfaces.Add(P1.FFaces[J-1]);
            end;
         end;
      end;
   end;

   // if NO seedfaces could be found (which should not occur) then
   // pick a random one (the one with the largest area)
   if Seedfaces.Count=0 then
   begin
      for I:=1 to FFaces.Count do
      begin
         Face:=FFaces[I-1];
         Area:=Face.Area;
         if (I=1) or (Area>SeedArea) then
         begin
            SeedFace:=face;
            SeedArea:=Area;
         end;
      end;
      if Seedface<>nil then
      begin
         Seedfaces.Add(SeedFace);
      end;
   end;

   // sort seedfaces
   for I:=1 to Seedfaces.Count-1 do
   begin
      SeedFace:=Seedfaces[I-1];
      SeedArea:=Seedface.Area;
      for J:=2 to SeedFaces.Count do
      begin
         Child:=Seedfaces[J-1];
         Area:=Child.Area;
         if Area<seedArea then
         begin
            Seedfaces.Exchange(I-1,J-1);
            SeedArea:=Area;
         end;
      end;
   end;
   if SeedFaces.Count>0 then
   begin
      MaxError:=1e10;
      BestIndex:=-1;
      I:=1;
      FNoIterations:=0;
      // Keep trying to develop the faces until no error has occured and the max. error<1e-7 and the number of iterations<=25
      while I<=seedfaces.count do
      begin
         Seedface:=Seedfaces[I-1];
         ProcessFaces(Seedface,Error,ErrorIndex);
         inc(FNoIterations);
         if (ErrorIndex<>-1) and (Seedfaces.Count<25) then
         begin
            // Add faces where an error occured as new seedfaces, these
            // are generally areas where gauss curvature<>0.0
            if Seedfaces.IndexOf(FDonelist[ErrorIndex])=-1 then Seedfaces.Add(FDonelist[ErrorIndex]);
         end;
         if Error<MaxError then
         begin
            MaxError:=Error;
            BestIndex:=I-1;
         end;
         inc(I);
      end;

      // Restore the best development
      if (BestIndex<>-1) and (BestIndex<>Seedfaces.Count-1) then
      begin
         Seedface:=Seedfaces[BestIndex];
         ProcessFaces(Seedface,Error,ErrorIndex);
      end;

      // Assemble all boundaryedges
      FBoundaryEdges.Clear;
      for I:=1 to FEdges.Count do
      begin
         Edge:=FEdges[I-1];
         // Only edges with 1 attached face in the ffaces list are valid
         N:=0;
         for J:=1 to Edge.NumberOfFaces do if FFaces.SortedIndexOf(Edge.Face[J-1])<>-1 then inc(N);
         if N=1 then FBoundaryEdges.Add(Edge);
      end;
      FBoundaryEdges.Capacity:=FBoundaryEdges.Count;

      // calculate min/max coordinates in 2D
      for I:=1 to FPoints.Count do
      begin
         if I=1 then
         begin
            FMin2D:=F2DCoordinates[I-1].Coordinate;
            FMax2D:=FMin2D;
         end else
         begin
            if F2DCoordinates[I-1].Coordinate.X<FMin2D.X then FMin2D.X:=F2DCoordinates[I-1].Coordinate.X;
            if F2DCoordinates[I-1].Coordinate.Y<FMin2D.Y then FMin2D.Y:=F2DCoordinates[I-1].Coordinate.Y;
            if F2DCoordinates[I-1].Coordinate.X>FMax2D.X then FMax2D.X:=F2DCoordinates[I-1].Coordinate.X;
            if F2DCoordinates[I-1].Coordinate.Y>FMax2D.Y then FMax2D.Y:=F2DCoordinates[I-1].Coordinate.Y;
         end;
      end;

      // finally find optimal rotation angle such that the
      // area of the bounding box is minimal
      OptArea:=0;
      OptAngle:=0;
      for I:=0 to 180 do
      begin
         Rotation:=I/2;
         Extents(Min,Max);
         Area:=(Max.X-Min.X)*(Max.Y-Min.Y);
         if I=0 then
         begin
            OptArea:=Area;
            OptAngle:=Rotation;
         end;
         if Area<OptArea then
         begin
            OptArea:=Area;
            OptAngle:=Rotation;
         end;
      end;
      Rotation:=OptAngle;
      Extents(Min,Max);
      if Max.X-Min.X<Max.Y-Min.Y then Rotation:=Rotation-90;
      if Owner.Symmetric then
      begin
         // Now check if the surface has one of its sides on the centerplane
         FMirror:=False;
         TmpEdges:=TFasterList.Create;
         for J:=1 to FEdges.Count do
         begin
            Edge:=FEdges[J-1];
            if (Edge.NumberOfFaces=1) and (abs(Edge.StartPoint.Coordinate.Y)<=1e-4) and (abs(Edge.EndPoint.Coordinate.Y)<=1e-4) then
            begin
               FMirror:=True;
               TmpEdges.Add(Edge);
            end;
         end;
         if TmpEdges.Count>0 then
         begin
            if FMirror then
            begin
               // Now if all facenormals have a Y-coordinate of approx. 0.0 then
               // this is probably a bottom panel, a deck or a transom
               for J:=1 to FDoneList.Count do
               begin
                  Face:=FDonelist[J-1];
                  Normal:=Face.FaceNormal;
                  if abs(Normal.Y)>1e-2 then
                  begin
                     FMirror:=False;
                     // Y-coordinate of Normal is too big, do not attach
                     Break;
                  end;
               end;
   
               if FMirror then
               begin
                  // Check if all points on the centerline are developed onto a 2Dline
                  // if so, then this line is used to mirror the other half of the layer
                  // so it forms 1 whole panel and there is no need to unfold it.
                  SortedEdges:=TFasterList.Create;
                  self.FOwner.Owner.IsolateEdges(TmpEdges,SortedEdges);
                  TmpEdges.Destroy;
                  TmpEdges:=nil;
                  for J:=SortedEdges.Count downto 1 do
                  begin
                     TmpEdges:=SortedEdges[J-1];
                     if J>1 then TmpEdges.Destroy;
                  end;
                  if TmpEdges<>nil then if TmpEdges.Count>0 then
                  begin
                     P1:=TmpEdges[0];
                     P2:=TmpEdges[TmpEdges.Count-1];
                     if P1=P2 then // closed loop, pick another point
                     begin
                        J:=TmpEdges.Count;
                        while (J>1) and (P1=P2) do
                        begin
                           P2:=TmpEdges[J-1];
                           Dec(J);
                        end;
                     end;
                     if P1<>P2 then
                     begin
                        BestIndex:=FPoints.SortedIndexOf(P1);
                        P3D1.X:=F2DCoordinates[BestIndex].Coordinate.X;
                        P3D1.Y:=F2DCoordinates[BestIndex].Coordinate.Y;
                        P3D1.Z:=0.0;
                        BestIndex:=FPoints.SortedIndexOf(P2);
                        P3D2.X:=F2DCoordinates[BestIndex].Coordinate.X;
                        P3D2.Y:=F2DCoordinates[BestIndex].Coordinate.Y;
                        P3D2.Z:=0.0;
                        for J:=2 to TmpEdges.Count-1 do
                        begin
                           P3:=TmpEdges[J-1];
                           BestIndex:=FPoints.SortedIndexOf(P3);
                           P3D3.X:=F2DCoordinates[BestIndex].Coordinate.X;
                           P3D3.Y:=F2DCoordinates[BestIndex].Coordinate.Y;
                           P3D3.Z:=0.0;
                           Dist:=DistancepointToLine(P3D1,P3D2,P3D3);
                           if Dist>1e-3 then
                           begin
                              FMirror:=False;
                              break;
                           end;
                        end;
                        if FMirror then
                        begin
                           J:=FPoints.SortedIndexOf(P1);
                           P3D1.X:=F2DCoordinates[J].Coordinate.X;
                           P3D1.Y:=F2DCoordinates[J].Coordinate.Y;
                           P3D1.Z:=0.0;
                           J:=FPoints.SortedIndexOf(P2);
                           P3D2.X:=F2DCoordinates[J].Coordinate.X;
                           P3D2.Y:=F2DCoordinates[J].Coordinate.Y;
                           P3D2.Z:=0.0;
                           P3D3:=P3D2;
                           P3D3.Z:=1.0;
                           FMirror:=True;
                           FMirrorPlane:=PlanePPP(P3D1,P3D2,P3D3);
   
                           // calculate min/max coordinates in 2D of the mirror part
                           for J:=1 to FPoints.Count do
                           begin
                              P3D2:=SetPoint(F2DCoordinates[J-1].Coordinate.X,F2DCoordinates[J-1].Coordinate.Y,0.0);
                              P3D1:=MirrorPlane(P3D2,FMirrorplane);
                              if P3D1.X<FMin2D.X then FMin2D.X:=P3D1.X else if P3D1.X>FMax2D.X then FMax2D.X:=P3D1.X;
                              if P3D1.Y<FMin2D.Y then FMin2D.Y:=P3D1.Y else if P3D1.Y>FMax2D.Y then FMax2D.Y:=P3D1.Y;
                           end;
                        end;
                     end;
                  end;
                  SortedEdges.Destroy;
               end;
            end;
         end;
         TmpEdges.Destroy;
      end;

      // Assemble cornerpoints for dimensioning
      for I:=1 to FPoints.Count do
      begin
         P1:=FPoints[I-1];
         N:=0;
         For J:=1 to P1.NumberOfFaces do if FFaces.SortedIndexOf(P1.Face[J-1])<>-1 then
         begin
            inc(N);
         end;
         if (N=1) or (P1.VertexType=svCorner) then
         begin
            FCorners.Add(P1);
         end;
      end;
   end else MessageDlg('Seed could not be found!',mtError,[mbOk],0);
   Seedfaces.Destroy;
   Seedfaces := nil;
   FFaces.Destroy;
   FFaces := nil;
end;{TFreeDevelopedPatch.Unroll}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeEntity                                             }
{                                                                                                   }
{ This is the base class of all 3D entities in the project                                          }
{---------------------------------------------------------------------------------------------------}
function TFreeEntity.FGetMin:T3DCoordinate;
begin
   if not Build then Rebuild;
   Result:=FMin;
end;{TFreeEntity.FGetMin}

function TFreeEntity.FGetMax:T3DCoordinate;
begin
   if not Build then Rebuild;
   Result:=FMax;
end;{TFreeEntity.FGetMax}

procedure TFreeEntity.FSetBuild(Val:Boolean);
begin
   if Val<>FBuild then
   begin
      FBuild:=Val;
      if not Val then
      begin
         FMin:=ZERO;
         FMAx:=ZERO;
      end;
   end;
end;{TFreeEntity.FSetBuild}

constructor TFreeEntity.Create;
// Create and initialise all data
begin
   inherited Create;
   //Clear;
   FBuild:=False;
   FMin:=ZERO;
   FMax:=ZERO; // The min/max boundary coordinates of the entity after it has been build
   FPenWidth:=1;        // Pen thickness to use when drawing
   FColor:=clBlack;     // Color when drawing
   FPenstyle:=psSolid;  // Pen style for drawing the line
end;{TFreeEntity.Create}

procedure TFreeEntity.Clear;
begin
   Build:=False;
   FMin:=ZERO;
   FMAx:=ZERO;
   FColor:=clBlack;
   FPenwidth:=1;
   FPenStyle:=psSolid;
end;{TFreeEntity.Clear}

destructor TFreeEntity.Destroy;
begin
   Inherited Destroy;
end;{TFreeEntity.Destroy}

procedure TFreeEntity.Extents(Var Min,Max : T3DCoordinate);
begin
   if not Build then Rebuild;
   MinMax(FMin,Min,Max);
   MinMax(FMax,Min,Max);
end;{TFreeEntity.Extents}

procedure TFreeEntity.Draw;
begin
end;{TFreeEntity.Draw}

procedure TFreeEntity.Rebuild;
begin
   FBuild := true;
end;{TFreeEntity.Rebuild}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeSpline                                             }
{                                                                                                   }
{ 3D CSpline                                                                                        }
{                                                                                                   }
{ Copied from page 107 of the book: "Numerical recipes in fortan 77"                                }
{ Url: http://www.library.cornell.edu/nr/bookfpdf/f3-3.pdf                                          }
{ Modified to use chordlength parametrisation for smoother interpolation and to accept              }
{ knuckles in the controlpoints                                                                     }
{---------------------------------------------------------------------------------------------------}
procedure TFreeSpline.FSetBuild(val:boolean);
begin
   if not val then
   begin
      Setlength(FDerivatives,0);
      Setlength(FParameters,0);
      // Clear extents
      FMin.X:=0;
      FMin.Y:=0;
      FMin.Z:=0;
      FMax.X:=1;
      FMax.Y:=1;
      Fmax.Z:=1;
      FTotalLength:=0.0;
   end;
   Inherited FSetBuild(Val);
end;{TFreeSpline.FSetBuild}

procedure TFreeSpline.FSetCapacity(Val:integer);
begin
   if Val<>FCapacity then
   begin
      FCapacity:=Val;
      Setlength(FPoints,FCapacity);
      { // MM: this logic may cause Build not set in case of decrease of Capacity.
        // MM: Replacing it with unconditional Build:=false;
      if FNoPoints>FCapacity then
      begin
         // Make sure that number of points does not exceed the capacity of the curve
         FNoPoints:=FCapacity;
         Build:=false;
      end;}
      Setlength(FKnuckles,FCapacity);
      Build:=false;
   end;
end;{TFreeSpline.FSetCapacity}

procedure TFreeSpline.FSetFragments(Val:Integer);
begin
   if Val<>FFragments then
   begin
      FFragments:=val;
      Build:=False;
   end;
end;{TFreeSpline.FSetFragments}

function TFreeSpline.FGetFragments:Integer;
begin
   if not Build then Rebuild;
   Result:=FFragments;
end;{TFreeSpline.FGetFragments}

function TFreeSpline.FGetKnuckle(Index:integer):Boolean;
begin
   if (Index>=0) and (Index<FNoPoints)
      then Result:=FKnuckles[Index]
      else Raise Exception.Create('List index out of bounds in '+ClassName+'.FGetKnuckle. ('+IntToStr(Index)+').');
end;{TFreeSpline.FGetKnuckle}

procedure TFreeSpline.FSetKnuckle(Index:integer;Value:Boolean);
begin
   if (Index>=0) and (Index<FNoPoints) then
   begin
      FKnuckles[Index]:=Value;
      Build:=false;
   end else Raise Exception.Create('List index out of bounds in '+ClassName+'.FSetKnuckle. ('+IntToStr(Index)+').');
end;{TFreeSpline.FSetKnuckle}

procedure TFreeSpline.FSetPoint(Index:Integer;P:T3DCoordinate);
begin
   if (Index>=0) and (Index<NumberOfPoints) then
   begin
      FPoints[index]:=P;
      Build:=False;
   end else Raise exception.Create('Point index out of bounds!');
end;{TFreeSpline.FSetPoint}

function TFreeSpline.FGetParameter(Index:integer):TFloatType;
begin
   if (Index>=0) and (Index<FNoPoints) then
   begin
      if not build then rebuild;
      Result:=FParameters[Index];
   end else Raise Exception.Create('List index out of bounds in '+ClassName+'.FGetParameter. ('+IntToStr(Index)+').');
end;{TFreeSpline.FGetParameter}

function TFreeSpline.FGetPoint(Index:Integer):T3DCoordinate;
begin
   if (Index>=0) and (Index<NumberOfPoints)
      then Result:=FPoints[index]
      else Raise exception.Create('Point index out of bounds!');
end;{TFreeSpline.FGetPoint}

procedure TFreeSpline.Rebuild;
var I,K,K1,I1,I2  : integer;
    Len     : TFloatType;
    Sig,P      : TFloatType;
    U          : TFreeCoordinateArray;
    Un,Qn      : T3DCoordinate;
    DPar1,DPar2,DPar3: single;   
begin
   Build:=False;

   // First attempt to eliminate double points
   I:=2;
   FTotalLength:=0;
   while I<=FNoPoints do
   begin
      Len:=Sqrt(DistPP3D(FPoints[I-2],FPoints[I-1]));
      FTotalLength:=FTotalLength+Len;
      Inc(I);
   end;

   if FNoPoints>1 then
   begin
      Setlength(FDerivatives,FNoPoints);
      Setlength(FParameters,FNoPoints);
      SetLength(U,FNoPoints);

      Len:=0;
      if abs(FTotalLength)<1e-5 then
      begin
         // zero arclength, use uniform parameterisation
         for I:=0 to FNoPoints-1 do FParameters[I]:=I/(FNoPoints-1);
      end else
      begin
         FParameters[0]:=0.0;
         for I:=2 to FNoPoints-1 do
         begin
            Len:=Len+Sqrt(DistPP3D(FPoints[I-2],FPoints[I-1]));
            FParameters[I-1]:=Len/FTotalLength;
         end;
         FParameters[FNoPoints-1]:=1.0;
      end;

      FDerivatives[0].X:=0;
      FDerivatives[0].Y:=0;
      FDerivatives[0].Z:=0;
      U[0]:=FDerivatives[0];

      for I:=2 to FNoPoints-1 do
      begin
         I1:=I-1;
         I2:=I-2;
         if Knuckle[I1] then
         begin
            U[I1].X:=0;
            U[I1].Y:=0;
            U[I1].Z:=0;
            FDerivatives[I1].X:=0;
            FDerivatives[I1].Y:=0;
            FDerivatives[I1].Z:=0;
         end else
         begin
            DPar1:=FParameters[I]-FParameters[I2];
            DPar2:=FParameters[I1]-FParameters[I2];
            DPar3:=FParameters[I]-FParameters[I1];
            if (abs(DPar1)<1e-5) or (abs(DPar2)<1e-5) or (abs(DPar3)<1e-5) then
            begin
               FDerivatives[I1]:=ZERO;
            end else
            begin
               Sig:=DPar2/DPar1;
               // first x-value
               P:=Sig*FDerivatives[I2].X+2.0;
               FDerivatives[I1].X:=(Sig-1.0)/P;
               U[I1].X:=(6.0*((FPoints[I].X-FPoints[I1].X)/DPar3-(FPoints[I1].X-FPoints[I2].X)/DPar2)/DPar1-sig*U[I2].X)/p;
               // then y-value			   
               P:=Sig*FDerivatives[I2].Y+2.0;
               FDerivatives[I1].Y:=(Sig-1.0)/P;
               U[I1].Y:=(6.0*((FPoints[I].Y-FPoints[I1].Y)/DPar3-(FPoints[I1].Y-FPoints[I2].Y)/DPar2)/DPar1-sig*U[I2].Y)/p;
               // then Z-value
               P:=Sig*FDerivatives[I2].Z+2.0;
               FDerivatives[I1].Z:=(Sig-1.0)/P;
               U[I1].Z:=(6.0*((FPoints[I].Z-FPoints[I1].Z)/DPar3-(FPoints[I1].Z-FPoints[I2].Z)/DPar2)/DPar1-sig*U[I2].Z)/p;
            end;
         end;
      end;

      Qn.X:=0.0;
      Qn.Y:=0.0;
      Qn.Z:=0.0;
      Un:=Qn;

      FDerivatives[FNoPoints-1].X:=(Un.X-Qn.X*U[FNoPoints-2].X)/(Qn.X*FDerivatives[FNoPoints-2].X+1.0);
      FDerivatives[FNoPoints-1].Y:=(Un.Y-Qn.Y*U[FNoPoints-2].Y)/(Qn.Y*FDerivatives[FNoPoints-2].Y+1.0);
      FDerivatives[FNoPoints-1].Z:=(Un.Z-Qn.Z*U[FNoPoints-2].Z)/(Qn.Z*FDerivatives[FNoPoints-2].Z+1.0);

      // Back substitution
      for K:=FNoPoints-1 downto 1 do
      begin
         K1:=K-1; 
         FDerivatives[K1].X:=FDerivatives[K1].X*FDerivatives[K].X+U[K1].X;
         FDerivatives[K1].Y:=FDerivatives[K1].Y*FDerivatives[K].Y+U[K1].Y;
         FDerivatives[K1].Z:=FDerivatives[K1].Z*FDerivatives[K].Z+U[K1].Z;
      end;
   end;
   // MM: Suspicious setting of FBuild before inherited Rebuild.
   // MM: FBuild:=true; is moved to TFreeEntity.Rebuild;
   //FBuild:=true;

   // Determine min/max values
   if FNoPoints>0 then
   begin
      for I:=1 to FNoPoints do
      begin
         if I=1 then
         begin
            FMin:=FPoints[I-1];
            FMax:=FMin;
         end else MinMax(FPoints[I-1],FMin,FMax);
      end;
   end;
   FFragments := FNoPoints * 1; //MM: just set
   inherited Rebuild;
end;{TFreeSpline.Rebuild}

function TFreeSpline.SecondDerive(Parameter:TFloatType):T3DCoordinate;
var Lo,Hi,K : integer;
    Frac    : TFloatType;
begin
   Result.X:=0;
   Result.Y:=0;
   Result.Z:=0;
   if FNoPoints<2 then exit;
   if not FBuild then Rebuild;
   if FNoPoints<2 then exit;
   if FNoPoints=2 then
   begin
      Lo:=0;
      Hi:=1;
   end else
   begin
      Lo:=0;
      Hi:=FNoPoints-1;
      repeat
         K:=(Lo+Hi) div 2;
         try
            if FParameters[K]<Parameter then Lo:=K
                                        else Hi:=K;
         except
            FParameters[K]:=FParameters[K]-1+1;
         end;
      until Hi-Lo<=1;
   end;
   if FParameters[Hi]-FParameters[Lo]<=0.0 then Frac:=0.5
                                           else Frac:=(Parameter-FParameters[Lo])/(FParameters[Hi]-FParameters[Lo]);
   Result.X:=FDerivatives[Lo].X+Frac*(FDerivatives[Hi].X-FDerivatives[Lo].X);
   Result.Y:=FDerivatives[Lo].Y+Frac*(FDerivatives[Hi].Y-FDerivatives[Lo].Y);
   Result.Z:=FDerivatives[Lo].Z+Frac*(FDerivatives[Hi].Z-FDerivatives[Lo].Z);
end;{TFreeSpline.SecondDerive}

// Remove points that do not contribute significantly to the shape
function TFreeSpline.Simplify(Criterium:TFloatType):Boolean;
var Weights    : array of TFloatType;
    TotalLength: TFloatType;
    I,Index    : Integer;
    N1,N2:Integer;

   Function Weight(Index:Integer):TFloatType;
   var P1,P2,P3   : T3DCoordinate;
       Length     : TFloatType;
       Dist       : TFloatType;
   begin
      if (Index=0) or (Index=NumberOfPoints-1) or (Knuckle[Index]) then Result:=1e10 else
      begin
         P1:=Point[Index-1];
         P2:=Point[Index];
         P3:=Point[Index+1];
         Length:=Sqrt((P3.X-P1.X)*(P3.X-P1.X)+(P3.Y-P1.Y)*(P3.Y-P1.Y)+(P3.Z-P1.Z)*(P3.Z-P1.Z));
         if Length<1e-5 then
         begin
            Result:=0.0;
         end else
         begin
            Dist:=DistancepointToLine(P2,P1,P3);
            if Dist<1e-2 then
            begin
               if Length*Length/TotalLength>0.01
                  then Result:=1e10
                  else Result:=1e8*Dist*Dist*Length;
            end else Result:=1e8*Dist*Dist*Length;
         end;
      end;
   end;{Weight}

   Function FindNextPoint:integer;
   var MinVal:TFloatType;
       I:Integer;
   begin
      Result:=-1;
      if NumberOfPoints<3 then exit;
      MinVal:=Weights[1];
      Result:=1;
      I:=2;
      While (I<NumberOfPoints) and (MinVal>0) do
      begin
         if Weights[I-1]<MinVal then
         begin
            MinVal:=Weights[I-1];
            Result:=I-1;
         end;
         Inc(I);
      end;
   end;{FindNextPoint}

begin
   Result:=False;
   if NumberOfPoints<3 then
   begin
      Result:=True;
      exit;
   end;

   N1:=0;
   N2:=0;
   for I:=1 to numberofpoints do if Knuckle[I-1] then inc(N1);

   TotalLength:=FTotalLength*FTotalLength;
   if TotalLength=0 then exit;
   SetLength(Weights,NumberOfPoints);
   for I:=1 to NumberOfPoints do Weights[I-1]:=Weight(I-1)/TotalLength;
   try
      repeat
         Index:=FindNextPoint;
         if Index<>-1 then
         begin
            if (Index=0) or (Index=FNoPoints-1) or (FNoPoints<3) then
            begin
               Index:=-1;
            end else
            begin
               if Weights[Index]<Criterium then
               begin
                  Move(Weights[Index+1],Weights[Index],(FNoPoints-Index-1)*SizeOf(TFloatType));
                  Move(FPoints[Index+1],FPoints[Index],(FNoPoints-Index-1)*SizeOf(T3DCoordinate));
                  Move(FKnuckles[Index+1],FKnuckles[Index],(FNoPoints-Index-1));
                  Dec(FNoPoints);
                  if (Index-1>=0) and (Index-1<FNoPoints) then Weights[Index-1]:=Weight(Index-1)/TotalLength;
                  if (Index>=0) and (Index<FNoPoints) then     Weights[Index]:=Weight(Index)/TotalLength;
                  if (Index+1>=0) and (Index+1<FNoPoints) then Weights[Index+1]:=Weight(Index+1)/TotalLength;
               end else Index:=-1;
            end;
         end;
      until index=-1;
      Result:=True;
   except
      Result:=False;
   end;

   // MM: Useless code? Commenting out.
   {
   for I:=1 to numberofpoints do
      if Knuckle[I-1]
         then inc(N2);
   if N1<>N2 then
   begin
      Build:=false;
   end;
   }

   Capacity:=NumberOfPoints;
   Fragments:=NumberOfPoints-1;
   Build:=False;
end;{TFreeSpline.Simplify}

procedure TFreeSpline.Add(P:T3DCoordinate);
begin
   if NumberOfPoints=Capacity then
   begin
      // Make sure that the allocated memory is sufficient
      Capacity:=capacity+IncrementSize;
   end;
   FPoints[FNoPoints]:=P;
   FKnuckles[FNoPoints]:=False;
   inc(FNoPoints);
   FFragments:=FNoPoints-1;
   Build:=False;  // Curve needs to be rebuild
end;{TFreeSpline.Add}

// Copy all data from another spline
procedure TFreeSpline.Assign(Spline:TFreeSpline);
begin
   FMin:=Spline.FMin;
   FMax:=Spline.FMax;
   FPenWidth:=Spline.FPenwidth;
   FColor:=Spline.FColor;
   FPenstyle:=Spline.FPenStyle;
   Capacity:=Spline.NumberOfPoints;
   // Copy controlpoints
   Move(Spline.FPoints[0],FPoints[0],Spline.NumberOfPoints*SizeOf(T3DCoordinate));
   // copy knuckles
   Move(Spline.FKnuckles[0],FKnuckles[0],Spline.NumberOfPoints*SizeOf(Boolean));
   FNoPoints:=Spline.NumberOfPoints;
   FFragments:=FNoPoints-1;
   FShowCurvature:=Spline.ShowCurvature;
   FCurvatureScale:=Spline.FCurvatureScale;
   FCurvatureColor:=Spline.FCurvatureColor;
   Build:=False;
end;{TFreeSpline.Assign}

function TFreeSpline.CoordLength(T1,T2:TFloatType):TFloatType;
var I : Integer;
    T : TFloatType;
    P1,P2 : T3DCoordinate;
begin
   Result:=0.0;
   P1.X := 0;   P1.Y := 0;   P1.Z := 0;
   if not build then Rebuild;
   if not Build then exit;
   For I:=0 to Fragments do
   begin
      T:=T1+(I/Fragments)*(T2-T1);
      P2:=Value(T);
      if I>0 then Result:=Result+Sqrt(Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y)+Sqr(P2.Z-P1.Z));
      P1:=P2;
   end;
end;{TFreeSpline.CoordLength}

function TFreeSpline.ChordlengthApproximation(Percentage:TFloatType):extended;
var Totallength  : TFloatType;
    Desiredlength: TFloatType;
    Parameter    : TFloatType;
    Length       : TFloatType;
    T1,T2        : TFloatType;
    L1,L2        : TFloatType;
    Counter      : integer;
begin
   T1:=0;
   T2:=1;
   Parameter:=1.0;
   Counter:=0;
   L1:=0;
   L2:=0;
   DesiredLength:=0;
   if Percentage<0.0 then Result:=0.0 else
      if Percentage>1.0 then Result:=1.0 else
   begin
      repeat
         Length:=CoordLength(0,Parameter);
         if counter=0 then
         begin
            L1:=0;
            L2:=Length;
            TotalLength:=Length;
            DesiredLength:=Percentage*Totallength;
            Parameter:=Percentage;
         end else
         begin
            if Length>Desiredlength then
            begin
               T2:=Parameter;
               L2:=Length;
            end else
            begin
               T1:=Parameter;
               L1:=Length;
            end;
            Parameter:=T1+((DesiredLength-L1)/(L2-L1))*(T2-T1);
            if Parameter<0 then Parameter:=0;
            if Parameter>1 then Parameter:=1;
         end;
         Inc(Counter);
      until(Counter>75) or (abs(Length-DesiredLength)<1e-3);
      Result:=Parameter;
   end;
end;{TFreeSpline.ChordlengthApproximation}

constructor TFreeSpline.Create;
begin
  inherited Create;
   Setlength(FPoints,0);
   Setlength(FDerivatives,0);
   Setlength(FParameters,0);
   Setlength(FKnuckles,0);
   FCapacity:=0;
   FNoPoints:=0;
end;{TFreeSpline.Create}

function TFreeSpline.Curvature(Parameter:TFloatType;var Value,Normal:T3DCoordinate):TFloatType;
var Vel1,Acc      : T3DCoordinate;
    CrossProduct  : T3DCoordinate;
    L,Denom       : TFloatType;
    VdotA,VdotV   : TFloatType;
begin
   Value:=self.Value(Parameter);
   Vel1:=FirstDerive(Parameter);
   Acc:=SecondDerive(Parameter);
   // Crossproduct of first and second derive
   CrossProduct.X:=(Acc.Y*(Vel1.Z)-(Acc.Z)*Vel1.Y);
   CrossProduct.Y:=-(Acc.X*(Vel1.Z)-(Acc.Z)*Vel1.X);
   CrossProduct.Z:=(Acc.X*Vel1.Y-Acc.Y*Vel1.X);
   L:=Sqrt((CrossProduct.X*CrossProduct.X)+(CrossProduct.Y*CrossProduct.Y)+(CrossProduct.Z*CrossProduct.Z));
   if L=0 then L:=0.00001;
   VdotA:=(Vel1.X*Acc.X)+(Vel1.Y*Acc.Y)+(Vel1.Z*Acc.Z);
   VdotV:=(Vel1.X*Vel1.X)+(Vel1.Y*Vel1.Y)+(Vel1.Z*Vel1.Z);
	Denom:=Power(VdotV,1.5);
	if Denom>0 then Result:=L/Denom
              else Result:=0;
   Normal.X:=vdotv*acc.X-vdota*vel1.X;
	Normal.Y:=vdotv*acc.Y-vdota*vel1.Y;
	Normal.Z:=vdotv*acc.Z-vdota*vel1.Z;
   Normal:=Normalize(Normal);
end;{TFreeSpline.Curvature}

procedure TFreeSpline.DeletePoint(Index:Integer);
var I : integer;
begin
   if NumberOfPoints>0 then
   begin
      dec(FNoPoints);
      FFragments:=FNoPoints-1;
      for I:=Index to NumberOfPoints-1 do
      begin
         FPoints[I]:=FPoints[I+1];
         FKnuckles[I]:=FKnuckles[I+1];
      end;
      Build:=false;
   end;
end;{TFreeSpline.DeletePoint}

function TFreeSpline.DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
var I,Tmp    : Integer;
    Pt,P1,P2 : TPoint;
    V1,V2,P  : T3DCoordinate;
    Param    : TFloatType;
begin
   Result:=1000000;
   // Check if cursor position lies within the boundaries
   Pt.X:=X;
   Pt.Y:=Y;
   if (Pt.X>=0) and (Pt.X<=Viewport.Width) and (Pt.Y>=0) and (Pt.Y<=Viewport.Height) then
   begin
      P1:=Viewport.Project(value(0.0));
      P:=Value(0);
      V1:=P;
      for I:=1 to Fragments do
      begin
         V2:=Value((I-1)/(Fragments-1));
         P2:=Viewport.Project(V2);
         Tmp:=Round(DistanceToLine(P1,P2,X,Y,Param));
         if Tmp<Result then
         begin
            result:=Tmp;
            P:=Interpolate(V1,V2,Param);
         end;
         P1:=P2;
         V1:=V2;
      end;
   end;
end;{TFreeSpline.DistanceToCursor}

function TFreeSpline.FirstDerive(Parameter:TFloatType):T3DCoordinate;
var P1,P2:T3DCoordinate;
    T1,T2:TFloatType;
begin
   T1:=Parameter-1e-3;
   T2:=Parameter+1e-3;
   if T1<0.0 then T1:=0.0;
   if T2>1.0 then T2:=1.0;
   P1:=Value(T1);
   P2:=Value(T2);
   Result.X:=(P2.X-P1.X)/(T2-T1);
   Result.Y:=(P2.Y-P1.Y)/(T2-T1);
   Result.Z:=(P2.Z-P1.Z)/(T2-T1);
end;{TFreeSpline.FirstDerive}

procedure TFreeSpline.Insert(Index:Integer;P:T3DCoordinate);
var I : integer;
begin
   if (Index>=0) and (Index<NumberOfPoints) then
   begin
      if NumberOfPoints=Capacity then Capacity:=Capacity+IncrementSize;
      for I:=NumberOfPoints-1 downto Index do
      begin
         FPoints[I+1]:=FPoints[I];
         FKnuckles[I+1]:=FKnuckles[I];
      end;
      FPoints[Index]:=P;
      FKnuckles[Index]:=False;
      inc(FNoPoints);
      FFragments:=FNoPoints-1;
      Build:=false;
   end else raise Exception.Create('Index out of range'+EOL+IntToStr(Index)+#32+IntToStr(FNoPoints));
end;{TFreeSpline.Insert}

procedure TFreeSpline.Draw(Viewport:TFreeViewport);
var I       : Integer;
    P1,P2   : T3DCoordinate;
    PArray1 : array of TPoint;
    PArray2 : array of TPoint;
    Pt      : TPoint;
    C       : TFloatType;
    Normal  : T3DCoordinate;
    R,G,B   : Integer;
begin
   if not Build then Rebuild;
   if Viewport.ViewportMode=vmWireFrame then
   begin
      if ShowCurvature then
      begin
         SetLength(PArray1,Fragments);
         SetLength(PArray2,Fragments);
         for I:=1 to Fragments do
         begin
            C:=Curvature((I-1)/(Fragments-1),P1,Normal);
            PArray1[I-1]:=Viewport.Project(P1);
            P2.X:=P1.X-C*2*CurvatureScale*Normal.X;
            P2.Y:=P1.Y-C*2*CurvatureScale*Normal.Y;
            P2.Z:=P1.Z-C*2*CurvatureScale*Normal.Z;
            PArray2[I-1]:=Viewport.Project(P2);
         end;
         Viewport.SetPenWidth(1);
         Viewport.PenColor:=CurvatureColor;
         for I:=1 to Fragments do if (I mod 4=0) or (I=1) or (I=Fragments) then
         begin
            Viewport.MoveTo(PArray1[I-1].X,PArray1[I-1].Y);
            Viewport.LineTo(PArray2[I-1].X,PArray2[I-1].Y);
         end;
         Viewport.Polyline(PArray2);
      end else
      begin
         SetLength(PArray1,Fragments);
         for I:=1 to Fragments do
         begin
            P1:=Value((I-1)/(Fragments-1));
            PArray1[I-1]:=Viewport.Project(P1);
         end;
      end;
      Viewport.SetPenWidth(1);
      Viewport.PenColor:=Color;
      Viewport.PenStyle:=FPenstyle;
      Viewport.Polyline(PArray1);
      if ShowPoints then
      begin
         Viewport.Fontname:='small fonts';
         Viewport.FontSize:=7;
         Viewport.FontColor:=clBlack;
         Viewport.BrushStyle:=bsClear;
         for I:=1 to NumberOfPoints do
         begin
            Pt:=Viewport.Project(Point[I-1]);
            Viewport.Ellipse(Pt.X-2,Pt.Y-2,Pt.X+2,Pt.Y+2);
            Viewport.TextOut(Pt.X+2,Pt.Y,IntToStr(I));
         end;
      end;
   end else
   begin
      // draw to z-buffer
      if not Build then Rebuild;
      R:=GetRValue(Color);
      G:=GetGValue(Color);
      B:=GetBValue(Color);
      P1:=Value(0.0);
      for I:=1 to Fragments do
      begin
         P2:=Value(I/Fragments);
         Viewport.DrawLineToZBuffer(P1,P2,R,G,B);
         P1:=P2;
      end;
   end;
end;{TFreeSpline.Draw}

procedure TFreeSpline.InsertSpline(Index:Integer;Invert,DuplicatePoint:Boolean;Source:TFreeSpline);
var I:Integer;
    NoNewPoints:Integer;
begin
   if NumberOfPoints=0 then
   begin
      Capacity:=Source.NumberOfPoints;
      if Invert then
      begin
         for I:=0 to Source.NumberOfPoints-1 do
         begin
            FPoints[Source.NumberOfPoints-1-I]:=Source.FPoints[I];
            FKnuckles[Source.NumberOfPoints-1-I]:=Source.FKnuckles[I];
         end;
      end else
      begin
         for I:=0 to Source.NumberOfPoints-1 do
         begin
            FPoints[I]:=Source.FPoints[I];
            FKnuckles[I]:=Source.FKnuckles[I];
         end;
      end;
      FNoPoints:=Source.NumberOfpoints;
      FFragments:=FNoPoints-1;
      Build := False;
   end else
   begin
      if DuplicatePoint then NoNewPoints:=Source.NumberOfPoints-1
                        else NoNewPoints:=Source.NumberOfPoints;
      Capacity:=NumberOfPoints+NoNewPoints;
      Build:=False;
      if Index<NumberOfPoints then
      begin
         // insert space for new points
         Move(FPoints[Index],FPoints[Index+NoNewPoints],(NumberOfPoints-Index)*SizeOf(T3DCoordinate));
         Move(FKnuckles[Index],FKnuckles[Index+NoNewPoints],(NumberOfPoints-Index));
         // Insert the new data
         if Invert then
         begin
            Knuckle[index]:=Knuckle[Index] or Source.Knuckle[Source.NumberOfPoints-1];
            for I:=1 to NoNewPoints do
            begin
               FPoints[Index+I-1]:=Source.FPoints[Source.FNoPoints-I];
               FKnuckles[Index+I-1]:=Source.FKnuckles[Source.FNoPoints-I];
            end;
         end else
         begin
            Knuckle[index]:=Knuckle[Index] or Source.Knuckle[0];
            Move(Source.FPoints[0],FPoints[Index],NoNewPoints*SizeOf(T3DCoordinate));
            Move(Source.FKnuckles[0],FKnuckles[Index],NoNewPoints);
         end;
      end else
      begin
         if Invert then
         begin
            Knuckle[NumberOfPoints-1]:=Knuckle[NumberOfPoints-1] or Source.Knuckle[Source.NumberOfPoints-1];
            for I:=1 to NoNewPoints do
            begin
               FPoints[FNoPoints-1+I]:=Source.FPoints[Source.FNoPoints-I-1];
               FKnuckles[FNoPoints-1+I]:=Source.FKnuckles[Source.FNoPoints-I-1];
            end;
         end else
         begin
            Knuckle[NumberOfPoints-1]:=Knuckle[NumberOfPoints-1] or Source.Knuckle[0];
            if DuplicatePoint then
            begin
               // Add controlpoints
               Move(Source.FPoints[1],FPoints[NumberOfPoints],NoNewPoints*SizeOf(T3DCoordinate));
               // Add knuckles
               Move(Source.FKnuckles[1],FKnuckles[NumberOfPoints],NoNewPoints);
            end else
            begin
               // Add controlpoints
               Move(Source.FPoints[0],FPoints[NumberOfPoints],NoNewPoints*SizeOf(T3DCoordinate));
               // Add knuckles
               Move(Source.FKnuckles[0],FKnuckles[NumberOfPoints],NoNewPoints);
            end;
         end;
      end;
      inc(FNoPoints,NoNewPoints);
      FFragments:=FNoPoints-1;
      if not DuplicatePoint then Fknuckles[index]:=True;
      Build := False;
   end;
end;{TFreeSpline.InsertSpline}

function TFreeSpline.IntersectPlane(Plane:T3DPlane;var Output:TFreeIntersectionData):Boolean;
var Capacity   : Integer;
    I          : Integer;
    P1,P2      : T3DCoordinate;
    S1,S2      : TFloatType;
    T1,T2,T    : TFloatType;

    procedure AddToOutput(P:T3DCoordinate;Parameter:TFloatType);
    begin
       if Output.NumberOfIntersections=Capacity then
       begin
          inc(Capacity,10);
          Setlength(Output.Points,Capacity);
          Setlength(Output.Parameters,Capacity);
       end;
       Output.Points[Output.NumberOfIntersections]:=P;
       Output.Parameters[Output.NumberOfIntersections]:=Parameter;
       inc(Output.NumberOfIntersections);
    end;{AddToOutput}

begin
   Capacity:=0;
   Output.NumberOfIntersections:=0;
   Setlength(Output.Points,Capacity);
   Setlength(Output.Parameters,Capacity);
   T1:=0.0;
   P1:=value(T1);
   S1:=Plane.a*P1.x+Plane.b*P1.y+Plane.c*P1.z+Plane.d;
   if abs(S1)<1e-6 then AddToOutput(P1,T1);
   for I:=1 to Fragments do
   begin
      T2:=I/Fragments;
      P2:=Value(T2);
      S2:=Plane.a*P2.x+Plane.b*P2.y+Plane.c*P2.z+Plane.d;
      if abs(S2)<1e-6 then AddToOutput(P2,T2);
      if ((S1<0.0) and (S2>0.0)) or ((S2<0.0) and (S1>0.0)) then
      begin
         // intersection found
         T:=-s1/(s2-s1);
         T:=T1+T*(T2-T1);
         AddToOutput(Value(T),T);
      end;
      P1:=P2;
      S1:=S2;
      T1:=T2;
   end;
   Result:=Output.NumberOfIntersections>0;
end;{TFreeSpline.IntersectPlane}

// Invert the direction of controlpoints and knuckles
procedure TFreeSpline.InvertDirection;
var Mid : Integer;
    I   : Integer;
    P   : T3DCoordinate;
    K   : Boolean;
begin
   Mid:=(FNoPoints div 2) - 1;
   for I:=0 to Mid do
   begin
      P:=FPoints[I];
      FPoints[I]:=FPoints[FNoPoints-I-1];
      FPoints[FNoPoints-I-1]:=P;
      K:=FKnuckles[I];
      FKnuckles[I]:=FKnuckles[FNoPoints-I-1];
      FKnuckles[FNoPoints-I-1]:=K;
   end;
   Build:=False;
end;{TFreeSpline.InvertDirection}

procedure TFreeSpline.LoadBinary(Source:TFreeFileBuffer);
var I,N  : Integer;
    P    : T3DCoordinate;
    K    : Boolean;
begin
   Source.Load(FShowCurvature);
   Source.Load(FCurvatureScale);
   Source.Load(N);
   Capacity:=N;
   for I:=1 to N do
   begin
      Source.Load(P);
      Add(P);
      Source.Load(K);
      Knuckle[I-1]:=K;
   end;
end;{TFreeSpline.LoadBinary}

procedure TFreeSpline.SaveBinary(Destination:TFreeFileBuffer);
var I:Integer;
begin
   Destination.Add(FShowCurvature);
   Destination.Add(FCurvatureScale);
   Destination.Add(NumberOfPoints);
   for I:=1 to NumberOfPoints do
   begin
      Destination.Add(Point[I-1]);
      Destination.Add(Knuckle[I-1]);
   end;
end;{TFreeSpline.SaveBinary}

procedure TFreeSpline.SaveToDXF(Strings:TStringList;Layername:string;SendMirror:Boolean);
var P       : T3DCoordinate;
    I,J,Ind : integer;
    NParams : Integer;
    Params  : TFloatArray;
begin
  Ind:=FindDXFColorIndex(Color);
  NParams:=0;
  Setlength(Params,NumberOfPoints);
  // count number of knucklepoints
  if not Build then Rebuild;
  for I:=2 to NumberOfPoints-1 do
  begin
     if Knuckle[I-1] then
     begin
        Params[NParams]:=Parameter[I-1];
        inc(NParams);
     end;
  end;
  Setlength(Params,NParams+Fragments);
  for I:=1 to Fragments do
  begin
     Params[NParams]:=(I-1)/(Fragments-1);
     inc(NParams);
  end;
  SortFloatArray(Params,NParams);
  Strings.Add('0'+EOL+'POLYLINE');
  Strings.Add('8'+EOL+LayerName);   // layername
  Strings.Add('62'+EOL+IntToStr(Ind));  // color by layer
  Strings.Add('70'+EOL+'10');   // not closed
  Strings.Add('66'+EOL+'1');    // vertices follow
  for J:=1 to NParams do
  begin
     P:=Value(Params[J-1]);
     Strings.Add('0'+EOL+'VERTEX');
     Strings.Add('8'+EOL+LayerName);
     Strings.Add('10'+EOL+Truncate(P.X,4));
     Strings.Add('20'+EOL+Truncate(P.Y,4));
     Strings.Add('30'+EOL+Truncate(P.Z,4));
     Strings.Add('70'+EOL+'32');    // 3D polyline mesh vertex
  end;
  Strings.Add('0'+EOL+'SEQEND');
  if SendMirror then
  begin
     // send starboard side of the ship also
     Strings.Add('0'+EOL+'POLYLINE');
     Strings.Add('8'+EOL+LayerName);   // layername
     Strings.Add('62'+EOL+IntToStr(Ind));  // color by layer
     Strings.Add('70'+EOL+'10');   // not closed
     Strings.Add('66'+EOL+'1');    // vertices follow
     for J:=0 to NParams do
     begin
        P:=Value(Params[J-1]);
        P.Y:=-P.Y;
        Strings.Add('0'+EOL+'VERTEX');
        Strings.Add('8'+EOL+LayerName);
        Strings.Add('10'+EOL+Truncate(P.X,4));
        Strings.Add('20'+EOL+Truncate(P.Y,4));
        Strings.Add('30'+EOL+Truncate(P.Z,4));
        Strings.Add('70'+EOL+'32');    // 3D polyline mesh vertex
     end;
     Strings.Add('0'+EOL+'SEQEND');
  end;
end;{TFreeSpline.SaveToDXF}

procedure TFreeSpline.Clear;
begin
   Setlength(FDerivatives,0);
   Setlength(FParameters,0);
   FTotalLength:=0.0;
   FNoPoints:=0;
   inherited Clear;
   Build:=False;
   Setlength(FPoints,0);
   FCapacity:=0;
   FNoPoints:=0;
   FFragments:=0;
   FShowCurvature:=false;
   FCurvatureScale:=0.10;
   FCurvatureColor:=clFuchsia;
   FShowPoints:=False;
end;{TFreeSpline.Clear}

function TFreeSpline.Value(Parameter:extended):T3DCoordinate;
var Lo,Hi   : integer;
    K       : Integer;
    H,a,b,a3,b3,h2   : TFloatType;
begin
   Result.X:=0;
   Result.Y:=0;
   Result.Z:=0;
   if FNoPoints<2 then exit;
   if not FBuild
      then Rebuild;
   if (length(FParameters)<FNoPoints)
         then Rebuild;
   if (length(FDerivatives)<FNoPoints)
         then Rebuild;

   if FNoPoints<2 then exit;
   if FNoPoints=2 then
   begin
      Lo:=0;
      Hi:=1;
   end else
   begin
      if (length(FParameters)<FNoPoints)
            then Rebuild;
      Lo:=0;
      Hi:=FNoPoints-1;
      repeat
         K:=(Lo+Hi) div 2;
         try
            if FParameters[K]<Parameter then Lo:=K
                                         else Hi:=K;
         except
            FParameters[K]:=FParameters[K]-1+1;
         end;
         if (length(FParameters)<FNoPoints)
               then Rebuild;
      until Hi-Lo<=1;
   end;

   // MM: debug
   if (Hi>(FNoPoints-1)) or (Hi < 0)
      then raise Exception.Create('Hi is out of range'+EOL+IntToStr(Hi)+' out of 0:'+IntToStr(FNoPoints));
   if (Lo>(FNoPoints-1)) or (Hi < 0)
      then raise Exception.Create('Lo is out of range'+EOL+IntToStr(Hi)+' out of 0:'+IntToStr(FNoPoints));
   if (length(FParameters)<FNoPoints)
     then raise Exception.Create('length(FParameters)<FNoPoints'+EOL+IntToStr(length(FParameters))
                                     +' '+IntToStr(FNoPoints));
   if (length(FDerivatives)<FNoPoints)
     then raise Exception.Create('length(FDerivatives)<FNoPoints'+EOL+IntToStr(length(FParameters))
                                  +' '+IntToStr(FNoPoints));
   // MM: end debug

   H:=FParameters[Hi]-FParameters[Lo];

   if abs(H)<1e-6 then
   begin
      //Raise exception.Create('Invalid cspline');
      Result:=FPoints[Hi];
   end else
   begin
      a:=(FParameters[Hi]-Parameter)/H;
      b:=(Parameter-FParameters[Lo])/H;
      a3:=a*a*a-a; 
      b3:=b*b*b-b; 
      h2:=h*h/6.0;
      Result.X:=A*FPoints[Lo].X+B*FPoints[Hi].X+(a3*FDerivatives[Lo].X+b3*(Fderivatives[Hi].X))*h2;
      Result.Y:=A*FPoints[Lo].Y+B*FPoints[Hi].Y+(a3*FDerivatives[Lo].Y+b3*(Fderivatives[Hi].Y))*h2;
      Result.Z:=A*FPoints[Lo].Z+B*FPoints[Hi].Z+(a3*FDerivatives[Lo].Z+b3*(Fderivatives[Hi].Z))*h2;
   end;
end;{TFreeSpline.Value}

{--------------------------------------------------------------------------------------------------}
{                                           TFreeNURBSurface                                         }
{--------------------------------------------------------------------------------------------------}
procedure TFreeNURBSurface.SetCapacity(Col,Row:integer);
var I  : Integer;
begin
   Setlength(FControlPoints,Row);
   for I:=1 to Row do
   begin
      Setlength(FControlPoints[I-1],Col);
   end;
   FColCapacity:=Col;
   FRowCapacity:=Row;
end;{TFreeNURBSurface.SetCapacity}

function TFreeNURBSurface.FGetpoint(Col,Row:Integer):T3DCoordinate;
begin
   Result:=FControlPoints[Row][Col];
end;{TFreeNURBSurface.FGetpoint}

procedure TFreeNURBSurface.FSetColDegree(Val:Integer);
begin
   if Val>5 then Val:=5;
   if Val<>FColDegree then
   begin
      FColDegree:=Val;
      Build:=False;
   end;
end;{TFreeNURBSurface.FSetColDegree}

procedure TFreeNURBSurface.FSetRowDegree(Val:Integer);
begin
   if Val>5 then Val:=5;
   if Val<>FRowDegree then
   begin
      FRowDegree:=Val;
      Build:=False;
   end;
end;{TFreeNURBSurface.FSetRowDegree}

procedure TFreeNURBSurface.SetDefaultColKnotvector;
var I,L    : Integer;
    No     : Integer;
begin
   if FColDegree>FColCount-1 then FColDegree:=FColCount-1;
   L:=FColCount+FColDegree+1;
   Setlength(FColKnots,L);
   No:=(FColCount+FColDegree+1)-2*FColDegree;
   for I:=1 to FColDegree do FColKnots[I-1]:=0.0;
   for I:=1 to No do
   begin
      FColKnots[I+FColDegree-1]:=((I-1)/(No-1));
      if FColKnots[I-1]<0.0 then FColKnots[I-1]:=0.0 else
         if FColKnots[I-1]>1.0 then FColKnots[I-1]:=1.0;
   end;
   for I:=1 to FColDegree do FColKnots[FColDegree+No+I-1]:=1.000;
end;{TFreeNURBSurface.SetDefaultColKnotvector}

procedure TFreeNURBSurface.SetDefaultRowKnotvector;
var I,L    : Integer;
    No     : Integer;
begin
   if FRowDegree>FRowCount-1 then FRowDegree:=FRowCount-1;
   L:=FRowCount+FRowDegree+1;
   Setlength(FRowKnots,L);
   No:=(FRowCount+FRowDegree+1)-2*FRowDegree;
   for I:=1 to FRowDegree do FRowKnots[I-1]:=0.0;
   for I:=1 to No do
   begin
      FRowKnots[I+FRowDegree-1]:=((I-1)/(No-1));
      if FRowKnots[I-1]<0.0 then FRowKnots[I-1]:=0.0 else
         if FRowKnots[I-1]>1.0 then FRowKnots[I-1]:=1.0;
   end;
   for I:=1 to FRowDegree do FRowKnots[FRowDegree+No+I-1]:=1.000;
end;{TFreeNURBSurface.SetDefaultRowKnotvector}

procedure TFreeNURBSurface.SetUniformColKnotvector;
var I,L    : Integer;
begin
   if FColDegree>FColCount-1 then FColDegree:=FColCount-1;
   L:=FColCount+FColDegree+1;
   Setlength(FColKnots,L);
   for I:=1 to L do
   begin
      FColKnots[I-1]:=(I-1)/(L-1);
         if FColKnots[I-1]<0.0 then FColKnots[I-1]:=0.0 else
            if FColKnots[I-1]>1.0 then FColKnots[I-1]:=1.0;
   end;
end;{TFreeNURBSurface.SetUniformColKnotvector}

procedure TFreeNURBSurface.SetUniformRowKnotvector;
var I,L    : Integer;
begin
   if FRowDegree>FRowCount-1 then FRowDegree:=FRowCount-1;
   L:=FRowCount+FRowDegree+1;
   Setlength(FRowKnots,L);
   for I:=1 to L do
   begin
      FRowKnots[I-1]:=(I-1)/(L-1);
         if FRowKnots[I-1]<0.0 then FRowKnots[I-1]:=0.0 else
            if FRowKnots[I-1]>1.0 then FRowKnots[I-1]:=1.0;
   end;
end;{TFreeNURBSurface.SetUniformRowKnotvector}

procedure TFreeNURBSurface.FSetPoint(Col,Row:Integer;Val:T3DCoordinate);
begin
   if (Col>ColCapacity) and (Row>RowCapacity) then SetCapacity(Col,Row) else
      if Col>ColCapacity then SetCapacity(Col,RowCapacity) else
         if Row>RowCapacity then SetCapacity(ColCapacity,Row);
   FControlPoints[Row][Col]:=Val;
   Build:=false;
end;{TFreeNURBSurface.FSetPoint}

procedure TFreeNURBSurface.FSetColCapacity(Val:integer);
begin
   if Val<>FColCapacity then SetCapacity(Val,FRowCapacity);
end;{TFreeNURBSurface.FSetColCapacity}

procedure TFreeNURBSurface.FSetRowCapacity(Val:integer);
begin
   if Val<>FRowCapacity then SetCapacity(FColCapacity,Val);
end;{TFreeNURBSurface.FSetVCapacity}

procedure TFreeNURBSurface.Clear;
begin
   Build:=false;
   Inherited Clear;
   SetCapacity(0,0);
   FColCount:=0;
   FRowCount:=0;
   FColDegree:=3;
   FRowDegree:=3;
   Setlength(FColKnots,0);
   Setlength(FRowKnots,0);
end;{TFreeNURBSurface.Clear}

procedure TFreeNURBSurface.DeleteColumn(Col:Integer);
var I:Integer;
begin
   for I:=1 to Rowcount do
   begin
      Move(FControlpoints[I-1][Col+1],FControlpoints[I-1][Col],(Colcount-Col-1)*SizeOf(T3DCoordinate));
   end;
   Dec(FColCount);
   Build:=False;
end;{TFreeNURBSurface.DeleteColumn}

procedure TFreeNURBSurface.DeleteRow(Row:Integer);
var I:Integer;
begin
   for I:=Row+1 to Rowcount-1 do
      Move(FControlpoints[I][0],FControlpoints[I-1][0],(Colcount)*SizeOf(T3DCoordinate));
   Dec(FRowCount);
   Build:=False;
end;{TFreeNURBSurface.DeleteColumn}

procedure TFreeNURBSurface.InsertColKnot(U:TFloatType);
var Index,L    : Integer;
    I,J,k,N,X  : Integer;
    J1,X1,I1   : Integer;
    NewPoints  : TFreeCoordinateGrid;
    Alpha      : array of double;
begin
   if Length(FColKnots)=0 then SetDefaultColKnotvector;
   FBuild:=True;
   // Find lowest index
   Index:=0;
   for J:=1 to Length(FColknots)-1 do
   begin
      if (FColknots[J-1]<U) and (FColknots[J]>=U) then
      begin
         Index:=J-1;
         break;
      end;
   end;

   if Index<>-1 then
   begin
      I:=index;
      N:=FColcount;;
      k:=FColdegree+1;
      Setlength(Alpha,N+1);
      Setlength(Newpoints,RowCount);
      L:=I-k+1;
      if L<0 then L:=0;
      for J:=0 to N do
      begin
         if J<=L then alpha[J]:=1.0 else
            if (L+1<=J) and (J<=I) then
            begin
               if FColknots[j+k-1]-FColknots[J]=0 then alpha[J]:=0
                                                  else alpha[J]:=(U-FColknots[J])/(FColknots[j+k-1]-FColknots[J]);
            end else alpha[J]:=0;
      end;
      for J:=1 to RowCount do
      begin
         J1:=J-1;
         Setlength(Newpoints[J-1],N+1);
         for X:=0 to N do
         begin
         X1:=X-1;
            if Alpha[X]=0.0 then NewPoints[J1][X]:=FControlpoints[J1][X1] else
               if Alpha[X]=1.0 then NewPoints[J1][X]:=FControlpoints[J1][X] else
               begin
                  NewPoints[J1][X].X:=(1-Alpha[X])*FControlpoints[J1][X1].X+alpha[X]*FControlpoints[J1][X].X;
                  NewPoints[J1][X].Y:=(1-Alpha[X])*FControlpoints[J1][X1].Y+alpha[X]*FControlpoints[J1][X].Y;
                  NewPoints[J1][X].Z:=(1-Alpha[X])*FControlpoints[J1][X1].Z+alpha[X]*FControlpoints[J1][X].Z;
               end;
         end;
      end;
      for I:=1 to Rowcount do
      begin
         I1:=I-1; 
         setlength(FControlpoints[I1],FColcount+1);
         Move(Newpoints[I1][0],FControlpoints[I1][0],(FColcount+1)*SizeOf(T3DCoordinate));
      end;
      // create new knotvector
      X:=length(FColknots);
      Setlength(FColknots,X+1);
      Move(FColknots[index],FColknots[index+1],(X-Index)*SizeOf(TFloatType));
      FColknots[index+1]:=U;
      FColCapacity:=FColcount;
      inc(FColcount);
   end;
end;{TFreeNURBSurface.InsertColKnot}

procedure TFreeNURBSurface.InsertRowKnot(V:TFloatType);
var Index      : Integer;
    I,J,k,N,X  : Integer;
    J1,X1      : Integer;
    NewPoints  : TFreeCoordinateGrid;
    Alpha      : array of double;
begin

   if Length(FRowKnots)=0 then SetDefaultRowKnotvector;
   FBuild:=True;
   // Find lowest index
   Index:=-1;
   for J:=1 to Length(FRowknots)-1 do
   begin
      if (FRowknots[J-1]<=V) and (FRowknots[J]>V) then
      begin
         Index:=J-1;
         break;
      end;
   end;

   if Index<>-1 then
   begin
      I:=index;
      N:=FRowcount;
      k:=FRowdegree+1;
      Setlength(Alpha,N+1);
      Setlength(Newpoints,RowCount+1);
      for J:=0 to N do
      begin
         Setlength(Newpoints[J],Colcount);
         if J<=I-k+1 then alpha[J]:=1.0 else
            if (I-k+2<=J) and (J<=I) then
            begin
               if FRowknots[j+k-1]-FRowknots[J]=0 then alpha[J]:=0
                                                  else alpha[J]:=(V-FRowknots[J])/(FRowknots[j+k-1]-FRowknots[J]);
            end else alpha[J]:=0;
      end;

      for J:=1 to ColCount do
      begin
         J1:=J-1; 
         for X:=0 to N do
         begin
            X1:=X-1; 
            if Alpha[X]=0.0 then NewPoints[X][J1]:=FControlpoints[X1][J1] else
               if Alpha[X]=1.0 then NewPoints[X][J1]:=FControlpoints[X][J1] else
               begin
                  NewPoints[X][J1].X:=(1-Alpha[X])*FControlpoints[X1][J1].X+alpha[X]*FControlpoints[X][J1].X;
                  NewPoints[X][J1].Y:=(1-Alpha[X])*FControlpoints[X1][J1].Y+alpha[X]*FControlpoints[X][J1].Y;
                  NewPoints[X][J1].Z:=(1-Alpha[X])*FControlpoints[X1][J1].Z+alpha[X]*FControlpoints[X][J1].Z;
               end;
         end;
      end;
      // Replace the current controlpoints with the new ones
      Setlength(FControlpoints,Rowcount+1);
      for I:=0 to Rowcount do
      begin
         setlength(FControlpoints[I],FColcount);
         Move(Newpoints[I][0],FControlpoints[I][0],(FColcount)*SizeOf(T3DCoordinate));
      end;
      // create the new new knotvector
      X:=length(FRowknots);
      Setlength(FRowknots,X+1);
      Move(FRowknots[index],FRowknots[index+1],(X-Index)*SizeOf(TFloatType));
      FRowknots[index+1]:=V;
      inc(FRowcount);
      FRowCapacity:=FRowcount;
   end;
end;{TFreeNURBSurface.InsertRowKnot}

procedure TFreeNURBSurface.NormalizeKnotVectors;
var I,N     : Integer;
    Min,Max : TFloatType;
    New     : TFloatType;
begin
   N:=length(FRowknots);
   if N>0 then
   begin
      Min:=FRowknots[0];
      Max:=FRowknots[N-1];
      if Min<>Max then for I:=1 to N do
      begin
         New:=(FRowknots[I-1]-Min)/(Max-Min);
         if New>1.0 then New:=1.0;
         FRowknots[I-1]:=New;
      end;
   end;
   N:=length(FColknots);
   if N>0 then
   begin
      Min:=FColknots[0];
      Max:=FColknots[N-1];
      if Min<>Max then for I:=1 to N do
      begin
         New:=(FColknots[I-1]-Min)/(Max-Min);
         if New>1.0 then New:=1.0;
         FColknots[I-1]:=New;
      end;
   end;

end;{TFreeNURBSurface.NormalizeKnotVectors}

procedure TFreeNURBSurface.Rebuild;
begin
   if Build then Build:=false;
   if FColDegree>FColcount-1 then FColDegree:=FColcount-1;
   if FRowDegree>FRowcount-1 then FRowDegree:=FRowcount-1;
   SetDefaultColKnotVector;
   SetDefaultRowKnotVector;
   Build:=True;
   Inherited Rebuild;
end;{TFreeNURBSurface.Rebuild}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionBase                                    }
{                                                                                                   }
{ TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
{---------------------------------------------------------------------------------------------------}
constructor TFreeSubdivisionBase.Create(Owner:TFreeSubdivisionSurface);
begin
   Inherited Create;
   FOwner:=Owner;
end;{TFreeSubdivisionBase.Create}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionControlCurve                            }
{---------------------------------------------------------------------------------------------------}
procedure TFreeSubdivisionControlCurve.FSetBuild(Val:Boolean);
begin
   FBuild:=Val;
end;{TFreeSubdivisionControlCurve.FSetBuild}

procedure TFreeSubdivisionControlCurve.FSetSelected(val:Boolean);
var Index : Integer;
begin
   Index:=Owner.FSelectedControlCurves.IndexOf(self);
   if Val then
   begin
      // Only add if it is not already in the list
      if Index=-1 then Owner.FSelectedControlCurves.Add(self);
   end else
   begin
      if Index<>-1 then Owner.FSelectedControlCurves.Delete(index);
   end;
   if Assigned(Owner.FOnSelectItem) then Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlCurve.FSetSelected}

procedure TFreeSubdivisionControlCurve.AddPoint(P:TFreesubdivisionPoint);
begin
   FControlPoints.Add(P);
   Build:=False;
end;{TFreeSubdivisionControlCurve.AddPoint}

function TFreeSubdivisionControlCurve.FGetColor:TColor;
begin
   if Selected then Result:=Owner.Selectedcolor
               else Result:=Owner.ControlCurveColor;
end;{TFreeSubdivisionControlCurve.FGetColor}

function TFreeSubdivisionControlCurve.FGetSelected:Boolean;
begin
   Result:=Owner.FSelectedControlCurves.IndexOf(self)<>-1;
end;{TFreeSubdivisionControlCurve.FGetSelected}

function TFreeSubdivisionControlCurve.FGetVisible:Boolean;
begin
   Result:=Owner.ShowControlCurves;
end;{TFreeSubdivisionControlCurve.FGetVisible}

function TFreeSubdivisionControlCurve.FGetNumberOfControlPoints:Integer;
begin
   Result:=FControlPoints.Count;
end;{TFreeSubdivisionControlCurve.FGetNumberOfControlPoints}

function TFreeSubdivisionControlCurve.FGetControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
begin
   Result:=FControlPoints[index];
end;{TFreeSubdivisionControlCurve.FGetControlPoint}

procedure TFreeSubdivisionControlCurve.Clear;
begin
   FControlPoints.Clear;
   FVisible:=True;
   FCurve.Clear;
   FSubdividedPoints.Clear;
   FBuild:=False;
end;{TFreeSubdivisionControlCurve.Clear}

constructor TFreeSubdivisionControlCurve.Create(Owner:TFreeSubdivisionSurface);
begin
   Inherited Create(Owner);
   FControlPoints:=TFasterList.Create;
   FSubdividedPoints:=TFasterList.Create;
   FCurve:=TFreeSpline.Create;
   Clear;
end;{TFreeSubdivisionControlCurve.Create}

procedure TFreeSubdivisionControlCurve.Delete;
var Index : Integer;
    I     : Integer;
    P1,P2 : TFreeSubdivisionPoint;
    Edge  : TFreeSubdivisionEdge;
begin
   Index:=Owner.FSelectedControlCurves.IndexOf(self);
   if Index<>-1 then Owner.FSelectedControlCurves.Delete(Index);
   Index:=Owner.FControlCurves.IndexOf(self);
   if Index<>-1 then Owner.FControlCurves.Delete(Index);
   // Remove references from control edges
   for I:=2 to FControlPoints.Count do
   begin
      P1:=FControlPoints[I-2];
      P2:=FControlPoints[I-1];
      Edge:=Owner.EdgeExists(P1,P2);
      if Edge<>nil then begin
        Edge.FCurve:=nil;
      end;
   end;
   FControlPoints.Clear;
   // Remove references from subdivided edges
   for I:=2 to FSubdividedPoints.Count do
   begin
      P1:=FSubdividedPoints[I-2];
      P2:=FSubdividedPoints[I-1];
      Edge:=Owner.EdgeExists(P1,P2);
      if Edge<>nil then begin
        Edge.FCurve:=nil;
      end;
   end;
   FSubdividedPoints.Clear;
   Destroy;
end;{TFreeSubdivisionControlCurve.Delete}

procedure TFreeSubdivisionControlCurve.DeleteEdge(Edge:TFreeSubdivisionControlEdge);
var I,J     : Integer;
    P1,P2   : TFreeSubdivisionPoint;
    AnEdge  : TFreeSubdivisionEdge;
    NewCurve: TFreeSubdivisionControlCurve;
    DelCurve: Boolean;

begin
   DelCurve:=False;
   I:=2;
   While I<=FControlPoints.Count do
   begin
      if ((FControlPoints[I-2]=Edge.FStartPoint) and (FControlPoints[I-1]=Edge.EndPoint)) or
         ((FControlPoints[I-2]=Edge.EndPoint) and (FControlPoints[I-1]=Edge.FStartPoint)) then
      begin
         // Remove references to this curve from control edges
         for J:=2 to FControlPoints.Count do
         begin
            P1:=FControlPoints[J-2];
            P2:=FControlPoints[J-1];
            AnEdge:=Owner.EdgeExists(P1,P2);
            if AnEdge<>nil then AnEdge.FCurve:=nil;
         end;
         // Remove references from subdivided edges
         if Build then for J:=2 to FSubdividedPoints.Count do
         begin
            P1:=FSubdividedPoints[J-2];
            P2:=FSubdividedPoints[J-1];
            AnEdge:=Owner.EdgeExists(P1,P2);
            if AnEdge<>nil then AnEdge.FCurve:=nil;
         end;
         FSubdividedPoints.Clear;


         if I-2>0 then
         begin
            // build first new curve
            NewCurve:=TFreeSubdivisionControlCurve.Create(Owner);
            NewCurve.FControlPoints.Capacity:=I-1;
            Owner.AddControlCurve(Newcurve);
            P1:=nil;
            for J:=0 to I-2 do
            begin
               P2:=FControlPoints[J];
               NewCurve.FControlPoints.Add(P2);
               if J>0 then
               begin
                  AnEdge:=Owner.EdgeExists(P1,P2);
                  if AnEdge<>nil then AnEdge.FCurve:=NewCurve;
               end;
               P1:=P2;
            end;
            NewCurve.Selected:=Selected;
         end;
         if I-1<FControlPoints.Count-1 then
         begin
            // build second new curve
            NewCurve:=TFreeSubdivisionControlCurve.Create(Owner);
            NewCurve.FControlPoints.Capacity:=FControlPoints.Count-(I-1);
            Owner.AddControlCurve(Newcurve);
            P1:=nil;
            for J:=I-1 to FControlPoints.Count-1 do
            begin
               P2:=FControlPoints[J];
               NewCurve.FControlPoints.Add(P2);
               if J>I-1 then
               begin
                  AnEdge:=Owner.EdgeExists(P1,P2);
                  if AnEdge<>nil then AnEdge.FCurve:=NewCurve;
               end;
               P1:=P2;
            end;
            NewCurve.Selected:=Selected;
         end;
         DelCurve:=True;
         break;
      end;
      inc(I);
   end;
   if DelCurve then
   begin
      FControlPoints.Clear;
      J:=Owner.FSelectedControlCurves.IndexOf(self);
      if J<>-1 then Owner.FSelectedControlCurves.Delete(J);
      J:=Owner.FControlCurves.IndexOf(self);
      if J<>-1 then Owner.FControlCurves.Delete(J);
      Destroy;
   end;

end;{TFreeSubdivisionControlCurve.DeleteEdge}

destructor TFreeSubdivisionControlCurve.Destroy;
begin
   Clear;
   FControlPoints.Destroy;
   FControlPoints := nil;
   FSubdividedPoints.Destroy;
   FSubdividedPoints := nil;
   FCurve.Destroy;
   FCurve := nil;
   Inherited Destroy;
end;{TFreeSubdivisionControlCurve.Destroy}

function TFreeSubdivisionControlCurve.DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
var I,Tmp    : Integer;
    Pt,P1,P2 : TPoint;
    V1,V2    : T3DCoordinate;
    Param    : TFloatType;
begin
   if (Viewport.ViewType=fvBodyPlan) and (not Owner.DrawMirror) then
   begin
      Result:=1000000;
      // Check if cursor position lies within the boundaries
      Pt.X:=X;
      Pt.Y:=Y;
      if (Pt.X>=0) and (Pt.X<=Viewport.Width) and (Pt.Y>=0) and (Pt.Y<=Viewport.Height) then
      begin
         V1:=FCurve.Value(0.0);
         if V1.X<Owner.MainframeLocation then V1.Y:=-V1.Y;
         for I:=1 to FCurve.Fragments do
         begin
            V2:=FCurve.Value((I-1)/(FCurve.Fragments-1));
            if V2.X<Owner.MainframeLocation then V2.Y:=-V2.Y;
            if ((V1.X<Owner.MainframeLocation) and (V2.X<Owner.MainframeLocation)) or
               ((V1.X>Owner.MainframeLocation) and (V2.X>Owner.MainframeLocation)) then
            begin
               P1:=Viewport.Project(V1);
               P2:=Viewport.Project(V2);
               Tmp:=Round(DistanceToLine(P1,P2,X,Y,Param));
               if Tmp<Result then result:=Tmp;
            end;
            P1:=P2;
            V1:=V2;
         end;
      end;
   end else Result:=FCurve.DistanceToCursor(X,Y,Viewport);
   if Owner.DrawMirror then
   begin
      for I:=1 to FCurve.NumberOfPoints do
      begin
         V1:=FCurve.Point[I-1];
         V1.Y:=-V1.Y;
         FCurve.Point[I-1]:=V1;
      end;
      try
         Tmp:=FCurve.DistanceToCursor(X,Y,Viewport);
         if Tmp<Result then Result:=Tmp;
      finally
         for I:=1 to FCurve.NumberOfPoints do
         begin
            V1:=FCurve.Point[I-1];
            V1.Y:=-V1.Y;
            FCurve.Point[I-1]:=V1;
         end;
      end;
   end;
end;{TFreeSubdivisionControlCurve.DistanceToCursor}

procedure TFreeSubdivisionControlCurve.Draw(Viewport:TFreeViewport);
var Sel     : Boolean;
    P1,P2   : TFreeSubdivisionControlPoint;
    I,J     : Integer;
    I1,I2,J1: Integer;
    Scale   : Integer;
    Fragm   : Integer;
    NParam  : Integer;
    Param   : TFloatArray;
    Edge    : TFreesubdivisionControlEdge;
    Plane   : T3DPlane;
    Output  : TFreeIntersectionData;
    PArray1 : array of TPoint;
    PArray2 : array of TPoint;
    P3D,P3D2: T3DCoordinate;
    Normal  : T3DCoordinate;
    C,T,C2  : TFloatType;
begin
   if FCurve.NumberOfPoints>1 then
   begin
      FCurve.Color:=Color;
      Sel:=Selected;
      FCurve.ShowCurvature:=(Sel) and (Owner.FShowCurvature);
      if FCurve.ShowCurvature then FCurve.Fragments:=600
                              else FCurve.Fragments:=250;
      FCurve.CurvatureColor:=Owner.FCurvatureColor;
      FCurve.CurvatureScale:=Owner.FCurvatureScale;
      if not Owner.ShowControlNet and (Sel) then
      begin
         For I:=2 to FControlPoints.Count do
         begin
            P1:=FControlPoints[I-2];
            P2:=FControlPoints[I-1];
            Edge:=Owner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
            if Edge<>nil then Edge.Draw(False,Viewport);
            if I=2 then P1.Draw(Viewport);
            P2.Draw(Viewport);
         end;
      end;

      if (Viewport.ViewType=fvBodyPlan) and (not Owner.DrawMirror) then
      begin
         Plane:=SetPlane(1.0,0.0,0.0,-Owner.MainframeLocation);
         NParam:=2;
         Setlength(Param,NParam);
         Param[0]:=0.0;
         Param[1]:=1.0;
         if FCurve.IntersectPlane(Plane,Output) then
         begin
            Setlength(Param,NParam+Output.NumberOfIntersections);
            for I:=1 to Output.NumberOfIntersections do
            begin
               Param[NParam]:=Output.Parameters[I-1];
               inc(NParam);
            end;
            SortFloatArray(Param,NParam);
         end;
         for I:=2 to NParam do
         begin
            I1:=I-1;
            I2:=I-2;
            P3D:=FCurve.Value(0.5*(Param[I2]+Param[I1]));
            if P3D.X<Owner.MainframeLocation then Scale:=-1 else scale:=1;
            Fragm:=Round((Param[I1]-Param[I2])*FCurve.Fragments);
            if Fragm<10 then Fragm:=10;
            if FCurve.ShowCurvature then
            begin
               SetLength(PArray1,Fragm);
               SetLength(PArray2,Fragm);
               for J:=1 to Fragm do
               begin
                  J1:=J-1;
                  T:=Param[I2]+(Param[I1]-Param[I2])*(J1)/(Fragm-1);
                  C:=FCurve.Curvature(T,P3D,Normal);
                  C2:=C*2*FCurve.CurvatureScale; 
                  P3D.Y:=P3D.Y*Scale;
                  Normal.Y:=Normal.Y*Scale;
                  PArray1[J1]:=Viewport.Project(P3D);
                  P3D2.X:=P3D.X-C2*Normal.X;
                  P3D2.Y:=P3D.Y-C2*Normal.Y;
                  P3D2.Z:=P3D.Z-C2*Normal.Z;
                  PArray2[J1]:=Viewport.Project(P3D2);
               end;
               Viewport.SetPenWidth(1);
               Viewport.PenColor:=FCurve.CurvatureColor;
               for J:=1 to Fragm do if (J mod 4=0) or (J=1) or (J=Fragm) then
               begin
                  J1:=J-1;
                  Viewport.MoveTo(PArray1[J1].X,PArray1[J1].Y);
                  Viewport.LineTo(PArray2[J1].X,PArray2[J1].Y);
               end;
               Viewport.Polyline(PArray2);
            end else
            begin
               SetLength(PArray1,Fragm);
               for J:=1 to Fragm do
               begin
                  J1:=J-1;
                  T:=Param[I2]+(Param[I1]-Param[I2])*(J1)/(Fragm-1);
                  P3D:=FCurve.Value(T);
                  P3D.Y:=P3D.Y*Scale;
                  PArray1[J1]:=Viewport.Project(P3D);
               end;
            end;
            Viewport.SetPenWidth(1);
            Viewport.PenColor:=Color;
            Viewport.PenStyle:=FCurve.Penstyle;
            Viewport.Polyline(PArray1);
         end;
      end else FCurve.Draw(Viewport);
      if Owner.DrawMirror then
      begin
         for I:=1 to FCurve.NumberOfPoints do
         begin
            P3D:=FCurve.Point[I-1];
            P3D.Y:=-P3D.Y;
            FCurve.Point[I-1]:=P3D;
         end;
         FCurve.Draw(Viewport);
         for I:=1 to FCurve.NumberOfPoints do
         begin
            P3D:=FCurve.Point[I-1];
            P3D.Y:=-P3D.Y;
            FCurve.Point[I-1]:=P3D;
         end;
      end;
   end;
end;{TFreeSubdivisionControlCurve.Draw}

procedure TFreeSubdivisionControlCurve.InsertControlPoint(P1,P2,New:TFreeSubdivisionControlPoint);
var I    : Integer;
    I1,I2: Integer;
begin
   I:=2;
   While I<=FControlPoints.Count do
   begin
      I1:=I-1;
      I2:=I-2;
      if ((FControlPoints[I2]=P1) and (FControlPoints[I1]=P2)) or
         ((FControlPoints[I1]=P1) and (FControlPoints[I2]=P2)) then
      begin
         FControlPoints.Insert(I1,New);
      end;
      inc(I);
   end;
end;{TFreeSubdivisionControlCurve.InsertControlPoint}

procedure TFreeSubdivisionControlCurve.InsertEdgePoint(P1,P2,New:TFreeSubdivisionPoint);
var I : Integer;
    I1,I2: Integer;
begin
   I:=2;
   While I<=FSubdividedPoints.Count do
   begin
      I1:=I-1;
      I2:=I-2;
      if ((FSubdividedPoints[I2]=P1) and (FSubdividedPoints[I1]=P2)) or
         ((FSubdividedPoints[I1]=P1) and (FSubdividedPoints[I2]=P2)) then
      begin
         FSubdividedPoints.Insert(I1,New);
      end;
      inc(I);
   end;
end;{TFreeSubdivisionControlCurve.InsertEdgePoint}

procedure TFreeSubdivisionControlCurve.LoadBinary(Source:TFreeFileBuffer);
var I,N,Ind : Integer;
    P1,P2  : TFreeSubdivisionPoint;
    Edge   : TFreeSubdivisionEdge;
    Sel    : Boolean;
begin
   Source.Load(N);
   FControlPoints.Capacity:=N;
   P1:=nil;
   for I:=1 to N do
   begin
      Source.Load(Ind);
      P2:=Owner.FControlPoints[ind];
      FControlPoints.Add(P2);
      if I>1 then
      begin
         Edge:=Owner.EdgeExists(P1,P2);
         if Edge<>nil then
         begin
            Edge.FCurve:=self;
         end;
      end;
      P1:=P2;
   end;
   FSubdividedPoints.AddList(FControlPoints);
   Source.Load(Sel);
   if Sel then selected:=True;
end;{TFreeSubdivisionControlCurve.LoadBinary}

procedure TFreeSubdivisionControlCurve.ReplaceVertexPoint(Old,New:TFreeSubdivisionPoint);
var I:Integer;
begin
   for I:=1 to FSubdividedPoints.Count do if FSubdividedPoints[I-1]=old then
   begin
      FSubdividedPoints[I-1]:=New;
      FCurve.Clear;
   end;
end;{TFreeSubdivisionControlCurve.ReplaceVertexPoint}

procedure TFreeSubdivisionControlCurve.SaveBinary(Destination:TFreeFileBuffer);
var I,Ind : Integer;
    P     : TFreeSubdivisionPoint;
begin
   Destination.Add(NumberOfControlPoints);
   for I:=1 to NumberOfControlPoints do
   begin
      P:=FControlPoints[I-1];
      Ind:=Owner.FControlPoints.SortedIndexOf(P);
      Destination.Add(Ind);
   end;
   Destination.Add(Selected);
end;{TFreeSubdivisionControlCurve.SaveBinary}

procedure TFreeSubdivisionControlCurve.SaveToDXF(Strings:TStringList);
var Layer : string;
begin
   Layer:='Control_curves';
   FCurve.Fragments:=FCurve.NumberOfPoints;
   FCurve.SaveToDXF(Strings,Layer,Owner.DrawMirror);
end;{TFreeSubdivisionControlCurve.SaveToDXF}

{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionLayer                                   }
{                                                                                                   }
{ TFreeSubdivisionLayer is a layertype class                                                        }
{                                                                                                   }
{ All individual controlfaces can be assigned to a layer. Properties such as color,                 }
{ visibility etc. are common for all controlfaces belonging the the same layer                      }
{---------------------------------------------------------------------------------------------------}
function TFreeSubdivisionLayer.FGetColor:TColor;
begin
   Result:=FColor;
end;{TFreeSubdivisionLayer.FGetColor}

function TFreeSubdivisionLayer.FGetCount:Integer;
begin
   Result:=FPatches.Count;
end;{TFreeSubdivisionLayer.FGetCount}

function TFreeSubdivisionLayer.FGetDXFLayername:string;
begin
   Result:=self.Name;
end;{TFreeSubdivisionLayer.FGetDXFLayername}

function TFreeSubdivisionLayer.FGetName:string;
begin
   if FDescription='' then Result:=Userstring(33)+#32+IntToStr(LayerId)
                      else Result:=FDescription;
end;{TFreeSubdivisionLayer.FGetName}

function TFreeSubdivisionLayer.FGetItems(Index:Integer):TFreeSubdivisionControlFace;
begin
   Result:=FPatches[Index];
end;{TFreeSubdivisionLayer.FGetItems}

function TFreeSubdivisionLayer.FGetLayerIndex:Integer;
begin
   Result:=Owner.FLayers.IndexOf(self);
end;{TFreeSubdivisionLayer.FGetLayerIndex}

function TFreeSubdivisionLayer.FGetSurfaceProperties:TLayerProperties;
var I,J,K,II     : Integer;
    Coeff,Xmin,Xmax: Single;
    Child        : TFreeSubdivisionFace;
    ffile        : textfile;
    Area         : TFloatType;
    Center       : T3DCoordinate;

    procedure ProcessTriangle(P1,P2,P3:T3DCoordinate);
    var 
        ax,ay,az     : TFloatType;
    begin
       Center.X:=(P1.X+P2.X+P3.X)/3;
       Center.Y:=(P1.Y+P2.Y+P3.Y)/3;
       Center.Z:=(P1.Z+P2.Z+P3.Z)/3;
       ax:=(P1.y-P2.y)*(P1.z+P2.z)+(P2.y-P3.y)*(P2.z+P3.z)+(P3.y-P1.y)*(P3.z+P1.z);
       ay:=(P1.z-P2.z)*(P1.x+P2.x)+(P2.z-P3.z)*(P2.x+P3.x)+(P3.z-P1.z)*(P3.x+P1.x);
       az:=(P1.x-P2.x)*(P1.y+P2.y)+(P2.x-P3.x)*(P2.y+P3.y)+(P3.x-P1.x)*(P3.y+P1.y);
       Area:=Sqrt(ax*ax+ay*ay+az*az)*0.5;
       Result.SurfaceArea:=Result.SurfaceArea+Area;
       Result.SurfaceCenterOfGravity.X:=Result.SurfaceCenterOfGravity.X+Area*Center.X;
       Result.SurfaceCenterOfGravity.Y:=Result.SurfaceCenterOfGravity.Y+Area*Center.Y;
       Result.SurfaceCenterOfGravity.Z:=Result.SurfaceCenterOfGravity.Z+Area*Center.Z;
    end;{ProcessTriangle}

begin
   II:=0;
   Coeff:=Thickness*MaterialDensity;
   Fillchar(Result,SizeOf(Result),0);
    if FileExists('Weights.txt') { *Converted from FileExists* } and (coeff>0)
       then begin
      Assignfile(FFile,'Weights.txt');  
      Append(FFile);
      II:=1;
      if Symmetric then
        begin
        writeln(ffile,Count:4,Items[0].ChildCount:4,' 2');
        Coeff:=Coeff*2;
        end
      else writeln(ffile,intToStr(Count)+' '+IntToStr(Items[0].ChildCount)+' 1');
    end;
   for I:=1 to Count do
   begin
      For J:=1 to Items[I-1].ChildCount do
      begin
         Child:=Items[I-1].Child[J-1];
         for K:=3 to Child.NumberOfpoints do ProcessTriangle(Child.Point[0].Coordinate,Child.Point[K-2].Coordinate,Child.Point[K-1].Coordinate);
         if ii=1 then begin
             if Child.NumberOfpoints=3 then begin
                       Xmin:=Child.Point[0].Coordinate.X;
                       Xmax:=Child.Point[0].Coordinate.X;
                       if Child.Point[1].Coordinate.X>Xmax then Xmax:=Child.Point[1].Coordinate.X;
                       if Child.Point[2].Coordinate.X>Xmax then Xmax:=Child.Point[2].Coordinate.X;
                       if Child.Point[1].Coordinate.X<Xmin then Xmin:=Child.Point[1].Coordinate.X;
                       if Child.Point[2].Coordinate.X<Xmin then Xmin:=Child.Point[2].Coordinate.X;
                       if area*coeff>10000 then write(ffile,Center.X:7:3,Center.Y:7:3,Center.Z:7:3,Area*Coeff:10:1)	
                                           else write(ffile,Center.X:7:3,Center.Y:7:3,Center.Z:7:3,Area*Coeff:10:3);
                       writeln(ffile,Xmin:8:3,Xmax:8:3,' 3');	
             end else begin 
                  if area*coeff>10000 then writeln(ffile,Center.X:7:3,Center.Y:7:3,Center.Z:7:3,Area*Coeff:10:1,(Child.Point[0].Coordinate.X+Child.Point[3].Coordinate.X)/2:8:3,(Child.Point[1].Coordinate.X+Child.Point[2].Coordinate.X)/2:8:3,' 4')	
                                      else writeln(ffile,Center.X:7:3,Center.Y:7:3,Center.Z:7:3,Area*Coeff:10:3,(Child.Point[0].Coordinate.X+Child.Point[3].Coordinate.X)/2:8:3,(Child.Point[1].Coordinate.X+Child.Point[2].Coordinate.X)/2:8:3,' 4');	
                 end; 
          end; 
      end;
   end;
   if Result.SurfaceArea<>0 then
   begin
      Result.SurfaceCenterOfGravity.X:=Result.SurfaceCenterOfGravity.X/Result.SurfaceArea;
      Result.SurfaceCenterOfGravity.Y:=Result.SurfaceCenterOfGravity.Y/Result.SurfaceArea;
      Result.SurfaceCenterOfGravity.Z:=Result.SurfaceCenterOfGravity.Z/Result.SurfaceArea;
      if Symmetric then
      begin
         Result.SurfaceArea:=2*Result.SurfaceArea;
         Result.SurfaceCenterOfGravity.Y:=0.0;
      end;
      Result.Weight:=Result.SurfaceArea*Thickness*MaterialDensity;
   end;
   if ii=1 then begin
      writeln(ffile,Result.SurfaceCenterOfGravity.X:7:3,Result.SurfaceCenterOfGravity.Y:7:3,Result.SurfaceCenterOfGravity.Z:7:3,Result.SurfaceArea*Thickness*MaterialDensity:12:1,Thickness:8:3,MaterialDensity:8:3);	
      closefile(ffile);      
   end;
end;{TFreeSubdivisionLayer.FGetSurfaceProperties}

procedure TFreeSubdivisionLayer.FSetFDevelopable(Val:Boolean);
begin
   if val<>FDevelopable then
   begin
      FDevelopable:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetFDevelopable}

procedure TFreeSubdivisionLayer.FSetName(Val:String);
begin
   if Uppercase(Val)<>Uppercase(FDescription) then
   begin
      FDescription:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
      if (self=Owner.ActiveLayer) and (assigned(Owner.FOnChangeActiveLayer)) then owner.FOnChangeActiveLayer(Owner,Owner.ActiveLayer);
   end;
end;{TFreeSubdivisionLayer.FSetName}

procedure TFreeSubdivisionLayer.FSetSymmetric(Val:Boolean);
begin
   if Val<>FSymmetric then
   begin
      FSymmetric:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetSymmetric}
procedure TFreeSubdivisionLayer.FSetColor(Val:TColor);
begin
   if Val<>FColor then
   begin
      FColor:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
      if (self=Owner.ActiveLayer) and (assigned(Owner.FOnChangeActiveLayer)) then owner.FOnChangeActiveLayer(Owner,Owner.ActiveLayer);
   end;
end;{TFreeSubdivisionLayer.FSetColor}

procedure TFreeSubdivisionLayer.FSetShowInLinesplan(val:boolean);
begin
   if Val<>FShowInLinesplan then
   begin
      FShowInLinesplan:=val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetShowInLinesplan}

procedure TFreeSubdivisionLayer.FSetUseInHydrostatics(val:boolean);
begin
   if val<>FUseInHydrostatics then
   begin
      FUseInHydrostatics:=Val;
      if FUseInHydrostatics and (not FSymmetric) then FSymmetric:=true;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetUseInHydrostatics}

procedure TFreeSubdivisionLayer.FSetUseForIntersections(val:Boolean);
begin
   if val<>FUseForIntersections then
   begin
      FUseForIntersections:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetUseForIntersections}

procedure TFreeSubdivisionLayer.FSetVisible(Val:Boolean);
begin
   if Val<>FVisible then
   begin
      FVisible:=Val;
      if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   end;
end;{TFreeSubdivisionLayer.FSetVisible}

procedure TFreeSubdivisionLayer.AddControlFace(ControlFace:TFreeSubdivisionControlFace);
begin
   // disconnect from current layer
   if ControlFace.Layer<>nil then ControlFace.Layer.DeleteControlFace(ControlFace);
   if FPatches.Indexof(ControlFace)=-1 then FPatches.Add(ControlFace);
   ControlFace.FLayer:=self;
end;{TFreeSubdivisionLayer.AddPatch}

procedure TFreeSubdivisionLayer.AssignProperties(Source:TFreeSubdivisionLayer);
begin
   FColor:=Source.FColor;
   FVisible:=True;
   FDescription:='';
   FSymmetric:=Source.FSymmetric;
   FDevelopable:=Source.FDevelopable;
   FMaterialDensity:=Source.FMaterialDensity;
   FThickness:=Source.FThickness;
end;{TFreeSubdivisionLayer.AssignProperties}

function ToStr(c:T3DCoordinate):String;
begin
   result:='('+FloatToStr(c.X)+','+FloatToStr(c.Y)+','+FloatToStr(c.Z)+')';
end;

procedure TFreeSubdivisionLayer.FindEdgeFaceIntersectionPoints(
  Layer:TFreeSubdivisionLayer;
  var NewEdge : TFreeSubdivisionControlEdge;
  var NewPoints: TFasterList);
var I,J,K,L    : Integer;
    Edges      : TFasterlist;
    P1,P2      : TFreeSubdivisionPoint;
    P          : TFreeSubdivisionControlPoint;
    Edge       : TFreeSubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    Child      : TFreeSubdivisionFace;
    IntFound   : Boolean;
    Inserted   : Boolean;
    Plane      : T3DPlane;
    S1,S2,T    : TFloatType;
    P2D,P3D,esp,eep, CP0,CP2,CP1 : T3DCoordinate;
    PP3D : ^T3DCoordinate;
    PiT,esl,psl : Boolean;
    label StartAgain;
begin
   Edges:=TFasterList.Create;
   try
      StartAgain:
      // assemble all controledges in a list
      for I:=1 to Layer.Count do
      begin
         Face:=Layer.Items[I-1];
         P1:=Face.Point[Face.NumberOfPoints-1];
         for J:=1 to Face.NumberOfpoints do
         begin
            P2:=Face.Point[J-1];
            Edge:=Owner.EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
            if Edge<>nil then begin
              if Edges.SortedIndexOf(Edge)=-1
                then Edges.AddSorted(Edge);
            end;
            P1:=P2;
         end;
      end;

      // now check all edges for intersection with layer 2
      I:=1;
      while I<=Edges.Count do
      begin
         Edge:=Edges[I-1];
         IntFound:=False;
         esp := Edge.FStartpoint.FCoordinate;
         eep := Edge.FEndpoint.FCoordinate;
         J:=1;
         while (J<=NewEdge.FFaces.Count) and (not IntFound) do
         begin
            Face:=TFreeSubdivisionControlFace(NewEdge.Face[J-1]);
            esl := Edge.Selected;
            psl := Face.Selected;
            if esl then
             if psl
               then T:=0;
            K:=1;
            while (K<=Face.ChildCount) and (not IntFound) do
            begin
               Child:=Face.Child[K-1];
               L:=3;
               while (L<=Child.NumberOfpoints) and (not IntFound) do
               begin
                 CP0 := Child.Point[0].Coordinate;
                 CP2 := Child.Point[L-2].Coordinate;
                 CP1 := Child.Point[L-1].Coordinate;

                 Plane:=PlanePPP(CP0,CP2,CP1);

                 S1:= Plane.a*esp.X
                     +Plane.b*esp.Y
                     +Plane.c*esp.Z
                     +Plane.d;
                 S2:= Plane.a*eep.X
                     +Plane.b*eep.Y
                     +Plane.c*eep.Z
                     +Plane.d;
                 if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
                  begin
                     // Edge intersects the plane, does it lie in the triangle?
                     if S1=S2 then T:=0.5
                              else T:=-s1/(s2-s1);
                     P3D.X:=esp.X+T*(eep.X-esp.X);
                     P3D.Y:=esp.Y+T*(eep.Y-esp.Y);
                     P3D.Z:=esp.Z+T*(eep.Z-esp.Z);

                     PiT := false; //PIT0:=false;
                     //if PointInBlock(P3D, Face.Min, Face.Max) then
                        begin
                        PiT:=PointInTriangleBarycentric(P3D, CP0,CP2,CP1);
                        //PiT:=PointInTriangle(P3D, CP0,CP2,CP1);
                        end;
                     //if Edge.Selected or Face.Selected then
                       //write('P3D:',ToStr(P3D), ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1));
                     //if PIT<>PIT0
                        //then writeln ('PIT<>PIT0');
                     if PiT then
                     begin
                        //if Edge.Selected or Face.Selected then
                        //   write(' :IN');
                        // Yes, we have a valid intersection here
                        //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. Edge:',I,' FaceChild:',K,' Point:',L);
                        //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. P3D(',P3D.X,',',P3D.Y,',',P3D.Z,')');

                        //P := TFreeSubdivisionControlPoint.Create(Owner);
                        //if (P<>nil) then
                          begin
                          //P.FSetCoordinate(P3D);
                          New(PP3D);
                          PP3D^.X:=P3D.X; PP3D^.Y:=P3D.Y; PP3D^.Z:=P3D.Z;
                          NewPoints.Add(PP3D);
                          end;

                     end;
                     //if Edge.Selected or Face.Selected then
                        //writeln();
                  end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
                 inc(L);
               end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
               inc(K);
            end; //while (K<=Face.ChildCount) and (not IntFound) do
            inc(J);
         end; //while (J<=Layer.Count) and (not IntFound) do
         inc(I);
      end; //while I<=Edges.Count do
   finally
      Edges.Destroy;
   end;
end;

// find all intersection points of edges of the Self with subfaces of Layer
procedure TFreeSubdivisionLayer.FindIntersectionPoints(
  Layer:TFreeSubdivisionLayer;
  var NewPoints: TFasterList);
var I,J,K,L    : Integer;
    Edges      : TFasterlist;
    P1,P2      : TFreeSubdivisionPoint;
    P          : TFreeSubdivisionControlpoint;
    Edge       : TFreeSubdivisionControlEdge;
    Face       : TFreeSubdivisionControlface;
    Child      : TFreeSubdivisionFace;
    IntFound   : Boolean;
    Inserted   : Boolean;
    Plane      : T3DPlane;
    S1,S2,T    : TFloatType;
    P2D,P3D,esp,eep, CP0,CP2,CP1 : T3DCoordinate;
    PiT : Boolean;
    label StartAgain;
begin
   Edges:=TFasterList.Create;
   try
      StartAgain:
      // assemble all controledges in a list
      for I:=1 to Count do
      begin
         Face:=Items[I-1];
         P1:=Face.Point[Face.NumberOfPoints-1];
         for J:=1 to Face.NumberOfpoints do
         begin
            P2:=Face.Point[J-1];
            Edge:=Owner.EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
            if Edge<>nil then begin
              if Edges.SortedIndexOf(Edge)=-1
                then Edges.AddSorted(Edge);
            end;
            P1:=P2;
         end;
      end;

      // now check all edges for intersection with layer 2
      I:=1;
      while I<=Edges.Count do
      begin
         Edge:=Edges[I-1];
         IntFound:=False;
         esp := Edge.FStartpoint.FCoordinate;
         eep := Edge.FEndpoint.FCoordinate;
         J:=1;
         while (J<=Layer.Count) and (not IntFound) do
         begin
            Face:=Layer.Items[J-1];
            K:=1;
            while (K<=Face.ChildCount) and (not IntFound) do
            begin
               Child:=Face.Child[K-1];
               L:=3;
               while (L<=Child.NumberOfpoints) and (not IntFound) do
               begin
                 CP0 := Child.Point[0].Coordinate;
                 CP2 := Child.Point[L-2].Coordinate;
                 CP1 := Child.Point[L-1].Coordinate;

                 Plane:=PlanePPP(CP0,CP2,CP1);

                 S1:= Plane.a*esp.X
                     +Plane.b*esp.Y
                     +Plane.c*esp.Z
                     +Plane.d;
                 S2:= Plane.a*eep.X
                     +Plane.b*eep.Y
                     +Plane.c*eep.Z
                     +Plane.d;
                 if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
                  begin
                     // Edge intersects the plane, does it lie in the triangle?
                     if S1=S2 then T:=0.5
                              else T:=-s1/(s2-s1);
                     P3D.X:=esp.X+T*(eep.X-esp.X);
                     P3D.Y:=esp.Y+T*(eep.Y-esp.Y);
                     P3D.Z:=esp.Z+T*(eep.Z-esp.Z);

                     PiT := false; //PIT0:=false;
                     //Face.CalcExtents;
                     //if PointInBlock(P3D, Face.Min, Face.Max) then
                        begin
                        PiT:=PointInTriangleBarycentric(P3D, CP0,CP2,CP1);
                        //PiT:=PointInTriangle(P3D, CP0,CP2,CP1);
                        end;
                     //if Edge.Selected or Face.Selected then
                       //write('P3D:',ToStr(P3D), ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1));
                     //if PIT<>PIT0
                        //then writeln ('PIT<>PIT0');
                     if PiT then
                     begin
                        //if Edge.Selected or Face.Selected then
                        //   write(' :IN');
                        // Yes, we have a valid intersection here
                        //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. Edge:',I,' FaceChild:',K,' Point:',L);
                        //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. P3D(',P3D.X,',',P3D.Y,',',P3D.Z,')');
                        P:=Edge.InsertControlPoint(P3D);
                        if P<>nil then
                        begin
                           //IntFound:=true;  //MM:I do not know why to stop searching for all intersections ?
                           //Result:=True;
                           P.Selected:=true;
                           NewPoints.Add(P);
                           goto StartAgain;
                        end;
                     end;
                     //if Edge.Selected or Face.Selected then
                        //writeln();
                  end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
                  inc(L);
               end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
               inc(K);
            end; //while (K<=Face.ChildCount) and (not IntFound) do
            inc(J);
         end; //while (J<=Layer.Count) and (not IntFound) do
         inc(I);
      end; //while I<=Edges.Count do
   finally
      Edges.Destroy;
   end;
end;

// find all intersection control points of edges of the Self with subfaces of Layer
procedure TFreeSubdivisionLayer.FindIntersectionControlPoints(
  Layer:TFreeSubdivisionLayer;
  var NewPoints: TFasterList);
var I,J,K,L    : Integer;
    Edges      : TFasterlist;
    P1,P2      : TFreeSubdivisionPoint;
    P          : TFreeSubdivisionControlPoint;
    Edge       : TFreeSubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    IntFound   : Boolean;
    Inserted   : Boolean;
    Plane      : T3DPlane;
    S1,S2,T    : TFloatType;
    P2D,P3D,esp,eep, CP0,CP2,CP1 : T3DCoordinate;
    PiT : Boolean;
    label StartAgain;
begin
   Edges:=TFasterList.Create;
   try
      StartAgain:
      // assemble all controledges in a list
      for I:=1 to Count do
      begin
         Face:=Items[I-1];
         P1:=Face.Point[Face.NumberOfPoints-1];
         for J:=1 to Face.NumberOfpoints do
         begin
            P2:=Face.Point[J-1];
            Edge:=Owner.EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
            if Edge<>nil then begin
              if Edges.SortedIndexOf(Edge)=-1
                then Edges.AddSorted(Edge);
            end;
            P1:=P2;
         end;
      end;

      // now check all edges for intersection with layer 2
      I:=1;
      while I<=Edges.Count do
      begin
         Edge:=Edges[I-1];
         IntFound:=False;
         esp := Edge.FStartpoint.FCoordinate;
         eep := Edge.FEndpoint.FCoordinate;
         J:=1;
         while (J<=Layer.Count) and (not IntFound) do
         begin
           Face:=Layer.Items[J-1];
           L:=2;
           while (L<Face.NumberOfPoints) and (not IntFound) do
           begin
             CP0 := Face.Point[0].Coordinate;
             CP2 := Face.Point[L-1].Coordinate;
             CP1 := Face.Point[L].Coordinate;

             Plane:=PlanePPP(CP0,CP2,CP1);

             S1:= Plane.a*esp.X + Plane.b*esp.Y + Plane.c*esp.Z + Plane.d;
             S2:= Plane.a*eep.X + Plane.b*eep.Y + Plane.c*eep.Z + Plane.d;
             if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
              begin
                 // Edge intersects the plane, does it lie in the triangle?
                 if S1=S2 then T:=0.5 else T:=-s1/(s2-s1);
                 P3D.X:=esp.X+T*(eep.X-esp.X);
                 P3D.Y:=esp.Y+T*(eep.Y-esp.Y);
                 P3D.Z:=esp.Z+T*(eep.Z-esp.Z);

                 PiT := false; //PIT0:=false;
                 if 1=1//PointInBlock(P3D, Face.Min, Face.Max)
                    then begin
                    //TFreeSubdivisionControlPoint(Face.Point[0]).Selected:=true;
                    //TFreeSubdivisionControlPoint(Face.Point[L-1]).Selected:=true;
                    //TFreeSubdivisionControlPoint(Face.Point[L]).Selected:=true;
                    PiT:=PointInTriangleBarycentric(P3D, CP0,CP2,CP1);
                    //if PiT then writeln('P3D:',ToStr(P3D),' L:',L, ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1),' Face.p#:',Face.NumberOfPoints);
                    //if PiT then writeln(' :IN') else writeln(' :out');
                    end;
                 if PiT then
                 begin
                    P:=Edge.InsertControlPoint(P3D);
                    if P<>nil then
                    begin
                       P.Selected:=true;
                       NewPoints.Add(P);
                       goto StartAgain;
                    end;
                 end;
              end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
              inc(L);
           end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
           inc(J);
         end; //while (J<=Layer.Count) and (not IntFound) do
         inc(I);
      end; //while I<=Edges.Count do
   finally
      Edges.Destroy;
   end;
end;

procedure Sort3DPoints(var Points:TFasterList);
var I,J:integer;
    P1,P2      : T3DCoordinate;
    //SP:TFreeSubdivisionPoint;
begin
  for I:=0 to Points.Count-1 do
    for J:=I+1 to Points.Count-1 do
      begin
        P1:=TFreeSubdivisionPoint(Points[I]).Coordinate;
        P2:=TFreeSubdivisionPoint(Points[J]).Coordinate;
        if P1.X > P2.X
          then Points.Exchange(I,J)
          else if (P1.X = P2.X) and (P1.Y > P2.Y)
            then Points.Exchange(I,J)
            else if (P1.X = P2.X) and (P1.Y = P2.Y) and (P1.Z > P2.Z)
              then Points.Exchange(I,J);
      end;
end;

{
The two lines may not meet at a single point.
The best you can do in general is find the point on line1 closest to line2 and vise versa.
Connect those two points to create the common normal direction.
Given two lines passing through 3D points r1=[r1x,r1y,r1z] and r2=[r2x,r2y,r2z]
and having unit directions e1=[e1x,e1y,e1z] and e2=[e2x,e2y,e2z]
you can find the points on the line which are closest to the other line like this:

    Find the direction projection u=Dot(e1,e2)=e1x*e2x+e1y*e2y+e1z*e2z
    If u==1 then lines are parallel. No intersection exists.
    Find the separation projections t1=Dot(r2-r1,e1) and t2=Dot(r2-r1,e2)
    Find distance along line1 d1 = (t1-u*t2)/(1-u*u)
    Find distance along line2 d2 = (t2-u*t1)/(u*u-1)
    Find the point on line1 p1=Add(r1,Scale(d1,e1))
    Find the point on line2 p2=Add(r2,Scale(d2,e2))
}
function LineIntersection(P1b,P1e, P2b, P2e : T3DCoordinate): T3DCoordinate;
begin
  result := ZERO;
end; //LineIntersection


function TFreeSubdivisionLayer.CalculateIntersectionPoints(Layer:TFreeSubdivisionLayer):Boolean;
var I,J,K,L,M    : Integer;
    Edges      : TFasterlist;
    NewPoints,EdgeNewPoints  : TFasterList;
    P1,P2      : TFreeSubdivisionPoint;
    P          : TFreeSubdivisionControlPoint;
    Edge       : TFreeSubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    Child      : TFreeSubdivisionFace;
    IntFound   : Boolean;
    Inserted   : Boolean;
    Plane      : T3DPlane;
    S1,S2,T    : TFloatType;
    P2D,P3D,esp,eep, CP0,CP2,CP1 : T3DCoordinate;
    PP3D : ^T3DCoordinate;
    PiT : Boolean;
    //label StartAgan;
begin
   Result:=False;
   Edges:=TFasterList.Create;
   NewPoints:=TFasterList.Create;
   EdgeNewPoints:=TFasterList.Create;
   try
      // find all intersection points of control edges of the Self with subdivision faces of Layer
     FindIntersectionPoints(Layer, NewPoints);
     // find all intersection points of control edges of the Self with control faces of Layer
     // FindIntersectionControlPoints(Layer, NewPoints);
     // find all intersection points of control edges of Layer with control faces of the Self
     // Layer.FindIntersectionControlPoints(Self, NewPoints);

     if NewPoints.Count>0 then
      begin
         // Try to find multiple new points belonging to the same face and insert an edge
         I:=0;
         Sort3DPoints(NewPoints);
         Edges.Clear;
         while I < NewPoints.Count do
         begin
            P1:=NewPoints[I];
            J:=0;
            while J < P1.NumberOfFaces do
            begin
               Face:=P1.Face[J] as TFreeSubdivisionControlFace;
               K:=0;
               Inserted:=False;
               while (K < Face.NumberOfPoints) and (Not Inserted) do
               begin
                  P2:=Face.Point[K] as TFreeSubdivisionControlPoint;
                  if (P1<>P2) and (NewPoints.IndexOf(P2)<>-1) then
                  begin
                     // this is also a new point, first check if an edge already exists between P1 and P2
                     if Owner.EdgeExists(P1,P2)=nil then
                     begin
                        Inserted:=True;
                        // split the Face into two new ones. The Face is placed into DelayedDestroyList
                        Edge:=Face.InsertEdge(P1 as TFreesubdivisionControlPoint,
                                              P2 as TFreesubdivisionControlPoint);
                        Edge.Selected:=True;
                        for L:=0 to Edge.FFaces.Count-1 do
                          TFreeSubdivisionControlFace(Edge.Face[L]).Selected:=true;
                        //Face.Selected:=True;
                        //Edges.Add(Edge); // what for ?
                        //TODO: For Intersect faces - find intersections of Edge's faces with Layer's dubdiv edges and insert points into Self edges.

                        Result := true;
                     end;
                  end;
                  inc(K);
               end; //while (K<=Face.NumberOfPoints-1) and (Not Inserted) do
               if not Inserted then inc(J);
            end; //while J<=P1.NumberOfFaces-1 do
            DelayedDestroyList.DestroyAll;
            inc(I);
         end; //while I<=NewPoints.Count-1 do
      end; //if NewPoints.Count>0 then

     //TODO: For Intersect Controlfaces - Merge points with same XYZ
     {
     StartAgan:

     Owner.Rebuild;
     I:=0;
     while I < Edges.Count do
       begin
       Edge := Edges[I];
       //get an existing Edge just in case the Edge from Edges has been rebuilt
       Edge := Owner.EdgeExists( Edge.StartPoint, Edge.EndPoint ) as TFreeSubdivisionControlEdge;
       if Edge <> nil then
         begin
         EdgeNewPoints.Clear;
         FindEdgeFaceIntersectionPoints(Layer,Edge,EdgeNewPoints);
         for M:=0 to EdgeNewPoints.Count-1 do
           begin
             PP3D:=EdgeNewPoints[M];
             P:=Owner.AddControlPoint(PP3D^);
             P.Selected:=true;
             //P:=Edge.InsertControlPoint( PP3D^ );
             P:=nil;
             Dispose(PP3D);
             EdgeNewPoints[M]:=nil;
             if P<>nil then
              begin
                if Edge.NextEdge <> nil then
                  Edges.Add(Edge.NextEdge);
                goto StartAgan;
              end;
           end;
         end;
         inc(I);
       end;
      }
   finally
     NewPoints.Destroy;
     Edges.Destroy;
     EdgeNewPoints.Destroy;
   end;
end;{TFreeSubdivisionLayer.CalculateIntersectionPoints}

constructor TFreeSubdivisionLayer.Create(Owner:TFreeSubdivisionSurface);
begin
   inherited Create;
   FOwner:=Owner;
   FPatches:=TFasterList.Create;
   Clear;
end;{TFreeSubdivisionLayer.Create}

procedure TFreeSubdivisionLayer.Clear;
begin
   FLayerID:=-1;
   FPatches.Clear;
   FColor:=Owner.LayerColor;
   FVisible:=True;
   FDescription:='';
   FSymmetric:=True;
   FDevelopable:=False;
   FUseForIntersections:=True;
   FUseInHydrostatics:=True;
   FShowInLinesplan:=True;
   FMaterialDensity:=0.0;
   FThickness:=0.0;
   FAlphaBlend:=255;
end;{TFreeSubdivisionLayer.Clear}

function TFreeSubdivisionLayer.Delete:Boolean;
var I     : Integer;
    Index : Integer;
begin
   Result:=True;
   for I:=Count downto 1 do Items[I-1].Delete;
   if Owner.FActiveLayer=self then Owner.FActiveLayer:=nil;
   Index:=LayerIndex;
   if Index<>-1 then Owner.FLayers.Delete(Index);
   Clear;
   if assigned(Owner.FOnChangeLayerData) then Owner.FOnChangeLayerData(self);
   Destroy;
end;{TFreeSubdivisionLayer.Delete}

procedure TFreeSubdivisionLayer.DeleteControlFace(ControlFace:TFreeSubdivisionControlFace);
var Index : Integer;
begin
   Index:=FPatches.IndexOf(ControlFace);
   if index<>-1 then FPatches.Delete(Index);
end;{TFreeSubdivisionLayer.DeleteControlFace}

destructor TFreeSubdivisionLayer.Destroy;
begin
   FPatches.Destroy;
   FPatches:=nil;
   Inherited Destroy;
end;{TFreeSubdivisionLayer.Destroy}

procedure TFreeSubdivisionLayer.Draw(Viewport:TFreeViewport);
var I,J  : Integer;
    Face : TFreeSubdivisionControlface;
    Edge : TFreeSubdivisionEdge;
begin
   if Visible and (Count>0) then
   begin
      if Viewport.ViewportMode<>vmWireframe then
      begin
         if Viewport.ViewportMode=vmShadeGauss
            then
              for I:=1 to Count do
                Items[I-1].Draw(Viewport,Owner.FMinGaussCurvature,Owner.FMaxGaussCurvature)
            else
              for I:=1 to count do
                Items[I-1].Draw(Viewport);
      end else
      begin
         if Owner.ShowInteriorEdges then
         begin
            Viewport.SetPenWidth(1);
            Viewport.PenColor:=Color;
            For I:=1 to Count do Items[I-1].Draw(Viewport);
         end;
         // Draw all interior crease-edges
         Viewport.SetPenWidth(1);
         Viewport.PenStyle:=psSolid;
         Viewport.PenColor:=Owner.CreaseColor;
         For I:=1 to Count do
         begin
            Face:=Items[I-1];
            for J:=1 to Face.FControlEdges.Count do
            begin
               Edge:=Face.FControlEdges[J-1];
               if Edge.FCrease then
               begin
                  Edge.Draw(Owner.DrawMirror and Symmetric,Viewport);
               end;
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionLayer.Draw}

procedure TFreeSubdivisionLayer.Extents(var Min,Max:T3DCoordinate);
var I    : Integer;
    Face : TFreeSubdivisionControlface;
    P    : T3DCoordinate;
begin
   if Visible then for I:=1 to Count do
   begin
      Face:=Items[I-1];
      MinMax(Face.Min,Min,Max);
      MinMax(Face.Max,Min,Max);
      if (Symmetric) and (Owner.DrawMirror) then
      begin
         P:=Face.Min;
         P.Y:=-P.Y;
         MinMax(P,Min,Max);
         P:=Face.Max;
         P.Y:=-P.Y;
         MinMax(P,Min,Max);
      end;
   end;
end;{TFreeSubdivisionLayer.Extents}

procedure TFreeSubdivisionLayer.LoadBinary(Source:TFreeFileBuffer);
var I:Integer;
begin
   Source.Load(FDescription);
   Source.Load(FLayerID);
   if FLayerID>Owner.FLastusedLayerID then Owner.FLastusedLayerID:=FLayerID;
   Source.Load(FColor);
   Source.Load(FVisible);
   Source.Load(FSymmetric);
   Source.Load(FDevelopable);
   FMaterialDensity:=0.0;
   FThickness:=0.0;
   FUseForIntersections:=True;
   FUseInHydrostatics:=True;
   FShowInLinesplan:=True;
   if Source.Version>=fv180 then
   begin
      Source.Load(FUseForIntersections);
      Source.Load(FUseInHydrostatics);
      if Source.Version>=fv191 then
      begin
         Source.Load(FMaterialDensity);
         Source.Load(FThickness);
         if Source.Version>=fv201 then
         begin
            Source.Load(FShowInLinesplan);
            if Source.Version>=fv260 then
            begin
               Source.Load(I);
               FalphaBlend:=I;
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionLayer.LoadBinary}

procedure TFreeSubdivisionLayer.LoadFromStream(var LineNr:Integer;Strings:TStringList);
var Str     : string;
begin
   // read description
   inc(LineNr);
   FDescription:=Strings[LineNr];
   // read Layer identification
   inc(LineNr);
   Str:=Strings[LineNr];
   FLayerID:=ReadIntFromStr(LineNr,Str);
   if FLayerID>Owner.FLastusedLayerID then Owner.FLastusedLayerID:=FLayerID;
   // read color
   FColor:=ReadIntFromStr(LineNr,Str);
   // read visible
   FVisible:=ReadBoolFromStr(LineNr,Str);
   FSymmetric:=ReadBoolFromStr(LineNr,Str);
   // read developability
   if Str<>'' then FDevelopable:=ReadBoolFromStr(LineNr,Str)
              else FDevelopable:=False;
   // read calc. intersections flag
   if Str<>'' then FUseForIntersections:=ReadBoolFromStr(LineNr,Str)
              else FUseForIntersections:=True;
   // read use in hydrostatics flag
   if Str<>'' then FUseInHydrostatics:=ReadBoolFromStr(LineNr,Str)
              else FUseInHydrostatics:=True;
end;{TFreeSubdivisionLayer.LoadFromStream}

procedure TFreeSubdivisionLayer.MoveDown;
var Index:Integer;
begin
   Index:=Owner.FLayers.IndexOf(self);
   if index<Owner.Flayers.Count-1 then Owner.FLayers.Exchange(Index+1,index);
end;{TFreeSubdivisionLayer.MoveDown}

procedure TFreeSubdivisionLayer.MoveUp;
var Index:Integer;
begin
   Index:=Owner.FLayers.IndexOf(self);
   if index>0 then Owner.FLayers.Exchange(Index-1,index);
end;{TFreeSubdivisionLayer.MoveUp}

procedure TFreeSubdivisionLayer.SaveToDXF(Strings:TStringList);
var Layers        : TFasterList;
    Assembled     : TFreeFaceArray;
    NAssembled    : Integer;
    I,J,K         : Integer;
    Cols,Rows     : Integer;
    AssFace       : TFreeFaceGrid;
    Face          : TFreeSubdivisionControlFace;
    Grid          : TFreeSubdivisionGrid;
    P             : T3DCoordinate;
    DXFName       : string;

begin
   if visible then
   begin
      Layers:=TFasterList.Create;
      Layers.Add(self);
      Owner.AssembleFacesToPatches(Layers,amRegular,Assembled,NAssembled);
      if NAssembled>0 then
      begin
         // assign all patches to new layers
         for I:=1 to NAssembled do
         begin
            AssFace:=Assembled[I-1];
            if ((AssFace.NCols>1) and (AssFace.NRows>=1)) or
               ((AssFace.NCols>=1) and (AssFace.NRows>1)) or
               ((AssFace.NCols=1) and (AssFace.NRows=1) and (AssFace.Faces[0][0].NumberOfPoints=4)) then
            begin
               Owner.ConvertToGrid(AssFace,Cols,Rows,Grid);
               if (Cols>0) and (Rows>0) then
               begin
                  DXFName:=DXFLayerName;
                  Strings.Add('0'+EOL+'POLYLINE');
                  Strings.Add('8'+EOL+DXFName);
                  Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(color)));
                  Strings.Add('66'+EOL+'1');
                  Strings.Add('70'+EOL+'16');
                  Strings.Add('71'+EOL+IntToStr(Rows));
                  Strings.Add('72'+EOL+IntToStr(Cols));
                  for J:=1 to Rows do
                  begin
                     for K:=1 to Cols do
                     begin
                        P:=Grid[J-1][K-1].FCoordinate;
                        Strings.Add('0'+EOL+'VERTEX');
                        Strings.Add('8'+EOL+DXFName);
                        Strings.Add('10'+EOL+Truncate(P.X,4));
                        Strings.Add('20'+EOL+Truncate(P.Y,4));
                        Strings.Add('30'+EOL+Truncate(P.Z,4));
                        Strings.Add('70'+EOL+'64');    // polygon mesh vertex
                     end;
                  end;
                  Strings.add('0'+EOL+'SEQEND');
                  if (Symmetric) and (Owner.DrawMirror) then
                  begin
                     Strings.Add('0'+EOL+'POLYLINE');
                     Strings.Add('8'+EOL+DXFName);
                     Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(color)));
                     Strings.Add('66'+EOL+'1');
                     Strings.Add('70'+EOL+'16');
                     Strings.Add('71'+EOL+IntToStr(Rows));
                     Strings.Add('72'+EOL+IntToStr(Cols));
                     for J:=1 to Rows do
                     begin
                        for K:=1 to Cols do
                        begin
                           P:=Grid[J-1][K-1].FCoordinate;
                           Strings.Add('0'+EOL+'VERTEX');
                           Strings.Add('8'+EOL+DXFName);
                           Strings.Add('10'+EOL+Truncate(P.X,4));
                           Strings.Add('20'+EOL+Truncate(-P.Y,4));
                           Strings.Add('30'+EOL+Truncate(P.Z,4));
                           Strings.Add('70'+EOL+'64');    // polygon mesh vertex
                        end;
                     end;
                     Strings.add('0'+EOL+'SEQEND');
                  end;
               end else
               begin
                  for J:=1 to AssFace.NRows do
                  begin
                     for K:=1 to AssFace.NCols do
                     begin
                        Face:=AssFace.Faces[J-1][K-1];
                        if Face<>nil then Face.SaveToDXF(Strings);
                     end;
                  end;
               end;
            end else if (AssFace.NCols=1) and (AssFace.NRows=1) then
            begin
               Face:=AssFace.Faces[0][0];
               Face.SaveToDXF(Strings);
            end;
         end;
      end;
      Layers.Destroy;
   end;
end;{TFreeSubdivisionLayer.SaveToDXF}

procedure TFreeSubdivisionLayer.SaveBinary(Destination:TFreeFileBuffer);
begin
   Destination.Add(FDescription);
   Destination.Add(FLayerID);
   Destination.Add(FColor);
   Destination.Add(FVisible);
   Destination.Add(FSymmetric);
   Destination.Add(FDevelopable);
   if Destination.Version>=fv180 then
   begin
      Destination.Add(FUseForIntersections);
      Destination.Add(FUseInHydrostatics);
      if Destination.Version>=fv191 then
      begin
         Destination.Add(FMaterialDensity);
         Destination.Add(FThickness);
         if Destination.Version>=fv201 then
         begin
            Destination.Add(FShowInLinesplan);
            if Destination.Version>=fv260 then
            begin
               Destination.Add(FalphaBlend);
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionLayer.SaveBinary}

procedure TFreeSubdivisionLayer.SaveToStream(Strings:TStringList);
var Str   : string;
begin
   Strings.Add(FDescription);
   Str:=IntToStr(FLayerID)+#32+
        IntToStr(FColor)+#32+
        BoolToStr(FVisible)+#32+
        BoolToStr(FSymmetric)+#32+
        BoolToStr(FDevelopable);
   Str:=Str+#32+BoolToStr(FUseForIntersections)+#32+BoolToStr(FUseInHydrostatics);
   Strings.Add(Str);
end;{TFreeSubdivisionLayer.SaveToStream}

procedure TFreeSubdivisionLayer.SelectAll;
var I: integer;
begin
for i:=0 to Count-1 do
  Items[i].Selected:=True;
end;

procedure TFreeSubdivisionLayer.Unroll(Destination:TFasterList);
var ToDoList   : TFasterList;
    DoneList   : TFasterList;
    Current    : TFasterList;
    I          : Integer;
    Face       : TFreeSubdivisionControlFace;
    Patch,Copy : TFreeDevelopedPatch;
    Str        : string;

    procedure FindAttachedFaces(List:TFasterList;Face:TFreeSubdivisionControlFace);
    var I,J    : Integer;
        Index  : Integer;
        P1,P2  : TFreeSubdivisionPoint;
        Edge   : TFreeSubdivisionEdge;
    begin
       P1:=Face.Point[Face.NumberOfPoints-1];
       for I:=1 to Face.NumberOfpoints do
       begin
          P2:=Face.Point[I-1];
          Edge:=Face.Owner.EdgeExists(P1,P2);
          if Edge<>nil then
          begin
             for J:=1 to Edge.NumberOfFaces do if Edge.Face[J-1]<>Face then
             begin
                Index:=ToDoList.IndexOf(Edge.Face[J-1]);
                if Index<>-1 then
                begin
                   List.Add(Edge.Face[J-1]);
                   ToDoList.Delete(Index);
                   FindAttachedFaces(List,Edge.Face[J-1] as TFreeSubdivisionControlFace);
                end;
             end;
          end;
          P1:=p2;
       end;
    end;{FindAttachedFaces}

begin

   ToDoList:=TFasterList.Create;
   DoneList:=TFasterList.Create;
   try
      ToDoList.Capacity:=FPatches.Count;
      ToDoList.AddList(FPatches);
      if ToDoList.Count>0 then
      begin
         while ToDoList.Count>0 do
         begin
            Face:=ToDoList[ToDoList.Count-1];
            ToDoList.Delete(ToDoList.Count-1);
            Current:=TFasterList.Create;
            Current.Add(Face);
            FindAttachedFaces(Current,Face);
            DoneList.Add(Current);
         end;
         // Unroll each separate surface area
         for I:=1 to DoneList.Count do
         begin
            Current:=DoneList[I-1];
            if Current.Count>0 then
            begin
               Patch:=TFreeDevelopedPatch.Create(self);
               Patch.Unroll(Current);
               if DoneList.Count=1 then Str:=Name
                                   else Str:=Name+#32+Lowercase(Userstring(198))+#32+IntToStr(I);
               Patch.Name:=Str;
               Destination.Add(Patch);
               if (not Patch.FMirror) and (self.Symmetric) then
               begin
                  // Create the starboard half
                  Patch.Name:=Str+' (SB)';
                  Copy:=TFreeDevelopedPatch.Create(self);
                  Copy.Assign(Patch,True);
                  Copy.Name:=Str+' (P)';
                  Destination.Add(Copy);
               end;
            end;
            Current.Destroy;
         end;
      end;
   finally
      ToDoList.Destroy;
      DoneList.Destroy;
   end;
end;{TFreeSubdivisionLayer.Unroll}

{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionPoint                                    }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionPoint.FGetEdge(Index:Integer):TFreeSubdivisionEdge;
begin
   Result:=FEdges[Index];
end;{TFreeSubdivisionPoint.FGetEdge}

function TFreeSubdivisionPoint.FGetCoordinate:T3DCoordinate;
begin
   Result:=FCoordinate;
end;{TFreeSubdivisionPoint.FGetCoordinate}

function TFreeSubdivisionPoint.FGetCurvature:extended;
var I          : Integer;
    Index      : Integer;
    PrevIndex  : Integer;
    NextIndex  : Integer;
    Prev,Next  : TFreeSubdivisionPoint;
    Face       : TFreeSubdivisionface;
    Sigma,Tmp  : extended;

   function Angle_VV_3D(P1,P2,P3:T3DCoordinate):extended;
   var V1X,V1Y,V1Z : extended;
       V2X,V2Y,V2Z : extended;
       L           : extended;
   begin
      V1X:=P1.X-P2.X;
      V1Y:=P1.Y-P2.Y;
      V1Z:=P1.Z-P2.Z;
      V2X:=P3.X-P2.X;
      V2Y:=P3.Y-P2.Y;
      V2Z:=P3.Z-P2.Z;
      L:=Sqrt((V1X*V1X)+(V1Y*V1Y)+(V1Z*V1Z));
      if L<>0 then
      begin
         V1X:=V1X/L;
         V1Y:=V1Y/L;
         V1Z:=V1Z/L;
      end;
      L:=Sqrt((V2X*V2X)+(V2Y*V2Y)+(V2Z*V2Z));
      if L<>0 then
      begin
         V2X:=V2X/L;
         V2Y:=V2Y/L;
         V2Z:=V2Z/L;
      end;
      L:=(V1X*V2X)+(V1Y*V2Y)+(V1Z*V2Z);
      if L<-1 then L:=-1 else if L>1 then L:=1;
      Result:=ArcCos(L);
   end;{Angle_VV_3D}

begin
   Result:=0.0;
   for I:=1 to FEdges.Count do If Edge[I-1].NumberOfFaces<2 then exit;
//   if VertexType in [svRegular,svDart] then
   begin
      Sigma:=0;
      for I:=1 to NumberOfFaces do
      begin
         Face:=FFaces[I-1];
         Index:=Face.FPoints.IndexOf(self);
         PrevIndex:=(index+(Face.FPoints.Count-1)) mod Face.FPoints.Count;
         NextIndex:=(PrevIndex+2) mod Face.FPoints.Count;
         Prev:=Face.FPoints[PrevIndex];
         Next:=Face.FPoints[NextIndex];
         Tmp:=Angle_VV_3D(Prev.Coordinate,Self.Coordinate,Next.Coordinate);
         Sigma:=Sigma+Tmp;
      end;
      Result:=(360-RadToDeg(sigma));
   end;// else Result:=0.0;
end;{TFreeSubdivisionPoint.FGetCurvature}

function TFreeSubdivisionPoint.FGetFace(Index:Integer):TFreeSubdivisionFace;
begin
   Result:=FFaces[Index];
end;{TFreeSubdivisionPoint.FGetFace}

function TFreeSubdivisionPoint.FGetIndex:Integer;
begin
   Result:=Owner.FPoints.IndexOf(Self);
end;{TFreeSubdivisionPoint.FGetIndex}

function TFreeSubdivisionPoint.FGetIsBoundaryVertex:Boolean;
var I:Integer;
begin
   Result:=False;
   if abs(Coordinate.Y)>1e-4 then for I:=1 to NumberOfEdges do Result:=Result or Edge[I-1].IsBoundaryEdge;
end;{TFreeSubdivisionPoint.FGetIsBoundaryVertex}

function TFreeSubdivisionPoint.FGetNormal:T3DCoordinate;
var I,J,Index : Integer;
    Face    : TFreeSubdivisionFace;
    P1,P3   : T3DCoordinate;
    C,N     : T3DCoordinate;
begin
   Result.X:=0;
   Result.Y:=0;
   Result.Z:=0;
   for I:=1 to FFaces.Count do
   begin
      Face:=FFaces[I-1];
      if Face.NumberOfpoints>4 then
      begin
         // Face possibly concave at this point
         // use the normal of ALL points from this face
         C:=Face.FaceCenter;
         N:=ZERO;
         for J:=2 to Face.NumberOfpoints do
         begin
            N:=AddPoint(N,UnifiedNormal(C,Face.Point[J-2].Coordinate,Face.Point[J-1].Coordinate));
         end;
         N:=Normalize(N);
         Result:=AddPoint(Result,N);
      end else
      begin
         Index:=Face.FPoints.IndexOf(Self);
         J:=(Index+Face.FPoints.Count-1) mod (Face.FPoints.Count);
         P1:=Face.Point[J].Coordinate;
         J:=(Index+Face.FPoints.Count+1) mod (Face.FPoints.Count);
         P3:=Face.Point[J].Coordinate;
         Result:=AddPoint(Result,UnifiedNormal(P1,FCoordinate,P3));
      end;
   end;
   Result:=Normalize(Result);
end;{TFreeSubdivisionPoint.FGetNormal}

function TFreeSubdivisionPoint.FGetNumberOfCurves:Integer;
var I:Integer;
begin
   Result:=0;
   for I:=1 to NumberOfEdges do if Edge[I-1].FCurve<>nil then inc(Result);
end;{TFreeSubdivisionPoint.FGetNumberOfCurves}

function TFreeSubdivisionPoint.FGetNumberOfEdges:integer;
begin
   Result:=FEdges.Count;
end;{TFreeSubdivisionPoint.FGetNumberOfEdges}

function TFreeSubdivisionPoint.FGetNumberOfFaces:integer;
begin
   Result:=FFaces.Count;
end;{TFreeSubdivisionPoint.FGetNumberOfFaces}

function TFreeSubdivisionPoint.FGetRegularPoint:Boolean;
var I,N : Integer;
begin
   Result:=False;
   // this procedure was only tested to TRACE regular quad edges
   // and regular/irregular CREASE edges for dxf export
   if (NumberOfFaces=5) and (NumberOfEdges=5) then
   begin
      // boundary of quad and triangle
      N:=0;
      for I:=1 to NumberOfFaces do if Face[I-1].NumberOfPoints=3 then inc(N);
      if (N=3) then Result:=True;
   end else if (NumberOfFaces=6) and (NumberOfEdges=6) then
   begin
      // regular point with all triangles
      N:=0;
      for I:=1 to NumberOfFaces do if Face[I-1].NumberOfPoints=3 then inc(N);
      if (N=6) then Result:=True;
   end else if (NumberOfFaces=4) and (NumberOfEdges=4) then
   begin
      // regular point with all quads
      Result:=True;
   end else //if (NumberOfEdges=3) and (NumberOfFaces=2) then
   begin
      // regular quad boundary edge
      N:=0;
      for I:=1 to NumberOfEdges do if Edge[I-1].NumberOfFaces=1 then inc(N);
      // test for regular point on boundaryedge
      if (N=2) and (NumberOfEdges=3) then Result:=True;
   end;
end;{TFreeSubdivisionPoint.FGetRegularPoint}

function TFreeSubdivisionPoint.FGetLimitPoint:T3DCoordinate;
var I,N,NN,Ind  : Integer;
            P30,P33,P: TFreeSubdivisionPoint;
            Face     : TFreeSubdivisionFace;
            Edge     : TFreeSubdivisionEdge;
begin
   if VertexType in [svDart,svRegular] then
   begin
      Result:=ZERO;
      N:=NumberOfFaces;
      for I:=1 to NumberOfFaces do
      begin
         Face:=FFaces[I-1];
         Ind:=Face.IndexOfPoint(self);
         P30:=Face.Point[(Ind+1) mod Face.Numberofpoints];
         P33:=Face.Point[(Ind+2) mod Face.Numberofpoints];
         Result.X:=Result.X+n*FCoordinate.X+4*P30.Coordinate.X+P33.Coordinate.X;
         Result.Y:=Result.Y+n*FCoordinate.Y+4*P30.Coordinate.Y+P33.Coordinate.Y;
         Result.Z:=Result.Z+n*FCoordinate.Z+4*P30.Coordinate.Z+P33.Coordinate.Z;
      end;
      NN:=n*(N+5);
      Result.X:=Result.X/NN;
      Result.Y:=Result.Y/NN;
      Result.Z:=Result.Z/NN;
   end else if VertexType=svCrease then
  begin
      P30:=nil;
      P33:=nil;
      for I:=1 to NumberOfEdges do
      begin
         Edge:=FEdges[I-1];
         if Edge.Crease then
         begin
            if Edge.StartPoint=self then P:=Edge.EndPoint
                                    else P:=Edge.StartPoint;
            if P30=nil then P30:=P
                       else P33:=P;
         end;
      end;
      if (P30<>nil) and (P33<>nil) then
      begin
         Result.X:=(P30.Coordinate.X+4.0*FCoordinate.X+P33.Coordinate.X)/6.0;
         Result.Y:=(P30.Coordinate.Y+4.0*FCoordinate.Y+P33.Coordinate.Y)/6.0;
         Result.Z:=(P30.Coordinate.Z+4.0*FCoordinate.Z+P33.Coordinate.Z)/6.0;
      end else
      begin
         MessageDlg(Userstring(199)+'!',mtError,[mbOk],0);
         Result:=FCoordinate;
      end;
   end else Result:=FCoordinate;
end;{TFreeSubdivisionPoint.FGetLimitPoint}

function TFreeSubdivisionPoint.IsRegularNURBSPoint(Faces:TFasterList):Boolean;
var I,Ind,N:Integer;
    Boundary:Boolean;
begin
   if VertexType in [svRegular,svdart] then Result:=NumberOfFaces=4 else
   if VertexType in [svCrease] then
   begin
      //Faces:=nil;
      if Faces<>nil then
      begin
         N:=0;
         for I:=1 to NumberOfFaces do
         begin
            Ind:=Faces.SortedIndexOf(face[I-1]);
            if Ind<>-1 then inc(N);
         end;
         Result:=N=2;
         if (N=2) and (NumberOfFaces<>4) then
         begin
            Result:=true;
         end;
      end else
      begin
         // boundary edge ?
         Boundary:=False;
         for I:=1 to NumberOfEdges do Boundary:=Boundary or (Edge[I-1].NumberOfFaces=1);
         if Boundary then Result:=NumberOfFaces=2
                     else Result:=NumberOfFaces=4;
      end;
   end else Result:=False;
end;{TFreeSubdivisionPoint.IsRegularNURBSPoint}

procedure TFreeSubdivisionPoint.FSetCoordinate(Val:T3DCoordinate);
begin
   FCoordinate:=Val;
end;{TFreeSubdivisionPoint.FSetCoordinate}

procedure TFreeSubdivisionPoint.AddEdge(Edge:TFreeSubdivisionEdge);
begin
   if FEdges.IndexOf(Edge)=-1 then
   begin
      FEdges.Add(Edge);
   end;
end;{TFreeSubdivisionPoint.AddEdge}

procedure TFreeSubdivisionPoint.AddFace(Face:TFreeSubdivisionFace);
begin
   if FFaces.IndexOf(Face)=-1 then
   begin
      FFaces.Add(Face);
   end;
end;{TFreeSubdivisionPoint.AddFace}

function TFreeSubdivisionPoint.Averaging:T3DCoordinate;
var I,J,Nt,Nq     : Integer;
    a             : TFloatType;
    Weight,TotalWeight:TFloatType;
    Center        : T3DCoordinate;
    Face          : TFreeSubdivisionFace;
    Edge          : TFreeSubdivisionEdge;
    P             : TFreeSubdivisionPoint;
begin
   if (NumberOfedges=0) or (FVertexType=svCorner) then Result:=FCoordinate else
   begin
      if FVertexType=svCrease then
      begin
         Result.X:=FCoordinate.X*0.50;
         Result.Y:=FCoordinate.Y*0.50;
         Result.Z:=FCoordinate.Z*0.50;
         for I:=1 to FEdges.Count do
         begin
            Edge:=FEdges[I-1];
            if (Edge.FFaces.Count=1) or (Edge.FCrease) then
            begin
               if Edge.FStartpoint=self then P:=Edge.FEndpoint
                                        else P:=Edge.FStartpoint;
               Result.X:=Result.X+0.25*P.FCoordinate.X;
               Result.Y:=Result.Y+0.25*P.FCoordinate.Y;
               Result.Z:=Result.Z+0.25*P.FCoordinate.Z;
            end;
         end;
      end else
      begin
         TotalWeight:=0.0;
         Result:=ZERO;
         Nt:=0;
         for I:=1 to FFaces.Count do
         begin
            Face:=FFaces[I-1];
            Center:=ZERO;
            if Face.NumberOfpoints=3 then
            begin
               inc(Nt);
               // Calculate centerpoint
               for j:=1 to Face.NumberOfpoints do
               begin
                  P:=Face.FPoints[J-1];
                  if P=self then Weight:=1/4
                            else Weight:=3/8;
                  Center.X:=Center.X+Weight*P.FCoordinate.X;
                  Center.Y:=Center.Y+Weight*P.FCoordinate.Y;
                  Center.Z:=Center.Z+Weight*P.FCoordinate.Z;
               end;
               Weight:=Pi/3;
            end else if Face.NumberOfpoints=4 then
            begin
               // Calculate centerpoint
               for j:=1 to Face.NumberOfpoints do
               begin
                  P:=Face.FPoints[J-1];
                  Weight:=1/4;
                  Center.X:=Center.X+Weight*P.FCoordinate.X;
                  Center.Y:=Center.Y+Weight*P.FCoordinate.Y;
                  Center.Z:=Center.Z+Weight*P.FCoordinate.Z;
               end;
               Weight:=Pi/2;
            end else Raise exception.Create('Invalid number of points in TFreeSubdivisionPoint.Averaging');
            Result.X:=Result.X+Weight*Center.X;
            Result.Y:=Result.Y+Weight*Center.Y;
            Result.Z:=Result.Z+Weight*Center.Z;
            TotalWeight:=TotalWeight+Weight;
         end;
         if TotalWeight<>0 then
         begin
            Result.X:=Result.X/TotalWeight;
            Result.Y:=Result.Y/TotalWeight;
            Result.Z:=Result.Z/TotalWeight;
         end;
         Nq:=FFaces.Count-Nt;

         if Nt=FFaces.Count then
         begin
            // apply averaging in case of vertex surrounded by triangles
            a:=5/3-8/3*sqr(3/8+1/4*cos(2*Pi/FFaces.Count));
         end else if Nq=FFaces.Count then
         begin
            // apply averaging in case of vertex surrounded by quads
            a:=4/FFaces.Count;
         end else
         begin
            // apply averaging in case of vertex on boundary of quads and triangles
            if (Nq=0) and (Nt=3) then
            begin
               a:=1.5;
            end else a:=12/(3*Nq+2*Nt);
         end;
         if a<>1.0 then
         begin
            Result.X:=FCoordinate.X+a*(Result.X-FCoordinate.X);
            Result.Y:=FCoordinate.Y+a*(Result.Y-FCoordinate.Y);
            Result.Z:=FCoordinate.Z+a*(Result.Z-FCoordinate.Z);
         end;
      end;
   end;
end;{TFreeSubdivisionPoint.Averaging}

function TFreeSubdivisionPoint.CalculateVertexPoint:TFreeSubdivisionPoint;
var Point   : T3DCoordinate;
    Edge    : TFreeSubdivisionEdge;
    I       : Integer;
begin
   Point:=FCoordinate;
   Result:=TFreeSubdivisionPoint.Create(Owner);
   Result.FVertexType:=FVertexType;
   Result.FCoordinate:=Point;
   for I:=1 to Fedges.Count do
   begin
      Edge:=FEdges[I-1];
      if Edge.FCurve<>nil then
      begin
         Edge.FCurve.ReplaceVertexPoint(Self,Result);
      end;
   end;
end;{TFreeSubdivisionPoint.CalculateVertexPoint}

procedure TFreeSubdivisionPoint.Clear;
begin
   FCoordinate.X:=0; FCoordinate.Y:=0; FCoordinate.Z:=0;
   FFaces.Clear;
   FEdges.Clear;
   FVertexType:=svRegular;
end;{TFreeSubdivisionPoint.Clear}

constructor TFreeSubdivisionPoint.Create(Owner:TFreeSubdivisionSurface);
begin
   inherited Create(Owner);
   FFaces:=TFasterList.Create;
   FEdges:=TFasterList.Create;
   Clear;
end;{TFreeSubdivisionPoint.Create}

procedure TFreeSubdivisionPoint.DeleteEdge(Edge:TFreeSubdivisionEdge);
var Index:Integer;
begin
   index:=FEdges.IndexOf(Edge);
   if Index<>-1 then FEdges.Delete(Index);
end;{TFreeSubdivisionPoint.DeleteEdge}

procedure TFreeSubdivisionPoint.DeleteFace(Face:TFreeSubdivisionFace);
var Index:Integer;
begin
   index:=FFaces.IndexOf(Face);
   if Index<>-1 then FFaces.Delete(Index);
end;{TFreeSubdivisionPoint.DeleteFace}

destructor TFreeSubdivisionPoint.Destroy;
begin
   Clear;
   FFaces.Destroy;
   FFaces:=nil;
   FEdges.Destroy;
   Fedges:=nil;
   inherited Destroy;
end;{TFreeSubdivisionPoint.Destroy}

function TFreeSubdivisionPoint.IndexOfFace(Face:TFreeSubdivisionFace):Integer;
begin
   Result:=FFaces.IndexOf(Face);
end;{TFreeSubdivisionPoint.IndexOfFace}

{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionControlPoint                             }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionControlPoint.FGetColor:TColor;
begin
   if Selected then Result:=Owner.Selectedcolor else
   begin
      if Locked then Result:=clDkGray else
      begin
         if IsLeak then Result:=Owner.LeakColor else
         Case FVertexType of
            svRegular : Result:=Owner.RegularPointColor;
            svCorner  : Result:=Owner.CornerPointColor;
            svDart    : result:=Owner.DartPointColor;
            svCrease  : result:=Owner.CreasePointColor;
            else Result:=clRed;
         end;
      end;
   end;
end;{TFreeSubdivisionControlPoint.FGetColor}

function TFreeSubdivisionControlPoint.FGetIndex:Integer;
begin
    Result:=Owner.FControlPoints.IndexOf(Self);
end;{TFreeSubdivisionControlPoint.FGetIndex}

function TFreeSubdivisionControlPoint.FGetIsLeak:boolean;
begin
   Result:=(abs(Coordinate.Y)>1e-4) and (IsBoundaryVertex);
end;{TFreeSubdivisionControlPoint.FGetIsLeak}

function TFreeSubdivisionControlPoint.FGetSelected:Boolean;
begin
   Result:=Owner.FSelectedControlPoints.IndexOf(self)<>-1;
end;{TFreeSubdivisionControlPoint.FGetSelected}

function TFreeSubdivisionControlPoint.FGetVisible:Boolean;
var I       : Integer;
    CFace   : TFreeSubdivisionControlFace;
begin
   // meant for controlpoints only.
   // a controlpoint is visible if at least one of it's
   // neighbouring controlfaces belongs to a visible layer
   Result:=False;
   if Owner.ShowControlNet then
   begin
      for i:=1 to FFaces.Count do if Face[I-1] is TFreeSubdivisionControlFace then
      begin
         CFace:=FFaces[I-1];
         if CFace.Layer<>nil then
         begin
            if CFace.Layer.Visible then
            begin
               Result:=True;
               exit;
            end;
         end;
      end;
   end;
   // Finally check if the point is selected.
   // Selected points must be visible at all times
   // Points with no faces connected also!
   if not Result then Result:=(Selected) or (NumberOfFaces=0);
   if (not Result) and (Owner.NumberOfControlCurves>0) then
   begin
      For I:=1 to NumberOfEdges do if Edge[I-1].FCurve<>nil then
      begin
         if Edge[I-1].FCurve.Selected then
         begin
            Result:=true;
            break;
         end;
      end;
   end;
end;{TFreeSubdivisionControlPoint.FGetVisible}

procedure TFreeSubdivisionControlPoint.FSetSelected(val:Boolean);
var Index : Integer;
begin
   Index:=Owner.FSelectedControlPoints.IndexOf(self);
   if Val then
   begin
      // Only add if it is not already in the list
      if Index=-1 then Owner.FSelectedControlPoints.Add(self);
   end else
   begin
      if Index<>-1 then Owner.FSelectedControlPoints.Delete(index);
   end;
   if Assigned(Owner.FOnSelectItem) then Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlPoint.FSetSelected}

procedure TFreeSubdivisionControlPoint.FSetLocked(val:Boolean);
begin
   if Val<>FLocked then
   begin
      FLocked:=Val;
   end;
end;{TFreeSubdivisionControlPoint.FSetLocked}

procedure TFreeSubdivisionControlPoint.FSetCoordinate(Val:T3DCoordinate);
begin
   if not Locked then
   begin
      Inherited FSetCoordinate(Val);
      Owner.Build:=False;
   end;
end;{TFreeSubdivisionControlPoint.FSetCoordinate}

procedure TFreeSubdivisionControlPoint.Collapse;
var I,J     : Integer;
    Face    : TFreeSubdivisionControlFace;
    Edge1   : TFreesubdivisionControlEdge;
    Edge2   : TFreesubdivisionControlEdge;
    Edges   : TFasterList;
    Points  : TFasterList;
    Sorted  : TFasterList;
    Checklist:TFasterList;
    P1,P2   : TFreeSubdivisionControlPoint;
    Crease  : Boolean;
    EdgeCollapse:Boolean;
begin
   if NumberOfFaces<=2 then
   begin
      Selected:=False;
      P1:=nil;
      P2:=nil;
      // This is possibly a point on a boundary edge,
      // check for this special case
      Edge1:=nil;
      Edge2:=nil;
      for i:=1 to NumberOfEdges do if Edge[I-1].NumberOfFaces=1 then
      begin
         if Edge1=nil then Edge1:=Edge[I-1] as TFreesubdivisionControlEdge else
                           Edge2:=Edge[I-1] as TFreesubdivisionControlEdge;
      end;
      if (Edge1<>nil) and (Edge2<>nil) then
      begin
         for I:=NumberOfEdges downto 1 do if Edge[I-1].NumberOfFaces>1 then
         begin
            Edge1:=Edge[I-1] as TFreesubdivisionControlEdge;
            Edge1.Collapse;
         end;
      end;

      if NumberOfEdges=2 then
      begin
         EdgeCollapse:=True;
         Edge1:=FEdges[0];
         if Edge1.FStartpoint=self then P1:=Edge1.FEndpoint as TFreeSubdivisionControlPoint
                                   else P1:=Edge1.FStartpoint as TFreeSubdivisionControlPoint;
         Edge2:=FEdges[1];
         if Edge2.FStartpoint=self then P2:=Edge2.FEndpoint as TFreeSubdivisionControlPoint
                                   else P2:=Edge2.FStartpoint as TFreeSubdivisionControlPoint;
         Crease:=Edge1.Crease or Edge2.Crease;
      end else
      begin
         Crease:=False;
         EdgeCollapse:=False;
      end;
      for I:=NumberOfFaces downto 1 do
      begin
         Face:=FFaces[I-1];
         Points:=TFasterList.Create;
         for J:=1 to Face.FPoints.Count do if Face.FPoints[J-1]<>self then Points.Add(Face.FPoints[J-1]);
         FOwner.AddControlFace(Points,False,Face.Layer);
         Points.Destroy;
         Face.Delete;
      end;

      if EdgeCollapse then
      begin
         Edge1:=FOwner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
         if Edge1<>nil then
         begin
            Edge1.Crease:=Crease;// or (Edge1.NumberOfFaces=1);
         end;
      end;

   end else
   begin
      Checklist:=TFasterList.Create;
      Edges:=TFasterList.Create;
      for i:=1 to NumberOfEdges do if Edge[I-1].StartPoint=self then CheckList.Add(Edge[I-1].EndPoint)
                                                                else CheckList.Add(Edge[I-1].StartPoint);
      for I:=1 to NumberOfFaces do
      begin
         Face:=Ffaces[I-1];
         P1:=Face.FPoints[Face.NumberOfPoints-1];
         for J:=1 to Face.FPoints.Count do
         begin
            P2:=face.FPoints[J-1];
            if (P1<>self) and (P2<>self) then
            begin
               Edge1:=FOwner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
               if Edge1<>nil then if Edges.IndexOf(Edge1)=-1 then Edges.Add(Edge1);
            end;
            P1:=P2;
         end;
      end;
      // sort edges in correct order and add new face
      if Edges.Count>2 then
      begin
         Sorted:=TFasterList.Create;
         Fowner.IsolateEdges(Edges,Sorted);
         Edges.Destroy;
         for I:=1 to Sorted.Count do
         begin
            Points:=Sorted[I-1];
            if Points.Count>2 then
            begin
               Face:=FOwner.AddControlFace(Points,False);
               if Face<>nil then
               begin
                  P1:=Face.FPoints[Face.FPoints.Count-1];
                  for J:=1 to Face.FPoints.Count do
                  begin
                     P2:=face.FPoints[J-1];
//                     Edge1:=fowner.EdgeExists(p1,p2) as TFreesubdivisionControlEdge;
//                     if edge1<>nil then
//                     begin
//                        {
//                        Cur:=Edge.FFaces.IndexOf(self);
//                        if Cur=-1 then Edge.Crease:=Edge.NumberOfFaces<2 else
//                           Edge.Crease:=Edge.NumberOfFaces<3;
//                        }
//                     end;
                     P1:=P2;
                  end;
               end;
            end;
            Points.Destroy;
         end;
         Sorted.Destroy;
      end else Edges.Destroy;
      delete;

      for i:=Checklist.Count downto 1 do
      begin
         P1:=Checklist[I-1];
         if (P1.NumberOfFaces>1) and (P1.NumberOfEdges=2) then
         begin
            P1.Collapse;
         end;
      end;
      checklist.Destroy;
   end;
end;{TFreeSubdivisionControlPoint.Collapse}

constructor TFreeSubdivisionControlPoint.Create(Owner:TFreeSubdivisionSurface);
begin
   inherited Create(Owner);
end;{TFreeSubdivisionControlPoint.Create}

procedure TFreeSubdivisionControlPoint.Delete;
var Index:Integer;
    Edge :TFreesubdivisionControlEdge;
begin
   // delete from selection;
   Selected:=False;

   if FEdges.Count>0 then
   begin
      Index:=Owner.FControlPoints.IndexOf(Self);
      if Index<>-1 then Owner.FControlPoints.Delete(index);

      while FEdges<>nil do //.Count>0 do
      begin
         Edge:=self.Edge[NumberOfEdges-1] as TFreesubdivisionControlEdge;
         Edge.Delete;
      end;
   end else
   begin
      Index:=Owner.FControlPoints.IndexOf(Self);
      if Index<>-1 then Owner.FControlPoints.Delete(index);
      Destroy;
   end;
end;{TFreeSubdivisionControlPoint.Delete}

procedure TFreeSubdivisionControlPoint.Draw(Viewport:TFreeViewport);
var P    : TPoint;
    Pz   : TShadePoint;
    I    : Integer;
    P3D  : T3DCoordinate;
begin
   if Viewport.ViewportMode<>vmWireframe then
   begin
      P3D:=FCoordinate;
      if Viewport.ViewType=fvBodyplan then if P3D.X<=Owner.MainframeLocation then P3D.Y:=-P3D.Y;
      P:=Viewport.Project(P3D);
      Pz:=Viewport.ProjectToZBuffer(1.002*ZBufferScaleFactor,P3D);
      // Check if the point lies within the viewport's drawingcanvas boundaries
      if (Pz.X>=0) and (Pz.Y>=0) and (Pz.X<Viewport.ClientWidth) and (Pz.Y<Viewport.ClientHeight) then
      begin
         // Compare to Z buffer to check visibility;
         if Pz.Z>=Viewport.ZBuffer.FBuffer[Pz.Y][Pz.X] then
         begin
            // yes, the point is visible
            Viewport.ZBuffer.FBuffer[Pz.Y][Pz.X]:=Pz.Z;
            if Viewport.PenWidth<>1 then Viewport.PenWidth:=1;
            Viewport.PenColor:=Color;
            Viewport.BrushStyle:=bsClear;
            Viewport.PenStyle:=psSolid;
            for I:=1 to Owner.ControlPointSize do Viewport.Rectangle(Pz.X-I,Pz.Y-I,Pz.X+I,Pz.Y+I);
         end;
      end;
   end else
   begin
      //with Viewport.DrawingCanvas do
      //begin
         P3D:=FCoordinate;
         if Viewport.ViewType=fvBodyplan then if P3D.X<=Owner.MainframeLocation then P3D.Y:=-P3D.Y;
         P:=Viewport.Project(P3D);
         if Viewport.PenWidth<>1 then Viewport.PenWidth:=1;
         Viewport.PenColor:=Color;
         if Viewport.BrushStyle<>bsClear then Viewport.BrushStyle:=bsClear;
         if Selected then
         begin
            //FInitializeCanvas(Viewport,1,Color,Mode);
            for I:=1 to Owner.ControlPointSize do Viewport.Rectangle(P.X-I,P.Y-I,P.X+I,P.Y+I);
            Viewport.Rectangle(P.X-Owner.ControlPointSize-2,P.Y-Owner.ControlPointSize-2,P.X+Owner.ControlPointSize+2,P.Y+Owner.ControlPointSize+2);
         end else
         begin
            for I:=1 to Owner.ControlPointSize do Viewport.Rectangle(P.X-I,P.Y-I,P.X+I,P.Y+I);
         end;
      //end;
   end;
end;{TFreeSubdivisionControlPoint.Draw}

function TFreeSubdivisionControlPoint.DistanceToCursor(X,Y:Integer;Viewport:TFreeViewport):integer;
var Pt: TPoint;
    P : T3DCoordinate;
begin
   // Check if cursor position lies within the boundaries
   P:=FCoordinate;
   if (Viewport.ViewType=fvBodyplan) and (P.X<=Owner.MainframeLocation) then P.Y:=-P.Y;
   Pt:=Viewport.Project(P);
   if (Pt.X>=0) and (Pt.X<=Viewport.Width) and (Pt.Y>=0) and (Pt.Y<=Viewport.Height) then Result:=Round(Sqrt(Sqr(Pt.X-X)+SQR(Pt.Y-Y)))
                                                                                     else Result:=1000000;
end;{TFreeSubdivisionControlPoint.DistanceToCursor}

procedure TFreeSubdivisionControlPoint.LoadBinary(Source:TFreeFileBuffer);
var I    : Integer;
    Sel  : Boolean;
begin
   Source.Load(FCoordinate);
   Source.Load(I);
   FVertextype:=TFreeVertexType(I);
   Source.Load(Sel);
   if Sel then Selected:=True;
   if Source.Version>=fv198 then
   begin
      Source.Load(FLocked);
   end;
end;{TFreeSubdivisionControlPoint.LoadBinary}

procedure TFreeSubdivisionControlPoint.LoadFromStream(var LineNr:Integer;Strings:TStringList);
var Str  : string;
    I    : Integer;
    sel  : Boolean;
begin
   // FCoordinate
   Inc(LineNr);
   Str:=Strings[LineNr];
   FCoordinate.X:=ReadFloatFromStr(LineNr,Str);
   FCoordinate.Y:=ReadFloatFromStr(LineNr,Str);
   FCoordinate.Z:=ReadFloatFromStr(LineNr,Str);
   // FVertexType
   if Str<>'' then
   begin
      I:=ReadIntFromStr(LineNr,Str);
      FVertextype:=TFreeVertexType(I);
      if Str<>'' then
      begin
         Sel:=ReadBoolFromStr(LineNr,Str);
         if sel then Selected:=True;
      end;
   end else FVertextype:=TFreeVertexType(0);
end;{TFreeSubdivisionControlPoint.LoadFromStream}

procedure TFreeSubdivisionControlPoint.SaveBinary(Destination:TFreeFileBuffer);
begin
   Destination.Add(FCoordinate);
   Destination.Add(Ord(VertexType));
   Destination.Add(Selected);
   if Destination.Version>=fv198 then
   begin
      Destination.Add(Locked);
   end;
end;{TFreeSubdivisionControlPoint.SaveBinary}

procedure TFreeSubdivisionControlPoint.SaveToStream(Strings:TStringlist);
var Str  : String;
begin
   Str:=Truncate(Fcoordinate.X,5)+#32+Truncate(Fcoordinate.Y,5)+#32+Truncate(Fcoordinate.Z,5);
   Str:=Str+#32+IntToStr(Ord(VertexType));
   Str:=Str+#32+BoolToStr(Selected);
   Strings.Add(Str);
end;{TFreeSubdivisionControlPoint.SaveToStream}

{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionEdge                                     }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionEdge.FGetIndex:Integer;
begin
   Result:=Owner.FEdges.IndexOf(self);
   if result=-1 then
   begin
      Result:=Owner.FControlEdges.IndexOf(self);
      if Result=-1 then Result:=0;
   end;
end;{TFreeSubdivisionEdge.FGetIndex}

function TFreeSubdivisionEdge.FGetIsBoundaryEdge:Boolean;
begin
   Result:=False;
   if NumberOfFaces=1 then Result:=(abs(StartPoint.Coordinate.Y)>1e-4) or (abs(EndPoint.Coordinate.Y)>1e-4);
end;{TFreeSubdivisionEdge.FGetIsBoundaryEdge}

function TFreeSubdivisionEdge.FGetFace(Index:Integer):TFreeSubdivisionFace;
begin
   Result:=FFaces[Index];
end;{TFreeSubdivisionEdge.FGetFace}

function TFreeSubdivisionEdge.FGetNumberOfFaces:Integer;
begin
   Result:=FFaces.Count;
end;{TFreeSubdivisionEdge.FGetNumberOfFaces}

procedure TFreesubdivisionEdge.FSetCrease(Val:Boolean);
var I,N  : Integer;
    Edge : TFreeSubdivisionEdge;
begin
   if NumberOfFaces=1 then Val:=True;  // Boundary edges must ALWAYS be crease edges
   if (Val<>FCrease) then
   begin
      FCrease:=Val;
      N:=0;
      for I:=1 to FStartPoint.FEdges.Count do
      begin
         Edge:=FStartPoint.FEdges[I-1];
         if Edge.Crease then Inc(N);
      end;
      if FStartpoint.VertexType=svCorner then
      begin
         if (FStartPoint.NumberOfFaces>1) and (N=2) then FStartpoint.VertexType:=svCrease; 
      end else
      begin
         if N=0 then FStartpoint.VertexType:=svRegular else
          if N=1 then FStartpoint.VertexType:=svDart else
           if N=2 then FStartpoint.VertexType:=svCrease else
            if N>2 then FStartpoint.VertexType:=svCorner;
      end;

      N:=0;
      for I:=1 to FEndPoint.FEdges.Count do
      begin
         Edge:=FEndPoint.FEdges[I-1];
         if Edge.Crease then Inc(N);
      end;
      if FEndpoint.VertexType=svCorner then
      begin
         if (FEndPoint.NumberOfFaces>1) and (N=2) then FEndpoint.VertexType:=svCrease;
      end else
      begin
         if N=0 then FEndpoint.VertexType:=svRegular else
          if N=1 then FEndpoint.VertexType:=svDart else
           if N=2 then FEndpoint.VertexType:=svCrease else
            if N>2 then FEndpoint.VertexType:=svCorner;
      end;

      FStartPoint.FOwner.Build:=false;
   end;
end;{TFreesubdivisionEdge.FSetCrease}

function TFreesubdivisionEdge.FGetPreviousEdge:TFreeSubdivisionEdge;
var P          : TFreeSubdivisionPoint;
    Edge       : TFreeSubdivisionEdge;
    I,J,Index  : Integer;
    Sharesface : Boolean;
begin
   P:=Startpoint;
   Result:=nil;
   if (P.RegularPoint) and (P.VertexType<>svCorner) then
   begin
      // Find previous edge
      for I:=1 to P.NumberOfEdges do if P.Edge[I-1]<>self then
      begin
         Edge:=P.Edge[I-1];
         if Edge.Crease=self.Crease then
         begin
            SharesFace:=False;
            for J:=1 to self.NumberOfFaces do
            begin
               Index:=Edge.FFaces.IndexOf(self.Face[J-1]);
               if Index<>-1 then
               begin
                  SharesFace:=True;
                  Break;
               end;
            end;
            if not SharesFace then
            begin
               if Edge.StartPoint=self.StartPoint then Edge.SwapData;
               Result:=Edge;
               exit;
            end;
         end;
      end;
   end;
end;{TFreesubdivisionEdge.FGetPreviousEdge}

function TFreesubdivisionEdge.FGetNextEdge:TFreeSubdivisionEdge;
var P          : TFreeSubdivisionPoint;
    Edge       : TFreeSubdivisionEdge;
    I,J,Index  : Integer;
    Sharesface : Boolean;
begin
   P:=Endpoint;
   Result:=nil;
   if (P.RegularPoint) and (P.VertexType<>svCorner) then
   begin
      // Find Next edge
      for I:=1 to P.NumberOfEdges do if P.Edge[I-1]<>self then
      begin
         Edge:=P.Edge[I-1];
         if Edge.Crease=self.Crease then
         begin
            SharesFace:=False;
            for J:=1 to self.NumberOfFaces do
            begin
               Index:=Edge.FFaces.IndexOf(self.Face[J-1]);
               if Index<>-1 then
               begin
                  SharesFace:=True;
                  Break;
               end;
            end;
            if not SharesFace then
            begin
               if Edge.StartPoint=self.StartPoint then Edge.SwapData;
               Result:=Edge;
               exit;
            end;
         end;
      end;
   end;
end;{TFreesubdivisionEdge.FGetNextEdge}

procedure TFreeSubdivisionEdge.AddFace(Face:TFreeSubdivisionFace);
begin
   if FFaces.IndexOf(Face)=-1 then
   begin
      FFaces.Add(Face);
   end;
end;{TFreeSubdivisionEdge.AddFace}

procedure TFreeSubdivisionEdge.Assign(Edge:TFreeSubdivisionEdge);
begin
   FCrease:=Edge.FCrease;
   FControlEdge:=Edge.FControlEdge;
end;{TFreeSubdivisionEdge.Assign}

function TFreeSubdivisionEdge.CalculateEdgePoint:TFreeSubdivisionPoint;
var Point   : T3DCoordinate; SP, EP : TFreeSubdivisionPoint;
begin
   SP:=FStartpoint;
   EP:=FEndpoint;
   Point.X:=0.5*(SP.FCoordinate.X+EP.FCoordinate.X);
   Point.Y:=0.5*(SP.FCoordinate.Y+EP.FCoordinate.Y);
   Point.Z:=0.5*(SP.FCoordinate.Z+EP.FCoordinate.Z);
   Result:=TFreeSubdivisionPoint.Create(FStartPoint.Owner);
   if FCrease then Result.FVertexType:=svCrease;
   if FCurve<>nil then
   begin
      FCurve.InsertEdgePoint(FStartPoint,FEndPoint,Result);
   end;
   Result.FCoordinate:=Point;
end;{TFreeSubdivisionEdge.CalculateEdgePoint}

procedure TFreeSubdivisionEdge.Clear;
begin
   FStartpoint:=nil;
   FEndpoint:=nil;
   FCurve:=nil;
   FFaces.Clear;
   FCrease:=False;
   FControlEdge:=False;
end;{TFreeSubdivisionEdge.Clear}

procedure TFreeSubdivisionEdge.SwapData;
var Tmp:TFreeSubdivisionPoint;
begin
   Tmp:=FStartpoint;
   FStartpoint:=FEndpoint;
   FEndpoint:=Tmp;
end;{TFreeSubdivisionEdge.SwapData}

constructor TFreeSubdivisionEdge.Create(Owner:TFreeSubdivisionSurface);
begin
   Inherited Create(Owner);
   FFaces:=TFasterList.Create;
   clear;
end;{TFreeSubdivisionEdge.Create}

procedure TFreeSubdivisionEdge.DeleteFace(Face:TFreeSubdivisionFace);
var Index:Integer;
begin
   Index:=FFaces.IndexOf(Face);
   if Index<>-1 then
   begin
      FFaces.Delete(Index);
      if FFaces.Count=1 then Crease:=true else
         if FFaces.Count=0 then Crease:=False;
   end;
end;{TFreeSubdivisionEdge.DeleteFace}

destructor TFreeSubdivisionEdge.Destroy;
begin
   if self=nil then exit;
   Clear;
   FFaces.Destroy;
   FFaces := nil;
   Inherited Destroy;
end;{TFreeSubdivisionEdge.Destroy}

function TFreeSubdivisionEdge.DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;
var Pt,Pt1,Pt2 : TPoint;
    P1,P2,M    : T3DCoordinate;
    Param      : TFloatType;
    Tmp        : Integer;
begin
   // Check if cursor position lies within the boundaries
   P:=ZERO;
   Result:=10000;
   if Viewport.ViewType=fvBodyplan then
   begin
      P1:=StartPoint.FCoordinate;
      P2:=FEndPoint.FCoordinate;
      if ((P1.X<Owner.MainframeLocation) and (P2.X>Owner.MainframeLocation)) or
         ((P1.X>Owner.MainframeLocation) and (P2.X<Owner.MainframeLocation)) then
      begin
         if P2.X-P1.X<>0 then M:=Interpolate(P1,P2,(Owner.MainframeLocation-P1.X)/(P2.X-P1.X))
                         else M:=MidPoint(P1,P2);
         if P1.X<=Owner.MainframeLocation then
         begin
            // P2 lies on port
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P2,M,Param);
            end;
            // P1 lies on starboard
            P1.Y:=-P1.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P1,M,Param);
            end;
         end else
         begin
            // P1 lies on port
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P1,M,Param);
            end;
            // P2 lies on starboard
            P2.Y:=-P2.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P2,M,Param);
            end;
         end;
      end else
      begin
         if P1.X<=Owner.MainframeLocation then P1.Y:=-P1.Y;
         if P2.X<=Owner.MainframeLocation then P2.Y:=-P2.Y;
         Pt1:=Viewport.Project(P1);
         Pt2:=Viewport.Project(P2);
         Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
         if Tmp<Result then
         begin
            Result:=Tmp;
            P:=Interpolate(P1,P2,Param);
         end;
      end;
   end else
   begin
      Pt.X:=X;
      Pt.Y:=Y;
      Pt1:=Viewport.Project(StartPoint.Coordinate);
      Pt2:=Viewport.Project(EndPoint.Coordinate);
      Result:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
      P:=Interpolate(StartPoint.Coordinate,EndPoint.Coordinate,Param);
   end;
end;{TFreeSubdivisionEdge.DistanceToCursor}

procedure TFreeSubdivisionEdge.Draw(DrawMirror:Boolean;Viewport:TFreeViewport);
var P1,P2,M : T3DCoordinate;
    Pt1,Pt2 : TPoint;
begin
   P1:=StartPoint.FCoordinate;
   P2:=EndPoint.FCoordinate;

   if (DrawMirror=false) and (Viewport.ViewType=fvBodyplan) then
   begin
      if ((P1.X<Owner.MainframeLocation) and (P2.X>Owner.MainframeLocation)) or
         ((P1.X>Owner.MainframeLocation) and (P2.X<Owner.MainframeLocation)) then
      begin
         if P2.X-P1.X<>0 then M:=Interpolate(P1,P2,(Owner.MainframeLocation-P1.X)/(P2.X-P1.X))
                         else M:=MidPoint(P1,P2);
         if P1.X<=Owner.MainframeLocation then
         begin
            // P2 lies on port
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Viewport.MoveTo(Pt1.X,Pt1.Y);
            Viewport.LineTo(Pt2.X,Pt2.Y);
            // P1 lies on starboard
            P1.Y:=-P1.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Viewport.MoveTo(Pt1.X,Pt1.Y);
            Viewport.LineTo(Pt2.X,Pt2.Y);
         end else
         begin
            // P1 lies on port
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Viewport.MoveTo(Pt1.X,Pt1.Y);
            Viewport.LineTo(Pt2.X,Pt2.Y);
            // P2 lies on starboard
            P2.Y:=-P2.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Viewport.MoveTo(Pt1.X,Pt1.Y);
            Viewport.LineTo(Pt2.X,Pt2.Y);

         end;
      end else
      begin
         if P1.X<=Owner.MainframeLocation then P1.Y:=-P1.Y;
         if P2.X<=Owner.MainframeLocation then P2.Y:=-P2.Y;
         Pt1:=Viewport.Project(P1);
         Pt2:=Viewport.Project(P2);
         Viewport.MoveTo(Pt1.X,Pt1.Y);
         Viewport.LineTo(Pt2.X,Pt2.Y);
      end;

   end else
   begin
      Pt1:=Viewport.Project(P1);
      Pt2:=Viewport.Project(P2);
      Viewport.MoveTo(Pt1.X,Pt1.Y);
      Viewport.LineTo(Pt2.X,Pt2.Y);
      if DrawMirror then
      begin
         P1.Y:=-P1.Y;
         P2.Y:=-P2.Y;
         Pt1:=Viewport.Project(P1);
         Pt2:=Viewport.Project(P2);
         Viewport.MoveTo(Pt1.X,Pt1.Y);
         Viewport.LineTo(Pt2.X,Pt2.Y);
      end;
   end;
end;{TFreeSubdivisionEdge.Draw}

{--------------------------------------------------------------------------------------------------}
{                                         TFreesubdivisionControlEdge                              }
{--------------------------------------------------------------------------------------------------}
function TFreesubdivisionControlEdge.FGetColor:TColor;
begin
   if Selected then Result:=Owner.Selectedcolor else
     if NumberOfFaces>2 then Result:=clLime else
       if Crease then Result:=Owner.CreaseEdgeColor else
          Result:=Owner.EdgeColor;
end;{TFreesubdivisionControlEdge.FGetColor}

function TFreesubdivisionControlEdge.FGetIndex:Integer;
begin
   Result:=Owner.FControlEdges.IndexOf(self);
end;{TFreesubdivisionControlEdge.FGetIndex}

function TFreesubdivisionControlEdge.FGetIsBoundaryEdge:Boolean;
var I,N  : Integer;
    Face : TFreeSubdivisionControlface;
begin
   N:=0;
   for I:=1 to NumberOfFaces do
   begin
      Face:=FFaces[I-1];
      if Face.Layer.UseInHydrostatics then inc(N);
   end;
   if N=1 then Result:=(abs(StartPoint.Coordinate.Y)>1e-4) or (abs(EndPoint.Coordinate.Y)>1e-4)
          else Result:=false;
end;{TFreesubdivisionControlEdge.FGetIsBoundaryEdge}

procedure TFreesubdivisionControlEdge.FSetSelected(val:Boolean);
var Index : Integer;
begin
   Index:=Owner.FSelectedControlEdges.IndexOf(self);
   if Val then
   begin
      // Only add if it is not already in the list
      if Index=-1 then Owner.FSelectedControlEdges.Add(self);
   end else
   begin
      if Index<>-1 then Owner.FSelectedControlEdges.Delete(index);
   end;
   if Assigned(Owner.FOnSelectItem) then Owner.FOnSelectItem(self);
end;{TFreesubdivisionControlEdge.FSetSelected}

function TFreesubdivisionControlEdge.FGetSelected:Boolean;
begin
   Result:=Owner.FSelectedControlEdges.IndexOf(self)<>-1;
end;{TFreesubdivisionControlEdge.FGetSelected}

function TFreesubdivisionControlEdge.FGetVisible:Boolean;
var I       : Integer;
    CFace   : TFreeSubdivisionControlFace;
begin
   // meant for controledges only.
   // a controledge is visible if at least one of it's
   // neighbouring controlfaces belongs to a visible layer
   Result:=False;
   if Owner.ShowControlNet then
   begin
      for I:=1 to FFaces.Count do if Face[I-1] is TFreeSubdivisionControlFace then
      begin
         CFace:=FFaces[I-1];
         if CFace.Layer<>nil then
         begin
            if CFace.Layer.Visible then
            begin
               Result:=True;
               exit;
            end;
         end;
      end;
   end;
   // Finally check if the edge is selected.
   // Selected edges must be visible at all times
   if not Result then Result:=Selected;
   if (not Result) and (FCurve<>nil) then Result:=FCurve.Selected;
end;{TFreesubdivisionControlEdge.FGetVisible}

procedure TFreesubdivisionControlEdge.Collapse;
var Face1,Face2   : TFreeSubdivisionControlFace;
    NewFace       : TFreeSubdivisionControlFace;
    Edge          : TFreeSubdivisionEdge;
    Ind1,Ind2     : Integer;
    Ind3,Ind4     : Integer;
    P1,P2         : TFreeSubdivisionPoint;
    S,E           : TFreeSubdivisionControlPoint;
    I             : Integer;
    Layer         : TFreeSubdivisionLayer;

    procedure Swap(var Ind1,Ind2:Integer);
    var Tmp:Integer;
    begin
       Tmp:=Ind1;
       Ind1:=Ind2;
       Ind2:=Tmp;
    end;

begin
   if NumberOfFaces=2 then if (Face[0] is TFreeSubdivisionControlFace) and (Face[1] is TFreeSubdivisionControlFace) then
   begin
      if (StartPoint.NumberOfEdges>2) and (EndPoint.NumberOfEdges>2) then
      begin
         if FCurve<>nil then FCurve.DeleteEdge(self);
         
         if selected then Selected:=False;
         S:=Startpoint as TFreeSubdivisionControlPoint;
         E:=Endpoint as TFreeSubdivisionControlPoint;
         Owner.Build:=False;
         Face1:=Face[0] as TFreeSubdivisionControlFace;
         Face2:=Face[1] as TFreeSubdivisionControlFace;
         // Check faces for consistent ordering of the points (same normal direction)
         // because inconsistent ordering can lead to access violations
         P1:=Face1.Point[Face1.NumberOfPoints-1];
         for I:=1 to Face1.NumberOfPoints do
         begin
            P2:=Face1.Point[I-1];
            if ((P1=FStartPoint) and (P2=FEndpoint)) or ((P2=FStartPoint) and (P1=FEndpoint)) then
            begin
               Ind1:=Face2.IndexOfPoint(P2);
               Ind2:=(Ind1+1) mod Face2.NumberOfPoints; // select the next index
               if Face2.Point[ind2]=P1 then
               begin
                  // Direction is OK, do nothing
               end else
               begin
                  // direction is not ok, invert points
                  Face2.FlipNormal;
               end;
               break;
            end else P1:=P2;
         end;

         Layer:=Face1.Layer;
         // Remove the controlfaces from the layers which they belong to
         Face1.Layer.DeleteControlFace(Face1);
         Face2.Layer.DeleteControlFace(Face2);
         Ind1:=Face1.FPoints.IndexOf(FStartPoint);
         Ind2:=Face1.FPoints.IndexOf(FEndPoint);
         if (Ind2<Ind1) and (abs(Ind2-Ind1)=1) then Swap(Ind1,Ind2);
         Ind3:=Face2.FPoints.IndexOf(FStartPoint);
         Ind4:=Face2.FPoints.IndexOf(FEndPoint);
         if (Ind4<Ind3) and (abs(Ind4-Ind3)=1) then Swap(Ind3,Ind4);

         if (Ind1=0) and (Ind2=Face1.NumberOfpoints-1) and
            (Ind3=0) and (Ind4=Face2.NumberOfpoints-1) then
         begin
            Swap(Ind1,Ind2);
            Swap(Ind3,Ind4);
         end;
         if (Ind1=0) and (Ind2=Face1.NumberOfpoints-1) then
         begin
            Swap(Ind1,Ind2);
         end;
         if (Ind3=0) and (Ind4=Face2.NumberOfpoints-1) then
         begin
            Swap(Ind3,Ind4);
         end;
         // Remove all references to Face1
         for I:=1 to Face1.NumberOfpoints do Face1.Point[I-1].DeleteFace(Face1);
         // Remove all references to Face2
         for I:=1 to Face2.NumberOfpoints do Face2.Point[I-1].DeleteFace(Face2);
         // Add the new face                                        f

         NewFace:=TFreeSubdivisionControlFace.Create(Owner);
         NewFace.FLayer:=layer;
         Owner.FControlFaces.Add(NewFace);
         for I:=0 to Ind1 do
         begin
            if NewFace.FPoints.IndexOf(Face1.Point[I])=-1 then
            begin
               NewFace.AddPoint(Face1.Point[I]);
            end;
         end;
         begin
            for I:=Ind4 to Face2.NumberOfpoints-1 do
            begin
               if NewFace.FPoints.IndexOf(Face2.Point[I])=-1 then
               begin
                  NewFace.AddPoint(Face2.Point[I]);
               end;
            end;
            for I:=0 to Ind3 do
            begin
               if NewFace.FPoints.IndexOf(Face2.Point[I])=-1 then
               begin
                  NewFace.AddPoint(Face2.Point[I]);
               end;
            end;
         end;
         for I:=Ind2 to Face1.NumberOfpoints-1 do
         begin
            if NewFace.FPoints.IndexOf(Face1.Point[I])=-1 then
            begin
               NewFace.AddPoint(Face1.Point[I]);
            end;
         end;
         // Check if all appropriate points are added
         if Newface.NumberOfpoints<>Face1.NumberOfpoints+Face2.NumberOfpoints-2 then
         messageDlg(Userstring(200)+#32+IntToStr(self.EdgeIndex),mtError,[mbOk],0);

         P1:=NewFace.Point[Newface.NumberOfPoints-1];
         for I:=1 to Newface.NumberOfPoints do
         begin
            P2:=NewFace.Point[I-1];
            Edge:=Owner.EdgeExists(P1,P2);
            if Edge<>nil then
            begin
               Ind1:=Edge.FFaces.IndexOf(Face1);
               if Ind1<>-1 then Edge.FFaces.Delete(Ind1);
               Ind1:=Edge.FFaces.IndexOf(Face2);
               if Ind1<>-1 then Edge.FFaces.Delete(Ind1);
               Edge.AddFace(NewFace);
               if Edge.NumberOfFaces<2 then Edge.Crease:=True;
            end;
            P1:=p2;
         end;
         // connect the new face to a layer
         Layer.AddControlFace(NewFace);
         if Crease then Crease:=false;
         StartPoint.DeleteEdge(self);
         Endpoint.DeleteEdge(self);
         Ind1:=Owner.FControlEdges.IndexOf(self);
         if Ind1<>-1 then Owner.FControlEdges.Delete(Ind1);
         Ind1:=Owner.FControlFaces.IndexOf(Face1);
         if Ind1<>-1 then Owner.FControlFaces.Delete(Ind1);
         Ind1:=Owner.FControlFaces.IndexOf(Face2);
         if Ind1<>-1 then Owner.FControlFaces.Delete(Ind1);
         Face1.Destroy;
         face2.Destroy;

         // check if startpoint and endpoint can be collapsed aswell
         if (S.NumberOfFaces>1) and (S.NumberOfEdges=2) then
         begin
            S.Collapse;
         end;
         if (E.NumberOfFaces>1) and (E.NumberOfEdges=2) then
         begin
            E.Collapse;
         end;
         Owner.Build:=False;
         Destroy;
      end;
   end;
end;{TFreesubdivisionControlEdge.Collapse}

constructor TFreesubdivisionControlEdge.Create(Owner:TFreeSubdivisionSurface);
begin
   Inherited Create(Owner);
end;{TFreesubdivisionControlEdge.Create}

procedure TFreesubdivisionControlEdge.Delete;
var I          : Integer;
    Index      : Integer;
    Face       : TFreeSubdivisionControlFace;
    Point      : TFreeSubdivisionControlPoint;
begin
   // delete from selection;
   Selected:=False;
   if FCurve<>nil then FCurve.DeleteEdge(self);
   Index:=Owner.FControlEdges.IndexOf(self);
   if Index<>-1 then
   begin
      Owner.FControlEdges.Delete(index);
      for I:=FFaces.Count downto 1 do
      begin
         Face:=FFaces[I-1];
         Face.Delete;
      end;
      // Remove endpoint from startpoint neighbours
      EndPoint.DeleteEdge(self);
      if EndPoint.NumberOfEdges=0 then
      begin
         Point:=Endpoint as TFreeSubdivisionControlPoint;
         Point.Delete;
      end;
      // Remove startpoint from endpoint neighbours
      StartPoint.DeleteEdge(self);
      if StartPoint.NumberOfEdges=0 then
      begin
         Point:=StartPoint as TFreeSubdivisionControlPoint;
         Point.Delete;
      end;
      Destroy;
   end;
end;{TFreesubdivisionControlEdge.Delete}

function TFreesubdivisionControlEdge.DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;
var Pt,Pt1,Pt2 : TPoint;
    P1,P2,M    : T3DCoordinate;
    Param      : TFloatType;
    Tmp        : Integer;
begin
   // Check if cursor position lies within the boundaries
   P:=ZERO;
   Result:=10000;
   if Viewport.ViewType=fvBodyplan then
   begin
      P1:=StartPoint.FCoordinate;
      P2:=FEndPoint.FCoordinate;
      if (((P1.X<Owner.MainframeLocation) and (P2.X>Owner.MainframeLocation)) or
         ((P1.X>Owner.MainframeLocation) and (P2.X<Owner.MainframeLocation))) and (not Owner.DrawMirror) then
      begin
         if P2.X-P1.X<>0 then M:=Interpolate(P1,P2,(Owner.MainframeLocation-P1.X)/(P2.X-P1.X))
                         else M:=MidPoint(P1,P2);
         if P1.X<=Owner.MainframeLocation then
         begin
            // P2 lies on port
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P2,M,Param);
            end;
            // P1 lies on starboard
            P1.Y:=-P1.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P1,M,Param);
            end;
         end else
         begin
            // P1 lies on port
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P1,M,Param);
            end;
            // P2 lies on starboard
            P2.Y:=-P2.Y;
            M.Y:=-M.Y;
            Pt1:=Viewport.Project(P2);
            Pt2:=Viewport.Project(M);
            Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
            if Tmp<Result then
            begin
               Result:=Tmp;
               P:=Interpolate(P2,M,Param);
            end;
         end;
      end else
      begin
         if P1.X<=Owner.MainframeLocation then P1.Y:=-P1.Y;
         if P2.X<=Owner.MainframeLocation then P2.Y:=-P2.Y;
         Pt1:=Viewport.Project(P1);
         Pt2:=Viewport.Project(P2);
         Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
         if Tmp<Result then
         begin
            Result:=Tmp;
            P:=Interpolate(P1,P2,Param);
         end;
      end;
   end else
   begin
      Pt.X:=X;
      Pt.Y:=Y;
      Pt1:=Viewport.Project(StartPoint.Coordinate);
      Pt2:=Viewport.Project(EndPoint.Coordinate);
      Result:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
      P:=Interpolate(StartPoint.Coordinate,EndPoint.Coordinate,Param);
   end;
end;{TFreesubdivisionControlEdge.DistanceToCursor}

procedure TFreesubdivisionControlEdge.Draw(DrawMirror:Boolean;Viewport:TFreeViewport);
var P1,P2,M : T3DCoordinate;
    Pt1,Pt2 : TPoint;
begin
   if Visible then
   begin
      P1:=StartPoint.FCoordinate;
      P2:=EndPoint.FCoordinate;
      if Viewport.ViewportMode<>vmWireframe then
      begin
         Viewport.DrawLineToZBuffer(P1,P2,GetRValue(Color),GetGValue(Color),GetBValue(Color));
      end else
      begin
         Viewport.PenColor:=(Color);
         if Crease then Viewport.SetPenWidth(2)
                   else Viewport.SetPenWidth(1);
         if Viewport.ViewType=fvBodyplan then
         begin
            if ((P1.X<Owner.MainframeLocation) and (P2.X>Owner.MainframeLocation)) or
               ((P1.X>Owner.MainframeLocation) and (P2.X<Owner.MainframeLocation)) then
            begin
               if P2.X-P1.X<>0 then M:=Interpolate(P1,P2,(Owner.MainframeLocation-P1.X)/(P2.X-P1.X))
                               else M:=MidPoint(P1,P2);
               if P1.X<=Owner.MainframeLocation then
               begin
                  // P2 lies on port
                  Pt1:=Viewport.Project(P2);
                  Pt2:=Viewport.Project(M);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
                  // P1 lies on starboard
                  P1.Y:=-P1.Y;
                  M.Y:=-M.Y;
                  Pt1:=Viewport.Project(P1);
                  Pt2:=Viewport.Project(M);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
               end else
               begin
                  // P1 lies on port
                  Pt1:=Viewport.Project(P1);
                  Pt2:=Viewport.Project(M);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
                  // P2 lies on starboard
                  P2.Y:=-P2.Y;
                  M.Y:=-M.Y;
                  Pt1:=Viewport.Project(P2);
                  Pt2:=Viewport.Project(M);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
               end;
            end else
            begin
               if P1.X<=Owner.MainframeLocation then P1.Y:=-P1.Y;
               if P2.X<=Owner.MainframeLocation then P2.Y:=-P2.Y;
               Pt1:=Viewport.Project(P1);
               Pt2:=Viewport.Project(P2);
               Viewport.MoveTo(Pt1.X,Pt1.Y);
               Viewport.LineTo(Pt2.X,Pt2.Y);
            end;
         end else
         begin
            Pt1:=Viewport.Project(P1);
            Pt2:=Viewport.Project(P2);
            Viewport.MoveTo(Pt1.X,Pt1.Y);
            Viewport.LineTo(Pt2.X,Pt2.Y);
         end;
      end;
   end;
end;{TFreesubdivisionControlEdge.Draw}

function TFreeSubdivisionControlEdge.InsertControlPoint(P:T3DCoordinate):TFreeSubdivisionControlpoint;
var I     : Integer;
    I1,I2 : Integer;
    Face  : TFreeSubdivisionFace;
    Edge  : TFreeSubdivisionControlEdge;
    ds,de : TFloatType;
begin
   result:=nil;
   ds := DistPP3D(P,FStartPoint.Coordinate);
   de := DistPP3D(P,FEndPoint.Coordinate);
   if (abs(ds)<=1e-3) or (abs(de)<=1e-3) then exit; //such point exists

   Result:=TFreeSubdivisionControlpoint.Create(FOwner);
   Result.FCoordinate:=P;
   if FCurve<>nil then
   begin
      // insert the new point in the controlcurve
      FCurve.InsertControlPoint(TFreeSubdivisionControlPoint(FStartPoint),TFreeSubdivisionControlPoint(FEndPoint),Result);
   end;
   FOwner.FControlPoints.Add(Result);
   for I:=1 to NumberOfFaces do
   begin
      Face:=FFaces[I-1];
      I1:=Face.FPoints.IndexOf(FStartPoint);
      I2:=Face.FPoints.IndexOf(FEndPoint);
      if (I1<>-1) and (I2<>-1) then
      begin
         if (I2=I1+1) then Face.FPoints.Insert(I2,Result) else
            if (I1=I2+1) then Face.FPoints.Insert(I1,Result) else
               if (I1=0) and (I2=Face.FPoints.Count-1) then Face.FPoints.Insert(0,Result) else
                  if (I2=0) and (I1=Face.FPoints.Count-1) then Face.FPoints.Insert(0,Result);
         Result.AddFace(Face);
      end;
   end;
   FEndpoint.DeleteEdge(self);
   Edge:=FOwner.AddControlEdge(Result,FEndPoint);
   Edge.FCrease:=FCrease;
   Edge.FCurve:=FCurve;
   if FCrease then Result.FVertexType:=svCrease;
   for I:=1 to NumberOffaces do Edge.AddFace(FFaces[I-1]);
   FEndPoint:=Result;
   Result.AddEdge(self);
end;{TFreeSubdivisionControlEdge.InsertControlPoint}

procedure TFreesubdivisionControlEdge.LoadBinary(Source:TFreeFileBuffer);
var Index   : Integer;
    Sel     : Boolean;
begin
   // Read startpoint
   Source.Load(Index);
   if Index=-1 then Index:=0;
   if Index<>-1 then
   begin
      FStartPoint:=Owner.FControlPoints[Index];
      FStartPoint.FEdges.Add(Self);
   end;
   // Read endpoint
   Source.Load(Index);
   if Index=-1 then Index:=0;
   if Index<>-1 then
   begin
      FEndPoint:=Owner.FControlPoints[Index];
      FEndPoint.FEdges.Add(Self);
   end;
   Source.Load(FCrease);
   Source.Load(Sel);
   if Sel then selected:=True;
end;{TFreesubdivisionControlEdge.LoadBinary}

procedure TFreesubdivisionControlEdge.LoadFromStream(var LineNr:Integer;Strings:TStringList);
var Str:string;
    Index:Integer;
    Sel:Boolean;
begin
   Inc(LineNr);
   Str:=Strings[LineNr];
   // FStartpoint
   Index:=ReadIntFromStr(LineNr,Str);
   if index=-1 then index:=0;
   if Index<>-1 then
   begin
      FStartPoint:=Owner.FControlPoints[Index];
      FStartPoint.FEdges.Add(Self);
   end;
   // FEndpoint
   Index:=ReadIntFromStr(LineNr,Str);
   if index=-1 then index:=0;
   if Index<>-1 then
   begin
      FEndPoint:=Owner.FControlPoints[Index];
      FEndPoint.FEdges.Add(Self);
   end;
   // FCrease
   FCrease:=ReadIntFromStr(LineNr,Str)=1;
   if Str<>'' then
   begin
      // Flag to indicate that this edge was selected when the model was saved (for undo-purposes)
      Sel:=ReadBoolFromStr(LineNr,Str);
      Selected:=Sel;
   end;
end;{TFreesubdivisionControlEdge.LoadFromStream}

procedure TFreesubdivisionControlEdge.SaveBinary(Destination:TFreeFileBuffer);
begin
   Destination.Add(Owner.FControlPoints.SortedIndexOf(FStartpoint));
   Destination.Add(Owner.FControlPoints.SortedIndexOf(FEndpoint));
   Destination.Add(FCrease);
   Destination.Add(Selected);
end;{TFreesubdivisionControlEdge.SaveBinary}

procedure TFreesubdivisionControlEdge.SaveToStream(Strings:TStringlist);
begin
   Strings.Add(IntToStr(Owner.FControlPoints.SortedIndexOf(FStartpoint))+#32+
               IntToStr(Owner.FControlPoints.SortedIndexOf(FEndpoint))+#32+
               IntToStr(Ord(FCrease))+#32+BoolToStr(Selected));
end;{TFreesubdivisionControlEdge.SaveToStream}

procedure TFreesubdivisionControlEdge.Trace;
var P          : TFreeSubdivisionControlPoint;
    Edge       : TFreeSubdivisionControlEdge;
    I,J,Index  : Integer;
    Sharesface : Boolean;
//    Point   : TFreeSubdivisionControlPoint;
//    Entity  : TFreeSubdivisionBase;
begin
   P:=Startpoint as TFreeSubdivisionControlPoint;

//begin select point
//   Point:=Entity as TFreeSubdivisionControlPoint;
//   Point.Selected:=not Point.Selected;
//   if not Point.Selected then Point:=SelectedControlPoint[NumberOfSelectedControlPoints-1];
//   for J:=1 to NumberOfViewports do self.Viewport[J-1].Refresh;
//end select point

   if (P.RegularPoint) and (P.VertexType<>svCorner) then
   begin
      // Find next edge
      for I:=1 to P.NumberOfEdges do if P.Edge[I-1]<>self then
      begin
         Edge:=P.Edge[I-1] as TFreeSubdivisionControlEdge;
         if (Edge.Selected<>self.Selected) and (Edge.Crease=self.Crease) then
         begin
            SharesFace:=False;
            for J:=1 to self.NumberOfFaces do
            begin
               Index:=Edge.FFaces.IndexOf(self.Face[J-1]);
               if Index<>-1 then
               begin
                  SharesFace:=True;
                  Break;
               end;
            end;
            if not SharesFace then
            begin
               if Edge.StartPoint=self.StartPoint then Edge.SwapData;
               Edge.Selected:=self.Selected;
               Edge.Trace;
               Break;
            end;
         end;
      end;
   end;
   P:=Endpoint as TFreeSubdivisionControlPoint;
   if (P.RegularPoint) and (P.VertexType<>svCorner) then
   begin
      // Find next edge
      for I:=1 to P.NumberOfEdges do if P.Edge[I-1]<>self then
      begin
         Edge:=P.Edge[I-1] as TFreeSubdivisionControlEdge;
         SharesFace:=False;
         if (Edge.Selected<>self.Selected) and (Edge.Crease=self.Crease) then
         begin
            for J:=1 to self.NumberOfFaces do
            begin
               Index:=Edge.FFaces.IndexOf(self.Face[J-1]);
               if Index<>-1 then
               begin
                  SharesFace:=True;
                  Break;
               end;
            end;
            if not SharesFace then
            begin
               if Edge.Endpoint=self.EndPoint then Edge.SwapData;
               Edge.Selected:=self.Selected;
               Edge.Trace;
               Break;
            end;
         end;
      end;
   end;
end;{TFreesubdivisionControlEdge.Trace}

{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionFace                                     }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionFace.FGetArea:TFloatType;
var I:Integer;
    function TriangleArea(P1,P2,P3:T3DCoordinate):TFloatType;
    var ax,ay,az     : TFloatType;
    begin
       ax:=0.5*((P1.y-P2.y)*(P1.z+P2.z)+(P2.y-P3.y)*(P2.z+P3.z)+
                (P3.y-P1.y)*(P3.z+P1.z));
       ay:=0.5*((P1.z-P2.z)*(P1.x+P2.x)+(P2.z-P3.z)*(P2.x+P3.x)+
                (P3.z-P1.z)*(P3.x+P1.x));
       az:=0.5*((P1.x-P2.x)*(P1.y+P2.y)+(P2.x-P3.x)*(P2.y+P3.y)+
                (P3.x-P1.x)*(P3.y+P1.y));
       Result:=Sqrt(ax*ax+ay*ay+az*az);
    end;{TriangleArea}

begin
   Result:=0.0;
   for I:=3 to NumberOfPoints do Result:=Result+TriangleArea(Point[0].Coordinate,Point[I-2].Coordinate,Point[I-1].Coordinate);
end;{TFreeSubdivisionFace.FGetArea}

function TFreeSubdivisionFace.FGetFaceCenter:T3DCoordinate;
var I : Integer;
    P : TFreeSubdivisionPoint;
begin
   Result:=ZERO;
   if FPoints.Count>1 then
   begin
      for I:=1 to FPoints.Count do
      begin
         P:=FPoints[I-1];
         Result.X:=Result.X+P.FCoordinate.X;
         Result.Y:=Result.Y+P.FCoordinate.Y;
         Result.Z:=Result.Z+P.FCoordinate.Z;
      end;
      Result.X:=Result.X/FPoints.Count;
      Result.Y:=Result.Y/FPoints.Count;
      Result.Z:=Result.Z/FPoints.Count;
   end;
end;{TFreeSubdivisionFace.FGetCentre}

function TFreeSubdivisionFace.FGetFaceNormal:T3DCoordinate;
var I       : Integer;
    N,C,P   : T3DCoordinate;
    P1,P2   : TFreeSubdivisionPoint;
begin
   Result:=ZERO;
   C:=ZERO;
   //calculate center of the face
   for I:=1 to NumberOfPoints do
   begin
      P:=Point[I-1].Coordinate;
      C.X:=C.X+P.X;
      C.Y:=C.Y+P.Y;
      C.Z:=C.Z+P.Z;
   end;
   C.X:=C.X/NumberOfPoints;
   C.Y:=C.Y/NumberOfPoints;
   C.Z:=C.Z/NumberOfPoints;
   // calculate normal
   P1:=Point[NumberOfPoints-1];
   for I:=1 to NumberOfPoints do
   begin
      P2:=Point[I-1];
      N:=UnifiedNormal(C,P1.Coordinate,P2.Coordinate);
      Result.X:=Result.X+N.X;
      Result.Y:=Result.Y+N.Y;
      Result.Z:=Result.Z+N.Z;
      P1:=P2;
   end;
   Result:=Normalize(Result);
end;{TFreeSubdivisionFace.FGetFaceNormal}

function TFreeSubdivisionFace.FGetNumberOfPoints;
begin
   Result:=FPoints.Count;
end;{TFreeSubdivisionFace.FGetNumberOfPoints}

function TFreeSubdivisionFace.FGetPoint(Index:Integer):TFreeSubdivisionPoint;
begin
   Result:=FPoints[Index];
end;{TFreeSubdivisionFace.FGetPoint}

procedure TFreeSubdivisionFace.AddPoint(Point:TFreeSubdivisionPoint);
begin
   FPoints.Add(Point);
   Point.AddFace(self);
end;{TFreeSubdivisionFace.AddPoint}

function TFreeSubdivisionFace.CalculateFacePoint:TFreeSubdivisionPoint;
var I    : Integer;
    P    : T3DCoordinate;
    Centre:T3DCoordinate;
begin
   Result:=nil;
   Centre:=ZERO;
   if (FPoints.Count>3) or (Owner.FSubdivisionMode=fmCatmullClark) then
   begin
      if FPoints.Count>0 then
      begin
         for I:=1 to FPoints.Count do
         begin
            P:=Point[I-1].FCoordinate;
            Centre.X:=Centre.X+P.X;
            Centre.Y:=Centre.Y+P.Y;
            Centre.Z:=Centre.Z+P.Z;
         end;
         Centre.X:=Centre.X/FPoints.Count;
         Centre.Y:=Centre.Y/FPoints.Count;
         Centre.Z:=Centre.Z/FPoints.Count;
      end;
      Result:=TFreeSubdivisionPoint.Create(Owner);
      Result.FCoordinate:=Centre;
   end else if FPoints.Count=3 then
   begin
      Result:=nil;
   end;
end;{TFreeSubdivisionFace.CalculateFacePoint}

procedure TFreeSubdivisionFace.Clear;
begin
   FPoints.Clear;
end;{TFreeSubdivisionFace.Clear}

constructor TFreeSubdivisionFace.Create(Owner:TFreeSubdivisionSurface);
begin
   inherited Create(Owner);
   FPoints:=TFasterlist.Create;
   Clear;
end;{TFreeSubdivisionFace.Create}

destructor TFreeSubdivisionFace.Destroy;
begin
   Clear;
   FPoints.Destroy;
   FPoints := nil;
   inherited Destroy;
end;{TFreeSubdivisionFace.Destroy}

// Inverts the point ordering of the face
procedure TFreeSubdivisionFace.FlipNormal;
var Mid :Integer;
    I   : INteger;
begin
   Mid:=(FPoints.Count div 2) - 1;
   for I:=0 to Mid do
   begin
      FPoints.Exchange(I,FPoints.Count-I-1);
   end;
end;{TFreeSubdivisionFace.FlipNormal}

function TFreeSubdivisionFace.IndexOfPoint(P:TFreeSubdivisionPoint):Integer;
begin
   Result:=FPoints.IndexOf(P);
end;{TFreeSubdivisionFace.IndexOfPoint}

procedure TFreeSubdivisionFace.Subdivide(Owner:TFreeSubdivisionSurface;ControlFace:Boolean;VertexPoints,EdgePoints,FacePoints,InteriorEdges,ControlEdges,Dest:TFasterList);
var P2         : TFreeSubdivisionPoint;
    I,TmpIndex : Integer;
    NewFace    : TFreeSubdivisionFace;
    Index,J    : Integer;
    Pts        : array[0..3] of TFreeSubdivisionPoint;
    PrevEdge   : TFreeSubdivisionEdge;
    CurrEdge   : TFreeSubdivisionEdge;

    PrevEdgePoint,
    CurrEdgePoint,
    P2Point,
    NewLocation:TFreeSubdivisionPoint;

    procedure EdgeCheck(P1,P2:TFreeSubdivisionPoint;Crease,Controledge:Boolean;Curve:TFreeSubdivisionControlCurve);
    var NewEdge:TFreeSubdivisionEdge;
    begin
       NewEdge:=nil;
       if (P1<>nil) and (P2<>nil) then
       begin
          NewEdge:=Owner.EdgeExists(P1,P2);
          if NewEdge=nil then
          begin
            NewEdge:=TFreeSubdivisionEdge.Create(Owner);
            NewEdge.FStartpoint:=P1;
            NewEdge.FEndpoint:=P2;
            NewEdge.FFaces.Capacity:=2;
            NewEdge.FStartpoint.FEdges.Add(NewEdge);
            NewEdge.FEndpoint.FEdges.Add(NewEdge);
            NewEdge.FControlEdge:=ControlEdge;
            NewEdge.FCrease:=Crease;
            if NewEdge.FControlEdge then ControlEdges.Add(NewEdge)
                                    else InteriorEdges.Add(NewEdge);
          end else if NewEdge.FControlEdge then ControlEdges.Add(NewEdge);
          if NewEdge.FControlEdge then
          begin
             NewEdge.FCurve:=Curve;
          end;
       end else
       begin
          Showmessage('Error in TFreeSubdivisionFace.Subdivide');
       end;
       NewEdge.FFaces.Add(NewFace);
    end;{EdgeCheck}

begin
   if (NumberOfPoints<>3) or (Owner.FSubdivisionMode=fmCatmullClark) then
   begin
      for I:=1 to FPoints.Count do
      begin
         P2:=FPoints[I-1];
         Index:=(I-2+FPoints.Count) mod FPoints.Count;
         PrevEdge:=Owner.EdgeExists(P2,FPoints[Index]);
         Index:=(I+FPoints.Count) mod FPoints.Count;
         CurrEdge:=Owner.EdgeExists(P2,FPoints[Index]);
         Index:=(I-1) mod 4;
         TmpIndex:=VertexPoints.SortedIndexOf(P2);
         Pts[Index]:=VertexPoints.Objects[TmpIndex];// P2.FNewLocation;
         P2Point:=Pts[Index];
         Index:=(Index+1) mod 4;
         TmpIndex:=EdgePoints.SortedIndexOf(CurrEdge);
         Pts[index]:=EdgePoints.Objects[TmpIndex];//CurrEdge.FNewLocation;
         CurrEdgePoint:=Pts[index];
         Index:=(Index+1) mod 4;
         TmpIndex:=FacePoints.SortedIndexOf(self);
         Pts[index]:=FacePoints.Objects[TmpIndex];//self.FNewLocation;
         NewLocation:=Pts[index];
         Index:=(Index+1) mod 4;
         TmpIndex:=EdgePoints.SortedIndexOf(PrevEdge);
         Pts[index]:=Edgepoints.Objects[TmpIndex];// PrevEdge.FNewLocation;
         PrevEdgePoint:=Pts[index];
         // add the new face
         NewFace:=TFreeSubdivisionFace.Create(Owner);
         Dest.Add(NewFace);
         try
            EdgeCheck(PrevEdgePoint,P2Point,PrevEdge.Crease,PrevEdge.FControlEdge or ControlFace,PrevEdge.FCurve);
            EdgeCheck(P2Point,CurrEdgePoint,CurrEdge.Crease,CurrEdge.FControlEdge or ControlFace,CurrEdge.FCurve);
            EdgeCheck(CurrEdgePoint,NewLocation,False,False,nil);
            EdgeCheck(PrevEdgePoint,NewLocation,False,False,nil);
         except
            {
            EdgeCheck(PrevEdgePoint,P2Point,PrevEdge.Crease,PrevEdge.FControlEdge or ControlFace,PrevEdge.FCurve);
            EdgeCheck(P2Point,CurrEdgePoint,CurrEdge.Crease,CurrEdge.FControlEdge or ControlFace,CurrEdge.FCurve);
            EdgeCheck(CurrEdgePoint,NewLocation,False,False,nil);
            EdgeCheck(PrevEdgePoint,NewLocation,False,False,nil);
            }
         end;


         NewFace.FPoints.Capacity:=4;
         for J:=1 to 4 do
         begin
            // Add new face to points
            Pts[J-1].FFaces.Add(NewFace);
            NewFace.FPoints.Add(Pts[J-1]);
         end;
      end;
   end else if NumberOfPoints=3 then
   begin
      // Special case, quadrisect triancle by connecting all three
      // edge points
      // first the three surrounding triangles
      for I:=1 to FPoints.Count do
      begin
         P2:=FPoints[I-1];
         Index:=(I-2+FPoints.Count) mod FPoints.Count;
         PrevEdge:=Owner.EdgeExists(P2,FPoints[Index]);
         Index:=(I+FPoints.Count) mod FPoints.Count;
         CurrEdge:=Owner.EdgeExists(P2,FPoints[Index]);

         Index:=0;
         TmpIndex:=EdgePoints.SortedIndexOf(PrevEdge);
         Pts[Index]:=EdgePoints.Objects[tmpindex];// PrevEdge.FNewLocation;
         Index:=1;
         TmpIndex:=VertexPoints.SortedIndexOf(P2);
         Pts[index]:=VertexPoints.Objects[Tmpindex];// P2.FNewLocation;
         Index:=2;
         TmpIndex:=EdgePoints.SortedIndexOf(CurrEdge);
         Pts[index]:=Edgepoints.Objects[Tmpindex];// CurrEdge.FNewLocation;
         // add the new face
         NewFace:=TFreeSubdivisionFace.Create(Owner);
         Dest.Add(NewFace);
         EdgeCheck(Pts[0],Pts[1],PrevEdge.Crease,PrevEdge.FControlEdge or ControlFace,PrevEdge.FCurve);
         EdgeCheck(Pts[1],Pts[2],CurrEdge.Crease,CurrEdge.FControlEdge or ControlFace,CurrEdge.FCurve);
         EdgeCheck(Pts[2],Pts[0],False,False,nil);
         NewFace.FPoints.Capacity:=3;
         for J:=1 to 3 do
         begin
            // Add new face to points
            Pts[J-1].FFaces.Add(NewFace);
            NewFace.FPoints.Add(Pts[J-1]);
            Pts[J-1].AddFace(NewFace);
         end;
      end;

      // then the center triangle
      for I:=1 to FPoints.Count do
      begin
         P2:=FPoints[I-1];
         Index:=(I-2+FPoints.Count) mod FPoints.Count;
         PrevEdge:=Owner.EdgeExists(P2,FPoints[Index]);
         TmpIndex:=EdgePoints.SortedIndexOf(PrevEdge);
         Pts[I-1]:=EdgePoints.Objects[TmpIndex];// PrevEdge.FNewLocation;
      end;
      // add the new face
      NewFace:=TFreeSubdivisionFace.Create(Owner);
      Dest.Add(NewFace);
      EdgeCheck(Pts[0],Pts[1],False,False,nil);
      EdgeCheck(Pts[1],Pts[2],False,False,nil);
      EdgeCheck(Pts[2],Pts[0],False,False,nil);
      NewFace.FPoints.Capacity:=3;
      for J:=1 to 3 do
      begin
         // Add new face to points
         Pts[J-1].FFaces.Add(NewFace);
         NewFace.FPoints.Add(Pts[J-1]);
         Pts[J-1].AddFace(NewFace);
      end;
   end;
end;{TFreeSubdivisionFace.Subdivide}

{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionControlFace                              }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionControlFace.FGetChild(Index:Integer):TFreeSubdivisionFace;
begin
   Result:=FChildren[index];
end;{TFreeSubdivisionControlFace.FGetChild}

function TFreeSubdivisionControlFace.FGetChildCount:Integer;
begin
   Result:=FChildren.Count;
end;{TFreeSubdivisionControlFace.FGetChildCount}

function TFreeSubdivisionControlFace.FGetColor:TColor;
begin
   if Selected then Result:=Owner.Selectedcolor
               else Result:=Layer.Color;
end;{TFreeSubdivisionControlFace.FGetColor}

function TFreeSubdivisionControlFace.FGetControlEdge(Index:Integer):TFreeSubdivisionEdge;
begin
   Result:=FControlEdges[Index];
end;{TFreeSubdivisionControlFace.FGetControlEdge}

procedure TFreeSubdivisionControlFace.FSetSelected(val:Boolean);
var Index : Integer;
begin
   Index:=Owner.FSelectedControlFaces.IndexOf(self);
   if Val then
   begin
      // Only add if it is not already in the list
      if Index=-1 then Owner.FSelectedControlFaces.Add(self);
   end else
   begin
      if Index<>-1 then Owner.FSelectedControlFaces.Delete(index);
   end;
   if Assigned(Owner.FOnSelectItem) then Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlFace.FSetSelected}

function TFreeSubdivisionControlFace.FGetSelected:Boolean;
begin
   Result:=Owner.FSelectedControlFaces.IndexOf(self)<>-1;
end;{TFreeSubdivisionControlFace.FGetSelected}

function TFreeSubdivisionControlFace.FGetControlEdgeCount:Integer;
begin
   Result:=FControlEdges.Count;
end;{TFreeSubdivisionControlFace.FGetControlEdgeCount}

function TFreeSubdivisionControlFace.FGetEdge(Index:Integer):TFreeSubdivisionEdge;
begin
   Result:=FEdges[Index];
end;{TFreeSubdivisionControlFace.FGetEdge}

function TFreeSubdivisionControlFace.FGetEdgeCount:Integer;
begin
   Result:=FEdges.Count;
end;{TFreeSubdivisionControlFace.FGetEdgeCount}

function TFreeSubdivisionControlFace.FGetIndex:Integer;
begin
   Result:=Owner.FControlFaces.IndexOf(self);
end;{TFreeSubdivisionControlFace.FGetIndex}

function TFreeSubdivisionControlFace.FGetVisible:Boolean;
begin
   Result:=Layer.Visible or Selected;
end;{TFreeSubdivisionControlFace.FGetVisible}

procedure TFreeSubdivisionControlFace.FSetLayer(Val:TFreeSubdivisionLayer);
begin
   if Val<>FLayer then
   begin
      if FLayer<>nil then
      begin
         // Disconnect from current layer
         FLayer.DeleteControlFace(self);
         FLayer:=nil;
      end;
      FLayer:=Val;
      if FLayer<>nil then
      begin
         // Connect to the new layer
         FLayer.AddControlFace(self);
      end;
   end;
end;{TFreeSubdivisionControlFace.FSetLayer}

procedure TFreeSubdivisionControlFace.CalcExtents;
var I,J     : Integer;
    Face    : TFreeSubdivisionFace;
    P1      : TFreeSubdivisionPoint;
begin
   // Calculate min/max coordinate of all children
   if NumberOfPoints>0 then FMin:=Point[0].Coordinate
                       else FMin:=ZERO;
   FMax:=FMin;
   if FChildren.Count>0 then
   begin
      for I:=1 to FChildren.Count do
      begin
         Face:=FChildren[I-1];
         for J:=1 to Face.FPoints.Count do
         begin
            P1:=Face.FPoints[J-1];
            if (I=1) and (J=1) then
            begin
               FMin:=P1.Coordinate;
               FMax:=FMin;
            end;
            MinMax(P1.FCoordinate,FMin,FMax);
         end;
      end;
   end else
   for I:=2 to NumberOfPoints do
     MinMax(Point[I-1].Coordinate,FMin,FMax);
end;{TFreeSubdivisionControlFace.CalcExtents}

procedure TFreeSubdivisionControlFace.Clear;
begin
   ClearChildren;
   inherited Clear;
   FLayer:=nil;
end;{TFreeSubdivisionControlFace.Clear}

procedure TFreeSubdivisionControlFace.ClearChildren;
var I:Integer;
begin
   if Assigned(FChildren) then begin
     for I:=1 to Childcount do
       Child[I-1].Destroy;
     FChildren.Clear;
   end;
   if Assigned(FEdges) then begin
     for I:=1 to EdgeCount do
       Edge[I-1].Destroy;
     FEdges.Clear;
   end;
end;{TFreeSubdivisionControlFace.ClearChildren}

constructor TFreeSubdivisionControlFace.Create(Owner:TFreeSubdivisionSurface);
begin
   FLayer:=nil;
   FChildren:=TFasterlist.Create;
   FEdges:=TFasterList.Create;
   FControlEdges:=TFasterList.Create;
   Inherited Create(Owner);
end;{TFreeSubdivisionControlFace.Create}

function TFreeSubdivisionControlFace.DistanceToCursor(X,Y:Integer;var P:T3DCoordinate;Viewport:TFreeViewport):integer;
var I,Dist,Tmp : Integer;
    Param      : TFloatType;
    Edge       : TFreeSubdivisionEdge;
    Pt1,Pt2    : TPoint;
    P1,P2,M    : T3DCoordinate;
    D : TFloatType;
begin
   Result:=1000000;
   P:=ZERO;
   if Owner.ShowInteriorEdges then
   begin
      // check distance to all interior edges
      for I:=1 to FEdges.Count do
      begin
         Edge:=FEdges[I-1];
         P1:=Edge.Startpoint.Coordinate;
         P2:=Edge.FEndpoint.Coordinate;

         if (Viewport.ViewType=fvBodyplan) and (not Owner.DrawMirror) then
         begin
            if ((P1.X<Owner.MainframeLocation) and (P2.X>Owner.MainframeLocation)) or
               ((P1.X>Owner.MainframeLocation) and (P2.X<Owner.MainframeLocation)) then
            begin
               if P2.X-P1.X<>0 then M:=Interpolate(P1,P2,(Owner.MainframeLocation-P1.X)/(P2.X-P1.X))
                               else M:=MidPoint(P1,P2);
               if P1.X<=Owner.MainframeLocation then
               begin
                  // P2 lies on port
                  Pt1:=Viewport.Project(P2);
                  Pt2:=Viewport.Project(M);
                  Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
                  if Tmp<Result then
                  begin
                     Result:=Tmp;
                     P:=Interpolate(P2,M,Param);
                  end;
                  // P1 lies on starboard
                  P1.Y:=-P1.Y;
                  M.Y:=-M.Y;
                  Pt1:=Viewport.Project(P1);
                  Pt2:=Viewport.Project(M);
                  Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
                  if Tmp<Result then
                  begin
                     Result:=Tmp;
                     P:=Interpolate(P1,M,Param);
                  end;
               end else
               begin
                  // P1 lies on port
                  Pt1:=Viewport.Project(P1);
                  Pt2:=Viewport.Project(M);
                  Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
                  if Tmp<Result then
                  begin
                     Result:=Tmp;
                     P:=Interpolate(P1,M,Param);
                  end;
                  // P2 lies on starboard
                  P2.Y:=-P2.Y;
                  M.Y:=-M.Y;
                  Pt1:=Viewport.Project(P2);
                  Pt2:=Viewport.Project(M);
                  Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
                  if Tmp<Result then
                  begin
                     Result:=Tmp;
                     P:=Interpolate(P2,M,Param);
                  end;
               end;
            end else
            begin
               if P1.X<=Owner.MainframeLocation then P1.Y:=-P1.Y;
               if P2.X<=Owner.MainframeLocation then P2.Y:=-P2.Y;
               Pt1:=Viewport.Project(P1);
               Pt2:=Viewport.Project(P2);
               Tmp:=Round(DistanceToLine(Pt1,Pt2,X,Y,Param));
               if Tmp<Result then
               begin
                  Result:=Tmp;
                  P:=Interpolate(P1,P2,Param);
               end;
            end;
         end else
         begin
//            Dist:=Round(DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param));
            D := DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param);
            Dist:=Round(D);
            if I=1 then
            begin
               Result:=Dist;
               P:=Interpolate(P1,P2,Param);
            end else if Dist<Result then
            begin
               Result:=Dist;
               P:=Interpolate(P1,P2,Param);
            end;
            if (Layer.Symmetric) and (Owner.DrawMirror) then
            begin
               P1.Y:=-P1.Y;
               P2.Y:=-P2.Y;
               Dist:=Round(DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param));
               if Dist<Result then
               begin
                  Result:=Dist;
                  P:=Interpolate(P1,P2,Param);
               end;
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionControlFace.DistanceToCursor}

procedure TFreeSubdivisionControlFace.Delete;
var I          : Integer;
    Index      : Integer;
    Edge       : TFreesubdivisionControlEdge;
    P1,P2      : TFreeSubdivisionPoint;
begin
   // delete from selection;
   Selected:=false;
   // remove from layer
   Layer:=nil;
   Index:=Owner.FControlFaces.IndexOf(Self);
   if Index<>-1 then
   begin
      Owner.FControlFaces.Delete(Index);
      P1:=Point[NumberOfPoints-1];
      P1.DeleteFace(Self);
      for I:=1 to NumberOfPoints do
      begin
         P2:=Point[I-1];
         Edge:=Owner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
         P2.DeleteFace(Self);
         if Edge<>nil then
         begin
            Edge.DeleteFace(Self);
            if Edge.NumberOfFaces=0 then Edge.Delete;
         end;
         P1:=P2;
      end;
   end;
   Owner.Build:=False;
   Clear;
   //Destroy;
   DelayedDestroyList.Add(Self);
end;{TFreeSubdivisionControlFace.Delete}

destructor TFreeSubdivisionControlFace.Destroy;
begin
   logger.debug('TFreeSubdivisionControlFace.Destroy. $'+IntToStr(PtrUInt(Self)));
   FEdges.Destroy;
   FEdges := nil;
   FControlEdges.Destroy;
   FControlEdges := nil;
   FChildren.Destroy;
   FChildren := nil;
   Inherited Destroy;
end;{TFreeSubdivisionControlFace.Destroy}

procedure TFreeSubdivisionControlFace.Draw(Viewport:TFreeViewport);
    Type TIntData = record
                       P         : T3DCoordinate;
                       DotProd   : TFloatType;
                    end;

var I,J,K,index   : Integer;
    Edge          : TFreeSubdivisionEdge;
    Child         : TFreeSubdivisionFace;
    Point         : TFreeSubdivisionPoint;
    P1,P2,P3      : T3DCoordinate;
    Camera        : T3DCoordinate;
    R,G,B         : Byte;
    Ru,Gu,Bu      : byte;
    Alpha         : Byte;
    Min,Max,Tmp   : TFloatType;
    C1,C2,C3      : TFloatType;
    Above,Below   : TFreeCoordinateArray;
    Na,Nb         : Integer;
    Intersections : array of TIntData;
    Capacity      : Integer;
    Points        : TFasterList;

    procedure DrawNormal(P,N:T3DCoordinate;color:TColor);
    var P1,P2  : TPoint;
        V:T3DCoordinate;
        L,Ldes      : TFloatType;
    begin
      if (Viewport.ViewType=fvBodyplan) and (not Owner.DrawMirror) and (P.X<=Owner.MainframeLocation) then
      begin
         P.Y:=-P.Y;
         N.Y:=-N.Y;
      end;
      P1:=Viewport.Project(P);
      Viewport.PenColor:=color;
      V.X:=P.X+N.X;
      V.Y:=P.Y+N.Y;
      V.Z:=P.Z+N.Z;
      // Calculate the length of the normal on screen
      P2:=Viewport.Project(V);
      L:=sqr(P2.X-P1.X)+sqr(P2.Y-P1.Y);
      if L>=0 then L:=Sqrt(L) else L:=0.0;
      if L>0 then
      begin
         Ldes:=0.0075*Screen.Width;
         if LDes<10 then Ldes:=10;
         // Scale the length of the normal such that it is a fixed
         // length relative to the screen resolution
         N.X:=(Ldes/L)*N.X;
         N.Y:=(Ldes/L)*N.Y;
         N.Z:=(Ldes/L)*N.Z;
         V.X:=P.X+N.X;
         V.Y:=P.Y+N.Y;
         V.Z:=P.Z+N.Z;
         P2:=Viewport.Project(V);
         Viewport.MoveTo(P1.X,P1.Y);
         Viewport.LineTo(P2.X,P2.Y);
      end;
    end;{Draw normal}

    procedure ZebraStripe(Camera:T3DCoordinate;Point1,Point2,Point3:TFreeSubdivisionPoint;R,G,B,Zr,Zg,Zb:Byte);
    const Width=0.02;
    var d1,d2,d3     : TFloatType;
        Mindp,Maxdp  : TFloatType;
        Eye,Vector   : T3DCoordinate;
        P1,P2,P3     : T3DCoordinate;
        N1,N2,N3     : T3DCoordinate;
        NInt,Lo,Hi   : Integer;
        Pts          : array of T3DCoordinate;
        I,J,Npts     : Integer;

        procedure Add(P:T3DCoordinate;Dp:TFloatType);
        begin
           if NInt=Capacity then
           begin
              inc(Capacity,10);
              Setlength(Intersections,Capacity);
           end;
           Inc(NInt);
           Intersections[NInt-1].P:=P;
           Intersections[NInt-1].DotProd:=Dp;
        end;

        procedure Process(P:T3DCoordinate;Dp:TFloatType);
        var PrevDp   : TFloatType;
            I,L,H    : Integer;
            Val,T    : TFloatType;
            PrevP,P3D: T3DCoordinate;
        begin
           PrevDp:=Intersections[NInt-1].DotProd;
           L:=Trunc(PrevDp/Width);
           H:=Trunc(Dp/Width);
           if (H-L)>0 then
           begin
              PrevP:=Intersections[NInt-1].P;
              for I:=L to H do
              begin
                 Val:=I*Width;
                 if (Val>PrevDp) and (Val<Dp) then
                 begin
                  T:=(Val-PrevDp)/(Dp-PrevDp);
                  P3D.X:=PrevP.X+T*(P.X-PrevP.X);
                  P3D.Y:=PrevP.Y+T*(P.Y-PrevP.Y);
                  P3D.Z:=PrevP.Z+T*(P.Z-PrevP.Z);
                  Add(P3D,Val);
                 end;
              end;
              Add(P,Dp);
           end else if L-H>0 then
           begin
              PrevP:=Intersections[NInt-1].P;
              for I:=L downto H do
              begin
                 Val:=I*Width;
                 if (Val<PrevDp) and (Val>Dp) then
                 begin
                  T:=(Val-PrevDp)/(Dp-PrevDp);
                  P3D.X:=PrevP.X+T*(P.X-PrevP.X);
                  P3D.Y:=PrevP.Y+T*(P.Y-PrevP.Y);
                  P3D.Z:=PrevP.Z+T*(P.Z-PrevP.Z);
                  Add(P3D,Val);
                 end;
              end;
              Add(P,Dp);
           end else Add(P,Dp);
        end;{Process}

    begin
       NInt:=0;
       P1:=Point1.FCoordinate;
       P2:=Point2.FCoordinate;
       P3:=Point3.FCoordinate;
       Vector:=UnifiedNormal(P1,P2,P3);
       if Point1.VertexType in [svRegular,svDart] then N1:=Point1.Normal
                                                  else N1:=Vector;
       if Point2.VertexType in [svRegular,svDart] then N2:=Point2.Normal
                                                  else N2:=Vector;
       if Point3.VertexType in [svRegular,svDart] then N3:=Point3.Normal
                                                  else N3:=Vector;
       Eye:=Normalize(Subtract(Camera,P1));
       d1:=Dotproduct(Eye,N1);
       if d1<0 then d1:=-d1;
       Eye:=Normalize(Subtract(Camera,P2));
       d2:=Dotproduct(Eye,N2);
       if d2<0 then d2:=-d2;
       Eye:=Normalize(Subtract(Camera,P3));
       d3:=Dotproduct(Eye,N3);
       if d3<0 then d3:=-d3;

       MindP:=d1;
       MaxDp:=d1;
       if d2<Mindp then Mindp:=d2;
       if d2>Maxdp then Maxdp:=d2;
       if d3<Mindp then Mindp:=d3;
       if d3>Maxdp then Maxdp:=d3;

       Lo:=Trunc(MinDp/Width);
       if Lo<0 then Lo:=0;
       Hi:=trunc(MaxDp/Width);
       if Hi>Round(1/Width) then Hi:=Round(1/Width);
       if Lo=Hi then
       begin
         if not odd(Lo) then Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha)
                        else Viewport.ShadeTriangle(P1,P2,P3,Zr,Zg,Zb,Alpha);
       end else
       begin
          Add(P1,d1);
          Process(P2,d2);
          Process(P3,d3);
          Process(P1,d1);
          Setlength(Pts,NInt);
          for I:=Lo to Hi+1 do
          begin
            NPts:=0;
            for J:=1 to NInt-1 do
            begin
               if ((Intersections[J-1].DotProd>=(I-1)*Width) or (abs(Intersections[J-1].DotProd-(I-1)*Width)<1e-6)) and
                  ((Intersections[J-1].DotProd<=I*Width) or (abs(Intersections[J-1].DotProd-I*Width)<1e-6) )then
               begin
                  inc(Npts);
                  Pts[NPts-1]:=Intersections[J-1].P;
               end;
            end;
            for J:=3 to NPts do
            begin
               if odd(I) then Viewport.ShadeTriangle(Pts[0],Pts[J-2],Pts[J-1],R,G,B,Alpha)
                         else Viewport.ShadeTriangle(Pts[0],Pts[J-2],Pts[J-1],Zr,Zg,Zb,Alpha);
            end;
         end;
      end;
      if (Owner.DrawMirror) and (Layer.Symmetric) then
      begin
         P1.Y:=-P1.Y;
         P2.Y:=-P2.Y;
         P3.Y:=-P3.Y;
         N1.Y:=-N1.Y;
         N2.Y:=-N2.Y;
         N3.Y:=-N3.Y;
         NInt:=0;

         Eye:=Normalize(Subtract(Camera,P1));
         d1:=Dotproduct(Eye,N1);
         if d1<0 then d1:=-d1;
         Eye:=Normalize(Subtract(Camera,P2));
         d2:=Dotproduct(Eye,N2);
         if d2<0 then d2:=-d2;
         Eye:=Normalize(Subtract(Camera,P3));
         d3:=Dotproduct(Eye,N3);
         if d3<0 then d3:=-d3;

         MindP:=d1;
         MaxDp:=d1;
         if d2<Mindp then Mindp:=d2;
         if d2>Maxdp then Maxdp:=d2;
         if d3<Mindp then Mindp:=d3;
         if d3>Maxdp then Maxdp:=d3;

         Lo:=Trunc(MinDp/Width);
         if Lo<0 then Lo:=0;
         Hi:=trunc(MaxDp/Width);
         if Hi>Round(1/Width) then Hi:=Round(1/Width);
         if Lo=Hi then
         begin
            if not odd(Lo) then Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha)
                           else Viewport.ShadeTriangle(P1,P2,P3,Zr,Zg,Zb,Alpha);
         end else
         begin
            Add(P1,d1);
            Process(P2,d2);
            Process(P3,d3);
            Process(P1,d1);
            Setlength(Pts,NInt);
            for I:=Lo to Hi+1 do
            begin
               NPts:=0;
               for J:=1 to NInt-1 do
               begin
                  if ((Intersections[J-1].DotProd>=(I-1)*Width) or (abs(Intersections[J-1].DotProd-(I-1)*Width)<1e-6)) and
                     ((Intersections[J-1].DotProd<=I*Width) or (abs(Intersections[J-1].DotProd-I*Width)<1e-6) )then
                  begin
                     inc(Npts);
                     Pts[NPts-1]:=Intersections[J-1].P;
                  end;
               end;
               for J:=3 to NPts do
               begin
                  if odd(I) then Viewport.ShadeTriangle(Pts[0],Pts[J-2],Pts[J-1],R,G,B,Alpha)
                            else Viewport.ShadeTriangle(Pts[0],Pts[J-2],Pts[J-1],Zr,Zg,Zb,Alpha);
               end;
            end;
         end;
      end;
    end;{ZebraStripe}

begin
   if Viewport.ViewportMode<>vmWireframe then
   begin
      R:=GetRValue(Layer.Color);
      G:=GetGValue(Layer.Color);
      B:=GetBValue(Layer.Color);
      Alpha:=Layer.AlphaBlend;
      Viewport.BeginUpdate;
      if (Owner.ShadeUnderWater) and (Viewport.ViewportMode=vmShade) and (Layer.UseInHydrostatics)then
      begin
         // Clip all triangles against the waterline plane
         Ru:=GetRValue(Owner.UnderWaterColor);
         Gu:=GetGValue(Owner.UnderWaterColor);
         Bu:=GetBValue(Owner.UnderWaterColor);

         Setlength(Above,6);
         setlength(Below,6);
         for I:=1 to ChildCount do
         begin
            Child:=Self.Child[I-1];
            for J:=2 to Child.NumberOfpoints-1 do
            begin
               P1:=Child.Point[0].Coordinate;
               P2:=Child.Point[J-1].Coordinate;
               P3:=Child.Point[J].Coordinate;

               // Check if clipping is required
               Min:=Owner.WaterlinePlane.a*P1.x+Owner.WaterlinePlane.b*P1.y+Owner.WaterlinePlane.c*P1.z+Owner.WaterlinePlane.d;
               Max:=Min;
               Tmp:=Owner.WaterlinePlane.a*P2.x+Owner.WaterlinePlane.b*P2.y+Owner.WaterlinePlane.c*P2.z+Owner.WaterlinePlane.d;
               if Tmp<Min then Min:=Tmp else if Tmp>Max then Max:=Tmp;
               Tmp:=Owner.WaterlinePlane.a*P3.x+Owner.WaterlinePlane.b*P3.y+Owner.WaterlinePlane.c*P3.z+Owner.WaterlinePlane.d;
               if Tmp<Min then Min:=Tmp else if Tmp>Max then Max:=Tmp;
               if Max<=0.0 then
               begin
                  // entirely below the plane
                  Viewport.ShadeTriangle(P1,P2,P3,Ru,Gu,Bu,Alpha);
               end else if Min>=0.0 then
               begin
                  // entirely above the plane
                  Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha);
               end else
               begin
                  // pierces water, clip triangle
                  ClipTriangle(P1,P2,P3,Owner.WaterlinePlane,Na,Nb,Above,Below);
                  for K:=3 to Na do Viewport.ShadeTriangle(Above[0],Above[K-2],Above[K-1],R,G,B,Alpha);
                  for K:=3 to Nb do Viewport.ShadeTriangle(Below[0],Below[K-2],Below[K-1],Ru,Gu,Bu,Alpha);
               end;
               if (Owner.DrawMirror) and (Layer.Symmetric) then
               begin
                  P1.Y:=-P1.Y;
                  P2.Y:=-P2.Y;
                  P3.Y:=-P3.Y;
                  // Check if clipping is required
                  Min:=Owner.WaterlinePlane.a*P1.x+Owner.WaterlinePlane.b*P1.y+Owner.WaterlinePlane.c*P1.z+Owner.WaterlinePlane.d;
                  Max:=Min;
                  Tmp:=Owner.WaterlinePlane.a*P2.x+Owner.WaterlinePlane.b*P2.y+Owner.WaterlinePlane.c*P2.z+Owner.WaterlinePlane.d;
                  if Tmp<Min then Min:=Tmp else if Tmp>Max then Max:=Tmp;
                  Tmp:=Owner.WaterlinePlane.a*P3.x+Owner.WaterlinePlane.b*P3.y+Owner.WaterlinePlane.c*P3.z+Owner.WaterlinePlane.d;
                  if Tmp<Min then Min:=Tmp else if Tmp>Max then Max:=Tmp;
                  if Max<=0.0 then
                  begin
                     // entirely below the plane
                     Viewport.ShadeTriangle(P1,P2,P3,Ru,Gu,Bu,Alpha);
                  end else if Min>=0.0 then
                  begin
                     // entirely above the plane
                     Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha);
                  end else
                  begin
                     // pierces water, clip triangle
                     ClipTriangle(P1,P2,P3,Owner.WaterlinePlane,Na,Nb,Above,Below);
                     for K:=3 to Na do Viewport.ShadeTriangle(Above[0],Above[K-2],Above[K-1],R,G,B,Alpha);
                     for K:=3 to Nb do Viewport.ShadeTriangle(Below[0],Below[K-2],Below[K-1],Ru,Gu,Bu,Alpha);
                  end;
               end;
            end;
         end;
      end else if Viewport.ViewportMode=vmShadeZebra then
      begin
         Ru:=GetRValue(Owner.ZebraColor);
         Gu:=GetGValue(Owner.ZebraColor);
         Bu:=GetBValue(Owner.ZebraColor);
         Camera:=Viewport.RotatedPointBack(Viewport.FCameraLocation);
         Capacity:=10;
         Setlength(Intersections,Capacity);
         for I:=1 to ChildCount do
         begin
            Child:=Self.Child[I-1];
            for J:=2 to Child.NumberOfpoints-1
              do ZebraStripe(Camera,Child.Point[0],Child.Point[J-1],Child.Point[J],R,G,B,Ru,Gu,Bu);
         end;
         if capacity<0 then exit;
      end else
      begin
         for I:=1 to ChildCount do
         begin
            Child:=Self.Child[I-1];
            for J:=2 to Child.NumberOfpoints-1 do
            begin
               P1:=Child.Point[0].Coordinate;
               P2:=Child.Point[J-1].Coordinate;
               P3:=Child.Point[J].Coordinate;
               if Viewport.ViewportMode<>vmShade then
               begin
                  Index:=Owner.FPoints.SortedIndexOf(Child.Point[0]);
                  C1:=Owner.FGausCurvature[index];
                  Index:=Owner.FPoints.SortedIndexOf(Child.Point[J-1]);
                  C2:=Owner.FGausCurvature[index];
                  Index:=Owner.FPoints.SortedIndexOf(Child.Point[J]);
                  C3:=Owner.FGausCurvature[index];
                  Viewport.ShadeTriangle(P1,P2,P3,C1,C2,C3);
                  if (Owner.DrawMirror) and (Layer.Symmetric) then
                  begin
                     P1.Y:=-P1.Y;
                     P2.Y:=-P2.Y;
                     P3.Y:=-P3.Y;
                     Viewport.ShadeTriangle(P1,P2,P3,C1,C2,C3);
                  end;
               end else
               begin
                  Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha);
                  if (Owner.DrawMirror) and (Layer.Symmetric) then
                  begin
                     P1.Y:=-P1.Y;
                     P2.Y:=-P2.Y;
                     P3.Y:=-P3.Y;
                     Viewport.ShadeTriangle(P1,P2,P3,R,G,B,Alpha);
                  end;
               end;
            end;
         end;
      end;
      Viewport.EndUpdate;
   end else
   begin
      // Draw interior edges (not descending from controledges)
      Viewport.PenStyle:=psSolid;
      Viewport.PenWidth:=1;
      if Selected then Viewport.PenColor:=Owner.Selectedcolor
                  else Viewport.PenColor:=Layer.Color;
      for I:=1 to FEdges.Count do
      begin
         Edge:=FEdges[I-1];
         Edge.Draw(Owner.DrawMirror and Layer.Symmetric,Viewport);
      end;
      // Draw edges descending from controledges, but slighly darker then interior edges
      if not Selected then
      begin
         R:=round(0.6*GetRValue(Layer.Color));
         G:=round(0.6*GetGValue(Layer.Color));
         B:=round(0.6*GetBValue(Layer.Color));
         Viewport.PenColor:=RGB(R,G,B);
      end;
      for I:=1 to FControlEdges.Count do
      begin
         Edge:=FControlEdges[I-1];
         Edge.Draw(Owner.DrawMirror and Layer.Symmetric,Viewport);
      end;
      if (Selected) and (Owner.ShowNormals) then
      begin
         // Draw nornals
         // First assemble all points within this controlface
         Points:=TFasterList.Create;
         for I:=1 to ChildCount do
         begin
            Child:=Self.Child[I-1];
            for J:=1 to Child.NumberOfpoints-1 do
            begin
               Point:=Child.Point[J-1];
               if Points.SortedIndexOf(Point)=-1 then Points.AddSorted(Point);
            end;
         end;
         for I:=1 to Points.Count do
         begin
            Point:=Points[I-1];
            P1:=Point.Coordinate;
            P2:=Point.Normal;
            DrawNormal(P1,P2,Owner.NormalColor);
            if (Layer.Symmetric) and (Owner.DrawMirror) then
            begin
               P1.Y:=-P1.Y;
               P2.Y:=-P2.Y;
               DrawNormal(P1,P2,Owner.NormalColor);
            end;
         end;
         Points.Destroy;
      end;
   end;
end;{TFreeSubdivisionControlFace.Draw}

procedure TFreeSubdivisionControlFace.Draw(Viewport:TFreeViewport;MinCurvature,MaxCurvature:TFloatType);
var I,J     : Integer;
    Index   : Integer;
    Child   : TFreeSubdivisionFace;
    P1,P2,P3: T3DCoordinate;
    Curv    : TFloatType;
    R1,G1,B1: Byte;
    R2,G2,B2: Byte;
    R3,G3,B3: Byte;
    ChidCnt, ChildPointsCnt: Integer;

    function Fragment(Curvature:TFloatType):TFloatType;
    const contrast=0.1;
    begin
       if (Curvature>-1e-4) and (Curvature<1e-4) then result:=0.5 else
       begin
          if Curvature>0 then Result:=0.5+0.5*Power(Curvature/MaxCurvature,contrast)
                         else Result:=0.5-0.5*Power(Curvature/MinCurvature,contrast);
          Result:=1-result;
          {
          if (Result<0.2) then Result:=0.10 else
             if (Result>=0.2) and (Result<0.4) then Result:=0.3 else
                if (Result>=0.4) and (Result<0.6) then Result:=0.50 else
                   if (Result>=0.6) and (Result<0.8) then Result:=0.8 else
                      if (Result>=0.8) then Result:=1.0;
          }
       end;
    end;

begin
   Viewport.BeginUpdate;
   ChidCnt:=ChildCount;
   for I:=1 to ChidCnt do
   begin
      Child:=Self.Child[I-1];
      ChildPointsCnt:=Child.NumberOfpoints;
      for J:=2 to ChildPointsCnt-1 do
      begin
         P1:=Child.Point[0].Coordinate;
         Index:=Owner.FPoints.SortedIndexOf(Child.Point[0]);
         Curv:=Owner.FGausCurvature[index];
         FillColor(Fragment(Curv),R1,G1,B1);
         P2:=Child.Point[J-1].Coordinate;
         Index:=Owner.FPoints.SortedIndexOf(Child.Point[J-1]);
         Curv:=Owner.FGausCurvature[index];
         FillColor(Fragment(Curv),R2,G2,B2);
         P3:=Child.Point[J].Coordinate;
         Index:=Owner.FPoints.SortedIndexOf(Child.Point[J]);
         Curv:=Owner.FGausCurvature[index];
         FillColor(Fragment(Curv),R3,G3,B3);
         Viewport.ShadeTriangle(P1,P2,P3,R1,G1,B1,R2,G2,B2,R3,G3,B3);
         if (Owner.DrawMirror) and (Layer.Symmetric) then
         begin
            P1.Y:=-P1.Y;
            P2.Y:=-P2.Y;
            P3.Y:=-P3.Y;
            Viewport.ShadeTriangle(P1,P2,P3,R1,G1,B1,R2,G2,B2,R3,G3,B3);
         end;
      end;
   end;
   Viewport.EndUpdate;
end;{TFreeSubdivisionControlFace.Draw}

function TFreeSubdivisionControlFace.InsertEdge(P1,P2:TFreeSubdivisionControlPoint):TFreesubdivisionControlEdge;
var Tmp,I   : Integer;
    Pts     : TFasterList;
begin
   Result:=nil;
   try
      if (P1.FFaces.IndexOf(self)<>-1) and (P2.FFaces.IndexOf(self)<>-1) then
      begin
         if Owner.EdgeExists(P1,P2)<>nil then exit;
         Tmp:=IndexOfPoint(P1);
         Pts:=TFasterList.Create;
         Pts.Add(P1);
         for I:=1 to NumberOfpoints do
         begin
            Tmp:=(Tmp+1) mod NumberOfpoints;
            Pts.Add(Point[Tmp]);
            if Pts[Pts.Count-1]=P2 then break;
         end;
         if Pts.Count>2 then
         begin
            Owner.AddControlFace(Pts,False,Layer);
         end;
         Tmp:=IndexOfPoint(P2);
         Pts.Clear;
         Pts.Add(P2);
         for I:=1 to NumberOfpoints do
         begin
            Tmp:=(Tmp+1) mod NumberOfpoints;
            Pts.Add(Point[Tmp]);
            if Pts[Pts.Count-1]=P1 then break;
         end;
         if Pts.Count>2 then
         begin
            Owner.AddControlFace(Pts,False,Layer);
         end;
         Pts.Destroy;
         Delete;  ///MM ??? Why it suicide itself? It does not makes sence. I comment it. No does not work without it.
      end;
      Result:=Fowner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
      if Result<>nil then Result.Crease:=False;
   except
     Result:=Fowner.EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
   end;
end;{TFreeSubdivisionControlFace.InsertEdge}

procedure TFreeSubdivisionControlFace.LoadBinary(Source:TFreeFileBuffer);
var I,N     : Integer;
    Index   : Integer;
    P1,P2   : TFreeSubdivisionControlPoint;
    Edge    : TFreeSubdivisionControlEdge;
    Sel     : Boolean;
begin
   // Read controlpoint data
   Source.Load(N);
   FPoints.Clear;
   FPoints.Capacity:=N;
   for I:=1 to N do
   begin
      Source.Load(Index);
      if Index=-1 then Index:=0;
      if Index<>-1 then
      begin
         P1:=Owner.FControlPoints[Index];
         FPoints.Add(P1);
         P1.FFaces.Add(Self);
      end;
   end;
   // Read layer-index
   Source.Load(Index);
   if (Index>=0) and (Index<Owner.FLayers.Count) then
   begin
      FLayer:=Owner.Layer[Index];
   end else
   begin
      FLayer:=Owner.Layer[0]; // Reference to an invalid layer. Assign to owners default layer
   end;
   if FLayer<>nil then FLayer.AddControlFace(self)
                  else Raise Exception.Create('Invalid layer reference in procedure TFreeSubdivisionControlFace.LoadBinary!');

   Source.Load(Sel);
   if sel then Selected:=True;
   P1:=FPoints[NumberOfPoints-1];
   for I:=1 to NumberOfPoints do
   begin
      P2:=FPoints[I-1];
      Edge:=Owner.EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
      if Edge<>nil then Edge.FFaces.Add(Self) else
      begin
         Edge:=Owner.AddControlEdge(P1,P2);
         Edge.Crease:=True;
         MessageDlg('Could not find edge!',mtError,[mbOk],0);
      end;
      P1:=P2;
   end;
end;{TFreeSubdivisionControlFace.LoadBinary}

procedure TFreeSubdivisionControlFace.LoadFromStream(var LineNr:Integer;Strings:TStringList);
var Str     : string;
    I,N     : Integer;
    Index   : Integer;
    P1,P2   : TFreeSubdivisionControlPoint;
    Edge    : TFreeSubdivisionControlEdge;
    Sel     : Boolean;
begin
   Inc(LineNr);
   Str:=Strings[LineNr];
   // Read controlpoint data
   N:=ReadIntFromStr(LineNr,Str);
   FPoints.Clear;
   FPoints.Capacity:=N;
   for I:=1 to N do
   begin
      Index:=ReadIntFromStr(LineNr,Str);
      if Index=-1 then Index:=0;
      if Index<>-1 then
      begin
         P1:=Owner.FControlPoints[Index];
         FPoints.Add(P1);
         P1.FFaces.Add(Self);
      end;
   end;
   // Read layer-index
   Index:=ReadIntFromStr(LineNr,Str);
   if (Index>=0) and (Index<Owner.FLayers.Count) then
   begin
      FLayer:=Owner.Layer[Index];
   end else
   begin
      FLayer:=Owner.Layer[0]; // Reference to an invalid layer. Assign to owners default layer
   end;
   if FLayer<>nil then FLayer.AddControlFace(self)
                  else Raise Exception.Create('Invalid layer reference in procedure TFreeSubdivisionControlFace.LoadFromStream!');

   if Str<>'' then
   begin
      Sel:=ReadBoolFromStr(LineNr,Str);
      if sel then Selected:=True;
   end;

   P1:=FPoints[NumberOfPoints-1];
   for I:=1 to NumberOfPoints do
   begin
      P2:=FPoints[I-1];
      Edge:=Owner.EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
      if Edge<>nil then Edge.FFaces.Add(Self)
                   else MessageDlg(Userstring(201)+'!',mtError,[mbOk],0);
      P1:=P2;
   end;
end;{TFreeSubdivisionControlFace.LoadFromStream}

procedure TFreeSubdivisionControlFace.RemoveReferences;
var P1,P2   : TFreeSubdivisionPoint;
    I       : Integer;
    Edge    : TFreeSubdivisionEdge;
begin
   P1:=FPoints[FPoints.Count-1];
   for I:=1 to FPoints.Count do
   begin
      P2:=FPoints[I-1];
      P2.DeleteFace(self);
      Edge:=FOwner.EdgeExists(P1,P2);
      if Edge<>nil then
      begin
         Edge.DeleteFace(self);
      end;
      P1:=p2;
   end;
end;{TFreeSubdivisionControlFace.RemoveReferences}

procedure TFreeSubdivisionControlFace.SaveBinary(Destination:TFreeFileBuffer);
var I,Index : Integer;
begin
   Destination.Add(NumberOfPoints);
   for I:=1 to NumberOfPoints do Destination.Add(Owner.FControlPoints.SortedIndexOf(Point[I-1]));
   // Add layer index
   if Layer<>nil then Index:=Owner.FLayers.IndexOf(Layer)
                 else Index:=-1;
   Destination.Add(Index);
   Destination.Add(Selected);
end;{TFreeSubdivisionControlFace.SaveBinary}

procedure TFreeSubdivisionControlFace.SaveToDXF(Strings:TStringList);
var I,J,K      : Integer;
    Cols,Rows  : Integer;
    Child      : TFreeSubdivisionFace;
    LayerName  : string;
    ColorIndex : Integer;
    P          : T3DCoordinate;
    Grid       : TFreeSubdivisionGrid;
    FaceData   : TFreeFaceGrid;
begin
   LayerName:=self.Layer.Name;
   ColorIndex:=FindDXFColorIndex(Layer.Color);
   if NumberOfpoints=4 then
   begin
      // create one polymesh for all childfaces
      FaceData.NCols:=1;
      FaceData.NRows:=1;
      Setlength(FaceData.Faces,FaceData.NRows);
      Setlength(FaceData.Faces[0],FaceData.NCols);
      FaceData.Faces[0][0]:=Self;
      Owner.ConvertToGrid(FaceData,Cols,Rows,Grid);
      if (Rows>0) and (Cols>0) then
      begin
         Strings.Add('0'+EOL+'POLYLINE');
         Strings.Add('8'+EOL+LayerName);
         Strings.Add('62'+EOL+IntToStr(ColorIndex));
         Strings.Add('66'+EOL+'1');
         Strings.Add('70'+EOL+'16');
         Strings.Add('71'+EOL+IntToStr(Cols));
         Strings.Add('72'+EOL+IntToStr(Rows));
         for I:=1 to Rows do
         begin
            for J:=1 to Cols do
            begin
               P:=Grid[I-1][J-1].FCoordinate;
               Strings.Add('0'+EOL+'VERTEX');
               Strings.Add('8'+EOL+LayerName);
               Strings.Add('10'+EOL+Truncate(P.X,4));
               Strings.Add('20'+EOL+Truncate(P.Y,4));
               Strings.Add('30'+EOL+Truncate(P.Z,4));
               Strings.Add('70'+EOL+'64');    // polygon mesh vertex
            end;
         end;
         Strings.add('0'+EOL+'SEQEND');
         if (Layer.Symmetric) and (Owner.DrawMirror) then
         begin
            Strings.Add('0'+EOL+'POLYLINE');
            Strings.Add('8'+EOL+LayerName);
            Strings.Add('62'+EOL+IntToStr(ColorIndex));
            Strings.Add('66'+EOL+'1');
            Strings.Add('70'+EOL+'16');
            Strings.Add('71'+EOL+IntToStr(Cols));
            Strings.Add('72'+EOL+IntToStr(Rows));
            for I:=1 to Rows do
            begin
               for J:=1 to Cols do
               begin
                  P:=Grid[I-1][J-1].FCoordinate;
                  Strings.Add('0'+EOL+'VERTEX');
                  Strings.Add('8'+EOL+LayerName);
                  Strings.Add('10'+EOL+Truncate(P.X,4));
                  Strings.Add('20'+EOL+Truncate(-P.Y,4));
                  Strings.Add('30'+EOL+Truncate(P.Z,4));
                  Strings.Add('70'+EOL+'64');    // polygon mesh vertex
               end;
            end;
            Strings.add('0'+EOL+'SEQEND');
         end;
      end;
   end else
   begin
      // send all child faces as 3D faces
      for J:=1 to ChildCount do
      begin
         Child:=self.Child[J-1];
         Strings.Add('0'+EOL+'3DFACE');
         Strings.Add('8'+EOL+LayerName);
         Strings.Add('62'+EOL+IntToStr(ColorIndex));
         for K:=1 to Child.NumberOfpoints do
         begin
            P:=Child.Point[K-1].Coordinate;
            Strings.Add(IntToStr(10+K-1)+EOL+Truncate(P.X,4));
            Strings.Add(IntToStr(20+K-1)+EOL+Truncate(P.Y,4));
            Strings.Add(IntToStr(30+K-1)+EOL+Truncate(P.Z,4));
         end;
         if Child.NumberOfpoints=3 then
         begin
            // 4th point is same as third
            Strings.Add(IntToStr(13)+EOL+Truncate(P.X,4));
            Strings.Add(IntToStr(23)+EOL+Truncate(P.Y,4));
            Strings.Add(IntToStr(33)+EOL+Truncate(P.Z,4));
         end;
         if (Layer.Symmetric) and (Owner.DrawMirror) then
         begin
            // send starboard side also
            Strings.Add('0'+EOL+'3DFACE');
            Strings.Add('8'+EOL+LayerName);
            Strings.Add('62'+EOL+IntToStr(ColorIndex));
            for K:=Child.NumberOfpoints downto 1 do
            begin
               P:=Child.Point[K-1].Coordinate;
               P.Y:=-P.Y;
               Strings.Add(IntToStr(10+K-1)+EOL+Truncate(P.X,4));
               Strings.Add(IntToStr(20+K-1)+EOL+Truncate(P.Y,4));
               Strings.Add(IntToStr(30+K-1)+EOL+Truncate(P.Z,4));
            end;
            if Child.NumberOfpoints=3 then
            begin
               // 4th point is same as third
               Strings.Add(IntToStr(13)+EOL+Truncate(P.X,4));
               Strings.Add(IntToStr(23)+EOL+Truncate(P.Y,4));
               Strings.Add(IntToStr(33)+EOL+Truncate(P.Z,4));
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionControlFace.SaveToDXF}

procedure TFreeSubdivisionControlFace.SaveToStream(Strings:TStringlist);
var Str     : string;
    I,Index : Integer;
begin
   Str:=IntToStr(NumberOfPoints);
   for I:=1 to NumberOfPoints do Str:=Str+#32+IntToStr(Owner.FControlPoints.SortedIndexOf(Point[I-1]));
   // Add layer index
   if Layer<>nil then Index:=Owner.FLayers.IndexOf(Layer)
                 else Index:=-1;
   Str:=Str+#32+IntToStr(Index)+#32+BoolToStr(Selected);
   strings.Add(Str);
end;{TFreeSubdivisionControlFace.SaveToStream}

procedure TFreeSubdivisionControlFace.Subdivide(Owner:TFreeSubdivisionSurface;ControlFace:Boolean;VertexPoints,EdgePoints,FacePoints,InteriorEdges,ControlEdges,Dest:TFasterList);
var TmpList : TFasterlist;
    Tmp     : TFasterList;
    I       : Integer;
    Face    : TFreeSubdivisionFace;
begin
   FControlEdges.Clear;
   if FChildren.Count=0 then
   begin
      FChildren.Capacity:=NumberOfPoints;
      FEdges.Capacity:=4;
      Inherited Subdivide(Owner,True,VertexPoints,EdgePoints,FacePoints,FEdges,FControlEdges,FChildren);
   end else
   begin
      TmpList:=TFasterlist.Create;
      TmpList.Capacity:=4*ChildCount;
      Tmp:=TFasterList.Create;
      I:=Round(Power(2,Owner.FCurrentSubdivisionLevel));
      Tmp.Capacity:=2*(I*(I-1));
      for I:=1 to FChildren.Count do
      begin
         Face:=FChildren[I-1];
         Face.Subdivide(Owner,False,VertexPoints,EdgePoints,FacePoints,Tmp,FControlEdges,TmpList);
      end;
      ClearChildren;
      FEdges.Destroy;
      FEdges:=Tmp;
      FChildren.Destroy;
      FChildren:=TmpList;
   end;
   FControlEdges.Capacity:=FControlEdges.Count;
   ControlEdges.Capacity:=ControlEdges.Count+FControlEdges.Count;
   for I:=1 to FControlEdges.Count do
   begin
      if ControlEdges.SortedIndexOf(FControlEdges[I-1])=-1
       then ControlEdges.AddSorted(FControlEdges[I-1]);
   end;
   CalcExtents;
end;{TFreeSubdivisionControlFace.Subdivide}

// select all controlfaces connected to the current one that belong to the same layer and are not separated by a crease edge
procedure TFreeSubdivisionControlFace.Trace;
var ToDoList : TFasterList;
    I        : Integer;
    Face     : TFreeSubdivisionControlface;
    Prev     : TCursor;

    procedure FindAttachedFaces(List:TFasterList;Face:TFreeSubdivisionControlFace);
    var I,J    : Integer;
        Index  : Integer;
        P1,P2  : TFreeSubdivisionPoint;
        Edge   : TFreeSubdivisionEdge;
    begin
       P1:=Face.Point[Face.NumberOfPoints-1];
       for I:=1 to Face.NumberOfpoints do
       begin
          P2:=Face.Point[I-1];
          Edge:=Face.Owner.EdgeExists(P1,P2);
          if Edge<>nil then if not Edge.Crease then
          begin
             for J:=1 to Edge.NumberOfFaces do if Edge.Face[J-1]<>Face then
             begin
                Index:=ToDoList.IndexOf(Edge.Face[J-1]);
                if Index<>-1 then
                begin
                   TFreeSubdivisionControlface(Edge.Face[J-1]).Selected:=selected;
                   //List.Add(Edge.Face[J-1]);
                   ToDoList.Delete(Index);
                   FindAttachedFaces(List,Edge.Face[J-1] as TFreeSubdivisionControlFace);
                end;
             end;
          end;
          P1:=p2;
       end;
    end;{FindAttachedFaces}

begin
   Prev:=screen.Cursor;
   Screen.Cursor:=crHourglass;
   try
      ToDoList:=TFasterList.Create;
      ToDoList.capacity:=Layer.Count;
      for I:=1 to Layer.Count do
      begin
         Face:=Layer.Items[I-1];
         if (Face<>self) and (Face.Selected<>self.Selected) then ToDoList.Add(Layer.Items[I-1]);
      end;
      ToDoList.Sort;
      FindAttachedFaces(ToDoList,self);
      ToDoList.Destroy;
   finally
      Screen.Cursor:=prev;
   end;
end;{TFreeSubdivisionControlFace.Trace}
{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionSurface                                  }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionSurface.AddControlPoint(P:T3DCoordinate):TFreeSubdivisionControlPoint;
var I             : Integer;
    MaxError      : Double;
    Edge          : TFreeSubdivisionEdge;
    Point         : TFreeSubdivisionControlPoint;

   function NewPoint(P:T3DCoordinate):TFreeSubdivisionControlPoint;
   begin
      Result:=TFreeSubdivisionControlPoint.Create(self);
      Result.FCoordinate:=P;
      FControlPoints.Add(Result);
   end;{NewPoint}

begin
   result:=nil;
   MaxError:=1e-5;

   for I:=1 to NumberOfControlEdges do
   begin
      Edge:=FControlEdges[I-1];
      if Edge.FFaces.Count<=1 then // boundary edge
      begin
         if SquaredDistPP(P,Edge.FStartpoint.FCoordinate)<=MaxError then
         begin
            Result:=Edge.FStartpoint as TFreeSubdivisionControlPoint;
            break;
         end else if SquaredDistPP(P,Edge.FEndpoint.FCoordinate)<=MaxError then
         begin
            Result:=Edge.FEndpoint as TFreeSubdivisionControlPoint;
            break;
         end;
      end;
   end;
   if Result=nil then
   begin
      // Search controlpoints without edges
      for I:=1 to FControlPoints.Count do
      begin
         Point:=ControlPoint[I-1];
         if Point.NumberOfEdges=0 then
         begin
            if SquaredDistPP(P,Point.FCoordinate)<=MaxError then
            begin
               Result:=Point;
               break;
            end;
         end;
      end;
   end;
   if Result=nil then Result:=NewPoint(P);
end;{TFreeSubdivisionSurface.AddControlPoint}

procedure TFreeSubdivisionSurface.AddControlPoint(P:TFreeSubdivisionControlPoint);
begin
   if FControlPoints.IndexOf(P)=-1 then
   begin
      FControlPoints.Add(P);
      P.FOwner:=self;
   end;
   Build:=False;
end;{TFreeSubdivisionSurface.AddControlPoint}

// Adds a new controlpoint at 0,0,0 without checking other points
function TFreeSubdivisionSurface.AddControlPoint:TFreeSubdivisionControlPoint;
begin
   Result:=TFreeSubdivisionControlPoint.Create(self);
   Result.FCoordinate:=ZERO;
   FControlPoints.Add(Result);
end;{TFreeSubdivisionSurface.AddControlPoint}

function TFreeSubdivisionSurface.AddNewLayer:TFreeSubdivisionLayer;
begin
   Result:=TFreeSubdivisionLayer.Create(Self);
   FLayers.Add(Result);
   Result.FLayerID:=FRequestNewLayerID;
   ActiveLayer:=Result;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
end;{TFreeSubdivisionSurface.AddNewLayer}

// Tries to assemble quads into as few as possible rectangular patches
procedure TFreeSubdivisionSurface.AssembleFacesToPatches(Layers:TFasterList;Mode:TFreeAssembleMode;var AssembledPatches:TFreeFaceArray;var NAssembled:Integer);
var ToDoList   : TFasterlist;
    DoneList   : TFasterlist;
    Current    : TFasterlist;
    I          : integer;
    Capacity   : Integer;
    Face       : TFreeSubdivisionControlFace;
    Layer      : TFreeSubdivisionLayer;
    ErrInd     : Integer;

    function GetFace(P1,P2,P3,P4:TFreeSubdivisionPoint):TFreeSubdivisionControlFace;
    var I    : Integer;
        Face : TFreeSubdivisionFace;
    begin
       Result:=nil;
       for I:=1 to P1.NumberOfFaces do
       begin
          Face:=P1.Face[I-1];
          if (P2.IndexOfFace(Face)<>-1) and (P3.IndexOfFace(Face)<>-1) and (P4.IndexOfFace(Face)<>-1) then
          begin
             Result:=Face as TFreeSubdivisionControlFace;
             exit;
          end;
       end;
    end;{GetFace}

    procedure FindAttachedFaces(List:TFasterlist;Face:TFreeSubdivisionControlFace);
    var I,J    : integer;
        Index  : integer;
        P1,P2  : TFreeSubdivisionPoint;
        Edge   : TFreeSubdivisionEdge;
    begin
       P1:=Face.Point[Face.NumberOfPoints-1];
       for I:=1 to Face.NumberOfpoints do
       begin
          P2:=Face.Point[I-1];
          Edge:=Face.Owner.EdgeExists(P1,P2);
          if Edge<>nil then
          begin
             if not Edge.Crease then
             begin
                for J:=1 to Edge.NumberOfFaces do if Edge.Face[J-1]<>Face then
                begin
                   Index:=ToDoList.IndexOf(Edge.Face[J-1]);
                   if Index<>-1 then
                   begin
                      List.Add(Edge.Face[J-1]);
                      ToDoList.Delete(Index);
                      FindAttachedFaces(List,Edge.Face[J-1] as TFreeSubdivisionControlFace);
                   end;
                end;
             end;
          end;
          P1:=p2;
       end;
    end;{FindAttachedFaces}

    procedure AssembleFaces(CtrlFaces:TFasterList;var Assembled:TFreeFaceArray;var NAssembled:Integer);
    Type TPointRow   = record
                          NPoints:Integer;
                          Points:array of TFreeSubdivisionPoint;
                       end;
         TPointGrid  = record
                           NRows       : Integer;
                           RowCapacity : Integer;
                           ColCapacity : Integer;
                           Rows        : array of TPointRow;
                        end;

    var I,J       : Integer;
        Index     : Integer;
        NCols     : Integer;
        NRows     : Integer;
        Face      : TFreeSubdivisionControlFace;
        Grid      : TFreeSubdivisionGrid;
        NewFace   : TFreeFaceGrid;
        CheckFaces: TFasterList; // temporary list with all controlfaces that is used to check if a crease vertex is
                                 // regular (depends on which side of the crease edge is being processed)

        function FindCornerFace:TFreesubdivisionControlFace;
        var I,J,K : Integer;
            NFaces: Integer;
            Face  : TFreeSubdivisionControlFace;
            Face2 : TFreeSubdivisionControlFace;
            P     : TFreeSubdivisionPoint;
        begin
           Result:=nil;
           I:=1;
           while (I<=CtrlFaces.Count) and (Result=nil) do
           begin
              Face:=Ctrlfaces[I-1];
              J:=1;
              while (J<=Face.NumberOfpoints) and (Result=nil) do
              begin
                 P:=Face.Point[J-1];
                 NFaces:=1;
                 for K:=1 to P.NumberOfFaces do
                 begin
                    Face2:=P.Face[K-1] as TFreeSubdivisionControlFace;
                    if Face2<>Face then if CtrlFaces.SortedIndexOf(Face2)<>-1 then
                    begin
                       inc(NFaces);
                       break;
                    end;
                 end;
                 if NFaces=1 then Result:=Face;
                 inc(J);
              end;
              inc(I);
           end;
           if Result=nil then Result:=Ctrlfaces[CtrlFaces.Count-1]
        end;{FindCornerFace}

        procedure AddFace(Face:TFreeFaceGrid);
        begin
           if NAssembled=Capacity then
           begin
              inc(Capacity,50);
              setlength(Assembled,Capacity);
           end;
           inc(NAssembled);
           Assembled[NAssembled-1]:=Face;
        end;{AddFace}

        procedure DoAssemble(var Grid:TFreeSubdivisionGrid;var Cols,Rows:Integer;Faces:TFasterList);
        var SearchBottom   : Boolean;
            SearchTop      : Boolean;
            SearchLeft     : Boolean;
            SearchRight    : Boolean;
            Counter,Index  : Integer;
            I,J            : Integer;
            NFaces         : Integer;
            Edge1,Edge2    : TFreeSubdivisionEdge;
            Face           : TFreeSubdivisionFace;
            TmpFaces       : array of TFreeSubdivisionFace;
            DataOK         : Boolean;

            function ValidFace(Face:TFreeSubdivisionface):Boolean;
            var I,J,N,Index : Integer;
                Tmp         : TFreeSubdivisionFace;
            begin
               Result:=False;
               if Face.NumberOfpoints=4 then
               begin
                  Index:=Faces.SortedIndexOf(Face);
                  if Index<>-1 then
                  begin
                     Result:=True;
                     for I:=1 to NFaces do if TmpFaces[I-1]=face then
                     begin
                        result:=false;
                        exit;
                     end;
                     if NFaces>0 then
                     begin
                        // must also be connected to previous face
                        Tmp:=TmpFaces[NFaces-1];
                        N:=0;
                        for J:=1 to Face.NumberOfpoints do
                        begin
                           if Tmp.IndexOfPoint(Face.Point[J-1])<>-1 then inc(N);
                        end;
                        Result:=N>1;
                     end;
                  end;
               end;
            end;{ValidFace}

        begin
           Counter:=0;
           SearchBottom:=True;
           SearchTop:=True;
           SearchRight:=True;
           SearchLeft:=True;

           if Mode=amNURBS then
           begin
              if not Grid[0][0].IsRegularNURBSPoint(CheckFaces) then
              begin
                 SearchLeft:=False;
                 SearchTop:=False;
              end;
              if not Grid[Rows-1][0].IsRegularNURBSPoint(CheckFaces) then
              begin
                 SearchLeft:=False;
                 SearchBottom:=False;
              end;
              if not Grid[Rows-1][Cols-1].IsRegularNURBSPoint(CheckFaces) then
              begin
                 SearchRight:=False;
                 SearchBottom:=False;
              end;
              if not Grid[0][Cols-1].IsRegularNURBSPoint(CheckFaces) then
              begin
                 SearchRight:=False;
                 SearchTop:=False;
              end;
           end;

           while ((SearchBottom) or (SearchTop) or (SearchRight) or (SearchLeft)) and (Faces.Count>0) do
           begin
              inc(Counter);
              if Counter>4 then Counter:=1;
              if (Counter=1) and (SearchBottom) then
              begin
                 Setlength(TmpFaces,Cols);
                 NFaces:=0;
                 for I:=2 to Cols do
                 begin
                    Edge1:=EdgeExists(Grid[Rows-1][I-2],Grid[Rows-1][I-1]);
                    if Edge1<>nil then if not Edge1.Crease then for J:=1 to Edge1.NumberOfFaces do
                    begin
                       Face:=Edge1.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Cols-1 then
                 begin
                   // search was successfull
                   for I:=1 to NFaces do
                   begin
                      Setlength(Grid,Rows+1);
                      Setlength(Grid[Rows],Cols);
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);

                      Index:=Face.IndexOfPoint(Grid[Rows-1][I]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[Rows-1][I-1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[Rows][I-1]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[Rows][I]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[Rows-1][I-1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[Rows-1][I] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[Rows][I]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[Rows][I-1]:=Face.Point[index];
                         end;
                      end;
                   end;
                   // check if the boundary edges do not switch between crease or not crease
                   DataOK:=True;
                   if Mode=amNURBS then
                   begin
                      Edge1:=EdgeExists(Grid[Rows][0],Grid[Rows-1][0]);
                      Edge2:=EdgeExists(Grid[Rows-1][0],Grid[Rows-2][0]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      Edge1:=EdgeExists(Grid[Rows][Cols-1],Grid[Rows-1][Cols-1]);
                      Edge2:=EdgeExists(Grid[Rows-1][Cols-1],Grid[Rows-2][Cols-1]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      for I:=2 to Cols-1 do if not Grid[Rows][I-1].IsRegularNURBSPoint(CheckFaces) then
                         DataOK:=False;
                   end;
                   if not DataOK then
                   begin
                      // do not add the current row
                      for I:=1 to NFaces do Faces.AddSorted(TmpFaces[I-1]);
                      SearchBottom:=False;
                   end else inc(Rows);
                 end else SearchBottom:=False;
              end else if (Counter=2) and (SearchRight) then
              begin
                 Setlength(TmpFaces,Rows);
                 NFaces:=0;
                 for I:=2 to Rows do
                 begin
                    Edge1:=EdgeExists(Grid[I-1][Cols-1],Grid[I-2][Cols-1]);
                    if Edge1<>nil then if not Edge1.Crease then for J:=1 to Edge1.NumberOfFaces do
                    begin
                       Face:=Edge1.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Rows-1 then
                 begin
                   // search was successfull
                   for I:=1 to Rows do
                   begin
                      Setlength(grid[I-1],Cols+1);
                      Grid[I-1][Cols]:=nil;
                   end;
                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);
                      Index:=Face.IndexOfPoint(Grid[I-1][Cols-1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[I][Cols-1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I][Cols]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I-1][Cols]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[I][Cols-1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[I-1][Cols-1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I-1][Cols]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I][Cols]:=Face.Point[index];
                         end;
                      end;
                   end;
                   DataOK:=True;
                   if Mode=amNURBS then
                   begin
                      Edge1:=EdgeExists(Grid[0][Cols],Grid[0][Cols-1]);
                      Edge2:=EdgeExists(Grid[0][Cols-1],Grid[0][Cols-2]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      Edge1:=EdgeExists(Grid[Rows-1][Cols],Grid[Rows-1][Cols-1]);
                      Edge2:=EdgeExists(Grid[Rows-1][Cols-1],Grid[Rows-1][Cols-2]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      for I:=2 to Rows-1 do if not Grid[I-2][Cols].IsRegularNURBSPoint(CheckFaces) then
                         DataOK:=False;
                   end;
                   if not DataOK then
                   begin
                      // do not add the current row
                      for I:=1 to NFaces do Faces.AddSorted(TmpFaces[I-1]);
                      SearchRight:=False;
                   end else inc(Cols);
                 end else SearchRight:=False;
              end else if (Counter=3) and (SearchTop) then
              begin
                 Setlength(TmpFaces,Cols);
                 NFaces:=0;
                 for I:=2 to Cols do
                 begin
                    Edge1:=EdgeExists(Grid[0][I-2],Grid[0][I-1]);
                    if Edge1<>nil then if not Edge1.Crease then for J:=1 to Edge1.NumberOfFaces do
                    begin
                       Face:=Edge1.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Cols-1 then
                 begin
                   // search was successfull
                   Setlength(Grid,Rows+1);
                   Setlength(Grid[Rows],Cols);
                   for I:=Rows downto 1 do
                   begin
                      for J:=1 to Cols do Grid[I][J-1]:=Grid[I-1][J-1];
                   end;
                   for I:=1 to Cols do Grid[0][I-1]:=nil;

                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);

                      Index:=Face.IndexOfPoint(Grid[1][I-1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[1][I] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[0][I]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[0][I-1]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[1][I]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[1][I-1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[0][I-1]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[0][I]:=Face.Point[index];
                         end;
                      end;
                   end;
                   DataOK:=True;
                   if Mode=amNURBS then
                   begin
                      Edge1:=EdgeExists(Grid[0][0],Grid[1][0]);
                      Edge2:=EdgeExists(Grid[1][0],Grid[2][0]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      Edge1:=EdgeExists(Grid[0][Cols-1],Grid[1][Cols-1]);
                      Edge2:=EdgeExists(Grid[1][Cols-1],Grid[2][Cols-1]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      for I:=2 to Cols-1 do if not Grid[0][I-1].IsRegularNURBSPoint(CheckFaces) then
                         DataOK:=False;
                   end;
                   if not DataOK then
                   begin
                      // do not add the current row
                      for I:=1 to NFaces do Faces.AddSorted(TmpFaces[I-1]);
                      SearchTop:=False;
                      for I:=1 to Rows do
                      begin
                         for J:=1 to Cols do Grid[I-1][J-1]:=Grid[I][J-1];
                      end;
                   end else inc(Rows);
                 end else SearchTop:=False;
              end else  if (Counter=4) and (SearchLeft) then
              begin
                 Setlength(TmpFaces,Rows);
                 NFaces:=0;
                 for I:=2 to Rows do
                 begin
                    Edge1:=EdgeExists(Grid[I-2][0],Grid[I-1][0]);
                    if Edge1<>nil then if not Edge1.Crease then for J:=1 to Edge1.NumberOfFaces do
                    begin
                       Face:=Edge1.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Rows-1 then
                 begin
                   // search was successfull
                   for I:=1 to Rows do
                   begin
                      Setlength(grid[I-1],Cols+1);
                      for J:=Cols downto 1 do Grid[I-1][J]:=Grid[I-1][J-1];
                   end;
                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);
                      Index:=Face.IndexOfPoint(Grid[I][1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[I-1][1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I-1][0]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I][0]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[I-1][1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[I][1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I][0]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I-1][0]:=Face.Point[index];
                         end;
                      end;
                   end;
                   DataOK:=True;
                   if Mode=amNURBS then
                   begin
                      Edge1:=EdgeExists(Grid[0][0],Grid[0][1]);
                      Edge2:=EdgeExists(Grid[0][1],Grid[0][2]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      Edge1:=EdgeExists(Grid[Rows-1][0],Grid[Rows-1][1]);
                      Edge2:=EdgeExists(Grid[Rows-1][1],Grid[Rows-1][2]);
                      if (Edge1<>nil) and (Edge2<>nil) then if Edge1.Crease<>Edge2.Crease then DataOK:=False;
                      for I:=2 to Rows-1 do if not Grid[I-2][0].IsRegularNURBSPoint(CheckFaces) then
                         DataOK:=False;
                   end;
                   if not DataOK then
                   begin
                      // do not add the current column
                      for I:=1 to NFaces do Faces.AddSorted(TmpFaces[I-1]);
                      SearchLeft:=False;
                      for I:=1 to Rows do
                      begin
                        for J:=1 to Cols do Grid[I-1][J-1]:=Grid[I-1][J];
                      end;
                   end else inc(Cols);
                 end else SearchLeft:=False;
              end;

              if Mode=amNURBS then
              begin
                 if not Grid[0][0].IsRegularNURBSPoint(CheckFaces) then
                 begin
                    SearchLeft:=False;
                    SearchTop:=False;
                 end;
                 if not Grid[Rows-1][0].IsRegularNURBSPoint(CheckFaces) then
                 begin
                    SearchLeft:=False;
                    SearchBottom:=False;
                 end;
                 if not Grid[Rows-1][Cols-1].IsRegularNURBSPoint(CheckFaces) then
                 begin
                    SearchRight:=False;
                    SearchBottom:=False;
                 end;
                 if not Grid[0][Cols-1].IsRegularNURBSPoint(CheckFaces) then
                 begin
                    SearchRight:=False;
                    SearchTop:=False;
                 end;
              end;
           end;
        end;{DoAssemble}

    begin
       CtrlFaces.Sort;
       CheckFaces:=TFasterList.Create;
       Checkfaces.AddList(CtrlFaces);
       while CtrlFaces.Count>0 do
       begin
          // find a corner face
          Face:=FindCornerFace;
          inc(ErrInd);

          if Face<>nil then
          begin
            Index:=CtrlFaces.SortedIndexOf(Face);
            if Index<>-1 then CtrlFaces.Delete(Index);

            if Face.NumberOfpoints=4 then
            begin
               NCols:=2;
               NRows:=2;
               Setlength(Grid,NRows);
               Setlength(Grid[0],NCols);
               Setlength(Grid[1],NCols);
               Grid[0][1]:=Face.Point[0];
               Grid[0][0]:=Face.Point[1];
               Grid[1][0]:=Face.Point[2];
               Grid[1][1]:=Face.Point[3];

               DoAssemble(Grid,NCols,NRows,CtrlFaces);

               Newface.NCols:=NCols-1;
               NewFace.NRows:=NRows-1;
               Setlength(NewFace.Faces,NewFace.NRows);
               for I:=2 to NRows do
               begin
                  Setlength(NewFace.Faces[I-2],NewFace.NCols);
                  for J:=2 to NCols do
                  begin
                     Face:=GetFace(Grid[I-2][J-1],Grid[I-2][J-2],Grid[I-1][J-2],Grid[I-1][J-1]);
                     if Face<>nil then
                     begin
                        NewFace.Faces[I-2][J-2]:=Face;
                     end else
                     begin
                        //Raise Exception.Create('Error while assembling faces'+#32+IntToStr(ErrInd));
                     end;
                  end;
               end;

               AddFace(NewFace);
            end else
            begin
               NewFace.NCols:=1;
               NewFace.NRows:=1;
               Setlength(NewFace.Faces,NewFace.NRows);
               Setlength(NewFace.Faces[0],NewFace.NCols);
               NewFace.Faces[0][0]:=Face;
               AddFace(NewFace);
            end;

          end else Raise Exception.Create('No valid cornerface found!');
       end;
       Checkfaces.Destroy;
    end;{AssembleFaces}

begin
   ToDoList:=TFasterlist.Create;
   DoneList:=TFasterlist.Create;
   NAssembled:=0;
   try
      // use all visible faces
      for I:=1 to Layers.Count do
      begin
         Layer:=Layers[I-1];
         if Layer.Visible then
         begin
            ToDoList.Capacity:=ToDoList.Count+Layer.Count;
            ToDoList.AddList(Layer.FPatches);
         end;
      end;
      if ToDoList.Count>0 then
      begin
         while ToDoList.Count>0 do
         begin
            Face:=ToDoList[ToDoList.Count-1];
            ToDoList.Delete(ToDoList.Count-1);
            Current:=TFasterlist.Create;
            Current.Add(Face);
            FindAttachedFaces(Current,Face);
            DoneList.Add(Current);
         end;
         Capacity:=50;
         setlength(AssembledPatches,Capacity);
         // Assign all groups to different layers
         for I:=1 to DoneList.Count do
         begin
            Current:=DoneList[I-1];
            if Current.Count>0 then AssembleFaces(Current,AssembledPatches,NAssembled);
            Current.Destroy;
         end;
      end;
      ToDoList.Destroy;
   finally
      DoneList.Destroy;
   end;
end;{TFreeSubdivisionSurface.AssembleFacesToPatches}

// Calculate Gauss. curvature in each point of the mesh and store it in a array
procedure TFreeSubdivisionSurface.CalculateGaussCurvature;
var I       : Integer;
    Point   : TFreeSubdivisionPoint;
begin
   if not build then Rebuild;
   setlength(FGausCurvature,FPoints.Count);
   FPoints.Sort;
   FMinGaussCurvature:=0.0;
   FMaxGaussCurvature:=0.0;
   for I:=1 to FPoints.Count do
   begin
      Point:=FPoints[I-1];
      FGausCurvature[I-1]:=Point.Curvature;
      if I=1 then
      begin
         FMinGaussCurvature:=FGausCurvature[I-1];
         FMaxGaussCurvature:=FGausCurvature[I-1];
      end;
      if FGausCurvature[I-1]<FMinGaussCurvature then FMinGaussCurvature:=FGausCurvature[I-1];
      if FGausCurvature[I-1]>FMaxGaussCurvature then FMaxGaussCurvature:=FGausCurvature[I-1];
   end;
end;{TFreeSubdivisionSurface.CalculateGaussCurvature}

function TFreeSubdivisionSurface.FGetControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
begin
   Result:=TObject(FControlpoints[index]) as TFreeSubdivisionControlPoint;
end;{TFreeSubdivisionSurface.FGetControlPoint}

function TFreeSubdivisionSurface.FGetControlEdge(Index:Integer):TFreesubdivisionControlEdge;
begin
   Result:=TObject(FControlEdges[index]) as TFreesubdivisionControlEdge;
end;{TFreeSubdivisionSurface.FGetControlEdge}

function TFreeSubdivisionSurface.FGetControlCurve(Index:Integer):TFreesubdivisionControlCurve;
begin
   Result:=TObject(FControlCurves[index]) as TFreesubdivisionControlCurve;
end;{TFreeSubdivisionSurface.FGetControlCurve}

function TFreeSubdivisionSurface.FGetControlFace(Index:Integer):TFreeSubdivisionControlFace;
begin
   Result:=TObject(FControlFaces[index]) as TFreeSubdivisionControlFace;
end;{TFreeSubdivisionSurface.FGetControlFace}

function TFreeSubdivisionSurface.FGetGaussCurvatureCalculated:boolean;
begin
   Result:=Build and (length(FGausCurvature)=FPoints.Count);
end;{TFreeSubdivisionSurface.FGetGaussCurvatureCalculated}

function TFreeSubdivisionSurface.FGetLayer(Index:Integer):TFreeSubdivisionLayer;
begin
   if (Index>=0) and (Index<Flayers.Count) then Result:=FLayers[index] else
   begin
      Raise Exception.Create('Invalid layer index!');
   end;
end;{TFreeSubdivisionSurface.FGetLayer}

function TFreeSubdivisionSurface.FGetNumberOfControlPoints:Integer;
begin
   Result:=FControlPoints.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfControlEdges:Integer;
begin
   Result:=FControlEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlEdges}

function TFreeSubdivisionSurface.FGetNumberOfControlCurves:Integer;
begin
   Result:=FControlCurves.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlCurves}

function TFreeSubdivisionSurface.FGetNumberOfControlFaces:Integer;
begin
   Result:=FControlFaces.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlFaces}

function TFreeSubdivisionSurface.FGetNumberOfFaces:Integer;
var I:Integer;
begin
   Result:=0;
   for I:=1 to NumberOfControlfaces do inc(Result,ControlFace[I-1].ChildCount);
end;{TFreeSubdivisionSurface.FGetNumberOfFaces}

function TFreeSubdivisionSurface.FGetNumberOfLayers:Integer;
begin
   Result:=FLayers.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfLayers}

function TFreeSubdivisionSurface.FGetNumberOfLockedPoints:Integer;
var I:Integer;
begin
   Result:=0;
   for I:=1 to NumberOfControlPoints do if ControlPoint[I-1].Locked then inc(Result);
end;{TFreeSubdivisionSurface.FGetNumberOfLockedPoints}

function TFreeSubdivisionSurface.FGetPoint(Index:Integer):TFreeSubdivisionPoint;
begin
   if FPoints.Count>0 then Result:=FPoints[index]
                      else Result:=FControlpoints[index];
end;{TFreeSubdivisionSurface.FGetPoint}

function TFreeSubdivisionSurface.FGetEdge(Index:Integer):TFreeSubdivisionEdge;
begin
   if FEdges.Count>0 then Result:=FEdges[index]
                     else Result:=FControlEdges[index];
end;{TFreeSubdivisionSurface.FGetEdge}

function TFreeSubdivisionSurface.FGetNumberOfPoints:Integer;
begin
   if FPoints.Count>0 then Result:=FPoints.Count
                      else Result:=FControlpoints.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfPoints}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlCurves:Integer;
begin
   Result:=FSelectedControlCurves.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlCurves}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlEdges:Integer;
begin
   Result:=FSelectedControlEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlEdges}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlFaces:Integer;
begin
   Result:=FSelectedControlFaces.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlFaces}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlPoints:Integer;
begin
   Result:=FSelectedControlPoints.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfSelectedLockedPoints:Integer;
var I:Integer;
begin
   Result:=0;
   for I:=1 to NumberOfSelectedControlPoints do if SelectedControlPoint[I-1].Locked then inc(Result);
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedLockedPoints}

function TFreeSubdivisionSurface.FGetNumberOfEdges:Integer;
begin
   if FEdges.Count>0 then Result:=FEdges.Count
                     else Result:=FControlEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfEdges}

function TFreeSubdivisionSurface.FGetSelectedControlCurve(Index:Integer):TFreeSubdivisionControlCurve;
begin
   Result:=FSelectedControlCurves[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlCurve}

function TFreeSubdivisionSurface.FGetSelectedControlEdge(Index:Integer):TFreeSubdivisionControlEdge;
begin
   Result:=FSelectedControlEdges[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlEdge}

function TFreeSubdivisionSurface.FGetSelectedControlFace(Index:Integer):TFreeSubdivisionControlFace;
begin
   Result:=FSelectedControlfaces[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlFace}

function TFreeSubdivisionSurface.FGetSelectedControlPoint(Index:Integer):TFreeSubdivisionControlPoint;
begin
   Result:=FSelectedControlPoints[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlPoint}

function TFreeSubdivisionSurface.FRequestNewLayerID:Integer;
begin
   inc(FLastusedLayerID);
   Result:=FLastusedLayerID;
end;{TFreeSubdivisionSurface.FRequestNewLayerID}

procedure TFreeSubdivisionSurface.FSetActiveLayer(Val:TFreeSubdivisionLayer);
begin
   FActiveLayer:=Val;
   if assigned(FOnChangeActiveLayer) then FOnChangeActiveLayer(self,FActiveLayer);
end;{TFreeSubdivisionSurface.FSetActiveLayer}

procedure TFreeSubdivisionSurface.FSetBuild(Val:Boolean);
var I : Integer;
begin
   inherited FSetBuild(Val);
   if not Val then
   begin
      ClearFaces;
      for I:=1 to NumberofControlCurves do ControlCurve[I-1].Build:=False;
      FCurrentSubdivisionLevel:=0;
      Setlength(FGausCurvature,0);
      FMinGaussCurvature:=0.0;
      FMaxGaussCurvature:=0.0;
   end;
end;{TFreeSubdivisionSurface.FSetBuild}

procedure TFreeSubdivisionSurface.FSetDesiredSubdivisionLevel(val:byte);
begin
   if Val>4 then Val:=4;
   if Val<>FDesiredSubdivisionLevel then
   begin
      FDesiredSubdivisionLevel:=val;
      Build:=False;
   end;
end;{TFreeSubdivisionSurface.FSetDesiredSubdivisionLevel}

procedure TFreeSubdivisionSurface.FSetFShowControlNet(Val:Boolean);
begin
   if Val<>FShowControlNet then
   begin
      FShowControlNet:=Val;
   end;
end;{TFreeSubdivisionSurface.FSetFShowControlNet}

procedure TFreeSubdivisionSurface.FSetSubdivisionMode(val:TFreeSubdivisionMode);
begin
   if val<>FSubdivisionMode then
   begin
      FSubdivisionMode:=val;
      Build:=False;
   end;
end;{TFreeSubdivisionSurface.FSetSubdivisionMode}

function TFreeSubdivisionSurface.AddControlFace(Points:array of T3DCoordinate;NoPoints:Integer):TFreeSubdivisionControlFace;
var I,J,N      : Integer;
    P          : T3DCoordinate;
    Edge       : TFreeSubdivisionEdge;
    Point,Prev : TFreeSubdivisionPoint;
    dist       : TFloatType;
    MaxError   : double;
    InValidFace: boolean;
begin

   // Remove double points
   I:=0;
   MaxError:=1/Power(10,Decimals);
   while I<NoPoints-1 do
   begin
      J:=I+1;
      while J<NoPoints do
      begin
         Dist:=DistPP3D(Points[I],Points[j]);
         if Dist<=MaxError then
         begin
            for N:=J to NoPoints-2 do Points[N]:=Points[N+1];
            Dec(NoPoints);
         end else Inc(J);
      end;
      Inc(I);
   end;

   Prev:=nil;
   if NoPoints>2 then
   begin
      Result:=TFreeSubdivisionControlFace.Create(Self);
      //P1:=Points[NoPoints-1];
      //Prev:=AddControlPoint(Decimals,P1);
      for I:=1 to NoPoints do
      begin
         P:=Points[I-1];
         Point:=AddControlPoint(P);
         Result.AddPoint(Point);

         if I>1 then
         begin
            Edge:=AddControlEdge(Prev,Point);
            Edge.AddFace(Result);
         end;
         Prev:=Point;
      end;
      Point:=Result.Point[0];
      Edge:=AddControlEdge(Prev,Point);
      Edge.AddFace(Result);

      // Check if a point refers to the same face more than once ==> invalid face
      InValidFace:=False;
      for I:=1 to Result.NumberOfpoints do
      begin
         N:=0;
         Point:=Result.Point[I-1];
         for J:=1 to Point.NumberOfFaces do if Point.Face[J-1]=Result then Inc(N);
         if N>1 then
         begin
            InvalidFace:=True;
         end;
      end;

      if (Result.NumberOfpoints<3) or (InvalidFace) then
      begin
         // Delete invalid controlfaces
         for J:=1 to Result.NumberOfpoints do
         begin
            Result.Point[J-1].DeleteFace(Result);
            if J=1 then Edge:=EdgeExists(Result.Point[Result.NumberOfPoints-1],Result.Point[J-1])
                   else Edge:=EdgeExists(Result.Point[J-2],Result.Point[J-1]);
            if Edge<>nil then
            begin
               Edge.DeleteFace(Result);
               Edge.StartPoint.DeleteEdge(Edge);
               Edge.StartPoint.DeleteFace(Result);
               Edge.EndPoint.DeleteEdge(Edge);
               Edge.EndPoint.DeleteFace(Result);
            end;
         end;
         Result.Destroy;
         Result:=nil;
      end else
      begin
         FControlFaces.Add(Result);
      end;
   end else Result:=nil;
   Build:=False;
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlEdge(P1,P2:TFreeSubdivisionPoint):TFreesubdivisionControlEdge;
var Edge : TFreesubdivisionControlEdge;
begin
   Edge:=EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
   if Edge=nil then
   begin
      Edge:=TFreesubdivisionControlEdge.Create(Self);
      Edge.Startpoint:=P1;
      Edge.Endpoint:=P2;
      Edge.FControlEdge:=True;
      P1.AddEdge(Edge);
      P2.AddEdge(Edge);
      FControlEdges.Add(Edge);
      Result:=Edge;
   end else Result:=Edge;
end;{TFreeSubdivisionSurface.AddControlEdge}

procedure TFreeSubdivisionSurface.AddControlCurve(Curve:TFreesubdivisionControlCurve);
begin
   FControlCurves.Add(Curve);
   Curve.FOwner:=self;
   Build:=False;
end;{TFreeSubdivisionSurface.AddControlCurve}

function TFreeSubdivisionSurface.AddControlFace(Points:TFasterList;CheckEdges:Boolean;Layer:TFreeSubdivisionLayer):TFreeSubdivisionControlFace;
var I,J,N      : Integer;
    Index      : Integer;
    P1,P2      : TFreeSubdivisionControlPoint;
    Edge       : TFreesubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    FaceExists : boolean;
begin

   Result:=nil;
/////   if Points.Count>2 then if Points[Points.Count-1]=Points[0] then Points.Delete(Points.Count-1);
   if Points.Count>2 then
   begin
      if Points[Points.Count-1]=Points[0] then Points.Delete(Points.Count-1);
      // Check if another patch with the same vertices exists
      FaceExists:=False;
      I:=1;
      while I<=Points.Count do
      begin
         P1:=Points[I-1];
         J:=1;
         while J<=P1.NumberOfFaces do
         begin
            Face:=P1.Face[J-1] as TFreeSubdivisionControlFace;
            if Face.NumberOfpoints=Points.Count then
            begin
               FaceExists:=True;
               N:=1;
               while (N<=Points.Count) and (FaceExists) do
               begin
                  Index:=Face.FPoints.IndexOf(Points[N-1]);
                  if Index=-1 then
                  begin
                     N:=Points.Count;
                     FaceExists:=False;
                  end else Inc(N);
               end;
               if FaceExists then
               begin
                  Result:=nil;
                  exit;
               end;
            end;
            inc(J);
         end;
         Inc(I);
      end;
      if FaceExists then
      begin
         Result:=nil;
         exit;
      end;

      Result:=TFreeSubdivisionControlFace.Create(Self);
      if Layer=nil then Layer:=self.Layer[0];
      Result.FLayer:=layer;
      Layer.AddControlFace(Result);
      Result.FPoints.Capacity:=Points.Count;
      FControlFaces.Add(Result);

      P1:=Points[Points.Count-1];
      for I:=1 to Points.Count do
      begin
         P2:=Points[I-1];
         P2.FFaces.Add(Result);
         Result.FPoints.Add(P2);
         Edge:=EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
         if Edge=nil then
         begin
            Edge:=TFreesubdivisionControlEdge.Create(Self);
            Edge.FStartpoint:=P1;
            Edge.FEndpoint:=P2;
            Edge.FControlEdge:=True;
            P1.FEdges.Add(Edge);
            P2.FEdges.Add(Edge);
            FControlEdges.Add(Edge);
            Edge.FFaces.Add(Result);
            Edge.Crease:=true;
         end else
         begin
            Edge.AddFace(Result);
            if CheckEdges then Edge.Crease:=Edge.NumberOfFaces<2;
         end;
         P1:=P2;
      end;
//      if Result.NumberOfpoints<3 then
//      begin
//         Result.Destroy;
//         Result:=nil;
//      end else Build:=False;
   end;
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlFaceN(Points:TFasterList;CheckEdges:Boolean;Layer:TFreeSubdivisionLayer):TFreeSubdivisionControlFace;
var I,J,N      : Integer;
    Index      : Integer;
    P1,P2      : TFreeSubdivisionControlPoint;
    Edge       : TFreesubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    FaceExists : boolean;
begin

   Result:=nil;
   if Points.Count>2 then
   begin
      if Points[Points.Count-1]=Points[0] then Points.Delete(Points.Count-1);
      // Check if another patch with the same vertices exists
      FaceExists:=False;
      I:=1;
      while I<=Points.Count do
      begin
         P1:=Points[I-1];
         J:=1;
         while J<=P1.NumberOfFaces do
         begin
            Face:=P1.Face[J-1] as TFreeSubdivisionControlFace;
            if Face.NumberOfpoints=Points.Count then
            begin
               FaceExists:=True;
               N:=1;
               while (N<=Points.Count) and (FaceExists) do
               begin
                  Index:=Face.FPoints.IndexOf(Points[N-1]);
                  if Index=-1 then
                  begin
                     N:=Points.Count;
                     FaceExists:=False;
                  end else Inc(N);
               end;
               if FaceExists then
               begin
                  Result:=nil;
                  exit;
               end;
            end;
            inc(J);
         end;
         Inc(I);
      end;
      if FaceExists then
      begin
         Result:=nil;
         exit;
      end;

      Result:=TFreeSubdivisionControlFace.Create(Self);
      if Layer=nil then Layer:=self.Layer[0];
      Result.FLayer:=layer;
      Layer.AddControlFace(Result);
      Result.FPoints.Capacity:=Points.Count;
      FControlFaces.Add(Result);

      P1:=Points[Points.Count-1];
      for I:=1 to Points.Count do
      begin
         P2:=Points[I-1];
         P2.FFaces.Add(Result);
         Result.FPoints.Add(P2);
         Edge:=EdgeExists(P1,P2) as TFreesubdivisionControlEdge;
         if Edge=nil then
         begin
            Edge:=TFreesubdivisionControlEdge.Create(Self);
            Edge.FStartpoint:=P1;
            Edge.FEndpoint:=P2;
            Edge.FControlEdge:=True;
            P1.FEdges.Add(Edge);
            P2.FEdges.Add(Edge);
            FControlEdges.Add(Edge);
            Edge.FFaces.Add(Result);
         end else
         begin
            Edge.AddFace(Result);
         end;
         P1:=P2;
      end;
   end;
end;{TFreeSubdivisionSurface.AddControlFaceN}



function TFreeSubdivisionSurface.AddControlFace(Points:TFasterList;CheckEdges:Boolean):TFreeSubdivisionControlFace;
begin
   Result:=AddControlFace(Points,CheckEdges,nil);
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlFace(Points:TList;CheckEdges:Boolean):TFreeSubdivisionControlFace;
var Tmp  : TFasterlist;
    I    : Integer;
begin
   Tmp:=TFasterList.Create;
   Tmp.Capacity:=Points.Count;
   for I:=1 to Points.Count do Tmp.Add(Points[I-1]);
   Result:=AddControlFace(Tmp,CheckEdges,nil);
   Tmp.Destroy;
end;{TFreeSubdivisionSurface.AddControlFace}

procedure TFreeSubdivisionSurface.Clear;
var I       : Integer;
    Layer   : TFreeSubdivisionLayer;
begin
   inherited Clear;
   for I:=1 to FControlPoints.Count do ControlPoint[I-1].Destroy;
   FControlPoints.Clear;
   for I:=1 to NumberOfControlFaces do ControlFace[I-1].Destroy;
   FControlFaces.Clear;
   for I:=1 to NumberOfControlEdges do ControlEdge[I-1].Destroy;
   FControlEdges.Clear;
   for I:=1 to NumberOfControlCurves do ControlCurve[I-1].Destroy;
   FControlCurves.Clear;
   for I:=1 to FEdges.Count do Edge[I-1].Destroy;
   FEdges.Clear;
   for I:=1 to FPoints.Count do Point[I-1].Destroy;
   FPoints.Clear;
   for i:=1 to NumberOfLayers do self.Layer[I-1].Destroy;
   FLayers.Clear;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
   FLastusedLayerID:=-1;
   // delete lists with selected items
   FSelectedControlPoints.Clear;
   FSelectedControlEdges.Clear;
   FSelectedControlFaces.Clear;
   FSelectedControlCurves.Clear;
   // Add one default layer and set it to active
   Layer:=AddNewLayer;
   ActiveLayer:=Layer;
   Build:=False;
   FDrawMirror:=False;
   FShowControlNet:=True;
   FShowInteriorEdges:=False;
   FInitialized:=False;
   FDesiredSubdivisionLevel:=1;
   FShowNormals:=True;
   FShadeUnderWater:=False;
   FMainframeLocation:=1e10;
end;{TFreeSubdivisionSurface.Clear}

procedure TFreeSubdivisionSurface.ClearFaces;
var I : integer;
begin
   if Assigned(FControlFaces) then
     for I:=1 to NumberOfControlFaces do
       Controlface[I-1].ClearChildren; // deletes children and rendermesh
   if Assigned(FEdges) then begin
     for I:=1 to FEdges.Count do Edge[I-1].Destroy;
     FEdges.Clear;
   end;
   if Assigned(FPoints) then begin
     for I:=1 to FPoints.Count do Point[I-1].Destroy;
     FPoints.Clear;
   end;
   if Assigned(FControlFaces) then
     for I:=1 to NumberOfControlFaces do
       Controlface[I-1].FControlEdges.Clear;
end;{TFreeSubdivisionSurface.ClearFaces}

procedure TFreeSubdivisionSurface.Clearselection;
// Deselect all selected items at once
begin
   FSelectedControlPoints.Clear;
   FSelectedControlEdges.Clear;
   FSelectedControlFaces.Clear;
   FSelectedControlCurves.Clear;
   if Assigned(FOnSelectItem) then FOnSelectItem(nil);
end;{TFreeSubdivisionSurface.Clearselection}

procedure TFreeSubdivisionSurface.ConvertToGrid(Input:TFreeFaceGrid;var Cols,Rows:Integer;var Grid:TFreeSubdivisionGrid);
var CtrlFace   : TFreeSubdivisionControlFace;
    Faces      : TFasterList;
    Backup     : TFasterList;
    Face       : TFreeSubdivisionFace;
    I,J,N,Ind  : Integer;

        procedure DoAssemble(var Grid:TFreeSubdivisionGrid;var Cols,Rows:Integer;Faces:TFasterList);
        var SearchBottom   : Boolean;
            SearchTop      : Boolean;
            SearchLeft     : Boolean;
            SearchRight    : Boolean;
            Counter,Index  : Integer;
            I,J            : Integer;
            NFaces         : Integer;
            Edge           : TFreeSubdivisionEdge;
            Face           : TFreeSubdivisionFace;
            TmpFaces       : array of TFreeSubdivisionFace;

            function ValidFace(Face:TFreeSubdivisionface):Boolean;
            var I,J,N,Index : Integer;
                Tmp         : TFreeSubdivisionFace;
            begin
               Result:=False;
               if Face.NumberOfpoints=4 then
               begin
                  Index:=Faces.SortedIndexOf(Face);
                  if Index<>-1 then
                  begin
                     Result:=True;
                     for I:=1 to NFaces do if TmpFaces[I-1]=face then
                     begin
                        result:=false;
                        exit;
                     end;
                     if NFaces>0 then
                     begin
                        // must also be connected to previous face
                        Tmp:=TmpFaces[NFaces-1];
                        N:=0;
                        for J:=1 to Face.NumberOfpoints do
                        begin
                           if Tmp.IndexOfPoint(Face.Point[J-1])<>-1 then inc(N);
                        end;
                        Result:=N>1;
                     end;
                  end;
               end;
            end;{ValidFace}

        begin
           Counter:=0;
           SearchBottom:=True;
           SearchTop:=True;
           SearchRight:=True;
           SearchLeft:=True;
           while ((SearchBottom) or (SearchTop) or (SearchRight) or (SearchLeft)) and (Faces.Count>0) do
           begin
              inc(Counter);
              if Counter>4 then Counter:=1;
              if (Counter=1) and (SearchBottom) then
              begin
                 Setlength(TmpFaces,Cols);
                 NFaces:=0;
                 for I:=2 to Cols do
                 begin
                    Edge:=EdgeExists(Grid[Rows-1][I-2],Grid[Rows-1][I-1]);
                    if Edge<>nil then for J:=1 to Edge.NumberOfFaces do
                    begin
                       Face:=Edge.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Cols-1 then
                 begin
                  // search was successfull
                   for I:=1 to NFaces do
                   begin
                      Setlength(Grid,Rows+1);
                      Setlength(Grid[Rows],Cols);
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);

                      Index:=Face.IndexOfPoint(Grid[Rows-1][I]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[Rows-1][I-1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[Rows][I-1]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[Rows][I]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[Rows-1][I-1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[Rows-1][I] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[Rows][I]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[Rows][I-1]:=Face.Point[index];
                         end;
                      end;
                   end;
                   inc(Rows);
                 end else SearchBottom:=False;
              end else if (Counter=2) and (SearchRight) then
              begin
                 Setlength(TmpFaces,Rows);
                 NFaces:=0;
                 for I:=2 to Rows do
                 begin
                    Edge:=EdgeExists(Grid[I-1][Cols-1],Grid[I-2][Cols-1]);
                    if Edge<>nil then for J:=1 to Edge.NumberOfFaces do
                    begin
                       Face:=Edge.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Rows-1 then
                 begin
                   // search was successfull
                   for I:=1 to Rows do
                   begin
                      Setlength(grid[I-1],Cols+1);
                      Grid[I-1][Cols]:=nil;
                   end;
                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);
                      Index:=Face.IndexOfPoint(Grid[I-1][Cols-1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[I][Cols-1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I][Cols]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I-1][Cols]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[I][Cols-1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[I-1][Cols-1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I-1][Cols]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I][Cols]:=Face.Point[index];
                         end;
                      end;
                   end;
                   inc(Cols);
                 end else SearchRight:=False;
              end else if (Counter=3) and (SearchTop) then
              begin
                 Setlength(TmpFaces,Cols);
                 NFaces:=0;
                 for I:=2 to Cols do
                 begin
                    Edge:=EdgeExists(Grid[0][I-2],Grid[0][I-1]);
                    if Edge<>nil then for J:=1 to Edge.NumberOfFaces do
                    begin
                       Face:=Edge.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Cols-1 then
                 begin
                   // search was successfull
                   Setlength(Grid,Rows+1);
                   Setlength(Grid[Rows],Cols);
                   for I:=Rows downto 1 do
                   begin
                      for J:=1 to Cols do Grid[I][J-1]:=Grid[I-1][J-1];
                   end;
                   for I:=1 to Cols do Grid[0][I-1]:=nil;

                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);

                      Index:=Face.IndexOfPoint(Grid[1][I-1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[1][I] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[0][I]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[0][I-1]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[1][I]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[1][I-1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[0][I-1]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[0][I]:=Face.Point[index];
                         end;
                      end;
                   end;
                   inc(Rows);
                 end else SearchTop:=False;
              end else  if (Counter=4) and (SearchLeft) then
              begin
                 Setlength(TmpFaces,Rows);
                 NFaces:=0;
                 for I:=2 to Rows do
                 begin
                    Edge:=EdgeExists(Grid[I-2][0],Grid[I-1][0]);
                    if Edge<>nil then for J:=1 to Edge.NumberOfFaces do
                    begin
                       Face:=Edge.Face[J-1];
                       if ValidFace(Face) then
                       begin
                          TmpFaces[NFaces]:=Face;
                          inc(NFaces);
                          break;
                       end;
                    end;
                    if NFaces<>I-1 then break;
                 end;
                 if NFaces=Rows-1 then
                 begin
                   // search was successfull
                   for I:=1 to Rows do
                   begin
                      Setlength(grid[I-1],Cols+1);
                      for J:=Cols downto 1 do Grid[I-1][J]:=Grid[I-1][J-1];
                      //Move(Grid[I-1][0],Grid[I-1][1],Cols*SizeOf(Pointer));
                   end;
                   for I:=1 to NFaces do
                   begin
                      Face:=TmpFaces[I-1];
                      Index:=Faces.SortedIndexOf(Face);
                      if Index<>-1 then Faces.Delete(index);
                      Index:=Face.IndexOfPoint(Grid[I][1]);
                      Index:=(Index+1) mod Face.NumberOfpoints;
                      if Face.Point[index]=Grid[I-1][1] then
                      begin
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I-1][0]:=Face.Point[index];
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         Grid[I][0]:=Face.Point[index];
                      end else
                      begin
                         Index:=Face.IndexOfPoint(Grid[I-1][1]);
                         Index:=(Index+1) mod Face.NumberOfpoints;
                         if Face.Point[index]=Grid[I][1] then
                         begin
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I][0]:=Face.Point[index];
                            Index:=(Index+1) mod Face.NumberOfpoints;
                            Grid[I-1][0]:=Face.Point[index];
                         end;
                      end;
                   end;
                   inc(Cols);
                 end else SearchLeft:=False;
              end;
           end;
        end;{DoAssemble}

begin
   Cols:=0;
   Rows:=0;
   if (Input.NCols=0) or (Input.NRows=0) then exit;
   N:=Input.Faces[0][0].ChildCount;
   // assmeble all childfaces in one temp. sorted list
   backup:=TFasterList.Create;
   backup.Capacity:=Input.NCols*Input.NRows*N;
   for I:=1 to Input.NRows do
   begin
      for J:=1 to Input.NCols do
      begin
         CtrlFace:=Input.Faces[I-1][J-1];
         backup.AddList(Ctrlface.FChildren);
      end;
   end;
   backup.Sort;
   if backup.Count>0 then
   begin
      try
         Faces:=TFasterList.Create;
         Ind:=0;
         repeat
            Faces.Assign(Backup);
            inc(Ind);
            Face:=Faces[Ind-1];
            Faces.Delete(Ind-1);
            Rows:=2;
            Cols:=2;
            Setlength(Grid,Rows);
            Setlength(grid[0],Cols);
            Setlength(grid[1],Cols);
            grid[0][1]:=Face.Point[0];
            grid[0][0]:=Face.Point[1];
            grid[1][0]:=Face.Point[2];
            grid[1][1]:=Face.Point[3];
            DoAssemble(Grid,Cols,Rows,Faces);
         until (Faces.Count=0) or (Ind=Backup.Count);
         if Faces.Count<>0 then
         begin
            MessageDlg('Could not establish the entire grid!',mtError,[mbOk],0);
         end;
         Faces.Destroy;
      finally
         Backup.Destroy;
      end;
   end;
end;{TFreeSubdivisionSurface.ConvertToGrid}

procedure TFreeSubdivisionSurface.Edge_Connect;
var Face          : TFreeSubdivisionControlFace;
    Edge          : TFreeSubdivisionControlEdge;
    V1,V2         : TFreeSubdivisionControlPoint;
    I,J           : Integer;
begin
   if NumberOfSelectedControlPoints>1 then
   begin
      for I:=NumberOfSelectedControlPoints-1 downto 1 do
      begin
         V1:=SelectedControlPoint[NumberOfSelectedControlPoints-2];
         V2:=SelectedControlPoint[NumberOfSelectedControlPoints-1];
         if EdgeExists(V1,V2)=nil then
         begin
            if (V1.NumberOfFaces=0) and (V2.NumberOfFaces=0) then
            begin
               Edge:=AddControlEdge(V1,V2);
               if Edge<>nil then edge.Crease:=True;
            end
            else
            For J:=1 to V1.NumberOfFaces do
            begin
               Face:=V1.Face[J-1] as TFreeSubdivisionControlFace;
               if V2.IndexOfFace(Face)<>-1 then
               begin
                  Face.InsertEdge(V1,V2);
                  V2.Selected:=false;
                  Build:=False;
                  break;
               end;
            end;
            DelayedDestroyList.DestroyAll;
         end
         else if NumberOfSelectedControlPoints=2
            then MessageDlg(Userstring(202)+'!',mtWarning,[mbOk],0);
      end;
      for I:=NumberOfSelectedControlPoints downto 1 do SelectedControlPoint[I-1].Selected:=False;
   end;
end;{TFreeSubdivisionSurface.Edge_Connect}

{
 Can new edge be inserted?
 Following conditions have to be satisfied:
  - two or more points must be selected;
  - two points must belong same face;
  - edge for these points must not exist.
}
function TFreeSubdivisionSurface.CanInsertEdge: boolean;
var Face          : TFreeSubdivisionControlFace;
    Edge          : TFreeSubdivisionControlEdge;
    V1,V2         : TFreeSubdivisionControlPoint;
    I,J           : Integer;
begin
   result := false;
   if NumberOfSelectedControlPoints>1 then
   begin
      for I:=NumberOfSelectedControlPoints-1 downto 1 do
      begin
         V1:=SelectedControlPoint[NumberOfSelectedControlPoints-2];
         V2:=SelectedControlPoint[NumberOfSelectedControlPoints-1];
         if EdgeExists(V1,V2)=nil then
         begin
            if (V1.NumberOfFaces=0) and (V2.NumberOfFaces=0) then
            begin
               Edge:=AddControlEdge(V1,V2);
               if Edge<>nil then edge.Crease:=True;
            end
            else
            For J:=1 to V1.NumberOfFaces do
            begin
               Face:=V1.Face[J-1] as TFreeSubdivisionControlFace;
               if V2.IndexOfFace(Face)<>-1 then
               begin
                  result := true;
                  break;
               end;
            end;
         end
         else if NumberOfSelectedControlPoints=2
            then result := false;
      end;
   end;
end;{TFreeSubdivisionSurface.Edge_Connect}


procedure TFreeSubdivisionSurface.ExportFEFFile(Strings:TStringList);
var I       : Integer;
begin
   // Add layer information
   Strings.Add(IntToStr(NumberOfLayers));
   for I:=1 to NumberOfLayers do
   begin
      Strings.Add(Layer[I-1].Name);
      Strings.Add(IntToStr(Layer[I-1].LayerID)+#32+
                  IntToStr(Layer[I-1].Color)+#32+
                  BoolToStr(Layer[I-1].Visible)+#32+
                  BoolToStr(Layer[I-1].Developable)+#32+
                  BoolToStr(Layer[I-1].Symmetric)+#32+
                  BoolToStr(Layer[I-1].FUseForIntersections)+#32+
                  BoolToStr(Layer[I-1].FUseInHydrostatics)+#32+
                  BoolToStr(Layer[I-1].FShowInLinesplan)+#32+
                  FloatToStrF(Layer[I-1].MaterialDensity,ffFixed,10,8)+#32+
                  FloatToStrF(Layer[I-1].Thickness,ffFixed,10,8));
   end;
   // first sort controlpoints for faster acces of function (Indexof())
   FControlPoints.Sort;
   Strings.Add(IntToStr(NumberOfControlPoints));
   for I:=1 to NumberOfControlPoints do ControlPoint[I-1].SaveToStream(Strings);
   Strings.Add(IntToStr(NumberOfControlEdges));
   for I:=1 to NumberOfControlEdges do ControlEdge[I-1].SaveToStream(Strings);
   Strings.Add(IntToStr(NumberOfControlFaces));
   for I:=1 to NumberOfControlFaces do ControlFace[I-1].SaveToStream(Strings);
end;{TFreeSubdivisionSurface.ExportFEFFile}

procedure TFreeSubdivisionSurface.ExportObjFile(ExportControlNet:Boolean;Strings:TStringList);
var I,J,K : Integer;
    Index : Integer;
    Tmp   : TFasterList;
    CFace : TFreeSubdivisionControlFace;
    Child : TFreeSubdivisionFace;
    Str   : string;
    P     : TFreeSubdivisionPoint;
begin
   if not ExportControlNet then
   begin
      // export subdivided surface

      if not build then rebuild;
      // first sort controlpoints for faster acces of function (Indexof())
      Strings.Add('# FREE!ship model');
      FPoints.Sort;
      // Create points for portside
      Tmp:=TFasterList.Create;
      Tmp.Capacity:=NumberOfPoints;
      for I:=1 to NumberOfPoints do
      begin
         Strings.Add('v'+#32+FloatToStrF(Point[I-1].Coordinate.y,ffFixed,7,4)
                        +#32+FloatToStrF(Point[I-1].Coordinate.z,ffFixed,7,4)
                        +#32+FloatToStrF(Point[I-1].Coordinate.x,ffFixed,7,4));
         if Point[I-1].Coordinate.y>0 then Tmp.Add(Point[I-1]);
      end;
      if DrawMirror then
      begin
         // Create points for portside
         Tmp.Sort;
         for I:=1 to Tmp.Count do
         begin
            P:=Tmp[I-1];
            Strings.Add('v'+#32+FloatToStrF(-P.Coordinate.y,ffFixed,7,4)
                           +#32+FloatToStrF(P.Coordinate.z,ffFixed,7,4)
                           +#32+FloatToStrF(P.Coordinate.x,ffFixed,7,4));
         end;
      end;

      for I:=1 to NumberOfControlFaces do
      begin
         CFace:=ControlFace[I-1];
         if CFace.Layer.Visible then
         begin
            for J:=1 to CFace.ChildCount do
            begin
               // portside
               Str:='f';
               Child:=CFace.Child[J-1];
               for k:=1 to Child.FPoints.Count do
               begin
                  Index:=FPoints.SortedIndexOf(Child.FPoints[K-1]);
                  if Index<>-1 then Str:=Str+#32+IntToStr(index+1);
               end;
               Strings.Add(Str);
               if (CFace.Layer.Symmetric) and (DrawMirror) then
               begin
                  // Starboard side
                  Str:='f';
                  Child:=CFace.Child[J-1];
                  for k:=Child.FPoints.Count downto 1 do
                  begin
                     if Child.Point[K-1].Coordinate.Y>0 then
                     begin
                        Index:=Tmp.SortedIndexOf(Child.Point[K-1])+NumberOfPoints;
                     end else Index:=FPoints.SortedIndexOf(Child.FPoints[K-1]);
                     Str:=Str+#32+IntToStr(index+1);
                  end;
                  Strings.Add(Str);
               end;
            end;
         end;
      end;
      Tmp.Destroy;
   end else
   begin
      // export the controlnet only
      if not build then rebuild;
      // first sort controlpoints for faster acces of function (Indexof())
      Strings.Add('# FREE!ship model');
      FControlPoints.Sort;
      // Create ControlPoints for portside
      Tmp:=TFasterList.Create;
      Tmp.Capacity:=NumberOfControlPoints;
      for I:=1 to NumberOfControlPoints do
      begin
         Strings.Add('v'+#32+FloatToStrF(ControlPoint[I-1].Coordinate.y,ffFixed,7,4)
                        +#32+FloatToStrF(ControlPoint[I-1].Coordinate.z,ffFixed,7,4)
                        +#32+FloatToStrF(ControlPoint[I-1].Coordinate.x,ffFixed,7,4));
         if ControlPoint[I-1].Coordinate.y>0 then Tmp.Add(ControlPoint[I-1]);
      end;
      if DrawMirror then
      begin
         // Create ControlPoints for portside
         Tmp.Sort;
         for I:=1 to Tmp.Count do
         begin
            P:=Tmp[I-1];
            Strings.Add('v'+#32+FloatToStrF(-P.Coordinate.y,ffFixed,7,4)
                           +#32+FloatToStrF(P.Coordinate.z,ffFixed,7,4)
                           +#32+FloatToStrF(P.Coordinate.x,ffFixed,7,4));
         end;
      end;
   
      for I:=1 to NumberOfControlFaces do
      begin
         CFace:=ControlFace[I-1];
         if CFace.Layer.Visible then
         begin
            // portside
            Str:='f';
            for k:=1 to Cface.FPoints.Count do
            begin
               Index:=FControlPoints.SortedIndexOf(Cface.FPoints[K-1]);
               if Index<>-1 then Str:=Str+#32+IntToStr(index+1);
            end;
            Strings.Add(Str);
            if (CFace.Layer.Symmetric) and (DrawMirror) then
            begin
               // Starboard side
               Str:='f';
               for k:=Cface.FPoints.Count downto 1 do
               begin
                  if Cface.Point[K-1].Coordinate.Y>0 then
                  begin
                     Index:=Tmp.SortedIndexOf(Cface.Point[K-1])+NumberOfControlPoints;
                  end else Index:=FControlPoints.SortedIndexOf(Cface.FPoints[K-1]);
                  Str:=Str+#32+IntToStr(index+1);
               end;
               Strings.Add(Str);
            end;
         end;
      end;
      Tmp.Destroy;
   end;
end;{TFreeSubdivisionSurface.ExportObjFile}

procedure TFreeSubdivisionSurface.Extents(Var Min,Max : T3DCoordinate);
var I    : Integer;
begin
   if not build then Rebuild;
   if NumberOfControlFaces>0 then
   begin
      for I:=1 to NumberOfLayers do Layer[I-1].Extents(Min,Max);
      for I:=1 to NumberOfControlPoints do if ControlPoint[I-1].NumberOfFaces=0 then
      begin
         MinMax(ControlPoint[I-1].FCoordinate,Min,Max);
      end;
   end else
   begin
      MinMax(FMin,Min,Max);
      MinMax(FMax,Min,Max);
   end;
end;{TFreeSubdivisionSurface.Extents}

procedure TFreeSubdivisionSurface.ExtrudeEdges(Edges:TFasterList;Direction:T3DCoordinate);
var I,INdex    : Integer;
    Edge,Tmp   : TFreesubdivisionControlEdge;
    Vertices   : TFasterList;
    Newedges   : TFasterList;
    Points     : TFasterList;
    Point1     : TFreeSubdivisionControlPoint;
    Point2     : TFreeSubdivisionControlPoint;
    Face       : TFreeSubdivisionControlFace;
    NoEdges    : integer;
    NoVertices : Integer;
begin
   Vertices:=TFasterList.Create;
   // first assemble all points
   for I:=1 to Edges.Count do
   begin
      Edge:=Edges[I-1];
      if Vertices.IndexOf(Edge.StartPoint)=-1 then Vertices.Add(Edge.StartPoint);
      if Vertices.IndexOf(Edge.EndPoint)=-1 then Vertices.Add(Edge.EndPoint);
   end;
   NoEdges:=FControlEdges.Count;
   NoVertices:=FControlPoints.Count;
   try
      NewEdges:=TFasterList.Create;
      // create all new extruded points
      for I:=1 to Vertices.Count do
      begin
         Point1:=Vertices[I-1];
         Point2:=TFreeSubdivisionControlPoint.Create(self);
         FControlPoints.Add(Point2);
         Point2.Coordinate:=AddPointSymm(Point1.Coordinate,Direction);
         Vertices.Objects[I-1]:=Point2;
      end;
      Points:=TFasterList.Create;
      for I:=1 to Edges.Count do
      begin
         Edge:=Edges[I-1];
         Points.Clear;
         Points.Add(Edge.EndPoint);
         Points.Add(Edge.StartPoint);
         Point1:=nil;
         Point2:=nil;
         Index:=Vertices.IndexOf(Edge.StartPoint);
         if Index<>-1 then
         begin
            Point1:=Vertices.Objects[index];
            Points.Add(Point1);
         end;

         Index:=Vertices.IndexOf(Edge.EndPoint);
         if Index<>-1 then
         begin
            Point2:=Vertices.Objects[index];
            Points.Add(Point2);
         end;
         Face:=AddControlFace(Points,True);
         Face.Layer:=self.ActiveLayer;
         if (Point1<>nil) and (Point2<>nil) then
         begin
            Tmp:=EdgeExists(Point1,Point2) as TFreeSubdivisionControlEdge;
            if Tmp<>nil then NewEdges.Add(Tmp);
         end;
         if (Edge.StartPoint.VertexType=svCorner) and (Point1<>nil) then
         begin
            Tmp:=EdgeExists(Edge.StartPoint,Point1) as TFreeSubdivisionControlEdge;
            if Tmp<>nil then Tmp.Crease:=true;
         end;
         if (Edge.EndPoint.VertexType=svCorner) and (Point2<>nil) then
         begin
            Tmp:=EdgeExists(Edge.EndPoint,Point2) as TFreeSubdivisionControlEdge;
            if Tmp<>nil then Tmp.Crease:=true;
         end;
         Edge.Crease:=true;
      end;
      Points.Destroy;

      Edges.Clear;
      // return the new edges
      for I:=1 to NewEdges.Count do Edges.Add(Newedges[I-1]);
      Newedges.Destroy;
   finally
      Vertices.Destroy;
      Initialize(NoVertices+1,NoEdges+1,NumberOfControlFaces+1);
      Build:=False;
   end;
end;{TFreeSubdivisionSurface.ExtrudeEdges}

procedure TFreeSubdivisionSurface.CalculateIntersections(Plane:T3DPlane;Faces,Destination:TFasterList);
type IntersectionData = record
                           Point    : T3DCoordinate;
                           Knuckle  : Boolean;
                           Edge     : TFreeSubdivisionEdge;
                        end;
     TSegment  = record
                    StartP,EndP:IntersectionData;
                 end;

var I,J,K,N,M  : Integer;
    Edge       : TFreeSubdivisionEdge;
    P1,P2,P3   : TFreeSubdivisionPoint;
    CtrlFace   : TFreesubdivisionControlFace;
    Skip       : Boolean;
    Side1      : TFloatType;
    Side2      : TFloatType;
    AbsSide1   : TFloatType;
    AbsSide2   : TFloatType;
    Parameter  : TFloatType;
    Output     : T3DCoordinate;
    Spline     : TFreeSpline;
    Copy       : TFreeSpline;
    Face,F2    : TFreeSubdivisionface;
    IntArray   : array of IntersectionData;
    ArrayLength: Integer;
    NoPoints   : Integer;
    Size       : Integer;
    CenterPlane: Boolean;
    Edges      : TFasterList;
    AddEdge    : Boolean;
    InPlane    : Boolean;
    Segments   : array of TSegment;
    StartP,EndP: IntersectionData;
    Segment    : TSegment;
    NSegments  : Integer;
    SegmCapacity:Integer;

begin
   // first assemble all edges belonging to this set of faces
   Edges:=TFasterlist.Create;
   Edges.Capacity:=Faces.Count+100;
   ArrayLength:=10;
   Setlength(IntArray,ArrayLength);
   Size:=SizeOf(IntersectionData);
   CenterPlane:=(abs(abs(Plane.b)-1)<1e-5) and (abs(Plane.d)<1e-4);

   NSegments:=0;
   SegmCapacity:=50;
   Setlength(Segments,SegmCapacity);

   for I:=1 to Faces.Count do
   begin
      CtrlFace:=Faces[I-1];
      for J:=1 to Ctrlface.ChildCount do
      begin
         Face:=Ctrlface.Child[J-1];
         NoPoints:=0;
         P1:=Face.Point[Face.NumberOfPoints-1];
         Side1:=Plane.A*P1.Coordinate.x+Plane.B*P1.Coordinate.y+Plane.C*P1.Coordinate.z+Plane.D;
         for K:=1 to Face.FPoints.Count do
         begin
            P2:=Face.FPoints[K-1];
            Side2:=Plane.A*P2.Coordinate.x+Plane.B*P2.Coordinate.y+Plane.C*P2.Coordinate.z+Plane.D;
            AddEdge:=False;
            if ((Side1<-1e-5) and (Side2>1e-5)) or
               ((Side1>1e-5) and (Side2<-1e-5)) then
            begin
               // regular intersection of edge
               // add the edge to the list
               Parameter:=-side1/(side2-side1);
               Output.X:=P1.Coordinate.X+Parameter*(P2.Coordinate.X-P1.Coordinate.X);
               Output.Y:=P1.Coordinate.Y+Parameter*(P2.Coordinate.Y-P1.Coordinate.Y);
               Output.Z:=P1.Coordinate.Z+Parameter*(P2.Coordinate.Z-P1.Coordinate.Z);
               Inc(NoPoints);
               if NoPoints>ArrayLength then
               begin
                  Inc(ArrayLength,10);
                  Setlength(IntArray,ArrayLength);
               end;
               IntArray[NoPoints-1].Point:=Output;
               Edge:=EdgeExists(P1,P2);
               if Edge<>nil then
               begin
                  IntArray[NoPoints-1].Knuckle:=Edge.Crease;
                  IntArray[NoPoints-1].Edge:=Edge;
               end else
               begin
                  IntArray[NoPoints-1].Knuckle:=False;
                  IntArray[NoPoints-1].Edge:=nil;
               end;
            end else
            begin
               // Does the edge lie entirely within the plane??
               if ((abs(side1)<=1e-5) and (abs(Side2)<=1e-5)) then
               begin
                  // If so then add this edge ONLY if:
                  // 1. The edge is a boundary edge
                  // 2. At least ONE of the attached faces does NOT lie in the plane
                  Edge:=EdgeExists(P1,P2);
                  if Edge<>nil then
                  begin
                     if Edge.FFaces.Count=1 then AddEdge:=True else
                     begin
                        for N:=1 to Edge.FFaces.Count do
                        begin
                           F2:=Edge.FFaces[N-1];
                           for M:=1 to F2.FPoints.Count do
                           begin
                              P3:=F2.Point[M-1];
                              Parameter:=Plane.A*P3.Coordinate.x+Plane.B*P3.Coordinate.y+Plane.C*P3.Coordinate.z+Plane.D;
                              if abs(Parameter)>1e-5 then
                              begin
                                 AddEdge:=True;
                                 break;
                              end;
                           end;
                           if AddEdge then break;
                        end;
                     end;
                     if AddEdge then
                     begin
                        if Edges.SortedIndexOf(Edge)=-1 then
                        begin
                           Edges.AddSorted(Edge);
                           inc(NSegments);
                           if NSegments>SegmCapacity then
                           begin
                              inc(SegmCapacity,75);
                              Setlength(Segments,SegmCapacity);
                           end;
                           Segments[NSegments-1].StartP.Point:=P1.FCoordinate;
                           Segments[NSegments-1].StartP.Edge:=Edge;
                           if not Edge.FCrease then Segments[NSegments-1].StartP.Knuckle:=P1.VertexType<>svRegular
                                               else Segments[NSegments-1].StartP.Knuckle:=P1.VertexType=svCorner;
                           Segments[NSegments-1].EndP.Point:=P2.FCoordinate;
                           Segments[NSegments-1].EndP.Edge:=Edge;
                           if not Edge.FCrease then Segments[NSegments-1].EndP.Knuckle:=P2.VertexType<>svRegular
                                               else Segments[NSegments-1].EndP.Knuckle:=P2.VertexType=svCorner;
                        end;
                     end;
                  end;
               end else if abs(Side2)<1e-5 then
               begin
                  Inc(NoPoints);
                  if NoPoints>ArrayLength then
                  begin
                     Inc(ArrayLength,10);
                     Setlength(IntArray,ArrayLength);
                  end;
                  IntArray[NoPoints-1].Point:=P2.Coordinate;
                  IntArray[NoPoints-1].Knuckle:=P2.VertexType<>svRegular;
                  IntArray[NoPoints-1].Edge:=EdgeExists(P1,P2);
               end;
            end;
            P1:=P2;
            Side1:=Side2;
         end;
         if NoPoints>1 then
         begin
            if IntArray[0].Edge=IntArray[NoPoints-1].Edge then if DistPP3D(IntArray[0].Point,IntArray[NoPoints-1].Point)<1e-4 then
            begin
               dec(NoPoints);
            end;
            K:=2;
            while K<=NoPoints do
            begin
               if IntArray[K-1].Edge=IntArray[K-2].Edge then
               begin
                  if DistPP3D(IntArray[K-1].Point,IntArray[K-2].Point)<1e-4 then
                  begin
                     Move(IntArray[K-1],IntArray[K-2],(NoPoints-K+1)*SizeOf(T3DCoordinate));
                     dec(NoPoints);
                  end else inc(K);
               end else inc(K);
            end;

            for K:=2 to NoPoints do
            begin
               inc(NSegments);
               if NSegments>SegmCapacity then
               begin
                  inc(SegmCapacity,50);
                  Setlength(Segments,SegmCapacity);
               end;
               Segments[NSegments-1].StartP:=IntArray[K-2];
               Segments[NSegments-1].EndP:=IntArray[K-1];
            end;
         end;
      end;
   end;

   // convert segments into polylines
   Spline:=nil;
   while NSegments>0 do
   begin
      if Spline=nil then
      begin
         Spline:=TFreeSpline.Create;
         Spline.Capacity:=NSegments;
         Destination.Add(Spline);
         Segment:=Segments[NSegments-1];
         Spline.Add(Segment.StartP.Point);
         Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.StartP.Knuckle;
         Spline.Add(Segment.EndP.Point);
         Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.EndP.Knuckle;
         StartP:=Segment.StartP;
         EndP:=Segment.EndP;
         dec(NSegments);
      end;
      AddEdge:=False;
      J:=1;
      while J<=NSegments do
      begin
         Segment:=Segments[J-1];
         if (EndP.Edge=Segment.StartP.Edge) then
         begin
            AddEdge:=DistPP3D(EndP.Point,Segment.StartP.Point)<1e-4;
            if AddEdge then
            begin
               Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.StartP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints-1];
               Spline.Add(Segment.EndP.Point);
               Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.EndP.Knuckle;
               EndP:=Segment.EndP;
            end;
         end else if (EndP.Edge=Segment.EndP.Edge) then
         begin
            AddEdge:=DistPP3D(EndP.Point,Segment.EndP.Point)<1e-4;
            if AddEdge then
            begin
               Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.EndP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints-1];
               Spline.Add(Segment.StartP.Point);
               Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.StartP.Knuckle;
               EndP:=Segment.StartP;
            end;
         end else if (StartP.Edge=Segment.StartP.Edge) then
         begin
            AddEdge:=DistPP3D(StartP.Point,Segment.StartP.Point)<1e-4;
            if AddEdge then
            begin
               Spline.Knuckle[0]:=Segment.StartP.Knuckle or Spline.Knuckle[0];
               Spline.Insert(0,Segment.EndP.Point);
               Spline.Knuckle[0]:=Segment.EndP.Knuckle;
               StartP:=Segment.EndP;
            end;
         end else if (StartP.Edge=Segment.EndP.Edge) then
         begin
            AddEdge:=DistPP3D(StartP.Point,Segment.EndP.Point)<1e-4;
            if AddEdge then
            begin
               Spline.Knuckle[0]:=Segment.EndP.Knuckle or Spline.Knuckle[0];
               Spline.Insert(0,Segment.StartP.Point);
               Spline.Knuckle[0]:=Segment.StartP.Knuckle;
               StartP:=Segment.StartP;
            end;
         end else if Segment.StartP.Edge=Segment.EndP.Edge then
         begin
            // special case, edge lies entirely in plane
            // perform more extensive test to check whether the two edges
            // are possibly connected
            if (StartP.Edge.FStartpoint.FEdges.IndexOf(Segment.StartP.Edge)<>-1) or
               (StartP.Edge.FEndpoint.FEdges.IndexOf(Segment.StartP.Edge)<>-1) or
               (EndP.Edge.FStartpoint.FEdges.IndexOf(Segment.StartP.Edge)<>-1) or
               (EndP.Edge.FEndpoint.FEdges.IndexOf(Segment.StartP.Edge)<>-1) then
            begin
               AddEdge:=DistPP3D(EndP.Point,Segment.StartP.Point)<1e-4;
               if AddEdge then
               begin
                  Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.StartP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints-1];
                  Spline.Add(Segment.EndP.Point);
                  Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.EndP.Knuckle;
                  EndP:=Segment.EndP;
               end else
               begin
                  AddEdge:=DistPP3D(EndP.Point,Segment.EndP.Point)<1e-4;
                  if AddEdge then
                  begin
                     Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.EndP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints-1];
                     Spline.Add(Segment.StartP.Point);
                     Spline.Knuckle[Spline.NumberOfPoints-1]:=Segment.StartP.Knuckle;
                     EndP:=Segment.StartP;
                  end else
                  begin
                     AddEdge:=DistPP3D(StartP.Point,Segment.StartP.Point)<1e-4;
                     if AddEdge then
                     begin
                        Spline.Knuckle[0]:=Segment.StartP.Knuckle or Spline.Knuckle[0];
                        Spline.Insert(0,Segment.EndP.Point);
                        Spline.Knuckle[0]:=Segment.EndP.Knuckle;
                        StartP:=Segment.EndP;
                     end else
                     begin
                        AddEdge:=DistPP3D(StartP.Point,Segment.EndP.Point)<1e-4;
                        if AddEdge then
                        begin
                           Spline.Knuckle[0]:=Segment.EndP.Knuckle or Spline.Knuckle[0];
                           Spline.Insert(0,Segment.StartP.Point);
                           Spline.Knuckle[0]:=Segment.StartP.Knuckle;
                           StartP:=Segment.StartP;
                        end;
                     end;
                  end;
               end;
            end;
         end;

         if AddEdge then
         begin
            Move(Segments[J],Segments[J-1],(NSegments-J)*SizeOf(TSegment));
            dec(NSegments);
            J:=1;
            AddEdge:=false;
         end else inc(J);
      end;
      if not AddEdge then
      begin
         Spline:=nil;
      end;
   end;
   if Destination.Count>1 then
   begin
      Destination.Capacity:=Destination.Count;
      JoinSplineSegments(0.01,False,Destination);
      for I:=Destination.Count downto 1 do
      begin
         // Remove tiny fragments of very small length
         Spline:=Destination[I-1];
         if Spline.NumberOfPoints>1 then
         begin
            Parameter:=SquaredDistPP(Spline.Min,Spline.Max);
            if Parameter<1e-3 then
            begin
               Spline.Destroy;
               Destination.Delete(I-1);
            end;
         end;
      end;
   end;
   
   Edges.Destroy;
end;{TFreeSubdivisionSurface.CalculateIntersections}

constructor TFreeSubdivisionSurface.Create;
begin
  inherited Create;
   FControlPoints:=TFasterList.Create;
   FControlEdges:=TFasterList.Create;
   FControlFaces:=TFasterList.Create;
   FPoints:=TFasterList.Create;
   FEdges:=TFasterList.Create;
   FLayers:=TFasterList.Create;
   FControlCurves:=TFasterList.Create;
   FSelectedControlPoints:=TFasterList.Create;
   FSelectedControlEdges:=TFasterList.Create;
   FSelectedControlFaces:=TFasterList.Create;
   FSelectedControlCurves:=TFasterList.Create;
   FControlPointSize:=4;
   FEdgeColor:=clDkGray;
   FSelectedcolor:=clYellow;
   FCreasePointColor:=clLime;
   FRegularPointColor:=clSilver;
   FCornerPointColor:=clAqua;
   FDartPointColor:=$00C08000;
   FLeakColor:=clRed;
   FLayerColor:=clAqua;
   FCreaseColor:=clBlue;
   FCreaseEdgeColor:=clRed;
   FNormalColor:=clSilver;
   FCurvatureColor:=clWhite;
   FShowCurvature:=True;
   FCurvatureScale:=0.25;
   FShowControlCurves:=True;
   FControlCurveColor:=clRed;
   FSubdivisionMode:=fmQuadTriangle;
   FZebraColor:=clBlack;
end;{TFreeSubdivisionSurface.Create}

destructor TFreeSubdivisionSurface.Destroy;
var I : Integer;
begin
   Clear;
   // Make sure to also destroy the default layer (layer 0)
   for I:=1 to NumberOfLayers do Layer[I-1].Destroy;
   FLayers.Clear;
   FControlFaces.Free;
   FControlEdges.Free;
   FControlPoints.Free;
   FEdges.Free;
   FPoints.Free;
   FLayers.Free;
   FControlCurves.Free;
   FSelectedControlPoints.Free;
   FSelectedControlEdges.Free;
   FSelectedControlFaces.Free;
   FSelectedControlCurves.Free;
   inherited Destroy;
end;{TFreeSubdivisionSurface.Destroy}

procedure TFreeSubdivisionSurface.Draw(Viewport:TFreeViewport);
var I       : Integer;
    Edge    : TFreeSubdivisionEdge;
begin
   if not Build then Rebuild;
   if Viewport.ViewportMode<>vmWireframe then
     begin
        if Viewport.ViewportMode in [vmShadeGauss,vmShadeDevelopable] then
           if not GaussCurvatureCalculated
              then CalculateGaussCurvature;
     end
   else inherited Draw(Viewport);

   for I:=1 to NumberOfLayers do
     Layer[I-1].Draw(Viewport);

   if ShowControlNet then
   begin
      for I:=1 to NumberOfControlEdges do
      begin
         Edge:=ControlEdge[I-1];
         Edge.Draw(False,Viewport);
      end;
      for I:=1 to NumberOfControlPoints do
         if ControlPoint[I-1].Visible then ControlPoint[I-1].Draw(Viewport);
   end;
   for I:=1 to NumberOfControlCurves do if ControlCurve[I-1].Visible
      then ControlCurve[I-1].Draw(Viewport);
end;{TFreeSubdivisionSurface.Draw}

function TFreeSubdivisionSurface.EdgeExists(P1,P2:TFreeSubdivisionPoint):TFreeSubdivisionEdge;
var I    : Integer;
    Edge : TFreeSubdivisionEdge;
begin
   Result:=nil;
   // If the edge exists then it must exist
   // in both the points, therefore only the point
   // with the smallest number of edges has to be checked
   if P1.FEdges.Count<=P2.FEdges.Count then
   begin
      for I:=1 to P1.FEdges.Count do
      begin
         Edge:=P1.FEdges[I-1];
         if ((Edge.FStartpoint=P1) and (Edge.FEndpoint=P2)) or
            ((Edge.FStartpoint=P2) and (Edge.FEndpoint=P1)) then
         begin
            Result:=Edge;
            exit;
         end;
      end;
   end else for I:=1 to P2.FEdges.Count do
   begin
      Edge:=P2.FEdges[I-1];
      if ((Edge.FStartpoint=P1) and (Edge.FEndpoint=P2)) or
         ((Edge.FStartpoint=P2) and (Edge.FEndpoint=P1)) then
      begin
         Result:=Edge;
         exit;
      end;
   end;
end;{TFreeSubdivisionSurface.EdgeExists}

procedure TFreeSubdivisionSurface.ExtractAllEdgeLoops(var Destination:TFasterList);
var SourceList : TFasterList;
    Loop,Points: TFasterList;
    I,Index    : Integer;
    Edge       : TFreeSubdivisionEdge;
    NextEdge   : TFreeSubdivisionEdge;
begin
   SourceList:=TFasterList.Create;
   for I:=1 to Self.FEdges.Count do
   begin
      Edge:=FEdges[I-1];
      if Edge.Crease then
      begin
         SourceList.Add(Edge);
      end;
   end;
   SourceList.Sort;
   while SourceList.Count>0 do
   begin
      Edge:=SourceList[SourceList.Count-1];
      SourceList.Delete(SourceList.Count-1);
      Loop:=TFasterList.Create;
      Loop.Add(Edge);
      // trace edge to back
      repeat
         NextEdge:=Edge.PreviousEdge;
         if NextEdge<>nil then
         begin
            Index:=SourceList.SortedIndexOf(NextEdge);
            if Index<>-1 then
            begin
               Loop.Insert(0,NextEdge);
               SourceList.Delete(index);
               Edge:=NextEdge;
            end else NextEdge:=nil;
         end;
      until NextEdge=nil;
      Edge:=Loop[Loop.Count-1];
      // trace edge to front
      repeat
         NextEdge:=Edge.NextEdge;
         if NextEdge<>nil then
         begin
            Index:=SourceList.SortedIndexOf(NextEdge);
            if Index<>-1 then
            begin
               Loop.Add(NextEdge);
               SourceList.Delete(index);
               Edge:=NextEdge;
            end else NextEdge:=nil;
         end;
      until NextEdge=nil;

      SortEdges(Loop,Points);
      if Points<>nil then Destination.Add(Points);
      Loop.Destroy;
   end;
   SourceList.Destroy;
end;{TFreeSubdivisionSurface.ExtractAllEdgeLoops}

// extracts all points that are used by the faces in the selectedfaces list
// only points completely surrounded by faces in the faces list are extracted
procedure TFreeSubdivisionSurface.ExtractPointsFromFaces(SelectedFaces,Points:TFasterList;var LockedPoints:Integer);
var I,J,K,N : Integer;
    Face    : TFreeSubdivisionface;
    P       : TFreeSubdivisionControlPoint;
    OK      : Boolean;
begin
   Points.Capacity:=4*SelectedFaces.Count;
   SelectedFaces.Sort;
   LockedPoints:=0;
   for I:=1 to SelectedFaces.Count do
   begin
      Face:=SelectedFaces[I-1];
      for J:=1 to Face.NumberOfpoints do
      begin
         P:=Face.Point[J-1] as TFreeSubdivisionControlPoint;
         if Points.SortedIndexOf(P)=-1 then
         begin
            OK:=True;
            for K:=1 to P.NumberOfFaces do
            begin
               N:=SelectedFaces.SortedIndexOf(P.Face[K-1]);
               if N=-1 then
               begin
                  OK:=False;
                  break;
               end;
            end;
            if OK then
            begin
               Points.AddSorted(P);
               if P.Locked then inc(LockedPoints);
            end;
         end;
      end;
   end;
end;{TFreeSubdivisionSurface.ExtractPointsFromFaces}

// Extracts all controlpoints from thee entire selection of faces, edges and points
procedure TFreeSubdivisionSurface.ExtractPointsFromSelection(SelectedPoints:TFasterList;var LockedPoints:Integer);
var I,J     : Integer;
    Face    : TFreeSubdivisionFace;
    Edge    : TFreeSubdivisionEdge;
    P       : TFreeSubdivisionControlPoint;
begin
   SelectedPoints.Capacity:=4*NumberOfSelectedControlfaces+
                            2*NumberOfSelectedControlEdges+
                              NumberOfSelectedControlPoints+1;
   LockedPoints:=0;
   for I:=1 to NumberOfSelectedControlFaces do
   begin
      Face:=SelectedControlface[I-1];
      for J:=1 to Face.NumberOfpoints do
      begin
         P:=Face.Point[J-1] as TFreeSubdivisionControlPoint;
         if SelectedPoints.SortedIndexOf(P)=-1 then SelectedPoints.AddSorted(P);
      end;
   end;
   for I:=1 to NumberOfSelectedControlEdges do
   begin
      Edge:=SelectedControlEdge[I-1];
      P:=Edge.StartPoint as TFreeSubdivisionControlPoint;
      if SelectedPoints.SortedIndexOf(P)=-1 then SelectedPoints.AddSorted(P);
      P:=Edge.EndPoint as TFreeSubdivisionControlPoint;
      if SelectedPoints.SortedIndexOf(P)=-1 then SelectedPoints.AddSorted(P);
   end;
   for I:=1 to NumberOfSelectedControlPoints do
   begin
      P:=SelectedControlPoint[I-1];
      if SelectedPoints.SortedIndexOf(P)=-1 then SelectedPoints.AddSorted(P);
   end;
   // count number of locked points
   for I:=1 to SelectedPoints.Count do
   begin
      P:=SelectedPoints[I-1];
      if P.Locked then inc(LockedPoints);
   end;

end;{TFreeSubdivisionSurface.ExtractPointsFromSelection}

procedure TFreeSubdivisionSurface.ImportFEFFile(Strings:TStringList;var LineNr:Integer);
var Str        : string;
    I,J,N,Np   : Integer;
    Index      : Integer;
    Point,P1,P2: TFreeSubdivisionControlPoint;
    Edge       : TFreeSubdivisionControlEdge;
    Face       : TFreeSubdivisionControlFace;
    Layer      : TFreeSubdivisionLayer;

   function NewPoint(P:T3DCoordinate):TFreeSubdivisionControlPoint;
   begin
      Result:=TFreeSubdivisionControlPoint.Create(self);
      Result.FCoordinate:=P;
      FControlPoints.Add(Result);
   end;{NewPoint}

begin
   // Read layer information
   inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      if I>NumberOfLayers then Layer:=self.AddNewLayer
                          else Layer:=self.Layer[I-1];
      inc(LineNr);
      Layer.FDescription:=Strings[LineNr];;
      inc(LineNr);
      Str:=Strings[LineNr];
      Layer.FLayerID:=ReadIntFromstr(LineNr,Str);
      if Layer.FLayerID>FLastusedLayerID then FLastusedLayerID:=Layer.FLayerID;
      Layer.FColor:=ReadIntFromstr(LineNr,Str);
      Layer.FVisible:=ReadBoolFromStr(LineNr,Str);
      Layer.FDevelopable:=ReadBoolFromStr(LineNr,Str);
      Layer.FSymmetric:=ReadBoolFromStr(LineNr,Str);
      Layer.FSymmetric:=True;
      Layer.FUseForIntersections:=ReadBoolFromStr(LineNr,Str);
      Layer.FUseInHydrostatics:=ReadBoolFromStr(LineNr,Str);
      Layer.FShowInLinesplan:=ReadBoolFromStr(LineNr,Str);
      Layer.FMaterialDensity:=ReadFloatFromStr(LineNr,Str);
      Layer.FThickness:=ReadFloatFromStr(LineNr,Str);
   end;
   if Assigned(FOnChangeLayerData) then FOnChangeLayerData(self);

   Inc(LineNr);
   Str:=Strings[LineNr];
   // Read controlpoints
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Point:=TFreeSubdivisionControlPoint.Create(self);
      FControlPoints.Add(Point);
      Point.LoadFromStream(LineNr,Strings);
   end;

   // Read controlEdges
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Edge:=TFreeSubdivisionControlEdge.Create(self);
      Edge.FControlEdge:=True;
      FControlEdges.Add(Edge);
      Edge.LoadFromStream(LineNr,Strings);
   end;

   // Read controlFaces
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Face:=TFreeSubdivisionControlFace.Create(self);
      FControlFaces.Add(Face);
      Inc(LineNr);
      Str:=Strings[LineNr];
      Np:=ReadIntFromStr(LineNr,Str);
      for J:=1 to Np do
      begin
         Index:=ReadIntFromStr(LineNr,Str);
         // Attach controlface to controlpoints
         Face.AddPoint(ControlPoint[Index]);
      end;
      // Attach controlface to the already existing control edges
      P1:=Face.FPoints[Face.FPoints.Count-1];
      for J:=1 to Face.FPoints.Count do
      begin
         P2:=Face.FPoints[J-1];
         Edge:=EdgeExists(P1,P2) as TFreeSubdivisionControlEdge;
         if Edge<>nil then Edge.AddFace(Face)
                      else MessageDlg(Userstring(201)+'!',mtError,[mbOk],0);
         P1:=P2;
      end;
      // Read Layerindex
      Index:=ReadIntFromStr(LineNr,Str);
      While Index>NumberOfLayers-1 do
      begin
         AddNewLayer;
      end;
      Layer:=FLayers[Index];
      Layer.AddControlFace(Face);
   end;
   Build:=False;
   FInitialized:=True;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
   if assigned(FOnChangeActiveLayer) then FOnChangeActiveLayer(self,self.Layer[0]);
end;{TFreeSubdivisionSurface.ImportFsile}

procedure TFreeSubdivisionSurface.ImportGrid(Points:TFreeCoordinateGrid;Cols,Rows:Integer;Layer:TFreesubdivisionLayer);
var Grid       : TFreeSubdivisionGrid;
    FacePoints : TFasterList;
    Edge       : TFreeSubdivisionEdge;
    I,J        : Integer;
begin
   Setlength(Grid,Rows);
   for I:=1 to Rows do
   begin
      Setlength(Grid[I-1],Cols);
      for J:=1 to Cols do Grid[I-1][J-1]:=AddControlPoint(Points[I-1][J-1]);
   end;

   FacePoints:=TFasterList.Create;
   for I:=2 to Rows do
   begin
      for J:=2 to Cols do
      begin
         FacePoints.Clear;
         if FacePoints.IndexOf(Grid[I-1,J-1])=-1 then FacePoints.Add(Grid[I-1,J-1]);
         if FacePoints.IndexOf(Grid[I-1,J-2])=-1 then FacePoints.Add(Grid[I-1,J-2]);
         if FacePoints.IndexOf(Grid[I-2,J-2])=-1 then FacePoints.Add(Grid[I-2,J-2]);
         if FacePoints.IndexOf(Grid[I-2,J-1])=-1 then FacePoints.Add(Grid[I-2,J-1]);
         if FacePoints.Count>=3 then if Layer<>nil then AddControlFace(FacePoints,True,Layer)
                                                   else AddControlFace(FacePoints,True);
      end;
   end;
   // set crease edges
   for I:=2 to Cols do
   begin
      Edge:=EdgeExists(Grid[0][I-2],Grid[0][I-1]);
      if Edge<>nil then Edge.Crease:=true;
      Edge:=EdgeExists(Grid[Rows-1][I-2],Grid[Rows-1][I-1]);
      if Edge<>nil then Edge.Crease:=true;
   end;
   for I:=2 to Rows do
   begin
      Edge:=EdgeExists(Grid[I-2][0],Grid[I-1][0]);
      if Edge<>nil then Edge.Crease:=true;
      Edge:=EdgeExists(Grid[I-2][Cols-1],Grid[I-1][Cols-1]);
      if Edge<>nil then Edge.Crease:=true;
   end;
   // set cornerpoints
   for I:=1 to Rows do
      for J:=1 to Cols do if Grid[I-1][J-1].NumberOfFaces<2 then
      Grid[I-1][J-1].VertexType:=svCorner;

   FacePoints.Destroy;
end;{TFreeSubdivisionSurface.ImportGrid}

procedure TFreeSubdivisionSurface.Initialize(PointStartIndex,EdgeStartIndex,FaceStartIndex:Integer);
var I : Integer;
    Edge:TFreeSubdivisionEdge;
begin
   // Identify all border edges
   if EdgeStartIndex<=NumberOfControlEdges then for I:=EdgeStartIndex to NumberOfControlEdges do
   begin
      Edge:=ControlEdge[I-1];
      if Edge.NumberOfFaces=0 then
      begin
         Edge.StartPoint.DeleteEdge(Edge);
         Edge.EndPoint.DeleteEdge(Edge);
         Edge.Crease:=false;
      end else if Edge.NumberOfFaces<>2 then Edge.Crease:=true;
   end;

   //for I:=1 to NumberOfControlEdges do if ControlEdge[I-1].NumberOfFaces<>2 then ControlEdge[I-1].Crease:=True;
   for I:=PointStartIndex to NumberOfControlPoints do if ControlPoint[I-1].NumberOfFaces<2 then ControlPoint[I-1].VertexType:=svCorner;
   FInitialized:=True;
end;{TFreeSubdivisionSurface.Initialize}

function TFreeSubdivisionSurface.IntersectPlane(Plane:T3DPlane;HydrostaticsLayersOnly:Boolean;List:TFasterList):Boolean;
var I,J              : Integer;
    CtrlFace         : TFreesubdivisionControlFace;
    IntersectedFaces : TFasterList;
    Min,Max          : T3DCoordinate;
    UseLayer         : Boolean;
    Layer            : TFreeSubdivisionLayer;
begin
   Result:=False;
   if not build then Rebuild;
   if not PlaneIntersectsBox(self.Min,self.Max,Plane) then exit;
   IntersectedFaces:=TFasterList.Create;
   for I:=1 to NumberOfLayers do
   begin
      Layer:=self.Layer[I-1];
      if HydrostaticsLayersOnly then UseLayer:=Layer.UseInHydrostatics
                                else UseLayer:=Layer.UseForIntersections;
      if UseLayer then for J:=1 to Layer.Count do
      begin
         CtrlFace:=Layer.Items[J-1];
         Min:=CtrlFace.Min;
         Max:=CtrlFace.MAx;
         if PlaneIntersectsBox(Min,Max,Plane) then IntersectedFaces.Add(CtrlFace);
      end;
   end;
   CalculateIntersections(Plane,IntersectedFaces,List);
   IntersectedFaces.Destroy;
   Result:=List.Count>0;
end;{TFreeSubdivisionSurface.IntersectPlane}

// inserts points on edges (visible edges only) that intersect the input plane
procedure TFreeSubdivisionSurface.InsertPlane(Plane:T3DPlane;AddCurves:Boolean);
var I,J,K      : Integer;
    S1,S2,T    : TFloatType;
    P          : T3DCoordinate;
    face       : TFreeSubdivisionControlface;
    Edge       : TFreeSubdivisionControlEdge;
    NewP       : TFreeSubdivisionControlpoint;
    P1,P2      : TFreeSubdivisionControlPoint;
    Curve      : TFreeSubdivisionControlCurve;
    Points     : TFasterList;
    Edges      : TFasterList;
    SortedEdges: TFasterList;
    Inserted   : Boolean;
begin
   I:=1;
   Points:=TFasterList.Create;
   while I<=NumberOfControlEdges do
   begin
      Edge:=self.ControlEdge[I-1];
      if Edge.Visible then
      begin
         S1:=Plane.a*Edge.Startpoint.Coordinate.x+Plane.b*Edge.Startpoint.Coordinate.y+Plane.c*Edge.Startpoint.Coordinate.z+Plane.d;
         S2:=Plane.a*Edge.Endpoint.Coordinate.x+Plane.b*Edge.Endpoint.Coordinate.y+Plane.c*Edge.Endpoint.Coordinate.z+Plane.d;
         if ((S1<-1e-5) and (S2>1e-5)) or ((S1>1e-5) and (S2<-1e-5)) then
         begin
            if S1=S2 then
            begin
               T:=0.5;
            end else T:=-s1/(s2-s1);
            P.X:=Edge.Startpoint.Coordinate.X+T*(Edge.Endpoint.Coordinate.X-Edge.Startpoint.Coordinate.X);
            P.Y:=Edge.Startpoint.Coordinate.Y+T*(Edge.Endpoint.Coordinate.Y-Edge.Startpoint.Coordinate.Y);
            P.Z:=Edge.Startpoint.Coordinate.Z+T*(Edge.Endpoint.Coordinate.Z-Edge.Startpoint.Coordinate.Z);
            NewP:=Edge.InsertControlPoint(P);

            Points.Add(NewP);
         end;
      end;
      inc(I);
   end;
   if Points.Count>0 then
   begin

      // Try to find multiple points belonging to the same face and insert an edge
      I:=1;
      Points.Sort;

      Edges:=TFasterList.Create;
      while I<=Points.Count do
      begin
         P1:=Points[I-1];
         //writeln('InsertPlane: Point[',I,'].NumberOfFaces=',P1.NumberOfFaces);
         J:=1;
         while J<=P1.NumberOfFaces do
         begin
            Face:=P1.Face[J-1] as TFreeSubdivisionControlface;
            K:=1;
            Inserted:=False;
            while (K<=Face.NumberOfpoints) and (Not Inserted) do
            begin
               //writeln('InsertPlane:  K=',K);
               P2:=Face.Point[K-1] as TFreeSubdivisionControlPoint;
               if (P1<>P2) and (Points.SortedIndexOf(P2)<>-1) then
               begin
                  // this is also a new point, first check if an edge already exists between P1 and P2
                  if EdgeExists(P1,P2)=nil then
                  begin
                     Inserted:=True;
                     Edge:=Face.InsertEdge(P1,P2);
                     Edge.Selected:=True;    //selecting causes loosing Face.FPoints !
                     Edges.Add(Edge);
                     if not Assigned(Face)
                       then break;
                  end;
               end;
               inc(K);
            end;
            DelayedDestroyList.DestroyAll;
            if not Inserted then inc(J);
         end;
         inc(I);
      end;
      if AddCurves then
      begin
         Points.Destroy;
         SortedEdges:=TFasterList.Create;
         IsolateEdges(Edges,SortedEdges);
         for I:=1 to SortedEdges.Count do
         begin
            Points:=SortedEdges[I-1];
            if Points.Count>1 then
            begin
               Curve:=TFreeSubdivisionControlCurve.Create(self);
               AddControlCurve(Curve);
               for J:=1 to Points.Count do
               begin
                  P1:=Points[J-1];
                  Curve.AddPoint(P1);
                  if J>1 then
                  begin
                     Edge:=Curve.Owner.EdgeExists(Curve.ControlPoint[J-2],Curve.ControlPoint[J-1]) as TFreeSubdivisionControlEdge;
                     if Edge<>nil then
                     begin
                        Edge.Curve:=Curve;
                     end;
                  end;
               end;
            end;
         Points.Destroy;
         end;
         SortedEdges.Destroy;
      end else Points.Destroy;
      Edges.Destroy;
      Build:=False;
   end else Points.Destroy;
end;{TFreeSubdivisionSurface.InsertPlane}

procedure TFreeSubdivisionSurface.IsolateEdges(Source,destination:TFasterList);
var I        : Integer;
    Edge     : TFreesubdivisionControlEdge;
    Edge2    : TFreesubdivisionControlEdge;
    TmpEdges : TFasterList;
    TmpPts   : TFasterList;
    Findmore : Boolean;
begin
   // Try to isolate individual (closed) sets of edges
   TmpEdges:=TFasterList.Create;
   while Source.Count>0 do
   begin
      Edge:=Source[0];
      Source.Delete(0);
      FindMore:=True;
      TmpEdges.Clear;
      TmpEdges.Add(Edge);
      while (Source.Count>0) and (FindMore) do
      begin
         FindMore:=False;
         for I:=1 to Source.Count do
         begin
            Edge2:=Source[I-1];
            // compare at start
            Edge:=TmpEdges[0];
            if (Edge2.FStartpoint=Edge.FStartpoint) or (Edge2.FStartpoint=Edge.FEndpoint) or
               (Edge2.FEndpoint=Edge.FStartpoint) or (Edge2.FEndpoint=Edge.FEndpoint) then
            begin
               TmpEdges.Insert(0,Edge2);
               Source.Delete(I-1);
               Findmore:=true;
               break;
            end else
            begin
               Edge:=TmpEdges[Tmpedges.Count-1];
               if (Edge2.FStartpoint=Edge.FStartpoint) or (Edge2.FStartpoint=Edge.FEndpoint) or
                  (Edge2.FEndpoint=Edge.FStartpoint) or (Edge2.FEndpoint=Edge.FEndpoint) then
               begin
                  TmpEdges.Add(Edge2);
                  Source.Delete(I-1);
                  Findmore:=true;
                  break;
               end;
            end;
         end;
      end;
      if TmpEdges.Count>0 then
      begin
         // Sort all found edges in correct order
         SortEdges(TmpEdges,TmpPts);
         if TmpPts<>nil then Destination.Add(TmpPts);
      end;
   end;
   TmpEdges.Destroy;
end;{TFreeSubdivisionSurface.IsolateEdges}

procedure TFreeSubdivisionSurface.LoadBinary(Source:TFreeFileBuffer);
var I,N     : Integer;
    Point   : TFreeSubdivisionControlPoint;
    Edge    : TFreeSubdivisionControlEdge;
    Face    : TFreeSubdivisionControlFace;
    Curve   : TFreeSubdivisionControlCurve;
    Layer   : TFreeSubdivisionLayer;
begin
   // First load layerdata
   Source.Load(N);
   if N<>0 then
   begin
      // Delete current layers and load new ones
      for I:=1 to NumberOfLayers do self.Layer[I-1].Destroy;
      FLayers.Clear;
      FLayers.Capacity:=N;
      for I:=1 to N do
      begin
         Layer:=AddNewLayer;
         Layer.LoadBinary(Source);
      end;
   end else
   begin
      // No layers in the file, so keep the current default one
   end;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
   // Read index of active layer
   Source.Load(N);
   ActiveLayer:=self.Layer[N];
   if assigned(FOnChangeActiveLayer) then FOnChangeActiveLayer(self,self.Layer[0]);
   // Read controlpoints
   Source.Load(N);
   FControlPoints.Capacity:=N;
   for I:=1 to N do
   begin
      Point:=TFreeSubdivisionControlPoint.Create(self);
      Point.LoadBinary(Source);
      FControlPoints.Add(Point);
   end;
   // Read controlEdges
   Source.Load(N);
   FControlEdges.Capacity:=N;
   for I:=1 to N do
   begin
      Edge:=TFreeSubdivisionControlEdge.Create(self);
      Edge.FControlEdge:=True;
      Edge.LoadBinary(Source);
      FControlEdges.Add(Edge);
   end;
   if Source.Version>=fv195 then
   begin
      // Load controlcurves
      Source.Load(N);
      FControlCurves.Capacity:=N;
      for I:=1 to N do
      begin
         Curve:=TFreeSubdivisionControlCurve.Create(self);
         Curve.LoadBinary(Source);
         FControlCurves.Add(Curve);
      end;
   end;

   // Read controlFaces
   Source.Load(N);
   FControlFaces.Capacity:=N;
   for I:=1 to N do
   begin
      Face:=TFreeSubdivisionControlFace.Create(self);
      Face.LoadBinary(Source);
      FControlFaces.Add(Face);
   end;
   Build:=False;
   FInitialized:=True;
   if assigned(FOnChangeLayerData)
      then FOnChangeLayerData(self);
   if assigned(FOnChangeActiveLayer)
      then FOnChangeActiveLayer(self,self.Layer[0]);
end;{TFreeSubdivisionSurface.LoadBinary}

procedure TFreeSubdivisionSurface.LoadFromStream(var LineNr:Integer;Strings:TStringList);
var Str    : string;
    I,N     : Integer;
    Point   : TFreeSubdivisionControlPoint;
    Edge    : TFreeSubdivisionControlEdge;
    Face    : TFreeSubdivisionControlFace;
    Layer   : TFreeSubdivisionLayer;
begin
   // First read layerdata
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   if N<>0 then
   begin
      // Delete current layers and load new ones
      for I:=1 to NumberOfLayers do self.Layer[I-1].Destroy;
      FLayers.Clear;
      FLayers.Capacity:=N;
      for I:=1 to N do
      begin
         Layer:=AddNewLayer;
         Layer.LoadFromStream(LineNr,Strings);
      end;
   end else
   begin
      // No layers in the file, so keep the current default one
   end;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
   // Read index of active layer
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   ActiveLayer:=self.Layer[N];
   if assigned(FOnChangeActiveLayer) then FOnChangeActiveLayer(self,self.Layer[0]);

   Inc(LineNr);
   Str:=Strings[LineNr];
   // Read controlpoints
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Point:=TFreeSubdivisionControlPoint.Create(self);
      FControlPoints.Add(Point);
      Point.LoadFromStream(LineNr,Strings);
   end;

   // Read controlEdges
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Edge:=TFreeSubdivisionControlEdge.Create(self);
      Edge.FControlEdge:=True;
      FControlEdges.Add(Edge);
      Edge.LoadFromStream(LineNr,Strings);
   end;
   // Read controlFaces
   Inc(LineNr);
   Str:=Strings[LineNr];
   N:=ReadIntFromStr(LineNr,Str);
   for I:=1 to N do
   begin
      Face:=TFreeSubdivisionControlFace.Create(self);
      FControlFaces.Add(Face);
      Face.LoadFromStream(LineNr,Strings);
   end;
   Build:=False;
   FInitialized:=True;
   if assigned(FOnChangeLayerData) then FOnChangeLayerData(self);
   if assigned(FOnChangeActiveLayer) then FOnChangeActiveLayer(self,self.Layer[0]);
end;{TFreeSubdivisionSurface.LoadFromStream}

procedure TFreeSubdivisionSurface.LoadVRMLFile(Filename:string);
var VRMLList      : TVRMLList;
    I,J,K,N       : Integer;
    Index         : Integer;
    Data          : TFasterList;
    CoordInfo     : TVRMLCoordinate3;
    FaceInfo      : TVRMLIndexedFaceSet;
    Points        : TFasterList;
    Face          : TIntArray;
    Layer         : TFreeSubdivisionLayer;
    FacePoints    : TFasterList;
    AddedCtrlPts  : TFasterList;
    CtrPoint      : TFreeSubdivisionControlpoint;
begin
   if FileExists(Filename) then
   begin
      VRMLList:=TVRMLList.Create;
      VRMLList.LoadFromFile(Filename);
      Data:=VRMLList.ExtractFaceSetData;
      if Data<>nil then
      begin
         Clear;
         try

            AddedCtrlPts:=TFasterList.Create;
            AddedCtrlPts.Capacity:=Data.Count;
            // Assemble coordinate sets
            for I:=1 to Data.Count do
            begin
               FaceInfo:=Data[I-1];
               if AddedCtrlPts.SortedIndexOf(FaceInfo.Coordinates)=-1 then AddedCtrlPts.AddSorted(FaceInfo.Coordinates);
            end;


            // now add actual controlPoints
            for I:=1 to AddedCtrlPts.Count do
            begin
               CoordInfo:=AddedCtrlPts[I-1];
               Points:=TFasterList.Create;
               Points.Capacity:=CoordInfo.Count;
               AddedCtrlPts.Objects[I-1]:=points;
               for J:=1 to CoordInfo.Count do
               begin
                  Points.Add(AddControlPoint(CoordInfo.Point[J-1]));
               end;
            end;


            // Add controlfaces
            FacePoints:=TFasterList.Create;
            for I:=1 to Data.Count do
            begin
               FaceInfo:=Data[I-1];
               Index:=AddedCtrlPts.SortedIndexOf(FaceInfo.Coordinates);
               if Index<>-1 then
               begin
                  Points:=AddedCtrlPts.Objects[Index];
                  Layer:=AddNewLayer;
                  for J:=1 to FaceInfo.Count do
                  begin
                     Face:=FaceInfo.Face[J-1];
                     if Face<>nil then
                     begin
                        N:=length(Face);
                        FacePoints.Clear;
                        for K:=1 to N do
                        begin
                           Index:=face[K-1];
                           if (Index>=0) and (Index<Points.Count) then
                           begin
                              CtrPoint:=Points[index];
                              if FacePoints.IndexOf(CtrPoint)=-1 then FacePoints.Add(CtrPoint);
                           end;
                        end;
                        if FacePoints.Count>2 then AddControlFaceN(FacePoints,True,Layer);
                     end;
                  end;
               end;
            end;
            FacePoints.Destroy;

            for I:=1 to AddedCtrlPts.Count do
            begin
               Points:=AddedCtrlPts.Objects[I-1];
               Points.Destroy;
            end;
            AddedCtrlPts.Destroy;


            // delete empty layers
            for I:=NumberOfLayers downto 1 do
            begin
               if (self.Layer[I-1].Count=0) and (NumberOfLayers>1) then self.Layer[I-1].Delete;
            end;
            ActiveLayer:=self.Layer[NumberOfLayers-1];
         finally
            build:=false;
            Data.Destroy;
         end;
      end else MessageDlg(Userstring(203)+'.',mtError,[mbOk],0);
      VRMLList.Destroy;
   end;
end;{TFreeSubdivisionSurface.LoadVRMLFile}

// Check if a controlpoint still exists and is not deleted
function TFreeSubdivisionSurface.PointExists(P:TFreeSubdivisionControlPoint):Boolean;
begin
   Result:=FControlPoints.IndexOf(P)<>-1;
end;{TFreeSubdivisionSurface.PointExists}

procedure TFreeSubdivisionSurface.Rebuild;
var I,J     : Integer;
    Curve   : TFreeSubdivisionControlCurve;
    Edge1   : TFreeSubdivisionEdge;
    Edge2   : TFreeSubdivisionEdge;
    Point   : TFreeSubdivisionPoint;
begin
   if not FInitialized then Initialize(1,1,1);
   if self.NumberOfControlFaces>0 then
   begin
      for I:=1 to NumberOfControlCurves do
      begin
         Curve:=ControlCurve[I-1];
         if FCurrentSubdivisionLevel=0 then
         begin
            Curve.FBuild:=False;
            Curve.FSubdividedPoints.Clear;
            Curve.FSubdividedPoints.AddList(Curve.FControlPoints);
         end;
      end;
      FBuild:=True;
      while (FCurrentSubdivisionLevel<FDesiredSubdivisionlevel) and (FControlFaces.Count>0) do
      begin
         Subdivide;
      end;
      for I:=1 to NumberOfControlfaces do
      begin
         ControlFace[I-1].CalcExtents;
         if I=1 then
         begin
            FMin:=Controlface[I-1].FMin;
            FMax:=Controlface[I-1].FMax;
         end else
         begin
            MinMax(Controlface[I-1].FMin,FMin,FMax);
            MinMax(Controlface[I-1].FMax,FMin,FMax);
         end;
      end;
      for I:=1 to NumberOfControlCurves do
      begin
         Curve:=ControlCurve[I-1];
         Curve.FCurve.Clear;
         Curve.FCurve.Capacity:=Curve.FSubdividedPoints.Count;
         for J:=1 to Curve.FSubdividedPoints.Count do
         begin
            Point:=Curve.FSubdividedPoints[J-1];
            Curve.FCurve.Add(Point.Coordinate);
            if (J>1) and (J<Curve.FSubdividedPoints.Count) then
            begin
               if Point.VertexType=svCorner then Curve.FCurve.Knuckle[J-1]:=True else
               begin
                  Edge1:=EdgeExists(Curve.FSubdividedPoints[J-2],Curve.FSubdividedPoints[J-1]);
                  Edge2:=EdgeExists(Curve.FSubdividedPoints[J-1],Curve.FSubdividedPoints[J]);
                  if (Edge1=nil) or (Edge2=nil) then
                  begin
                     //if (Edge1.Crease=False) and (Edge2.Crease=False) then Curve.FCurve.Knuckle[J-1]:=Point.VertexType=svCrease;
                  end else if (Edge1.Crease=False) and (Edge2.Crease=False) then Curve.FCurve.Knuckle[J-1]:=Point.VertexType=svCrease;
               end;
            end;
            Curve.FBuild:=true;
         end;
      end;
   end else if NumberOfControlPoints>0 then
   begin
      for I:=1 to NumberOfControlPoints do
      begin
         if I=1 then
         begin
            FMin:=ControlPoint[I-1].Coordinate;
            FMax:=FMin;
         end else MinMax(ControlPoint[I-1].Coordinate,FMin,FMax);
      end;
   end else
   begin
      FMin:=ZERO;
      FMax.X:=1.0;
      FMax.Y:=1.0;
      FMax.Z:=1.0;
   end;
end;{TFreeSubdivisionSurface.Rebuild}

procedure TFreeSubdivisionSurface.SaveBinary(Destination:TFreeFileBuffer);
var I     : Integer;
begin
   // First save layerdata
   Destination.Add(NumberOfLayers);
   for I:=1 to NumberOfLayers do Layer[I-1].SaveBinary(Destination);
   // Save index of active layer
   Destination.Add(ActiveLayer.LayerIndex);
   // first sort controlpoints for faster acces of function (Indexof())
   FControlPoints.Sort;
   Destination.Add(NumberOfControlPoints);
   for I:=1 to NumberOfControlPoints do ControlPoint[I-1].SaveBinary(Destination);
   Destination.Add(NumberOfControlEdges);
   for I:=1 to NumberOfControlEdges do ControlEdge[I-1].SaveBinary(Destination);
   if Destination.Version>=fv195 then
   begin
      Destination.Add(NumberOfControlCurves);
      for I:=1 to NumberOfControlCurves do ControlCurve[I-1].SaveBinary(Destination);
   end;
   Destination.Add(NumberOfControlFaces);
   for I:=1 to NumberOfControlFaces do ControlFace[I-1].SaveBinary(Destination);
end;{TFreeSubdivisionSurface.SaveBinary}

procedure TFreeSubdivisionSurface.SaveToStream(Strings:TStringlist);
var I     : Integer;
begin
   // First save layerdata
   Strings.Add(IntToStr(NumberOfLayers));
   for I:=1 to NumberOfLayers do Layer[I-1].SaveToStream(Strings);
   // Save index of active layer
   Strings.Add(IntToStr(ActiveLayer.LayerIndex));
   // first sort controlpoints for faster acces of function (Indexof())
   FControlPoints.Sort;
   Strings.Add(IntToStr(NumberOfControlPoints));
   for I:=1 to NumberOfControlPoints do ControlPoint[I-1].SaveToStream(Strings);
   Strings.Add(IntToStr(NumberOfControlEdges));
   for I:=1 to NumberOfControlEdges do ControlEdge[I-1].SaveToStream(Strings);
   Strings.Add(IntToStr(NumberOfControlFaces));
   for I:=1 to NumberOfControlFaces do ControlFace[I-1].SaveToStream(Strings);
end;{TFreeSubdivisionSurface.SaveToStream}

procedure TFreeSubdivisionSurface.Selection_Delete;
var I : Integer;
begin
   // First controlcurves, then faces, edges and finally points!
   I:=NumberOfSelectedControlCurves;
   while I>=1 do
   begin
      SelectedControlCurve[I-1].Delete;
      dec(I);
      if I>NumberOfSelectedControlCurves then I:=NumberOfSelectedControlCurves;
   end;

   I:=self.NumberOfSelectedControlFaces;
   while I>=1 do
   begin
      SelectedControlFace[I-1].Delete;
      dec(I);
      if I>NumberOfSelectedControlFaces then I:=NumberOfSelectedControlFaces;
   end;
   I:=self.NumberOfSelectedControlEdges;
   while I>=1 do
   begin
      SelectedControlEdge[I-1].Delete;
      dec(I);
      if I>NumberOfSelectedControlEdges then
      begin
         // Security check ==> if an edge is deleted from one isolated patch (with all edges only attached
         // to the current face), the patch will become degenerate and therefore the patch (and the other
         // edges) will be deleted, and NumberOfSelectedControlEdges may become smaller than index I
         I:=NumberOfSelectedControlEdges;
      end;
   end;
   I:=self.NumberOfSelectedControlPoints;
   while I>=1 do
   begin
      if not SelectedControlPoint[I-1].Locked then SelectedControlPoint[I-1].Delete;
      dec(I);
      if I>NumberOfSelectedControlPoints then
      begin
         // Same as above
         I:=NumberOfSelectedControlPoints;
      end;
   end;
   Build:=False;
end;{TFreeSubdivisionSurface.Selection_Delete}

procedure TFreeSubdivisionSurface.SortEdges(Edges:TFasterList);
var Edge1,Edge2:TFreeSubdivisionEdge;
    J          :Integer;
begin
   if Edges.Count<=1 then exit else
   begin
      Edge1:=Edges[0];
      for J:=2 to Edges.Count do
      begin
         Edge2:=Edges[J-1];
         if J=2 then
         begin
            if (Edge1.FStartPoint=Edge2.FStartPoint) then
            begin
               Edge1.SwapData;
            end else if (Edge1.FStartPoint=Edge2.FEndPoint) then
            begin
               Edge1.SwapData;
               Edge2.SwapData;
            end else if (Edge1.FEndPoint=Edge2.FStartPoint) then
            begin
            end else if (Edge1.FEndPoint=Edge2.FEndPoint) then
            begin
               Edge2.SwapData;
            end;
         end else
         begin
            if (Edge1.FEndPoint=Edge2.FEndPoint) then Edge2.SwapData;

            if (Edge1.FEndPoint=Edge2.FStartPoint) then
            begin
               Edge2.SwapData;
               Edge2.SwapData;
            end;

         end;
         Edge1:=Edge2;
      end;
   end;
end;{TFreeSubdivisionSurface.SortEdges}

procedure TFreeSubdivisionSurface.SortEdges(Edges:TFasterList;var Points:TFasterList);
var I    : Integer;
    Edge : TFreeSubdivisionEdge;
begin
   if Edges.Count>0 then
   begin
      Points:=TFasterList.Create;
      Points.Capacity:=Edges.Count+1;
      SortEdges(Edges);
      for I:=1 to Edges.Count do
      begin
         Edge:=Edges[I-1];
         if I=1 then Points.Add(Edge.StartPoint);
         //if Edge.EndPoint<>Points[0] then Points.Add(Edge.EndPoint);
         Points.Add(Edge.EndPoint);
      end;
   end else Points:=nil;
end;{TFreeSubdivisionSurface.SortEdges}

procedure TFreeSubdivisionSurface.SubDivide;
type TQuadData     = record
                       P:array[1..4] of TFreeSubdivisionPoint;
                       Crease1,Crease2:Boolean;
                     end;
var I,J,Number    : Integer;
    CtrlFace      : TFreeSubdivisionControlFace;
    Edge          : TFreeSubdivisionEdge;
    Point         : TFreeSubdivisionPoint;
//    NewPointList  : TFasterList;
    NewEdgeList   : TFasterList;
    TmpPoints     : array of T3DCoordinate;
    VertexPoints  : TFasterList;
    FacePoints    : TFasterList;
    EdgePoints    : TFasterList;
begin
   if NumberOfControlFaces<1 then exit;
   inc(FCurrentSubdivisionLevel,1);
//   NewPointList:=TFasterList.Create;
//   NewPointlist.Capacity:=2*NumberOfPoints;
   NewEdgeList:=TFasterList.Create;
   I:=Round(Power(2,FCurrentSubdivisionLevel));
   NewEdgelist.Capacity:=NumberOfControlEdges*I;
   Number:=NumberOfFaces;

   // Create the list with new facepoints and a reference to the original face
   FacePoints:=TFasterList.Create;
   // Create the list with new edgepoints and a reference to the original edge
   EdgePoints:=TFasterList.Create;
   // Create the list with new vertexpoints and a reference to the original vertex
   VertexPoints:=TFasterList.Create;

   if Number=0 then
   begin
      FacePoints.Capacity:=NumberOfControlFaces;
      for I:=1 to NumberOfControlFaces do
      begin
         CtrlFace:=ControlFace[I-1];
         FacePoints.AddObject(CtrlFace,CtrlFace.CalculateFacePoint);
      end;
   end else
   begin
      FacePoints.Capacity:=4*NumberOfControlFaces;
      for I:=1 to NumberOfControlFaces do
      begin
         CtrlFace:=ControlFace[I-1];
         for J:=1 to CtrlFace.Childcount do FacePoints.AddObject(CtrlFace.Child[J-1],CtrlFace.Child[J-1].CalculateFacePoint);
         for J:=1 to CtrlFace.Edgecount do EdgePoints.AddObject(CtrlFace.Edge[J-1],CtrlFace.Edge[J-1].CalculateEdgePoint);
      end;
   end;
   // Calculate other edgepoints
   EdgePoints.Capacity:=EdgePoints.Count+NumberOfEdges;
   for I:=1 to NumberOfEdges do EdgePoints.AddObject(self.Edge[I-1],self.Edge[I-1].CalculateEdgePoint);
   // Calculate vertexpoints
   VertexPoints.Capacity:=VertexPoints.Count+NumberOfPoints;
   for I:=1 to NumberOfpoints do VertexPoints.AddObject(Self.Point[I-1],Self.Point[I-1].CalculateVertexPoint);
   // Sort the new points for faster acces
   VertexPoints.Sort;
   EdgePoints.Sort;
   FacePoints.Sort;

   // finally create the refined mesh over the newly create vertexpoints, edgepoints and facepoints
   for I:=1 to NumberOfControlFaces do
   begin
      CtrlFace:=ControlFace[I-1];
      CtrlFace.Subdivide(Self,True,VertexPoints,EdgePoints,FacePoints,nil,NewEdgeList,nil);
   end;

   // cleanup old mesh
   for I:=1 to FEdges.Count do
   begin
      Edge:=FEdges[I-1];
      Edge.Destroy;
   end;
   FEdges.Destroy;
   FEdges:=NewEdgeList;
   for I:=1 to FPoints.Count do
   begin
      Point:=FPoints[I-1];
      Point.Destroy;
   end;
   // Add all new points int the point-list
   FPoints.Clear;
   FPoints.Capacity:=VertexPoints.Count+EdgePoints.Count+FacePoints.Count;
   for I:=1 to VertexPoints.Count do if VertexPoints.Objects[I-1]<>nil then FPoints.Add(VertexPoints.Objects[I-1]);
   for I:=1 to EdgePoints.Count do if EdgePoints.Objects[I-1]<>nil then FPoints.Add(EdgePoints.Objects[I-1]);
   for I:=1 to FacePoints.Count do if FacePoints.Objects[I-1]<>nil then FPoints.Add(FacePoints.Objects[I-1]);
   FPoints.Capacity:=FPoints.Count;
   // Cleanup temp. lists
   VertexPoints.Destroy;
   EdgePoints.Destroy;
   FacePoints.Destroy;
   // perform averaging procedure to smooth the new mesh
   Setlength(TmpPoints,FPoints.Count);
   for I:=1 to FPoints.Count do
   begin
      Point:=FPoints[I-1];
      TmpPoints[I-1]:=Point.Averaging;
   end;
   for I:=1 to FPoints.Count do
   begin
      Point:=FPoints[I-1];
      Point.FCoordinate:=TmpPoints[I-1];
   end;
end;{TFreeSubdivisionSurface.SubDivide}


// -----------------------------------------------------------------------------
constructor TFreeDestroyList.Create;
begin
   inherited Create;
end;

procedure TFreeDestroyList.DestroyAll;
var O: TObject; I: integer;
begin
  while Count>0 do begin
    O := TObject(Items[0]);
    if Assigned(O) then begin
      O.Destroy;
      Delete(0);
    end;
  end;
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


