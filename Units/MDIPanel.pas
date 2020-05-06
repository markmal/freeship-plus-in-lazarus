unit MDIPanel;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Types, TypInfo,
// LCL
LCLStrConsts, LCLType, LCLProc, LCLIntf, LCLVersion, LCLClasses,
LMessages, LResources, GraphType, Graphics, Menus,
ActnList,
ClipBrd, Controls, ImgList, Themes,
// LazUtils
LazFileUtils, LazUTF8, LazLoggerBase,
{$ifndef wince}gettext,{$endif}// remove ifdefs when gettext is fixed and a new fpc is released
Forms, StdCtrls,
ExtCtrls, ComCtrls, Buttons;

type
  TWindowPositionState = (wpsNone, wpsMoving, wpsResizing);
  TWindowResizingSide = (
    wrszsNW, wrszsN, wrszsNE,
    wrszsW, wrszsZ, wrszsE,
    wrszsSW, wrszsS, wrszsSE);

  TCaptionButton = ( // Form title bar items
    cbSystemMenu, // system menu
    cbMinimize,   // minimize button
    cbMaximize,   // maximize button
    cbRestore,   // minimize button
    cbClose
    );
  TCaptionButtons = set of TCaptionButton;

type
  TMDIClientPanel = class(TPanel)
    public
      procedure ActiveDefaultControlChanged(NewControl: TControl); override;
  end;
{$ifdef Use_MouseClickProxy}
  TMouseClickProxy = class
  private
    FDestroyed: boolean;
    FOwner: TControl;
    FOwnerOnClick: TNotifyEvent;
    FProxyOnClick: TNotifyEvent;
  public
    constructor Create(TheOwner: TControl);
    destructor Destroy; override;
    procedure OnClick(Sender: TObject);
    property OwnerOnClick: TNotifyEvent read FOwnerOnClick;
    property ProxyOnClick: TNotifyEvent read FProxyOnClick write FProxyOnClick;
    class function IsOnClick(P: TNotifyEvent): boolean;
  end;
{$endif}
type  TMDIPanelManager = class;

type
  { TCustomMDIPanel }

  TCustomMDIPanel = class(TCustomPanel)
  private
    FClientControls:TFPList;
    FCaptionPanel: TPanel;
    FCaptionLabel: TLabel;
    FClientPanel: TMDIClientPanel; //TScrollBox;
    FSystemButton: TImage;
    FCloseButton: TSpeedButton;
    FMaximizeButton: TSpeedButton;
    FMinimizeButton: TSpeedButton;
    FRestoreButton: TSpeedButton;
    FSystemPopupMenu: TPopupMenu;
    FMenuItemMinimize: TMenuItem;
    FMenuItemMaximize: TMenuItem;
    FMenuItemRestore: TMenuItem;
    FMenuItemClose: TMenuItem;
    FMDIPanelManager: TMDIPanelManager;
  private
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnShortcut: TShortCutEvent;
    FOnShow: TNotifyEvent;

    FNormalBounds: Trect; // bounds when not maximized, minimized or hidden
    FWindowState: TWindowState;
    FCornerSize: integer;
    //FBorderColor: TColor;
    FActiveBorderColor: TColor;
    FInactiveBorderColor: TColor;
    FActive: boolean;
    FParentForm: TCustomForm;
    FPassiveBevel: TGraphicControl;
    FClickProxies: TFPList;
    FCaptionButtons: TCaptionButtons;
    FWindowResizingSide: TWindowResizingSide;

    //FLastResizeWidth:integer;
    //FLastResizeHeight:integer;
    //FLastResizeClientWidth:integer;
    //FLastResizeClientHeight:integer;

    FClosing: boolean;
    FFormState:TFormState;
    FFormStyle:TFormStyle;
    FPosition: TPosition;

    FActionClose: TAction;
    FActionList: TActionList;
    procedure FActionCloseOnExecute(sender:TObject);
  private

    WindowPositionState: TWindowPositionState;
    WindowCaptionMouseX, WindowCaptionMouseY: integer;

    function getUniqueName(nameBase: string): string;
    procedure CreateCaptionPanel(aCaptionButtons:TCaptionButtons);
    procedure CreateClientPanel;
    procedure PopulateClientPanel;
    procedure CreateSystemPopupMenu;
    procedure DeleteSystemPopupMenu;
    //procedure CreateSystemActions;
    procedure setDefaultSystemIcon;

    function drawCloseIcon(size: integer): TBitmap;
    function drawMaximizeIcon(size: integer): TBitmap;
    function drawMinimizeIcon(size: integer): TBitmap;
    function drawRestoreIcon(size: integer): TBitmap;

    procedure SystemButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DoClose(CloseAction: TCloseAction);
    procedure MaximizeButtonClick(Sender: TObject);
    procedure DoMaximize;
    procedure MinimizeButtonClick(Sender: TObject);
    procedure DoMinimize;
    procedure RestoreButtonClick(Sender: TObject);
    procedure DoRestore;
    procedure OrderButtons;
    procedure SetCaptionButtons(aVal:TCaptionButtons);

    //procedure ActionActionSystemMenuExecute(Sender: TObject);

    procedure CaptionPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CaptionPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure CaptionPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    procedure BorderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BorderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure BorderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SetBorderCursor(X, Y: integer);

    function deriveCaptionHeight:integer;
    function deriveCaptionIconHeight:integer;

    procedure SetCaption(const Value: TCaption);
    function GetCaption: TCaption;
    function GetControl(const Index: integer): TControl;
    function GetControlCount: integer;

    {$ifdef Use_MouseClickProxy}
    procedure setMouseClickProxiesRecursively(Ctrl: TControl);
    procedure unsetMouseClickProxiesRecursively(Ctrl: TControl);
    procedure setMouseClickProxies;
    procedure unsetMouseClickProxies;
    {$endif Use_MouseClickProxy}
    procedure PassivePanelOnClick(Sender: TObject);
    procedure ScreenOnActiveControlChanged(Sender: TObject; LastControl: TControl);
    procedure ClientPanelOnMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
  protected
    procedure LMSysCommand(var message: TLMessage); message LM_SYSCOMMAND;
    procedure Release;
    procedure Deactivate; virtual;
    procedure Paint; override;
    function  GetBorderColor: TColor;
    procedure SetActive(val: boolean);
    procedure SetActiveBorderColor(AValue: TColor);
    procedure SetInactiveBorderColor(AValue: TColor);
    procedure SetParent(NewParent: TWinControl); override;
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure SetName(const Value: TComponentName); override;
    procedure WndProc(var TheMessage: TLMessage); override;
    //procedure Resize; override;
  private
    function IsIconStored: Boolean;
    procedure ProcessResource;
    procedure SetMDIPanelManager(AValue: TMDIPanelManager);
    procedure InactivateSiblings;
  public
    constructor CreateNew(AOwner: TComponent); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;

    procedure InsertControl(AControl: TControl);
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;

    function  GetIcon :TIcon;
    procedure SetIcon(val:TIcon);

    property Controls[Index: integer]: TControl read GetControl;
    property ControlCount: integer read GetControlCount;

    property CaptionButtons:TCaptionButtons read FCaptionButtons write SetCaptionButtons;
    property FormStyle:TFormStyle read FFormStyle write FFormStyle default fsMDIChild;
    property MDIPanelManager:TMDIPanelManager read FMDIPanelManager write SetMDIPanelManager;
    property Position: TPosition read FPosition write FPosition default poDesigned;

    property Active: boolean read FActive write setActive;
    property BorderColor: TColor read GetBorderColor;
    property ActiveBorderColor: TColor read FActiveBorderColor write SetActiveBorderColor default clDefault;
    property InactiveBorderColor: TColor read FInactiveBorderColor write SetInactiveBorderColor default clDefault;
    property Caption: TCaption read GetCaption write SetCaption;
    property CaptionPanel: TPanel read FCaptionPanel;
    property Icon: TIcon read getIcon write setIcon stored IsIconStored;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;


    // unused plugs from TCustomForm just to satisfy IDE save/read properties
    private
        FActiveControl: TControl;
        FAllowDropFiles: Boolean;
        FAlphaBlend: Boolean;
        FAlphaBlendValue: Byte;
        FAutoScroll: Boolean;
        FBorderIcons: TBorderIcons;
        FDesignTimePPI: Integer;
        FDefaultMonitor: TDefaultMonitor;
        FHelpFile: string;
        FKeyPreview: Boolean;
        //FMDIChildren: array of TCustomMDIPanel;
        FMenu : TMainMenu;
        FOnCloseQuery : TCloseQueryEvent;
        FOnDropFiles: TDropFilesEvent;
        //FOnShowModalFinished: TModalDialogFinished;
        FOnWindowStateChange: TNotifyEvent;
        FPixelsPerInch: Integer;
        FScaled: Boolean;
        FPopupMode: TPopupMode;
        FPopupParent: TCustomForm;
        FShowInTaskBar: TShowInTaskbar;
    public
      property ActiveControl: TControl read FActiveControl write FActiveControl;
      property AllowDropFiles: Boolean read FAllowDropFiles write FAllowDropFiles default False;
      property AlphaBlend: Boolean read FAlphaBlend write FAlphaBlend;
      property AlphaBlendValue: Byte read FAlphaBlendValue write FAlphaBlendValue;
      property AutoScroll: Boolean read FAutoScroll write FAutoScroll default False;// auto show/hide scrollbars
      property BorderIcons: TBorderIcons read FBorderIcons write FBorderIcons
        default [biSystemMenu, biMinimize, biMaximize];
      property DefaultMonitor: TDefaultMonitor read FDefaultMonitor
        write FDefaultMonitor default dmActiveForm;
      property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI default 96;
      property HelpFile: string read FHelpFile write FHelpFile;
      property KeyPreview: Boolean read FKeyPreview write FKeyPreview default False;
      property Menu : TMainMenu read FMenu write FMenu;
      property OnCloseQuery : TCloseQueryEvent
                       read FOnCloseQuery write FOnCloseQuery stored True;
      property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
      property OnHelp: THelpEvent read FOnHelp write FOnHelp;
      property OnHide: TNotifyEvent read FOnHide write FOnHide;
      property OnResize stored True;
      property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
      //property OnShowModalFinished: TModalDialogFinished read FOnShowModalFinished write FOnShowModalFinished;
      property OnWindowStateChange: TNotifyEvent
                           read FOnWindowStateChange write FOnWindowStateChange;
      {property ParentFont default False;
      property Position: TPosition read FPosition write SetPosition default poDesigned;
      property RestoredLeft: integer read FRestoredLeft;
      property RestoredTop: integer read FRestoredTop;
      property RestoredWidth: integer read FRestoredWidth;
      property RestoredHeight: integer read FRestoredHeight;
      property Visible stored VisibleIsStored default false;
      }
      property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch stored False;
      property Scaled: Boolean read FScaled write FScaled default True;
      property PopupMode: TPopupMode read FPopupMode write FPopupMode default pmNone;
      property PopupParent: TCustomForm read FPopupParent write FPopupParent;
      property ShowInTaskBar: TShowInTaskbar read FShowInTaskbar write FShowInTaskBar
                                      default stDefault;
      property WindowState: TWindowState read FWindowState write FWindowState
                                         default wsNormal;

  end;

  TMDIPanel = class(TCustomMDIPanel)
  private
    FLCLVersion: string;
    function LCLVersionIsStored: boolean;
  published
    property Active;
    property BorderColor;
    property ActiveBorderColor;
    property InactiveBorderColor;
    property Caption;
    property CaptionPanel;
    property Icon;
    property OnActivate;
    property OnDeactivate;
    property OnClose;
    property OnCreate;
    property OnDestroy;
    property OnShow;

    // from TForm
    property Action;
    property ActiveControl;
    property Align;
    property AllowDropFiles;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultMonitor;
    property DesignTimePPI;
    property DockSite;
    property DoubleBuffered;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FormStyle;
    property HelpFile;
    property KeyPreview;
    property Menu;
    property OnChangeBounds;
    property OnClick;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHelp;
    property OnHide;
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
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShowHint;
    property OnStartDock;
    property OnUnDock;
    property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property SessionProperties;
    property ShowHint;
    property ShowInTaskBar;
    property UseDockManager;
    property LCLVersion: string read FLCLVersion write FLCLVersion stored LCLVersionIsStored;
    property Scaled;
    property Visible;
    property WindowState;
  end;


  { TMDIPanelManager }

  TMDIPanelManager = class
    private
      FMDIPanels: TFPList;
      function GetMDIPanel(Index: Integer): TCustomMDIPanel;
      function GetPanelCount: integer;
    public
      constructor Create; virtual;
      destructor Destroy; override;

      function  IndexOf(APanel: TCustomMDIPanel):integer;
      procedure Add(APanel: TCustomMDIPanel);
      procedure Insert(APanel: TCustomMDIPanel);
      procedure Insert(APanel: TCustomMDIPanel; Index: integer);
      procedure Remove(APanel: TCustomMDIPanel);
      procedure Delete(Index:integer);

      function  FindActivePanel: TCustomMDIPanel;

      procedure Tile;
      procedure Cascade;

    public
      property MDIPanels[Index: Integer]: TCustomMDIPanel read GetMDIPanel;
      property PanelCount:integer read GetPanelCount;
  end;

implementation
uses  intfgraphics, lazcanvas, FPImage, lcl;

{$I MDIPanel.inc}
{$I MDIPanelManager.inc}

end.
