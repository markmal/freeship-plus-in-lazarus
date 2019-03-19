unit ScrollPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, Graphics, Controls, ExtCtrls, Buttons, Arrow;

type

TMyArrow = class(TArrow)
protected
  function GetClientRect: TRect; override;
end;

TScrollPanel = class(TCustomPanel)
private
  FLeftButton:TArrow;
  FRightButton:TArrow;
  FControlPanel:TCustomPanel;
  FControlSelected:integer;
  FControlAutosize: boolean;
  FControlConstraints: TSizeConstraints;
  FControlBorderSpacing: TControlBorderSpacing;
  FStartControl: integer;
  FOnSelectionChanged: TNotifyEvent;
  FIsSelectionInteractive: boolean;

  procedure setControlAutosize(AValue: boolean);
  procedure setControlConstraints(AValue: TSizeConstraints);
  procedure setControlBorderSpacing(AValue: TControlBorderSpacing);
  procedure setControlSelected(index:integer);

  procedure DoOnLeftButtonActionExecute(Sender: TObject);
  procedure DoOnRightButtonActionExecute(Sender: TObject);
  procedure DoOnControlActionExecute(Sender: TObject);
  procedure DoAlignment;
  procedure DoEnableButtons;
  function  getControlsWidth:integer;

public
  constructor Create(TheOwner: TComponent); override;
  procedure insertSpeedButton(aCaption:string; Index: integer);
  procedure DoOnResize; override;
  procedure showSelected;
  function  ConrolIndex(aControl: TObject):integer;

published
  property ControlSelected:integer read FControlSelected write setControlSelected;
  property ControlAutosize:boolean read FControlAutosize write setControlAutosize;
  property ControlConstraints: TSizeConstraints read FControlConstraints
                                                write setControlConstraints;
  property ControlBorderSpacing: TControlBorderSpacing read FControlBorderSpacing
                                                      write setControlBorderSpacing;
  property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  property ControlPanel:TCustomPanel read FControlPanel;
  property IsSelectionInteractive: boolean read FIsSelectionInteractive;
end;

implementation

function TMyArrow.GetClientRect: TRect;
begin
  result := Rect(0, 0, Width, Height);
  InflateRect(result, -BorderSpacing.InnerBorder, -BorderSpacing.InnerBorder);
end;

constructor TScrollPanel.Create(TheOwner: TComponent);
  function makeArrowButton(aOwner:TComponent; aType:TArrowType):TArrow;
  begin
    result := TMyArrow.Create(aOwner);
    with result do
      begin
      Parent:=Self;
      AntiAliasingMode:=amOn;
      AutoSize:=true;
      ArrowType:=aType;
      ArrowPointerAngle:=120;
      ArrowColor:=clBtnText;
      ShadowType:=stFilled;
      ShadowType:=stNone;
      BorderSpacing.InnerBorder := 2;
      Action := TBasicAction.Create(result);
      Action.ActionComponent:=result;
      end;
  end;
begin
  inherited Create(TheOwner);

  FIsSelectionInteractive := false;

  FLeftButton := makeArrowButton(Self, atLeft);
  FLeftButton.Align:=alLeft;
  FLeftButton.Action.OnExecute := @DoOnLeftButtonActionExecute;

  FRightButton := makeArrowButton(Self, atRight);
  FRightButton.Align:=alRight;
  FRightButton.Action.OnExecute := @DoOnRightButtonActionExecute;

  FControlPanel := TPanel.Create(Self);
  with FControlPanel do
    begin
    Parent:=Self;
    Align:=alClient;
    AutoSize:=true;
    BevelOuter:=bvNone;
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    end;

  FControlConstraints := TSizeConstraints.Create(Self);
  FControlBorderSpacing := TControlBorderSpacing.Create(Self);
  FStartControl:=0;

end;


procedure TScrollPanel.setControlAutosize(AValue: boolean);
begin
  FControlAutosize := Avalue;
  DoAlignment;
end;

procedure TScrollPanel.setControlConstraints(AValue: TSizeConstraints);
begin
  FControlConstraints := AValue;
  DoAlignment;
end;

procedure TScrollPanel.setControlBorderSpacing(AValue: TControlBorderSpacing);
begin
  FControlBorderSpacing := AValue;
  DoAlignment;
end;

procedure TScrollPanel.setControlSelected(index:integer);
var i:integer;
begin
  FControlSelected := index;
  for i:=0 to FControlPanel.ControlCount - 1 do
    FControlPanel.Controls[i].Font.Style:=[];
  if (index >= 0) and (index < (FControlPanel.ControlCount)) then
    begin
    FControlPanel.Controls[index].Font.Style:=[fsBold];
    if assigned(FOnSelectionChanged) then
       FOnSelectionChanged(Self);
    end;
  DoAlignment;
end;

procedure TScrollPanel.showSelected;
var i,j:integer; s:string; sc:TControl;
begin
  if (FControlSelected < 0) or (FControlSelected >= (FControlPanel.ControlCount)) then
    exit;
  sc := FControlPanel.Controls[FControlSelected];
  DoAlignment;
  while not sc.Visible do
    DoOnLeftButtonActionExecute(nil);
  s:=sc.Caption;
  i:=sc.BoundsRect.Left;
  i:=sc.BoundsRect.Right;
  j:=FControlPanel.ClientWidth;
  while sc.BoundsRect.Right > FControlPanel.ClientWidth do
    begin
    i:=sc.BoundsRect.Right;
    i:=FControlPanel.ClientWidth;
    DoOnRightButtonActionExecute(nil);
    end;
end;

procedure TScrollPanel.insertSpeedButton(aCaption:string; Index: integer);
var sb:TSpeedButton; tw, fw:integer;
  fd: TFontData; bmp: TBitmap;
begin
  sb:=TSpeedButton.Create(FControlPanel);
  sb.Caption:=aCaption;
  sb.AllowAllUp:=true;
  sb.AutoSize:=true;
  sb.Constraints := FControlConstraints;
  sb.BorderSpacing := FControlBorderSpacing;
  fw := FControlBorderSpacing.InnerBorder;
  if assigned(sb.Canvas) and sb.Canvas.HandleAllocated
  then tw:=sb.Canvas.TextWidth(aCaption)
  else
    begin
      bmp := TBitmap.Create;
      try
        bmp.Canvas.Font.Assign(sb.Font);
        tw := bmp.Canvas.TextWidth(aCaption);
      finally
        bmp.Free;
      end;
    end;
  sb.Constraints.MinWidth:=tw+fw*2;
  sb.Margin:=fw;
  sb.Action := TBasicAction.Create(sb);
  sb.Action.ActionComponent:=sb;
  sb.Action.OnExecute := @DoOnControlActionExecute;
  FControlPanel.InsertControl(sb,Index);
  DoAlignment;
end;

function TScrollPanel.ConrolIndex(aControl: TObject):integer;
var i:integer;
begin
  for i:=0 to FControlPanel.ControlCount - 1 do
    if FControlPanel.Controls[i] = aControl
    then begin result:=i; exit; end;
end;

procedure TScrollPanel.DoOnControlActionExecute(Sender: TObject);
begin
  FIsSelectionInteractive := true;
  if Sender is TBasicAction then
     Self.setControlSelected(ConrolIndex(TBasicAction(Sender).ActionComponent));
  FIsSelectionInteractive := false;
end;

procedure TScrollPanel.DoOnLeftButtonActionExecute(Sender: TObject);
begin
  dec(FStartControl);
  if FStartControl < 0 then FStartControl := 0;
  DoAlignment;
end;

procedure TScrollPanel.DoOnRightButtonActionExecute(Sender: TObject);
var lc, lcr : integer;
begin
  lc := FControlPanel.ControlCount-1;
  inc(FStartControl);
  if FStartControl > lc then
     FStartControl := lc;
  DoAlignment;
end;

function TScrollPanel.getControlsWidth:integer;
var i,g:integer;
begin
  g:=FControlPanel.ChildSizing.HorizontalSpacing;
  result:=0;
  for i:=0 to FControlPanel.ControlCount - 1 do
    result := result + FControlPanel.Controls[i].Width;
  result := result + (FControlPanel.ControlCount - 1) * g;
  result := result + FControlPanel.ChildSizing.LeftRightSpacing*2;
end;

procedure TScrollPanel.DoAlignment;
var i, cw:integer;
begin
  cw:=getControlsWidth;
  FLeftButton.Visible := cw > FControlPanel.ClientWidth;
  FRightButton.Visible := cw > FControlPanel.ClientWidth;

  for i:=0 to FControlPanel.ControlCount - 1 do
    FControlPanel.Controls[i].Visible := (i >= FStartControl);

  DoEnableButtons;
  invalidate;
end;

procedure TScrollPanel.DoEnableButtons;
var lc, lcr : integer;
begin
  if FControlPanel.ControlCount = 0 then exit;

  FLeftButton.Enabled := (FStartControl > 0);

  lc := FControlPanel.ControlCount-1;
  if FStartControl > lc then
     FStartControl := lc;
  lcr := FControlPanel.Controls[lc].BoundsRect.Right;

  FRightButton.Enabled := (lcr > FControlPanel.ClientWidth);

  if FLeftButton.Enabled
    then FLeftButton.ArrowColor:=clBtnText
    else FLeftButton.ArrowColor:= clGray;
  if FRightButton.Enabled
    then FRightButton.ArrowColor:=clBtnText
    else FRightButton.ArrowColor:=clGray;
end;


procedure TScrollPanel.DoOnResize;
begin
  inherited DoOnResize;
  DoAlignment;
  //if Assigned(FOnResize) then FOnResize(Self);
  //DoCallNotifyHandler(chtOnResize);
end;

end.

