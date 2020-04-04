unit LightDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ButtonPanel,
  FreeGeometry;

type

  { TLightDialog }

  TLightDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    GroupBoxPosition: TGroupBox;
    GroupBoxPosition1: TGroupBox;
    LabelX: TLabel;
    LabelAmbience: TLabel;
    LabelLuminance: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    TrackBarX: TTrackBar;
    TrackBarY: TTrackBar;
    TrackBarZ: TTrackBar;
    TrackBarAmbience: TTrackBar;
    TrackBarIntensity: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TrackBarAmbienceChange(Sender: TObject);
    procedure TrackBarIntensityChange(Sender: TObject);
    procedure TrackBarXChange(Sender: TObject);
    procedure TrackBarYChange(Sender: TObject);
    procedure TrackBarZChange(Sender: TObject);
  private
  public
    ViewPort:TFreeViewPort;
    function getLight:TFreeLight;
  end;

implementation

{$R *.lfm}

{ TLightDialog }

function TLightDialog.getLight:TFreeLight;
begin
  result.Position.X:=TrackBarX.Position;
  result.Position.Y:=TrackBarY.Position;
  result.Position.Z:=TrackBarZ.Position;
  result.Ambient:= TrackBarAmbience.Position;
  result.Intensity := TrackBarIntensity.Position;
end;

procedure TLightDialog.TrackBarXChange(Sender: TObject);
begin
  ViewPort.Light := getLight;
end;

procedure TLightDialog.TrackBarYChange(Sender: TObject);
begin
  ViewPort.Light := getLight;
end;

procedure TLightDialog.TrackBarZChange(Sender: TObject);
begin
  ViewPort.Light := getLight;
end;

procedure TLightDialog.TrackBarAmbienceChange(Sender: TObject);
begin
  ViewPort.Light := getLight;
end;

procedure TLightDialog.TrackBarIntensityChange(Sender: TObject);
begin
  ViewPort.Light := getLight;
end;

procedure TLightDialog.FormClose(Sender:TObject; var CloseAction:TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TLightDialog.FormShow(Sender: TObject);
var H:integer;
begin
  H:=Canvas.TextHeight('|');
  H:= abs(round(h * Font.PixelsPerInch / 72.0));

  self.Constraints.MinHeight:=  ButtonPanel.BoundsRect.Bottom
                               +ButtonPanel.BorderSpacing.Bottom
                               +ButtonPanel.BorderSpacing.Around
                               +self.BorderWidth
                               +(self.Height - self.ClientHeight)
                               +H;
end;

end.


