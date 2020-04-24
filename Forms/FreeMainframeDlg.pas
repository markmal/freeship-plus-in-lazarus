unit FreeMainframeDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, Spin,
  FreeTypes, Types;

type
  TEditMode = (emProgrammatic, emMouse, emKeyboard);

  TFreeMainframeDialogChangeEvent = procedure (Sender: TObject; aValue: TFloatType) of object;

  { TFreeMainframeDialog }

  TFreeMainframeDialog = class(TForm)
    BitBtn1: TBitBtn;
    fseMainframeLocation: TFloatSpinEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TopPanel: TPanel;
    TrackBar1: TTrackBar;
    procedure fseMainframeLocationChange(Sender: TObject);
    procedure fseMainframeLocationEditingDone(Sender: TObject);
    procedure fseMainframeLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure fseMainframeLocationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure fseMainframeLocationMouseWheel(Sender: TObject;
      Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FEditMode:TEditMode;
    FMainframeLocation:TFloatType;
    FLength, FWidest, FSpaciest: TFloatType;
    FOnMainframeLocationChange: TFreeMainframeDialogChangeEvent;
    procedure SetMainframeLocation(AValue: TFloatType);
  public
    procedure SetDimensions(aLength, aWidest, aSpaciest: TFloatType);
    property MainframeLocation:TFloatType read FMainframeLocation write SetMainframeLocation;
    property OnMainframeLocationChange: TFreeMainframeDialogChangeEvent
             read FOnMainframeLocationChange
             write FOnMainframeLocationChange;
  end;

var
  FreeMainframeDialog: TFreeMainframeDialog;

implementation

{$R *.lfm}

{ TFreeMainframeDialog }

procedure TFreeMainframeDialog.fseMainframeLocationMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEditMode := emMouse;
end;

procedure TFreeMainframeDialog.fseMainframeLocationMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FEditMode := emMouse;
end;

procedure TFreeMainframeDialog.SpeedButton1Click(Sender: TObject);
begin
  SetMainframeLocation( 0.5 * FLength );
end;

procedure TFreeMainframeDialog.SpeedButton2Click(Sender: TObject);
begin
  SetMainframeLocation( FWidest );
end;

procedure TFreeMainframeDialog.SpeedButton3Click(Sender: TObject);
begin
  SetMainframeLocation( FSpaciest );
end;

procedure TFreeMainframeDialog.TrackBar1Change(Sender: TObject);
begin
  SetMainframeLocation(0.001 * TrackBar1.Position);
end;

procedure TFreeMainframeDialog.SetMainframeLocation(AValue: TFloatType);
begin
  if FMainframeLocation = AValue then Exit;
  FMainframeLocation:=AValue;
  //FEditMode := emProgrammatic;
  fseMainframeLocation.Value:=AValue;
  TrackBar1.Position:=round(AValue*1000);
  if assigned(OnMainframeLocationChange) then
     OnMainframeLocationChange(Self, FMainframeLocation);
end;

// Lendth of ship, location of widest place, location of spaciest place
procedure TFreeMainframeDialog.SetDimensions(aLength, aWidest, aSpaciest: TFloatType);
begin
  FLength := aLength;
  FWidest := aWidest;
  FSpaciest := aSpaciest;
  TrackBar1.Min:=0;
  TrackBar1.Max:=round(aLength*1000);
  fseMainframeLocation.MinValue:=0.0;
  fseMainframeLocation.MaxValue:=aLength;
end;

procedure TFreeMainframeDialog.fseMainframeLocationKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FEditMode := emKeyboard;
end;

procedure TFreeMainframeDialog.fseMainframeLocationEditingDone(Sender: TObject);
begin
  //if abs(fseMainframeLocation.Value - AValue) < 1e-5 then exit;
  FEditMode := emKeyboard;
  SetMainframeLocation(fseMainframeLocation.Value);
end;

procedure TFreeMainframeDialog.fseMainframeLocationChange(Sender: TObject);
begin
  if FEditMode in [emMouse] then
     SetMainframeLocation(fseMainframeLocation.Value);
end;

end.

