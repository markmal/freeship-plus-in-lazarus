unit FreeSplitSectionDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, Spin,
  FreeTypes, Types;

type
  TEditMode = (emProgrammatic, emMouse, emKeyboard);

  TFreeSplitSectionDialogChangeEvent = procedure (Sender: TObject; aValue: TFloatType) of object;

  { TFreeSplitSectionDialog }

  TFreeSplitSectionDialog = class(TForm)
    BitBtn1: TBitBtn;
    fseSplitSectionLocation: TFloatSpinEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TopPanel: TPanel;
    tbSplitSectionLocation: TTrackBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fseSplitSectionLocationChange(Sender: TObject);
    procedure fseSplitSectionLocationEditingDone(Sender: TObject);
    procedure fseSplitSectionLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure fseSplitSectionLocationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure fseSplitSectionLocationMouseWheel(Sender: TObject;
      Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure tbSplitSectionLocationChange(Sender: TObject);
  private
    FEditMode:TEditMode;
    FSplitSectionLocation:TFloatType;
    FLength, FWidest, FSpaciest: TFloatType;
    FOnSplitSectionLocationChange: TFreeSplitSectionDialogChangeEvent;
    procedure SetSplitSectionLocation(AValue: TFloatType);
  public
    procedure SetDimensions(aLength, aWidest, aSpaciest: TFloatType);
    property SplitSectionLocation:TFloatType read FSplitSectionLocation write SetSplitSectionLocation;
    property OnSplitSectionLocationChange: TFreeSplitSectionDialogChangeEvent
             read FOnSplitSectionLocationChange
             write FOnSplitSectionLocationChange;
  end;

var
  FreeSplitSectionDialog: TFreeSplitSectionDialog;

implementation

{$R *.lfm}

{ TFreeSplitSectionDialog }

procedure TFreeSplitSectionDialog.fseSplitSectionLocationMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEditMode := emMouse;
end;

procedure TFreeSplitSectionDialog.fseSplitSectionLocationMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FEditMode := emMouse;
end;

procedure TFreeSplitSectionDialog.SpeedButton1Click(Sender: TObject);
begin
  SetSplitSectionLocation( 0.5 * FLength );
end;

procedure TFreeSplitSectionDialog.SpeedButton2Click(Sender: TObject);
begin
  SetSplitSectionLocation( FWidest );
end;

procedure TFreeSplitSectionDialog.SpeedButton3Click(Sender: TObject);
begin
  SetSplitSectionLocation( FSpaciest );
end;

procedure TFreeSplitSectionDialog.tbSplitSectionLocationChange(Sender: TObject);
begin
  SetSplitSectionLocation(0.001 * tbSplitSectionLocation.Position);
end;

procedure TFreeSplitSectionDialog.SetSplitSectionLocation(AValue: TFloatType);
begin
  if FSplitSectionLocation = AValue then Exit;
  FSplitSectionLocation:=AValue;
  //FEditMode := emProgrammatic;
  fseSplitSectionLocation.Value:=AValue;
  tbSplitSectionLocation.Position:=round(AValue*1000);
  if assigned(OnSplitSectionLocationChange) then
     OnSplitSectionLocationChange(Self, FSplitSectionLocation);
end;

// Lendth of ship, location of widest place, location of spaciest place
procedure TFreeSplitSectionDialog.SetDimensions(aLength, aWidest, aSpaciest: TFloatType);
begin
  FLength := aLength;
  FWidest := aWidest;
  FSpaciest := aSpaciest;
  tbSplitSectionLocation.Min:=0;
  tbSplitSectionLocation.Max:=round(aLength*1000);
  fseSplitSectionLocation.MinValue:=0.0;
  fseSplitSectionLocation.MaxValue:=aLength;
end;

procedure TFreeSplitSectionDialog.fseSplitSectionLocationKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FEditMode := emKeyboard;
end;

procedure TFreeSplitSectionDialog.fseSplitSectionLocationEditingDone(Sender: TObject);
begin
  //if abs(fseSplitSectionLocation.Value - AValue) < 1e-5 then exit;
  FEditMode := emKeyboard;
  SetSplitSectionLocation(fseSplitSectionLocation.Value);
end;

procedure TFreeSplitSectionDialog.fseSplitSectionLocationChange(Sender: TObject);
begin
  if FEditMode in [emMouse] then
     SetSplitSectionLocation(fseSplitSectionLocation.Value);
end;

procedure TFreeSplitSectionDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult:=mrClose;
  Close;
end;

procedure TFreeSplitSectionDialog.FormShow(Sender: TObject);
begin

end;

end.

