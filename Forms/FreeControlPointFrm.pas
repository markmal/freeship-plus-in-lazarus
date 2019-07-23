{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
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

unit FreeControlPointFrm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Math,
     FreeTypes,
     FreeGeometry,
     StdCtrls, Buttons, ExtCtrls, Spin, FileCtrl, Types;

type

 TEntryMethod = ( emNone, emTyping, emArrowKeys, emMouse, emPaste );

{ TFreeControlPointForm }

 TFreeControlPointForm  = class(TForm)
     EditLinearConstraintA: TEdit;
     EditAnchorPoint: TEdit;
     EditLinearConstraintB: TEdit;
    EditName: TEdit;
    EditX: TFloatSpinEdit;
    EditAX: TFloatSpinEdit;
    EditAY: TFloatSpinEdit;
    EditAZ: TFloatSpinEdit;
    FilterComboBoxLinearConstraintA: TFilterComboBox; //TODO delete if not used
    FilterComboBoxLinearConstraintB: TFilterComboBox; //TODO delete if not used
    GroupBoxLinearConstraint: TGroupBox;
    GroupBoxAnchorConstraint: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelName: TLabel;
    LabelX: TLabel;
    EditY: TFloatSpinEdit;
    EditZ: TFloatSpinEdit;
    LabelAX: TLabel;
    LabelY: TLabel;
    LabelAY: TLabel;
    LabelZ: TLabel;
    CheckBoxCorner: TCheckBox;
    LabelAZ: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label4: TLabel;
    EditDistance: TFloatSpinEdit;
    Label5: TLabel;
    EditAngles: TFloatSpinEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    SpeedButtonRemoveLinearConstraint: TSpeedButton;
    SpeedButtonRemoveAnchorPoint: TSpeedButton;
    procedure CheckBoxCornerChange(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure EditNameEditingDone(Sender: TObject);
    procedure EditXChange(Sender: TObject);
    procedure EditXEditingDone(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: char);
    procedure EditYChange(Sender: TObject);
    procedure EditYEditingDone(Sender: TObject);
    procedure EditZChange(Sender: TObject);
    procedure EditZEditingDone(Sender: TObject);
    {procedure FilterComboBoxLinearConstraintAChange(Sender: TObject);  //TODO delete if not used
    procedure PopulateFilterComboBoxLinearConstraintA(Sender: TObject); //TODO delete if not used
    procedure FilterComboBoxLinearConstraintBChange(Sender: TObject); //TODO delete if not used
    procedure PopulateFilterComboBoxLinearConstraintB(Sender: TObject); //TODO delete if not used
    }
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButtonRemoveAnchorPointClick(Sender: TObject);
    procedure SpeedButtonRemoveLinearConstraintClick(Sender: TObject);
   private  { Private declarations }
      FActiveControlPoint  : TFreeSubdivisionControlPoint;
      FFreeShip            : TComponent;
      FSkipCheckbox1OnClick: boolean;
      FActiveControlPointChanging: boolean;
      FilterComboBoxLinearConstraintAPopulating, FilterComboBoxLinearConstraintBPopulating : boolean; //TODO delete if not used
      EnteredControl: TControl;
      EntryMethod: TEntryMethod;
      procedure FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
      procedure FSetActiveControlPointCorner(isCorner: boolean);
   public   { Public declarations }
      property ActiveControlPoint   : TFreeSubdivisionControlPoint read FActiveControlPoint write FSetActiveControlPoint;
      property FreeShip             : TComponent read FFreeShip write FFreeShip;
      //property FreeShip : TFreeShip read FFreeShip write FFreeShip;
end;

var FreeControlPointForm: TFreeControlPointForm;

implementation

uses FreeLanguageSupport,
     FreeShipUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const cDegree=#$C2#$B0; //UTF8 degree sign
      cAlpha=#$CE#$B1;
      cBeta=#$CE#$B2;
      cGamma=#$CE#$B3;

resourcestring
  rsAngle = 'Angle';
  rsAngles = 'Angles';
  rsLength = 'Length';

  rsNameIsNotUnique = 'Name Is Not Unique';
  rsPointNameChanged = 'Point Name Changed';

  rsXCoordinate = 'X Coordinate';
  rsYCoordinate = 'Y Coordinate';
  rsZCoordinate = 'Z Coordinate';


procedure TFreeControlPointForm.FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
var I,N : Integer;
    BCol:TColor;
    FCol:TColor;
    Npoi: Integer;  
    R,R1,R2,alfa,beta,gamma,acos,len    : single;
    cos1a,cos2a,cos1b,cos2b,cos1g,cos2g : single;
    C0,CN,C1,C2,C3,Cp,Ci:T3DCoordinate;
    IsPointDifferent: boolean;
begin
   IsPointDifferent := FActiveControlPoint <> Val;
   FActiveControlPoint:=Val;
   FActiveControlPointChanging := true;

   if ActiveControlPoint=nil then
   begin
      Visible:=False;
      EditX.Value:=0.0;
      EditY.Value:=0.0;
      EditZ.Value:=0.0;
      EditAX.Value:=0.0;
      EditAY.Value:=0.0;
      EditAZ.Value:=0.0;
      CheckBoxCorner.Checked:=false;
      EditName.Text := '';
      Self.Caption := '';
   end else
   begin
      R:=0.0;
      EditDistance.Value:=0.0;

      Self.Caption := 'Point['+IntToStr(FActiveControlPoint.Id) +']:'+FActiveControlPoint.Name;
                   {+ ' ('+FloatToStr(FActiveControlPoint.Coordinate.X)
                     +','+FloatToStr(FActiveControlPoint.Coordinate.Y)
                     +','+FloatToStr(FActiveControlPoint.Coordinate.Z)
                     +')';}

      Npoi:=TFreeShip(FreeShip).NumberOfSelectedControlPoints-1;
      if Npoi >= 0 then begin
         C0:=TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate;
         CN:=TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate;;
      end;

      if Npoi>= 1 then begin
          R:=SQRT(Sqr(C0.X-CN.X)+Sqr(C0.Y-CN.Y)+Sqr(C0.Z-CN.Z));
          EditDistance.Value := R;
          EditAngles.Value:=0.0;
          end    
      else begin
          EditDistance.Value:=0.0;
      end;

      if  Npoi>= 4 then begin
          R:=SQRT(Sqr(C0.X-CN.X)+Sqr(C0.Y-CN.Y)+Sqr(C0.Z-CN.Z));
          EditDistance.Value := R;
          len:=0;		  
          for i:=1 to Npoi do
          begin
           Cp:=TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate;
           Ci:=TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate;
           R:=SQRT(Sqr(Cp.X-Ci.X)+Sqr(Cp.Y-Ci.Y)+Sqr(Cp.Z-Ci.Z));
		   len:=len+R;
          end;
          Label5.Caption:=rsLength;
          EditAngles.Value := len;
      end;

	  
      if R=0 then R:=0.0001;
      if  Npoi=1 then begin
        alfa:=abs(C0.X-CN.X)/R;
        beta:=abs(C0.Y-CN.Y)/R;
        gamma:=abs(C0.Z-CN.Z)/R;
        EditAX.Value:=ArcCos(alfa)*57.29578;
        EditAY.Value:=ArcCos(beta)*57.29578;
        EditAZ.Value:=ArcCos(gamma)*57.29578;
        end;

      if Npoi=0 then begin
        R:=SQRT(Sqr(C0.X)+Sqr(C0.Y)+Sqr(C0.Z));
        if R=0 then R:=0.0001;				
        alfa:=C0.X/R;
        beta:=C0.Y/R;
        gamma:=C0.Z/R;
        EditAX.Value:=ArcCos(alfa)*57.29578;
        EditAY.Value:=ArcCos(beta)*57.29578;
        EditAZ.Value:=ArcCos(gamma)*57.29578;
        {EditAngles.Text := Truncate(ArcCos(alfa)*57.29578,2)+';'+Truncate(ArcCos(beta)*57.29578,2)+';'+Truncate(ArcCos(gamma)*57.29578,2);}
      end;

      if  Npoi=2 then begin
        C1:=TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate;
        C2:=TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate;
        R1:=SQRT(Sqr(C0.X-C1.X)+Sqr(C0.Y-C1.Y)+Sqr(C0.Z-C1.Z));
        R2:=SQRT(Sqr(C1.X-C2.X)+Sqr(C1.Y-C2.Y)+Sqr(C1.Z-C2.Z));

        if R1>0 then
        begin
        cos1a:=abs(C0.X-C1.X)/R1;
        cos1b:=abs(C0.Y-C1.Y)/R1;
        cos1g:=abs(C0.Z-C1.Z)/R1;
        end
        else
        begin
        cos1a:=0;
        cos1b:=0;
        cos1g:=0;
        end;

        if R2>0 then
        begin
        cos2a:=abs(C1.X-C2.X)/R2;
        cos2b:=abs(C1.Y-C2.Y)/R2;
        cos2g:=abs(C1.Z-C2.Z)/R2;
        end
        else
        begin
        cos2a:=0;
        cos2b:=0;
        cos2g:=0;
        end;
        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g;
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Value := ArcCos(acos)*57.29578;
      end;

      if  Npoi=3 then begin
        C1:=TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate;
        C2:=TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate;
        C3:=TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate;
        R1:=SQRT(Sqr(C0.X-C1.X)+Sqr(C0.Y-C1.Y)+Sqr(C0.Z-C1.Z));
        R2:=SQRT(Sqr(C2.X-C3.X)+Sqr(C2.Y-C3.Y)+Sqr(C2.Z-C3.Z));
        if R1>0 then
        begin
        cos1a:=abs(C0.X-C1.X)/R1;
        cos1b:=abs(C0.Y-C1.Y)/R1;
        cos1g:=abs(C0.Z-C1.Z)/R1;
        end
        else
        begin
        cos1a:=0;
        cos1b:=0;
        cos1g:=0;
        end;

        if R2>0 then
        begin
        cos2a:=(C2.X-C3.X)/R2;
        cos2b:=(C2.Y-C3.Y)/R2;
        cos2g:=(C2.Z-C3.Z)/R2;
        end
        else
        begin
        cos1a:=0;
        cos1b:=0;
        cos1g:=0;
        end;

        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g;
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Value := ArcCos(acos)*57.29578;
      end;


      EditX.Value:=FActiveControlPoint.Coordinate.X;
      EditY.Value:=FActiveControlPoint.Coordinate.Y;
      EditZ.Value:=FActiveControlPoint.Coordinate.Z;

      if IsPointDifferent then
      begin
        if  Npoi<2 then Label5.Caption:=rsAngles;
        if  Npoi=2 then Label5.Caption:=rsAngle;
        if  Npoi=3 then Label5.Caption:=rsAngle;
        if  Npoi>3 then Label5.Caption:=rsAngles;

        if (CheckBoxCorner.Checked <> (FActiveControlPoint.VertexType=svCorner))
           then begin
              FSkipCheckbox1OnClick:=true;
              CheckBoxCorner.Checked:=FActiveControlPoint.VertexType=svCorner;
              FSkipCheckbox1OnClick:=false;
           end;

        // Count the number of crease edges connected to this point
        N:=0;
        for I:=1 to FActiveControlPoint.NumberOfEdges do
          if FActiveControlPoint.Edge[I-1].Crease then inc(N);
        CheckBoxCorner.Enabled:=((N>0) and (N<3) and (not Val.Locked)); // points with more than two crease edges must always be a corner

        EditX.Enabled:=not Val.Locked;
        EditY.Enabled:=not Val.Locked;
        EditZ.Enabled:=not Val.Locked;
        {if Val.Locked then
        begin
           BCol:=clBtnFace;
           FCol:=clDkgray;
        end else
        begin
           BCol:=clWindow;
           FCol:=clBlack;
        end;
        if EditX.Color<>BCol then EditX.Color:=BCol;
        if EditX.Font.Color<>FCol then EditX.Font.Color:=FCol;
        if EditY.Color<>BCol then EditY.Color:=BCol;
        if EditY.Font.Color<>FCol then EditY.Font.Color:=FCol;
        if EditZ.Color<>BCol then EditZ.Color:=BCol;
        if EditZ.Font.Color<>FCol then EditZ.Font.Color:=FCol;}

        EditName.Text := Val.Name;
        EditName.Color:= clDefault;

        //PopulateFilterComboBoxLinearConstraintA(nil);   //TODO delete if not used
        //PopulateFilterComboBoxLinearConstraintB(nil);   //TODO delete if not used

        if (FActiveControlPoint.LinearConstraintPointA<>nil)
           and (FActiveControlPoint.LinearConstraintPointB<>nil) then
          begin
            GroupBoxLinearConstraint.Visible := true;
            EditLinearConstraintA.Text:='';
            EditLinearConstraintB.Text:='';

            if (FActiveControlPoint.LinearConstraintPointA<>nil) then
              if (FActiveControlPoint.LinearConstraintPointA.Name > '') then
               EditLinearConstraintA.Text := FActiveControlPoint.LinearConstraintPointA.Name
               else EditLinearConstraintA.Text := 'Point['+IntToStr(FActiveControlPoint.LinearConstraintPointA.Id)+']';
            if (FActiveControlPoint.LinearConstraintPointB<>nil) then
              if (FActiveControlPoint.LinearConstraintPointB.Name > '') then
               EditLinearConstraintB.Text := FActiveControlPoint.LinearConstraintPointB.Name
               else EditLinearConstraintB.Text := 'Point['+IntToStr(FActiveControlPoint.LinearConstraintPointB.Id)+']';
          end
        else
          GroupBoxLinearConstraint.Visible := false;


        if (FActiveControlPoint.AnchorPoint<>nil) then
          begin
            GroupBoxAnchorConstraint.Visible := true;
            EditAnchorPoint.Text := '';
            if (FActiveControlPoint.AnchorPoint.Name > '') then
             EditAnchorPoint.Text := FActiveControlPoint.AnchorPoint.Name
             else EditAnchorPoint.Text := 'Point['+IntToStr(FActiveControlPoint.AnchorPoint.Id)+']';
          end
        else
          GroupBoxAnchorConstraint.Visible := false;

      end;

   end;
   FActiveControlPointChanging := false;
end;{TFreeControlPointForm.FSetActiveControlPoint}

procedure TFreeControlPointForm.EditXChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if FActiveControlPointChanging then exit;
   if EnteredControl <> EditX then exit;
   if not(EditX.Focused and EditX.Enabled and not EditX.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
     if EntryMethod <> emTyping then
     begin
        TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointMove,True);
        P:=ActiveControlPoint.Coordinate;
        //P.X:=P.X+TFreeShip(FreeShip).Visibility.CursorIncrement;
        P.X:=EditX.Value;
        ActiveControlPoint.Coordinate:=P;
        TFreeShip(FreeShip).Build:=False;
        TFreeShip(FreeShip).FileChanged:=True;
        TFreeShip(FreeShip).Redraw;
        //ActiveControlPoint:=ActiveControlPoint;
     end;
end;

procedure TFreeControlPointForm.EditYChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if FActiveControlPointChanging then exit;
   if EnteredControl <> EditY then exit;
   if not(EditY.Focused and EditY.Enabled and not EditY.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
     if EntryMethod <> emTyping then
     begin
        TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointMove,True);
        P:=ActiveControlPoint.Coordinate;
        //P.Y:=P.Y+TFreeShip(FreeShip).Visibility.CursorIncrement;
        P.Y:=EditY.Value;
        ActiveControlPoint.Coordinate:=P;
        TFreeShip(FreeShip).Build:=False;
        TFreeShip(FreeShip).FileChanged:=True;
        TFreeShip(FreeShip).Redraw;
        ///ActiveControlPoint:=ActiveControlPoint;
     end;
end;

procedure TFreeControlPointForm.EditZChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if FActiveControlPointChanging then exit;
   if EnteredControl <> EditZ then exit;
   if not(EditZ.Focused and EditZ.Enabled and not EditZ.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
     if EntryMethod <> emTyping then
     begin
        TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointMove,True);
        P:=ActiveControlPoint.Coordinate;
        //P.Z:=P.Z+TFreeShip(FreeShip).Visibility.CursorIncrement;
        P.Z:=EditZ.Value;
        ActiveControlPoint.Coordinate:=P;
        TFreeShip(FreeShip).Build:=False;
        TFreeShip(FreeShip).FileChanged:=True;
        TFreeShip(FreeShip).Redraw;
        //ActiveControlPoint:=ActiveControlPoint;
     end;
end;

procedure TFreeControlPointForm.EditXEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if Self.FActiveControlPointChanging then exit;
   if not Self.Active then exit;
   //if not Self.Focused then exit;
   if EnteredControl <> EditX then exit;

   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      //Val := ConvertCoordinate(EditX.Text, P.X);
      Val := EditX.Value;
      if (abs(P.X-Val)>1e-5) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
         I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            //Val:=ConvertCoordinate(EditX.Text, P.X);
            Val := EditX.Value;
            if abs(P.X-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(rsXCoordinate,True);
                  saved := true;
               end;
               P.X:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;
//  finally update the text field:
         if EditX.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            //Val:= ConvertCoordinate(EditX.Text, P.X);
            Val := EditX.Value;
         end
         else Val:= 0;
         EditX.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit1Exit}

procedure TFreeControlPointForm.EditEnter(Sender: TObject);
begin
  EnteredControl := Sender as TControl;
  EntryMethod := emNone;
end;

procedure TFreeControlPointForm.EditExit(Sender: TObject);
begin
  EnteredControl := nil;
  EntryMethod := emNone;
end;

procedure TFreeControlPointForm.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EntryMethod := emMouse;
end;

procedure TFreeControlPointForm.EditMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  EntryMethod := emMouse;
end;

procedure TFreeControlPointForm.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  EntryMethod := emTyping;
  if ((Key=38) or (Key=40) and ([] = Shift)) then
   EntryMethod := emArrowKeys;
  if ((Key=86) and ( ssCtrl in Shift)) then // Ctrl-V
   EntryMethod := emPaste;
  if ((Key=45) and ( ssShift in Shift)) then // Shift-Ins
   EntryMethod := emPaste;
end;

procedure TFreeControlPointForm.EditKeyPress(Sender: TObject; var Key: char);
begin
  if ((Key >= '0') and (Key <= '9'))
    or (Key=DefaultFormatSettings.DecimalSeparator) or (Key='.') or (Key=',') or (Key='+') or (Key='-')
    then EntryMethod := emTyping;
end;

procedure TFreeControlPointForm.EditYEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if Self.FActiveControlPointChanging then exit;
   if not Self.Active then exit;
   //if not Self.Focused then exit;
   if EnteredControl <> EditY then exit;

   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      //Val := ConvertCoordinate(EditY.Text, P.Y);
      Val := EditY.Value;
      if (abs(P.Y-Val)>1e-5) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            //Val:=ConvertCoordinate(EditY.Text, P.Y);
            Val := EditY.Value;
            if abs(P.Y-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(rsYCoordinate,True);
                  saved := true;
               end;
               P.Y:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;

//  finally update the text field:
         if EditY.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            //Val:= ConvertCoordinate(EditY.Text, P.Y);
            Val := EditY.Value;
         end
         else Val:= 0;
         EditY.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit2Exit}

procedure TFreeControlPointForm.EditZEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if Self.FActiveControlPointChanging then exit;
   if not Self.Active then exit;
   //if not Self.Focused then exit;
   if EnteredControl <> EditZ then exit;

   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      //Val := ConvertCoordinate(EditZ.Text, P.Z);
      Val := EditZ.Value;
      if (abs(P.Z-Val)>1e-5) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            //Val:=ConvertCoordinate(EditZ.Text, P.Z);
            Val := EditZ.Value;
            if abs(P.Z-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(rsZCoordinate,True);
                  saved := true;
               end;
               P.Z:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;

//  finally update the text field:
         if EditZ.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            //Val:=ConvertCoordinate(EditZ.Text, P.Z);
            Val := EditZ.Value;
         end
         else Val:= 0;
         EditZ.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit3Exit}

{
procedure TFreeControlPointForm.FilterComboBoxLinearConstraintAChange(
  Sender: TObject);
var i:integer;   pLCP:TFreeSubdivisionPoint;
begin
   if Self.FActiveControlPointChanging then exit;
   if not Self.Active then exit;
   //if not Self.Focused then exit;
  if FilterComboBoxLinearConstraintAPopulating then exit;
  if self.EnteredControl <> FilterComboBoxLinearConstraintA then exit;

  pLCP:=FActiveControlPoint.LinearConstraintPointA;

  i := FilterComboBoxLinearConstraintA.ItemIndex;
  //FActiveControlPoint.LinearConstraintPointA := nil;
  if i > -1 then
    FActiveControlPoint.LinearConstraintPointA := TFreeSubdivisionControlPoint(
        FilterComboBoxLinearConstraintA.Items.Objects[FilterComboBoxLinearConstraintA.ItemIndex]
    );

  if pLCP <> FActiveControlPoint.LinearConstraintPointA then
  begin
    TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointLinearConstraintChanged,True);
    TFreeShip(FreeShip).FileChanged:=True;
  end;

  if (FActiveControlPoint.LinearConstraintPointA <> nil)
  and(FActiveControlPoint.LinearConstraintPointB <> nil)
  //then FActiveControlPoint.AdjustToLinearConstraint(TFreeShip(FreeShip).GetFocusedViewport);
    then FActiveControlPoint.AdjustToLinearConstraint(nil);

end;

procedure TFreeControlPointForm.PopulateFilterComboBoxLinearConstraintA(
  Sender: TObject);
var PL:TStringList; i:integer; N,PN,LCAN,LCBN:String;
begin
  FilterComboBoxLinearConstraintAPopulating:=true;
   LCAN:=''; LCBN:='';
   if FActiveControlPoint.LinearConstraintPointA<>nil then
    LCAN:=FActiveControlPoint.LinearConstraintPointA.Name;
   if FActiveControlPoint.LinearConstraintPointB<>nil then
    LCBN:=FActiveControlPoint.LinearConstraintPointB.Name;
   PL:=TFreeShip(FreeShip).GetAllNamedPoints;
   PL.Sort;
   FilterComboBoxLinearConstraintA.ClearSelection;
   FilterComboBoxLinearConstraintA.Clear;
   FilterComboBoxLinearConstraintA.AddItem('',nil);
   for i:=0 to PL.Count-1 do
   begin
     //N:=FActiveControlPoint.Name; PN:=PL[i];
     if (PL[i]<>FActiveControlPoint.Name) and (PL[i]<>LCBN) then
     begin
       FilterComboBoxLinearConstraintA.AddItem(PL[i],PL.Objects[i]);
       if (PL[i] = LCAN)
         then FilterComboBoxLinearConstraintA.ItemIndex:=
              FilterComboBoxLinearConstraintA.Items.Count-1;
     end;
   end;
   FilterComboBoxLinearConstraintAPopulating:=false;
   PL.Free;
end;

procedure TFreeControlPointForm.FilterComboBoxLinearConstraintBChange(
  Sender: TObject);
var i:integer;  pLCP:TFreeSubdivisionPoint;
begin
  if Self.FActiveControlPointChanging then exit;
  if not Self.Active then exit;
   //if not Self.Focused then exit;
  if FilterComboBoxLinearConstraintBPopulating then exit;
  if self.EnteredControl <> FilterComboBoxLinearConstraintB then exit;

  pLCP:=FActiveControlPoint.LinearConstraintPointA;

  i := FilterComboBoxLinearConstraintB.ItemIndex;
  //FActiveControlPoint.LinearConstraintPointB := nil;
  if i > -1 then
    FActiveControlPoint.LinearConstraintPointB := TFreeSubdivisionControlPoint(
        FilterComboBoxLinearConstraintB.Items.Objects[FilterComboBoxLinearConstraintB.ItemIndex]
    );

  if pLCP <> FActiveControlPoint.LinearConstraintPointB then
  begin
    TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointLinearConstraintChanged,True);
    TFreeShip(FreeShip).FileChanged:=True;
  end;

  if (FActiveControlPoint.LinearConstraintPointA <> nil)
  and(FActiveControlPoint.LinearConstraintPointB <> nil)
  //then FActiveControlPoint.AdjustToLinearConstraint(TFreeShip(FreeShip).GetFocusedViewport);
  then FActiveControlPoint.AdjustToLinearConstraint(nil);
end;

procedure TFreeControlPointForm.PopulateFilterComboBoxLinearConstraintB(
  Sender: TObject);
var PL:TStringList; i:integer; N,PN,LCAN,LCBN:String;
begin
  FilterComboBoxLinearConstraintBPopulating := true;
  LCAN:=''; LCBN:='';
  if FActiveControlPoint.LinearConstraintPointA<>nil then
    LCAN:=FActiveControlPoint.LinearConstraintPointA.Name;
  if FActiveControlPoint.LinearConstraintPointB<>nil then
    LCBN:=FActiveControlPoint.LinearConstraintPointB.Name;

  PL:=TFreeShip(FreeShip).GetAllNamedPoints;
   PL.Sort;
   FilterComboBoxLinearConstraintB.Clear;
   FilterComboBoxLinearConstraintB.ClearSelection;
   FilterComboBoxLinearConstraintB.AddItem('',nil);
   for i:=0 to PL.Count-1 do
     if (PL[i]<>FActiveControlPoint.Name) and (PL[i]<>LCAN) then
     begin
       FilterComboBoxLinearConstraintB.AddItem(PL[i],PL.Objects[i]);
       if (PL[i] = LCBN)
         then FilterComboBoxLinearConstraintB.ItemIndex:=
              FilterComboBoxLinearConstraintB.Items.Count-1;
     end;
   FilterComboBoxLinearConstraintBPopulating := false;
   PL.Free;
end;
}

procedure TFreeControlPointForm.CheckBoxCornerChange(Sender: TObject);
begin
   if FSkipCheckbox1OnClick then exit;
   FSetActiveControlPointCorner(CheckBoxCorner.Checked);
end;

procedure TFreeControlPointForm.ComboBox1Enter(Sender: TObject);
begin
end;


procedure TFreeControlPointForm.EditNameEditingDone(Sender: TObject);
var S: String;
    saved: Boolean;
begin
   saved := false;
   if not Self.Active then exit;
   //if not Self.Focused then exit;
   if EnteredControl <> EditName then exit;

   S:=trim(EditName.Text);
   if (ActiveControlPoint<>nil) and (ActiveControlPoint.Name <> S)  then
   if (S='') or (TFreeShip(FreeShip).FindByName(S) = nil) then
   begin
      EditName.Color:=clDefault;
      if not saved then
       begin
          TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointNameChanged,True);
          saved := true;
       end;

     ActiveControlPoint.Name := S;
     EditName.Text := S;

     if saved then
       begin
          TFreeShip(FreeShip).Build:=False;
          TFreeShip(FreeShip).FileChanged:=True;
          TFreeShip(FreeShip).Redraw;
          ActiveControlPoint:=ActiveControlPoint;
       end;
   end
   else
   begin
     EditName.Color:=clYellow;
     ShowMessage(rsNameIsNotUnique);
   end;
end;

procedure TFreeControlPointForm.FSetActiveControlPointCorner(isCorner: boolean);
var I,N     : Integer;
    OldType : TFreeVertexType;
    Undo    : TFreeUndoObject;
begin
  if (ActiveControlPoint<>nil)
    and (isCorner <> (ActiveControlPoint.VertexType=svCorner))
  then
   begin
      // Count the number of crease edges connected to this point
      OldType:=ActiveControlPoint.VertexType;
      Undo:=TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(213),false);
      if (ActiveControlPoint.Vertextype=svCorner) and (not isCorner)
      then
         begin
            N:=0;
            // Count the number of incident crease edges
            for I:=1 to ActiveControlPoint.NumberOfEdges do
                if FActiveControlPoint.Edge[I-1].Crease then inc(N);
            Case N of
               0 : ActiveControlPoint.Vertextype:=svRegular;
               1 : ActiveControlPoint.VertexType:=svDart;
               2 : ActiveControlPoint.VertexType:=svCrease;
               // points with more than two crease edges must always be a corner
            end;
         end;
      if (ActiveControlPoint.Vertextype<>svCorner) and (isCorner)
         then ActiveControlPoint.VertexType:=svCorner;

      if ActiveControlPoint.VertexType<>OldType then
      begin
         Undo.Accept;
         TFreeShip(FreeShip).Build:=False;
         TFreeShip(FreeShip).FileChanged:=True;
         TFreeShip(FreeShip).Redraw;
         ActiveControlPoint:=ActiveControlPoint;
      end else Undo.Delete;
   end;
end;{TFreeControlPointForm.CheckBox1MouseUp}

procedure TFreeControlPointForm.FormActivate(Sender: TObject);
begin
  FSkipCheckbox1OnClick:=false;
  EditX.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;
  EditY.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;
  EditZ.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;
  LabelAX.Caption:=cDegree;
  LabelAY.Caption:=cDegree;
  LabelAZ.Caption:=cDegree;
  Label1.Caption:=cAlpha;
  Label2.Caption:=cBeta;
  Label3.Caption:=cGamma;
end;

procedure TFreeControlPointForm.FormCreate(Sender: TObject);
begin
end;

procedure TFreeControlPointForm.FormShow(Sender: TObject);
begin

end;

procedure TFreeControlPointForm.SpeedButton1Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.X:=P.X+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton1Click}

procedure TFreeControlPointForm.SpeedButton4Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.X:=P.X-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton4Click}

procedure TFreeControlPointForm.SpeedButton2Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Y:=P.Y+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton2Click}

procedure TFreeControlPointForm.SpeedButton5Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Y:=P.Y-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton5Click}

procedure TFreeControlPointForm.SpeedButton3Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Z:=P.Z+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton3Click}

procedure TFreeControlPointForm.SpeedButton6Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Z:=P.Z-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton6Click}

procedure TFreeControlPointForm.SpeedButtonRemoveAnchorPointClick(
  Sender: TObject);
begin
   if ActiveControlPoint<>nil then
    begin
       TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointAnchorConstraintChanged,True);
       ActiveControlPoint.AnchorPoint:=nil;
       EditAnchorPoint.Text:='';
       TFreeShip(FreeShip).FileChanged:=True;
       TFreeShip(FreeShip).Redraw;
       ActiveControlPoint:=ActiveControlPoint;
    end;
end;

procedure TFreeControlPointForm.SpeedButtonRemoveLinearConstraintClick(
  Sender: TObject);
begin
  if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(rsPointLinearConstraintChanged,True);
      ActiveControlPoint.SetLinearConstraint(nil,nil);
      EditLinearConstraintA.Text:='';
      EditLinearConstraintB.Text:='';
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;

end.
