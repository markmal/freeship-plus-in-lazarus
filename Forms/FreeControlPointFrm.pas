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
     StdCtrls, Buttons, ExtCtrls, Spin;

type

{ TFreeControlPointForm }

 TFreeControlPointForm  = class(TForm)
     EditName: TEdit;
    EditX: TFloatSpinEdit;
    EditAX: TFloatSpinEdit;
    EditAY: TFloatSpinEdit;
    EditAZ: TFloatSpinEdit;
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
    Label6: TLabel;
    LabelAZ: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label4: TLabel;
    EditDistance: TFloatSpinEdit;
    Label5: TLabel;
    EditAngles: TFloatSpinEdit;
    Panel3: TPanel;
    procedure CheckBoxCornerChange(Sender: TObject);
    procedure EditNameEditingDone(Sender: TObject);
    procedure EditXChange(Sender: TObject);
    procedure EditXEditingDone(Sender: TObject);
    procedure EditYChange(Sender: TObject);
    procedure EditYEditingDone(Sender: TObject);
    procedure EditZChange(Sender: TObject);
    procedure EditZEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
   private  { Private declarations }
      FActiveControlPoint  : TFreeSubdivisionControlPoint;
      FFreeShip            : TComponent;
      FSkipCheckbox1OnClick: boolean;
      FActiveControlPointChanging: boolean;
      procedure FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
      procedure FSetActiveControlPointCorner(isCorner: boolean);
   public   { Public declarations }
      property ActiveControlPoint   : TFreeSubdivisionControlPoint read FActiveControlPoint write FSetActiveControlPoint;
      property FreeShip             : TComponent read FFreeShip write FFreeShip;
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

procedure TFreeControlPointForm.FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
var I,N : Integer;
    BCol:TColor;
    FCol:TColor;
    Npoi: Integer;  
    R,R1,R2,alfa,beta,gamma,acos,len    : single;
    cos1a,cos2a,cos1b,cos2b,cos1g,cos2g : single;
    C0,CN,C1,C2,C3,Cp,Ci:T3DCoordinate;
begin
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
   end else
   begin
      R:=0.0;
      EditDistance.Value:=0.0;
      Npoi:=TFreeShip(FreeShip).NumberOfSelectedControlPoints-1;
      C0:=TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate;
      CN:=TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate;;
      if Npoi>= 1 then begin
          Label5.Caption:=Userstring(1477);
          R:=SQRT(Sqr(C0.X-CN.X)+Sqr(C0.Y-CN.Y)+Sqr(C0.Z-CN.Z));
          EditDistance.Value := R;
          EditAngles.Value:=0.0;
          end    
      else begin
          Label5.Caption:=Userstring(1477);
          EditDistance.Value:=0.0;
      end;

      if  Npoi>= 4 then begin
          //Label5.Caption:=Userstring(1477);
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
          Label5.Caption:=Userstring(1493);
          EditAngles.Value := len;
      end;

	  
      if R=0 then R:=0.0001;
      if  Npoi=1 then begin
        Label5.Caption:=Userstring(1477);
        alfa:=abs(C0.X-CN.X)/R;
        beta:=abs(C0.Y-CN.Y)/R;
        gamma:=abs(C0.Z-CN.Z)/R;
        EditAX.Value:=ArcCos(alfa)*57.29578;
        EditAY.Value:=ArcCos(beta)*57.29578;
        EditAZ.Value:=ArcCos(gamma)*57.29578;
        end;
      if Npoi=0 then begin
        Label5.Caption:=Userstring(1477); 
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
        Label5.Caption:=Userstring(1476);
        C1:=TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate;
        C2:=TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate;
        R1:=SQRT(Sqr(C0.X-C1.X)+Sqr(C0.Y-C1.Y)+Sqr(C0.Z-C1.Z));
        R2:=SQRT(Sqr(C1.X-C2.X)+Sqr(C1.Y-C2.Y)+Sqr(C1.Z-C2.Z));
        cos1a:=abs(C0.X-C1.X)/R1;
        cos1b:=abs(C0.Y-C1.Y)/R1;
        cos1g:=abs(C0.Z-C1.Z)/R1;
        cos2a:=abs(C1.X-C2.X)/R2;
        cos2b:=abs(C1.Y-C2.Y)/R2;
        cos2g:=abs(C1.Z-C2.Z)/R2;
        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g; 
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Value := ArcCos(acos)*57.29578;
      end;
      if  Npoi=3 then begin
        Label5.Caption:=Userstring(1476);
        C1:=TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate;
        C2:=TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate;
        C3:=TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate;
        R1:=SQRT(Sqr(C0.X-C1.X)+Sqr(C0.Y-C1.Y)+Sqr(C0.Z-C1.Z));
        R2:=SQRT(Sqr(C2.X-C3.X)+Sqr(C2.Y-C3.Y)+Sqr(C2.Z-C3.Z));
        cos1a:=(C0.X-C1.X)/R1;
        cos1b:=(C0.Y-C1.Y)/R1;
        cos1g:=(C0.Z-C1.Z)/R1;
        cos2a:=(C2.X-C3.X)/R2;
        cos2b:=(C2.Y-C3.Y)/R2;
        cos2g:=(C2.Z-C3.Z)/R2;
        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g; 
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Value := ArcCos(acos)*57.29578;
      end;
	  
      EditX.Value:=FActiveControlPoint.Coordinate.X;
      EditY.Value:=FActiveControlPoint.Coordinate.Y;
      EditZ.Value:=FActiveControlPoint.Coordinate.Z;

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

   end;
   FActiveControlPointChanging := false;
end;{TFreeControlPointForm.FSetActiveControlPoint}

procedure TFreeControlPointForm.EditXChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if FActiveControlPointChanging then exit;
   if not(EditX.Focused and EditX.Enabled and not EditX.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
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
   if not(EditY.Focused and EditY.Enabled and not EditY.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
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
   if not(EditZ.Focused and EditZ.Enabled and not EditZ.ReadOnly) then exit;
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
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
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditX.Text, P.X);
      if (abs(P.X-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
         I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditX.Text, P.X);
            if abs(P.X-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(210),True);
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
            Val:= ConvertCoordinate(EditX.Text, P.X);
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

procedure TFreeControlPointForm.EditYEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditY.Text, P.Y);
      if (abs(P.Y-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditY.Text, P.Y);
            if abs(P.Y-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(211),True);
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
            Val:= ConvertCoordinate(EditY.Text, P.Y);
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
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditZ.Text, P.Z);
      if (abs(P.Z-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditZ.Text, P.Z);
            if abs(P.Z-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(212),True);
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
            Val:=ConvertCoordinate(EditZ.Text, P.Z);
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

procedure TFreeControlPointForm.CheckBoxCornerChange(Sender: TObject);
begin
   if FSkipCheckbox1OnClick then exit;
   FSetActiveControlPointCorner(CheckBoxCorner.Checked);
end;

resourcestring rsPointNameChanged = 'Point Name Changed';

procedure TFreeControlPointForm.EditNameEditingDone(Sender: TObject);
var S: String;
    saved: Boolean;
begin
   saved := false;
   S:=trim(EditName.Text);
   if (ActiveControlPoint<>nil) and (ActiveControlPoint.Name <> S)  then
   begin
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

end.
