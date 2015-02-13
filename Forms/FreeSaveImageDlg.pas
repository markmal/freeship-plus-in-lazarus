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

unit FreeSaveImageDlg;

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
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
     ExtCtrls;

type TSaveImageDialog = class(TForm)
                              Panel2: TPanel;
    Label3: TLabel;
    Label6: TLabel;
                              Edit1: TEdit;
                              Edit2: TEdit;
                              Panel1: TPanel;
                              Panel3: TPanel;
                              Label1: TLabel;
                              Label2: TLabel;
                              Label4: TLabel;
                              _Label5: TLabel;
                              Label7: TLabel;
                              Edit3: TEdit;
                              SpeedButton1: TSpeedButton;
                              SaveDialog: TSaveDialog;
                              BitBtn1: TSpeedButton;
                              BitBtn2: TSpeedButton;
                              procedure Edit1KeyPress(Sender: TObject; var Key: Char);
                              procedure Edit1Exit(Sender: TObject);
                              procedure Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
                              procedure Edit2Exit(Sender: TObject);
                              procedure SpeedButton1Click(Sender: TObject);
                              procedure BitBtn1Click(Sender: TObject);
                              procedure BitBtn2Click(Sender: TObject);
                           private   { Private declarations }
                              FRatio   : single;
                              function FGetFilename:string;
                              function FGetImageWidth:integer;
                              procedure FSetImageWidth(val:integer);
                              procedure FSetFilename(val:string);
                              function FGetImageHeight:integer;
                              procedure FSetImageHeight(val:integer);
                           public    { Public declarations }
                              function Execute:Boolean;
                              procedure SetImageSize;
                              property Filename:string read FGetFilename write FSetFilename;
                              property ImageWidth:integer read FGetImageWidth write FSetImageWidth;
                              property ImageHeight:integer read FGetImageHeight write FSetImageHeight;
                        end;
var SaveImageDialog:TSaveImageDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSaveImageDialog.SetImageSize;
var Size : single;
begin
   Size:=ImageWidth*ImageHeight*3/1024;
   // Skip translation
   if size>1024 then _Label5.caption:=FloatToStrF(Size/1024,ffFixed,7,2)+' MB'
                else _Label5.caption:=FloatToStrF(Size,ffFixed,7,0)+' KB'
   // End Skip translation
end;{TSaveImageDialog.SetImageSize}

function TSaveImageDialog.FGetFilename:string;
begin
   Result:=Edit3.Text;
end;{TSaveImageDialog.FGetFilename}

function TSaveImageDialog.FGetImageWidth:integer;
begin
   if Edit1.Text='' then result:=1
                    else Result:=StrToInt(Edit1.Text);
end;{TSaveImageDialog.FGetImageWidth}

procedure TSaveImageDialog.FSetImageWidth(val:integer);
begin
   Edit1.Text:=IntToStr(Val);
   SetImageSize;
end;{TSaveImageDialog.FSetImageWidth}

procedure TSaveImageDialog.FSetFilename(val:string);
begin
   Edit3.Text:=val;
end;{TSaveImageDialog.FSetFilename}

function TSaveImageDialog.FGetImageHeight:integer;
begin
   if Edit2.Text='' then result:=1
                    else Result:=StrToInt(Edit2.Text);
end;{TSaveImageDialog.FGetImageHeight}

procedure TSaveImageDialog.FSetImageHeight(val:integer);
begin
   Edit2.Text:=IntToStr(Val);
   SetImageSize;
end;{TSaveImageDialog.FSetImageHeight}

function TSaveImageDialog.Execute:Boolean;
begin
   FRatio:=Imagewidth/ImageHeight;
   Showmodal;
   Result:=ModalResult=mrOk;
end;{TSaveImageDialog.Execute}

procedure TSaveImageDialog.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
end;{TSaveImageDialog.Edit1KeyPress}

procedure TSaveImageDialog.Edit1Exit(Sender: TObject);
begin
   ImageWidth:=self.ImageWidth;
   ImageHeight:=Round(Imagewidth/FRatio);
end;{TSaveImageDialog.Edit1Exit}

procedure TSaveImageDialog.Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
   if Key=13 then SelectNext(Activecontrol,True,true);
end;{TSaveImageDialog.Edit1KeyDown}

procedure TSaveImageDialog.Edit2Exit(Sender: TObject);
begin
  inherited;
  ImageHeight:=self.ImageHeight;
  ImageWidth:=Round(FRatio*ImageHeight);
end;{TSaveImageDialog.Edit2Exit}

procedure TSaveImageDialog.SpeedButton1Click(Sender: TObject);
begin
   SaveDialog.FileName:=Filename;
   if SaveDialog.Execute then Filename:=SaveDialog.FileName;
end;{TSaveImageDialog.SpeedButton1Click}

procedure TSaveImageDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TSaveImageDialog.BitBtn1Click}

procedure TSaveImageDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TSaveImageDialog.BitBtn2Click}

end.