{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2015, by Mark Malakanov                                                      }
{    FREE!ship Plus for Lazarus project page: https://code.google.com/p/freeship-plus-in-lazarus/  }
{    FREE!ship Plus homepage      : http://www.hydronship.net                                      }
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

unit FreeAboutDlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Messages,
  LCLIntf, LCLType, LMessages,
  StdCtrls, ValEdit, Grids,
  FreeVersionUnit;

type

  { TFreeAboutDlg }

  TFreeAboutDlg = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelFreeShipProjectLink: TLabel;
    Label7: TLabel;
    LabelFreeShipHomeLink: TLabel;
    LabelProductVersion: TLabel;
    LabelTitle: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    StringGridCopyrights: TStringGrid;
    StringGridVersionInfo: TStringGrid;
    StringGridCredits: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure LabelFreeShipHomeLinkClick(Sender: TObject);
    procedure LabelFreeShipProjectLinkClick(Sender: TObject);
  private
    { private declarations }
    procedure AddVersionInfo(sName,sValue:String);
    procedure AddCredits(sName,sNote:String);
    procedure AddCopyrights(sLine:String);
  public
    { public declarations }
  end;

var
  FreeAboutDlg: TFreeAboutDlg;

implementation

{$R *.lfm}

{ TFreeAboutDlg }

procedure TFreeAboutDlg.AddVersionInfo(sName,sValue:String);
begin
   with StringGridVersionInfo do
   begin
   RowCount := RowCount + 1;
   Cells[0,RowCount-1]:=sName;
   Cells[1,RowCount-1]:=sValue;
   end;
end;

procedure TFreeAboutDlg.AddCredits(sName,sNote:String);
begin
   with StringGridCredits do
   begin
   RowCount := RowCount + 1;
   Cells[0,RowCount-1]:=sName;
   Cells[1,RowCount-1]:=sNote;
   end;
end;

procedure TFreeAboutDlg.AddCopyrights(sLine:String);
begin
   with StringGridCopyrights do
   begin
   RowCount := RowCount + 1;
   Cells[0,RowCount-1]:=sLine;
   end;
end;

procedure TFreeAboutDlg.FormCreate(Sender: TObject);
begin
  LabelProductVersion.Caption := LabelProductVersion.Caption+ResourceVersionInfo;

  //StringGridVersionInfo.ColCount:=2;
  //StringGridVersionInfo.RowCount:=0;
  AddVersionInfo('Product version',FREESHIP_VERSION);
  AddVersionInfo('Program version',ResourceVersionInfo);
  AddVersionInfo('SVN Revision',IntToStr(SUBVERSION_REVISION));
  AddVersionInfo('Build date',COMPILE_DATE+' '+COMPILE_TIME);
  AddVersionInfo('Compiler version',{$I %FPCVERSION%});
  AddVersionInfo('Target CPU',TARGET_CPU);
  AddVersionInfo('Target OS',TARGET_OS);

  //StringGridCredits.ColCount:=2;
  //StringGridCredits.RowCount:=0;
  AddCredits('Initial Author','Martijn van Engeland, 2005');
  AddCredits('Author','Timoshenko Victor F, from 2007');
  AddCredits('Translation','Martijn van Engeland');
  AddCredits('Conversion to FPC','Mark Malakanov, from 2015');
  AddCredits('New scalable icons','Tom (https://github.com/tvld), from 2015');
  AddCredits('Improvements, fixing issues','Sönke J. Peters, from 2015');

  AddCopyrights('Copyright © 2005, by Martijn van Engeland');
  AddCopyrights('Copyright © 2007-2012, by Timoshenko Victor F.');
  AddCopyrights('Copyright © 2015, by Mark Malakanov');

end;

procedure TFreeAboutDlg.LabelFreeShipHomeLinkClick(Sender: TObject);
begin
  OpenURL(LabelFreeShipHomeLink.Caption);
end;

procedure TFreeAboutDlg.LabelFreeShipProjectLinkClick(Sender: TObject);
begin
  OpenURL(LabelFreeShipProjectLink.Caption);
end;

end.

