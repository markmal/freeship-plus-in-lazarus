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
unit FreeMatrices;
{$mode objfpc}{$H+}

// Skip translation

interface

uses //Grids,
     Dialogs,
     Classes,
     SysUtils;

const Singulier=True;
      Regulier=false;
      MatrixError = 1e-5;

type TFreeMatrixError= class(Exception);
     TFreeMatrixType = Single;
     TFreeMatrixRow  = Array of TFreeMatrixType;
     TFreeMatrix     = class(TObject)
                          private
                             FRows        : array of TFreeMatrixRow;
                             FNoRows      : integer;
                             FNoColumns   : integer;
                             function FGetSquare:Boolean;
                             function FGetValue(Row,Col:Integer):TFreeMatrixType;
                             procedure FSetValue(Row,Col:Integer;Value:TFreeMatrixType);
                          public
                             procedure    Add(Matrix:TFreeMatrix);
                             procedure    Assign(Matrix:TFreeMatrix);
                             procedure    Clear;
                             function     Copy:TFreeMatrix;
                             constructor  Create;
                             procedure    CreateIdentity;
                             destructor   Destroy;                                 override;
                             procedure    DumpToFile(Filename:string;Length,Digits:Integer);
                             procedure    Fill(Value:TFreeMatrixType);
                             function     Invert:TFreeMatrix;
                             function     Multiply(Matrix:TFreeMatrix):TFreeMatrix;
                             procedure    SetSize(Cols,Rows:Integer);
                             procedure    Save(Strings:TStringList;Description:string);
                             function     Solve(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
                             function     Gauss(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
                             function     GaussJordan(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
                             procedure    Subtract(Matrix:TFreeMatrix);
                             procedure    Transpose;
                             property     ColCount                : integer read FNoColumns;
                             property     Square                  : Boolean read FGetSquare;
                             property     RowCount                : integer read FNoRows;
                             property     Value[Row,Col:Integer]  : TFreeMatrixType read FGetValue write FSetValue;
                        end;

implementation

{ ######################### TFreeMatrix ######################### }
procedure TFreeMatrix.Add(Matrix:TFreeMatrix);
var Source  : TFreeMatrixRow;
    Target  : TFreeMatrixRow;
    I,J     : integer;
begin
   Target:=nil;
   Source:=nil;
   if (Matrix.RowCount=RowCount) and (Matrix.ColCount=ColCount) then
   begin
      for I:=0 to RowCount-1 do
      begin
         Source:=Matrix.FRows[I];
         Target:=FRows[I];
         for J:=0 to ColCount-1 do Target[J]:=Target[J]+Source[J];
      end;
   end else Raise TFreeMatrixError.Create('Matrix size does not match in Add');
end;{TFreeMatrix.Add}

procedure TFreeMatrix.Assign(Matrix:TFreeMatrix);
var I,Size:Integer;
begin
   SetSize(Matrix.ColCount,Matrix.RowCount);
   Size:=ColCount*SizeOf(TFreeMatrixType);
   for I:=0 to RowCount-1 do Move(Matrix.FRows[I][0],FRows[I][0],Size);
end;{TFreeMatrix.Assign}

function TFreeMatrix.FGetSquare:Boolean;
begin
   Result:=(ColCount=RowCount) and (ColCount>0);
end;{TFreeMatrix.FGetSquare}

function TFreeMatrix.FGetValue(Row,Col:Integer):TFreeMatrixType;
begin
   if (Col>=0) and (Col<FNoColumns) and
      (Row>=0) and (Row<FNoRows) then Result:=FRows[Row][Col] else
   begin
      Raise TFreeMatrixError.Create('Error in accesing matrix');
   end;
end;{TFreeMatrix.FGetValue}

procedure TFreeMatrix.FSetValue(Row,Col:Integer;Value:TFreeMatrixType);
begin
   if (Col>=0) and (Col<FNoColumns) and
      (Row>=0) and (Row<FNoRows) then
      begin
         FRows[Row][Col]:=Value;
      end else Raise TFreeMatrixError.Create('Error in accesing matrix');
end;{TFreeMatrix.FSetValue}

procedure TFreeMatrix.Clear;
begin
   SetLength(FRows,0);
   FNoRows:=0;
   FNoColumns:=0;
end;{TFreeMatrix.Clear}

function TFreeMatrix.Copy:TFreeMatrix;
begin
   Result:=TFreeMatrix.Create;
   Result.Assign(Self);
end;{TFreeMatrix.Copy}

constructor TFreeMatrix.Create;
begin
   inherited Create;
   SetLength(FRows,0);
   FNoRows:=0;
   FNoColumns:=0;
   Clear;
end;{TFreeMatrix.Create}

procedure TFreeMatrix.CreateIdentity;
var i : integer;
begin
   if (RowCount=ColCount) and (RowCount>0) then
   begin
      Fill(0.0);
      for I:=0 to RowCount-1 do Value[I,I]:=1.0;
   end else Raise TFreeMatrixError.Create('Matrix must be square to create identity');
end;{TFreeMatrix.CreateIdentity}

destructor TFreeMatrix.Destroy;
begin
   Clear;
   inherited Destroy;
end;{TFreeMatrix.Destroy}

procedure TFreeMatrix.DumpToFile(Filename:string;Length,Digits:Integer);
var I,J:Integer;
    FFile:TextFile;
begin
   AssignFile(FFile,Filename);
   rewrite(FFile);
   Writeln(FFile);
   for I:=0 to RowCount-1 do
   begin
      For J:=0 to ColCount-1 do Write(FFile,Value[I,J]:Length:Digits);
      Writeln(FFile);
   end;
   CloseFile(FFile);
end;{TFreeMatrix.DumpToFile}

procedure TFreeMatrix.Fill(Value:TFreeMatrixType);
var i,j  : integer;
    Size : Integer;
    Row  : TFreeMatrixRow;
    Row0 : TFreeMatrixRow;
begin
   Size:=1;
   for I:=0 to RowCount-1 do
   begin
      Row:=FRows[I];
      if I=0 then
      begin
         for J:=0 to ColCount-1 do Row[J]:=Value;
         Size:=ColCount*SizeOf(TFreeMatrixType);
         Row0:=Row;
      end else
      begin
         // Copy entire contents of row[0] to row[I]
         Move(Row0[0],Row[0],Size);
      end;
   end;
end;{TFreeMatrix.Fill}

function TFreeMatrix.Invert:TFreeMatrix;
var I,J,K,L,N : integer;
    Factor  : extended;
    amax,h  : Double;
    State   : Boolean;
    IMax    : Integer;
    Det     :Extended;
    Inverted:TFreeMatrix;
    Back:TFreeMatrix;
begin
   Result:=nil;
   if Square then
   begin
      if ColCount=RowCount then
      begin
         Back:=Copy;
         Inverted:=TFreeMatrix.Create;
         Inverted.SetSize(ColCount,RowCount);
         Inverted.CreateIdentity;

         State:=Regulier;
         N:=ColCount;
         I:=0;
         Det:=1.0;
         repeat
            Inc(I);
            IMax:=I;
            AMax:=abs(Value[I-1,I-1]);
            if I<>N then for K:=I+1 to N do if Abs(Value[K-1,I-1])>AMax then
            begin
               IMax:=K;
               AMax:=abs(Value[K-1,I-1]);
            end;
            if AMax<MatrixError then State:=Singulier;
            if State=Regulier then
            begin
               if I<>Imax then // Swap rows if necessary
               begin
                  Det:=-Det;
                  for L:=I to N do
                  begin
                     H:=Value[I-1,L-1];
                     Value[I-1,L-1]:=Value[IMax-1,L-1];
                     Value[IMax-1,L-1]:=H;
                  end;
                  for L:=1 to N do
                  begin
                     H:=Inverted.Value[I-1,L-1];
                     Inverted.Value[I-1,L-1]:=Inverted.Value[IMax-1,L-1];
                     Inverted.Value[IMax-1,L-1]:=H;
                  end;
               end;
               // Sweep column clear
               if I<>N then
               begin
                  for K:=I+1 to N do if abs(Value[K-1,I-1])>MatrixError then
                  begin
                     Factor:=Value[k-1,I-1]/Value[I-1,I-1];
                     if Factor<>0 then
                     begin
                        for L:=I to N do Value[K-1,L-1]:=Value[K-1,L-1]-Factor*Value[I-1,L-1];
                        for L:=1 to N do Inverted.Value[K-1,L-1]:=Inverted.Value[K-1,L-1]-Factor*Inverted.Value[I-1,L-1];
                     end;
                  end;
               end;
               if I<>1 then for K:=I-1 downto 1 do if abs(Value[K-1,I-1])>MatrixError then
               begin
                  Factor:=Value[K-1,I-1]/Value[I-1,I-1];
                  if Factor<>0 then
                  begin
                     for L:=I to N do Value[K-1,L-1]:=Value[K-1,L-1]-Factor*Value[I-1,L-1];
                     for L:=1 to N do Inverted.Value[K-1,L-1]:=Inverted.Value[K-1,L-1]-Factor*Inverted.Value[I-1,L-1];
                  end;
               end;
            end;
         until (I=N) or (State=Singulier);
         if abs (Value[N-1,N-1])<MatrixError then State:=Singulier;
         if State=Regulier then
         begin
            for I:=1 to N do
            begin
               Det:=Det*Value[I-1,I-1];
               Factor:=Value[I-1,I-1];
               Value[I-1,I-1]:=1.0;
               for J:=1 to N do
               begin
                  Inverted.Value[I-1,J-1]:=Inverted.Value[I-1,J-1]/Factor;
               end;
            end;
            Result:=Inverted;
         end else
         begin
            //MessageDlg('Matrix could not be solved.',mtError,[mbok],0);
            Inverted.Destroy;
         end;
         Assign(Back);
         Back.Destroy;
      end else MessageDlg('Matrix size must match to be solved',mtError,[mbOk],0);
   end else MessageDlg('Matrix must be square to invert',mtError,[mbOK],0);
end;{TFreeMatrix.Invert}

function TFreeMatrix.Multiply(Matrix:TFreeMatrix):TFreeMatrix;
var i,j,k : integer;
begin
   if ColCount<>Matrix.RowCount then raise TFreeMatrixError.Create('Matrix size do not match in multiply') else
   begin
      Result:=TFreeMatrix.Create;
      Result.SetSize(Matrix.ColCount,RowCount);

      for I:=0 to Result.RowCount-1 do
      begin
         for J:=0 to Result.ColCount-1 do
         begin
            for K:=0 to ColCount-1 do
               Result.Value[I,J]:=Result.Value[I,J]+
                  Value[I,K]*Matrix.Value[K,J];
         end;
      end;
   end;
end;{TFreeMatrix.Multiply}

procedure TFreeMatrix.SetSize(Cols,Rows:Integer);
var i    : integer;
begin
   SetLength(FRows,Rows);
   FNoRows:=Rows;
   FNoColumns:=Cols;
   for I:=0 to RowCount-1 do Setlength(FRows[I],Colcount);
   // set all values to zero
   Fill(0.0);
end;{TFreeMatrix.SetSize}

procedure TFreeMatrix.Save(Strings:TStringList;Description:string);
var I,J:Integer;
    V:Single;
    Str,Tmp:String;
begin
   Strings.Add(Description);
   for I:=1 to RowCount do
   begin
     Str:='';
     for J:=1 to ColCount do
     begin
        V:=Value[I-1,J-1];
        Tmp:=FloatToStrF(V,ffFixed,7,5);
        while length(Tmp)<9 do Tmp:=#32+Tmp;
        Str:=Str+#32+Tmp;
     end;
     Strings.Add(Str);
   end;
end;{TFreeMatrix.Save}

function TFreeMatrix.Solve(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
const Singulier=True;
      Regulier=false;
var I,J,K,L,N : integer;
    Factor  : extended;
    amax,h  : Double;
    State   : Boolean;
    IMax    : Integer;
    Row     : TFreeMatrixRow;
begin
   Result:=False;
   Solution:=nil;
   if Square then
   begin
      if Matrix.RowCount=RowCount then
      begin
         N:=FNoRows;
         State:=Regulier;
         for I:=1 to N-1 do
         begin
            IMax:=I;
            AMax:=abs(Value[I-1,I-1]);

            for K:=I+1 to N do
            begin
               if abs(Value[I-1,K-1])>AMax then
               begin
                  IMax:=K;
                  AMax:=abs(Value[I-1,K-1]);
               end;
            end;

            if abs(AMax)<MatrixError then State:=Singulier;
            if State=regulier then
            begin
               if I<>Imax then
               begin
                  for L:=I to N do
                  begin
                     H:=Value[L-1,I-1];
                     Value[L-1,I-1]:=Value[L-1,IMax-1];
                     Value[L-1,Imax-1]:=H;
                  end;
                  Row:=Matrix.FRows[I-1];
                  Matrix.FRows[I-1]:=Matrix.FRows[IMax-1];
                  Matrix.FRows[IMax-1]:=Row;
               end;

               for K:=I+1 to N do if abs(Value[I-1,K-1])>MatrixError then
               begin
                  Factor:=Value[I-1,K-1]/Value[I-1,I-1];
                  for L:=1 to N do Value[L-1,K-1]:=Value[L-1,K-1]-factor*Value[L-1,I-1];
                  for L:=1 to Matrix.ColCount do
                  begin
                     Matrix.Value[L-1,K-1]:=Matrix.Value[L-1,K-1]-Factor*Matrix.Value[L-1,I-1];
                  end;
               end;
            end else
            begin
            end;
         end;

         if abs(Value[N-1,N-1])<MatrixError then State:=singulier;

         if State=regulier then
         begin

            Solution:=TFreeMatrix.Create;
            Solution.SetSize(Matrix.ColCount,N);
            for K:=0 to Solution.ColCount-1 do
            begin
               {
               Solution.Value[K,N-1]:=Matrix.Value[K,N-1]/Value[N-1,N-1];
               for L:=N-1 downto 1 do
               begin
                  Solution.Value[K,L-1]:=(1/Value[L-1,L-1])*(Matrix.Value[K,L-1]-Value[L-1,L-1+1]*Solution.Value[K,L-1+1]);
               end;
               }
               // According to testbook:
               Solution.Value[K,N-1]:=Matrix.Value[K,N-1]/Value[N-1,N-1];
               for L:=N-1 downto 1 do
               begin
                  Solution.Value[K,L-1]:=Matrix.Value[K,L-1];
                  for J:=L+1 to N do Solution.Value[K,L-1]:=Solution.Value[K,L-1]-Value[J-1,L-1]*Solution.Value[K,J-1];
                  Solution.Value[K,L-1]:=Solution.Value[K,L-1]/Value[L-1,L-1];
               end;

            end;
            Result:=True;
         end else MessageDlg('Matrix could not be solved.',mtError,[mbok],0);
      end else MessageDlg('Matrix size must match to be solved',mtError,[mbOk],0);
   end else MessageDlg('Matrix must be square to be solved',mtError,[mbOk],0);
end;{TFreeMatrix.Solve}

function TFreeMatrix.Gauss(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
const Singulier=True;
      Regulier=false;
var I,J,K,L,N : integer;
    Factor  : extended;
    amax,h  : Double;
    State   : Boolean;
    IMax    : Integer;
    Row     : TFreeMatrixRow;
begin
   Result:=False;
   Solution:=nil;
   if Square then
   begin
      if Matrix.RowCount=RowCount then
      begin
         N:=FNoRows;
         State:=Regulier;
         I:=0;
         Repeat
            Inc(I);
            IMax:=I;
            AMax:=abs(Value[I-1,I-1]);
            for K:=I+1 to N do
            begin
               if abs(Value[K-1,I-1])>AMax then
               begin
                  IMax:=K;
                  AMax:=abs(Value[K-1,I-1]);
               end;
            end;

            if AMax<MatrixError then State:=Singulier;
            if State=regulier then
            begin
               if I<>Imax then
               begin
                  for L:=I to N do
                  begin
                     H:=Value[I-1,L-1];
                     Value[I-1,L-1]:=Value[IMax-1,L-1];
                     Value[Imax-1,L-1]:=H;
                  end;
                  Row:=Matrix.FRows[I-1];
                  Matrix.FRows[I-1]:=Matrix.FRows[IMax-1];
                  Matrix.FRows[IMax-1]:=Row;
               end;

               for K:=I+1 to N do if abs(Value[K-1,I-1])>MatrixError then
               begin
                  Factor:=Value[K-1,I-1]/Value[I-1,I-1];
                  if Factor<>0 then
                  begin
                     for L:=I to N do Value[K-1,L-1]:=Value[K-1,L-1]-factor*Value[I-1,L-1];
                     for L:=1 to Matrix.ColCount do Matrix.Value[L-1,K-1]:=Matrix.Value[L-1,K-1]-Factor*Matrix.Value[L-1,I-1];
                  end;
               end;
            end;
         until (I=N-1) or (State=Regulier);

         if abs(Value[N-1,N-1])<MatrixError then State:=singulier;

         if State=regulier then
         begin
            Solution:=TFreeMatrix.Create;
            Solution.SetSize(Matrix.ColCount,N);
            for K:=0 to Solution.ColCount-1 do
            begin
               // According to testbook:
               Solution.Value[N-1,K]:=Matrix.Value[N-1,K]/Value[N-1,N-1];
               for L:=N-1 downto 1 do
               begin
                  Solution.Value[L-1,K]:=Matrix.Value[L-1,K];
                  for J:=L+1 to N do Solution.Value[L-1,K]:=Solution.Value[L-1,K]-Value[L-1,J-1]*Solution.Value[J-1,K];
                  Solution.Value[L-1,K]:=Solution.Value[L-1,K]/Value[L-1,L-1];
               end;
            end;
            Result:=True;
         end else MessageDlg('Matrix could not be solved.',mtError,[mbok],0);
      end else MessageDlg('Matrix size must match to be solved',mtError,[mbOk],0);
   end else MessageDlg('Matrix must be square to be solved',mtError,[mbOk],0);
end;{TFreeMatrix.Gauss}

function TFreeMatrix.GaussJordan(Matrix:TFreeMatrix;var Solution:TFreeMatrix):Boolean;
var I,J,K,N  : integer;
    Inverted : TFreeMatrix;
begin
   Result:=False;
   Solution:=nil;
   if Square then
   begin
      Inverted:=Self.Invert;
      if Inverted<>nil then
      begin
         N:=RowCount;
         Solution:=TFreeMatrix.Create;
         Solution.SetSize(Matrix.ColCount,Matrix.RowCount);
         for J:=1 to Solution.ColCount do
            for I:=1 to N do for K:=1 to N do Solution.Value[I-1,J-1]:=Solution.Value[I-1,J-1]+Inverted.Value[I-1,K-1]*Matrix.Value[K-1,J-1];
         Result:=True;
         Inverted.Destroy;
      end;
   end else MessageDlg('Matrix must be square to be solved',mtError,[mbOk],0);
end;{TFreeMatrix.GaussJordan}

procedure TFreeMatrix.Subtract(Matrix:TFreeMatrix);
var Source  : TFreeMatrixRow;
    Target  : TFreeMatrixRow;
    I,J     : integer;
begin
   if (Matrix.RowCount=RowCount) and (Matrix.ColCount=ColCount) then
   begin
      for I:=0 to RowCount-1 do
      begin
         Source:=Matrix.FRows[I];
         Target:=FRows[I];
         for J:=0 to ColCount-1 do Target[J]:=Target[J]-Source[J];
      end;
   end else Raise TFreeMatrixError.Create('Matrix size does not match in Subtract');
end;{TFreeMatrix.Subtract}

procedure TFreeMatrix.Transpose;
var I,J:Integer;
    Tmp:TFreeMatrix;
begin
   Tmp:=TFreeMatrix.Create;
   Tmp.SetSize(RowCount,ColCount);
   for I:=0 to ColCount-1 do
   begin
      for J:=0 to RowCount-1 do Tmp.Value[I,J]:=Value[J,I];
   end;
   Assign(Tmp);
   Tmp.Destroy;
end;{TFreeMatrix.Transpose}

end.

