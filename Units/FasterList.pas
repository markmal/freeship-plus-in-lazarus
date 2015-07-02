{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : marven@users.sourceforge.net                                   }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : http://homepages.ipact.nl/~martijn                             }
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
{    If the program is interactive, make it output a short notice like this when              }
{    it starts in an interactive mode:                                                        }
{                                                                                             }
{    Gnomovision version 69, Copyright (C) year name of author Gnomovision comes              }
{    with ABSOLUTELY NO WARRANTY; for details type `show w'. This is free software,           }
{    and you are welcome to redistribute it under certain conditions; type `show c'           }
{    for details.                                                                             }
{#############################################################################################}

unit FasterList;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses math;

type {---------------------------------------------------------------------------------------------------}
     {                                       TFasterList                                                 }
     {                                                                                                   }
     {   TFasterlist is an improved version of Delphi's standard TList object.                           }
     {   It is stripped of unneccesary code to improve speed.                                            }
     {   Additionally the contents (pointers) can be sorted so that the time needed to search an item    }
     {   has been redced with a factor 2^n                                                               }
     {                                                                                                   }
     {   Use the SORT            method to sort the list                                                 }
     {   Use the SORTEDINDEXOF() function to perform a binary search in the sorted list                  }
     {---------------------------------------------------------------------------------------------------}
      TFasterList        = class
                              private
                                 FCount      : Integer;
                                 FCapacity   : Integer;
                                 FSorted     : Boolean;
                                 FUseUserData: Boolean;
                                 FList       : array of Pointer;
                                 FData       : array of Pointer; // Pointers to any user specified object
                                 function FGet(Index: Integer): Pointer;
                                 function FGetObject(Index: Integer): Pointer;
                                 function FGetMemory:Integer;
                                 procedure FGrow;
                                 procedure FSet(Index: Integer; Item: Pointer);
                                 procedure FSetObject(Index: Integer; Item: Pointer);
                                 procedure FSetCapacity(NewCapacity: Integer);
                              public
                                 procedure Add(Item:Pointer);
                                 procedure AddList(List:TFasterList);
                                 procedure AddObject(Item,UserObject: Pointer);
                                 procedure AddSorted(Item:Pointer);
                                 procedure AddSortedObject(Item,UserObject:Pointer);
                                 procedure Assign(List:TFasterList);
                                 constructor Create;
                                 procedure Clear;                                                        virtual;
                                 destructor Destroy;                                                     override;
                                 procedure Delete(Index: Integer);
                                 procedure Exchange(Index1, Index2: Integer);
                                 function IndexOf(Item: Pointer): Integer;                               // normal TList function
                                 procedure Insert(Index: Integer; Item: Pointer);
                                 procedure Sort;
                                 function SortedIndexOf(Item: Pointer): Integer;
                                 property Capacity             : Integer read FCapacity write FSetCapacity;
                                 property Count                : Integer read FCount;
                                 property Items[Index:Integer] : Pointer read FGet write FSet; default;
                                 property Memory               : Integer read FGetMemory;
                                 property Objects[Index:Integer] : Pointer read FGetObject write FSetObject;
                              end;

implementation

{--------------------------------------------------------------------------------------------------}
{                                       TFasterList                                                }
{--------------------------------------------------------------------------------------------------}
destructor TFasterList.Destroy;
begin
   Clear;
   inherited Destroy;
end;{TFasterList.Destroy}

procedure TFasterList.Add(Item: Pointer);
var Cur,Prev:PtrUInt;
begin
   if FCount=FCapacity then FGrow;
   FList[FCount]:=Item;
   if FUseUserData then FData[FCount]:=nil;
   if FCount<1 then FSorted:=True
   else if FSorted then
     begin
        Prev:=PtrUInt(FList[FCount-1]);
        Cur:=PtrUInt(Item);
        FSorted:=Prev<Cur;
     end;
   Inc(FCount);
end;{TFasterList.Add}

procedure TFasterList.AddObject(Item,UserObject: Pointer);
var Cur,Prev:PtrUInt;
begin
   if FCount=FCapacity then FGrow;
   if not FUseUserdata then
   begin
      Setlength(FData,FCapacity);
      FUseUserdata:=True;
   end;

   FList[FCount]:=Item;
   FData[FCount]:=UserObject;
   if FCount<1 then FSorted:=True
   else if FSorted then
     begin
        Prev:=PtrUInt(FList[FCount-1]);
        Cur:=PtrUInt(Item);
        FSorted:=Prev<Cur;
     end;
   Inc(FCount);
end;{TFasterList.Add}

procedure TFasterList.AddList(List:TFasterList);
var NewCap:Integer;
begin
   if List.FCount = 0 then exit;
   NewCap:=FCount+List.FCount;
   FUseUserData:=FUseUserData or List.FUseUserData;
   if NewCap>FCapacity then
   begin
      FCapacity:=NewCap;
      Setlength(FList,FCapacity);
      if FUseUserData then Setlength(FData,FCapacity);
   end;
   Move(List.FList[0],FList[FCount],List.FCount*SizeOf(Pointer));
   if FUseUserData then Move(List.FData[0],FData[FCount],List.FCount*SizeOf(Pointer));
   FSorted:=False;
   Inc(FCount,List.FCount);
end;{TFasterList.AddList}

procedure TFasterList.AddSorted(Item:Pointer);
var Address : PtrUInt;
    L,H,Mid : Integer;
begin
   if FCount=FCapacity then FGrow;
   if FCount=0 then
   begin
      FList[FCount]:=Item;
      if FUseUserData then FData[FCount]:=nil;
      Inc(FCount);
   end else
   begin
      Address:=PtrUInt(Item);
      // check start
      if Address<PtrUInt(FList[0]) then
      begin
         // insert at start
         Move(FList[0],FList[1],FCount*SizeOf(Pointer));
         FList[0]:=Item;
         if FUseUserData then
         begin
            Move(FData[0],FData[1],FCount*SizeOf(Pointer));
            FData[0]:=nil;
         end;
      end else if Address>PtrUInt(FList[FCount-1]) then
      begin
         // add at end
         FList[FCount]:=Item;
         if FUseUserdata then FData[FCount]:=nil;
      end else
      begin
         // perform binary search to quickly find the location
         L:=0;
         H:=FCount-1;
         while H-L>1 do
         begin
            Mid:=Floor(0.5*(L+H));
            if Address<PtrUInt(FList[Mid]) then H:=Mid-1
                                            else L:=Mid+1;
         end;
         if Address<PtrUInt(FList[L]) then Mid:=L else
            if Address<PtrUInt(FList[H]) then Mid:=H else
               Mid:=H+1;
         Move(FList[Mid],FList[Mid+1],(FCount-Mid)*SizeOf(Pointer));
         FList[Mid]:=Item;
         if FUseUserdata then
         begin
            Move(FData[Mid],FData[Mid+1],(FCount-Mid)*SizeOf(Pointer));
            FData[Mid]:=nil;
         end;
      end;
      Inc(FCount);
   end;
end;{TFasterList.AddSorted}

procedure TFasterList.AddSortedObject(Item,UserObject:Pointer);
var Address : PtrUInt;
    L,H,Mid : Integer;
begin
   if not FUseUserData then
   begin
      FUseUserData:=True;
      Setlength(FData,FCapacity);
   end;

   if FCount=FCapacity then FGrow;
   if FCount=0 then
   begin
      FList[FCount]:=Item;
      if FUseUserData then FData[FCount]:=UserObject;
      Inc(FCount);
   end else
   begin
      Address:=PtrUInt(Item);
      // check start
      if Address<PtrUInt(FList[0]) then
      begin
         // insert at start
         Move(FList[0],FList[1],FCount*SizeOf(Pointer));
         FList[0]:=Item;
         if FUseUserData then
         begin
            Move(FData[0],FData[1],FCount*SizeOf(Pointer));
            FData[0]:=UserObject;
         end;
      end else if Address>PtrUInt(FList[FCount-1]) then
      begin
         // add at end
         FList[FCount]:=Item;
         if FUseUserdata then FData[FCount]:=UserObject;
      end else
      begin
         // perform binary search to quickly find the location
         L:=0;
         H:=FCount-1;
         while H-L>1 do
         begin
            Mid:=Floor(0.5*(L+H));
            if Address<PtrUInt(FList[Mid]) then H:=Mid-1
                                            else L:=Mid+1;
         end;
         if Address<PtrUInt(FList[L]) then Mid:=L else
            if Address<PtrUInt(FList[H]) then Mid:=H else
               Mid:=H+1;
         Move(FList[Mid],FList[Mid+1],(FCount-Mid)*SizeOf(Pointer));
         FList[Mid]:=Item;
         if FUseUserdata then
         begin
            Move(FData[Mid],FData[Mid+1],(FCount-Mid)*SizeOf(Pointer));
            FData[Mid]:=UserObject;
         end;
      end;
      Inc(FCount);
   end;
end;{TFasterList.AddSortedObject}

procedure TFasterList.Assign(List:TFasterList);
begin
   FUseUserdata:=List.FUseUserData;
   Capacity:=List.Count;
   Move(List.FList[0],FList[0],List.Count*SizeOf(Pointer));
   if FUseUserdata then Move(List.FData[0],FData[0],List.Count*SizeOf(Pointer));
   FCount:=List.Count;
   FSorted:=List.FSorted;
end;{TFasterList.Assign}

constructor TFasterList.Create;
begin
   Inherited Create;
   FCount:=0;
   FCapacity:=0;
   FSorted:=False;
   FList:=nil;
   FData:=nil;
end;{TFasterList.Create}

procedure TFasterList.Clear;
begin
   FCount:=0;
   FSetCapacity(0);
   FSorted:=False;
   FUseUserData:=False;
end;{TFasterList.Clear}

procedure TFasterList.Delete(Index: Integer);
begin
   Dec(FCount);
   if Index<FCount then
   begin
      Move(FList[Index+1],FList[Index],(FCount-Index)*SizeOf(Pointer));
      if FUseUserData then Move(FData[Index+1],FData[Index],(FCount-Index)*SizeOf(Pointer));
   end;
end;{TFasterList.Delete}

procedure TFasterList.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
   Item:=FList[Index1];
   FList[Index1]:=FList[Index2];
   FList[Index2]:=Item;
   if FUseUserData then
   begin
      Item:=FData[Index1];
      FData[Index1]:=FData[Index2];
      FData[Index2]:=Item;
   end;
   if FSorted then FSorted:=False;
end;{TFasterList.Exchange}

function TFasterList.FGet(Index: Integer): Pointer;
begin
   if (Index>=0) and (Index<FCount) then Result:=FList[Index]
                                    else Result:=nil;
end;{TFasterList.FGet}

function TFasterList.FGetObject(Index: Integer): Pointer;
begin
   if (Index>=0) and (Index<FCount) and (FUseUserData) then Result:=FData[Index]
                                                       else Result:=nil;
end;{TFasterList.FGetObject}

function TFasterList.FGetMemory:Integer;
begin
   Result:=SizeOf(Pointer)+           // self        : pointer
           SizeOf(Integer)+           // FCapacity   : integer
           SizeOf(Integer)+           // fcount      : integer
           SizeOf(Boolean)+           // FSorted     : Boolean
           SizeOf(Boolean)+           // FUseUserdata: boolean
           SizeOf(Pointer)*FCapacity;
   if FUseUserData then inc(Result,FCapacity*SizeOf(Pointer));
end;{TFasterList.FGetMemory}

procedure TFasterList.FGrow;
var Delta : Integer;
begin
  if FCapacity>64 then
  begin
     Delta:=FCapacity div SizeOf(Pointer);
     if Delta>1024 then Delta:=1024;
  end else
    if FCapacity>8 then Delta:=16 else
      Delta:=4;
  FSetCapacity(FCapacity + Delta);
end;{TFasterList.FGrow}

function TFasterList.IndexOf(Item: Pointer): Integer;
var I : Integer;
begin
   Result:=-1;
   for I:=1 to FCount do
   if FList[I-1]=Item then
     begin
        Result:=I-1;
        break;
     end;
end;{TFasterList.IndexOf}

procedure TFasterList.Insert(Index: Integer; Item: Pointer);
begin
   if FCount=FCapacity then FGrow;
   if Index<FCount then
   begin
      Move(FList[Index],FList[Index + 1],(FCount-Index)*SizeOf(Pointer));
      if FUseUserData then Move(FData[Index],FData[Index + 1],(FCount-Index)*SizeOf(Pointer));
   end;
   FList[Index]:=Item;
   if FUseUserData then FData[index]:=nil;
   if FSorted then FSorted:=False;
   Inc(FCount);
end;{TFasterList.Insert}

procedure TFasterList.Sort;

   procedure QuickSort(L,R:Integer);
   var I, J : Integer;
       Val  : PtrUInt;

       Procedure Swap(I,J:Integer);
       var Tmp : Pointer;
       begin
          Tmp:=FList[I];
          FList[I]:=FList[J];
          FList[J]:=Tmp;
          if FUseUserdata then
          begin
            Tmp:=FData[I];
            FData[I]:=FData[J];
            FData[J]:=Tmp;
          end;
       end;

   begin
      I:=L;
      J:=R;
      Val:=PtrUInt(FList[(L+R) div 2]);
      repeat
	      While PtrUInt(FList[I])<Val do Inc(I);
	      while Val<PtrUInt(FList[J]) do Dec(J);
         if I<=J then
         begin
            Swap(I,J);
            Inc(I);
            Dec(J);
         end;
      Until I>J;
      if L<J then QuickSort(L,J);
      if I<R then QuickSort(I,R);
   end;{QuickSort}

begin
   if (FCount>1) then {and not (FSorted) then}
   begin
      QuickSort(0,FCount-1);
      FSorted:=True;
   end;
end;{TFasterList.Sort}

function TFasterList.SortedIndexOf(Item: Pointer): Integer;
var MemAddr : PtrUInt;
    MidVal  : PtrUInt;
    L,H,Mid : Integer;
begin
   Result:=-1;
   MemAddr:=PtrUInt(Item);
   L:=0;
   H:=FCount-1;
   while L<=H do
   begin
      Mid:=Floor(0.5*(L+H));
      MidVal:=PtrUInt(FList[Mid]);
      if MemAddr=MidVal then
      begin
         Result:=Mid;
         exit;
      end else
      begin
         if MemAddr<MidVal then
         begin
            H:=Mid-1;
         end else
         begin
            L:=Mid+1;
         end;
      end;
   end;
end;{TFasterList.SortedIndexOf}

procedure TFasterList.FSet(Index: Integer; Item: Pointer);
begin
   FList[Index]:=Item;
   if FUseUserData then FData[index]:=nil;
end;{TFasterList.FSet}

procedure TFasterList.FSetObject(Index: Integer; Item: Pointer);
begin
   if not FUseUserdata then
   begin
      Setlength(FData,FCapacity);
      FUseUserdata:=True;
   end;
   FData[Index]:=Item;
end;{TFasterList.FSetObject}

procedure TFasterList.FSetCapacity(NewCapacity: Integer);
begin
   Setlength(FList,NewCapacity);
   if FUseUserData then Setlength(FData,NewCapacity);
   FCapacity := NewCapacity;
   if FCapacity<FCount then FCount:=Fcapacity;
end;{TFasterList.FSetCapacity}

end.
