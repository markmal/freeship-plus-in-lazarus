{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionBase                                    }
{ TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
{---------------------------------------------------------------------------------------------------}
constructor TFreeSubdivisionBase.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create;
  FOwner := Owner;
end;{TFreeSubdivisionBase.Create}

procedure TFreeSubdivisionBase.PrintDebug;
begin
  Logger.Debug( self.ClassName+'.FOwner:'+ IntToHex(QWord(FOwner),16));
end;

