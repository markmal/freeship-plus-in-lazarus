{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionBase                                    }
{ TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
{---------------------------------------------------------------------------------------------------}

constructor TFreeSubdivisionBase.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  FSurface := Owner;
  InUnreference := false;
  IsUnreferenceEnabled := true;
end;{TFreeSubdivisionBase.Create}

procedure TFreeSubdivisionBase.PrintDebug;
begin
  //Logger.Debug( self.ClassName+'.FOwner:'+ IntToHex(QWord(FSurface),16));
  Logger.Debug( self.ClassName+':'+IntToStr(Id));
end;

