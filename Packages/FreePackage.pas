{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FreePackage;

interface

uses
  FreeShipUnit,
  FreeNumInput,
  FreeGeometry,
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FreeShipUnit', @FreeShipUnit.Register);
  RegisterUnit('FreeNumInput', @FreeNumInput.Register);
  RegisterUnit('FreeGeometry', @FreeGeometry.Register);
end;

initialization
  RegisterPackage('FreePackage', @Register);
end.
