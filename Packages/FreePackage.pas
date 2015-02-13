{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FreePackage;

interface

uses
  FreeGeometry, FreeShipUnit, FreeNumInput, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FreeGeometry', @FreeGeometry.Register);
  RegisterUnit('FreeShipUnit', @FreeShipUnit.Register);
  RegisterUnit('FreeNumInput', @FreeNumInput.Register);
end;

initialization
  RegisterPackage('FreePackage', @Register);
end.
