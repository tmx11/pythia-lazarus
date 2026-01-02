{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pythia;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pythia.Register, Pythia.ChatForm, Pythia.Config, Pythia.Context, 
  Pythia.AI.Client, Pythia.GitHub.Auth, Pythia.SettingsForm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Pythia.Register', @Pythia.Register.Register);
end;

initialization
  RegisterPackage('pythia', @Register);
end.
