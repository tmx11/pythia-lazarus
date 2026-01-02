unit Pythia.Config;
interface

uses
  SysUtils, Classes, IniFiles;

type
  TPythiaConfig = class
  private
    class function GetIniFile: TIniFile;
  public
    class function GetConfigPath: string;
    class function GetOpenAIKey: string;
    class procedure SetOpenAIKey(const Value: string);
    class function GetAnthropicKey: string;
    class procedure SetAnthropicKey(const Value: string);
    class function GetDefaultModel: string;
    class procedure SetDefaultModel(const Value: string);
  end;

implementation

{ TPythiaConfig }

class function TPythiaConfig.GetConfigPath: string;
var
  AppDataPath: string;
begin
  AppDataPath := GetEnvironmentVariable('APPDATA') + '\Pythia';
  if not DirectoryExists(AppDataPath) then
    ForceDirectories(AppDataPath);
  Result := AppDataPath + '\pythia.ini';
end;

class function TPythiaConfig.GetIniFile: TIniFile;
begin
  Result := TIniFile.Create(GetConfigPath);
end;

class function TPythiaConfig.GetOpenAIKey: string;
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Result := Ini.ReadString('API', 'OpenAIKey', '');
  finally
    Ini.Free;
  end;
end;

class procedure TPythiaConfig.SetOpenAIKey(const Value: string);
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Ini.WriteString('API', 'OpenAIKey', Value);
  finally
    Ini.Free;
  end;
end;

class function TPythiaConfig.GetAnthropicKey: string;
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Result := Ini.ReadString('API', 'AnthropicKey', '');
  finally
    Ini.Free;
  end;
end;

class procedure TPythiaConfig.SetAnthropicKey(const Value: string);
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Ini.WriteString('API', 'AnthropicKey', Value);
  finally
    Ini.Free;
  end;
end;

class function TPythiaConfig.GetDefaultModel: string;
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Result := Ini.ReadString('Settings', 'DefaultModel', 'gpt-4');
  finally
    Ini.Free;
  end;
end;

class procedure TPythiaConfig.SetDefaultModel(const Value: string);
var
  Ini: TIniFile;
begin
  Ini := GetIniFile;
  try
    Ini.WriteString('Settings', 'DefaultModel', Value);
  finally
    Ini.Free;
  end;
end;

end.
