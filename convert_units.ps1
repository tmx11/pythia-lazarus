# Lazarus Conversion Script

Write-Host "Converting Pythia units to Lazarus..." -ForegroundColor Cyan

# Convert all Source/*.pas files
$sourceFiles = Get-ChildItem "Source\*.pas"
foreach ($file in $sourceFiles) {
    Write-Host "Converting $($file.Name)..." -ForegroundColor Yellow
    $content = Get-Content $file.FullName -Raw
    
    # Replace System.* and Vcl.* prefixes
    $content = $content -creplace 'System\.SysUtils', 'SysUtils'
    $content = $content -creplace 'System\.Classes', 'Classes'
    $content = $content -creplace 'System\.IniFiles', 'IniFiles'
    $content = $content -creplace 'System\.IOUtils', ''
    $content = $content -creplace 'System\.JSON', 'fpjson, jsonparser'
    $content = $content -creplace 'System\.Net\.HttpClient', 'fphttpclient, opensslsockets'
    $content = $content -creplace 'System\.Net\.URLClient', ''
    $content = $content -creplace 'System\.Variants', 'Variants'
    $content = $content -creplace 'Winapi\.Windows', 'LCLIntf, LCLType'
    $content = $content -creplace 'Winapi\.Messages', 'LMessages'
    $content = $content -creplace 'Winapi\.ShellAPI', 'LCLIntf'
    $content = $content -creplace 'Vcl\.Graphics', 'Graphics'
    $content = $content -creplace 'Vcl\.Controls', 'Controls'
    $content = $content -creplace 'Vcl\.Forms', 'Forms'
    $content = $content -creplace 'Vcl\.Dialogs', 'Dialogs'
    $content = $content -creplace 'Vcl\.StdCtrls', 'StdCtrls'
    $content = $content -creplace 'Vcl\.ExtCtrls', 'ExtCtrls'
    $content = $content -creplace 'Vcl\.ComCtrls', 'ComCtrls'
    
    # Replace TPath and TDirectory
    $content = $content -creplace 'TPath\.Combine\(([^,]+),\s*([^)]+)\)', '$1 + PathDelim + $2'
    $content = $content -creplace 'TDirectory\.Exists', 'DirectoryExists'
    $content = $content -creplace 'TDirectory\.CreateDirectory', 'ForceDirectories'
    
    # Replace HTTP client
    $content = $content -creplace 'THttpClient', 'TFPHTTPClient'
    $content = $content -creplace 'TNetHeaders', 'TStringList'
    
    # Replace JSON
    $content = $content -creplace 'TJSONObject\.ParseJSONValue', 'GetJSON'
    $content = $content -creplace '\.GetValue\(([^)]+)\)\.Value', '.Strings[$1]'
    
    # Replace ToolsAPI with IDEIntf
    $content = $content -creplace 'ToolsAPI', 'LazIDEIntf, IDEWindowIntf, IDECommands, MenuIntf'
    
    # Replace TRichEdit with TMemo (comment out for now)
    $content = $content -creplace ': TRichEdit', ': TMemo  // was TRichEdit'
    
    Set-Content $file.FullName $content -NoNewline
}

Write-Host "Conversion complete!" -ForegroundColor Green
