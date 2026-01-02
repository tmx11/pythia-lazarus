# Pythia Test Tools

Command-line utilities for testing Pythia AI functionality.

## Building

```powershell
cd tools
.\build-tests.ps1
```

Or use make:

```cmd
make all
```

## Usage

### Check GitHub Copilot Authentication

```powershell
.\test_github_auth.exe
```

Shows:
- Authentication status
- Token (masked)
- Config file location

### Show Configuration

```powershell
.\show_config.exe
```

Displays:
- Config file path
- API key status (masked)
- GitHub authentication status
- Full config file contents (with sensitive data masked)

### Test API Connections

```powershell
.\test_connection.exe
```

Tests all configured APIs:
- GitHub Copilot (GPT-4, GPT-3.5)
- OpenAI (GPT-4, GPT-3.5 Turbo)
- Anthropic (Claude 3.5 Sonnet)

Each test sends "Reply with only: OK" and checks the response.

### Send Chat Message

```powershell
# GitHub Copilot (FREE!)
.\test_chat.exe copilot-gpt4 "What is Delphi?"

# OpenAI
.\test_chat.exe gpt4 "Explain Free Pascal"

# Anthropic
.\test_chat.exe claude-sonnet "What are Pascal records?"
```

Available models:
- `copilot-gpt4` - GitHub Copilot: GPT-4
- `copilot-gpt35` - GitHub Copilot: GPT-3.5 Turbo
- `gpt4` - OpenAI GPT-4
- `gpt35` - OpenAI GPT-3.5 Turbo
- `claude-sonnet` - Claude 3.5 Sonnet
- `claude-opus` - Claude 3 Opus

## Configuration

Test tools use the same config file as the IDE package:
- Windows: `%APPDATA%\Pythia\pythia.ini`

To configure API keys:
1. Run Lazarus IDE
2. Open Pythia chat window (View → IDE Internals Windows → Pythia AI Chat)
3. Click "Settings" button
4. Enter API keys or sign in with GitHub

Or edit the INI file directly:

```ini
[OpenAI]
APIKey=sk-...

[Anthropic]
APIKey=sk-ant-...

[GitHub]
AuthToken=...
```

## Troubleshooting

### "Could not initialize OpenSSL library"

Ensure OpenSSL DLLs are in your PATH or current directory:
- `libssl-1_1-x64.dll`
- `libcrypto-1_1-x64.dll`

They're installed in `C:\lazarus\` during setup.

### "GitHub Copilot not authenticated"

Run the IDE Settings dialog to authenticate with GitHub OAuth device flow.

### Compilation Errors

Make sure you've built the main package first:

```powershell
cd ..
C:\lazarus\lazbuild.exe --build-all pythia.lpk
```
