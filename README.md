# Pythia - AI Copilot for Delphi IDE

A VS Code Copilot-style AI chat assistant plugin for Embarcadero Delphi 12.

## Features

- **GitHub Copilot Integration**: Use GitHub Copilot Chat (FREE tier) - no API keys required! 🆓
- **Integrated Chat Window**: VS Code-style chat interface accessible via `Tools > Pythia AI Chat` or `Ctrl+Shift+P`
- **Multiple AI Models**: GitHub Copilot (GPT-4, Claude 3.5), OpenAI, Anthropic
- **Delphi Expert**: Trained to help with Delphi programming questions, code review, and debugging
- **Persistent Configuration**: OAuth tokens stored securely in user's AppData folder

## Development Workflow

### Quick Testing (Standalone App)
For rapid development without IDE reinstallation:

1. Open `PythiaApp.dproj` in Delphi 12
2. Press **F9** to compile and run
3. Test chat functionality in standalone window
4. Make changes, rebuild, repeat

### IDE Plugin Installation
Once features are verified in standalone app:

#### Option 1: Clean Install (Recommended)
Run the automated clean install script to remove old installations:
```powershell
.\clean-install.ps1
```
This will:
- Stop Delphi IDE
- Remove all old Pythia registry entries
- Clean build the package to `Win32\Debug\pythia.bpl`
- Install to IDE and restart Delphi

#### Option 2: Manual Installation
1. Close Delphi IDE completely
2. Open `pythia.dproj` in Delphi 12
3. Build the package: **Shift+F9** or **Project > Build pythia**
4. BPL will be in: `Win32\Debug\pythia.bpl`
5. Open **Component > Install Packages**
6. Click **Add**, browse to `Win32\Debug\pythia.bpl`
7. Click **OK** and restart Delphi

**Access Pythia:**
- Menu: **Tools > Pythia AI Chat...**
- Keyboard: **Ctrl+Shift+P**

**Troubleshooting Duplicates:**
If you see duplicate menu items, run `.\clean-install.ps1` to completely remove old installations.

**Both projects share the same Source/*.pas units** - changes automatically work in both!

## Configuration

### GitHub Copilot (Recommended - FREE)
1. Open **Tools > Pythia AI Chat** (or run PythiaApp.exe)
2. Click **Settings** button
3. Click **Sign in with GitHub**
4. Enter the device code shown at https://github.com/login/device
5. You're ready! No API keys needed.

### API Keys (Optional)
If you want to use OpenAI/Anthropic directly:
   - OpenAI API key from https://platform.openai.com/api-keys
   - Anthropic API key from https://console.anthropic.com/settings/keys

Configuration is stored in: `%APPDATA%\Pythia\pythia.ini`

## Usage

### Opening the Chat Window
- Menu: **Tools > Pythia AI Chat**
- Keyboard: **Ctrl+Shift+P**

### Using the Chat
1. Select your preferred AI model from the dropdown
2. Type your question or request in the input box
3. Press **Send** button or **Ctrl+Enter**
4. View the AI response in the chat window

### Example Prompts
- "How do I implement a thread-safe singleton in Delphi?"
- "Review this code for memory leaks"
- "What's the best way to parse JSON in Delphi?"
- "Convert this code to use generics"

## Project Structure

```
pythia2/
├── Source/
│   ├── Pythia.Register.pas      # IDE plugin registration
│   ├── Pythia.ChatForm.pas/.dfm # Chat UI form
│   ├── Pythia.AI.Client.pas     # AI API client
│   └── Pythia.Config.pas        # Configuration manager
├── pythia.dpk                   # Package source
└── pythia.dproj                 # Project file
```

## Requirements

- Embarcadero Delphi 12 (Athens)
- Windows 32-bit or 64-bit
- Internet connection for AI API calls
- Valid API key(s) for OpenAI and/or Anthropic

## Development

To modify or extend Pythia:

1. Clone/open the project in Delphi 12
2. Make your changes to the source files
3. Rebuild the package
4. Uninstall old version from IDE if needed
5. Install the new version

## API Costs

Both OpenAI and Anthropic charge per API usage. Monitor your usage:
- OpenAI: https://platform.openai.com/usage
- Anthropic: https://console.anthropic.com/settings/usage

## Troubleshooting

**Chat window doesn't appear**
- Ensure package is properly installed
- Check IDE's Package list (Component > Install Packages)
- Restart Delphi IDE

**API errors**
- Verify API keys are correct in Settings
- Check internet connection
- Ensure you have API credits remaining

**Build errors**
- Verify Delphi 12 is properly installed
- Check all source files are present
- Clean and rebuild the project

## License

[Your License Here]

## Contributing

Contributions welcome! Please submit pull requests or open issues for bugs/features.

## Roadmap

- [ ] Settings dialog with API key management
- [ ] Code context awareness (send selected editor code)
- [ ] Syntax highlighting in chat responses
- [ ] Export chat history
- [ ] Custom system prompts
- [ ] Inline code suggestions
- [ ] Integration with IDE code editor

## Credits

Created by [Your Name]

Inspired by GitHub Copilot Chat in VS Code.
