# Pythia - AI Copilot for Lazarus IDE

A VS Code Copilot-style AI chat assistant plugin for Lazarus IDE (Free Pascal).

## Features

- **GitHub Copilot Integration**: Use GitHub Copilot Chat (FREE tier) - no API keys required! 🆓
- **Integrated Chat Window**: VS Code-style chat interface accessible via `View > IDE Internals Windows > PythiaChatWindow`
- **Multiple AI Models**: GitHub Copilot (GPT-4, Claude 3.5), OpenAI, Anthropic
- **Lazarus Expert**: Trained to help with Free Pascal/Object Pascal programming in Lazarus IDE
- **Dockable Window**: Chat window integrates seamlessly into Lazarus IDE
- **Context Awareness**: AI sees your current file, cursor position, and selection
- **Command-Line Build**: Full `lazbuild` automation without IDE restrictions
- **Persistent Configuration**: OAuth tokens stored securely in user's AppData folder

## Development Workflow

### Quick Testing (Standalone App)
For rapid development without IDE reinstallation:

1. Open `PythiaApp.dproj` in Lazarus IDE
2. Press **F9** to compile and run
3. Test chat functionality in standalone window
4. Make changes, rebuild, repeat

### IDE Plugin Installation

#### Automated Clean Install (Recommended)
Run the PowerShell script to completely rebuild and install:
```powershell
.\CLEAN_INSTALL.ps1
```
This will:
- Stop Lazarus IDE
- Remove all old Pythia package registrations
- Clean build artifacts from `lib/` directory
- Compile package with `lazbuild --build-all pythia.lpk`
- Rebuild entire IDE with `lazbuild --add-package pythia.lpk --build-ide=`
- Start Lazarus with Pythia installed

#### Manual Installation via lazbuild
```bash
# 1. Build package
lazbuild --build-all pythia.lpk

# 2. Stop IDE and rebuild with package
taskkill /F /IM lazarus.exe
lazbuild --add-package pythia.lpk --build-ide=

# 3. Start IDE
C:\lazarus\lazarus.exe
```

**Access Pythia:**
- Menu: **View > IDE Internals Windows > PythiaChatWindow**

**Both projects (pythia.lpk and PythiaApp.dpr) share the same Source/*.pas units** - changes automatically work in both!

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
3. Press **Send** button or **Enter**
4. View the AI response in the chat window (word-wrapped with code block formatting)

### Example Prompts
- "How do I implement a thread-safe singleton in Free Pascal?"
- "Review this code for memory leaks in Lazarus"
- "What's the best way to parse JSON in Free Pascal?"
- "Convert this code to use Free Pascal generics"
- "How do I create a custom component for Lazarus?"

## Project Structure

```
pythia-lazarus/
├── Source/
│   ├── Pythia.Register.pas      # IDE plugin registration
│   ├── Pythia.ChatForm.pas/.lfm # Chat UI form (Lazarus)
│   ├── Pythia.AI.Client.pas     # AI API client
│   ├── Pythia.Config.pas        # Configuration manager
│   ├── Pythia.Context.pas       # IDE context extraction
│   └── Pythia.FileEdit.pas      # File editing integration
├── pythia.lpk                   # Lazarus package file
├── pythia.pas                   # Package unit
├── PythiaApp.dpr                # Standalone test app
├── CLEAN_INSTALL.ps1            # Automated installation
└── README.md                    # This file
```

## Requirements

- **Lazarus IDE 3.2.0** or later
- **Free Pascal Compiler 3.2.2** or later
- Windows (64-bit recommended)
- Internet connection for AI API calls
- Optional: API key(s) for OpenAI and/or Anthropic (GitHub Copilot is FREE)

## Development

To modify or extend Pythia:

1. Clone repository: `git clone git@github.com:tmx11/pythia-lazarus.git`
2. Open `pythia.lpk` or `PythiaApp.dpr` in Lazarus IDE
3. Make your changes to the source files in `Source/`
4. Test with PythiaApp first (F9 to run standalone)
5. Run `.\CLEAN_INSTALL.ps1` to rebuild IDE plugin
6. Access via **View > IDE Internals Windows > PythiaChatWindow**

## API Costs

Both OpenAI and Anthropic charge per API usage. Monitor your usage:
- OpenAI: https://platform.openai.com/usage
- Anthropic: https://console.anthropic.com/settings/usage

## Troubleshooting

**Chat window doesn't appear**
- Check **View > IDE Internals Windows** menu
- Ensure package is properly installed via `lazbuild --add-package`
- Restart Lazarus IDE
- Run `.\CLEAN_INSTALL.ps1` for clean reinstall

**API errors**
- Verify API keys are correct in Settings
- For GitHub Copilot: Re-authenticate via Settings > Sign in with GitHub
- Check internet connection
- Ensure you have API credits remaining (OpenAI/Anthropic)

**Build errors**
- Verify Lazarus 3.2.0+ and FPC 3.2.2+ are installed
- Check all source files are present in `Source/` directory
- Run `lazbuild --build-all pythia.lpk` to see detailed errors
- Clean lib directory: `Remove-Item lib -Recurse -Force`

**Word wrap issues**
- Word wrap is enabled by default in TMemo
- Long lines automatically wrap in chat window

## License

MIT License - See LICENSE file for details

## Contributing

Contributions welcome! This project was ported from Delphi to Lazarus for freedom from licensing restrictions.

**Key Differences from Delphi:**
- Uses `.lpk` (Lazarus package) instead of `.dpk`
- Uses `.lfm` (Lazarus form) instead of `.dfm`
- Compiled with Free Pascal Compiler (FPC) instead of Delphi compiler
- Command-line build via `lazbuild` (no IDE restrictions)
- Dockable windows via `IDEWindowIntf` unit

## Roadmap

- [x] Port from Delphi 12 to Lazarus IDE
- [x] Command-line build process with lazbuild
- [x] Dockable chat window
- [x] IDE context extraction (current file, cursor, selection)
- [x] Status bar with version, git branch, file, stats
- [x] Word wrap for long messages
- [x] Code block formatting (indented with 4 spaces)
- [x] Visual markers [USER]/[AI] for message distinction
- [ ] Syntax highlighting for code blocks
- [ ] Todo list pane with beads integration
- [ ] Terminal integration for running commands
- [ ] Diff viewer for file edits
- [ ] Persistent chat history

## Credits

Originally created for Delphi 12, ported to Lazarus IDE for open-source freedom.

Inspired by GitHub Copilot Chat in VS Code.
