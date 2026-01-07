# Build Instructions for Pythia (Delphi 12 CE)

## Important Note About Delphi Community Edition

**Delphi Community Edition does not support command-line compilation.** You must build the package through the IDE.

## Building the Package

### Step 1: Open the Project
1. Launch Delphi 12 (Athens) IDE
2. Open **File > Open Project...**
3. Navigate to: `d:\dev\delphi\pythia2\pythia.dproj`
4. Click **Open**

### Step 2: Build the Package
1. In the IDE, go to **Project > Build pythia**
2. Or press **Shift+F9**
3. Check the **Messages** window at the bottom for any errors

### Step 3: Install the Package
1. Go to **Component > Install Packages...**
2. Click **Add...**
3. Navigate to the compiled BPL file:
   - Location: `d:\dev\delphi\pythia2\Win32\Debug\pythia.bpl`
4. Click **Open**
5. Click **OK** to close the dialog
6. **Restart Delphi IDE** for the plugin to fully activate

### Step 4: Verify Installation
1. After restarting, check **Tools** menu
2. You should see **"Pythia AI Chat..."** menu item
3. Press **Ctrl+Shift+P** or click the menu to open the chat window

## Troubleshooting Build Errors

### Common Issues:

**Missing ToolsAPI unit:**
- Ensure `designide.dcp` is properly referenced in the package requires clause
- Check that Delphi 12 is properly installed

**Cannot find unit 'Pythia.Register':**
- Verify all source files exist in `Source\` folder
- Check project search paths include `Source\`

**Design-time package error:**
- Ensure package has both `{$RUNONLY}` and `{$DESIGNONLY}` directives
- Check pythia.dpk file contains proper package declaration

**BPL not found:**
- Build successful but BPL missing? Check output path in project options
- Default: `Win32\Debug\pythia.bpl`

### If Build Fails:

1. **Check all source files exist:**
   ```
   Source\Pythia.Register.pas
   Source\Pythia.ChatForm.pas
   Source\Pythia.ChatForm.dfm
   Source\Pythia.AI.Client.pas
   Source\Pythia.Config.pas
   ```

2. **Verify package requires:**
   - rtl
   - designide
   - vcl
   - vclx

3. **Check Messages window** for specific error details

4. **Clean and rebuild:**
   - Project > Clean
   - Project > Build pythia

## After Successful Installation

### Configure API Keys:
1. Tools > Pythia AI Chat (or Ctrl+Shift+P)
2. Click **Settings** button
3. Enter your API key(s):
   - OpenAI: Get from https://platform.openai.com/api-keys
   - Anthropic: Get from https://console.anthropic.com/settings/keys

### Test the Plugin:
1. Open chat window
2. Select a model (GPT-4 or Claude)
3. Type: "Hello, can you help me with Delphi?"
4. Press Send or Ctrl+Enter

## Build Output Location

After successful build, the package files will be in:
```
Win32\Debug\
├── pythia.bpl          <- Install this file
├── pythia.dcp
├── *.dcu files
└── *.obj files
```

## Need Help?

If you encounter build errors:
1. Check the Messages window in IDE
2. Verify all dependencies are installed
3. Ensure Delphi 12 CE is up to date
4. Check that all source files are saved

---

**Remember:** Delphi CE must build packages through the IDE, not via command line!
