# Makefile for Pythia test tools
# Usage: make all

FPC = C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe
FPCFLAGS = -MDelphi -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -Fu..\Source -Fi..\Source -FE.
UNITS = ..\Source\Pythia.Config.pas ..\Source\Pythia.GitHub.Auth.pas ..\Source\Pythia.AI.Client.pas

all: test_github_auth.exe test_chat.exe show_config.exe test_connection.exe

test_github_auth.exe: test_github_auth.pas $(UNITS)
	$(FPC) $(FPCFLAGS) test_github_auth.pas

test_chat.exe: test_chat.pas $(UNITS)
	$(FPC) $(FPCFLAGS) test_chat.pas

show_config.exe: show_config.pas $(UNITS)
	$(FPC) $(FPCFLAGS) show_config.pas

test_connection.exe: test_connection.pas $(UNITS)
	$(FPC) $(FPCFLAGS) test_connection.pas

clean:
	del /Q *.exe *.o *.ppu 2>nul

.PHONY: all clean
