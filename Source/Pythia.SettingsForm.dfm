object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Pythia Settings'
  ClientHeight = 520
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 720
    Height = 469
    ActivePage = TabGitHub
    Align = alClient
    TabOrder = 0
    object TabGitHub: TTabSheet
      Caption = 'GitHub Copilot'
      object LabelGitHub: TLabel
        Left = 16
        Top = 24
        Width = 130
        Height = 20
        Caption = 'GitHub Copilot'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelGitHubStatus: TLabel
        Left = 16
        Top = 56
        Width = 168
        Height = 15
        Caption = 'Status: Not authenticated'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object LabelGitHubInfo: TLabel
        Left = 16
        Top = 180
        Width = 655
        Height = 60
        Caption = 
          'GitHub Copilot provides FREE AI assistance using your existing G' +
          'itHub subscription.'#13#10'Click "Sign in with GitHub" and follow the' +
          ' instructions in your browser.'#13#10'This is the same authentication' +
          ' used by VS Code Copilot.'#13#10'No API keys required!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object ButtonGitHubSignIn: TButton
        Left = 16
        Top = 88
        Width = 185
        Height = 33
        Caption = 'Sign in with GitHub'
        TabOrder = 0
        OnClick = ButtonGitHubSignInClick
      end
      object ButtonGitHubSignOut: TButton
        Left = 208
        Top = 88
        Width = 145
        Height = 33
        Caption = 'Sign out'
        Enabled = False
        TabOrder = 1
        OnClick = ButtonGitHubSignOutClick
      end
    end
    object TabAPI: TTabSheet
      Caption = 'API Keys'
      object LabelOpenAI: TLabel
        Left = 16
        Top = 24
        Width = 96
        Height = 15
        Caption = 'OpenAI API Key:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelAnthropic: TLabel
        Left = 16
        Top = 88
        Width = 116
        Height = 15
        Caption = 'Anthropic API Key:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelInfo: TLabel
        Left = 16
        Top = 168
        Width = 655
        Height = 45
        Caption = 
          'Get your API keys from:'#13#10'OpenAI: https://platform.openai.com/a' +
          'pi-keys'#13#10'Anthropic: https://console.anthropic.com/settings/key' +
          's'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object LabelConfigPath: TLabel
        Left = 16
        Top = 140
        Width = 655
        Height = 15
        Cursor = crHandPoint
        Caption = 'Config file path will be set in code'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = LabelConfigPathClick
      end
      object EditOpenAIKey: TEdit
        Left = 16
        Top = 45
        Width = 675
        Height = 23
        TabOrder = 0
      end
      object EditAnthropicKey: TEdit
        Left = 16
        Top = 109
        Width = 675
        Height = 23
        TabOrder = 1
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 469
    Width = 720
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOK: TButton
      Left = 538
      Top = 12
      Width = 81
      Height = 29
      Caption = 'Save'
      Default = True
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 626
      Top = 12
      Width = 81
      Height = 29
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
end
