object ServerForm: TServerForm
  Left = 431
  Top = 241
  Width = 367
  Height = 286
  Caption = 'ServerForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
    0000000777777777777777777700000000000007F82288888888888887700000
    3FB30007F8AA888888000000877088883FB30887F88888888877777787707377
    33330777FFFFFFFFFFFFFFFFF770000007800000777777777777777778700000
    0780000007000000000000007780000007800000078888888888888707700000
    0780000007FFFFFFFFFFFF87700000000000000007F7777777777F8770007777
    7777777777F7066666667F8770007F822888888887F7066666667F8770007F8A
    A888888007F706E666667F8770007F888888888777F706E666667F8770007FFF
    FFFFFFFFF7F7066666667F87700007777777777777F7000000007F8770000070
    0000000007F7777777777F87700000788888888887FFFFFFFFFFFFF77000007F
    FFFFFFFFFF788888888888887000007F7444444444F77777777777777000007F
    70CCCCCCC4F87700000000000000007F70CCCCCCC4F87700000000000000007F
    70CECCCCC4F87700000000000000007F70CECCCCC4F87700000000000000007F
    70CCCCCCC4F87700000000000000007F7000000004F87700000000000000007F
    7777777777F87700000000000000007FFFFFFFFFFFFF77000000000000000007
    8888888888888700000000000000000077777777777777000000000000000000
    0000000000000000000000000000FFF00003FFE00001F0600000000000000000
    000000000000F8F00000F8F80000F8F80001F8F8000380000003000000030000
    000300000003000000030000000380000003C0000003C0000003C0000003C000
    0007C0001FFFC0001FFFC0001FFFC0001FFFC0001FFFC0001FFFC0001FFFC000
    1FFFE0001FFFF0003FFFFFFFFFFF280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000000F00B3
    0078A777777077B3077FFFFFFF70007000700000007000700078888888000000
    007F0777780078A7777F066678007FFFFF7F0E6678007000007F000008007888
    887FFFFFFF007F444447777777707F0CCC48000000007F0ECC48000000007F00
    0048000000007FFFFFFF000000000777777770000000CE000000000000000000
    0000CC000000CC01000080010000000100000001000000010000000100000001
    0000007F0000007F0000007F0000007F0000807F0000}
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 119
    Width = 351
    Height = 109
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      351
      109)
    object GroupBox1: TGroupBox
      Left = 4
      Top = 4
      Width = 351
      Height = 99
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Status'
      TabOrder = 0
      object Label2: TLabel
        Left = 30
        Top = 32
        Width = 36
        Height = 13
        Caption = 'Clients:'
      end
      object ClientCountLabel: TLabel
        Left = 72
        Top = 32
        Width = 81
        Height = 13
        Caption = 'ClientCountLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 27
        Top = 16
        Width = 37
        Height = 13
        Caption = 'Uptime:'
      end
      object UpTimeLabel: TLabel
        Left = 72
        Top = 16
        Width = 60
        Height = 13
        Caption = 'UpTimeLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 196
        Top = 48
        Width = 56
        Height = 13
        Caption = 'Server Log:'
      end
      object SrvLogLabel: TLabel
        Left = 258
        Top = 48
        Width = 87
        Height = 13
        AutoSize = False
        Caption = 'SrvLogLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 15
        Top = 48
        Width = 49
        Height = 13
        Caption = 'Requests:'
      end
      object RequestCountLabel: TLabel
        Left = 72
        Top = 48
        Width = 94
        Height = 13
        Caption = 'RequestCountLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 188
        Top = 64
        Width = 64
        Height = 13
        Caption = 'Auto Backup:'
      end
      object AutoBkpLabel: TLabel
        Left = 258
        Top = 64
        Width = 87
        Height = 13
        AutoSize = False
        Caption = 'AutoBkpLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 14
        Top = 64
        Width = 48
        Height = 13
        Caption = 'Received:'
      end
      object Label10: TLabel
        Left = 6
        Top = 80
        Width = 61
        Height = 13
        Caption = 'Transmitted:'
      end
      object RcvBytesLabel: TLabel
        Left = 72
        Top = 64
        Width = 70
        Height = 13
        Caption = 'RcvBytesLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object TrmBytesLabel: TLabel
        Left = 72
        Top = 80
        Width = 70
        Height = 13
        Caption = 'TrmBytesLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 229
        Top = 16
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object PortLabel: TLabel
        Left = 258
        Top = 16
        Width = 45
        Height = 13
        Caption = 'PortLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object TimeOutLabel: TLabel
        Left = 258
        Top = 32
        Width = 86
        Height = 13
        AutoSize = False
        Caption = 'TimeOutLabel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label1: TLabel
        Left = 209
        Top = 32
        Width = 42
        Height = 13
        Caption = 'Timeout:'
      end
    end
  end
  object DisplayRE: TRichEdit
    Left = 0
    Top = 0
    Width = 351
    Height = 119
    Align = alClient
    Lines.Strings = (
      'DisplayRE')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object RequestBroker1: TRequestBroker
    UserData = 0
    Options = [rboDisplayObjectCount]
    OnDisplay = AppServer1Display
    Left = 290
    Top = 40
  end
  object AppServer1: TAppServer
    Options = [asoDisplayCommands, asoDisplayClientCount]
    Banner = 'Welcome to JVCS Application server'
    Addr = '0.0.0.0'
    Port = '2106'
    ClientTimeout = 30
    TimeoutInterval = 5
    RequestBroker = RequestBroker1
    ListenBacklog = 5
    OnDisplay = AppServer1Display
    OnClientConnected = AppServer1ClientConnected
    OnClientClosed = AppServer1ClientClosed
    OnBeforeSendReply = AppServer1BeforeSendReply
    OnAfterSendReply = AppServer1AfterSendReply
    OnBeforeProcessRequest = AppServer1BeforeProcessRequest
    OnAfterProcessRequest = AppServer1AfterProcessRequest
    Left = 290
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 258
    Top = 40
    object Server1: TMenuItem
      Caption = '&Server'
      object Start1: TMenuItem
        Caption = 'St&art'
        OnClick = Start1Click
      end
      object Stop1: TMenuItem
        Caption = 'St&op'
        OnClick = Stop1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Port1: TMenuItem
        Caption = '&Port...'
        OnClick = Port1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object AutostartwithWindows1: TMenuItem
        Caption = 'Autostart with &Windows'
        OnClick = AutostartwithWindows1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Hide1: TMenuItem
        Caption = '&Hide Window'
        OnClick = Hide1Click
      end
      object HideOnClose1: TMenuItem
        Caption = 'Hide Window on &Close'
        OnClick = HideOnClose1Click
      end
      object StartHidden1: TMenuItem
        Caption = 'S&tart Hidden'
        OnClick = StartHidden1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Security1: TMenuItem
      Caption = 'S&ecurity'
      object DisableServerBanner1: TMenuItem
        Caption = '&Disable Server Banner'
        OnClick = DisableServerBanner1Click
      end
      object DisableSQLObject1: TMenuItem
        Caption = 'D&isable SQL Server Object'
        OnClick = DisableSQLObject1Click
      end
      object LogClientAccessFaults1: TMenuItem
        Caption = 'Log &unauthorized Client Requests'
        OnClick = LogClientAccessFaults1Click
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object ChangeServerBanner1: TMenuItem
        Caption = 'Change Server Banner...'
        OnClick = ChangeServerBanner1Click
      end
    end
    object Cliemts1: TMenuItem
      Caption = '&Clients'
      object ClientLists1: TMenuItem
        Caption = 'Cl&ient Lists'
        object AllClients1: TMenuItem
          Caption = '&All Clients'
          OnClick = AllClients1Click
        end
        object LoggedIn1: TMenuItem
          Caption = '&Logged In'
          OnClick = LoggedIn1Click
        end
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Disconnectall1: TMenuItem
        Caption = '&Disconnect All'
        OnClick = Disconnectall1Click
      end
      object LogoutAll1: TMenuItem
        Caption = '&Log out All'
        OnClick = LogoutAll1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Clienttimeout1: TMenuItem
        Caption = 'Client &Timeout...'
        OnClick = Clienttimeout1Click
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object VerifyClientTimestamp1: TMenuItem
        Caption = '&Verify Client Timestamp'
        OnClick = VerifyClientTimestamp1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object ForceGlobalSettings1: TMenuItem
        Caption = '&Force Global Settings'
        OnClick = ForceGlobalSettings1Click
      end
    end
    object Versionarchive1: TMenuItem
      Caption = '&Version Archive'
      object Directory1: TMenuItem
        Caption = '&Archive Folder/ Alias...'
        OnClick = Directory1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Backup1: TMenuItem
        Caption = '&Live Backup'
        object Autobackup1: TMenuItem
          Caption = '&Auto Backup Settings...'
          OnClick = Autobackup1Click
        end
        object Targetfolder1: TMenuItem
          Caption = '&Target Folder'
          OnClick = Targetfolder1Click
        end
        object N8: TMenuItem
          Caption = '-'
        end
        object Execute1: TMenuItem
          Caption = '&Execute'
          OnClick = Execute1Click
        end
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object Upgradearchive1: TMenuItem
        Caption = 'Up&grade database archive'
        Hint = 'Call database archive upgrade procedure manually'
        OnClick = Upgradearchive1Click
      end
    end
    object Display1: TMenuItem
      Caption = '&Display'
      object Copytoclipboard1: TMenuItem
        Caption = 'C&opy to Clipboard'
        OnClick = Copytoclipboard1Click
      end
      object Savetofile1: TMenuItem
        Caption = '&Save to File...'
        OnClick = Savetofile1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Clear1: TMenuItem
        Caption = '&Clear'
        OnClick = Clear1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ShowDebugMessages1: TMenuItem
        Caption = 'S&how Debug Messages'
        OnClick = ShowDebugMessages1Click
      end
    end
    object Log1: TMenuItem
      Caption = '&Log'
      object Logfileactive1: TMenuItem
        Caption = 'Server Logfile &active'
        OnClick = Logfileactive1Click
      end
      object Showlogfile1: TMenuItem
        Caption = '&Show Logfile...'
        OnClick = Showlogfile1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Clearlogfile1: TMenuItem
        Caption = '&Clear Logfile'
        OnClick = Clearlogfile1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Functionlist1: TMenuItem
        Caption = '&Function List'
        OnClick = Functionlist1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
  end
  object UpTimer: TTimer
    Enabled = False
    OnTimer = UpTimerTimer
    Left = 322
    Top = 40
  end
  object TATimer: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = TATimerTimer
    Left = 322
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 258
    Top = 8
  end
end
