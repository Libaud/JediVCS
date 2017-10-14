object FormLogon: TFormLogon
  Left = 388
  Top = 319
  ActiveControl = EditUser
  BorderStyle = bsDialog
  Caption = 'Logon to database'
  ClientHeight = 203
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelUser: TLabel
    Left = 8
    Top = 12
    Width = 52
    Height = 13
    Caption = '&Username:'
    FocusControl = EditUser
  end
  object LabelPassword: TLabel
    Left = 10
    Top = 36
    Width = 50
    Height = 13
    Caption = '&Password:'
    FocusControl = EditPassword
  end
  object LabelServer: TLabel
    Left = 25
    Top = 60
    Width = 36
    Height = 13
    Caption = '&Server:'
    FocusControl = EditServer
  end
  object DatabaseLabel: TLabel
    Left = 9
    Top = 84
    Width = 50
    Height = 13
    Alignment = taRightJustify
    Caption = '&Database:'
    Enabled = False
    FocusControl = EditDatabase
  end
  object FFServerLabel: TLabel
    Left = 12
    Top = 108
    Width = 50
    Height = 13
    Caption = 'FF server:'
    Enabled = False
    Visible = False
  end
  object lbFIBCharSet: TLabel
    Left = 15
    Top = 148
    Width = 46
    Height = 13
    Caption = '&Char Set:'
    FocusControl = cbFIBCharSet
    Visible = False
  end
  object EditUser: TEdit
    Left = 72
    Top = 8
    Width = 320
    Height = 21
    TabOrder = 0
    Text = 'EditUser'
  end
  object EditPassword: TEdit
    Left = 72
    Top = 32
    Width = 320
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    Text = 'EditPassword'
  end
  object EditServer: TEdit
    Left = 72
    Top = 56
    Width = 320
    Height = 21
    TabOrder = 2
    Text = 'EditServer'
  end
  object ButtonOK: TButton
    Left = 232
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object ButtonCancel: TButton
    Left = 316
    Top = 168
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object CheckBoxAutoLogon: TCheckBox
    Left = 12
    Top = 172
    Width = 180
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Log on automatically on start-up'
    TabOrder = 5
  end
  object EditDatabase: TJvFilenameEdit
    Left = 72
    Top = 80
    Width = 320
    Height = 21
    AddQuotes = False
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = 'EditDatabase'
  end
  object cbFFServerMode: TComboBox
    Left = 72
    Top = 104
    Width = 320
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    Visible = False
    Items.Strings = (
      'Local server (Database info required)'
      'Remote server (Server info required)')
  end
  object cbUseFirebirdEmbeded: TCheckBox
    Left = 12
    Top = 163
    Width = 180
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use &Firebird embedded'
    TabOrder = 8
    Visible = False
    OnClick = cbUseFirebirdEmbededClick
  end
  object cbUseTrustedNTConnection: TCheckBox
    Left = 12
    Top = 123
    Width = 180
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use &Trusted NT Connection'
    TabOrder = 9
    Visible = False
    OnClick = cbUseTrustedNTConnectionClick
  end
  object cbFIBCharSet: TComboBox
    Left = 73
    Top = 143
    Width = 320
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 10
    Visible = False
    Items.Strings = (
      'NONE'
      'ASCII'
      'BIG_5'
      'CYRL'
      'DOS437'
      'DOS850'
      'DOS852'
      'DOS857'
      'DOS860'
      'DOS861'
      'DOS863'
      'DOS865'
      'EUCJ_0208'
      'GB_2312'
      'ISO8859_1'
      'ISO8859_2'
      'KSC_5601'
      'NEXT'
      'OCTETS'
      'SJIS_0208'
      'UNICODE_FSS'
      'UTF8'
      'WIN1250'
      'WIN1251'
      'WIN1252'
      'WIN1253'
      'WIN1254'
      'DOS737'
      'DOS775'
      'DOS858'
      'DOS862'
      'DOS864'
      'DOS866'
      'DOS869'
      'WIN1255'
      'WIN1256'
      'WIN1257'
      'ISO8859_3'
      'ISO8859_4'
      'ISO8859_5'
      'ISO8859_6'
      'ISO8859_7'
      'ISO8859_8'
      'ISO8859_9'
      'ISO8859_13'
      'KOI8R'
      'KOI8U'
      'WIN1258'
      'TIS620'
      'GBK'
      'CP943C')
  end
end
