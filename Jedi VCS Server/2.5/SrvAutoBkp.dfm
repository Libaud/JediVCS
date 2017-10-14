object SrvAutoBackup: TSrvAutoBackup
  Left = 340
  Top = 313
  BorderStyle = bsDialog
  Caption = 'Auto Backup'
  ClientHeight = 95
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 36
    Width = 61
    Height = 13
    Caption = 'Backup time:'
  end
  object dtpBkpTime: TDateTimePicker
    Left = 80
    Top = 32
    Width = 97
    Height = 21
    CalAlignment = dtaLeft
    Date = 36478.588626446800000000
    Time = 36478.588626446800000000
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkTime
    ParseInput = False
    TabOrder = 1
  end
  object cbAutoBkpActive: TCheckBox
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Auto backup active'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
