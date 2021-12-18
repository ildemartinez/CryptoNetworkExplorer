object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 433
  ClientWidth = 733
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 733
    Height = 433
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 184
    ExplicitWidth = 697
    ExplicitHeight = 225
  end
  object TrayIcon1: TTrayIcon
    OnDblClick = TrayIcon1DblClick
    Left = 408
    Top = 120
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    Left = 256
    Top = 96
  end
end
