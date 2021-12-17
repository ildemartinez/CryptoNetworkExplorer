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
    Left = 200
    Top = 240
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 448
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
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
  object ipwIPPort1: TipwIPPort
    RemoteHost = '129.226.125.10'
    RemotePort = 8333
    SSLCertStore = 'MY'
    OnConnected = ipwIPPort1Connected
    Left = 456
    Top = 296
  end
end
