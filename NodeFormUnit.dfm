object NodeForm: TNodeForm
  Left = 0
  Top = 0
  Caption = 'NodeForm'
  ClientHeight = 463
  ClientWidth = 712
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object TabSheet11: TPageControl
    Left = 0
    Top = 0
    Width = 712
    Height = 463
    ActivePage = Peers
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 24
    ExplicitTop = 48
    ExplicitWidth = 657
    ExplicitHeight = 361
    object TabSheet1: TTabSheet
      Caption = 'Main'
      object ActionMainMenuBar2: TActionMainMenuBar
        Left = 0
        Top = 0
        Width = 704
        Height = 29
        ActionManager = ActionManager1
        Caption = 'ActionMainMenuBar1'
        Color = clMenuBar
        ColorMap.DisabledFontColor = 7171437
        ColorMap.HighlightColor = clWhite
        ColorMap.BtnSelectedFont = clBlack
        ColorMap.UnusedColor = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Spacing = 0
        ExplicitTop = 8
      end
    end
    object Peers: TTabSheet
      Caption = 'Peers'
      ImageIndex = 1
      object Label1: TLabel
        Left = 240
        Top = 104
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Button1: TButton
        Left = 64
        Top = 99
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 0
        OnClick = Button1Click
      end
      object ActionMainMenuBar1: TActionMainMenuBar
        Left = 0
        Top = 0
        Width = 704
        Height = 25
        ActionManager = ActionManager1
        Caption = 'ActionMainMenuBar1'
        Color = clMenuBar
        ColorMap.DisabledFontColor = 7171437
        ColorMap.HighlightColor = clWhite
        ColorMap.BtnSelectedFont = clBlack
        ColorMap.UnusedColor = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Spacing = 0
        ExplicitLeft = 528
        ExplicitTop = 64
        ExplicitWidth = 150
        ExplicitHeight = 29
      end
      object Memo1: TMemo
        Left = 184
        Top = 184
        Width = 393
        Height = 209
        Lines.Strings = (
          'Memo1')
        TabOrder = 2
      end
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actGetPeers
          end>
        ActionBar = ActionMainMenuBar1
      end
      item
        Items = <
          item
            Action = Action2
            Caption = '&Action2'
          end>
        ActionBar = ActionMainMenuBar2
      end>
    Left = 304
    Top = 8
    StyleName = 'Platform Default'
    object actGetPeers: TAction
      Caption = 'GetPeers'
      OnExecute = actGetPeersExecute
    end
    object Action2: TAction
      Caption = 'Action2'
    end
  end
end
