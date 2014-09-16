object QueryDesignerDialog: TQueryDesignerDialog
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure Relation'
  ClientHeight = 206
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grbConfigure: TGroupBox
    Left = 0
    Top = 0
    Width = 342
    Height = 132
    Align = alTop
    Caption = 'Configure Relation'
    TabOrder = 0
    object lblForeignKey: TLabel
      Left = 10
      Top = 76
      Width = 43
      Height = 13
      Caption = 'Key Field'
    end
    object lblForeignTable: TLabel
      Left = 10
      Top = 49
      Width = 65
      Height = 13
      Caption = 'Foreign Table'
    end
    object btnDel: TSpeedButton
      Left = 303
      Top = 69
      Width = 26
      Height = 25
      Caption = '-'
      Visible = False
      OnClick = btnDelClick
    end
    object lblDisplayField: TLabel
      Left = 10
      Top = 102
      Width = 76
      Height = 13
      Caption = 'Add Select Field'
    end
    object lblSourceField: TLabel
      Left = 10
      Top = 21
      Width = 58
      Height = 13
      Caption = 'Source Field'
    end
    object cmbDisplayMember: TComboBox
      Left = 88
      Top = 100
      Width = 209
      Height = 21
      TabOrder = 4
    end
    object cmbValueMember: TComboBox
      Left = 88
      Top = 73
      Width = 209
      Height = 21
      TabOrder = 2
    end
    object cmbForeignTable: TComboBox
      Left = 88
      Top = 46
      Width = 209
      Height = 21
      TabOrder = 1
      OnChange = cmbForeignTableChange
    end
    object btnAddField: TButton
      Left = 303
      Top = 97
      Width = 25
      Height = 25
      Caption = '+'
      TabOrder = 3
      OnClick = btnAddFieldClick
    end
    object cmbSourceField: TComboBox
      Left = 88
      Top = 18
      Width = 209
      Height = 21
      TabOrder = 0
      OnChange = cmbForeignTableChange
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 165
    Width = 342
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 146
    object btnAccept: TButton
      Left = 162
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      ImageIndex = 10
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 243
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ImageIndex = 11
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlSelectFields: TPanel
    Left = 0
    Top = 132
    Width = 342
    Height = 33
    Align = alClient
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    ExplicitTop = 81
    ExplicitHeight = 65
  end
end
