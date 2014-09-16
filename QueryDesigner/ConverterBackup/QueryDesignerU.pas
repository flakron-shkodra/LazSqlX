﻿unit QueryDesignerU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Contnrs,
  Vcl.ExtCtrls, TableInfo, Vcl.StdCtrls, Vcl.ToolWin,
  Vcl.ComCtrls, DbInfo, Vcl.ImgList, SqlGenerator, Vcl.GraphUtil;

type

  TQueryDesigner = class(TFrame)
    // GUI CONTROLS DECLARE
    scbDesigner: TScrollBox;
    pbArea: TPaintBox;
    tlbToolbox: TToolBar;
    btnGenerateQuery: TToolButton;
    ilToolboxImages: TImageList;
    txtSqlQuery: TMemo;
    ToolButton1: TToolButton;
    btnDeleteTable: TToolButton;
    btnClear: TToolButton;
    Splitter1: TSplitter;
    pnlDesigner: TPanel;
    // CONTROLS END

    procedure Splitter1Paint(Sender: TObject);
    procedure btnDeleteTableClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnGenerateQueryClick(Sender: TObject);
    procedure pbAreaPaint(Sender: TObject);
    procedure pbAreaClick(Sender: TObject);
    procedure scbDesignerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure scbDesignerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure scbDesignerResize(Sender: TObject);
  private
    { Private declarations }

    inReposition: boolean;
    FNodePositioning: boolean;
    oldPos: TPoint;

    SelectedControl: TObject;
    ControlPressed: boolean;
    FNodes: TObjectList;
    FDatabaseInfo: TDatabaseInfo;
    FTableInfoList: TTableInfos;
    IsFieldDragging: boolean;
    FFCurrentNodeControl: TWinControl;
    FArrowImage: TBitmap;
    FRectImage: TBitmap;
    FArrowImageToLeft: TBitmap;
    procedure SetFCurrentNodeControl(const Value: TWinControl);
    procedure SetArrowImage(const Value: TBitmap);
    procedure SetRectImage(const Value: TBitmap);
    procedure SetArrowImageToLeft(const Value: TBitmap);
    procedure SetArrowImageToRight(const Value: TBitmap);
    property FCurrentNodeControl: TWinControl read FFCurrentNodeControl
      write SetFCurrentNodeControl;

    procedure CreateNodes;
    procedure PositionNodes(AroundControl: TWinControl);
    procedure SetNodesVisible(Visible: boolean);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    procedure NodeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure NodeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure NodeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure AssignEvents(ctrl: TObject);
    // **************DRAG MOVE RESIZE RELATED END************************************************

    // *****************FIELD LIST DRAG RELATED BEGIN*****************************************************
    procedure OnFieldStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure OnFieldDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure OnFieldDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure OnFieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OnFieldMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OnFieldMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    // *****************FIELD LIST DRAG RELATED END*****************************************************

    procedure GenerateTableControl(CursorPos: TPoint; parent: TWinControl;
      Schema, Tablename: string);
    procedure SetDatabaseInfo(const Value: TDatabaseInfo);
    procedure SetTableInfoList(const Value: TTableInfos);

    function FindTableControl(Tablename: string): TPanel;
    function FindFieldControl(tableControl: TPanel; fieldName: string): TPanel;
    function GetSqlQuery: string;

    procedure UpdateToolbar;

  public
    { Public declarations }
    DbSchema: string;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddTable(Schema, Tablename: string; Position: TPoint);
    procedure Clear;
    procedure RemoveTable(Tablename: string);

    procedure GenerateQuery;

    property DatabaseInfo: TDatabaseInfo read FDatabaseInfo
      write SetDatabaseInfo;
    property TableInfoList: TTableInfos read FTableInfoList
      write SetTableInfoList;

    property SqlQuery: string read GetSqlQuery;
    property ArrowImageToLeft: TBitmap read FArrowImageToLeft
      write SetArrowImageToLeft;
    property ArrowImageToRight: TBitmap read FArrowImage
      write SetArrowImageToRight;

    property RectImage: TBitmap read FRectImage write SetRectImage;

  end;

  TControlAccess = class(TControl);

  TDragFieldObject = class(TDragControlObjectEx)
  private
    FDragImages: TDragImageList;
    FPrevAccepted: Boolean;
    FDragText: string;
    procedure SetDragText(const Value: string);
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  public
    property DragText:string read FDragText write SetDragText;
    destructor Destroy; override;
  end;


  // bitmap max width = 32768 pixels
const
  CMAX_BITMAP_WIDTH = 1024 * 32;

  // define a static array of TRGBTriple
type
  TArrayOfPixel = array [0 .. CMAX_BITMAP_WIDTH - 1] of TRGBTriple;
  PArrayOfPixel = ^TArrayOfPixel;

implementation

uses QueryDesignerDialogU;

{$R *.dfm}
{ TQueryDesignerFrame }

procedure TQueryDesigner.AddTable(Schema, Tablename: string; Position: TPoint);
begin
  DbSchema := Schema;
  GenerateTableControl(Position, scbDesigner, Schema, Tablename);
end;

procedure TQueryDesigner.AssignEvents(ctrl: TObject);
begin
  if (ctrl is TPanel) then
  begin
    (ctrl as TPanel).OnMouseDown := ControlMouseDown;
    (ctrl as TPanel).OnMouseUp := ControlMouseUp;
    (ctrl as TPanel).OnMouseMove := ControlMouseMove;
  end;

end;

procedure TQueryDesigner.btnClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TQueryDesigner.btnDeleteTableClick(Sender: TObject);
begin
  if FCurrentNodeControl <> nil then
  begin
    RemoveTable(FCurrentNodeControl.Hint);
    SetNodesVisible(False);
    FCurrentNodeControl := nil;
    pbArea.Invalidate;
  end;
end;

procedure TQueryDesigner.btnGenerateQueryClick(Sender: TObject);
begin
  if FCurrentNodeControl <> nil then
    GenerateQuery;
end;

procedure TQueryDesigner.Clear;
var
  I: integer;
  c: TControl;
  ti: TTableInfo;
begin
  txtSqlQuery.Clear;
  for ti in TableInfoList do
  begin
    c := scbDesigner.FindChildControl(ti.TableCSharpName);
    if c <> nil then
      c.Free;
  end;

  SetNodesVisible(False);
  FFCurrentNodeControl := nil;

  pbArea.Invalidate;

end;

procedure TQueryDesigner.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Self.Invalidate;
  if (Sender is TWinControl) then
  begin
    inReposition := True;
    SetCapture(TWinControl(Sender).Handle);
    GetCursorPos(oldPos);
    SelectedControl := Sender;
    TWinControl(Sender).SetFocus;
    ControlPressed := True;
    PositionNodes(TWinControl(Sender));
  end;
end;

procedure TQueryDesigner.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  newPos: TPoint;
begin
  newPos := Point(0, 0);
  if inReposition then
  begin
    with TWinControl(Sender) do
    begin
      GetCursorPos(newPos);
      Screen.Cursor := crSize;
      Left := Left - oldPos.X + newPos.X;
      Top := Top - oldPos.Y + newPos.Y;
      oldPos := newPos;
      PositionNodes(TWinControl(Sender));
    end;
  end;

end;

procedure TQueryDesigner.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  CtrlCaption: string;
begin
  CtrlCaption := '';
  if inReposition then
  begin
    Screen.Cursor := crDefault;
    ReleaseCapture;
    inReposition := False;
  end;
  if (ssCtrl in Shift) and (Button = mbLeft) then
  begin
    if (Sender is TButton) or (Sender is TGroupBox) then
      if InputQuery('Edit Caption', 'Set control Text', CtrlCaption) then
      begin
        if (Sender is TButton) then
          (Sender as TButton).Caption := CtrlCaption;

        if (Sender is TGroupBox) then
          (Sender as TGroupBox).Caption := CtrlCaption;
      end;
  end;
end;

constructor TQueryDesigner.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FNodes := TObjectList.Create(True);
  CreateNodes;
  Splitter1.Height := 15;
  //Self.ControlStyle := Self.ControlStyle + [csDisplayDragImage];
  pbArea.ControlStyle := pbArea.ControlStyle + [csDisplayDragImage];
end;

procedure TQueryDesigner.CreateNodes;
var
  Node: integer;
  Panel: TPanel;
begin
  for Node := 0 to 7 do
  begin
    if FindComponent('Node' + IntToStr(Node)) = nil then
    begin
      Panel := TPanel.Create(Self);
      FNodes.Add(Panel);
      with Panel do
      begin
        // ParentBackground:=False;
        BevelOuter := bvNone;
        BevelInner := bvNone;

        Color := clBlue;
        ParentBackground := False;
        Name := 'Node' + IntToStr(Node);
        Caption := '';
        Width := 5;
        Height := 5;
        parent := Self;
        Visible := False;

        case Node of
          0, 4:
            Cursor := crSizeNWSE;
          1, 5:
            Cursor := crSizeNS;
          2, 6:
            Cursor := crSizeNESW;
          3, 7:
            Cursor := crSizeWE;
        end;
        OnMouseDown := NodeMouseDown;
        OnMouseMove := NodeMouseMove;
        OnMouseUp := NodeMouseUp;
      end;
    end;
  end;
end;

destructor TQueryDesigner.Destroy;
begin
  if FNodes <> nil then
    FNodes.Free;

  // TableInfoList.Clear;
  // TableInfoList.Free;
  if Assigned(ArrowImageToRight) then
    ArrowImageToRight.Free;

  if Assigned(ArrowImageToLeft) then
    ArrowImageToLeft.Free;

  if Assigned(RectImage) then
    RectImage.Free;

  inherited Destroy;
end;

function TQueryDesigner.FindFieldControl(tableControl: TPanel;
  fieldName: string): TPanel;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to tableControl.ControlCount - 1 do
  begin
    if (tableControl.Controls[I] is TPanel) then
      if (tableControl.Controls[I] as TPanel).Hint = fieldName then
        Result := (tableControl.Controls[I] as TPanel);
  end;

end;

function TQueryDesigner.FindTableControl(Tablename: string): TPanel;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to scbDesigner.ControlCount - 1 do
  begin
    if scbDesigner.Controls[I] is TPanel then
      if (scbDesigner.Controls[I] as TPanel).Hint = Tablename then
        Result := (scbDesigner.Controls[I] as TPanel);
  end;
end;

procedure TQueryDesigner.GenerateQuery;
var
  p: TProcedureNames;
  s: TSqlGenerator;
  ti: TTableInfo;
  outPut: TStringList;

begin

  try
    try
      ti := TableInfoList.TableByName(FCurrentNodeControl.Hint);
      if ti <> nil then
      begin
        s := TSqlGenerator.Create(DatabaseInfo, p);
        outPut := s.GetSql(0, ti, qtSelect);
        txtSqlQuery.Lines := outPut;
      end;
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;

  finally
    outPut.Free;
    s.Free;
  end;

end;

procedure TQueryDesigner.GenerateTableControl(CursorPos: TPoint;
  parent: TWinControl; Schema, Tablename: string);

var
  ti: TTableInfo;
  outPut: TStringList;
  pnlTable: TPanel;
  tlbToolbar: TToolBar;
  lblTableCaption: TLabel;
  field: TFieldInfo;
  fieldPanel: TPanel;
  lblFieldCaption: TLabel;
  ik: TImportedKey;
  I: integer;
  tableWidth: integer;

begin

  try

    if FindTableControl(Tablename) = nil then
    begin

      ti := TableInfoList.GetTableInfo(Schema, Tablename);
      if ti <> nil then
      begin

        TableInfoList.Add(ti);
        tableWidth := 10;

        pnlTable := TPanel.Create(parent);
        parent.InsertControl(pnlTable);
        pnlTable.Height := 70;
        pnlTable.Width := 50;
        pnlTable.Hint := Tablename;
        pnlTable.Caption := '';
        pnlTable.Left := CursorPos.X;
        pnlTable.Top := CursorPos.Y;
        pnlTable.ControlStyle := pnlTable.ControlStyle + [csDisplayDragImage];

        for I:=ti.AllFields.Count-1 downto 0 do
        begin
          field := ti.AllFields[I];
          fieldPanel := TPanel.Create(pnlTable);
          pnlTable.InsertControl(fieldPanel);
          fieldPanel.ParentBackground := False;
          fieldPanel.Height := 20;
          fieldPanel.Align := alTop;
          fieldPanel.Color := clWhite;
          lblFieldCaption := TLabel.Create(fieldPanel);
          fieldPanel.InsertControl(lblFieldCaption);
          lblFieldCaption.Align := alClient;
          lblFieldCaption.Caption := field.fieldName;
          // lblFieldCaption.Enabled := False;
          lblFieldCaption.Layout := tlCenter;
          fieldPanel.Hint := field.fieldName;
//          fieldPanel.DragMode := dmAutomatic;
          fieldPanel.OnStartDrag := OnFieldStartDrag;
          fieldPanel.OnDragOver := OnFieldDragOver;
          fieldPanel.OnDragDrop := OnFieldDragDrop;
          fieldPanel.OnMouseDown := OnFieldMouseDown;
          fieldPanel.OnMouseUp := OnFieldMouseUp;
          fieldPanel.OnMouseMove := OnFieldMouseMove;
          fieldPanel.ControlStyle := fieldPanel.ControlStyle + [csDisplayDragImage];
          fieldPanel.Tag := 1;
          lblFieldCaption.Enabled := False;
          fieldPanel.Caption := '';
          if pbArea.Canvas.TextWidth(field.fieldName) > tableWidth then
            tableWidth := pbArea.Canvas.TextWidth(field.fieldName) + 10;

        end;

        tlbToolbar := TToolBar.Create(pnlTable);
        pnlTable.InsertControl(tlbToolbar);
        tlbToolbar.Height := 13;
        tlbToolbar.DrawingStyle := dsGradient;
        tlbToolbar.AutoSize := True;
        tlbToolbar.Enabled := False;

        lblTableCaption := TLabel.Create(pnlTable);
        tlbToolbar.InsertControl(lblTableCaption);

        lblTableCaption.Caption := Tablename;
        lblTableCaption.Layout := tlCenter;
        lblTableCaption.Alignment := taCenter;

        pnlTable.Width := tableWidth;

        if pbArea.Canvas.TextWidth(lblTableCaption.Caption) > pnlTable.Width
        then
          pnlTable.Width := pbArea.Canvas.TextWidth
            (lblTableCaption.Caption) + 10;

        pnlTable.Height := 20 * (ti.AllFields.Count) + (tlbToolbar.Height + 2);

        lblTableCaption.Width := pnlTable.Width;

        pnlTable.Name := ti.TableCSharpName;

        AssignEvents(pnlTable);
        pnlTable.BringToFront;

        if ti.ImportedKeys <> nil then
          if ti.ImportedKeys.Count > 0 then
          begin

            for I := 0 to ti.ImportedKeys.Count - 1 do
            begin
              if ti.ImportedKeys[I].ForiegnTableName <> Tablename then
                GenerateTableControl(Point(CursorPos.X + pnlTable.Width + 50,
                  CursorPos.Y), parent, DbSchema,
                  ti.ImportedKeys[I].ForiegnTableName);
            end;

          end;

      end;
      FCurrentNodeControl := pnlTable;
      PositionNodes(FCurrentNodeControl);
    end;

  except
    on e: Exception do
      ShowMessage(e.Message);
  end;

end;

function TQueryDesigner.GetSqlQuery: string;
begin
  Result := txtSqlQuery.Text;
end;

procedure TQueryDesigner.NodeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Sender is TWinControl) then
  begin
    FNodePositioning := True;
    SetCapture(TWinControl(Sender).Handle);
    GetCursorPos(oldPos);
  end;
end;

procedure TQueryDesigner.NodeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  newPos: TPoint;
  frmPoint: TPoint;
  OldRect: TRect;
  AdjL, AdjR, AdjT, AdjB: boolean;
begin
  newPos := Point(0, 0);
  if FNodePositioning then
  begin
    begin
      with TWinControl(Sender) do
      begin
        GetCursorPos(newPos);
        with FCurrentNodeControl do
        begin // resize
          frmPoint := FCurrentNodeControl.parent.ScreenToClient
            (Mouse.CursorPos);
          OldRect := FCurrentNodeControl.BoundsRect;
          AdjL := False;
          AdjR := False;
          AdjT := False;
          AdjB := False;
          case FNodes.IndexOf(TWinControl(Sender)) of
            0:
              begin
                AdjL := True;
                AdjT := True;
              end;
            1:
              begin
                AdjT := True;
              end;
            2:
              begin
                AdjR := True;
                AdjT := True;
              end;
            3:
              begin
                AdjR := True;
              end;
            4:
              begin
                AdjR := True;
                AdjB := True;
              end;
            5:
              begin
                AdjB := True;
              end;
            6:
              begin
                AdjL := True;
                AdjB := True;
              end;
            7:
              begin
                AdjL := True;
              end;
          end;

          if AdjL then
            OldRect.Left := frmPoint.X;
          if AdjR then
            OldRect.Right := frmPoint.X;
          if AdjT then
            OldRect.Top := frmPoint.Y;
          if AdjB then
            OldRect.Bottom := frmPoint.Y;
          SetBounds(OldRect.Left, OldRect.Top, OldRect.Right - OldRect.Left,
            OldRect.Bottom - OldRect.Top);
        end;
        Left := Left - oldPos.X + newPos.X;
        Top := Top - oldPos.Y + newPos.Y;
        oldPos := newPos;
      end;
    end;
    PositionNodes(FCurrentNodeControl);
    // PositionLines(FCurrentNodeControl);
  end;

end;

procedure TQueryDesigner.NodeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FNodePositioning then
  begin
    Screen.Cursor := crDefault;
    ReleaseCapture;
    FNodePositioning := False;
  end;
end;

procedure TQueryDesigner.OnFieldDragDrop(Sender, Source: TObject;
  X, Y: integer);
var
  foreignTable: string;
  foreignKey: string;
  foreignSchema: string;

  sourceField: string;
  sourceTable: string;
  selectFields:TStringList;
  ti: TTableInfo;
  foreignTi: TTableInfo;
  ik: TImportedKey;
  fi: TFieldInfo;
  I:Integer;
begin
  foreignTable := ((Sender as TPanel).parent as TPanel).Hint;
  foreignKey := (Sender as TPanel).Hint;

  if not (Source is TDragFieldObject) then Exit;


  sourceField := ((Source as TDragFieldObject).Control as TPanel).Hint;
  sourceTable := (((Source as TDragFieldObject).Control as TPanel).parent as TPanel).Hint;

  ti := TableInfoList.TableByName(sourceTable);
  foreignTi := TableInfoList.TableByName(foreignTable);

  ik := ti.ImportedKeys.GetByName(foreignKey);

  QueryDesignerDialog.cmbForeignTable.Text := foreignTable;
  QueryDesignerDialog.cmbValueMember.Text := foreignKey;

  if ik <> nil then
  begin
    foreignSchema := ik.ForiegnSchema;
    selectFields := ik.SelectFields;
  end else
  selectFields := TStringList.Create;

  if foreignSchema = '' then
    foreignSchema := DbSchema;
  QueryDesignerDialog.grbConfigure.Caption := 'Source: [' + sourceTable + '.' +
    sourceField + ']';



  if QueryDesignerDialog.ShowModal(DatabaseInfo, foreignSchema, sourceTable,sourceField, foreignTable,
    foreignKey, selectFields) = mrOk then
  begin
    if ik = nil then
    begin
      ik := TImportedKey.Create;
      ti.ImportedKeys.Add(ik);
    end;

    for fi in ti.AllFields do
    begin
      if fi.fieldName = sourceField then
      begin
        fi.IsReference := True;
        Break;
      end;
    end;

    ik.ForiegnSchema := DbSchema;
    ik.Tablename := (((Source as TDragFieldObject).Control as TPanel).parent as TPanel).Hint;
    ik.ColumnName := QueryDesignerDialog.cmbSourceField.Text;

    ik.ForiegnTableName := QueryDesignerDialog.cmbForeignTable.Text;
    ik.ForiegnColumnName := QueryDesignerDialog.cmbValueMember.Text;
    ik.SelectFields.Clear;
    ik.SelectFields.AddStrings(QueryDesignerDialog.SelectFields);

  end;

  (Sender as TPanel).Color := clWhite;
  pbArea.Invalidate;
end;

procedure TQueryDesigner.OnFieldDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
    Accept := False;
  if IsDragObject(Source) then
    with TDragFieldObject(Source) do
      if Control is TPanel then
      begin
        if Control is TPanel then
        begin
          (Control as TPanel).Tag := 1;
          Accept := True;
        end;
                
      end;
end;

procedure TQueryDesigner.OnFieldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  fi: TFieldInfo;
  ik: TImportedKey;
  ti :TTableInfo;
  fieldName: string;
  Tablename: string;
begin
  (Sender as TPanel).BeginDrag(False);
  if (Sender is TPanel) then
  begin
    if Button = mbRight then
    begin
      (Sender as TPanel).Color := clRed;
      IsFieldDragging := True;
    end;
  end;

  if Button = mbRight then     
  if (Sender is TPanel) then
  begin
    (Sender as TPanel).Color := clWhite;
    IsFieldDragging := False;
    fieldName := (Sender as TPanel).Hint;
    Tablename := ((Sender as TPanel).parent as TPanel).Hint;

    ti := TableInfoList.TableByName(Tablename);
    if ti <> nil then
    begin
      for ik in ti.ImportedKeys do
      begin
        if ik <> nil then
          if ik.ColumnName = fieldName then
          begin
            ik.Tablename := '';
            ik.ColumnName := '';
            ik.ForiegnSchema := '';
            ik.ForiegnColumnName := '';
            ik.SelectFields.Clear;

            Break;
          end;
      end;
      fi := ti.FieldByName(fieldName);
      if fi <> nil then
        fi.IsReference := False;

    end;
    pbArea.Invalidate;
  end;

end;

procedure TQueryDesigner.OnFieldMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin

end;

procedure TQueryDesigner.OnFieldMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin

end;

procedure TQueryDesigner.OnFieldStartDrag(Sender: TObject; var DragObject: TDragObject);
var
 d:TDragFieldObject;
begin
  d := TDragFieldObject.Create((Sender as TPanel));
  d.DragText := (Sender as TPanel).Hint;
  DragObject := d;
end;

procedure TQueryDesigner.pbAreaClick(Sender: TObject);
begin
  FCurrentNodeControl := nil;
end;

procedure TQueryDesigner.pbAreaPaint(Sender: TObject);
var
  ti: TTableInfo;
  ik: TImportedKey;

  pnlSourceTable: TPanel;
  pnlSourceField: TPanel;

  pnlDestTable: TPanel;
  pnlDestField: TPanel;
  I: integer;

  sourceRect: TRect;
  destRect: TRect;
  startPoint: TPoint;
  endPoint: TPoint;
  arrow: TBitmap;
begin

  try

    for I := 0 to TableInfoList.Count - 1 do
    begin
      ti := TableInfoList[I];
      if ti = nil then
        Continue;

      pnlSourceTable := FindTableControl(ti.Tablename);
      if pnlSourceTable <> nil then
      begin
        pbArea.Canvas.Pen.Color := clBlue;
        pbArea.Canvas.Pen.Width := 2;

        if ti.ImportedKeys <> nil then
          for ik in ti.ImportedKeys do
          begin
            arrow := ArrowImageToRight;
            pnlSourceField := FindFieldControl(pnlSourceTable, ik.ColumnName);
            if pnlSourceField <> nil then
            begin
              // pbArea.Canvas.Pen.Style := psAlternate;
              startPoint := Point(pnlSourceTable.Left,
                pnlSourceTable.Top + pnlSourceField.Top + 10);

              pbArea.Canvas.MoveTo(startPoint.X, startPoint.Y);

              pnlDestTable := FindTableControl(ik.ForiegnTableName);
              if pnlDestTable <> nil then
              begin

                if pnlDestTable.Left > pnlSourceTable.Left then
                begin
                  startPoint :=
                    Point(pnlSourceTable.Left + pnlSourceTable.Width,
                    pnlSourceTable.Top + pnlSourceField.Top + 10);
                end;

                pnlDestField := FindFieldControl(pnlDestTable,
                  ik.ForiegnColumnName);

                if pnlDestField <> nil then
                  if pnlDestTable.Left > pnlSourceTable.Left then
                  begin
                    endPoint := Point(pnlDestTable.Left,
                      pnlDestTable.Top + pnlDestField.Top + 10);
                  end
                  else
                  begin
                    endPoint := Point(pnlDestTable.Left + pnlDestTable.Width +
                      ArrowImageToLeft.Width,
                      pnlDestTable.Top + pnlDestField.Top - 10);
                    arrow := ArrowImageToLeft;
                  end;

                sourceRect := Rect(startPoint.X - 5, startPoint.Y - 5,
                  startPoint.X + 5, startPoint.Y + 5);

                destRect := Rect(endPoint.X - 8, endPoint.Y - 5, endPoint.X + 5,
                  endPoint.Y + 5);

                // DRAW LINE FROM SOURCE TO DEST FIELD
                pbArea.Canvas.Pen.Color := clBlue;
                pbArea.Canvas.MoveTo(sourceRect.Left + sourceRect.Width,
                  sourceRect.Top + 5);
                pbArea.Canvas.LineTo(destRect.Left, destRect.Top + 5);

                if Assigned(arrow) and Assigned(RectImage) then
                begin
                  pbArea.Canvas.StretchDraw(sourceRect, RectImage);
                  pbArea.Canvas.Draw(destRect.Left, destRect.Top, arrow);
                end;

              end;

            end;

          end;

      end;
    end;

  except

  end;

end;

procedure TQueryDesigner.PositionNodes(AroundControl: TWinControl);
var
  Node, T, L, CT, CL, FR, FB, FT, FL: integer;
  TopLeft: TPoint;
begin
  FCurrentNodeControl := nil;
  if AroundControl = nil then
    Exit;

  for Node := 0 to 7 do
  begin
    with AroundControl do
    begin
      CL := (Width div 2) + Left - 2;
      CT := (Height div 2) + Top - 2;
      FR := Left + Width - 2;
      FB := Top + Height - 2;
      FT := Top - 2;
      FL := Left - 2;
      case Node of
        0:
          begin
            T := FT;
            L := FL;
          end;
        1:
          begin
            T := FT;
            L := CL;
          end;
        2:
          begin
            T := FT;
            L := FR;
          end;
        3:
          begin
            T := CT;
            L := FR;
          end;
        4:
          begin
            T := FB;
            L := FR;
          end;
        5:
          begin
            T := FB;
            L := CL;
          end;
        6:
          begin
            T := FB;
            L := FL;
          end;
        7:
          begin
            T := CT;
            L := FL;
          end;
      else
        T := 0;
        L := 0;
      end;
      TopLeft := parent.ClientToScreen(Point(L, T));
    end;
    with TPanel(FNodes[Node]) do
    begin
      TopLeft := parent.ScreenToClient(TopLeft);
      Top := TopLeft.Y;
      Left := TopLeft.X;
    end;
  end;
  FCurrentNodeControl := AroundControl;
  pbArea.Invalidate;
  SetNodesVisible(True);
end;

procedure TQueryDesigner.RemoveTable(Tablename: string);
var
  I: integer;
  ti: TTableInfo;
  c: TControl;
begin
  I := -1;
  I := TableInfoList.IndexOf(Tablename);

  if I > -1 then
  begin
    c := scbDesigner.FindChildControl(TableInfoList[I].TableCSharpName);
    if c <> nil then
      c.Free;

    if TableInfoList[I] <> nil then
    begin
      FCurrentNodeControl := nil;
      SetNodesVisible(False);
      TableInfoList[I].Tablename := '';
    end;
  end;

end;

procedure TQueryDesigner.scbDesignerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  scbDesigner.ScrollBy(0,3);
  pbArea.Invalidate;
  if FCurrentNodeControl<>nil then
  PositionNodes(FCurrentNodeControl);
end;

procedure TQueryDesigner.scbDesignerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
    scbDesigner.ScrollBy(0,-3);
    pbArea.Invalidate;
    if FCurrentNodeControl<>nil then
  PositionNodes(FCurrentNodeControl);
end;

procedure TQueryDesigner.scbDesignerResize(Sender: TObject);
begin
  try
    pbArea.Invalidate;
    if FCurrentNodeControl<>nil then
    PositionNodes(FCurrentNodeControl);
  except
  end;
end;

procedure TQueryDesigner.SetArrowImage(const Value: TBitmap);
begin
  FArrowImage := Value;
end;

procedure TQueryDesigner.SetArrowImageToLeft(const Value: TBitmap);
begin
  FArrowImageToLeft := Value;
end;

procedure TQueryDesigner.SetArrowImageToRight(const Value: TBitmap);
begin
  FArrowImage := Value;
end;

procedure TQueryDesigner.SetDatabaseInfo(const Value: TDatabaseInfo);
begin
  FDatabaseInfo := Value;
  if TableInfoList = nil then
    TableInfoList := TTableInfos.Create(FDatabaseInfo);

end;

procedure TQueryDesigner.SetFCurrentNodeControl(const Value: TWinControl);
begin
  FFCurrentNodeControl := Value;
  UpdateToolbar;
  SetNodesVisible(FFCurrentNodeControl <> nil);
end;

procedure TQueryDesigner.SetNodesVisible(Visible: boolean);
var
  Node: integer;
begin
  for Node := 0 to 7 do
    TWinControl(FNodes.Items[Node]).Visible := Visible;
end;

procedure TQueryDesigner.SetRectImage(const Value: TBitmap);
begin
  FRectImage := Value;
end;

procedure TQueryDesigner.SetTableInfoList(const Value: TTableInfos);
begin
  FTableInfoList := Value;
end;

procedure TQueryDesigner.Splitter1Paint(Sender: TObject);
var
  s: string;
begin
  // s := 'SQL Quqery';
  // Splitter1.Canvas.TextOut((Splitter1.Width div 2) -
  // (Splitter1.Canvas.TextWidth(s) div 2), 2, 'SQL Query');
end;

procedure TQueryDesigner.UpdateToolbar;
begin
  btnGenerateQuery.Enabled := FCurrentNodeControl <> nil;
  btnDeleteTable.Enabled := FCurrentNodeControl <> nil;
end;

{ TMyDragControlObject }

destructor TDragFieldObject.Destroy;
begin
   FDragImages.Free;
  inherited;
end;

function TDragFieldObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if FPrevAccepted <> Accepted then
    with FDragImages do
    begin
      EndDrag;
      SetDragImage(Ord(Accepted), 0, 0);
      BeginDrag(GetDesktopWindow, X, Y);
    end;
  FPrevAccepted := Accepted;
  Result := inherited GetDragCursor(Accepted, X, Y);
end;

function TDragFieldObject.GetDragImages: TDragImageList;
const
  SNoDrop = '-';
  Margin = 0;
var
  Bmp: TBitmap;
begin
  if FDragImages = nil then
  begin
    FDragImages := TDragImageList.Create(nil);
    Bmp := TBitmap.Create;
    try
      //Bmp.Canvas.Font.Assign(TControlAccess(Control).Font);
      Bmp.Canvas.Brush.Color := clRed;
      Bmp.Canvas.Font.Size := 10;
      Bmp.Width :=  Bmp.Canvas.TextWidth(FDragText);
      Bmp.Height := Bmp.Canvas.TextHeight(FDragText);
      Bmp.Canvas.TextOut(Margin, 0, SNoDrop);

      
      Bmp.Canvas.Brush.Color := clSkyBlue;
      FDragImages.Width := Bmp.Width;
      FDragImages.Height := Bmp.Height;
      FDragImages.Add(Bmp, nil);

      Bmp.Width := Bmp.Canvas.TextWidth(FDragText);
      Bmp.Height := Bmp.Canvas.TextHeight(FDragText);
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Bmp.Canvas.TextOut(Margin, 0, FDragText);
      FDragImages.Add(Bmp, nil);
      FDragImages.SetDragImage(0, 0, 0);
    finally
      Bmp.Free;
    end;
  end;
  Result := FDragImages;
end;

procedure TDragFieldObject.SetDragText(const Value: string);
begin
  FDragText := Value;
end;

end.
