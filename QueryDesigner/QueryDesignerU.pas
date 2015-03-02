unit QueryDesignerU;

{$MODE objfpc}

interface

uses
  SysUtils, Variants,
  Classes,
  Graphics, Controls, Forms, Dialogs, Contnrs,
  ExtCtrls, AsTableInfo, AsDbType, StdCtrls, ToolWin,
  ComCtrls, ImgList, AsSqlGenerator, GraphUtil,
  LCL, LCLType, LCLIntf, Menus;

type

  { TQueryDesigner }

  TQueryDesigner = class(TFrame)
   ArrowRightImage: TImage;
   ArrowLeftImage: TImage;
   mitEditVirtualRelations: TMenuItem;
   RectangleImage: TImage;
    // GUI CONTROLS DECLARE
    scbDesigner: TScrollBox;
    pbArea: TPaintBox;
    tlbToolbox: TToolBar;
    btnGenerateQuery: TToolButton;
    ilToolboxImages: TImageList;
    btnAddTable: TToolButton;
    btnProperties: TToolButton;
    txtSqlQuery: TMemo;
    ToolButton1: TToolButton;
    btnDeleteTable: TToolButton;
    btnClear: TToolButton;
    Splitter1: TSplitter;
    pnlDesigner: TPanel;
    // CONTROLS END

    procedure btnAddTableClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure mitEditVirtualRelationsClick(Sender: TObject);
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
   FDbInfo: TAsDbConnectionInfo;
   FSchema: String;
    { Private declarations }

    inReposition: boolean;
    FNodePositioning: boolean;
    oldPos: TPoint;
    SelectedControl: TObject;
    ControlPressed: boolean;
    FNodes: TObjectList;
    FTableInfoList: TAsTableInfos;
    IsFieldDragging: boolean;
    FFCurrentNodeControl: TWinControl;
    procedure SetDbInfo(AValue: TAsDbConnectionInfo);
    procedure SetFCurrentNodeControl(const Value: TWinControl);
    procedure SetSchema(AValue: String);
    property FCurrentNodeControl: TWinControl read FFCurrentNodeControl
      write SetFCurrentNodeControl;

    procedure CreateNodes;
    procedure PositionNodes(AroundControl: TWinControl);
    procedure SetNodesVisible(aVisible: boolean);
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

    procedure AddVirtualRelation(sourceTable, sourceField, foreignTable,
     foreignKey: string);
    function GenerateTableControl(CursorPos: TPoint; aParent: TWinControl;
      Schema, Tablename: string):Boolean;

    procedure SetTableInfoList(const Value: TAsTableInfos);

    function FindTableControl(Tablename: string): TPanel;
    function FindFieldControl(tableControl: TPanel; fieldName: string): TPanel;
    function GetSqlQuery: TStrings;

    procedure UpdateToolbar;

  public
    { Public declarations }
    FDbSchema: string;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function AddTable(Schema, Tablename: string; Position: TPoint):Boolean;
    procedure Clear;
    procedure RemoveTable(Tablename: string);

    procedure GenerateQuery;

    property TableInfoList: TAsTableInfos read FTableInfoList
      write SetTableInfoList;
    property Schema:String read FSchema write SetSchema;

    property SqlQuery: TStrings read GetSqlQuery;
    property DbInfo:TAsDbConnectionInfo read FDbInfo write SetDbInfo;


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
//type
  //TArrayOfPixel = array [0 .. CMAX_BITMAP_WIDTH - 1] of TRGBTriple;
  //PArrayOfPixel = ^TArrayOfPixel;

implementation

uses QueryDesignerDialogU, QueryDesignerTablesU, QueryDesignerPropertyGridU,
 AsSqlParser;

{$R *.lfm}
{ TQueryDesignerFrame }

function TQueryDesigner.AddTable(Schema, Tablename: string; Position: TPoint
 ): Boolean;
begin
  FDbSchema := Schema;
  Result := GenerateTableControl(Position, scbDesigner, Schema, Tablename);
end;

procedure TQueryDesigner.AssignEvents(ctrl: TObject);
begin
  if (ctrl is TPanel) then
  begin
    (ctrl as TPanel).OnMouseDown := @ControlMouseDown;
    (ctrl as TPanel).OnMouseUp := @ControlMouseUp;
    (ctrl as TPanel).OnMouseMove := @ControlMouseMove;
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
  ti: TAsTableInfo;
  a: TPanel;
begin

  txtSqlQuery.Clear;

  //the first one is paintbox
  for I:=1 to scbDesigner.ControlCount-1 do
  begin
    scbDesigner.Controls[1].Free;
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
    //SetCapture(TWinControl(Sender).Handle);
    SetCaptureControl(TControl(Sender));
    //GetCursorPos(oldPos);
    oldPos := Mouse.CursorPos;
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
      //GetCursorPos(newPos);
      newPos := Mouse.CursorPos;;
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
  CtrlCaption: AnsiString;
begin
  CtrlCaption := '';
  if inReposition then
  begin
    Screen.Cursor := crDefault;
    //ReleaseCapture;
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

constructor TQueryDesigner.Create(aOwner: TComponent);
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
        OnMouseDown := @NodeMouseDown;
        OnMouseMove := @NodeMouseMove;
        OnMouseUp := @NodeMouseUp;
      end;
    end;
  end;
end;

destructor TQueryDesigner.Destroy;
begin
  if FNodes <> nil then
    FNodes.Free;
   if Assigned(TableInfoList) then
   TableInfoList.Free;
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
  p: TAsProcedureNames;
  s: TAsSqlGenerator;
  ti: TAsTableInfo;
  outPut: TStringList;
begin

  try
    try
      ti := TableInfoList.TableByName(FCurrentNodeControl.Hint);
      if ti <> nil then
      begin
        s := TAsSqlGenerator.Create(FDbInfo,p);
        outPut := s.GenerateQuery(0, ti, qtSelect);
        txtSqlQuery.Lines.AddStrings(outPut);
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

function TQueryDesigner.GenerateTableControl(CursorPos: TPoint;
 aParent: TWinControl; Schema, Tablename: string): Boolean;

var
  ti: TAsTableInfo;
  outPut: TStringList;
  pnlTable: TPanel;
  tlbToolbar: TToolBar;
  lblTableCaption: TLabel;
  field: TAsFieldInfo;
  fieldPanel: TPanel;
  lblFieldCaption: TLabel;
  ik: TAsImportedKeyInfo;
  I: integer;
  tableWidth: integer;

begin

  Result := False;
  if TableInfoList.TableByName(Tablename)<>nil then
  exit;

  try

    if FindTableControl(Tablename) = nil then
    begin


        ti := TableInfoList.Add(Schema,Tablename);
        tableWidth := 10;

        pnlTable := TPanel.Create(aParent);
        aParent.InsertControl(pnlTable);
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
          fieldPanel.OnStartDrag := @OnFieldStartDrag;
          fieldPanel.OnDragOver := @OnFieldDragOver;
          fieldPanel.OnDragDrop := @OnFieldDragDrop;
          fieldPanel.OnMouseDown := @OnFieldMouseDown;
          fieldPanel.OnMouseUp := @OnFieldMouseUp;
          fieldPanel.OnMouseMove := @OnFieldMouseMove;
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

        pnlTable.Name := ti.TableNameAsControlName;

        AssignEvents(pnlTable);
        pnlTable.BringToFront;

        if ti.ImportedKeys <> nil then
          if ti.ImportedKeys.Count > 0 then
          begin

            for I := 0 to ti.ImportedKeys.Count - 1 do
            begin
              if ti.ImportedKeys[I].ForeignTableName <> Tablename then
                GenerateTableControl(Point(CursorPos.X + pnlTable.Width + 50,
                  CursorPos.Y), aParent, FDbSchema,
                  ti.ImportedKeys[I].ForeignTableName);
            end;

          end;


      FCurrentNodeControl := pnlTable;
      PositionNodes(FCurrentNodeControl);
      Result := True;
    end;

  except
    on e: Exception do
      ShowMessage(e.Message);
  end;

end;

function TQueryDesigner.GetSqlQuery: TStrings;
begin
  Result := txtSqlQuery.Lines;
end;

procedure TQueryDesigner.NodeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Sender is TWinControl) then
  begin
    FNodePositioning := True;
    //SetCapture(TWinControl(Sender).Handle);
    SetCaptureControl(TControl(Sender));
    //Mouse.Capture;
    //GetCursorPos(oldPos);
    oldPos := Mouse.CursorPos;

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
        //GetCursorPos(newPos);
        newPos := Mouse.CursorPos;
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
    //ReleaseCapture;

    FNodePositioning := False;
  end;
end;

procedure TQueryDesigner.OnFieldDragDrop(Sender, Source: TObject;
  X, Y: integer);
var
  foreignTable: string;
  foreignKey: string;
  sourceField: string;
  sourceTable: string;
begin
  if not (Source is TDragFieldObject) then Exit;

  foreignTable := ((Sender as TPanel).parent as TPanel).Hint;
  foreignKey := (Sender as TPanel).Hint;
  sourceField := ((Source as TDragFieldObject).Control as TPanel).Hint;
  sourceTable := (((Source as TDragFieldObject).Control as TPanel).parent as TPanel).Hint;

  AddVirtualRelation(sourceTable,sourceField,foreignTable,foreignKey);

  (Sender as TPanel).Color := clWhite;
  pbArea.Invalidate;
end;

procedure TQueryDesigner.OnFieldDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
    Accept := False;

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
  fi: TAsFieldInfo;
  ik: TAsImportedKeyInfo;
  ti :TAsTableInfo;
  fieldName: string;
  Tablename: string;
  index:integer;
  i: Integer;
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

      for i:=0 to ti.ImportedKeys.Count-1 do
      begin
        //if ik <> nil then
        //  if ik.ColumnName = fieldName then
        //  begin
        //    ik.Tablename := '';
        //    ik.ColumnName := '';
        //    ik.ForeignSchema := '';
        //    ik.ForeignColumnName := '';
        //    ik.SelectFields.Clear;
        //
        //    Break;
        //  end;
        if ti.ImportedKeys[I].ColumnName = fieldName then
        begin
          index:=i;
          Break;
        end;
      end;

      ti.ImportedKeys.Delete(Index);

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

procedure TQueryDesigner.AddVirtualRelation(sourceTable,sourceField,foreignTable,foreignKey: string);
var
  foreignSchema: string;
  selectFields:TStringList;
  ti: TAsTableInfo;
  foreignTi: TAsTableInfo;
  ik: TAsImportedKeyInfo;
  fi: TAsFieldInfo;
  I:Integer;
begin


  ti := TableInfoList.TableByName(sourceTable);
  foreignTi := TableInfoList.TableByName(foreignTable);

  ik := ti.ImportedKeys.GetByName(foreignKey);

  QueryDesignerDialog.cmbForeignTable.Text := foreignTable;
  QueryDesignerDialog.cmbValueMember.Text := foreignKey;

  if ik <> nil then
  begin
    foreignSchema := ik.ForeignSchema;
    selectFields := ik.SelectFields;
  end else
  selectFields := TStringList.Create;

  if foreignSchema = '' then
    foreignSchema := FDbSchema;
  QueryDesignerDialog.grbConfigure.Caption := 'Source: [' + sourceTable + '.' +
    sourceField + ']';



  if QueryDesignerDialog.ShowModal(FDbInfo, foreignSchema, sourceTable,sourceField, foreignTable,
    foreignKey, selectFields) = mrOk then
  begin
    if ik = nil then
    begin
      ik := ti.ImportedKeys.Add;
    end;

    for I:=0 to ti.AllFields.Count-1 do
    begin
      if ti.AllFields[I].fieldName = sourceField then
      begin
        ti.AllFields[I].IsReference := True;
        Break;
      end;
    end;

    ik.ForeignSchema := FDbSchema;
    ik.Tablename := sourceTable;
    ik.ColumnName := QueryDesignerDialog.cmbSourceField.Text;

    ik.ForeignTableName := QueryDesignerDialog.cmbForeignTable.Text;
    ik.ForeignColumnName := QueryDesignerDialog.cmbValueMember.Text;
    ik.SelectFields.Clear;
    ik.SelectFields.AddStrings(QueryDesignerDialog.SelectFields);

  end;
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
  ti: TAsTableInfo;
  ik: TAsImportedKeyInfo;

  pnlSourceTable: TPanel;
  pnlSourceField: TPanel;

  pnlDestTable: TPanel;
  pnlDestField: TPanel;
  I,J: integer;

  sourceRect: TRect;
  destRect: TRect;
  startPoint: TPoint;
  endPoint: TPoint;
  arrow: TBitmap;
  midPoint1:TPoint;
  midPoint2:TPoint;
  midX: Integer;
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
          for J:=0 to ti.ImportedKeys.Count-1 do
          begin
             ik := ti.ImportedKeys[J];
            arrow := ArrowRightImage.Picture.Bitmap;
            pnlSourceField := FindFieldControl(pnlSourceTable, ik.ColumnName);
            if pnlSourceField <> nil then
            begin
              // pbArea.Canvas.Pen.Style := psAlternate;
              startPoint := Point(pnlSourceTable.Left,
                pnlSourceTable.Top + pnlSourceField.Top + 10);

              pbArea.Canvas.MoveTo(startPoint.X, startPoint.Y);

              pnlDestTable := FindTableControl(ik.ForeignTableName);
              if pnlDestTable <> nil then
              begin

                if pnlDestTable.Left > pnlSourceTable.Left then
                begin
                  startPoint :=
                    Point(pnlSourceTable.Left + pnlSourceTable.Width,
                    pnlSourceTable.Top + pnlSourceField.Top + 10);
                end;

                pnlDestField := FindFieldControl(pnlDestTable,
                  ik.ForeignColumnName);

                if pnlDestField <> nil then
                  if pnlDestTable.Left > pnlSourceTable.Left then
                  begin
                    endPoint := Point(pnlDestTable.Left,
                      pnlDestTable.Top + pnlDestField.Top + 10);
                  end
                  else
                  begin
                    endPoint := Point(pnlDestTable.Left + pnlDestTable.Width +25
                      {ArrowImageToLeft.Width},
                      pnlDestTable.Top + pnlDestField.Top - 10);
                    arrow := ArrowLeftImage.Picture.Bitmap;
                  end;

                sourceRect := Rect(startPoint.X - 5, startPoint.Y - 5,
                  startPoint.X + 5, startPoint.Y + 5);

                destRect := Rect(endPoint.X - 8, endPoint.Y - 5, endPoint.X + 5,
                  endPoint.Y + 5);

                // DRAW LINE FROM SOURCE TO DEST FIELD
                pbArea.Canvas.Pen.Color := clBlue;
                pbArea.Canvas.Pen.Cosmetic:=True;
                //pbArea.Canvas.MoveTo((sourceRect.Left + sourceRect.Right),(sourceRect.Top + 5));
                //pbArea.Canvas.LineTo(destRect.Left, destRect.Top + 5);

                midX := ((startPoint.x + endPoint.x) div 2 );

                midPoint1 := Point(midx,startPoint.y);
                midPoint2 := Point(midx,endPoint.y);

                if startpoint.y = endpoint.y then
                begin
                 pbArea.Canvas.Line(startPoint,endPoint);
                end else
                begin
                 pbArea.Canvas.Line(startPoint,midPoint1);
                 pbArea.Canvas.Line(midPoint1,midPoint2);
                 pbArea.Canvas.Line(midPoint2,endPoint);
                end;

                if Assigned(arrow) then
                begin
                  arrow.TransparentColor:=clWhite;
                  arrow.Transparent:=True;
                  pbArea.Canvas.StretchDraw(sourceRect, RectangleImage.Picture.Bitmap);
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
  ti: TAsTableInfo;
  c: TControl;
begin
  I := -1;
  I := TableInfoList.IndexOf(Tablename);

  if I > -1 then
  begin
    c := scbDesigner.FindChildControl(TableInfoList[I].TableNameAsControlName);

    if c <> nil then
    c.Free;

    TableInfoList.TableByName(Tablename).Free;

    //if TableInfoList[I] <> nil then
    //begin
    //  FCurrentNodeControl := nil;
    //  SetNodesVisible(False);
    //  TableInfoList[I].Tablename := '';
    //end;
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

procedure TQueryDesigner.SetSchema(AValue: String);
begin
 if FSchema=AValue then Exit;
 FSchema:=AValue;
end;

procedure TQueryDesigner.SetFCurrentNodeControl(const Value: TWinControl);
begin
  FFCurrentNodeControl := Value;
  UpdateToolbar;
  SetNodesVisible(FFCurrentNodeControl <> nil);
end;

procedure TQueryDesigner.SetDbInfo(AValue: TAsDbConnectionInfo);
begin
 if FDbInfo=AValue then Exit;
 FDbInfo:=AValue;

 if TableInfoList = nil then
    TableInfoList := TAsTableInfos.Create(nil,FDBInfo);

end;

procedure TQueryDesigner.SetNodesVisible(aVisible: boolean);
var
  Node: integer;
begin
  for Node := 0 to 7 do
    TWinControl(FNodes.Items[Node]).Visible := aVisible;
end;


procedure TQueryDesigner.SetTableInfoList(const Value: TAsTableInfos);
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

procedure TQueryDesigner.btnAddTableClick(Sender: TObject);
var
  lst:TStringList;
  I: Integer;
  x,y:integer;
  lastControl:TControl;
begin

  try
    lst := TAsDbUtils.GetTablenames(FDbInfo);


    if QueryDesignerTables = nil then
    QueryDesignerTables := TQueryDesignerTables.Create(Self);

    with QueryDesignerTables do
    begin
      chkTables.Clear;
      for I:=0 to lst.Count-1 do
      begin
        chkTables.Items.Add(lst[I]);
      end;

      if ShowModal = mrOK then
      begin
        lastControl := scbDesigner.Controls[scbDesigner.ControlCount-1];

        if (lastControl is TPaintBox) then
        begin
          x := 50;
          y := 50;
        end else
        begin
          x := lastControl.Left + lastControl.Width;
          y := lastControl.Top;
        end;

        for I:=0 to chkTables.Count-1 do
        begin
          if chkTables.Checked[I] then
          begin
            if (AddTable(FSchema,chkTables.Items[I],Point(x,y))) then
            begin
              lastControl := scbDesigner.Controls[scbDesigner.ControlCount-1];
              Inc(x,lastControl.Left+lastControl.Width+10);
            end;
          end;
        end;
      end;
    end;
  finally
    lst.Free;
  end;
end;

procedure TQueryDesigner.btnPropertiesClick(Sender: TObject);
var
  ti:TAsTableInfo;
begin
  ti := TableInfoList.TableByName(FCurrentNodeControl.Hint);
  if ti <>nil then
  begin
    QueryDesignerPropertyGrid.pnlTop.Caption:=ti.Tablename;
    QueryDesignerPropertyGrid.TIPropertyGrid1.TIObject:=ti;
    QueryDesignerPropertyGrid.ShowModal;
    QueryDesignerPropertyGrid.TIPropertyGrid1.TIObject:=nil;
  end;

end;

procedure TQueryDesigner.FrameClick(Sender: TObject);
begin

end;

procedure TQueryDesigner.mitEditVirtualRelationsClick(Sender: TObject);
var
  ti:TAsTableInfo;
begin
  ti := TableInfoList.TableByName(FCurrentNodeControl.Hint);
  if ti <>nil then
  begin
    QueryDesignerPropertyGrid.pnlTop.Caption:=ti.Tablename;
    QueryDesignerPropertyGrid.TIPropertyGrid1.TIObject:=ti;
    QueryDesignerPropertyGrid.ShowModal;
    QueryDesignerPropertyGrid.TIPropertyGrid1.TIObject:=nil;
  end;

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
      BeginDrag(Handle, X, Y);
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

initialization




end.
