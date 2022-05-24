{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit SqlExecThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqldb, mssqlconn, SQLDBLaz, AsDbType, AsParamDialog,
  strutils, ZDataset, syncobjs;

type

  { TSqlExecThread }

  TOnSqlExecThreadFinish = procedure(Sender: TObject; IsTableData:boolean) of object;

  TSqlExecThread = class(TThread)
  private
    FLock:TCriticalSection;
    FExecutionTime: TTime;
    FExecuteAsThread: boolean;
    FLastError: string;
    FMessage: string;
    FOnFinish: TOnSqlExecThreadFinish;
    FQuery: TAsQuery;
    FRecordCount: word;
    FSchema: string;
    FActive: boolean;
    FCommand: string;
    FTablenameAssigned:Boolean;
    FIsSelect:Boolean;
    function GetIsTerminated: Boolean;
    function GetOriginalQuery: string;
    procedure SetDurationTime(AValue: TTime);
    procedure SetExecuteAsThread(AValue: boolean);
    procedure SetLastError(AValue: string);
    procedure SetMessage(AValue: string);
    procedure SetOnFinish(AValue: TOnSqlExecThreadFinish);
    procedure SetRecordCount(AValue: word);
    procedure ShowParamDialog;
  protected
    procedure Execute; override;
    procedure SqlExecute;
  public


    constructor Create(Schema: string; const sqlQuery: TAsQuery; OnFinish: TOnSqlExecThreadFinish);
    property Active: boolean read FActive;
    property LastError: string read FLastError write SetLastError;
    procedure ExecuteSQL(sqlCommand: string; TableData:Boolean);
    property ExecuteAsThread: boolean read FExecuteAsThread write SetExecuteAsThread;
    property ExecutionTime: TTime read FExecutionTime;
    property RecordCount: word read FRecordCount;
    property OnFinish: TOnSqlExecThreadFinish read FOnFinish write SetOnFinish;
    property Message: string read FMessage write SetMessage;
    property IsSelect:Boolean read FIsSelect;
    property IsTerminated:Boolean read GetIsTerminated;
    destructor Destroy; override;

  end;

const
  ErrorMsg_ZeosNoResultSet = 'Can not open a Resultset';
  ErrorMsg_SqlDBNoResultSet = 'Cannot open a non-select statement';


implementation

uses AsSqlParser;

{ TSqlExecThread }

function IsUTF8String(S: string): boolean;
var
  WS: WideString;
begin
  WS := UTF8Decode(S);
  Result := (WS <> S) and (WS <> '');
end;


procedure TSqlExecThread.SetMessage(AValue: string);
begin
  if FMessage = AValue then
    Exit;
  FMessage := AValue;
end;

procedure TSqlExecThread.SetLastError(AValue: string);
begin
  if FLastError = AValue then
    Exit;
  FLastError := AValue;
end;

function TSqlExecThread.GetOriginalQuery: string;
begin
  Result := FCommand;
end;

function TSqlExecThread.GetIsTerminated: Boolean;
begin
 Result := Terminated;
end;


procedure TSqlExecThread.SetDurationTime(AValue: TTime);
begin
  if FExecutionTime = AValue then
    Exit;
  FExecutionTime := AValue;
end;

procedure TSqlExecThread.SetExecuteAsThread(AValue: boolean);
begin
  if FExecuteAsThread = AValue then
    Exit;
  FExecuteAsThread := AValue;
end;


procedure TSqlExecThread.SetOnFinish(AValue: TOnSqlExecThreadFinish);
begin
  if FOnFinish = AValue then
    Exit;
  FOnFinish := AValue;
end;


procedure TSqlExecThread.SetRecordCount(AValue: word);
begin
  if FRecordCount = AValue then
    Exit;
  FRecordCount := AValue;
end;

procedure TSqlExecThread.ShowParamDialog;
begin
   TAsParamDialog.ShowParamDialog(FQuery,True);
end;

procedure TSqlExecThread.Execute;
begin
  FLastError := EmptyStr;
  FActive := True;
  try
    try
      SqlExecute;
    except
      on e: Exception do
      begin
        FLastError := e.Message;
      end;
    end;

    if Assigned(FOnFinish) then
      FOnFinish(Self, FTablenameAssigned);
  finally
    FActive := False;
  end;
end;

procedure TSqlExecThread.SqlExecute;
var
  t1: TTime;
  affected:Integer;
begin
  //FLock.Acquire; //fails with zeos components
   FIsSelect:=AnsiContainsText(Lowercase(FCommand),'select') or AnsiContainsText(Lowercase(FCommand),'call') or
   AnsiContainsText(Lowercase(FCommand),'exec');
   try
     FLastError := '';
     Sleep(300);
     t1 := Time;
    if Assigned(FQuery) then
    begin
      FQuery.Close;
      FQuery.SQL.Text:=FCommand;
      FQuery.PacketRecords:=-1;
      try

       if FQuery.Params.Count>0 then
         Synchronize(@ShowParamDialog);

       FQuery.Open;
      except on E:Exception do
       begin
         if (E.Message=ErrorMsg_ZeosNoResultSet) or (E.Message=ErrorMsg_SqlDBNoResultSet) then
         begin
           try
            FIsSelect:= False;
            affected:=FQuery.RowsAffected;
            FMessage := 'Command successfully executed. Rows affected (' +IntToStr(affected) + ')';
           except
            raise;
           end;
         end else
         begin
          raise;
         end;

       end;
      end;

      if FIsSelect then
      begin
       FExecutionTime := Time - t1;
       FRecordCount := FQuery.RecordCount;
       FIsSelect:= True;
       FMessage := 'Execution time [' + TimeToStr(Time-t1) + '] Records [' +IntToStr(FRecordCount) + ']';
      end;

    end;

  finally
  //  FLock.Release;
  end;

end;

constructor TSqlExecThread.Create(Schema: string; const sqlQuery: TAsQuery;
 OnFinish: TOnSqlExecThreadFinish);
begin
  FLock := TCriticalSection.Create;
  FSchema := Schema;
  FQuery := sqlQuery;
  FExecuteAsThread := True;
  FOnFinish := OnFinish;
  inherited Create(True);
  inherited FreeOnTerminate:=True;
end;

procedure TSqlExecThread.ExecuteSQL(sqlCommand: string; TableData: Boolean);
begin
  FtableNameAssigned:=TableData;
  if Trim(sqlCommand) <> EmptyStr then
    FCommand := sqlCommand
  else
  begin
    raise Exception.Create('No SqlCommand');
  end;

  if FExecuteAsThread then
  begin
    inherited Start;
  end
  else
  begin
    SqlExecute;
  end;

end;

destructor TSqlExecThread.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

end.
