unit LoadingIndicator;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AnimatedGif,StdCtrls,ExtCtrls,LazSqlXResources;

type

  { TLoadingIndicator }

  TLoadingIndicator = class(TPaintBox)
  private
    FTimer:TTimer;
    FGif:TAnimatedGif;
    FLoaded:Boolean;
    procedure OnTimer(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);
    procedure StartAnimation;
    procedure StopAnimation;
    destructor Destroy;override;
  end;


implementation

{ TLoadingIndicator }


procedure TLoadingIndicator.OnTimer(Sender: TObject);
begin

  if Assigned(FGif) then
  begin
    Repaint;
    FGif.Show(Self.Canvas,Self.ClientRect);
  end else
  FTimer.Enabled:=False;

end;

constructor TLoadingIndicator.Create(AOwner: TComponent);
var
 r:TResourceStream;
begin
   inherited Create(AOwner);
   FLoaded:=False;
   FTimer := TTimer.Create(Self);
   FTimer.Enabled:=False;
   FTimer.OnTimer:=@OnTimer;
   FTimer.Interval:= 50;
   FGif := TAnimatedGif.Create;
   FGif.EraseColor:=Self.Color;
   FGif.BackgroundMode:=TGifBackgroundMode.gbmEraseBackground;
   Width:=16;
   Height:=16;
   Visible:=False;
   try
     FGif.LoadFromStream(TLazSqlXResources.LoadingGif);
     FLoaded := True;
   except

   end;
end;

procedure TLoadingIndicator.StartAnimation;
begin
  if FLoaded then
  begin
    Visible:=True;
    FTimer.Enabled:=True;
  end;
end;

procedure TLoadingIndicator.StopAnimation;
begin
  if FLoaded then
  begin
    FTimer.Enabled:=False;
    Visible:=False;
    Repaint;
  end;
end;

destructor TLoadingIndicator.Destroy;
begin
  //If FLoaded then
  FGif.Free;
  FTimer.Free;

  inherited Destroy;
end;

end.

