unit LoadingIndicator;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AnimatedGif,StdCtrls,ExtCtrls;

type

  { TLoadingIndicator }

  TLoadingIndicator = class(TPaintBox)
  private
    FTimer:TTimer;
    FGif:TAnimatedGif;
    procedure OnTimer(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);
    procedure StartAnimation;
    procedure StopAnimation;
    destructor Destroy;override;
  end;


implementation

{ TLoadingIndicator }
{$R LoadingIndicator.rc}

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
    r := TResourceStream.Create(HINSTANCE,'LOADING','GIF');
    FGif.LoadFromStream(r);
   finally
     r.Free;
   end;

end;

procedure TLoadingIndicator.StartAnimation;
begin
    Visible:=True;
    FTimer.Enabled:=True;
end;

procedure TLoadingIndicator.StopAnimation;
begin
  FTimer.Enabled:=False;
  Visible:=False;
  Repaint;
end;

destructor TLoadingIndicator.Destroy;
begin
    FGif.Free;
   inherited Destroy;
end;

end.

