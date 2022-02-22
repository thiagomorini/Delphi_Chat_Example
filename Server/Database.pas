unit Database;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type

  TUser = record
    FId: Integer;
    FNickname: string;
    FHost: string;
    FPort: Integer;
    FConnected: Boolean;
    FIdSession: Double;
    FData: TStrings;
  end;

  TDatabase = class
  private
    FIndex: Integer;
    FUsers: array of TUser;
  protected
    function GetUser(Index: Integer): TUser;
    procedure SetUser(Index: Integer; const Value: TUser);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddUser(Nickname: string; Host: string; Port: Integer; IdSession: Double);
    //procedure RemoveUser(Host: string; Port: Integer);
    //function GetUserId(Host: string; Port: Integer): Integer;
    procedure RemoveUser(IdSession: Double);
    function GetUserId(IdSession: Double): Integer;
    function GetUsers: TStrings;
    procedure SaveMessage(IdUser: Integer; Text: string);
    procedure SaveMessageAllUsers(Text: string);
    property Users[Index: Integer]: TUser read GetUser write SetUser;
  end;

implementation

{ TDatabase }

constructor TDatabase.Create;
begin
  FIndex := 0;
end;

destructor TDatabase.Destroy;
var
  I: Integer;
begin
  for I := Low(FUsers) to High(FUsers) do
    if Assigned(FUsers[I].FData) then
      FreeAndNil(FUsers[I].FData);

  inherited;
end;

function TDatabase.GetUser(Index: Integer): TUser;
begin
  Result := FUsers[Index];
end;

procedure TDatabase.SetUser(Index: Integer; const Value: TUser);
begin
  if FUsers[Index].FId <> Value.FId then
    FUsers[Index] := Value;
end;

procedure TDatabase.AddUser(Nickname: string; Host: string; Port: Integer; IdSession: Double);
begin
  if Self.GetUserId(IdSession) > 0 then
  begin
    FUsers[Self.GetUserId(IdSession)-1].FConnected := True;
    Exit;
  end;

  SetLength(FUsers, FIndex+1);
  FUsers[FIndex].FId := FIndex+1;
  FUsers[FIndex].FNickname := Nickname;
  FUsers[FIndex].FHost := Host;
  FUsers[FIndex].FPort := Port;
  FUsers[FIndex].FIdSession := IdSession;
  FUsers[FIndex].FConnected := True;

  FUsers[FIndex].FData := TStringList.Create;

  Inc(FIndex);
end;

procedure TDatabase.RemoveUser(IdSession: Double);
begin
  if Self.FUsers = nil then Exit;

  FUsers[Self.GetUserId(IdSession)-1].FConnected := False;
end;

function TDatabase.GetUserId(IdSession: Double): Integer;
var
  I: Integer;
begin
  result := 0;

  for I := Low(FUsers) to High(FUsers) do
  begin
    if FUsers[I].FIdSession = IdSession then
    begin
      Result := FUsers[I].FId;
      Exit;
    end;
  end;
end;

function TDatabase.GetUsers: TStrings;
var
  I: Integer;
  Nickname: string;
begin
  if Self.FUsers = nil then Exit;

  Result := TStringList.Create;
  Result.Add('EVERYONE');

  for I := Low(FUsers) to High(FUsers) do
  begin
    Nickname := FUsers[I].FNickname;

    if FUsers[I].FConnected then
      Nickname := Nickname + ' (online)'
    else
      Nickname := Nickname + ' (offline)';

    Result.Add(Nickname);
  end;
end;

procedure TDatabase.SaveMessage(IdUser: Integer; Text: string);
begin
  if IdUser < 0 then  // server
    IdUser := 0;

  FUsers[IdUser].FData.Add('#F;' + Text);
end;

procedure TDatabase.SaveMessageAllUsers(Text: string);
var
  I: Integer;
begin
  for I := Low(FUsers) to High(FUsers) do
    SaveMessage(I, Text);
end;

end.
