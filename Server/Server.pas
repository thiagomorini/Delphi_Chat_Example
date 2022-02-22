unit Server;

interface

uses
  System.Classes, System.SysUtils, System.Win.ScktComp, Database;

const
  DEFAULT_PORT = 1001;

  SStatusNotListening = 'Status: Not listening';
  SStatusListening = 'Status: Listening on port';

type
  TNotifyEventUpdateChat = procedure(Sender: TObject; UpdatedLine: string) of object;

  TServer = class
  private
    FServerSocket: TServerSocket;
    FHost: string;
    FDatabase: TDatabase;
    FOnUpdateChat: TNotifyEventUpdateChat;
    FOnUpdateUsers: TNotifyEvent;
    procedure SendLoggedUsersToAllClients(const Text: string);
    procedure ServerSocketClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
  protected
    function GetActive: Boolean;
    function GetHost: string;
    function GetPort: Integer;
    function GetStatus: string;
    procedure SetActive(const Value: Boolean);
    procedure SetPort(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendToClient(IdUser: Integer; const Text: string);
    procedure SendToAllClients(const Text: string);

    procedure ConnectUser(Nickname: string; Host: string; Port: Integer; IdSession: Double);
    procedure DisconnectUser(IdSession: Double);
    function GetUsers: TStrings;

    property Active: Boolean read GetActive write SetActive;
    property Host: string read GetHost;
    property Port: Integer read GetPort write SetPort;
    property Status: string read GetStatus;
    property OnUpdateChat: TNotifyEventUpdateChat read FOnUpdateChat write FOnUpdateChat;
    property OnUpdateUsers: TNotifyEvent read FOnUpdateUsers write FOnUpdateUsers;
  end;

implementation

{ TServer }

constructor TServer.Create;
begin
  FServerSocket := TServerSocket.Create(nil);
  FServerSocket.ServerType := stNonBlocking;
  FServerSocket.Port := DEFAULT_PORT; // Default;
  FServerSocket.Active := False;

  FServerSocket.OnClientConnect := ServerSocketClientConnect;
  FServerSocket.OnClientDisconnect := ServerSocketClientDisconnect;
  FServerSocket.OnClientRead := ServerSocketClientRead;

  FDatabase := TDatabase.Create;
end;

destructor TServer.Destroy;
begin
  if Assigned(FDatabase) then
    FreeAndNil(FDatabase);

  if Assigned(FServerSocket) then
    FreeAndNil(FServerSocket);

  inherited;
end;

function TServer.GetActive: Boolean;
begin
  Result := FServerSocket.Active;
end;

function TServer.GetHost: string;
begin
  if FServerSocket.Socket.LocalHost <> '' then
    FHost := FServerSocket.Socket.LocalHost;

  Result := FHost;
end;

function TServer.GetPort: Integer;
begin
  Result := FServerSocket.Port;
end;

function TServer.GetStatus: string;
begin
  if not Self.Active then
    Result := SStatusNotListening
  else
    Result := Format('%s ', [SStatusListening]) + IntToStr(Self.Port);
end;

procedure TServer.SetActive(const Value: Boolean);
begin
  if FServerSocket.Active <> Value then
    FServerSocket.Active := Value;
end;

procedure TServer.SetPort(const Value: Integer);
begin
  if FServerSocket.Port <> Value then
    FServerSocket.Port := Value;
end;

procedure TServer.SendToClient(IdUser: Integer; const Text: string);
begin
  FServerSocket.Socket.Connections[IdUser-1].SendText(AnsiString(Text));
  FDatabase.SaveMessage(IdUser, Text);

  (* Event FOnUpdateChat *)
  if Assigned(FOnUpdateChat) then
    FOnUpdateChat(self, Text);
end;

procedure TServer.SendLoggedUsersToAllClients(const Text: string);
var
  I: Integer;
begin
  for I := 0 to FServerSocket.Socket.ActiveConnections - 1 do
    FServerSocket.Socket.Connections[I].SendText(AnsiString(Text));
end;

procedure TServer.SendToAllClients(const Text: string);
var
  I: Integer;
begin
  for I := 0 to FServerSocket.Socket.ActiveConnections - 1 do
    FServerSocket.Socket.Connections[I].SendText(AnsiString(Text));

  FDatabase.SaveMessageAllUsers(Text);

  (* Event FOnUpdateChat *)
  if Assigned(FOnUpdateChat) then
    FOnUpdateChat(self, Text);
end;

procedure TServer.ServerSocketClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Self.SendToAllClients('<' + Socket.RemoteHost + ':' + Socket.RemotePort.ToString + ' just connected>');
end;

procedure TServer.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
var
  I: Integer;
begin
  //Self.SendToAllClients('<' + Socket.RemoteHost + ':' + Socket.RemotePort.ToString + ' just disconnected>');
  if Assigned(FOnUpdateUsers) then
    FOnUpdateUsers(self);
end;

procedure TServer.ServerSocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
  ReceivedMessage: string;
  S: TStrings;
begin
  ReceivedMessage := String(Socket.ReceiveText);

  S := TStringList.Create;
  S.StrictDelimiter := True;
  S.Delimiter := ';';

  try
    if Copy(ReceivedMessage, 1, 2) = '#C' then // New client connection
    begin
      Delete(ReceivedMessage, 1, 2);
      S.DelimitedText := ReceivedMessage;
      Self.ConnectUser(S.Strings[0], S.Strings[1], StrToInt(S.Strings[2]), StrToFloat(S.Strings[3]));
    end;

    if Copy(ReceivedMessage, 1, 2) = '#D' then // Client disconnection
    begin
      Delete(ReceivedMessage, 1, 2);
      S.DelimitedText := ReceivedMessage;
      Self.DisconnectUser(StrToFloat(S.Strings[0]));
    end;

    if Copy(ReceivedMessage, 1, 2) = '#S' then // Sending message
    begin
      Delete(ReceivedMessage, 1, 2);
      S.DelimitedText := ReceivedMessage;
      FDatabase.SaveMessage(StrToInt(S.Strings[0]), S.Strings[1]);

      //Self.GetMessages();

      (* Event FOnUpdateChat *)
      //if Assigned(FOnUpdateChat) then
        //FOnUpdateChat(self, S.Strings[1]);
    end;
  finally
    if Assigned(S) then
      FreeAndNil(S);
  end;
end;

procedure TServer.ConnectUser(Nickname: string; Host: string; Port: Integer; IdSession: Double);
begin
  FDatabase.AddUser(Nickname, Host, Port, IdSession);

  (* Event FOnUpdateUsers *)
  if Assigned(FOnUpdateUsers) then
    FOnUpdateUsers(self);
end;

procedure TServer.DisconnectUser(IdSession: Double);
begin
  Self.FDatabase.RemoveUser(IdSession);

  (* Event FOnUpdateUsers *)
  //if Assigned(FOnUpdateUsers) then
    //FOnUpdateUsers(self);
end;

function TServer.GetUsers: TStrings;
var
  I: Integer;
  S: string;
begin
  Result := TStringList.Create;
  Result := FDatabase.GetUsers;

  S := '#U'; // Send logged users
  for I := 0 to Result.Count-1 do
    S := S + Result.Strings[I] + ';';
  Delete(S, Length(S), 1);

  SendLoggedUsersToAllClients(S);
end;

end.
