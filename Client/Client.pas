unit Client;

interface

uses
  System.SysUtils, System.Classes, System.Win.ScktComp;

const
  DEFAULT_HOST = '192.168.43.91';
  DEFAULT_PORT = 1001;

  SStatusDisconnected = 'Status: Disconnected';
  SStatusConnected = 'Status: Connected with';

type
  TNotifyEventUpdateChat = procedure(Sender: TObject; UpdatedLine: string) of object;
  TNotifyEventUpdateUsers = procedure(Sender: TObject; Users: TStrings) of object;

  TClient = class
  private
    FIdSession: Double;
    FClientSocket: TClientSocket;
    FNickname: string;
    FOnUpdateChat: TNotifyEventUpdateChat;
    FOnUpdateUsers: TNotifyEventUpdateUsers;
    procedure ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
  protected
    function GetActive: Boolean;
    function GetHost: string;
    function GetPort: Integer;
    function GetStatus: string;
    procedure SetActive(const Value: Boolean);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Send(IdUser: Integer; const Text: string);
    property Nickname: string read FNickname write FNickname;
    property Active: Boolean read GetActive write SetActive;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property Status: string read GetStatus;
    property OnUpdateChat: TNotifyEventUpdateChat read FOnUpdateChat write FOnUpdateChat;
    property OnUpdateUsers: TNotifyEventUpdateUsers read FOnUpdateUsers write FOnUpdateUsers;
  end;

implementation

{ TClient }

constructor TClient.Create;
begin
  FClientSocket := TClientSocket.Create(nil);
  FClientSocket.ClientType := ctNonBlocking;
  FClientSocket.Host := DEFAULT_HOST; // Default
  FClientSocket.Port := DEFAULT_PORT; // Default
  FClientSocket.Active := False;

  FIdSession := Now;

  FClientSocket.OnConnect := ClientSocketConnect;
  FClientSocket.OnDisconnect := ClientSocketDisconnect;
  FClientSocket.OnRead := ClientSocketRead;
end;

destructor TClient.Destroy;
begin
  if Assigned(FClientSocket) then
    FreeAndNil(FClientSocket);

  inherited;
end;

function TClient.GetActive: Boolean;
begin
  Result := FClientSocket.Active;
end;

function TClient.GetHost: string;
begin
  Result := FClientSocket.Host;
end;

function TClient.GetPort: Integer;
begin
  Result := FClientSocket.Port;
end;

function TClient.GetStatus: string;
begin
  if not Self.Active then
    Result := SStatusDisconnected
  else
    Result := Format('%s ', [SStatusConnected]) + FClientSocket.Socket.RemoteHost;
end;

procedure TClient.SetActive(const Value: Boolean);
begin
  if FClientSocket.Active <> Value then
    FClientSocket.Active := Value;
end;

procedure TClient.SetHost(const Value: string);
begin
  if FClientSocket.Host <> Value then
    FClientSocket.Host := Value;
end;

procedure TClient.SetPort(const Value: Integer);
begin
  if FClientSocket.Port <> Value then
    FClientSocket.Port := Value;
end;

procedure TClient.Send(IdUser: Integer; const Text: string);
begin
  FClientSocket.Socket.SendText(AnsiString('#S' + IdUser.ToString + ';' +
                                           Text));

  (* Event FOnUpdateChat *)
  if Assigned(FOnUpdateChat) then
    FOnUpdateChat(self, Text);
end;

procedure TClient.ClientSocketConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.SendText(AnsiString('#C' + FNickname + ';' +
                             Socket.LocalHost + ';' +
                             Socket.LocalPort.ToString) + ';' +
                             FloatToStr(FIdSession));
end;

procedure TClient.ClientSocketDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.SendText(AnsiString('#D' + FloatToStr(FIdSession)));
end;

procedure TClient.ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  ReceivedMessage: string;
  S: TStrings;
begin
  ReceivedMessage := String(Socket.ReceiveText);

  if Copy(ReceivedMessage, 1, 2) = '#U' then // Logged users
  begin
    S := TStringList.Create;
    S.StrictDelimiter := True;
    S.Delimiter := ';';

    try
      Delete(ReceivedMessage, 1, 2);
      S.DelimitedText := ReceivedMessage;

      (* Event FOnUpdateUsers *)
      if Assigned(FOnUpdateUsers) then
        FOnUpdateUsers(self, S);
    finally
      if Assigned(S) then
        FreeAndNil(S);
    end;

    Exit;
  end;

  // When client receive message
  //GetMessages();

  (* Event FOnUpdateChat *)
  if Assigned(FOnUpdateChat) then
    FOnUpdateChat(self, ReceivedMessage);
end;

end.
