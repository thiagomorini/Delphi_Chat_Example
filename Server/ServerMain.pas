unit ServerMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ButtonGroup,

  Server;

type
  TFMain = class(TForm)
    StatusBar: TStatusBar;
    Panel1: TPanel;
    Panel2: TPanel;
    Label2: TLabel;
    edtPort: TEdit;
    btnConnect: TBitBtn;
    Chat: TMemo;
    edtMessage: TEdit;
    btnSend: TBitBtn;
    ListUsers: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure edtMessageKeyPress(Sender: TObject; var Key: Char);
  private
    IdSession: Double;
    procedure CheckServer;
    procedure UpdateChat(Sender: TObject; UpdatedLine: string);
    procedure UpdateUsers(Sender: TObject);
  public
    Server: TServer;
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

procedure TFMain.btnConnectClick(Sender: TObject);
begin
  Server.Port := StrToInt(edtPort.Text);

  if btnConnect.Caption = 'Connect' then
  begin
    Server.Active := True;
    btnConnect.Caption := 'Disconnect';
  end
  else
  begin
    Server.Active := False;
    btnConnect.Caption := 'Connect';
  end;

  CheckServer;
end;

procedure TFMain.btnSendClick(Sender: TObject);
begin
  if (Trim(edtMessage.Text) = '') or not (Server.Active) then Exit;

  if ListUsers.ItemIndex = 0 then
    Server.SendToAllClients('[Server] says: ' + edtMessage.Text)
  else if ListUsers.ItemIndex = 1 then
    Chat.Lines.Add('[Server] says: ' + edtMessage.Text)
  else
    Server.SendToClient(ListUsers.ItemIndex-1, '[Server] says: ' + edtMessage.Text);

  edtMessage.Clear;
  edtMessage.SetFocus;
end;

procedure TFMain.CheckServer;
begin
  if not Assigned(Server) then Exit;

  StatusBar.SimpleText := Server.Status;

  if Server.Active then
    Server.ConnectUser('SERVER', Server.Host, Server.Port, IdSession)
  else
    Server.DisconnectUser(IdSession);
end;

procedure TFMain.edtMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btnSend.Click;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Server := TServer.Create;
  Server.OnUpdateChat := UpdateChat;
  Server.OnUpdateUsers := UpdateUsers;
  IdSession := Now;
  CheckServer;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  if Assigned(Server) then
    FreeAndNil(Server);
end;

procedure TFMain.UpdateChat(Sender: TObject; UpdatedLine: string);
begin
  Chat.Lines.Add(UpdatedLine);
end;

procedure TFMain.UpdateUsers(Sender: TObject);
begin
  ListUsers.Clear;
  ListUsers.Items.AddStrings(Server.GetUsers);
  ListUsers.ItemIndex := 0;
end;

end.
