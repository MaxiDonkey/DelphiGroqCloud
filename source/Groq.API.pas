unit Groq.API;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.Net.HttpClient, System.Net.URLClient, System.Net.Mime,
  System.JSON, Groq.Errors, Groq.API.Params, System.SysUtils;

type
  /// <summary>
  /// See at https://console.groq.com/docs/errors
  /// </summary>
  GroqException = class(Exception)
  private
    FCode: Int64;
    FMsg: string;
    FType: string;
  public
    constructor Create(const ACode: Int64; const AError: TErrorCore); reintroduce; overload;
    constructor Create(const ACode: Int64; const Value: string); reintroduce; overload;
    property Code: Int64 read FCode write FCode;
    property Msg: string read FMsg write FMsg;
    property &Type: string read FType write FType;
  end;

  GroqExceptionAPI = class(Exception);

  /// <summary>
  /// Only part of the resource is being delivered, usually in response to range headers sent by the client.
  /// Ensure this is expected for the request being made.
  /// </summary>
  GroqExceptionPartialContentError = class(GroqException);

  /// <summary>
  /// The server could not understand the request due to invalid syntax.
  /// Review the request format and ensure it is correct.
  /// </summary>
  GroqExceptionRequestError = class(GroqException);

  /// <summary>
  /// The request was well-formed but could not be followed due to semantic errors.
  /// Verify the data provided for correctness and completeness.
  /// </summary>
  GroqExceptionUnprocessableEntityError = class(GroqException);

  /// <summary>
  /// Too many requests were sent in a given timeframe. Implement request throttling and respect rate limits.
  /// </summary>
  GroqExceptionTooManyRequestsError = class(GroqException);

  /// <summary>
  /// The request was not successful because it lacks valid authentication credentials for the requested resource.
  /// Ensure the request includes the necessary authentication credentials and the api key is valid.
  /// </summary>
  GroqExceptionUnauthorizedError = class(GroqException);

  /// <summary>
  /// The requested resource could not be found. Check the request URL and the existence of the resource.
  /// </summary>
  GroqExceptionNotFoundError = class(GroqException);

  /// <summary>
  /// A generic error occurred on the server. Try the request again later or contact support if the issue persists.
  /// </summary>
  GroqExceptionInternalServerError = class(GroqException);

  /// <summary>
  /// The server received an invalid response from an upstream server. This may be a temporary issue; retrying the request might resolve it.
  /// </summary>
  GroqExceptionBadGatewayError = class(GroqException);

  /// <summary>
  /// The server is not ready to handle the request, often due to maintenance or overload. Wait before retrying the request.
  /// </summary>
  GroqExceptionServiceUnavailableError = class(GroqException);

  GroqExceptionInvalidResponse = class(GroqException);

  TGroqAPI = class
  public
    const
      URL_BASE = 'https://api.groq.com/openai/v1';
  private
    FToken: string;
    FBaseUrl: string;
    FCustomHeaders: TNetHeaders;

    procedure SetToken(const Value: string);
    procedure SetBaseUrl(const Value: string);
    procedure RaiseError(Code: Int64; Error: TErrorCore);
    procedure ParseError(const Code: Int64; const ResponseText: string);
    procedure SetCustomHeaders(const Value: TNetHeaders);
  protected
    function GetHeaders: TNetHeaders; virtual;
    function GetClient: THTTPClient; virtual;
    function GetRequestURL(const Path: string): string;
    function Get(const Path: string; Response: TStringStream): Integer; overload;
    function Delete(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;
    function Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer; overload;
    function ParseResponse<T: class, constructor>(const Code: Int64; const ResponseText: string): T;
    procedure CheckAPI;
  public
    function Get<TResult: class, constructor>(const Path: string): TResult; overload;
    function Get<TResult: class, constructor; TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;
    procedure GetFile(const Path: string; Response: TStream); overload;
    function Delete<TResult: class, constructor>(const Path: string): TResult; overload;
    function Post<TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback = nil): Boolean; overload;
    function Post<TResult: class, constructor; TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;
    procedure Post<TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>; Response: TStream; Event: TReceiveDataCallback = nil); overload;       //AJOUT
    function Post<TResult: class, constructor>(const Path: string): TResult; overload;
    function PostForm<TResult: class, constructor; TParams: TMultipartFormData, constructor>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;
  public
    constructor Create; overload;
    constructor Create(const AToken: string); overload;
    destructor Destroy; override;
    property Token: string read FToken write SetToken;
    property BaseUrl: string read FBaseUrl write SetBaseUrl;
    property CustomHeaders: TNetHeaders read FCustomHeaders write SetCustomHeaders;
  end;
  {$WARNINGS ON}

  TGroqAPIRoute = class
  private
    FAPI: TGroqAPI;
    procedure SetAPI(const Value: TGroqAPI);
  public
    property API: TGroqAPI read FAPI write SetAPI;
    constructor CreateRoute(AAPI: TGroqAPI); reintroduce;
  end;

implementation

uses
  REST.Json, System.NetConsts;

constructor TGroqAPI.Create;
begin
  inherited;
  FToken := '';
  FBaseUrl := URL_BASE;
end;

constructor TGroqAPI.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TGroqAPI.Destroy;
begin
  inherited;
end;

function TGroqAPI.Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback): Integer;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Headers := GetHeaders + [TNetHeader.Create('Content-Type', 'application/json')];
    Stream := TStringStream.Create;
    Client.ReceiveDataCallBack := OnReceiveData;
    try
      Stream.WriteString(Body.ToJSON);
      Stream.Position := 0;
      Result := Client.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
    finally
      Client.OnReceiveData := nil;
      Stream.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TGroqAPI.Get(const Path: string; Response: TStringStream): Integer;
var
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Result := Client.Get(GetRequestURL(Path), Response, GetHeaders).StatusCode;
  finally
    Client.Free;
  end;
end;

function TGroqAPI.Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer;
var
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Result := Client.Post(GetRequestURL(Path), Body, Response, GetHeaders).StatusCode;
  finally
    Client.Free;
  end;
end;

function TGroqAPI.Post(const Path: string; Response: TStringStream): Integer;
var
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Result := Client.Post(GetRequestURL(Path), TStream(nil), Response, GetHeaders).StatusCode;
  finally
    Client.Free;
  end;
end;

procedure TGroqAPI.Post<TParams>(const Path: string; ParamProc: TProc<TParams>; Response: TStream; Event: TReceiveDataCallback);
var
  Params: TParams;
  Code: Integer;
  Headers: TNetHeaders;
  Stream, Strings: TStringStream;
  Client: THTTPClient;
begin
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);

    CheckAPI;
    Client := GetClient;
    try
      Client.ReceiveDataCallBack := Event;
      Headers := GetHeaders + [TNetHeader.Create('Content-Type', 'application/json')];
      Stream := TStringStream.Create;
      try
        Stream.WriteString(Params.JSON.ToJSON);
        Stream.Position := 0;
        Code := Client.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
        case Code of
          200..299:
            ; {success}
        else
          Strings := TStringStream.Create;
          try
            Response.Position := 0;
            Strings.LoadFromStream(Response);
            ParseError(Code, Strings.DataString);
          finally
            Strings.Free;
          end;
        end;
      finally
        Client.OnReceiveData := nil;
        Stream.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    Params.Free;
  end;
end;

function TGroqAPI.Post<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TGroqAPI.Post<TParams>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean;
var
  Params: TParams;
  Code: Integer;
begin
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response, Event);
    case Code of
      200..299:
        Result := True;
    else
      Result := False;
    end;
  finally
    Params.Free;
  end;
end;

function TGroqAPI.Post<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Post(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TGroqAPI.Delete(const Path: string; Response: TStringStream): Integer;
var
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Result := Client.Delete(GetRequestURL(Path), Response, GetHeaders).StatusCode;
  finally
    Client.Free;
  end;
end;

function TGroqAPI.Delete<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Delete(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TGroqAPI.PostForm<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TGroqAPI.Get<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Pairs: TArray<string> := [];
    for var Pair in Params.ToStringPairs do
      Pairs := Pairs + [Pair.Key + '=' + Pair.Value];
    var QPath := Path;
    if Length(Pairs) > 0 then
      QPath := QPath + '?' + string.Join('&', Pairs);
    Code := Get(QPath, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TGroqAPI.Get<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Get(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TGroqAPI.GetClient: THTTPClient;
begin
  Result := THTTPClient.Create;
  Result.AcceptCharSet := 'utf-8';
end;

procedure TGroqAPI.GetFile(const Path: string; Response: TStream);
var
  Code: Integer;
  Strings: TStringStream;
  Client: THTTPClient;
begin
  CheckAPI;
  Client := GetClient;
  try
    Code := Client.Get(GetRequestURL(Path), Response, GetHeaders).StatusCode;
    case Code of
      200..299:
        ; {success}
    else
      Strings := TStringStream.Create;
      try
        Response.Position := 0;
        Strings.LoadFromStream(Response);
        ParseError(Code, Strings.DataString);
      finally
        Strings.Free;
      end;
    end;
  finally
    Client.Free;
  end;
end;

function TGroqAPI.GetHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('Authorization', 'Bearer ' + FToken)] + FCustomHeaders;
end;

function TGroqAPI.GetRequestURL(const Path: string): string;
begin
  Result := Format('%s/%s', [FBaseURL, Path]);
end;

procedure TGroqAPI.CheckAPI;
begin
  if FToken.IsEmpty then
    raise GroqExceptionAPI.Create('Token is empty!');
  if FBaseUrl.IsEmpty then
    raise GroqExceptionAPI.Create('Base url is empty!');
end;

procedure TGroqAPI.RaiseError(Code: Int64; Error: TErrorCore);
begin
  case Code of
    {--- Informational Codes }
    206:
      raise GroqExceptionPartialContentError.Create(Code, Error);

    {--- Client Error Codes }
    400:
      raise GroqExceptionRequestError.Create(Code, Error);
    401:
      raise GroqExceptionUnauthorizedError.Create(Code, Error);
    404:
      raise GroqExceptionNotFoundError.Create(Code, Error);
    422:
      raise GroqExceptionUnprocessableEntityError.Create(Code, Error);
    429:
      raise GroqExceptionTooManyRequestsError.Create(Code, Error);

    {--- Server Error Codes }
    500:
      raise GroqExceptionInternalServerError.Create(Code, Error);
    502:
      raise GroqExceptionBadGatewayError.Create(Code, Error);
    503:
      raise GroqExceptionServiceUnavailableError.Create(Code, Error);
  else
    raise GroqException.Create(Code, Error);
  end;
end;

procedure TGroqAPI.ParseError(const Code: Int64; const ResponseText: string);
var
  Error: TErrorCore;
begin
  Error := nil;
  try
    try
      Error := TJson.JsonToObject<TError>(ResponseText);
    except
      Error := nil;
    end;
    if Assigned(Error) and Assigned(Error) then
      RaiseError(Code, Error)
  finally
    if Assigned(Error) then
      Error.Free;
  end;
end;

function TGroqAPI.ParseResponse<T>(const Code: Int64; const ResponseText: string): T;
begin
  Result := nil;
  case Code of
    200..299:
      try
        Result := TJson.JsonToObject<T>(ResponseText);
      except
        Result := nil;
      end;
  else
    ParseError(Code, ResponseText);
  end;
  if not Assigned(Result) then
    raise GroqExceptionInvalidResponse.Create(Code, 'Empty or invalid response');
end;

procedure TGroqAPI.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

procedure TGroqAPI.SetCustomHeaders(const Value: TNetHeaders);
begin
  FCustomHeaders := Value;
end;

procedure TGroqAPI.SetToken(const Value: string);
begin
  FToken := Value;
end;

{ TGroqAPIRoute }

constructor TGroqAPIRoute.CreateRoute(AAPI: TGroqAPI);
begin
  inherited Create;
  FAPI := AAPI;
end;

procedure TGroqAPIRoute.SetAPI(const Value: TGroqAPI);
begin
  FAPI := Value;
end;

{ GroqException }

constructor GroqException.Create(const ACode: Int64; const Value: string);
begin
  Code := ACode;
  Msg := Value;
  inherited Create(Format('error %d: %s', [ACode, Msg]));
end;

constructor GroqException.Create(const ACode: Int64; const AError: TErrorCore);
begin
  Code := ACode;
  Msg := (AError as TError).Error.Message;
  &type := (AError as TError).Error.&Type;
  inherited Create(Format('error (%d) - type %s'+ sLineBreak + '%s', [Code, &Type, Msg]));
end;

end.

