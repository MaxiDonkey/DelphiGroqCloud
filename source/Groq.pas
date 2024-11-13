unit Groq;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient, Groq.API, Groq.Chat, Groq.Models,
  Groq.Audio;

type
  /// <summary>
  /// The <c>IGroq</c> interface provides access to the various features and routes of the Groq AI API.
  /// This interface allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This interface should be implemented by any class that wants to provide a structured way of accessing
  /// the Groq AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  ///
  /// To use this interface, instantiate a class that implements it, set the required properties such as
  /// <see cref="Token"/> and <see cref="BaseURL"/>, and call the relevant methods for the desired operations.
  /// <code>
  ///   var Groq: IGroq := TGroq.Create(API_TOKEN);
  /// </code>
  /// <seealso cref="TGroq"/>
  /// </remarks>
  IGroq = interface
    ['{7E69221E-3C24-4B38-9AE9-894714CA9A47}']
    function GetAPI: TGroqAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetModelsRoute: TModelsRoute;
    function GetAudioRoute: TAudioRoute;

    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TGroqAPI for making API calls.
    /// </returns>
    property API: TGroqAPI read GetAPI;
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.Groq.com/v1
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    // TODO
    property Models: TModelsRoute read GetModelsRoute;
    property Audio: TAudioRoute read GetAudioRoute;
  end;

  /// <summary>
  /// The <c>TGroqFactory</c> class is responsible for creating instances of
  /// the <see cref="IGroq"/> interface. It provides a factory method to instantiate
  /// the interface with a provided API token and optional header configuration.
  /// </summary>
  /// <remarks>
  /// This class provides a convenient way to initialize the <see cref="IGroq"/> interface
  /// by encapsulating the necessary configuration details, such as the API token and header options.
  /// By using the factory method, users can quickly create instances of <see cref="IGroq"/> without
  /// manually setting up the implementation details.
  /// </remarks>
  TGroqFactory = class
    /// <summary>
    /// Creates an instance of the <see cref="IGroq"/> interface with the specified API token
    /// and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Groq API services.
    /// </param>
    /// <param name="Option">
    /// An optional header configuration of type <see cref="THeaderOption"/> to customize the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IGroq"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateInstance(const AToken: string): IGroq;
  end;

  /// <summary>
  /// The TGroq class provides access to the various features and routes of the Groq AI API.
  /// This class allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This class should be implemented by any class that wants to provide a structured way of accessing
  /// the Groq AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  /// <seealso cref="TGroq"/>
  /// </remarks>
  TGroq = class(TInterfacedObject, IGroq)
  strict private

  private
    FAPI: TGroqAPI;

    FChatRoute: TChatRoute;
    FModelsRoute: TModelsRoute;
    FAudioRoute: TAudioRoute;

    function GetAPI: TGroqAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);

    function GetChatRoute: TChatRoute;
    function GetModelsRoute: TModelsRoute;
    function GetAudioRoute: TAudioRoute;

  public
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TGroqAPI for making API calls.
    /// </returns>
    property API: TGroqAPI read GetAPI;
    /// <summary>
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.Groq.com/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    // TODO
    property Models: TModelsRoute read GetModelsRoute;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TGroq"/> class with optional header configuration.
    /// </summary>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor is typically used when no API token is provided initially.
    /// The token can be set later via the <see cref="Token"/> property.
    /// </remarks>
    constructor Create; overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="TGroq"/> class with the provided API token and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with the Groq AI API.
    /// </param>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor allows the user to specify an API token at the time of initialization.
    /// </remarks>
    constructor Create(const AToken: string); overload;
    /// <summary>
    /// Releases all resources used by the current instance of the <see cref="TGroq"/> class.
    /// </summary>
    /// <remarks>
    /// This method is called to clean up any resources before the object is destroyed.
    /// It overrides the base <see cref="TInterfacedObject.Destroy"/> method.
    /// </remarks>
    destructor Destroy; override;
  end;

implementation

{ TGroq }

constructor TGroq.Create;
begin
  inherited Create;
  FAPI := TGroqAPI.Create;
end;

constructor TGroq.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TGroq.Destroy;
begin
  FChatRoute.Free;
  FModelsRoute.Free;
  FAudioRoute.Free;
  inherited;
end;

function TGroq.GetAPI: TGroqAPI;
begin
  Result := FAPI;
end;

function TGroq.GetAudioRoute: TAudioRoute;
begin
  if not Assigned(FAudioRoute) then
    FAudioRoute := TAudioRoute.CreateRoute(API);
  Result := FAudioRoute;
end;

function TGroq.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TGroq.GetChatRoute: TChatRoute;
begin
  if not Assigned(FChatRoute) then
    FChatRoute := TChatRoute.CreateRoute(API);
  Result := FChatRoute;
end;

function TGroq.GetModelsRoute: TModelsRoute;
begin
  if not Assigned(FModelsRoute) then
    FModelsRoute := TModelsRoute.CreateRoute(API);
  Result := FModelsRoute;
end;

function TGroq.GetToken: string;
begin
  Result := FAPI.Token;
end;

procedure TGroq.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TGroq.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TGroqFactory }

class function TGroqFactory.CreateInstance(const AToken: string): IGroq;
begin
  Result := TGroq.Create(AToken);
end;

end.
