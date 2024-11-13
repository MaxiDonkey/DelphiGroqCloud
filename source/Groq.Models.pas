unit Groq.Models;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  Groq.API.Params, Groq.API, Groq.Async.Params, Groq.Async.Support;

type
  /// <summary>
  /// Represents a language model provided by GroqCloud.
  /// </summary>
  /// <remarks>
  /// The <c>TModel</c> class contains detailed information about a specific language model available through the GroqCloud API, including its identifier, owner, creation time, and other metadata.
  /// </remarks>
  TModel = class
  private
    FId: string;
    FObject: string;
    FCreated: Int64;
    [JsonNameAttribute('owned_by')]
    FOwnedBy: string;
    FActive: Boolean;
    [JsonNameAttribute('context_window')]
    FContextWindow: Int64;
    [JsonNameAttribute('public_apps')]
    FPublicApps: string;
  public
    /// <summary>
    /// Gets or sets the unique identifier of the model.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Gets or sets the object type, typically 'model'.
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Gets or sets the creation timestamp of the model.
    /// </summary>
    /// <remarks>
    /// The timestamp is represented as a Unix epoch time in seconds.
    /// </remarks>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// Gets or sets the owner of the model.
    /// </summary>
    property OwnedBy: string read FOwnedBy write FOwnedBy;
    /// <summary>
    /// Gets or sets a value indicating whether the model is active.
    /// </summary>
    property Active: Boolean read FActive write FActive;
    /// <summary>
    /// Gets or sets the context window size of the model.
    /// </summary>
    /// <remarks>
    /// The context window size determines the maximum number of tokens the model can process at once.
    /// </remarks>
    property ContextWindow: Int64 read FContextWindow write FContextWindow;
    /// <summary>
    /// Gets or sets the public applications associated with the model.
    /// </summary>
    property PublicApps: string read FPublicApps write FPublicApps;
  end;

  /// <summary>
  /// Represents a collection of language models provided by GroqCloud.
  /// </summary>
  /// <remarks>
  /// The <c>TModels</c> class contains an array of <c>TModel</c> instances and metadata about the collection.
  /// </remarks>
  TModels = class
  private
    FObject: string;
    FData: TArray<TModel>;
  public
    /// <summary>
    /// Gets or sets the object type, typically 'list'.
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Gets or sets the array of models.
    /// </summary>
    property Data: TArray<TModel> read FData write FData;
    /// <summary>
    /// Destroys the instance of <c>TModels</c> and releases all resources.
    /// </summary>
    /// <remarks>
    /// The destructor frees all <c>TModel</c> instances contained in the <c>Data</c> array.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request using <c>TModel</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModel</c> type extends the <c>TAsynParams&lt;TModel&gt;</c> record to handle the lifecycle of an asynchronous model operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking model search operations and is specifically tailored for scenarios where multiple model choices are required.
  /// </remarks>
  TAsynModel = TAsynCallBack<TModel>;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request that returns a collection of models using <c>TModels</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModels</c> type extends the <c>TAsynCallBack&lt;TModels&gt;</c> record to handle the lifecycle of an asynchronous operation
  /// involving multiple models. This includes event handlers that trigger at key points in the process, such as when the operation begins,
  /// completes successfully, or encounters an error.
  /// This structure is designed for non-blocking operations that return a collection of models. The <c>TModels</c> type represents
  /// a collection of <c>TModel</c> instances, making this type useful when working with multiple model objects in asynchronous requests.
  /// </remarks>
  TAsynModels = TAsynCallBack<TModels>;

  /// <summary>
  /// Gère les routes API liées à la récupération de modèles et fournit des méthodes pour répertorier ou récupérer des modèles spécifiques.
  /// </summary>
  /// <remarks>
  /// La classe <c>TModelsRoute</c> propose des méthodes pour récupérer des modèles à partir de l'API.
  /// Elle comprend la méthode <c>List</c> qui permet de répertorier tous les modèles, de récupérer des résultats ou de récupérer un modèle spécifique par son nom.
  /// Cette classe interagit avec l'API sous-jacente pour récupérer les données du modèle et renvoie des instances de <c>TModels</c> ou <c>TModel</c>.
  /// </remarks>
  TModelsRoute = class(TGroqAPIRoute)
  public
    /// <summary>
    /// Asynchronously retrieves the list of all available models.
    /// </summary>
    /// <param name="CallBacks">
    /// A <c>TFunc&lt;TAsynModels&gt;</c> representing the callback to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to fetch all available models asynchronously.
    /// The <paramref name="CallBacks"/> function is invoked when the operation completes,
    /// either successfully or with an error.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// GroqCloud.Models.AsynList(
    ///    function : TAsynModels
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; List: TModels)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynList(CallBacks: TFunc<TAsynModels>);
    /// <summary>
    /// Asynchronously retrieves a specific model by its unique identifier.
    /// </summary>
    /// <param name="ModelId">
    /// The unique identifier of the model to retrieve.
    /// </param>
    /// <param name="CallBacks">
    /// A <c>TFunc&lt;TAsynModel&gt;</c> representing the callback to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to fetch a specific model asynchronously.
    /// The <paramref name="CallBacks"/> function is invoked when the operation completes,
    /// either successfully or with an error.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BearerKey);
    /// GroqCloud.Models.AsynRetrieve('model-id',
    ///   function: TAsynModel
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure(Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; Model: TModel)
    ///       begin
    ///         // Handle the retrieved model
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; Error: string)
    ///       begin
    ///         // Handle the error message
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynRetrieve(const ModelId: string; CallBacks: TFunc<TAsynModel>);
    /// <summary>
    /// Retrieves the list of all available models.
    /// </summary>
    /// <returns>
    /// A <c>TModels</c> object containing the list of models.
    /// </returns>
    /// <remarks>
    /// This method sends a request to the API to fetch all available models without pagination or filtering.
    /// It returns a <c>TModels</c> object containing the collection of models.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// var List := GroqCloud.Models.List;
    /// try
    ///   for var Item in List.Models do
    ///     WriteLn( Item.DisplayName );
    /// finally
    ///   List.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TModels;
    /// <summary>
    /// Retrieves a specific model by its unique identifier.
    /// </summary>
    /// <param name="ModelId">
    /// The unique identifier of the model to retrieve.
    /// </param>
    /// <returns>
    /// A <c>TModel</c> object representing the retrieved model.
    /// </returns>
    /// <remarks>
    /// This method sends a request to the API to fetch a specific model synchronously.
    /// It returns a <c>TModel</c> object containing the model's details.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BearerKey);
    /// var Model := GroqCloud.Models.Retrieve('model-id');
    /// try
    ///   WriteLn('Model Name: ', Model.Id);
    ///   // Process the model
    /// finally
    ///   Model.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Retrieve(const ModelId: string): TModel;
  end;

implementation

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TModelsRoute }

procedure TModelsRoute.AsynList(CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsynRetrieve(const ModelId: string;
  CallBacks: TFunc<TAsynModel>);
begin
  with TAsynCallBackExec<TAsynModel, TModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModel
      begin
        Result := Self.Retrieve(ModelId);
      end);
  finally
    Free;
  end;
end;

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

function TModelsRoute.Retrieve(const ModelId: string): TModel;
begin
  Result := API.Get<TModel>(Format('models/%s', [ModelId]));
end;

end.
