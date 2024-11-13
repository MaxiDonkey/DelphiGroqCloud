unit Groq.Audio;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.Mime, REST.JsonReflect, System.JSON,
  Rest.Json, REST.Json.Types, Groq.API, Groq.API.Params, Groq.Common,
  Groq.Async.Params, Groq.Async.Support;

type
  /// <summary>
  /// Specifies the format types for audio transcription and translation responses.
  /// </summary>
  TResponseFormatType = (
    /// <summary>
    /// Specifies the response format as JSON, providing a structured, machine-readable format.
    /// </summary>
    json,
    /// <summary>
    /// Specifies the response format as plain text, providing a simple transcription or translation result in text form.
    /// </summary>
    text,
    /// <summary>
    /// Specifies the response format as verbose JSON, which includes additional metadata alongside the transcription or translation result.
    /// </summary>
    verbose_json
  );

  /// <summary>
  /// Helper record for the <c>TResponseFormatType</c> enumeration, providing utility methods for converting
  /// between <c>TResponseFormatType</c> values and their string representations.
  /// </summary>
  TResponseFormatTypeHelper = record helper for TResponseFormatType
    /// <summary>
    /// Converts the current <c>TResponseFormatType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TResponseFormatType</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// Represents the parameters for an audio transcription request, extending the <c>TMultipartFormData</c> class to support
  /// multipart form data for uploading audio files and setting transcription parameters.
  /// </summary>
  /// <remarks>
  /// This class provides methods for configuring various transcription parameters, such as setting the audio file or stream,
  /// language, model ID, optional prompt, response format, and sampling temperature.
  /// </remarks>
  TAudioTranscription = class(TMultipartFormData)
  public
    /// <summary>
    /// Adds an audio file to the transcription request.
    /// </summary>
    /// <param name="FileName">
    /// The path to the audio file to transcribe. Supported formats include flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    function &File(const FileName: string): TAudioTranscription; overload;
    /// <summary>
    /// Adds an audio stream to the transcription request.
    /// </summary>
    /// <param name="Stream">
    /// The audio stream to transcribe.
    /// </param>
    /// <param name="FileName">
    /// The name of the audio file. Supported formats include flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    function &File(const Stream: TStream; const FileName: string): TAudioTranscription; overload;
    /// <summary>
    /// Sets the language of the input audio.
    /// </summary>
    /// <param name="Value">
    /// The language code (e.g., 'en', 'fr') of the input audio.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    function Language(const Value: string): TAudioTranscription;
    /// <summary>
    /// Sets the ID of the model to use for transcription.
    /// </summary>
    /// <param name="Value">
    /// The model ID to use. Only 'whisper-large-v3' is currently available.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    function Model(const Value: string): TAudioTranscription;
    /// <summary>
    /// Sets an optional prompt to guide the model's style or continue a previous audio segment.
    /// </summary>
    /// <param name="Value">
    /// The prompt text.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    /// <remarks>
    /// The prompt should match the audio language.
    /// </remarks>
    function Prompt(const Value: string): TAudioTranscription;
    /// <summary>
    /// Sets the format of the transcript output.
    /// </summary>
    /// <param name="Value">
    /// The desired response format. Options are json, text, or verbose_json.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    function ResponseFormat(const Value: TResponseFormatType): TAudioTranscription;
    /// <summary>
    /// Sets the sampling temperature for the transcription.
    /// </summary>
    /// <param name="Value">
    /// The sampling temperature, between 0 and 1. Higher values make the output more random, while lower values make it more focused and deterministic.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranscription</c>.
    /// </returns>
    /// <remarks>
    /// If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit.
    /// </remarks>
    function Temperature(const Value: Double): TAudioTranscription;
    /// <summary>
    /// Initializes a new instance of the <c>TAudioTranscription</c> class.
    /// </summary>
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents the parameters for an audio translation request, extending the <c>TMultipartFormData</c> class to support
  /// multipart form data for uploading audio files and setting translation parameters.
  /// </summary>
  /// <remarks>
  /// This class provides methods for configuring various translation parameters, such as setting the audio file or stream,
  /// model ID, optional prompt, response format, and sampling temperature.
  /// </remarks>
  TAudioTranslation = class(TMultipartFormData)
  public
    /// <summary>
    /// Adds an audio file to the translation request.
    /// </summary>
    /// <param name="FileName">
    /// The path to the audio file to translate. Supported formats include flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    function &File(const FileName: string): TAudioTranslation; overload;
    /// <summary>
    /// Adds an audio stream to the translation request.
    /// </summary>
    /// <param name="Stream">
    /// The audio stream to translate.
    /// </param>
    /// <param name="FileName">
    /// The name of the audio file. Supported formats include flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    function &File(const Stream: TStream; const FileName: string): TAudioTranslation; overload;
    /// <summary>
    /// Sets the ID of the model to use for translation.
    /// </summary>
    /// <param name="Value">
    /// The model ID to use. Only 'whisper-large-v3' is currently available.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    function Model(const Value: string): TAudioTranslation;
    /// <summary>
    /// Sets an optional prompt to guide the model's style or continue a previous audio segment.
    /// </summary>
    /// <param name="Value">
    /// The prompt text.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    /// <remarks>
    /// The prompt should match the audio language.
    /// </remarks>
    function Prompt(const Value: string): TAudioTranslation;
    /// <summary>
    /// Sets the format of the translation output.
    /// </summary>
    /// <param name="Value">
    /// The desired response format. Options are json, text, or verbose_json.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    function ResponseFormat(const Value: TResponseFormatType): TAudioTranslation;
    /// <summary>
    /// Sets the sampling temperature for the translation.
    /// </summary>
    /// <param name="Value">
    /// The sampling temperature, between 0 and 1. Higher values make the output more random, while lower values make it more focused and deterministic.
    /// </param>
    /// <returns>
    /// The current instance of <c>TAudioTranslation</c>.
    /// </returns>
    /// <remarks>
    /// If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit.
    /// </remarks>
    function Temperature(const Value: Single): TAudioTranslation;
    /// <summary>
    /// Initializes a new instance of the <c>TAudioTranslation</c> class.
    /// </summary>
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents the text result of an audio transcription or translation.
  /// </summary>
  TAudioText = class
  private
    FText: string;
    [JsonNameAttribute('x_groq')]
    FXGroq: TXGroq;
  public
    /// <summary>
    /// Gets or sets the transcribed or translated text.
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Gets or sets the XGroq metadata.
    /// </summary>
    property XGroq: TXGroq read FXGroq write FXGroq;
  end;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request using <c>TAudioText</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynAudioText</c> type extends the <c>TAsynParams&lt;TAudioText&gt;</c> record to handle the lifecycle of an asynchronous model operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking model search operations and is specifically tailored for scenarios where multiple model choices are required.
  /// </remarks>
  TAsynAudioText = TAsynCallBack<TAudioText>;

  /// <summary>
  /// Provides methods to interact with audio transcription and translation API endpoints, extending the <c>TGroqAPIRoute</c> class
  /// to support specialized audio processing requests.
  /// </summary>
  /// <remarks>
  /// This class includes methods for both synchronous and asynchronous audio transcriptions and translations.
  /// It allows users to send audio data for processing and retrieve transcriptions or translations in different formats.
  /// </remarks>
  TAudioRoute = class(TGroqAPIRoute)
    /// <summary>
    /// Asynchronously creates a transcription of the provided audio.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TAudioTranscription</c> parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynAudioText</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to transcribe the provided audio asynchronously.
    ///The <c>CallBacks</c> function is invoked when the operation completes, either successfully or with an error.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// GroqCloud.Audio.ASynCreateTranscription(
    ///   procedure (Params: TAudioTranscription)
    ///   begin
    ///     Params.Model('whisper-large-v3-turbo');
    ///     Params.File('Z:\my_folder\Audio\sound.mp3');
    ///   end,
    ///   function : TAsynAudioText
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Audio: TAudioText)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure ASynCreateTranscription(ParamProc: TProc<TAudioTranscription>; CallBacks: TFunc<TAsynAudioText>);
    /// <summary>
    /// Asynchronously translates the provided audio into English.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TAudioTranslation</c> parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynAudioText</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to translate the provided audio asynchronously.
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or with an error.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// GroqCloud.Audio.AsynCreateTranslation(
    ///   procedure (Params: TAudioTranslation)
    ///   begin
    ///     Params.Model('whisper-large-v3-turbo');
    ///     Params.File('Z:\my_folder\Audio\sound.mp3');
    ///   end,
    ///   function : TAsynAudioText
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Audio: TAudioText)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure ASynCreateTranslation(ParamProc: TProc<TAudioTranslation>; CallBacks: TFunc<TAsynAudioText>);
    /// <summary>
    /// Creates a transcription of the provided audio.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TAudioTranscription</c> parameters.
    /// </param>
    /// <returns>
    /// A <c>TAudioText</c> object containing the transcription result.
    /// </returns>
    /// <remarks>
    /// This method sends a synchronous request to the API to transcribe the provided audio.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// var Transcript := GroqCloud.Audio.CreateTranscription(
    ///     procedure (Params: TAudioTranscription)
    ///     begin
    ///       Params.Model('whisper-large-v3');
    ///       Params.File('Z:\my_folder\Audio\sound.mp3');
    ///     end;
    /// try
    ///   WriteLn(Transcript.Text);
    /// finally
    ///   Transcript.Free;
    /// end;
    /// </code>
    /// </remarks>
    function CreateTranscription(ParamProc: TProc<TAudioTranscription>): TAudioText;
    /// <summary>
    /// Translates the provided audio into English.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TAudioTranslation</c> parameters.
    /// </param>
    /// <returns>
    /// A <c>TAudioText</c> object containing the translation result.
    /// </returns>
    /// <remarks>
    /// This method sends a synchronous request to the API to translate the provided audio.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// var Translate := GroqCloud.Audio.CreateTranslation(
    ///     procedure (Params: TAudioTranslation)
    ///     begin
    ///       Params.Model('whisper-large-v3');
    ///       Params.File('Z:\my_folder\Audio\sound.mp3');
    ///     end);
    /// try
    ///   WriteLn(Translate.Text);
    /// finally
    ///   Translate.Free;
    /// end;
    /// </code>
    /// </remarks>
    function CreateTranslation(ParamProc: TProc<TAudioTranslation>): TAudioText;
  end;

implementation

uses
  System.Net.URLClient;

{ TAudioTranscription }

function TAudioTranscription.&File(const FileName: string): TAudioTranscription;
begin
  AddFile('file', FileName);
  Result := Self;
end;

constructor TAudioTranscription.Create;
begin
  inherited Create(True);
end;

function TAudioTranscription.&File(const Stream: TStream;
  const FileName: string): TAudioTranscription;
begin
  AddStream('file', Stream, True, FileName);
  Result := Self;
end;

function TAudioTranscription.Language(const Value: string): TAudioTranscription;
begin
  AddField('language', Value);
  Result := Self;
end;

function TAudioTranscription.Model(const Value: string): TAudioTranscription;
begin
  AddField('model', Value);
  Result := Self;
end;

function TAudioTranscription.Prompt(const Value: string): TAudioTranscription;
begin
  AddField('prompt', Value);
  Result := Self;
end;

function TAudioTranscription.ResponseFormat(
  const Value: TResponseFormatType): TAudioTranscription;
begin
  AddField('response_format', Value.ToString);
  Result := Self;
end;

function TAudioTranscription.Temperature(
  const Value: Double): TAudioTranscription;
begin
  AddField('temperature', Value.ToString);
  Result := Self;
end;

{ TResponseFormatTypeHelper }

function TResponseFormatTypeHelper.ToString: string;
begin
  case Self of
    json:
      Exit('json');
    text:
      Exit('text');
    verbose_json:
      Exit('verbose_json');
  end;
end;

{ TAudioRoute }

procedure TAudioRoute.ASynCreateTranscription(
  ParamProc: TProc<TAudioTranscription>; CallBacks: TFunc<TAsynAudioText>);
begin
  with TAsynCallBackExec<TAsynAudioText, TAudioText>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAudioText
      begin
        Result := Self.CreateTranscription(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAudioRoute.ASynCreateTranslation(ParamProc: TProc<TAudioTranslation>;
  CallBacks: TFunc<TAsynAudioText>);
begin
  with TAsynCallBackExec<TAsynAudioText, TAudioText>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAudioText
      begin
        Result := Self.CreateTranslation(ParamProc);
      end);
  finally
    Free;
  end;
end;

function TAudioRoute.CreateTranscription(
  ParamProc: TProc<TAudioTranscription>): TAudioText;
begin
  Result := API.PostForm<TAudioText, TAudioTranscription>('audio/transcriptions', ParamProc);
end;

function TAudioRoute.CreateTranslation(
  ParamProc: TProc<TAudioTranslation>): TAudioText;
begin
  Result := API.PostForm<TAudioText, TAudioTranslation>('audio/translations', ParamProc);
end;

{ TAudioTranslation }

function TAudioTranslation.&File(const FileName: string): TAudioTranslation;
begin
  AddFile('file', FileName);
  Result := Self;
end;

constructor TAudioTranslation.Create;
begin
  inherited Create(True);
end;

function TAudioTranslation.&File(const Stream: TStream;
  const FileName: string): TAudioTranslation;
begin
  AddStream('file', Stream, True, FileName);
  Result := Self;
end;

function TAudioTranslation.Model(const Value: string): TAudioTranslation;
begin
  AddField('model', Value);
  Result := Self;
end;

function TAudioTranslation.Prompt(const Value: string): TAudioTranslation;
begin
  AddField('prompt', Value);
  Result := Self;
end;

function TAudioTranslation.ResponseFormat(
  const Value: TResponseFormatType): TAudioTranslation;
begin
  AddField('response_format', Value.ToString);
  Result := Self;
end;

function TAudioTranslation.Temperature(const Value: Single): TAudioTranslation;
begin
  AddField('temperature', FormatFloat('0,0', Value));
  Result := Self;
end;

end.
