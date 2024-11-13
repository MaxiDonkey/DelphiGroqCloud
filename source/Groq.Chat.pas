unit Groq.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Groq.API.Params, Groq.API, Groq.Functions.Core, Groq.Common,
  Groq.NetEncoding.Base64, Groq.Async.Params, Groq.Async.Support;

type
  /// <summary>
  /// Type of message role
  /// </summary>
  TRoleType = (
    /// <summary>
    /// System message
    /// </summary>
    system,
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    assistant,
    /// <summary>
    /// Tool message
    /// </summary>
    tool
  );

  /// <summary>
  /// Helper record for the <c>TRoleType</c> enumeration, providing utility methods for converting
  /// between <c>TRoleType</c> values and their string representations.
  /// </summary>
  TRoleTypeHelper = record helper for TRoleType
    /// <summary>
    /// Converts the current <c>TRoleType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TRoleType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TRoleType</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TRoleType</c>.
    /// </param>
    /// <returns>
    /// The <c>TRoleType</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TRoleType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TRoleType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TRoleType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TRoleTypeInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TRoleType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TRoleType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TRoleType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TRoleType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TRoleType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TRoleType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TRoleType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Controls which (if any) tool is called by the model.  
  /// </summary>
  TToolChoiceType = (
    /// <summary>
    /// The model will not call any tool and instead generates a message.  
    /// </summary>
    none,
    /// <summary>
    /// The model can pick between generating a message or calling one or more tools.  
    /// </summary>
    auto,
    /// <summary>
    /// The model must call one or more tools.  
    /// </summary>
    /// <remarks>
    /// Specifying a particular tool via {"type": "function", "function": {"name": "my_function"}} forces the model to call that tool.
    /// <para>
    /// With Llama models  
    /// </para>
    /// </remarks>
    required
  );

  /// <summary>
  /// Helper record for the <c>TToolChoiceType</c> enumeration, providing utility methods for converting
  /// between <c>TToolChoiceType</c> values and their string representations.
  /// </summary>
  TToolChoiceTypeHelper = record helper for TToolChoiceType
    /// <summary>
    /// Converts the <c>TToolChoiceType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TToolChoiceType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TToolChoiceType</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// Allow to characterize the content type of a payload
  /// </summary>
  TContentType = (
    /// <summary>
    /// The type of the content is a text.  
    /// </summary>
    text,
    /// <summary>
    /// The type of the content is an URL.  
    /// </summary>
    imageUrl
  );

  /// <summary>
  /// Helper record for the <c>TContentType</c> enumeration, providing utility methods for converting
  /// between <c>TContentType</c> values and their string representations.
  /// </summary>
  TContentTypeHelper = record helper for TContentType
    /// <summary>
    /// Converts the <c>TContentType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TContentType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TContentType</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// The reason the model stopped generating tokens.
  /// </summary>
  TFinishReasonType = (
    /// <summary>
    /// If the model hit a natural stop point or a provided stop sequence
    /// </summary>
    stop,
    /// <summary>
    /// If the maximum number of tokens specified in the request was reached
    /// </summary>
    length,
    /// <summary>
    /// If content was omitted due to a flag from our content filters
    /// </summary>
    content_filter,
    /// <summary>
    /// If the model called a tool
    /// </summary>
    tool_calls,
    /// <summary>
    /// (deprecated) If the model called a function
    /// </summary>
    function_call
  );

  /// <summary>
  /// Helper record for the <c>TFinishReasonType</c> enumeration, providing utility methods for converting
  /// between <c>TFinishReasonType</c> values and their string representations.
  /// </summary>
  TFinishReasonTypeHelper = record helper for TFinishReasonType
    /// <summary>
    /// Converts the <c>TFinishReasonType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TFinishReasonType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFinishReasonType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TFinishReasonType</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TFinishReasonType</c>.
    /// </param>
    /// <returns>
    /// The <c>TFinishReasonType</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TFinishReasonType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TFinishReasonType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TFinishReasonType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TFinishReasonTypeInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TFinishReasonType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TFinishReasonType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFinishReasonType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TFinishReasonType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TFinishReasonType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TFinishReasonType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TFinishReasonType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Specifies the response format options for the model's output.
  /// </summary>
  /// <remarks>
  /// This enumeration allows you to control the structure of the model's output, either as plain text or as a structured JSON object.
  /// Use <c>to_text</c> when a simple textual response is sufficient, or <c>to_json_object</c> for structured data outputs.
  /// </remarks>
  TResponseFormat = (
    /// <summary>
    /// Requests the model to output its response as plain text.
    /// </summary>
    /// <remarks>
    /// Use this option when only a textual response is needed without any specific formatting.
    /// </remarks>
    to_text,
    /// <summary>
    /// Requests the model to output its response as a JSON object.
    /// </summary>
    /// <remarks>
    /// This option enables JSON mode, ensuring the model outputs well-formed JSON.
    /// <para>
    /// Important: When using JSON mode, provide instructions to the model within your messages to produce JSON output.
    /// </para>
    /// </remarks>
    to_json_object
  );

  /// <summary>
  /// A class to represent and set up content parameters for chat message payloads.
  /// </summary>
  /// <remarks>
  /// This class allows you to specify various types of content, such as text or image URLs, within a payload.
  /// It facilitates building structured message payloads for content delivery.
  /// </remarks>
  TContentParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the type of the content within the payload.
    /// </summary>
    /// <param name="Value">
    /// The content type, defined by the <c>TContentType</c> enumeration (e.g., text or image URL).
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentParams</c> instance, enabling method chaining.
    /// </returns>
    /// <remarks>
    /// Setting the content type helps in determining how the payload should be interpreted (e.g., as plain text or as an image URL).
    /// </remarks>
    function &Type(const Value: TContentType): TContentParams;
    /// <summary>
    /// Sets the text content of the payload.
    /// </summary>
    /// <param name="Value">
    /// The text string to be included as content.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentParams</c> instance, enabling method chaining.
    /// </returns>
    /// <remarks>
    /// This method allows you to include plain text as the payload content, which can be used in text-based messages.
    /// </remarks>
    function Text(const Value: string): TContentParams;
    /// <summary>
    /// Sets the content of the payload as an image URL.
    /// </summary>
    /// <param name="Value">
    /// The image URL to be included as content.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentParams</c> instance, enabling method chaining.
    /// </returns>
    /// <remarks>
    /// This method allows you to specify an image URL as the content, which can be used to display images in message payloads.
    /// </remarks>
    function ImageUrl(const Value: string): TContentParams;
    /// <summary>
    /// Creates and returns a <c>TContentParams</c> instance with text content.
    /// </summary>
    /// <param name="Value">
    /// The text string to be set as the payload content.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentParams</c> instance with text content.
    /// </returns>
    /// <remarks>
    /// This static method provides a convenient way to initialize a <c>TContentParams</c> object with text content.
    /// </remarks>
    class function AddText(const Value: string): TContentParams;
    /// <summary>
    /// Creates and returns a <c>TContentParams</c> instance with image file content.
    /// </summary>
    /// <param name="Value">
    /// The file path or URL of the image to be set as the payload content.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentParams</c> instance with image content.
    /// </returns>
    /// <remarks>
    /// This static method provides a convenient way to initialize a <c>TContentParams</c> object with image content, useful for including image URLs in messages.
    /// </remarks>
    class function AddImageFile(const Value: string): TContentParams;
  end;

  /// <summary>
  /// Represents a payload for a chat message, defining the sender role and the message content.
  /// </summary>
  /// <remarks>
  /// The <c>TPayload</c> class provides methods to set the role of the message sender
  /// (e.g., user, assistant, system, or tool) and allows customization of the content,
  /// which can be in plain text or a JSON array for more complex message structures.
  /// This class is essential for constructing structured chat messages, especially in
  /// contexts where distinguishing between message roles is important.
  /// </remarks>
  TPayload = class(TJSONParam)
  public
    /// <summary>
    /// Specifies the role of the message sender.
    /// </summary>
    /// <param name="Value">
    /// A <c>TRoleType</c> enum value representing the role (user, assistant, system, or tool).
    /// </param>
    /// <returns>
    /// The updated <c>TPayload</c> instance, enabling method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to define the sender's role for the current message,
    /// which is critical for establishing context in multi-turn conversations.
    /// </remarks>
    function Role(const Value: TRoleType): TPayload;
    /// <summary>
    /// Sets a single string as the content of the message.
    /// </summary>
    /// <param name="Value">
    /// A string representing the message content.
    /// </param>
    /// <returns>
    /// The updated <c>TPayload</c> instance, enabling method chaining.
    /// </returns>
    function Content(const Value: string): TPayload; overload;
    /// <summary>
    /// Sets an array of payload content as the message content.
    /// </summary>
    /// <param name="Value">
    /// A <c>TJSONArray</c> representing the complex message content.
    /// </param>
    /// <returns>
    /// The updated <c>TPayload</c> instance, enabling method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method when creating a message with multiple content elements,
    /// such as text and image URLs, allowing for a structured message format.
    /// </remarks>
    function Content(const Value: TJSONArray): TPayload; overload;
    /// <summary>
    /// Creates a user message with the specified content.
    /// </summary>
    /// <param name="Value">
    /// The text content of the user message.
    /// </param>
    /// <returns>
    /// A <c>TPayload</c> instance representing a user message.
    /// </returns>
    class function User(const Value: string): TPayload; overload;
    /// <summary>
    /// Creates a user message with text content and an array of image URLs.
    /// </summary>
    /// <param name="Value">
    /// The text content of the user message.
    /// </param>
    /// <param name="Images">
    /// An array of strings, each representing an image URL to include in the message.
    /// </param>
    /// <returns>
    /// A <c>TPayload</c> instance representing a user message with images.
    /// </returns>
    /// <remarks>
    /// This method is useful for messages containing both text and visual content.
    /// Each image URL is included in the payload as a separate JSON element.
    /// </remarks>
    class function User(const Value: string; Images: TArray<string>): TPayload; overload;
    /// <summary>
    /// Creates an assistant message with the specified content.
    /// </summary>
    /// <param name="Value">
    /// The text content of the assistant message.
    /// </param>
    /// <returns>
    /// A <c>TPayload</c> instance representing an assistant message.
    /// </returns>
    class function Assistant(const Value: string): TPayload;
    /// <summary>
    /// Creates a system message with the specified content.
    /// </summary>
    /// <param name="Value">
    /// The text content of the system message.
    /// </param>
    /// <returns>
    /// A <c>TPayload</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// System messages are often used for configuring the chat session or providing
    /// context before any user or assistant interactions.
    /// </remarks>
    class function System(const Value: string): TPayload;
  end;

  /// <summary>
  /// The <c>TToolsChoice</c> class provides methods to define and specify tool choices
  /// that the model can invoke during chat operations, particularly for cases where
  /// specific functions are available for call.
  /// </summary>
  /// <remarks>
  /// This class allows you to set up a tool choice by specifying its type and the
  /// function to be called. It provides a structured way to instruct the model on
  /// specific tools it can utilize, especially in scenarios where calling external
  /// functions may enhance chat interactions.
  /// </remarks>
  TToolsChoice = class(TJSONParam)
  public
    /// <summary>
    /// Sets the type of the tool to be used by the model.
    /// </summary>
    /// <param name="Value">
    /// A string representing the type of tool (e.g., "function").
    /// </param>
    /// <returns>
    /// Returns the updated <c>TToolsChoice</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify the nature of the tool. Typically,
    /// setting this to "function" informs the model that it may call
    /// a particular function as part of its response generation.
    /// </remarks>
    function &Type(const Value: string): TToolsChoice;
    /// <summary>
    /// Specifies the function name to be used by the model as a tool.
    /// </summary>
    /// <param name="Value">
    /// The name of the function that the model may call.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TToolsChoice</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Setting a function name here is essential when using functions as tools in
    /// the chat model. This enables the model to identify and use specific functions
    /// relevant to a chat session, depending on the conversation flow and context.
    /// </remarks>
    function &Function(const Value: string): TToolsChoice;
    /// <summary>
    /// Creates a new <c>TToolsChoice</c> instance with the type set to "function"
    /// and the specified function name.
    /// </summary>
    /// <param name="Value">
    /// The name of the function to be called.
    /// </param>
    /// <returns>
    /// Returns a new <c>TToolsChoice</c> instance configured for function calls.
    /// </returns>
    /// <remarks>
    /// This method provides a convenient way to initialize a <c>TToolsChoice</c>
    /// instance specifically for cases where the model needs to access a function.
    /// By using this factory method, you streamline the setup for a function tool choice.
    /// </remarks>
    class function New(const Value: string): TToolsChoice;
  end;

  /// <summary>
  /// This class provides parameters for configuring chat message payloads within the API.
  /// </summary>
  /// <remarks>
  /// The <c>TChatParams</c> class allows you to specify various attributes for controlling message behavior,
  /// such as response length, temperature, frequency penalties, and tool usage. It offers a flexible
  /// structure for customizing chat interactions through property and method chaining.
  /// </remarks>
  TChatParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the frequency penalty to reduce the likelihood of repetitive tokens in the output.
    /// </summary>
    /// <param name="Value">
    /// A number between -2.0 and 2.0, where positive values penalize repeated tokens.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing method chaining.
    /// </returns>
    /// <remarks>
    /// Use this parameter to adjust token repetition based on their frequency in the text, thereby
    /// encouraging more unique tokens if set to a positive value.
    /// </remarks>
    function FrequencyPenalty(const Value: Double): TChatParams;
    /// <summary>
    /// <para>
    /// NOT YET SUPPORTED
    /// </para>
    /// Applies a logit bias on certain tokens in the output (currently unsupported).
    /// </summary>
    /// <param name="Value">
    /// A JSON string specifying the token biases.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing method chaining.
    /// </returns>
    /// <remarks>
    /// Although currently unsupported, this parameter allows for modifying token likelihood, which can guide
    /// the model toward or away from specified tokens in future implementations.
    /// </remarks>
    function LogitBias(const Value: string): TChatParams;
    /// <summary>
    /// <para>
    /// NOT YET SUPPORTED
    /// </para>
    /// Enables the output of log probabilities for each token (currently unsupported).
    /// </summary>
    /// <param name="Value">
    /// A Boolean flag indicating whether to return log probabilities for tokens.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Log probabilities are useful for analysis and debugging. This option will include log
    /// probabilities with each token if it is enabled in future model updates.
    /// </remarks>
    function Logprobs(const Value: Boolean): TChatParams;
    /// <summary>
    /// Sets the maximum number of tokens for the generated output.
    /// </summary>
    /// <param name="Value">
    /// Maximum token count, constrained by the model's context limit.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This parameter limits the total token count in the output to manage response length effectively.
    /// </remarks>
    function MaxToken(const Value: Integer): TChatParams;
    //// <summary>
    /// Adds a set of messages as input for the chat interaction.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TPayload</c> messages representing the conversation history.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This method provides a way to pass a history of messages for multi-turn conversation management.
    /// </remarks>
    function Messages(const Value: TArray<TPayload>): TChatParams; overload;
    /// <summary>
    /// Adds messages from a JSON object as input for the chat interaction.
    /// </summary>
    /// <param name="Value">
    /// A <c>TJSONObject</c> representing the conversation messages.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This method allows you to include messages directly from a JSON object, facilitating integration with pre-existing JSON structures or data sources.
    /// </remarks>
    function Messages(const Value: TJSONObject): TChatParams; overload;
    /// <summary>
    /// Sets the model ID to be used for the chat interaction.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model ID.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Use this parameter to specify which model variant to employ, as different models may provide
    /// distinct characteristics or capabilities.
    /// </remarks>
    function Model(const Value: string): TChatParams;
    //// <summary>
    /// Specifies how many response options to generate.
    /// </summary>
    /// <param name="Value">
    /// The number of choices, with 1 being the only supported option currently.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Although currently limited to 1, this parameter may allow multiple responses in future implementations.
    /// </remarks>
    function N(const Value: Integer): TChatParams;
    /// <summary>
    /// Sets whether tool calls can be made in parallel.
    /// </summary>
    /// <param name="Value">
    /// A Boolean flag to enable or disable parallel tool calls.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    function ParallelToolCalls(const Value: Boolean): TChatParams;
    /// <summary>
    /// Sets the presence penalty to encourage new topics.
    /// </summary>
    /// <param name="Value">
    /// A value between -2.0 and 2.0, where positive values encourage topic variety.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Presence penalty helps drive topic diversity in the generated output.
    /// </remarks>
    function PresencePenalty(const Value: Double): TChatParams;
    /// <summary>
    /// Specifies the format for the response output.
    /// </summary>
    /// <param name="Value">
    /// Either <c>to_text</c> or <c>to_json_object</c> for structured JSON output.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This parameter controls the response format. Set to JSON to enforce structured responses, especially for API usage.
    /// </remarks>
    function ResponseFormat(const Value: TResponseFormat): TChatParams;
    /// <summary>
    /// Seeds the sampling for deterministic output.
    /// </summary>
    /// <param name="Value">
    /// An integer seed for consistent results across identical queries.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Deterministic sampling is not guaranteed but can be influenced by setting this value.
    /// </remarks>
    function Seed(const Value: Integer): TChatParams;
    /// <summary>
    /// Defines sequences that will terminate generation.
    /// </summary>
    /// <param name="Value">
    /// A string or array of strings indicating stop sequences.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This feature helps control response endings by specifying phrases or sequences where generation should halt.
    /// </remarks>
    function Stop(const Value: string): TChatParams; overload;
    /// <summary>
    /// Defines sequences that will terminate generation.
    /// </summary>
    /// <param name="Value">
    /// An array of strings indicating stop sequences.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This method allows specifying multiple phrases or sequences where the generation should stop, giving you fine-grained control over the response endings.
    /// </remarks>
    function Stop(const Value: TArray<string>): TChatParams; overload;
    /// <summary>
    /// Enables token streaming for partial responses.
    /// </summary>
    /// <param name="Value">
    /// A Boolean indicating whether streaming mode is active.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Streaming sends token-by-token responses, useful for real-time applications or progressive displays.
    /// </remarks>
    function Stream(const Value: Boolean): TChatParams;
    /// <summary>
    /// Configures stream response options, such as usage metrics.
    /// </summary>
    /// <param name="Value">
    /// A Boolean to include usage metrics in streamed responses.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    function StreamOptions(const Value: Boolean = True): TChatParams;
    /// <summary>
    /// Sets the sampling temperature to control randomness in the output.
    /// </summary>
    /// <param name="Value">
    /// A number between 0 and 2, with higher values making responses more random.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Temperature and <c>top_p</c> work in conjunction but should typically not be used together. Higher temperatures result in less deterministic outputs.
    /// </remarks>
    function Temperature(const Value: Double): TChatParams;
    /// <summary>
    /// Controls which (if any) tool is called by the model.
    /// </summary>
    /// <param name="Value">
    /// A value defined by <c>TToolChoiceType</c>
    /// </param>
    /// <returns>
    /// A <c>TChatParams</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// <para>
    /// - <c>none</c> means the model will not call any tool and instead generates a message.
    /// </para>
    /// <para>
    /// - <c>auto</c> means the model can pick between generating a message or calling one or more tools.
    /// </para>
    /// <para>
    /// - <c>required</c> means the model must call one or more tools. Specifying a particular tool via {"type": "function", "function": {"name": "my_function"}} forces the model to call that tool.
    /// </para>
    /// <c>none</c> is the default when no tools are present. <c>auto</c> is the default if tools are present.
    /// Show possible types
    /// </remarks>
    function ToolChoice(const Value: TToolChoiceType): TChatParams; overload;
    /// <summary>
    /// Specifies a particular tool or function to be called by the model.
    /// </summary>
    /// <param name="Value">
    /// A string representing the name of the tool or function the model should use.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// By specifying the name of the tool, you can force the model to call a specific tool or function, overriding the default behavior.
    /// </remarks>
    function ToolChoice(const Value: string): TChatParams; overload;
    /// <summary>
    /// Sets the tool choice parameters for the model using a <c>TToolsChoice</c> instance.
    /// </summary>
    /// <param name="Value">
    /// A <c>TToolsChoice</c> object defining the tool or function the model may call.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This method allows for advanced configuration of the tool choice, including specifying types and functions through a <c>TToolsChoice</c> object.
    /// </remarks>
    function ToolChoice(const Value: TToolsChoice): TChatParams; overload;
    /// <summary>
    /// A list of tools the model may call.
    /// </summary>
    /// <param name="Value">
    /// A JSON object defined in advance
    /// </param>
    /// <returns>
    /// A <c>TChatParams</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// Currently, only functions are supported as a tool. Use this to provide a list of functions the model may generate JSON inputs for. A max of 128 functions are supported.
    /// </remarks>
    function Tools(const Value: TJSONObject): TChatParams; overload;
    /// <summary>
    /// Specifies a list of tools that the model may call during the chat interaction.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TJSONObject</c> instances, each representing a tool or function definition.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Use this method to provide multiple tool or function definitions that the model can choose from. Currently, only functions are supported as tools, and a maximum of 128 functions can be specified.
    /// </remarks>
    function Tools(const Value: TArray<TJSONObject>): TChatParams; overload;
    /// <summary>
    /// Specifies a list of tools that the model may call, using an array of function core interfaces.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>IFunctionCore</c> instances representing the functions the model may use.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This method allows you to define tools as interfaces, enabling more dynamic and flexible function definitions for the model to call. A maximum of 128 functions can be provided.
    /// </remarks>
    function Tools(const Value: TArray<IFunctionCore>): TChatParams; overload;
    /// <summary>
    /// This is not yet supported by any of our models.
    /// </summary>
    /// <param name="Value">
    /// An integer value between 0 and 20
    /// </param>
    /// <returns>
    /// A <c>TChatParams</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// The integer value specifying the number of most likely tokens to return at each token position, each with an associated log probability. logprobs must be set to true if this parameter is used.
    /// </remarks>
    function TopLogprobs(const Value: Integer): TChatParams;
    /// <summary>
    /// An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass.
    /// </summary>
    /// <param name="Value">
    /// A value of 0.1 means only the tokens comprising the top 10% probability mass are considered.
    /// </param>
    /// <returns>
    /// A <c>TChatParams</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// We generally recommend altering this or temperature but not both.
    /// </remarks>
    function TopP(const Value: Double): TChatParams;
    /// <summary>
    /// A unique identifier representing your end-user
    /// </summary>
    /// <param name="Value">
    /// An Id for the end-user
    /// </param>
    /// <returns>
    /// A <c>TChatParams</c> instance representing a system message.
    /// </returns>
    /// <remarks>
    /// This can help us monitor and detect abuse.
    /// </remarks>
    function User(const Value: String): TChatParams;
  end;

  /// <summary>
  /// Represents the arguments for a function call within a chat interaction.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the name and arguments of a function that may be called by the chat model.
  /// It is used to define the function invocation details when the model decides to execute a function as part of its response.
  /// </remarks>
  TFunctionArgs = class
  private
    FName: string;
    FArguments: string;
  public
    /// <summary>
    /// The name of the function to be called.
    /// </summary>
    /// <remarks>
    /// This should match the name of a function defined in the tools available to the model.
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// A JSON-formatted string representing the arguments to pass to the function.
    /// </summary>
    /// <remarks>
    /// The arguments should be formatted as a JSON object, matching the expected parameters of the function.
    /// </remarks>
    property Arguments: string read FArguments write FArguments;
  end;

  /// <summary>
  /// Represents a tool call made by the chat model, potentially including a function call.
  /// </summary>
  /// <remarks>
  /// This class captures the details of a tool invocation, including its identifier, type, and the function arguments if applicable.
  /// It is used to represent tools that the model decides to call during a conversation, such as functions for additional processing or data retrieval.
  /// </remarks>
  TToolCall = class
  private
    FId: string;
    FType: string;
    FFunction: TFunctionArgs;
  public
    /// <summary>
    /// A unique identifier for the tool call.
    /// </summary>
    /// <remarks>
    /// The identifier can be used to track or reference specific tool calls within the conversation.
    /// </remarks>
    property Id: string read FId write FId;  
    /// <summary>
    /// The type of the tool being called (e.g., "function").
    /// </summary>
    /// <remarks>
    /// The type indicates the nature of the tool, such as whether it's a function call or another kind of tool.
    /// </remarks>
    property &Type: string read FType write FType;
    /// <summary>
    /// Details of the function to be called, including its name and arguments.
    /// </summary>
    /// <remarks>
    /// If the tool call is a function call, this property contains the specifics of the function invocation.
    /// </remarks>
    property &Function: TFunctionArgs read FFunction write FFunction;
    /// <summary>
    /// Destructor for the TToolCall class.
    /// </summary>
    /// <remarks>
    /// Ensures that the associated TFunctionArgs instance is properly destroyed when the TToolCall instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a message in a chat conversation, including its role, content, and any tool calls made.
  /// </summary>
  /// <remarks>
  /// This class encapsulates a single message within a chat interaction, providing properties for the sender's role,
  /// the message content, any refusal messages, and any tool calls the model may have initiated.
  /// It is essential for tracking the flow of conversation and any actions taken by the model.
  /// </remarks>
  TChatMessage = class
  private
    [JsonReflectAttribute(ctString, rtString, TRoleTypeInterceptor)]
    FRole: TRoleType;
    FContent: string;
    FRefusal: string;
    [JsonNameAttribute('tool_calls')]
    FToolCalls: TArray<TToolCall>;
  public
    /// <summary>
    /// The role of the message sender (e.g., user, assistant, system, tool).
    /// </summary>
    property Role: TRoleType read FRole write FRole;
    /// <summary>
    /// The textual content of the message.
    /// </summary>
    /// <remarks>
    /// The content may include the assistant's response, user input, or system prompts.
    /// </remarks>
    property Content: string read FContent write FContent;
    /// <summary>
    /// A message indicating any refusal from the model to comply with a request.
    /// </summary>
    /// <remarks>
    /// This property is used when the model chooses not to fulfill a request due to policy or other constraints.
    /// </remarks>
    property Refusal: string read FRefusal write FRefusal;
    /// <summary>
    /// An array of tool calls made by the model during this message.
    /// </summary>
    /// <remarks>
    /// If the model invokes any tools (e.g., functions) during its response generation, those tool calls are recorded here.
    /// </remarks>
    property ToolCalls: TArray<TToolCall> read FToolCalls write FToolCalls;
    /// <summary>
    /// Destructor for the TChatMessage class.
    /// </summary>
    /// <remarks>
    /// Ensures that all associated TToolCall instances are properly destroyed when the TChatMessage instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the top log probabilities for tokens in the model's output.
  /// </summary>
  /// <remarks>
  /// This class captures the token, its log probability, and the byte representation of the token.
  /// It is used for detailed analysis of the model's token predictions, especially when log probabilities are requested.
  /// </remarks>
  TTopLogprobs = class
  private
    FToken: string;
    FLogprob: Double;
    FBytes: TArray<Int64>;
  public
    /// <summary>
    /// The token text as predicted by the model.
    /// </summary>
    property Token: string read FToken write FToken;
    /// <summary>
    /// The log probability of the token.
    /// </summary>
    /// <remarks>
    /// Log probabilities are in natural logarithm base and represent the likelihood of the token.
    /// </remarks>
    property Logprob: Double read FLogprob write FLogprob;
    /// <summary>
    /// The byte sequence representing the token.
    /// </summary>
    /// <remarks>
    /// Useful for understanding the token at the byte level, especially for encoding or decoding processes.
    /// </remarks>
    property Bytes: TArray<Int64> read FBytes write FBytes;
  end;

  /// <summary>
  /// Represents the log probabilities for a content token in the model's output.
  /// </summary>
  /// <remarks>
  /// This class includes the token, its log probability, the byte representation, and the top log probabilities for alternative tokens.
  /// It provides a comprehensive view of the model's decision-making for each token.
  /// </remarks>
  TLogprobsContent = class
  private
    FToken: string;
    FLogprob: Double;
    FBytes: TArray<Int64>;
    [JsonNameAttribute('top_logprobs')]
    FTopLogprobs: TArray<TTopLogprobs>;
  public
    /// <summary>
    /// The token text as generated in the content.
    /// </summary>
    property Token: string read FToken write FToken;
    /// <summary>
    /// The log probability of the token.
    /// </summary>
    /// <remarks>
    /// Provides the likelihood of the token appearing in this context.
    /// </remarks>
    property Logprob: Double read FLogprob write FLogprob;
    /// <summary>
    /// The byte sequence representing the token.
    /// </summary>
    property Bytes: TArray<Int64> read FBytes write FBytes;
    /// <summary>
    /// An array of top alternative tokens with their log probabilities.
    /// </summary>
    /// <remarks>
    /// This allows for analysis of other tokens the model considered at this position and their respective probabilities.
    /// </remarks>
    property TopLogprobs: TArray<TTopLogprobs> read FTopLogprobs write FTopLogprobs;
    /// <summary>
    /// Destructor for the TLogprobsContent class.
    /// </summary>
    /// <remarks>
    /// Ensures that all associated TTopLogprobs instances are properly destroyed when the TLogprobsContent instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the log probabilities for a refusal token in the model's output.
  /// </summary>
  /// <remarks>
  /// This class is identical to TLogprobsContent and is used when the model outputs a refusal message.
  /// It inherits from TLogprobsContent to reuse the same structure for consistency.
  /// </remarks>
  TLogprobsRefusal = TLogprobsContent;

  /// <summary>
  /// Contains the log probabilities for both content and refusal tokens in the model's output.
  /// </summary>
  /// <remarks>
  /// This class aggregates the log probabilities of all tokens generated by the model, including both content and any refusal messages.
  /// It is useful for analyzing the model's token-level outputs comprehensively.
  /// </remarks>
  TLogprobs = class
  private
    FContent: TArray<TLogprobsContent>;
    FRefusal: TArray<TLogprobsRefusal>;
  public
    /// <summary>
    /// An array of log probabilities for content tokens.
    /// </summary>
    property Content: TArray<TLogprobsContent> read FContent write FContent;
    /// <summary>
    /// An array of log probabilities for refusal tokens.
    /// </summary>
    /// <remarks>
    /// Populated when the model generates a refusal message instead of the requested content.
    /// </remarks>
    property Refusal: TArray<TLogprobsRefusal> read FRefusal write FRefusal;
    /// <summary>
    /// Destructor for the TLogprobs class.
    /// </summary>
    /// <remarks>
    /// Ensures that all associated TLogprobsContent and TLogprobsRefusal instances are properly destroyed when the TLogprobs instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a single choice returned by the chat model, including the message and any associated metadata.
  /// </summary>
  /// <remarks>
  /// This class encapsulates one of the possible responses the model generates, including the message content,
  /// finish reason, and log probabilities if requested. It is used to parse and handle the model's output choices.
  /// </remarks>
  TChatChoice = class
  private
    FIndex: Int64;
    [JsonReflectAttribute(ctString, rtString, TFinishReasonTypeInterceptor)]
    [JsonNameAttribute('finish_reason')]
    FFinishReason: TFinishReasonType;
    FLogprobs: TLogprobs;
    FMessage: TChatMessage;
    FDelta: TChatMessage;
  public
    /// <summary>
    /// The index of this choice in the list of choices returned.
    /// </summary>
    property Index: Int64 read FIndex write FIndex;
    /// <summary>
    /// The reason why the model stopped generating the output.
    /// </summary>
    /// <remarks>
    /// Indicates whether the model stopped due to reaching the end of the message, hitting a stop sequence, or other reasons.
    /// </remarks>
    property FinishReason: TFinishReasonType read FFinishReason write FFinishReason;
    /// <summary>
    /// The message generated by the model.
    /// </summary>
    /// <remarks>
    /// Contains the actual content of the model's response.
    /// </remarks>
    property Message: TChatMessage read FMessage write FMessage;
    /// <summary>
    /// Incremental message content for streaming responses.
    /// </summary>
    /// <remarks>
    /// Used when responses are streamed token by token; contains the latest delta in the message.
    /// </remarks>
    property Delta: TChatMessage read FDelta write FDelta;
    /// <summary>
    /// The log probabilities associated with the tokens in the message.
    /// </summary>
    /// <remarks>
    /// Populated if log probabilities were requested; provides detailed token-level probability information.
    /// </remarks>
    property Logprobs: TLogprobs read FLogprobs write FLogprobs;
    /// <summary>
    /// Destructor for the TChatChoice class.
    /// </summary>
    /// <remarks>
    /// Ensures that all associated TChatMessage and TLogprobs instances are properly destroyed when the TChatChoice instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Provides usage metrics for a chat interaction with the model.
  /// </summary>
  /// <remarks>
  /// This class contains detailed information about token usage and timing, allowing for analysis of the model's performance and billing calculations.
  /// </remarks>
  TChatUsage = class
  private
    [JsonNameAttribute('queue_time')]
    FQueueTime: Double;
    [JsonNameAttribute('prompt_tokens')]
    FPromptTokens: Int64;
    [JsonNameAttribute('prompt_time')]
    FPromptTime: Double;
    [JsonNameAttribute('completion_tokens')]
    FCompletionTokens: Int64;
    [JsonNameAttribute('completion_time')]
    FCompletionTime: Double;
    [JsonNameAttribute('total_tokens')]
    FTotalTokens: Int64;
    [JsonNameAttribute('total_time')]
    FTotalTime: Double;
  public
    /// <summary>
    /// The time in seconds the request spent in the queue before processing.
    /// </summary>
    property QueueTime: Double read FQueueTime write FQueueTime;
    /// <summary>
    /// The number of tokens in the prompt sent to the model.
    /// </summary>
    property PromptTokens: Int64 read FPromptTokens write FPromptTokens;
    /// <summary>
    /// The time in seconds taken to process the prompt.
    /// </summary>
    property PromptTime: Double read FPromptTime write FPromptTime;
    /// <summary>
    /// The number of tokens generated in the model's completion.
    /// </summary>
    property CompletionTokens: Int64 read FCompletionTokens write FCompletionTokens;
    /// <summary>
    /// The time in seconds taken to generate the completion.
    /// </summary>
    property CompletionTime: Double read FCompletionTime write FCompletionTime;
    /// <summary>
    /// The total number of tokens used (prompt and completion).
    /// </summary>
    property TotalTokens: Int64 read FTotalTokens write FTotalTokens;
    /// <summary>
    /// The total time in seconds for processing the request.
    /// </summary>
    property TotalTime: Double read FTotalTime write FTotalTime;
  end;

  /// <summary>
  /// Represents the complete response from the chat model, including choices and usage metrics.
  /// </summary>
  /// <remarks>
  /// This class is the top-level container for the model's response to a chat completion request.
  /// It includes the generated choices, usage statistics, and other metadata.
  /// </remarks>
  TChat = class
  private
    FId: string;
    FObject: string;
    FCreated: Int64;
    FModel: string;
    FChoices: TArray<TChatChoice>;
    FUsage: TChatUsage;
    [JsonNameAttribute('system_fingerprint')]
    FSystemFingerprint: string;
    [JsonNameAttribute('x_groq')]
    FXGroq: TXGroq;
  public
    /// <summary>
    /// The unique identifier for the chat completion.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, typically "chat.completion".
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The timestamp when the completion was created.
    /// </summary>
    /// <remarks>
    /// Represented as seconds since the Unix epoch.
    /// </remarks>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// The model used for generating the completion.
    /// </summary>
    property Model: string read FModel write FModel;
    /// <summary>
    /// An array of choices returned by the model.
    /// </summary>
    /// <remarks>
    /// Each choice represents a possible completion generated by the model.
    /// </remarks>
    property Choices: TArray<TChatChoice> read FChoices write FChoices;
    /// <summary>
    /// Usage information for the completion request.
    /// </summary>
    property Usage: TChatUsage read FUsage write FUsage;
    /// <summary>
    /// A fingerprint representing the system state.
    /// </summary>
    /// <remarks>
    /// Used for internal tracking and debugging purposes.
    /// </remarks>
    property SystemFingerprint: string read FSystemFingerprint write FSystemFingerprint;
    /// <summary>
    /// Additional metadata provided by the Groq API.
    /// </summary>
    property XGroq: TXGroq read FXGroq write FXGroq;
    /// <summary>
    /// Destructor for the TChat class.
    /// </summary>
    /// <remarks>
    /// Ensures that all associated TChatChoice and TXGroq instances are properly destroyed when the TChat instance is freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = reference to procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = TAsynCallBack<TChat>;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = TAsynStreamCallBack<TChat>;

  /// <summary>
  /// The <c>TChatRoute</c> class inherits from <c>TGroqAPIRoute</c> and provides an interface for managing various interactions with the chat API.
  /// It supports creating chat completion requests in synchronous, asynchronous, and streaming modes, offering mechanisms to handle responses generated by the model.
  /// </summary>
  /// <remarks>
  /// This class facilitates sending messages to a chat model, receiving responses, and managing them, whether synchronously or asynchronously.
  /// The primary methods in the class are:
  /// <para>
  /// - <c>Create</c> : Sends a chat request and waits for a full response.
  /// </para>
  /// <para>
  /// - <c>AsynCreate</c> : Performs an asynchronous chat completion request with event handling.
  /// </para>
  /// <para>
  /// - <c>CreateStream</c> : Initiates a chat completion request in streaming mode, receiving tokens progressively.
  /// </para>
  /// <para>
  /// - <c>ASynCreateStream</c> : Performs an asynchronous request in streaming mode with event handling.
  /// </para>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including callbacks for processing responses or errors.
  /// </remarks>
  TChatRoute = class(TGroqAPIRoute)
  public
    /// <summary>
    /// Create an asynchronous completion for chat message
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the chat request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided callBacks.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    /// GroqCloud.Chat.AsynCreate(
    ///   procedure (Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///   function: TAsynChat
    ///   begin
    ///     Result.Sender := Memo1;  // Instance passed to callback parameter
    ///
    ///     Result.OnStart := nil;   // If nil then; Can be omitted
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Chat: TChat)
    ///     begin
    ///       var M := Sender as TMemo; // Because Result.Sender = Memo1
    ///       // Handle success operation
    ///     end;
    ///
    ///     Result.OnError := procedure (Sender: TObject; Value: string)
    ///     begin
    ///       // Handle error message
    ///     end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChat>);
    /// <summary>
    /// Creates an asynchronous streaming chat completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynChatStream</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// CheckBox1.Checked := False;  //Click to stop the streaming
    /// // WARNING - Move the following line into the main OnCreate
    /// //var GroqCoud := TGroqFactory.CreateInstance(BaererKey);
    /// GroqCoud.Chat.AsynCreateStream(
    ///   procedure(Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///
    ///   function: TAsynChatStream
    ///   begin
    ///     Result.Sender := Memo1; // Instance passed to callback parameter
    ///     Result.OnProgress :=
    ///         procedure (Sender: TObject; Chat: TChat)
    ///         begin
    ///           // Handle progressive updates to the chat response
    ///         end;
    ///     Result.OnSuccess :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Handle success when the operation completes
    ///         end;
    ///     Result.OnError :=
    ///         procedure (Sender: TObject; Value: string)
    ///         begin
    ///           // Handle error message
    ///         end;
    ///     Result.OnDoCancel :=
    ///         function: Boolean
    ///         begin
    ///           Result := CheckBox1.Checked; // Click on checkbox to cancel
    ///         end;
    ///     Result.OnCancellation :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Processing when process has been canceled
    ///         end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreateStream(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChatStream>);
    /// <summary>
    /// Creates a completion for the chat message using the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, setting token limits, etc.
    /// </param>
    /// <returns>
    /// Returns a <c>TChat</c> object that contains the chat response, including the choices generated by the model.
    /// </returns>
    /// <exception cref="GroqExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="GroqExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The <c>Create</c> method sends a chat completion request and waits for the full response. The returned <c>TChat</c> object contains the model's generated response, including multiple choices if available.
    ///
    /// Example usage:
    /// <code>
    ///   var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    ///   var Chat := GroqCloud.Chat.Create(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     for var Item in Chat.Candidates do
    ///       WriteLn(Item.Text);
    ///   finally
    ///     Chat.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TChatParams>): TChat;
    /// <summary>
    /// Creates a chat message completion with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, and adjusting other settings like token limits or temperature.
    /// </param>
    /// <param name="Event">
    /// A callback of type <c>TChatEvent</c> that is triggered with each chunk of data received during the streaming process. It includes the current state of the <c>TChat</c> object, a flag indicating if the stream is done, and a boolean to handle cancellation.
    /// </param>
    /// <returns>
    /// Returns <c>True</c> if the streaming process started successfully, <c>False</c> otherwise.
    /// </returns>
    /// <remarks>
    /// This method initiates a chat request in streaming mode, where the response is delivered incrementally in real-time.
    /// The <c>Event</c> callback will be invoked multiple times as tokens are received.
    /// When the response is complete, the <c>IsDone</c> flag will be set to <c>True</c>, and the <c>Chat</c> object will be <c>nil</c>.
    /// The streaming process can be interrupted by setting the <c>Cancel</c> flag to <c>True</c> within the event.
    ///
    /// Example usage:
    /// <code>
    ///   var GroqCloud := TGroqFactory.CreateInstance(BaererKey);
    ///   GroqCloud.Chat.CreateStream(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///       Params.Stream(True);
    ///     end,
    ///
    ///     procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       // Handle displaying
    ///     end
    ///   );
    /// </code>
    /// </remarks>
    function CreateStream(ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TRoleTypeHelper }

class function TRoleTypeHelper.Create(const Value: string): TRoleType;
begin
  var Index := IndexStr(Value.ToLower, ['user', 'assistant', 'system', 'tool']);
  if Index = -1 then
    raise Exception.Create('Role type unknown');
  Result := TRoleType(index);
end;

function TRoleTypeHelper.ToString: string;
begin
  case Self of
    user:
      Exit('user');
    assistant:
      Exit('assistant');
    system:
      Exit('system');
    tool:
      Exit('tool');
  end;
end;

{ TRoleTypeInterceptor }

function TRoleTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TRoleType>.ToString;
end;

procedure TRoleTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TRoleType.Create(Arg)));
end;

{ TPayload }

class function TPayload.Assistant(const Value: string): TPayload;
begin
  Result := TPayload.Create.Role(TRoleType.assistant).Content(Value);
end;

function TPayload.Content(const Value: string): TPayload;
begin
  Result := TPayload(Add('content', Value));
end;

function TPayload.Content(const Value: TJSONArray): TPayload;
begin
  Result := TPayload(Add('content', Value));
end;

function TPayload.Role(const Value: TRoleType): TPayload;
begin
  Result := TPayload(Add('role', Value.ToString));
end;

class function TPayload.System(const Value: string): TPayload;
begin
  Result := TPayload.Create.Role(TRoleType.system).Content(Value);
end;

class function TPayload.User(const Value: string;
  Images: TArray<string>): TPayload;
begin
  var JSONArray := TJSONArray.Create.Add(TContentParams.AddText(Value).Detach);
  for var Item in Images do
    JSONArray.Add(TContentParams.AddImageFile(Item).Detach);
  Result := TPayload.Create.Role(TRoleType.user).Content(JSONArray);
end;

class function TPayload.User(const Value: string): TPayload;
begin
  Result := TPayload.Create.Role(TRoleType.user).Content(Value);
end;

{ TChatParams }

function TChatParams.Messages(const Value: TArray<TPayload>): TChatParams;
begin
  var JSONMessages := TJSONArray.Create;
  for var Item in Value do
    JSONMessages.Add(Item.Detach);
  Result := TChatParams(Add('messages', JSONMessages));
end;

function TChatParams.FrequencyPenalty(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('frequency_penalty', Value));
end;

function TChatParams.LogitBias(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('logit_bias', Value));
end;

function TChatParams.Logprobs(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('logprobs', Value));
end;

function TChatParams.MaxToken(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_token', Value));
end;

function TChatParams.Messages(const Value: TJSONObject): TChatParams;
begin
  Result := TChatParams(Add('messages', Value));
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.N(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('n', Value));
end;

function TChatParams.ParallelToolCalls(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('parallel_tool_calls', Value));
end;

function TChatParams.PresencePenalty(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('presence_penalty', Value));
end;

function TChatParams.ResponseFormat(const Value: TResponseFormat): TChatParams;
begin
  var JSONParam: TJSONObject := nil;
  case Value of
    to_text:
      JSONParam := TJSONObject.Create.AddPair('type', 'text');
    to_json_object:
      JSONParam := TJSONObject.Create.AddPair('type', 'json_object');
  end;
  if Assigned(JSONParam) then
    Result := TChatParams(Add('response_format', JSONParam)) else
    Result := Self;
end;

function TChatParams.Seed(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('seed', Value));
end;

function TChatParams.Stop(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('stop', Value));
end;

function TChatParams.Stop(const Value: TArray<string>): TChatParams;
begin
  Result := TChatParams(Add('stop', Value));
end;

function TChatParams.Stream(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('stream', Value));
end;

function TChatParams.StreamOptions(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('temperature', TJSONObject.Create.AddPair('include_usage', Value)));
end;

function TChatParams.Temperature(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('temperature', Value));
end;

function TChatParams.Tools(const Value: TJSONObject): TChatParams;
begin
  Result := TChatParams(Add('tools', Value));
end;

function TChatParams.Tools(const Value: TArray<TJSONObject>): TChatParams;
begin
  var JSONTools := TJSONArray.Create;
  for var Item in Value do
    JSONTools.Add(Item);
  Result := TChatParams(Add('tools', JSONTools));
end;

function TChatParams.ToolChoice(const Value: TToolChoiceType): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', Value.ToString));
end;

function TChatParams.ToolChoice(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', TToolsChoice.New(Value).Detach));
end;

function TChatParams.ToolChoice(const Value: TToolsChoice): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', Value.Detach));
end;

function TChatParams.Tools(const Value: TArray<IFunctionCore>): TChatParams;
begin
  var JSONTools := TJSONArray.Create;
  for var Item in Value do
    JSONTools.Add(Item.ToJson);
  Result := TChatParams(Add('tools', JSONTools));
end;

function TChatParams.TopLogprobs(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('top_logprobs', Value));
end;

function TChatParams.TopP(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('top_p', Value));
end;

function TChatParams.User(const Value: String): TChatParams;
begin
  Result := TChatParams(Add('user', Value));
end;

{ TChatRoute }

procedure TChatRoute.AsynCreate(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChat>);
begin
  with TAsynCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TChatRoute.AsynCreateStream(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChatStream>);
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
        procedure()
        begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStream(ParamProc,
                procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  if not IsDone and Assigned(Chat) then
                    begin
                      var LocalChat := Chat;
                      Chat := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChat);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
                          end;
                        end);
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnSuccess(Sender);
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
        end);
  Task.Start;
end;

function TChatRoute.Create(ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('chat/completions', ParamProc);
end;

function TChatRoute.CreateStream(ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TChatParams>('chat/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Chat: TChat;
        TextBuffer: string;
        Line: string;
        Ret: Integer;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        repeat
          Ret := TextBuffer.IndexOf(#10, RetPos);
          if Ret < 0 then
            Continue;
          Line := TextBuffer.Substring(RetPos, Ret - RetPos);
          RetPos := Ret + 1;

          if Line.IsEmpty or Line.StartsWith(#10) then
            Continue;
          Chat := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Chat := TJson.JsonToObject<TChat>(Data);
          except
            Chat := nil;
          end;

          try
            Event(Chat, IsDone, AAbort);
          finally
            Chat.Free;
          end;
        until Ret < 0;

      end);
  finally
    Response.Free;
  end;
end;

{ TChatChoice }

destructor TChatChoice.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDelta) then
    FDelta.Free;
  if Assigned(FLogprobs) then
    FLogprobs.Free;
  inherited;
end;

{ TChat }

destructor TChat.Destroy;
begin
  for var Item in FChoices do
    Item.Free;
  if Assigned(FUsage) then
    FUsage.Free;
  if Assigned(FXGroq) then
    FXGroq.Free;
  inherited;
end;

{ TToolsChoice }

function TToolsChoice.&Function(const Value: string): TToolsChoice;
begin
  Result := TToolsChoice(Add('function', TJSONObject.Create.AddPair('naem', Value)));
end;

class function TToolsChoice.New(const Value: string): TToolsChoice;
begin
  Result := TToolsChoice.Create.&Type('function').&Function(Value);
end;

function TToolsChoice.&Type(const Value: string): TToolsChoice;
begin
  Result := TToolsChoice(Add('type', Value));
end;

{ TToolChoiceTypeHelper }

function TToolChoiceTypeHelper.ToString: string;
begin
  case Self of
    none:
      Exit('none');
    auto:
      Exit('auto');
    required:
      Exit('required');
  end;
end;

{ TContentParams }

function TContentParams.&Type(const Value: TContentType): TContentParams;
begin
  Result := TContentParams(Add('type', Value.ToString));
end;

class function TContentParams.AddImageFile(const Value: string): TContentParams;
begin
  Result := TContentParams.Create.&Type(TContentType.imageUrl).ImageUrl(Value);
end;

class function TContentParams.AddText(const Value: string): TContentParams;
begin
  Result := TContentParams.Create.&Type(TContentType.text).Text(Value);
end;

function TContentParams.ImageUrl(const Value: string): TContentParams;
begin
  Result := TContentParams(Add('image_url', TJSONObject.Create.AddPair('url', ImageDataProvider(Value))));
end;

function TContentParams.Text(const Value: string): TContentParams;
begin
  Result := TContentParams(Add('text', Value));
end;

{ TContentTypeHelper }

function TContentTypeHelper.ToString: string;
begin
  case Self of
    text:
      Exit('text');
    imageUrl:
      Exit('image_url');
  end;
end;

{ TFinishReasonTypeHelper }

class function TFinishReasonTypeHelper.Create(
  const Value: string): TFinishReasonType;
begin
  var index :=  IndexStr(Value.ToLower, ['stop', 'length', 'content_filter', 'tool_calls', 'function_call']);
  if index = -1 then
    raise Exception.Create('Finish reason type unknown.');
  Result := TFinishReasonType(index);
end;

function TFinishReasonTypeHelper.ToString: string;
begin
  case Self of
    stop:
      Exit('stop');
    length:
      Exit('length');
    content_filter:
      Exit('content_filter');
    tool_calls:
      Exit('tool_calls');
    function_call:
      Exit('function_call');
  end;
end;

{ TFinishReasonTypeInterceptor }

function TFinishReasonTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFinishReasonType>.ToString;
end;

procedure TFinishReasonTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFinishReasonType.Create(Arg)));
end;

{ TLogprobs }

destructor TLogprobs.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  for var Item in FRefusal do
    Item.Free;
  inherited;
end;

{ TLogprobsContent }

destructor TLogprobsContent.Destroy;
begin
  for var Item in FTopLogprobs do
    Item.Free;
  inherited;
end;

{ TToolCall }

destructor TToolCall.Destroy;
begin
  if Assigned(FFunction) then
    FFunction.Free;
  inherited;
end;

{ TChatMessage }

destructor TChatMessage.Destroy;
begin
  for var Item in FToolCalls do
    Item.Free;
  inherited;
end;

end.
