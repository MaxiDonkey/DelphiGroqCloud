# Delphi GroqCloud API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20the%2011/13/2024-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Groq cloud console](#Groq-cloud-console)
    - [Get a key](#Get-a-key)
    - [Settings](#Settings)
- [Usage](#Usage)
    - [Asynchronous callback mode management](#Asynchronous-callback-mode-management)
    - [Groq models overview](#Groq-models-overview)
    - [Embeddings](#Embeddings)
    - [Text generation](#Text-generation)
        - [Chat completion](#Chat-completion)
             - [Synchronously text generation example](#Synchronously-text-generation-example)
             - [Asynchronously text generation example](#Asynchronously-text-generation-example)
        - [Stream chat](#Stream-chat)
             - [Synchronously chat stream](#Synchronously-chat-stream)
             - [Asynchronously chat stream](#Asynchronously-chat-stream) 
        - [Build an interactive chat](#Build-an-interactive-chat)
        - [System instructions](#System-instructions)
        - [Configure text generation](#Configure-text-generation)
    - [Vision](#Vision)
        - [Supported Model](#Supported-Model)
        - [Supported image MIME](#Supported-image-MIME)
        - [How to use vision](#How-to-use-vision)
            - [Asynchronous vision using a base64-encoded image](#Asynchronous-vision-using-a-base64-encoded-image)
            - [Asynchronous vision using an image URL](#Asynchronous-vision-using-an-image-URL)
            - [JSON Mode with Images](#JSON-Mode-with-Images)
            - [Limitations](#Limitations)
    - [Speech](#Speech)
        - [Supported models](#Supported-models) 
        - [Transcription code example](#Transcription-code-example)
        - [Translation code example](#Translation-code-example)
    - [Tool use](#Tool-use)
        - [How tool use works](#How-tool-use-works)
        - [Supported models](#Supported-models)
        - [Tool use code example](#Tool-use-code-example)
        - [How to create a tool](#How-to-create-a-tool)
    - [Content moderation](#Content-moderation)
    - [Fine-tuning](#Fine-tuning)
    - [Display methods for the tutorial ](#Display-methods-for-the-tutorial )
- [Contributing](#contributing)
- [License](#license)

<br/>
<br/>

# Introduction

Welcome to the unofficial **GroqCloud API Wrapper** for **Delphi**. This project provides a **Delphi** interface for accessing and interacting with the powerful language models available on **GroqCloud**, including those developed by : <br/>
      **`Meta`** <sub>LLama</sub>, **`OpenAI`** <sub>Whisper</sub>, **`MistralAI`** <sub>mixtral</sub>, and **`Google`** <sub>Gemma</sub>. <br/> With this library, you can seamlessly integrate state-of-the-art language generation, chat and vision capabilities, code generation, or speech-to-text transcription into your **Delphi** applications.

**GroqCloud** offers a high-performance, efficient platform optimized for running large language models via its proprietary Language Processing Units (LPUs), delivering speed and energy efficiency that surpass traditional GPUs. This wrapper simplifies access to these models, allowing you to leverage **GroqCloud's** cutting-edge infrastructure without the overhead of managing the underlying hardware.

For more details on GroqCloud's offerings, visit the [official GroqCloud documentation](https://groq.com/groqcloud/).

<br/>

# Groq cloud console

## Get a key

To initialize the API instance, you need to obtain an [API key](https://console.groq.com/keys) from GroqCloud.

Once you have a token, you can initialize `IGroq` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

> [!NOTE]
>```Pascal
>uses Groq;
>
>var GroqCloud := TGroqFactory.CreateInstance(API_KEY);
>```

>[!Warning]
> To use the examples provided in this tutorial, especially to work with asynchronous methods, I recommend defining the Groq interface with the widest possible scope.
><br/>
> So, set `GroqCloud := TGroqFactory.CreateInstance(API_KEY);` in the `OnCreate` event of your application.
><br/> 
>Where `GroqCloud: IGroq`

<br/>

## Settings

You can access your GroqCloud account settings to view your payment information, usage, limits, logs, teams, and profile by following [this link](https://console.groq.com/settings).

<br/>

# Usage

## Asynchronous callback mode management

In the context of asynchronous methods, for a method that does not involve streaming, callbacks use the following generic record: `TAsynCallBack<T> = record` defined in the `Gemini.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnError: TProc<TObject, string>; 
```
<br/>

For methods requiring streaming, callbacks use the generic record `TAsynStreamCallBack<T> = record`, also defined in the `Gemini.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnProgress: TProc<TObject, T>;
       OnError: TProc<TObject, string>;
       OnCancellation: TProc<TObject>;
       OnDoCancel: TFunc<Boolean>;
```

The name of each property is self-explanatory; if needed, refer to the internal documentation for more details.

<br/>

## Groq models overview

GroqCloud currently supports the [following models](https://console.groq.com/docs/models).

Hosted models can be accessed directly via the GroqCloud Models API endpoint by using the model IDs listed above. To retrieve a JSON list of all available models, use the endpoint at `https://api.groq.com/openai/v1/models`.

1. **Synchronously**

```Pascal
// uses Groq, Groq.Models;

  var Models := GroqCloud.Models.List;
  try
    for var Item in Models.Data do
      WriteLn(Item.Id);
  finally
    Models.Free;
  end;
```

2. **Asynchronously**

```Pascal
// uses Groq, Groq.Models;

  GroqCloud.Models.AsynList(
    function : TAsynModels
    begin
      Result.Sender := Memo1; //Set a TMemo on the form
      Result.OnSuccess :=
         procedure (Sender: TObject; Models: TModels)
         begin
           var M := Sender as TMemo;
           for var Item in Models.Data do
             begin
               M.Lines.Text := M.Text + Item.Id + sLineBreak;
               M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
             end;
         end;
      Result.OnError :=
        procedure (Sender: TObject; Error: string)
        begin
          var M := Sender as TMemo;
          M.Lines.Text := M.Text + Error + sLineBreak;
          M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
        end;
    end);
```

<br/>

## Embeddings

**GroqCloud** does not provide any solutions for text integration.

<br/>

## Text generation

### Chat completion

The **Groq Chat Completions API** interprets a series of messages and produces corresponding response outputs. These models can handle either multi-turn conversations or single-interaction tasks.

JSON Mode (Beta) JSON mode is currently in beta and ensures that all chat completions are in valid JSON format.

**How to Use:** <br/>
  1. Include `"response_format": {"type": "json_object"}` in your chat completion request.
  2. In the system prompt, specify the structure of the desired JSON output (see sample system prompts below).
<br/>

**Best Practices for Optimal Beta Performance:** <br/>
- For JSON generation, Mixtral is the most effective model, followed by Gemma, and then Llama.
- Use pretty-printed JSON for better readability over compact JSON.
- Keep prompts as concise as possible.
<br/>

**Beta Limitations:** <br/>
- Streaming is not supported.
- Stop sequences are not supported.
<br/>

**Error Code:** <br/>
If JSON generation fails, `Groq` will respond with a **400 error**, specifying `json_validate_failed` as the error code.

<br/>

>[!NOTE]
> We will use only Meta models in all the examples provided for text generation.
>

<br/>

#### Synchronously text generation example

The `GroqCloud` API allows for text generation using various inputs, like text and images. It's versatile and can support a wide array of applications, including: <br/>

- Creative writing
- Text completion
- Summarizing open-ended text
- Chatbot development
- Any custom use cases you have in mind

In the examples below, we'll use the `Display` procedures to make things simpler.
>[!TIP]
>```Pascal
>procedure Display(Sender: TObject; Value: string); overload;
>begin
>  var M := Sender as TMemo;
>  M.Lines.Text := M.Text + Value + sLineBreak;
>  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>end;
>```
>
>```Pascal
>procedure Display(Sender: TObject; Chat: TChat); overload;
>begin
>  for var Choice in Chat.Choices do
>    Display(Sender, Choice.Message.Content);
>end;
>```


```Pascal
// uses Groq, Groq.Chat;

  var Chat := GroqCloud.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Messages([TPayload.User('Explain the importance of fast language models')]);
      Params.Model('llama-3.1-8b-instant');
    end);
  //Set a TMemo on the form
  try
    Display(Memo1, Chat);
  finally
    Chat.Free;
  end;
```

<br/>

#### Asynchronously text generation example

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.AsynCreate(
    procedure (Params: TChatParams)
    begin
      Params.Messages([TPayload.User('Explain the importance of fast language models')]);
      Params.Model('llama-3.1-70b-versatile');
    end,
    //Set a TMemo on the form
    function : TAsynChat
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### Stream chat

#### Synchronously chat stream

In the examples below, we'll use the `Display` procedures to make things simpler.
>[!TIP]
>```Pascal
>procedure DisplayStream(Sender: TObject; Value: string); overload;
>begin
>  var M := Sender as TMemo;
>  for var index := 1 to Value.Length  do
>    if Value.Substring(index).StartsWith(#13)
>      then
>        begin
>          M.Lines.Text := M.Text + sLineBreak;
>          M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>        end
>      else
>        begin
>          M.Lines.BeginUpdate;
>          try
>            M.Lines.Text := M.Text + Value[index];
>            M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>          finally
>            M.Lines.EndUpdate;
>          end;
>        end;
>end;
>```
>
>```Pascal
>procedure DisplayStream(Sender: TObject; Chat: TChat); overload;
>begin
>  for var Item in Chat.Choices do
>    if Assigned(Item.Delta) then
>      DisplayStream(Sender, Item.Delta.Content)
>    else
>    if Assigned(Item.Message) then
>      DisplayStream(Sender, Item.Message.Content);
>end;
>```

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.CreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Messages([TPayload.User('How did we come to develop thermodynamics?')]);
      Params.Model('llama3-70b-8192');
      Params.Stream(True);
    end,
    procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if Assigned(Chat) then
        DisplayStream(Memo1, Chat);
    end);
```

<br/>

#### Asynchronously chat stream

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Messages([TPayload.User('How did we come to develop thermodynamics?')]);
      Params.Model('llama-3.1-70b-versatile');
      Params.Stream(True);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```

<br/>

### Build an interactive chat

You can utilize the `GroqCloud` API to build interactive chat experiences customized for your users. With the API’s chat capability, you can facilitate multiple rounds of questions and answers, allowing users to gradually work toward their solutions or get support for complex, multi-step issues. This feature is particularly valuable for applications that need ongoing interaction, like :
- Chatbots, 
- Educational tools
- Customer support assistants.

Here’s an asynchrounly sample of a simple chat setup:

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-3.2-3b-preview');
      Params.Messages([
        TPayload.User('Hello'),
        TPayload.Assistant('Great to meet you. What would you like to know?'),
        TPayload.User('I have two dogs in my house. How many paws are in my house?')
      ]);
      Params.Stream(True);
    end,
    //Set a TMemo on the form
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```
<br/>

### System instructions

When configuring an AI model, you have the option to set guidelines for how it should respond. For instance, you could assign it a particular role, like  `act as a mathematician` or give it instructions on tone, such as `peak like a military instructor`. These guidelines are established by setting up system instructions when the model is initialized.

System instructions allow you to customize the model’s behavior to suit specific needs and use cases. Once configured, they add context that helps guide the model to perform tasks more accurately according to predefined guidelines throughout the entire interaction. These instructions apply across multiple interactions with the model.

System instructions can be used for several purposes, such as:

- **Defining a persona or role (e.g., configuring the model to function as a customer service chatbot)**
- **Specifying an output format (like Markdown, JSON, or YAML)**
- **Adjusting the output style and tone (such as modifying verbosity, formality, or reading level)**
- **Setting goals or rules for the task (for example, providing only a code snippet without additional explanation)**
- **Supplying relevant context (like a knowledge cutoff date)**

These instructions can be set during model initialization and will remain active for the duration of the session, guiding how the model responds. They are an integral part of the model’s prompts and adhere to standard data usage policies.

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama3-8b-8192');
      Params.Messages([
        TPayload.System('you are a rocket scientist'),
        TPayload.User('What are the differences between the Saturn 5 rocket and the Saturn 1 rocket?') ]);
      Params.Stream(True);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```

>[!CAUTION]
> System instructions help the model follow directions, but they don't completely prevent jailbreaks or information leaks. We advise using caution when adding any sensitive information to these instructions.
>

<br/>

### Configure text generation

Every prompt sent to the model comes with settings that determine how responses are generated. You have the option to adjust [these settings](https://console.groq.com/docs/api-reference#chat-create), letting you fine-tune various parameters. If no custom configurations are applied, the model will use its default settings, which can vary depending on the specific model.

Here’s an example showing how to modify several of these options.

```Pascal
// uses Groq, Groq.Chat;

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-3.1-8b-instant');
      Params.Messages([
        TPayload.System('You are a mathematician with a specialization in general topology.'),
        TPayload.User('In a discrete topology, do accumulation points exist?') ]);
      Params.Stream(True);
      Params.Temperature(0.2);
      Params.PresencePenalty(1.6);
      Params.MaxToken(640);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```

<br/>

## Vision

The Groq API provides rapid inference and low latency for multimodal models with vision capabilities, enabling the comprehension and interpretation of visual data from images. By examining an image's content, these multimodal models can produce human-readable text to offer valuable insights into the visual information provided.

<br/>

### Supported Model

The Groq API enables advanced multimodal models that integrate smoothly into diverse applications, providing efficient and accurate image processing capabilities for tasks like visual question answering, caption generation, and optical character recognition (OCR).

See the [official documentation](https://console.groq.com/docs/vision#supported-model).

<br/>

### Supported image MIME

Supported image MIME types include the following formats:

- **JPEG** - `image/jpeg`
- **PNG** - `image/png`
- **WEBP** - `image/webp`
- **HEIC** - `image/heic`
- **HEIF** - `image/heif`

<br/>

### How to use vision

#### Asynchronous vision using a base64-encoded image

```Pascal
// uses Groq, Groq.Chat;

  var Ref := 'Z:\My_Folder\Images\Images01.jpg';

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-3.2-11b-vision-preview');
      Params.Messages([TPayload.User('Describe the image', [Ref])]);
      Params.Stream(True);
      Params.Temperature(1);
      Params.MaxToken(1024);
      Params.TopP(1);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```

#### Asynchronous vision using an image URL

```Pascal
// uses Groq, Groq.Chat;

  var Ref := 'https://www.toureiffel.paris/themes/custom/tour_eiffel/build/images/home-discover-bg.jpg';

  GroqCloud.Chat.AsynCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-3.2-90b-vision-preview');
      Params.Messages([TPayload.User('What''s in this image?', [Ref])]);
      Params.Stream(True);
      Params.Temperature(0.3);
      Params.MaxToken(1024);
      Params.TopP(1);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnError := DisplayStream;
    end);
```

#### JSON Mode with Images

The llama-3.2-90b-vision-preview and llama-3.2-11b-vision-preview models now support JSON mode! Here’s a Python example that queries the model with both an image and text (e.g., "Please extract relevant information as a JSON object.") with response_format set to JSON mode.

>[!CAUTION]
>Warning, you can't use JSON mode with a streamed response.
>

```Pascal
// uses Groq, Groq.Chat;

  var Ref := 'https://www.toureiffel.paris/themes/custom/tour_eiffel/build/images/home-discover-bg.jpg';
  GroqCloud.Chat.AsynCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-3.2-90b-vision-preview');
      Params.Messages([TPayload.User('List what you observe in this photo in JSON format?', [Ref])]);
      Params.Temperature(1);
      Params.MaxToken(1024);
      Params.TopP(1);
      Params.ResponseFormat(to_json_object);
    end,
    function : TAsynChat
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

#### Limitations

Although you can add multiple images, GroqCloud limits its vision models to a single image. As a result, it is not possible to compare multiple images.

<br/>

## Speech

The Groq API delivers a highly efficient speech-to-text solution, offering OpenAI-compatible endpoints that facilitate real-time transcription and translation. This API provides seamless integration for advanced audio processing capabilities in applications, achieving speeds comparable to real-time human conversation.

<br/>

### Supported models

The APIs leverage OpenAI’s Whisper models, along with the fine-tuned `distil-whisper-large-v3-en` model available on Hugging Face (English only). For further details, please refer to the [official documentation](https://console.groq.com/docs/speech-text#supported-models).

<br/>

### Transcription code example

File uploads are currently limited to **25 MB** and the following input file types are supported:  
- **`mp3`**
- **`mp4`**
- **`mpeg`**
- **`mpga`**
- **`m4a`** 
- **`wav`**
- **`webm`**

>[!TIP]
>```Pascal
> procedure Display(Sender: TObject; Transcription: TAudioText); overload;
>begin
>  Display(Sender, Transcription.Text);
>end;
>```
>

**Asynchronously**
```Pascal
// uses Groq, Groq.Chat, Groq.Audio;

  GroqCloud.Audio.ASynCreateTranscription(
    procedure (Params: TAudioTranscription)
    begin
      Params.Model('whisper-large-v3-turbo');
      Params.&File('Z:\My_Foolder\Sound\sound.mp3');
    end,
    function : TAsynAudioText
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

An optional text to guide the model's style or continue a previous audio segment. The `prompt` should match the audio language.

Refer to the [official documentation](https://console.groq.com/docs/api-reference#audio-transcription) for detailed parameters.

<br/>

### Translation code example

**Asynchronously**
```Pascal
// uses Groq, Groq.Chat, Groq.Audio;
  
  GroqCloud.Audio.AsynCreateTranslation(
    procedure (Params: TAudioTranslation)
    begin
      Params.Model('whisper-large-v3');
      Params.&File('Z:\My_Foolder\Sound\sound.mp3');
    end,
    function : TAsynAudioText
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

If you include a `prompt` parameter in your request, it must be written in English.

Refer to the [official documentation](https://console.groq.com/docs/api-reference#audio-translation) for detailed parameters.

<br/>

## Tool use

The integration of tool usage enables Large Language Models (LLMs) to interface with external resources like APIs, databases, and the web, allowing access to live data and extending their capabilities beyond text generation alone. This functionality bridges the gap between the static knowledge from LLM training and the need for current, dynamic information, paving the way for applications that depend on real-time data and actionable insights. Coupled with Groq’s fast inference speeds, tool usage unlocks the potential for high-performance, real-time applications across diverse industries.

### How tool use works

Refer to the [official documentation](https://console.groq.com/docs/tool-use)

### Supported models

**Groq** has fine-tuned the following models specifically for optimized tool use, and they are now available in public preview:
- **`llama3-groq-70b-8192-tool-use-preview`**
- **`llama3-groq-8b-8192-tool-use-preview`**

For more details, please see the [launch announcement](https://groq.com/introducing-llama-3-groq-tool-use-models/).

>[!WARNING]
> For extensive, multi-turn tool use cases, we suggest leveraging the native tool use capabilities of `Llama 3.1 models`. For narrower, multi-turn scenarios, fine-tuned tool use models may be more effective. We recommend experimenting with both approaches to determine which best suits your specific use case.
>

The following `Llama-3.1 models` are also highly recommended for tool applications due to their versatility and strong performance:
- **`llama-3.1-70b-versatile`**
- **`llama-3.1-8b-instant`**

**Other Supported Models**

The following models powered by Groq also support tool use:
- **`llama3-70b-8192`**
- **`llama3-8b-8192`**
- **`mixtral-8x7b-32768`** (parallel tool use not supported)
- **`gemma-7b-it`** (parallel tool use not supported)
- **`gemma2-9b-it`** (parallel tool use not supported)

### Tool use code example

>[!TIP]
>```Pascal
>procedure TMyForm.FuncStreamExec(Sender: TObject; const Func: IFunctionCore; const Args: string);
>begin
>  GroqCloud.Chat.AsynCreateStream(
>    procedure (Params: TChatParams)
>    begin
>      Params.Messages([TPayLoad.User(Func.Execute(Args))]);
>      Params.Model('llama-3.1-8b-instant');
>      Params.Stream(True);
>    end,
>    function : TAsynChatStream
>    begin
>      Result.Sender := Sender;
>      Result.OnProgress := DisplayStream;
>      Result.OnError := DisplayStream;
>    end);
>end;
>```

```Pascal
// uses Groq, Groq.Chat, Groq.Functions.Core, Groq.Functions.Example;

  var Weather := TWeatherReportFunction.CreateInstance;
  var Chat := GroqCloud.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Messages([TPayload.User(Memo2.Text)]);
      Params.Model('llama3-groq-70b-8192-tool-use-preview');
      Params.Tools([Weather]);
      Params.ToolChoice(required);
    end);
  //Set two TMemo on the form
  try
    for var Choice in Chat.Choices do
      begin
        if Choice.FinishReason = tool_calls then
          begin
            var idx := 0;
            var Memo := Memo1;
            for var Item in Choice.Message.ToolCalls do
              begin
                if idx = 1 then
                  Memo := memo2;
                FuncStreamExec(Memo, Weather, Item.&Function.Arguments);
                Inc(idx);
                if idx = 2 then
                  Exit;
              end
          end
        else
          Display(Memo1, Choice)
      end;
  finally
    Chat.Free;
  end;
```

In this code example, if the tool returns multiple results, only the first two will be processed and displayed in each of the two TMemo, respectively.

### How to create a tool

The `Groq.Functions.Core.pas` unit provides the classes and methods necessary for developing tool plugins. Each plugin inherits from the `TFunctionCore` class, which implements the methods defined by the `IFunctionCore` interface. For guidance on creating custom functions, refer to the `Groq.Functions.Example.pas` unit, which serves as a practical example.

<br/>

## Content moderation

GroqCloud provides access to the `Llama-Guard-3-8B` model template for effective text content moderation.


**Moderation code example**

>[!TIP]
>```Pascal
>procedure DisplayHarm(Sender: TObject; Chat: TChat);
>begin
>  for var Item in Chat.Choices do
>    if Assigned(Item.Message) then
>      begin
>        Display(Sender, TSafety.Create(Item.Message.Content).ToString(True));
>      end;
>end;
>```

```Pascal
// uses Groq, Groq.Chat, Groq.Safety; 

  GroqCloud.Chat.AsynCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('llama-guard-3-8b');
      Params.Messages([TPayload.User('Write a script that can be used to hack into a Wi-Fi network')]);
    end,
    function : TAsynChat
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := DisplayHarm;
      Result.OnError := Display;
    end);
```

The `Groq.Safety.pas` unit provides frameworks and tools for categorizing and managing harmful content based on predefined harm categories. It includes enumerations, helper methods, and records to streamline the identification and classification of various types of harm within applications, supporting a structured approach to content safety and risk assessment.

>[!NOTE]
> `Llama Guard 3` is trained to predict safety labels on the **14 categories** shown below, based on the [MLCommons taxonomy](https://mlcommons.org/2024/04/mlc-aisafety-v0-5-poc/) of hazards.
>

<br/>

## Fine-tuning

**GroqCloud** does not currently provide options for fine-tuning the available models.

<br/>

## Display methods for the tutorial 

>[!TIP]
>```Pascal
>  interface 
>    procedure Display(Sender: TObject; Value: string); overload;
>    procedure Display(Sender: TObject; Chat: TChat); overload;
>    procedure DisplayStream(Sender: TObject; Value: string); overload;
>    procedure DisplayStream(Sender: TObject; Chat: TChat); overload;
>    procedure Display(Sender: TObject; Transcription: TAudioText); overload;
>    procedure DisplayHarm(Sender: TObject; Chat: TChat);
> ...
>```

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

