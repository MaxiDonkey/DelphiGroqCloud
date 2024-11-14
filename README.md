# Delphi GroqCloud API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20the%2011/13/2024-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Groq cloud console](#Groq-cloud-console)
    - [Get a key](Get-a-key)
    - [Settings](Settings)
- [Usage](#Usage)
    - [Asynchronous callback mode management](#Asynchronous-callback-mode-management)
    - [Groq models overview](#Groq-models-overview)
    - [Embeddings](#Embeddings)
    - [Text generation](#Text-generation)
        - [Chat completion](#Chat-completion)
             - [Synchronously text generation example](#Synchronously-text-generation-example)
             - [Asynchronously text generation example](#Asynchronously-text-generation-example)
        - [Stream chat](#Stream-chat)
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

Hosted models can be accessed directly via the GroqCloud Models API endpoint by using the model IDs listed above. To retrieve a JSON list of all available models, use the endpoint at https://api.groq.com/openai/v1/models.

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
>procedure Display(Sender: TObject; Value: string);
>begin
>  var M := Sender as TMemo;
>  M.Lines.Text := M.Text + Value + sLineBreak;
>  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>end;
>```
>
>```Pascal
>procedure Display(Sender: TObject; Chat: TChat);
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

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

