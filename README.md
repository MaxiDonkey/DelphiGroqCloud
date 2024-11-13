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

<br/>

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

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

