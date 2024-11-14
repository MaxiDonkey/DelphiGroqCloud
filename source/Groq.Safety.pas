unit Groq.Safety;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes;

type
  /// <summary>
  /// Harm Taxonomy and Policy
  /// </summary>
  THarmCategory = (
    /// <summary>
    /// Non-Violent Crimes
    /// </summary>
    S1,
    /// <summary>
    /// Sex-Related Crimes
    /// </summary>
    S2,
    /// <summary>
    /// Child Sexual Exploitation
    /// </summary>
    S3,
    /// <summary>
    /// Defamation
    /// </summary>
    S4,
    /// <summary>
    /// Defamation
    /// </summary>
    S5,
    /// <summary>
    /// Specialized Advice
    /// </summary>
    S6,
    /// <summary>
    /// Privacy
    /// </summary>
    S7,
    /// <summary>
    /// Intellectual Property
    /// </summary>
    S8,
    /// <summary>
    /// Indiscriminate Weapons
    /// </summary>
    S9,
    /// <summary>
    /// Hate
    /// </summary>
    S10,
    /// <summary>
    /// Suicide & Self-Harm
    /// </summary>
    S11,
    /// <summary>
    /// Sexual Content
    /// </summary>
    S12,
    /// <summary>
    /// Elections
    /// </summary>
    S13,
    /// <summary>
    /// Code Interpreter Abuse
    /// </summary>
    S14
  );

  /// <summary>
  /// Helper record for the <c>THarmCategory</c> enumeration, providing utility methods for converting
  /// between <c>THarmCategory</c> values and their string representations.
  /// </summary>
  THarmCategoryHelper = record helper for THarmCategory
    /// <summary>
    /// Converts the current <c>THarmCategory</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>THarmCategory</c> value.
    /// </returns>
    function ToString(Full: Boolean = False): string;
    /// <summary>
    /// Converts a string representation of a <c>THarmCategory</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>THarmCategory</c>.
    /// </param>
    /// <returns>
    /// The <c>THarmCategory</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): THarmCategory; static;
  end;

  /// <summary>
  /// Represents safety information by categorizing harmful content based on predefined harm categories.
  /// </summary>
  /// <remarks>
  /// The <c>TSafety</c> record maintains two arrays. The <c>Harm</c> array contains short string representations of harm categories,
  /// while the <c>HarmFull</c> array contains full string descriptions of harm categories.
  /// It provides methods to create an instance from a comma or newline-separated string of category identifiers
  /// and to convert the safety information back to a string format.
  /// </remarks>
  TSafety = record
  private
    FHarm: TArray<string>;
    FHarmFull: TArray<string>;
  public
    /// <summary>
    /// Initializes a new instance of the <c>TSafety</c> record based on a string of category identifiers.
    /// </summary>
    /// <param name="Value">
    /// A string containing comma or newline-separated identifiers corresponding to <c>THarmCategory</c> values (e.g., "S1,S2,S3").
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record populated with the specified harm categories.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown when an invalid harm category identifier is encountered.
    /// </exception>
    class function Create(const Value: string): TSafety; static;
    /// <summary>
    /// Converts the safety information to its string representation.
    /// </summary>
    /// <param name="Full">
    /// A boolean value indicating whether to include full descriptions of harm categories.
    /// Set to <c>False</c> to include only short identifiers.
    /// </param>
    /// <returns>
    /// A string representing the safety information. If <c>Full</c> is <c>True</c>, full descriptions are returned;
    /// otherwise, only short identifiers are included. If no harm categories are present, the string "Safe" is returned.
    /// </returns>
    function ToString(Full: Boolean = False): string;
    /// <summary>
    /// Gets or sets the array of short string representations of harm categories.
    /// </summary>
    /// <value>
    /// An array of strings where each string is a short identifier corresponding to a <c>THarmCategory</c> value.
    /// </value>
    property Harm: TArray<string> read FHarm write FHarm;
    /// <summary>
    /// Gets or sets the array of full string descriptions of harm categories.
    /// </summary>
    /// <value>
    /// An array of strings where each string is a full description corresponding to a <c>THarmCategory</c> value.
    /// </value>
    property HarmFull: TArray<string> read FHarmFull write FHarmFull;
  end;

implementation

uses
  System.StrUtils;

type
  THarmCategoryContent = class
  private
    FHarmContent: TArray<string>;
  public
    function GetS1Content: string;
    function GetS2Content: string;
    function GetS3Content: string;
    function GetS4Content: string;
    function GetS5Content: string;
    function GetS6Content: string;
    function GetS7Content: string;
    function GetS8Content: string;
    function GetS9Content: string;
    function GetS10Content: string;
    function GetS11Content: string;
    function GetS12Content: string;
    function GetS13Content: string;
    function GetS14Content: string;
    function Add(const Value: string): THarmCategoryContent;
    constructor Create;
  end;

var
  HarmCategoryContent: THarmCategoryContent;

{ THarmCategoryContent }

function THarmCategoryContent.Add(const Value: string): THarmCategoryContent;
begin
  FHarmContent := FHarmContent + [Value];
  Result := Self;
end;

constructor THarmCategoryContent.Create;
begin
  inherited Create;
  Self.Add(GetS1Content)
    .Add(GetS2Content)
    .Add(GetS3Content)
    .Add(GetS4Content)
    .Add(GetS5Content)
    .Add(GetS6Content)
    .Add(GetS7Content)
    .Add(GetS8Content)
    .Add(GetS9Content)
    .Add(GetS10Content)
    .Add(GetS11Content)
    .Add(GetS12Content)
    .Add(GetS13Content)
    .Add(GetS14Content);
end;

function THarmCategoryContent.GetS10Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Hate');
    WriteLine('Responses that demean or dehumanize people on the basis of their sensitive, personal characteristics (i.e., race, color, ethnicity, national origin, disability, religious affiliation, caste, sexual orientation, sex, gender identity, and/or serious disease)');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS11Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Suicide & Self-Harm');
    WriteLine('Responses that enable, encourage, or endorse acts of intentional self-harm, including:');
    WriteLine(' - suicide');
    WriteLine(' - self-injury (ex: cutting)');
    WriteLine(' - disordered eating');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS12Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Sexual Content');
    WriteLine('Responses that contain erotica');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS13Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Elections');
    WriteLine('Responses that contain factually incorrect information about electoral systems and processes, including in the time, place, or manner of voting in civic elections');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS14Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Code Interpreter Abuse');
    WriteLine('Responses that seek to abuse code interpreters, including those that enable denial of service attacks, container escapes or privilege escalation exploits');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS1Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Violent Crimes');
    WriteLine('Responses that enable, encourage, or endorse the commission of violent crimes, including:');
    WriteLine(' - unlawful violence toward people (ex: terrorism, genocide, murder, hate-motivated violent crimes, child abuse, assault, battery, kidnapping)');
    WriteLine(' - unlawful violence toward animals (ex: animal abuse)');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS2Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Non-Violent Crimes');
    WriteLine('Responses that enable, encourage, or endorse the commission of non-violent crimes, including:');
    WriteLine(' - personal crimes (ex: labor trafficking, threats, intimidation, hate-motivated non-violent crimes)');
    WriteLine(' - financial crimes (ex: fraud, scams, money laundering)');
    WriteLine(' - property crimes (ex: theft, arson, vandalism)');
    WriteLine(' - drug crimes (ex: creating or using regulated substances)');
    WriteLine(' - weapons crimes (ex: producing unlicensed firearms)');
    WriteLine(' - cyber crimes (ex: hacking)');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS3Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Sex-Related Crimes');
    WriteLine('Responses that enable, encourage, or endorse the commission of sex-related crimes, including');
    WriteLine(' - sex trafficking');
    WriteLine(' - sexual assault (ex: rape)');
    WriteLine(' - sexual harassment (physical, verbal, or visual)');
    WriteLine(' - prostitution');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS4Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Child Sexual Exploitation');
    WriteLine(' Responses that contain, describe, enable, encourage, or endorse the sexual abuse of children');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS5Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Defamation');
    WriteLine('Responses that are both verifiably false and likely to injure a living person’s reputation');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS6Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Specialized Advice');
    WriteLine('Responses that contain specialized financial, medical, or legal advice, or that indicate dangerous activities or objects are safe');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS7Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Privacy');
    WriteLine('Responses that contain sensitive, nonpublic personal information that could undermine someone''s physical, digital, or financial security');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS8Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Intellectual Property');
    WriteLine('Responses that may violate the intellectual property rights of any third party');
    Result := ToString;
  finally
    Free;
  end;
end;

function THarmCategoryContent.GetS9Content: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('Indiscriminate Weapons');
    WriteLine('Responses that enable, encourage, or endorse the creation of indiscriminate weapons, including:');
    WriteLine(' - chemical weapons (ex: nerve gas)');
    WriteLine(' - biological weapons (ex: anthrax)');
    WriteLine(' - radiological weapons (ex: cobalt bombs)');
    WriteLine(' - nuclear weapons (ex: fission bombs)');
    WriteLine(' - high-yield explosive weapons (ex: cluster munitions)');
    Result := ToString;
  finally
    Free;
  end;
end;

{ THarmCategoryHelper }

class function THarmCategoryHelper.Create(const Value: string): THarmCategory;
begin
  var index := IndexStr(Value.ToLower, [
    's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 's11', 's12', 's13', 's14']);
  if index = -1 then
    raise Exception.Create('Harm category type unkonwn');
  Result := THarmCategory(index);
end;

function THarmCategoryHelper.ToString(Full: Boolean): string;
begin
  try
    Result := HarmCategoryContent.FHarmContent[Integer(Self)];
    if not Full and not Result.IsEmpty then
      Result := Result.Split([#10])[0];
  except
    raise;
  end;
end;

{ TSafety }

class function TSafety.Create(const Value: string): TSafety;
begin
  for var Item in Value.Split([#10, ',']) do
    begin
      try
        Result.FHarm := Result.FHarm + [THarmCategory.Create(Item).ToString];
        Result.FHarmFull := Result.FHarmFull + [THarmCategory.Create(Item).ToString(True)];
      except
      end;
    end;
end;

function TSafety.ToString(Full: Boolean): string;
begin
  if Length(FHarm) = 0 then
    Exit('Safe');
  case Full of
    False:
      Result := Result.Join(#10, FHarm);
    else
      Result := Result.Join(#10, FHarmFull);
  end;
end;

initialization
  HarmCategoryContent := THarmCategoryContent.Create;
finalization
  HarmCategoryContent.Free;
end.

