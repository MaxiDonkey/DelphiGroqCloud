unit Groq.Errors;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  REST.Json.Types;

type
  TErrorCore = class abstract
  end;

  /// <summary>
  /// See at https://console.groq.com/docs/errors
  /// </summary>
  TErrorObject = class
  private
    FType: string;
    FMessage: string;
  public
    /// <summary>
    /// A classification of the error type, such as "invalid_request_error",
    /// indicating the general category of the problem encountered.
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// A descriptive message explaining the nature of the error, intended
    /// to aid developers in diagnosing the problem.
    /// </summary>
    property Message: string read FMessage write FMessage;
  end;

  /// <summary>
  /// Error Object Structure
  /// The error object follows a specific structure, providing a clear and actionable
  /// message alongside an error type classification
  /// </summary>
  TError = class(TErrorCore)
  private
    FError: TErrorObject;
  public
    /// <summary>
    /// The primary container for error details.
    /// </summary>
    property Error: TErrorObject read FError write FError;
    destructor Destroy; override;
  end;

implementation

{ TError }

destructor TError.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

end.
