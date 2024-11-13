unit Groq.NetEncoding.Base64;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGroqCloud
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding, System.Net.Mime, System.IOUtils;

  /// <summary>
  /// Encodes the content of a file into a Base64-encoded string.
  /// </summary>
  /// <param name="FileLocation">The full path to the file that will be encoded.</param>
  /// <returns>A Base64-encoded string representing the content of the file.</returns>
  /// <exception cref="Exception">Thrown if the specified file does not exist at the provided location.</exception>
  /// <remarks>
  /// This method reads the file from the specified location and converts it to a Base64 string.
  /// It uses different encoding methods depending on the version of the RTL.
  /// For RTL version 35.0 and later, it uses <c>TNetEncoding.Base64String.Encode</c>,
  /// and for earlier versions, it uses <c>TNetEncoding.Base64.Encode</c>.
  /// </remarks>
  function EncodeBase64(FileLocation : string) : WideString;
  /// <summary>
  /// Retrieves the MIME type of the specified file based on its location.
  /// </summary>
  /// <param name="FileLocation">The full path to the file whose MIME type is to be resolved.</param>
  /// <returns>
  /// A string representing the MIME type of the file.
  /// If the file does not exist, an exception will be raised.
  /// </returns>
  /// <exception cref="Exception">
  /// Thrown if the specified file cannot be found at the provided location.
  /// </exception>
  /// <remarks>
  /// This method checks if the specified file exists and retrieves its MIME type
  /// using the <c>TMimeTypes.Default.GetFileInfo</c> method.
  /// Ensure the provided path is valid before calling this function.
  /// </remarks>
  function ResolveMimeType(const FileLocation: string): string;
  /// <summary>
  /// Retrieves the size of the specified file in bytes.
  /// </summary>
  /// <param name="FileLocation">
  /// The full path to the file whose size is to be determined.
  /// </param>
  /// <returns>
  /// An <c>Int64</c> value representing the file size in bytes.
  /// </returns>
  /// <exception cref="Exception">
  /// Raised if the specified file cannot be accessed or does not exist at the provided location.
  /// </exception>
  /// <remarks>
  /// This function verifies the existence of the specified file and, if accessible, retrieves its size
  /// using the <c>TFile.GetSize</c> method. Ensure that the file path is valid and accessible
  /// before calling this function.
  /// </remarks>
  function FileSize(const FileLocation: string): Int64;
  /// <summary>
  /// Provides the image data as a Base64-encoded string with a MIME type or as a direct URL.
  /// </summary>
  /// <param name="FileLocation">
  /// The full path to the image file on the local filesystem or a URL.
  /// </param>
  /// <returns>
  /// A string representing the image data:
  /// <para>
  /// - If <paramref name="FileLocation"/> is a local file path, it returns a data URI with a MIME type and Base64-encoded content.
  /// </para>
  /// <para>
  /// - If <paramref name="FileLocation"/> is a URL (starting with "http"), it returns the URL as-is.
  /// </para>
  /// </returns>
  /// <exception cref="Exception">
  /// Raised if the file does not exist at the provided local file path.
  /// </exception>
  /// <remarks>
  /// This function checks if <paramref name="FileLocation"/> is a URL by verifying if it starts with "http".
  /// If it is a URL, it returns it directly as the output.
  /// For local files, it verifies the file's existence, retrieves the MIME type, and encodes the content in Base64 format
  /// to create a data URI for embedding purposes. This data URI can then be used directly in HTML or other contexts where
  /// embedded image data is required.
  /// </remarks>
  function ImageDataProvider(FileLocation : string) : WideString;

implementation

function EncodeBase64(FileLocation : string): WideString;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found : %s', [FileLocation]);

  var Stream := TMemoryStream.Create;
  var StreamOutput := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(FileLocation);
    Stream.Position := 0;
    {$IF RTLVersion >= 35.0}
    TNetEncoding.Base64String.Encode(Stream, StreamOutput);
    {$ELSE}
    TNetEncoding.Base64.Encode(Stream, StreamOutput);
    {$ENDIF}
    Result := StreamOutput.DataString;
  finally
    Stream.Free;
    StreamOutput.Free;
  end;
end;

function ResolveMimeType(const FileLocation: string): string;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found: %s', [FileLocation]);

  var LKind: TMimeTypes.TKind;
  TMimeTypes.Default.GetFileInfo(FileLocation, Result, LKind);
end;

function FileSize(const FileLocation: string): Int64;
begin
  try
    FileSize := TFile.GetSize(FileLocation);
  except
    raise;
  end;
end;

function ImageDataProvider(FileLocation : string) : WideString;
begin
  if FileLocation.ToLower.StartsWith('http') then
    Result := FileLocation
  else
  if FileExists(FileLocation) then
    Result := Format('data:%s;base64,%s', [ResolveMimeType(FileLocation), EncodeBase64(FileLocation)])
  else
    raise Exception.CreateFmt('File not found : %s', [FileLocation]);
end;

end.
