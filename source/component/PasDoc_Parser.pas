{ @abstract(provides all the parsing functionality of pasdoc)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @cvs($Date$)

  Parsing implements most of the functionality of the pasdoc program.

  It provides the @link(TParser) object, which scans the command line parameters
  for file names and switches and then starts collecting information from those
  files, issueing warnings to standard out if necessary. }

unit PasDoc_Parser;

interface

uses
  Classes,
  PasDoc_Types,
  PasDoc_Items,
  PasDoc_Scanner,
  PasDoc_Tokenizer,
  StringVector;

type
  { Parser class that will process a complete unit file and all of its
    include files, regarding directives.
    When creating this object constructor @link(Create) takes as an argument 
    an input stream and a list of directives.
    Parsing work is done by calling @link(ParseUnit) method.
    If no errors appear, should return a @link(TPasUnit) object with
    all information on the unit. 
    
    Things that parser inits in items it returns:
    
    - Of every TPasItem :
      Name, RawDescription, State, IsDeprecated, IsPlatformSpecific, 
      IsLibrarySpecific. 
      
      Note to IsDeprecated: parser inits it basing on hint directive
      "deprecated" presence in source file; it doesn't handle the fact 
      that @@deprecated tag may be specified inside RawDescription.
      
      Note to RawDescription: parser inits them from user's comments
      that preceded given item in source file.
      It doesn't handle the fact that @@member and @@value tags
      may also assign RawDescription for some item.
      
    - Of TPasCio: Ancestors, Fields, Methods, Properties, MyType.
    - Of TPasEnum: Members.
    - Of TPasMethod: What, FullDeclararation.
    - Of TPasVarConst: FullDeclaration.
    - Of TPasProperty: IndexDecl, FullDeclaration.
      PropType (only if was specified in property declaration).
      It was intended that parser will also set Default,
      NoDefault, StoredId, DefaultId, Reader, Writer attributes, 
      but it's still not implemented.
    - Of TPasUnit; UsesUnits, Types, Variables, CIOs, Constants,
      FuncsProcs.
    
    It doesn't init other values. 
    E.g. AbstractDescription or DetailedDescription of TPasItem 
    should be inited while expanding this item's tags.
    E.g. SourceFileDate and SourceFileName of TPasUnit must
    be set by other means.

    TODO -- for now it's not really consistent how the errors in parsing
    are reported. Some errors cause @link(ParseUnit) and other ParseXxx
    methods to exit with false, some errors cause raising an exception. }
  TParser = class
  private
    procedure SetCommentMarkers(const Value: TStringList);
    
    { Skips all whitespace and comments and while it sees some hint directive
      (platform, library, deprecated) it consumes it, sets appropriate
      property of Item (IsPlatformSpecific, IsLibrarySpecific or IsDeprecated)
      to true and goes further.
      
      Stops when Scanner.PeekToken returns some non-whitespace non-comment 
      non-hint-directive token. }
    procedure ParseHintDirectives(Item: TPasItem);
  protected
    { Last comment found in input or nil if no comment available.
    Will be modified by @link(GetLastComment). }
    LastCommentToken: TToken;
    { The underlying scanner object. }
    Scanner: TScanner;
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FCommentMarkers: TStringList;
    FMarkersOptional: boolean;
    FClassMembers: TAccessibilities;

    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    { Clears the last comment token. Should be issued soon after
      @link(GetLastComment) was called with @code(ClearLastComment) set to
      @False. }
    procedure ClearLastComment;
    { Returns the comment (or other data) in t. If t = nil, the
      Result will be an empty string. If FreeToken is @True, @Name frees t.
      Otherwise, t stays untouched for further use. if present, comment
      markers are removed from the beginning and end of the data.}
    function ExtractComment(const FreeToken: Boolean;
      var t: TToken): string;
    { Returns the last comment that was found in input. If there was none, the
      Result will be an empty string. If ClearLastComment is @True, @Name clears
      the last comment. Otherwise, it stays untouched for further use. }
    function GetLastComment(const AClearLastComment: Boolean): String;
    { Get next token T from scanner that is neither whitespace nor comment.
      Return true on success. }
    function GetNextNonWCToken(var t: TToken; var LCollector: string): Boolean; overload;
    function GetNextNonWCToken(var t: TToken): Boolean; overload;
    function ParseArguments(var a: string): Boolean;
    { Parses a constructor, a destructor, a function or a procedure.
      Resulting @link(TPasMethod) item will be returned in M.
      CS may contain the 'class' keyword - its exact spelling is taken from
      this variable.
      CDFP contains the keyword constructor, destructor, function or procedure
      in the exact spelling as it was found in input.
      Key contains one of the KEY_xxx constants for the What field of the
      resulting method object.
      D may contain a description or nil. }
    function ParseCDFP(var M: TPasMethod; CS, CDFPS: string; Key: TKeyWord;
      d: string; const NeedName: boolean): Boolean;
    { Parses a class, an interface or an object.
      U is the unit this item will be added to on success.
      N is the name of this item.
      CIOType describes if item is class, interface or object.
      D may contain a description or nil. }
    function ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
      TCIOType; d: string; const IsInRecordCase: boolean): Boolean;
    { }
    function ParseRecordCase(const R: TPasCio; const SubCase: boolean): boolean;
    function ParseConstant(const U: TPasUnit; t: TToken): Boolean;
    function ParseInterfaceSection(const U: TPasUnit): Boolean;
    function ParseProperty(var p: TPasProperty): Boolean;
    function ParseType(const U: TPasUnit; var t: TToken): Boolean;
    
    function ParseEnum(var p: TPasEnum): boolean;

    function ParseUses(const U: TPasUnit): Boolean;
    function ParseVariables(const U: TPasUnit; var t: TToken): Boolean;
    function SkipDeclaration(const VC: TPasVarConst): Boolean;
    
    { Reads tokens and throws them away as long as they are either whitespace
      or comments. 

      @returns(true if a non-white token is found (then you know that 
      Scanner.PeekToken or Scanner.GetToken will return with true) 
      or false if stream ended.) }
    function SkipWhitespaceAndComments(var LCollector: string): Boolean; overload;
    
    { Same thing as SkipWhitespaceAndComments(Dummy) }
    function SkipWhitespaceAndComments: Boolean; overload;
  public
    { Create a parser, initialize the scanner with input stream S.
      All strings in SD are defined compiler directives. }
    constructor Create(
      const InputStream: TStream;
      const Directives: TStringVector;
      const IncludeFilePaths: TStringVector;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName: string);
    { Release all dynamically allocated memory. }
    destructor Destroy; override;
    function ParseUnit(var U: TPasUnit): Boolean;

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkersOptional: boolean read fMarkersOptional write fMarkersOptional;
    property ClassMembers: TAccessibilities read FClassMembers write FClassMembers;
  end;

implementation

uses
  SysUtils,
  Utils;

{ ---------------------------------------------------------------------------- }
{ TParser }
{ ---------------------------------------------------------------------------- }

procedure TParser.ClearLastComment;
begin
  if Assigned(LastCommentToken) then begin
    LastCommentToken.Free;
    LastCommentToken := nil;
  end;
end;

constructor TParser.Create(
  const InputStream: TStream;
  const Directives: TStringVector;
  const IncludeFilePaths: TStringVector;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName: string);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  Scanner := TScanner.Create(InputStream, OnMessageEvent, VerbosityLevel, AStreamName);
  Scanner.AddDirectives(Directives);
  Scanner.IncludeFilePaths := IncludeFilePaths;
  FCommentMarkers := TStringlist.Create;
end;

{ ---------------------------------------------------------------------------- }

destructor TParser.Destroy;
begin
  FCommentMarkers.Free;
  Scanner.Free;
  LastCommentToken.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity <= FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }
function TParser.ExtractComment(const FreeToken: Boolean; var t: TToken): string;
var
  l: Integer;
  i: integer;
  Marker: string;
begin
  if Assigned(t) then begin
    Result := t.Data;
    if FreeToken then begin
      t.Free;
      t := nil;
    end;

    { remove comment characters here }
    l := Length(Result);

      if l > 0 then
        case Result[1] of
          '{':
            begin
              Delete(Result, 1, 1);
              Dec(l);

              if (l > 0) and (Result[l] = '}') then
                Delete(Result, Length(Result), 1);
            end;

          '/':
            if (l > 1) and (Result[2] = '/') then
              begin
                Delete(Result, 1, 2);
              end;

          '(':
            if (l > 1) and (Result[2] = '*') then
              begin
                Delete(Result, 1, 2);
                Dec(l, 2);
                if (l > 1) and (Result[l - 1] = '*') and (Result[l] = ')') then
                  Delete(Result, l - 1, 2);
              end;
        end;
  end
  else
    Result := '';

  if (Result = '') or (CommentMarkers.Count = 0) then
    exit;

  for i := 0 to CommentMarkers.Count-1 do begin
    Marker := CommentMarkers[i];
    l := Length(Marker);
    if (Length(Result) >= l) and (Copy(Result, 1, l) = Marker) then begin
      Delete(Result, 1, l);
      exit;
    end;
  end;
  if not MarkersOptional then
    Result := '';
end;

function TParser.GetLastComment(const AClearLastComment: Boolean): string;
begin
  result := ExtractComment(AClearLastComment, LastCommentToken);
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetNextNonWCToken(var t: TToken; var LCollector: string): Boolean;
begin
  Assert(t=nil);
  if SkipWhitespaceAndComments(LCollector) then begin
    Result := Scanner.GetToken(t)
  end else begin
    Result := False;
  end;
end;

function TParser.GetNextNonWCToken(var t: TToken): Boolean;
var
  LDummy: string;
begin
  Result := GetNextNonWCToken(t, LDummy);
end;

function TParser.ParseArguments(var a: string): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  Finished := False;
  a := '';
  repeat
    if Scanner.GetToken(t) then begin
      if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
        SYM_RIGHT_PARENTHESIS) then begin
        Finished := True
      end else begin
        if (t.MyType = TOK_SYMBOL) and
          ((t.Info.SymbolType = SYM_COLON) or
          (t.Info.SymbolType = SYM_COMMA) or
          (t.Info.SymbolType = SYM_SEMICOLON)) then begin
          if (Length(a) > 0) and (a[Length(a)] = ' ') then
            SetLength(a, Length(a) - 1);
          a := a + t.Data;
        end
        else if (t.MyType = TOK_WHITESPACE) then begin
          if (Length(a) > 0) and (a[Length(a)] <> ' ') then a := a + ' ';
        end
        else if t.MyType in [TOK_COMMENT_PAS, TOK_COMMENT_CSTYLE, TOK_COMMENT_EXT, TOK_DIRECTIVE] then
          begin
              { ignore }
        end
        else { otherwise copy }
          a := a + t.Data;
      end;
      FreeAndNil(t);
    end;
  until Finished;
  ParseArguments := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseCDFP(var M: TPasMethod; CS, CDFPS: string; Key:
  TKeyword; d: string; const NeedName: boolean): Boolean;
var
  IsSemicolon: Boolean;
  t: TToken;
  level: Integer;
  InvalidType: boolean;
begin
  Result := False;
  M := TPasMethod.Create;
  M.RawDescription := d;
  t := nil;
  case Key of
    KEY_CONSTRUCTOR:
      M.What := METHOD_CONSTRUCTOR;
    KEY_DESTRUCTOR:
      M.What := METHOD_DESTRUCTOR;
    KEY_FUNCTION, KEY_PROCEDURE, KEY_OPERATOR:
      M.What := METHOD_FUNCTION_PROCEDURE;
  else
    DoError('FATAL ERROR: CDFP got invalid key.', [], 1);
  end;

  { next non-wc token must be the name }
  if NeedName then begin
    if (not GetNextNonWCToken(t)) then begin
      M.Free;
      DoError('Could not get next non white space token', [], 0);
    end;
    if (Key = KEY_OPERATOR) then
    begin
      InvalidType := (t.MyType <> TOK_SYMBOL);
    end
    else
    begin
      InvalidType := (t.MyType <> TOK_IDENTIFIER);
    end;

    if InvalidType then begin
      M.Free;
      FreeAndNil(t);
      DoError('Could not get next identifier', [], 0);
    end;
    if (Length(CS) > 0) then CS := CS + ' ';
    M.Name := t.Data;
    DoMessage(5, mtInformation, 'Parsing %s %s', [CDFPS, M.Name]);
    M.FullDeclaration := CS + CDFPS + ' ' + M.Name;
    FreeAndNil(t);
  end;

  { copy tokens until first semicolon with parenthesis level zero }
  level := 0;
  repeat
    if (not Scanner.GetToken(t)) then begin
      M.Free;
      DoError('Could not get next token', [], 0);
    end;
    case t.MyType of 
      TOK_COMMENT_PAS, TOK_COMMENT_CSTYLE, TOK_COMMENT_EXT: { ignore };
      TOK_WHITESPACE:
        begin
          { add exactly *one space* at the end of M.FullDeclaration }
          if Length(M.FullDeclaration) > 0 then begin
            if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
              M.FullDeclaration := M.FullDeclaration + ' ';
          end;
        end
      else
        M.FullDeclaration := M.FullDeclaration + t.Data;
    end;
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_LEFT_PARENTHESIS)
      then Inc(level);
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_RIGHT_PARENTHESIS)
      then Dec(level);
    IsSemicolon := (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
      SYM_SEMICOLON);
    FreeAndNil(t);
  until IsSemicolon and (Level = 0);
  
  { first get non-WC token - if it is not an identifier in SD_SET put it back
    into stream and leave; otherwise copy tokens until semicolon }
  repeat
    FreeAndNil(t);
    if (not GetNextNonWCToken(t)) then begin
      M.Free;
      DoError('Could not get next non white space token', [], 0);
    end;
    
    if t.MyType = TOK_IDENTIFIER then
    begin
      case t.Info.StandardDirective of
        SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
          SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE, SD_INLINE,
          SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
          SD_VARARGS:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);

            if not GetNextNonWCToken(t) then begin
              M.Free;
              Exit;
            end;
          end;

        { * External declarations might be followed by a string constant.
          * Messages are followed by an integer constant between 1 and 49151 which
            specifies the message ID. }
        SD_EXTERNAL, SD_MESSAGE, SD_NAME:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);

            // Keep on reading up to the next semicolon or declaration
            repeat
              if not GetNextNonWCToken(t) then begin
                M.Free;
                DoError('Could not get next non white space token', [], 0);
              end;

              if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_SEMICOLON) then begin
                Break
              end else begin
                if t.MyType = TOK_IDENTIFIER then 
                  case t.Info.StandardDirective of
                    SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT, SD_EXTERNAL,
                      SD_FAR, SD_FORWARD, SD_NAME, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
                      SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL:
                      begin
                        // FScanner.UnGetToken(t);
                        Break;
                      end;
                  end;

                M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
                FreeAndNil(t);
              end;
            until False;
          end;
        SD_DEPRECATED: begin
          M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
          M.IsDeprecated := True;
          FreeAndNil(t);
          GetNextNonWCToken(t);
        end;
        SD_PLATFORM: begin
          M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
          M.IsPlatformSpecific := True;
          FreeAndNil(t);
          GetNextNonWCToken(t);
        end;
      else
        begin
          Scanner.UnGetToken(t);
          Break;
        end;
      end; // case
    end else
    if t.MyType = TOK_KEYWORD then 
    begin
      case t.Info.KeyWord of 
        KEY_INLINE: begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);
            GetNextNonWCToken(t);
          end;
        KEY_LIBRARY:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            M.IsLibrarySpecific := True;
            FreeAndNil(t);
            GetNextNonWCToken(t);
          end;
        else 
          begin
            Scanner.UnGetToken(t);
            Break;        
          end;
      end;
    end else
    begin
      Scanner.UnGetToken(t);
      Break;
    end;

    { Apparently, the Delphi compiler does NOT enforce that
      directives must be separated and be terminated by a semicolon,
      even though Delphi help consistently uses them consistently.
      However, we take the compiler as a reference and try to mimic its behaviour. }
    { Is current token a semicolon? }
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_SEMICOLON) then
      M.FullDeclaration := M.FullDeclaration + ';'
    else begin
      M.FullDeclaration := M.FullDeclaration + ' ';
      Scanner.UnGetToken(t);
    end;

  until False;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
  TCIOType; d: string; const IsInRecordCase: boolean): Boolean;

  { Parse field declaration, i.e. something like
      NAME1, NAME2, ... : TYPE;
    If AddToFields then adds parsed fields to i.Fields.
    State of created fields is set to given State parameter. }
  function ParseCIOField(var t: TToken; 
    i: TPasCio; AddToFields: boolean; State: TAccessibility): boolean;
  var
    FirstFieldLoop: Boolean;
    FieldsAdded: TPasItems;
    f: TPasItem;
    DummyField: TPasVarConst;
    M: TPasMethod;
  begin
    Result := false;
    
    FieldsAdded := TPasItems.Create(false);
    try
      FirstFieldLoop := True;
      repeat
        f := TPasItem.Create;
        if FirstFieldLoop then begin
          f.Name := t.Data;
          FirstFieldLoop := False;
        end else begin
          if not GetNextNonWCToken(t) then Exit;
          if t.MyType <> TOK_IDENTIFIER then begin
            DoError('%s: Identifier expected.', [Scanner.GetStreamInfo], 0);
          end;
          f.Name := t.Data;
        end;

        f.State := State;
        f.RawDescription := GetLastComment(False);
        if AddToFields then begin
          i.Fields.Add(f);
          FieldsAdded.Add(f);
        end else begin
          f.Free;
        end;
        FreeAndNil(t);

        if not GetNextNonWCToken(t) then Exit;

        if t.MyType = TOK_SYMBOL then begin
          case t.Info.SymbolType of
            SYM_COMMA:
              begin
                FreeAndNil(t);
                Continue;
              end;
            SYM_COLON:
              begin
                Break;
              end;
          end;
        end;
        // We reach here only if token is not a comma or a colon.
        FreeAndNil(t);
        DoError('%s: Expected comma or colon in Field declaration.', [Scanner.GetStreamInfo], 0);
      until False;
      ClearLastComment;
      FreeAndNil(t);
      GetNextNonWCToken(t);
      
      { This is the place where DummyField and FieldsAdded are useful:
        we will try to parse the rest of fields declaration to DummyField
        properties. Then we will copy some properties from DummyField
        to all items in FieldsAdded. This way we e.g. parse hint directives
        (like deprecated, platform etc.) into DummyField properties
        and then copy them into FieldsAdded items. }
      DummyField := TPasVarConst.Create;
      try
        if (t.MyType = TOK_KEYWORD) then begin
          case t.Info.KeyWord of
            KEY_FUNCTION, KEY_PROCEDURE: begin
                if ParseCDFP(M,'','',t.Info.KeyWord,d,false) then begin
                  M.Free;
                  FreeAndNil(t);
                end else begin
                  FreeAndNil(t);
                  exit;
                end;
              end;
            KEY_RECORD: begin
              ParseCIO(nil, '', CIO_RECORD, '', False);
              end;
            KEY_PACKED: begin
                FreeAndNil(t);
                GetNextNonWCToken(t);
                if (t.MyType = TOK_KEYWORD) and (t.Info.KeyWord = KEY_RECORD) then begin
                  ParseCIO(nil, '', CIO_PACKEDRECORD, '', False);
                end else begin
                  SkipDeclaration(DummyField);
                end;
              end;
            else begin
                FreeAndNil(t);
                if not SkipDeclaration(DummyField) then begin
                  exit;
                end;
              end;
            end;
        end else begin
          FreeAndNil(t);
          if not SkipDeclaration(DummyField) then begin
            Exit;
          end;
        end;
        
        FieldsAdded.SetIsDeprecated(DummyField.IsDeprecated);
        FieldsAdded.SetIsPlatformSpecific(DummyField.IsPlatformSpecific);
        FieldsAdded.SetIsLibrarySpecific(DummyField.IsLibrarySpecific);
      finally DummyField.Free end;
    finally FieldsAdded.Free end;
    
    Result := true;
  end;
  
var
  CS: string;
  CSFound: Boolean;
  Finished: Boolean;
  i: TPasCio;
  M: TPasMethod;
  p: TPasProperty;
  s: string;
  State: TAccessibility;
  t: TToken;
begin
  t := nil;
  Result := False;
  DoMessage(5, mtInformation, 'Parsing class/interface/object "%s"', [CioName]);
  i := nil;
  try
    if not GetNextNonWCToken(t) then Exit;

    { Test for forward class definition here:
        class MyClass = class;
      with no ancestor or class members listed after the word class. }
    if t.IsSymbol(SYM_SEMICOLON) then begin
      Result := True; // No error, continue the parsing.
      t.Free;
      Exit;
    end;

    i := TPasCio.Create;
    i.Name := CioName;
    i.RawDescription := d;
    i.MyType := CIOType;
    { get all ancestors; remember, this could look like
      TNewClass = class ( Classes.TClass, MyClasses.TFunkyClass, MoreClasses.YAC) ... end;
      All class ancestors are supposed to be included in the docs!
    }
    { TODO -otwm :
      That's not quite true since multiple inheritance is not supported by 
       Delphi/Kylix or FPC. Every entry but the first must be an interface. }
    if t.IsSymbol(SYM_LEFT_PARENTHESIS) then begin
        { optional ancestor introduced by ( }
      FreeAndNil(t);
      Finished := False;
      { outer repeat loop: one ancestor per pass }
      repeat
        FreeAndNil(t);
        if not GetNextNonWCToken(t) then Exit;
        if t.MyType = TOK_IDENTIFIER then begin { an ancestor }
          s := t.Data;
              { inner repeat loop: one part of the ancestor per name }
          repeat
            FreeAndNil(t);
            if not Scanner.GetToken(t) then begin
              Exit;
            end;
            if not t.IsSymbol(SYM_PERIOD) then begin
              Scanner.UnGetToken(t);
              t := nil;
              Break; { leave inner repeat loop }
            end;
            FreeAndNil(t);
            s := s + '.';
            if not Scanner.GetToken(t) or (t.MyType <> TOK_IDENTIFIER) then
              DoError('%s: expected class, object or interface in ancestor declaration.', [Scanner.GetStreamInfo], 0);

            s := s + t.Data;
          until False;
          i.Ancestors.Add(s);
        end else begin
          if (t.IsSymbol(SYM_COMMA)) then
              { comma, separating two ancestors } begin
            FreeAndNil(t);
          end else begin
            Finished := t.IsSymbol(SYM_RIGHT_PARENTHESIS);
            FreeAndNil(t);
            if not Finished then
              DoError('%s: Error - ")" expected.', [Scanner.GetStreamInfo], 0);
          end;
        end;
      until Finished;
    end else begin
      Scanner.UnGetToken(t);
      case i.MyType of
        CIO_CLASS: begin
          if not SameText(i.Name, 'tobject') then begin
            i.Ancestors.Add('TObject');
          end;
        end;
        CIO_SPINTERFACE: begin
          if not SameText(i.Name, 'idispinterface') then begin
            i.Ancestors.Add('IDispInterface');
          end;
        end;
        CIO_INTERFACE: begin
          if not SameText(i.Name, 'iinterface') then begin
            i.Ancestors.Add('IInterface');
          end;
        end;
        CIO_OBJECT: begin
          if not SameText(i.Name, 'tobject') then begin
            i.Ancestors.Add('TObject');
          end;
        end;
      end;
    end;
    GetNextNonWCToken(t);
    if (t.IsSymbol(SYM_LEFT_BRACKET)) then begin
      FreeAndNil(t);
      { for the time being, we throw away the ID itself }
      if (not GetNextNonWCToken(t)) then begin
        Exit;
      end;
      if (t.MyType <> TOK_STRING) and (t.MyType <> TOK_IDENTIFIER) then
        DoError('%s: Error - literal String or identifier as interface ID expected.', [Scanner.GetStreamInfo], 0);
      FreeAndNil(t);
      if not GetNextNonWCToken(t) then begin
        Exit;
      end;
      if not t.IsSymbol(SYM_RIGHT_BRACKET) then
        DoError('%s: Error - "]" expected.', [Scanner.GetStreamInfo], 0);
    end else begin
      Scanner.UnGetToken(t);
    end;

    { now collect methods, fields and properties }
    CS := '';

    (* Members at the beginning of a class declaration that don<92>t have a specified
       visibility are by default published, provided the class is compiled in the
       $M+ state or is derived from a class compiled in the $M+ state;
       otherwise, such members are public.

       How do we resolve the inherited classes' $M+ state? *)
    if Scanner.SwitchOptions['M'] then begin
      State := STATE_PUBLISHED;
    end else begin
      State := STATE_PUBLIC;
    end;

    Finished := False;
    repeat
      CSFound := False;
      FreeAndNil(t);
      if not GetNextNonWCToken(t) then begin
        i.Free;
        Exit;
      end;
      if (t.IsSymbol(SYM_SEMICOLON)) then begin
          { A forward declaration of type "name = class(ancestor);" }
        FreeAndNil(t);
        if Assigned(U) then U.AddCIO(i);
        Result := True;
        Exit;
      end
      else
        if (t.MyType = TOK_KEYWORD) then
          case t.Info.KeyWord of
            KEY_CLASS: begin
                CS := t.Data;
                CSFound := True;
              end;
            KEY_CONSTRUCTOR,
              KEY_DESTRUCTOR,
              KEY_FUNCTION,
              KEY_PROCEDURE: begin
                d := GetLastComment(True);
                if (not ParseCDFP(M, CS, t.Data, t.Info.KeyWord, d, True))
                  then begin
                  i.Free;
                  FreeAndNil(t);
                  Exit;
                end;
                M.State := State;
                if State in ClassMembers then begin
                  i.Methods.Add(M);
                end
                else
                begin
                  M.Free;
                end;
              end;
            KEY_END: Finished := True;
            KEY_PROPERTY: begin
                if (not ParseProperty(p)) then begin
                  FreeAndNil(t);
                  i.Free;
                  Exit;
                end;
                p.State := State;
                if State in ClassMembers then begin
                  i.Properties.Add(p);
                end
                else
                begin
                  FreeAndNil(p);
                end;
              end;
            KEY_CASE: begin
              try
                if not ParseRecordCase(i, false) then begin
                  FreeAndNil(t);
                  i.Free;
                  Exit;
                end;
                except
                  FreeAndNil(t);
                  i.Free;
                  raise;
                end;
              end;
          else begin
              i.Free;
              try
                DoError('%s: Error, unexpected reserved keyword "%s".',
                  [Scanner.GetStreamInfo, KeyWordArray[t.Info.KeyWord]], 0);
              finally
                FreeAndNil(t);
              end;
            end;
          end
        else
          if (t.MyType = TOK_IDENTIFIER) then
          begin
            CS := t.Data;
            case t.Info.StandardDirective of
              SD_DEFAULT: begin
                  if not SkipDeclaration(nil) then begin
                    DoError('%s: Could not skip declaration after default property.', [Scanner.GetStreamInfo], 0);
                  end;
                  DoMessage(5, mtInformation, 'Skipped default property keyword.', []);
                end;
              SD_PUBLIC: State := STATE_PUBLIC;
              SD_PUBLISHED: State := STATE_PUBLISHED;
              SD_PRIVATE: State := STATE_PRIVATE;
              SD_PROTECTED: State := STATE_PROTECTED;
              SD_AUTOMATED: State := STATE_AUTOMATED;
              else
                if not ParseCIOField(t, i, State in ClassMembers, State) then
                  Exit;
            end;
          end;
      if (not CSFound) then CS := '';
      FreeAndNil(t);
    until Finished;
    
    ParseHintDirectives(i);
    
    GetNextNonWCToken(t);
    try
      if not t.IsSymbol(SYM_SEMICOLON) then
      begin
        if IsInRecordCase then
        begin
          if t.IsSymbol(SYM_RIGHT_PARENTHESIS) then 
            Scanner.UnGetToken(t) else 
          begin
            i.Free;
            DoError('%s: unexpected symbol at end of sub-record.', 
              [Scanner.GetStreamInfo], 0);
          end;
        end else 
        begin
          i.Free;
          DoError('%s: Semicolon at the end of Class / Object / Interface' +
            ' / Record expected.', [Scanner.GetStreamInfo], 0);
        end;
      end;
    finally
      FreeAndNil(t);
    end;

    if Assigned(U) then U.AddCIO(i) else i.Free;
  except
    t.Free;
    raise;
  end;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseConstant(const U: TPasUnit; t: TToken): Boolean;
var
  i: TPasVarConst;
begin
  Result := False;
  i := TPasVarConst.Create;
  i.Name := t.Data;
  DoMessage(5, mtInformation, 'Parsing constant %s.', [i.Name]);
  i.RawDescription := GetLastComment(True);
  if SkipDeclaration(i) then begin
    U.AddConstant(i);
    Result := True;
  end
  else
    DoError('Could not skip declaration of constant "%s".', [i.Name], 0);
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseEnum(var p: TPasEnum): boolean;
var
  t: TToken;
  item: TPasItem;
begin
  t := nil;
  p := TPasEnum.Create;

  GetNextNonWCToken(t);
  while not t.IsSymbol(SYM_RIGHT_PARENTHESIS) do begin
    if t.MyType = TOK_IDENTIFIER then begin
      item := TPasItem.Create;
      item.Name := t.Data;
      item.RawDescription := GetLastComment(True);
      p.Members.Add(item);
    end;
    if t.IsSymbol(SYM_EQUAL) then begin
      FreeAndNil(t);
      GetNextNonWCToken(t);
    end;
    FreeAndNil(t);
    GetNextNonWCToken(t);
  end;
  FreeAndNil(t);
  Scanner.GetToken(t);
  FreeAndNil(t);
  Result := true;
end;

function TParser.ParseInterfaceSection(const U: TPasUnit): Boolean;
const
  MODE_UNDEFINED = 0;
  MODE_CONST = 1;
  MODE_TYPE = 2;
  MODE_VAR = 3;
var
  d: string;
  Finished: Boolean;
  Mode: Integer;
  M: TPasMethod;
  t: TToken;
begin
  DoMessage(4, mtInformation, 'Entering interface section of unit %s',[U.Name]);
  Result := False;
  Finished := False;
  Mode := MODE_UNDEFINED;
  t := nil;
  repeat
    if not GetNextNonWCToken(t) then
      DoError('Could not get next non-whitespace, non-comment token in file %s', [Scanner.GetStreamInfo], 0);

    try
    case t.MyType of
      TOK_IDENTIFIER: begin
          // s := t.Data;
          case Mode of
            MODE_CONST:
              if (not ParseConstant(U, t)) then Exit;
            MODE_TYPE:
              if (not ParseType(U, t)) then Exit;
            MODE_VAR:
              if (not ParseVariables(U, t)) then Exit;
          else
            DoError('%s: Error, unexpected identifier "%s".',
              [Scanner.GetStreamInfo, t.Data], 0);
          end;
        end;
      TOK_KEYWORD: begin
          case t.Info.KeyWord of
            KEY_RESOURCESTRING,
              KEY_CONST:
              Mode := MODE_CONST;
            KEY_OPERATOR: begin
                d := GetLastComment(True);
                if (not ParseCDFP(M, '', t.Data, t.Info.KeyWord, d, True))
                then begin
                  Exit;
                end;
                u.FuncsProcs.Add(M);
                Mode := MODE_UNDEFINED;
                //DoError('Sorry, FreePascal operator overloading syntax cannot be parsed currently', [], 100);
              end;
            KEY_FUNCTION,
              KEY_PROCEDURE: begin
                d := GetLastComment(True);
                if (not ParseCDFP(M, '', t.Data, t.Info.KeyWord, d, True))
                  then begin
                  Exit;
                end;
                u.FuncsProcs.Add(M);
                Mode := MODE_UNDEFINED;
              end;
            KEY_IMPLEMENTATION:
              Finished := True;
            KEY_TYPE:
              Mode := MODE_TYPE;
            KEY_USES:
              if not ParseUses(U) then Exit;
            KEY_THREADVAR,
              KEY_VAR:
              Mode := MODE_VAR;
          else
            DoError('%s: Unexpected keyword %s.', [Scanner.GetStreamInfo,
              t.Data], 0);
          end;
        end;
    end;
    finally
    FreeAndNil(t);
    end;
  until Finished;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseProperty(var p: TPasProperty): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  t := nil;
  ParseProperty := False;
  if (not GetNextNonWCToken(t)) then Exit;
  if (t.MyType <> TOK_IDENTIFIER) then begin
    FreeAndNil(t);
    DoError('%s: expected identifier as property name.',
      [Scanner.GetStreamInfo], 0);
  end;
  p := TPasProperty.Create;
  p.Name := t.Data;
  DoMessage(5, mtInformation, 'Parsing property %s', [p.Name]);
  p.IndexDecl := '';
  p.Proptype := '';
  FreeAndNil(t);
  p.RawDescription := GetLastComment(True);
  if (not GetNextNonWCToken(t)) then Exit;
  { get index }
  if (t.IsSymbol(SYM_LEFT_BRACKET)) then begin
    FreeAndNil(t);
    p.IndexDecl := '[';
    repeat
      if not Scanner.GetToken(t) then
        DoError('Error, could not parse property in file %s', [Scanner.GetStreamInfo], 0);

      if not (t.MyType in [TOK_COMMENT_PAS, TOK_COMMENT_EXT, TOK_COMMENT_CSTYLE, TOK_DIRECTIVE]) then
        p.IndexDecl := p.IndexDecl + t.Data;
      Finished := t.IsSymbol(SYM_RIGHT_BRACKET);
      FreeAndNil(t);
    until Finished;
      { next nonwc token should be the colon }
    if (not GetNextNonWCToken(t)) then
      Exit;
  end
  else begin
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
      p.FullDeclaration := p.Name + ';';
      FreeAndNil(t);
      ParseProperty := True;
      Exit;
    end;
  end;

  { now if there is a colon, it is followed by the type }
  if t.IsSymbol(SYM_COLON) then begin
      { get property type }
    FreeAndNil(t);
    if (not GetNextNonWCToken(t)) then Exit;
    if (t.MyType <> TOK_IDENTIFIER) and (t.MyType <> TOK_KEYWORD) then
      DoError('Identifier expected, found %s in file %s',
        [TOKEN_TYPE_NAMES[t.MyType], Scanner.GetStreamInfo], 0);

    p.Proptype := t.Data;
    FreeAndNil(t);
    p.FullDeclaration := p.Name + p.IndexDecl + ': ' + p.Proptype + ';';
  end
  else
    p.FullDeclaration := p.Name + ';';

  { simply skipping the rest of declaration }
  if not SkipDeclaration(nil) then
    DoError('Could not skip rest of declaration in file %s', [Scanner.GetStreamInfo], 0);

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

{
  TYPENAME =
    class of ... ;               => "normal" type
    class ( ANCESTOR<S> )        => class
          ANYTHING               => class
    object ( ) end ;                 => object
          ANYTHING
    interface end ;              => interface
}

function TParser.ParseRecordCase(const R: TPasCio;
  const SubCase: boolean): boolean;
var
  t1, t2: TToken;
  P: TPasItem;
  LLastWasComma: boolean;
  s: string;
  LNeedId: boolean;
  ParenCount: integer;
begin
  Result := True;
  ParenCount := 0;
  t1:=nil; t2:=nil;
  GetNextNonWCToken(t1);
  if t1.MyType <> TOK_IDENTIFIER then begin
    Result := False;
    FreeAndNil(t1);
  end else begin
    GetNextNonWCToken(t2);
    if (t2.MyType = TOK_SYMBOL) and (t2.Info.SymbolType = SYM_COLON) then begin
      // case x:Type of
      FreeAndNil(t2); // colon
      GetNextNonWCToken(t2);
      P := TPasItem.Create;
      p.Name := t1.Data;
      p.RawDescription := GetLastComment(True);
      R.Fields.Add(p);
    end else begin
      // case Type of
      Scanner.UnGetToken(t2);
    end;
    FreeAndNil(t2);
    FreeAndNil(t1);
    GetNextNonWCToken(t1);
    if (t1.MyType <> TOK_KEYWORD) or (t1.Info.KeyWord <> KEY_OF) then begin
      FreeAndNil(t1);
      DoError('OF expected',[],1);
    end;
    FreeAndNil(t1);
    GetNextNonWCToken(t1);
    LNeedId := True;
    repeat
      while true do begin
        case t1.MyType of
          TOK_SYMBOL: begin
              case t1.Info.SymbolType of
                SYM_COLON: break;
                SYM_COMMA: LNeedId := True;
              end;
            end;
          TOK_IDENTIFIER,
          TOK_NUMBER: if not LNeedId then begin
                        s := t1.Data;
                        FreeAndNil(t1);
                        DoError('did not expect identifier %s here!', [s], 1);
                      end;
          else begin
            s := t1.Data;
            FreeAndNil(t1);
            DoError('unexpected token: %s', [s], 1);
          end;
        end;
        FreeAndNil(t1);
        GetNextNonWCToken(t1);
      end;
      // read all identifiers before colon

      FreeAndNil(t1);
      GetNextNonWCToken(t1);
      if (t1.MyType <> TOK_SYMBOL) or (t1.Info.SymbolType <> SYM_LEFT_PARENTHESIS) then begin
        FreeAndNil(t1);
        DoError('( expected', [], 1);
      end;
      FreeAndNil(t1);
      GetNextNonWCToken(t1);
      while (t1.MyType <> TOK_SYMbol) or (T1.Info.SymbolType <> SYM_RIGHT_PARENTHESIS) do begin
        if (t1.MyType = TOK_IDENTIFIER) or (ParenCount > 0) then begin
          P := TPasItem.Create;
          p.RawDescription := GetLastComment(True);
          P.Name:=t1.Data;
          R.Fields.Add(p);
          if (ParenCount = 0) then
          begin
            FreeAndNil(t1);
            GetNextNonWCToken(t1);
          end;
          LLastWasComma := false;
          while (t1.MyType <> TOK_SYMBOL)
            OR ((t1.Info.SymbolType <> SYM_SEMICOLON)
            and (t1.Info.SymbolType <> SYM_RIGHT_PARENTHESIS))
            or ((t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS)
            and (ParenCount > 0)) do begin
            if (t1.MyType = TOK_IDENTIFIER) then begin
              if LLastWasComma then begin
                p := TPasItem.Create;
                p.RawDescription := GetLastComment(True);
                p.Name := t1.data;
                R.Fields.Add(p);
              end;
            end;
            if t1.MyType = TOK_KEYWORD then begin
              if (t1.Info.KeyWord = KEY_RECORD) then begin
                ParseCIO(nil, '', CIO_RECORD, '', True);
              end;
              if (t1.Info.KeyWord = KEY_PACKED) then begin
                FreeAndNil(t1);
                GetNextNonWCToken(t1);
                if (t1.MyType = TOK_KEYWORD) and (t1.Info.KeyWord = KEY_RECORD) then begin
                  ParseCIO(nil, '', CIO_PACKEDRECORD, '', True);
                end;
              end;
            end;
            LLastWasComma := false;
            if (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_COMMA) then begin
              LLastWasComma := True;
            end;
            if (not (t1.Info.SymbolType in [SYM_RIGHT_PARENTHESIS, SYM_COLON]))
              or ((t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS) and
              (ParenCount > 0)) then begin

              if t1.Info.SymbolType = SYM_LEFT_PARENTHESIS then
              begin
                Inc(ParenCount)
              end
              else if t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS then
              begin
                Dec(ParenCount)
              end;
            end;
            FreeAndNil(t1);
            GetNextNonWCToken(t1);
          end;
          if (t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS)
            and (ParenCount = 0) then
          begin
            Scanner.UnGetToken(t1);
          end;
        end else begin
          if (t1.MyType = TOK_KEYWORD) and (t1.Info.KeyWord = KEY_CASE) then begin
            ParseRecordCase(R, true);
          end else begin
            FreeAndNil(t1);
            DoError('Invalid keyword found',[],1);
          end;
        end;
        FreeAndNil(t1); // free token
        GetNextNonWCToken(t1);
      end;
      FreeAndNil(t1); // free ')' token
      GetNextNonWCToken(t1); // next
      if (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_SEMICOLON) then begin
        FreeAndNil(t1);
        GetNextNonWCToken(t1);
      end;
      if (t1.MyType = TOK_KEYWORD) and (t1.Info.KeyWord = KEY_END) then break;
      if subcase and (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS) then break;
    until false;
    Scanner.UnGetToken(t1);
  end;
end;

function TParser.ParseType(const U: TPasUnit; var t: TToken): Boolean;
var
  d: string;
  i: TPasItem;
  n: string;
  LCollected, LTemp: string;
  M: TPasMethod;
  E: TPasEnum;
begin
  Result := False;
  n := t.Data;
  DoMessage(5, mtInformation, 'Parsing type "%s"', [n]);
  FreeAndNil(t);
  d := GetLastComment(True);
  if (not GetNextNonWCToken(t, LCollected)) then
    Exit;

  if (not t.IsSymbol(SYM_EQUAL)) then begin
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
      FreeAndNil(t);
      t := nil;
      Result := True;
      Exit;
    end;
    FreeAndNil(t);
    DoError('"=" expected in file %s', [Scanner.GetStreamInfo], 0);
  end;
  LCollected := LCollected + t.Data;
  FreeAndNil(t);

  if (not GetNextNonWCToken(t, LTemp)) then
    Exit;
  LCollected := LCollected + LTemp + t.Data;

  if (t.MyType = TOK_KEYWORD) then
    case t.Info.KeyWord of
      KEY_CLASS: begin
          FreeAndNil(t);
          if (not GetNextNonWCToken(t, LTemp)) then Exit;
          LCollected := LCollected + LTemp + t.Data;
          if (t.MyType = TOK_KEYWORD) and (t.Info.KeyWord = KEY_OF) then begin
            { include "identifier = class of something;" as standard type }
          end else begin
            Scanner.UnGetToken(t);
            t := nil;
            if not ParseCIO(U, n, CIO_CLASS, d, False) then Exit;
            Result := True;
            Exit;
          end;
        end;
      KEY_DISPINTERFACE: begin
          FreeAndNil(t);
          if not ParseCIO(U, n, CIO_SPINTERFACE, d, False) then Exit;
          Result := True;
          Exit;
        end;
      KEY_INTERFACE: begin
          FreeAndNil(t);
          if not ParseCIO(U, n, CIO_INTERFACE, d, False) then Exit;
          Result := True;
          Exit;
        end;
      KEY_OBJECT: begin
          FreeAndNil(t);
          if not ParseCIO(U, n, CIO_OBJECT, d, False) then Exit;
          Result := True;
          Exit;
        end;
      KEY_RECORD: begin
          FreeAndNil(t);
          if not ParseCIO(U, n, CIO_RECORD, d, False) then Exit;
          Result := True;
          Exit;
        end;
      KEY_PACKED: begin
          FreeAndNil(t);
          GetNextNonWCToken(t, LTemp);
          LCollected := LCollected + LTemp + t.Data;
          if (t.MyType = TOK_KEYWORD) AND (t.Info.KeyWord = KEY_RECORD) then begin
            FreeAndNil(t);
            if not ParseCIO(U, n, CIO_PACKEDRECORD, d, False) then exit;
            Result := True;
            exit;
          end;
        end;
    end;
  if Assigned(t) then begin
    if (t.MyType = TOK_KEYWORD) then begin
      if t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then begin
        if ParseCDFP(M, d, t.Data, t.Info.KeyWord, d, False) then begin
          M.Name := n;
          U.AddType(M);
          Result := True;
          exit;
        end else begin
          DoError('Very strange condition - found function but could not parse', [], 1);
        end;
      end;
    end;
    if t.IsSymbol(SYM_LEFT_PARENTHESIS) then begin
      if ParseEnum(E) then begin
        E.Name := n;
        E.RawDescription := d;
        U.AddType(E);
        Result := True;
        exit;
      end;
    end;
    SetLength(LCollected, Length(LCollected)-Length(t.Data));
    Scanner.UnGetToken(t);
  end;

  i := TPasVarConst.Create;
  TPasVarConst(i).FullDeclaration := LCollected;
  if not SkipDeclaration(TPasVarConst(i)) then begin
    i.Free;
  end else begin
    i.Name := n;
    i.RawDescription := d;
    U.AddType(i);
    Result := True;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseUnit(var U: TPasUnit): Boolean;
var
  t: TToken;
begin
  t := nil;
  Result := False;
  { get 'unit' keyword }
  if not GetNextNonWCToken(t) then Exit;
  if (t.MyType <> TOK_KEYWORD) or (t.Info.KeyWord <> KEY_UNIT) then
    DoError(Scanner.GetStreamInfo + ': keyword "unit" expected.', [], 0);
  
  FreeAndNil(t);

  U := TPasUnit.Create;
  try
    U.RawDescription := GetLastComment(True);
    if not GetNextNonWCToken(t) then Exit;

    { get unit name identifier }
    if t.MyType <> TOK_IDENTIFIER then
      DoError(Scanner.GetStreamInfo + ': identifier (unit name) expected.',
        [], 0);
    U.Name := t.Data;
    FreeAndNil(t);
    
    ParseHintDirectives(U);
    
    { skip semicolon }
    if not GetNextNonWCToken(t) then Exit;
    if not t.IsSymbol(SYM_SEMICOLON) then
      DoError(Scanner.GetStreamInfo + ': semicolon expected.', [], 0);
    FreeAndNil(t);
    if not GetNextNonWCToken(t) then Exit;

    { get 'interface' keyword }
    if (t.MyType <> TOK_KEYWORD) or (t.Info.KeyWord <> KEY_INTERFACE) then
      DoError(Scanner.GetStreamInfo + ': keyword "INTERFACE" expected.', [], 0);
    { now parse the interface section of that unit }
    Result := ParseInterfaceSection(U);
  except
    FreeAndNil(U);
    FreeAndNil(t);
    raise;
  end;
  FreeAndNil(t);
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseUses(const U: TPasUnit): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  t := nil;
  Result := False;

  repeat
    if not GetNextNonWCToken(t) then Exit;
    if t.MyType <> TOK_IDENTIFIER then
      DoError('%s: Error, unit name expected (found %s, %s.',
        [Scanner.GetStreamInfo, t.GetTypeName, t.Data], 0);
    U.UsesUnits.Add(t.Data);
    FreeAndNil(t);
    if not GetNextNonWCToken(t) then Exit;
    if (t.MyType <> TOK_SYMBOL) and
      (t.Info.SymbolType <> SYM_COMMA) and
      (t.Info.SymbolType <> SYM_SEMICOLON) then
      DoError('%s: Error, comma or semicolon expected.',
        [Scanner.GetStreamInfo], 0);

    Finished := t.Info.SymbolType = SYM_SEMICOLON;
    FreeAndNil(t);
  until Finished;

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseVariables(const U: TPasUnit; var t: TToken): Boolean;
var
  Finished: Boolean;
  FirstLoop: Boolean;
  i, dummy: TPasVarConst;
  m: TPasMethod;
  LNew: TPasItems;
  j: Integer;
  LCollector: string;
  ttemp: TToken;
  FirstCheck: boolean;
begin
  Result := False;
  dummy := TPasVarConst.Create;
  LNew := TPasItems.Create(false);
  FirstLoop := True;
  try
    repeat
      i := TPasVarConst.Create;
      if FirstLoop then begin
        i.Name := t.Data;
        FirstLoop := False;
      end else begin
        if (not GetNextNonWCToken(t, LCollector)) then Exit;
        dummy.FullDeclaration := dummy.FullDeclaration + LCollector;
        if (t.MyType <> TOK_IDENTIFIER) then
          DoError('%s: Identifier expected.', [Scanner.GetStreamInfo], 0);
        i.Name := t.Data;
      end;
      i.RawDescription := GetLastComment(False);
      U.AddVariable(i);
      LNew.Add(i);
      FreeAndNil(t);
      if (not GetNextNonWCToken(t, LCollector)) then Exit;
      dummy.FullDeclaration := dummy.FullDeclaration + LCollector;
      if (t.MyType <> TOK_SYMBOL) or
        ((t.Info.SymbolType <> SYM_COMMA) and
        (t.Info.SymbolType <> SYM_COLON)) then
        DoError('%s: Expected comma or colon in var declaration.',
          [Scanner.GetStreamInfo], 0);

      Finished := (t.Info.SymbolType = SYM_COLON);
      if (t.MyType <> TOK_SYMBOL) OR (t.Info.SymbolType <> SYM_COMMA) then begin
        dummy.FullDeclaration := dummy.FullDeclaration + t.Data;
      end;
      FreeAndNil(t);
    until Finished;
    ClearLastComment;
    GetNextNonWCToken(t, LCollector);
    dummy.FullDeclaration := dummy.FullDeclaration + LCollector + t.Data;
    if (t.MyType = TOK_KEYWORD) and (t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE]) then begin
      ParseCDFP(m, '', t.Data, t.Info.KeyWord, '', False);
      dummy.FullDeclaration := dummy.FullDeclaration + m.FullDeclaration;
      m.Free;
      FreeAndNil(t);
      GetNextNonWCToken(t, LCollector);
      dummy.FullDeclaration := dummy.FullDeclaration + LCollector;
      if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_EQUAL) then begin
        dummy.FullDeclaration := dummy.FullDeclaration + t.Data;
        SkipDeclaration(dummy);
      end else begin
        Scanner.UnGetToken(t);
      end;
    end else begin
      if not SkipDeclaration(dummy) then Exit;
    end;

    // The following section allows PasDoc to parse variable modifiers in FPC.
    // See: http://www.freepascal.org/docs-html/ref/refse19.html
    ClearLastComment;
    Finished := False;
    FirstCheck := True;
    repeat
      ttemp := nil;
      if (not GetNextNonWCToken(ttemp, LCollector)) then Exit;
      if FirstCheck then
      begin
        // If the first non-white character token after the semicolon
        // is "cvar", "export', "external", or "public", there is
        // a variable modifier present.

        // This does not take into account the "absolute" modifier
        // (which is not preceeded by a semicolon).
        FirstCheck := False;
        if ( (ttemp.MyType = TOK_KEYWORD) and 
             (ttemp.Info.KeyWord in [KEY_CVAR]) ) or
           ( (ttemp.MyType = TOK_IDENTIFIER) and 
             (ttemp.Info.StandardDirective in 
               [SD_EXPORT, SD_EXTERNAL, SD_PUBLIC]) ) then 
        begin
          dummy.FullDeclaration := dummy.FullDeclaration +  ' ' + ttemp.Data;
          FreeAndNil(ttemp)
        end
        else
        begin
          Finished := True;
          Scanner.UnGetToken(ttemp);
        end;
        while not Finished do
        begin
          if (not GetNextNonWCToken(ttemp, LCollector)) then Exit;
          if (ttemp.MyType = TOK_SYMBOL) and (ttemp.Info.SymbolType = SYM_SEMICOLON) then
          begin
            Finished := True;
            FirstCheck := False;
            dummy.FullDeclaration := dummy.FullDeclaration +  ttemp.Data;
          end
          else
          begin
            dummy.FullDeclaration := dummy.FullDeclaration +  ' ' + ttemp.Data;
          end;
          FreeAndNil(ttemp)
        end;
      end;
    until Finished and not FirstCheck;

    Result := True;
    for j := LNew.Count-1 downto 0 do begin
      TPasVarConst(LNew.PasItemAt[j]).FullDeclaration := dummy.FullDeclaration;
    end;
    LNew.SetIsDeprecated(dummy.IsDeprecated);
    LNew.SetIsPlatformSpecific(dummy.IsPlatformSpecific);
    LNew.SetIsLibrarySpecific(dummy.IsLibrarySpecific);
  finally
    dummy.free;
    LNew.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.SetCommentMarkers(const Value: TStringList);
begin
  FCommentMarkers.Assign(Value);
end;

function TParser.SkipDeclaration(const VC: TPasVarConst): Boolean;
var
  EndLevel: Integer;
  IsSemicolon: Boolean;
  PLevel: Integer;
  t: TToken;
  LCollector: string;
begin
  Result := False;
  EndLevel := 0;
  PLevel := 0;
  t := nil;
  repeat
    if not GetNextNonWCToken(t, LCollector) then Exit;
    if Assigned(VC) then begin
      VC.FullDeclaration := VC.FullDeclaration + LCollector;
    end;
    case t.MyType of
      TOK_SYMBOL:
        case t.Info.SymbolType of
          SYM_LEFT_PARENTHESIS: Inc(PLevel);
          SYM_RIGHT_PARENTHESIS: Dec(PLevel);
        end;
      TOK_KEYWORD:
        case t.Info.KeyWord of
          KEY_END: Dec(EndLevel);
          KEY_RECORD: Inc(EndLevel);
          KEY_LIBRARY: 
            if Assigned(VC) then VC.IsLibrarySpecific := true;
        end;
      TOK_IDENTIFIER:
        case t.Info.StandardDirective of
          SD_PLATFORM: 
            if Assigned(VC) then VC.IsPlatformSpecific := true;
          SD_DEPRECATED: 
            if Assigned(VC) then VC.IsDeprecated := true;
        end;
    end;
    IsSemicolon := (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
      SYM_SEMICOLON);
    if Assigned(VC) then VC.FullDeclaration := VC.FullDeclaration + t.Data;
    if EndLevel<0 then begin
      // within records et al. the last declaration need not be terminated by ;
      Scanner.UnGetToken(t);
      Result := True;
      exit;
    end;
    FreeAndNil(t);
  until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.SkipWhitespaceAndComments(var LCollector: string): Boolean;
var
  t: TToken;
begin
  Result := False;
  t := nil;
  LCollector := '';
  repeat
    if not Scanner.PeekToken(t) then break;
    case t.MyType of
      TOK_WHITESPACE: 
        begin
          Scanner.ConsumeToken;
          LCollector := LCollector + t.Data;
          FreeAndNil(t);
        end;
      TOK_COMMENT_PAS, TOK_COMMENT_EXT, TOK_COMMENT_CSTYLE:
        begin
          Scanner.ConsumeToken;
          // If there are several comments in a row, combine them.
          if Assigned(LastCommentToken) and
             (t.MyType = TOK_COMMENT_CSTYLE) and 
             (t.MyType = LastCommentToken.MyType) then 
          begin
            t.Data := GetLastComment(True) + LineEnding + ExtractComment(False, t);
            
            (* Remember that t.Data must be in the form acceptable by
               ExtractComment again. And the code above just removed comments
               braces and comment markers from the comment. This means that 
               we must do something ugly now:
               1. apply again comment braces (otherwise comments like
                  {( * bla bla * )} could not work as expected, as they
                  would be stripped from braces more than once.)
               2. add again marker, if it's not optional (otherwise
                  comments could be errorneously rejected because they no
                  longer have required marker)
            *)
            if (not MarkersOptional) and (CommentMarkers.Count > 0) then
              t.Data := CommentMarkers[0] + t.Data;
            t.Data := '{' + t.Data + '}';
          end;
          if Assigned(LastCommentToken) then
          begin
            LastCommentToken.Free;
          end;
          LastCommentToken := t;
          t := nil;
        end 
      else 
        begin
          Result := True;
          break;
        end;
    end;
  until False;
end;

function TParser.SkipWhitespaceAndComments: Boolean; 
var 
  Dummy: string;
begin
  Result := SkipWhitespaceAndComments(Dummy);
end;

{ ------------------------------------------------------------ }

procedure TParser.ParseHintDirectives(Item: TPasItem);
var
  t: TToken;
begin
  t := nil;
  repeat
    if not SkipWhitespaceAndComments then
      DoError('%s: Unexpected end of stream', [Scanner.GetStreamInfo], 1);
    Scanner.PeekToken(t);
    
    if (t.MyType = TOK_IDENTIFIER) and 
       (t.Info.StandardDirective = SD_PLATFORM) then
    begin
      Scanner.ConsumeToken;
      Item.IsPlatformSpecific := true;
      FreeAndNil(t);
    end else
    if (t.MyType = TOK_IDENTIFIER) and 
       (t.Info.StandardDirective = SD_DEPRECATED) then
    begin
      Scanner.ConsumeToken;
      Item.IsDeprecated := true;
      FreeAndNil(t);
    end else
    if (t.MyType = TOK_KEYWORD) and 
       (t.Info.KeyWord = KEY_LIBRARY) then
    begin
      Scanner.ConsumeToken;
      Item.IsLibrarySpecific := true;
      FreeAndNil(t);
    end else
      break;
  until false;
end;

end.
