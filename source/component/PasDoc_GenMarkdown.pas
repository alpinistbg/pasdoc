{
  Copyright 1998-2018 PasDoc developers.

  This file is part of "PasDoc".

  "PasDoc" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "PasDoc" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "PasDoc"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ @abstract(Markdown output generator.) }
unit PasDoc_GenMarkdown;

{$I pasdoc_defines.inc}

interface

uses
  Classes,
  SysUtils,
  PasDoc_Utils,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_StringVector,
  PasDoc_Types,
  PasDoc_StringPairVector,
  PasDoc_Base;

type

  TMarkdownVariant = (mdvOrig, mdvRedmine, mdvGithub);

  { TMarkdownDocGenerator }

  TMarkdownDocGenerator = class(TDocGenerator)
  protected
    function OneLineCodeString(const S: String): String;
    function InlineCodeString(const S: String): String;
    function CodeString(const S: String): String; override;
    function CodeWithLinks(const AItem: TPasItem; const ACode: String;
      ExtractedLinks: TStrings = Nil): String;
    function ConvertString(const S: String): String; override;
    function ConvertChar(C: Char): String; override;
    procedure WriteUnit(const HL: Integer; const U: TPasUnit); override;
    function DistinctName(const AItem: TPasItem): String;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;
    function FormatSection(HL: integer; const Anchor: String;
      const Caption: String): String; override;
    function FormatAnchor(const Anchor: String): String; override;
    function FormatTableHeader(AHeaders: TStringArray): String;
    function FormatTableRow(ACells: TStringArray): String;
    function FormatTable(Table: TTableData): String; override;
    function FormatList(ListData: TListData): String; override;
    function FormatBold(const Text: String): String; override;
    function FormatItalic(const Text: String): String; override;
    function FormatSubsc(const Text: String): String;
    function FormatNote(const Text: string): String; override;
    function FormatPascalCode(const Line: String): String; override;
    function FormatTableOfContents(Sections: TStringPairVector): string;
      override;

    function MakeItemLink(const AItem: TBaseItem; const LinkCaption: String;
      const LinkContext: TLinkContext): String; override;
  private
    FPasDoc: TPasDoc;
    FSoleUnit: Boolean;
    FHasProjectName: Boolean;
    FVariant: TMarkdownVariant;
    FPara: String;
    FIndent: Integer;

    function GithubAnchor(const Anchor: String): String;

    function ItemDescription(AItem: TPasItem): String;
    function ItemTypeStr(AItem: TPasItem): String;

    procedure WriteHintDirectives(const AItem: TPasItem);
    procedure WriteDescription(const AItem: TPasItem);
    procedure WriteSeeAlso(const AItem: TPasItem);
    procedure WritePasItem(const AItem: TPasItem);
    procedure WriteDatesAuthor(const AItem: TBaseItem; Header: Boolean = False);

    procedure HR;
    procedure WritePara(const AText: String);

    procedure Indent;
    procedure UnIndent;

    function Hn(AIn: Integer = 0; AAnchor: String = ''): String;
    function AppendEmptyLine(AText: String = ''): String;
    function BR: String;
    function LinkTo(AText, ARef: String): String;
    function LinkToAnchor(AText, AAnchor: String): String;
    procedure WriteHeading(AText: String; AAnchor: String = '');
    procedure WithEmptyLine(AText: String = '');
    procedure WriteTableHeader(AHeaders: TStringArray);
    procedure WriteTableRow(ACells: TStringArray);

    procedure WriteRoutine(const AItem: TPasRoutine);
    procedure WriteConstant(const AItem: TPasItem);
    procedure WriteVariable(const AItem: TPasItem);
    procedure WriteType(const AItem: TPasItem);
    procedure WriteStructure(const AItem: TPasCIO);
    procedure WriteProperty(const AItem: TPasProperty);
    procedure WriteProjHeader;
  public
    constructor Create(AOwner: TComponent; AFormat: String); reintroduce;
    procedure WriteDocumentation; override;
    function GetFileExtension: String; override;
    property HasProjectName: Boolean read FHasProjectName write FHasProjectName;
    property PasDoc: TPasDoc read FPasDoc write FPasDoc;
  end;

implementation

uses
  StrUtils;

type
  TMarkdownElement = (
    mdvBoldLeft,
    mdvBoldRight,
    mdvItalicLeft,
    mdvItalicRight,
    mdvSubscriptLeft,
    mdvSubscriptRight,
    mdvUnorderedList,
    mdvOrderedList,
    mdvDefinitionList,
    mdvInlCodeLeft,
    mdvInlCodeRight,
    mdvLongCodeBegin,
    mdvLongCodeEnd,
    mdvLineBreak
  );

const
  LE = LineEnding;
  MdElement: array[TMarkdownElement, TMarkdownVariant] of String = (
  // orig, redmine, github
    ('**', '**', '**'), // mdvBoldLeft
    ('**', '**', '**'), // mdvBoldRight
    ( '*', '__', '*'), // mdvItalicLeft
    ( '*', '__', '*'), // mdvItalicRight
    ( '~', '~', '<sub>'),  // mdvSubscriptLeft,
    ( '~', '~', '</sub>'),  // mdvSubscriptRight,
    ( '-', '*', '-'),  // mdvUnorderedList,
    ( '1.', '#', '1.'),  // mdvOrderedList,
    ( ':'+LE, ' := ', ''),  // mdvDefinitionList,
    ( '`',  '@', '`'), // mdvInlCodeLeft
    ( '`',  '@', '`'), // mdvInlCodeRight
    ( '```',  '<pre><code class="pascal">', '```'), // mdvLongCodeBegin
    ( '```',  '</code></pre>', '```'), // mdvLongCodeEnd
    ( '<br/>', '<br/>', '<br/>') // mdvLineBreak
  );

function AppSep(S1, S2, ASep: String): String;
begin
  if S2 <> ''
    then Result := S1 + IfThen(S1 <> '', ASep) + S2
    else Result := S1;
end;

function IsML(S: String): Boolean; // Multiline?
begin
  Result := PosSet(LE, S) > 0;
end;

{ TMarkdownDocGenerator }

function TMarkdownDocGenerator.FormatBold(const Text: String): String;
begin
  Result := MdElement[mdvBoldLeft, FVariant] + Text +
    MdElement[mdvBoldRight, FVariant];
end;

function TMarkdownDocGenerator.FormatItalic(const Text: String): String;
begin
  Result := MdElement[mdvItalicLeft, FVariant] + Text +
    MdElement[mdvItalicRight, FVariant];
end;

function TMarkdownDocGenerator.FormatSubsc(const Text: String): String;
begin
  Result := MdElement[mdvSubscriptLeft, FVariant] + Text +
    MdElement[mdvSubscriptRight, FVariant];
end;

function TMarkdownDocGenerator.FormatNote(const Text: string): String;
begin
  Result := '> ' + Text + LE + LE;
end;

function TMarkdownDocGenerator.FormatPascalCode(const Line: String): String;
begin
  Result := LE + CodeString(inherited FormatPascalCode(Line));
end;

function TMarkdownDocGenerator.FormatTableOfContents(Sections: TStringPairVector
  ): string;
begin
  Result := inherited FormatTableOfContents(Sections);
end;

function TMarkdownDocGenerator.MakeItemLink(const AItem: TBaseItem;
  const LinkCaption: String; const LinkContext: TLinkContext): String;
begin
  Result := LinkToAnchor(LinkCaption, AItem.QualifiedName);
end;

function ReplaceML(const S: String): String;
const
  Replacements: array of TCharReplacement = (
    (cChar:  #9; sSpec: ' '),
    (cChar: #10; sSpec: ' '),
    (cChar: #13; sSpec: ' ')
  );
begin
  Exit(StringReplaceChars(S, Replacements));
end;

function TMarkdownDocGenerator.OneLineCodeString(const S: String): String;
begin
  if IsML(S)
    then Result := InlineCodeString(ReplaceML(S))
    else Result := InlineCodeString(S);
end;

function TMarkdownDocGenerator.InlineCodeString(const S: String): String;
begin
  Result := MdElement[mdvInlCodeLeft, FVariant] + S +
    MdElement[mdvInlCodeRight, FVariant];
end;

function TMarkdownDocGenerator.CodeString(const S: String): String;
begin
  if IsML(S) then
    Result :=
      MdElement[mdvLongCodeBegin, FVariant] +
      IfThen(not StartsStr(LE, S), LE) +
      S + LE +
      MdElement[mdvLongCodeBegin, FVariant] + LE
  else
    Result := InlineCodeString(S);
end;

function TMarkdownDocGenerator.CodeWithLinks(const AItem: TPasItem;
  const ACode: String; ExtractedLinks: TStrings): String;
const
  IdentDelims: TSysCharSet = [Low(TSysCharSet)..High(TSysCharSet)] -
    ['.', '_', '0'..'9', 'A'..'Z', 'a'..'z'];
  IdentNonStart: TSysCharSet = ['.', '0'..'9'];
var
  P: Integer;
  FoundPos: Integer;
  S, LinkText, Brk: String;
  FoundItem: TBaseItem;
begin
  // Replace CR,LF,TAB with spaces
  Result := ReplaceML(ACode);

  // Split the string after the 80-th column
  P := 80;
  Brk := MdElement[mdvLineBreak, FVariant];
  while P < Length(Result) do
  begin
    ExtractSubstr(Result, P, [' ', #9]);
    if P < Length(Result) then
    begin
      Result := Copy(Result, 1, P - 1) + Brk + Copy(Result, P, MaxInt);
      Inc(P, 80{Length(Brk) - 1});
    end;
  end;

  P := 1;
  // Redmine links not working so far
  if FVariant = mdvRedmine then
  else repeat
    FoundPos := P;
    S := ExtractSubstr(Result, P, IdentDelims);
    if S.IsEmpty or (S[1] in IdentNonStart) then
      Break;
    FoundItem := SearchItem(S, AItem, False);
    if Assigned(FoundItem) then
    begin
      LinkText := MakeItemLink(FoundItem, S, lcCode);
      if not LinkText.IsEmpty then
      begin
        if Assigned(ExtractedLinks) then
          ExtractedLinks.Add(LinkText);
        Result := Copy(Result, 1, FoundPos - 1) + LinkText +
          Copy(Result, FoundPos + Length(S), MaxInt); // Replace with the link
        Inc(P, Length(LinkText) - Length(S)); // Adjust the extract position
      end;
    end;
  until S.IsEmpty;
end;

function TMarkdownDocGenerator.ConvertString(const S: String): String;
begin
  Result := S;
end;

function TMarkdownDocGenerator.ConvertChar(C: Char): String;
begin
  Result := ConvertString(C);
end;

procedure TMarkdownDocGenerator.WriteUnit(const HL: Integer; const U: TPasUnit);
var
  I: Integer;
  S: String;
begin
  if not Assigned(U) then
  begin
    DoMessage(1, pmtError, 'TMarkdownDocGenerator.WriteUnit: ' +
      'Unit variable has not been initialized.', []);
    Exit;
  end;

  if U.FileNewerThanCache(DestinationDirectory + U.OutputFileName) then
  begin
    DoMessage(3, pmtInformation, 'Data for unit "%s" was loaded from cache, '+
      'and output file of this unit exists and is newer than cache, '+
      'skipped.', [U.Name]);
    Exit;
  end;

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  FIndent := HL;

  WriteHeading('Unit ' + DistinctName(U), U.QualifiedName);

  if FVariant = mdvRedmine then
  begin
    // Generate a TOC for Redmine
    WritePara('{{>toc}}');
    WithEmptyLine();
    WithEmptyLine();
  end;

  if U.HasDescription then
    WritePara(ItemDescription(U));

  Indent;

  WriteDatesAuthor(U, True);

  // Used units
  if WriteUsesClause and not IsEmpty(U.UsesUnits) then
  begin
    WriteHeading('Uses');
    S := '';
    for I := 0 to Pred(U.UsesUnits.Count) do
      WriteDirectLine(MdElement[mdvUnorderedList, FVariant] + ' ' + U.UsesUnits[I]);
    WithEmptyLine();
  end;

  if (U.CIOs.Count > 0) or (U.FuncsProcs.Count > 0) or (U.Types.Count > 0) or
    (U.Constants.Count > 0) or (U.Variables.Count > 0)
  then
  begin
    WriteHeading('Overview');

    Indent;

    if U.CIOs.Count > 0 then
    begin
      //WriteHeading('Classes, Interfaces, Objects and Records');
      WriteTableHeader(['Classes, Interfaces, Objects and Records'{'Name'}, 'Description']);
      for I := 0 to Pred(U.CIOs.Count) do
        with U.CIOs.PasItemAt[I] as TPasCIO do
          WriteTableRow([
            CioTypeToString(MyType) + ' ' + MakeItemLink(U.CIOs.PasItemAt[I], Name, lcNormal),
            AbstractDescription
          ]);
        WithEmptyLine();
    end;

    if U.FuncsProcs.Count > 0 then
    begin
      //WriteHeading('Functions and Procedures');
      WriteTableHeader(['Functions and Procedures'{'Name'}, 'Description']);
      for I := 0 to Pred(U.FuncsProcs.Count) do
        with U.FuncsProcs.PasItemAt[I] as TPasRoutine do
          WriteTableRow([
            CodeWithLinks(U, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Types.Count > 0 then
    begin
      //WriteHeading('Types');
      WriteTableHeader(['Types'{'Name'}, 'Description']);
      for I := 0 to Pred(U.Types.Count) do
        // Gives a typecast exception on a procedure of object type
        with U.Types.PasItemAt[I] {as TPasType} do
          WriteTableRow([
            CodeWithLinks(U, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Constants.Count > 0 then
    begin
      //WriteHeading('Constants');
      WriteTableHeader(['Constants'{'Name'}, 'Description']);
      for I := 0 to Pred(U.Constants.Count) do
        with U.Constants.PasItemAt[I] as TPasConstant do
          WriteTableRow([
            CodeWithLinks(U, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Variables.Count > 0 then
    begin
      //WriteHeading('Variables');
      WriteTableHeader(['Variables'{'Name'}, 'Description']);
      for I := 0 to Pred(U.Variables.Count) do
        with U.Variables.PasItemAt[I] do
          WriteTableRow([
            CodeWithLinks(U, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    UnIndent;

    WriteHeading('Description');

    Indent;

    if U.CIOs.Count > 0 then
    begin
      WriteHeading('Classes, Interfaces, Objects and Records');
      Indent;
      for I := 0 to Pred(U.CIOs.Count) do
        WriteStructure(U.CIOs.PasItemAt[I] as TPasCIO);
      WithEmptyLine();
      UnIndent;
    end;

    if U.FuncsProcs.Count > 0 then
    begin
      WriteHeading('Functions and Procedures');
      Indent;
      for I := 0 to Pred(U.FuncsProcs.Count) do
        WriteRoutine(U.FuncsProcs.PasItemAt[I] as TPasRoutine);
      WithEmptyLine();
      UnIndent;
    end;

    if U.Types.Count > 0 then
    begin
      WriteHeading('Types');
      Indent;
      for I := 0 to Pred(U.Types.Count) do
        // Gives a typecast exception on a procedure of object type
        WriteType(U.Types.PasItemAt[I] {as TPasType});
      WithEmptyLine();
      UnIndent;
    end;

    if U.Constants.Count > 0 then
    begin
      WriteHeading('Constants');
      Indent;
      for I := 0 to Pred(U.Constants.Count) do
        WriteConstant(U.Constants.PasItemAt[I] as TPasConstant);
      WithEmptyLine();
    end;

    if U.Variables.Count > 0 then
    begin
      WriteHeading('Variables');
      Indent;
      for I := 0 to Pred(U.Variables.Count) do
        WriteVariable(U.Variables.PasItemAt[I]);
      WithEmptyLine();
      UnIndent;
    end;

    UnIndent;

  end;

  WithEmptyLine(BR);

  UnIndent;
end;

function TMarkdownDocGenerator.DistinctName(const AItem: TPasItem): String;
begin
  if FSoleUnit
    then Result := AItem.Name
    else Result := AItem.QualifiedName;
end;

procedure TMarkdownDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem; const Id: TTranslationID);
begin
  WriteHeading(ExternalItem.Title, ExternalItem.FullLink);
  WriteDatesAuthor(ExternalItem, True);
  WithEmptyLine(ExternalItem.DetailedDescription);
  WithEmptyLine(BR);
end;

function TMarkdownDocGenerator.FormatSection(HL: integer; const Anchor: String;
  const Caption: String): String;
begin
  Result := AppendEmptyLine(LE + LE + Hn(HL) + AppSep(Caption, FormatAnchor(Anchor), ' '));
end;

function TMarkdownDocGenerator.FormatAnchor(const Anchor: String): String;
begin
  Result := '';
  if Anchor.IsEmpty then
    Exit;
  case FVariant of
    mdvOrig: Result := '{#' + Anchor + '}';
    mdvRedmine: Result := '(#' + Anchor + ')';
    mdvGithub: Result := '<a name="' + GithubAnchor(Anchor) + '"></a>';
  end;
end;

function TMarkdownDocGenerator.FormatTableHeader(AHeaders: TStringArray
  ): String;
var
  H, L, S: String;
begin
  case FVariant of
    mdvOrig, mdvGithub:
      begin
        H := ''; L := '';
        for S in AHeaders do
        begin
          H := H + '|' + S;
          L := L + '|---';
        end;
        Result := H + '|' + LE + L + '|' + LE;
      end;
    mdvRedmine:
      begin
        H := '';
        for S in AHeaders do
          H := H + '|_. ' + S;
        Result := H + '|' + LE;
      end;
  end;
end;

function TMarkdownDocGenerator.FormatTableRow(ACells: TStringArray): String;
var
  R, S: String;
begin
  R := '';
  for S in ACells do
    R := R + '|' + S;
  Result := R + '|' + LE;
end;

function TMarkdownDocGenerator.FormatTable(Table: TTableData): String;
var
  R, C: Integer;
  A: TStringArray;
  Row: TRowData;
begin
  Result := LE;
  for R := 0 to Pred(Table.Count) do
  begin
    Row := Table.Items[R] as TRowData;
    SetLength(A, Row.Cells.Count);
    for C := 0 to Pred(Row.Cells.Count) do
      A[C] := Row.Cells[C];
    if Row.Head
      then Result := Result + FormatTableHeader(A)
      else Result := Result + FormatTableRow(A);
  end;
  Result := Result + LE;
end;

function TMarkdownDocGenerator.FormatList(ListData: TListData): String;
const
  ListTag: array[TListType] of String =
  ( '- ', '1. ', '' );
var
  I: Integer;
  ListItem: TListItemData;
begin
  // Re-init the tags table
  ListTag[ltUnordered] := MdElement[mdvUnorderedList, FVariant];
  ListTag[ltOrdered] := MdElement[mdvOrderedList, FVariant];
  ListTag[ltDefinition] := MdElement[mdvDefinitionList, FVariant];

  Result := LE + LE;
  for I := 0 to ListData.Count - 1 do
  begin
    ListItem := ListData.Items[I] as TListItemData;
    if ListData.ListType = ltDefinition then
    begin
      Result := Result + ListItem.ItemLabel + LE + ': ' + ListItem.Text;
      //Result := Result + AppSep(ListItem.ItemLabel, ListItem.Text, LE + ': ');
      //if IsML(Result) then
      //  Result := Result + '=:';
    end
    else
      Result := Result + ListItem.Text;
    Result := Result + LE;
  end;
  Result := Result + LE + LE; //?
end;

function TMarkdownDocGenerator.ItemDescription(AItem: TPasItem): String;
var
  Abstract, Detailed: String;
begin
  if AItem.HasDescription then
  begin
    Result := '';
    if AItem.AbstractDescription <> '' then
      Result := Result + AItem.AbstractDescription + LE + LE;
    if AItem.DetailedDescription <> '' then
      Result := Result + AItem.DetailedDescription + LE;
  end
  else
    Result := '';
end;

function TMarkdownDocGenerator.ItemTypeStr(AItem: TPasItem): String;
begin
  if AItem is TPasRoutine then
    Result := RoutineTypeToString(TPasRoutine(AItem).What)
  else if AItem is TPasCio then
    Result := CioTypeToString(TPasCio(AItem).MyType)
  else if AItem is TPasProperty then
    Result := 'property'
  else if AItem is TPasEnum then
    Result := 'enum'
  else if AItem is TPasType then
    Result := 'type'
  else if AItem is TPasConstant then
    Result := 'const'
  else if AItem is TPasFieldVariable then
    Result := 'var';
end;

procedure TMarkdownDocGenerator.WriteRoutine(const AItem: TPasRoutine);
var
  I: Integer;
  TypeStr, HereLink, LinksText, LinksMore: String;
  Links: TStringList;
begin
  TypeStr := RoutineTypeToString(AItem.What);
  WriteHeading(TypeStr + ' ' + DistinctName(AItem), AItem.QualifiedName);
  WritePasItem(AItem);
end;

procedure TMarkdownDocGenerator.WriteConstant(const AItem: TPasItem);
begin
  WriteHeading('Constant ' + DistinctName(AItem), AItem.QualifiedName);
  WriteTableHeader(['Name', AItem.Name]);
  WriteTableRow(['declaration', OneLineCodeString(AItem.FullDeclaration)]);
  WriteTableRow(['visibility', VisToStr(AItem.Visibility)]);
  WithEmptyLine();

  WriteDescription(AItem);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteVariable(const AItem: TPasItem);
begin
  WriteHeading('Variable ' + DistinctName(AItem), AItem.QualifiedName);
  WritePasItem(AItem);
end;

procedure TMarkdownDocGenerator.WriteType(const AItem: TPasItem);

  procedure WriteEnumMembers(const Item: TPasEnum);
  var
    I: Integer;
  begin
    WriteTableHeader(['Enum', 'Declaration', 'Comment']);
    for I := 0 to Pred(Item.Members.Count) do
      with Item.Members.PasItemAt[I] do
      //WriteConstant(Item.Members.PasItemAt[i]);
      WriteTableRow([Name, OneLineCodeString(FullDeclaration), AbstractDescription]);
  end;

begin
  WriteHeading('Type ' + DistinctName(AItem), AItem.QualifiedName);
  WriteTableHeader([AItem.Name, '']);
  WriteTableRow(['declaration', OneLineCodeString(AItem.FullDeclaration)]);
  WriteTableRow(['visibility', VisToStr(AItem.Visibility)]);
  WithEmptyLine();

  WriteDescription(AItem);
  WithEmptyLine();

  if AItem is TPasEnum then
  begin
    Indent;
    WriteEnumMembers(TPasEnum(AItem));
    UnIndent;
  end;
end;

procedure TMarkdownDocGenerator.WriteStructure(const AItem: TPasCIO);
var
  I: Integer;
  HasAncestors, HasFields, HasMethods, HasProps: Boolean;
  Hdr: String;

  function ComposeDecl: String;
  var
    I: Integer;
    AncestorList: String;
  begin
    Result := 'type ' + AItem.NameWithGeneric + ' = ' +
      CioTypeToString(AItem.MyType) +
      GetClassDirectiveName(AItem.ClassDirective);
    if AItem.Ancestors.Count > 0 then
    begin
      AncestorList := '';
      for I := 0 to Pred(AItem.Ancestors.Count) do
        AncestorList := AppSep(AncestorList, AItem.Ancestors[I].Value, ', ');
      Result := Result + '(' + AncestorList + ')';
    end;
    if AItem.ClassDirective = CT_HELPER then
      Result := AppSep(Result, AItem.HelperTypeIdentifier, ' for ');
  end;

begin
  WriteHeading(
    CioTypeToString(AItem.MyType) + ' ' + DistinctName(AItem),
    AItem.QualifiedName);
  HR;

  HasAncestors := AItem.Ancestors.Count > 1{0}; // Always >0
  HasFields := AItem.Fields.Count > 0;
  HasMethods := AItem.Methods.Count > 0;
  HasProps := AItem.Properties.Count > 0;

  Hdr := '';
  if AItem.HasDescription then
    Hdr := AppSep(Hdr, LinkToAnchor('Description', AItem.QualifiedName + '-desc'), ' ');
  if HasAncestors then
    Hdr := AppSep(Hdr, LinkToAnchor('Hierarchy', AItem.QualifiedName + '-hier'), ' ');
  if HasFields then
    Hdr := AppSep(Hdr, LinkToAnchor('Fields', AItem.QualifiedName + '-fields'), ' ');
  if HasMethods then
    Hdr := AppSep(Hdr, LinkToAnchor('Methods',  AItem.QualifiedName + '-mths'), ' ');
  if HasProps then
    Hdr := AppSep(Hdr, LinkToAnchor('Properties', AItem.QualifiedName + '-props'), ' ');

  if not Hdr.IsEmpty then
  begin
    //WriteTableHeader(SplitString(Hdr, ' '));
    WithEmptyLine(Hdr);
  end;

  Indent;

  WriteHeading('Declaration');
  WritePara(CodeWithLinks(AItem.MyUnit, ComposeDecl));
  WithEmptyLine();

  if AItem.HasDescription then
  begin
    WriteHeading('Description', AItem.QualifiedName + '-desc');
    WritePara(ItemDescription(AItem));
    WithEmptyLine();
  end;

  WriteDatesAuthor(AItem, True);

  if HasAncestors then
  begin
    WriteHeading('Hierarchy', AItem.QualifiedName + '-hier');
    for I := 0 to Pred(AItem.Ancestors.Count) do
      WriteDirectLine(MdElement[mdvUnorderedList, FVariant] + ' ' + AItem.Ancestors[I].Name);
    WithEmptyLine();
  end;

  if HasFields or HasMethods or HasProps then
  begin
    WriteHeading('Overview');

    Indent;

    if HasFields then
    begin
      //WriteHeading('Fields', AItem.QualifiedName + '-fields');
      WriteDirectLine(FormatAnchor(AItem.QualifiedName + '-fields'));
      WriteTableHeader(['Field'{'Name'}, '', 'Description']);
      for I := 0 to Pred(AItem.Fields.Count) do
        with AItem.Fields.PasItemAt[I] do
          WriteTableRow([
            FormatSubsc(VisToStr(Visibility)),
            CodeWithLinks(AItem, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if HasMethods then
    begin
      //WriteHeading('Methods', AItem.QualifiedName + '-mths');
      WriteDirectLine(FormatAnchor(AItem.QualifiedName + '-mths'));
      WriteTableHeader(['Method'{'Name'}, '', 'Description']);
      for I := 0 to Pred(AItem.Methods.Count) do
        with AItem.Methods.PasItemAt[I] as TPasRoutine do
          with AItem.Methods.PasItemAt[I] do
            WriteTableRow([
              FormatSubsc(VisToStr(Visibility)),
              CodeWithLinks(AItem, FullDeclaration),
              AbstractDescription
            ]);
      WithEmptyLine();
    end;

    if HasProps then
    begin
      //WriteHeading('Properties', AItem.QualifiedName + '-props');
      WriteDirectLine(FormatAnchor(AItem.QualifiedName + '-props'));
      WriteTableHeader(['Property'{'Name'}, '', 'Description']);
      for I := 0 to Pred(AItem.Properties.Count) do
        with AItem.Properties.PasItemAt[I] as TPasProperty do
          with AItem.Properties.PasItemAt[I] do
            WriteTableRow([
              FormatSubsc(VisToStr(Visibility)),
              CodeWithLinks(AItem, FullDeclaration),
              AbstractDescription
            ]);
      WithEmptyLine();
    end;

    UnIndent;

    if HasFields then
    begin
      WriteHeading('Fields');
      Indent;
      for I := 0 to Pred(AItem.Fields.Count) do
        WriteVariable(AItem.Fields.PasItemAt[I]);
      WithEmptyLine();
      UnIndent;
    end;

    if HasMethods then
    begin
      WriteHeading('Methods');
      Indent;
      for I := 0 to Pred(AItem.Methods.Count) do
        WriteRoutine(AItem.Methods.PasItemAt[I] as TPasRoutine);
      WithEmptyLine();
      UnIndent;
    end;

    if HasProps then
    begin
      WriteHeading('Properties');
      Indent;
      for I := 0 to Pred(AItem.Properties.Count) do
        WriteProperty(AItem.Properties.PasItemAt[I] as TPasProperty);
      WithEmptyLine();
      UnIndent;
    end;

    // ???
    for I := 0 to Pred(AItem.Types.Count) do
      WriteType(AItem.Types.PasItemAt[I]);

    for I := 0 to Pred(AItem.Cios.Count) do
      WriteStructure(AItem.Cios.PasItemAt[I] as TPasCio);

  end;

  UnIndent;
end;

procedure TMarkdownDocGenerator.WriteProperty(const AItem: TPasProperty);
begin
  WriteHeading('Property ' + DistinctName(AItem), AItem.QualifiedName);
  WritePasItem(AItem);
end;

procedure TMarkdownDocGenerator.WriteProjHeader;
var
  I: Integer;
  FirstSourceFile: String;
begin
  if FSoleUnit then
    Exit;
  WriteHeading(Title);
  if not Assigned(Units) or (Units.Count < 1) then
    Exit;
  // Put the first file at the front of the units
  FirstSourceFile := PasDoc.SourceFileNames.FirstName;
  for I := 0 to Pred(Units.Count) do
    if (Units.UnitAt[I].SourceFileName = FirstSourceFile) and (I > 0) then
    begin
      Units.Exchange(0, I);
      Break;
    end;
  WriteTableHeader(['Unit', 'File', 'Abstract']);
  for I := 0 to Pred(Units.Count) do
    with Units.UnitAt[I] do
      WriteTableRow([MakeItemLink(Units.UnitAt[I], Name, lcNormal),
        SourceFileName, AbstractDescription]);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteHintDirectives(const AItem: TPasItem);
begin
  if hdDeprecated in AItem.HintDirectives then
    WriteDirect(FormatNote(AppSep('DEPRECATED', AItem.DeprecatedNote, ': ')));
  if hdPlatform in AItem.HintDirectives then
    WriteDirect(FormatNote('PLATFORM SPECIFIC'));
  if hdLibrary in AItem.HintDirectives then
    WriteDirect(FormatNote('LIBRARY SPECIFIC'));
  if hdExperimental in AItem.HintDirectives then
    WriteDirect(FormatNote('EXPERIMENTAL'));
end;

procedure TMarkdownDocGenerator.WriteDescription(const AItem: TPasItem);

  procedure WriteNoDescription;
  begin
    {not imlemented}
  end;

begin
  if AItem.AbstractDescription <> '' then
  begin
    WriteDirect(AItem.AbstractDescription);
    if AItem.DetailedDescription <> '' then
    begin
      if not AItem.AbstractDescriptionWasAutomatic then
        WriteDirect(LE+LE);
      WithEmptyLine(AItem.DetailedDescription);
    end
    else
      WriteDirect(LE+LE);
  end
  else if AItem.DetailedDescription <> '' then
  begin
    WithEmptyLine(AItem.DetailedDescription);
  end
  else
  begin
    if (AItem is TPasItem) then
      WriteNoDescription();
  end;
end;

procedure TMarkdownDocGenerator.WriteSeeAlso(const AItem: TPasItem);
var
  I: Integer;
  SeeAlsoItem: TBaseItem;
  SeeAlsoLink, L, R: String;
begin
  if AItem.SeeAlso.Count < 1 then
    Exit;
  WriteTableHeader(['See also:', '']);
  for I := 0 to Pred(AItem.SeeAlso.Count) do
  begin
    SeeAlsoLink := SearchLink(AItem.SeeAlso[I].Name, AItem,
      AItem.SeeAlso[I].Value, lnfWarn, SeeAlsoItem);
    if SeeAlsoItem <> Nil
      then L := SeeAlsoLink
      else L := AItem.SeeAlso[I].Name;
    if (SeeAlsoItem <> Nil) and (SeeAlsoItem is TPasItem)
      then R := TPasItem(SeeAlsoItem).AbstractDescription
      else R := '';
    WriteTableRow([L, R]);
  end;
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WritePasItem(const AItem: TPasItem);
var
  I: Integer;
  TypeStr, HereLink, LinksText, LinksMore, S, S1, S2, S3, Address: String;
  Links: TStringList;
begin
  TypeStr := ItemTypeStr(AItem);

  // Link to the current item
  HereLink := MakeItemLink(AItem, AItem.Name, lcCode);
  Links := TStringList.Create;
  try
    CodeWithLinks(AItem.MyUnit, AItem.FullDeclaration, Links);
    I := Links.IndexOf(HereLink);
    if I > -1 then
      Links.Delete(I); // Delete link to the current item
    Links.LineBreak := ', ';
    Links.SkipLastLineBreak := True;
    LinksText := Links.Text; // Combine links with commas
  finally
    Links.Free;
  end;

  LinksMore := '';
  // Parent object
  if Assigned(AItem.MyObject) then
    LinksMore := CioTypeToString(AItem.MyObject.MyType) + ' ' +
      MakeItemLink(AItem.MyObject, AItem.MyObject.Name, lcCode);
  // Containing unit
  if Assigned(AItem.MyUnit) then
    LinksMore := AppSep(LinksMore, 'unit ' +
      MakeItemLink(AItem.MyUnit, AItem.MyUnit. Name, lcCode), ', ');

  LinksText := AppSep(LinksText, LinksMore, ', ');

  WriteTableHeader([TypeStr, AItem.Name]);
  WriteTableRow(['declaration', AItem.FullDeclaration]);
  WriteTableRow(['visibility', VisToStr(AItem.Visibility)]);
  if not LinksText.IsEmpty then
    WriteTableRow(['related', LinksText]);
  WriteDatesAuthor(AItem);
  WithEmptyLine();

  WriteHintDirectives(AItem);

  WriteDescription(AItem);
  WithEmptyLine();

  if AItem.Attributes.Count > 0 then
  begin
    WriteTableHeader(['Attribute', 'Value']);
    for I := 0 to Pred(AItem.Attributes.Count) do
      WriteTableRow([AItem.Attributes[I].Name, AItem.Attributes[I].Value]);
  end;

  if AItem.Params.Count > 0 then
  begin
    WriteTableHeader(['Parameter', 'Description']);
    for I := 0 to Pred(AItem.Params.Count) do
      WriteTableRow([AItem.Params[I].Name, AItem.Params[I].Value]);
  end;

  if AItem is TPasRoutine then
    if TPasRoutine(AItem).Returns <> '' then
       WriteTableRow([FormatItalic('(result)'), TPasRoutine(AItem).Returns]);
  WithEmptyLine();

  if AItem.Raises.Count > 0 then
  begin
    WriteTableHeader(['Raises', 'Description']);
    for I := 0 to Pred(AItem.Raises.Count) do
      WriteTableRow([CodeWithLinks(AItem, AItem.Raises[I].Name), AItem.Raises[I].Value]);
  end;

  WriteSeeAlso(AItem);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteDatesAuthor(const AItem: TBaseItem;
  Header: Boolean);
var
  S3, S, S2, S1,
    Address: String;
begin
  if AItem.Created.IsEmpty and AItem.LastMod.IsEmpty and (AItem.Authors.Count < 1) then
    Exit;

  if Header then
    WriteTableHeader(['date', 'value']);

  if not AItem.Created.IsEmpty then
    WriteTableRow(['created', AItem.Created]);
  if not AItem.LastMod.IsEmpty then
    WriteTableRow(['modified', AItem.LastMod]);

  if AItem.Authors.Count > 0 then
  begin
    S3 := 'author' + IfThen(AItem.Authors.Count > 1, 's');
    for S in AItem.Authors do
    begin
      if ExtractEmailAddress(S, S1, S2, Address) then
        S1 := S1 + LinkTo(Address, 'mailto:' + Address) + S2
      else if ExtractWebAddress(s, S1, S2, Address) then
        // ExtractWebAddress is WRONG when there are trailing chars!
        S1 := S1 + LinkTo(Address, 'http://' + Address) + S2
      else
        S1 := S;
      WriteTableRow([S3, S1]);
      S3 := '';
    end;
  end;
  if Header then
    WithEmptyLine();
end;

procedure TMarkdownDocGenerator.HR;
begin
  WriteDirectLine('- - -');
end;

function TMarkdownDocGenerator.BR: String;
begin
  Result := MdElement[mdvLineBreak, FVariant];
end;

procedure TMarkdownDocGenerator.WritePara(const AText: String);
begin
  WriteDirectLine(AText);
end;

procedure TMarkdownDocGenerator.Indent;
begin
  Inc(FIndent);
end;

procedure TMarkdownDocGenerator.UnIndent;
begin
  Dec(FIndent);
end;

function TMarkdownDocGenerator.Hn(AIn: Integer; AAnchor: String): String;
begin
  case FVariant of
    mdvOrig, mdvGithub:
      begin
        Result := StringOfChar('#', FIndent + AIn) + ' ';
        if not AAnchor.IsEmpty then
          // Writing an anschor discards everything to EOL
          ;{Result := Result + Anchor(AAnchor);}
      end;
    mdvRedmine:
      begin
        Result := Format('h%d. ', [FIndent + AIn]);
        // Redmine anchors not working as expected...
        //if AAnchor.IsEmpty
        //  then Result := Format('h%d. ', [FIndent + AIn])
        //  else Result := Format('h%d%s. ', [FIndent + AIn, Anchor(AAnchor)]);
      end;
  otherwise
    Result := '';
  end;
end;

procedure TMarkdownDocGenerator.WriteHeading(AText: String; AAnchor: String);
begin
  case FVariant of
  // the anchor must be after the text in normal mode!
    mdvOrig, mdvGithub: WriteDirect(Hn() + AppendEmptyLine(AppSep(AText, FormatAnchor(AAnchor), ' ')));
    mdvRedmine: WriteDirect(Hn(0, AAnchor) + AppendEmptyLine(AText));
  end;
end;

function TMarkdownDocGenerator.AppendEmptyLine(AText: String): String;
begin
  if EndsStr(LE, AText) or AText.IsEmpty
    then Result := AText + LE
    else Result := AText + LE + LE;
end;

function TMarkdownDocGenerator.GithubAnchor(const Anchor: String): String;
const
  Replacements: array of TCharReplacement = (
    (cChar: ' '; sSpec: '-'),
    (cChar:  '.'; sSpec: '-'),
    (cChar:  #9; sSpec: ''),
    (cChar: #10; sSpec: ''),
    (cChar: #13; sSpec: '')
  );
begin
  Result := Trim(LowerCase(StringReplaceChars(Anchor, Replacements))) + 'Z';
end;

function TMarkdownDocGenerator.LinkTo(AText, ARef: String): String;
begin
  case FVariant of
    mdvOrig, mdvGithub: Result := '[' + AText + '](' + ARef + ')';
    mdvRedmine: Result := '"' + AText + '":' + ARef;
  else
    Result := AText;
  end;
end;

function TMarkdownDocGenerator.LinkToAnchor(AText, AAnchor: String): String;
begin
  case FVariant of
    mdvOrig: Result := LinkTo(AText, '#' + AAnchor);
    mdvGithub: Result := LinkTo(AText, '#' + GithubAnchor(AAnchor));
    //mdvRedmine: '[[' + AText + '|#' + AAnchor + ']]'; // DOESN'T WORK!
  else
    Result := AText;
  end;
end;

procedure TMarkdownDocGenerator.WithEmptyLine(AText: String);
begin
  WriteDirect(AppendEmptyLine(AText));
end;

procedure TMarkdownDocGenerator.WriteTableHeader(AHeaders: TStringArray);
begin
  WriteDirect(FormatTableHeader(AHeaders));
end;

procedure TMarkdownDocGenerator.WriteTableRow(ACells: TStringArray);
begin
  WriteDirect(FormatTableRow(ACells));
end;

constructor TMarkdownDocGenerator.Create(AOwner: TComponent; AFormat: String);
begin
  inherited Create(AOwner);
  case AFormat of
    'markdown-redmine': FVariant := mdvRedmine;
    'markdown-github': FVariant := mdvGithub;
  otherwise
    FVariant := mdvOrig;
  end;
  FIndent := 1;
end;

procedure TMarkdownDocGenerator.WriteDocumentation;
begin
  FSoleUnit := Assigned(Units) and (Units.Count = 1);

  // No project name?
  if not HasProjectName then
    if FSoleUnit then
      // Use the only unit name instead of 'docs'
      ProjectName := Units.UnitAt[0].OutputFileName
    else
    begin
      // More than one unit - name must be specified
      DoMessage(1, pmtError, 'TMarkdownDocGenerator.WriteDocumentation: ' +
        'Project name must be specified.', []);
      Exit;
    end;

  if not CreateStream(ProjectName + GetFileExtension) then
    Exit;

  try
    inherited;
    WriteProjHeader();
    WriteIntroduction();
    WriteUnits(1);
    WriteAdditionalFiles();
    WriteConclusion();
  finally
    CloseStream;
  end;
end;

function TMarkdownDocGenerator.GetFileExtension: String;
begin
  Result := '.md';
end;

end.

