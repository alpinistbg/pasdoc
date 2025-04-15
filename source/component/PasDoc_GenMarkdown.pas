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
  PasDoc_StringPairVector;

type

  TMarkdownVariant = (mdvOrig, mdvRedmine, mdvGithub);

  { TMarkdownDocGenerator }

  TMarkdownDocGenerator = class(TDocGenerator)
  protected
    function OneLineCodeString(const S: String): String;
    function InlineCodeString(const S: String): String;
    function CodeString(const S: String): String; override;
    function CodeWithLinks(const AItem: TPasItem; const ACode: String): String;
    function ConvertString(const S: String): String; override;
    function ConvertChar(C: Char): String; override;
    procedure WriteUnit(const HL: Integer; const U: TPasUnit); override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;
    function FormatSection(HL: integer; const Anchor: String;
      const Caption: String): String; override;
    function FormatAnchor(const Anchor: String): String; override;
    function FormatTable(Table: TTableData): String; override;
    function FormatList(ListData: TListData): String; override;
    function FormatBold(const Text: String): String; override;
    function FormatItalic(const Text: String): String; override;
    function FormatSubsc(const Text: String): String;
    function FormatNote(const Text: string): String; override;
    function FormatPascalCode(const Line: String): String; override;

    function MakeItemLink(const Item: TBaseItem; const LinkCaption: String;
      const LinkContext: TLinkContext): String; override;
  private
    FVariant: TMarkdownVariant;
    FPara: String;
    FIndent: Integer;

    function GithubAnchor(const Anchor: String): String;

    function ItemDescription(Item: TPasItem): String;

    procedure WriteRoutine(const Item: TPasRoutine);
    procedure WriteConstant(const Item: TPasItem);
    procedure WriteVariable(const Item: TPasItem);
    procedure WriteType(const Item: TPasItem);
    procedure WriteStructure(const Item: TPasCIO);
    procedure WriteProperty(const Item: TPasProperty);

    procedure WriteHintDirectives(const AItem: TPasItem);
    procedure WriteDescription(const AItem: TPasItem);
    procedure WriteSeeAlso(const AItem: TPasItem);

    procedure HR;
    procedure WritePara(const AText: String);

    procedure Indent;
    procedure UnIndent;

    function Hn(AIn: Integer = 0; AAnchor: String = ''): String;
    function AppendEmptyLine(AText: String = ''): String;
    //function Anchor(AText: String): String;
    function LinkToAnchor(AText, AAnchor: String): String;
    procedure WriteHeading(AText: String; AAnchor: String = '');
    procedure WithEmptyLine(AText: String = '');
    procedure WriteTableHeader(AHeaders: TStringArray);
    procedure WriteTableRow(ACells: TStringArray);

  public
    constructor Create(AOwner: TComponent); override;
    procedure WriteDocumentation; override;
    function GetFileExtension: String; override;
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
    mdvLongCodeEnd
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
    ( '```',  '</code></pre>', '```') // mdvLongCodeEnd
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

function TMarkdownDocGenerator.MakeItemLink(const Item: TBaseItem;
  const LinkCaption: String; const LinkContext: TLinkContext): String;
begin
  Result := LinkToAnchor(LinkCaption, Item.QualifiedName); //'[' + LinkCaption + '](#' + Item.QualifiedName + ')';
end;

function TMarkdownDocGenerator.OneLineCodeString(const S: String): String;
begin
  if IsML(S)
    then Result := InlineCodeString(ReplaceStr(S, LE, ' '))
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
  const ACode: String): String;
const
  IdentDelims: TSysCharSet = [Low(TSysCharSet)..High(TSysCharSet)] -
    ['.', '_', '0'..'9', 'A'..'Z', 'a'..'z'];
  IdentNonStart: TSysCharSet = ['.', '0'..'9'];
var
  P: Integer = 1;
  FoundPos: Integer;
  S, LinkText: String;
  FoundItem: TBaseItem;
begin
  Result := ACode;
  // Redmine links not working so far
  if FVariant = mdvRedmine then
    Exit;
  repeat
    FoundPos := P;
    S := ExtractSubstr(Result, P, IdentDelims);
    if S.IsEmpty or (S[1] in IdentNonStart) then
      Break;
    FoundItem := SearchItem(S, AItem, False);
    if Assigned(FoundItem) then
    begin
      LinkText := MakeItemLink(FoundItem, S, lcNormal);
      if not LinkText.IsEmpty then
      begin
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
  if not Assigned(U) then begin
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

  if not CreateStream(U.OutputFileName + GetFileExtension) then
    Exit;

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  FIndent := HL;

  WriteHeading('Unit ' + U.Name);

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
        with U.Types.PasItemAt[I] as TPasType do
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
        WriteType(U.Types.PasItemAt[I] as TPasType);
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

  UnIndent;
end;

procedure TMarkdownDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem; const Id: TTranslationID);
begin
  // TODO
end;

function TMarkdownDocGenerator.FormatSection(HL: integer; const Anchor: String;
  const Caption: String): String;
begin
  Result := '';
  // TODO
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

function TMarkdownDocGenerator.FormatTable(Table: TTableData): String;
const
  RowElement: array [Boolean] of string = ('| ', '| ');
var
  RowNum, ColNum: Integer;
  Row: TRowData;
begin
  Result := LE + LE;
  // TODO
  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum] as TRowData;
    Result := Result + RowElement[Row.Head] + LE;
    for ColNum := 0 to Row.Cells.Count - 1 do
      Result := Result + Row.Cells[ColNum] + ' ';
    Result := Result + ' |' + LE;
  end;
  Result := Result + LE + LineEnding;
end;

function TMarkdownDocGenerator.FormatList(ListData: TListData): String;
const
  ListTag: array[TListType] of String =
  ( '- ', '1. ', '' );
var
  I: Integer;
  ListItem: TListItemData;
begin
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

function TMarkdownDocGenerator.ItemDescription(Item: TPasItem): String;
var
  Abstract, Detailed: String;
begin
  if Item.HasDescription then
  begin
    Result := '';
    if Item.AbstractDescription <> '' then
      Result := Result + Item.AbstractDescription + LE + LE;
    if Item.DetailedDescription <> '' then
      Result := Result + Item.DetailedDescription + LE;
  end
  else
    Result := '';
end;

procedure TMarkdownDocGenerator.WriteRoutine(const Item: TPasRoutine);
var
  I: Integer;
  TypeStr: String;
begin
  TypeStr := RoutineTypeToString(Item.What);
  WriteHeading(TypeStr + ' ' + ConvertString(Item.Name), Item.QualifiedName);

  WriteTableHeader([TypeStr{'name'}, ConvertString(Item.Name)]);
  WriteTableRow(['declaration', ConvertString(OneLineCodeString(Item.FullDeclaration))]);
  WriteTableRow(['visibility', VisToStr(Item.Visibility)]);
  WithEmptyLine();

  WriteHintDirectives(Item);

  if Item.Params.Count > 0 then
  begin
    WriteTableHeader(['Parameter'{'name'}, 'Description']);
    for I := 0 to Pred(Item.Params.Count) do
      WriteTableRow([ConvertString(item.params[i].name), item.params[i].value]);
  end;

  if Item.Returns <> '' then
     WriteTableRow([FormatItalic('(result)'), Item.Returns]);
  WithEmptyLine();

  WriteDescription(Item);
  WithEmptyLine();

  WriteSeeAlso(Item);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteConstant(const Item: TPasItem);
begin
  WriteHeading('Constant ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteTableHeader(['Name', ConvertString(Item.Name)]);
  WriteTableRow(['declaration', ConvertString(OneLineCodeString(Item.FullDeclaration))]);
  WriteTableRow(['visibility', VisToStr(Item.Visibility)]);
  WithEmptyLine();

  WriteDescription(Item);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteVariable(const Item: TPasItem);
begin
  WriteHeading('Variable ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteTableHeader(['Name', ConvertString(Item.Name)]);
  WriteTableRow(['declaration', ConvertString(OneLineCodeString(Item.FullDeclaration))]);
  WriteTableRow(['visibility', VisToStr(Item.Visibility)]);
  WithEmptyLine();

  WriteDescription(Item);
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.WriteType(const Item: TPasItem);

  procedure WriteEnumMembers(const Item: TPasEnum);
  var
    I: Integer;
  begin
    WriteTableHeader(['Enum', 'Declaration', 'Comment']);
    for I := 0 to Pred(Item.Members.Count) do
      with Item.Members.PasItemAt[I] do
      //WriteConstant(Item.Members.PasItemAt[i]);
      WriteTableRow([Name, ConvertString(OneLineCodeString(FullDeclaration)), AbstractDescription]);
  end;

begin
  WriteHeading('Type ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteTableHeader(['Name', ConvertString(Item.Name)]);
  WriteTableRow(['declaration', ConvertString(OneLineCodeString(Item.FullDeclaration))]);
  WriteTableRow(['visibility', VisToStr(Item.Visibility)]);
  WithEmptyLine();

  WriteDescription(Item);
  WithEmptyLine();

  if Item is TPasEnum then
  begin
    Indent;
    WriteEnumMembers(TPasEnum(Item));
    UnIndent;
  end;
end;

procedure TMarkdownDocGenerator.WriteStructure(const Item: TPasCIO);
var
  I: Integer;
  HasAncestors, HasFields, HasMethods, HasProps: Boolean;
  Hdr: String;

  function ComposeDecl: String;
  var
    I: Integer;
    AncestorList: String;
  begin
    Result := 'type ' + Item.NameWithGeneric + ' = ' +
      CioTypeToString(Item.MyType) +
      GetClassDirectiveName(Item.ClassDirective);
    if Item.Ancestors.Count > 0 then
    begin
      AncestorList := '';
      for I := 0 to Pred(Item.Ancestors.Count) do
        AncestorList := AppSep(AncestorList, Item.Ancestors[I].Value, ', ');
      Result := Result + '(' + AncestorList + ')';
    end;
    if Item.ClassDirective = CT_HELPER then
      Result := AppSep(Result, Item.HelperTypeIdentifier, ' for ');
  end;

begin
  WriteHeading(
    CioTypeToString(Item.MyType) + ' ' + ConvertString(Item.Name),
    Item.QualifiedName);
  HR;

  HasAncestors := Item.Ancestors.Count > 0;
  HasFields := Item.Fields.Count > 0;
  HasMethods := Item.Methods.Count > 0;
  HasProps := Item.Properties.Count > 0;

  Hdr := '';
  if Item.HasDescription then
    Hdr := AppSep(Hdr, LinkToAnchor('Description', Item.QualifiedName + '-desc'), ' ');
  if HasAncestors then
    Hdr := AppSep(Hdr, LinkToAnchor('Hierarchy', Item.QualifiedName + '-hier'), ' ');
  if HasFields then
    Hdr := AppSep(Hdr, LinkToAnchor('Fields', Item.QualifiedName + '-fields'), ' ');
  if HasMethods then
    Hdr := AppSep(Hdr, LinkToAnchor('Methods',  Item.QualifiedName + '-mths'), ' ');
  if HasProps then
    Hdr := AppSep(Hdr, LinkToAnchor('Properties', Item.QualifiedName + '-props'), ' ');

  if not Hdr.IsEmpty then
  begin
    //WriteTableHeader(SplitString(Hdr, ' '));
    WithEmptyLine(Hdr);
  end;

  Indent;

  WriteHeading('Declaration');
  WritePara(CodeWithLinks(Item.MyUnit, ComposeDecl));
  WithEmptyLine();

  if Item.HasDescription then
  begin
    WriteHeading('Description', Item.QualifiedName + '-desc');
    WritePara(ItemDescription(Item));
    WithEmptyLine();
  end;

  if HasAncestors then
  begin
    WriteHeading('Hierarchy', Item.QualifiedName + '-hier');
    for I := 0 to Pred(Item.Ancestors.Count) do
      WriteDirectLine(MdElement[mdvUnorderedList, FVariant] + ' ' + Item.Ancestors[I].Name);
    WithEmptyLine();
  end;

  if HasFields or HasMethods or HasProps then
  begin
    WriteHeading('Overview');

    Indent;

    if HasFields then
    begin
      //WriteHeading('Fields', Item.QualifiedName + '-fields');
      WriteDirectLine(FormatAnchor(Item.QualifiedName + '-fields'));
      WriteTableHeader(['Field'{'Name'}, '', 'Description']);
      for I := 0 to Pred(Item.Fields.Count) do
        with Item.Fields.PasItemAt[I] do
          WriteTableRow([
            FormatSubsc(VisToStr(Visibility)),
            CodeWithLinks(Item, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if HasMethods then
    begin
      //WriteHeading('Methods', Item.QualifiedName + '-mths');
      WriteDirectLine(FormatAnchor(Item.QualifiedName + '-mths'));
      WriteTableHeader(['Method'{'Name'}, '', 'Description']);
      for I := 0 to Pred(Item.Methods.Count) do
        with Item.Methods.PasItemAt[I] as TPasRoutine do
          with Item.Methods.PasItemAt[I] do
            WriteTableRow([
              FormatSubsc(VisToStr(Visibility)),
              CodeWithLinks(Item, FullDeclaration),
              AbstractDescription
            ]);
      WithEmptyLine();
    end;

    if HasProps then
    begin
      //WriteHeading('Properties', Item.QualifiedName + '-props');
      WriteDirectLine(FormatAnchor(Item.QualifiedName + '-props'));
      WriteTableHeader(['Property'{'Name'}, '', 'Description']);
      for I := 0 to Pred(Item.Properties.Count) do
        with Item.Properties.PasItemAt[I] as TPasProperty do
          with Item.Properties.PasItemAt[I] do
            WriteTableRow([
              FormatSubsc(VisToStr(Visibility)),
              CodeWithLinks(Item, FullDeclaration),
              AbstractDescription
            ]);
      WithEmptyLine();
    end;

    UnIndent;

    if HasFields then
    begin
      WriteHeading('Fields');
      Indent;
      for I := 0 to Pred(Item.Fields.Count) do
        WriteVariable(Item.Fields.PasItemAt[I]);
      WithEmptyLine();
      UnIndent;
    end;

    if HasMethods then
    begin
      WriteHeading('Methods');
      Indent;
      for I := 0 to Pred(Item.Methods.Count) do
        WriteRoutine(Item.Methods.PasItemAt[I] as TPasRoutine);
      WithEmptyLine();
      UnIndent;
    end;

    if HasProps then
    begin
      WriteHeading('Properties');
      Indent;
      for I := 0 to Pred(Item.Properties.Count) do
        WriteProperty(Item.Properties.PasItemAt[I] as TPasProperty);
      WithEmptyLine();
      UnIndent;
    end;

    // ???
    for I := 0 to Pred(Item.Types.Count) do
      WriteType(Item.Types.PasItemAt[I]);

    for I := 0 to Pred(Item.Cios.Count) do
      WriteStructure(Item.Cios.PasItemAt[I] as TPasCio);

  end;
  UnIndent;
end;

procedure TMarkdownDocGenerator.WriteProperty(const Item: TPasProperty);
begin
  WriteHeading('Property ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteTableHeader(['Name', ConvertString(Item.Name)]);
  WriteTableRow(['declaration', ConvertString(OneLineCodeString(Item.FullDeclaration))]);
  WriteTableRow(['visibility', VisToStr(Item.Visibility)]);
  WithEmptyLine();

  WriteDescription(Item);
  WithEmptyLine();

  //WriteDirectLine(
  //  '| | |' + LE + '|---|---|' + LE +
  //  '| property name | ' + ConvertString(Item.name) + ' |' + LE +
  //  '| indexdecl | ' + ConvertString(Item.indexDecl) + ' |' + LE +
  //  '| type | ' + ConvertString(Item.Proptype) + ' |' + LE +
  //  '| reader | ' + ConvertString(Item.reader) + ' |' + LE +
  //  '| writer | ' + ConvertString(Item.writer) + ' |' + LE +
  //  '| default_in_class | ' + ConvertString(BoolToStr(Item.DefaultInClass, True)) + ' |' + LE +
  //  '| default_value | ' + ConvertString(Item.DefaultValue) + ' |' + LE +
  //  '| nodefault | ' + ConvertString(BoolToStr(item.NoDefault, True)) + ' |' + LE +
  //  '| stored | ' + ConvertString(Item.Stored) + ' |' + LE +
  //  '| visibility | ' + VisToStr(Item.Visibility) + ' |');

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
      else L := ConvertString(AItem.SeeAlso[I].Name);
    if (SeeAlsoItem <> Nil) and (SeeAlsoItem is TPasItem)
      then R := TPasItem(SeeAlsoItem).AbstractDescription
      else R := '';
    WriteTableRow([L, R]);
  end;
  WithEmptyLine();
end;

procedure TMarkdownDocGenerator.HR;
begin
  WriteDirectLine('- - -');
end;

procedure TMarkdownDocGenerator.WritePara(const AText: String);
begin
  WriteDirectLine({FPara +} AText);
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
  Replacements: array{[0..4]} of TCharReplacement = (
    (cChar: ' '; sSpec: '-'),
    (cChar:  '.'; sSpec: '-'),
    (cChar:  #9; sSpec: ''),
    (cChar: #10; sSpec: ''),
    (cChar: #13; sSpec: '')
  );
begin
  Result := Trim(LowerCase(StringReplaceChars(Anchor, Replacements))) + 'Z';
end;

function TMarkdownDocGenerator.LinkToAnchor(AText, AAnchor: String): String;
begin
  case FVariant of
    mdvOrig: Result := '[' + AText + '](#' + AAnchor + ')';
    mdvGithub: Result := '[' + AText + '](#' + GithubAnchor(AAnchor) + ')';
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
        WriteDirectLine(H + '|');
        WriteDirectLine(L + '|');
      end;

    mdvRedmine:
      begin
        H := '';
        for S in AHeaders do
          H := H + '|_. ' + S;
        WriteDirectLine(H + '|');
      end;

  end;
end;

procedure TMarkdownDocGenerator.WriteTableRow(ACells: TStringArray);
var
  R, S: String;
begin
  R := '';
  for S in ACells do
    R := R + '|' + S;
  WriteDirectLine(R + '|');
end;

constructor TMarkdownDocGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariant := mdvGithub;
end;

procedure TMarkdownDocGenerator.WriteDocumentation;
begin
  FVariant := mdvGithub;

  //StartSpellChecking('sgml');
  inherited;
  WriteUnits(1);
  WriteIntroduction;
  WriteAdditionalFiles;
  WriteConclusion;
  //EndSpellChecking;
end;

function TMarkdownDocGenerator.GetFileExtension: String;
begin
  Result := '.md';
end;

end.

