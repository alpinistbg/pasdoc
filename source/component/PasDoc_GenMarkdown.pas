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

  TMarkdownVariant = (mdvOrig, mdvRedmine);

  { TMarkdownDocGenerator }

  TMarkdownDocGenerator = class(TDocGenerator)
  protected
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

    function ItemDescription(Item: TPasItem): String;

    procedure WriteRoutine(const Item: TPasRoutine);
    procedure WriteConstant(const Item: TPasItem);
    procedure WriteVariable(const Item: TPasItem);
    procedure WriteType(const Item: TPasItem);
    procedure WriteStructure(const Item: TPasCIO);
    procedure WriteProperty(const Item: TPasProperty);

    procedure WritePara(const AText: String);

    procedure Indent;
    procedure UnIndent;

    function Hn(AIn: Integer = 0; AAnchor: String = ''): String;
    function AppendEmptyLine(AText: String = ''): String;
    function Anchor(AText: String): String;
    function LinkToAnchor(AText, AAnchor: String): String;
    procedure WriteHeading(AText: String; AAnchor: String = '');
    procedure WithEmptyLine(AText: String = '');
    procedure WriteTableHeader(AHeaders: TStringArray);
    procedure WriteTableRow(ACells: TStringArray);

  public
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
  // orig, redmine
    ('**', '**'), // mdvBoldLeft
    ('**', '**'), // mdvBoldRight
    ( '*', '__'), // mdvItalicLeft
    ( '*', '__'), // mdvItalicRight
    ( '~', '~'),  // mdvSubscriptLeft,
    ( '~', '~'),  // mdvSubscriptRight,
    ( '-', '*'),  // mdvUnorderedList,
    ( '1.', '#'),  // mdvOrderedList,
    ( ':'+LE, ' := '),  // mdvDefinitionList,
    ( '`',  '@'), // mdvInlCodeLeft
    ( '`',  '@'), // mdvInlCodeRight
    ( '```',  '<pre><code class="pascal">'), // mdvLongCodeBegin
    ( '```',  '</code></pre>') // mdvLongCodeEnd
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
  Result := '[' + LinkCaption + '](#' + Item.QualifiedName + ')';
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
    Result := MdElement[mdvInlCodeLeft, FVariant] + S +
      MdElement[mdvInlCodeRight, FVariant];
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

  //FVariant := mdvRedmine;

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
      WriteHeading('Classes, Interfaces, Objects and Records');
      WriteTableHeader(['Name', 'Description']);
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
      WriteHeading('Functions and Procedures');
      WriteTableHeader(['Name', 'Description']);
      for I := 0 to Pred(U.FuncsProcs.Count) do
        with U.FuncsProcs.PasItemAt[I] as TPasRoutine do
          WriteTableRow([
            ReplaceStr(FullDeclaration, Name, MakeItemLink(U.FuncsProcs.PasItemAt[I], Name, lcNormal)),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Types.Count > 0 then
    begin
      WriteHeading('Types');
      WriteTableHeader(['Name', 'Description']);
      for I := 0 to Pred(U.Types.Count) do
        with U.Types.PasItemAt[I] as TPasType do
          WriteTableRow([
            //ReplaceStr(FullDeclaration, Name, MakeItemLink(U.Types.PasItemAt[I], Name, lcNormal) ) +
            CodeWithLinks(U, FullDeclaration),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Constants.Count > 0 then
    begin
      WriteHeading('Constants');
      WriteTableHeader(['Name', 'Description']);
      for I := 0 to Pred(U.Constants.Count) do
        with U.Constants.PasItemAt[I] as TPasConstant do
          WriteTableRow([
            ReplaceStr(FullDeclaration, Name, MakeItemLink(U.Constants.PasItemAt[I], Name, lcNormal)),
            AbstractDescription
          ]);
      WithEmptyLine();
    end;

    if U.Variables.Count > 0 then
    begin
      WriteHeading('Variables');
      WriteTableHeader(['Name', 'Description']);
      for I := 0 to Pred(U.Variables.Count) do
        with U.Variables.PasItemAt[I] do
          WriteTableRow([
            ReplaceStr(FullDeclaration, Name, MakeItemLink(U.Variables.PasItemAt[I], Name, lcNormal)),
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
      for I := 0 to Pred(U.CIOs.Count) do
        WriteStructure(U.CIOs.PasItemAt[I] as TPasCIO);
      WithEmptyLine();
    end;

    if U.FuncsProcs.Count > 0 then
    begin
      WriteHeading('Functions and Procedures');
      for I := 0 to Pred(U.FuncsProcs.Count) do
        WriteRoutine(U.FuncsProcs.PasItemAt[I] as TPasRoutine);
      WithEmptyLine();
    end;

    if U.Types.Count > 0 then
    begin
      WriteHeading('Types');
      for I := 0 to Pred(U.Types.Count) do
        WriteType(U.Types.PasItemAt[I] as TPasType);
      WithEmptyLine();
    end;

    if U.Constants.Count > 0 then
    begin
      WriteHeading('Constants');
      for I := 0 to Pred(U.Constants.Count) do
        WriteConstant(U.Constants.PasItemAt[I] as TPasConstant);
      WithEmptyLine();
    end;

    if U.Variables.Count > 0 then
    begin
      WriteHeading('Variables');
      for I := 0 to Pred(U.Variables.Count) do
        WriteVariable(U.Variables.PasItemAt[I]);
      WithEmptyLine();
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
  // TODO
  Result := Format(':anchor: "%s"', [Anchor]);
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
begin
  WriteHeading(
    RoutineTypeToString(Item.What) + ' ' + ConvertString(Item.Name),
    Item.QualifiedName);

  WriteDirectLine(
      //'| | |' + LE +
      '| name | ' + ConvertString(Item.Name) + ' |' + LE +
      '|---:|---|' + LE +
      //'| type | ' + ConvertString(RoutineTypeToString(Item.What)) + ' |' + LE +
      '| declaration | ' + ConvertString(CodeString(Item.FullDeclaration)) + ' |' + LE +
      '| visibility | ' + VisToStr(Item.Visibility) + ' |' );

  if Item.Params.Count > 0 then
  begin
    WithEmptyLine();
    WriteDirectLine('| name | desc |' + LE + '|---|---|');
    for I := 0 to Pred(Item.Params.Count) do
      WriteDirectLine(
        '| ' + ConvertString(item.params[i].name) + ' | ' +
          item.params[i].value + ' |');
  end;

  if Item.Returns <> '' then
    WriteDirectLine(
        '| *(result)* | ' + Item.Returns + ' |');

  WithEmptyLine();

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
end;

procedure TMarkdownDocGenerator.WriteConstant(const Item: TPasItem);
begin
  WriteHeading('Constant ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteDirectLine(
    //'| - | - |' + LE +
    '| name | ' + ConvertString(Item.Name) + ' |' + LE +
    '|---:|---|' + LE +
    '| declaration | ' + ConvertString(CodeString(Item.FullDeclaration)) + ' |' + LE +
    '| visibility | ' + VisToStr(Item.visibility) + ' |');

  WithEmptyLine();

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
end;

procedure TMarkdownDocGenerator.WriteVariable(const Item: TPasItem);
begin
  WriteHeading('Variable ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteDirectLine(
    '| | |' + LE + '|---|---|' + LE +
    '| var name | ' + ConvertString(Item.Name) + ' |' + LE +
    '| declaration | ' + ConvertString(Item.FullDeclaration) + ' |' + LE +
    '| visibility | ' + VisToStr(Item.visibility) + ' |');

  WithEmptyLine();

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
end;

procedure TMarkdownDocGenerator.WriteType(const Item: TPasItem);

  procedure WriteEnumMembers(const Item: TPasEnum);
  var
    I: Integer;
  begin
    for I := 0 to Item.Members.Count - 1 do
      WriteConstant(Item.Members.PasItemAt[i]);
  end;

begin
  WriteHeading('Type ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteDirectLine(
    '| | |' + LE + '|---|---|' + LE +
    '| type name | ' + ConvertString(item.Name) + ' |' + LE +
    '| declaration | ' + ConvertString(item.FullDeclaration)  + ' |' + LE +
    '| visibility | ' + VisToStr(item.visibility)  + ' |');

  WithEmptyLine();

  if Item.HasDescription then
    WritePara(ItemDescription(Item));

  if Item is TPasEnum then
  begin
    Indent;
    WriteEnumMembers(TPasEnum(Item));
    UnIndent;
  end;
  WithEmptyLine();

end;

procedure TMarkdownDocGenerator.WriteStructure(const Item: TPasCIO);
var
  I: Integer;
begin
  WriteHeading(
    CioTypeToString(Item.MyType) + ' ' + ConvertString(Item.Name),
    Item.QualifiedName);

  WithEmptyLine(

    LinkToAnchor('Description', Item.QualifiedName + '-desc') + ' ' +
    LinkToAnchor('Hierarchy', Item.QualifiedName + '-hier') + ' ' +
    LinkToAnchor('Fields', Item.QualifiedName + '-fields') + ' ' +
    LinkToAnchor('Methods',  Item.QualifiedName + '-mths') + ' ' +
    LinkToAnchor('Properties', Item.QualifiedName + '-props'));


    //WriteDirectLine(
    //'| | |' + LE + '|---|---|' + LE +
    //'| structure name | ' + ConvertString(Item.name) + ' |' + LE +
    //'| name_with_generic | ' + ConvertString(Item.NameWithGeneric) + ' |' + LE +
    //'| type | ' + ConvertString(CioTypeToString(Item.MyType)) + ' |' + LE +
    //'| visibility | ' + VisToStr(Item.visibility) + ' |');

  Indent;

  // No Item.FullDeclaration? How it is handled in HTML?
  WriteHeading('Declaration');

  FormatPascalCode(Item.FullDeclaration);
  WithEmptyLine();

  WriteHeading('Description', Item.QualifiedName + '-desc');

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
  WithEmptyLine();

  WriteHeading('Hierarchy', Item.QualifiedName + '-hier');

  for I := 0 to Pred(Item.Ancestors.Count) do
    //WriteDirectLine(
    //  '| | |' + LE + '|---|---|' + LE +
    //  '| ancestor name | ' + ConvertString(Item.Ancestors[I].Name) + ' |' + LE +
    //  '| declaration | ' + ConvertString(Item.Ancestors[I].Value) + ' |');
    WriteDirectLine(MdElement[mdvUnorderedList, FVariant] + ' ' + Item.Ancestors[I].Name);
  WithEmptyLine();

  if (Item.Fields.Count > 0) or (Item.Methods.Count > 0) or
    (Item.Properties.Count > 0)
  then
  begin
    WriteHeading('Overview');

    Indent;

    if Item.Fields.Count > 0 then
    begin
      WriteHeading('Fields', Item.QualifiedName + '-fields');
      WriteDirectLine('|| Name | Description |' + LE + '|---|:---|:---|');
      for I := 0 to Pred(Item.Fields.Count) do
        with Item.Fields.PasItemAt[I] do
          WritePara(
            '|' + FormatSubsc(VisToStr(Visibility)) +
            '|' + CodeWithLinks(Item, FullDeclaration) +
            '|' + AbstractDescription +
            '|');
      WithEmptyLine();
    end;

    if Item.Methods.Count > 0 then
    begin
      WriteHeading('Methods', Item.QualifiedName + '-mths');
      WriteDirectLine('|| Name | Description |' + LE + '|---|:---|:---|');
      for I := 0 to Pred(Item.Methods.Count) do
        with Item.Methods.PasItemAt[I] as TPasRoutine do
          WritePara(
            '|' + FormatSubsc(VisToStr(Visibility)) +
            '|' + CodeWithLinks(Item, FullDeclaration) +
            '|' + AbstractDescription +
            '|');
      WithEmptyLine();
    end;

    if Item.Properties.Count > 0 then
    begin
      WriteHeading('Properties', Item.QualifiedName + '-props');
      WriteDirectLine('|| Name | Description |' + LE + '|---|:---|:---|');
      for I := 0 to Pred(Item.Properties.Count) do
        with Item.Properties.PasItemAt[I] as TPasProperty do
          WritePara(
            '|' + FormatSubsc(VisToStr(Visibility)) +
            '|' + CodeWithLinks(Item, FullDeclaration) +
            '|' + AbstractDescription +
            '|');
      WithEmptyLine();
    end;

    UnIndent;

    WriteHeading('Fields');

    for I := 0 to Pred(Item.Fields.Count) do
      WriteVariable(Item.Fields.PasItemAt[I]);
    WithEmptyLine();

    WriteHeading('Methods');

    //for I := 0 to Item.Methods.Count - 1 do
    //  with Item.Methods.PasItemAt[I] as TPasRoutine do
    //    WriteDirectLine('- [' + Name + '](#' + QualifiedName + ')');
    for I := 0 to Item.Methods.Count - 1 do
      WriteRoutine(Item.Methods.PasItemAt[I] as TPasRoutine);
    WithEmptyLine();

    WriteHeading('Properties');

    for I := 0 to Item.Properties.Count - 1 do
      WriteProperty(Item.Properties.PasItemAt[I] as TPasProperty);
    WithEmptyLine();

    //???

    for I := 0 to Item.Types.Count - 1 do
      WriteType(Item.Types.PasItemAt[I]);

    for I := 0 to Item.Cios.Count - 1 do
      WriteStructure(Item.Cios.PasItemAt[I] as TPasCio);

    UnIndent;

  end;
end;

procedure TMarkdownDocGenerator.WriteProperty(const Item: TPasProperty);
begin
  WriteHeading('Property ' + ConvertString(Item.Name), Item.QualifiedName);
  WriteDirectLine(
    '| | |' + LE + '|---|---|' + LE +
    '| property name | ' + ConvertString(Item.name) + ' |' + LE +
    '| indexdecl | ' + ConvertString(Item.indexDecl) + ' |' + LE +
    '| type | ' + ConvertString(Item.Proptype) + ' |' + LE +
    '| reader | ' + ConvertString(Item.reader) + ' |' + LE +
    '| writer | ' + ConvertString(Item.writer) + ' |' + LE +
    '| default_in_class | ' + ConvertString(BoolToStr(Item.DefaultInClass, True)) + ' |' + LE +
    '| default_value | ' + ConvertString(Item.DefaultValue) + ' |' + LE +
    '| nodefault | ' + ConvertString(BoolToStr(item.NoDefault, True)) + ' |' + LE +
    '| stored | ' + ConvertString(Item.Stored) + ' |' + LE +
    '| visibility | ' + VisToStr(Item.Visibility) + ' |');

  WithEmptyLine();

  if Item.HasDescription then
    WritePara(ItemDescription(Item));

  WithEmptyLine();
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
    mdvOrig:
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
    mdvOrig: WriteDirect(Hn() + AppendEmptyLine(AppSep(AText, Anchor(AAnchor), ' ')));
    mdvRedmine: WriteDirect(Hn(0, AAnchor) + AppendEmptyLine(AText));
  end;
end;

function TMarkdownDocGenerator.AppendEmptyLine(AText: String): String;
begin
  if EndsStr(LE, AText) or AText.IsEmpty
    then Result := AText + LE
    else Result := AText + LE + LE;
end;

function TMarkdownDocGenerator.Anchor(AText: String): String;
begin
  Result := '';
  if AText.IsEmpty then
    Exit;
  case FVariant of
    mdvOrig: Result := '{#' + AText + '}';
    mdvRedmine: Result := '(#' + AText + ')';
  end;
end;

function TMarkdownDocGenerator.LinkToAnchor(AText, AAnchor: String): String;
begin
  case FVariant of
    mdvOrig: Result := '[' + AText + '](#' + AAnchor + ')';
    //mdvRedmine: '[[' + AText + '|#' + AAnchor + ']]'; // DOESN'T WORK!
  else
    Result := '';
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
    mdvOrig:
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

procedure TMarkdownDocGenerator.WriteDocumentation;
begin
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

