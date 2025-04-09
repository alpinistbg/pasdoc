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

  { TMarkdownDocGenerator }

  TMarkdownDocGenerator = class(TDocGenerator)
  protected
    function CodeString(const S: String): String; override;
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
    function FormatNote(const Text: string): string; override;
    function FormatPascalCode(const Line: string): string; override;

    function MakeItemLink(const Item: TBaseItem; const LinkCaption: string;
      const LinkContext: TLinkContext): string; override;
  private
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

    function Hn(AIn: Integer = 0): String;
  public
    procedure WriteDocumentation; override;
    function GetFileExtension: String; override;
  end;

implementation

uses
  StrUtils;

function AppSep(S1, S2, ASep: String): String;
begin
  if S2 <> '' then
    Result := S1 + IfThen(S1 <> '', ASep) + S2
  else
    Result := S1;
end;

function IsML(S: String): Boolean;
begin
  Result := PosSet(LineEnding, S) > 0;
end;

{ TMarkdownDocGenerator }

function TMarkdownDocGenerator.FormatBold(const Text: String): String;
begin
  Result := '**' + Text + '**';
end;

function TMarkdownDocGenerator.FormatItalic(const Text: String): String;
begin
  Result := '*' + Text + '*';
end;

function TMarkdownDocGenerator.FormatNote(const Text: string): string;
begin
  Result := '> ' + Text + LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.FormatPascalCode(const Line: string): string;
begin
  Result := LineEnding + CodeString(inherited FormatPascalCode(Line));
end;

function TMarkdownDocGenerator.MakeItemLink(const Item: TBaseItem;
  const LinkCaption: string; const LinkContext: TLinkContext): string;
begin
  Result := //inherited MakeItemLink(Item, LinkCaption, LinkContext);
    '[' + LinkCaption + '](#' + Item.QualifiedName + ')';
end;

function TMarkdownDocGenerator.CodeString(const S: String): String;
begin
  if IsML(S)
    then Result := '```' + IfThen(not StartsStr(LineEnding, S), LineEnding) + S + LineEnding + '```' + LineEnding
    else Result := '`' + S + '`';
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
  WriteDirectLine(Hn() + 'Unit ' + U.Name + LineEnding);  //ConvertString(U.SourceFileName));

  if U.HasDescription then
    WritePara(ItemDescription(U));

  // Used units
  if WriteUsesClause and not IsEmpty(U.UsesUnits) then
  begin
    S := '';
    for I := 0 to Pred(U.UsesUnits.Count) do
      S := AppSep(S, U.UsesUnits[I], ', ');
    WritePara(LineEnding + 'Uses' + LineEnding + ': ' + S);
  end;

  WriteDirectLine(LineEnding);
  WriteDirectLine(LineEnding);

  Indent;

  // Global constants
  for I := 0 to U.Constants.Count - 1 do
    WriteConstant(U.Constants.PasItemAt[I]);

  // Global types
  for I := 0 to u.Types.Count - 1 do
    WriteType(U.Types.PasItemAt[I]);

  // Global classes
  for I := 0 to U.CIOs.Count - 1 do
    WriteStructure(U.CIOs.PasItemAt[I] as TPasCIO);

  // Global functions
  if U.FuncsProcs.Count > 0 then
  begin
    for I := 0 to Pred(U.FuncsProcs.Count) do
      WriteRoutine(U.FuncsProcs.PasItemAt[I] as TPasRoutine);
  end;

  // Global vars
  for I := 0 to U.Variables.Count - 1 do
    WriteVariable(U.Variables.PasItemAt[I]);

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
  Result := LineEnding + LineEnding;
  // TODO
  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum] as TRowData;
    Result := Result + RowElement[Row.Head] + LineEnding;
    for ColNum := 0 to Row.Cells.Count - 1 do
      Result := Result + Row.Cells[ColNum] + ' ';
    Result := Result + ' |' + LineEnding;
  end;
  Result := Result + LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.FormatList(ListData: TListData): String;
const
  ListTag: array[TListType] of String =
  ( '- ', '1. ', '' );
var
  I: Integer;
  ListItem: TListItemData;
begin
  Result := LineEnding + LineEnding;
  for I := 0 to ListData.Count - 1 do
  begin
    ListItem := ListData.Items[I] as TListItemData;
    if ListData.ListType = ltDefinition then
    begin
      Result := Result + ListItem.ItemLabel + LineEnding + ': ' + ListItem.Text;
      //Result := Result + AppSep(ListItem.ItemLabel, ListItem.Text, LineEnding + ': ');
      //if IsML(Result) then
      //  Result := Result + '=:';
    end
    else
      Result := Result + ListItem.Text;
    Result := Result + LineEnding;
  end;
  Result := Result + LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.ItemDescription(Item: TPasItem): String;
var
  Abstract, Detailed: String;
begin
  if Item.HasDescription then
  begin
    Result := '';
    if Item.AbstractDescription <> '' then
      Result := Result + Item.AbstractDescription + LineEnding + LineEnding;
    if Item.DetailedDescription <> '' then
      Result := Result + Item.DetailedDescription + LineEnding;
  end
  else
    Result := '';
end;

procedure TMarkdownDocGenerator.WriteRoutine(const Item: TPasRoutine);
var
  I: Integer;
begin
  WritePara(
    Hn() + RoutineTypeToString(Item.What) + ' ' + ConvertString(Item.Name) +
    ' {#' + Item.QualifiedName + '}' +
    LineEnding);

  WriteDirectLine(
      //'| | |' + LineEnding +
      '| name | ' + ConvertString(Item.Name) + ' |' + LineEnding +
      '|---:|---|' + LineEnding +
      //'| type | ' + ConvertString(RoutineTypeToString(Item.What)) + ' |' + LineEnding +
      '| declaration | ' + ConvertString(CodeString(Item.FullDeclaration)) + ' |' + LineEnding +
      '| visibility | ' + VisToStr(Item.Visibility) + ' |' );

  if Item.Params.Count > 0 then
  begin
    WriteDirectLine(LineEnding);
    WriteDirectLine('| name | desc |' + LineEnding + '|---|---|');
    for I := 0 to Pred(Item.Params.Count) do
      WriteDirectLine(
        '| ' + ConvertString(item.params[i].name) + ' | ' +
          item.params[i].value + ' |');
  end;

  if Item.Returns <> '' then
    WriteDirectLine(
        '| *(result)* | ' + Item.Returns + ' |');

  WriteDirectLine(LineEnding);

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
end;

procedure TMarkdownDocGenerator.WriteConstant(const Item: TPasItem);
begin
  WritePara(Hn() + 'Constant ' + ConvertString(Item.Name) + LineEnding);
  WriteDirectLine(
    //'| - | - |' + LineEnding +
    '| name | ' + ConvertString(Item.Name) + ' |' + LineEnding +
    '|---:|---|' + LineEnding +
    '| declaration | ' + ConvertString(CodeString(Item.FullDeclaration)) + ' |' + LineEnding +
    '| visibility | ' + VisToStr(Item.visibility) + ' |');

  WriteDirectLine(LineEnding);

  if Item.HasDescription then
    WritePara(ItemDescription(Item));
end;

procedure TMarkdownDocGenerator.WriteVariable(const Item: TPasItem);
begin
  WritePara(Hn() + 'Variable ' + ConvertString(Item.Name) + LineEnding);
  WriteDirectLine(
    '| | |' + LineEnding + '|---|---|' + LineEnding +
    '| var name | ' + ConvertString(Item.Name) + ' |' + LineEnding +
    '| declaration | ' + ConvertString(Item.FullDeclaration) + ' |' + LineEnding +
    '| visibility | ' + VisToStr(Item.visibility) + ' |');

  WriteDirectLine(LineEnding);

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
  WritePara(Hn() + 'Type ' + ConvertString(Item.Name) + LineEnding);
  WriteDirectLine(
    '| | |' + LineEnding + '|---|---|' + LineEnding +
    '| type name | ' + ConvertString(item.Name) + ' |' + LineEnding +
    '| declaration | ' + ConvertString(item.FullDeclaration)  + ' |' + LineEnding +
    '| visibility | ' + VisToStr(item.visibility)  + ' |');

  WriteDirectLine(LineEnding);

  if Item.HasDescription then
    WritePara(ItemDescription(Item));

  if Item is TPasEnum then
  begin
    Indent;
    WriteEnumMembers(TPasEnum(Item));
    UnIndent;
  end;
  WriteDirectLine(LineEnding);

end;

procedure TMarkdownDocGenerator.WriteStructure(const Item: TPasCIO);
var
  I: Integer;
begin
  WritePara(Hn() + CioTypeToString(Item.MyType) + ' ' + ConvertString(Item.Name) + LineEnding);

  WriteDirectLine(
    '| | |' + LineEnding + '|---|---|' + LineEnding +
    '| structure name | ' + ConvertString(Item.name) + ' |' + LineEnding +
    '| name_with_generic | ' + ConvertString(Item.NameWithGeneric) + ' |' + LineEnding +
    '| type | ' + ConvertString(CioTypeToString(Item.MyType)) + ' |' + LineEnding +
    '| visibility | ' + VisToStr(Item.visibility) + ' |');

  Indent;

  WriteDirectLine(LineEnding);

  if Item.HasDescription then
    WritePara(ItemDescription(Item));

  WriteDirectLine(LineEnding);

  for I := 0 to Item.Ancestors.Count - 1 do
    WriteDirectLine(
      '| | |' + LineEnding + '|---|---|' + LineEnding +
      '| ancestor name | ' + ConvertString(Item.Ancestors[I].Name) + ' |' + LineEnding +
      '| declaration | ' + ConvertString(Item.Ancestors[I].Value) + ' |');

  for I := 0 to Item.Methods.Count - 1 do
    with Item.Methods.PasItemAt[I] as TPasRoutine do
      WriteDirectLine('- [' + Name + '](#' + QualifiedName + ')');

  for I := 0 to Item.Methods.Count - 1 do
    WriteRoutine(Item.Methods.PasItemAt[I] as TPasRoutine);

  for I := 0 to Item.Fields.Count - 1 do
    WriteVariable(Item.fields.PasItemAt[I]);

  for I := 0 to Item.Properties.Count - 1 do
    WriteProperty(Item.Properties.PasItemAt[I] as TPasProperty);

  for I := 0 to Item.Types.Count - 1 do
    WriteType(Item.Types.PasItemAt[I]);

  for I := 0 to Item.Cios.Count - 1 do
    WriteStructure(Item.Cios.PasItemAt[I] as TPasCio);

  UnIndent;
end;

procedure TMarkdownDocGenerator.WriteProperty(const Item: TPasProperty);
begin
  WritePara(Hn() + 'Property ' + ConvertString(Item.Name) + LineEnding);
  WriteDirectLine(
    '| | |' + LineEnding + '|---|---|' + LineEnding +
    '| property name | ' + ConvertString(Item.name) + ' |' + LineEnding +
    '| indexdecl | ' + ConvertString(Item.indexDecl) + ' |' + LineEnding +
    '| type | ' + ConvertString(Item.Proptype) + ' |' + LineEnding +
    '| reader | ' + ConvertString(Item.reader) + ' |' + LineEnding +
    '| writer | ' + ConvertString(Item.writer) + ' |' + LineEnding +
    '| default_in_class | ' + ConvertString(BoolToStr(Item.DefaultInClass, True)) + ' |' + LineEnding +
    '| default_value | ' + ConvertString(Item.DefaultValue) + ' |' + LineEnding +
    '| nodefault | ' + ConvertString(BoolToStr(item.NoDefault, True)) + ' |' + LineEnding +
    '| stored | ' + ConvertString(Item.Stored) + ' |' + LineEnding +
    '| visibility | ' + VisToStr(Item.Visibility) + ' |');

  WriteDirectLine(LineEnding);

  if Item.HasDescription then
    WritePara(ItemDescription(Item));

  WriteDirectLine(LineEnding);
end;

procedure TMarkdownDocGenerator.WritePara(const AText: String);
begin
  WriteDirectLine({FPara +} AText);
end;

procedure TMarkdownDocGenerator.Indent;
begin
  Inc(FIndent);
  FPara := 'p' + StringOfChar('(', FIndent) + '. ';
end;

procedure TMarkdownDocGenerator.UnIndent;
begin
  Dec(FIndent);
  FPara := 'p' + StringOfChar('(', FIndent) + '. ';
end;

function TMarkdownDocGenerator.Hn(AIn: Integer): String;
begin
  Result := StringOfChar('#', FIndent + AIn) + ' ';
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

