unit lcc_xmlutilities;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF FPC}
    DOM,
    XMLRead,
    XMLWrite,
  {$ELSE}
  Xml.XMLDoc,
  Xml.xmldom,
  Xml.XMLIntf,
  {$ENDIF}
  SysUtils;

type
  LccXmlNode ={$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
  LccXmlDocument = {$IFDEF FPC}TXMLDocument{$ELSE}IXMLDocument{$ENDIF};
  LccXmlAttribute = {$IFDEF FPC}TDOMAttr{$ELSE}IXMLNode{$ENDIF};


// Document functions
function XmlLoadFromFile(FilePath: string): LccXmlDocument;
function XmlLoadFromStream(Stream: TStream): LccXmlDocument;
function BuildConfigurationDocument(CdiXMLFilePath: string): LccXmlDocument;
procedure XmlFreeDocument(var XmlDoc: LccXmlDocument);
function XmlCreateEmptyDocument: LccXmlDocument;
procedure XmlWriteToFile(FilePath: string; XmlDoc: LccXmlDocument);
function XmlCreateChildNode(XmlDoc: LccXmlDocument; ParentNode: LccXmlNode; Element, Content: domString): LccXmlNode;
function XmlCreateRootNode(XmlDoc: LccXmlDocument; Element, Content: domString): LccXmlNode;

// Find functions
function XmlFindChildNode(XmlNode: LccXmlNode; Name: domString): LccXmlNode;
function XmlFirstChild(XmlNode: LccXmlNode): LccXmlNode;
function XmlFindRootNode(XmlDoc: LccXmlDocument; RootName: domString): LccXmlNode;

// Element value (name) functions
function XmlFirstChildValue(XmlNode: LccXmlNode): domString;
function XmlNextSiblingValue(XmlNode: LccXmlNode): domString;
function XmlNodeName(XmlNode: LccXmlNode): domString;

// Element content (text) functions
function XmlNodeTextContent(XmlNode: LccXmlNode): domString;
procedure XmlNodeSetTextContent(XmlNode: LccXmlNode; Text: domString);

procedure XmlNodeSetFirstLevelTextContent(XMLDoc: LccXmlDocument; RootElement, ChildElement, Content: domString; Force: Boolean); overload;
procedure XmlNodeSetFirstLevelTextContent(FilePath, RootElement, ChildElement, Content: domString; Force: Boolean); overload;

// Enumerator functions
function XmlNextSiblingNode(XmlNode: LccXmlNode): LccXmlNode;

// Attribute functions
procedure XmlAttributeCreateAndSet(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: domString);
procedure XmlAttributeForce(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: domString);
function XmlAttributeRead(TargetNode: LccXmlNode; Attribute: domString): domString;
function XmlAttributeExists(TargetNode: LccXmlNode; Attribute: domString): Boolean;
procedure XmlAttributeRemove(TargetNode: LccXmlNode; Attribute: domString);


implementation

procedure XmlAttributeForce(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: domString);
{$IFDEF FPC}
var
  AttributeNode: LccXmlNode;
{$ENDIF}
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
  begin
    AttributeNode := TargetNode.Attributes.GetNamedItem(Attribute);
    if Assigned(AttributeNode) then
      XmlNodeSetTextContent(AttributeNode, Content)
    else
      XmlAttributeCreateAndSet(XmlDoc, TargetNode, Attribute, Content);
  end;
  {$ELSE}
    TargetNode.SetAttributeNS(Attribute, '', Content)
  {$ENDIF}
end;

function XmlAttributeRead(TargetNode: LccXmlNode; Attribute: domString): domString;
var
  Node: LccXmlNode;
begin
  Result := '';
  Node := nil;
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    Node := TargetNode.Attributes.GetNamedItem(Attribute);
  if Assigned(Node) then
    Result := Node.NodeValue;
  {$ELSE}
  Result := TargetNode.Attributes[Attribute];
  {$ENDIF}
end;

function XmlAttributeExists(TargetNode: LccXmlNode; Attribute: domString): Boolean;
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    Result := Assigned(TargetNode.Attributes.GetNamedItem(Attribute));
  {$ELSE}
  Result := TargetNode.Attributes[Attribute] <> ''
  {$ENDIF}
end;

procedure XmlAttributeRemove(TargetNode: LccXmlNode; Attribute: domString);
{$IFNDEF FPC}
var
  Node: IXMLNode;
{$ENDIF}
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    if Assigned( TargetNode.Attributes.GetNamedItem(Attribute)) then
      TargetNode.Attributes.RemoveNamedItem(Attribute);
  {$ELSE}
  Node := TargetNode.AttributeNodes.FindNode(Attribute);
  if Assigned(Node) then
    TargetNode.AttributeNodes.Remove(Node)
  {$ENDIF}
end;

function XmlLoadFromStream(Stream: TStream): LccXmlDocument;
begin
  Result := nil;
  Stream.Position := 0;
  {$IFDEF FPC}
  ReadXMLFile(Result, Stream);
  {$ELSE}
  Result := XmlLoadFromStream(Stream);
  {$ENDIF}
end;

function BuildConfigurationDocument(CdiXMLFilePath: string): LccXmlDocument;

  procedure RunCdi(ChildNode: LccXmlNode; var CurrentAddress: Integer);
  var
    Attrib: domString;
    ReplicationCount, i: Integer;
  begin
     while Assigned(ChildNode) do
     begin
       XmlAttributeRemove(ChildNode, 'offset');   // Remove any "offset" attribute not used
   //    XmlAttributeForce(Result, ChildNode, 'testing', 'wow');

       if ChildNode.NodeName = 'group' then    // If it is a group then recurse into it.
       begin
         XmlAttributeForce(Result, ChildNode, 'origin', domString( IntToStr(CurrentAddress)));
         ReplicationCount := 1;
         Attrib := XmlAttributeRead(ChildNode, 'replication');
         if Attrib <> '' then
           ReplicationCount := StrToInt( string( Attrib));
         for i := 0 to ReplicationCount-1 do
           RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'int' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt( string( Attrib))
         else
           Inc(CurrentAddress, 1);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'domString' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt( string( Attrib));  // else broken
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'eventid' then
       begin
         Inc(CurrentAddress, 8);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'bit' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'map' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'relation' then
       begin
          RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end;
       ChildNode := XmlNextSiblingNode(ChildNode);
     end;
  end;

var
  RootNode, SegmentNode: LccXmlNode;
  CurrentAddress: Integer;
begin
  Result := XmlLoadFromFile(CdiXmlFilePath);
  if Assigned(Result) then
  begin
    CurrentAddress := 0;
    RootNode := XmlFindRootNode(Result, 'ndi');
    if Assigned(RootNode) then
    begin
      SegmentNode := XmlFindChildNode(RootNode, 'segment');
      while Assigned(SegmentNode) do              // Run all the Segements in the file
      begin
        XmlAttributeForce(Result, SegmentNode, 'origin', domString( IntToStr(CurrentAddress)));
        XmlAttributeRemove(SegmentNode, 'offset');
        // From here on it can be recursive down into groups, etc...
        RunCdi(XmlFirstChild(SegmentNode), CurrentAddress);
        SegmentNode := XmlNextSiblingNode(SegmentNode);
      end;
    end;
  end;

end;

procedure XmlFreeDocument(var XmlDoc: LccXmlDocument);
begin
  {$IFDEF FPC}
  FreeAndNil(XmlDoc)
  {$ELSE}
     // Is and interface and will free itself
  {$ENDIF}
end;

function XmlCreateEmptyDocument: LccXmlDocument;
begin
  {$IFDEF FPC}
  Result := TXMLDocument.Create;
  {$ELSE}
  Result := NewXMLDocument();
  Result.Encoding := 'UTF-8';
  Result.Options := [doNodeAutoIndent];
  {$ENDIF}
end;

procedure XmlWriteToFile(FilePath: string; XmlDoc: LccXmlDocument);
begin
  {$IFDEF FPC}
  WriteXMLFile(XmlDoc, FilePath);
  {$ELSE}
  XmlDoc.SaveToFile(FilePath)
  {$ENDIF}
end;

function XmlCreateChildNode(XmlDoc: LccXmlDocument; ParentNode: LccXmlNode; Element, Content: domString): LccXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlDoc.CreateElement(Element);
  ParentNode.AppendChild(Result);
  if Content <> '' then
    Result.TextContent := Content;
  {$ELSE}
  Result := ParentNode.AddChild(Element);
  if Content <> '' then
    Result.Text := Content
  {$ENDIF}
end;

function XmlCreateRootNode(XmlDoc: LccXmlDocument; Element, Content: domString): LccXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlDoc.CreateElement(Element);
  XmlDoc.AppendChild(Result);
  Result.TextContent := Content;
  {$ELSE}
  Result := XmlDoc.AddChild(Element);
  Result.Text := Content;
  {$ENDIF}
end;

function XmlLoadFromFile(FilePath: string): LccXmlDocument;
begin
  Result := nil;
  {$IFDEF FPC}
  ReadXMLFile(Result, FilePath);
  {$ELSE}
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  Result.LoadFromFile(FilePath);
  {$ENDIF}
end;

function XmlFindChildNode(XmlNode: LccXmlNode; Name: domString): LccXmlNode;
begin
  Result := XmlNode.{$IFNDEF FPC}ChildNodes.{$ENDIF}FindNode(Name);
end;

function XmlFirstChild(XmlNode: LccXmlNode): LccXmlNode;
begin
  Result := XmlNode.{$IFDEF FPC}FirstChild{$ELSE}ChildNodes.First{$ENDIF}
end;

function XmlFirstChildValue(XmlNode: LccXmlNode): domString;
var
  Child: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Child := XmlFirstChild(XmlNode);
  if Assigned(Child) then
    Result := Child.NodeValue;
end;

function XmlNextSiblingValue(XmlNode: LccXmlNode): domString;
var
  Sibling: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Sibling := XmlNextSiblingNode(XmlNode);
  if Assigned(Sibling) then
    Result := Sibling.NodeValue;
end;

function XmlNodeName(XmlNode: LccXmlNode): domString;
begin
  Result := XmlNode.NodeName;
end;

function XmlNodeTextContent(XmlNode: LccXmlNode): domString;
begin
  Result := XmlNode.{$IFDEF FPC}TextContent{$ELSE}Text{$ENDIF}
end;

procedure XmlNodeSetTextContent(XmlNode: LccXmlNode; Text: domString);
begin
  XmlNode.{$IFDEF FPC}TextContent{$ELSE}Text{$ENDIF} := Text;
end;

procedure XmlNodeSetFirstLevelTextContent(FilePath, RootElement, ChildElement, Content: domString; Force: Boolean);
var
  XMLDoc: LccXmlDocument;
begin
  // Does not Force the FilePath and and a new XML file
  XMLDoc := XmlLoadFromFile(FilePath);
  if Assigned(XMLDoc) then
  begin
    XmlNodeSetFirstLevelTextContent(XMLDoc, RootElement, ChildElement, Content, Force);
    XmlWriteToFile(FilePath, XMLDoc)
  end;
end;

procedure XmlNodeSetFirstLevelTextContent(XMLDoc: LccXmlDocument; RootElement, ChildElement, Content: domString; Force: Boolean);
var
  RootNode, ChildNode: LccXmlNode;
begin
  if Assigned(XMLDoc) then
  begin
    RootNode := XmlFindRootNode(XMLDoc, RootElement);
    if not Assigned(RootNode) and Force then
      RootNode := XmlCreateRootNode(XMLDoc, RootElement, '');
    if Assigned(RootNode) then
    begin
      ChildNode := XmlFindChildNode(RootNode, ChildElement);
      if not Assigned(ChildNode) and Force then
        XmlCreateChildNode(XMLDoc, RootNode, ChildElement, Content)
      else
        XmlNodeSetTextContent(ChildNode, Content)
    end;
  end;
end;

function XmlNextSiblingNode(XmlNode: LccXmlNode): LccXmlNode;
begin
  Result := XmlNode.NextSibling;
end;

procedure XmlAttributeCreateAndSet(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: domString);
{$IFDEF FPC}
var
  AttributeNode: LccXmlAttribute;
{$ENDIF}
begin
  {$IFDEF FPC}
  AttributeNode := XmlDoc.CreateAttribute(Attribute);
  XmlNodeSetTextContent(AttributeNode, Content);
  TargetNode.Attributes.SetNamedItem(AttributeNode);
  {$ELSE}
  TargetNode.SetAttributeNS(Attribute, '', Content);
  {$ENDIF}
end;

function XmlFindRootNode(XmlDoc: LccXmlDocument; RootName: domString): LccXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlFindChildNode(XmlDoc, RootName);
  {$ELSE}
  Result := XmlDoc.ChildNodes.FindNode(RootName);
  {$ENDIF}
end;

end.

