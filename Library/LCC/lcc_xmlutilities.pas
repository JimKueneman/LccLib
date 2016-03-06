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
function XmlLoadFromFile(FIlePath: string): LccXmlDocument;
function XmlLoadFromStream(XmlStream: TStream): LccXmlDocument;
function BuildConfigurationDocument(CdiXMLFilePath: string): LccXmlDocument;
procedure XmlFreeDocument(var XmlDoc: LccXmlDocument);
function XmlCreateEmptyDocument: LccXmlDocument;
procedure XmlWriteToFile(FilePath: string; XmlDoc: LccXmlDocument);
function XmlCreateChildNode(XmlDoc: LccXmlDocument; ParentNode: LccXmlNode; Element, Content: string): LccXmlNode;
function XmlCreateRootNode(XmlDoc: LccXmlDocument; Element, Content: string): LccXmlNode;

// Find functions
function XmlFindChildNode(XmlNode: LccXmlNode; Name: string): LccXmlNode;
function XmlFirstChild(XmlNode: LccXmlNode): LccXmlNode;
function XmlFindRootNode(XmlDoc: LccXmlDocument; RootName: string): LccXmlNode;

// Element value (name) functions
function XmlFirstChildValue(XmlNode: LccXmlNode): string;
function XmlNextSiblingValue(XmlNode: LccXmlNode): string;
function XmlNodeName(XmlNode: LccXmlNode): string;

// Element content (text) functions
function XmlNodeTextContent(XmlNode: LccXmlNode): string;
procedure XmlNodeSetTextContent(XmlNode: LccXmlNode; Text: string);

// Enumerator functions
function XmlNextSiblingNode(XmlNode: LccXmlNode): LccXmlNode;

// Attribute functions
function XmlAttributeCreateAndSet(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: string): Boolean;
procedure XmlAttributeForce(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: string);
function XmlAttributeRead(TargetNode: LccXmlNode; Attribute: string): string;
function XmlAttributeExists(TargetNode: LccXmlNode; Attribute: string): Boolean;
procedure XmlAttributeRemove(TargetNode: LccXmlNode; Attribute: string);


implementation

procedure XmlAttributeForce(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: string);
var
  AttributeNode: LccXmlNode;
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

function XmlAttributeRead(TargetNode: LccXmlNode; Attribute: string): string;
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

function XmlAttributeExists(TargetNode: LccXmlNode; Attribute: string): Boolean;
begin
  Result := False;
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    Result := Assigned(TargetNode.Attributes.GetNamedItem(Attribute));
  {$ELSE}
  Result := TargetNode.Attributes[Attribute] <> ''
  {$ENDIF}
end;

procedure XmlAttributeRemove(TargetNode: LccXmlNode; Attribute: string);
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

function XmlLoadFromStream(XmlStream: TStream): LccXmlDocument;
begin
  Result := nil;
  {$IFDEF FPC}
  ReadXMLFile(Result, XmlStream);
  {$ELSE}
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  Result.LoadFromXML(XmlStream);
  {$ENDIF}
end;

function BuildConfigurationDocument(CdiXMLFilePath: string): LccXmlDocument;

  procedure RunCdi(ChildNode: LccXmlNode; var CurrentAddress: Integer);
  var
    Attrib: string;
    ReplicationCount, i: Integer;
  begin
     while Assigned(ChildNode) do
     begin
       XmlAttributeRemove(ChildNode, 'offset');   // Remove any "offset" attribute not used
   //    XmlAttributeForce(Result, ChildNode, 'testing', 'wow');

       if ChildNode.NodeName = 'group' then    // If it is a group then recurse into it.
       begin
         XmlAttributeForce(Result, ChildNode, 'origin', IntToStr(CurrentAddress));
         ReplicationCount := 1;
         Attrib := XmlAttributeRead(ChildNode, 'replication');
         if Attrib <> '' then
           ReplicationCount := StrToInt(Attrib);
         for i := 0 to ReplicationCount-1 do
           RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'int' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt(Attrib)
         else
           Inc(CurrentAddress, 1);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'string' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt(Attrib);  // else broken
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
        XmlAttributeForce(Result, SegmentNode, 'origin', IntToStr(CurrentAddress));
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
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  {$ENDIF}
end;

procedure XmlWriteToFile(FilePath: string; XmlDoc: LccXmlDocument);
begin
  {$IFDEF FPC}
  WriteXMLFile(XmlDoc, FilePath);
  {$ELSE}
  XmlDoc.SaveToXML(FilePath);
  {$ENDIF}
end;

function XmlCreateChildNode(XmlDoc: LccXmlDocument; ParentNode: LccXmlNode; Element, Content: string): LccXmlNode;
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

function XmlCreateRootNode(XmlDoc: LccXmlDocument; Element, Content: string): LccXmlNode;
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

function XmlLoadFromFile(FIlePath: string): LccXmlDocument;
begin
  Result := nil;
  {$IFDEF FPC}
  ReadXMLFile(Result, FilePath);
  {$ELSE}
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  Result.LoadFromXML(FilePath);
  {$ENDIF}
end;

function XmlFindChildNode(XmlNode: LccXmlNode; Name: string): LccXmlNode;
begin
  Result := XmlNode.{$IFNDEF FPC}ChildNodes.{$ENDIF}FindNode(Name);
end;

function XmlFirstChild(XmlNode: LccXmlNode): LccXmlNode;
begin
  Result := XmlNode.{$IFDEF FPC}FirstChild{$ELSE}ChildNodes.First{$ENDIF}
end;

function XmlFirstChildValue(XmlNode: LccXmlNode): string;
var
  Child: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Child := XmlFirstChild(XmlNode);
  if Assigned(Child) then
    Result := Child.NodeValue;
end;

function XmlNextSiblingValue(XmlNode: LccXmlNode): string;
var
  Sibling: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Sibling := XmlNextSiblingNode(XmlNode);
  if Assigned(Sibling) then
    Result := Sibling.NodeValue;
end;

function XmlNodeName(XmlNode: LccXmlNode): string;
begin
  Result := XmlNode.NodeName;
end;

function XmlNodeTextContent(XmlNode: LccXmlNode): string;
begin
  Result := XmlNode.{$IFDEF FPC}TextContent{$ELSE}Text{$ENDIF}
end;

procedure XmlNodeSetTextContent(XmlNode: LccXmlNode; Text: string);
begin
  XmlNode.{$IFDEF FPC}TextContent{$ELSE}Text{$ENDIF} := Text;
end;

function XmlNextSiblingNode(XmlNode: LccXmlNode): LccXmlNode;
begin
  Result := XmlNode.NextSibling;
end;

function XmlAttributeCreateAndSet(XmlDoc: LccXmlDocument; TargetNode: LccXmlNode; Attribute, Content: string): Boolean;
var
  AttributeNode: LccXmlAttribute;
begin
  {$IFDEF FPC}
  AttributeNode := XmlDoc.CreateAttribute(Attribute);
  XmlNodeSetTextContent(AttributeNode, Content);
  TargetNode.Attributes.SetNamedItem(AttributeNode);
  {$ELSE}
  TargetNode.SetAttributeNS(Attribute, '', Content);
  {$ENDIF}
end;

function XmlFindRootNode(XmlDoc: LccXmlDocument; RootName: string): LccXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlFindChildNode(XmlDoc, RootName);
  {$ELSE}
  Result := XmlDoc.ChildNodes.FindNode(RootName);
  {$ENDIF}
end;

end.

