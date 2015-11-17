unit intel_hex_parser;

// Version 0.1
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimkueneman@yahoo.com> and Vcc
//


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  TYPECODE_DATA                         = 00;
  TYPECODE_EOF                          = 01;
  TYPECODE_EXTENDED_SEGMENT_ADDRESS     = 02;
  TYPECODE_EXTENDED_LINEAR_ADDRESS      = 04;
  TYPECODE_START_LINEAR_ADDRESS         = 05;

  ERROR_WRITE_BLOCK_OK                          = 0;
  ERROR_WRITE_BLOCK_SMALLER_THAN_INSTRUCTION    = 1;

  ERROR_ERASE_BLOCK_OK                          = 0;
  ERROR_ERASE_BLOCK_SMALLER_THAN_INSTRUCTION    = 1;


type
  TDataBlock = array of byte;
  PDataBlock = ^TDataBlock;

  TIntelHexType = (
    iht_INHX8,          // Original Intel 8 bit format
    iht_INHX8M,         // Extension which maps to a 16 bit instruction but decided to leave the address in the HEX file map to bytes, this makes the addresses double what the MCU uses (physical address = Hex address / 2)
    iht_INH24_dsPIC30,  // Extension which maps to a 24 bit dsPIC30
    iht_INH24_dsPIC33,  // Extension which maps to a 24 bit dsPIC33
    iht_INHX32          // 32 bit extension which added the 0x04 type to define the upper 2 bytes of the 32 bit address, the correct MCU address is used in this format so there is no manipulating the address needed
  );

  TIntelHexInfo = record
    HexType: TIntelHexType;       // The format of the HEX file
    AddressIncrement: Byte;       // Address increment for a single instruction (can be the same as the bytes per address or not, depends on the MCU)
    BytesPerInstruction: Byte;    // Number of Bytes in each address offset increase
    DoubleAddress: Boolean;       // True if the HEX address is double the physical address
  end;


  TIntelHexParser = class;
  TPhysicalWriteBlockList = class;
  TPhysicalEraseBlockList = class;
  TPhysicalAddressBlockList = class;
  TBaseBlockList = class;

  TCustomAddressBlockFunc = procedure(BlockList: TPhysicalAddressBlockList) of object;

    { TIgnoreBound }

  TIgnoreBound = class
  private
    FHiAddress: DWord;
    FLoAddress: DWord;
  public
    property LoAddress: DWord read FLoAddress write FLoAddress;
    property HiAddress: DWord read FHiAddress write FHiAddress;
  end;

  { TIgnoreBoundsList }

  TIgnoreBoundsList = class(TList)
  private
    function GetIgnoreBound(Index: Integer): TIgnoreBound;
    procedure SetIgnoreBound(Index: Integer; AValue: TIgnoreBound);
  public
    function AddBound(LoAddress, HiAddress: DWord): TIgnoreBound;
    procedure Clear; override;
    property IgnoreBound[Index: Integer]: TIgnoreBound read GetIgnoreBound write SetIgnoreBound;
  end;

  { TIgnoreBoundsGroups }

  TIgnoreBoundsGroups = class(TList)
  private
    function FGroups(Index: Integer): TIgnoreBoundsList;
    procedure FGroups(Index: Integer; AValue: TIgnoreBoundsList);
  public
    function AddGroup: TIgnoreBoundsList;
    procedure Clear; override;
    property Group[Index: Integer]: TIgnoreBoundsList read FGroups write FGroups;
  end;

  { TBaseBlock }

  TBaseBlock = class
  private
    FAddressStart: DWord;
    FDataBlock: TDataBlock;
    FOwner: TBaseBlockList;
    function GetAddressCount: DWord; virtual;
    function GetAddressIncrement: Byte;
    function GetAddressLast: DWord; virtual;
    function GetByteCount: DWord; virtual;
    function GetBytesPerInstruction: DWord; virtual;
    function GetHexType: TIntelHexType;
  public
    procedure AddChunk(DataArray: TDataBlock; DataLen: Byte);
    procedure FillBlock(AByte: Byte);
    function JumpToInstruction(Address: DWord): PByte;
    function ReadDWordInstruction(Address: DWord): DWord;
    function Read24BitInstruction(Address: DWord): DWord;
    function ReadWordInstruction(Address: DWord): Word;
    function ReadByteInstruction(Address: DWord): Byte;
    function WriteDWordInstruction(Address: DWord; Instruction: DWord): Boolean;
    function Write24BitInstruction(Address: DWord; Instruction: DWord): Boolean;
    function WriteWordInstruction(Address: DWord; Instruction: Word): Boolean;
    function WriteByteInstruction(Address: DWord; Instruction: Byte): Boolean;
    property AddressStart: DWord read FAddressStart;
    property AddressCount: DWord read GetAddressCount;
    property AddressLast: DWord read GetAddressLast;
    property AddressIncrement: Byte read GetAddressIncrement;
    property DataBlock: TDataBlock read FDataBlock;
    property ByteCount: DWord read GetByteCount;
    property BytesPerInstruction: DWord read GetBytesPerInstruction;
    property HexType: TIntelHexType read GetHexType;
    property Owner: TBaseBlockList read FOwner;
  end;

  { TBaseBlockList }

  TBaseBlockList = class
  private
    FBlockList: TList;
    FOwner: TIntelHexParser;
    function GetAddressIncrement: DWord;
    function GetBytesPerInstruction: DWord;
    function GetCount: Integer;
    function GetHexType: TIntelHexType;
  protected
    property BlockList: TList read FBlockList write FBlockList;
  public
    constructor Create(AnOwner: TIntelHexParser); virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteBlock(Block: TBaseBlock);
    function FindBlock(Address: DWord): TBaseBlock;
    function JumpToInstruction(Address: DWord): PByte;
    property AddressIncrement: DWord read GetAddressIncrement;
    property Count: Integer read GetCount;
    property BytesPerInstruction: DWord read GetBytesPerInstruction;
    property HexType: TIntelHexType read GetHexType;
    property Owner: TIntelHexParser read FOwner;
  end;

  { TPhysicalAddressBlock }

  TPhysicalAddressBlock = class(TBaseBlock)
  public
    constructor Create(AnOwner: TPhysicalAddressBlockList; AnAddressStart: DWord); virtual;
  end;

  { TPhysicalAddressBlockList }

  TPhysicalAddressBlockList = class(TBaseBlockList)
  private
    function GetBlock(Index: Integer): TPhysicalAddressBlock;
    procedure SetBlock(Index: Integer; AValue: TPhysicalAddressBlock);
  public
    function AddBlankBlock(AnAddressStart: DWord; AddressCount: DWord; FillChar: Byte): TPhysicalAddressBlock;
    function AddBlock(AnAddressStart: DWord): TPhysicalAddressBlock;
    function Clone(Block: TPhysicalAddressBlock): TPhysicalAddressBlock;
    property Block[Index: Integer]: TPhysicalAddressBlock read GetBlock write SetBlock; default;
  end;

  { TPhysicalEraseBlock }

  TPhysicalEraseBlock = class(TBaseBlock)
  private
    FBlockSize: DWord;
    FBlockCount: DWord;
    function GetByteCount: DWord; override;
  public
    constructor Create(AnOwner: TPhysicalEraseBlockList; AnAddressStart: DWord; AnEraseBlockSize: DWord); virtual;
    // The number of blocks to erase, the total address space erased is Count * BlockSize
    property BlockCount: DWord read FBlockCount write FBlockCount;
    // The size of a single erase block in bytes
    property BlockSize: DWord read FBlockSize;
  end;

  { TPhysicalEraseBlockList }

  TPhysicalEraseBlockList = class(TBaseBlockList)
  private
    function GetBlock(Index: Integer): TPhysicalEraseBlock;
    procedure SetBlock(Index: Integer; AValue: TPhysicalEraseBlock);
  public
    function BlockCovered(ABlock: TBaseBlock): Boolean;
    function AddBlock(AnAddressStart: DWord; AnEraseBlockSize: DWord): TPhysicalEraseBlock;
    function Clone(Block: TPhysicalEraseBlock): TPhysicalEraseBlock;
    property Block[Index: Integer]: TPhysicalEraseBlock read GetBlock write SetBlock; default;
  end;


  { TPhysicalWriteBlock }

  TPhysicalWriteBlock = class(TBaseBlock)
  public
    constructor Create(AnOwner: TPhysicalWriteBlockList; AnAddressStart: DWord); virtual;
  end;

  { TPhysicalWriteBlockList }

  TPhysicalWriteBlockList = class(TBaseBlockList)
  private
    function GetBlock(Index: Integer): TPhysicalWriteBlock;
    procedure SetBlock(Index: Integer; AValue: TPhysicalWriteBlock);
  public
    function AddBlankBlock(AnAddressStart: DWord; AddressCount: DWord; FillChar: Byte): TPhysicalWriteBlock;
    function AddBlock(AnAddressStart: DWord): TPhysicalWriteBlock;
    function Clone(Block: TPhysicalWriteBlock): TPhysicalWriteBlock;
    property Block[Index: Integer]: TPhysicalWriteBlock read GetBlock write SetBlock; default;
  end;

  { TIntelHexParser }

  TIntelHexParser = class
  private
    FActiveBlock: TPhysicalAddressBlock;
    FCustomAddressFunc: TCustomAddressBlockFunc;
    FAlignWriteBlockOnEraseSize: Boolean;
    FRawAddressBlockList: TPhysicalAddressBlockList;
    FEraseBlockList: TPhysicalEraseBlockList;
    FEraseBlockSize: DWord;
    FHexFile: string;
    FHexInfo: TIntelHexInfo;   // This can not be reliably detected from the file, you have know and set this manually
    FPack: Boolean;
    FSorted: Boolean;
    FPhysicalWriteBlockList: TPhysicalWriteBlockList;
    FWriteBlockSize: DWord;
    FWriteBufferSize: DWord;
    procedure SetSorted(AValue: Boolean);
  protected
    property ActiveBlock: TPhysicalAddressBlock read FActiveBlock write FActiveBlock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Parses the HEX file using the passed information about the file and MCU communication link
    function ParseHex(AHexFile: string; AHexInfo: TIntelHexInfo; AnEraseBlockSize, AWriteBlockSize, AWriteBufferSize: DWord; IgnoreBounds: TIgnoreBoundsGroups; AnAlignWriteBlockOnEraseSize: Boolean): Boolean;
    // Builds the Erase Blocks to send to the MCU based on the Parsed Hex file and EraseBlockSize
    function BuildEraseBlockList: Integer;
    // Builds the Write Blocks to send to the MCU based on the Parsed Hex file and WriteBufferSize
    function BuildWriteBlockList: Integer;
    // Raw Address Blocks as parsed from Hex file, it does undo the 2x address encoded into the Hex file if the hex for the MCU doubles it
    property PhysicalAddressBlockList: TPhysicalAddressBlockList read FRawAddressBlockList;
    // Start Addresses and block count (based on EraseBlockSize) that minimized the number of Erase Block objects to cover all that need to be written, physical address for direct usage in the MCU calculated
    property PhysicalEraseBlockList: TPhysicalEraseBlockList read FEraseBlockList;
    // Blocks of data to send to the MCU based on the communication link buffer size defined in WriteBufferSize, physical address for direct usage in the MCU calculated
    property PhysicalWriteBlockList: TPhysicalWriteBlockList read FPhysicalWriteBlockList;
    // Defines the size of an EraseBlock in the MCU
    property EraseBlockSize: DWord read FEraseBlockSize;
    // Path and file name of the HEX file
    property HexFile: string read FHexFile;
    // Tells the parser what format the file is in.  This is needed to decide how to handle the addresses in the HEX file
    property HexInfo: TIntelHexInfo read FHexInfo;
    // Pack will use a single AddressBlock for as many contiguious addresses as possible, if false a new AddressBlock object will be created for every line in the HEX file
    property Pack: Boolean read FPack write FPack;
    // Sorts the addressed from lower to higher
    property Sorted: Boolean read FSorted write SetSorted;
    // Defines the Write Block boundry size
    property WriteBlockSize: DWord read FWriteBlockSize;
    // Defines the size of a buffer that the MCU can handle when writing (not the WriteBlock size of the MCU but the connection link buffer size)
    property WriteBufferSize: DWord read FWriteBufferSize;
    //
    property AlignWriteBlockOnEraseSize: Boolean read FAlignWriteBlockOnEraseSize;
    // Custom callback after the Hex file loads to add custom blocks
    property CustomAddressFunc: TCustomAddressBlockFunc read FCustomAddressFunc write FCustomAddressFunc;
  end;

implementation

function AddressBlockSort(Item1, Item2: Pointer): Integer;
begin
  Result := TPhysicalAddressBlock(Item1).FAddressStart - TPhysicalAddressBlock(Item2).FAddressStart;
end;

function Low(X: DWord): Byte;
begin
  Result := Byte(X and $000000FF)
end;

function High(X: DWord): Byte;
begin
  Result := Byte((X shr 8) and $000000FF)
end;

function Higher(X: DWord): Byte;
begin
  Result := Byte((X shr 16) and $000000FF)
end;

function Highest(X: DWord): Byte;
begin
  Result := Byte((X shr 24) and $000000FF)
end;

function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
    Result := Ord(Ch) - 48;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;

function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result * 16;
end;

function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;

{ TIgnoreBoundsGroups }

function TIgnoreBoundsGroups.FGroups(Index: Integer): TIgnoreBoundsList;
begin
  Result := TIgnoreBoundsList( Items[Index])
end;

procedure TIgnoreBoundsGroups.FGroups(Index: Integer; AValue: TIgnoreBoundsList );
begin
  Items[Index] := AValue;
end;

function TIgnoreBoundsGroups.AddGroup: TIgnoreBoundsList;
begin
  Result := TIgnoreBoundsList.Create;
  Add(Result);
end;

procedure TIgnoreBoundsGroups.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      Group[i].Free;
  finally
    inherited Clear;
  end;
end;

{ TIgnoreBoundsList }

function TIgnoreBoundsList.GetIgnoreBound(Index: Integer): TIgnoreBound;
begin
  Result := TIgnoreBound( inherited Items[Index]);
end;

procedure TIgnoreBoundsList.SetIgnoreBound(Index: Integer; AValue: TIgnoreBound);
begin
  inherited Items[Index] := AValue;
end;

function TIgnoreBoundsList.AddBound(LoAddress, HiAddress: DWord): TIgnoreBound;
begin
  Result := TIgnoreBound.Create;
  Add(Result);
  Result.LoAddress := LoAddress;
  Result.HiAddress := HiAddress;
end;

procedure TIgnoreBoundsList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      IgnoreBound[i].Free;
  finally
    inherited Clear;
  end;
end;

{ TBaseBlock }

function TBaseBlock.GetAddressCount: DWord;
begin
  Result := ByteCount div Owner.Owner.HexInfo.BytesPerInstruction;
end;

function TBaseBlock.GetAddressIncrement: Byte;
begin
  Result := Owner.Owner.HexInfo.AddressIncrement;
end;

function TBaseBlock.GetAddressLast: DWord;
begin
  Result := AddressStart + ((AddressCount - 1) * Owner.Owner.HexInfo.AddressIncrement);
end;

function TBaseBlock.GetByteCount: DWord;
begin
  Result := Length(FDataBlock);
end;

function TBaseBlock.GetBytesPerInstruction: DWord;
begin
  Result := Owner.Owner.HexInfo.BytesPerInstruction;
end;

function TBaseBlock.GetHexType: TIntelHexType;
begin
  Result := Owner.Owner.HexInfo.HexType;
end;

procedure TBaseBlock.AddChunk(DataArray: TDataBlock; DataLen: Byte);
var
  i, iDataBlock: Integer;
begin
  iDataBlock := Length(FDataBlock);
  SetLength(FDataBlock, iDataBlock + DataLen);
  i := 0;
  while i < DataLen do
  begin
    DataBlock[iDataBlock] := DataArray[i];
    Inc(iDataBlock);
    Inc(i);
  end;
end;

procedure TBaseBlock.FillBlock(AByte: Byte);
var
  i: Integer;
begin
  for i := 0 to Length(FDataBlock) - 1 do
    DataBlock[i] := $AA; //AByte
end;

function TBaseBlock.JumpToInstruction(Address: DWord): PByte;
begin
  Result := nil;
  if (Address >= AddressStart) and (Address <= (AddressLast + Owner.Owner.HexInfo.AddressIncrement)) then
    Result := @DataBlock[BytesPerInstruction * (Address - AddressStart) div Owner.Owner.HexInfo.AddressIncrement];
end;

function TBaseBlock.ReadDWordInstruction(Address: DWord): DWord;
var
  BytePtr: PByte;
begin
  Result := 0;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
    Result := PDWord( BytePtr)^
end;

function TBaseBlock.Read24BitInstruction(Address: DWord): DWord;
var
  BytePtr: PByte;
begin
  Result := 0;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
  begin
    if Owner.HexType = iht_INH24_dsPIC30 then
    begin
      // 24 bit Microchip dsPIC30 MCU is odd
      Result := DWord(BytePtr^ shl 16);
      Inc(BytePtr);
      Result := Result or BytePtr^;
      Inc(BytePtr);
      Result := Result or DWord(BytePtr^ shl 8);
    end else
    begin
      // 24 bit Microchip dsPIC33 MCU not quite as odd
      Result := BytePtr^;
      Inc(BytePtr);
      Result := Result or DWord(BytePtr^ shl 8);
      Inc(BytePtr);
      Result := Result or DWord(BytePtr^ shl 16);
    end;
  end;
end;

function TBaseBlock.ReadWordInstruction(Address: DWord): Word;
var
  BytePtr: PByte;
begin
  Result := 0;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
    Result := PWord( BytePtr)^
end;

function TBaseBlock.ReadByteInstruction(Address: DWord): Byte;
var
  BytePtr: PByte;
begin
  Result := 0;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
    Result := BytePtr^
end;

function TBaseBlock.WriteDWordInstruction(Address: DWord; Instruction: DWord): Boolean;
var
  BytePtr: PByte;
begin
  Result := False;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
  begin
    BytePtr^ := Low(Instruction);
    Inc(BytePtr);
    BytePtr^ := High(Instruction);
    Inc(BytePtr);
    BytePtr^ := Higher(Instruction);
    Inc(BytePtr);
    BytePtr^ := Highest(Instruction);
    Result := True;
  end;
end;

function TBaseBlock.Write24BitInstruction(Address: DWord; Instruction: DWord): Boolean;
var
  BytePtr: PByte;
begin
  Result := False;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
  begin
    if Owner.HexType = iht_INH24_dsPIC30 then
    begin
      // 24 bit Microchip dsPIC30 MCU is odd
      BytePtr^ := Higher(Instruction);
      Inc(BytePtr);
      BytePtr^ := Low(Instruction);
      Inc(BytePtr);
      BytePtr^ := High(Instruction);
    end else
    begin
    // 24 bit Microchip dsPIC33 MCU is not quite so odd
      BytePtr^ := Low(Instruction);
      Inc(BytePtr);
      BytePtr^ := High(Instruction);
      Inc(BytePtr);
      BytePtr^ := Higher(Instruction);
    end;
    Result := True;
  end;
end;

function TBaseBlock.WriteWordInstruction(Address: DWord; Instruction: Word): Boolean;
var
  BytePtr: PByte;
begin
  Result := False;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
  begin
    BytePtr^ := Low(Instruction);
    Inc(BytePtr);
    BytePtr^ := High(Instruction);
    Result := True;
  end;
end;

function TBaseBlock.WriteByteInstruction(Address: DWord; Instruction: Byte): Boolean;
var
  BytePtr: PByte;
begin
  Result := False;
  BytePtr := JumpToInstruction(Address);
  if Assigned(BytePtr) then
  begin
    BytePtr^ := Low(Instruction);
    Result := True;
  end;
end;


{ TPhysicalWriteBlockList }

function TPhysicalWriteBlockList.GetBlock(Index: Integer): TPhysicalWriteBlock;
begin
  Result := TPhysicalWriteBlock( BlockList[Index]);
end;

procedure TPhysicalWriteBlockList.SetBlock(Index: Integer; AValue: TPhysicalWriteBlock);
begin
  BlockList[Index] := AValue;
end;

function TPhysicalWriteBlockList.AddBlankBlock(AnAddressStart: DWord;
  AddressCount: DWord; FillChar: Byte): TPhysicalWriteBlock;
var
  TempCount: DWord;
begin
  Result := AddBlock(AnAddressStart);
  TempCount := AddressCount * BytesPerInstruction div AddressIncrement;
  SetLength(Result.FDataBlock, TempCount);
  Result.FillBlock(FillChar);
end;

function TPhysicalWriteBlockList.AddBlock(AnAddressStart: DWord): TPhysicalWriteBlock;
begin
  Result := TPhysicalWriteBlock.Create(Self, AnAddressStart);
  BlockList.Add(Result);
end;

function TPhysicalWriteBlockList.Clone(Block: TPhysicalWriteBlock
  ): TPhysicalWriteBlock;
begin
  Result := AddBlock(Block.AddressStart);
  Result.AddChunk(Block.DataBlock, Block.ByteCount);
end;

{ TPhysicalWriteBlock }

constructor TPhysicalWriteBlock.Create(AnOwner: TPhysicalWriteBlockList;
  AnAddressStart: DWord);
begin
  inherited Create;
  FOwner := AnOwner;
  FAddressStart := AnAddressStart;
end;

{ TPhysicalEraseBlock }

constructor TPhysicalEraseBlock.Create(AnOwner: TPhysicalEraseBlockList;
  AnAddressStart: DWord; AnEraseBlockSize: DWord);
begin
  inherited Create;
  FOwner := AnOwner;
  FAddressStart := AnAddressStart;
  FBlockSize := AnEraseBlockSize;
end;

function TPhysicalEraseBlock.GetByteCount: DWord;
begin
  Result := BlockSize * BlockCount;
end;

{ TBaseBlockList }

procedure TBaseBlockList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to BlockList.Count - 1 do
      TObject( BlockList[i]).Free
  finally
    BlockList.Count := 0;
  end;
end;


constructor TBaseBlockList.Create(AnOwner: TIntelHexParser);
begin
  inherited Create;
  FOwner := AnOwner;
  FBlockList := TList.Create;
end;

destructor TBaseBlockList.Destroy;
begin
  Clear;
  FreeAndNil(FBlockList);
  inherited Destroy;
end;

function TBaseBlockList.GetCount: Integer;
begin
  Result := BlockList.Count;
end;

function TBaseBlockList.GetHexType: TIntelHexType;
begin
  Result := Owner.HexInfo.HexType;
end;

function TBaseBlockList.JumpToInstruction(Address: DWord): PByte;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Count - 1 do
  begin
    Result := TBaseBlock( BlockList[i]).JumpToInstruction(Address);
    if Result <> nil then
      Break;
    Inc(i);
  end;
end;

function TBaseBlockList.FindBlock(Address: DWord): TBaseBlock;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Count do
  begin
    if (Address >= TBaseBlock( BlockList[i]).AddressStart) and (Address <= TBaseBlock( BlockList[i]).AddressLast) then
    begin
      Result := TBaseBlock( BlockList[i]);
      Break;
    end;
    Inc(i)
  end;
end;

function TBaseBlockList.GetAddressIncrement: DWord;
begin
  Result := Owner.HexInfo.AddressIncrement
end;

function TBaseBlockList.GetBytesPerInstruction: DWord;
begin
  Result := Owner.HexInfo.BytesPerInstruction;
end;

procedure TBaseBlockList.DeleteBlock(Block: TBaseBlock);
var
  i: Integer;
begin
  if Assigned(Block) then
  begin
    i := BlockList.IndexOf(Block);
    if i > -1 then
      BlockList.Delete(i);
  end;
end;

{ TPhysicalEraseBlockList }

function TPhysicalEraseBlockList.AddBlock(AnAddressStart: DWord; AnEraseBlockSize: DWord): TPhysicalEraseBlock;
begin
  Result := TPhysicalEraseBlock.Create(Self, AnAddressStart, AnEraseBlockSize);
  BlockList.Add(Result);
end;

function TPhysicalEraseBlockList.BlockCovered(ABlock: TBaseBlock): Boolean;
//
// Checks to see if the Address Block passed is covered by any Erase Block in the
// EraseBlockList.  In other words if the memory is erased in this list the passed
// address block can safely be written to (Flash must be erased as a write can only set
// a bit not clear it (or vice-versa)
var
  i: Integer;
  TestEraseBlock: TPhysicalEraseBlock;
begin
  i := 0;
  Result := False;
  while (i < Count) and not Result do
  begin
    TestEraseBlock := Block[i];
    if (ABlock.AddressStart >= TestEraseBlock.AddressStart) and (ABlock.AddressStart <= (TestEraseBlock.AddressLast+TestEraseBlock.AddressIncrement)) then
    begin
      if (ABlock.AddressLast <= TestEraseBlock.AddressLast) then
        Result := True
      else begin
        while ABlock.AddressLast > TestEraseBlock.AddressLast do
          Inc(TestEraseBlock.FBlockCount);
        Result := True;
      end;
    end;
    Inc(i);
  end;
end;

function TPhysicalEraseBlockList.Clone(Block: TPhysicalEraseBlock): TPhysicalEraseBlock;
begin
  Result := AddBlock(Block.AddressStart, Block.FBlockSize);
  Result.FBlockCount := Block.FBlockCount;
end;

function TPhysicalEraseBlockList.GetBlock(Index: Integer): TPhysicalEraseBlock;
begin
   Result := TPhysicalEraseBlock( BlockList[Index])
end;

procedure TPhysicalEraseBlockList.SetBlock(Index: Integer; AValue: TPhysicalEraseBlock);
begin
  BlockList[Index] := AValue;
end;

{ TIntelHexParser }

function TIntelHexParser.BuildEraseBlockList: Integer;
//
// This function walks the AddressBlock list and using the EraseBlockSize creates a
// list of EraseBocks.  Since normally an erase block on a mcu is MUCH larger than
// the minimum write block this function looks at the Addressblocks and find the miniumum
// number of calls to erase that will cover the bytes that need to be written.  Is
// does so such that the EraseBlock as an inital address and the number of times the
// EraseBlock in the mcu is to be called in sequence before a new address needs to be
// written.  It minimizes the data that need to be transfered across the connection
// between the mcu and PC.
//
var
  i: Integer;
  TestWriteBlock: TPhysicalWriteBlock;
  TestEraseBlock: TPhysicalEraseBlock;
  AddressesPerEraseBlock: DWord;
begin
  Result := ERROR_ERASE_BLOCK_OK;
  if HexInfo.BytesPerInstruction > EraseBlockSize then
    Result := ERROR_ERASE_BLOCK_SMALLER_THAN_INSTRUCTION
  else begin
    AddressesPerEraseBlock := EraseBlockSize div HexInfo.BytesPerInstruction * HexInfo.AddressIncrement;
    PhysicalEraseBlockList.Clear;
    if (EraseBlockSize > 0) and (PhysicalWriteBlockList.Count > 0) then
    begin
      for i := 0 to PhysicalWriteBlockList.Count - 1 do
      begin
        TestWriteBlock := PhysicalWriteBlockList[i];
        if not PhysicalEraseBlockList.BlockCovered(TestWriteBlock) then
        begin
          // Start the TestEraseBlock on an EraseBlockSize Boundry that covers the TestWriteBlock starting address EraseBlockSize is in bytes...
          TestEraseBlock := PhysicalEraseBlockList.AddBlock( (TestWriteBlock.AddressStart div AddressesPerEraseBlock) * AddressesPerEraseBlock, EraseBlockSize);
          TestEraseBlock.FBlockCount := 1;
        end;
      end;
    end;
  end;
end;

function TIntelHexParser.BuildWriteBlockList: Integer;
var
  i: Integer;
  CurrentAddress, iAddressBlock: DWord;
  TestAddressBlock: TPhysicalAddressBlock;
  WriteBlock: TBaseBlock;
  NextAddress, WriteBlockAlignedAddress, WriteBlockAddressMaxCount: DWord;
  DataArray: array of Byte;
begin
  Result := ERROR_WRITE_BLOCK_OK;
  PhysicalWriteBlockList.Clear;
  if Pack and Sorted and (PhysicalAddressBlockList.Count > 0) and (WriteBlockSize > 0) then
  begin
    if WriteBlockSize < HexInfo.BytesPerInstruction then
      Result := ERROR_WRITE_BLOCK_SMALLER_THAN_INSTRUCTION
    else begin
      // Because the AddressBlocks are packed and sorted it saves us a lot of effort here.
      // Anything contiguous is already handled by the Pack so we can just run these address blocks
      // and split them up in to WriteBufferSize chunks
      for iAddressBlock := 0 to PhysicalAddressBlockList.Count - 1 do
      begin
        TestAddressBlock := PhysicalAddressBlockList[iAddressBlock];
        WriteBlockAddressMaxCount := WriteBlockSize div TestAddressBlock.BytesPerInstruction * TestAddressBlock.AddressIncrement;

        NextAddress := TestAddressBlock.AddressStart;
        CurrentAddress := TestAddressBlock.AddressStart;
        while CurrentAddress <= TestAddressBlock.AddressLast do
        begin
          WriteBlock := PhysicalWriteBlockList.FindBlock(CurrentAddress);
          if not Assigned(WriteBlock) then
          begin
            WriteBlockAlignedAddress := (CurrentAddress div WriteBlockAddressMaxCount) * WriteBlockAddressMaxCount;
            WriteBlock := PhysicalWriteBlockList.AddBlankBlock(WriteBlockAlignedAddress, WriteBlockAddressMaxCount, $FF);
          end;
          case HexInfo.BytesPerInstruction of
            1 : WriteBlock.WriteByteInstruction(CurrentAddress, TestAddressBlock.ReadByteInstruction(CurrentAddress));
            2 : WriteBlock.WriteWordInstruction(CurrentAddress, TestAddressBlock.ReadWordInstruction(CurrentAddress));
            3 : WriteBlock.Write24BitInstruction(CurrentAddress, TestAddressBlock.Read24BitInstruction(CurrentAddress));
            4 : WriteBlock.WriteDWordInstruction(CurrentAddress, TestAddressBlock.ReadDWordInstruction(CurrentAddress));
          end;
          Inc(CurrentAddress, HexInfo.AddressIncrement);
        end;
      end;
    end;
  end;
end;

procedure TIntelHexParser.SetSorted(AValue: Boolean);
begin
  if AValue <> Fsorted then
  begin
    FSorted:=AValue;
    if FSorted then
      PhysicalAddressBlockList.FBlockList.Sort(@AddressBlockSort);
  end;
end;

constructor TIntelHexParser.Create;
begin
  inherited Create;
  FRawAddressBlockList := TPhysicalAddressBlockList.Create(Self);
  FEraseBlockList := TPhysicalEraseBlockList.Create(Self);
  FPhysicalWriteBlockList := TPhysicalWriteBlockList.Create(Self);
  FActiveBlock := nil;
  FEraseBlockSize := 4096;
  FWriteBufferSize := 4096;
  FPack := True;
  FSorted := True;
  FCustomAddressFunc := nil;
end;

destructor TIntelHexParser.Destroy;
begin
  FreeAndNil(FRawAddressBlockList);
  FreeAndNil(FEraseBlockList);
  FreeAndNil(FPhysicalWriteBlockList);
  inherited Destroy;
end;

function TIntelHexParser.ParseHex(AHexFile: string; AHexInfo: TIntelHexInfo;
  AnEraseBlockSize, AWriteBlockSize, AWriteBufferSize: DWord;
  IgnoreBounds: TIgnoreBoundsGroups; AnAlignWriteBlockOnEraseSize: Boolean
  ): Boolean;
//
// This function parses the HEX file.  It runs works its way through the file and
// creates AddressBlock objects based on a start address.  As it walks through the file
// if two consecutive data block have consecutive addresses the data is appended to
// the existing AddressBlock, if Pack is True.  If not a new AddressBlock is created with a new
// initial address and added to the list.
//
var
  Hex: TStringList;
  AddressDivisor, DataLen: DWord;
  iIgnoreGroup, iIgnoreBound: Integer;
  i, j, iStrIndex: Integer;
  TypeCode: Cardinal;
  Address, CurrentOffsetAddress, NextAddress, LastAddress: DWord;
  Line: ansistring;
  DataArray: array[0..255] of Byte;
  DynArray: TDataBlock;
  CheckSum: Byte;
  s: ansistring;
  IgnoreGroup: TIgnoreBoundsList;
  IgnoreBound: TIgnoreBound;
  PhysicalBlock: TPhysicalAddressBlock;
  DynDataArray: Array of Byte;
begin
  Result := False;
  SetLength(DynArray, 3);
  FHexFile := AHexFile;
  FHexInfo := AHexInfo;
  FEraseBlockSize := AnEraseBlockSize;
  FWriteBufferSize := AWriteBufferSize;
  FWriteBlockSize := AWriteBlockSize;
  FAlignWriteBlockOnEraseSize := AnAlignWriteBlockOnEraseSize;
  if FileExists(HexFile) then
  begin
    PhysicalAddressBlockList.Clear;
    PhysicalEraseBlockList.Clear;
    FActiveBlock := nil;
    CurrentOffsetAddress := 0;
    LastAddress := 0;
    Hex := TStringList.Create;
    try
      if HexInfo.DoubleAddress then
        AddressDivisor := 2
      else
        AddressDivisor := 1;

      Hex.LoadFromFile(HexFile);
      for i := 0 to Hex.Count - 1 do
      begin
        Line := Trim(Hex[i]);
        s := Copy(Line, 8, 2);
        TypeCode := Lo( HexToInt(s));
        s := Copy(Line, 2, 2);
        DataLen := Lo( HexToInt(s));
        s := Copy(Line, 4, 4);
        Address := DWord( HexToInt(s));

        iStrIndex := 10;
        for j := 0 to DataLen - 1 do
        begin
          s := Copy(Line, iStrIndex, 2);
          DataArray[j] := Lo( HexToInt(s));
          iStrIndex := iStrIndex + 2;
        end;
        s := Copy(Line, iStrIndex, 2);
        CheckSum := Lo( HexToInt(s));

        case TypeCode of
          TYPECODE_DATA :
              begin
                NextAddress := (CurrentOffsetAddress + Address) div AddressDivisor;
                if Pack then
                begin
                  if not Assigned(ActiveBlock) then
                    FActiveBlock := PhysicalAddressBlockList.AddBlock(NextAddress)
                  else begin
                    if (LastAddress <> NextAddress) then
                      FActiveBlock := PhysicalAddressBlockList.AddBlock(NextAddress);
                  end
                end else
                  FActiveBlock := PhysicalAddressBlockList.AddBlock(NextAddress);

                if HexInfo.BytesPerInstruction = 3 then
                begin
                  j := 0;
                  while j < DataLen do
                  begin
                    if HexInfo.HexType = iht_INH24_dsPIC30 then
                    begin
                      // 24 bit Microchip dsPIC30 MCU is odd
                      DynArray[0] := DataArray[j+2];
                      DynArray[1] := DataArray[j+0];
                      DynArray[2] := DataArray[j+1];
                    end else
                    begin
                      // 24 bit Microchip dsPIC33 MCU is odd
                      DynArray[0] := DataArray[j+0];
                      DynArray[1] := DataArray[j+1];
                      DynArray[2] := DataArray[j+2];
                    end;
                    ActiveBlock.AddChunk(DynArray, 3);
                    Inc(j, 4)
                  end
                end else
                  ActiveBlock.AddChunk(DataArray, DataLen);

                LastAddress := NextAddress + (DataLen div AddressDivisor);
              end;
          TYPECODE_EOF  :
              begin
                Result := True;
              end;
          TYPECODE_EXTENDED_SEGMENT_ADDRESS :
              begin
                WriteLn('Extended Segment Address encountered, not supported yet');
                Break;
              end;
          TYPECODE_EXTENDED_LINEAR_ADDRESS :
              begin
                Address := (DataArray[0] shl 8) or DataArray[1];
                CurrentOffsetAddress := Address shl 16;
              end;
          TYPECODE_START_LINEAR_ADDRESS :
              begin
                WriteLn('Start Linear Address encountered, not supported yet');
                Break;
              end
        else
          WriteLn('Unknown Type Command');
          Break;
        end;
      end;

      if Assigned(CustomAddressFunc) then
        CustomAddressFunc(PhysicalAddressBlockList);

      if Assigned(IgnoreBounds) then
      begin
        for iIgnoreGroup := 0 to IgnoreBounds.Count - 1 do
        begin
          IgnoreGroup := IgnoreBounds.Group[iIgnoreGroup];                      // Pick up a Group of Bounds to look at
          iIgnoreBound := IgnoreGroup.Count - 1;
          while iIgnoreBound >= 0 do                                            // Row through the Bounds of the group looking for the highest address space that exists in the Block
          begin
            IgnoreBound := IgnoreGroup.IgnoreBound[iIgnoreBound];
            for j := PhysicalAddressBlockList.Count - 1 downto 0 do
            begin
              if (PhysicalAddressBlockList.Block[j].AddressStart >= IgnoreBound.LoAddress) and (PhysicalAddressBlockList.Block[j].AddressLast <= IgnoreBound.HiAddress) then
              begin
                PhysicalAddressBlockList.DeleteBlock(PhysicalAddressBlockList.Block[j]);
                iIgnoreBound := 0;                                              // We only use the highest address bounds that are in the block, this is how we deal with configruation addresses that are different across a MCU family
              end;
            end;
            Dec(iIgnoreBound);
          end
        end;
      end;

      if Sorted then
        PhysicalAddressBlockList.FBlockList.Sort(@AddressBlockSort);

      if Pack and AlignWriteBlockOnEraseSize then
      begin
        for i := 0 to PhysicalAddressBlockList.Count - 1 do
        begin
          PhysicalBlock := PhysicalAddressBlockList.Block[i];
          if PhysicalBlock.ByteCount mod EraseBlockSize <> 0 then
          begin
            j := (PhysicalBlock.ByteCount div EraseBlockSize * EraseBlockSize) + EraseBlockSize;
            j := j - PhysicalBlock.ByteCount;
            SetLength(DynDataArray, j);
            for j := 0 to Length(DynDataArray) - 1 do
              DynDataArray[j] := $BB;
            PhysicalBlock.AddChunk(DynDataArray, Length(DynDataArray));
          end;
        end;
      end;
    finally
      Hex.Free
    end;
  end;
end;


{ TPhysicalAddressBlockList }

function TPhysicalAddressBlockList.GetBlock(Index: Integer): TPhysicalAddressBlock;
begin
  Result := TPhysicalAddressBlock( BlockList[Index])
end;

function TPhysicalAddressBlockList.AddBlankBlock(AnAddressStart: DWord; AddressCount: DWord; FillChar: Byte): TPhysicalAddressBlock;
var
  TempCount: DWord;
begin
  Result := AddBlock(AnAddressStart);
  TempCount := AddressCount * BytesPerInstruction div AddressIncrement;
  SetLength(Result.FDataBlock, TempCount);
  Result.FillBlock(FillChar);
end;

function TPhysicalAddressBlockList.AddBlock(AnAddressStart: DWord): TPhysicalAddressBlock;
begin
  Result := TPhysicalAddressBlock.Create(Self, AnAddressStart);
  BlockList.Add(Result);
end;

function TPhysicalAddressBlockList.Clone(Block: TPhysicalAddressBlock): TPhysicalAddressBlock;
begin
  Result := AddBlock(Block.AddressStart);
  Result.AddChunk(Block.DataBlock, Block.ByteCount);
end;

procedure TPhysicalAddressBlockList.SetBlock(Index: Integer; AValue: TPhysicalAddressBlock);
begin
  BlockList[Index] := AValue;
end;

{ TPhysicalAddressBlock }

constructor TPhysicalAddressBlock.Create(AnOwner: TPhysicalAddressBlockList; AnAddressStart: DWord);
begin
  inherited Create;
  FOwner := AnOwner;
  FAddressStart := AnAddressStart;
  FDataBlock := nil;
end;

end.

