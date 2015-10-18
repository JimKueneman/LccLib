unit lcc_compiler_types;

interface

{$IFDEF FPC}
uses
  Graphics;
{$ENDIF}

{$I lcc_compilers.inc}

{$IFDEF FPC}
type
  LccString = ansistring;
  LccChar = ansichar;
  PLccString = ^ansistring;
  PLccChar = ^ansichar;
{$ELSE}
  {$IFDEF LCC_MOBILE}
  type
    LccString = string;
    LccChar = char;
    PLccString = ^string;
    PLccChar = ^char;
  {$ELSE}
  type
    LccString = ansistring;
    LccChar = ansichar;
    PLccString = ^ansistring;
    PLccChar = ^ansichar;
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
type
  TLccTextLayout = TTextLayout;
{$ELSE}
type
  TLccTextLayout = (tlTop, tlCenter, tlBottom);
{$ENDIF}

implementation

end.