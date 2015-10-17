unit lcc_compiler_types;

interface

{$I lcc_compilers.inc}

{$IFDEF FPC}
type
  LccString = ansistring;
  LccChar = ansichar;
{$ELSE}
  {$IFDEF LCC_MOBILE}
  type
    LccString = string;
    LccChar = char;
  {$ELSE}
  type
    LccString = ansistring;
    LccChar = ansichar;
  {$ENDIF}
{$ENDIF}
type
  PLccString = ^LccString;
  PLccChar = ^LccChar;

implementation

end.
