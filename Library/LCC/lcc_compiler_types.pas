unit lcc_compiler_types;

interface

{$IFDEF FPC}
uses
  Graphics;
{$ENDIF}

{$I lcc_compilers.inc}


{$IFDEF FPC}
type
  TLccTextLayout = TTextLayout;
{$ELSE}
type
  TLccTextLayout = (tlTop, tlCenter, tlBottom);
{$ENDIF}

implementation

end.
