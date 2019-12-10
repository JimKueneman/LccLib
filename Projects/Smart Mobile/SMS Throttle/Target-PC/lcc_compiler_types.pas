unit lcc_compiler_types;

interface

{$I lcc_compilers.inc}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
  uses
    Graphics;
  {$ENDIF}
{$ENDIF}

{$I lcc_compilers.inc}


{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
  type
    TLccTextLayout = TTextLayout;
  {$ENDIF}
{$ELSE}
type
  TLccTextLayout = (tlTop, tlCenter, tlBottom);
{$ENDIF}

implementation

end.
