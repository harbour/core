.386

HB_STARTSYMBOLS segment dword use32 public 'DATA'
HB_STARTSYMBOLS ends

HB_SYMBOLS segment dword use32 public 'DATA'
HB_SYMBOLS ends

HB_ENDSYMBOLS segment dword use32 public 'DATA'
HB_ENDSYMBOLS ends

DGROUP  group HB_STARTSYMBOLS, HB_SYMBOLS, HB_ENDSYMBOLS

    public  _hb_firstsymbol, _hb_lastsymbol

HB_STARTSYMBOLS segment
  _hb_firstsymbol label byte
HB_STARTSYMBOLS ends

HB_ENDSYMBOLS segment
  _hb_lastsymbol label byte
HB_ENDSYMBOLS ends

    end
