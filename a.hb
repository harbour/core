
#include "fileio.ch"

local nattr

? hb_FGetAttr( __FILE__, @nAttr )
? hb_numtohex( __tip_FAttrToUmask( nAttr ), 4 )

FUNCTION __tip_FAttrToUmask( nAttr )
   RETURN hb_bitOr( ;
      Min( hb_bitAnd( nAttr, HB_FA_SUID ), 1 ) * 0x4000, ;
      Min( hb_bitAnd( nAttr, HB_FA_SGID ), 1 ) * 0x2000, ;
      Min( hb_bitAnd( nAttr, HB_FA_SVTX ), 1 ) * 0x1000, ;
      Min( hb_bitAnd( nAttr, HB_FA_RUSR ), 1 ) * 0x0400, ;
      Min( hb_bitAnd( nAttr, HB_FA_WUSR ), 1 ) * 0x0200, ;
      Min( hb_bitAnd( nAttr, HB_FA_XUSR ), 1 ) * 0x0100, ;
      Min( hb_bitAnd( nAttr, HB_FA_RGRP ), 1 ) * 0x0040, ;
      Min( hb_bitAnd( nAttr, HB_FA_WGRP ), 1 ) * 0x0020, ;
      Min( hb_bitAnd( nAttr, HB_FA_XGRP ), 1 ) * 0x0010, ;
      Min( hb_bitAnd( nAttr, HB_FA_ROTH ), 1 ) * 0x0004, ;
      Min( hb_bitAnd( nAttr, HB_FA_WOTH ), 1 ) * 0x0002, ;
      Min( hb_bitAnd( nAttr, HB_FA_XOTH ), 1 ) * 0x0001 )
