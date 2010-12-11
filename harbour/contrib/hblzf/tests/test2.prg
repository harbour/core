/*
 * $Id: test2.prg 14179 2010-12-08 23:34:33Z petr_ch $
 */

/* hbmk2 testz.prg -lhbbz2 -lbz2 -lhblzf -llzf -es2 -w3 */

#include "simpleio.ch"

#define _NREPL_     50

PROCEDURE Main()
   LOCAL cStr := Replicate( hb_memoRead( hb_argv( 0 ) ), _NREPL_ )
   LOCAL aCompressedData := { NIL, NIL, NIL, NIL }
   LOCAL hFuncs := { ;
      "GZIP" => @hb_gzCompress(), ;
      "ZLIB" => @hb_zCompress(), ;
      "BZ2 " => @hb_bz2_compress(), ;
      "LZF " => @hb_lzf_compress();
      }
   LOCAL hFuncs2 := { ;
      "GZIP" => @hb_zUncompress(), ;
      "ZLIB" => @hb_zUncompress(), ;
      "BZ2 " => @hb_bz2_uncompress(), ;
      "LZF " => @hb_lzf_decompress();
      }

   MakeTest( @hFuncs, @aCompressedData, @cStr )
   MakeTest( @hFuncs2, @aCompressedData )

   RETURN

STATIC PROCEDURE MakeTest( ... )
   LOCAL e, e2, cRes, cFmt
   LOCAL nResult := 0
   LOCAL nBegin, nEnd
   LOCAL lCmp := ( PCount() > 2 )

   FOR EACH e, e2 IN hb_pValue( 1 ), hb_pValue( 2 )
      nBegin := hb_secondsCPU()
      cRes := Eval( e:__enumValue(), iif( lCmp, hb_pValue( 3 ), e2 ), NIL, @nResult )
      nEnd := hb_secondsCPU()
      IF lCmp 
         e2 := cRes
      ENDIF
      cFmt := hb_strFormat( ;
         "%s: %d -> %d, Ratio %.2f%%, Times %.2f", ;
         e:__enumKey(), ;
         Len( IIf( lCmp, hb_pValue( 3 ), e2 ) ), Len( cRes ), ;
         ( Len( cRes ) / Len( IIf( lCmp, hb_pValue( 3 ), e2 ) ) ) * 100, ;
         nEnd - nBegin;
         )
      ? cFmt
   NEXT
   ?
   RETURN
