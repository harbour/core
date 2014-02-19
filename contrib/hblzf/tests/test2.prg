#require "hbbz2"
#require "hblzf"

#define _NREPL_     50

PROCEDURE Main()

   LOCAL cStr := Replicate( hb_MemoRead( hb_ProgName() ), _NREPL_ )
   LOCAL aCompressedData := { NIL, NIL, NIL, NIL }

   LOCAL hFuncs := { ;
      "GZIP" => @hb_gzCompress(), ;
      "ZLIB" => @hb_ZCompress(), ;
      "BZ2 " => @hb_bz2_Compress(), ;
      "LZF " => @hb_lzf_compress() }

   LOCAL hFuncs2 := { ;
      "GZIP" => @hb_ZUncompress(), ;
      "ZLIB" => @hb_ZUncompress(), ;
      "BZ2 " => @hb_bz2_Uncompress(), ;
      "LZF " => @hb_lzf_decompress() }

   MakeTest( @hFuncs, @aCompressedData, @cStr )
   MakeTest( @hFuncs2, @aCompressedData )

   RETURN

STATIC PROCEDURE MakeTest( ... )

   LOCAL e1, e2, cRes
   LOCAL nResult := 0
   LOCAL nBegin, nEnd
   LOCAL lCmp := ( PCount() > 2 )

   FOR EACH e1, e2 IN hb_PValue( 1 ), hb_PValue( 2 )
      nBegin := hb_SecondsCPU()
      cRes := Eval( e1:__enumValue(), iif( lCmp, hb_PValue( 3 ), e2 ), NIL, @nResult )
      nEnd := hb_SecondsCPU()
      IF lCmp
         e2 := cRes
      ENDIF
      ? hb_StrFormat( "%s: %d -> %d, Ratio %.2f%%, Times %.2f", ;
         e1:__enumKey(), ;
         Len( iif( lCmp, hb_PValue( 3 ), e2 ) ), Len( cRes ), ;
         ( Len( cRes ) / Len( iif( lCmp, hb_PValue( 3 ), e2 ) ) ) * 100, ;
         nEnd - nBegin )
   NEXT
   ?

   RETURN
