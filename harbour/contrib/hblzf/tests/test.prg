/*
 * $Id: test.prg 14179 2010-12-08 23:34:33Z petr_ch $
 */

#include "simpleio.ch"

#define TEST_STRING "This is test of LZF extension"
#define _NREPL_     128

#define  EINVAL     22 /* Invalid argument */

PROCEDURE Main()
   LOCAL cStr, str_compressed, str_decompressed
   LOCAL b64_expected_result := "BFRoaXMgIAIUdGVzdCBvZiBMWkYgZXh0ZW5zaW9u"
   LOCAL errno := 0

   ? "LZF Api version is", ;
      hb_ntos( hb_lzf_version() ) + "(0x" + hb_numtohex( hb_lzf_version() ) +")"
   ? "LibLZF optimized for", iif( hb_lzf_optimized_for_speed(), "speed.", "compression." )

   ? "--- test 1 ---"
   /*
      Lenght of output buffer is calculated as out_len := in_len + delta
      If the output buffer is not large enough or any error occurs
      hb_lzf_compress return 0
   */
   cStr := TEST_STRING
   str_compressed := lzf_compress( cStr, -1 )

   ? "Lenght of a string is", hb_ntos( Len( cStr ) )
   ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )

   ? "--- test 2 ---"
   /*
      By default ( delta == 0 ) lenght of output buffer is
      int( len( data_in ) * 1.04 ) + 1
   */

   cStr := TEST_STRING
   str_compressed := lzf_compress( cStr )

   ? "Lenght of a string is", hb_ntos( Len( cStr ) )
   ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )

   ? hb_base64encode( str_compressed ) == b64_expected_result

   ? "--- test 3 ---"
   cStr := Replicate( TEST_STRING, _NREPL_ )
   str_compressed := lzf_compress( cStr )

   ? "Lenght of a string is", hb_ntos( Len( cStr ) )
   ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )

   ? "--- test 4 ---"
   str_decompressed := lzf_decompress( str_compressed, @errno, NIL )

   IF errno == EINVAL
      ? "LZF decompression failed, compressed data corrupted"
   ELSE
      ? cStr == str_decompressed
   ENDIF

   RETURN
