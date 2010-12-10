/*
 * $Id: test.prg 14179 2010-12-08 23:34:33Z petr_ch $
 */

#include "simpleio.ch"

#define TEST_STRING "This is test of LZF extension"
#define _NREPL_     128

#define  LZF_OK              0
#define  LZF_BUF_ERROR       1
#define  LZF_MEM_ERROR       2
#define  LZF_DATA_CORRUPTED  22

PROCEDURE Main()
   LOCAL cStr, str_compressed, str_decompressed
   LOCAL b64_expected_result := "BFRoaXMgIAIUdGVzdCBvZiBMWkYgZXh0ZW5zaW9u"
   LOCAL nLen, errno := 0, nResult := 0

   ? "LZF Api version is", ;
      hb_ntos( hb_lzf_version() ) + "(0x" + hb_numtohex( hb_lzf_version() ) +")"
   ? "LibLZF optimized for", iif( hb_lzf_optimized_for_speed(), "speed.", "compression." )

   ? "--- test 1 ---"
   /*
      If the output buffer is not large enough or any error occurs
      hb_lzf_compress return NIL
   */
   cStr := TEST_STRING
   str_compressed := hb_lzf_compress( cStr, 15, @nResult )

   IF nResult == LZF_OK
      ? "Lenght of a string is", hb_ntos( Len( cStr ) )
      ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )
   ELSE
      ? "hb_lzf_compress() return ", iif( nResult == LZF_BUF_ERROR, "LZF_BUF_ERROR", "LZF_MEM_ERROR" )
   ENDIF

   ? "--- test 2 ---"
   cStr := TEST_STRING
   str_compressed := Space( 15 )
   str_compressed := hb_lzf_compress( cStr, @str_compressed, @nResult )

   IF nResult == LZF_OK
      ? "Lenght of a string is", hb_ntos( Len( cStr ) )
      ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )
   ELSE
      ? "hb_lzf_compress() return ", iif( nResult == LZF_BUF_ERROR, "LZF_BUF_ERROR", "LZF_MEM_ERROR" )
   ENDIF

   ? "--- test 3 ---"
   nLen := hb_lzf_compressBound( cStr )

   cStr := TEST_STRING
   str_compressed := hb_lzf_compress( cStr, nLen, @nResult )

   IF nResult == LZF_OK
      ? "Lenght of a string is", hb_ntos( Len( cStr ) )
      ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )

      ? hb_base64encode( str_compressed ) == b64_expected_result
   ELSE
      ? "hb_lzf_compress() return ", iif( nResult == LZF_BUF_ERROR, "LZF_BUF_ERROR", "LZF_MEM_ERROR" )
   ENDIF

   ? "--- test 4 ---"
   nLen := hb_lzf_compressBound( cStr )
   str_compressed := Space( nLen )

   cStr := TEST_STRING
   str_compressed := hb_lzf_compress( cStr, @str_compressed, @nResult )

   IF nResult == LZF_OK
      ? "Lenght of a string is", hb_ntos( Len( cStr ) )
      ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )

      ? hb_base64encode( str_compressed ) == b64_expected_result
   ELSE
      ? "hb_lzf_compress() return ", iif( nResult == LZF_BUF_ERROR, "LZF_BUF_ERROR", "LZF_MEM_ERROR" )
   ENDIF

   ? "--- test 5 ---"
   cStr := Replicate( TEST_STRING, _NREPL_ )
   str_compressed := hb_lzf_compress( cStr, NIL, @nResult )

   IF nResult == LZF_OK
      ? "Lenght of a string is", hb_ntos( Len( cStr ) )
      ? "Lenght of a compressed string is", hb_ntos( Len( str_compressed ) )
   ELSE
      ? "hb_lzf_compress() return ", iif( nResult == LZF_BUF_ERROR, "LZF_BUF_ERROR", "LZF_MEM_ERROR" )
   ENDIF

   ? "--- test 6 ---"
   str_decompressed := lzf_decompress( str_compressed, @errno, NIL )

   IF errno == LZF_DATA_CORRUPTED
      ? "LZF decompression failed, compressed data corrupted"
   ELSE
      ? cStr == str_decompressed
   ENDIF

   ? "--- test 7 ---"
   cStr := Replicate( TEST_STRING, _NREPL_ )
   str_compressed := hb_zcompress( cStr, NIL, @nResult )

   str_decompressed := lzf_decompress( str_compressed, @errno, NIL )

   IF errno == LZF_DATA_CORRUPTED
      ? "LZF decompression failed, compressed data corrupted !"
   ELSE
      ? cStr == str_decompressed
   ENDIF

   RETURN