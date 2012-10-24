/*
 * $Id$
 */

#require "hbmlzo"
#require "hbbz2"
#require "hblzf"

#include "simpleio.ch"

#include "hbmlzo.ch"

#define TEST_STRING  "This is test of LZO extension"
#define FMT_STRING   "Compressed %d bytes into %d bytes (%d)"
#define FMT2_STRING  "Decompressed %d bytes back into %d bytes (%d)"

PROCEDURE Main()
   LOCAL cStr, cCompressed, cDeCompressed
   LOCAL checksum
   LOCAL nLen, nLenC, nLenD, nResult
   LOCAL k
   LOCAL aRepl := { 10, 100, 1000, 10000, 100000, 1000000 }

   /* about miniLZO */
   ?? "miniLZO -- mini subset of the LZO real-time data compression library"
   ? "Ver. " + ;
      lzo_version_string() + ;
      " (0x" + hb_numtohex( lzo_version() ) +"), " + ;
      lzo_version_date()

   ?
   cStr := TEST_STRING
   cCompressed := hb_lzo1x_1_compress( @cStr, @nLenC, @nResult )
   ShowResult( @cStr, @cCompressed, @nLenC, @nResult )

   FOR EACH k IN aRepl
      cStr := Replicate( TEST_STRING, k )

      cCompressed := hb_lzo1x_1_compress( @cStr, @nLenC, @nResult )
      ShowResult( @cStr, @cCompressed, @nLenC, @nResult )
      IF nResult != LZO_E_OK
         RETURN
      ENDIF
   NEXT

   ?
   cStr := Replicate( TEST_STRING, 500 )
   nLen := Len( cStr )
   checksum := hb_adler32( cStr )
   cCompressed := hb_lzo1x_1_compress( @cStr, @nLenC, @nResult )
   ShowResult( @cStr, @cCompressed, @nLenC, @nResult )

   /*
    * lzo1x_decompress_safe - The 'safe' decompressor. Somewhat slower.
    *
    * This decompressor will catch all compressed data violations and
    * return an error code in this case - it will never crash.
    */

   nLenD := nLenC
   cDeCompressed := hb_lzo1x_decompress_safe( @cCompressed, @nLenD, @nResult )

   IF nResult != LZO_E_OK .OR. nLenD != nLen .OR. checksum != hb_adler32( cDeCompressed )
      ? "Internal error - decompression failed: ", hb_ntos( nResult )
   ELSE
      ? hb_strFormat( FMT2_STRING, nLenC, nLenD, nLen )
   ENDIF

   /*
    * lzo1x_decompress
    * The 'standard' decompressor. Pretty fast - use this whenever possible.
    *
    * This decompressor expects valid compressed data.
    * If the compressed data gets corrupted somehow (e.g. transmission
    * via an erroneous channel, disk errors, ...) it will probably _crash_
    * your application because absolutely no additional checks are done.
    */

   nLenD := nLen
   cDeCompressed := hb_lzo1x_decompress( @cCompressed, @nLenD, @nResult )

   IF nResult != LZO_E_OK .OR. nLenD != nLen .OR. checksum != hb_adler32( cDeCompressed )
      ? "Internal error - decompression failed: ", hb_ntos( nResult )
   ELSE
      ? hb_strFormat( FMT2_STRING, nLenC, nLenD, nLen )
   ENDIF

   ?
   ? "Simple compression test passed."

/*
   cStr := Replicate( TEST_STRING, 1000000 )
   ?
   ? "BZ2 ", hb_ntos( Len( hb_bz2_compress( cStr, NIL, @nResult ) ) )
   ? "GZIP", hb_ntos( Len( hb_gzCompress( cStr, NIL, @nResult ) ) )
   ? "ZLIB", hb_ntos( Len( hb_zCompress( cStr, NIL,  @nResult ) ) )
   ? "LZF ", hb_ntos( Len( hb_lzf_compress( cStr, NIL, @nResult ) ) )
   ? "LZO ", hb_ntos( Len( hb_lzo1x_1_compress( cStr, NIL, @nResult ) ) )

   cStr := Replicate( hb_memoRead( hb_argv( 0 ) ), 50 )
   ?
   ? "BZ2 ", hb_ntos( Len( hb_bz2_compress( cStr, NIL, @nResult ) ) )
   ? "GZIP", hb_ntos( Len( hb_gzCompress( cStr, NIL, @nResult ) ) )
   ? "ZLIB", hb_ntos( Len( hb_zCompress( cStr, NIL,  @nResult ) ) )
   ? "LZF ", hb_ntos( Len( hb_lzf_compress( cStr, NIL, @nResult ) ) )
   ? "LZO ", hb_ntos( Len( hb_lzo1x_1_compress( cStr, NIL, @nResult ) ) )
*/
   RETURN

STATIC PROCEDURE ShowResult( cStr, cCompressed, nLen, nResult )

   IF nResult == LZO_E_OK
      ? hb_strFormat( FMT_STRING, Len( cStr ), nLen, Len( cCompressed ) )
   ELSEIF nResult == LZO_E_OUT_OF_MEMORY
      ? "Out of memory.."
   ELSEIF nResult == LZO_E_NOT_COMPRESSIBLE
      ? "This block contains incompressible data", hb_ntos( nLen )
   ELSE
      ? "Internal error - compression failed:", hb_ntos( nResult )
   ENDIF

   RETURN
