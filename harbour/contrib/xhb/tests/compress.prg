/*
 * $Id$
 */

// ****************************************************
// Demo of realtime string compression
//
// Giancarlo Niccolai

#require "xhb"

#include "hbcomprs.ch"

PROCEDURE Main()

   LOCAL cText := "A  text  to  be  compressed             "
   LOCAL cComp
   LOCAL cDecomp
   LOCAL nError, nBufLen

   CLS
   @ 1, 15 SAY "X H A R B O U R - ZLIB based compression test"

   @ 2, 5 SAY "TEST 1: using on-the-fly Buffer creation"

   nBufLen := hb_CompressBufLen( Len( cText ) )
   // cComp and cDecomp will be created with the correct length
   cComp := hb_Compress( cText )
   cDecomp := hb_Uncompress( nBuflen, cComp )

   @ 3, 7 SAY "Uncompressed: (" + hb_ntos( Len( cText ) ) + ")" + cText + "<<"
   @ 4, 7 SAY "Compressed (" + hb_ntos( Len( cComp ) ) + ")" + hb_StrToHex( cComp ) + "<<"
   @ 5, 7 SAY "Decompressed: (" + hb_ntos( Len( cDecomp ) ) + ")" + cDecomp + "<<"

   @ 7, 5 SAY "TEST 2: using preallocated buffers"

   cComp := Space( nBufLen )
   // We allocate more space (manual says 0.1% + 12, but you can never know...)
   // to allow compression of uncompressable strings to grow a little.
   cDecomp := Space( Int( nBufLen * 1.1 ) )

   // on exit, nBuflen will contain the length of the compressed buffer
   hb_Compress( cText, Len( cText ), @cComp, @nBuflen )
   hb_Uncompress( Len( cText ), cComp, nBuflen, @cDecomp )

   @ 8, 7 SAY "Uncompressed: (" + hb_ntos( Len( cText ) ) + ")" + cText + "<<"
   @ 9, 7 SAY "Compressed (" + hb_ntos( nBuflen ) + ")" + hb_StrToHex( cComp ) + "<<"
   // Notice: this time the lenght of the destination buffer is not the lenght of
   // the original buffer, but Int(nBufLen * 1.1)
   @ 10, 7 SAY "Decompressed: (" + hb_ntos( Len( cDecomp ) ) + ")" + cDecomp + "<<"

   @ 12, 5 SAY "TEST 3: Generating an error"

   nBufLen := hb_CompressBufLen( Len( cText ) )
   cComp := Space( nBufLen )
   cDecomp := Space( Len( cText ) )

   // we generate an error: 3 is not a valid length for this buffer
   nBuflen := 3
   nError := hb_Compress( cText, Len( cText ), @cComp, @nBuflen )

   IF nError != HB_Z_OK
      @ 13, 7 SAY "Error generated (" + hb_ntos( Len( cComp ) ) + ")" + ;
         hb_CompressErrorDesc( nError )
   ELSE
      @ 13, 7 SAY "NO Error generated (" + hb_ntos( Len( cComp ) ) + ")" + ;
         hb_CompressErrorDesc( nError )
   ENDIF

   @ 22, 25 SAY "Press a key to terminate"

   Inkey( 0 )

   RETURN
