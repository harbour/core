// Demo of realtime string compression by Giancarlo Niccolai

#require "xhb"

#include "hbcomprs.ch"

PROCEDURE Main()

   LOCAL cText := "A  text  to  be  compressed             "
   LOCAL cComp
   LOCAL cDecomp
   LOCAL nError, nBufLen

   ? "xHarbour - ZLIB based compression test"

   ?
   ? "TEST 1: using on-the-fly Buffer creation"

   nBufLen := hb_CompressBufLen( hb_BLen( cText ) )
   // cComp and cDecomp will be created with the correct length
   cComp := hb_Compress( cText )
   cDecomp := hb_Uncompress( nBuflen, cComp )

   ? "Uncompressed: (" + hb_ntos( hb_BLen( cText ) ) + ")" + cText + "<<"
   ? "Compressed (" + hb_ntos( hb_BLen( cComp ) ) + ")" + hb_StrToHex( cComp ) + "<<"
   ? "Decompressed: (" + hb_ntos( hb_BLen( cDecomp ) ) + ")" + cDecomp + "<<"

   ?
   ? "TEST 2: using preallocated buffers"

   cComp := Space( nBufLen )
   // We allocate more space (manual says 0.1% + 12, but you can never know...)
   // to allow compression of uncompressable strings to grow a little.
   cDecomp := Space( Int( nBufLen * 1.1 ) )

   // on exit, nBuflen will contain the length of the compressed buffer
   hb_Compress( cText, hb_BLen( cText ), @cComp, @nBuflen )
   hb_Uncompress( hb_BLen( cText ), cComp, nBuflen, @cDecomp )

   ? "Uncompressed: (" + hb_ntos( hb_BLen( cText ) ) + ")" + cText + "<<"
   ? "Compressed (" + hb_ntos( nBuflen ) + ")" + hb_StrToHex( cComp ) + "<<"
   // Notice: this time the length of the destination buffer is not the length of
   // the original buffer, but Int(nBufLen * 1.1)
   ? "Decompressed: (" + hb_ntos( hb_BLen( cDecomp ) ) + ")" + cDecomp + "<<"

   ?
   ? "TEST 3: Generating an error"

   nBufLen := hb_CompressBufLen( hb_BLen( cText ) )
   cComp := Space( nBufLen )

   // we generate an error: 3 is not a valid length for this buffer
   nBuflen := 3
   nError := hb_Compress( cText, hb_BLen( cText ), @cComp, @nBuflen )

   ? iif( nError != HB_Z_OK, "Error generated", "NO Error generated" ), ;
      "(" + hb_ntos( hb_BLen( cComp ) ) + ")", hb_CompressErrorDesc( nError )

   RETURN
