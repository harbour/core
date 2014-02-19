/* Harbour Project source code
   http://harbour-project.org/
   Public domain program written by David G. Holm <dholm@jsd-llc.com>
*/

// Test program for DevPos() and DevOut() Clipper compatibility
// The result is either dev_cl.prn or dev_hb.prn, depending upon
// which compiler created the program. Both files should be 263444
// bytes in size and should be identical.

PROCEDURE Main()

#ifdef __HARBOUR__
   SET PRINTER TO dev_hb.prn
#else
   SET PRINTER TO dev_cl.prn
#endif
   SET DEVICE TO PRINTER
   DevPos( -2, 76 )
   ? PRow(), PCol()
   DevOut( "First text written!" )
   ? PRow(), PCol()
   DevOut( "Hello" )
   ? PRow(), PCol()
   DevPos( 6, 74 )
   ? PRow(), PCol()
   DevOut( "Off to the side!!" )
   ? PRow(), PCol()
   DevPos( 8, -12 )
   ? PRow(), PCol()
   DevPos( 13, -12 )
   ? PRow(), PCol()
   DevOut( "More test text!" )
   ? PRow(), PCol()
   DevOut( "Yet more text!" )
   ? PRow(), PCol()
   DevPos( 19, 85 )
   ? PRow(), PCol()
   DevPos( 500, 20 )
   ? PRow(), PCol()
   DevOut( "!" )
   ? PRow(), PCol()

   RETURN
