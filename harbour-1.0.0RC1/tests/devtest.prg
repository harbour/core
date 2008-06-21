/*
 * $Id$
 */

// Test program for DEVPOS() and DEVOUT() Clipper compatibility
// The result is either devtestc.prn or devtesth.prn, depending upon
// which compiler created the program. Both files should be 263,444
// bytes in size and should be identical.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Public domain program written by David G. Holm <dholm@jsd-llc.com>
*/

procedure main()
#ifdef __HARBOUR__
   SET PRINTER TO devtesth
#else
   SET PRINTER TO devtestc
#endif
   SET DEVICE TO PRINTER
   DevPos( -2, 76 )
   ? Prow(), Pcol()
   DevOut( "First text written!" )
   ? Prow(), Pcol()
   DevOut( "Hello" )
   ? Prow(), Pcol()
   DevPos( 6, 74 )
   ? Prow(), Pcol()
   DevOut( "Off to the side!!" )
   ? Prow(), Pcol()
   DevPos( 8, -12 )
   ? Prow(), Pcol()
   DevPos( 13, -12 )
   ? Prow(), Pcol()
   DevOut( "More test text!" )
   ? Prow(), Pcol()
   DevOut( "Yet more text!" )
   ? Prow(), Pcol()
   DevPos( 19, 85 )
   ? Prow(), Pcol()
   DevPos( 500, 20 )
   ? Prow(), Pcol()
   DevOut( "!" )
   ? Prow(), Pcol()
quit