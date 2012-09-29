/*
 * $Id$
 */

/*
 * Author....: Leo Letendre
 * CIS ID....: 73607,233
 *
 * This is an original work by Robert DiFalco and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.9   17 Oct 1992 16:28:58   GLENN
 * Leo cleaned up documentation blocks.
 *
 *    Rev 1.8   28 Sep 1992 01:38:14   GLENN
 * Leo added FT_MGETSENS(), FT_MSETSENS(), FT_MSETPAGE(), FT_MGETPAGE(),
 * and FT_MVERSION().
 *
 *
 *    Rev 1.7   01 Jul 1992 01:45:18   GLENN
 * Leo added documentation to FT_MDEFCRS and others. Added FT_MCONOFF(),
 * FT_MINIT(), FT_MGETCOORD() and FT_MSETCOORD().  Restructured
 * FT_MGETX() and FT_MGETY() for possible speed improvement and MAIN to
 * better demonstrate some of the concerns when programming the mouse.
 * Added ability to change the number of rows and columns to demonstrate
 * use in 43, 50 row mode etc. which is now supported in FT_MRESET() and
 * FT_MINIT().
 *
 *    Rev 1.6   23 Sep 1991 01:14:38   GLENN
 * Corrected errors in syntax documention for FT_MBUTPRS() and FT_MDBLCLK(),
 * found by Nantucket's Steve Silverwood.
 *
 *    Rev 1.5   17 Aug 1991 15:34:52   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.4   15 Aug 1991 23:06:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.3   17 Jul 1991 22:28:40   GLENN
 * Leo fixed a potential bug in ft_mcursor().
 *
 *    Rev 1.2   27 May 1991 13:40:30   GLENN
 * Leo Letendre sent me a revision of mouse1.prg where he built in support
 * for a three-button mouse, and revised the "double click" detection
 * algorithm.
 *
 * Brought in compliance with new ft_int86().
 *
 *    Rev 1.1   11 May 1991 00:16:48   GLENN
 * ft_mgetpos() had a bug where the x and y coordinates were reversed.
 * Changed x coordinate to aRegs[3] and y coordinate to aRegs[4], just
 * like in ft_msetpos().
 *
 *    Rev 1.0   01 Apr 1991 01:01:48   GLENN
 * Nanforum Toolkit
 *
 */

// The original mouse routines were written by Robert diFalco but
// Leo Letendre has made such major additions and modifications
// and fixes that I've given him sole credit. -- G. Scott

#include "ftint86.ch"

THREAD STATIC t_aReg[ 10 ]
THREAD STATIC t_lCrsState := .F.
THREAD STATIC t_lMinit := .F.

FUNCTION FT_MINIT()

   // If not previously initialized then try

   IF ! t_lMinit
      t_lMinit := ( FT_MRESET() != 0 )
   ELSE
      // Reset maximum x and y limits

      FT_MYLIMIT( 0, 8 * MaxRow() )
      FT_MXLIMIT( 0, 8 * MaxCol() )
   ENDIF

   RETURN t_lMinit

FUNCTION FT_MRESET()

   t_aReg[ AX ] := 0       // set mouse function call 0
   FT_INT86( 51, t_aReg )  // execute mouse interrupt
   t_lCrsState := .F.      // Cursor is off after reset

   // Reset maximum x and y limits

   FT_MYLIMIT( 0, 8 * MaxRow() )
   FT_MXLIMIT( 0, 8 * MaxCol() )

   RETURN t_aReg[ AX ]         // return status code

FUNCTION FT_MCURSOR( lState )

   LOCAL lSavState := t_lCrsState

   IF HB_ISLOGICAL( lState )
      IF ( t_lCrsState := lState )
         FT_MSHOWCRS()
      ELSE
         FT_MHIDECRS()
      ENDIF
   ENDIF

   RETURN lSavState

FUNCTION FT_MSHOWCRS()

   t_aReg[ AX ] := 1         // set mouse function call 1
   FT_INT86( 51, t_aReg )    // execute mouse interrupt
   t_lCrsState := .T.

   RETURN NIL                // no output from function

FUNCTION FT_MHIDECRS()       // decrement internal cursor flag and hide cursor

   t_aReg[ AX ] := 2         // set mouse function call 2
   FT_INT86( 51, t_aReg )    // execute mouse interrupt
   t_lCrsState := .F.

   RETURN NIL              // no output from function

FUNCTION FT_MGETPOS( nX, nY )

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   t_aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, t_aReg )           // execute mouse interrupt
   nX := t_aReg[ DX ]               // store new x-coordinate
   nY := t_aReg[ CX ]               // store new y-coordinate

   RETURN t_aReg[ BX ]              // return button status

FUNCTION FT_MGETCOORD( nX, nY )

   // Duplicated code from FT_MGETPOS() for speed reasons

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   t_aReg[ AX ] := 3               // set mouse function call 3
   FT_INT86( 51, t_aReg )          // execute mouse interrupt
   nX := Int( t_aReg[ DX ] / 8 )   // store new x-coordinate
   nY := Int( t_aReg[ CX ] / 8 )   // store new y-coordinate

   RETURN t_aReg[ BX ]             // return button status

FUNCTION FT_MGETX()

   // Duplicated code from FT_MGETPOS() for speed reasons

   t_aReg[ AX ] := 3              // set mouse function call 3
   FT_INT86( 51, t_aReg )         // execute mouse interrupt

   RETURN Int( t_aReg[ DX ] / 8 ) // return x-coordinate

FUNCTION FT_MGETY()

   // Duplicated code from FT_MGETPOS() for speed reasons

   t_aReg[ AX ] := 3              // set mouse function call 3
   FT_INT86( 51, t_aReg )         // execute mouse interrupt

   RETURN Int( t_aReg[ CX ] / 8 ) // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location

   t_aReg[ AX ] := 4             // set mouse function call 4
   t_aReg[ CX ] := nY            // assign new x-coordinate
   t_aReg[ DX ] := nX            // assign new y-coordinate
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN NIL                    // no function output

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location

   t_aReg[ AX ] := 4             // set mouse function call 4
   t_aReg[ CX ] := nY * 8        // assign new x-coordinate
   t_aReg[ DX ] := nX * 8        // assign new y-coordinate
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN NIL                     // no function output

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates

   t_aReg[ AX ] := 7                    // set mouse function call 7
   t_aReg[ CX ] := nXMin                // load vertical minimum parameter
   t_aReg[ DX ] := nXMax                // load vertical maximum parameter
   FT_INT86( 51, t_aReg )               // execute mouse interrupt

   RETURN NIL

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates

   t_aReg[ AX ] := 8                   // set mouse function call 8
   t_aReg[ CX ] := nYMin               // load horz minimum parameter
   t_aReg[ DX ] := nYMax               // load horz maximum parameter
   FT_INT86( 51, t_aReg )              // execute mouse interrupt

   RETURN NIL                          // no function output

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information

   t_aReg[ AX ] := 5            // set mouse function call 5
   t_aReg[ BX ] := nButton      // pass parameter for left or right button
   FT_INT86( 51, t_aReg )       // execute mouse interrupt
   nButPrs := t_aReg[ BX ] // store updated press count
   nX := t_aReg[ DX ]      // x-coordinate at last press
   nY := t_aReg[ CX ]      // y-coordinate at last press

   RETURN t_aReg[ AX ]               // return button status

FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information

   t_aReg[ AX ] := 6             // set mouse function call 6
   t_aReg[ BX ] := nButton       // pass parameter for left or right button
   FT_INT86( 51, t_aReg )        // execute mouse interrupt
   nButRel := t_aReg[ BX ]       // store updated release count
   nX := t_aReg[ DX ]            // x-coordinate at last release
   nY := t_aReg[ CX ]            // y-coordinate at last release

   RETURN t_aReg[ AX ]           // return button status

/*
FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks

   t_aReg[ AX ] := 10        // set mouse function call 10
   t_aReg[ BX ] := nCurType  // load cursor type parameter
   t_aReg[ CX ] := nScrMask  // load screen mask value
   t_aReg[ DX ] := nCurMask  // load cursor mask value
   FT_INT86( 51, t_aReg )    // execute mouse interrupt

   RETURN NIL                // no function output
*/
