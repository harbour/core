/*
 * $Id$
 */

/*
 * File......: pvid.prg
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   16 Oct 1992 00:05:22   GLENN
 * Just making sure we had Ted's most current revision.
 *
 *    Rev 1.0   22 Aug 1992 16:51:32   GLENN
 * Initial revision.
 */

#include "set.ch"

#define PV_ROW     1
#define PV_COL     2
#define PV_COLOR   3
#define PV_IMAGE   4
#define PV_CURSOR  5
#define PV_BLINK   6
#define PV_NOSNOW  7
#define PV_MAXROW  8
#define PV_MAXCOL  9
#define PV_SCORE  10

THREAD static aVideo := {}

function FT_PushVid()

AAdd( aVideo, { row(), ;
                col(), ;
                setcolor(), ;
                savescreen( 0, 0, maxrow(), maxcol() ), ;
                set( _SET_CURSOR ), ;
                setblink(), ;
                nosnow(), ;
                maxrow() + 1, ;
                maxcol() + 1, ;
                set( _SET_SCOREBOARD ) } )

return len( aVideo )

function FT_PopVid()

local nNewSize := len( aVideo ) - 1
local aBottom  := ATail( aVideo )

if nNewSize >= 0
   setmode( aBottom[ PV_MAXROW ], aBottom[ PV_MAXCOL ] )
   set( _SET_CURSOR, aBottom[ PV_CURSOR ] )
   nosnow( aBottom[ PV_NOSNOW ] )
   setblink( aBottom[ PV_BLINK ] )
   restscreen( 0, 0, maxrow(), maxcol(), aBottom[ PV_IMAGE ] )
   setcolor( aBottom[ PV_COLOR ] )
   setpos( aBottom[ PV_ROW ], aBottom[ PV_COL ] )
   set( _SET_SCOREBOARD, aBottom[ PV_SCORE ] )

   aSize( aVideo, nNewSize )
endif

return len( aVideo )
