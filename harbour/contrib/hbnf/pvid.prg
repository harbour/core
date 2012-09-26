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

THREAD STATIC t_aVideo := {}

FUNCTION FT_PushVid()

   AAdd( t_aVideo, { Row(), ;
      Col(), ;
      SetColor(), ;
      SaveScreen( 0, 0, MaxRow(), MaxCol() ), ;
      Set( _SET_CURSOR ), ;
      SetBlink(), ;
      NoSnow(), ;
      MaxRow() + 1, ;
      MaxCol() + 1, ;
      Set( _SET_SCOREBOARD ) } )

   RETURN Len( t_aVideo )

FUNCTION FT_PopVid()

   LOCAL nNewSize := Len( t_aVideo ) - 1
   LOCAL aBottom  := ATail( t_aVideo )

   IF nNewSize >= 0
      SetMode( aBottom[ PV_MAXROW ], aBottom[ PV_MAXCOL ] )
      SET( _SET_CURSOR, aBottom[ PV_CURSOR ] )
      NoSnow( aBottom[ PV_NOSNOW ] )
      SetBlink( aBottom[ PV_BLINK ] )
      RestScreen( 0, 0, MaxRow(), MaxCol(), aBottom[ PV_IMAGE ] )
      SetColor( aBottom[ PV_COLOR ] )
      SetPos( aBottom[ PV_ROW ], aBottom[ PV_COL ] )
      Set( _SET_SCOREBOARD, aBottom[ PV_SCORE ] )

      ASize( t_aVideo, nNewSize )
   ENDIF

   RETURN Len( t_aVideo )
