/* This is an original work by Ted Means and is placed in the public domain.

      Rev 1.1   16 Oct 1992 00:05:22   GLENN
   Just making sure we had Ted's most current revision.

      Rev 1.0   22 Aug 1992 16:51:32   GLENN
   Initial revision.
 */

#define PV_ROW      1
#define PV_COL      2
#define PV_COLOR    3
#define PV_IMAGE    4
#define PV_CURSOR   5
#define PV_BLINK    6
#define PV_NOSNOW   7
#define PV_MAXROW   8
#define PV_MAXCOL   9
#define PV_SCORE    10

THREAD STATIC t_aVideo := {}

FUNCTION ft_PushVid()

   AAdd( t_aVideo, { ;
      Row(), ;
      Col(), ;
      SetColor(), ;
      SaveScreen( 0, 0, MaxRow(), MaxCol() ), ;
      SetCursor(), ;
      SetBlink(), ;
      NoSnow(), ;
      MaxRow() + 1, ;
      MaxCol() + 1, ;
      Set( _SET_SCOREBOARD ) } )

   RETURN Len( t_aVideo )

FUNCTION ft_PopVid()

   LOCAL nSize := Len( t_aVideo )
   LOCAL aBottom

   IF nSize >= 1
      aBottom := ATail( t_aVideo )

      SetMode( aBottom[ PV_MAXROW ], aBottom[ PV_MAXCOL ] )
      SetCursor( aBottom[ PV_CURSOR ] )
      NoSnow( aBottom[ PV_NOSNOW ] )
      SetBlink( aBottom[ PV_BLINK ] )
      RestScreen( 0, 0, MaxRow(), MaxCol(), aBottom[ PV_IMAGE ] )
      SetColor( aBottom[ PV_COLOR ] )
      SetPos( aBottom[ PV_ROW ], aBottom[ PV_COL ] )
      Set( _SET_SCOREBOARD, aBottom[ PV_SCORE ] )

      ASize( t_aVideo, nSize - 1 )
   ENDIF

   RETURN nSize
