/*
 * $Id$
 */

/*
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This function is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1991 03:09:44   GLENN
 * Allowed "No peg at that location" messagee to exceed the boundary of the
 * box at the bottom of the matrix.  Just shortened the message to "No
 * piece there, per Greg's instructions.
 *
 *    Rev 1.2   15 Aug 1991 23:04:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:38   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:00   GLENN
 * Nanforum Toolkit
 *
 */

#include "inkey.ch"
#include "setcurs.ch"

#translate SINGLEBOX(<top>, <left>, <bottom>, <right>) => ;
      @ < top > , < left > , < bottom > , < right > BOX hb_UTF8ToStr( "┌─┐│┘─└│ " )
#translate DOUBLEBOX(<top>, <left>, <bottom>, <right>) => ;
      @ < top > , < left > , < bottom > , < right > BOX hb_UTF8ToStr( "╔═╗║╝═╚║ " )
MEMVAR getlist

/*
   here's the board array -- structure of which is:
   board_[xx, 1] - subarray containing box coordinates for this peg
   board_[xx, 2] - subarray containing all adjacent locations
   board_[xx, 3] - subarray containing all target locations
   board_[xx, 4] - is the location occupied or not? .T. -> Yes, .F. -> No
*/

THREAD STATIC t_board_ := {;
   { { 0, 29, 2, 34 }, { 2, 4 }, { 3, 9 }, .T. } , ;
   { { 0, 37, 2, 42 }, { 5 }, { 10 }, .T. }      , ;
   { { 0, 45, 2, 50 }, { 2, 6 }, { 1, 11 }, .T. } , ;
   { { 3, 29, 5, 34 }, { 5, 9 }, { 6, 16 }, .T. } , ;
   { { 3, 37, 5, 42 }, { 10 }, { 17 }, .T. } , ;
   { { 3, 45, 5, 50 }, { 5, 11 }, { 4, 18 }, .T. } , ;
   { { 6, 13, 8, 18 }, { 8, 14 }, { 9, 21 }, .T. } , ;
   { { 6, 21, 8, 26 }, { 9, 15 }, { 10, 22 }, .T. } , ;
   { { 6, 29, 8, 34 }, { 4, 8, 10, 16 }, { 1, 7, 11, 23 }, .T. } , ;
   { { 6, 37, 8, 42 }, { 5, 9, 11, 17 }, { 2, 8, 12, 24 }, .T. } , ;
   { { 6, 45, 8, 50 }, { 6, 10, 12, 18 }, { 3, 9, 13, 25 }, .T. } , ;
   { { 6, 53, 8, 58 }, { 11, 19 }, { 10, 26 }, .T. } , ;
   { { 6, 61, 8, 66 }, { 12, 20 }, { 11, 27 }, .T. } , ;
   { { 9, 13, 11, 18 }, { 15 }, { 16 }, .T. } , ;
   { { 9, 21, 11, 26 }, { 16 }, { 17 }, .T. } , ;
   { { 9, 29, 11, 34 }, { 9, 15, 17, 23 }, { 4, 14, 18, 28 }, .T. } , ;
   { { 9, 37, 11, 42 }, { 10, 16, 18, 24 }, { 5, 15, 19, 29 }, .F. } , ;
   { { 9, 45, 11, 50 }, { 11, 17, 19, 25 }, { 6, 16, 20, 30 }, .T. } , ;
   { { 9, 53, 11, 58 }, { 18 }, { 17 }, .T. } , ;
   { { 9, 61, 11, 66 }, { 19 }, { 18 }, .T. } , ;
   { { 12, 13, 14, 18 }, { 14, 22 }, { 7, 23 }, .T. } , ;
   { { 12, 21, 14, 26 }, { 15, 23 }, { 8, 24 }, .T. } , ;
   { { 12, 29, 14, 34 }, { 16, 22, 24, 28 }, { 9, 21, 25, 31 }, .T. } , ;
   { { 12, 37, 14, 42 }, { 17, 23, 25, 29 }, { 10, 22, 26, 32 }, .T. } , ;
   { { 12, 45, 14, 50 }, { 18, 24, 26, 30 }, { 11, 23, 27, 33 }, .T. } , ;
   { { 12, 53, 14, 58 }, { 19, 25 }, { 12, 24 }, .T. } , ;
   { { 12, 61, 14, 66 }, { 20, 26 }, { 13, 25 }, .T. } , ;
   { { 15, 29, 17, 34 }, { 23, 29 }, { 16, 30 }, .T. } , ;
   { { 15, 37, 17, 42 }, { 24 }, { 17 }, .T. } , ;
   { { 15, 45, 17, 50 }, { 25, 29 }, { 18, 28 }, .T. } , ;
   { { 18, 29, 20, 34 }, { 28, 32 }, { 23, 33 }, .T. } , ;
   { { 18, 37, 20, 42 }, { 29 }, { 24 }, .T. } , ;
   { { 18, 45, 20, 50 }, { 30, 32 }, { 25, 31 }, .T. } }

FUNCTION FT_PEGS()

   LOCAL XX, MOVE, MPOS, POSSIBLE_, BUFFER, TOPROW, OLDSCORE, MOVE2
   LOCAL SCANBLOCK, OLDCOLOR := SetColor( "w/n" )
   LOCAL oldscrn := SaveScreen( 0, 0, MaxRow(), MaxCol() )
/*
   the following code block is used in conjunction with ASCAN()
   to validate entry when there is more than one possible move
*/

   scanblock := {| a | a[ 2 ] == move2 }
   cls
   SetColor( "w/r" )
   SINGLEBOX( 22, 31, 24, 48 )
   @ 23, 33 SAY "Your move:"
   AEval( t_board_, {| a, x | HB_SYMBOL_UNUSED( a ), drawbox( x ) } )
   DO WHILE LastKey() != K_ESC .AND. moremoves()
      move := 1
      SetColor( "w/n" )
      @ 23, 44 GET move PICTURE "##" RANGE 1, 33
      READ
      IF move > 0
         DO CASE
         CASE ! t_board_[ move ][ 4 ]
            err_msg( "No piece there!" )
         OTHERWISE
            possible_ := {}
            FOR xx := 1 TO Len( t_board_[ move ][ 2 ] )
               IF t_board_[ t_board_[ move ][ 2, xx ] ][ 4 ] .AND. ;
                     ! t_board_[ t_board_[ move ][ 3, xx ] ][ 4 ]
                  AAdd( possible_, { t_board_[ move ][ 2, xx ], t_board_[ move ][ 3, xx ] } )
               ENDIF
            NEXT
            // only one available move -- do it
            DO CASE
            CASE Len( possible_ ) == 1
               // clear out original position and the position you jumped over
               t_board_[ move ][ 4 ] := t_board_[ possible_[ 1, 1 ] ][ 4 ] := .F.
               t_board_[ possible_[ 1, 2 ] ][ 4 ] := .T.
               drawbox( move, t_board_[move] )
               drawbox( possible_[ 1, 1 ] )
               drawbox( possible_[ 1, 2 ] )
            CASE Len( possible_ ) == 0
               err_msg( "Illegal move!" )
            OTHERWISE
               move2 := possible_[ 1, 2 ]
               toprow := 21 - Len( possible_ )
               SetColor( "+w/b" )
               buffer := SaveScreen( toprow, 55, 22, 74 )
               DOUBLEBOX( toprow, 55, 22, 74 )
               @ toprow, 58 SAY "Possible Moves"
               DevPos( toprow, 65 )
               AEval( possible_, {| a | DevPos( Row() + 1, 65 ), ;
                  DevOutPict( a[ 2 ], "##" ) } )
               oldscore := Set( _SET_SCOREBOARD, .F. )
               @23, 44 GET move2 PICTURE "##" ;
                  VALID AScan( possible_, scanblock ) > 0
               READ
               RestScreen( toprow, 55, 22, 74, buffer )
               Set( _SET_SCOREBOARD, oldscore )
               mpos := AScan( possible_, {| a | move2 == a[ 2 ] } )
               // clear out original position and the position you jumped over
               t_board_[ move ][ 4 ] := t_board_[ possible_[ mpos, 1 ] ][ 4 ] := .F.
               t_board_[ move2 ][ 4 ] := .T.
               drawbox( move )
               drawbox( possible_[ mpos, 1 ] )
               drawbox( move2 )

            ENDCASE
         ENDCASE
         move := 1
      ENDIF
   ENDDO
   SetColor( oldcolor )
   RestScreen( 0, 0, MaxRow(), MaxCol(), oldscrn )

   RETURN NIL

//--------------------------------------------------------------------*

STATIC FUNCTION DrawBox( nelement )

   SetColor( iif( t_board_[ nelement ][ 4 ], "+w/rb", "w/n" ) )
   @ t_board_[ nelement ][ 1, 1 ], t_board_[ nelement ][1,2], t_board_[ nelement ][ 1, 3 ], ;
      t_board_[ nelement ][ 1, 4 ] BOX hb_UTF8ToStr( "┌─┐│┘─└│ " )
   DevPos( t_board_[ nelement ][ 1, 1 ] + 1, t_board_[ nelement ][ 1, 2 ] + 2 )
   DevOut( hb_ntos( nelement ) )

   RETURN NIL

//--------------------------------------------------------------------*

STATIC FUNCTION err_msg( msg )

   LOCAL buffer := SaveScreen( 23, 33, 23, 47 )

   SetCursor( SC_NONE )
   SetColor( "+w/r" )
   @ 23, 33 SAY msg
   Inkey( 2 )
   SetCursor( SC_NORMAL )
   RestScreen( 23, 33, 23, 47, buffer )

   RETURN nil

//--------------------------------------------------------------------*

STATIC FUNCTION moremoves()

   LOCAL xx, yy, canmove := .F. , piecesleft := 0, buffer

   FOR xx := 1 TO 33
      FOR yy := 1 TO Len( t_board_[ xx ][ 2 ] )
         IF t_board_[ xx ][ 4 ] .AND.  ;            // if current location is filled
               t_board_[ t_board_[ xx ][ 2, yy ] ][ 4 ] .AND. ;  // adjacent must be filled
            ! t_board_[ t_board_[ xx ][ 3, yy ] ][ 4 ]           // target must be empty
            canmove := .T.
            EXIT
         ENDIF
      NEXT
      // increment number of pieces left
      IF t_board_[ xx ][ 4 ]
         piecesleft++
      ENDIF
   NEXT
   IF ! canmove
      SetColor( "+w/b" )
      buffer := SaveScreen( 18, 55, 21, 74 )
      DOUBLEBOX( 18, 55, 21, 74 )
      @ 19, 58 SAY "No more moves!"
      @ 20, 58 SAY hb_ntos( piecesleft ) + " pieces left"
      Inkey( 0 )
      RestScreen( 18, 55, 21, 74, buffer )
   ENDIF

   RETURN canmove

//--------------------------------------------------------------------*
