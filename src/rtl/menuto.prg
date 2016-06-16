/*
 * PROMPT/MENU TO commands
 *
 * Released to Public Domain by Phil Barnett <philb@iag.net>
 *
 */

#pragma -gc0

/* NOTE: Recursive use is supported. */

#include "color.ch"
#include "inkey.ch"
#include "hbmemvar.ch"
#include "setcurs.ch"

#define _ITM_ROW     1
#define _ITM_COL     2
#define _ITM_PROMPT  3
#define _ITM_MSG     4
#define _ITM_COLOR   5

THREAD STATIC t_aLevel   := {}
THREAD STATIC t_nPointer := 1

/* NOTE: <cColor> parameter is a Harbour extension. */

FUNCTION __AtPrompt( nRow, nCol, cPrompt, cMsg, cColor )

   IF t_nPointer < 1
      t_nPointer := 1
   ENDIF

   // add the current level empty array.
   DO WHILE Len( t_aLevel ) < t_nPointer
      AAdd( t_aLevel, {} )
   ENDDO

   // add to the static array
   AAdd( t_aLevel[ t_nPointer ], { ;
      nRow, ;      // _ITM_ROW
      nCol, ;      // _ITM_COL
      cPrompt, ;   // _ITM_PROMPT
      cMsg, ;      // _ITM_MSG
      cColor } )   // _ITM_COLOR

   // put this prompt on the screen right now
   DispOutAt( nRow, nCol, cPrompt, cColor )

   RETURN .F.

FUNCTION __MenuTo( bBlock, cVariable )

   LOCAL nKey, nKeyStd
   LOCAL cKey
   LOCAL y
   LOCAL q
   LOCAL n
   LOCAL lExit
   LOCAL nArrLen
   LOCAL xMsg
   LOCAL nMsgCol
   LOCAL nMsgRow
   LOCAL lMsgCenter
   LOCAL nSaveCursor
   LOCAL cSaveReadVar

   LOCAL lDeclared
   LOCAL bAction
#ifdef HB_COMPAT_C53
   LOCAL nMouseClik
#endif

   LOCAL nPointer
   LOCAL cColor
   LOCAL cColorSelect
   LOCAL cColorNormal
#ifndef HB_CLP_STRICT
   LOCAL nHiLited
#endif

   // Detect if a memvar was passed
   lDeclared := ! __mvExist( cVariable )

   IF lDeclared
      __mvPublic( cVariable )
   ENDIF

   n := Eval( bBlock )

   // if no prompts were defined, exit with 0

   IF t_nPointer < 1 .OR. t_nPointer > Len( t_aLevel )

      n := 0

   ELSE

      t_nPointer++
      nPointer := t_nPointer

      nArrLen := Len( t_aLevel[ nPointer - 1 ] )

      // put choice in a valid range

      IF ! HB_ISNUMERIC( n ) .OR. n < 1
         n := 1
      ENDIF

      IF n > nArrLen
         n := nArrLen
      ENDIF

      //

#ifndef HB_CLP_STRICT
      nHiLited := 0
#endif

      nSaveCursor := SetCursor( iif( Set( _SET_INTENSITY ), SC_NONE, NIL ) )
      cSaveReadVar := ReadVar( hb_asciiUpper( cVariable ) )
      xMsg := ""
      nMsgCol := 0
      nMsgRow := Set( _SET_MESSAGE )
      lMsgCenter := Set( _SET_MCENTER )
      lExit := .F.

      DO WHILE n != 0

         // should we display messages?
         IF nMsgRow > 0

            IF ! Empty( xMsg )
               DispOutAt( nMsgRow, nMsgCol, Space( hb_ULen( xMsg ) ) )
            ENDIF

            xMsg := t_aLevel[ nPointer - 1 ][ n ][ _ITM_MSG ]

            // Codeblock messages (yes, they are documented!)
            IF HB_ISEVALITEM( xMsg )
               xMsg := Eval( xMsg )
            ENDIF

            hb_default( @xMsg, "" )

            IF lMsgCenter
               nMsgCol := Int( ( MaxCol() - hb_ULen( xMsg ) ) / 2 )
            ENDIF

            DispOutAt( nMsgRow, nMsgCol, xMsg )

         ENDIF

         // save the current row
         q := n

         cColor := t_aLevel[ t_nPointer - 1 ][ n ][ _ITM_COLOR ]
         cColorNormal := hb_ColorIndex( iif( Empty( hb_ColorIndex( cColor, CLR_STANDARD ) ), SetColor(), cColor ), CLR_STANDARD )
         IF Set( _SET_INTENSITY )
            cColorSelect := hb_ColorIndex( iif( Empty( hb_ColorIndex( cColor, CLR_ENHANCED ) ), SetColor(), cColor ), CLR_ENHANCED )
         ELSE
            cColorSelect := cColorNormal
         ENDIF

#ifndef HB_CLP_STRICT
         // avoid flicker
         IF nHiLited != n
            nHiLited := n
#endif
            // highlight the prompt
            DispOutAt( ;
               t_aLevel[ nPointer - 1 ][ n ][ _ITM_ROW ], ;
               t_aLevel[ nPointer - 1 ][ n ][ _ITM_COL ], ;
               t_aLevel[ nPointer - 1 ][ n ][ _ITM_PROMPT ], ;
               cColorSelect )
#ifndef HB_CLP_STRICT
         ENDIF
#endif

         IF lExit
            EXIT
         ENDIF

         nKey := nKeyStd := 0
         DO WHILE nKey == 0

            // wait for a keystroke
            nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

            IF ( bAction := SetKey( nKey ) ) != NIL .OR. ;
               ( bAction := SetKey( nKeyStd ) ) != NIL

               Eval( bBlock, n )
               Eval( bAction, ProcName( 1 ), ProcLine( 1 ), hb_asciiUpper( cVariable ) )
               n := Eval( bBlock )

               DO CASE
               CASE n < 1
                  n := 1
               CASE n > nArrLen
                  n := nArrLen
               ENDCASE

               nKey := nKeyStd := 0

            ENDIF
         ENDDO

         // check for keystrokes
         SWITCH nKeyStd
#ifdef HB_COMPAT_C53
         CASE K_MOUSEMOVE
            EXIT
         CASE K_LBUTTONDOWN
         CASE K_LDBLCLK
            IF ( nMouseClik := HitTest( t_aLevel[ nPointer - 1 ], ;
               MRow(), MCol() ) ) > 0
               n := nMouseClik
            ENDIF
            /* QUESTION: Clipper does this, but shouldn't we only
                         exit when HitTest() was successful? */
            IF nKeyStd == K_LDBLCLK
               lExit := .T.
            ENDIF
            EXIT
#endif
         CASE K_DOWN
         CASE K_RIGHT
            IF ++n > nArrLen
               n := iif( Set( _SET_WRAP ), 1, nArrLen )
            ENDIF
            EXIT
         CASE K_UP
         CASE K_LEFT
            IF --n < 1
               n := iif( Set( _SET_WRAP ), nArrLen, 1 )
            ENDIF
            EXIT
         CASE K_HOME
            n := 1
            EXIT
         CASE K_END
            n := nArrLen
            EXIT
         CASE K_ENTER
         CASE K_PGUP
         CASE K_PGDN
            lExit := .T.
            EXIT
         CASE K_ESC
            n := 0
            EXIT
         OTHERWISE
            // did user hit a hot key?
            IF ! HB_ISNULL( cKey := Upper( hb_keyChar( nKeyStd ) ) )
               FOR y := 1 TO nArrLen
                  IF hb_LeftEqI( LTrim( t_aLevel[ nPointer - 1 ][ y ][ _ITM_PROMPT ] ), cKey )
                     n := y
                     lExit := .T.
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDSWITCH

         IF n != 0
#ifndef HB_CLP_STRICT
            // avoid flicker
            IF nHiLited != n
               nHiLited := 0
#endif
               DispOutAt( ;
                  t_aLevel[ nPointer - 1 ][ q ][ _ITM_ROW ], ;
                  t_aLevel[ nPointer - 1 ][ q ][ _ITM_COL ], ;
                  t_aLevel[ nPointer - 1 ][ q ][ _ITM_PROMPT ], ;
                  cColorNormal )
#ifndef HB_CLP_STRICT
            ENDIF
#endif
         ENDIF

      ENDDO

      ReadVar( cSaveReadVar )
      SetCursor( nSaveCursor )

      t_nPointer := nPointer
      t_nPointer--
      ASize( t_aLevel, t_nPointer - 1 )

   ENDIF

   Eval( bBlock, n )

   IF lDeclared
      __mvXRelease( cVariable )
   ENDIF

   SetPos( MaxRow() - 1, 0 )

   RETURN n

#ifdef HB_COMPAT_C53

STATIC FUNCTION HitTest( aMenu, nMRow, nMCol )

   LOCAL aMenuItem

   FOR EACH aMenuItem IN aMenu
      IF nMRow == aMenuItem[ _ITM_ROW ] .AND. ;
         nMCol >= aMenuItem[ _ITM_COL ] .AND. ;
         nMCol < aMenuItem[ _ITM_COL ] + hb_ULen( aMenuItem[ _ITM_PROMPT ] )

         RETURN aMenuItem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

#endif
