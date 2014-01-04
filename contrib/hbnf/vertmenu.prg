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
 *    Rev 1.1   15 Aug 1991 23:04:48   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   01 Apr 1991 01:02:26   GLENN
 * Nanforum Toolkit
 *
 */

/* display vertical menu */

PROCEDURE ft_Menu2( aMenuInfo, cColors )

   LOCAL nChoice     := 1
   LOCAL nOptions    := Len( aMenuInfo )
   LOCAL nMaxwidth   := 0
   LOCAL nLeft
   LOCAL x
   LOCAL cOldscreen
   LOCAL nTop
   LOCAL lOldwrap    := Set( _SET_WRAP, .T. )
   LOCAL lOldcenter  := Set( _SET_MCENTER, .T. )
   LOCAL lOldmessrow := Set( _SET_MESSAGE )
   LOCAL cOldcolor   := SetColor( cColors )

   /* if no message row has been established, use bottom row */
   IF lOldmessrow == 0
      Set( _SET_MESSAGE, MaxRow() )
   ENDIF

   /* determine longest menu option */
   AEval( aMenuInfo, {| ele | nMaxwidth := Max( nMaxwidth, Len( ele[ 1 ] ) ) } )

   /* establish top and left box coordinates */
   nLeft := ( ( MaxCol() + 1 ) - nMaxwidth ) / 2
   nTop  := ( ( MaxRow() + 1 ) - ( nOptions + 2 ) ) / 2

   DO WHILE nChoice != 0 .AND. nChoice != nOptions

      cOldscreen := SaveScreen( nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth )

      hb_DispBox( nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth, hb_UTF8ToStrBox( "┌─┐│┘─└│ " ) )
      SetPos( nTop, nLeft )
      FOR x := 1 TO Len( aMenuInfo )
         IF Len( aMenuInfo[ x ] ) > 1 .AND. aMenuInfo[ x, 2 ] != NIL
            @ Row() + 1, nLeft PROMPT PadR( aMenuInfo[ x, 1 ], nMaxwidth ) ;
               MESSAGE aMenuInfo[ x, 2 ]
         ELSE
            @ Row() + 1, nLeft PROMPT PadR( aMenuInfo[ x, 1 ], nMaxwidth )
         ENDIF
      NEXT

      MENU TO nChoice

      RestScreen( nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth, cOldscreen )

      /* execute action block attached to this option if there is one */
      IF nChoice > 0 .AND. Len( aMenuInfo[ nChoice ] ) == 3
         Eval( aMenuInfo[ nChoice, 3 ] )
      ENDIF

   ENDDO

   /* restore previous message and wrap settings */
   Set( _SET_MESSAGE, lOldmessrow )
   Set( _SET_MCENTER, lOldcenter )
   Set( _SET_WRAP,    lOldwrap )
   SetColor( cOldcolor )

   RETURN
