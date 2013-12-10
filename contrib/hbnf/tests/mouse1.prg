#require "hbnf"

#ifndef __HARBOUR__
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

// Pass valid row and column values for different video modes to change modes

PROCEDURE Main( nRow, nCol )

   LOCAL nX, nY, cSavClr
   LOCAL cSavScr := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL nSaveRow := MaxRow() + 1, nSaveCol := MaxCol() + 1
   LOCAL nMinor, nType, nIRQ
   LOCAL aType := { "Bus", "Serial", "InPort", "PS/2", "HP" }
   LOCAL nHoriz, nVert, nDouble

   IF nRow == NIL
      nRow := MaxRow() + 1
   ELSE
      nRow := Val( nRow )
   ENDIF

   IF nCol == NIL
      nCol := MaxCol() + 1
   ELSE
      nCol := Val( nCol )
   ENDIF

   IF ! SetMode( nRow, nCol )
      @ MaxRow(), 0 SAY "Mode Change unsuccessful:" + Str( nRow, 2 ) + " by";
         + Str( nCol, 3 )
      RETURN
   ENDIF

   IF Empty( ft_MInit() )
      @ MaxRow(), 0 SAY "Mouse driver is not installed!"
      SetMode( nSaveRow, nSaveCol )
      RETURN
   ENDIF

   SET CURSOR OFF

   // ..... Set up the screen
   cSavClr := SetColor( "w/n" )
   @ 0, 0, MaxRow(), MaxCol() BOX hb_UTF8ToStr( "░░░░░░░░░" )

   SetColor( "GR+/RB" )
   Scroll( 7, 2, 19, 63, 0 )
   @ 7, 2 TO 20, 63

   @ 17, 10 TO 19, 40 double

   SetColor( "N/W" )
   @ 18, 11 SAY "  Double Click here to Quit  "

   SetColor( "GR+/RB" )

   // ..... Start the demo

   @ MaxRow(), 0 SAY "Driver version: " + ;
      hb_ntos( ft_MVersion( @nMinor, @nType, @nIRQ ) ) + "." + ;
      hb_ntos( nMinor )
   @ Row(), Col() SAY " " + aType[ nType ] + " mouse using IRQ " + Str( nIRQ, 2 )

   ft_MGetSens( @nHoriz, @nVert, @nDouble )  // Get the current sensitivities
   ft_MSetSens( 70, 70, 60 )    // Bump up the sensitivity of the mouse

   ft_MShowCrs()
   ft_MSetCoord( 10, 20 )  // just an arbitrary place for demo

   // put the unchanging stuff

   DevPos( 9, 10 )
   DevOut( "FT_MMICKEYS :" )

   DevPos( 10, 10 )
   DevOut( "FT_MGETPOS  :" )

   DevPos( 11, 10 )
   DevOut( "FT_MGETX    :" )

   DevPos( 12, 10 )
   DevOut( "FT_MGETY    :" )

   DevPos( 13, 10 )
   DevOut( "FT_MGETCOORD:" )

   DevPos( 14, 10 )
   DevOut( "FT_MBUTPRS  :" )

   DevPos( 16, 10 )
   DevOut( "FT_MBUTREL  :" )

   nX := nY := 1
   DO WHILE .T.

      // If we are not moving then wait for movement.
      // This whole demo is a bit artificial in its requirements when compared
      // to a "normal" CLIPPER program so some of these examples are a bit out of
      // the ordinary.

      DO WHILE nX == 0 .AND. nY == 0
         ft_MMickeys( @nX, @nY )
      ENDDO
      // tell the mouse driver where updates will be taking place so it can hide
      // the cursor when necessary.

      ft_MCOnOff( 9, 23, 16, 53 )

      DevPos( 9, 23 )
      DevOut( nX )
      DevOut( nY )

      DevPos( 10, 23 )
      DevOut( ft_MGetPos( @nX, @nY ) )
      DevOut( nX )
      DevOut( nY )

      DevPos( 11, 23 )
      DevOut( ft_MGetX() )

      DevPos( 12, 23 )
      DevOut( ft_MGetY() )

      DevPos( 13, 23 )
      DevOut( ft_MGetCoord( @nX, @nY ) )
      DevOut( nX )
      DevOut( nY )

      nX := nY := 0
      DevPos( 14, 23 )
      DevOut( ft_MButPrs( 1 ) )
      DevOut( ft_MButPrs( 0,, nX, nY ) )
      DevPos( 15, 23 )

      // show only the last Press since it flashes by so quickly

      IF nX != 0 .OR. nY != 0
         DevOut( nX )
         DevOut( nY )
      ENDIF

      nX := nY := 0
      DevPos( 16, 23 )
      DevOut( ft_MButRel( 0,, @nX, @nY ) )

      // show only the last release since it flashes by so quickly

      IF nX != 0 .OR. nY != 0
         DevOut( nX )
         DevOut( nY )
      ENDIF

      // Restore the cursor if it has been hidden

      ft_MShowCrs()

      IF ft_MInRegion( 18, 11, 18, 39 )

         // Change the type of cursor when in the box. Just slightly different than the
         // normal. The character is shown in high intensity.

         ft_MDefCrs( 0, 32767, 32512 )
         IF ft_MDblClk( 2, 0, 0.8 )
            EXIT
         ENDIF
      ENDIF

      IF ft_MInRegion( 18, 11, 18, 39 )

         // Change the type of cursor when in the box. Just slightly different than the
         // normal. The character is shown in high intensity.

         ft_MDefCrs( 0, 32767, 32512 )
      ELSE

         // Put the cursor back to normal mode

         ft_MDefCrs( 0, 30719, 30464 )
      ENDIF

      ft_MMickeys( @nX, @nY )
   ENDDO

   ft_MHideCrs()

   SetMode( nSaveRow, nSaveCol )
   SetColor( cSavClr )
   RestScreen( 0, 0, MaxRow(), MaxCol(), cSavScr )
   DevPos( MaxRow(), 0 )

   // Reset sensitivity

   ft_MSetSens( nHoriz, nVert, nDouble )

   RETURN
