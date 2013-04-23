// Harbour Class TBrowse and TBColumn sample

#include "inkey.ch"

PROCEDURE Main()

   LOCAL oBrowse := TBrowseNew( 5, 5, 16, 30 )
   LOCAL aTest0  := { "This", "is", "a", "browse", "on", "an", "array", "test", "with", "a", "long", "data" }
   LOCAL aTest1  := { 1, 2, 3, 4, 5, 6, 7, 8, 10000, - 1000, 54, 456342 }
   LOCAL aTest2  := { Date(), Date() + 4, Date() + 56, Date() + 14, Date() + 5, Date() + 6, Date() + 7, Date() + 8, Date() + 10000, Date() - 1000, Date() - 54, Date() + 456342 }
   LOCAL aTest3  := { .T., .F., .T., .T., .F., .F., .T., .F., .T., .T., .F., .F. }
   LOCAL n       := 1
   LOCAL nCursor
   LOCAL cColor
   LOCAL nRow, nCol
#ifndef HB_COMPAT_C53
   LOCAL nKey
   LOCAL nTmpRow, nTmpCol
   LOCAL lEnd    := .F.
#endif

   oBrowse:colorSpec     := "W+/B, N/BG"
   oBrowse:ColSep        := hb_UTF8ToStrBox( "│" )
   oBrowse:HeadSep       := hb_UTF8ToStrBox( "╤═" )
   oBrowse:FootSep       := hb_UTF8ToStrBox( "╧═" )
   oBrowse:GoTopBlock    := {|| n := 1 }
   oBrowse:GoBottomBlock := {|| n := Len( aTest0 ) }
   oBrowse:SkipBlock     := {| nSkip, nPos | nPos := n, ;
      n := iif( nSkip > 0, Min( Len( aTest0 ), n + nSkip ), ;
      Max( 1, n + nSkip ) ), n - nPos }

   oBrowse:AddColumn( TBColumnNew( "First",  {|| n } ) )
   oBrowse:AddColumn( TBColumnNew( "Second", {|| aTest0[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Third",  {|| aTest1[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Forth",  {|| aTest2[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Fifth",  {|| aTest3[ n ] } ) )
   oBrowse:GetColumn( 1 ):Footing := "Number"
   oBrowse:GetColumn( 2 ):Footing := "String"

   oBrowse:GetColumn( 2 ):Picture := "@!"

   oBrowse:GetColumn( 3 ):Footing := "Number"
   oBrowse:GetColumn( 3 ):Picture := "999,999.99"
   oBrowse:GetColumn( 4 ):Footing := "Dates"
   oBrowse:GetColumn( 5 ):Footing := "Logical"
   // needed since I've changed some columns _after_ I've added them to TBrowse object
   oBrowse:Configure()

   Alert( oBrowse:ClassName() )
   Alert( oBrowse:GetColumn( 1 ):ClassName() )

   oBrowse:Freeze := 1
   nCursor := SetCursor( 0 )
   cColor := SetColor( "W+/B" )
   nRow := Row()
   nCol := Col()
   hb_DispBox( 4, 4, 17, 31, hb_UTF8ToStrBox( "┌─┐│┘─└│ " ) )
#ifdef HB_COMPAT_C53
   oBrowse:SetKey( 0, {| ob, nkey | DefProc( ob, nKey ) } )
   WHILE .T.
      oBrowse:ForceStable()
      IF oBrowse:applykey( Inkey( 0 ) ) == -1
         EXIT
      ENDIF
   ENDDO
#else
   WHILE ! lEnd
      oBrowse:ForceStable()

      nKey := Inkey( 0 )

      DO CASE
      CASE nKey == K_ESC
         SetPos( 17, 0 )
         lEnd := .T.

      CASE nKey == K_DOWN
         oBrowse:Down()

      CASE nKey == K_UP
         oBrowse:Up()

      CASE nKey == K_LEFT
         oBrowse:Left()

      CASE nKey == K_RIGHT
         oBrowse:Right()

      CASE nKey == K_PGDN
         oBrowse:pageDown()

      CASE nKey == K_PGUP
         oBrowse:pageUp()

      CASE nKey == K_CTRL_PGUP
         oBrowse:goTop()

      CASE nKey == K_CTRL_PGDN
         oBrowse:goBottom()

      CASE nKey == K_HOME
         oBrowse:home()

      CASE nKey == K_END
         oBrowse:end()

      CASE nKey == K_CTRL_LEFT
         oBrowse:panLeft()

      CASE nKey == K_CTRL_RIGHT
         oBrowse:panRight()

      CASE nKey == K_CTRL_HOME
         oBrowse:panHome()

      CASE nKey == K_CTRL_END
         oBrowse:panEnd()

      CASE nKey == K_TAB
         hb_DispOutAt( 0, 0, Time() )

      ENDCASE

   ENDDO
#endif
   SetPos( nRow, nCol )
   SetColor( cColor )
   SetCursor( nCursor )

   RETURN

#ifdef HB_COMPAT_C53

STATIC FUNCTION DefProc( oBrowse, nKey )

   IF nKey == K_TAB
      hb_DispOutAt( 0, 0, Time() )
      oBrowse:Refreshall()
   ENDIF

   RETURN 1

#endif
