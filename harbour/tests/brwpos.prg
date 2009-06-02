/*
 * $Id$
 */

/********************************************************
* TEST BROWSE ROWPOS FOR COMPILER IN CLIPPER AND HARBOUR
*********************************************************/
static s_nRecNo   := 1
static s_nLastRec := 100
static s_lFixPos := .F.

Procedure Main()
   Local nRow := 1

   Cls
   @ 0,4  say 'Is current RecNo but not repositioned until FixPos .T. <F2> Change FixPos'
   @ MaxRow(),1 say 'Please press <Intro> to select or <Esc> to exit and <F2> to FixPos is '
   while( LastKey() != 27 )
      @ 0,0 say s_nRecNo picture '###'
      @ MaxRow(),68 say iif(s_lFixPos, '.T.','.F.')
      nRow := TestBrw( nRow  )
   enddo
return

Function TestBrw( nRowIni )
   Local nKey, oBrw := TBrowseNew( 1, 0, MaxRow() - 1, MaxCol() )

   oBrw:SkipBlock     := { | n | n := iif( n < 0, max( n, 1 - s_nRecNo ), ;
                                                  min( s_nLastRec - s_nRecNo, n ) ), ;
                                 s_nRecNo += n, n }
   oBrw:GoTopBlock    := { || s_nRecNo := 1 }
   oBrw:GoBottomBlock := { || s_nRecNo := s_nLastRec }
   oBrw:AddColumn( TBColumnNew( 'RecNo #', {|| s_nRecNo } ) )

   if( s_lFixPos .and. nRowIni > 1 )
      Eval( oBrw:skipBlock, 1 - nRowIni )
   endif

   oBrw:rowPos := nRowIni
   while( .T. )
      while( !oBrw:stabilize() )
      enddo
      nKey := Inkey( 0 )
      if( nKey == 27 .or. nKey == 13 )
         exit
      elseif( nKey == -1 )
         s_lFixPos := iif(s_lFixPos, .F., .T.)
         exit
      elseif( nKey == 24 )
         oBrw:Down()
      elseif( nKey == 05 )
         oBrw:Up()
      elseif( nKey == 03 )
         oBrw:pageDown()
      elseif( nKey == 18 )
         oBrw:pageUp()
      elseif( nKey == 29 ).or.( nKey == 31 )
         oBrw:goTop()
      elseif( nKey == 23 ).or.( nKey == 30 )
         oBrw:goBottom()
      elseif( nKey == 01 )
         oBrw:rowPos := 1
      elseif( nKey == 06 )
         oBrw:rowPos := s_nLastRec
      endif
   enddo
return oBrw:rowPos
/*****
* END
******/
