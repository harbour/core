#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#include "achoice.ch"
#include "inkey.ch"

/* TOFIX: Code below demonstrates an AChoice() difference between Harbour
          and Clipper it is called with the number of items in the array is
          less than number of rows determined by ( nBottom - nTop + 1 ),
          and a user function is specified for cUserFunction. In the attached
          example, a box is drawn around the area used by AChoice() to make
          it easier to see the difference in action. When cUserFunction is
          not specified, the bottom line of the box is not overwritten.
          In Clipper, the bottom line of the box is not overwritten, but
          in Harbour it is. */

MEMVAR m_aItems

PROCEDURE Main()

   LOCAL nResult

   PRIVATE m_aItems := { ;
      "Apple", ;
      "Blueberry", ;
      "Cashew", ;
      "Grape", ;
      "Hazelnut", ;
      "Jackfruit", ;
      "Kumquat", ;
      "Mulberry" }

   CLS
   @ 7, 25 TO 8 + Len( m_aItems ), 57
   nResult := AChoice( 8, 26, 8 + Len( m_aItems ), 55, m_aItems,, "HotChoice" )
   IF nResult > 0
      Alert( m_aItems[ nResult ] + " selected" )
   ENDIF

   RETURN

FUNCTION HotChoice( nStatus, nCurrent, window_pos )  /* must be a public function */

   LOCAL nKey, cKey

   HB_SYMBOL_UNUSED( nCurrent )
   HB_SYMBOL_UNUSED( window_pos )

   DO CASE
   CASE nStatus == AC_EXCEPT
      nKey := LastKey()
      cKey := Upper( hb_keyChar( nKey ) )
      DO CASE
      CASE AScan( m_aItems, {| c | Left( c, 1 ) == cKey } ) > 0
         hb_keyPut( K_ENTER )
         RETURN AC_GOTO
      CASE nKey == K_ENTER
         RETURN AC_SELECT
      CASE nKey == K_ESC
         RETURN AC_ABORT
      OTHERWISE
         ?? Chr( 7 )
      ENDCASE
   CASE nStatus == AC_NOITEM
      RETURN AC_ABORT
   ENDCASE

   RETURN AC_CONT
