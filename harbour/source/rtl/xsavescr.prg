/* 
 * $Id$
 */

STATIC s_cScrn

PROCEDURE __XSAVESCREEN()

   s_cScrn := { Row(), Col(), SaveScreen() }

   RETURN

PROCEDURE __XRESTSCREEN()

   IF s_cScrn != NIL
      RestScreen( , , , , s_cScrn[ 3 ] )
      SetPos( s_cScrn[ 1 ], s_cScrn[ 2 ] )
      s_cScrn := NIL
   ENDIF

   RETURN
