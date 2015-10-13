#include "inkey.ch"

PROCEDURE Main()

   LOCAL nb

   // hb_keyPut( { K_END, K_END, K_UP, K_HOME } )            // Visual glitch
   // hb_keyPut( { K_CTRL_END, K_CTRL_END, K_CTRL_HOME } )   // RTE

   CLS
   nb := 20
   DispBox( 4, 9, nb + 1, 51,, "W+/B,N/BG" )
   AChoice( 5, 10, nb, 50, ;
      { "menu 1", "menu 2", "menu 3", "menu 4", "menu 5", "menu 6", "menu 7", "menu 8", "menu 9", "menu 10" }, ;
      { .T., .T., .T., .T., .F., .F., .F., .T., .F., .F. } )

   RETURN
