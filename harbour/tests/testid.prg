/*
 * $Id$
 */

#ifndef TEST
#warning Warning: This sample must be tested using /dTEST compiler flag
#endif

#define FIRST
#define SECOND
#define THIRD

PROCEDURE Main()

   QOut( "testing Harbour /d compiler flag" )

   #ifdef TEST
      QOut( "Fine, you have just tested the /d compiler flag" )
   #else
      QOut( "Please use /dTEST compiler flag" )
      QOut( "Or run 'set HB_USER_PRGFLAGS=/dTEST' if you are using the GNU Make System" )
   #endif

   #ifdef FIRST
      QOut( "FIRST is defined" )

      #ifdef SECOND
         QOut( "FIRST and SECOND are defined" )

         #ifdef THIRD
            QOut( "FIRST, SECOND and THIRD are defined" )
         #else
            QOut( "THIRD is not defined" )
         #endif

      #else
         QOut( "SECOND is not defined" )
      #endif

   #else
      QOut( "FIRST is not defined" )
   #endif

   RETURN
