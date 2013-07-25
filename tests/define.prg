#ifndef TEST
#warning Warning: This sample must be tested using /dTEST compiler flag
#endif

#define FIRST
#define SECOND
#define THIRD

PROCEDURE Main()

   ? "testing Harbour /d compiler flag"

   #ifdef TEST
      ? "Fine, you have just tested the /d compiler flag"
   #else
      ? "Please use /dTEST compiler flag"
      ? "Or run 'set HB_USER_PRGFLAGS=/dTEST' if you are using the GNU Make System"
   #endif

   #ifdef FIRST
      ? "FIRST is defined"

      #ifdef SECOND
         ? "FIRST and SECOND are defined"

         #ifdef THIRD
            ? "FIRST, SECOND and THIRD are defined"
         #else
            ? "THIRD is not defined"
         #endif

      #else
         ? "SECOND is not defined"
      #endif

   #else
      ? "FIRST is not defined"
   #endif

   RETURN
