#include "example.ch"
Private x,ar,y
 x = " /* 111 */ "
  /* comment string 1
             string 2
             string 3 */
 ar = { 1000, 2000, 3000, 4000,; && Comment
        5000, 6000, 7000 }
#ifdef MAXPOS
 y = MAXPOS
#else
 y = MINPOS
#endif
 y = MAX(y,10)
return