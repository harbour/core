/*
 * $Id$
 */

#include "hbsetup.h"

#if defined(DOS) && defined(__BORLANDC__)
  #include <limits.h>
  extern unsigned _stklen = UINT_MAX;
#endif

extern int harbour_main( int argc, char * argv[] );

int main( int argc, char * argv[] )
{
  return harbour_main(argc, argv);
}

#ifdef __IBMCPP__
int isatty (int handle)
{
   return (handle < 4) ? 1 : 0;
}
#endif
