/*
 * $Id$
 */

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
