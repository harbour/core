/*
 * $Id
 */

/* Windows applications entry point */

#include <windows.h>

void hb_cmdargInit( int argc, char * argv[] );
void hb_vmInit( void );
void hb_vmQuit( void );

HANDLE hb_hInstance = 0, hb_hPrevInstance = 0;

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    LPSTR lpCmdLine,          /* pointer to command line */
                    int nCmdShow )             /* show state of window */
{
   char * argv[ 1 ];  /* TO DO: parse lpCmdLine and generate the proper values */

   argv[ 0 ] = 0;     /* temporary workaround */

   hb_cmdargInit( 0, argv );
   hb_vmInit();
   hb_vmQuit();

   return 0;  /* TO DO: return the proper exit value */
}
