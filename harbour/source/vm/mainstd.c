/*
 * $Id
 */

/* Std applications entry point */

void hb_vmInit( void );
void hb_vmQuit( void );

int main( int argc, char * argv[] )
{
   hb_cmdargInit( argc, argv );
   hb_vmInit();
   hb_vmQuit();
}
