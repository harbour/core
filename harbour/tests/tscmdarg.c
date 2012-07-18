/*
 * $Id$
 */

#include "hbapi.h"

void hb_cmdargTEST( void )
{
   char * pszArg;

   printf("INFO: %i\n", hb_cmdargCheck( "INFO" ) );
   printf("   F: %s\n", pszArg = hb_cmdargString( "F" ) ); if( pszArg ) hb_xfree( pszArg );
   printf("  Fn: %i\n", hb_cmdargNum( "F" ) );
   printf("TEMP: %s\n", pszArg = hb_cmdargString( "TEMP" ) ); if( pszArg ) hb_xfree( pszArg );

   printf("INFO: %i\n", hb_cmdargCheck( "INFO" ) );
   printf("   F: %s\n", pszArg = hb_cmdargString( "F" ) ); if( pszArg ) hb_xfree( pszArg );
   printf("  Fn: %i\n", hb_cmdargNum( "F" ) );
   printf("TEMP: %s\n", pszArg = hb_cmdargString( "TEMP" ) ); if( pszArg ) hb_xfree( pszArg );
}
