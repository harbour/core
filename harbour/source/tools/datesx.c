#include <extend.h>
#include <time.h>

HARBOUR DATETIME( void )
{
   time_t current_time;
   char * szResult = ( char * ) _xgrab( 26 );

   time( &current_time );

   szResult = strcpy( szResult, ctime( &current_time ) );

   _retc( szResult );
   _xfree( szResult );
}

