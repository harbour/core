/*
 * $Id$
 */

#include "hbapi.h"
#include <time.h>

HARBOUR HB_DATETIME( void )
{
   time_t current_time;
   char * szResult = ( char * ) hb_xgrab( 26 );

   time( &current_time );

   szResult = strcpy( szResult, ctime( &current_time ) );

   hb_retc( szResult );
   hb_xfree( szResult );
}

