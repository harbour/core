/*
 * $Id$
 */

#include <extend.h>
#include <ctype.h>
#include <init.h>

#define CHR_HARD1   (char)141
#define CHR_HARD2   (char)10

HARBOUR HB_HARDCR(void);


static SYMBOL symbols[] = {
{ "HARDCR", FS_PUBLIC, HB_HARDCR, 0 }
};

HB_INIT_SYMBOLS_BEGIN( HardCR__InitSymbols );
HB_INIT_SYMBOLS_END( HardCR__InitSymbols )
#pragma startup HardCR__InitSymbols

char *hb_hardcr( char *string )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         if( *s == CHR_HARD1 && *(s+1) == CHR_HARD2 )
            *s++ = '\n';
      *s = '\0';
   }
   return string;
}

HARBOUR HB_HARDCR( void )
{
   if( ISCHAR( 1 ) )
      hb_retc( hb_hardcr( hb_parc( 1 ) ) );
   else
      hb_retc( "" );
}
