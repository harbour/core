/*
 * $Id$
 */

#include <extend.h>
#include <init.h>
#include <ctype.h>

#define CHR_HARD1   (char)141
#define CHR_HARD2   (char)10

#define CHR_SOFT1   (char)13
#define CHR_SOFT2   (char)10

HARBOUR HB_MEMOTRAN(void);

static SYMBOL symbols[] = {
{ "MEMOTRAN", FS_PUBLIC, HB_MEMOTRAN, 0 }
};

HB_INIT_SYMBOLS( Memotran__InitSymbols );
/*
void Memotran__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}
*/

char *hb_memotran( char *string, char *hardcr, char *softcr )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
      {
         if( *s == CHR_HARD1 && *(s+1) == CHR_HARD2 )
            *s++ = *hardcr;
         if( *s == CHR_SOFT1 && *(s+1) == CHR_SOFT2 )
            *s++ = *softcr;
      }

      *s = '\0';
   }
   return string;
}

HARBOUR HB_MEMOTRAN( void )
{
   if( ISCHAR( 1 ) )
   {
      char *hardcr  = ISCHAR( 2 ) ? hb_parc( 2 ):";";
      char *softcr  = ISCHAR( 3 ) ? hb_parc( 3 ):" ";

      hb_retc( hb_memotran( hb_parc( 1 ), hardcr, softcr ) );
   }
   else
     hb_retc( "" );
}
