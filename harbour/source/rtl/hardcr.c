#include <extend.h>
#include <ctype.h>

#define CHR_HARD1   (char)141
#define CHR_HARD2   (char)10

HARBOUR HB_HARDCR(void);

static SYMBOL symbols[] = {
{ "HARDCR", FS_PUBLIC, HB_HARDCR, 0 }
};

void HardCR__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}


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
   if( _pcount() == 1 )
   {
      PHB_ITEM pItem = _param(1, IT_STRING );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
            _retc( hb_hardcr( pItem->value.szText ) );
      }
   }
}
