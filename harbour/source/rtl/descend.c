#include <extend.h>
#include <ctype.h>

void ProcessSymbols( SYMBOL *, WORD );

HARBOUR HB_DESCEND(void);

static SYMBOL symbols[] = {
{ "DESCEND", FS_PUBLIC, HB_DESCEND, 0 }
};

void Descend__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}

char *hb_strdescend( char *string )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         *s = 256 - *s;
   }
   return string;
}

HARBOUR HB_DESCEND( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pItem = _param(1, IT_ANY );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
            _retc( hb_strdescend( pItem->value.szText ) );
         else if( IS_DATE( pItem ) )
            _retnl( 5231808 - pItem->value.lDate );
         else if( IS_INTEGER( pItem ) )
            _retnd( -1 * pItem->value.iNumber );
         else if( IS_LONG( pItem ) )
            _retnd( -1 * pItem->value.lNumber );
         else if( IS_DOUBLE( pItem ) )
            _retnd( -1 * pItem->value.dNumber );
         else if( IS_LOGICAL( pItem ) )
            _retl( !pItem->value.iLogical );
         else
            _retc( "NIL" );
      }
   }
}
