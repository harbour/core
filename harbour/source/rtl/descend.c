/*
 * $Id$
 */
#include <extend.h>
#include <ctype.h>

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
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param(1, IT_ANY );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
            hb_retc( hb_strdescend( pItem->value.szText ) );
         else if( IS_DATE( pItem ) )
            hb_retnl( 5231808 - pItem->value.lDate );
         else if( IS_INTEGER( pItem ) )
            hb_retnd( -1 * pItem->value.iNumber );
         else if( IS_LONG( pItem ) )
            hb_retnd( -1 * pItem->value.lNumber );
         else if( IS_DOUBLE( pItem ) )
            hb_retnd( -1 * pItem->value.dNumber );
         else if( IS_LOGICAL( pItem ) )
            hb_retl( !pItem->value.iLogical );
         else
            hb_retc( "NIL" );
      }
   }
}
