/*
 * $Id$
 */
#include <extend.h>
#include <ctype.h>

#define CHR_HARD1   (char)141
#define CHR_HARD2   (char)10

#define CHR_SOFT1   (char)13
#define CHR_SOFT2   (char)10

HARBOUR HB_MEMOTRAN(void);

static SYMBOL symbols[] = {
{ "MEMOTRAN", FS_PUBLIC, HB_MEMOTRAN, 0 }
};

void Memotran__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}


char *hb_memotran( char *string, char *hardcr, char *softcr )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         if( *s == CHR_HARD1 && *(s+1) == CHR_HARD2 )
            *s++ = *hardcr;
         if( *s == CHR_SOFT1 && *(s+1) == CHR_SOFT2 )
            *s++ = *softcr;

      *s = '\0';
   }
   return string;
}

HARBOUR HB_MEMOTRAN( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param(1, IT_STRING );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
         {
            PHB_ITEM pItem2 = hb_param(2, IT_STRING );
            PHB_ITEM pItem3 = hb_param(3, IT_STRING );

            char *hardcr  = ( pItem2 ) ? pItem2->value.szText:";";
            char *softcr  = ( pItem3 ) ? pItem3->value.szText:" ";

            hb_retc( hb_memotran( pItem->value.szText, hardcr, softcr ) );
         }
      }
   }
}
