/*
 * $Id$
 */

/* Harbour dynamic symbol table management
 *
 * Copyright(C) 1999 by Antonio Linares.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 *
 */

#include "extend.h"
#include <ctype.h>

typedef struct
{
   PDYNSYM pDynSym;             /* Pointer to dynamic symbol */
} DYNHB_ITEM, * PDYNHB_ITEM;

#define SYM_ALLOCATED   -1

PSYMBOL  hb_NewSymbol( char * szName );
PDYNSYM  hb_FindDynSym( char * szName );

static PDYNHB_ITEM pDynItems = 0;              /* Pointer to dynamic items */
static WORD     wDynSymbols = 0;     /* Number of symbols present */
static WORD     wClosestDynSym = 0;
              /* Closest symbol for match. hb_FindDynSym will search for the name. */
              /* If it cannot find the name, it positions itself to the */
              /* closest symbol.  */

void LogSymbols( void )
{
   WORD w;

   for( w = 0; w < wDynSymbols; w++ )   /* For all dynamic symbols */
      printf( "%i %s\n", w + 1, pDynItems[ w ].pDynSym->pSymbol->szName );
}

#define RIGHT_GREATER 2
#define LEFT_GREATER  1
#define SYM_EQUAL     0

static WORD hb_strgreater( char * sz1, char * sz2 )
{
   /* Values returned : SYM_EQUAL, LEFT_GREATER, RIGHT_GREATER */

   while( *( sz1 ) && *( sz2 ) && *( sz1 ) == *( sz2 ) )
   {
     sz1++;
     sz2++;
   }
   if ( ( *( sz1 ) == 0 && *( sz2 ) != 0 ) ||
        ( *( sz2 ) > *( sz1 ) )               )
      return RIGHT_GREATER;

   if ( ( *( sz1 ) != 0 && *( sz2 ) == 0 ) ||
        ( *( sz1 ) > *( sz2 ) )               )
      return LEFT_GREATER;
   return SYM_EQUAL;
}

static void hb_strupr( char * szText )
{
   char *p;

   for( p = szText; *p; p++ )
      *p = toupper( *p );
}

PSYMBOL hb_NewSymbol( char * szName )      /* Create a new symbol */
{
   PSYMBOL pSymbol = ( PSYMBOL ) hb_xgrab( sizeof( SYMBOL ) );

   pSymbol->szName = ( char * ) hb_xgrab( strlen( szName ) + 1 );
   pSymbol->cScope = SYM_ALLOCATED; /* to know what symbols to release when exiting the app */
   strcpy( pSymbol->szName, szName );
   pSymbol->pFunPtr = NULL;
   pSymbol->pDynSym = NULL;

   return pSymbol;
}

PDYNSYM hb_NewDynSym( PSYMBOL pSymbol )    /* creates a new dynamic symbol */
{
   PDYNSYM pDynSym = hb_FindDynSym( pSymbol->szName ); /* Find position */
   WORD w;

   if( pDynSym )            /* If name exists */
   {
      if( ! ( pSymbol->cScope & ( FS_STATIC | FS_INIT | FS_EXIT ) ) ) /* only for FS_PUBLIC */
      {
         if( ( ! pDynSym->pFunPtr ) && pSymbol->pFunPtr ) /* The DynSym existed */
            pDynSym->pFunPtr = pSymbol->pFunPtr;  /* but had no function ptr assigned */
      }
      pSymbol->pDynSym = pDynSym;    /* place a pointer to DynSym */
      return pDynSym;                /* Return pointer to DynSym */
   }

   if( ! wDynSymbols )      /* Do we have any symbols ? */
      pDynSym = pDynItems[ 0 ].pDynSym;     /* Point to first symbol */
                            /* *<1>* Remember we already got this one */
   else
   {                        /* We want more symbols ! */
      pDynItems = ( PDYNHB_ITEM ) hb_xrealloc( pDynItems, ( wDynSymbols + 1 ) * sizeof( DYNHB_ITEM ) );

      if( wClosestDynSym <= wDynSymbols )   /* Closest < current !! */
      {                                     /* Here it goes :-) */
         for( w = 0; w < ( wDynSymbols - wClosestDynSym ); w++ )
            memcpy( &pDynItems[ wDynSymbols - w ],
                    &pDynItems[ wDynSymbols - w - 1 ], sizeof( DYNHB_ITEM ) );
      }                                     /* Insert element in array */
      pDynSym = ( PDYNSYM ) hb_xgrab( sizeof( DYNSYM ) );
      pDynItems[ wClosestDynSym ].pDynSym = pDynSym;    /* Enter DynSym */
   }

   wDynSymbols++;                   /* Got one more symbol */
   pDynSym->pSymbol = pSymbol;
   pDynSym->hMemvar = 0;
   pDynSym->hArea   = 0;

   if( ! ( pSymbol->cScope & ( FS_STATIC | FS_INIT | FS_EXIT ) ) ) /* only for FS_PUBLIC */
   {
      if( pDynSym->pFunPtr != pSymbol->pFunPtr ) /* it contains a function pointer */
         pDynSym->pFunPtr = pSymbol->pFunPtr;    /* place the function at DynSym */
   }
   pSymbol->pDynSym = pDynSym;                /* place a pointer to DynSym */

   return pDynSym;
}

PDYNSYM hb_GetDynSym( char * szName )  /* finds and creates a symbol if not found */
{
   PDYNSYM pDynSym;
   char * szUprName = ( char * ) hb_xgrab( strlen( szName ) + 1 );

   strcpy( szUprName, szName ); /* make a copy as we may get a const string */
   hb_strupr( szUprName );      /* turn it uppercase */

   /* if( strlen( szUprName ) > 10 )
      szUprName[ 10 ] = 0; keeps this here for 10 chars /c compatibility mode */

   pDynSym = hb_FindDynSym( szUprName );
   if( ! pDynSym )       /* Does it exists ? */
      pDynSym = hb_NewDynSym( hb_NewSymbol( szUprName ) );   /* Make new symbol */

   hb_xfree( szUprName );                                /* release memory */

   return pDynSym;
}

PDYNSYM hb_FindDynSym( char * szName )
{
   WORD wFirst = 0, wLast = wDynSymbols, wMiddle = wLast / 2;

   if( ! pDynItems )
   {
      pDynItems = ( PDYNHB_ITEM ) hb_xgrab( sizeof( DYNHB_ITEM ) );     /* Grab array */
      pDynItems->pDynSym = ( PDYNSYM ) hb_xgrab( sizeof( DYNSYM ) );
                /* Always grab a first symbol. Never an empty bucket. *<1>* */
      pDynItems->pDynSym->hMemvar = 0;
      pDynItems->pDynSym->pSymbol = 0;
      pDynItems->pDynSym->pFunPtr = 0;
      return 0;
   }
   else
   {        /* Classic Tree Insert Sort Mechanism
             *
             * Insert Sort means the new item is entered alphabetically into
             * the array. In this case pDynItems !
             *
             * 1) We start in the middle of the array.
             * 2a) If the symbols are equal -> we have found the symbol !!
             *     Champagne ! We're done.
             *  b) If the symbol we are looking for ('ge') is greater than the
             *     middle ('po'), we start looking left.
             *     Only the first part of the array is going to be searched.
             *     Go to (1)
             *  c) If the symbol we are looking for ('ge') is smaller than the
             *     middle ('ko'), we start looking right
             *     Only the last part of the array is going to be searched.
             *     Go to (1)
             */
      wClosestDynSym = wMiddle;                   /* Start in the middle      */

      while( wFirst < wLast )
      {
         switch( hb_strgreater( pDynItems[ wMiddle ].pDynSym->pSymbol->szName, szName ) )
         {
            case SYM_EQUAL:  /* they are equals */
                 return pDynItems[ wMiddle ].pDynSym;

            case LEFT_GREATER:  /* pMiddle is greater */
                 wLast = wMiddle;
                 wClosestDynSym = wMiddle;
                 break;

            case RIGHT_GREATER:  /* szName is greater */
                 wFirst = wMiddle + 1;
                 wClosestDynSym = wFirst;
                 break;
         }
         wMiddle = wFirst + ( ( wLast - wFirst ) / 2 );
      }
   }
   return 0;
}

void hb_ReleaseDynamicSymbols( void )
{
   WORD w;

   for( w = 0; w < wDynSymbols; w++ )
   {
      /* it is a allocated symbol ? */
      if( ( pDynItems + w )->pDynSym->pSymbol->cScope == (SYMBOLSCOPE)SYM_ALLOCATED )
      {
         hb_xfree( ( pDynItems + w )->pDynSym->pSymbol->szName );
         hb_xfree( ( pDynItems + w )->pDynSym->pSymbol );
      }

      hb_xfree( ( pDynItems + w )->pDynSym );
   }

   hb_xfree( pDynItems );
}

HARBOUR HB_DYNSYMNAME(void)            /* Get name of symbol */
{                               /* cSymbol = DynSymName( dsIndex ) */
   hb_retc( pDynItems[ hb_parnl( 1 ) - 1 ].pDynSym->pSymbol->szName );
}

HARBOUR HB_DYNSYMBOLS(void)            /* How much symbols do we have */
{                               /* dsCount = DynSymbols() */
   hb_retnl( wDynSymbols );
}

HARBOUR HB_GETDYNSYM(void)         /* Gimme index number of symbol */
                            /* dsIndex = hb_GetDynSym( cSymbol ) */
{
   hb_retnl( ( LONG ) hb_GetDynSym( hb_parc( 1 ) ) );
}

