/*
 * $Id$
 */

/*
   Harbour Project source code

   Harbour dynamic symbol table management

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

#include "extend.h"

#define SYM_ALLOCATED ( ( SYMBOLSCOPE ) -1 )

typedef struct
{
   PHB_DYNS pDynSym;             /* Pointer to dynamic symbol */
} DYNHB_ITEM, * PDYNHB_ITEM, * DYNHB_ITEM_PTR;

static PDYNHB_ITEM s_pDynItems = NULL;    /* Pointer to dynamic items */
static WORD        s_wDynSymbols = 0;     /* Number of symbols present */
static WORD        s_wClosestDynSym = 0;
              /* Closest symbol for match. hb_dynsymFind() will search for the name. */
              /* If it cannot find the name, it positions itself to the */
              /* closest symbol.  */

void hb_dynsymLog( void )
{
   WORD w;

   for( w = 0; w < s_wDynSymbols; w++ )   /* For all dynamic symbols */
      printf( "%i %s\n", w + 1, s_pDynItems[ w ].pDynSym->pSymbol->szName );
}

PHB_SYMB hb_symbolNew( char * szName )      /* Create a new symbol */
{
   PHB_SYMB pSymbol = ( PHB_SYMB ) hb_xgrab( sizeof( HB_SYMB ) );

   pSymbol->szName = ( char * ) hb_xgrab( strlen( szName ) + 1 );
   pSymbol->cScope = SYM_ALLOCATED; /* to know what symbols to release when exiting the app */
   strcpy( pSymbol->szName, szName );
   pSymbol->pFunPtr = NULL;
   pSymbol->pDynSym = NULL;

   return pSymbol;
}

PHB_DYNS hb_dynsymNew( PHB_SYMB pSymbol )    /* creates a new dynamic symbol */
{
   PHB_DYNS pDynSym = hb_dynsymFind( pSymbol->szName ); /* Find position */

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

   if( s_wDynSymbols == 0 )   /* Do we have any symbols ? */
      pDynSym = s_pDynItems[ 0 ].pDynSym;     /* Point to first symbol */
                            /* *<1>* Remember we already got this one */
   else
   {                        /* We want more symbols ! */
      s_pDynItems = ( PDYNHB_ITEM ) hb_xrealloc( s_pDynItems, ( s_wDynSymbols + 1 ) * sizeof( DYNHB_ITEM ) );

      if( s_wClosestDynSym <= s_wDynSymbols )   /* Closest < current !! */
      {                                     /* Here it goes :-) */
         WORD w;

         for( w = 0; w < ( s_wDynSymbols - s_wClosestDynSym ); w++ )
            memcpy( &s_pDynItems[ s_wDynSymbols - w ],
                    &s_pDynItems[ s_wDynSymbols - w - 1 ], sizeof( DYNHB_ITEM ) );
      }                                     /* Insert element in array */
      pDynSym = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
      s_pDynItems[ s_wClosestDynSym ].pDynSym = pDynSym;    /* Enter DynSym */
   }

   s_wDynSymbols++;                   /* Got one more symbol */
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

PHB_DYNS hb_dynsymGet( char * szName )  /* finds and creates a symbol if not found */
{
   PHB_DYNS pDynSym;
   char * szUprName = ( char * ) hb_xgrab( strlen( szName ) + 1 );

   strcpy( szUprName, szName ); /* make a copy as we may get a const string */
   hb_strupr( szUprName );      /* turn it uppercase */

   /* if( strlen( szUprName ) > 10 )
      szUprName[ 10 ] = '\0'; keeps this here for 10 chars /c compatibility mode */

   pDynSym = hb_dynsymFind( szUprName );
   if( ! pDynSym )       /* Does it exists ? */
      pDynSym = hb_dynsymNew( hb_symbolNew( szUprName ) );   /* Make new symbol */

   hb_xfree( szUprName );                                /* release memory */

   return pDynSym;
}

PHB_DYNS hb_dynsymFind( char * szName )
{
   if( s_pDynItems == NULL )
   {
      s_pDynItems = ( PDYNHB_ITEM ) hb_xgrab( sizeof( DYNHB_ITEM ) );     /* Grab array */
      s_pDynItems->pDynSym = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
                /* Always grab a first symbol. Never an empty bucket. *<1>* */
      s_pDynItems->pDynSym->hMemvar = 0;
      s_pDynItems->pDynSym->pSymbol = NULL;
      s_pDynItems->pDynSym->pFunPtr = NULL;
      return NULL;
   }
   else
   {        /* Classic Tree Insert Sort Mechanism
             *
             * Insert Sort means the new item is entered alphabetically into
             * the array. In this case s_pDynItems !
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

      WORD wFirst = 0;
      WORD wLast = s_wDynSymbols;
      WORD wMiddle = wLast / 2;

      s_wClosestDynSym = wMiddle;                   /* Start in the middle      */

      while( wFirst < wLast )
      {
         switch( hb_strgreater( s_pDynItems[ wMiddle ].pDynSym->pSymbol->szName, szName ) )
         {
            case HB_STRGREATER_EQUAL:  /* they are equals */
                 return s_pDynItems[ wMiddle ].pDynSym;

            case HB_STRGREATER_LEFT:  /* pMiddle is greater */
                 wLast = wMiddle;
                 s_wClosestDynSym = wMiddle;
                 break;

            case HB_STRGREATER_RIGHT:  /* szName is greater */
                 wFirst = wMiddle + 1;
                 s_wClosestDynSym = wFirst;
                 break;
         }
         wMiddle = wFirst + ( ( wLast - wFirst ) / 2 );
      }
   }
   return NULL;
}

void hb_dynsymEval( PHB_DYNS_FUNC pFunction, void * Cargo )
{
   BOOL bCont = TRUE;
   WORD i;

   for( i = 0; i < s_wDynSymbols && bCont; i++ )
      bCont = ( pFunction )( s_pDynItems[ i ].pDynSym, Cargo );
}


void hb_dynsymRelease( void )
{
   WORD w;

   for( w = 0; w < s_wDynSymbols; w++ )
   {
      /* it is a allocated symbol ? */
      if( ( s_pDynItems + w )->pDynSym->pSymbol->cScope == SYM_ALLOCATED )
      {
         hb_xfree( ( s_pDynItems + w )->pDynSym->pSymbol->szName );
         hb_xfree( ( s_pDynItems + w )->pDynSym->pSymbol );
      }

      hb_xfree( ( s_pDynItems + w )->pDynSym );
   }

   hb_xfree( s_pDynItems );
}

HARBOUR HB___DYNSCOUNT( void ) /* How much symbols do we have: dsCount = __dynsymCount() */
{
   hb_retnl( s_wDynSymbols );
}

HARBOUR HB___DYNSGETNAME( void ) /* Get name of symbol: cSymbol = __dynsymGetName( dsIndex ) */
{
   hb_retc( s_pDynItems[ hb_parnl( 1 ) - 1 ].pDynSym->pSymbol->szName );
}

HARBOUR HB___DYNSGETINDEX( void ) /* Gimme index number of symbol: dsIndex = __dynsymGetIndex( cSymbol ) */
{
   hb_retnl( ( LONG ) hb_dynsymGet( hb_parc( 1 ) ) );
}

