/*
 * $Id$
 */

/* Harbour dynamic symbol table management */

#include <extend.h>
#include <ctype.h>

typedef struct
{
   PDYNSYM pDynSym;             /* Pointer to dynamic symbol */
} DYNITEM, * PDYNITEM;

#define SYM_ALLOCATED   -1

PSYMBOL  NewSymbol( char * szName );
PDYNSYM  FindDynSym( char * szName );

static PDYNITEM pDynItems = 0;              /* Pointer to dynamic items */
static WORD     wDynSymbols = 0;     /* Number of symbols present */
static WORD     wClosestDynSym = 0;
              /* Closest symbol for match. FindDynSym will search for the name. */
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

static WORD _strgreater( char * sz1, char * sz2 )
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

PSYMBOL NewSymbol( char * szName )      /* Create a new symbol */
{
   PSYMBOL pSymbol = ( PSYMBOL ) _xgrab( sizeof( SYMBOL ) );

   pSymbol->szName = ( char * ) _xgrab( strlen( szName ) + 1 );
   pSymbol->cScope = SYM_ALLOCATED; /* to know what symbols to release when exiting the app */
   strcpy( pSymbol->szName, szName );

   return pSymbol;
}

PDYNSYM NewDynSym( PSYMBOL pSymbol )    /* creates a new dynamic symbol */
{
   PDYNSYM pDynSym = FindDynSym( pSymbol->szName ); /* Find position */
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
      pDynItems = ( PDYNITEM ) _xrealloc( pDynItems, ( wDynSymbols + 1 ) * sizeof( DYNITEM ) );

      if( wClosestDynSym <= wDynSymbols )   /* Closest < current !! */
      {                                     /* Here it goes :-) */
         for( w = 0; w < ( wDynSymbols - wClosestDynSym ); w++ )
            memcpy( &pDynItems[ wDynSymbols - w ],
                    &pDynItems[ wDynSymbols - w - 1 ], sizeof( DYNITEM ) );
      }                                     /* Insert element in array */
      pDynSym = ( PDYNSYM ) _xgrab( sizeof( DYNSYM ) );
      pDynItems[ wClosestDynSym ].pDynSym = pDynSym;    /* Enter DynSym */
   }

   wDynSymbols++;                   /* Got one more symbol */
   pDynSym->pSymbol = pSymbol;

   if( ! ( pSymbol->cScope & ( FS_STATIC | FS_INIT | FS_EXIT ) ) ) /* only for FS_PUBLIC */
   {
      if( pDynSym->pFunPtr != pSymbol->pFunPtr ) /* it contains a function pointer */
         pDynSym->pFunPtr = pSymbol->pFunPtr;    /* place the function at DynSym */
   }
   pSymbol->pDynSym = pDynSym;                /* place a pointer to DynSym */

   return pDynSym;
}

static void OurStrUpr( char * szText )
{
   char *p;

   for( p = szText; *p; p++ )
      *p = toupper( *p );
}

PDYNSYM GetDynSym( char * szName )  /* finds and creates a symbol if not found */
{
   PDYNSYM pDynSym;
   char * szUprName = ( char * ) _xgrab( strlen( szName ) + 1 );

   strcpy( szUprName, szName ); /* make a copy as we may get a const string */
   OurStrUpr( szUprName );      /* turn it uppercase */

   /* if( strlen( szUprName ) > 10 )
      szUprName[ 10 ] = 0; keeps this here for 10 chars /c compatibility mode */

   pDynSym = FindDynSym( szUprName );
   if( ! pDynSym )       /* Does it exists ? */
      pDynSym = NewDynSym( NewSymbol( szUprName ) );   /* Make new symbol */

   _xfree( szUprName );                                /* release memory */

   return pDynSym;
}

PDYNSYM FindDynSym( char * szName )
{
   WORD wFirst = 0, wLast = wDynSymbols, wMiddle = wLast / 2;

   if( ! pDynItems )
   {
      pDynItems = ( PDYNITEM ) _xgrab( sizeof( DYNITEM ) );     /* Grab array */
      pDynItems->pDynSym = ( PDYNSYM ) _xgrab( sizeof( DYNSYM ) );
                /* Always grab a first symbol. Never an empty bucket. *<1>* */
      pDynItems->pDynSym->wMemvar = 0;
      pDynItems->pDynSym->pSymbol = 0;
      pDynItems->pDynSym->pFunPtr = 0;
      return 0;
   }
   else
   {        /* Classic Tree Insert Sort Mechanism
            //
            // Insert Sort means the new item is entered alphabetically into
            // the array. In this case pDynItems !
            //
            // 1) We start in the middle of the array.
            // 2a) If the symbols are equal -> we have found the symbol !!
            //     Champagne ! We're done.
            //  b) If the symbol we are looking for ('ge') is greater than the
            //     middle ('po'), we start looking left.
            //     Only the first part of the array is going to be searched.
            //     Go to (1)
            //  c) If the symbol we are looking for ('ge') is smaller than the
            //     middle ('ko'), we start looking right
            //     Only the last part of the array is going to be searched.
            //     Go to (1)
            */
      wClosestDynSym = wMiddle;                   /* Start in the middle      */

      while( wFirst < wLast )
      {
         switch( _strgreater( pDynItems[ wMiddle ].pDynSym->pSymbol->szName, szName ) )
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

void ReleaseDynamicSymbols( void )
{
   WORD w;

   for( w = 0; w < wDynSymbols; w++ )
   {
      /* it is a allocated symbol ? */
      if( ( pDynItems + w )->pDynSym->pSymbol->cScope == SYM_ALLOCATED )
      {
         _xfree( ( pDynItems + w )->pDynSym->pSymbol->szName );
         _xfree( ( pDynItems + w )->pDynSym->pSymbol );
      }

      _xfree( ( pDynItems + w )->pDynSym );
   }

   _xfree( pDynItems );
}

HARBOUR DYNSYMNAME()            /* Get name of symbol */
{                               /* cSymbol = DynSymName( dsIndex ) */
   _retc( pDynItems[ _parnl( 1 ) - 1 ].pDynSym->pSymbol->szName );
}

HARBOUR DYNSYMBOLS()            /* How much symbols do we have */
{                               /* dsCount = DynSymbols() */
   _retnl( wDynSymbols );
}

HARBOUR GETDYNSYM()         /* Gimme index number of symbol */
                            /* dsIndex = GetDynSym( cSymbol ) */
{
   _retnl( ( LONG ) GetDynSym( _parc( 1 ) ) );
}

