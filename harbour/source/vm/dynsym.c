/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic symbol table management
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"

typedef struct
{
   PHB_DYNS pDynSym;             /* Pointer to dynamic symbol */
} DYNHB_ITEM, * PDYNHB_ITEM, * DYNHB_ITEM_PTR;

static PDYNHB_ITEM s_pDynItems = NULL;    /* Pointer to dynamic items */
static USHORT      s_uiDynSymbols = 0;    /* Number of symbols present */

typedef struct _HB_SYM_HOLDER
{
   HB_SYMB  symbol;
   struct _HB_SYM_HOLDER * pNext;
   char     szName[ 1 ];
}
HB_SYM_HOLDER, * PHB_SYM_HOLDER;

static PHB_SYM_HOLDER s_pAllocSyms = NULL;


/* Closest symbol for match. hb_dynsymFind() will search for the name. */
/* If it cannot find the name, it positions itself to the */
/* closest symbol.  */
static USHORT      s_uiClosestDynSym = 0; /* TOFIX: This solution is not thread safe. */

void hb_dynsymLog( void )
{
   USHORT uiPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymLog()"));

   for( uiPos = 0; uiPos < s_uiDynSymbols; uiPos++ )   /* For all dynamic symbols */
      printf( "%i %s\n", uiPos + 1, s_pDynItems[ uiPos ].pDynSym->pSymbol->szName );
}

HB_EXPORT PHB_SYMB hb_symbolNew( const char * szName )      /* Create a new symbol */
{
   PHB_SYM_HOLDER pHolder;
   int iLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_symbolNew(%s)", szName));

   iLen = strlen( szName );
   pHolder = ( PHB_SYM_HOLDER ) hb_xgrab( sizeof( HB_SYM_HOLDER ) + iLen );
   memcpy( pHolder->szName, szName, iLen + 1 );
   pHolder->pNext = s_pAllocSyms;
   s_pAllocSyms = pHolder;

   pHolder->symbol.szName        = pHolder->szName;
   pHolder->symbol.scope.value   = 0;
   pHolder->symbol.value.pFunPtr = NULL;
   pHolder->symbol.pDynSym       = NULL;

   return &pHolder->symbol;
}

HB_EXPORT PHB_DYNS hb_dynsymNew( PHB_SYMB pSymbol )    /* creates a new dynamic symbol */
{
   PHB_DYNS pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymNew(%p)", pSymbol));

   pDynSym = hb_dynsymFind( pSymbol->szName ); /* Find position */

   if( pDynSym )            /* If name exists */
   {
      if( ( pDynSym->pSymbol->scope.value &
            pSymbol->scope.value & HB_FS_LOCAL ) != 0 &&
            pDynSym->pSymbol != pSymbol )
      {
         /* Someone is using linker which allows to create binaries
          * with multiple function definitions. It's a big chance that
          * wrong binaries are created in such case, f.e both functions
          * linked and not all references updated. Anyhow now we will
          * have to guess which symbol is the real local one [druzus]
          */
         /* Let's check if linker updated function address so both symbols
          * refer to the same function
          */
         if( pDynSym->pSymbol->value.pFunPtr == pSymbol->value.pFunPtr )
         {
            /* The addresses have been updated, f.e. in such case works GCC
             * in Linux (but not MinGW and DJGPP) if user will allow to create
             * binaries with multiple symbols by
             *    -Wl,--allow-multiple-definition
             * when whole module cannot be cleanly replaced.
             * OpenWatcom for Linux, DOS and Windows (I haven't testes OS2
             * version), POCC and XCC (with /FORCE:MULTIPLE) also update
             * addresses in such case.
             *
             * We are guessing that symbols are registered in reverted order
             * so we remove the HB_FS_LOCAL flag from previously registered
             * symbol but some linkers may use different order so it does
             * not have to be true in all cases
             */
            pDynSym->pSymbol->scope.value &= ~HB_FS_LOCAL;
         }
         else
         {
            /* We have multiple symbol with the same name which refer
             * to different public functions inside this single binary
             * Let's check if this symbol is loaded from dynamic library
             * (.so, .dll, .dyn, ...) or .hrb file
             */
            if( pSymbol->scope.value & HB_FS_PCODEFUNC )
            {
               /* It's dynamic module so we are guessing that HVM
                * intentionally not updated function address allowing
                * multiple functions, f.e. programmer asked about keeping
                * local references using HB_LIBLOAD()/__HBRLOAD() parameter.
                * In such case update pDynSym address in the new symbol but
                * do not register it as the main one
                */
               pSymbol->pDynSym = pDynSym;    /* place a pointer to DynSym */
               return pDynSym;                /* Return pointer to DynSym */
            }
            /* The multiple symbols comes from single binaries - we have to
             * decide what to do with them. We can leave it as is or we can
             * try to overload one symbol so both will point to the same
             * function. For .prg code such overloading will work but not
             * for C code which makes sth like: HB_FUNC_EXEC( funcname )
             * In such case we cannot do anything - we cannot even detect
             * such situation. In some cases even linker cannot detect it
             * because C compiler can make autoinlining or some bindings
             * which are not visible for linker
             */
            /* Let's try to overload one of the functions. Simple:
             *    pDynSym->pSymbol->value.pFunPtr = pSymbol->value.pFunPtr;
             * is not good idea because it's possible that this symbol will
             * be overloaded yet another time after preprocessing rest of
             * symbols so we will use HB_FS_DEFERRED flag which is updated
             * dynamically in hb_vmSend()/hb_vmDo() functions
             */
#define HB_OVERLOAD_MULTIPLE_FUNC

#if defined( HB_OVERLOAD_MULTIPLE_FUNC )
#if defined( __GNUC__ ) && !defined( __DJGPP__ )
            pDynSym->pSymbol->scope.value &= ~HB_FS_LOCAL;
            pDynSym->pSymbol->scope.value |= HB_FS_DEFERRED;
#else
            pSymbol->scope.value &= ~HB_FS_LOCAL;
            pSymbol->scope.value |= HB_FS_DEFERRED;
#endif
#endif
         }
      }
      if( ( !pDynSym->pSymbol->value.pFunPtr && pSymbol->value.pFunPtr ) ||
          ( pSymbol->scope.value & HB_FS_LOCAL ) != 0 )
      {
         pDynSym->pSymbol = pSymbol;
#ifndef HB_NO_PROFILER
         pDynSym->ulCalls = 0;   /* profiler support */
         pDynSym->ulTime  = 0;   /* profiler support */
         pDynSym->ulRecurse = 0; /* profiler support */
#endif
      }
      pSymbol->pDynSym = pDynSym;    /* place a pointer to DynSym */
      return pDynSym;                /* Return pointer to DynSym */
   }

   if( s_uiDynSymbols == 0 )   /* Do we have any symbols ? */
      pDynSym = s_pDynItems[ 0 ].pDynSym;     /* Point to first symbol */
                            /* *<1>* Remember we already got this one */
   else
   {                        /* We want more symbols ! */
      s_pDynItems = ( PDYNHB_ITEM ) hb_xrealloc( s_pDynItems, ( s_uiDynSymbols + 1 ) * sizeof( DYNHB_ITEM ) );

      if( s_uiClosestDynSym <= s_uiDynSymbols )   /* Closest < current !! */
      {                                     /* Here it goes :-) */
         USHORT uiPos;

         for( uiPos = 0; uiPos < ( s_uiDynSymbols - s_uiClosestDynSym ); uiPos++ )
            memcpy( &s_pDynItems[ s_uiDynSymbols - uiPos ],
                    &s_pDynItems[ s_uiDynSymbols - uiPos - 1 ], sizeof( DYNHB_ITEM ) );
      }                                     /* Insert element in array */
      pDynSym = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
      s_pDynItems[ s_uiClosestDynSym ].pDynSym = pDynSym;    /* Enter DynSym */
   }

   s_uiDynSymbols++;                   /* Got one more symbol */
   pDynSym->pSymbol  = pSymbol;
   pDynSym->hMemvar  = 0;
   pDynSym->uiArea   = 0;
   pDynSym->uiSymNum = s_uiDynSymbols;
#ifndef HB_NO_PROFILER
   pDynSym->ulCalls = 0;   /* profiler support */
   pDynSym->ulTime  = 0;   /* profiler support */
   pDynSym->ulRecurse = 0; /* profiler support */
#endif
   pSymbol->pDynSym = pDynSym;         /* place a pointer to DynSym */

   return pDynSym;
}

HB_EXPORT PHB_DYNS hb_dynsymGetCase( const char * szName )  /* finds and creates a symbol if not found */
{
   PHB_DYNS pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGetCase(%s)", szName));

   pDynSym = hb_dynsymFind( szName );
   if( ! pDynSym )       /* Does it exists ? */
      pDynSym = hb_dynsymNew( hb_symbolNew( szName ) );   /* Make new symbol */

   return pDynSym;
}

HB_EXPORT PHB_DYNS hb_dynsymGet( const char * szName )  /* finds and creates a symbol if not found */
{
   PHB_DYNS pDynSym;
   char szUprName[ HB_SYMBOL_NAME_LEN + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGet(%s)", szName));

   /* make a copy as we may get a const string, then turn it to uppercase */
   /* NOTE: This block is optimized for speed [vszakats] */
   {
      int iLen = HB_SYMBOL_NAME_LEN;
      char * pDest = szUprName;

      do
      {
         char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   pDynSym = hb_dynsymFind( szUprName );
   if( ! pDynSym )       /* Does it exists ? */
      pDynSym = hb_dynsymNew( hb_symbolNew( szUprName ) );   /* Make new symbol */

   return pDynSym;
}

HB_EXPORT PHB_DYNS hb_dynsymFindName( const char * szName )  /* finds a symbol */
{
   char szUprName[ HB_SYMBOL_NAME_LEN + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFindName(%s)", szName));

   /* make a copy as we may get a const string, then turn it to uppercase */
   /* NOTE: This block is optimized for speed [vszakats] */
   {
      int iLen = HB_SYMBOL_NAME_LEN;
      char * pDest = szUprName;

      do
      {
         char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   return hb_dynsymFind( szUprName );
}

HB_EXPORT PHB_DYNS hb_dynsymFind( const char * szName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFind(%s)", szName));

   if( s_pDynItems == NULL )
   {
      s_pDynItems = ( PDYNHB_ITEM ) hb_xgrab( sizeof( DYNHB_ITEM ) );     /* Grab array */
      s_pDynItems->pDynSym = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
                /* Always grab a first symbol. Never an empty bucket. *<1>* */
      memset( s_pDynItems->pDynSym, 0, sizeof( HB_DYNS ) );

      return NULL;
   }
   else
   {
      /* Classic Tree Insert Sort Mechanism
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

      USHORT uiFirst = 0;
      USHORT uiLast = s_uiDynSymbols;
      USHORT uiMiddle = uiLast / 2;

      s_uiClosestDynSym = uiMiddle;                  /* Start in the middle      */

      while( uiFirst < uiLast )
      {
         int iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szName );

         if( iCmp == 0 )
         {
            s_uiClosestDynSym = uiMiddle;
            return s_pDynItems[ uiMiddle ].pDynSym;
         }
         else if( iCmp < 0 )
         {
            uiLast = uiMiddle;
            s_uiClosestDynSym = uiMiddle;
         }
         else /* if( iCmp > 0 ) */
         {
            uiFirst = uiMiddle + 1;
            s_uiClosestDynSym = uiFirst;
         }

         uiMiddle = uiFirst + ( ( uiLast - uiFirst ) / 2 );
      }
   }

   return NULL;
}

HB_EXPORT PHB_SYMB hb_dynsymGetSymbol( const char * szName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGetSymbol(%s)", szName));

   return hb_dynsymGet( szName )->pSymbol;
}

HB_EXPORT PHB_SYMB hb_dynsymFindSymbol( const char * szName )
{
   PHB_DYNS pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFindSymbol(%s)", szName));

   pDynSym = hb_dynsymFind( szName );
   return pDynSym ? pDynSym->pSymbol : NULL;
}

HB_EXPORT PHB_SYMB hb_dynsymSymbol( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymSymbol(%p)", pDynSym));

   return pDynSym->pSymbol;
}

HB_EXPORT const char * hb_dynsymName( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymName(%p)", pDynSym));

   return pDynSym->pSymbol->szName;
}

HB_EXPORT BOOL hb_dynsymIsFunction( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymIsFunction(%p)", pDynSym));

   return pDynSym->pSymbol->value.pFunPtr != NULL;
}

HB_EXPORT HB_HANDLE hb_dynsymMemvarHandle( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymMemvarHandle(%p)", pDynSym));

   return pDynSym->hMemvar;
}

HB_EXPORT int hb_dynsymAreaHandle( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymAreaHandle(%p)", pDynSym));

   return pDynSym->uiArea;
}

HB_EXPORT void hb_dynsymSetAreaHandle( PHB_DYNS pDynSym, int iArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymSetAreaHandle(%p,%d)", pDynSym, iArea));

   pDynSym->uiArea = ( USHORT ) iArea;
}

void hb_dynsymEval( PHB_DYNS_FUNC pFunction, void * Cargo )
{
   BOOL bCont = TRUE;
   USHORT uiPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymEval(%p, %p)", pFunction, Cargo));

   for( uiPos = 0; uiPos < s_uiDynSymbols && bCont; uiPos++ )
      bCont = ( pFunction )( s_pDynItems[ uiPos ].pDynSym, Cargo );
}

void hb_dynsymRelease( void )
{
   PHB_SYM_HOLDER pHolder;
   USHORT uiPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymRelease()"));

   for( uiPos = 0; uiPos < s_uiDynSymbols; uiPos++ )
   {
      hb_xfree( ( s_pDynItems + uiPos )->pDynSym );
   }
   hb_xfree( s_pDynItems );
   s_pDynItems = NULL;
   s_uiDynSymbols = 0;

   while( s_pAllocSyms )
   {
      pHolder = s_pAllocSyms;
      s_pAllocSyms = s_pAllocSyms->pNext;
      hb_xfree( pHolder );
   }
}

#ifdef HB_EXTENSION

HB_FUNC( __DYNSCOUNT ) /* How much symbols do we have: dsCount = __dynsymCount() */
{
   hb_retnl( ( long ) s_uiDynSymbols );
}

HB_FUNC( __DYNSGETNAME ) /* Get name of symbol: cSymbol = __dynsymGetName( dsIndex ) */
{
   long lIndex = hb_parnl( 1 ); /* NOTE: This will return zero if the parameter is not numeric */

   if( lIndex >= 1 && lIndex <= s_uiDynSymbols )
      hb_retc( s_pDynItems[ lIndex - 1 ].pDynSym->pSymbol->szName );
   else
      hb_retc( NULL );
}

HB_FUNC( __DYNSGETINDEX ) /* Gimme index number of symbol: dsIndex = __dynsymGetIndex( cSymbol ) */
{
   PHB_DYNS pDynSym;
   char * szName = hb_parc( 1 );

   if( szName )
      pDynSym = hb_dynsymFindName( szName );
   else
      pDynSym = NULL;

   if( pDynSym )
      hb_retnl( ( long ) ( s_uiClosestDynSym + 1 ) );
   else
      hb_retnl( 0L );
}

HB_FUNC( __DYNSISFUN ) /* returns .t. if a symbol has a function/procedure pointer,
                          given its symbol index */
{
   long lIndex = hb_parnl( 1 ); /* NOTE: This will return zero if the parameter is not numeric */

   if( lIndex >= 1 && lIndex <= s_uiDynSymbols )
      hb_retl( hb_dynsymIsFunction( s_pDynItems[ lIndex - 1 ].pDynSym ) );
   else
      hb_retl( FALSE );
}

HB_FUNC( __DYNSGETPRF ) /* profiler: It returns an array with a function or procedure
                                     called and consumed times { nTimes, nTime }
                                     , given the dynamic symbol index */
{
#ifndef HB_NO_PROFILER
   long lIndex = hb_parnl( 1 ); /* NOTE: This will return zero if the parameter is not numeric */
#endif

   hb_reta( 2 );
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );

#ifndef HB_NO_PROFILER
   if( lIndex >= 1 && lIndex <= s_uiDynSymbols )
   {
      if( hb_dynsymIsFunction( s_pDynItems[ lIndex - 1 ].pDynSym ) ) /* it is a function or procedure */
      {
         hb_stornl( s_pDynItems[ lIndex - 1 ].pDynSym->ulCalls, -1, 1 );
         hb_stornl( s_pDynItems[ lIndex - 1 ].pDynSym->ulTime,  -1, 2 );
      }
   }
#endif
}

#endif

HB_FUNC( __DYNSN2PTR )
{
   char * szName = hb_parc( 1 );

   hb_retptr( szName ? hb_dynsymGet( szName ) : NULL );
}

HB_FUNC( __DYNSN2SYM )
{
   char * szName = hb_parc( 1 );

   if( szName )
   {
      hb_itemPutSymbol( hb_stackReturnItem(), hb_dynsymGet( szName )->pSymbol );
   }
}

HB_FUNC( __DYNSP2NAME )
{
   PHB_DYNS pDynSym = ( PHB_DYNS ) hb_parptr( 1 );

   hb_retc( ( pDynSym != NULL ? pDynSym->pSymbol->szName : "" ) );
}
