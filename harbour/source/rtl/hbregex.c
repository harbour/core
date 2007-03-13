/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * 
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* #define HB_PCRE_REGEX */

#define _HB_REGEX_INTERNAL_
#include "hbregex.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* This releases regex when called from the garbage collector */
static HB_GARBAGE_FUNC( hb_regexRelease )
{
#ifdef _HB_REGEX_INTERNAL_
   PHB_REGEX pRegEx = ( PHB_REGEX ) Cargo;
   regfree( &pRegEx->reg );
#else
   HB_SYMBOL_UNUSED( Cargo );
#endif
}

PHB_REGEX hb_regexCompile( const char *szRegEx, ULONG ulLen, int iFlags )
{
#ifdef _HB_REGEX_INTERNAL_
   PHB_REGEX pRegEx;

   HB_SYMBOL_UNUSED( ulLen );

   pRegEx = ( PHB_REGEX ) hb_gcAlloc( sizeof( HB_REGEX ), hb_regexRelease );
   hb_gcLock( pRegEx );
   memset( pRegEx, 0, sizeof( HB_REGEX ) );
   pRegEx->fFree = TRUE;
   pRegEx->iCFlags = REG_EXTENDED |
                     ( ( iFlags & HBREG_ICASE   ) ? REG_ICASE   : 0 ) |
                     ( ( iFlags & HBREG_NEWLINE ) ? REG_NEWLINE : 0 ) |
                     ( ( iFlags & HBREG_NOSUB   ) ? REG_NOSUB   : 0 );
   pRegEx->iEFlags = ( ( iFlags & HBREG_NOTBOL  ) ? REG_NOTBOL  : 0 ) |
                     ( ( iFlags & HBREG_NOTEOL  ) ? REG_NOTBOL  : 0 );

   if( regcomp( &pRegEx->reg, szRegEx, pRegEx->iCFlags ) != 0 )
   {
      hb_gcFree( pRegEx );
      pRegEx = NULL;
   }

   return pRegEx;

#else
   HB_SYMBOL_UNUSED( szRegEx );
   HB_SYMBOL_UNUSED( ulLen );
   HB_SYMBOL_UNUSED( iFlags );

   return NULL;
#endif
}

PHB_REGEX hb_regexGet( PHB_ITEM pRegExItm, int iFlags )
{
   PHB_REGEX pRegEx = NULL;

   if( pRegExItm )
   {
      if( HB_IS_POINTER( pRegExItm ) )
      {
         pRegEx = ( PHB_REGEX ) hb_itemGetPtrGC( pRegExItm, hb_regexRelease );
      }
      else if( HB_IS_STRING( pRegExItm ) )
      {
         ULONG ulLen = hb_itemGetCLen( pRegExItm );
         char * szRegEx = hb_itemGetCPtr( pRegExItm );
         if( ulLen > 0 )
            pRegEx = hb_regexCompile( szRegEx, ulLen, iFlags );
      }
   }

   if( !pRegEx )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid Regular expression", &hb_errFuncName, 1, pRegExItm );

   return pRegEx;
}

void      hb_regexFree( PHB_REGEX pRegEx )
{
#ifdef _HB_REGEX_INTERNAL_
   if( pRegEx && pRegEx->fFree )
   {
      regfree( &pRegEx->reg );
      hb_gcFree( pRegEx );
   }
#else
   HB_SYMBOL_UNUSED( pRegEx );
#endif
}

BOOL      hb_regexMatch( PHB_REGEX pRegEx, const char *szString, BOOL fFull )
{
#ifdef _HB_REGEX_INTERNAL_
   BOOL fMatch;

   fMatch = regexec( &pRegEx->reg, szString, 1, pRegEx->aMatches, pRegEx->iEFlags ) == 0;

   return fMatch && ( !fFull ||
            ( pRegEx->aMatches[0].rm_so == 0 &&
              pRegEx->aMatches[0].rm_eo == (int) strlen( szString ) ) );
#else
   HB_SYMBOL_UNUSED( pRegEx );
   HB_SYMBOL_UNUSED( szString );
   HB_SYMBOL_UNUSED( fFull );
   return FALSE;
#endif
}


HB_FUNC( HB_REGEXCOMP )
{
#ifdef _HB_REGEX_INTERNAL_
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen == 0 )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter count/type", 
                            &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
   {
      int iFlags = HBREG_EXTENDED;
      PHB_REGEX pRegEx;

      if( ISLOG( 2 ) && !hb_parl( 2 ) )
         iFlags |= HBREG_ICASE;
      if( hb_parl( 3 ) )
         iFlags |= HBREG_NEWLINE;

      pRegEx = hb_regexCompile( hb_parc( 1 ), ulLen, iFlags );
      if( pRegEx )
      {
         pRegEx->fFree = FALSE;
         hb_retptrGC( pRegEx );
         hb_gcUnlock( pRegEx );
      }
   }
#endif
}

HB_FUNC( HB_ISREGEX )
{
   hb_retl( hb_parptrGC( hb_regexRelease, 1 ) != NULL );
}

HB_FUNC( HB_ATX )
{
#ifdef _HB_REGEX_INTERNAL_
   char * pszString, * pszCopy = NULL;
   ULONG ulLen, ulStart, ulEnd;
   regmatch_t aMatches[ 1 ];
   PHB_REGEX pRegEx;
   PHB_ITEM pString;
   int iPCount = hb_pcount();

   pString = hb_param( 2, HB_IT_STRING );
   if( !pString )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameters",
                            &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   pszString = hb_itemGetCPtr( pString );
   ulLen     = hb_itemGetCLen( pString );
   pRegEx = hb_regexGet( hb_param( 1, HB_IT_ANY ),
                         ISLOG( 3 ) && !hb_parl( 3 ) ? HBREG_ICASE : 0 );
   if( !pRegEx )
      return;

   ulStart = hb_parnl( 4 );
   ulEnd = hb_parnl( 5 );

   if( ulLen && ulStart <= ulLen && ulStart <= ulEnd )
   {
      if( ulEnd > 0 && ulEnd < ulLen && pszString[ ulEnd ] != 0 )
      {
         if( ulStart > 1 )
         {
            pszString += ulStart - 1;
            ulEnd -= ulStart - 1;
         }
         pszCopy = ( char * ) hb_xgrab( ulEnd + 1 );
         memcpy( pszCopy, pszString, ulEnd );
         pszCopy[ ulEnd ] = '\0';
         pszString = pszCopy;
      }

      if( regexec( &pRegEx->reg, pszString, 1, aMatches, 0 ) == 0 )
      {
         ulStart = aMatches[0].rm_so + 1;
         ulLen = aMatches[0].rm_eo - aMatches[0].rm_so;
         hb_retclen( pszString + aMatches[0].rm_so, ulLen );
      }
      else
         ulStart = ulLen = 0;
   }
   else
      ulStart = ulLen = 0;

   hb_regexFree( pRegEx );
   if( pszCopy )
      hb_xfree( pszCopy );

   if( iPCount > 3 )
   {
      hb_stornl( ulStart, 4 );
      if( iPCount > 4 )
         hb_stornl( ulLen, 5 );
   }
#endif
}

static BOOL hb_regex( int iRequest )
{
#ifdef _HB_REGEX_INTERNAL_
   regmatch_t aMatches[ REGEX_MAX_GROUPS ];
   PHB_ITEM pRetArray, pMatch, pString;
   int i, iMatches, iMaxMatch;
   BOOL fResult = FALSE;
   PHB_REGEX pRegEx;
   char * pszString;
   ULONG ulLen;

   pString = hb_param( 2, HB_IT_STRING );
   if( !pString )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameters",
                            &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return FALSE;
   }
   pRegEx = hb_regexGet( hb_param( 1, HB_IT_ANY ),
                         ( ISLOG( 3 ) && !hb_parl( 3 ) ? HBREG_ICASE : 0 ) |
                         ( hb_parl( 4 ) ? HBREG_NEWLINE : 0 ) );
   if( !pRegEx )
      return FALSE;

   pszString = hb_itemGetCPtr( pString );
   ulLen     = hb_itemGetCLen( pString );
   iMatches  = 0;
   iMaxMatch = iRequest == 0 || iRequest == 4 || iRequest == 5 ?
               REGEX_MAX_GROUPS : 1;
   aMatches[0].rm_so = 0;
   aMatches[0].rm_eo = ulLen;
   if( regexec( &pRegEx->reg, pszString, iMaxMatch, aMatches, 0 ) == 0 )
   {
      switch ( iRequest )
      {
         case 0:
            /* Count sucessful matches */
            for( i = 0; i < iMaxMatch; i++ )
            {
               if( aMatches[i].rm_eo != -1 )
                  iMatches = i;
            }
            iMatches++;
            pRetArray = hb_itemArrayNew( iMatches );
            for( i = 0; i < iMatches; i++ )
            {
               if( aMatches[i].rm_eo > -1 )
                  hb_itemPutCL( hb_arrayGetItemPtr( pRetArray, i + 1 ),
                                pszString + aMatches[i].rm_so,
                                aMatches[i].rm_eo - aMatches[i].rm_so );
               else
                  hb_itemPutCL( hb_arrayGetItemPtr( pRetArray, i + 1 ), "", 0 );
            }
            hb_itemRelease( hb_itemReturnForward( pRetArray ) );
            fResult = TRUE;
            break;

         case 1: /* LIKE */
            fResult = aMatches[0].rm_so == 0 &&
                      ( ULONG ) aMatches[0].rm_eo == ulLen;
            break;

         case 2: /* MATCH */
            fResult = TRUE;
            break;

         case 3: /* SPLIT */
            iMaxMatch = hb_parni( 5 );
            pRetArray = hb_itemArrayNew( 0 );
            pMatch = hb_itemNew( NULL );
            do
            {
               hb_itemPutCL( pMatch, pszString, aMatches[0].rm_so );
               hb_arrayAddForward( pRetArray, pMatch );
               ulLen -= aMatches[0].rm_eo;
               pszString += aMatches[ 0 ].rm_eo;
               iMatches++;
            }
            while( aMatches[0].rm_eo && ulLen && ( iMaxMatch == 0 || iMatches < iMaxMatch ) &&
                   regexec( &pRegEx->reg, pszString, 1, aMatches, 0 ) == 0 );

            /* last match must be done also in case that pszString is empty;
               this would mean an empty split field at the end of the string */
            /* if( ulLen ) */
            {
               hb_itemPutCL( pMatch, pszString, ulLen );
               hb_arrayAddForward( pRetArray, pMatch );
            }
            hb_itemRelease( pMatch );

            hb_itemRelease( hb_itemReturnForward( pRetArray ) );
            fResult = TRUE;
            break;

         case 4: /* results AND positions */
            /* Count sucessful matches */
            for( i = 0; i < iMaxMatch; i++ )
            {
               if( aMatches[i].rm_eo != -1 )
                  iMatches = i;
            }
            iMatches++;
            pRetArray = hb_itemArrayNew( iMatches );

            for( i = 0; i < iMatches; i++ )
            {
               pMatch = hb_arrayGetItemPtr( pRetArray, i + 1 );
               hb_arrayNew( pMatch, 3 );
               if ( aMatches[i].rm_eo != -1 )
               {
                  /* matched string */
                  hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), pszString + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );
                  /* begin of match */
                  hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), aMatches[i].rm_so + 1 );
                  /* End of match */
                  hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), aMatches[i].rm_eo );
               }
               else
               {
                  hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), "", 0 );
                  hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), 0 );
                  hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), 0 );
               }
            }
            hb_itemRelease( hb_itemReturnForward( pRetArray ) );
            fResult = TRUE;
            break;

         case 5: /* _ALL_ results AND positions */
         {
            PHB_ITEM pAtxArray;
            int   iMax       = hb_parni( 5 );   /* max nuber of matches I want, 0 = unlimited */
            int   iGetMatch  = hb_parni( 6 );   /* Gets if want only one single match or a sub-match */
            BOOL  fOnlyMatch = !ISLOG( 7 ) || hb_parl( 7 ); /* if TRUE returns only matches and sub-matches, not positions */
            ULONG ulOffSet   = 0;
            int   iCount     = 0;

            /* Set new array */
            pRetArray = hb_itemArrayNew( 0 );
            do
            {
               /* Count sucessful matches */
               for( i = 0; i < iMaxMatch; i++ )
               {
                  if( aMatches[i].rm_eo != -1 )
                     iMatches = i;
               }
               iMatches++;

               /* If I want all matches */
               if( iGetMatch == 0 || // Check boundaries
                   ( iGetMatch < 0 || iGetMatch > iMatches ) )
               {
                  pAtxArray = hb_itemArrayNew( iMatches );
                  for( i = 0; i < iMatches; i++ )
                  {
                     pMatch = hb_arrayGetItemPtr( pAtxArray, i + 1 );
                     if( !fOnlyMatch )
                     {
                        hb_arrayNew( pMatch, 3 );
                        if ( aMatches[i].rm_eo != -1 )
                        {
                           /* matched string */
                           hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), pszString + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );
                           /* begin of match */
                           hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), ulOffSet + aMatches[i].rm_so + 1 );
                           /* End of match */
                           hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), ulOffSet + aMatches[i].rm_eo );
                        }
                        else
                        {
                           hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), "", 0 );
                           hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), 0 );
                           hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), 0 );
                        }
                     }
                     else
                     {
                        if( aMatches[i].rm_eo != -1 )
                           /* matched string */
                           hb_itemPutCL( pMatch, pszString + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );
                        else
                           hb_itemPutCL( pMatch, "", 0 );
                     }
                  }
                  hb_arrayAddForward( pRetArray, pAtxArray );
                  hb_itemRelease( pAtxArray );
               }
               else /* Here I get only single matches */
               {
                  i = iGetMatch - 1;
                  pMatch = hb_itemNew( NULL );
                  if( !fOnlyMatch )
                  {
                     hb_arrayNew( pMatch, 3 );
                     if( aMatches[i].rm_eo != -1 )
                     {
                        /* matched string */
                        hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), pszString + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );
                        /* begin of match */
                        hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), ulOffSet + aMatches[i].rm_so + 1 );
                        /* End of match */
                        hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), ulOffSet + aMatches[i].rm_eo );
                     }
                     else
                     {
                        hb_itemPutCL( hb_arrayGetItemPtr( pMatch, 1 ), "", 0 );
                        hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 2 ), 0 );
                        hb_itemPutNI( hb_arrayGetItemPtr( pMatch, 3 ), 0 );
                     }
                  }
                  else
                  {
                     if( aMatches[i].rm_eo != -1 )
                        /* matched string */
                        hb_itemPutCL( pMatch, pszString + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );
                     else
                        hb_itemPutCL( pMatch, "", 0 );
                  }
                  hb_arrayAddForward( pRetArray, pMatch );
                  hb_itemRelease( pMatch );
               }

               ulLen -= aMatches[0].rm_eo;
               pszString += aMatches[ 0 ].rm_eo;
               ulOffSet += aMatches[0].rm_eo;
               iCount++;
            }
            while( aMatches[0].rm_eo && ulLen && ( iMax == 0 || iCount < iMax ) &&
                   regexec( &pRegEx->reg, pszString, iMaxMatch, aMatches, 0 ) == 0 );
            hb_itemRelease( hb_itemReturnForward( pRetArray ) );
            fResult = TRUE;
            break;
         }
      }
   }
   else if( iRequest == 3 )
   {
      pRetArray = hb_itemArrayNew( 1 );
      hb_arraySet( pRetArray, 1, pString );
      hb_itemRelease( hb_itemReturnForward( pRetArray ) );
      fResult = TRUE;
   }

   hb_regexFree( pRegEx );
   return fResult;
#else
   HB_SYMBOL_UNUSED( iRequest );
   return FALSE;
#endif
}

/* Returns array of Match + Sub-Matches. */
HB_FUNC( HB_REGEX )
{
   hb_regex( 0 );
}

/* Returns just .T. if match found or .F. otherwise. */
HB_FUNC( HB_REGEXMATCH )
{
   hb_retl( hb_regex( hb_parl( 3 ) ? 1 : 2 ) );
}

/* Splits the string in an array of matched expressions */
HB_FUNC( HB_REGEXSPLIT )
{
   hb_regex( 3 );
}

/* Returns array of { Match, start, end }, { Sub-Matches, start, end } */
HB_FUNC( HB_REGEXATX )
{
   hb_regex( 4 );
}

/* 2005-12-16 - Francesco Saverio Giudice
  HB_RegExAll( cRegex, cString, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch, lOnlyMatch ) -> aAllRegexMatches

  This function return all matches from a Regex search.
  It is a mix from hb_RegEx() and hb_RegExAtX()

  PARAMETERS:
    cRegex         - Regex pattern string or precompiled Regex
    cString        - The string you want to search
    lCaseSensitive - default = FALSE
    lNewLine       - default = FALSE
    nMaxMatches    - default = unlimited, this limit number of matches that have to return
    nGetMatch      - default = unlimited, this returns only one from Match + Sub-Matches
    lOnlyMatch     - default = TRUE, if TRUE returns Matches, otherwise it returns also start and end positions
 */

HB_FUNC( HB_REGEXALL )
{
   hb_regex( 5 );
}
