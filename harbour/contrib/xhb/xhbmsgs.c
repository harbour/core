/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xHarbour compatible messages used in overloaded scalar classes
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbstack.h"
#include "hbmath.h"

HB_FUNC( XHB_HASHERROR )
{
   const char * szMessage = hb_itemGetSymbol( hb_stackBaseItem() )->szName;
   int          iPCount   = hb_pcount();

   if( iPCount == 1 )
   {
      if( szMessage[ 0 ] == '_' ) /* ASSIGN */
      {
         PHB_ITEM pIndex = hb_itemPutCConst( hb_stackAllocItem(), szMessage + 1 );
         PHB_ITEM pDest  = hb_hashGetItemPtr( hb_stackSelfItem(), pIndex, HB_HASH_AUTOADD_ASSIGN );
         hb_stackPop();
         if( pDest )
         {
            PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );
            hb_itemCopyFromRef( pDest, pValue );
            hb_itemReturn( pValue );
            return;
         }
      }
   }
   else if( iPCount == 0 ) /* ACCESS */
   {
      PHB_ITEM pIndex = hb_itemPutCConst( hb_stackAllocItem(), szMessage );
      PHB_ITEM pValue = hb_hashGetItemPtr( hb_stackSelfItem(), pIndex, HB_HASH_AUTOADD_ACCESS );
      hb_stackPop();
      if( pValue )
      {
         hb_itemReturn( pValue );
         return;
      }
   }

   if( szMessage[ 0 ] == '_' )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, szMessage + 1, HB_ERR_ARGS_SELFPARAMS );
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, szMessage, HB_ERR_ARGS_SELFPARAMS );
}

HB_FUNC( XHB_INCLUDE )
{
   PHB_ITEM pSelf = hb_stackSelfItem();
   PHB_ITEM pKey  = hb_param( 1, HB_IT_ANY );

   if( HB_IS_ARRAY( pSelf ) )
   {
      hb_retl( hb_arrayScan( pSelf, pKey, NULL, NULL, HB_TRUE ) != 0 );
   }
   else if( HB_IS_HASH( pSelf ) && ( HB_IS_HASHKEY( pKey ) || hb_hashLen( pKey ) == 1 ) )
   {
      hb_retl( hb_hashScan( pSelf, pKey, NULL ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pKey, pSelf );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_EEQUAL )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue == ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc == dValue );
   }
   else if( HB_IS_BLOCK( pSelf ) && HB_IS_BLOCK( pValue ) )
   {
      hb_retl( hb_codeblockId( pSelf ) == hb_codeblockId( pValue ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_EQUAL )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue == ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc == dValue );
   }
   else if( HB_IS_HASH( pSelf ) && HB_IS_HASH( pValue ) )
   {
      hb_retl( hb_hashId( pSelf ) == hb_hashId( pValue ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_NOTEQUAL )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue != ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc != dValue );
   }
   else if( HB_IS_HASH( pSelf ) && HB_IS_HASH( pValue ) )
   {
      hb_retl( hb_hashId( pSelf ) != hb_hashId( pValue ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_LESS )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue < ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc < dValue );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_LESSEQ )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue <= ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc <= dValue );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_GREATER )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue > ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc > dValue );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_GREATEREQ )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      double   dValue = hb_itemGetND( pSelf );
      hb_retl( dValue >= ( double ) uc );
   }
   else if( hb_itemGetCLen( pSelf ) == 1 && pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc     = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dValue = hb_itemGetND( pValue );
      hb_retl( ( double ) uc >= dValue );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

/*
 * check if array/string index is in valid range, update it if necessary
 * in xHarbour compatibility mode where negative indexes are used to access
 * data from tail
 */
#undef HB_IS_VALID_INDEX
#define HB_IS_VALID_INDEX( idx, max )  ( ( ( HB_ISIZ ) ( idx ) < 0 ? ( idx ) += ( max ) + 1 : ( idx ) ) > 0 && ( HB_SIZE ) ( idx ) <= ( max ) )

HB_FUNC( XHB_INDEX )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pIndex = hb_param( 1, HB_IT_ANY );

   if( hb_pcount() == 2 ) /* ASSIGN */
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );
      if( HB_IS_NUMERIC( pIndex ) )
      {
         HB_SIZE nIndex = hb_itemGetNS( pIndex );
         if( HB_IS_ARRAY( pSelf ) )
         {
            HB_SIZE nLen = hb_arrayLen( pSelf );
            if( HB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_itemMoveRef( hb_arrayGetItemPtr( pSelf, nIndex ), pValue );
            else
               hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
         }
         else if( HB_IS_STRING( pSelf ) )
         {
            HB_SIZE nLen = hb_itemGetCLen( pSelf );
            if( HB_IS_VALID_INDEX( nIndex, nLen ) )
            {
               char cValue = HB_IS_STRING( pValue ) ? hb_itemGetCPtr( pValue )[ 0 ] :
                             ( char ) hb_itemGetNI( pValue );
               if( nLen == 1 )
                  hb_itemPutCL( pSelf, &cValue, 1 );
               else
               {
                  char * pszText;
                  if( hb_itemGetWriteCL( pSelf, &pszText, &nLen ) &&
                      nIndex > 0 && nIndex <= nLen )
                     pszText[ nIndex - 1 ] = cValue;
               }
            }
            else
               hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
         }
         else
            hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
      }
      else
         hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );

      hb_itemReturn( pSelf );
   }
   else /* ACCESS */
   {
      if( HB_IS_NUMERIC( pIndex ) )
      {
         HB_SIZE nIndex = hb_itemGetNS( pIndex );
         if( HB_IS_ARRAY( pSelf ) )
         {
            HB_SIZE nLen = hb_arrayLen( pSelf );
            if( HB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_itemReturn( hb_arrayGetItemPtr( pSelf, nIndex ) );
            else
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );
         }
         else if( HB_IS_STRING( pSelf ) )
         {
            HB_SIZE nLen = hb_itemGetCLen( pSelf );
            if( HB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_retclen( hb_itemGetCPtr( pSelf ) + nIndex - 1, 1 );
            else
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );
         }
         else
            hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
   }
}

HB_FUNC( XHB_PLUS )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      int      iDec;
      double   dValue = hb_itemGetNDDec( pSelf, &iDec );
      hb_retnlen( dValue + uc, 0, iDec  );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      uc += ( HB_UCHAR ) hb_itemGetNI( pValue );
      hb_retclen( ( char * ) &uc, 1 );
   }
   else if( HB_IS_HASH( pSelf ) && HB_IS_HASH( pValue ) )
   {
      PHB_ITEM pHash = hb_hashClone( pSelf );
      hb_hashJoin( pHash, pValue, HB_HASH_UNION );
      hb_itemReturnRelease( pHash );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_MINUS )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      int      iDec;
      double   dValue = hb_itemGetNDDec( pSelf, &iDec );
      hb_retnlen( dValue - uc, 0, iDec  );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      uc -= ( HB_UCHAR ) hb_itemGetNI( pValue );
      hb_retclen( ( char * ) &uc, 1 );
   }
   else if( HB_IS_HASH( pSelf ) && HB_IS_HASH( pValue ) )
   {
      PHB_ITEM pHash = hb_hashClone( pSelf );
      hb_hashRemove( pHash, pValue );
      hb_itemReturnRelease( pHash );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_INC )
{
   PHB_ITEM pSelf = hb_stackSelfItem();

   if( HB_IS_NUMERIC( pSelf ) )
      hb_retnd( hb_itemGetND( pSelf ) + 1 );
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ] + 1;
      hb_retclen( ( char * ) &uc, 1 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++", 1, pSelf );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_DEC )
{
   PHB_ITEM pSelf = hb_stackSelfItem();

   if( HB_IS_NUMERIC( pSelf ) )
      hb_retnd( hb_itemGetND( pSelf ) - 1 );
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ] - 1;
      hb_retclen( ( char * ) &uc, 1 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--", 1, pSelf );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_MULT )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      int      iDec;
      double   dValue = hb_itemGetNDDec( pSelf, &iDec );
      hb_retndlen( dValue * uc, 0, iDec );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      int      iDec;
      double   dValue = hb_itemGetNDDec( pValue, &iDec );
      hb_retndlen( ( double ) uc * dValue, 0, iDec );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc1 = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ],
               uc2 = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      hb_retnint( uc1 * uc2 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_DIV )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      if( uc == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pSelf, pValue );
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
      else
         hb_retnd( hb_itemGetND( pSelf ) / uc );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 && pValue &&
            ( HB_IS_NUMERIC( pValue ) || hb_itemGetCLen( pValue ) == 1 ) )
   {
      HB_UCHAR uc       = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dDivisor = HB_IS_NUMERIC( pValue ) ? hb_itemGetND( pValue ) :
                          ( double ) ( ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ] );

      if( dDivisor == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pSelf, pValue );
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
      else
         hb_retnd( ( double ) uc / dDivisor );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_MOD )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      if( uc == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pSelf, pValue );
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
      else
         hb_retnd( fmod( hb_itemGetND( pSelf ), ( double ) uc ) );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 && pValue &&
            ( HB_IS_NUMERIC( pValue ) || hb_itemGetCLen( pValue ) == 1 ) )
   {
      HB_UCHAR uc       = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      double   dDivisor = HB_IS_NUMERIC( pValue ) ? hb_itemGetND( pValue ) :
                          ( double ) ( ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ] );

      if( dDivisor == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pSelf, pValue );
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
      else
         hb_retnd( fmod( ( double ) uc, dDivisor ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}

HB_FUNC( XHB_POW )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

   if( HB_IS_NUMERIC( pSelf ) && hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      hb_retnd( pow( hb_itemGetND( pSelf ), ( double ) uc ) );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            pValue && HB_IS_NUMERIC( pValue ) )
   {
      HB_UCHAR uc = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ];
      hb_retnd( pow( ( double ) uc, hb_itemGetND( pValue ) ) );
   }
   else if( HB_IS_STRING( pSelf ) && hb_itemGetCLen( pSelf ) == 1 &&
            hb_itemGetCLen( pValue ) == 1 )
   {
      HB_UCHAR uc1 = ( HB_UCHAR ) hb_itemGetCPtr( pSelf )[ 0 ],
               uc2 = ( HB_UCHAR ) hb_itemGetCPtr( pValue )[ 0 ];
      hb_retnd( pow( ( double ) uc1, ( double ) uc2 ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^", 2, pSelf, pValue );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}
