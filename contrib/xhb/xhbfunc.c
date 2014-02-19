/*
 * Harbour Project source code:
 *    xHarbour compatible wrapper functions
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 *    hb_F_Eof()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbstack.h"
#include "hbvm.h"

#if 0

/* NOTE: Dangerous functions, commented out. Harbour doesn't implement hb_retclen_const() */

HB_FUNC( HB_POINTER2STRING )
{
   PHB_ITEM pPointer = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pLen     = hb_param( 2, HB_IT_NUMERIC );

   if( HB_IS_POINTER( pPointer ) && pLen )
      hb_retclen_const( ( char * ) hb_itemGetPtr( pPointer ), hb_itemGetNS( pLen ) );
   else if( HB_IS_INTEGER( pPointer ) && pLen )
      hb_retclen_const( ( char * ) hb_itemGetNI( pPointer ), hb_itemGetNS( pLen ) );
   else if( HB_IS_LONG( pPointer ) && pLen )
      hb_retclen_const( ( char * ) hb_itemGetNL( pPointer ), hb_itemGetNS( pLen ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STRING2POINTER )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
      hb_retptr( ( void * ) hb_itemGetCPtr( pString ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#endif

/* xHarbour really returns:
 *       hb_retc( hb_cmdargARGVN( 0 ) );
 * probably typo - replicated here by hb_ProgName()
 */
HB_FUNC_TRANSLATE( HB_CMDARGARGV, HB_PROGNAME )


HB_FUNC( HB_VMMODE )
{
#if   defined( HB_NO_PROFILER ) && defined( HB_NO_TRACE ) && ! defined( HB_GUI )
   hb_retni( 2 ); /* optimized for console applications */
#elif defined( HB_NO_PROFILER ) && defined( HB_NO_TRACE ) && defined( HB_GUI )
   hb_retni( 1 ); /* optimized for gui applications */
#else
   hb_retni( 0 ); /* no optimization */
#endif
}

HB_FUNC( XHB__KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   if( ! hb_parl( 2 ) )
      hb_inkeyReset();

   if( HB_ISNUM( 1 ) )
   {
      hb_inkeyPut( hb_parni( 1 ) );
   }
   else if( HB_ISCHAR( 1 ) )
   {
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
   }
   else if( HB_ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      HB_SIZE  nIndex;
      HB_SIZE  nElements = hb_arrayLen( pArray );

      for( nIndex = 1; nIndex <= nElements; nIndex++ )
      {
         PHB_ITEM pItem = hb_arrayGetItemPtr( pArray, nIndex );

         if( HB_IS_NUMBER( pItem ) )
         {
            hb_inkeyPut( hb_itemGetNI( pItem ) );
         }
         else if( HB_IS_STRING( pItem ) )
         {
            hb_inkeySetText( ( const char * ) hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ) );
         }
      }
   }
}

HB_FUNC( HB_CREATELEN8 )
{
   char      buffer[ 8 ];
   HB_MAXINT nValue;

   if( HB_ISNUM( 1 ) )
   {
      nValue = hb_parnint( 1 );
      HB_PUT_LE_UINT64( buffer, nValue );
      hb_retclen( buffer, 8 );
   }
   else if( HB_ISBYREF( 1 ) && HB_ISNUM( 2 ) )
   {
      nValue = hb_parnint( 2 );
      HB_PUT_LE_UINT64( buffer, nValue );
      hb_storclen( buffer, 8, 1 );
   }
}

HB_FUNC( HB_GETLEN8 )
{
   const char * buffer = hb_parc( 1 );

   if( buffer && hb_parclen( 1 ) >= 8 )
      hb_retnint( HB_GET_LE_UINT64( buffer ) );
   else
      hb_retni( -1 );
}

HB_FUNC( HB_DESERIALBEGIN )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   if( pItem )
      hb_itemReturn( pItem );
}

HB_FUNC_TRANSLATE( HB_DESERIALNEXT, HB_DESERIALIZE )
HB_FUNC_TRANSLATE( WILDMATCH, HB_WILDMATCH )
HB_FUNC_TRANSLATE( HB_CHECKSUM, HB_ADLER32 )

HB_FUNC( HB_F_EOF )
{
   HB_ERRCODE uiError = 6;

   if( HB_ISNUM( 1 ) )
   {
      hb_retl( hb_fsEof( hb_numToHandle( hb_parnint( 1 ) ) ) );
      uiError = hb_fsError();
   }
   else
      hb_retl( HB_TRUE );

   hb_fsSetFError( uiError );
}

HB_FUNC( CURDIRX )
{
   HB_ERRCODE uiErrorOld = hb_fsError();
   char *     pbyBuffer  = ( char * ) hb_xgrab( HB_PATH_MAX + 1 );
   PHB_ITEM   pDrv       = hb_param( 1, HB_IT_STRING );
   int        iCurDrv    = hb_fsCurDrv();
   int        iDrv;

   if( pDrv && hb_parclen( 1 ) > 0 )
   {
      iDrv = ( int ) ( HB_TOUPPER( *hb_itemGetCPtr( pDrv ) ) - 'A' );
      if( iDrv != iCurDrv )
         hb_fsChDrv( iDrv );
   }
   else
      iDrv = iCurDrv;

   /* NOTE: hb_fsCurDirBuffEx() in xhb, but I couldn't decipher the difference. [vszakats] */
   hb_fsCurDirBuff( iDrv, pbyBuffer, HB_PATH_MAX );

   hb_retc_buffer( pbyBuffer );

   hb_fsChDrv( iCurDrv );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC_TRANSLATE( CSTR, HB_CSTR )

HB_FUNC( HB_ARRAYID )  /* for debugging: returns the array's "address" so dual references to same array can be seen */
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   hb_retptr( pArray ? hb_arrayId( pArray ) : NULL );
}

HB_FUNC( HB_HASHID )  /* for debugging: returns the array's "address" so dual references to same array can be seen */
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   hb_retptr( pHash ? hb_hashId( pHash ) : NULL );
}

HB_FUNC( __SENDRAWMSG )
{
   hb_dbg_objSendMessage( 0, hb_param( 1, HB_IT_ANY ),
                          hb_param( 2, HB_IT_ANY ), 3 );
}

HB_FUNC( HB_EXEC )
{
   if( HB_ISSYMBOL( 1 ) )
   {
      HB_BOOL fSend   = HB_FALSE;
      int     iParams = hb_pcount() - 1;

      if( iParams >= 1 )
      {
         fSend = iParams > 1 && ! HB_IS_NIL( hb_param( 2, HB_IT_ANY ) );
         iParams--;
      }
      else
         hb_vmPushNil();
      if( fSend )
         hb_vmSend( ( HB_USHORT ) iParams );
      else
         hb_vmDo( ( HB_USHORT ) iParams );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC_EXTERN( HB_USERNAME );
HB_FUNC_EXTERN( NETNAME );

HB_FUNC( XHB_NETNAME )
{
   if( hb_parni( 1 ) == 1 )
      HB_FUNC_EXEC( HB_USERNAME );
   else
      HB_FUNC_EXEC( NETNAME );
}

HB_FUNC_EXTERN( HB_MEMOWRIT );
HB_FUNC_EXTERN( MEMOWRIT );

HB_FUNC( XHB_MEMOWRIT )
{
   if( HB_ISLOG( 3 ) && ! hb_parl( 3 ) )
      HB_FUNC_EXEC( HB_MEMOWRIT );
   else
      HB_FUNC_EXEC( MEMOWRIT );
}

#if 0

/* length of buffer for CR/LF characters */
#if ! defined( HB_OS_EOL_LEN ) || HB_OS_EOL_LEN < 4
#  define CRLF_BUFFER_LEN  4
#else
#  define CRLF_BUFFER_LEN  HB_OS_EOL_LEN + 1
#endif

#if defined( HB_OS_UNIX ) && ! defined( HB_EOL_CRLF )
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 1;
#else
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_CR, HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 2;
#endif

HB_FUNC( HB_OSNEWLINE )
{
   hb_retc_const( s_szCrLf );
}

HB_FUNC( HB_OSPATHSEPARATOR )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_STRING );
}

#endif

#if 0

HB_FUNC( HB_ISBYREF )
{
   if( hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( 1 );
      if( HB_IS_BYREF( pItem ) )
         hb_retl( HB_IS_BYREF( hb_itemUnRefOnce( pItem ) ) );
   }
}

#endif

#if ! defined( HB_LEGACY_LEVEL5 )

HB_FUNC( HB_ISNIL )
{
   hb_retl( HB_ISNIL( 1 ) );
}

#endif

HB_FUNC_TRANSLATE( METHODNAME     , HB_METHODNAME )
HB_FUNC_TRANSLATE( LIBLOAD        , HB_LIBLOAD    )
HB_FUNC_TRANSLATE( LIBFREE        , HB_LIBFREE    )
HB_FUNC_TRANSLATE( HB_LIBDO       , DO            )
HB_FUNC_TRANSLATE( HB_BITISSET    , HB_BITTEST    )
HB_FUNC_TRANSLATE( SECONDSSLEEP   , HB_IDLESLEEP  )
HB_FUNC_TRANSLATE( HB_FUNCPTR     , __DYNSN2SYM   )
HB_FUNC_TRANSLATE( VALTOPRGEXP    , HB_VALTOEXP   )
HB_FUNC_TRANSLATE( HEXTONUM       , HB_HEXTONUM   )
HB_FUNC_TRANSLATE( NUMTOHEX       , HB_NUMTOHEX   )
HB_FUNC_TRANSLATE( HEXTOSTR       , HB_HEXTOSTR   )
HB_FUNC_TRANSLATE( STRTOHEX       , HB_STRTOHEX   )
HB_FUNC_TRANSLATE( ISPOINTER      , HB_ISPOINTER  )
HB_FUNC_TRANSLATE( HB_SETCODEPAGE , HB_CDPSELECT  )
HB_FUNC_TRANSLATE( DEFAULT        , __DEFAULTNIL  )
