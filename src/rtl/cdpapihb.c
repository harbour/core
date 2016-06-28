/*
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009-2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapicdp.h"

HB_FUNC( HB_CDPSELECT )
{
   const char * id = hb_parc( 1 );

   hb_retc( hb_cdpID() );

   if( id )
      hb_cdpSelectID( id );
}

HB_FUNC( HB_CDPEXISTS )
{
   const char * id = hb_parc( 1 );

   if( id )
      hb_retl( hb_cdpFind( id ) != NULL );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CDPUNIID )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? hb_cdpFindExt( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->uniTable->uniID : NULL );
}

HB_FUNC( HB_CDPINFO )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? hb_cdpFindExt( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->info : NULL );
}

HB_FUNC( HB_CDPISCHARIDX )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? hb_cdpFindExt( id ) : hb_vmCDP();
   HB_BOOL fResult = HB_FALSE;

   if( cdp )
   {
      fResult = HB_CDP_ISCHARIDX( cdp );
      if( HB_CDP_ISCUSTOM( cdp ) && HB_ISLOG( 2 ) )
      {
         if( hb_parl( 2 ) )
            cdp->type |= HB_CDP_TYPE_CHARIDX;
         else
            cdp->type &= ~HB_CDP_TYPE_CHARIDX;
      }
   }
   hb_retl( fResult );
}

HB_FUNC( HB_CDPCHARMAX )
{
   hb_retnl( ( 1 << ( ( int ) ( hb_cdpIsUTF8( hb_cdpFindExt( hb_parc( 1 ) ) ) ? sizeof( HB_WCHAR ) : sizeof( HB_UCHAR ) ) * 8 ) ) - 1 );
}

HB_FUNC( HB_CDPISUTF8 )
{
   hb_retl( hb_cdpIsUTF8( hb_cdpFindExt( hb_parc( 1 ) ) ) );
}

HB_FUNC( HB_CDPLIST )
{
   const char ** list = hb_cdpList();
   HB_ISIZ nPos;

   nPos = 0;
   while( list[ nPos ] )
      ++nPos;

   hb_reta( nPos );

   nPos = 0;
   while( list[ nPos ] )
   {
      hb_storvc( list[ nPos ], -1, nPos + 1 );
      ++nPos;
   }

   hb_xfree( ( void * ) list );
}

/* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
/* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */
HB_FUNC_TRANSLATE( __NATSORTVER, HB_CDPINFO )

/*
 * extended CP PRG functions
 */
HB_FUNC( HB_TRANSLATE )
{
   HB_SIZE nLen = hb_parclen( 1 );
   const char * szIdIn = hb_parc( 2 );
   const char * szIdOut = hb_parc( 3 );

   if( nLen && ( szIdIn || szIdOut ) )
   {
      PHB_CODEPAGE cdpIn = szIdIn ? hb_cdpFindExt( szIdIn ) : hb_vmCDP();
      PHB_CODEPAGE cdpOut = szIdOut ? hb_cdpFindExt( szIdOut ) : hb_vmCDP();

      if( cdpIn && cdpOut && cdpIn != cdpOut &&
          ( cdpIn->uniTable != cdpOut->uniTable ||
            HB_CDP_ISCUSTOM( cdpIn ) ||
            HB_CDP_ISCUSTOM( cdpOut ) ) )
      {
         char * szResult = hb_cdpnDup( hb_parc( 1 ), &nLen, cdpIn, cdpOut );
         hb_retclen_buffer( szResult, nLen );
      }
      else
         hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
   }
   else
      hb_retc_null();
}

HB_FUNC( HB_STRTOUTF8 )
{
   HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
   char * szDest = NULL;

   if( nLen )
   {
      const char * szCP = hb_parc( 2 );
      PHB_CODEPAGE cdp = szCP ? hb_cdpFindExt( szCP ) : hb_vmCDP();

      if( cdp )
      {
         if( hb_cdpIsUTF8( cdp ) )
         {
            hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
            return;
         }
         else
         {
            const char * szString = hb_parc( 1 );
            nDest = hb_cdpStrAsUTF8Len( cdp, szString, nLen, 0 );
            szDest = ( char * ) hb_xgrab( nDest + 1 );
            hb_cdpStrToUTF8( cdp, szString, nLen, szDest, nDest + 1 );
         }
      }
   }
   if( szDest )
      hb_retclen_buffer( szDest, nDest );
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8TOSTR )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen )
      {
         const char * szCP = hb_parc( 2 );
         PHB_CODEPAGE cdp = szCP ? hb_cdpFindExt( szCP ) : hb_vmCDP();

         if( cdp )
         {
            if( hb_cdpIsUTF8( cdp ) )
            {
               hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
               return;
            }
            else
            {
               szString = hb_parc( 1 );
               nDest = hb_cdpUTF8AsStrLen( cdp, szString, nLen, 0 );
               szDest = ( char * ) hb_xgrab( nDest + 1 );
               hb_cdpUTF8ToStr( cdp, szString, nLen, szDest, nDest + 1 );
            }
         }
      }

      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
