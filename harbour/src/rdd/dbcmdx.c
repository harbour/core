/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base RDD module (XPP functions)
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#ifdef HB_COMPAT_XPP

HB_FUNC_EXTERN( HB_DBPACK );

HB_FUNC( DBPACK )
{
   HB_FUNC_EXEC( HB_DBPACK );
}

HB_FUNC_EXTERN( HB_DBZAP );

HB_FUNC( DBZAP )
{
   HB_FUNC_EXEC( HB_DBZAP );
}

HB_FUNC( ORDWILDSEEK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      const char * szPattern = hb_parc( 1 );

      if( szPattern )
      {
         HB_BOOL fCont = hb_parl( 2 ), fBack = hb_parl( 3 ), fFound = HB_FALSE;
         DBORDERINFO OrderInfo;
         HB_ERRCODE errCode = HB_SUCCESS;

         memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
         OrderInfo.itmResult = hb_itemNew( NULL );

         if( !fCont )
         {
            const char * szKey;

            if( fBack )
               errCode = SELF_GOBOTTOM( pArea );
            else
               errCode = SELF_GOTOP( pArea );

            if( errCode == HB_SUCCESS )
            {
               errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &OrderInfo );
               if( errCode == HB_SUCCESS )
               {
                  szKey = hb_itemGetCPtr( OrderInfo.itmResult );
                  fFound = hb_strMatchWild( szKey, szPattern );
               }
            }
         }
         if( !fFound && errCode == HB_SUCCESS )
         {
            OrderInfo.itmNewVal = hb_param( 1, HB_IT_STRING );
            if( SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD,
                          &OrderInfo ) == HB_SUCCESS )
               fFound = hb_itemGetL( OrderInfo.itmResult );
         }
         hb_itemRelease( OrderInfo.itmResult );
         hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBSKIPPER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      LONG lSkipped = 0;
      LONG lRecs = 1;
      HB_BOOL fBEof;
      ULONG ulRecords = 0;

      if( SELF_RECCOUNT( pArea, &ulRecords ) == HB_SUCCESS && ulRecords > 0 )
      {
         if( HB_ISNUM( 1 ) )
            lRecs = hb_parnl( 1 );

         if( lRecs == 0 )
            SELF_SKIP( pArea, 0 );
         else if( lRecs > 0 )
         {
            if( SELF_EOF( pArea, &fBEof ) == HB_SUCCESS )
            {
               while( lSkipped < lRecs )
               {
                  if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
                     break;
                  if( SELF_EOF( pArea, &fBEof ) != HB_SUCCESS )
                     break;
                  if( fBEof )
                  {
                     SELF_SKIP( pArea, -1 );
                     break;
                  }
                  lSkipped++;
               }
            }
         }
         else /* if( lRecs < 0 ) */
         {
            while( lSkipped > lRecs )
            {
               if( SELF_SKIP( pArea, -1 ) != HB_SUCCESS )
                  break;
               if( SELF_BOF( pArea, &fBEof ) != HB_SUCCESS )
                  break;
               if( fBEof )
                  break;
               lSkipped--;
            }
         }
      }
      hb_retnl( lSkipped );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

#endif
