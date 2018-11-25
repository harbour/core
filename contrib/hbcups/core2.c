/*
 * CUPS wrappers
 *
 * Copyright 2017 Teo Fonrouge (tfonrouge@gmail.com)
 * Copyright 2010-2017 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include <cups/cups.h>
#include <cups/http.h>

#define HB_CUPS_VERS( ma, mi, pa )  \
   ( CUPS_VERSION_MAJOR > ma || \
   ( CUPS_VERSION_MAJOR == ma && \
   ( CUPS_VERSION_MINOR > mi || \
   ( CUPS_VERSION_MINOR == mi && \
     CUPS_VERSION_PATCH >= pa ) ) ) )

/* Parameter can have two values:

   STRING: host name
   HASH:
      "host"       => <hostName String>
      "port"       => <port Numeric>
      "encryption" => "HTTP_ENCRYPTION_ALWAYS" | "HTTP_ENCRYPTION_IF_REQUESTED" | "HTTP_ENCRYPTION_NEVER" | "HTTP_ENCRYPTION_REQUIRED"
      "blocking"   => .T. | .F.
      "msec"       => <timeout Numeric>
 */
static http_t * s_getHttpParam( int iParam )
{
   PHB_ITEM pHttp = hb_param( iParam, HB_IT_STRING | HB_IT_HASH );

   if( pHttp )
   {
      #if ! HB_CUPS_VERS( 1, 7, 0 )
         #define HTTP_ENCRYPTION_IF_REQUESTED  HTTP_ENCRYPT_IF_REQUESTED
         #define HTTP_ENCRYPTION_NEVER         HTTP_ENCRYPT_NEVER
         #define HTTP_ENCRYPTION_REQUIRED      HTTP_ENCRYPT_REQUIRED
         #define HTTP_ENCRYPTION_ALWAYS        HTTP_ENCRYPT_ALWAYS
      #endif

      const char *      host       = NULL;
      int               port       = 631;
      http_encryption_t encryption = HTTP_ENCRYPTION_IF_REQUESTED;
      int               blocking   = 0;
      int               msec       = 5000;

      if( HB_IS_STRING( pHttp ) )
      {
         host = hb_itemGetCPtr( pHttp );
      }
      else if( HB_IS_HASH( pHttp ) )
      {
         host     = hb_itemGetCPtr( hb_hashGetCItemPtr( pHttp, "host" ) );
         port     = hb_itemGetNI(   hb_hashGetCItemPtr( pHttp, "port" ) );
         blocking = hb_itemGetL(    hb_hashGetCItemPtr( pHttp, "blocking" ) ) ? 1 : 0;
         {
            const char * cVal = hb_itemGetCPtr( hb_hashGetCItemPtr( pHttp, "encryption" ) );

            if( strcmp( cVal, "HTTP_ENCRYPTION_IF_REQUESTED" ) == 0 )
               encryption = HTTP_ENCRYPTION_IF_REQUESTED;
            else if( strcmp( cVal, "HTTP_ENCRYPTION_NEVER" ) == 0 )
               encryption = HTTP_ENCRYPTION_NEVER;
            else if( strcmp( cVal, "HTTP_ENCRYPTION_REQUIRED" ) == 0 )
               encryption = HTTP_ENCRYPTION_REQUIRED;
            else if( strcmp( cVal, "HTTP_ENCRYPTION_ALWAYS" ) == 0 )
               encryption = HTTP_ENCRYPTION_ALWAYS;
         }
         msec = hb_itemGetNI( hb_hashGetCItemPtr( pHttp, "msec" ) );
      }

      if( host && *host )
      {
#if HB_CUPS_VERS( 1, 7, 0 )
         http_addrlist_t * addrlist = NULL;
         int family   = AF_UNSPEC;
         int * cancel = NULL;
         return httpConnect2( host, port, addrlist, family, encryption, blocking, msec, cancel );
#else
         HB_SYMBOL_UNUSED( blocking );
         HB_SYMBOL_UNUSED( msec );
         return httpConnectEncrypt( host, port, encryption );
#endif
      }
   }

   return NULL;
}

HB_FUNC( CUPSGETDEFAULT2 )
{
#if HB_CUPS_VERS( 1, 1, 21 )
   http_t * http = s_getHttpParam( 1 );

   hb_retc( http ? cupsGetDefault2( http ) : NULL );
#else
   hb_retc_null();
#endif
}

HB_FUNC( CUPSGETDESTS2 )
{
#if HB_CUPS_VERS( 1, 1, 21 )
   http_t * http = s_getHttpParam( 1 );

   if( http )
   {
      cups_dest_t * dest_list;
      HB_ISIZ       num_dests = ( HB_ISIZ ) cupsGetDests2( http, &dest_list );
      PHB_ITEM      pArray    = hb_itemArrayNew( num_dests );

      if( num_dests > 0 )
      {
         cups_dest_t * desk_list_bak = dest_list;
         HB_ISIZ       i;

         for( i = 1; i <= num_dests; ++i, ++dest_list )
            hb_arraySetC( pArray, i, dest_list->name );

         cupsFreeDests( num_dests, desk_list_bak );
      }

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
#else
   hb_reta( 0 );
#endif
}

HB_FUNC( CUPSPRINTFILE2 )
{
#if HB_CUPS_VERS( 1, 1, 21 )
   http_t * http = s_getHttpParam( 1 );

   if( http )
   {
      PHB_ITEM pOptions = hb_param( 5, HB_IT_HASH | HB_IT_ARRAY );

      int num_options         = 0;
      cups_option_t * options = NULL;

      if( pOptions )
      {
         HB_SIZE tmp;

         if( HB_IS_HASH( pOptions ) )
         {
            for( tmp = 1; tmp <= hb_hashLen( pOptions ); ++tmp )
            {
               PHB_ITEM pKey = hb_hashGetKeyAt( pOptions, tmp );
               PHB_ITEM pVal = hb_hashGetValueAt( pOptions, tmp );

               if( pKey && HB_IS_STRING( pKey ) && pVal )
                  num_options = cupsAddOption( hb_itemGetCPtr( pKey ), hb_itemGetCPtr( pVal ), num_options, &options );
            }
         }
         else if( HB_IS_ARRAY( pOptions ) )
         {
            for( tmp = 1; tmp <= hb_arrayLen( pOptions ); ++tmp )
            {
               const char * pszOption = hb_arrayGetCPtr( pOptions, tmp );

               if( pszOption )
                  num_options = cupsParseOptions( pszOption, num_options, &options );
            }
         }
      }

      hb_retni( cupsPrintFile2( http /* server */,
                                hb_parcx( 2 ) /* printer name */,
                                hb_parcx( 3 ) /* filename */,
                                hb_parcx( 4 ) /* title */,
                                num_options,
                                options ) );

      cupsFreeOptions( num_options, options );
   }
   else
      hb_retni( -1 );
#else
   hb_retni( -2 );
#endif
}
