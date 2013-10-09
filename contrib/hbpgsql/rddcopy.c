/*
 * Harbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour) (GC support)
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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

#include "hbpgsql.h"

#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbvm.h"
#include "hbdate.h"

typedef struct
{
   char *   buffer;
   int      position;
   int      length;
   HB_BOOL  str_trim;
   PGconn * connection;
} pgCopyContext;

#if PG_VERSION_NUM >= 80000
static HB_BOOL addToContext( pgCopyContext * context, const char c )
{
   if( context->position == context->length )
   {
      if( PQputCopyData( context->connection, context->buffer, context->position ) == -1 )
         return HB_FALSE;

      context->position = 0;
   }
   context->buffer[ context->position++ ] = ( HB_BYTE ) c;

   return HB_TRUE;
}
static HB_BOOL addStrToContext( pgCopyContext * context, const char * str )
{
   while( *str )
   {
      if( context->position == context->length )
      {
         if( PQputCopyData( context->connection, context->buffer, context->position ) == -1 )
            return HB_FALSE;

         context->position = 0;
      }
      context->buffer[ context->position++ ] = ( HB_BYTE ) *str++;
   }

   return HB_TRUE;
}
static HB_BOOL addStrnToContext( pgCopyContext * context, const char * str, HB_SIZE size )
{
   HB_SIZE nSize = 0;

   while( nSize < size )
   {
      if( context->connection == NULL || context->position == context->length )
      {
         if( PQputCopyData( context->connection, context->buffer, context->position ) == -1 )
            return HB_FALSE;
         context->position = 0;
      }
      context->buffer[ context->position++ ] = ( HB_BYTE ) str[ nSize++ ];
   }

   return HB_TRUE;
}

/* Export field value into the buffer in PG accepted CSV format */
static HB_BOOL exportBufSqlVar( pgCopyContext * context, PHB_ITEM pValue, const char * szQuote, const char * szEsc )
{
   switch( hb_itemType( pValue ) )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
      {
         HB_SIZE      nLen  = hb_itemGetCLen( pValue );
         HB_SIZE      nCnt  = 0;
         const char * szVal = hb_itemGetCPtr( pValue );

         if( ! addStrToContext( context, szQuote ) )
            return HB_FALSE;

         if( context->str_trim )
         {
            while( nLen && HB_ISSPACE( szVal[ nLen - 1 ] ) )
               nLen--;
         }

         while( *szVal && nCnt++ < nLen )
         {
            /* if( *szVal == *szDelim || *szVal == *szEsc || *szVal == *szQuote )
               we don't need to escape delim in CSV mode,
               only the quote and the escape itself */

            if( *szVal == *szQuote || *szVal == *szEsc )
               if( ! addToContext( context, *szEsc ) )
                  return HB_FALSE;
            if( ( HB_UCHAR ) *szVal >= 32 )
               if( ! addToContext( context, *szVal ) )
                  return HB_FALSE;
            szVal++;
         }
         if( ! addStrToContext( context, szQuote ) )
            return HB_FALSE;
         break;
      }

      case HB_IT_DATE:
      {
         char szDate[ 9 ];

         if( ! addStrToContext( context, szQuote ) )
            return HB_FALSE;
         hb_itemGetDS( pValue, szDate );
         if( szDate[ 0 ] == ' ' )
         {
            if( ! addStrToContext( context, "0100-01-01" ) )
               return HB_FALSE;
         }
         else
         {
            if( ! addStrnToContext( context, &szDate[ 0 ], 4 ) ||
                ! addToContext( context, '-' ) ||
                ! addStrnToContext( context, &szDate[ 4 ], 2 ) ||
                ! addToContext( context, '-' ) ||
                ! addStrnToContext( context, &szDate[ 6 ], 2 ) )
               return HB_FALSE;
         }
         if( ! addStrToContext( context, szQuote ) )
            return HB_FALSE;
         break;
      }

      case HB_IT_TIMESTAMP:
      {
         long lDate, lTime;
         char szDateTime[ 24 ];

         hb_itemGetTDT( pValue, &lDate, &lTime );
         hb_timeStampStr( szDateTime, lDate, lTime );
         if( ! addStrToContext( context, szQuote ) ||
             ! addStrToContext( context, szDateTime ) ||
             ! addStrToContext( context, szQuote ) )
            return HB_FALSE;
         break;
      }

      case HB_IT_LOGICAL:
#if 0
         if( ! addStrToContext( context, szQuote ) || ! addToContext( context, hb_itemGetL( pValue ) ? 'Y' : 'N' ) || ! addStrToContext( context, szQuote ) )
#endif
         if( ! addToContext( context, hb_itemGetL( pValue ) ? 'Y' : 'N' ) )
            return HB_FALSE;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
      {
         char szResult[ HB_MAX_DOUBLE_LENGTH ];
         int  iSize, iWidth, iDec;

         hb_itemGetNLen( pValue, &iWidth, &iDec );
         iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );
         if( hb_itemStrBuf( szResult, pValue, iSize, iDec ) )
         {
            int iPos = 0;
            while( iSize && HB_ISSPACE( szResult[ iPos ] ) )
            {
               iPos++;
               iSize--;
            }
            if( ! addStrnToContext( context, &szResult[ iPos ], iSize ) )
               return HB_FALSE;
         }
         else
         if( ! addToContext( context, '0' ) )
            return HB_FALSE;
         break;
      }
      /* an "M" field or the other, might be a "V" in SixDriver */
      default:
         return HB_FALSE;
   }

   return HB_TRUE;
}
#endif

HB_FUNC( HB_PQCOPYFROMWA )
{
#if PG_VERSION_NUM >= 80000
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      static const char * sc_szQuote = "\"";
      static const char * sc_szEsc   = "\"";
      static const char * sc_szDelim = ",";

      PGconn * pConn = hb_PGconn_par( 1 );

      const char *    szTable   = hb_parcx( 2 );
      PHB_ITEM        pWhile    = hb_param( 3, HB_IT_BLOCK );
      PHB_ITEM        pFor      = hb_param( 4, HB_IT_BLOCK );
      PHB_ITEM        pFields   = hb_param( 5, HB_IT_ARRAY );
      HB_ULONG        nCount    = hb_parnldef( 6, 0 );
      HB_BOOL         str_rtrim = hb_parldef( 7, HB_TRUE );
      HB_ULONG        nBufLen   = hb_parnldef( 8, 1 );
      HB_USHORT       uiFields;
      HB_ULONG        uiRecCount;
      HB_BOOL         bNoFieldPassed = ( pFields == NULL || hb_arrayLen( pFields ) == 0 );
      HB_BOOL         bEof = HB_FALSE;
      PHB_ITEM        pItem;
      HB_USHORT       uiFieldCopy = 0;
      HB_USHORT       uiIter;
      pgCopyContext * context;
      char *          szInit;
      char *          szFields = NULL;
      char *          szTmp;
      PGresult *      pgResult;
      HB_BOOL         bFail    = HB_FALSE;

      pItem = hb_itemNew( NULL );

      context = ( pgCopyContext * ) hb_xgrab( sizeof( pgCopyContext ) );
      memset( context, 0, sizeof( pgCopyContext ) );

      context->buffer     = ( char * ) hb_xgrab( sizeof( char ) * nBufLen * 1400 );
      context->position   = 0;
      context->length     = sizeof( char ) * nBufLen * 1400;
      context->str_trim   = str_rtrim;
      context->connection = pConn;

      SELF_FIELDCOUNT( pArea, &uiFields );

      if( ! bNoFieldPassed )
      {
         szFields      = ( char * ) hb_xgrab( sizeof( char ) * 2 );
         szFields[ 0 ] = '(';
         szFields[ 1 ] = '\0';
         uiFieldCopy   = ( HB_USHORT ) hb_arrayLen( pFields );

         for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
         {
            const char * szFieldName = hb_arrayGetCPtr( pFields, uiIter );
            if( szFieldName )
            {
               int iPos = hb_rddFieldIndex( pArea, szFieldName );

               szTmp = hb_xstrcpy( NULL, szFields, szFieldName, NULL );
               hb_xfree( szFields );
               szFields = szTmp;
               if( uiIter != uiFieldCopy )
               {
                  szTmp = hb_xstrcpy( NULL, szFields, sc_szDelim, NULL );
                  hb_xfree( szFields );
                  szFields = szTmp;
               }

               if( iPos )
               {
                  PHB_ITEM pFieldNum = hb_itemPutNI( NULL, iPos );
                  hb_itemArrayPut( pFields, uiIter, pFieldNum );
                  hb_itemRelease( pFieldNum );
                  continue;
               }
            }

            if( hb_arrayDel( pFields, uiIter ) )
            {
               hb_arraySize( pFields, hb_arrayLen( pFields ) - 1 );
               uiIter--;
               uiFieldCopy--;
            }
         }
         szTmp = hb_xstrcpy( NULL, szFields, ")", NULL );
         hb_xfree( szFields );
         szFields = szTmp;
      }

      if( szFields )
      {
         szInit = hb_xstrcpy( NULL, "COPY ", szTable, " ", szFields, " FROM STDIN WITH DELIMITER '", sc_szDelim, "' CSV  QUOTE AS '", sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", NULL );
         hb_xfree( szFields );
      }
      else
         szInit = hb_xstrcpy( NULL, "COPY ", szTable, " FROM STDIN WITH DELIMITER '", sc_szDelim, "' CSV  QUOTE AS '", sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", NULL );

      pgResult = PQexec( context->connection, szInit );
      if( PQresultStatus( pgResult ) != PGRES_COPY_IN )
      {
         PQclear( pgResult );
         hb_xfree( szInit );
         hb_xfree( context );
         hb_retl( HB_FALSE );
         return;
      }

      PQclear( pgResult );
      hb_xfree( szInit );

      uiRecCount = 0;
      while( ( nCount == 0 || uiRecCount < nCount ) &&
             ( ! pWhile || hb_itemGetL( hb_vmEvalBlock( pWhile ) ) ) )
      {

         if( SELF_EOF( pArea, &bEof ) != HB_SUCCESS )
            break;

         if( bEof )
            break;

         if( ! pFor || hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
         {
            if( bNoFieldPassed )
            {
               for( uiIter = 1; uiIter <= uiFields; uiIter++ )
               {
                  SELF_GETVALUE( pArea, uiIter, pItem );
                  if( ! exportBufSqlVar( context, pItem, sc_szQuote, sc_szEsc ) || ! addStrToContext( context, sc_szDelim ) )
                  {
                     bFail = HB_TRUE;
                     break;
                  }
               }
            }
            else
            {
               for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
               {
                  SELF_GETVALUE( pArea, ( HB_USHORT ) hb_arrayGetNI( pFields, uiIter ), pItem );
                  if( ! exportBufSqlVar( context, pItem, sc_szQuote, sc_szEsc ) || ! addStrToContext( context, sc_szDelim ) )
                  {
                     bFail = HB_TRUE;
                     break;
                  }
               }
            }

            if( bFail )
               break;

            context->position--;                         /* overwrite last comma with newline */
            if( ! addStrnToContext( context, "\n", 1 ) ) /* PostgreSQL handles both \r\n & \n, use shorter */
            {
               bFail = HB_TRUE;
               break;
            }

            uiRecCount++;
         }

         if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
            break;
      }

      for(;; )
      {
         if( bFail )
         {
            PQputCopyEnd( context->connection, "export buffer problems" );
            hb_retl( HB_FALSE );
            break;
         }

         if( ! addStrnToContext( context, "\\.\n", 3 ) ) /* end CSV transfer */
         {
            hb_retl( HB_FALSE );
            break;
         }

         if( PQputCopyData( context->connection, context->buffer, context->position ) == -1 )
         {
            hb_retl( HB_FALSE );
            break;
         }

         if( PQputCopyEnd( context->connection, NULL ) == -1 )
         {
            hb_retl( HB_FALSE );
            break;
         }

         pgResult = PQgetResult( context->connection );
         while( pgResult )
         {
            if( PQresultStatus( pgResult ) != PGRES_COMMAND_OK )
               bFail = HB_TRUE;

            PQclear( pgResult );
            pgResult = PQgetResult( context->connection );
         }

         if( bFail )
         {
            hb_retl( HB_FALSE );
            break;
         }

         hb_retl( HB_TRUE );
         break;
      }

      hb_itemRelease( pItem );
      hb_xfree( context->buffer );
      hb_xfree( context );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
#else
   hb_retc_null();
#endif
}
