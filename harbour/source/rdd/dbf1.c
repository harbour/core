/*
 * $Id$

   Copyright(C) 1999 by Bruno Cantero.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: bruno@issnet.net
 */

#include <time.h>
#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "rddapi.h"
#include "rddsys.ch"

typedef struct
{
   BYTE   bVersion;
   BYTE   bYear;
   BYTE   bMonth;
   BYTE   bDay;
   ULONG  ulRecords;
   USHORT uiHeaderLen;
   USHORT uiRecordLen;
   BYTE   bReserved1[ 16 ];
   BYTE   bHasTag;
   BYTE   bReserved2[ 3 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;


typedef struct
{
   BYTE bName[ 11 ];
   BYTE bType;
   BYTE bReserved1[ 4 ];
   BYTE bLen;
   BYTE bDec;
   BYTE bReserved2[ 13 ];
   BYTE bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;


HARBOUR HB__DBF( void );
HARBOUR HB_DBF_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBF",             FS_PUBLIC, HB__DBF,             0 },
{ "DBF_GETFUNCTABLE", FS_PUBLIC, HB_DBF_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbf1__InitSymbols
#endif

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling DBF: Bof()\n" );
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling DBF: Eof()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   printf( "Calling DBF: Found()\n" );
   return SUCCESS;
}

static ERRCODE GoBottom( AREAP pArea )
{
   printf( "Calling DBF: GoBottom()\n" );
   return SUCCESS;
}

static ERRCODE GoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling DBF: GoTo()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   printf( "Calling DBF: GoTop()\n" );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling DBF: Skip()\n" );
   return SUCCESS;
}

static ERRCODE CreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiCount;
   PHB_ITEM pFieldDesc;
   DBFIELDINFO pFieldInfo;

   SELF_SETFIELDEXTENT( pArea, pStruct->item.asArray.value->ulLen );
   pFieldInfo.typeExtended = 0;
   for( uiCount = 0; uiCount < pStruct->item.asArray.value->ulLen; uiCount++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiCount;
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetString( pFieldDesc, 1 );
      pFieldInfo.uiLen = 0;
      pFieldInfo.uiType = toupper( hb_arrayGetString( pFieldDesc, 2 )[ 0 ] );
      if( pFieldInfo.uiType == 'N' )
      {
         pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         pFieldInfo.uiDec = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 );
      }
      else
      {
         if( pFieldInfo.uiType == 'L' || pFieldInfo.uiType == 'D' ||
             pFieldInfo.uiType == 'M' )
            pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         else if( pFieldInfo.uiType == 'C' )
         {
            pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 ) +
                               ( ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 ) << 8 );
         }
         pFieldInfo.uiDec = 0;
      }
      SELF_ADDFIELD( pArea, &pFieldInfo );
   }
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   printf( "Calling DBF: Close()\n" );
   return SUCCESS;
}

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ".DBF" );
         break;
   }
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   printf( "Calling DBF: Open()\n" );
   return SUCCESS;
}

static ERRCODE WriteDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFFIELD pDBField;
   USHORT uiCount;
   LPFIELD pField;
   time_t t;
   struct tm * pTime;

   memset( &pHeader, 0, sizeof( DBFHEADER ) );
   pHeader.uiRecordLen = 1;
   pHeader.bVersion = 0x03;
   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      switch( pField->uiType )
      {
         case 'C':
         case 'N':
            pHeader.uiRecordLen += pField->uiLen;
            break;

         case 'M':
            pHeader.uiRecordLen += 10;
            pHeader.bVersion = 0x83;
            break;

         case 'D':
            pHeader.uiRecordLen += 8;
            break;

         case 'L':
            pHeader.uiRecordLen += 1;
            break;
      }
      pField++;
   }

   time( &t );
   pTime =  localtime( &t );
   pHeader.bYear = ( BYTE ) pTime->tm_year;
   pHeader.bMonth = ( BYTE ) pTime->tm_mon + 1;
   pHeader.bDay = ( BYTE ) pTime->tm_mday;
   pHeader.uiHeaderLen = ( USHORT ) ( 32 * ( pArea->uiFieldCount + 1 ) + 1 );
   pHeader.bHasTag = 0;
   pHeader.ulRecords = 0;
   if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                   sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      memset( &pDBField, 0, sizeof( DBFFIELD ) );
      strncpy( ( char * ) pDBField.bName, ( const char * ) pField->sym, sizeof( pDBField.bName ) );
      hb_strUpper( ( char * ) pDBField.bName, strlen( ( char * ) pDBField.bName ) );
      pDBField.bType = pField->uiType;
      switch( pDBField.bType )
      {
         case 'C':
            pDBField.bLen = pField->uiLen & 0xFF;
            pDBField.bDec = pField->uiLen >> 8;
            break;

         case 'M':
            pDBField.bLen = 10;
            pDBField.bDec = 0;
            break;

         case 'D':
            pDBField.bLen = 8;
            pDBField.bDec = 0;
            break;

         case 'L':
            pDBField.bLen = 1;
            pDBField.bDec = 0;
            break;

         case 'N':
            pDBField.bLen = pField->uiLen;
            pDBField.bDec = pField->uiDec;
            break;
      }
      if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) &pDBField,
                      sizeof( DBFFIELD ) ) != sizeof( DBFFIELD ) )
         return FAILURE;
      pField++;
   }
   if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) "\15\32", 2 ) != 2 )
      return FAILURE;
   return SUCCESS;
}

static RDDFUNCS dbfSuper = { 0 };

static RDDFUNCS dbfTable = { Bof,
                             Eof,
                             Found,
                             GoBottom,
                             GoTo,
                             GoTop,
                             Skip,
                             0,               /* Super AddField */
                             CreateFields,
                             0,               /* Super SetFieldsExtent */
                             Close,
                             0,               /* Super Create */
                             Info,
                             0,               /* Super NewArea */
                             Open,
                             0,               /* Super Release */
                             0,               /* Super StructSize */
                             0,               /* Super SysName */
                             WriteDBHeader
                           };

HARBOUR HB__DBF( void )
{
}

HARBOUR HB_DBF_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &dbfTable, &dbfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
