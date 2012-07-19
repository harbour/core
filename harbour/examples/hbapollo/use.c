/*
 * $Id: use.c 9576 2012-07-17 16:41:57Z andijahja $
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"

static const char *  cRDD[] = { "SDENTX", "SDEFOX", "SDENSX", "SDENSX_DBT" };
static const char *  aDescription[] =
{
   "WORKAREA (INTEGER)",  "FILENAME (STRING)",     "ALIAS (STRING)",
   "SHARED (HB_BOOL)",       "READONLY (HB_BOOL)",       "RDE TYTE (INTEGER)",
   "OPEN MODE (INTEGER)", "RDE TYPE (STRING)",     "COMMIT LEVEL (INTEGER)",
   "RECSIZE (INTEGER)",   "FIELD COUNT (INTEGER)", "DBF STRUCTURE (ARRAY)"
};

static void _sx_UseGlobalInfo( SX_DBOPENINFO OpenInfo )
{
   PHB_ITEM pData   = hb_itemNew( NULL );
   PHB_ITEM pItem   = hb_itemNew( NULL );

   hb_arrayNew( pItem, 12 ); /* Elements of SX_DBOPENINFO */
   hb_arraySet( pItem, 1, hb_itemPutNI( pData, OpenInfo.uiArea ) );
   hb_arraySet( pItem, 2, hb_itemPutC( pData, OpenInfo.cFilename ) );
   hb_arraySet( pItem, 3, hb_itemPutC( pData, OpenInfo.cAlias ) );
   hb_arraySet( pItem, 4, hb_itemPutL( pData, OpenInfo.fShared ) );
   hb_arraySet( pItem, 5, hb_itemPutL( pData, OpenInfo.fReadonly ) );
   hb_arraySet( pItem, 6, hb_itemPutNI( pData, OpenInfo.iRDEType ) );
   hb_arraySet( pItem, 7, hb_itemPutNI( pData, OpenInfo.iMode ) );
   hb_arraySet( pItem, 8, hb_itemPutC( pData, OpenInfo.cRDD ) );
   hb_arraySet( pItem, 9, hb_itemPutNI( pData, OpenInfo.iCommitLevel ) );
   hb_arraySet( pItem, 10, hb_itemPutNI( pData, OpenInfo.iRecSize ) );
   hb_arraySet( pItem, 11, hb_itemPutNI( pData, OpenInfo.iFieldCount ) );
   hb_arraySet( pItem, 12, OpenInfo.aFieldInfo );
   hb_arrayAdd( Opened_DBF_Property, pItem );

   hb_itemRelease( pData );
   hb_itemRelease( pItem );
}

void _sx_SetDBFInfo( int iOpenedArea, const char * szAlias, int iOpenMode,
                     int iRDEType )
{
   SX_DBOPENINFO  OpenInfo;
   PHB_ITEM       pStruct = _sx_DbStruct();

   OpenInfo.uiArea        = ( HB_USHORT ) iOpenedArea;
   OpenInfo.cFilename     = ( char * ) sx_BaseName();
   OpenInfo.cAlias        = szAlias;
   OpenInfo.fShared       = ( ( iOpenMode == 0 ) ? HB_TRUE : HB_FALSE );
   OpenInfo.fReadonly     = ( ( iOpenMode == READONLY ) ? HB_TRUE : HB_FALSE );
   OpenInfo.iRDEType      = ( HB_USHORT ) iRDEType;
   OpenInfo.iMode         = ( HB_USHORT ) iOpenMode;
   OpenInfo.cRDD          = cRDD[ iRDEType - 1 ];
   OpenInfo.iCommitLevel  = ( HB_USHORT ) sx_GetCommitLevel( ( WORD ) iOpenedArea );
   OpenInfo.iRecSize      = ( HB_USHORT ) sx_RecSize();
   OpenInfo.iFieldCount   = sx_FieldCount();
   OpenInfo.aFieldInfo    = hb_itemNew( NULL );
   hb_itemCopy( OpenInfo.aFieldInfo, pStruct /* _sx_DbStruct() */ );
   _sx_UseGlobalInfo( OpenInfo );
   hb_itemRelease( OpenInfo.aFieldInfo );
   hb_itemRelease( pStruct );
}

HB_FUNC( SX_USE )
{
   if( HB_ISCHAR( 1 ) )
   {
      PBYTE szDBFFileName = ( PBYTE ) _sx_CheckFileExt( hb_parc( 1 ) );

      if( hb_fsFileExists( ( const char * ) szDBFFileName ) )
      {
         char *   szAlias    = NULL;
         HB_BOOL     bIsAlloc;
         int      iRDEType;
         int      iOpenMode;
         int      iOpenedArea;

         /* Alias Passed? */
         if( HB_ISCHAR( 2 ) )
         {
            szAlias    = ( char * ) hb_parc( 2 );
            bIsAlloc   = ( strlen( szAlias ) == 0 );
         }
         else
            bIsAlloc = HB_TRUE;

         if( bIsAlloc )
         {
            szAlias = _sx_AutoAlias( hb_parc( 1 ) );
            hb_storc( szAlias, 2 );
         }

         /* Open Mode Passed? */
         if( HB_ISCHAR( 3 ) )
            iOpenMode = _sx_CheckOpenMode( hb_parc( 3 ) );
         else if( HB_ISNUM( 3 ) )
            iOpenMode = hb_parni( 3 );
         else
            iOpenMode = READWRITE;

         /* RDE Type Passed? */
         iRDEType = HB_ISCHAR( 4 ) ? _sx_CheckRDD( hb_parc( 4 ) ) : i_sxApi_RDD_Default;

         if( HB_ISNUM( 5 ) )
         {
            UINT iCommitLevel = hb_parni( 5 );

            if( iCommitLevel > 2 )
               iCommitLevel = 2;

            iOpenedArea = sx_UseEx( szDBFFileName, ( PBYTE ) szAlias, ( WORD ) iOpenMode,
                                    ( WORD ) iRDEType, ( WORD ) iCommitLevel );
         }
         else
         {
            iOpenedArea = sx_Use( szDBFFileName, ( PBYTE ) szAlias, ( WORD ) iOpenMode,
                                  ( WORD ) iRDEType );
         }

         hb_retni( iOpenedArea );

         if( iOpenedArea > 0 )
            _sx_SetDBFInfo( iOpenedArea, szAlias, iOpenMode, iRDEType );

         if( bIsAlloc )
            hb_xfree( szAlias );
      }
      else
         hb_errRT_BASE( EG_OPEN, 2020, NULL, "SX_USE", 1, hb_paramError( 1 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, "SX_USE", 1, hb_paramError( 1 ) );
}

HB_FUNC( SX_DBINFO )
{
   if( Opened_DBF_Property )
   {
      if( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) )
      {
         PHB_ITEM aSingleInfo   = hb_itemNew( NULL );
         char *   szAlias       = HB_ISCHAR( 1 ) ? ( char * ) hb_parc( 1 ) : ( char * ) sx_Alias( ( WORD ) hb_parni( 1 ) );
         HB_ISIZ  iLen          = strlen( szAlias ) + 1;

         hb_arrayNew( aSingleInfo, 0 );

         /* printf("Here....?\n"); */
         if( iLen > 1 )
         {
            HB_USHORT   ui;
            char *   szTmp = ( char * ) hb_xgrab( iLen );

            hb_snprintf( szTmp, iLen, "%s", szAlias );
            szTmp = _sx_upper( szTmp );

            for( ui = 0; ui < (HB_USHORT) hb_arrayLen( Opened_DBF_Property );
                 ui++ )
            {
               HB_BOOL           bFound     = HB_FALSE;
               PHB_ITEM       pInfo      = hb_arrayGetItemPtr( Opened_DBF_Property, ui + 1 );
               const char *   cAliasInfo = hb_arrayGetCPtr( pInfo, 3 );

               if( cAliasInfo )
               {
                  if( strcmp( szTmp, cAliasInfo ) == 0 )
                  {
                     hb_arrayAdd( aSingleInfo, pInfo );
                     bFound = HB_TRUE;
                  }

                  if( bFound )
                     break;
               }
            }

            hb_xfree( szTmp );
         }

         hb_itemReturnRelease( aSingleInfo );
      }
      else
      {
         if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
         {
            /* Complete Info With Description */
            ULONG    i;
            PHB_ITEM pInfo = NULL;

            hb_arrayCloneTo( pInfo, Opened_DBF_Property );

            for( i = 0; i < hb_arrayLen( pInfo ); i++ )
            {
               PHB_ITEM aDesc      = hb_arrayGetItemPtr( pInfo, i + 1 );
               ULONG    j;
               HB_BOOL     bNotArray  = HB_FALSE;

               for( j = 0; j < hb_arrayLen( aDesc ); j++ )
               {
                  PHB_ITEM pData   = hb_itemNew( NULL );
                  char *   szDesc  = ( char * ) hb_xgrab( 256 );

                  if( hb_arrayGetType( aDesc, j + 1 ) & HB_IT_STRING )
                  {
                     char * szStr = hb_arrayGetC( aDesc, j + 1 );
                     hb_snprintf( szDesc, 255, "%s=%s", aDescription[ j ], szStr );
                     hb_xfree( szStr );
                  }
                  else if( hb_arrayGetType( aDesc, j + 1 ) & HB_IT_NUMERIC )
                     hb_snprintf( szDesc, 255, "%s=%i", aDescription[ j ],
                                  hb_arrayGetNI( aDesc, j + 1 ) );
                  else if( hb_arrayGetType( aDesc, j + 1 ) & HB_IT_LOGICAL )
                     hb_snprintf( szDesc, 255, "%s=%s", aDescription[ j ],
                                  hb_arrayGetL( aDesc, j + 1 ) ? ".T." : ".F." );
                  else if( hb_arrayGetType( aDesc, j + 1 ) & HB_IT_ARRAY )
                  {
                     PHB_ITEM aField = hb_arrayGetItemPtr( aDesc, j + 1 );
                     ULONG    u;

                     bNotArray = HB_TRUE;

                     for( u = 0; u < hb_arrayLen( aField ); u++ )
                     {
                        PHB_ITEM _pF     = hb_arrayGetItemPtr( aField, u + 1 );
                        char *   szField = ( char * ) hb_xgrab( 256 );
                        char *   FName   = hb_arrayGetC( _pF, 1 );
                        char *   FType   = hb_arrayGetC( _pF, 2 );
                        int      FSize   = ( int ) hb_arrayGetNI( _pF, 3 );
                        int      FDec    = ( int ) hb_arrayGetNI( _pF, 4 );
                        PHB_ITEM pField  = hb_itemNew( NULL );

                        hb_snprintf( szField, 255, "{\"%s\", \"%s\", %i, %i}",
                                     FName, FType, FSize, FDec );
                        hb_arraySet( aField, u + 1, hb_itemPutC( pField, szField ) );
                        hb_xfree( szField );
                        hb_xfree( FName );
                        hb_xfree( FType );
                        hb_itemRelease( pField );
                     }
                  }        /* hb_arrayGetType( aDesc, j + 1 ) & HB_IT_ARRAY */

                  if( ! bNotArray )
                     hb_arraySet( aDesc, j + 1, hb_itemPutC( pData, szDesc ) );

                  hb_xfree( szDesc );
                  hb_itemRelease( pData );
               }           /* for ( j = 0; j < hb_arrayLen( aDesc ); j++ ) */
            }              /* for ( i = 0; i< hb_arrayLen( pInfo ); i++ ) */

            hb_itemReturnRelease( pInfo );
         }                 /* (HB_ISLOG(2) && hb_parl(2) ) */
         else
         {
            hb_itemReturn( Opened_DBF_Property );
         }
      }
   }
}

HB_BOOL _sx_Used( void )
{
   return Opened_DBF_Property ? hb_arrayLen( Opened_DBF_Property ) > 0 : HB_FALSE;
}

HB_FUNC( SX_USED )
{
   hb_retl( Opened_DBF_Property ? hb_arrayLen( Opened_DBF_Property ) > 0 : HB_FALSE );
}

int _sx_CheckOpenMode( const char * sSetDefault )
{
   const char *   sxOpenMode[]  = { "READWRITE", "READONLY", "EXCLUSIVE" };
   int            ui;
   int            iOpenMode     = READWRITE;

   for( ui = 0; ui < 3; ui++ )
   {
      if( strcmp( sxOpenMode[ ui ], sSetDefault ) == 0 )
      {
         iOpenMode = ui;
         break;
      }
   }

   return iOpenMode;
}
