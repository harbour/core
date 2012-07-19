/*
 * $Id$
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

HB_FUNC( SX_CREATENEW )
{
   hb_retni( sx_CreateNew( ( PBYTE ) hb_parc( 1 ),       /* Field name */
                           ( PBYTE ) hb_parc( 2 ),       /* Alias */
                           ( WORD ) hb_parni( 3 ),       /* RDE Type */
                           ( WORD ) hb_parni( 4 ) ) );   /* The maximum number of fields to
                                                            be added to the file structure */
}

HB_FUNC( SX_CREATEEXEC )
{
   hb_retl( sx_CreateExec() );
}

HB_FUNC( SX_CREATEFIELD )
{
   sx_CreateField( ( PBYTE ) hb_parc( 1 ),   /* Field name */
                   ( PBYTE ) hb_parc( 2 ),   /* Field type */
                   ( WORD ) hb_parni( 3 ),   /* Field lenght */
                   ( WORD ) hb_parni( 4 ) ); /* Field decimals */
}

HB_FUNC( SX_CREATEFROM )
{
   PBYTE cpFileName,
         cpAlias,
         cpSourceFile;
   int   iRDEType;
   HB_BOOL  lIsAlloc = HB_FALSE;

   if( ! HB_ISCHAR( 1 ) || ! HB_ISCHAR( 3 ) )
      hb_retl( HB_FALSE );

   /* New DBF File Name To Be Created */
   cpFileName = ( PBYTE ) hb_parc( 1 );
   if( strlen( ( char * ) cpFileName ) == 0 )
      hb_retl( HB_FALSE );

   /* Alias */
   if( HB_ISCHAR( 2 ) )
      cpAlias = ( PBYTE ) hb_parc( 2 );
   else
      cpAlias = ( PBYTE ) "";

   if( strlen( ( char * ) cpAlias ) == 0 )
   {
      cpAlias    = ( PBYTE ) _sx_AutoAlias( ( char * ) hb_parc( 1 ) );
      lIsAlloc   = HB_TRUE;
   }

   /* Source File Name for Structure */
   cpSourceFile = ( PBYTE ) hb_parc( 3 );

   /* RDD */
   if( HB_ISCHAR( 4 ) )
      iRDEType = _sx_CheckRDD( ( char * ) hb_parc( 4 ) );
   else if( HB_ISNUM( 4 ) )
      iRDEType = hb_parni( 4 );
   else
      iRDEType = i_sxApi_RDD_Default;

   if( ( iRDEType <= 0 ) || ( iRDEType > 4 ) )
      iRDEType = i_sxApi_RDD_Default;

   hb_retl( sx_CreateFrom( cpFileName, cpAlias, cpSourceFile, ( WORD ) iRDEType ) );

   _sx_SetDBFInfo( sx_WorkArea( cpAlias ), ( char * ) cpAlias, EXCLUSIVE, ( WORD ) iRDEType );

   if( lIsAlloc )
   {
      sx_Select( sx_WorkArea( cpAlias ) );
      sx_Close(); /* Close DBF if cAlias is not passed */
      _sx_DelOpenInfo( ( char * ) cpAlias );
      hb_xfree( cpAlias );
   }
}

HB_FUNC( SX_CREATENEWEX )
{
   UINT iCommitLevel = 1;

   if( HB_ISNUM( 5 ) )
   {
      iCommitLevel = hb_parni( 5 );
      if( iCommitLevel > 2 )
         iCommitLevel = 1;
   }

   hb_retni( sx_CreateNewEx( ( PBYTE ) hb_parc( 1 ),  /* Field name */
                             ( PBYTE ) hb_parc( 2 ),  /* Alias */
                             ( WORD ) hb_parni( 3 ),  /* RDE Type */
                             ( WORD ) hb_parni( 4 ),  /* The maximum number of fields to be added to the file structure */
                             iCommitLevel ) );
}

HB_FUNC( SX_DBCREATE )
{
   HB_USHORT uiSize,
          uiLen,
          iDec,
          iFieldLen,
          nResult;
   HB_BOOL  bAutoOpen,
         bSuccess = HB_FALSE;
   PBYTE cFieldName,
         cFieldType,
         szFileName,
         szAlias;
   PHB_ITEM pStruct,
            pFieldDesc;
   WORD     nDriver;
   HB_BOOL     bAlloc = HB_FALSE;
   HB_USHORT   iWorkArea;

   /*
      File Name Passed
    */

   /* printf("sx_dbcreate...1\n"); */
   if( HB_ISCHAR( 1 ) )
      szFileName = ( PBYTE ) hb_parc( 1 );
   else
      szFileName = ( PBYTE ) "";

   /*
      Array of Structure Passed
    */

   /* printf("sx_dbcreate...2\n"); */
   pStruct = hb_param( 2, HB_IT_ARRAY );
   if( pStruct )
      uiLen = ( HB_USHORT ) hb_arrayLen( pStruct );
   else
      uiLen = 0;

   /*
      Alias Passed
    */

   /* printf("sx_dbcreate...3\n"); */
   if( HB_ISCHAR( 4 ) )
   {
      bAutoOpen  = HB_TRUE;
      szAlias    = ( PBYTE ) hb_parc( 4 );
   }
   else
   {
      bAutoOpen  = HB_FALSE;
      szAlias    = ( PBYTE ) _sx_randomname( NULL );
      bAlloc     = HB_TRUE;
   }

   /*
      Assign Alias Automatically if not passed using ROOT File Name
    */

   /* printf("sx_dbcreate...4\n"); */
   if( strlen( ( char * ) szAlias ) == 0 )
   {
      szAlias = ( PBYTE ) _sx_AutoAlias( ( char * ) hb_parc( 1 ) );
      bAlloc  = HB_TRUE;
   }

   /*
      Driver
      Default is as set up by sx_RDDSetDefault(), SDENSX
    */

   /* printf("sx_dbcreate...5\n"); */
   if( HB_ISCHAR( 3 ) )
   {
      nDriver = ( WORD ) _sx_CheckRDD( ( char * ) hb_parc( 3 ) );
      if( ( nDriver <= 0 ) || ( nDriver > 4 ) )
         nDriver = ( WORD ) i_sxApi_RDD_Default;
   }
   else
      nDriver = ( WORD ) i_sxApi_RDD_Default;

   /* printf("sx_dbcreate...6\n"); */
   if( ( strlen( ( char * ) szFileName ) == 0 ) || ! pStruct || uiLen == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "SX_DBCREATE" );
      return;
   }

   /* printf("sx_dbcreate...7\n"); */
   for( uiSize = 0; uiSize < uiLen; uiSize++ )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize + 1 );

      if( hb_arrayLen( pFieldDesc ) < 4 )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "SX_DBCREATE" );
         return;
      }

      /*
         Validate items types of fields
       */
      if( ! ( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          ! ( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "SX_DBCREATE" );
         return;
      }
   }

   /*
      Now Create DBF
    */

   /* printf("sx_dbcreate...8\n"); */
   if( HB_ISNUM( 5 ) )        /* nCommitLevel Passed */
      nResult = sx_CreateNewEx( szFileName, ( PBYTE ) szAlias, nDriver, uiLen,
                                hb_parni( 5 ) );
   else
      nResult = sx_CreateNew( szFileName, ( PBYTE ) szAlias, nDriver, uiLen );

   if( nResult )
   {
      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize + 1 );
         cFieldName = ( PBYTE ) hb_arrayGetC( pFieldDesc, 1 );
         cFieldName = ( PBYTE ) _sx_alltrim( ( char * ) cFieldName );
         cFieldType = ( PBYTE ) hb_arrayGetC( pFieldDesc, 2 );
         iFieldLen  = ( SHORT ) hb_arrayGetNI( pFieldDesc, 3 );
         iDec       = ( SHORT ) hb_arrayGetNI( pFieldDesc, 4 );
         sx_CreateField( cFieldName, cFieldType, iFieldLen, iDec );
         hb_xfree( cFieldType );
         hb_xfree( cFieldName );
      }

      bSuccess = sx_CreateExec();

      if( bSuccess )
      {
         if( ! bAutoOpen )
            sx_Close();
         else
         {
            iWorkArea = sx_WorkArea( szAlias );
            if( iWorkArea > 0 )
               _sx_SetDBFInfo( iWorkArea, ( char * ) szAlias, READWRITE, nDriver );
         }
      }
   }

   hb_retl( bSuccess );

   if( bAlloc )
      hb_xfree( szAlias );
}
