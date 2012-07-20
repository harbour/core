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

#define __SXAPI_INIT
#include "sxapi.h"

static void __sx_CreateINITFile( const char * pIniFile )
{
   FILE * FIniHandle = hb_fopen( pIniFile, "w" );

   fprintf( FIniHandle, ";==========================\n" );
   fprintf( FIniHandle, ";SIXAPI initialization file\n" );
   fprintf( FIniHandle, ";==========================\n" );

   /* Notes */
   fprintf( FIniHandle, ";DateFormat AMERICAN  0 /* MM/DD/YY */\n" );
   fprintf( FIniHandle, ";DateFormat ANSI      1 /* YY.MM.DD */\n" );
   fprintf( FIniHandle, ";DateFormat BRITISH   2 /* DD/MM/YY */\n" );
   fprintf( FIniHandle, ";DateFormat FRENCH    3 /* DD/MM/YY */\n" );
   fprintf( FIniHandle, ";DateFormat GERMAN    4 /* DD.MM.YY */\n" );
   fprintf( FIniHandle, ";DateFormat ITALIAN   5 /* DD-MM-YY */\n" );
   fprintf( FIniHandle, ";DateFormat SPANISH   6 /* DD-MM-YY */\n\n" );
   fprintf( FIniHandle, ";RDEType SDENTX       1\n" );
   fprintf( FIniHandle, ";RDEType SDEFOX       2\n" );
   fprintf( FIniHandle, ";RDEType SDENSX       3\n" );
   fprintf( FIniHandle, ";RDEType SDENSXDBT    4\n\n" );
   fprintf( FIniHandle, ";RDEType DBFNTX       1\n" );
   fprintf( FIniHandle, ";RDEType DBFFOX       2\n" );
   fprintf( FIniHandle, ";RDEType DBFNSX       3\n" );
   fprintf( FIniHandle, ";RDEType DBFNSXDBT    4\n\n" );

   fprintf( FIniHandle, ";Commit Level\n" );
   fprintf( FIniHandle, ";0. Full commit. Always write data to disk. Do not use SDE buffers and force\n" );
   fprintf( FIniHandle, ";Windows to write any cached data. This offers the slowest performance\n" );
   fprintf( FIniHandle, ";(100x slower than level = 2 in some cases) but the data is guaranteed to\n" );
   fprintf( FIniHandle, ";always be saved upon each write.\n" );
   fprintf( FIniHandle, ";1. Normal commit. Do not use the SDE buffers, but do not force Windows to\n" );
   fprintf( FIniHandle, ";write the cached data. This offers very good performance and allows Windows\n" );
   fprintf( FIniHandle, ";to manage the data caching. (Usually 50-100%s slower than level = 2)\n", "%" );
   fprintf( FIniHandle, ";2. None. Let the SDE use its buffering mechanisms and do not force Windows\n" );
   fprintf( FIniHandle, ";to write the cached data. This offers the best performance\n" );
   fprintf( FIniHandle, ";(100x faster than level = 0 in some cases).\n\n" );

   fprintf( FIniHandle, ";ErrorLevel\n" );
   fprintf( FIniHandle, ";0  No error message at all.\n" );
   fprintf( FIniHandle, ";1  Only report Fatal errors (Default).\n" );
   fprintf( FIniHandle, ";2  Report all errors.\n\n" );

   fprintf( FIniHandle, ";MemoBlock Size\n" );
   fprintf( FIniHandle, ";This does not apply to CA-Clipper .DBT memo files, which use\n" );
   fprintf( FIniHandle, ";fixed 512 byte blocks.\n" );
   fprintf( FIniHandle, ";The default .FPT memo block size is 32 bytes. The default .SMT memo block size\n" );
   fprintf( FIniHandle, ";is 1. This function sets a new default block size that will be used when creating\n" );
   fprintf( FIniHandle, ";any new table that has memos and will also change the block size in memo files\n" );
   fprintf( FIniHandle, ";when the DBF is packed. It does not affect existing memo files except when the\n" );
   fprintf( FIniHandle, ";corresponding DBF is packed.\n" );
   fprintf( FIniHandle, ";The size must be a value from 1 through 1024.\n\n" );

   /* Preset Defaults */
   fprintf( FIniHandle, "\n[Setup]\n" );
   fprintf( FIniHandle, "SetErrorLevel=1\n" );
   fprintf( FIniHandle, "SetCommitLevel=2\n" );
   fprintf( FIniHandle, "SetDateFormat=2\n" );
   fprintf( FIniHandle, "SetCentury=1\n" );
   fprintf( FIniHandle, "SetDeleted=1\n" );
   fprintf( FIniHandle, "SetRDDDefault=3\n" );
   fprintf( FIniHandle, "SetMemoBlockSize=1\n" );
   fprintf( FIniHandle, "AutoOpenIndexIsDisabled=1\n" );
   fclose( FIniHandle );
}

static void hb_sixapiRddInit( void * cargo )
{
   BYTE  bBufferEXE[ 250 ];
   char  pIniFile[ 256 ];
   int   iDateFormat,
         iCentury,
         iDeleted,
         iAutoOpenDisabled,
         iCommitLevel;
   PHB_FNAME pFileName;

   HB_SYMBOL_UNUSED( cargo );

   GetModuleFileName( NULL, ( char * ) bBufferEXE, 249 );

   pFileName = hb_fsFNameSplit( ( const char * ) bBufferEXE );

   /* Modifiable SET-UP in SDE.INI */
   hb_snprintf( pIniFile, sizeof( pIniFile ), "%s/%s", pFileName->szPath, "sde.ini" );

   hb_xfree( pFileName );

   if( ! hb_fsFileExists( ( const char * ) pIniFile ) )
      __sx_CreateINITFile( ( const char * ) pIniFile );

   i_sxApi_Error_Level    = GetPrivateProfileInt( "Setup", "SetErrorLevel", 1, pIniFile );
   i_sxApi_RDD_Default    = GetPrivateProfileInt( "Setup", "SetRDDDefault", 3, pIniFile );
   iDateFormat            = GetPrivateProfileInt( "Setup", "SetDateFormat", 2, pIniFile );
   iDeleted               = GetPrivateProfileInt( "Setup", "SetDeleted", 1, pIniFile );
   iCentury               = GetPrivateProfileInt( "Setup", "SetCentury", 1, pIniFile );
   iCommitLevel           = GetPrivateProfileInt( "Setup", "SetCommitLevel", 2, pIniFile );
   iAutoOpenDisabled      = GetPrivateProfileInt( "Setup", "AutoOpenIndexIsDisabled", 1, pIniFile );
   i_sxApi_MemoBlock_Size = GetPrivateProfileInt( "Setup", "SetMemoBlockSize", 32, pIniFile );

   /* Disable AutoOpen Index File */
   if( iAutoOpenDisabled == 1 )
      _sx_SysProp( SDE_SP_SETDISABLEAUTO, ( PVOID ) 1 );

   sx_ErrorLevel( ( WORD ) i_sxApi_Error_Level );
   sx_CommitLevel( ( WORD ) iCommitLevel );
   sx_SetMemoBlockSize( ( WORD ) i_sxApi_MemoBlock_Size );

   /*
      C Declaration
      INT SDEAPI WINAPI sx_CommitLevel (INT nNewLevel);

      Description
      Set how and the SDE and Windows writes data to disk.

      Parameters
      nNewLevel: Value to set the commit level to. Possible values are 0,1 or 2.

      0. Full commit. Always write data to disk. Do not use SDE buffers and force
      Windows to write any cached data. This offers the slowest performance
      (100x slower than level = 2 in some cases) but the data is guaranteed to
      always be saved upon each write.

      1. Normal commit. Do not use the SDE buffers, but do not force Windows to
      write the cached data. This offers very good performance and allows Windows
      to manage the data caching. (Usually 50-100% slower than level = 2)

      2. None. Let the SDE use its buffering mechanisms and do not force Windows
      to write the cached data. This offers the best performance
      (100x faster than level = 0 in some cases).

      Return Value
      Current commit level value.
    */

   /* Global Set Ups */
   sx_SetStringType( 1 );                    /* C strings */
   sx_SetCentury( ( WORD ) iCentury );       /* dates to display century */
   sx_SetDateFormat( ( WORD ) iDateFormat ); /* date format DD/MM/CCYY */
   sx_SetDeleted( ( WORD ) iDeleted );       /* filter deleted records */

   /*
      Both dBase and Clipper UPPER() and LOWER() case conversion functions
      limit the characters eligible for case conversion. With UPPER(), only
      characters a-z are converted to upper case. With LOWER(), only
      characters A-Z are converted. Characters with diacritical marks ARE NOT
      CONVERTED when this switch is HB_TRUE if sx_SetTranslate is also set to HB_TRUE.
      To limit case conversion using this switch, set sx_SetTranslate to HB_TRUE
      and set the sx_SysProp value on as well.

      sx_SetTranslate( HB_TRUE );
      sx_SysProp( SDE_SP_SETLIMITCASECONV, (VOIDP)1 );
    */

   /* required database to be opened */
   #if 0
   sx_SetTranslate( HB_TRUE );
   sx_SysProp( SDE_SP_SETLIMITCASECONV, (PVOID)1 );
   #endif
   /* Init PHB_ITEM To Hold Opened DBF */
   if( Opened_DBF_Property == NULL )
   {
      Opened_DBF_Property = hb_itemNew( NULL );
      hb_arrayNew( Opened_DBF_Property, 0 );
   }
}

static void hb_sixapiRddExit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   sx_CloseAll();
   sx_FinalizeSession();

   if( Opened_DBF_Property )
   {
      hb_itemRelease( Opened_DBF_Property );
      Opened_DBF_Property = NULL;
   }
}

HB_CALL_ON_STARTUP_BEGIN( _hb_sixapi_rdd_init_ )
   hb_vmAtInit( hb_sixapiRddInit, NULL );
   hb_vmAtExit( hb_sixapiRddExit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sixapi_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
#  pragma startup _hb_sixapi_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_sixapi_rdd_init_ )
   #include "hbiniseg.h"
#endif
