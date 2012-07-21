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

/* Modified To return .T. if Century is set ON or .F. is Century is set OFF. */
static HB_BOOL       s_bCenturyIsOn  = HB_FALSE;
static char *        s_aFormat[]     =
{
   "AMERICAN",   "ANSI",       "BRITISH",    "FRENCH",     "GERMAN",     "ITALIAN",  "SPANISH",
   "MM/DD/YY",   "YY.MM.DD",   "DD/MM/YY",   "DD/MM/YY",   "DD.MM.YY",   "DD-MM-YY",
   "DD-MM-YY",   "MM/DD/YYYY", "YYYY.MM.DD", "DD/MM/YYYY", "DD/MM/YYYY",
   "DD.MM.YYYY", "DD-MM-YYYY", "DD-MM-YYYY"
};
static HB_BOOL       s_lDeletedIsOn  = HB_FALSE;
static int           s_iBaseYear     = 0;
static HB_BOOL       s_bSetExactIsOn = HB_FALSE;
static HB_BOOL       s_lSoftSeekIsOn;
static const char *  s_sxApiRDD[]    =
{
   "SDENTX", "SDEFOX", "SDENSX", "SDENSXDBT", "DBFNTX", "DBFIDX", "DBFNSX",
   "DBFNSXDBT"
};

/*
   C Declaration
   VOID FAR PASCAL sx_SetSoftSeek
     (HB_USHORT uiOnOff);

   Description
   Indicates whether or not index seeks that result in failure (i.e., the
   requested key value does not match any key in the index order either
   partially or exactly) should result in a successful conclusion if a key
   is found that is immediately greater than the requested key.

   Parameters
   uiOnOff: If True, then soft seeks are performed for all files in the current
   task. The soft seek setting is global in this respect.
   sx_SetSoftSeek is normally only turned on when necessary, and then turned off
   immediately after performing sx_Seek. See sx_Seek for details as to
   its behavior when sx_SetSoftSeek is set to True.
 */

HB_FUNC( SX_SETSOFTSEEK )
{
   PHB_ITEM pItem;
   WORD     wSoftSeek = 0;

   s_lSoftSeekIsOn = hb_setGetSoftSeek();

   hb_retl( s_lSoftSeekIsOn );

   if( HB_ISLOG( 1 ) )
      s_lSoftSeekIsOn = hb_parl( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      s_lSoftSeekIsOn = ( strcmp( _sx_upper( szTmp ), "ON" ) == 0 ) ? HB_TRUE : HB_FALSE;

      if( s_lSoftSeekIsOn )
         wSoftSeek = 1;

      hb_xfree( szTmp );
   }

   pItem = hb_itemPutL( NULL, s_lSoftSeekIsOn );
   hb_setSetItem( HB_SET_SOFTSEEK, pItem );
   hb_itemRelease( pItem );

   sx_SetSoftSeek( wSoftSeek );
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetExact (HB_USHORT uiOnOff);

   Description
   Indicates whether or not Seeks are to return True if a partial key match is
   made.

   Parameters
   uiOnOff: If True, key searches made with sx_Seek must match exactly in content
   and length. Partial key matches will result in False returns from sx_Seek.

   If False, the exact condition is turned off.

   The sx_SetExact setting is applied to all index seeks in the current task.
   It is global to the current task.

   NOTE: If sx_SetSoftSeek is set to True, the sx_SetExact setting is disabled.
 */

HB_FUNC( SX_SETEXACT )
{
   PHB_ITEM pItem;
   WORD     wSetExact = 0;

   s_bSetExactIsOn = hb_setGetExact();

   hb_retl( s_bSetExactIsOn );

   if( HB_ISLOG( 1 ) )
      s_bSetExactIsOn = hb_parl( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      s_bSetExactIsOn = ( strcmp( _sx_upper( szTmp ), "ON" ) == 0 ) ? HB_TRUE : HB_FALSE;

      if( s_bSetExactIsOn )
         wSetExact = 1;

      hb_xfree( szTmp );
   }

   sx_SetExact( wSetExact );

   pItem = hb_itemPutL( NULL, s_bSetExactIsOn );
   hb_setSetItem( HB_SET_EXACT, pItem );

   hb_itemRelease( pItem );
}

/*
   C Declaration

   HB_USHORT FAR PASCAL sx_SetEpoch (HB_USHORT uiBaseYear);

   Description
   Determines the interpretation of date strings with only two year digits. When
   such a string is converted to a date value, its year digits are compared with
   the year digits of iBaseYear. If the year digits in the date are greater than
   or equal to the year digits of iBaseYear, the date is assumed to fall within
   the same century as iBaseYear. Otherwise, the date is assumed to fall in the
   following century.

   Parameters
   iBaseYear specifies the base year of a 100-year period in which all dates
   containing only two year digits are assumed to fall.

   The default epoch value is 1900, causing dates with no century digits to be
   interpreted as falling within the twentieth century.
 */

HB_FUNC( SX_SETEPOCH )
{
   WORD     iEpoch;
   PHB_ITEM pItem;

   s_iBaseYear = hb_setGetEpoch();

   hb_retni( s_iBaseYear );

   if( HB_ISNUM( 1 ) )
   {
      iEpoch = ( WORD ) hb_parni( 1 );
      if( iEpoch == 0 )
         hb_errRT_BASE( EG_ARG, 2020, NULL, "SX_SETEPOCH", 1, hb_paramError( 1 ) );

      s_iBaseYear = iEpoch;

      pItem = hb_itemPutNI( NULL, iEpoch );
      hb_setSetItem( HB_SET_EPOCH, pItem );
      hb_itemRelease( pItem );

      sx_SetEpoch( iEpoch );
   }
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetDeleted(HB_USHORT uiDeleted);

   Description
   Makes deleted records either transparent or visible to record positioning
   functions.

   Setting sx_SetDeleted to HB_TRUE incurs certain performance penalties. Instead
   of using sx_SetDeleted HB_TRUE, consider creating conditional index files with a
   condition of .not. deleted.

   Parameters
   If True, deleted records will be invisible to all record positioning functions
   except sx_Go .
 */

HB_FUNC( SX_SETDELETED )
{
   PHB_ITEM pItem;
   WORD     wDeleted = 0;

   s_lDeletedIsOn = hb_setGetDeleted();

   hb_retl( s_lDeletedIsOn );

   if( HB_ISLOG( 1 ) )
      s_lDeletedIsOn = hb_parl( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      s_lDeletedIsOn = ( strcmp( _sx_upper( szTmp ), "ON" ) == 0 ) ? HB_TRUE : HB_FALSE;

      if( s_lDeletedIsOn )
         wDeleted = 1;

      hb_xfree( szTmp );
   }

   pItem = hb_itemPutL( NULL, s_lDeletedIsOn );
   hb_setSetItem( HB_SET_DELETED, pItem );
   hb_itemRelease( pItem );

   sx_SetDeleted( wDeleted );
}

static char * set_string( char * cDateFormat )
{
   char *   szString;
   HB_SIZE  ulLen = strlen( cDateFormat );

   if( ulLen > USHRT_MAX )
      ulLen = USHRT_MAX;

   szString            = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( szString, cDateFormat, ulLen );
   szString[ ulLen ]   = '\0';

   return szString;
}

static int _sx_CheckFormat( char * cFormat )
{
   int i;

   for( i = 0; i < 20; i++ )
   {
      if( strcmp( s_aFormat[ i ], cFormat ) == 0 )
         return i;
   }

   return -1;
}

static void __hb_setDateFormat( char * pDateFormat )
{
   HB_BOOL           flag    = HB_FALSE;
   unsigned int   i;
   int            ch,
                  year    = 0;
   char *         szDateF = set_string( pDateFormat );

   for( i = 0; i < strlen( szDateF ); i++ )
   {
      ch = szDateF[ i ];
      if( ! flag && ( ch == 'Y' || ch == 'y' ) )
         year++;        /* Only count the first set of consecutive "Y"s. */
      else if( year )
         flag = HB_TRUE;   /* Indicate non-consecutive. */
   }

   flag = ( year >= 4 );

   if( flag != hb_setGetCentury() )
   {
      hb_setSetCentury( flag );
   }

   hb_xfree( szDateF );
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetDateFormat (HB_USHORT uiDateType);

   Description
   Defines the format of date strings returned by sx_GetDateString.

   Parameters
   Any one of the following defined constanant values:

   AMERICAN 0 "MM/DD/YY"
   ANSI     1 "YY.MM.DD"
   BRITISH  2 "DD/MM/YY"
   FRENCH   3 "DD/MM/YY"
   GERMAN   4 "DD.MM.YY"
   ITALIAN  5 "DD-MM-YY"
   SPANISH  6 "DD-MM-YY"

   If sx_SetCentury is True, the century digits will precede the year digits
   in each of the formats shown above.
 */

HB_FUNC( SX_SETDATEFORMAT )
{
   WORD     iChecked;
   WORD     iDateFormat;
   char *   szDateFormat;

   hb_retc( hb_setGetDateFormat() );

   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );
      iChecked = ( WORD ) _sx_CheckFormat( _sx_upper( szTmp ) );
      hb_xfree( szTmp );

      if( iChecked == 0 )
         hb_errRT_BASE( EG_ARG, 2020, NULL, "SX_SETDATEFORMAT", 1,
                        hb_paramError( 1 ) );

      if( iChecked < 7 )
         iDateFormat = iChecked;
      else if( ( iChecked > 6 ) && ( iChecked < 14 ) )
      {
         iDateFormat = ( WORD ) ( iChecked - 7 );
         sx_SetCentury( HB_FALSE );
      }
      else
      {
         iDateFormat = ( WORD ) ( iChecked - 14 );
         sx_SetCentury( HB_TRUE );
      }
   }
   else
      return;

   /* hb_errRT_BASE( EG_ARG, 2020, NULL, "SX_SETDATEFORMAT", 1, hb_paramError( 1 ) ); */
   sx_SetDateFormat( iDateFormat );

   if( iChecked < 7 )
   {
      if( _sx_SetCentury() )
         szDateFormat = s_aFormat[ iChecked + 14 ];
      else
         szDateFormat = s_aFormat[ iChecked + 7 ];
   }
   else
      szDateFormat = s_aFormat[ iChecked ];

   __hb_setDateFormat( szDateFormat );
}

HB_BOOL _sx_SetCentury()
{
   return s_bCenturyIsOn;
}

/*
   C Declaration
   VOID FAR PASCAL sx_SetCentury (SHORT iValue);

   Description
   Indicates whether or not the two digits of the year designating century are
   to be returned by sx_GetDateString as part of a date string formatted
   according to the sx_SetDateFormat setting.

   Parameters
   If True, the century digits will be returned. If False, they will not.
 */

HB_FUNC( SX_SETCENTURY )
{
   WORD wCentury = 0;

   hb_retl( s_bCenturyIsOn );

   if( HB_ISLOG( 1 ) )
      s_bCenturyIsOn = hb_parl( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      s_bCenturyIsOn = ( strcmp( _sx_upper( szTmp ), "ON" ) == 0 ) ? HB_TRUE : HB_FALSE;

      if( s_bCenturyIsOn )
         wCentury = 1;

      hb_xfree( szTmp );
   }

   sx_SetCentury( wCentury );
}

HB_FUNC( SX_SETAUTOOPEN )
{
   HB_BOOL lAuto = ! _sx_SysProp( SDE_SP_GETDISABLEAUTO, NULL );

   if( HB_ISLOG( 1 ) )
      _sx_SysProp( SDE_SP_SETDISABLEAUTO, ( PVOID ) ( ! hb_parl( 1 ) ) );
   hb_retl( lAuto );
}

HB_FUNC( SX_ISAUTOOPENDISABLED )
{
   hb_retl( sx_SysProp( SDE_SP_GETDISABLEAUTO, ( PVOID ) NULL ) );
}

HB_FUNC( SX_DISABLEAUTOOPEN )
{
   hb_retl( _sx_SysProp( SDE_SP_SETDISABLEAUTO, ( PVOID ) 1 ) );
}

HB_FUNC( SX_SETCHRCOLLATE )
{
   PHB_ITEM vParam = hb_param( 1, HB_IT_ARRAY );

   if( vParam )
   {
      const char * szChrCollate[ 255 ];
      HB_SIZE ulLen = hb_arrayLen( vParam );
      HB_SIZE u;

      for( u = 0; u < ulLen ; u++ )
         szChrCollate[ u ] = hb_arrayGetCPtr( vParam, u + 1 );

      hb_retl( sx_SysProp( SDE_SP_SETCHRCOLLATE, ( PVOID ) szChrCollate ) );
   }
   else
      hb_retl( HB_FALSE );
}

/*
   VB Declaration
   Declare Function sx_SetHandles Lib "SDE7.DLL"
   (ByVal iNumHandles As Integer)

   As Integer

   Description
   Change the number of open file handles in the current Windows task. 255 max.

   Parameters
   iNumHandles: The new number of file handles allowed for the current Windows
   task. The maximum number is 255.

   Return Value
   The total number of handles available. This number should always be 255
   (it is sent by the low level Windows API function when the handles are set).
   If it is not 255, an error has occurred.
 */

HB_FUNC( SX_SETHANDLES )
{
   hb_retni( sx_SetHandles( ( WORD ) hb_parni( 1 ) /* iNumHandles */ ) );
}

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

HB_FUNC( SX_SETLIMITCASECONV )
{
   if( HB_ISLOG( 1 ) )
   {
      if( hb_parl( 1 ) )
         sx_SysProp( SDE_SP_SETLIMITCASECONV, ( PVOID ) 1 );
      else
         sx_SysProp( SDE_SP_SETLIMITCASECONV, ( PVOID ) 0 );
   }
}

/*
   VB Declaration
   Declare Sub sx_SetLockTimeout Lib "SDE7.DLL"
   (ByVal iSeconds As Integer)

   Description
   Sets the number of seconds allowed to retry a lock operation before failing.

   Parameters
   iSeconds: The lock operation will be continuously retried for this number of
   seconds before reporting failure. The default value is 1 second.
 */

HB_FUNC( SX_SETLOCKTIMEOUT )
{
   sx_SetLockTimeout( ( WORD ) hb_parni( 1 ) /* iSeconds */ );
}

/*
   VB Declaration

   Declare Sub sx_SetMemoBlockSize Lib "SDE7.DLL"
   (ByVal uiBlockSize As Integer)

   Description
   NOTE: This function does not apply to CA-Clipper .DBT memo files, which use
   fixed 512 byte blocks.
   The default .FPT memo block size is 32 bytes. The default .SMT memo block size
   is 1. This function sets a new default block size that will be used when creating
   any new table that has memos and will also change the block size in memo files
   when the DBF is packed. It does not affect existing memo files except when the
   corresponding DBF is packed.

   Parameters
   uiBlockSize: The new default block size. The size must be a value from
   1 through 1024.
 */

HB_FUNC( SX_SETMEMOBLOCKSIZE )
{
   /*
      The new default block size.
      The size must be a value from 1 through 1024.
    */
   hb_retni( sx_SetMemoBlockSize( ( WORD ) hb_parni( 1 ) ) );
}

HB_FUNC( SX_SETOEMCOLLATE )
{
   PHB_ITEM vParam = hb_param( 1, HB_IT_ARRAY );

   if( vParam )
   {
      const char * sxOEMCollate[ 255 ];
      HB_SIZE ulLen = hb_arrayLen( vParam );
      HB_SIZE u;

      for( u = 0; u < ulLen ; u++ )
         sxOEMCollate[ u ] = hb_arrayGetCPtr( vParam, u + 1 );

      hb_retl( sx_SysProp( SDE_SP_SETOEMCOLLATE, ( PVOID ) sxOEMCollate ) );
   }
   else
      hb_retl( HB_FALSE );
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetStringType (HB_USHORT uiStringType);

   Description
   Indicate whether strings returned should be formatted as Visual Basic variable
   length strings or "C" type binary zero delimited strings.

   The default value is to return Visual Basic strings.
   C programs must always set the string type in their initialization routine.

   Parameters
   uiStringType: Set uiStringType to 1 if C strings are to be returned.
   A setting of 0 returns Visual Basic variable length strings.
 */

HB_FUNC( SX_SETSTRINGTYPE )
{
   sx_SetStringType( ( WORD ) hb_parni( 1 ) /* uiStringType */ );
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetTranslate (HB_USHORT uiOnOff);

   Description
   Automatically translates record buffers stored in the OEM character set to
   Windows ANSI.

   European tables make heavy use of the upper end of the character set (values
   128 to 255) to display characters with diacritical marks. If the table was
   created with a DOS program, these characters are not the same as their ANSI
   counterparts. The result is so much gibberish being displayed on the Windows
   screen.

   OEM tables will automatically be translated to ANSI if sx_SetTranslate is set
   to True. The table records and indexes will, however, be maintained in the OEM
   character set if sx_SetTranslate remains True. This makes it possible to use
   an OEM table created under DOS with both DOS and Windows programs
   simultaneously.

   Parameters
   uiOnOff: If True, the record buffer is translated from the OEM character set
   to ANSI after being read. If still True at run time, it is translated back
   again before writing.

   As long as the setting is True, all key changes to indexes are translated to
   the OEM set before insertion. Search keys are also translated into OEM before
   the search is undertaken.

   The setting applies to the current table only.

   Permanent Translation
   To physically translate an OEM table to ANSI (if it will no longer be used in
   a DOS environment), use sx_GetRecord and sx_PutRecord in a read loop and toggle
   sx_SetTranslate on and off (True before the read, and False before the write).
   Ensure that the setting is False before closing as well. See the examples below.

   Special Case when using DESCEND()

   If you need to support legacy DOS application and also use the xBase DESCEND()
   function in the index key, you need to take special care to manage the translate
   state. For example, when calling sx_Seek, sx_Index, or sx_IndexTag you should
   first call sx_SetTranslate (HB_FALSE) and afterwards call sx_SetTranslate (HB_TRUE).
 */

HB_FUNC( SX_SETTRANSLATE )
{
   WORD wSetTrans = hb_parl( 1 ) ? ( WORD ) 1 : ( WORD ) 0;  /* uiOnOff */

   sx_SetTranslate( wSetTrans );
}

/*
   C Declaration

   VOID FAR PASCAL sx_SetTurboRead (HB_USHORT uiOnOrOff);

   Description
   sx_SetTurboRead is used to speed up record i/o functions (specifically sx_Skip
   and sx_Seek). If you are going to be going into a lengthy record read or seek
   loop, lock the file and then sx_SetTurboRead True. The default value is False.

   Parameters
   See description above.
 */

HB_FUNC( SX_SETTURBOREAD )
{
   WORD wSetTurbo = hb_parl( 1 ) ? ( WORD ) 1 : ( WORD ) 0;  /* uiOnOff */

   sx_SetTurboRead( wSetTurbo );
}

HB_FUNC( SX_SETUDFPATH )
{
   char * pbPath = ( char * ) hb_xgrab( 256 );

   sx_GetUDFPath( ( PBYTE ) pbPath, 255 );
   hb_retc_buffer( ( char * ) pbPath );

   if( HB_ISCHAR( 1 ) )
      sx_SetUDFPath( ( PBYTE ) hb_parc( 1 ) );
}

HB_FUNC( SX_GETCHRCOLLATE )
{
   hb_retc( ( char * ) sx_SysProp( SDE_SP_GETCHRCOLLATE, ( PVOID ) NULL ) );
}

HB_FUNC( SX_CLOSEALL )
{
   sx_CloseAll();

   if( Opened_DBF_Property )
      hb_arraySize( Opened_DBF_Property, 0 );
}

HB_FUNC( SX_GETDATEFORMAT )
{
   char *   aDate[] =
   {
      "AMERICAN", "ANSI", "BRITISH", "FRENCH", "GERMAN", "ITALIAN", "SPANISH"
   };
   HB_BOOL     bAskString = HB_FALSE;

   if( HB_ISNIL( 1 ) )
      hb_retni( sx_GetDateFormat() );
   else
   {
      if( HB_ISLOG( 1 ) )
         bAskString = hb_parl( 1 );

      if( bAskString )
      {
         int iFormat = sx_GetDateFormat();
         hb_retc( aDate[ iFormat ] );
      }
      else
         hb_retni( sx_GetDateFormat() );
   }
}

HB_FUNC( SX_GETOEMCOLLATE )
{
   hb_retc( ( char * ) sx_SysProp( SDE_SP_GETOEMCOLLATE, ( PVOID ) NULL ) );
}

HB_FUNC( SX_GETQUERYBIT )
{
   hb_retl( sx_GetQueryBit( hb_parnl( 1 ) /* lRecNum */ ) );
}

HB_FUNC( SX_GETSYSTEMCHARORDER )
{
   hb_retc( sx_GetSystemCharOrder() );
}

HB_FUNC( SX_GETSYSTEMLOCALE )
{
   hb_retc( sx_GetSystemLocale() );
}

HB_FUNC( SX_GETUDFPATH )
{
   char pbPath[ 256 ];

   sx_GetUDFPath( ( PBYTE ) pbPath, 255 );
   hb_retc( ( char * ) pbPath );
   hb_xfree( pbPath );
}

HB_FUNC( SX_SETGETTRIMMED )
{
   if( HB_ISLOG( 1 ) )
      bSetTrimmedON = hb_parl( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
      char *   szTmp   = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      bSetTrimmedON = ( strcmp( _sx_upper( szTmp ), "ON" ) == 0 ) ? HB_TRUE : HB_FALSE;

      hb_xfree( szTmp );
   }

   hb_retl( bSetTrimmedON );
}

HB_FUNC( SX_RDDSETDEFAULT )
{
   int ui;

   hb_retc( ( char * ) s_sxApiRDD[ i_sxApi_RDD_Default - 1 ] );

   if( HB_ISCHAR( 1 ) )
   {
      ui = _sx_CheckRDD( hb_parc( 1 ) );
      if( ( ui > 0 ) && ( ui < 5 ) )
         i_sxApi_RDD_Default = ui;
      else
         hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

int _sx_CheckRDD( const char * sSetDefault )
{
   int ui;

   if( HB_ISCHAR( 1 ) )
   {
      HB_BOOL bCorrect = HB_FALSE;

      #if 0
      HB_ISIZ iLen = strlen( sSetDefault ) + 1;
      char * szTmp = ( char * ) hb_xgrab( iLen );
      hb_snprintf( szTmp, iLen, "%s", sSetDefault );
      szTmp = _sx_upper( szTmp );
      #endif
      for( ui = 0; ui < 8; ui++ )
      {
         if( strcmp( s_sxApiRDD[ ui ], sSetDefault /* szTmp */ ) == 0 )
         {
            bCorrect = HB_TRUE;
            break;
         }
      }

      #if 0
      hb_xfree( szTmp );
      #endif

      if( bCorrect )
      {
         if( ui > 3 )
            ui -= 4;
         return ui + 1;
      }
      else
         return -1;
   }
   else
      return -1;
}

HB_FUNC( SX_ERRORLEVEL )
{
   /*
      ERRLEVEL_NONE  0  No error message at all.
      ERRLEVEL_FATAL  1  Only report Fatal errors (Default).
      ERRLEVEL_STANDARD 2  Report all errors.
    */
   if( HB_ISNUM( 1 ) )
   {
      WORD iErrorLevel = ( WORD ) hb_parni( 1 );

      if( iErrorLevel > 2 )
         iErrorLevel = 2;

      i_sxApi_Error_Level = iErrorLevel;

      sx_ErrorLevel( iErrorLevel );
   }

   hb_retni( i_sxApi_Error_Level );   /* Default ErrorLevel */
}
