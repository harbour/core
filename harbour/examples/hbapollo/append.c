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

static int        _checkDelim( int _nDelim );
static int        _checkDelimChar( const char * _cDelim );
static const char *  aDelim[] =
{
   "SDENTX",    "SDEFOX", "SDENSX", "SDENSX_DBT", "COMMA_DELIM", "SDF_FILE",
   "TAB_DELIM", "OEMNTX", "OEMFOX", "OEMNSX"
};
static int           nDelim[] = { 1, 2, 3, 4, 21, 22, 23, 31, 32, 33 };

/* 2003.05.08 added parameter for work area to work on */
HB_FUNC( SX_APPEND )
{
   WORD iPreviousArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_APPEND" );

   if( ! HB_ISNIL( 1 ) )
      iPreviousArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   if( HB_ISNUM( 2 ) )  /* How Many Append Blank */
   {
      int   nHowMany = hb_parni( 2 );
      int   ui;

      if( nHowMany > 0 )
      {
         for( ui = 0; ui < nHowMany; ui++ )
            sx_Append();
      }
   }
   else
      sx_Append();

   if( ! ( iPreviousArea == SX_DUMMY_NUMBER ) )
      sx_Select( iPreviousArea );
}

HB_FUNC( SX_APPENDEX )
{
   WORD  iPreviousArea = SX_DUMMY_NUMBER;
   int   iRetVal       = -1;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_APPENDEX" );

   if( ! HB_ISNIL( 1 ) )
      iPreviousArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   if( HB_ISNUM( 2 ) )  /* How Many Append Blank */
   {
      int   nHowMany = hb_parni( 2 );
      int   ui;

      if( nHowMany > 0 )
      {
         for( ui = 0; ui < nHowMany; ui++ )
            iRetVal = sx_AppendEx();
      }
   }
   else
      iRetVal = sx_AppendEx();

   if( iRetVal == -1 )
      iRetVal = 0;

   hb_retni( iRetVal );

   if( ! ( iPreviousArea == SX_DUMMY_NUMBER ) )
      sx_Select( iPreviousArea );
}

HB_FUNC( SX_APPENDBLANK )
{
   WORD iPreviousArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_APPENDBLANK" );

   if( ! HB_ISNIL( 1 ) )
      iPreviousArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   if( HB_ISNUM( 2 ) )  /* How Many Append Blank */
   {
      int   nHowMany = hb_parni( 2 );
      int   ui;

      if( nHowMany > 0 )
      {
         for( ui = 0; ui < nHowMany; ui++ )
            sx_AppendBlank();
      }
   }
   else
      sx_AppendBlank();

   if( ! ( iPreviousArea == SX_DUMMY_NUMBER ) )
      sx_Select( iPreviousArea );
}

HB_FUNC( SX_APPENDBLANKEX )
{
   WORD  iPreviousArea = SX_DUMMY_NUMBER;
   int   iRetVal       = -1;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_APPENDBLANKEX" );

   if( ! HB_ISNIL( 1 ) )
      iPreviousArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   if( HB_ISNUM( 2 ) )  /* How Many Append Blank */
   {
      int   nHowMany = hb_parni( 2 );
      int   ui;

      if( nHowMany > 0 )
      {
         for( ui = 0; ui < nHowMany; ui++ )
            iRetVal = sx_AppendBlankEx();
      }
   }
   else
      iRetVal = sx_AppendBlankEx();

   if( iRetVal == -1 )
      iRetVal = 0;

   hb_retni( iRetVal );

   if( ! ( iPreviousArea == SX_DUMMY_NUMBER ) )
      sx_Select( iPreviousArea );
}

HB_FUNC( SX_APPENDFROM )
{
   PBYTE cSourceFile;
   PBYTE cForScope;
   int   iRDEType;
   WORD  iPreviousArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_APPENDFROM" );

   if( ! HB_ISNIL( 4 ) )
      iPreviousArea = _sx_select( hb_param( 4, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
   {
      cSourceFile = ( PBYTE ) hb_parc( 1 );
      if( strlen( ( char * ) cSourceFile ) == 0 )
      {
         hb_retl( HB_FALSE );
         return;
      }

      if( HB_ISNUM( 2 ) )
      {
         iRDEType = _checkDelim( hb_parni( 2 ) );
         if( iRDEType == -1 )
         {
            hb_retl( HB_FALSE );
            return;
         }
      }
      else if( HB_ISCHAR( 2 ) )
      {
         iRDEType = _checkDelimChar( hb_parc( 2 ) );
         if( iRDEType == -1 )
         {
            hb_retl( HB_FALSE );
            return;
         }
      }
      else
         iRDEType = i_sxApi_RDD_Default;  /* Default RDD Driver */

      if( HB_ISCHAR( 3 ) )
         cForScope = ( PBYTE ) hb_parc( 3 );
      else
         cForScope = ( PBYTE ) 0;

      hb_retl( sx_AppendFrom( cSourceFile, ( WORD ) iRDEType, cForScope ) );

      if( ! ( iPreviousArea == SX_DUMMY_NUMBER ) )
         sx_Select( iPreviousArea );
   }
}

static int _checkDelim( int _nDelim )
{
   HB_USHORT ui;

   for( ui = 0; ui < 10; ui++ )
   {
      if( _nDelim == nDelim[ ui ] )
         return nDelim[ ui ];
   }

   return -1;
}

static int _checkDelimChar( const char * _cDelim )
{
   int      ui;
   int      iResult = -1;
   HB_ISIZ  iLen    = strlen( _cDelim ) + 1;
   char *   szTmp   = ( char * ) hb_xgrab( iLen );

   hb_snprintf( szTmp, iLen, "%s", _cDelim );
   szTmp = _sx_upper( szTmp );

   for( ui = 0; ui < 10; ui++ )
   {
      if( strcmp( szTmp, aDelim[ ui ] ) == 0 )
      {
         iResult = nDelim[ ui ];
         break;
      }
   }

   hb_xfree( szTmp );
   return iResult;
}
