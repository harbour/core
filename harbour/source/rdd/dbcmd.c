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

#include <extend.h>
#include <init.h>
#include <errorapi.h>
#include <set.h>

/* Change to rdd.api */
typedef struct _AREA
{
   USHORT  uArea;             // The number assigned to this workarea
} AREA, * PAREA;

HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_RDDSETDEFAULT( void );

HB_INIT_SYMBOLS_BEGIN( dbCmd__InitSymbols )
{ "DBSELECTAREA",  FS_PUBLIC, HB_DBSELECTAREA,  0 },
{ "DBSETDRIVER",   FS_PUBLIC, HB_DBSETDRIVER,   0 },
{ "RDDSETDEFAULT", FS_PUBLIC, HB_RDDSETDEFAULT, 0 }
HB_INIT_SYMBOLS_END( dbCmd__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbCmd__InitSymbols
#endif

#define WORKAREAS 250

/* From strings.c */
char * hb_strUpper( char * szText, long lLen );

static char * szDefDriver = 0; /* Default RDD name */
static USHORT uCurrArea = 1;  /* Selectd area */

AREA hb_WorkAreas[ WORKAREAS - 1 ];

void hb_rddInitialize( void )
{
   memset( hb_WorkAreas, 0, sizeof( AREA ) );
}

void hb_rddRelease( void )
{
   if( szDefDriver )
      hb_xfree( szDefDriver );
}

BOOL hb_IsRDD( char * szRDDName )
{
   return 1; /* Not implemented yet */
}

static USHORT hb_FindAlias( char * szAlias )
{
   return 1; /* Not implemented yet */
}

static void hb_SelectFirstAvailable( void )
{
   USHORT uCount;

   for( uCount = 0; uCount < WORKAREAS; uCount++ )
   {
      if( hb_WorkAreas[ uCount ].uArea == 0 )
      {
         uCurrArea = uCount + 1;
         return;
      }
   }
}


static void hb_dbSelectArea( USHORT uNewArea )
{
   if( uNewArea == 0)
      hb_SelectFirstAvailable();
   else if( uNewArea <= WORKAREAS )
      uCurrArea = uNewArea;
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uNewArea;
   char * szAlias;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      if( ( uNewArea = hb_FindAlias( szAlias ) ) == 0 )
      {
         hb_errorRT_BASE( EG_ARG, 1002, "Alias not found", szAlias );
         return;
      }
      hb_dbSelectArea( uNewArea );
   }
   else if( ISNUM( 1 ) )
      hb_dbSelectArea( hb_parni( 1 ) );
}

HARBOUR HB_DBSETDRIVER( void )
{
   HB_RDDSETDEFAULT();
}

HARBOUR HB_RDDSETDEFAULT( void )
{
   char * szNewDriver;
   WORD wLen;

   hb_retc( szDefDriver );
   if( ISCHAR( 1 ) )
   {
      szNewDriver = hb_parc( 1 );
      if( hb_IsRDD( szNewDriver ) )
      {
	 if( szDefDriver )
	    hb_xfree( szDefDriver );
	 wLen = strlen( szNewDriver );
	 szDefDriver = ( char * ) hb_xgrab( wLen + 1 );
	 strcpy( szDefDriver, hb_strUpper( szNewDriver, wLen ) );
      }
      else hb_errorRT_DBCMD( EG_ARG, 1015, "Argument error", "RDDSETDEFAULT" );
   }
}
