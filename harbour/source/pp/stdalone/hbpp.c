/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor standalone main module
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined(HB_TRACE_UTILS)
   #if defined(HB_TRACE_LEVEL)
      #undef HB_TRACE_LEVEL
   #endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "hbpp.h"
#include "hberrors.h"
#include "hbver.h"

static void AddSearchPath( char * szPath, PATHNAMES * * pSearchList );
static void OutTable( DEFINES * endDefine, COMMANDS * endCommand );

static int  s_iline = 0;
static char s_szLine[ HB_PP_STR_SIZE ];
static char s_szOutLine[ HB_PP_STR_SIZE ];
static int  s_iWarnings = 0;

PATHNAMES * hb_comp_pIncludePath = NULL;
PHB_FNAME hb_comp_pFileName = NULL;

int main( int argc, char * argv[] )
{
  FILE * handl_i;
  FILE * handl_o;
  char szFileName[ _POSIX_PATH_MAX ];
  char * szDefText;
  int iArg = 1;
  unsigned int i;
  BOOL bOutTable = FALSE;
  BOOL bOutNew = FALSE;
  DEFINES * stdef = hb_pp_topDefine;
  COMMANDS * stcmd = hb_pp_topCommand;

  HB_TRACE(HB_TR_DEBUG, ("main(%d, %p)", argc, argv));

  printf( "Harbour Preprocessor, Build %i%s (%04d.%02d.%02d)\n",
          hb_build, hb_revision, hb_year, hb_month, hb_day );
  printf( "Copyright 1999, http://www.harbour-project.org\n" );

  while( iArg < argc )
    {
      if( HB_ISOPTSEP(argv[ iArg ][ 0 ]))
        {
          switch( argv[ iArg ][ 1 ] )
            {
            case 'd':
            case 'D':   /* defines a #define from the command line */
              {
                i = 0;
                szDefText = hb_strdup( argv[ iArg ] + 2 );
                while( i < strlen( szDefText ) && szDefText[ i ] != '=' )
                  i++;
                if( szDefText[ i ] != '=' )
                  hb_pp_AddDefine( szDefText, 0 );
                else
                  {
                    szDefText[ i ] = 0;
                    hb_pp_AddDefine( szDefText, szDefText + i + 1 );
                  }
                free( szDefText );
              }
              break;
            case 'i':
            case 'I':
              AddSearchPath( argv[ iArg ]+2, &hb_comp_pIncludePath );
              break;
            case 'o':
            case 'O':
              bOutTable = TRUE;
              break;
            case 'n':
            case 'N':
              bOutNew = TRUE;
              break;
            case 'w':
            case 'W':
              s_iWarnings = 1;
              if( argv[ iArg ][ 2 ] )
                {  /*there is -w<0,1,2,3> probably */
                  s_iWarnings = argv[ iArg ][ 2 ] - '0';
                  if( s_iWarnings < 0 || s_iWarnings > 3 )
                    printf( "\nInvalid command line option: %s\n", argv[ iArg ] );
                }
              break;
            default:
              printf( "\nInvalid command line option: %s\n", &argv[ iArg ][ 1 ] );
              break;
            }
        }
      else  hb_comp_pFileName = hb_fsFNameSplit( argv[ iArg ] );
      iArg++;
    }
  if( hb_comp_pFileName )
    {
      if( ! hb_comp_pFileName->szExtension )
        hb_comp_pFileName->szExtension =".prg";

      hb_fsFNameMerge( szFileName, hb_comp_pFileName );

      if( ( handl_i = fopen( szFileName, "r" ) ) == NULL )
        {
          printf("\nCan't open %s\n", szFileName );
          return 1;
        }

      printf( "\nParsing file %s\n", szFileName );
    }
  else
    {
      printf( "\nSyntax:  %s <file[.prg]> [options]"
              "\n"
              "\nOptions:  /d<id>[=<val>]   #define <id>"
              "\n          /i<path>         add #include file search path"
              "\n          /o               creates hbpp.out with all tables"
              "\n          /n               with those only, which defined in your file"
              "\n          /w               enable warnings"
              "\n"
              , argv[ 0 ] );

      if( bOutTable )
        OutTable( NULL, NULL );

      return 1;
    }

  hb_comp_pFileName->szExtension = ".ppo";
  hb_fsFNameMerge( szFileName, hb_comp_pFileName );

  if( ( handl_o = fopen( szFileName, "wt" ) ) == NULL )
    {
      printf("\nCan't open %s\n", szFileName );
      return 1;
    }

  {
    char * szInclude = getenv( "INCLUDE" );

    if( szInclude )
      {
        char * pPath;
        char * pDelim;

        pPath = szInclude = hb_strdup( szInclude );
        while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
          {
            *pDelim = '\0';
            AddSearchPath( pPath, &hb_comp_pIncludePath );
            pPath = pDelim + 1;
          }
        AddSearchPath( pPath, &hb_comp_pIncludePath );
      }
  }

  hb_pp_aCondCompile = ( int * ) hb_xgrab( sizeof( int ) * 5 );

  hb_pp_Parse( handl_i, handl_o, NULL );
  fclose( handl_i );
  fclose( handl_o );

  if( bOutTable )
    OutTable( NULL, NULL );
  else if( bOutNew )
    OutTable( stdef, stcmd );

  printf( "\nOk" );

  return 0;
}

int hb_pp_Parse( FILE * handl_i, FILE * handl_o, char * szSource )
{
  char sBuffer[ HB_PP_BUFF_SIZE ];           /* File read buffer */
  char * ptr;
  int lContinue = 0;
  int iBuffer = 10, lenBuffer = 10;
  int lens = 0, rdlen;

  HB_SYMBOL_UNUSED( szSource );

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_Parse(%p, %p, %s)", handl_i, handl_o, szSource));

  while( ( rdlen = hb_pp_RdStr( handl_i, s_szLine + lens, HB_PP_STR_SIZE - lens, lContinue,
                                sBuffer, &lenBuffer, &iBuffer ) ) >= 0 )
    {
      if( ! hb_pp_lInclude ) s_iline++;
      lens += rdlen;

      if( s_szLine[ lens - 1 ] == ';' )
        {
          lContinue = 1;
          lens--;
          lens--;
          while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' ) lens--;
          s_szLine[ ++lens ] = ' ';
          s_szLine[ ++lens ] = '\0';
        }
      else
        {
          lContinue = 0;
          lens = 0;
        }

      if( *s_szLine != '\0' && !lContinue )
        {
          printf( "\r  line %i", s_iline );
          ptr = s_szLine;
          HB_SKIPTABSPACES( ptr );
          if( *ptr == '#' )
            {
              if( hb_pp_ParseDirective( ptr + 1 ) == 0 )
                *s_szLine = '\0';
            }
          else
            {
              if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
                hb_pp_ParseExpression( ptr, s_szOutLine );
              else
                *s_szLine = '\0';
            }
        }

      if( ! hb_pp_lInclude )
        {
          if( lContinue ) hb_pp_WrStr( handl_o, "\n" );
          else hb_pp_WrStr( handl_o, s_szLine );
        }
    }

  return 0;
}

static void OutTable( DEFINES * endDefine, COMMANDS * endCommand )
{
  FILE *handl_o;
  int ipos, len_mpatt, len_value;
  int num;
  DEFINES * stdef1 = hb_pp_topDefine, * stdef2 = NULL, * stdef3;
  COMMANDS * stcmd1 = hb_pp_topCommand, * stcmd2 = NULL, * stcmd3;

  HB_TRACE(HB_TR_DEBUG, ("OutTable(%p, %p)", endDefine, endCommand));

  while( stdef1 != endDefine )
    {
      stdef3 = stdef1->last;
      stdef1->last = stdef2;
      stdef2 = stdef1;
      stdef1 = stdef3;
    }
  while( stcmd1 != endCommand )
    {
      stcmd3 = stcmd1->last;
      stcmd1->last = stcmd2;
      stcmd2 = stcmd1;
      stcmd1 = stcmd3;
    }

  if( ( handl_o = fopen( "hbpp.out", "wt" ) ) == NULL )
    {
      printf( "\nCan't open hbpp.out\n" );
      return;
    }

  num = 1;
  while( stdef2 != NULL )
    {
      fprintf( handl_o, "\n   static DEFINES sD___%i = ", num );
      fprintf( handl_o, "{\"%s\",", stdef2->name );
      if( stdef2->pars )
        fprintf( handl_o, "\"%s\",", stdef2->pars );
      else
        fprintf( handl_o, "NULL," );
      fprintf( handl_o, "%d,", stdef2->npars );
      if( stdef2->value )
        fprintf( handl_o, "\"%s\"", stdef2->value );
      else
        fprintf( handl_o, "NULL" );
      if( num == 1 )
        fprintf( handl_o, ", NULL };" );
      else
        fprintf( handl_o, ", &sD___%i };", num - 1 );
      stdef2 = stdef2->last;
      num++;
    }
  fprintf( handl_o, "\n   DEFINES * hb_pp_topDefine = " );
  if( num == 1 )
    fprintf( handl_o, "NULL;" );
  else
    fprintf( handl_o, " = &sD___%i;\n", num - 1 );

  num = 1;
  while( stcmd2 != NULL )
    {
      fprintf( handl_o, "\n   static COMMANDS sC___%i = ", num );
      fprintf( handl_o, "{%d,\"%s\",", stcmd2->com_or_xcom, stcmd2->name );
      if( stcmd2->mpatt != NULL )
        {
          len_mpatt = hb_pp_strocpy( s_szLine, stcmd2->mpatt );
          while( ( ipos = hb_strAt( "\1", 1, s_szLine, len_mpatt ) ) > 0 )
            {
              hb_pp_Stuff( "\\1", s_szLine + ipos - 1, 2, 1, len_mpatt );
              len_mpatt++;
            }
          fprintf( handl_o, "\"%s\",", s_szLine );
        }
      else
        fprintf( handl_o, "NULL," );
      if( stcmd2->value != NULL )
        {
          len_value = hb_pp_strocpy( s_szLine, stcmd2->value );
          while( ( ipos = hb_strAt( "\1", 1, s_szLine, len_value ) ) > 0 )
            {
              hb_pp_Stuff( "\\1", s_szLine + ipos - 1, 2, 1, len_value );
              len_value++;
            }
          if( len_mpatt + len_value > 80 )
            fprintf( handl_o, "\n       " );
          fprintf( handl_o, "\"%s\"", s_szLine );
        }
      else fprintf( handl_o, "NULL" );
      if( num == 1 )
        fprintf( handl_o, ",NULL };" );
      else
        fprintf( handl_o, ",&sC___%i };", num - 1 );
      stcmd2 = stcmd2->last;
      num++;
    }
  fprintf( handl_o, "\n   COMMANDS * hb_pp_topCommand = " );
  if( num == 1 )
    fprintf( handl_o, "NULL;" );
  else
    fprintf( handl_o, " = &sC___%i;\n", num - 1 );

  stcmd1 = hb_pp_topTranslate;
  stcmd2 = NULL;
  while( stcmd1 != NULL )
    {
      stcmd3 = stcmd1->last;
      stcmd1->last = stcmd2;
      stcmd2 = stcmd1;
      stcmd1 = stcmd3;
    }
  num = 1;
  while( stcmd2 != NULL )
    {
      fprintf( handl_o, "\n   static COMMANDS sC___%i = ", num );
      fprintf( handl_o, "{%d,\"%s\",", stcmd2->com_or_xcom, stcmd2->name );
      if( stcmd2->mpatt != NULL )
        {
          len_mpatt = hb_pp_strocpy( s_szLine, stcmd2->mpatt );
          while( ( ipos = hb_strAt( "\1", 1, s_szLine, len_mpatt ) ) > 0 )
            {
              hb_pp_Stuff( "\\1", s_szLine + ipos - 1, 2, 1, len_mpatt );
              len_mpatt++;
            }
          fprintf( handl_o, "\"%s\",", s_szLine );
        }
      else
        fprintf( handl_o, "NULL," );
      if( stcmd2->value != NULL )
        {
          len_value = hb_pp_strocpy( s_szLine, stcmd2->value );
          while( ( ipos = hb_strAt( "\1", 1, s_szLine, len_value ) ) > 0 )
            {
              hb_pp_Stuff( "\\1", s_szLine + ipos - 1, 2, 1, len_value );
              len_value++;
            }
          if( len_mpatt + len_value > 80 )
            fprintf( handl_o, "\n       " );
          fprintf( handl_o, "\"%s\"", s_szLine );
        }
      else fprintf( handl_o, "NULL" );
      if( num == 1 )
        fprintf( handl_o, ",NULL };" );
      else
        fprintf( handl_o, ",&sC___%i };", num - 1 );
      stcmd2 = stcmd2->last;
      num++;
    }
  fprintf( handl_o, "\n   COMMANDS * hb_pp_topTranslate = " );
  if( num == 1 )
    fprintf( handl_o, "NULL;" );
  else
    fprintf( handl_o, " = &sT___%i;", num );

  fclose( handl_o );
}

/*
 * Function that adds specified path to the list of pathnames to search
 */
static void AddSearchPath( char * szPath, PATHNAMES * * pSearchList )
{
  PATHNAMES * pPath = *pSearchList;

  HB_TRACE(HB_TR_DEBUG, ("AddSearchPath(%s, %p)", szPath, pSearchList));

  if( pPath )
    {
      while( pPath->pNext )
        pPath = pPath->pNext;
      pPath->pNext = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
      pPath = pPath->pNext;
    }
  else
    {
      *pSearchList = pPath = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
    }
  pPath->pNext  = NULL;
  pPath->szPath = szPath;
}

void hb_compGenError( char * _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_compGenError(%p, %c, %d, %s, %s)", _szErrors, cPrefix, iError, szError1, szError2));

  printf( "\r(%i) ", s_iline );
  printf( "Error %c%04i  ", cPrefix, iError );
  printf( _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "\n\n" );

  exit( EXIT_FAILURE );
}

void hb_compGenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_compGenWarning(%p, %c, %d, %s, %s)", _szWarnings, cPrefix, iWarning, szWarning1, szWarning2));

  if( s_iWarnings )
    {
      char *szText = _szWarnings[ iWarning - 1 ];

      if( (szText[ 0 ] - '0') <= s_iWarnings )
        {
          printf( "\r(%i) ", s_iline );
          printf( "Warning %c%04i  ", cPrefix, iWarning );
          printf( szText + 1, szWarning1, szWarning2 );
          printf( "\n" );
        }
    }
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
  void * pMem = malloc( ulSize );

  HB_TRACE(HB_TR_DEBUG, ("hb_xgrab(%lu)", ulSize));

  if( ! pMem )
    hb_compGenError( hb_pp_szErrors, 'P', ERR_PPMEMALLOC, NULL, NULL );

  return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
  void * pResult = realloc( pMem, ulSize );

  HB_TRACE(HB_TR_DEBUG, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

  if( ! pResult )
    hb_compGenError( hb_pp_szErrors, 'P', ERR_PPMEMREALLOC, NULL, NULL );

  return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
  HB_TRACE(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

  if( pMem )
    free( pMem );
  else
    hb_compGenError( hb_pp_szErrors, 'P', ERR_PPMEMFREE, NULL, NULL );
}
