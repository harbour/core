/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source with real code generation
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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


#include "hbcomp.h"
#include <errno.h>

static PHB_PP_STATE s_pp_state = NULL;

BOOL hb_pp_NestedLiteralString = FALSE;
BOOL hb_pp_LiteralEscSeq = FALSE;
int hb_pp_StreamBlock = 0;
unsigned int hb_pp_MaxTranslateCycles = 1024;
char *hb_pp_STD_CH = NULL;



static void hb_pp_ErrorGen( char * szMsgTable[], char cPrefix, int iErrorCode,
                            const char * szParam1, const char * szParam2 )
{
   int iLine = hb_comp_iLine;

   /* I do not know why but compiler expect line number 1 bigger then
      real line number */
   hb_comp_iLine = hb_pp_line( s_pp_state ) + 1;
   if( cPrefix == 'W' )
      hb_compGenWarning( szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   else
      hb_compGenError( szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   hb_comp_iLine = iLine;
}

static FILE * hb_pp_IncludeOpen( char *szFileName, BOOL fSysFile, char * szFNameBuf )
{
   PHB_FNAME pFileName;
   FILE *file = NULL;
   PFILE pFile;
   BOOL fPath;

   pFileName = hb_fsFNameSplit( szFileName );
   fPath = pFileName->szPath && pFileName->szPath[ 0 ];
   errno = 0;
   if( !fSysFile )
   {
      if( !fPath && hb_comp_pFileName )
         pFileName->szPath = hb_comp_pFileName->szPath;

      hb_fsFNameMerge( szFNameBuf, pFileName );

      file = fopen( szFNameBuf, "r" );
   }

   if( !file && errno != EMFILE && hb_comp_pIncludePath )
   {
      HB_PATHNAMES * pSearch = hb_comp_pIncludePath;

      pFileName->szName = szFileName;
      pFileName->szExtension = NULL;
      while( pSearch && !file )
      {
         pFileName->szPath = pSearch->szPath;
         hb_fsFNameMerge( szFNameBuf, pFileName );
         file = fopen( szFNameBuf, "r" );
         pSearch = pSearch->pNext;
      }
   }
   hb_xfree( pFileName );

   if( file )
   {
      pFile = ( PFILE ) hb_xgrab( sizeof( _FILE ) );
      pFile->handle = file;
      pFile->pBuffer = hb_xgrab( HB_PP_BUFF_SIZE );
      pFile->iBuffer = pFile->lenBuffer = 10;
      pFile->yyBuffer = NULL;
      pFile->szFileName = hb_strdup( szFNameBuf );
      if( hb_comp_files.pLast )
         hb_comp_files.pLast->iLine = hb_comp_iLine;
      hb_comp_iLine = 1;
      pFile->iLine = 1;
      pFile->pPrev = hb_comp_files.pLast;
      hb_comp_files.pLast = pFile;
      hb_comp_files.iFiles++;
   }

   return file;
}

static void hb_pp_IncludeClose( FILE * file )
{
   if( hb_comp_files.iFiles > 0 && hb_comp_files.pLast &&
       file == hb_comp_files.pLast->handle )
   {
      PFILE pFile;
      fclose( hb_comp_files.pLast->handle );
      hb_xfree( hb_comp_files.pLast->pBuffer );
      hb_xfree( hb_comp_files.pLast->szFileName );
      pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
#if 0
      if( hb_comp_files.pLast->yyBuffer && hb_comp_files.iFiles == 1 )
         hb_compParserStop(); /* uses hb_comp_files.pLast */
#endif
      hb_xfree( hb_comp_files.pLast );
      hb_comp_files.pLast = pFile;
      if( hb_comp_files.pLast )
         hb_comp_iLine = hb_comp_files.pLast->iLine;
      hb_comp_files.iFiles--;
   }
}

static void hb_pp_PragmaDump( char * pBuffer, ULONG ulSize, int iLine )
{
   int iSaveLine = hb_comp_iLine;
   PINLINE pInline;

   /* I do not know why but compiler expect line number 1 bigger then
      real line number */
   hb_comp_iLine = iLine + 1;

   pInline = hb_compInlineAdd( NULL );
   pInline->pCode = ( BYTE * ) hb_xgrab( ulSize + 1 );
   memcpy( pInline->pCode, pBuffer, ulSize );
   pBuffer[ ulSize ] = '\0';
   pInline->lPCodeSize = ulSize;

   hb_comp_iLine = iSaveLine;
}

static BOOL hb_pp_CompilerSwitch( const char * szSwitch, int iValue )
{
   BOOL fError = FALSE;
   int i = strlen( szSwitch );

   if( i == 1 )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'a':
         case 'A':
            hb_comp_bAutoMemvarAssume = iValue != 0;
            break;

         case 'b':
         case 'B':
            hb_comp_bDebugInfo = iValue != 0;
            break;

         case 'l':
         case 'L':
            hb_comp_bLineNumbers = iValue != 0;
            break;

         case 'n':
         case 'N':
            hb_comp_bStartProc = iValue != 0;
            break;

         case 'p':
         case 'P':
            hb_comp_bPPO = iValue != 0;
            break;

         case 'q':
         case 'Q':
            hb_comp_bQuiet = iValue != 0;
            break;

         case 'v':
         case 'V':
            hb_comp_bForceMemvars = iValue != 0;
            break;

         case 'w':
         case 'W':
            if( iValue >= 0 && iValue <= 3 )
               hb_comp_iWarnings = iValue;
            else
               fError = TRUE;
            break;

         case 'z':
         case 'Z':
            hb_comp_bShortCuts = iValue != 0;
            break;

         default:
            fError = TRUE;
      }
   }
   else if( i == 2 )
   {
      if( szSwitch[ 1 ] - '0' == iValue && iValue >= 0 && iValue <= 3 &&
          ( szSwitch[ 0 ] == 'w' || szSwitch[ 0 ] == 'W' ) &&
          ( iValue >= 0 && iValue <= 3 ) )
         hb_comp_iWarnings = iValue;
      else if( hb_stricmp( szSwitch, "es" ) == 0 &&
               ( iValue == HB_EXITLEVEL_DEFAULT ||
                 iValue == HB_EXITLEVEL_SETEXIT ||
                 iValue == HB_EXITLEVEL_DELTARGET ) )
         hb_comp_iExitLevel = iValue;
      else
         fError = TRUE;
   }
   else if( i == 3 )
   {
      if( szSwitch[ 2 ] - '0' == iValue &&
          hb_strnicmp( szSwitch, "es", 2 ) == 0 &&
          ( iValue == HB_EXITLEVEL_DEFAULT ||
            iValue == HB_EXITLEVEL_SETEXIT ||
            iValue == HB_EXITLEVEL_DELTARGET ) )
         hb_comp_iExitLevel = iValue;
      else
         fError = TRUE;
   }
   /* xHarbour extension */
   else if( i >= 4 && hb_strnicmp( szSwitch, "TEXTHIDDEN", i ) == 0 &&
            iValue >= 0 && iValue <= 1 )
      hb_comp_iHidden = iValue;
   else
      fError = TRUE;

   return fError;
}


void hb_pp_SetRules( BOOL fQuiet, int argc, char * argv[] )
{
   char * szStdCh = hb_pp_STD_CH;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_SetRules()" ) );

   if( szStdCh && * szStdCh <= ' ' )
      szStdCh = "";

   hb_pp_Free();
   s_pp_state = hb_pp_new();
   if( s_pp_state )
   {
      hb_pp_init( s_pp_state, szStdCh, fQuiet,
                  hb_pp_IncludeOpen, hb_pp_IncludeClose,
                  hb_pp_ErrorGen, NULL, hb_pp_PragmaDump,
                  hb_pp_CompilerSwitch );

      /* Add /D and /undef: command line or envvar defines */
      hb_compChkDefines( argc, argv );

      /* mark current rules as standard ones */
      hb_pp_setStdBase( s_pp_state );
   }

   if( hb_comp_pFileName )
   {
      hb_xfree( ( void * ) hb_comp_pFileName );
      hb_comp_pFileName = NULL;
   }
   if( hb_pp_STD_CH )
   {
      hb_xfree( hb_pp_STD_CH );
      hb_pp_STD_CH = NULL;
   }
}

void hb_pp_Init( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_Init()" ) );

   if( s_pp_state )
   {
      hb_pp_reset( s_pp_state );
   }
}

void hb_pp_Free( void )
{
   if( s_pp_state )
   {
      hb_pp_free( s_pp_state );
      s_pp_state = NULL;
   }
}

void hb_pp_AddDefine( char *defname, char *value )
{
   if( s_pp_state )
      hb_pp_addDefine( s_pp_state, defname, value );
}

void hb_pp_ParseDirective( char * szLine )
{
   if( s_pp_state && szLine )
      hb_pp_parseLine( s_pp_state, szLine, NULL );
}

int hb_pp_Internal( FILE * handl_o, char * sOut )
{
   ULONG ulLen = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_pp_Internal(%p, %s)", handl_o, sOut));

   hb_pp_StreamBlock = 0;
   hb_pp_NestedLiteralString = FALSE;
   hb_pp_LiteralEscSeq = FALSE;

   if( s_pp_state && hb_comp_files.iFiles > 0 )
   {
      char * szLine;

      if( ! hb_pp_fileName( s_pp_state ) )
      {
         hb_pp_inFile( s_pp_state, hb_comp_files.pLast->szFileName,
                                   hb_comp_files.pLast->handle );
      }
      if( handl_o && ! hb_pp_outFileName( s_pp_state ) )
      {
         char szOutFileName[ _POSIX_PATH_MAX + 1 ];
         if( hb_comp_pFilePpo )
            hb_fsFNameMerge( szOutFileName, hb_comp_pFilePpo );
         else
            szOutFileName[ 0 ] = '\0';
         hb_pp_outFile( s_pp_state, szOutFileName, handl_o );
         /* dirty hack but works as workaround for pure PP and compiler
            API integration */
         if( handl_o == hb_comp_yyppo )
            hb_comp_yyppo = NULL;
      }

      szLine = hb_pp_nextLine( s_pp_state, &ulLen );
      /* I do not know why but compiler expect line number 1 bigger then
         real line number */
      hb_comp_iLine = hb_pp_line( s_pp_state ) + 1;

      if( ulLen >= HB_PP_STR_SIZE )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BUFFER_OVERFLOW, NULL, NULL );
      else if( szLine )
         memcpy( sOut, szLine, ulLen + 1 );
      else
         * sOut = '\0';
   }
   else
   {
      * sOut = '\0';
   }

   if( hb_comp_iLineINLINE && hb_pp_StreamBlock == 0 )
   {
      hb_comp_iLineINLINE = 0;
   }

   return ulLen;
}
