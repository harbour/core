/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source with real code generation
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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


#include "hbcomp.h"

static void hb_pp_ErrorGen( void * cargo,
                            const char * const szMsgTable[],
                            char cPrefix, int iErrorCode,
                            const char * szParam1, const char * szParam2 )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;
   int iCurrLine = HB_COMP_PARAM->currLine;
   const char * currModule = HB_COMP_PARAM->currModule;

   HB_COMP_PARAM->currLine = hb_pp_line( HB_COMP_PARAM->pLex->pPP );
   HB_COMP_PARAM->currModule = hb_pp_fileName( HB_COMP_PARAM->pLex->pPP );
   if( cPrefix == 'W' )
      hb_compGenWarning( HB_COMP_PARAM, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   else
      hb_compGenError( HB_COMP_PARAM, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   HB_COMP_PARAM->fError = HB_FALSE;
   HB_COMP_PARAM->currLine = iCurrLine;
   HB_COMP_PARAM->currModule = currModule;
}

static void hb_pp_Disp( void * cargo, const char * szMessage )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;

   hb_compOutStd( HB_COMP_PARAM, szMessage );
}

static void hb_pp_PragmaDump( void * cargo, char * pBuffer, HB_SIZE nSize,
                              int iLine )
{
   PHB_HINLINE pInline;

   pInline = hb_compInlineAdd( ( HB_COMP_PTR ) cargo, NULL, iLine );
   pInline->pCode = ( HB_BYTE * ) hb_xgrab( nSize + 1 );
   memcpy( pInline->pCode, pBuffer, nSize );
   pInline->pCode[ nSize ] = '\0';
   pInline->nPCodeSize = nSize;
}

static void hb_pp_hb_inLine( void * cargo, char * szFunc,
                             char * pBuffer, HB_SIZE nSize, int iLine )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;

   if( HB_COMP_PARAM->iLanguage != HB_LANG_C )
   {
      int iCurrLine = HB_COMP_PARAM->currLine;
      HB_COMP_PARAM->currLine = iLine;
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_REQUIRES_C, NULL, NULL );
      HB_COMP_PARAM->fError = HB_FALSE;
      HB_COMP_PARAM->currLine = iCurrLine;
   }
   else
   {
      PHB_HINLINE pInline = hb_compInlineAdd( HB_COMP_PARAM,
         hb_compIdentifierNew( HB_COMP_PARAM, szFunc, HB_IDENT_COPY ), iLine );
      pInline->pCode = ( HB_BYTE * ) hb_xgrab( nSize + 1 );
      memcpy( pInline->pCode, pBuffer, nSize );
      pInline->pCode[ nSize ] = '\0';
      pInline->nPCodeSize = nSize;
   }
}

static HB_BOOL hb_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                     int * piValue, HB_BOOL fSet )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;
   HB_BOOL fError = HB_FALSE;
   int iValue, i;

   iValue = *piValue;

   i = ( int ) strlen( szSwitch );
   if( i > 1 && ( ( int ) ( szSwitch[ i - 1 ] - '0' ) ) == iValue )
      --i;

   if( i == 1 )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'a':
         case 'A':
            if( fSet )
               HB_COMP_PARAM->fAutoMemvarAssume = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fAutoMemvarAssume ? 1 : 0;
            break;

         case 'b':
         case 'B':
            if( fSet )
               HB_COMP_PARAM->fDebugInfo = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fDebugInfo ? 1 : 0;
            break;

         case 'j':
         case 'J':
            if( fSet )
               HB_COMP_PARAM->fI18n = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fI18n ? 1 : 0;
            break;

         case 'l':
         case 'L':
            if( fSet )
               HB_COMP_PARAM->fLineNumbers = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fLineNumbers ? 1 : 0;
            break;

         case 'n':
         case 'N':
            if( fSet )
            {
               if( iValue >= 0 && iValue <= 2 )
                  HB_COMP_PARAM->iStartProc = iValue;
               else
                  fError = HB_TRUE;
            }
            else
               iValue = HB_COMP_PARAM->iStartProc;
            break;

         case 'p':
         case 'P':
            if( fSet )
               HB_COMP_PARAM->fPPO = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fPPO ? 1 : 0;
            break;

         case 'q':
         case 'Q':
            if( fSet )
               HB_COMP_PARAM->fQuiet = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fQuiet ? 1 : 0;
            break;

         case 'v':
         case 'V':
            if( fSet )
               HB_COMP_PARAM->fForceMemvars = iValue != 0;
            else
               iValue = HB_COMP_PARAM->fForceMemvars ? 1 : 0;
            break;

         case 'w':
         case 'W':
            if( fSet )
            {
               if( iValue >= 0 && iValue <= 3 )
                  HB_COMP_PARAM->iWarnings = iValue;
               else
                  fError = HB_TRUE;
            }
            else
               iValue = HB_COMP_PARAM->iWarnings;
            break;

         case 'z':
         case 'Z':
            if( fSet )
            {
               if( iValue )
                  HB_COMP_PARAM->supported &= ~HB_COMPFLAG_SHORTCUTS;
               else
                  HB_COMP_PARAM->supported |= HB_COMPFLAG_SHORTCUTS;
            }
            else
               iValue = ( HB_COMP_PARAM->supported & HB_COMPFLAG_SHORTCUTS ) ? 0 : 1;
            break;

         default:
            fError = HB_TRUE;
      }
   }
   else if( i == 2 )
   {
      if( szSwitch[ 0 ] == 'k' || szSwitch[ 0 ] == 'K' )
      {
         int iFlag = 0;
         /* -k? parameters are case sensitive */
         switch( szSwitch[ 1 ] )
         {
            case '?':
               if( fSet )
                  HB_COMP_PARAM->supported = iValue;
               else
                  iValue = HB_COMP_PARAM->supported;
               break;
            case 'c':
            case 'C':
               if( fSet )
               {
                  /* clear all flags - minimal set of features */
                  HB_COMP_PARAM->supported &= HB_COMPFLAG_SHORTCUTS;
                  HB_COMP_PARAM->supported |= HB_COMPFLAG_OPTJUMP |
                                              HB_COMPFLAG_MACROTEXT;
               }
               else
               {
                  iValue = ( HB_COMP_PARAM->supported & ~HB_COMPFLAG_SHORTCUTS ) ==
                           ( HB_COMPFLAG_OPTJUMP | HB_COMPFLAG_MACROTEXT ) ? 1 : 0;
               }
               break;
            case 'h':
            case 'H':
               iFlag = HB_COMPFLAG_HARBOUR;
               break;
            case 'o':
            case 'O':
               iFlag = HB_COMPFLAG_EXTOPT;
               break;
            case 'i':
            case 'I':
               iFlag = HB_COMPFLAG_HB_INLINE;
               break;
            case 'r':
            case 'R':
               iFlag = HB_COMPFLAG_RT_MACRO;
               break;
            case 'x':
            case 'X':
               iFlag = HB_COMPFLAG_XBASE;
               break;
            case 'j':
            case 'J':
               iFlag = HB_COMPFLAG_OPTJUMP;
               iValue = !iValue;
               break;
            case 'm':
            case 'M':
               iFlag = HB_COMPFLAG_MACROTEXT;
               iValue = !iValue;
               break;
            case 'd':
            case 'D':
               iFlag = HB_COMPFLAG_MACRODECL;
               break;
            case 's':
            case 'S':
               iFlag = HB_COMPFLAG_ARRSTR;
               break;
            default:
               fError = HB_TRUE;
         }
         if( ! fError && iFlag )
         {
            if( fSet )
            {
               if( iValue )
                  HB_COMP_PARAM->supported |= iFlag;
               else
                  HB_COMP_PARAM->supported &= ~iFlag;
            }
            else
            {
               if( iValue )
                  iValue = HB_COMP_PARAM->supported & iFlag ? 0 : 1;
               else
                  iValue = HB_COMP_PARAM->supported & iFlag ? 1 : 0;
            }
         }
      }
      else if( hb_strnicmp( szSwitch, "es", 2 ) == 0 )
      {
         if( fSet )
         {
            if( iValue == HB_EXITLEVEL_DEFAULT ||
                iValue == HB_EXITLEVEL_SETEXIT ||
                iValue == HB_EXITLEVEL_DELTARGET )
               HB_COMP_PARAM->iExitLevel = iValue;
         }
         else
            iValue = HB_COMP_PARAM->iExitLevel;
      }
      else if( hb_stricmp( szSwitch, "p+" ) == 0 )
      {
         if( fSet )
            HB_COMP_PARAM->fPPT = iValue != 0;
         else
            iValue = HB_COMP_PARAM->fPPT ? 1 : 0;
      }
      else
         fError = HB_TRUE;
   }
   /* xHarbour extension */
   else if( i >= 4 && hb_strnicmp( szSwitch, "TEXTHIDDEN", i ) == 0 )
   {
      if( fSet )
      {
         if( iValue >= 0 && iValue <= 1 )
            HB_COMP_PARAM->iHidden = iValue;
      }
      else
         iValue = HB_COMP_PARAM->iHidden;
   }
   else
      fError = HB_TRUE;

   *piValue = iValue;

   return fError;
}

static void hb_pp_fileIncluded( void * cargo, const char * szFileName )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;
   PHB_INCLST pIncFile, * pIncFilePtr;
   int iLen;

   pIncFilePtr = &HB_COMP_PARAM->incfiles;
   while( *pIncFilePtr )
   {
#if defined( HB_OS_UNIX )
      if( strcmp( ( *pIncFilePtr )->szFileName, szFileName ) == 0 )
         return;
#else
      if( hb_stricmp( ( *pIncFilePtr )->szFileName, szFileName ) == 0 )
         return;
#endif
      pIncFilePtr = &( *pIncFilePtr )->pNext;
   }

   iLen = ( int ) strlen( szFileName );
   pIncFile = ( PHB_INCLST ) hb_xgrab( sizeof( HB_INCLST ) + iLen );
   pIncFile->pNext = NULL;
   memcpy( pIncFile->szFileName, szFileName, iLen + 1 );
   *pIncFilePtr = pIncFile;
}

void hb_compInitPP( HB_COMP_DECL, int argc, const char * const argv[],
                    PHB_PP_OPEN_FUNC pOpenFunc )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compInitPP()" ) );

   if( HB_COMP_PARAM->pLex->pPP )
   {
      hb_pp_init( HB_COMP_PARAM->pLex->pPP, HB_COMP_PARAM->fQuiet,
                  HB_COMP_PARAM->iMaxTransCycles,
                  HB_COMP_PARAM, pOpenFunc, NULL,
                  hb_pp_ErrorGen, hb_pp_Disp, hb_pp_PragmaDump,
                  HB_COMP_ISSUPPORTED( HB_COMPFLAG_HB_INLINE ) ?
                  hb_pp_hb_inLine : NULL, hb_pp_CompilerSwitch );

      if( HB_COMP_PARAM->iTraceInclude )
         hb_pp_setIncFunc( HB_COMP_PARAM->pLex->pPP, hb_pp_fileIncluded );

      if( ! HB_COMP_PARAM->szStdCh )
         hb_pp_setStdRules( HB_COMP_PARAM->pLex->pPP );
      else if( HB_COMP_PARAM->szStdCh[ 0 ] > ' ' )
         hb_pp_readRules( HB_COMP_PARAM->pLex->pPP, HB_COMP_PARAM->szStdCh );
      else if( ! HB_COMP_PARAM->fQuiet )
         hb_compOutStd( HB_COMP_PARAM, "Standard command definitions excluded.\n" );

      hb_pp_initDynDefines( HB_COMP_PARAM->pLex->pPP, ! HB_COMP_PARAM->fNoArchDefs );

      /* Add /D and /undef: command line or envvar defines */
      hb_compChkDefines( HB_COMP_PARAM, argc, argv );

      /* add extended definitions files (-u+<file>) */
      if( HB_COMP_PARAM->iStdChExt > 0 )
      {
         int i = 0;

         while( i < HB_COMP_PARAM->iStdChExt )
            hb_pp_readRules( HB_COMP_PARAM->pLex->pPP,
                             HB_COMP_PARAM->szStdChExt[ i++ ] );
      }

      /* mark current rules as standard ones */
      hb_pp_setStdBase( HB_COMP_PARAM->pLex->pPP );
   }
}
