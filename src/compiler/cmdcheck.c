/*
 * Compiler command-line and environment parameters checking
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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
#include "hbset.h"

static char s_szUndefineMarker[ 1 ] = "";

static HB_SIZE hb_compChkOptionLen( const char * szSwitch, HB_BOOL fEnv )
{
   HB_SIZE nLen;

   if( fEnv )
   {
      nLen = 0;
      while( szSwitch[ nLen ] != '\0' &&
             szSwitch[ nLen ] != ' ' && szSwitch[ nLen ] != '-' )
         ++nLen;
   }
   else
      nLen = strlen( szSwitch );

   return nLen;
}

static const char * hb_compChkAddDefine( HB_COMP_DECL, const char * szSwitch,
                                         HB_BOOL fAdd, HB_BOOL fEnv )
{
   const char * szSwPtr = szSwitch;
   HB_SIZE nValue = 0;

   while( *szSwPtr && *szSwPtr != ' ' && ! HB_ISOPTSEP( *szSwPtr ) )
   {
      if( *szSwPtr == '=' )
      {
         nValue = szSwPtr - szSwitch;
         szSwPtr += hb_compChkOptionLen( szSwPtr, fEnv );
         break;
      }
      ++szSwPtr;
   }
   if( szSwPtr > szSwitch && *szSwitch != '=' )
   {
      char * szDefine = hb_strndup( szSwitch, szSwPtr - szSwitch );
      char * szValue = NULL;
      PHB_PPDEFINE * pDefinePtr;

      if( nValue )
      {
         szValue = szDefine + nValue;
         *szValue++ = '\0';
      }
      if( ! fAdd )
         szValue = s_szUndefineMarker;

      pDefinePtr = &HB_COMP_PARAM->ppdefines;
      while( *pDefinePtr != NULL &&
             strcmp( ( *pDefinePtr )->szName, szDefine ) != 0 )
         pDefinePtr = &( *pDefinePtr )->pNext;

      if( *pDefinePtr == NULL )
      {
         *pDefinePtr = ( PHB_PPDEFINE ) hb_xgrab( sizeof( HB_PPDEFINE ) );
         ( *pDefinePtr )->pNext = NULL;
      }
      else
         hb_xfree( ( *pDefinePtr )->szName );
      ( *pDefinePtr )->szName = szDefine;
      ( *pDefinePtr )->szValue = szValue;
   }
   return szSwPtr;
}

static void hb_compChkIgnoredInfo( HB_COMP_DECL, const char * szSwitch )
{
   char buffer[ 64 ];

   hb_snprintf( buffer, sizeof( buffer ),
                "Ignored unsupported command-line option: %s\n", szSwitch );
   hb_compOutStd( HB_COMP_PARAM, buffer );
}

static char * hb_compChkOptionDup( const char * szSwitch )
{
   return hb_strupr( hb_strndup( szSwitch,
                                 hb_compChkOptionLen( szSwitch, HB_TRUE ) ) );
}

static const char * hb_compChkOptionGet( const char * szSwitch,
                                         char ** pszResult, HB_BOOL fEnv )
{
   HB_SIZE nLen = hb_compChkOptionLen( szSwitch, fEnv );

   if( pszResult )
      *pszResult = hb_strndup( szSwitch, nLen );

   return szSwitch + nLen;
}

static const char * hb_compChkOptionFName( const char * szSwitch,
                                           PHB_FNAME * pResult, HB_BOOL fEnv )
{
   HB_SIZE nLen = hb_compChkOptionLen( szSwitch, fEnv );

   if( nLen > 0 )
   {
      if( *pResult )
         hb_xfree( *pResult );
      if( szSwitch[ nLen ] != '\0' )
      {
         char * szVal = hb_strndup( szSwitch, nLen );
         *pResult = hb_fsFNameSplit( szVal );
         hb_xfree( szVal );
      }
      else
         *pResult = hb_fsFNameSplit( szSwitch );
   }
   return szSwitch + nLen;
}

static const char * hb_compChkOptionAddPath( HB_COMP_DECL, const char * szSwitch,
                                             HB_BOOL fEnv )
{
   HB_SIZE nLen = hb_compChkOptionLen( szSwitch, fEnv );

   if( nLen > 0 )
   {
      if( szSwitch[ nLen ] != '\0' )
      {
         char * szVal = hb_strndup( szSwitch, nLen );
         hb_pp_addSearchPath( HB_COMP_PARAM->pLex->pPP, szSwitch, HB_FALSE );
         hb_xfree( szVal );
      }
      else
         hb_pp_addSearchPath( HB_COMP_PARAM->pLex->pPP, szSwitch, HB_FALSE );
   }
   return szSwitch + nLen;
}

static const char * hb_compChkParseSwitch( HB_COMP_DECL, const char * szSwitch,
                                           HB_BOOL fEnv )
{
   const char * szSwPtr = szSwitch;

   if( szSwPtr[ 0 ] == '-' && szSwPtr[ 1 ] == '-' )
   {
      if( strncmp( szSwPtr + 2, "version", 7 ) == 0 )
      {
         szSwPtr += 9;
         HB_COMP_PARAM->fLogo = HB_TRUE;
         HB_COMP_PARAM->fQuiet = HB_TRUE;
      }
      else if( strncmp( szSwPtr + 2, "help", 4 ) == 0 )
      {
         szSwPtr += 6;
         HB_COMP_PARAM->fLogo = HB_TRUE;
         HB_COMP_PARAM->fQuiet = HB_FALSE;
         HB_COMP_PARAM->fExit = HB_FALSE;
      }
   }
   else if( HB_ISOPTSEP( *szSwPtr ) )
   {
      ++szSwPtr;
      switch( HB_TOUPPER( *szSwPtr ) )
      {
         case 'A':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ++szSwPtr;
               HB_COMP_PARAM->fAutoMemvarAssume = HB_FALSE;
            }
            else
               HB_COMP_PARAM->fAutoMemvarAssume = HB_TRUE;
            break;

         case 'B':
         {
            char *szOption = hb_compChkOptionDup( szSwPtr );

            if( strcmp( szOption, "BUILD" ) == 0 )
            {
               HB_COMP_PARAM->fBuildInfo = HB_TRUE;
               szSwPtr += 5;
            }
            else if( szSwPtr[ 1 ] == '-' )
            {
               HB_COMP_PARAM->fDebugInfo = HB_FALSE;
               szSwPtr += 2;
            }
            else
            {
               HB_COMP_PARAM->fDebugInfo = HB_TRUE;
               HB_COMP_PARAM->fLineNumbers = HB_TRUE;
               ++szSwPtr;
            }
            hb_xfree( szOption );
            break;
         }

         case 'C':
         {
            char *szOption = hb_compChkOptionDup( szSwPtr );

            if( strlen( szOption ) >= 4 &&
                strncmp( "CREDITS", szOption, strlen( szOption ) ) == 0 )
            {
               HB_COMP_PARAM->fCredits = HB_TRUE;
               szSwPtr += strlen( szOption );
            }
            hb_xfree( szOption );
            break;
         }

         case 'D':
            szSwPtr = hb_compChkAddDefine( HB_COMP_PARAM, szSwPtr + 1, HB_TRUE, fEnv );
            break;

         case 'E':
            if( HB_TOUPPER( szSwPtr[ 1 ] ) == 'S' )
            {
               switch( szSwPtr[ 2 ] )
               {
                  case '1':
                     szSwPtr += 3;
                     HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_SETEXIT;
                     break;
                  case '2':
                     szSwPtr += 3;
                     HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_DELTARGET;
                     break;
                  case '0':
                     ++szSwPtr;
                     /* fallthrough */
                  default:
                     szSwPtr += 2;
                     HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_DEFAULT;
                     break;
               }
            }
            break;

         case 'F':
            switch( HB_TOUPPER( szSwPtr[ 1 ] ) )
            {
               case 'N':
                  if( szSwPtr[ 2 ] == ':' )
                  {
                     if( HB_TOUPPER( szSwPtr[ 3 ] ) == 'U' )
                     {
                        szSwPtr += 4;
                        hb_setSetFileCase( HB_SET_CASE_UPPER );
                     }
                     else if( HB_TOUPPER( szSwPtr[ 3 ] ) == 'L' )
                     {
                        szSwPtr += 4;
                        hb_setSetFileCase( HB_SET_CASE_LOWER );
                     }
                  }
                  else
                  {
                     szSwPtr += 2;
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     hb_setSetFileCase( HB_SET_CASE_MIXED );
                  }
                  break;
               case 'D':
                  if( szSwPtr[ 2 ] == ':' )
                  {
                     if( HB_TOUPPER( szSwPtr[ 3 ] ) == 'U' )
                     {
                        szSwPtr += 4;
                        hb_setSetDirCase( HB_SET_CASE_UPPER );
                     }
                     else if( HB_TOUPPER( szSwPtr[ 3 ] ) == 'L' )
                     {
                        szSwPtr += 4;
                        hb_setSetDirCase( HB_SET_CASE_LOWER );
                     }
                  }
                  else
                  {
                     szSwPtr += 2;
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     hb_setSetDirCase( HB_SET_CASE_MIXED );
                  }
                  break;
               case 'P':
                  szSwPtr += 2;
                  if( *szSwPtr == ':' )
                  {
                     if( szSwPtr[ 1 ] && szSwPtr[ 1 ] != ' ' )
                     {
                        hb_setSetDirSeparator( szSwPtr[ 1 ] );
                        szSwPtr += 2;
                     }
                  }
                  else
                  {
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     hb_setSetDirSeparator( HB_OS_PATH_DELIM_CHR );
                  }
                  break;
               case 'S':
                  szSwPtr += 2;
                  if( *szSwPtr == '-' )
                  {
                     ++szSwPtr;
                     hb_setSetTrimFileName( HB_FALSE );
                  }
                  else
                     hb_setSetTrimFileName( HB_TRUE );
            }
            break;

         case 'G':
            switch( HB_TOUPPER( szSwPtr[ 1 ] ) )
            {
               case 'C':
                  HB_COMP_PARAM->iLanguage = HB_LANG_C;
                  szSwPtr += 2;
                  switch( *szSwPtr )
                  {
                     case '1':
                        ++szSwPtr;
                        HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_NORMAL;
                        break;
                     case '2':
                        ++szSwPtr;
                        HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_VERBOSE;
                        break;
                     case '3':
                        ++szSwPtr;
                        HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_REALCODE;
                        break;
                     case '0':
                        ++szSwPtr;
                        /* fallthrough */
                     default:
                        HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_COMPACT;
                        break;
                  }
                  break;

               case 'H':
                  HB_COMP_PARAM->iLanguage = HB_LANG_PORT_OBJ;
                  szSwPtr += 2;
                  break;

               case 'J':
                  HB_COMP_PARAM->iLanguage = HB_LANG_JS;
                  szSwPtr += 2;
                  break;

               case 'D':
                  if( HB_COMP_PARAM->szDepExt )
                  {
                     hb_xfree( HB_COMP_PARAM->szDepExt );
                     HB_COMP_PARAM->szDepExt = NULL;
                  }
                  szSwPtr += 2;
                  if( *szSwPtr == '-' )
                  {
                     HB_COMP_PARAM->iTraceInclude = 0;
                     ++szSwPtr;
                  }
                  else
                  {
                     HB_COMP_PARAM->iTraceInclude = 2;
                     if( *szSwPtr == '.' )
                        szSwPtr = hb_compChkOptionGet( szSwPtr, &HB_COMP_PARAM->szDepExt, fEnv );
                  }
                  break;

               case 'E':
                  szSwPtr += 2;
                  switch( *szSwPtr )
                  {
                     case '1':
                        ++szSwPtr;
                        HB_COMP_PARAM->iErrorFmt = HB_ERRORFMT_IDE;
                        break;
                     case '0':
                        ++szSwPtr;
                        /* fallthrough */
                     default:
                        HB_COMP_PARAM->iErrorFmt = HB_ERRORFMT_CLIPPER;
                        break;
                  }
                  break;

               default:
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_UNSUPPORTED_LANG, NULL, NULL );
                  break;
            }
            break;

         case 'H':
         case '?':
            /* HELP message */
            break;

         case 'I':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case '-':
                  HB_COMP_PARAM->fINCLUDE = HB_FALSE;
                  ++szSwPtr;
                  break;
               case '+':
                  HB_COMP_PARAM->fINCLUDE = HB_TRUE;
                  ++szSwPtr;
                  break;
               default:
                  szSwPtr = hb_compChkOptionAddPath( HB_COMP_PARAM, szSwPtr, fEnv );
                  break;
            }
            break;

         case 'J':
            ++szSwPtr;
            HB_COMP_PARAM->fI18n = HB_TRUE;
            if( *szSwPtr )
               szSwPtr = hb_compChkOptionFName( szSwPtr, &HB_COMP_PARAM->pI18nFileName, fEnv );
            break;

         case 'K':
            ++szSwPtr;
            while( *szSwPtr && ! HB_COMP_PARAM->fExit )
            {
               int ch = HB_TOUPPER( *szSwPtr );

               ++szSwPtr;
               switch( ch )
               {
                  case '?':
                     hb_compPrintLogo( HB_COMP_PARAM );
                     hb_compPrintModes( HB_COMP_PARAM );
                     HB_COMP_PARAM->fLogo = HB_FALSE;
                     HB_COMP_PARAM->fQuiet = HB_TRUE;
                     break;

                  case 'H':
                     /* default Harbour mode */
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_HARBOUR;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_HARBOUR;
                     break;

                  case 'C':
                     /* clear all flags - minimal set of features */
                     HB_COMP_PARAM->supported &= HB_COMPFLAG_SHORTCUTS;
                     HB_COMP_PARAM->supported |= HB_COMPFLAG_OPTJUMP |
                                                 HB_COMPFLAG_MACROTEXT;
                     break;

                  case 'X':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_XBASE;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_XBASE;
                     break;

                  case 'I':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_HB_INLINE;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_HB_INLINE;
                     break;

                  case 'J':
                     if( *szSwPtr == '+' )
                     {
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_OPTJUMP;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_OPTJUMP;
                     break;

                  case 'M':
                     if( *szSwPtr == '+' )
                     {
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_MACROTEXT;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_MACROTEXT;
                     break;

                  case 'D':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_MACRODECL;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_MACRODECL;
                     break;

                  case 'R':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_RT_MACRO;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_RT_MACRO;
                     break;

                  case 'S':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_ARRSTR;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_ARRSTR;
                     break;

                  case 'O':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_EXTOPT;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_EXTOPT;
                     break;

                  case 'U':
                     if( *szSwPtr == '-' )
                     {
                        HB_COMP_PARAM->supported &= ~HB_COMPFLAG_USERCP;
                        ++szSwPtr;
                     }
                     else
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_USERCP;
                     break;

                  default:
                     ch = -1;
                     --szSwPtr;
                     break;
               }
               if( ch == -1 )
                  break;
            }
            break;

         case 'L':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               HB_COMP_PARAM->fLineNumbers = HB_TRUE;
               ++szSwPtr;
            }
            else
               HB_COMP_PARAM->fLineNumbers = HB_FALSE;
            break;

         case 'M':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               HB_COMP_PARAM->fSingleModule = HB_FALSE;
               ++szSwPtr;
            }
            else
               HB_COMP_PARAM->fSingleModule = HB_TRUE;
            break;

         case 'N':
            ++szSwPtr;
            HB_COMP_PARAM->fNoStartUp = *szSwPtr == '1';
            switch( *szSwPtr )
            {
               case '-':
                  HB_COMP_PARAM->iStartProc = 0;
                  ++szSwPtr;
                  break;
               case '2':
                  HB_COMP_PARAM->iStartProc = 2;
                  ++szSwPtr;
                  break;
               case '0':
               case '1':
                  ++szSwPtr;
                  /* fallthrough */
               default:
                  HB_COMP_PARAM->iStartProc = 1;
                  break;
            }
            break;

         case 'O':
            szSwPtr = hb_compChkOptionFName( szSwPtr + 1, &HB_COMP_PARAM->pOutPath, fEnv );
            break;

         case 'P':
            ++szSwPtr;
            if( *szSwPtr == '+' )
            {
               HB_COMP_PARAM->fPPT = HB_TRUE;
               ++szSwPtr;
            }
            else
            {
               if( HB_COMP_PARAM->pPpoPath )
               {
                  hb_xfree( HB_COMP_PARAM->pPpoPath );
                  HB_COMP_PARAM->pPpoPath = NULL;
               }
               if( *szSwPtr == '-' )
               {
                  HB_COMP_PARAM->fPPT = HB_COMP_PARAM->fPPO = HB_FALSE;
                  ++szSwPtr;
               }
               else
               {
                  if( *szSwPtr )
                     szSwPtr = hb_compChkOptionFName( szSwPtr, &HB_COMP_PARAM->pPpoPath, fEnv );
                  HB_COMP_PARAM->fPPO = HB_TRUE;
               }
            }
            break;

         case 'Q':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case 'l':
               case 'L':
                  HB_COMP_PARAM->fGauge = HB_FALSE;
                  ++szSwPtr;
                  break;
               case '2':
                  HB_COMP_PARAM->fFullQuiet = HB_TRUE;
                  /* fallthrough */
               case '0':
                  HB_COMP_PARAM->fLogo = HB_FALSE;
                  ++szSwPtr;
                  /* fallthrough */
               default:
                  HB_COMP_PARAM->fQuiet = HB_TRUE;
                  break;
            }
            break;

         case 'R':
            ++szSwPtr;
            if( szSwPtr[ 0 ] == ':' )
            {
               if( HB_ISDIGIT( szSwPtr[ 1 ] ) )
               {
                  int iCycles = 0;
                  ++szSwPtr;
                  while( HB_ISDIGIT( *szSwPtr ) )
                     iCycles = iCycles * 10 + *szSwPtr++ - '0';
                  if( iCycles > 0 )
                     HB_COMP_PARAM->iMaxTransCycles = iCycles;
               }
            }
            else
            {
               /* NOTE: ignored for Cl*pper compatibility:
                        /r[<lib>] request linker to search <lib> (or none) */
               hb_compChkIgnoredInfo( HB_COMP_PARAM, "-r[<lib>]" );
               szSwPtr = hb_compChkOptionGet( szSwPtr, NULL, fEnv );
            }
            break;

         case 'S':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case '-':
                  HB_COMP_PARAM->iSyntaxCheckOnly = 0;
                  ++szSwPtr;
                  break;
               case 'm':
               case 'M':
                  HB_COMP_PARAM->iSyntaxCheckOnly = 2;
                  ++szSwPtr;
                  break;
               default:
                  HB_COMP_PARAM->iSyntaxCheckOnly = 1;
                  break;
            }
            break;

         case 'T':
            /* NOTE: ignored for Cl*pper compatibility:
                     /t<path> path for temp file creation */
            hb_compChkIgnoredInfo( HB_COMP_PARAM, "-t<path>" );
            szSwPtr = hb_compChkOptionGet( szSwPtr + 1, NULL, fEnv );
            break;

         case 'U':
            if( hb_strnicmp( szSwPtr, "UNDEF:", 6 ) == 0 )
            {
               if( hb_strnicmp( szSwPtr + 6, ".ARCH.", 6 ) == 0 )
               {
                  HB_COMP_PARAM->fNoArchDefs = HB_TRUE;
                  szSwPtr += 12;
               }
               else
                  szSwPtr = hb_compChkAddDefine( HB_COMP_PARAM, szSwPtr + 6, HB_FALSE, fEnv );
               break;
            }
            ++szSwPtr;
            /* extended definitions file: -u+<file> */
            if( *szSwPtr == '+' )
            {
               if( szSwPtr[ 1 ] && hb_compChkOptionLen( szSwPtr + 1, fEnv ) > 0 )
               {
                  HB_COMP_PARAM->szStdChExt = ( char ** )
                     ( HB_COMP_PARAM->iStdChExt == 0 ?
                        hb_xgrab( sizeof( char * ) ) :
                        hb_xrealloc( HB_COMP_PARAM->szStdChExt,
                                     ( HB_COMP_PARAM->iStdChExt + 1 ) *
                                     sizeof( char * ) ) );
                  szSwPtr = hb_compChkOptionGet( szSwPtr + 1,
                                                 &HB_COMP_PARAM->szStdChExt[ HB_COMP_PARAM->iStdChExt++ ],
                                                 fEnv );
               }
            }
            else
            {
               if( HB_COMP_PARAM->szStdCh )
                  hb_xfree( HB_COMP_PARAM->szStdCh );
               szSwPtr = hb_compChkOptionGet( szSwPtr, &HB_COMP_PARAM->szStdCh, fEnv );
            }
            break;

         case 'V':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               HB_COMP_PARAM->fForceMemvars = HB_FALSE;
               ++szSwPtr;
            }
            else
               HB_COMP_PARAM->fForceMemvars = HB_TRUE;
            break;

         case 'W':
            ++szSwPtr;
            HB_COMP_PARAM->iWarnings = 1;
            if( *szSwPtr >= '0' && *szSwPtr <= '3' )
            {
               HB_COMP_PARAM->iWarnings = *szSwPtr - '0';
               ++szSwPtr;
            }
            break;

#ifdef YYDEBUG
         case 'Y':
            ++szSwPtr;
            extern int hb_comp_yydebug;
            hb_comp_yydebug = HB_TRUE;
            break;
#endif

         case 'Z':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               HB_COMP_PARAM->supported |= HB_COMPFLAG_SHORTCUTS;
               ++szSwPtr;
            }
            else
               HB_COMP_PARAM->supported &= ~HB_COMPFLAG_SHORTCUTS;
            break;
      }
   }

   if( ! HB_COMP_PARAM->fExit )
   {
      if( szSwPtr - szSwitch <= 1 ||
          ( *szSwPtr != '\0' && *szSwPtr != ' ' && ! HB_ISOPTSEP( *szSwPtr ) ) )
         hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F',
                          fEnv ? HB_COMP_ERR_BADOPTION : HB_COMP_ERR_BADPARAM,
                          szSwitch, NULL );
      else
         return szSwPtr;
   }

   return "";
}

/* check command-line parameters */
void hb_compChkCommandLine( HB_COMP_DECL, int argc, const char * const argv[] )
{
   int i;

   for( i = 1; i < argc && ! HB_COMP_PARAM->fExit; ++i )
   {
      const char * szSwitch = argv[ i ];

      if( HB_ISOPTSEP( szSwitch[ 0 ] ) )
      {
         do
            szSwitch = hb_compChkParseSwitch( HB_COMP_PARAM, szSwitch, HB_FALSE );
         while( *szSwitch != '\0' );
      }
   }
}

/* check environment parameters */
void hb_compChkEnvironment( HB_COMP_DECL )
{
   /* NOTE: if HARBOURCMD envvar exists then it's used instead of CLIPPERCMD */
   char * szEnvCMD = hb_getenv( "HARBOURCMD" );

   if( ! szEnvCMD || szEnvCMD[ 0 ] == '\0' )
   {
      if( szEnvCMD )
         hb_xfree( szEnvCMD );
      szEnvCMD = hb_getenv( "CLIPPERCMD" );
   }

   if( szEnvCMD )
   {
      const char * szSwitch = szEnvCMD;

      while( *szSwitch )
      {
         while( *szSwitch == ' ' )
            ++szSwitch;
         if( *szSwitch )
            szSwitch = hb_compChkParseSwitch( HB_COMP_PARAM, szSwitch, HB_TRUE );
      }
      hb_xfree( szEnvCMD );
   }
}

void hb_compChkAddIncPaths( HB_COMP_DECL )
{
   char * szInclude = hb_getenv( "INCLUDE" );

   if( szInclude )
   {
      if( szInclude[ 0 ] != '\0' )
         hb_pp_addSearchPath( HB_COMP_PARAM->pLex->pPP, szInclude, HB_FALSE );
      hb_xfree( szInclude );
   }
}

void hb_compChkSetDefines( HB_COMP_DECL )
{
   PHB_PPDEFINE pDefine = HB_COMP_PARAM->ppdefines;

   while( pDefine )
   {
      if( pDefine->szValue == s_szUndefineMarker )
         hb_pp_delDefine( HB_COMP_PARAM->pLex->pPP, pDefine->szName );
      else
         hb_pp_addDefine( HB_COMP_PARAM->pLex->pPP, pDefine->szName, pDefine->szValue );
      pDefine = pDefine->pNext;
   }
}
