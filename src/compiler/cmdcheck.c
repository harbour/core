/*
 * Harbour Project source code:
 * Compiler command line and HARBOURCMD/CLIPPERCMD checking
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at https://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
 *    hb_compChkCompilerSwitch()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_compChkEnvironVar()
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    PackDateTime()
 *    hb_compChkDefineSwitch()
 *    hb_compChkDefines()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbcomp.h"
#include "hbdate.h"

/* TODO: Add support for this compiler switches
   -r -t || hb_GetEnv( "TMP" )
 */


/* NOTE: Making the date and time info to fit into 32 bits can only be done
         in a "lossy" way, in practice that means it's not possible to unpack
         the exact date/time info from the resulting HB_ULONG. Since the year
         is only stored in 6 bits, 1980 will result in the same bit pattern
         as 2044. The purpose of this value is only used to *differenciate*
         between the dates ( the exact dates are not significant ), so this
         can be used here without problems. [vszakats] */

/* 76543210765432107654321076543210
   |.......|.......|.......|.......
   |____|                               Year    6 bits
   .     |__|                           Month   4 bits
   .         |___|                      Day     5 bits
   .              |___|                 Hour    5 bits
   .                   |____|           Minute  6 bits
   .                         |____|     Second  6 bits */

static HB_ULONG PackDateTime( void )
{
   union
   {
      struct
      {
         HB_U32 second : 6;         /* bits:  0 -  5 */
         HB_U32 minute : 6;         /* bits:  6 - 11 */
         HB_U32 hour   : 5;         /* bits: 12 - 16 */
         HB_U32 day    : 5;         /* bits: 16 - 21 */
         HB_U32 month  : 4;         /* bits: 22 - 25 */
         HB_U32 year   : 6;         /* bits: 26 - 31 */
      } ts;
      HB_U32 val;
   } u;
   int iYear, iMonth, iDay, iHour, iMinute, iSecond, iMillisec;

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSecond, &iMillisec );

   u.ts.year   = iYear - 1980;
   u.ts.month  = iMonth;
   u.ts.day    = iDay;
   u.ts.hour   = iHour;
   u.ts.minute = iMinute;
   u.ts.second = iSecond;

   return u.val;
}

static void hb_notSupportedInfo( HB_COMP_DECL, const char * szSwitch )
{
   char buffer[ 512 ];

   hb_snprintf( buffer, sizeof( buffer ),
                "Not yet supported command line option: %s\n", szSwitch );

   hb_compOutStd( HB_COMP_PARAM, buffer );
}

static void hb_compChkEnvironVar( HB_COMP_DECL, const char * szSwitch )
{
   if( szSwitch && ! HB_COMP_PARAM->fExit )
   {
      const char * s = szSwitch;

      /* If szSwitch doesn't start with a HB_OSOPTSEP char
       * show an error
       */
      if( ! HB_ISOPTSEP( *s ) )
         hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
      else
      {
         s++;
         switch( *s )
         {
            case 'a':
            case 'A':
               if( *( s + 1 ) == '-' )
                  HB_COMP_PARAM->fAutoMemvarAssume = HB_FALSE;
               else
                  HB_COMP_PARAM->fAutoMemvarAssume = HB_TRUE;
               break;

            case 'b':
            case 'B':
            {
               unsigned int i = 0;
               char *szOption = hb_strupr( hb_strdup( s ) );

               while( i < strlen( szOption ) && ! HB_ISOPTSEP( szOption[ i ] ) )
                  i++;
               szOption[ i ] = '\0';

               if( strcmp( szOption, "BUILD" ) == 0 )
                  HB_COMP_PARAM->fBuildInfo = HB_TRUE;
               else
               {
                  if( *( s + 1 ) == '-' )
                     HB_COMP_PARAM->fDebugInfo = HB_FALSE;
                  else
                  {
                     HB_COMP_PARAM->fDebugInfo = HB_TRUE;
                     HB_COMP_PARAM->fLineNumbers = HB_TRUE;
                  }
               }

               hb_xfree( szOption );
               break;
            }

            case 'c':
            case 'C':
            {
               unsigned int i = 0;
               char *szOption = hb_strupr( hb_strdup( s ) );

               while( i < strlen( szOption ) && ! HB_ISOPTSEP( szOption[ i ] ) )
                  i++;
               szOption[ i ] = '\0';

               if( strcmp( szOption, "CREDITS" ) == 0 ||
                   strcmp( szOption, "CREDIT" ) == 0 ||
                   strcmp( szOption, "CREDI" ) == 0 ||
                   strcmp( szOption, "CRED" ) == 0 )
                  HB_COMP_PARAM->fCredits = HB_TRUE;
               else
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, szOption, NULL );

               hb_xfree( szOption );
               break;
            }

            case 'd':
            case 'D':
               /* NOTE: Ignore these -d switches will be processed separately */
               break;

            case 'e':
            case 'E':
               if( *( s + 1 ) == 's' || *( s + 1 ) == 'S' )
               {
                  switch( *( s + 2 ) )
                  {
                     case '\0':
                     case '0':
                        HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_DEFAULT;
                        break;

                     case '1':
                        HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_SETEXIT;
                        break;

                     case '2':
                        HB_COMP_PARAM->iExitLevel = HB_EXITLEVEL_DELTARGET;
                        break;

                     default:
                        hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                  }
               }
               else
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );

               break;

            case 'g':
            case 'G':
               switch( *( s + 1 ) )
               {
                  case 'c':
                  case 'C':
                     HB_COMP_PARAM->iLanguage = HB_LANG_C;

                     switch( *( s + 2 ) )
                     {
                        case '3':
                           HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_REALCODE;
                           break;

                        case '2':
                           HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_VERBOSE;
                           break;

                        case '1':
                           HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_NORMAL;
                           break;

                        case '\0':
                        case '0':
                           HB_COMP_PARAM->iGenCOutput = HB_COMPGENC_COMPACT;
                           break;

                        default:
                           hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                     }
                     break;

                  case 'h':
                  case 'H':
                     HB_COMP_PARAM->iLanguage = HB_LANG_PORT_OBJ;
                     break;

                  case 'd':
                  case 'D':
                     if( HB_COMP_PARAM->szDepExt )
                     {
                        hb_xfree( HB_COMP_PARAM->szDepExt );
                        HB_COMP_PARAM->szDepExt = NULL;
                     }
                     if( s[ 2 ] == '-' )
                        HB_COMP_PARAM->iTraceInclude = 0;
                     else if( s[ 2 ] == '.' || s[ 2 ] == '\0' )
                     {
                        HB_COMP_PARAM->iTraceInclude = 2;
                        if( s[ 2 ] != '\0' )
                           HB_COMP_PARAM->szDepExt = hb_strdup( s + 2 );
                     }
                     else
                        hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                     break;

                  case 'e':
                  case 'E':
                     switch( *( s + 2 ) )
                     {
                        case '1':
                           HB_COMP_PARAM->iErrorFmt = HB_ERRORFMT_IDE;
                           break;

                        case '\0':
                        case '0':
                           HB_COMP_PARAM->iErrorFmt = HB_ERRORFMT_CLIPPER;
                           break;

                        default:
                           hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                     }
                     break;

                  default:
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_UNSUPPORTED_LANG, NULL, NULL );
                     break;
               }
               break;

               /* NOTE:
                  h or H from HELP or help
                */
            case 'h':
            case 'H':
            case '?':
               break;

               /* NOTE:
                  It already has support for several include files
                */
            case 'i':
            case 'I':
               switch( *( s + 1 ) )
               {
                  case '-':
                     HB_COMP_PARAM->fINCLUDE = HB_FALSE;
                     break;

                  case '+':
                     HB_COMP_PARAM->fINCLUDE = HB_TRUE;
                     break;

                  default:
                     hb_pp_addSearchPath( HB_COMP_PARAM->pLex->pPP, s + 1, HB_FALSE );
               }
               break;

            case 'j':
            case 'J':
               HB_COMP_PARAM->fI18n = HB_TRUE;
               if( s[ 1 ] )
                  HB_COMP_PARAM->pI18nFileName = hb_fsFNameSplit( s + 1 );
               break;

            case 'k':
            case 'K':
            {
               int i = 1;

               while( s[ i ] && ! HB_COMP_PARAM->fExit )
               {
                  switch( s[ i++ ] )
                  {
                     case '?':
                        hb_compPrintLogo( HB_COMP_PARAM );
                        hb_compPrintModes( HB_COMP_PARAM );
                        HB_COMP_PARAM->fLogo = HB_FALSE;
                        HB_COMP_PARAM->fQuiet = HB_TRUE;
                        break;

                     case 'h':
                     case 'H':
                        /* default Harbour mode */
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_HARBOUR;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_HARBOUR;
                        break;

                     case 'c':
                     case 'C':
                        /* clear all flags - minimal set of features */
                        HB_COMP_PARAM->supported &= HB_COMPFLAG_SHORTCUTS;
                        HB_COMP_PARAM->supported |= HB_COMPFLAG_OPTJUMP |
                                                    HB_COMPFLAG_MACROTEXT;
                        break;

                     case 'x':
                     case 'X':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_XBASE;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_XBASE;
                        break;

                     case 'i':
                     case 'I':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_HB_INLINE;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_HB_INLINE;
                        break;

                     case 'j':
                     case 'J':
                        if( s[ i ] == '+' )
                        {
                           i++;
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_OPTJUMP;
                        }
                        else
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_OPTJUMP;
                        break;

                     case 'm':
                     case 'M':
                        if( s[ i ] == '+' )
                        {
                           i++;
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_MACROTEXT;
                        }
                        else
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_MACROTEXT;
                        break;

                     case 'd':
                     case 'D':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_MACRODECL;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_MACRODECL;
                        break;

                     case 'r':
                     case 'R':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_RT_MACRO;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_RT_MACRO;
                        break;

                     case 's':
                     case 'S':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_ARRSTR;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_ARRSTR;
                        break;

                     case 'o':
                     case 'O':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_EXTOPT;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_EXTOPT;
                        break;

                     case 'u':
                     case 'U':
                        if( s[ i ] == '-' )
                        {
                           i++;
                           HB_COMP_PARAM->supported &= ~HB_COMPFLAG_USERCP;
                        }
                        else
                           HB_COMP_PARAM->supported |= HB_COMPFLAG_USERCP;
                        break;

                     default:
                        hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                        break;
                  }
               }
               break;
            }

            case 'l':
            case 'L':
               if( *( s + 1 ) == '-' )
                  HB_COMP_PARAM->fLineNumbers = HB_TRUE;
               else
                  HB_COMP_PARAM->fLineNumbers = HB_FALSE;
               break;

            case 'm':
            case 'M':
               if( *( s + 1 ) == '-' )
                  HB_COMP_PARAM->fSingleModule = HB_FALSE;
               else
                  HB_COMP_PARAM->fSingleModule = HB_TRUE;
               break;

            case 'n':
            case 'N':
               HB_COMP_PARAM->fNoStartUp = s[ 1 ] == '1';
               switch( s[ 1 ] )
               {
                  case '-':
                     HB_COMP_PARAM->iStartProc = 0;
                     break;
                  case '\0':
                  case '0':
                  case '1':
                     HB_COMP_PARAM->iStartProc = 1;
                     break;
                  case '2':
                     HB_COMP_PARAM->iStartProc = 2;
                     break;
                  default:
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
               }
               break;

            case 'o':
            case 'O':
               HB_COMP_PARAM->pOutPath = hb_fsFNameSplit( s + 1 );
               break;

               /* Added for preprocessor needs */
            case 'p':
            case 'P':
               if( s[ 1 ] == '+' && s[ 2 ] == '\0' )
                  HB_COMP_PARAM->fPPT = HB_TRUE;
               else
               {
                  if( HB_COMP_PARAM->pPpoPath )
                  {
                     hb_xfree( HB_COMP_PARAM->pPpoPath );
                     HB_COMP_PARAM->pPpoPath = NULL;
                  }
                  if( s[ 1 ] == '-' && s[ 2 ] == '\0' )
                     HB_COMP_PARAM->fPPO = HB_FALSE;
                  else
                  {
                     if( s[ 1 ] )
                        HB_COMP_PARAM->pPpoPath = hb_fsFNameSplit( s + 1 );
                     HB_COMP_PARAM->fPPO = HB_TRUE;
                  }
               }
               break;

            case 'q':
            case 'Q':
               switch( *( s + 1 ) )
               {
                  case '2':
                     HB_COMP_PARAM->fFullQuiet = HB_TRUE;
                  case '0':
                     HB_COMP_PARAM->fLogo = HB_FALSE;
                  default:
                     HB_COMP_PARAM->fQuiet = HB_TRUE;
               }
               break;

            case 'r':
            case 'R':
               if( *( s + 1 ) == ':' )
               {
                  int iOverflow;
                  int iCycles = ( int ) hb_strValInt( s + 2, &iOverflow );

                  if( ! iOverflow && iCycles > 0 )
                     HB_COMP_PARAM->iMaxTransCycles = iCycles;
               }
               else
               {
                  /* TODO: Implement this switch */
                  hb_notSupportedInfo( HB_COMP_PARAM, s );
               }
               break;

            case 's':
            case 'S':
               switch( *( s + 1 ) )
               {
                  case '\0':
                     HB_COMP_PARAM->iSyntaxCheckOnly = 1;
                     break;
                  case '-':
                     HB_COMP_PARAM->iSyntaxCheckOnly = 0;
                     break;
                  case 'm':
                  case 'M':
                     if( s[ 2 ] == '\0' )
                     {
                        HB_COMP_PARAM->iSyntaxCheckOnly = 2;
                        break;
                     }
                  default:
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
               }
               break;

            case 't':
            case 'T':
               /* TODO: Implement this switch */
               hb_notSupportedInfo( HB_COMP_PARAM, s );
               break;

            case 'u':
            case 'U':
               if( ( s[ 1 ] == 'N' || s[ 1 ] == 'n' ) &&
                   ( s[ 2 ] == 'D' || s[ 2 ] == 'd' ) &&
                   ( s[ 3 ] == 'E' || s[ 3 ] == 'e' ) &&
                   ( s[ 4 ] == 'F' || s[ 4 ] == 'f' ) && s[ 5 ] == ':' )
               {
                  /* NOTE: Ignore these -undef: switches (will be processed
                   *       separately) except -undef:.arch.
                   */
                  if( s[ 6 ] == '.' &&
                      ( s[  7 ] == 'A' || s[  7 ] == 'a' ) &&
                      ( s[  8 ] == 'R' || s[  8 ] == 'r' ) &&
                      ( s[  9 ] == 'C' || s[  9 ] == 'c' ) &&
                      ( s[ 10 ] == 'H' || s[ 10 ] == 'h' ) &&
                      s[ 11 ] == '.' )
                  {
                     HB_COMP_PARAM->fNoArchDefs = HB_TRUE;
                  }
                  break;
               }
               /* extended definitions file (-u+<file>) */
               if( s[ 1 ] == '+' )
               {
                  if( s[ 2 ] )
                  {
                     if( HB_COMP_PARAM->iStdChExt == 0 )
                        HB_COMP_PARAM->szStdChExt = ( char ** )
                           hb_xgrab( sizeof( char * ) );
                     else
                        HB_COMP_PARAM->szStdChExt = ( char ** )
                           hb_xrealloc( HB_COMP_PARAM->szStdChExt,
                                        ( HB_COMP_PARAM->iStdChExt + 1 ) *
                                        sizeof( char * ) );
                     HB_COMP_PARAM->szStdChExt[ HB_COMP_PARAM->iStdChExt++ ] =
                           hb_strdup( s + 2 );
                  }
                  else
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
               }
               else
               {
                  if( HB_COMP_PARAM->szStdCh )
                     hb_xfree( HB_COMP_PARAM->szStdCh );
                  HB_COMP_PARAM->szStdCh = hb_strdup( s + 1 );
               }
               break;

            case 'v':
            case 'V':
               if( *( s + 1 ) == '-' )
                  HB_COMP_PARAM->fForceMemvars = HB_FALSE;
               else
                  HB_COMP_PARAM->fForceMemvars = HB_TRUE;
               break;

            case 'w':
            case 'W':
               HB_COMP_PARAM->iWarnings = 1;
               if( s[ 1 ] )       /* there is -w<0,1,2,3> probably */
               {
                  HB_COMP_PARAM->iWarnings = s[ 1 ] - '0';
                  if( HB_COMP_PARAM->iWarnings < 0 || HB_COMP_PARAM->iWarnings > 3 )
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
               }
               break;

            case 'x':
            case 'X':
            {
               unsigned int i = 1;
               while( s[ i ] && ! HB_ISOPTSEP( s[ i ] ) &&
                      i < sizeof( HB_COMP_PARAM->szPrefix ) - 1 )
               {
                  ++i;
               }
               if( i > 1 )
               {
                  memcpy( HB_COMP_PARAM->szPrefix, s + 1, i - 1 );
                  HB_COMP_PARAM->szPrefix[ i - 1 ] = '_';
                  HB_COMP_PARAM->szPrefix[ i ] = '\0';
               }
               else
               {
                  hb_snprintf( HB_COMP_PARAM->szPrefix,
                               sizeof( HB_COMP_PARAM->szPrefix ),
                               "%08lX_", PackDateTime() );
               }
               break;
            }

#ifdef YYDEBUG
            case 'y':
            case 'Y':
               yydebug = HB_TRUE;
               break;
#endif

            case 'z':
            case 'Z':
               if( *( s + 1 ) == '-' )
                  HB_COMP_PARAM->supported |= HB_COMPFLAG_SHORTCUTS;
               else
                  HB_COMP_PARAM->supported &= ~HB_COMPFLAG_SHORTCUTS;
               break;

            default:
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
               break;
         }
      }
   }
}

void hb_compChkCompilerSwitch( HB_COMP_DECL, int iArg, const char * const Args[] )
{
   /* If iArg is passed check the command line options */
   if( iArg )
   {
      int i;

      /* Check all switches in command line
         They start with an OS_OPT_DELIMITER char
       */
      for( i = 1; i < iArg && ! HB_COMP_PARAM->fExit; i++ )
      {
         const char * szSwitch = Args[ i ];

         if( ! HB_ISOPTSEP( szSwitch[ 0 ] ) )
            continue;

         if( szSwitch[ 0 ] == '-' )
         {
            int  j = 1;
            char Switch[ 7 ];

            Switch[ 0 ] = '-';

            while( szSwitch[ j ] && ! HB_COMP_PARAM->fExit )
            {
               Switch[ 1 ] = szSwitch[ j ];

               if( szSwitch[ j + 1 ] == '-' )
               {
                  Switch[ 2 ] = '-';
                  Switch[ 3 ] = '\0';

                  hb_compChkEnvironVar( HB_COMP_PARAM, Switch );

                  j += 2;
                  continue;
               }
               else
               {
                  switch( Switch[ 1 ] )
                  {
                     case 'b':
                     case 'B':
                        if( ( szSwitch[ j + 1 ] == 'U' || szSwitch[ j + 1 ] == 'u' ) &&
                            ( szSwitch[ j + 2 ] == 'I' || szSwitch[ j + 2 ] == 'i' ) &&
                            ( szSwitch[ j + 3 ] == 'L' || szSwitch[ j + 3 ] == 'l' ) &&
                            ( szSwitch[ j + 4 ] == 'D' || szSwitch[ j + 4 ] == 'd' ) )
                        {
                           Switch[ 2 ] = 'U';
                           Switch[ 3 ] = 'I';
                           Switch[ 4 ] = 'L';
                           Switch[ 5 ] = 'D';
                           Switch[ 6 ] = '\0';

                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );

                           j += 5;
                           continue;
                        }
                        else if( ! szSwitch[ j + 1 ] )
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                           j += 1;
                           continue;
                        }
                        break;

                     case 'c':
                     case 'C':
                        if( ( szSwitch[ j + 1 ] == 'R' || szSwitch[ j + 1 ] == 'r' ) &&
                            ( szSwitch[ j + 2 ] == 'E' || szSwitch[ j + 2 ] == 'e' ) &&
                            ( szSwitch[ j + 3 ] == 'D' || szSwitch[ j + 3 ] == 'd' ) )
                        {
                           Switch[ 2 ] = 'R';
                           Switch[ 3 ] = 'E';
                           Switch[ 4 ] = 'D';
                           Switch[ 5 ] = '\0';

                           j += 4;

                           if( szSwitch[ j ] == 'I' || szSwitch[ j ] == 'i' )
                           {
                              j++;
                              if( szSwitch[ j ] == 'T' || szSwitch[ j ] == 't' )
                              {
                                 j++;
                                 if( szSwitch[ j ] == 'S' || szSwitch[ j ] == 's' )
                                 {
                                    j++;
                                 }
                              }
                           }
                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, Switch, NULL );
                        }
                        continue;

                     case 'd':
                     case 'D':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as part of #define and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'e':
                     case 'E':
                        if( ( szSwitch[ j + 1 ] == 'S' || szSwitch[ j + 1 ] == 's' ) &&
                            HB_ISDIGIT( szSwitch[ j + 2 ] ) )
                        {
                           Switch[ 2 ] = 'S';
                           Switch[ 3 ] = szSwitch[ j + 2 ];
                           Switch[ 4 ] = '\0';

                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                           j += 3;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, Switch, NULL );
                        }
                        continue;

                     case 'g':
                     case 'G':
                        if( szSwitch[ j + 1 ] == 'd' || szSwitch[ j + 1 ] == 'D' )
                        {
                           szSwitch += ( j - 1 );
                           hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );
                           j = ( int ) strlen( szSwitch );
                        }
                        else
                        {
                           /* Required argument */
                           Switch[ 2 ] = szSwitch[ j + 1 ];
                           if( Switch[ 2 ] )
                           {
                              if( HB_ISDIGIT( szSwitch[ j + 2 ] ) )
                              {
                                 /* Optional argument */
                                 Switch[ 3 ] = szSwitch[ j + 2 ];
                                 Switch[ 4 ] = '\0';
                                 j += 3;
                              }
                              else
                              {
                                 /* No optional argument */
                                 Switch[ 3 ] = '\0';
                                 j += 2;
                              }
                              hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                           }
                           else
                              hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, Switch, NULL );
                        }
                        continue;

                     case 'i':
                     case 'I':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as IncludePath and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'j':
                     case 'J':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'k':
                     case 'K':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as part of #define and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'n':
                     case 'N':
                        /* Required argument */
                        if( szSwitch[ j + 1 ] )
                        {
                           /* Optional argument */
                           Switch[ 2 ] = szSwitch[ j + 1 ];
                           Switch[ 3 ] = '\0';
                           j += 2;
                        }
                        else
                        {
                           /* No optional argument */
                           Switch[ 2 ] = '\0';
                           j += 1;
                        }
                        hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                        continue;

                     case 'o':
                     case 'O':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as OutputPath and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'p':
                     case 'P':
                        if( szSwitch[ j + 1 ] )
                        {
                           szSwitch += ( j - 1 );
                           hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                           /* Accept rest as PPOPath and continue with next Args[]. */
                           j += ( int ) strlen( szSwitch ) - 1;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                           j++;
                        }
                        continue;

                     case 'q':
                     case 'Q':
                        if( HB_ISDIGIT( szSwitch[ j + 1 ] ) )
                        {
                           Switch[ 2 ] = szSwitch[ j + 1 ];
                           Switch[ 3 ] = '\0';

                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );

                           j += 2;
                           continue;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                        }

                        break;

                     case 'r':
                     case 'R':
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );
                        j = ( int ) strlen( szSwitch ) - 1;
                        break;

                     case 's':
                     case 'S':
                        ++j;
                        Switch[ 2 ] = Switch[ 3 ] = Switch[ 4 ] = '\0';
                        if( szSwitch[ j ] == 'm' || szSwitch[ j ] == 'M' )
                        {
                           Switch[ 2 ] = szSwitch[ j++ ];
                           if( HB_ISDIGIT( szSwitch[ j ] ) || szSwitch[ j ] == '-' )
                              Switch[ 3 ] = szSwitch[ j++ ];
                        }
                        hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                        continue;

                     case 'u':
                     case 'U':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as part of .ch Path or "undef:<id>" and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case 'w':
                     case 'W':
                        if( HB_ISDIGIT( szSwitch[ j + 1 ] ) )
                        {
                           Switch[ 2 ] = szSwitch[ j + 1 ];
                           Switch[ 3 ] = '\0';

                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );

                           j += 2;
                           continue;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                        }

                        break;

                     case 'x':
                     case 'X':
                        szSwitch += ( j - 1 );
                        hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );

                        /* Accept rest as INIT Symbol and continue with next Args[]. */
                        j = ( int ) strlen( szSwitch );
                        continue;

                     case '-':
                     {
                        int l = ++j;
                        while( szSwitch[ j ] && ! HB_ISOPTSEP( szSwitch[ j ] ) )
                           j++;
                        if( szSwitch[ l - 1 ] == '-' && j - l == 7 &&
                            memcmp( &szSwitch[ l ], "version", 7 ) == 0 )
                        {
                           HB_COMP_PARAM->fLogo = HB_TRUE;
                           HB_COMP_PARAM->fQuiet = HB_TRUE;
                        }
                        else if( szSwitch[ l - 1 ] == '-' && j - l == 4 &&
                                 memcmp( &szSwitch[ l ], "help", 4 ) == 0 )
                        {
                           HB_COMP_PARAM->fLogo = HB_TRUE;
                           HB_COMP_PARAM->fQuiet = HB_FALSE;
                           HB_COMP_PARAM->fExit = HB_FALSE;
                        }
                        else
                           hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, &szSwitch[ l ], NULL );
                        if( szSwitch[ j ] )
                           ++j;
                        continue;
                     }
                     default:
                        Switch[ 2 ] = '\0';
                        hb_compChkEnvironVar( HB_COMP_PARAM, Switch );
                  }
               }

               j++;
            }
            continue;
         }

         while( ! HB_COMP_PARAM->fExit )
         {
            int j = 1;
            const char * szSwitch1 = szSwitch + j; /* hack to avoid what is seems a bug in 'Apple clang version 2.1 (tags/Apple/clang-163.7.1) (based on LLVM 3.0svn)' / XCode 4.1 [vszakats] */

            while( *szSwitch1 && ! HB_ISOPTSEP( *szSwitch1 ) )
            {
               j++;
               szSwitch1++;
            }

            if( szSwitch[ j ] == '/' )
            {
               char * szTmp = hb_strndup( szSwitch, j );
               hb_compChkEnvironVar( HB_COMP_PARAM, szTmp );
               hb_xfree( szTmp );
               szSwitch += j;
            }
            else
            {
               hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );
               break;
            }
         }
      }
   }
   else /* Check the environment variables */
   {
      /* NOTE: CLIPPERCMD enviroment variable
         is overriden if HARBOURCMD exists
       */
      char * szStrEnv = hb_getenv( "HARBOURCMD" );

      if( ! szStrEnv || szStrEnv[ 0 ] == '\0' )
      {
         if( szStrEnv )
            hb_xfree( szStrEnv );

         szStrEnv = hb_getenv( "CLIPPERCMD" );
      }

      if( szStrEnv )
      {
         char * szSwitch, * szPtr;

         szPtr = szStrEnv;
         while( *szPtr && ! HB_COMP_PARAM->fExit )
         {
            while( *szPtr == ' ' )
               ++szPtr;
            szSwitch = szPtr;
            if( *szSwitch )
            {
               while( *++szPtr )
               {
                  if( *szPtr == ' ' )
                  {
                     *szPtr++ = '\0';
                     break;
                  }
               }
               hb_compChkEnvironVar( HB_COMP_PARAM, szSwitch );
            }
         }
         hb_xfree( szStrEnv );
      }
   }
}

void hb_compChkPaths( HB_COMP_DECL )
{
   char * szInclude = hb_getenv( "INCLUDE" );

   if( szInclude )
   {
      if( szInclude[ 0 ] != '\0' )
         hb_pp_addSearchPath( HB_COMP_PARAM->pLex->pPP, szInclude, HB_FALSE );
      hb_xfree( szInclude );
   }
}

static void hb_compChkDefineSwitch( HB_COMP_DECL, const char * pszSwitch )
{
   if( pszSwitch && HB_ISOPTSEP( pszSwitch[ 0 ] ) )
   {
      if( pszSwitch[ 1 ] == 'd' || pszSwitch[ 1 ] == 'D' )
      {
         if( pszSwitch[ 2 ] )
         {
            char * szDefText = hb_strdup( pszSwitch + 2 ), * szAssign;

            szAssign = strchr( szDefText, '=' );
            if( szAssign )
               *szAssign++ = '\0';
            hb_pp_addDefine( HB_COMP_PARAM->pLex->pPP, szDefText, szAssign );
            hb_xfree( szDefText );
         }
      }
      else if( ( pszSwitch[ 1 ] == 'U' || pszSwitch[ 1 ] == 'u' ) &&
               ( pszSwitch[ 2 ] == 'N' || pszSwitch[ 2 ] == 'n' ) &&
               ( pszSwitch[ 3 ] == 'D' || pszSwitch[ 3 ] == 'd' ) &&
               ( pszSwitch[ 4 ] == 'E' || pszSwitch[ 4 ] == 'e' ) &&
               ( pszSwitch[ 5 ] == 'F' || pszSwitch[ 5 ] == 'f' ) &&
               pszSwitch[ 6 ] == ':' )
      {
         char *szDefText = hb_strdup( pszSwitch + 7 );
         unsigned int i = 0;

         while( szDefText[ i ] && ! HB_ISOPTSEP( szDefText[ i ] ) )
         {
            i++;
         }
         szDefText[ i ] = '\0';

         if( szDefText[ 0 ] )
         {
            if( hb_stricmp( szDefText, ".ARCH." ) == 0 )
               hb_pp_delDefine( HB_COMP_PARAM->pLex->pPP, szDefText );
         }
         hb_xfree( szDefText );
      }
   }
}

void hb_compChkDefines( HB_COMP_DECL, int iArg, const char * const Args[] )
{
   /* Check the environment variables */
   {
      /* NOTE: CLIPPERCMD enviroment variable is overriden
         if HARBOURCMD exists */
      char * szStrEnv = hb_getenv( "HARBOURCMD" );

      if( ! szStrEnv || szStrEnv[ 0 ] == '\0' )
      {
         if( szStrEnv )
            hb_xfree( szStrEnv );

         szStrEnv = hb_getenv( "CLIPPERCMD" );
      }

      if( szStrEnv )
      {
         char * szSwitch, * szPtr;

         szPtr = szStrEnv;
         while( *szPtr && ! HB_COMP_PARAM->fExit )
         {
            while( *szPtr == ' ' )
               ++szPtr;
            szSwitch = szPtr;
            if( *szSwitch )
            {
               while( *++szPtr )
               {
                  if( *szPtr == ' ' )
                  {
                     *szPtr++ = '\0';
                     break;
                  }
               }
               hb_compChkDefineSwitch( HB_COMP_PARAM, szSwitch );
            }
         }
         hb_xfree( szStrEnv );
      }
   }

   /* Check the command line options */
   {
      int i;

      /* Check all switches in command line They start with an OS_OPT_DELIMITER
         char */
      for( i = 1; i < iArg; i++ )
         hb_compChkDefineSwitch( HB_COMP_PARAM, Args[ i ] );
   }
}
