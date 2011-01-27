/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Support for #pragma directive and related functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
 * along with this software; see the file COPYING.  If not, write to
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

/*
 * See doc/pragma.txt to learn more about Harbour pragmas.
 */

#include "hbppdef.h"
#include "hbcomp.h"

static HB_BOOL StringToBool( char *, HB_BOOL );
static int  StringToInt( char *, int );
static HB_BOOL IsOnOffSwitch( char *, HB_BOOL );
static void DebugPragma( char *, int, HB_BOOL );

static HB_BOOL s_bTracePragma = HB_FALSE;

/* Size of abreviated pragma commands */
#define PRAGMAS_LEN 8

/* TODO:  Add support for:
          RequestLib    /R
 */

static PINLINE hb_compInlineAdd_( char * szFunName )
{
   HB_SYMBOL_UNUSED( szFunName );
   return NULL;
}

HB_BOOL hb_pp_ParsePragma( char * szLine )
{
   HB_BOOL bIgnore = HB_TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_ParsePragma(%s)", szLine ) );

   HB_SKIPTABSPACES( szLine );

   if( HB_ISOPTSEP( szLine[ 0 ] ) )
   {
      switch( szLine[ 1 ] )
      {
         case 'a':
         case 'A':
            hb_comp_bAutoMemvarAssume = IsOnOffSwitch( szLine, hb_comp_bAutoMemvarAssume );
            DebugPragma( szLine, -1, hb_comp_bAutoMemvarAssume );
            break;

         case 'b':
         case 'B':
            hb_comp_bDebugInfo     = IsOnOffSwitch( szLine, hb_comp_bDebugInfo );
            hb_comp_bLineNumbers   = hb_comp_bDebugInfo;
            DebugPragma( szLine, -1, hb_comp_bDebugInfo );
            break;

         case 'e':
         case 'E':

            if( szLine[ 2 ] == 's' ||
                szLine[ 2 ] == 'S' )
            {
               switch( szLine[ 3 ] )
               {
                  case '\0':
                  case '0':
                     hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT;
                     break;

                  case '1':
                     hb_comp_iExitLevel = HB_EXITLEVEL_SETEXIT;
                     break;

                  case '2':
                     hb_comp_iExitLevel = HB_EXITLEVEL_DELTARGET;
                     break;

                  default:
                     hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
               }
               DebugPragma( szLine, hb_comp_iExitLevel, HB_FALSE );
            }

            break;

         case 'l':
         case 'L':
            hb_comp_bLineNumbers = IsOnOffSwitch( szLine, hb_comp_bLineNumbers );
            DebugPragma( szLine, -1, hb_comp_bLineNumbers );
            break;

         case 'n':
         case 'N':
            hb_comp_bStartProc = IsOnOffSwitch( szLine, hb_comp_bStartProc );
            DebugPragma( szLine, -1, hb_comp_bStartProc );
            break;

         case 'p':
         case 'P':
            hb_comp_bPPO = IsOnOffSwitch( szLine, hb_comp_bPPO );
            DebugPragma( szLine, -1, hb_comp_bPPO );
            break;

         case 'v':
         case 'V':
            hb_comp_bForceMemvars = IsOnOffSwitch( szLine, hb_comp_bForceMemvars );
            DebugPragma( szLine, -1, hb_comp_bForceMemvars );
            break;

         case 'w':
         case 'W':
            if( szLine[ 2 ] != '\0' )
            {
               /* Check for +/- */
               if( szLine[ strlen( szLine ) - 1 ] == '+' ||
                   szLine[ strlen( szLine ) - 1 ] == '-' )
                  hb_comp_iWarnings = IsOnOffSwitch( szLine, hb_comp_iWarnings != 0 ) ? 1 : 0;
               else
               {
                  /* There is -w<0,1,2,3> probably */
                  hb_comp_iWarnings = szLine[ 2 ] - '0';
                  if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
                     hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );

                  DebugPragma( szLine, -1, hb_comp_iWarnings );
               }
            }
            break;

         case 'z':
         case 'Z':
            hb_comp_bShortCuts = IsOnOffSwitch( szLine, hb_comp_bShortCuts );
            DebugPragma( szLine, -1, hb_comp_bShortCuts );
            break;

         default:
            break;
      }
   }
   else
   {
      if( hb_strnicmp( szLine, "AUTOMEMVAR", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bAutoMemvarAssume = StringToBool( szLine, hb_comp_bAutoMemvarAssume );
         DebugPragma( szLine, -1, hb_comp_bAutoMemvarAssume );
      }
      else if( hb_strnicmp( szLine, "BEGINDUMP", PRAGMAS_LEN ) == 0 )
      {
         char     sBuffer[ HB_PP_STR_SIZE ], * pBuffer, sDirective[ 9 ];
         int      iSize, iOldSize;
         PINLINE  pInline;

         if( hb_comp_bPPO )
         {
            hb_pp_WrStr( hb_comp_yyppo, "#pragma BEGINDUMP" );
         }

         hb_pp_StreamBlock   = HB_PP_STREAM_DUMP_C;

         pInline             = hb_compInlineAdd_( NULL );

 DigestInline:

         iSize = hb_pp_Internal_( hb_comp_bPPO ? hb_comp_yyppo : NULL, sBuffer );
         if( iSize == 0 )
         {
            hb_pp_StreamBlock = 0;
            return bIgnore;
         }

         pBuffer = ( char * ) sBuffer;

         while( *pBuffer == ' ' || *pBuffer == '\t' )
         {
            pBuffer++;
         }
         if( *pBuffer == '#' )
         {
            pBuffer++;
            while( *pBuffer == ' ' || *pBuffer == '\t' )
            {
               pBuffer++;
            }
            hb_strncpyUpper( sDirective, pBuffer, 6 );
            if( memcmp( sDirective, "PRAGMA", 6 ) == 0 )
            {
               pBuffer += 6;
            }
            while( *pBuffer == ' ' || *pBuffer == '\t' )
            {
               pBuffer++;
            }
            hb_strncpyUpper( sDirective, pBuffer, 7 );
            if( memcmp( sDirective, "ENDDUMP", 7 ) == 0 )
            {
               hb_pp_StreamBlock = 0;
               return bIgnore;
            }
         }

         iOldSize   = 0;
         iSize      = strlen( ( char * ) sBuffer );
         if( pInline->pCode == NULL )
         {
            pInline->pCode = ( BYTE * ) hb_xgrab( iSize + 1 );
         }
         else
         {
            iOldSize         = strlen( ( char * ) pInline->pCode );
            pInline->pCode   = ( BYTE * ) hb_xrealloc( pInline->pCode, iOldSize + iSize + 1 );
         }
         memcpy( pInline->pCode + iOldSize, sBuffer, iSize + 1 );

         goto DigestInline;
      }
      else if( hb_strnicmp( szLine, "DEBUGINFO", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bDebugInfo     = StringToBool( szLine, hb_comp_bDebugInfo );
         hb_comp_bLineNumbers   = hb_comp_bDebugInfo;
         DebugPragma( szLine, -1, hb_comp_bDebugInfo );
      }
      else if( hb_strnicmp( szLine, "ENABLEWARNINGS", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToBool( szLine, hb_comp_iWarnings != 0 ) ? 1 : 0;
         DebugPragma( szLine, hb_comp_iWarnings, HB_FALSE );
      }
      else if( hb_strnicmp( szLine, "EXITSEVERITY", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iExitLevel = StringToInt( szLine, hb_comp_iExitLevel );
         if( hb_comp_iExitLevel != HB_EXITLEVEL_DEFAULT &&
             hb_comp_iExitLevel != HB_EXITLEVEL_SETEXIT &&
             hb_comp_iExitLevel != HB_EXITLEVEL_DELTARGET )
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( szLine, hb_comp_iExitLevel, HB_FALSE );
      }
      else if( hb_strnicmp( szLine, "DYNAMICMEMVAR", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bForceMemvars = StringToBool( szLine, hb_comp_bForceMemvars );
         DebugPragma( szLine, -1, hb_comp_bForceMemvars );
      }
      else if( hb_strnicmp( szLine, "LINENUMBER", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bLineNumbers = StringToBool( szLine, hb_comp_bLineNumbers );
         DebugPragma( szLine, -1, hb_comp_bLineNumbers );
      }
      else if( hb_strnicmp( szLine, "NOSTARTPROC", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bStartProc = StringToBool( szLine, hb_comp_bStartProc );
         DebugPragma( szLine, -1, hb_comp_bStartProc );
      }
      else if( hb_strnicmp( szLine, "PREPROCESSING", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bPPO = StringToBool( szLine, hb_comp_bPPO );
         DebugPragma( szLine, -1, hb_comp_bPPO );
      }
      else if( hb_strnicmp( szLine, "SHORTCUT", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bShortCuts = StringToBool( szLine, hb_comp_bShortCuts );
         DebugPragma( szLine, -1, hb_comp_bShortCuts );
      }
      else if( hb_strnicmp( szLine, "WARNINGLEVEL", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToInt( szLine, hb_comp_iWarnings );
         if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( szLine, hb_comp_iWarnings, HB_FALSE );
      }
      else if( hb_strnicmp( szLine, "TRACEPRAGMAS", PRAGMAS_LEN ) == 0 )
      {
         s_bTracePragma = StringToBool( szLine, s_bTracePragma );
         DebugPragma( szLine, -1, s_bTracePragma );
      }
      else if( hb_strnicmp( szLine, "__text", 6 ) == 0 )
      {
         bIgnore = hb_pp_StreamBlockBegin( szLine, HB_PP_STREAM_CLIPPER );
         DebugPragma( szLine, -1, s_bTracePragma );
      }
      else if( hb_strnicmp( szLine, "__stream", 8 ) == 0 )
      {
         bIgnore = hb_pp_StreamBlockBegin( szLine, HB_PP_STREAM_PRG );
         DebugPragma( szLine, -1, s_bTracePragma );
      }
      else if( hb_strnicmp( szLine, "__cstream", 8 ) == 0 )
      {
         bIgnore = hb_pp_StreamBlockBegin( szLine, HB_PP_STREAM_C );
         DebugPragma( szLine, -1, s_bTracePragma );
      }
      else if( hb_strnicmp( szLine, "__endtext", 9 ) == 0 )
      {
         hb_pp_BlockEnd();
         DebugPragma( szLine, -1, s_bTracePragma );
      }
      else if( hb_strnicmp( szLine, "RECURSELEVEL", PRAGMAS_LEN ) == 0 )
      {
         int   iOverflow;
         int   iMax;

         iMax = ( int ) hb_strValInt( szLine, &iOverflow );
         if( iOverflow || iMax < 1 )
            hb_pp_MaxTranslateCycles = 1024;
         else
            hb_pp_MaxTranslateCycles = ( unsigned int ) iMax;
         DebugPragma( szLine, hb_pp_MaxTranslateCycles, HB_FALSE );
      }
   }

   return bIgnore;
}

/* Checks for +/- within the string, sets bDefault if not found */
static HB_BOOL IsOnOffSwitch( char * pszStr, HB_BOOL bValue )
{
   int iPos = strlen( pszStr ) - 1;

   if( pszStr[ iPos ] == '+' )
      bValue = HB_TRUE;
   else if( pszStr[ iPos ] == '-' )
      bValue = HB_FALSE;

   return bValue;
}

/* Checks for ON/OFF within the string, sets bDefault if not found */
static HB_BOOL StringToBool( char * pszStr, HB_BOOL bValue )
{
   char * pos = strchr( pszStr, '=' );

   if( ! pos )
      pos = strchr( pszStr, '(' );

   if( pos )
   {
      pos++;

      HB_SKIPTABSPACES( pos );

      if( hb_strnicmp( pos, "ON", 2 ) == 0 )
         bValue = HB_TRUE;
      else if( hb_strnicmp( pos, "OFF", 3 ) == 0 )
         bValue = HB_FALSE;
   }

   return bValue;
}

/* Returns value after =, sets iDefault if not found */
static int StringToInt( char * pszStr, int iValue )
{
   char * pos = strchr( pszStr, '=' );

   if( ! pos )
      pos = strchr( pszStr, '(' );

   if( pos )
   {
      pos++;

      HB_SKIPTABSPACES( pos );

      if( *pos >= '0' && *pos <= '9' )
         iValue = *pos - '0';
   }

   return iValue;
}

/* This is only to debug pragmas now */
static void DebugPragma( char * pszStr, int iValue, HB_BOOL bValue )
{
   if( s_bTracePragma )
   {
      if( iValue >= 0 )
         printf( "#pragma set to %i \'%s\'\n", iValue, pszStr );
      else
         printf( "#pragma set to %s \'%s\'\n", bValue ? "ON" : "OFF", pszStr );
   }
}
