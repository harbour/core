/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Support for #pragma directive and related functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
 * See doc/pragma.txt to learn more about Harbour pragmas.
 */

#include "hbpp.h"
#include "hbcomp.h"

static BOOL StringToBool( char *, BOOL );
static int  StringToInt( char *, int );
static BOOL IsOnOffSwitch( char *, BOOL );
static void DebugPragma( char *, int, BOOL );

static BOOL s_bTracePragma = FALSE;

/* Size of abreviated pragma commands */
#define PRAGMAS_LEN       8

#if defined(__WATCOMC__)
  extern BOOL hb_pp_bInline;
#endif

/* TODO:  Add support for:
          RequestLib    /R
*/

void hb_pp_ParsePragma( char * szLine )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_pp_ParsePragma(%s)", szLine));

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
            hb_comp_bDebugInfo = IsOnOffSwitch( szLine, hb_comp_bDebugInfo );
            hb_comp_bLineNumbers = hb_comp_bDebugInfo;
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
                     hb_compGenError( hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
               }
               DebugPragma( szLine, hb_comp_iExitLevel, FALSE );
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
                     hb_compGenError( hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );

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
      hb_strupr( szLine );

      if( memcmp( szLine, "AUTOMEMVAR", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bAutoMemvarAssume = StringToBool( szLine, hb_comp_bAutoMemvarAssume );
         DebugPragma( szLine, -1, hb_comp_bAutoMemvarAssume );
      }
      else if( memcmp( szLine, "BEGINDUMP", PRAGMAS_LEN ) == 0 )
      {
         char sBuffer[ HB_PP_STR_SIZE ], *pBuffer, sDirective[9] ;
         int iSize;
#ifndef __WATCOMC__
         extern BOOL hb_pp_bInline;
#endif
         PINLINE pInline;

         if( hb_comp_bPPO )
         {
            hb_pp_WrStr( hb_comp_yyppo, "#pragma BEGINDUMP" );
         }

         hb_pp_bInline = TRUE;

         pInline = hb_compInlineAdd( NULL );

       DigestInline :

         iSize = hb_pp_Internal( hb_comp_bPPO ? hb_comp_yyppo : NULL, sBuffer );
         if( iSize == 0 )
         {
            hb_pp_bInline = FALSE;
            return;
         }

         pBuffer = (char*) sBuffer;

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
            hb_strupr( strncpy( sDirective, pBuffer, 6 ) );
            if( memcmp( sDirective, "PRAGMA", 6 ) == 0 )
            {
               pBuffer += 6;
            }
            while( *pBuffer == ' ' || *pBuffer == '\t' )
            {
               pBuffer++;
            }
            hb_strupr( strncpy( sDirective, pBuffer, 8 ) );
            if( memcmp( sDirective, "STOPDUMP", 8 ) == 0 )
            {
               hb_pp_bInline = FALSE;
               return;
            }
         }

         if( pInline->pCode == NULL )
         {
            pInline->pCode = (BYTE *) hb_xgrab( ( iSize = strlen( (char*) sBuffer ) ) + 1 );
            strcpy( (char*) pInline->pCode, (char*) sBuffer );
         }
         else
         {
            pInline->pCode = (BYTE *) hb_xrealloc( pInline->pCode, pInline->lPCodeSize + ( iSize = strlen( (char*) sBuffer ) ) + 1 );
            strcpy( (char *) (pInline->pCode + pInline->lPCodeSize), (char*) sBuffer );
         }
         pInline->lPCodeSize += iSize;

         goto DigestInline;
      }
      else if( memcmp( szLine, "DEBUGINFO", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bDebugInfo = StringToBool( szLine, hb_comp_bDebugInfo );
         hb_comp_bLineNumbers = hb_comp_bDebugInfo;
         DebugPragma( szLine, -1, hb_comp_bDebugInfo );
      }
      else if( memcmp( szLine, "ENABLEWARNINGS", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToBool( szLine, hb_comp_iWarnings != 0 ) ? 1 : 0;
         DebugPragma( szLine, hb_comp_iWarnings, FALSE );
      }
      else if( memcmp( szLine, "EXITSEVERITY", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iExitLevel = StringToInt( szLine, hb_comp_iExitLevel );
         if( hb_comp_iExitLevel != HB_EXITLEVEL_DEFAULT   &&
             hb_comp_iExitLevel != HB_EXITLEVEL_SETEXIT   &&
             hb_comp_iExitLevel != HB_EXITLEVEL_DELTARGET )
            hb_compGenError( hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( szLine, hb_comp_iExitLevel, FALSE );
      }
      else if( memcmp( szLine, "DYNAMICMEMVAR", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bForceMemvars = StringToBool( szLine, hb_comp_bForceMemvars );
         DebugPragma( szLine, -1, hb_comp_bForceMemvars );
      }
      else if( memcmp( szLine, "LINENUMBER", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bLineNumbers = StringToBool( szLine, hb_comp_bLineNumbers );
         DebugPragma( szLine, -1, hb_comp_bLineNumbers );
      }
      else if( memcmp( szLine, "NOSTARTPROC", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bStartProc = StringToBool( szLine, hb_comp_bStartProc );
         DebugPragma( szLine, -1, hb_comp_bStartProc );
      }
      else if( memcmp( szLine, "PREPROCESSING", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bPPO = StringToBool( szLine, hb_comp_bPPO );
         DebugPragma( szLine, -1, hb_comp_bPPO );
      }
      else if( memcmp( szLine, "SHORTCUT", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bShortCuts = StringToBool( szLine, hb_comp_bShortCuts );
         DebugPragma( szLine, -1, hb_comp_bShortCuts );
      }
      else if( memcmp( szLine, "WARNINGLEVEL", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToInt( szLine, hb_comp_iWarnings );
         if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
            hb_compGenError( hb_pp_szErrors, 'F', HB_PP_ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( szLine, hb_comp_iWarnings, FALSE );
      }
      else if( memcmp( szLine, "TRACEPRAGMAS", PRAGMAS_LEN ) == 0 )
      {
         s_bTracePragma = StringToBool( szLine, s_bTracePragma );
         DebugPragma( szLine, -1, s_bTracePragma );
      }
   }
}

/* Checks for +/- within the string, sets bDefault if not found */
static BOOL IsOnOffSwitch( char * pszStr, BOOL bValue )
{
   int iPos = strlen( pszStr ) - 1;

   if( pszStr[ iPos ] == '+' )
      bValue = TRUE;
   else if( pszStr[ iPos ] == '-' )
      bValue = FALSE;

   return bValue;
}

/* Checks for ON/OFF within the string, sets bDefault if not found */
static BOOL StringToBool( char * pszStr, BOOL bValue )
{
   char * pos = strchr( pszStr, '=' );

   if( ! pos )
      pos = strchr( pszStr, '(' );

   if( pos )
   {
      pos++;

      HB_SKIPTABSPACES( pos );

      if( memcmp( pos, "ON", 2 ) == 0 )
         bValue = TRUE;
      else if( memcmp( pos, "OFF", 3 ) == 0 )
         bValue = FALSE;
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
static void DebugPragma( char * pszStr, int iValue, BOOL bValue )
{
   if( s_bTracePragma )
   {
      if( iValue >= 0 )
         printf( "#pragma set to %i \'%s\'\n", iValue, pszStr );
      else
         printf( "#pragma set to %s \'%s\'\n", bValue ? "ON" : "OFF", pszStr );
   }
}

