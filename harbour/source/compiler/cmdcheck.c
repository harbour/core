/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler command line and HARBOURCMD/CLIPPERCMD checking
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_compChkCompilerSwitch()
 *    hb_compChkEnvironVar()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    PackDateTime()
 *    hb_compChkDefineSwitch()
 *    hb_compChkDefines()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <time.h>

#include "hbcomp.h"

/* TODO: Add support for this compiler switches
   -m -r -t || getenv( "TMP" ) -u
*/

/*
 * Function that adds specified path to the list of pathnames to search list
 */
static void AddSearchPath( char * szPath, PATHNAMES * * pSearchList )
{
   PATHNAMES * pPath = *pSearchList;

   if( pPath )
   {
      while( pPath->pNext )
         pPath = pPath->pNext;

      pPath->pNext = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
      pPath = pPath->pNext;
   }
   else
      *pSearchList = pPath = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );

   pPath->pNext  = NULL;
   pPath->szPath = szPath;
}

/* NOTE: Making the date and time info to fit into 32 bits can only be done
         in a "lossy" way, in practice that means it's not possible to unpack
         the exact date/time info from the resulting ULONG. Since the year
         is only stored in 6 bits, 1980 will result in the same bit pattern
         as 2044. The purpose of this value is only used to *differenciate*
         between the dates ( the exact dates are not significant ), so this
         can be used here without problems. [vszakats] */

/* 76543210765432107654321076543210
   |.......|.......|.......|.......
   |____|                               Year    6 bits
         |__|                           Month   4 bits
             |___|                      Day     5 bits
                  |___|                 Hour    5 bits
                       |____|           Minute  6 bits
                             |____|     Second  6 bits */

static ULONG PackDateTime( void )
{
   BYTE szString[ 4 ];
   BYTE nValue;

   time_t t;
   struct tm * oTime;

   time( &t );
   oTime = localtime( &t );

   nValue = ( BYTE ) ( ( ( oTime->tm_year + 1900 ) - 1980 ) & ( 2 ^ 6 ) ) ; /* 6 bits */
   szString[ 0 ]  = nValue << 2;
   nValue = ( BYTE ) ( oTime->tm_mon + 1 ); /* 4 bits */
   szString[ 0 ] |= nValue >> 2;
   szString[ 1 ]  = nValue << 6;
   nValue = ( BYTE ) ( oTime->tm_mday ); /* 5 bits */
   szString[ 1 ] |= nValue << 1;

   nValue = ( BYTE ) oTime->tm_hour; /* 5 bits */
   szString[ 1 ]  = nValue >> 4;
   szString[ 2 ]  = nValue << 4;
   nValue = ( BYTE ) oTime->tm_min; /* 6 bits */
   szString[ 2 ] |= nValue >> 2;
   szString[ 3 ]  = nValue << 6;
   nValue = ( BYTE ) oTime->tm_sec; /* 6 bits */
   szString[ 3 ] |= nValue;

   return HB_MKLONG( szString[ 3 ], szString[ 2 ], szString[ 1 ], szString[ 0 ] );
}

void hb_compChkCompilerSwitch( int iArg, char * Args[] )
{
   /* If iArg is passed check the command line options */
   if( iArg )
   {
      int i;

      /* Check all switches in command line
         They start with an OS_OPT_DELIMITER char
      */
      for( i = 0; i < iArg; i++ )
      {
         if( HB_ISOPTSEP( * Args[ i ] ) )
            hb_compChkEnvironVar( Args[ i ] );
      }
   }
   else
   /* Chech the environment variables */
   {
      /* NOTE: CLIPPERCMD enviroment variable
               is overriden if HARBOURCMD exists
      */
      char * szStrEnv = getenv( "HARBOURCMD" );

      if( ! szStrEnv )
         szStrEnv = getenv( "CLIPPERCMD" );

      if( szStrEnv )
      {
         char * szSwitch = strtok( szStrEnv, " " );

         /* Check the environment var
            while it isn't empty.
         */
         while( szSwitch != NULL )
         {
            hb_compChkEnvironVar( szSwitch );
            szSwitch = strtok( NULL, " " );
         }
      }
   }
}

void hb_compChkEnvironVar( char * szSwitch )
{
   if( szSwitch )
   {
      char * s = szSwitch;

      /* If szSwitch doesn't start with a HB_OSOPTSEP char
         show an error
      */
      if( !HB_ISOPTSEP( *s ) )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
      else
      {
         s++;
         switch( *s )
         {
             case 'a':
             case 'A':
                hb_comp_bAutoMemvarAssume = TRUE;
                break;

             case 'b':
             case 'B':
                hb_comp_bDebugInfo = TRUE;
                hb_comp_bLineNumbers = TRUE;
                break;

             case 'c':
             case 'C':
                {
                   unsigned int i = 0;
                   char * szCredits = hb_strupr( hb_strdup( s ) );
                   while( i < strlen( szCredits ) && !HB_ISOPTSEP( szCredits[ i ] ) )
                      i++;
                   szCredits[ i ] = '\0';

                   if( strcmp( szCredits, "CREDITS" ) ||
                       strcmp( szCredits, "CREDIT" ) ||
                       strcmp( szCredits, "CREDI" ) ||
                       strcmp( szCredits, "CRED" ) )
                   {
                      hb_comp_bCredits = TRUE;
                   }
                   else
                      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, szCredits, NULL );

                   free( szCredits );
                }
                break;

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
                         hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT;
                         break;

                      case '1':
                         hb_comp_iExitLevel = HB_EXITLEVEL_SETEXIT;
                         break;

                      case '2':
                         hb_comp_iExitLevel = HB_EXITLEVEL_DELTARGET;
                         break;

                      default:
                         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                   }
                }
                else
                   hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );

                break;

             case 'g':
             case 'G':
                switch( *( s + 1 ) )
                {
                   case 'c':
                   case 'C':
                      hb_comp_iLanguage = LANG_C;

                      switch( *( s + 2 ) )
                      {
                         case '\0':
                         case '2':
                            hb_comp_iGenCOutput = HB_COMPGENC_VERBOSE;
                            break;

                         case '1':
                            hb_comp_iGenCOutput = HB_COMPGENC_NORMAL;
                            break;

                         case '0':
                            hb_comp_iGenCOutput = HB_COMPGENC_COMPACT;
                            break;

                         default:
                            hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                      }
                      break;

                   case 'o':
                   case 'O':
                      hb_comp_iLanguage = LANG_OBJ32;
                      break;

                   case 'j':
                   case 'J':
                      hb_comp_iLanguage = LANG_JAVA;
                      break;

                   case 'h':
                   case 'H':
                      hb_comp_iLanguage = LANG_PORT_OBJ;
                      break;

                   default:
                      printf( "\nUnsupported output language option\n" );
                      exit( EXIT_FAILURE );
                }
                break;

               /* NOTE:
                  It already has support for several include files
               */
             case 'i':
             case 'I':
                {
                   char * pPath;
                   char * pDelim;

                   pPath = hb_strdup( s + 1 );
                   while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
                   {
                      * pDelim = '\0';
                      AddSearchPath( pPath, &hb_comp_pIncludePath );
                      pPath = pDelim + 1;
                   }
                   AddSearchPath( pPath, &hb_comp_pIncludePath );
                }
                break;

             case 'j':
             case 'J':
                switch( *( s + 1 ) )
                {
                   case '0':
                      hb_comp_iJumpOptimize = 0;
                      break;

                   case '1':
                      hb_comp_iJumpOptimize = 1;
                      break;
                }
                break;

             case 'l':
             case 'L':
                hb_comp_bLineNumbers = FALSE;
                break;

             case 'm':
             case 'M':
                /* TODO: Implement this switch */
                printf( "Not yet supported command line option: %s\n", s );
                break;

             case 'n':
             case 'N':
                hb_comp_bStartProc = FALSE;
                break;

             case 'o':
             case 'O':
                {
                   unsigned int i = 0;
                   char * szPath = hb_strdup( s + 1 );
                   while( i < strlen( szPath ) && !HB_ISOPTSEP( szPath[ i ] ) )
                      i++;
                   szPath[ i ] = '\0';

                   hb_comp_pOutPath = hb_fsFNameSplit( szPath );
                   free( szPath );
                }
                break;

             /* Added for preprocessor needs */
             case 'p':
             case 'P':
                hb_comp_bPPO = 1;
                break;

             case 'q':
             case 'Q':
                if( *( s + 1 ) == '0' )
                   hb_comp_bLogo = FALSE;

                hb_comp_bQuiet = TRUE;
                break;

             case 'r':
             case 'R':
                /* TODO: Implement this switch */
                printf( "Not yet supported command line option: %s\n", s );
                break;

             case 's':
             case 'S':
                hb_comp_bSyntaxCheckOnly = TRUE;
                break;

             case 't':
             case 'T':
                /* TODO: Implement this switch */
                printf( "Not yet supported command line option: %s\n", s );
                break;

             case 'u':
             case 'U':
                /* TODO: Implement this switch */
                printf( "Not yet supported command line option: %s\n", s );
                break;

             case 'v':
             case 'V':
                hb_comp_bForceMemvars = TRUE;
                break;

             case 'w':
             case 'W':
                  hb_comp_iWarnings = 1;
                  if( s[ 1 ] )
                  {  /*there is -w<0,1,2,3> probably */
                     hb_comp_iWarnings = s[ 1 ] - '0';
                     if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
                        hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                  }
                break;

             case 'x':
             case 'X':
                {
                   unsigned int i = 0;
                   char * szPrefix = hb_strdup( s + 1 );
                   while( i < strlen( szPrefix ) && !HB_ISOPTSEP( szPrefix[ i ] ) )
                      i++;
                   szPrefix[ i ] = '\0';

                   if( strlen( szPrefix ) == 0 )
                      sprintf( szPrefix, "%08lX_", PackDateTime() );

                   strncpy( hb_comp_szPrefix, szPrefix, 16 );
                   hb_comp_szPrefix[ 20 ] = '\0';
                   strcat( hb_comp_szPrefix, "_" );

                   free( szPrefix );
                }
                break;

#ifdef YYDEBUG
             case 'y':
             case 'Y':
                yydebug = TRUE;
                break;
#endif

             case 'z':
             case 'Z':
                hb_comp_bShortCuts = FALSE;
                break;

             default:
                hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
                break;
         }
      }
   }
}

void hb_compChkPaths( void )
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

static void hb_compChkDefineSwitch( char * pszSwitch )
{
   if( pszSwitch && HB_ISOPTSEP( pszSwitch[ 0 ] ) &&
      ( pszSwitch[ 1 ] == 'd' || pszSwitch[ 1 ] == 'D' ) )
   {
      char * szDefText = hb_strdup( pszSwitch + 2 );
      unsigned int i = 0;

      while( i < strlen( szDefText ) && ! HB_ISOPTSEP( szDefText[ i ] ) )
         i++;

      szDefText[ i ] = '\0';
      if( szDefText )
         hb_pp_AddDefine( szDefText, 0 );

      free( szDefText );
   }
}

void hb_compChkDefines( int iArg, char * Args[] )
{
   /* Chech the environment variables */
   {
      /* NOTE: CLIPPERCMD enviroment variable is overriden
               if HARBOURCMD exists */
      char * szStrEnv = getenv( "HARBOURCMD" );

      if( ! szStrEnv )
         szStrEnv = getenv( "CLIPPERCMD" );

      if( szStrEnv )
      {
         char * szSwitch = strtok( szStrEnv, " " );

         /* Check the environment var while it isn't empty. */
         while( szSwitch != NULL )
         {
            hb_compChkDefineSwitch( szSwitch );
            szSwitch = strtok( NULL, " " );
         }
      }
   }

   /* Check the command line options */
   {
      int i;

      /* Check all switches in command line They start with an OS_OPT_DELIMITER
         char */
      for( i = 0; i < iArg; i++ )
         hb_compChkDefineSwitch( Args[ i ] );
   }
}
