/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Command line and environment argument management
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include "hbapi.h"
#include "hbmemory.ch"

/* Command line argument management */
static int     s_argc = 0;
static char ** s_argv = NULL;

static char * hb_cmdargGet( const char * pszName, BOOL bRetValue );

void hb_cmdargInit( int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargInit(%d, %p)", argc, argv));

   s_argc = argc;
   s_argv = argv;
}

int hb_cmdargARGC( void )
{
   return s_argc;
}

char ** hb_cmdargARGV( void )
{
   return s_argv;
}

BOOL hb_cmdargIsInternal( const char * szArg )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargIsInternal(%s)", szArg));

   return strlen( szArg ) >= 2 &&
          szArg[ 0 ] == '/' &&
          szArg[ 1 ] == '/';
}

static char * hb_cmdargGet( const char * pszName, BOOL bRetValue )
{
   int i;
   char * pszEnvVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargGet(%s, %d)", pszName, (int) bRetValue));

   /* Check the command line first */

   for( i = 1; i < s_argc; i++ )
   {
      if( hb_cmdargIsInternal( s_argv[ i ] ) &&
         hb_strnicmp( s_argv[ i ] + 2, pszName, strlen( pszName ) ) == 0 )
      {
         if( bRetValue )
         {
            char * pszPos = s_argv[ i ] + 2 + strlen( pszName );
            char * pszRetVal;

            if( *pszPos == ':' )
               pszPos++;

            pszRetVal = ( char * ) hb_xgrab( strlen( pszPos ) + 1 );
            strcpy( pszRetVal, pszPos );

            return pszRetVal;
         }
         else
            return s_argv[ i ] + 2;
      }
   }

   /* Check the environment variable */

   pszEnvVar = getenv( "HARBOUR" );

   if( pszEnvVar == NULL )
      pszEnvVar = getenv( "CLIPPER" );

   if( pszEnvVar != NULL )
   {
      static const char * szSeparator = " ;,\t";
      char * pszNext;

      /* Step through all envvar switches. */

      /* NOTE: CA-Clipper doesn't need the switches to be separated by any 
               chars at all, Harbour is more strict/standard in this respect, 
               it requires the switches to be separated. */

      pszNext = pszEnvVar;

      while( *pszNext )
      {
         char * pszEnd;

         /* Search for the end of this switch */
         while( *pszNext && strchr( szSeparator, *pszNext ) == NULL )
            pszNext++;

         pszEnd = pszNext;

         /* Skip the separators after the switch */
         while( *pszNext && strchr( szSeparator, *pszNext ) )
            pszNext++;

         /* Check the switch */

         /* The // is optional in the envvar */
         if( hb_cmdargIsInternal( pszEnvVar ) )
            pszEnvVar += 2;

         if( hb_strnicmp( pszEnvVar, pszName, strlen( pszName ) ) == 0 )
         {
            if( bRetValue )
            {
               char * pszPos = pszEnvVar + strlen( pszName );
               char * pszRetVal;

               ULONG ulLen; /* NOTE: Use this variable as a workaround for MSC 8 internal error. [vszakats] */

               /* Skip value separator colon. */
               if( *pszPos == ':' )
                  pszPos++;

               ulLen = pszEnd - pszPos;

               pszRetVal = ( char * ) hb_xgrab( ulLen + 1 );
               strncpy( pszRetVal, pszPos, ulLen );
               pszRetVal[ ulLen ] = '\0';

               return pszRetVal;
            }
            else
               return pszEnvVar;
         }

         /* Step to the next switch */
         pszEnvVar = pszNext;
      }
   }

   return NULL;
}

BOOL hb_cmdargCheck( const char * pszName )
{
   return hb_cmdargGet( pszName, FALSE ) != NULL;
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

char * hb_cmdargString( const char * pszName )
{
   return hb_cmdargGet( pszName, TRUE );
}

int hb_cmdargNum( const char * pszName )
{
   char * pszValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargNum(%s)", pszName));

   pszValue = hb_cmdargGet( pszName, TRUE );
   if( pszValue )
   {
      int iValue = atoi( pszValue );

      hb_xfree( pszValue );

      return iValue;
   }
   else
      return -1;
}

/* Check if an internal switch has been set */

HB_FUNC( HB_ARGCHECK )
{
   hb_retl( ISCHAR( 1 ) ? hb_cmdargCheck( hb_parc( 1 ) ) : FALSE );
}

/* Returns the value of an internal switch */

HB_FUNC( HB_ARGSTRING )
{
   if( ISCHAR( 1 ) )
   {
      char * pszValue = hb_cmdargString( hb_parc( 1 ) );

      if( pszValue )
      {
         hb_retc( pszValue );
         hb_xfree( pszValue );
      }
   }
   else
      hb_retc( "" );
}

/* Returns the number of command line arguments passed to the application, this
   also includes the internal arguments. */

HB_FUNC( HB_ARGC )
{
   hb_retni( s_argc - 1 );
}

/* Returns a command line argument passed to the application. Calling it with
   the parameter zero, it will return the name of the executable, as written
   in the command line. */

HB_FUNC( HB_ARGV )
{
   if( ISNUM( 1 ) )
   {
      int argc = hb_parni( 1 );

      hb_retc( ( argc >= 0 && argc < s_argc ) ? s_argv[ argc ] : "" );
   }
   else
      hb_retc( "" );
}

#ifdef TEST

void hb_cmdargTEST( void )
{
   char * pszArg;

   printf("INFO: %i\n", hb_cmdargCheck( "INFO" ) );
   printf("   F: %s\n", pszArg = hb_cmdargString( "F" ) ); if( pszArg ) hb_xfree( pszArg );
   printf("  Fn: %i\n", hb_cmdargNum( "F" ) );
   printf("TEMP: %s\n", pszArg = hb_cmdargString( "TEMP" ) ); if( pszArg ) hb_xfree( pszArg );

   printf("INFO: %i\n", hb_cmdargCheck( "INFO" ) );
   printf("   F: %s\n", pszArg = hb_cmdargString( "F" ) ); if( pszArg ) hb_xfree( pszArg );
   printf("  Fn: %i\n", hb_cmdargNum( "F" ) );
   printf("TEMP: %s\n", pszArg = hb_cmdargString( "TEMP" ) ); if( pszArg ) hb_xfree( pszArg );
}

#endif

/* Check for command line internal arguments */
void hb_cmdargProcessVM( void )
{
   if( hb_cmdargCheck( "INFO" ) )
   {
      char * pszVersion = hb_version( 1 );
      char buffer[ 128 ];

      hb_outerr( pszVersion, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
      sprintf( buffer, "DS avail=%luKB  OS avail=%luKB  EMM avail=%luKB", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ) );
      hb_outerr( buffer, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_xfree( pszVersion );
   }

   if( hb_cmdargCheck( "BUILD" ) )
   {
      hb_outerr( "Harbour Compiler Build Info", 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
      hb_outerr( "---------------------------", 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "Strict CA-Clipper compatibility: ", 0 );
#if defined( HARBOUR_STRICT_CLIPPER_COMPATIBILITY )
      hb_outerr( "Yes", 0 );
#else
      hb_outerr( "No", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "CA-Clipper 5.3x extensions: ", 0 );
#if defined( HB_COMPAT_C53 )
      hb_outerr( "Yes", 0 );
#else
      hb_outerr( "No", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "Alaska XBase++ extensions: ", 0 );
#if defined( HB_COMPAT_XPP )
      hb_outerr( "Yes", 0 );
#else
      hb_outerr( "No", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "CA-Visual Objects extensions: ", 0 );
#if defined( HB_COMPAT_VO )
      hb_outerr( "Yes", 0 );
#else
      hb_outerr( "No", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "Object file generation support: ", 0 );
#if defined( HARBOUR_OBJ_GENERATION )
      hb_outerr( "Yes", 0 );
#else
      hb_outerr( "No", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "ANSI C usage: ", 0 );
#if defined( HARBOUR_STRICT_ANSI_C )
      hb_outerr( "Strict", 0 );
#else
      hb_outerr( "Non strict", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_outerr( "Compiler YACC debug mode: ", 0 );
#if defined( HARBOUR_YYDEBUG )
      hb_outerr( "On", 0 );
#else
      hb_outerr( "Off", 0 );
#endif
      hb_outerr( hb_consoleGetNewLine(), 0 );

      {
         char buffer[ 64 ];
                           
         sprintf( buffer, "Maximum symbol name length: %i", HB_SYMBOL_NAME_LEN );
         hb_outerr( buffer, 0 );
         hb_outerr( hb_consoleGetNewLine(), 0 );
      }
   }
}

