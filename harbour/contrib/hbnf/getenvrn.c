/*
 * $Id$
 */

/*
 * File......: getenvrn.c
 * Author....: Rick Whitt
 * CIS ID....: 70672,605
 *
 * This is an original work by Rick Whitt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *    Rev 1.2a  09 Sep 1996            JO
 * Added underscore prefix to environ() calls for MSC 8.0
 * Note: 5.2e version will work if linked with MSC OldNames.lib
 *
 *    Rev 1.2   01 Jan 1996 03:01:00   TED
 * Added prototypes to kill compiler warning.
 *
 *    Rev 1.1   15 Aug 1991 23:08:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   17 Jul 1991 22:08:12   GLENN
 * Initial revision.
 *
 */

#include "hbapi.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  if defined( HB_OS_DARWIN )
#     include <crt_externs.h>
#     define environ (*_NSGetEnviron())
#  elif !defined( __WATCOMC__ )
extern char **environ;
#  endif
#elif defined( HB_OS_DOS )
#  define environ _environ
extern char **_environ;
#elif defined( HB_OS_WIN )
#  include <windows.h>
#endif

#define NORETURN   0
#define CHARTYPE   1
#define ARRAYTYPE  2
#define CRLF       "\x0D\x0A"

HB_FUNC( FT_GETE )
{
#if defined( HB_OS_DOS ) || defined( HB_OS_UNIX )
   {
      char *buffer = NULL;
      int x;
      int buffsize = 0;
      int rettype = NORETURN;

      if( HB_ISCHAR( 1 ) )
         rettype = CHARTYPE;
      if( HB_ISARRAY( 1 ) )
         rettype = ARRAYTYPE;

      /* scan strings first and add up total size */
      if( rettype == CHARTYPE )
      {
         for( x = 0; environ[x]; x++ )
         {
            /* add length of this string plus 2 for the crlf */
            buffsize += ( strlen( environ[x] ) + 2 );
         }
         /* add 1 more byte for final nul character */
         buffsize++;
         /* now allocate that much memory and make sure 1st byte is a nul */
         buffer = ( char * ) hb_xgrab( buffsize + 1 );
         buffer[0] = '\0';
      }

      for( x = 0; environ[x]; x++ )
      {
         if( !environ[x] )
            /* null string, we're done */
            break;

         if( rettype == CHARTYPE )
         {
            /* tack string onto end of buffer */
            hb_strncat( buffer, environ[x], buffsize );
            /* add crlf at end of each string */
            hb_strncat( buffer, CRLF, buffsize );
         }
         else if( rettype == ARRAYTYPE )
            /* store string to next array element */
            hb_storvc( environ[x], 1, x + 1 );
      }

      if( rettype == CHARTYPE )
      {
         /* return buffer to app and free memory */
         hb_storc( buffer, 1 );
         hb_xfree( buffer );
      }

      /* return number of strings found */
      hb_retni( x );
   }
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   {
      char *buffer = NULL;
      LPTCH lpEnviron = GetEnvironmentStrings();
      char *sCurEnv;
      int x;
      HB_ISIZ buffsize = 0;
      int rettype = NORETURN;

      char * szEnviron = HB_TCHAR_CONVFROM( lpEnviron );

      if( HB_ISCHAR( 1 ) )
         rettype = CHARTYPE;
      if( HB_ISARRAY( 1 ) )
         rettype = ARRAYTYPE;

      if( rettype == CHARTYPE )
         /* scan strings first and add up total size */
      {
         for( sCurEnv = szEnviron; *sCurEnv; sCurEnv++ )
         {
            if( !*sCurEnv )
               /* null string, we're done */
               break;

            /* add length of this string plus 2 for the crlf */
            buffsize += ( strlen( ( char * ) sCurEnv ) + 2 );

            while( *sCurEnv )
               sCurEnv++;
         }
         /* add 1 more byte for final nul character */
         buffsize++;

         /* now allocate that much memory and make sure 1st byte is a nul */
         buffer = ( char * ) hb_xgrab( buffsize + 1 );
         buffer[0] = '\0';
      }
      x = 0;
      for( sCurEnv = szEnviron; *sCurEnv; sCurEnv++ )
      {
         if( !*sCurEnv )
            /* null string, we're done */
            break;

         if( rettype == CHARTYPE )
         {
            /* tack string onto end of buffer */
            hb_strncat( buffer, ( char * ) sCurEnv, buffsize );
            /* add crlf at end of each string */
            hb_strncat( buffer, CRLF, buffsize );
         }

         if( rettype == ARRAYTYPE )
            /* store string to next array element */
            hb_storvc( ( char * ) sCurEnv, 1, x + 1 );
         x++;
         while( *sCurEnv )
            sCurEnv++;
      }

      if( rettype == CHARTYPE )
      {
         /* return buffer to app and free memory */
         hb_storc( buffer, 1 );
         hb_xfree( buffer );
      }

      /* return number of strings found */
      hb_retni( x );

      HB_TCHAR_FREE( szEnviron );
      FreeEnvironmentStrings( ( LPTSTR ) lpEnviron );
   }
#else
   hb_storc( NULL, 1 );
   hb_retni( 0 );
#endif
}
