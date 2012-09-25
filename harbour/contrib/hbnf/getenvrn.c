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
#include "hbapiitm.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  if defined( HB_OS_DARWIN )
#     include <crt_externs.h>
#     define environ ( *_NSGetEnviron() )
#  elif ! defined( __WATCOMC__ )
extern char ** environ;
#  endif
#elif defined( HB_OS_DOS )
#  define environ    _environ
extern char ** _environ;
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
#  include "hbwinuni.h"
#  include <windows.h>
#endif

#define NORETURN     0
#define CHARTYPE     1
#define ARRAYTYPE    2
#define CRLF         "\x0D\x0A"

HB_FUNC( FT_GETE )
{
#if defined( HB_OS_DOS ) || defined( HB_OS_UNIX )
   {
      char *   buffer   = NULL;
      int      x;
      int      buffsize = 0;
      int      rettype  = HB_ISARRAY( 1 ) ? ARRAYTYPE :
                          ( HB_ISCHAR( 1 ) && HB_ISBYREF( 1 ) ? CHARTYPE : NORETURN );

      /* scan strings first and add up total size */
      if( rettype == CHARTYPE )
      {
         for( x = 0; environ[ x ]; x++ )
         {
            /* add length of this string plus 2 for the crlf */
            buffsize += ( strlen( environ[ x ] ) + 2 );
         }
         /* add 1 more byte for final nul character */
         buffsize++;
         /* now allocate that much memory and make sure 1st byte is a nul */
         buffer      = ( char * ) hb_xgrab( buffsize + 1 );
         buffer[ 0 ] = '\0';
      }

      for( x = 0; environ[ x ]; x++ )
      {
         if( ! environ[ x ] )
            /* null string, we're done */
            break;

         if( rettype == CHARTYPE )
         {
            /* tack string onto end of buffer */
            hb_strncat( buffer, environ[ x ], buffsize );
            /* add crlf at end of each string */
            hb_strncat( buffer, CRLF, buffsize );
         }
         else if( rettype == ARRAYTYPE )
            /* store string to next array element */
            hb_storvc( environ[ x ], 1, x + 1 );
      }

      if( rettype == CHARTYPE )
      {
         /* return buffer to app and free memory */
         if( ! hb_storclen_buffer( buffer, strlen( buffer ), 1 ) )
            hb_xfree( buffer );
      }

      /* return number of strings found */
      hb_retni( x );
   }
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   {
      LPTCH    lpEnviron   = GetEnvironmentStrings(), lpEnv;
      LPTSTR   lpResult    = NULL, lpDst;
      HB_SIZE  nSize       = 0, nCount = 0;
      PHB_ITEM pArray      = NULL;
      int      rettype     = HB_ISARRAY( 1 ) ? ARRAYTYPE :
                             ( HB_ISCHAR( 1 ) && HB_ISBYREF( 1 ) ? CHARTYPE : NORETURN );

      if( lpEnviron )
      {
         if( rettype == CHARTYPE )
         {
            for( lpEnv = lpEnviron; *lpEnv; lpEnv++ )
            {
               while( *++lpEnv )
                  ++nSize;
               nSize += 3;
            }
            if( nSize > 0 )
               lpResult = ( LPTSTR ) hb_xgrab( ( nSize + 1 ) * sizeof( TCHAR ) );
         }
         else if( rettype == ARRAYTYPE )
            pArray = hb_param( 1, HB_IT_ARRAY );

         for( lpEnv = lpEnviron, lpDst = lpResult; *lpEnv; lpEnv++ )
         {
            nCount++;
            if( rettype == CHARTYPE )
            {
               do
               {
                  *lpDst++ = *lpEnv++;
               }
               while( *lpEnv );
               *lpDst++ = '\r';
               *lpDst++ = '\n';
            }
            else if( rettype == ARRAYTYPE )
            {
               nSize = 0;
               while( lpEnv[ ++nSize ] )
                  ;
               HB_ARRAYSETSTRLEN( pArray, nCount, lpEnv, nSize - 1 );
               lpEnv += nSize;
            }
         }

         FreeEnvironmentStrings( lpEnviron );

         if( rettype == CHARTYPE )
         {
            PHB_ITEM pItem = HB_ITEMPUTSTRLEN( NULL, lpResult, nSize );
            if( ! hb_itemParamStoreRelease( 1, pItem ) )
               hb_itemRelease( pItem );
            hb_xfree( lpResult );
         }
      }
      else if( rettype == CHARTYPE )
         hb_storc( NULL, 1 );

      hb_retns( nCount );
   }
#else
   if( HB_ISCHAR( 1 ) )
      hb_storc( NULL, 1 );
   hb_retni( 0 );
#endif
}
