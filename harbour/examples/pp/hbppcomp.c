/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor & Compiler integration module
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
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
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined( HB_TRACE_UTILS )
   #if defined( HB_TRACE_LEVEL )
      #undef HB_TRACE_LEVEL
   #endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbppdef.h"
#include "hbcomp.h"

static void pp_ParseBuffer( PFILE, int * );
static char * pp_TextCommand( char * ptr, int * pLen );
static void pp_TextBlockFinish( void );
static void pp_StreamBlockFinish( void );

#if ! defined( HB_PP_DEBUG_MEMORY )
static char    s_szLine[ HB_PP_STR_SIZE ];
static char    s_szOutLine[ HB_PP_STR_SIZE ];
#else
static char *  s_szLine            = NULL;
static char *  s_szOutLine         = NULL;
#endif
int            hb_pp_LastOutLine   = 1;
static char *  s_TextOutFunc       = NULL;
static char *  s_TextEndFunc       = NULL;
static char *  s_TextStartFunc     = NULL;

/*
   HB_BOOL bDebug = HB_FALSE;
 */

void hb_pp_InternalFree( void )
{
#if defined( HB_PP_DEBUG_MEMORY )
   if( s_szLine )
   {
      hb_xfree( s_szLine );
      s_szLine = NULL;
   }
   if( s_szOutLine )
   {
      hb_xfree( s_szOutLine );
      s_szOutLine = NULL;
   }
#endif
   if( s_TextOutFunc )
   {
      hb_xfree( s_TextOutFunc );
      s_TextOutFunc = NULL;
   }
   if( s_TextEndFunc )
   {
      hb_xfree( s_TextEndFunc );
      s_TextEndFunc = NULL;
   }
   if( s_TextStartFunc )
   {
      hb_xfree( s_TextStartFunc );
      s_TextStartFunc = NULL;
   }
}

int hb_pp_Internal_( FILE * handl_o, char * sOut )
{
   PFILE    pFile;
   char *   ptr, * ptrOut, * tmpPtr;
   int      lContinue;
   int      rdlen;
   ULONG    lens;
   int      lLine = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_Internal_(%p, %s)", handl_o, sOut ) );

#if defined( HB_PP_DEBUG_MEMORY )
   if( ! s_szLine )
      s_szLine = ( char * ) hb_xgrab( HB_PP_STR_SIZE );
   if( ! s_szOutLine )
      s_szOutLine = ( char * ) hb_xgrab( HB_PP_STR_SIZE );
#endif

   hb_pp_NestedLiteralString = HB_FALSE;
   hb_pp_LiteralEscSeq       = HB_FALSE;
   for(;; )
   {
      pFile      = hb_comp_files.pLast;
      lens       = 0;
      lContinue  = 0;
      ptrOut     = sOut;
      while( ( rdlen = hb_pp_RdStr( pFile->handle, s_szLine + lens, HB_PP_STR_SIZE -
                                    lens, lContinue, ( char * ) pFile->pBuffer, &( pFile->lenBuffer ),
                                    &( pFile->iBuffer ) ) ) >= 0 )
      {
         lens += rdlen;
         hb_comp_iLine++;

         if( lens >= HB_PP_STR_SIZE - 2 )
         {
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_BUFFER_OVERFLOW, NULL, NULL );
         }

         if( hb_pp_StreamBlock )
         {
            if( hb_pp_StreamBlock == HB_PP_STREAM_DUMP_C )
               break;
            else if( hb_pp_StreamBlock == HB_PP_STREAM_CLIPPER )
            {
               /* Clipper compatible TEXT/ENDTEXT handling */
               ptr = s_szLine;
               HB_SKIPTABSPACES( ptr );
               if( hb_stricmp( ptr, "ENDTEXT" ) == 0 ||
                   hb_stricmp( ptr, "#pragma __endtext" ) == 0 )
               {
                  pp_TextBlockFinish();
                  break;
               }

               if( s_TextOutFunc )
               {
                  memmove( s_szLine + 1, s_szLine, lens++ );
                  s_szLine[ 0 ]             = '[';
                  s_szLine[ lens++ ]        = ']';
                  s_szLine[ lens ]          = '\0';
                  lens                      = hb_snprintf( s_szOutLine, HB_PP_STR_SIZE, s_TextOutFunc, s_szLine );
                  memcpy( s_szLine, s_szOutLine, lens + 1 );
                  hb_pp_NestedLiteralString = HB_TRUE;
                  break;
               }
               else
               {
                  s_szLine[ 0 ] = '\0'; /* discard text */
               }
            }
            else if( hb_pp_StreamBlock == HB_PP_STREAM_PRG )
            {
               ptr = s_szLine + lens - rdlen;
               HB_SKIPTABSPACES( ptr );
               if( hb_stricmp( ptr, "ENDTEXT" ) == 0 ||
                   hb_stricmp( ptr, "#pragma __endtext" ) == 0 )
               {
                  lens            -= rdlen;
                  s_szLine[ lens ] = '\0';
                  pp_StreamBlockFinish();
                  break;
               }
               else
               {
                  lContinue = 1;
                  continue;
               }
            }
            else if( hb_pp_StreamBlock == HB_PP_STREAM_C )
            {
               ptr = s_szLine + lens - rdlen;
               HB_SKIPTABSPACES( ptr );
               if( hb_stricmp( ptr, "ENDTEXT" ) == 0 ||
                   hb_stricmp( ptr, "#pragma __endtext" ) == 0 )
               {
                  lens               -= rdlen;
                  s_szLine[ lens ]    = '\0';
                  pp_StreamBlockFinish();
                  hb_pp_LiteralEscSeq = HB_TRUE;
                  break;
               }
               else
               {
                  lContinue = 1;
                  continue;
               }
            }
         }

         if( s_szLine[ lens - 1 ] == ';' )
         {
            lContinue = pFile->lenBuffer ? 1 : 0;
            lens--;
            lens--;
            while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' )
               lens--;
            s_szLine[ ++lens ]  = ' ';
            s_szLine[ ++lens ]  = '\0';
         }
         else
         {
            lContinue  = 0;
            lens       = 0;
         }

         if( ! lContinue )
         {
            pp_ParseBuffer( pFile, &lLine );
            break;
         }
      }

      if( rdlen < 0 )
      {
         if( hb_comp_files.iFiles == 1 )
            return 0;  /* we have reached the main EOF */
         else
         {
            hb_pp_CloseInclude();
            lLine = 1;
         }

         /* Ron Pinkas added 2000-06-22 */
         s_szLine[ 0 ] = '\0';
         break;
         /* Ron Pinkas end 2000-06-22 */
      }

      if( *s_szLine )
         break;
   }

   if( lLine )
   {
      if( hb_comp_files.iFiles == 1 )
         hb_pp_LastOutLine = hb_comp_iLine;

      hb_snprintf( ptrOut, HB_PP_STR_SIZE, "#line %d \"%s\"", ( hb_comp_files.pLast->iLine ), hb_comp_files.pLast->szFileName );

      while( *ptrOut )
         ptrOut++;

      /* Ron Pinkas added 2000-06-14 */
      tmpPtr = s_szLine;
      HB_SKIPTABSPACES( tmpPtr );

      /* Last Opened file ended without CR - adding CR to the #line directive. */
      if( *tmpPtr != '\0' )
      {
         *ptrOut++  = '\n';
         *ptrOut    = '\0';
      }
      /* Ron Pinkas end 2000-06-14 */
   }
   else
   {
      /* Ron Pinkas added 2000-06-13 */
      /* Ignore empty lines in #included files. */
      /* Ron Pinkas removed 2001-01-20 */
     #if 0
      if( ( hb_pp_LastOutLine != hb_comp_iLine ) && hb_comp_files.iFiles == 1 && handl_o )
         /* Ron Pinkas end 2000-06-13 */
         for(; hb_pp_LastOutLine < hb_comp_iLine; hb_pp_LastOutLine++ )
            hb_pp_WrStr( handl_o, "\n" );
    #endif
      /* END Ron Pinkas removed 2001-01-20 */
   }

   lens                = hb_pp_strocpy( ptrOut, s_szLine ) + ( ptrOut - sOut );

   *( sOut + lens++ )  = '\n';
   *( sOut + lens )    = '\0';

   if( hb_comp_iLineINLINE && hb_pp_StreamBlock == 0 )
   {
      hb_comp_iLine       = hb_comp_iLinePRG + ( hb_comp_iLine - hb_comp_iLineINLINE );
      hb_comp_iLine++;
      hb_comp_iLineINLINE = 0;
   }

   if( handl_o )
   {
      hb_pp_WrStr( handl_o, sOut );
   }

  #if 0
   printf( "%d : %s\n", hb_comp_iLine, sOut );
  #endif

   return lens;
}

static void pp_ParseBuffer( PFILE pFile, int * plLine )
{
   HB_BOOL  bCont = HB_TRUE;
   char *   ptr;

   while( bCont && *s_szLine != '\0' )
   {
      ptr = s_szLine;
      HB_SKIPTABSPACES( ptr );

      if( *ptr == '#' )
      {
         HB_BOOL bIgnore = hb_pp_ParseDirective_( ptr );

         if( pFile != hb_comp_files.pLast )
         {
            pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;

            if( *plLine )
            {
               hb_snprintf( s_szLine, HB_PP_STR_SIZE, "#line %d \"%s\"\n", pFile->iLine, pFile->szFileName );
               *plLine = 0;
            }
            else
               *s_szLine = '\0';

            hb_snprintf( s_szLine + strlen( s_szLine ), HB_PP_STR_SIZE - strlen( s_szLine ),
                         "#line 1 \"%s\"", hb_comp_files.pLast->szFileName );
            bCont = HB_FALSE;
         }
         else if( bIgnore )
            *s_szLine = '\0';
         else
         {
            hb_pp_ParseExpression( ptr, s_szOutLine, HB_TRUE );
         }
      }
      else
      {
         if( *ptr == '\0' )
         {
            if( hb_comp_files.iFiles == 1 )
               *s_szLine = '\0';
            else
               bCont = HB_FALSE;
         }
         else
         {
            if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
            {
               hb_pp_ParseExpression( ptr, s_szOutLine, HB_TRUE );
               HB_SKIPTABSPACES( ptr );

               bCont = ( *ptr == '#' );
            }
            else
               *s_szLine = '\0';
         }
      }
   }
}

static char * pp_TextCommand( char * ptr, int * pLen )
{
   int      i;
   char *   cCommand = NULL;

   i = 0;
   while( ptr[ i ] && ptr[ i ] != '|' )
   {
      i++;
   }
   if( i > 0 )
   {
      cCommand   = ( char * ) hb_xgrab( i + 1 );
      i          = 0;
      while( ptr[ i ] && ptr[ i ] != '|' )
      {
         cCommand[ i ] = ptr[ i ];
         i++;
      }
      cCommand[ i ] = '\0';
      *pLen        -= i + 1;
      if( *pLen )
         memcpy( ptr, ptr + i + 1, *pLen );
      else
         ptr[ 0 ] = '\0';
   }
   else if( i == 0 && *ptr == '|' )
   {
      ( *pLen )--;
      if( *pLen )
         memcpy( ptr, ptr + 1, *pLen );
      else
         *ptr = '\0';
   }

   return cCommand;
}

static void pp_TextBlockFinish( void )
{
   hb_pp_StreamBlock = 0;
   if( s_TextEndFunc )
   {
      hb_strncpy( s_szLine, s_TextEndFunc, sizeof( s_szLine ) - 1 );
      hb_xfree( s_TextEndFunc );
      s_TextEndFunc = NULL;
   }
   else
   {
      s_szLine[ 0 ] = '\0';
   }
   if( s_TextOutFunc )
   {
      hb_xfree( s_TextOutFunc );
      s_TextOutFunc = NULL;
   }
   if( s_TextStartFunc )
   {
      hb_xfree( s_TextStartFunc );
      s_TextStartFunc = NULL;
   }
   hb_pp_NestedLiteralString = HB_FALSE;
}

HB_BOOL hb_pp_StreamBlockBegin( char * ptr, int iStreamType )
{
   HB_BOOL  bIgnore = HB_TRUE;
   int      len;

   switch( iStreamType )
   {
      case HB_PP_STREAM_CLIPPER:
         /* internal handling of TEXT/ENDTEXT command
          * #pragma __text|functionOut|functionEnd|functionStart
          */
         len              = strlen( ptr ) - 6;
         memcpy( ptr, ptr + 7, len );
         s_TextOutFunc    = pp_TextCommand( ptr, &len );
         s_TextEndFunc    = pp_TextCommand( ptr, &len );
         s_TextStartFunc  = pp_TextCommand( ptr, &len );
         if( s_TextStartFunc )
         {
            memcpy( ptr, s_TextStartFunc, strlen( s_TextStartFunc ) + 1 );
            bIgnore = HB_FALSE;
         }
         hb_pp_StreamBlock = iStreamType;
         break;

      case HB_PP_STREAM_PRG:
         /* internal handling of TEXT/ENDTEXT command
          * #pragma __stream|functionOut|functionEnd|functionStart
          * (lines are joined with CR/LF or LF)
          */
         len                 = strlen( ptr ) - 8;
         memcpy( ptr, ptr + 9, len );
         s_TextOutFunc       = pp_TextCommand( ptr, &len );
         s_TextEndFunc       = pp_TextCommand( ptr, &len );
         s_TextStartFunc     = pp_TextCommand( ptr, &len );
         *ptr                = '\0';
         hb_pp_StreamBlock   = iStreamType;
         break;

      case HB_PP_STREAM_C:
         /* internal handling of TEXT/ENDTEXT command
          * #pragma __cstream|functionOut|functionEnd|functionStart
          * (lines are joined and C esc sequences are converted)
          */
         len                 = strlen( ptr ) - 9;
         memcpy( ptr, ptr + 10, len );
         s_TextOutFunc       = pp_TextCommand( ptr, &len );
         s_TextEndFunc       = pp_TextCommand( ptr, &len );
         s_TextStartFunc     = pp_TextCommand( ptr, &len );
         *ptr                = '\0';
         hb_pp_StreamBlock   = iStreamType;
         break;

      default:
         break;
   }
   return bIgnore;
}

static void pp_StreamBlockFinish( void )
{
   hb_pp_StreamBlock   = 0;
   hb_snprintf( s_szOutLine, HB_PP_STR_SIZE, s_TextOutFunc, s_szLine );
   s_szLine[ 0 ]       = '\0';
   if( s_TextStartFunc )
   {
      hb_strncat( s_szLine, s_TextStartFunc, sizeof( s_szLine ) - 1 );
      hb_xfree( s_TextStartFunc );
      s_TextStartFunc = NULL;
   }
   hb_strncat( s_szLine, "[", sizeof( s_szLine ) - 1 );
   hb_strncat( s_szLine, s_szOutLine, sizeof( s_szLine ) - 1 );
   hb_strncat( s_szLine, "]", sizeof( s_szLine ) - 1 );
   if( s_TextEndFunc )
   {
      hb_strncat( s_szLine, s_TextEndFunc, sizeof( s_szLine ) - 1 );
      hb_xfree( s_TextEndFunc );
      s_TextEndFunc = NULL;
   }
   if( s_TextOutFunc )
   {
      hb_xfree( s_TextOutFunc );
      s_TextOutFunc = NULL;
   }
   hb_pp_NestedLiteralString = HB_TRUE;
}

void hb_pp_BlockEnd()
{
   if( hb_pp_StreamBlock == HB_PP_STREAM_CLIPPER )
      pp_TextBlockFinish();
   else
      pp_StreamBlockFinish();
}


int hb_pp_ReadRules( void )
{
   PFILE    pFile;
   char *   ptr;
   int      lContinue;
   int      lens, rdlen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_ReadRules()" ) );
#if defined( HB_PP_DEBUG_MEMORY )
   if( ! s_szLine )
      s_szLine = ( char * ) hb_xgrab( HB_PP_STR_SIZE );
   if( ! s_szOutLine )
      s_szOutLine = ( char * ) hb_xgrab( HB_PP_STR_SIZE );
#endif
   for(;; )
   {
      pFile   = hb_comp_files.pLast;
      lens    = lContinue = 0;
      while( ( rdlen = hb_pp_RdStr( pFile->handle, s_szLine + lens, HB_PP_STR_SIZE -
                                    lens, lContinue, ( char * ) pFile->pBuffer, &( pFile->lenBuffer ),
                                    &( pFile->iBuffer ) ) ) >= 0 )
      {
         lens += rdlen;
         hb_comp_iLine++;

         if( s_szLine[ lens - 1 ] == ';' )
         {
            lContinue = 1;
            lens--;
            lens--;
            while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' )
               lens--;
            s_szLine[ ++lens ]  = ' ';
            s_szLine[ ++lens ]  = '\0';
         }
         else
         {
            lContinue  = 0;
            lens       = 0;
         }

         if( ! lContinue )
         {
            if( *s_szLine != '\0' )
            {
               ptr = s_szLine;
               HB_SKIPTABSPACES( ptr );

               if( *ptr == '#' )
               {
                  hb_pp_ParseDirective_( ptr );
               }

               *s_szLine = '\0';
            }

            break;
         }
      }

      if( rdlen < 0 )
      {
         if( hb_comp_files.iFiles == 1 )
         {
            break;     /* we have reached the main EOF */
         }
         else
         {
            hb_pp_CloseInclude();
            hb_pp_LastOutLine = hb_comp_iLine;
         }

         *s_szLine = '\0';
      }
   }

   return 0;
}
