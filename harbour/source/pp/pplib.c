/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor runtime library callable version
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 * www - http://www.harbour-project.org
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
#if ! defined(HB_TRACE_UTILS)
   #if defined(HB_TRACE_LEVEL)
      #undef HB_TRACE_LEVEL
   #endif
#endif

#include <stdio.h>
#include <setjmp.h>

#include "hbpp.h"
#include "hbcomp.h"
#include "hbapi.h"
#include "hbapierr.h"

#ifdef HB_EXTENSION

PATHNAMES * hb_comp_pIncludePath = NULL;
PHB_FNAME   hb_comp_pFileName = NULL;
FILES       hb_comp_files;
int         hb_comp_iLine;       /* currently parsed file line number */
int         hb_comp_iLinePRG;
int         hb_comp_iLineINLINE = 0;

/* These are need for the PP #pragma support */
BOOL hb_comp_bPPO = FALSE;                      /* flag indicating, is ppo output needed */
BOOL hb_comp_bStartProc = TRUE;                 /* holds if we need to create the starting procedure */
BOOL hb_comp_bLineNumbers = TRUE;               /* holds if we need pcodes with line numbers */

#if 0
BOOL hb_comp_bShortCuts = TRUE;                 /* .and. & .or. expressions shortcuts */
#endif

int  hb_comp_iWarnings = 0;                     /* enable parse warnings */
BOOL hb_comp_bAutoMemvarAssume = FALSE;         /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
BOOL hb_comp_bForceMemvars = FALSE;             /* holds if memvars are assumed when accesing undeclared variable (-v)*/
BOOL hb_comp_bDebugInfo = FALSE;                /* holds if generate debugger required info */
int  hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
FILE *hb_comp_yyppo = NULL;

static jmp_buf s_env;

/* TODO: Extend the function to allow directives
         and external include files              */

/* TODO: This function should return an error code. The preprocessed sting
 * should be returned  by a reference.
 */

static void AddSearchPath( char * szPath, PATHNAMES * * pSearchList )
{
  PATHNAMES * pPath = *pSearchList;

  HB_TRACE(HB_TR_DEBUG, ("AddSearchPath(%s, %p)", szPath, pSearchList));

  if( pPath )
    {
      while( pPath->pNext )
        pPath = pPath->pNext;
      pPath->pNext = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
      pPath = pPath->pNext;
    }
  else
    {
      *pSearchList = pPath = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
    }
  pPath->pNext  = NULL;
  pPath->szPath = hb_strdup( szPath );
}

HB_FUNC( __PP_INIT )
{
   hb_pp_Table();
   hb_pp_Init();
   hb_comp_files.iFiles = 0;

   if( ISCHAR( 1 ) )
   {
      char * pPath = hb_parc( 1 );
      char * pDelim;

      while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
        {
          *pDelim = '\0';
          AddSearchPath( pPath, &hb_comp_pIncludePath );
          pPath = pDelim + 1;
        }
      AddSearchPath( pPath, &hb_comp_pIncludePath );
   }
}

HB_FUNC( __PP_PATH )
{
   PATHNAMES * pPath = hb_comp_pIncludePath, * pPathNext;

   if( ISLOG( 2 ) && hb_parl( 2 ) )
   {
      while( pPath )
      {
         pPathNext = pPath->pNext;
         hb_xfree( pPath->szPath );
         hb_xfree( pPath );
         pPath = pPathNext;
      }
   }
   if( ISCHAR( 1 ) )
   {     
      char * cDelim;
      char * cPath = hb_parc( 1 );
     
      while( ( cDelim = strchr( cPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
        {
          *cDelim = '\0';
          AddSearchPath( cPath, &hb_comp_pIncludePath );
          cPath = cDelim + 1;
        }
      AddSearchPath( cPath, &hb_comp_pIncludePath );
   }
}

HB_FUNC( __PP_FREE )
{
  PATHNAMES * pPath = hb_comp_pIncludePath, * pPathNext;

   while( pPath )
   {
      pPathNext = pPath->pNext;
      hb_xfree( pPath->szPath );
      hb_xfree( pPath );
      pPath = pPathNext;
   }

   hb_pp_Free();
   if( hb_pp_aCondCompile )
   {
      hb_xfree( hb_pp_aCondCompile );
      hb_pp_aCondCompile = NULL;
   }
}

HB_FUNC( __PPADDRULE )
{
   if( ISCHAR( 1 ) )
   {
      char * ptr = hb_parc( 1 );
      char * hb_buffer;

      HB_SKIPTABSPACES( ptr );
      if( *ptr == '#' )
      {
         if( !hb_pp_aCondCompile )
         {
            hb_pp_Table();
            hb_pp_Init();
            hb_comp_files.iFiles = 0;
         }
         hb_pp_ParseDirective( ptr + 1 );
         if( hb_comp_files.pLast )
         {
            hb_buffer = ( char* ) hb_xgrab( HB_PP_STR_SIZE );
            while( hb_pp_Internal( NULL,hb_buffer ) > 0 );
            CloseInclude();
            hb_xfree( hb_buffer );
         }
         hb_retl( 1 );
      }
      else
         hb_retl( 0 );
   }
   else
      hb_retl( 0 );
}

HB_FUNC( __PREPROCESS )
{
   if( ISCHAR( 1 ) )
   {
      char * pText = ( char * ) hb_xgrab( HB_PP_STR_SIZE );
      char * pOut = ( char * ) hb_xgrab( HB_PP_STR_SIZE );

      if( setjmp( s_env ) == 0 )
      {
         char * ptr = pText;
         int slen;

         /*   hb_pp_Init();   */

         slen = HB_MIN( hb_parclen( 1 ), HB_PP_STR_SIZE - 1 );
         memcpy( pText, hb_parc( 1 ), slen );
         pText[ slen ] = 0; /* Preprocessor expects null-terminated string */
         memset( pOut, 0, HB_PP_STR_SIZE );

         HB_SKIPTABSPACES( ptr );

         if( !hb_pp_topDefine )
            hb_pp_Table();
         if( hb_pp_ParseExpression( ptr, pOut ) > 0 )
         {
            /* Some error here? */
         }

         hb_retc( pText ); /* Preprocessor returns parsed line in input buffer */
      }
      else
      {
         /* an error occured during parsing.
          * The longjmp was used in GenError()
          */
         hb_retc( "" );
      }

      hb_xfree( pText );
      hb_xfree( pOut );
   }
   else
      hb_retc( "" );
}

void hb_compGenError( char * szErrors[], char cPrefix, int iError, const char * szError1, const char * szError2 )
{
   PHB_ITEM pError;
   char buffer[ 128 ];

   HB_SYMBOL_UNUSED( cPrefix );

   HB_TRACE(HB_TR_DEBUG, ("GenError(%p, %c, %d, %s, %s)", szErrors, cPrefix, iError, szError1, szError2));

   /* TOFIX: The internal buffers allocated by the preprocessor should be
             deallocated here */

   sprintf( buffer, szErrors[ iError - 1 ], szError1, szError2 );
   pError = hb_errRT_New( ES_ERROR, "PP", 9999, ( ULONG ) iError, buffer, NULL, 0, EF_NONE | EF_CANDEFAULT );
   hb_errLaunch( pError );
   hb_errRelease( pError );

   longjmp( s_env, iError == 0 ? -1 : iError );
}

void hb_compGenWarning( char * szWarnings[], char cPrefix, int iWarning, const char * szWarning1, const char * szWarning2 )
{
   HB_TRACE(HB_TR_DEBUG, ("GenWarning(%p, %c, %d, %s, %s)", szWarnings, cPrefix, iWarning, szWarning1, szWarning2));

   /* NOTE: All warnings are simply ignored */

   HB_SYMBOL_UNUSED( szWarnings );
   HB_SYMBOL_UNUSED( cPrefix );
   HB_SYMBOL_UNUSED( iWarning );
   HB_SYMBOL_UNUSED( szWarning1 );
   HB_SYMBOL_UNUSED( szWarning2 );
}

PINLINE   hb_compInlineAdd( char * szFunName )
{
   HB_SYMBOL_UNUSED( szFunName );
   return NULL;
}

#endif

