/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor & Compiler integration module
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
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
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined(HB_TRACE_UTILS)
   #if defined(HB_TRACE_LEVEL)
      #undef HB_TRACE_LEVEL
   #endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbpp.h"
#include "hbcomp.h"

static char s_szLine[ HB_PP_STR_SIZE ];
static char s_szOutLine[ HB_PP_STR_SIZE ];

int hb_pp_Internal( FILE * handl_o, char * sOut )
{
  PFILE pFile;
  char * ptr, * ptrOut;
  int lContinue;
  int lens, rdlen;
  int lLine = 0, i;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_Internal(%p, %s)", handl_o, sOut));

  hb_pp_nEmptyStrings = 0;
  while( TRUE )
  {
     pFile = hb_comp_files.pLast;
     lens = lContinue = 0;
     ptrOut = sOut;
     while( ( rdlen = hb_pp_RdStr( pFile->handle, s_szLine + lens, HB_PP_STR_SIZE -
                  lens, lContinue, ( char * ) pFile->pBuffer, &( pFile->lenBuffer ),
                  &( pFile->iBuffer ) ) ) >= 0 )
       {
         lens += rdlen;

         if( s_szLine[ lens - 1 ] == ';' )
           {
             lContinue = 1;
             lens--;
             lens--;
             while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' ) lens--;
             s_szLine[ ++lens ] = ' ';
             s_szLine[ ++lens ] = '\0';

             hb_pp_nEmptyStrings++;
           }
         else
           {
             lContinue = 0;
             lens = 0;
           }

         if( !lContinue )
           {
             if( *s_szLine != '\0' )
               {
                 ptr = s_szLine;
                 HB_SKIPTABSPACES( ptr );
                 if( *ptr == '#' )
                   {
                     hb_pp_ParseDirective( ptr + 1 );
                     if( pFile != hb_comp_files.pLast )
                     {
                        pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
                        if( lLine )
                           sprintf( s_szLine, "#line %d \"%s\"\n",
                           pFile->iLine+hb_pp_nEmptyStrings, pFile->szFileName );
                        else
                           *s_szLine = '\0';
                        lLine = 0;
                        pFile->iLine += 1+hb_pp_nEmptyStrings;
                        sprintf( s_szLine+strlen(s_szLine), "#line 1 \"%s\"",
                                  hb_comp_files.pLast->szFileName );
                        hb_pp_nEmptyStrings = 0;
                     }
                     else
                     {
                        *s_szLine = '\0';
                        hb_pp_nEmptyStrings++;
                     }
                   }
                 else
                   {
                     if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
                     {
                         hb_pp_ParseExpression( ptr, s_szOutLine );
                     }
                     else
                     {
                       *s_szLine = '\0';
                       hb_pp_nEmptyStrings++;
                     }
                   }
               }
             else
                hb_pp_nEmptyStrings++;
             break;
           }
       }
     if( rdlen < 0 )
       {
        if( hb_comp_files.iFiles == 1 )
           return 0;      /* we have reached the main EOF */
        else
          {  /* we close the currently include file and continue */
           fclose( hb_comp_files.pLast->handle );
           hb_xfree( hb_comp_files.pLast->pBuffer );
           hb_xfree( hb_comp_files.pLast->szFileName );
           pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
           hb_xfree( hb_comp_files.pLast );
           hb_comp_files.pLast = pFile;
           hb_comp_iLine = hb_comp_files.pLast->iLine;
           hb_comp_files.iFiles--;
           lLine = 1;
           hb_pp_nEmptyStrings = 0;
          }
       }
     if( *s_szLine ) break;
  }
  if( lLine )
  {
     sprintf( ptrOut, "#line %d \"%s\"\n",hb_comp_iLine+hb_pp_nEmptyStrings,hb_comp_files.pLast->szFileName );
     while( *ptrOut ) ptrOut++;
  }
  else
  {
     if( hb_pp_nEmptyStrings )
        for( i=0;i<hb_pp_nEmptyStrings;i++ )
           *ptrOut++ = '\n';
  }
  lens = hb_pp_strocpy( ptrOut, s_szLine ) + ( ptrOut - sOut );
  *( sOut + lens++ ) = '\n';
  *( sOut + lens ) = '\0';

  if( handl_o )
    hb_pp_WrStr( handl_o, sOut );

  return lens;
}

int hb_pp_ReadRules( void )
{
  PFILE pFile;
  char * ptr;
  int lContinue;
  int lens, rdlen;
  int lLine = 0, i;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_ReadRules()"));

  hb_pp_nEmptyStrings = 0;
  while( TRUE )
  {
     pFile = hb_comp_files.pLast;
     lens = lContinue = 0;
     while( ( rdlen = hb_pp_RdStr( pFile->handle, s_szLine + lens, HB_PP_STR_SIZE -
                  lens, lContinue, ( char * ) pFile->pBuffer, &( pFile->lenBuffer ),
                  &( pFile->iBuffer ) ) ) >= 0 )
       {
         lens += rdlen;

         if( s_szLine[ lens - 1 ] == ';' )
           {
             lContinue = 1;
             lens--;
             lens--;
             while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' ) lens--;
             s_szLine[ ++lens ] = ' ';
             s_szLine[ ++lens ] = '\0';

             hb_pp_nEmptyStrings++;
           }
         else
           {
             lContinue = 0;
             lens = 0;
           }

         if( !lContinue )
           {
             if( *s_szLine != '\0' )
               {
                 ptr = s_szLine;
                 HB_SKIPTABSPACES( ptr );
                 if( *ptr == '#' )
                   {
                     hb_pp_ParseDirective( ptr + 1 );
                     if( pFile != hb_comp_files.pLast )
                     {
                        pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
                        if( lLine == 0)
                           *s_szLine = '\0';
                        lLine = 0;
                        pFile->iLine += 1+hb_pp_nEmptyStrings;
                        hb_pp_nEmptyStrings = 0;
                     }
                     else
                     {
                        *s_szLine = '\0';
                        hb_pp_nEmptyStrings++;
                     }
                   }
                 else
                   {
                      *s_szLine = '\0';
                      hb_pp_nEmptyStrings++;
                   }
               }
             else
                hb_pp_nEmptyStrings++;
             break;
           }
       }
     if( rdlen < 0 )
       {
        if( hb_comp_files.iFiles == 1 )
           return 0;      /* we have reached the main EOF */
        else
          {  /* we close the currently include file and continue */
           fclose( hb_comp_files.pLast->handle );
           hb_xfree( hb_comp_files.pLast->pBuffer );
           hb_xfree( hb_comp_files.pLast->szFileName );
           pFile = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
           hb_xfree( hb_comp_files.pLast );
           hb_comp_files.pLast = pFile;
           hb_comp_iLine = hb_comp_files.pLast->iLine;
           hb_comp_files.iFiles--;
           lLine = 1;
           hb_pp_nEmptyStrings = 0;
          }
       }
     if( *s_szLine ) break;
  }

  return lens;
}
