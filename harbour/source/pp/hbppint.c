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
#include "hberrors.h"

static int  s_iBuffer;
static int  s_lenBuffer;
static char s_szLine[ HB_PP_STR_SIZE ];
static char s_szOutLine[ HB_PP_STR_SIZE ];

void hb_pp_Init( void )
{
  HB_TRACE(HB_TR_DEBUG, ("Hbpp_init()"));

  s_lenBuffer = 10;
  s_iBuffer = 10;
  hb_pp_aCondCompile = ( int * ) hb_xgrab( sizeof( int ) * 5 );
}

int hb_pp_Internal( FILE * handl_i, FILE * handl_o, char * sOut )
{
  static char sBuffer[ HB_PP_BUFF_SIZE ];           /* File read buffer */
  char *ptr, *ptrOut = sOut;
  int lContinue = 0;
  int lens = 0, rdlen;
  int rezParse;
  int nline = 0;

  HB_TRACE(HB_TR_DEBUG, ("PreProcess(%p, %p, %s)", handl_i, handl_o, sOut));

  HB_SYMBOL_UNUSED( handl_o );

  while( ( rdlen = hb_pp_RdStr( handl_i, s_szLine + lens, HB_PP_STR_SIZE - lens, lContinue,
                                sBuffer, &s_lenBuffer, &s_iBuffer ) ) >= 0 )
    {
      if( ! hb_pp_lInclude )
        nline++;
      lens += rdlen;

      if( s_szLine[ lens - 1 ] == ';' )
        {
          lContinue = 1;
          lens--;
          lens--;
          while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' ) lens--;
          s_szLine[ ++lens ] = ' ';
          s_szLine[ ++lens ] = '\0';

          *ptrOut++ = '\n';
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
                  if( ( rezParse = hb_pp_ParseDirective( ptr + 1 ) ) == 0 )
                    *s_szLine = '\0';
                }
              else
                {
                  if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
                    {
                      if( ( rezParse = hb_pp_ParseExpression( ptr, s_szOutLine ) ) > 0 )
                        {
                          printf( "\nError number %u in line %u\n", rezParse, nline );
                        }
                    }
                  else
                    *s_szLine = '\0';
                }
            }
          break;
        }
    }
  if( rdlen < 0 ) return 0;

  lens = hb_pp_strocpy( ptrOut, s_szLine ) + ( ptrOut - sOut );
  *( sOut + lens++ ) = '\n';
  *( sOut + lens ) = '\0';

  return lens;
}

int hb_pp_Parse( FILE * handl_i, FILE * handl_o, char * szSource )
{
  char * sBuffer = ( char * ) hb_xgrab( HB_PP_BUFF_SIZE );           /* File read buffer */
  char * ptr;
  char szLine[ 16 ];
  int lContinue = 0;
  int iBuffer = 10, lenBuffer = 10;
  int lens = 0, rdlen, iLine = 0;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_Parse(%p, %p)", handl_i, handl_o));

  while( ( rdlen = hb_pp_RdStr( handl_i, s_szLine + lens, HB_PP_STR_SIZE - lens, lContinue,
                                sBuffer, &lenBuffer, &iBuffer ) ) >= 0 )
    {
      lens += rdlen;
      iLine++;

      if( s_szLine[ lens - 1 ] == ';' )
        {
          lContinue = 1;
          lens--;
          lens--;
          while( s_szLine[ lens ] == ' ' || s_szLine[ lens ] == '\t' ) lens--;
          s_szLine[ ++lens ] = ' ';
          s_szLine[ ++lens ] = '\0';
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
                  *s_szLine = '\0';
                }
              else
              {
                sprintf( szLine, "%d", iLine );
                hb_compGenWarning( hb_pp_szWarnings, 'I', WARN_NONDIRECTIVE, szSource, szLine );
              }
            }
        }
    }

  hb_xfree( sBuffer );

  return 0;
}
