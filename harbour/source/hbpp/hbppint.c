/*
 * $Id$

   Harbour Project source code

   This file contains some functions of preprocessor, which provides
   a link with compiler.

   Copyright 1999  Alexander S.Kresin <alex@belacy.belgorod.su>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
 */

#if defined(__GNUC__)
 #include <string.h>
 #include <stdlib.h>
#else
 #if (defined(_MSC_VER) || defined(__IBMCPP__) || defined(__WATCOMC__))
  #include <memory.h>
  #include <stdlib.h>
 #else
  #include <alloc.h>
  #include <mem.h>
 #endif
#endif
#include <stdio.h>
#include "hbpp.h"
#include "hberrors.h"

void Hbpp_init ( void );
int PreProcess( FILE*, FILE*, char *);
int Hp_Parse( FILE*, FILE* );

int iBuffer, lenBuffer;
int lPpo = 0;
char sLine[STR_SIZE], sOutLine[STR_SIZE];
FILE *yyppo;

void Hbpp_init ( void )
{
  lenBuffer = 10; iBuffer = 10;
  aCondCompile = (int*) hb_xgrab( sizeof(int) * 5 );
}

int PreProcess( FILE* handl_i, FILE* handl_o, char *sOut )
{
 static char sBuffer[BUFF_SIZE];           /* File read buffer */
 char *ptr, *ptrOut = sOut;
 int lContinue = 0;
 int lens=0, rdlen;
 int rezParse;
 while ( ( rdlen = pp_RdStr(handl_i,sLine+lens, STR_SIZE-lens,lContinue,
                                     sBuffer,&lenBuffer,&iBuffer ) ) >= 0 )
 {
  if ( !lInclude ) nline++;
  lens += rdlen;

  if( sLine[lens-1] == ';' )
  {
   lContinue = 1;
   lens--; lens--;
   while ( sLine[lens] == ' ' || sLine[lens] == '\t' ) lens--;
   if ( sLine[lens+1] == ' ' || sLine[lens+1] == '\t' ) lens++;
   sLine[++lens] = '\0';

   *ptrOut++ = '\n';
  }
  else { lContinue = 0; lens=0; }

  if ( !lContinue )
  {
   if ( *sLine != '\0' )
   {
     ptr = sLine;
     SKIPTABSPACES( ptr );
     if ( *ptr == '#' )
     {
      if ( (rezParse=ParseDirective( ptr+1 )) == 0 )
        *sLine = '\0';
     }
     else
     {
      if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
      {
        if ( (rezParse = ParseExpression( ptr, sOutLine)) > 0 )
        {
         printf ( "\nError number %u in line %u\n", rezParse, nline );
        }
      }
      else *sLine = '\0';
     }
   }
   break;
  }
 }
 if ( rdlen < 0 ) return 0;

 lens = strocpy( ptrOut, sLine ) + ( ptrOut - sOut );
 *( sOut + lens++ ) = '\n';
 *( sOut + lens ) = '\0';

 if ( lPpo )
    pp_WrStr(handl_o,sOut);

 return lens;
}

int Hp_Parse( FILE* handl_i, FILE* handl_o )
{
 char *sBuffer = (char *)hb_xgrab( BUFF_SIZE );           /* File read buffer */
 char *ptr;
 int lContinue = 0;
 int iBuffer = 10, lenBuffer = 10;
 int lens=0, rdlen;
 while ( ( rdlen = pp_RdStr(handl_i,sLine+lens, STR_SIZE-lens,lContinue,
                                     sBuffer,&lenBuffer,&iBuffer ) ) >= 0 )
 {
  lens += rdlen;

  if( sLine[lens-1] == ';' )
  {
   lContinue = 1;
   lens--; lens--;
   while ( sLine[lens] == ' ' || sLine[lens] == '\t' ) lens--;
   sLine[++lens] = '\0';
  }
  else { lContinue = 0; lens=0; }

  if ( !lContinue )
  {
   if ( *sLine != '\0' )
   {
     ptr = sLine;
     SKIPTABSPACES( ptr );
     if ( *ptr == '#' )
     {
      ParseDirective( ptr+1 );
      *sLine = '\0';
     }
     else
      GenWarning( _szPWarnings, 'I', WARN_NONDIRECTIVE, NULL, NULL );
   }
  }
 }
 hb_xfree( sBuffer );
 return 0;
}
