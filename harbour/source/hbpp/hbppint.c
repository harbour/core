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

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
 */

#include <stdlib.h>
#if defined(__GNUC__)
 #include <string.h>
#else
 #if (defined(_MSC_VER) || defined(__IBMCPP__) || defined(__WATCOMC__))
  #include <memory.h>
 #else
  #include <alloc.h>
  #include <mem.h>
 #endif
#endif
#include <stdio.h>
#include "harb.h"

extern int ParseDirective( char* );
extern int ParseExpression( char*, char* );
extern int RdStr(FILE*,char *,int,int,char*,int*,int*);
extern int WrStr(FILE*,char *);
extern int strolen ( char* );
extern int strocpy (char*, char* );

int PreProcess( FILE*, FILE*, char *);
int Hp_Parse( FILE*, FILE* );

#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++

extern int lInclude;
extern int *aCondCompile, nCondCompile;
extern int nline;

#define BUFF_SIZE 8192
#define STR_SIZE 8192
#define INITIAL_ACOM_SIZE 200

extern DEFINES *aDefnew ;
extern COMMANDS *aCommnew ;
extern TRANSLATES *aTranslates ;

int iBuffer, lenBuffer;
int lPpo = 0;
FILE *yyppo;

void Hbpp_init ( void )
{
  lenBuffer = 10; iBuffer = 10;
  aCondCompile = (int*) _xgrab( sizeof(int) * 5 );
  aDefnew = ( DEFINES * ) _xgrab( sizeof(DEFINES) * 50 );
  aCommnew = ( COMMANDS * ) _xgrab( sizeof(COMMANDS) * INITIAL_ACOM_SIZE );
  aTranslates = ( TRANSLATES * ) _xgrab( sizeof(TRANSLATES) * 50 );
}

int PreProcess( FILE* handl_i, FILE* handl_o, char *sOut )
{
 static char sBuffer[BUFF_SIZE];           /* File read buffer */
 char sLine[STR_SIZE], sOutLine[STR_SIZE], *ptr, *ptrOut = sOut;
 int lContinue = 0;
 int lens=0, rdlen;
 int rezParse;
 while ( ( rdlen = RdStr(handl_i,sLine+lens, STR_SIZE-lens,lContinue,
                                     sBuffer,&lenBuffer,&iBuffer ) ) >= 0 )
 {
  if ( !lInclude ) nline++;
  lens += rdlen;

  if( sLine[lens-1] == ';' )
  {
   lContinue = 1;
   lens--; lens--;
   while ( sLine[lens] == ' ' || sLine[lens] == '\t' ) lens--;
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
    WrStr(handl_o,sOut);

 return lens;
}

int Hp_Parse( FILE* handl_i, FILE* handl_o )
{
 char sBuffer[BUFF_SIZE];           /* File read buffer */
 char sLine[STR_SIZE], *ptr;
 int lContinue = 0;
 int iBuffer = 10, lenBuffer = 10;
 int lens=0, rdlen;
 while ( ( rdlen = RdStr(handl_i,sLine+lens, STR_SIZE-lens,lContinue,
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
      printf ( "\nNon directive in include file\n");
   }
  }
 }
 return 0;
}

void * _xgrab( ULONG ulSize )         /* allocates fixed memory */
{
   void * pMem = malloc( ulSize );

   if( ! pMem )
   {
      printf( "\n_xgrab error: can't allocate memory!\n" );
      exit( 1 );
   }

   return pMem;
}

void * _xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   void * pResult = realloc( pMem, ulSize );

   if( ! pResult )
   {
      printf( "\n_xrealloc error: can't reallocate memory!\n" );
      exit( 1 );
   }

   return pResult;
}

void _xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
      free( pMem );
}
