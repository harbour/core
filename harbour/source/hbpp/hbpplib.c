/*
 * $Id$
 */

/*
   Harbour Project source code

   This file contains source for a run-time preprocessing function

   Copyright (C) 1999 Felipe G. Coury
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

#include <stdio.h>
#include "hbpp.h"
#include "extend.h"
#include "itemapi.h"
#include "hberrors.h"

PATHNAMES *_pIncludePath = NULL;
PHB_FNAME _pFileName = NULL;
BOOL _bWarnings = FALSE;

/* TODO: Extend the function to allow directives
         and external include files              */
HARBOUR HB___PREPROCESS(void)
{
  if (ISCHAR(1))
  {
    char *pText = (char *)hb_xgrab(STR_SIZE);
    char *pOut = (char *)hb_xgrab(STR_SIZE);
    char *ptr = pText;

    int slen, resParse;

    slen = MIN(hb_parclen(1), STR_SIZE - 1);
    memcpy(pText, hb_parc(1), slen);
    pText[slen] = 0; /* Preprocessor expects null-terminated string */
    memset(pOut, 0, STR_SIZE);

    SKIPTABSPACES( ptr );

    if ( (resParse = ParseExpression( ptr, pOut )) > 0 )
    {
        /* Some error here? */
    }

    hb_retc(pText); /* Preprocessor returns parsed line in input buffer */

    hb_xfree(pText);
    hb_xfree(pOut);
  }
  else
    hb_retc("");
}

void GenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
   printf( "Error %c%i  ", cPrefix, iError );
   printf( _szErrors[ iError - 1 ], szError1, szError2 );
   printf( hb_consoleGetNewLine() );
   printf( hb_consoleGetNewLine() );

   exit( EXIT_FAILURE );
}

void GenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2)
{
   if( _bWarnings && iWarning < WARN_ASSIGN_SUSPECT ) /* TODO: add switch to set level */
   {
      printf( "Warning %c%i  ", cPrefix, iWarning );
      printf( _szWarnings[ iWarning - 1 ], szWarning1, szWarning2 );
      printf( hb_consoleGetNewLine() );
   }
}
