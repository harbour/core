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

#include <ctype.h>
#include "hbsetup.h"
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "harb.h"

#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++
#define STR_SIZE 8192

extern int ParseExpression( char*, char* );

PATHNAMES *_pIncludePath = NULL;

HARBOUR HB_PREPROCESS(void);

HB_INIT_SYMBOLS_BEGIN( Preprocess__InitSymbols )
{ "PREPROCESS",     FS_PUBLIC, HB_PREPROCESS    , 0 }
HB_INIT_SYMBOLS_END( Preprocess__InitSymbols );
#if ! defined(__GNUC__)
#pragma Preprocess__InitSymbols
#endif

/* TODO: Extend the function to allow directives
         and external include files              */
HARBOUR HB_PREPROCESS(void)
{
    int resParse;

    if( hb_pcount() == 1 )
    {
        PHB_ITEM pItem = hb_param( 1, IT_STRING );

        extern int strolen( char* );
        char szText[STR_SIZE];
        char szOut[STR_SIZE];
        memcpy(szText, pItem->item.asString.value, strolen(pItem->item.asString.value)+1);

        if(pItem)
        {
            char *ptr = szText;
            SKIPTABSPACES( ptr );
            if ( (resParse = ParseExpression( ptr, szOut )) > 0 )
            {
                // Some error here?
            }
        }

        hb_retc( szOut );
    }
}

void GenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
  /*char * szLine = ( char * ) OurMalloc( 160 );*/      /*2 lines of text */
  char * szLine = ( char * ) _xgrab( 160 );      /*2 lines of text */
  /* printf( "\r%s(%i) ", files.pLast->szFileName, iLine ); */
  printf( "Error %c%i  ", cPrefix, iError );
  sprintf( szLine, _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "%s\n\n", szLine );
  exit( 1 );
}

/*
 * Split given filename into path, name and extension
*/
FILENAME *SplitFilename( char *szFilename )
{
  /*FILENAME *pName =(FILENAME *)OurMalloc( sizeof(FILENAME) );*/
  FILENAME *pName =(FILENAME *) _xgrab( sizeof(FILENAME) );
  int iLen = strlen(szFilename);
  int iSlashPos, iDotPos;
  int iPos;

  pName->path =pName->name =pName->extension =NULL;

  iSlashPos =iLen-1;
  iPos =0;
  while( iSlashPos >= 0 && !IS_PATH_SEP(szFilename[ iSlashPos ]) )
    --iSlashPos;
  if( iSlashPos == 0 )
  {
    /* root path ->  \filename */
    pName->_buffer[ 0 ] =PATH_DELIMITER[ 0 ];
    pName->_buffer[ 1 ] ='\x0';
    pName->path =pName->_buffer;
    iPos =2;  /* first free position after the slash */
  }
  else if( iSlashPos > 0 )
  {
    /* path with separator ->  path\filename */
    memcpy( pName->_buffer, szFilename, iSlashPos );
    pName->_buffer[ iSlashPos ] ='\x0';
    pName->path =pName->_buffer;
    iPos =iSlashPos +1;   /* first free position after the slash */
  }

  iDotPos =iLen-1;
  while( iDotPos > iSlashPos && szFilename[ iDotPos ] != '.' )
    --iDotPos;
  if( (iDotPos-iSlashPos) > 1 )
  {
    /* the dot was found
     * and there is at least one character between a slash and a dot
     */
    if( iDotPos == iLen-1 )
    {
      /* the dot is the last character -use it as extension name */
      pName->extension =pName->_buffer+iPos;
      pName->_buffer[ iPos++ ] ='.';
      pName->_buffer[ iPos++ ] ='\x0';
    }
    else
    {
      pName->extension =pName->_buffer+iPos;
      /* copy rest of the string with terminating ZERO character */
      memcpy( pName->extension, szFilename+iDotPos+1, iLen-iDotPos );
      iPos +=iLen-iDotPos;
    }
  }
  else
    /* there is no dot in the filename or it is  '.filename' */
    iDotPos =iLen;

  pName->name =pName->_buffer+iPos;
  memcpy( pName->name, szFilename+iSlashPos+1, iDotPos-iSlashPos-1 );
  pName->name[ iDotPos-iSlashPos-1 ] ='\x0';

  return pName;
}

/*
 * This function joins path, name and extension into a string with a filename
*/
char *MakeFilename( char *szFileName, FILENAME *pFileName )
{
  if( pFileName->path && pFileName->path[ 0 ] )
  {
    /* we have not empty path specified */
    int iLen =strlen(pFileName->path);
    strcpy( szFileName, pFileName->path );
    /* if the path is a root directory then we don't need to add path separator */
    if( !(IS_PATH_SEP(pFileName->path[ 0 ]) && pFileName->path[ 0 ] == '\x0') )
    {
      /* add the path separator only in cases:
       *  when a name doesn't start with it
       *  when the path doesn't end with it
       */
      if( !( IS_PATH_SEP(pFileName->name[ 0 ]) || IS_PATH_SEP(pFileName->path[ iLen-1 ]) ) )
      {
        szFileName[ iLen++ ] =PATH_DELIMITER[ 0 ];
        szFileName[ iLen ] ='\x0';
      }
    }
    strcpy( szFileName+iLen, pFileName->name );
  }
  else
    strcpy( szFileName, pFileName->name );

  if( pFileName->extension )
  {
    int iLen =strlen(szFileName);

    if( !(pFileName->extension[ 0 ] == '.' || szFileName[ iLen-1 ] == '.') )
    {
      /* add extension separator only when extansion doesn't contain it */
      szFileName[ iLen++ ] ='.';
      szFileName[ iLen ]   ='\x0';
    }
    strcpy( szFileName+iLen, pFileName->extension );
  }

  return szFileName;
}
