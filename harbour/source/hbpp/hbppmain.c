/*
 * $Id$

   Harbour Project source code

   This file contains some functions of preprocessor, which need for
   standalone version ( including main() function ).

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

#if defined(__DJGPP__) || defined(__GNUC__)
 #include <string.h>
#else
 #include <alloc.h>
 #include <mem.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "harb.h"

int Hp_Parse( FILE*, FILE* );
extern int ParseDirective( char* );
extern int ParseExpression( char*, char* );
extern int RdStr(FILE*,char *,int,int,char*,int*,int*);
extern int WrStr(FILE*,char *);
extern DEFINES* AddDefine ( char*, char* );
extern int strolen ( char* );
extern char* strodup ( char * );

#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++

extern int lInclude;
extern int *aCondCompile, nCondCompile;
extern int nline;
extern DEFINES *aDefnew ;

#define BUFF_SIZE 8192
#define STR_SIZE 8192

#define INITIAL_ACOM_SIZE 200
extern COMMANDS *aCommnew ;
extern TRANSLATES *aTranslates ;

PATHNAMES *_pIncludePath = NULL;
void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */

int main (int argc,char* argv[])
{
FILE *handl_i,*handl_o;
char szFileName[ _POSIX_PATH_MAX ];
char * szDefText;
FILENAME *pFileName =NULL;
int iArg = 1, i;

   aDefnew = ( DEFINES * ) _xgrab( sizeof(DEFINES) * 50 );

   while( iArg < argc )
   {
     if( IS_OPT_SEP(argv[ iArg ][ 0 ]))
     {
       switch( argv[ iArg ][ 1 ] )
       {
         case 'd':
         case 'D':   /* defines a Lex #define from the command line */
           {
             i = 0;
             szDefText = strodup( argv[ iArg ] + 2 );
             while( i < strolen( szDefText ) && szDefText[ i ] != '=' )
               i++;
             if( szDefText[ i ] != '=' )
               AddDefine( szDefText, 0 );
             else
             {
               szDefText[ i ] = 0;
               AddDefine( szDefText, szDefText + i + 1 );
             }
             free( szDefText );
           }
           break;
         case 'i':
         case 'I':
           AddSearchPath( argv[ iArg ]+2, &_pIncludePath );
           break;
         default:
            printf( "\nInvalid command line option: %s\n", &argv[ iArg ][ 1 ] );
            break;
       }
     }
     else  pFileName =SplitFilename( argv[ iArg ] );
     iArg++;
   }
   if( pFileName )
   {
     if( !pFileName->extension )   pFileName->extension =".prg";
      MakeFilename( szFileName, pFileName );

      if ((handl_i = fopen(szFileName, "r")) == NULL)
         { printf("\nCan't open %s\n",szFileName); return 1; }
   }
   else { printf("\nFile name absent\n"); return 1; }

   pFileName->extension =".ppo";
   MakeFilename( szFileName, pFileName );

   if ((handl_o = fopen(szFileName, "wt" )) == NULL)
        { printf("\nCan't open %s\n",szFileName); return 1; }

   {
       char * szInclude = getenv( "INCLUDE" );

       if( szInclude )
       {
        char * pPath;
        char * pDelim;

        pPath = szInclude = strodup( szInclude );
        while( (pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR )) != NULL )
        {
          *pDelim = '\0';
          AddSearchPath( pPath, &_pIncludePath );
          pPath = pDelim + 1;
        }
        AddSearchPath( pPath, &_pIncludePath );
       }
    }

    aCondCompile = (int*) _xgrab( sizeof(int) * 5 );
    aCommnew = ( COMMANDS * ) _xgrab( sizeof(COMMANDS) * INITIAL_ACOM_SIZE );
    aTranslates = ( TRANSLATES * ) _xgrab( sizeof(TRANSLATES) * 50 );

    Hp_Parse(handl_i,handl_o );
    fclose(handl_i); fclose(handl_o);
/*
 for (int i=0;i<kolcommands;i++)
 {
  printf("\n{%d,\"%s\",",aCommnew[i].com_or_xcom, aCommands[i].name);
  if (aCommnew[i].mpatt !=NULL)   printf("\"%s\",",aCommnew[i].mpatt);
   else printf("NULL,");
  if (aCommnew[i].value !=NULL)   printf("\n\"%s\"},",aCommnew[i].value);
   else printf("\nNULL},");
 }
*/
 return 0;
}

int Hp_Parse( FILE* handl_i, FILE* handl_o )
{
 char sBuffer[BUFF_SIZE];           /* File read buffer */
 char sLine[STR_SIZE], sOutLine[STR_SIZE], *ptr;
 int lContinue = 0;
 int iBuffer = 10, lenBuffer = 10;
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
   if ( sLine[lens+1] == ' ' || sLine[lens+1] == '\t' ) lens++;
   sLine[++lens] = '\0';
  }
  else { lContinue = 0; lens=0; }

  if ( *sLine != '\0' && !lContinue )
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
       return rezParse;
      }
    }
    else *sLine = '\0';
   }
  }

  if(!lInclude)
  {
   if( lContinue ) WrStr(handl_o,"\n");  else WrStr(handl_o,sLine);
  }
 }
 return 0;
}

/*
 * Split given filename into path, name and extension
*/
FILENAME *SplitFilename( char *szFilename )
{
  FILENAME *pName =(FILENAME *)OurMalloc( sizeof(FILENAME) );
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

    if( !(pFileName->extension[ 0 ] == '.' || pFileName->name[ iLen-1 ] == '.') )
    {
      /* add extension separator only when extansion doesn't contain it */
      szFileName[ iLen++ ] ='.';
      szFileName[ iLen ]   ='\x0';
    }
    strcpy( szFileName+iLen, pFileName->extension );
  }

  return szFileName;
}

/*
 * Function that adds specified path to the list of pathnames to search
 */
void AddSearchPath( char *szPath, PATHNAMES * *pSearchList )
{
  PATHNAMES *pPath = *pSearchList;

  if( pPath )
  {
    while( pPath->pNext )
      pPath = pPath->pNext;
    pPath->pNext = ( PATHNAMES * ) OurMalloc( sizeof( PATHNAMES ) );
    pPath = pPath->pNext;
  }
  else
  {
    *pSearchList =pPath =(PATHNAMES *)OurMalloc( sizeof(PATHNAMES) );
  }
  pPath->pNext  = NULL;
  pPath->szPath = szPath;
}

void * OurMalloc( LONG lSize )
{
   void * pMem = malloc( lSize );

   if( ! pMem )
      printf( "\nCan't allocate memory!\n" );

   return pMem;
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

void GenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
  char * szLine = ( char * ) OurMalloc( 160 );      /*2 lines of text */
  /* printf( "\r%s(%i) ", files.pLast->szFileName, iLine ); */
  printf( "Error %c%i  ", cPrefix, iError );
  sprintf( szLine, _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "%s\n\n", szLine );
  exit( 1 );
}