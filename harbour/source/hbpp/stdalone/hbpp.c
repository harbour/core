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

#include <stdlib.h>
#if defined(__GNUC__)
 #include <unistd.h>
#else
 #if ! defined(__MPW__)
  #include <malloc.h>
 #endif
 #if (defined(_MSC_VER) || defined(__IBMCPP__))
  #include <memory.h>
 #else
  #include <mem.h>
 #endif
#endif
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "hbpp.h"
#include "hberrors.h"

extern int pp_strAt(char *, int, char*, int);
extern void pp_Stuff (char*, char*, int, int, int);

int Hp_Parse( FILE*, FILE* );
void OutTable( DEFINES*, COMMANDS* );
void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */

char sLine[STR_SIZE], sOutLine[STR_SIZE];

PATHNAMES *_pIncludePath = NULL;
PHB_FNAME _pFileName = NULL;
BOOL _bWarnings = FALSE;

int main (int argc,char* argv[])
{
   FILE *handl_i,*handl_o;
   char szFileName[ _POSIX_PATH_MAX ];
   char * szDefText;
   int iArg = 1, i;
   BOOL bOutTable = FALSE;
   BOOL bOutNew = FALSE;
   DEFINES *stdef = topDefine;
   COMMANDS *stcmd = topCommand;

   printf( "Harbour preprocessor\n" );
   while( iArg < argc )
   {
     if( IS_OPT_SEP(argv[ iArg ][ 0 ]))
     {
       switch( argv[ iArg ][ 1 ] )
       {
         case 'd':
         case 'D':   /* defines a #define from the command line */
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
         case 'o':
         case 'O':
            bOutTable = TRUE;
            break;
         case 'n':
         case 'N':
            bOutNew = TRUE;
            break;
         case 'w':
         case 'W':
            _bWarnings = TRUE;
            break;
         default:
            printf( "\nInvalid command line option: %s\n", &argv[ iArg ][ 1 ] );
            break;
       }
     }
     else  _pFileName =hb_fsFNameSplit( argv[ iArg ] );
     iArg++;
   }
   if( _pFileName )
   {
     if( !_pFileName->szExtension )   _pFileName->szExtension =".prg";
      hb_fsFNameMerge( szFileName, _pFileName );

      if ((handl_i = fopen(szFileName, "r")) == NULL)
         { printf("\nCan't open %s\n",szFileName); return 1; }
     printf( "\nParsing file %s\n", szFileName );
   }
   else
   {
     printf( "Syntax: hbpp <file.prg> [options]\n"
             "\nOptions: \n"
             "\t/d<id>[=<val>]\t#define <id>\n"
             "\t/i<path>\tadd #include file search path\n"
             "\t/o\t\tcreates hbpp.out with all tables\n"
             "\t/n\t\twith those only, which defined in your file\n"
             "\t/w\t\tenable warnings\n");

     if( bOutTable )
        OutTable( NULL, NULL );
     return 1;
   }

   _pFileName->szExtension =".ppo";
   hb_fsFNameMerge( szFileName, _pFileName );

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

   aCondCompile = (int*) hb_xgrab( sizeof(int) * 5 );

   Hp_Parse(handl_i,handl_o );
   fclose(handl_i); fclose(handl_o);

   if( bOutTable )
      OutTable( NULL, NULL );
   else if( bOutNew )
      OutTable( stdef, stcmd );

   printf( "\nOk" );
   return 0;
}

int Hp_Parse( FILE* handl_i, FILE* handl_o )
{
   char sBuffer[BUFF_SIZE];           /* File read buffer */
   char  *ptr;
   int lContinue = 0;
   int iBuffer = 10, lenBuffer = 10;
   int lens=0, rdlen;

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
      }
      else { lContinue = 0; lens=0; }

      if ( *sLine != '\0' && !lContinue )
      {
         printf( "\r  line %i", nline );
         ptr = sLine;
         SKIPTABSPACES( ptr );
         if ( *ptr == '#' )
         {
            if ( ParseDirective( ptr+1 ) == 0 )
               *sLine = '\0';
         }
         else
         {
            if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
               ParseExpression( ptr, sOutLine );
            else
               *sLine = '\0';
         }
      }

      if(!lInclude)
      {
         if( lContinue ) pp_WrStr(handl_o,"\n");
         else  pp_WrStr(handl_o,sLine);
      }
   }
   return 0;
}

void OutTable( DEFINES* endDefine, COMMANDS* endCommand )
{
   FILE *handl_o;
   int ipos, len_mpatt, len_value;
   int num;
   DEFINES *stdef1 = topDefine, *stdef2 = NULL, *stdef3;
   COMMANDS *stcmd1 = topCommand, *stcmd2 = NULL, *stcmd3;

   while( stdef1 != endDefine )
   {
      stdef3 = stdef1->last;
      stdef1->last = stdef2;
      stdef2 = stdef1;
      stdef1 = stdef3;
   }
   while( stcmd1 != endCommand )
   {
      stcmd3 = stcmd1->last;
      stcmd1->last = stcmd2;
      stcmd2 = stcmd1;
      stcmd1 = stcmd3;
   }

   if ((handl_o = fopen("hbpp.out", "wt" )) == NULL)
   {
      printf( "\nCan't open hbpp.out\n" );
      return;
   }

   num = 1;
   while( stdef2 != NULL )
   {
      fprintf( handl_o,"\n   static DEFINES sD___%i = ",num );
      fprintf( handl_o,"{\"%s\",", stdef2->name);
      if( stdef2->pars )
         fprintf(handl_o, "\"%s\",", stdef2->pars);
      else
         fprintf(handl_o, "NULL,");
      fprintf(handl_o, "%d,", stdef2->npars);
      if( stdef2->value )
         fprintf(handl_o, "\"%s\"", stdef2->value);
      else
         fprintf(handl_o, "NULL");
      if( num == 1 )
         fprintf(handl_o, ", NULL };");
      else
         fprintf(handl_o, ", &sD___%i };", num-1);
      stdef2 = stdef2->last;
      num++;
   }
   fprintf( handl_o,"\n   DEFINES *topDefine = " );
   if( num == 1 )
      fprintf( handl_o,"NULL;" );
   else
      fprintf( handl_o," = &sD___%i;\n",num-1 );

   num = 1;
   while( stcmd2 != NULL )
   {
      fprintf( handl_o,"\n   static COMMANDS sC___%i = ",num );
      fprintf( handl_o,"{%d,\"%s\",",stcmd2->com_or_xcom, stcmd2->name);
      if (stcmd2->mpatt !=NULL)
      {
         len_mpatt = strocpy( sLine, stcmd2->mpatt );
         while( (ipos = pp_strAt( "\1", 1, sLine, len_mpatt) ) > 0 )
         {
            pp_Stuff ( "\\1", sLine+ipos-1, 2, 1, len_mpatt );
            len_mpatt++;
         }
         fprintf(handl_o, "\"%s\",", sLine);
      }
      else
         fprintf(handl_o, "NULL,");
      if ( stcmd2->value !=NULL )
      {
         len_value = strocpy( sLine, stcmd2->value );
         while( (ipos = pp_strAt( "\1", 1, sLine, len_value) ) > 0 )
         {
            pp_Stuff ( "\\1", sLine+ipos-1, 2, 1, len_value );
            len_value++;
         }
         if( len_mpatt + len_value > 80 )
            fprintf( handl_o, "\n       " );
         fprintf(handl_o, "\"%s\"",sLine);
      }
      else fprintf(handl_o, "NULL");
      if( num == 1 )
         fprintf(handl_o, ",NULL };");
      else
         fprintf(handl_o, ",&sC___%i };", num-1);
      stcmd2 = stcmd2->last;
      num++;
   }
   fprintf( handl_o,"\n   COMMANDS *topCommand = " );
   if( num == 1 )
      fprintf( handl_o,"NULL;" );
   else
      fprintf( handl_o," = &sC___%i;\n",num-1 );

   stcmd1 = topTranslate, stcmd2 = NULL;
   while( stcmd1 != NULL )
   {
      stcmd3 = stcmd1->last;
      stcmd1->last = stcmd2;
      stcmd2 = stcmd1;
      stcmd1 = stcmd3;
   }
   num = 1;
   while( stcmd2 != NULL )
   {
      fprintf( handl_o,"\n   static COMMANDS sC___%i = ",num );
      fprintf( handl_o,"{%d,\"%s\",",stcmd2->com_or_xcom, stcmd2->name);
      if (stcmd2->mpatt !=NULL)
      {
         len_mpatt = strocpy( sLine, stcmd2->mpatt );
         while( (ipos = pp_strAt( "\1", 1, sLine, len_mpatt) ) > 0 )
         {
            pp_Stuff ( "\\1", sLine+ipos-1, 2, 1, len_mpatt );
            len_mpatt++;
         }
         fprintf(handl_o, "\"%s\",", sLine);
      }
      else
         fprintf(handl_o, "NULL,");
      if ( stcmd2->value !=NULL )
      {
         len_value = strocpy( sLine, stcmd2->value );
         while( (ipos = pp_strAt( "\1", 1, sLine, len_value) ) > 0 )
         {
            pp_Stuff ( "\\1", sLine+ipos-1, 2, 1, len_value );
            len_value++;
         }
         if( len_mpatt + len_value > 80 )
            fprintf( handl_o, "\n       " );
         fprintf(handl_o, "\"%s\"",sLine);
      }
      else fprintf(handl_o, "NULL");
      if( num == 1 )
         fprintf(handl_o, ",NULL };");
      else
         fprintf(handl_o, ",&sC___%i };", num-1);
      stcmd2 = stcmd2->last;
      num++;
   }
   fprintf( handl_o,"\n   COMMANDS *topTranslate = " );
   if( num == 1 )
      fprintf( handl_o,"NULL;" );
   else
      fprintf( handl_o," = &sT___%i;",num );

   fclose(handl_o);
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
    pPath->pNext = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
    pPath = pPath->pNext;
  }
  else
  {
    *pSearchList =pPath =(PATHNAMES *)hb_xgrab( sizeof(PATHNAMES) );
  }
  pPath->pNext  = NULL;
  pPath->szPath = szPath;
}

void GenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
  printf( "\r(%i) ", nline );
  printf( "Error %c%i  ", cPrefix, iError );
  printf( _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "\n\n" );

  exit( EXIT_FAILURE );
}

void GenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2)
{
    if( _bWarnings && iWarning < WARN_ASSIGN_SUSPECT ) /* TODO: add switch to set level */
    {
        printf( "\r(%i) ", nline );
        printf( "Warning %c%i  ", cPrefix, iWarning );
        printf( _szWarnings[ iWarning - 1 ], szWarning1, szWarning2 );
        printf( "\n" );
    }
}

#define IS_PATH_SEP( c ) ( strchr( OS_PATH_DELIMITER_LIST, ( c ) ) != NULL )

/* Split given filename into path, name and extension */
PHB_FNAME hb_fsFNameSplit( char * szFilename )
{
   PHB_FNAME pName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   int iLen = strlen( szFilename );
   int iSlashPos;
   int iDotPos;
   int iPos;

   pName->szPath =
   pName->szName =
   pName->szExtension = NULL;

   iSlashPos = iLen - 1;
   iPos = 0;

   while( iSlashPos >= 0 && !IS_PATH_SEP( szFilename[ iSlashPos ] ) )
      --iSlashPos;

   if( iSlashPos == 0 )
   {
      /* root path -> \filename */
      pName->szBuffer[ 0 ] = OS_PATH_DELIMITER;
      pName->szBuffer[ 1 ] = '\0';
      pName->szPath = pName->szBuffer;
      iPos = 2; /* first free position after the slash */
   }
   else if( iSlashPos > 0 )
   {
      /* If we are after a drive letter let's keep the following backslash */
      if( IS_PATH_SEP( ':' ) &&
         ( szFilename[ iSlashPos ] == ':' || szFilename[ iSlashPos - 1 ] == ':' ) )
      {
         /* path with separator -> d:\path\filename or d:path\filename */
         memcpy( pName->szBuffer, szFilename, iSlashPos + 1 );
         pName->szBuffer[ iSlashPos + 1 ] = '\0';
         iPos = iSlashPos + 2; /* first free position after the slash */
      }
      else
      {
         /* path with separator -> path\filename */
         memcpy( pName->szBuffer, szFilename, iSlashPos );
         pName->szBuffer[ iSlashPos ] = '\0';
         iPos = iSlashPos + 1; /* first free position after the slash */
      }

      pName->szPath = pName->szBuffer;
   }

   iDotPos = iLen - 1;
   while( iDotPos > iSlashPos && szFilename[ iDotPos ] != '.' )
      --iDotPos;

   if( ( iDotPos - iSlashPos ) > 1 )
   {
      /* the dot was found
       * and there is at least one character between a slash and a dot
       */
      if( iDotPos == iLen - 1 )
      {
         /* the dot is the last character - use it as extension name */
         pName->szExtension = pName->szBuffer + iPos;
         pName->szBuffer[ iPos++ ] = '.';
         pName->szBuffer[ iPos++ ] = '\0';
      }
      else
      {
         pName->szExtension = pName->szBuffer + iPos;
         /* copy rest of the string with terminating ZERO character */
         memcpy( pName->szExtension, szFilename + iDotPos + 1, iLen - iDotPos );
         iPos += iLen - iDotPos;
      }
   }
   else
      /* there is no dot in the filename or it is  '.filename' */
      iDotPos = iLen;

   pName->szName = pName->szBuffer + iPos;
   memcpy( pName->szName, szFilename + iSlashPos + 1, iDotPos - iSlashPos - 1 );
   pName->szName[ iDotPos - iSlashPos - 1 ] = '\0';

   return pName;
}

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char * szFileName, PHB_FNAME pFileName )
{
   if( pFileName->szPath && pFileName->szPath[ 0 ] )
   {
      /* we have not empty path specified */
      int iLen = strlen( pFileName->szPath );

      strcpy( szFileName, pFileName->szPath );

      /* if the path is a root directory then we don't need to add path separator */
      if( !( IS_PATH_SEP( pFileName->szPath[ 0 ] ) && pFileName->szPath[ 0 ] == '\0' ) )
      {
         /* add the path separator only in cases:
          *  when a name doesn't start with it
          *  when the path doesn't end with it
          */
         if( !( IS_PATH_SEP( pFileName->szName[ 0 ] ) || IS_PATH_SEP( pFileName->szPath[ iLen-1 ] ) ) )
         {
            szFileName[ iLen++ ] = OS_PATH_DELIMITER;
            szFileName[ iLen ] = '\0';
         }
      }
      strcpy( szFileName + iLen, pFileName->szName );
   }
   else
      strcpy( szFileName, pFileName->szName );

   if( pFileName->szExtension )
   {
      int iLen = strlen( szFileName );

      if( !( pFileName->szExtension[ 0 ] == '.' || szFileName[ iLen-1 ] == '.') )
      {
         /* add extension separator only when extansion doesn't contain it */
         szFileName[ iLen++ ] = '.';
         szFileName[ iLen ] = '\0';
      }
      strcpy( szFileName + iLen, pFileName->szExtension );
   }

   return szFileName;
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize );

   if( ! pMem )
      GenError( _szPErrors, 'P', ERR_PPMEMALLOC, NULL, NULL );

   return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   void * pResult = realloc( pMem, ulSize );

   if( ! pResult )
      GenError( _szPErrors, 'P', ERR_PPMEMREALLOC, NULL, NULL );

   return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
      free( pMem );
   else
      GenError( _szPErrors, 'P', ERR_PPMEMFREE, NULL, NULL );
}

