/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor core module
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

#include <stdlib.h>
#if ( defined(_MSC_VER) || defined(__IBMCPP__) || defined(__MINGW32_) )
   #include <memory.h>
#elif defined(__GNUC__)
   #include <string.h>
   #include <unistd.h>
#else
   #include <mem.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include "hbpp.h"
#include "hberrors.h"

int Hp_Parse( FILE*, FILE* );
int ParseDirective( char* );                    /* Parsing preprocessor directives ( #... ) */
int ParseDefine( char* );                       /* Process #define directive */
DEFINES* AddDefine ( char*, char* );            /* Add new #define to a linked list */
int ParseUndef( char* );                        /* Process #undef directive */
int ParseIfdef( char*, int);                    /* Process #ifdef directive */
void ParseCommand( char*, int, int );           /* Process #command or #translate directive */
void ConvertPatterns ( char*, int, char*, int ); /* Converting result pattern in #command and #translate */
COMMANDS* AddCommand ( char * );                /* Add new #command to an array  */
COMMANDS* AddTranslate ( char * );              /* Add new #translate to an array  */
int ParseExpression( char*, char* );            /* Parsing a line ( without preprocessor directive ) */
int WorkDefine ( char**, char*, DEFINES * );    /* Replace fragment of code with a #defined result text */
int WorkPseudoF ( char**, char*, DEFINES*);     /* Replace pseudofunction with a #defined result text */
int WorkCommand ( char*, char*, COMMANDS* );
int WorkTranslate ( char*, char*, COMMANDS*, int* );
int CommandStuff ( char *, char *, char *, int*, int, int );
int RemoveSlash( char * );
int WorkMarkers( char**, char**, char*, int* );
int getExpReal ( char *, char **, int, int );
int isExpres ( char* );
int TestOptional( char*, char* );
void SkipOptional( char** );

DEFINES* DefSearch(char *);
COMMANDS* ComSearch( char *, COMMANDS* );
COMMANDS* TraSearch( char *, COMMANDS* );
void SearnRep( char*,char*,int,char*,int*);
int ReplacePattern ( char, char*, int, char*, int );
void pp_rQuotes( char *, char * );
int pp_RdStr(FILE*,char *,int,int,char*,int*,int*);
int pp_WrStr(FILE*,char *);
int pp_strAt(char *, int, char*, int);
int md_strAt(char *, int, char*, int);
int IsInStr ( char, char*);
void pp_Stuff (char*, char*, int, int, int);
int strocpy (char*, char* );
int stroncpy (char*, char*, int);
int strincpy (char*, char*);
int truncmp (char**, char**, int);
int strincmp (char*, char**);
int strolen ( char* );
void stroupper ( char* );
int strotrim ( char* );
char* strodup ( char * );
int NextWord ( char**, char*, int);
int NextName ( char**, char* );
int NextParm ( char**, char* );
int Include( char *, PATHNAMES *, FILE** );
BOOL OpenInclude( char *, PATHNAMES *, FILE**, BOOL bStandardOnly );

#define ISNAME(c)  (isalnum(c) || (c)=='_' || (c) > 0x7e)
#define MAX_NAME 255
#define PATTERN_SIZE 2048

#define STATE_INIT 0
#define STATE_NORMAL 1
#define STATE_COMMENT 2
#define STATE_QUOTE1 3
#define STATE_QUOTE2 4
#define STATE_ID_END 5
#define STATE_ID 6
#define STATE_EXPRES 7
#define STATE_EXPRES_ID 8
#define STATE_BRACKET 9

#define IT_EXPR 1
#define IT_ID 2
#define IT_COMMA 3
#define IT_ID_OR_EXPR 4

int ParseState = 0;
int lInclude = 0;
int *aCondCompile, nCondCompile = 0, maxCondCompile = 5;
int nline=0;
int aIsRepeate[5],Repeate;
int numBrackets;
char groupchar;

/* Table with parse errors */
char * _szPErrors[] =
{
   "Can\'t open #include file: \'%s\'",
   "#else does not match #ifdef",
   "#endif does not match #ifdef",
   "Bad filename in #include",
   "#define without parameters",
   "Missing => in #translate/#command",
   "Error in pattern definition",
   "Cycled #define",
   "Invalid name follows #: \'%s\'",
   "#error: \'%s\'",
   "Memory allocation error",
   "Memory reallocation error",
   "Freeing a NULL memory pointer"
};

/* Table with parse warnings */
char * _szPWarnings[] =
{
   "Non directive in include file"
};

int ParseDirective( char* sLine )
{
   char sDirective[MAX_NAME];
   int i;
   FILE* handl_i;

   i = NextName( &sLine, sDirective );
   stroupper( sDirective );

   SKIPTABSPACES(sLine);

   if ( i == 4 && memcmp ( sDirective, "ELSE", 4 ) == 0 )
   {     /* ---  #else  --- */
     if ( nCondCompile == 0 )
       GenError( _szPErrors, 'P', ERR_DIRECTIVE_ELSE, NULL, NULL );
     else if ( nCondCompile == 1 || aCondCompile[nCondCompile-2] )
       aCondCompile[nCondCompile-1] = 1 - aCondCompile[nCondCompile-1];
   }

   else if ( i == 5 && memcmp ( sDirective, "ENDIF", 5 ) == 0 )
   {     /* --- #endif  --- */
     if ( nCondCompile == 0 )
       GenError( _szPErrors, 'P', ERR_DIRECTIVE_ENDIF, NULL, NULL );
     else nCondCompile--;
   }

   else if ( i == 5 && memcmp ( sDirective, "IFDEF", 5 ) == 0 )
     ParseIfdef ( sLine, TRUE ); /* --- #ifdef  --- */

   else if ( i == 6 && memcmp ( sDirective, "IFNDEF", 6 ) == 0 )
     ParseIfdef ( sLine, FALSE ); /* --- #ifndef  --- */

   else if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
 {
  if ( i == 7 && memcmp ( sDirective, "INCLUDE", 7 ) == 0 )
  {    /* --- #include --- */
   char cDelimChar;

   if ( *sLine != '\"' && *sLine != '\'' && *sLine != '<' )
     GenError( _szPErrors, 'P', ERR_WRONG_NAME, NULL, NULL );

   cDelimChar = *sLine;
   if (cDelimChar == '<')
     cDelimChar = '>';

   sLine++; i = 0;
   while ( *(sLine+i) != '\0' && *(sLine+i) != cDelimChar ) i++;
   if ( *(sLine+i) != cDelimChar )
     GenError( _szPErrors, 'P', ERR_WRONG_NAME, NULL, NULL );
   *(sLine+i) = '\0';

   /*   if ((handl_i = fopen(sLine, "r")) == NULL) */
   if ( !OpenInclude( sLine, _pIncludePath, &handl_i, (cDelimChar == '>') ) )
     GenError( _szPErrors, 'P', ERR_CANNOT_OPEN, sLine, NULL );
   lInclude++;
   Hp_Parse(handl_i, 0 );
   lInclude--;
   fclose(handl_i);
  }

  else if ( i == 6 && memcmp ( sDirective, "DEFINE", 6 ) == 0 )
   ParseDefine ( sLine );   /* --- #define  --- */

  else if ( i == 5 && memcmp ( sDirective, "UNDEF", 5 ) == 0 )
   ParseUndef ( sLine );    /* --- #undef  --- */

  else if ( (i == 7 && memcmp ( sDirective, "COMMAND", 7 ) == 0) ||
            (i == 8 && memcmp ( sDirective, "XCOMMAND", 8 ) == 0) )
                                /* --- #command  --- */
   ParseCommand ( sLine, (i==7)? FALSE:TRUE, TRUE );

  else if ( (i == 9 && memcmp ( sDirective, "TRANSLATE", 9 ) == 0) ||
            (i == 10 && memcmp ( sDirective, "XTRANSLATE", 10 ) == 0) )
                                /* --- #translate  --- */
   ParseCommand ( sLine, (i==9)? FALSE:TRUE, FALSE );

  else if ( i == 6 && memcmp ( sDirective, "STDOUT", 6 ) == 0 )
   printf ( "%s\n", sLine ); /* --- #stdout  --- */

  else if ( i == 5 && memcmp ( sDirective, "ERROR", 5 ) == 0 )
  {                        /* --- #error  --- */
   GenError( _szPErrors, 'P', ERR_EXPLICIT, sLine, NULL );
  }
  else if ( i == 4 && memcmp ( sDirective, "LINE", 4 ) == 0 )
    return -1;
  else GenError( _szPErrors, 'P', ERR_WRONG_DIRECTIVE, sDirective, NULL );
 }
 return 0;
}

int ParseDefine( char* sLine)
{
   char defname[MAX_NAME], pars[MAX_NAME];
   int i, npars = -1;
   DEFINES *lastdef;

   SKIPTABSPACES( sLine );
   if( isalpha( *sLine ) || *sLine == '_' || *sLine > 0x7e )
   {
      NextName( &sLine, defname );
      if ( *sLine == '(' ) /* If pseudofunction was found */
      {
         sLine++; i = 0;
         npars = 0;
         while ( *sLine != '\0' && *sLine != ')')
         {
            if ( *sLine == ',' ) npars++;
            if ( *sLine != ' ' && *sLine != '\t' ) *(pars+i++) = *sLine;
            sLine++;
         }
         if ( i > 0 ) npars++;
         *(pars+i) = '\0';
         sLine++;
      }
      SKIPTABSPACES(sLine);

      lastdef = AddDefine ( defname, ( *sLine == '\0' )? NULL : sLine );

      lastdef->npars = npars;
      lastdef->pars = ( npars == 0 )? NULL : strodup ( pars );
   }
   else
      GenError( _szPErrors, 'P', ERR_DEFINE_ABSENT, NULL, NULL );

   return 0;
}

DEFINES* AddDefine ( char* defname, char* value )
{
   DEFINES* stdef = DefSearch( defname );

   if ( stdef != NULL )
     stdef->pars = NULL;
   else
   {
     stdef = ( DEFINES * ) hb_xgrab( sizeof( DEFINES ) );
     stdef->last = topDefine;
     topDefine = stdef;
     stdef->name = strodup ( defname );
   }
   stdef->value = ( value == NULL )? NULL : strodup ( value );
   return stdef;
}

int ParseUndef( char* sLine)
{
 char defname[MAX_NAME];
 DEFINES* stdef;

 NextWord( &sLine, defname, FALSE );

 if ( ( stdef = DefSearch(defname) ) != NULL )
 {
  stdef->name = NULL;
 }
 return 0;
}

int ParseIfdef( char* sLine, int usl)
{
   char defname[MAX_NAME];
   DEFINES *stdef;

   if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
   {
     NextWord( &sLine, defname, FALSE );
     if ( *defname == '\0' )
       GenError( _szPErrors, 'P', ERR_DEFINE_ABSENT, NULL, NULL );
   }
   if ( nCondCompile == maxCondCompile )
   {
     maxCondCompile += 5;
     aCondCompile = (int*)hb_xrealloc( aCondCompile, sizeof( int ) * maxCondCompile );
   }
   if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
   {
     if ( ( (stdef = DefSearch(defname)) != NULL && usl )
       || ( stdef == NULL && !usl ) ) aCondCompile[nCondCompile] = 1;
     else aCondCompile[nCondCompile] = 0;
   }
   else aCondCompile[nCondCompile] = 0;
   nCondCompile++;
   return 0;
}

DEFINES* DefSearch(char *defname)
{
   int j;
   DEFINES * stdef = topDefine;

   while( stdef != NULL )
   {
     for ( j=0; *(stdef->name+j) == *(defname+j) &&
             *(stdef->name+j) != '\0'; j++ );
     if ( *(stdef->name+j) == *(defname+j) ) return stdef;
     stdef = stdef->last;
   }
   return NULL;
}

COMMANDS* ComSearch(char *cmdname, COMMANDS *stcmdStart)
{
   int j;
   COMMANDS *stcmd = ( stcmdStart )? stcmdStart:topCommand;

   while( stcmd != NULL )
   {
      for ( j=0; (*(stcmd->name+j)==toupper(*(cmdname+j))) &&
          (*(stcmd->name+j)!='\0') &&
          ((stcmd->com_or_xcom)? 1:(j<4 || ISNAME(*(cmdname+j+1)))); j++ );
      if ( (*(stcmd->name+j)==toupper(*(cmdname+j))) ||
           ( !stcmd->com_or_xcom && j >= 4 && *(stcmd->name+j)!='\0'
           && *(cmdname+j) == '\0' ) )
         break;
      stcmd = stcmd->last;
   }

   return stcmd;
}

COMMANDS* TraSearch(char *cmdname, COMMANDS *sttraStart)
{
   int j;
   COMMANDS *sttra = ( sttraStart )? sttraStart:topTranslate;

   while( sttra != NULL )
   {
      for ( j=0; *(sttra->name+j)==toupper(*(cmdname+j)) &&
          *(sttra->name+j)!='\0' &&
          ((sttra->com_or_xcom)? 1:(j<4 || ISNAME(*(cmdname+j+1)))); j++ );
      if ( *(sttra->name+j)==toupper(*(cmdname+j)) ||
            ( !sttra->com_or_xcom && j >= 4 &&
            *(sttra->name+j)!='\0' && *(cmdname+j) == '\0' ) )
         break;
      sttra = sttra->last;
   }
   return sttra;
}

void ParseCommand( char* sLine, int com_or_xcom, int com_or_tra )
{
   static char mpatt[PATTERN_SIZE], rpatt[PATTERN_SIZE];
   char cmdname[MAX_NAME];
   COMMANDS *stcmd;
   int mlen,rlen;
   int ipos;

   NextWord( &sLine, cmdname, FALSE );
   stroupper( cmdname );
   SKIPTABSPACES(sLine);

   if ( (ipos = pp_strAt( "=>", 2, sLine, strolen(sLine) )) > 0 )
      stroncpy( mpatt, sLine, ipos-1 );
   else
      GenError( _szPErrors, 'P', ERR_COMMAND_DEFINITION, NULL, NULL );
   mlen = strotrim( mpatt );

   sLine += ipos + 1;
   SKIPTABSPACES(sLine);
   strocpy( rpatt, sLine );
   rlen = strotrim( rpatt );

   ConvertPatterns ( mpatt, mlen, rpatt, rlen );

   if ( com_or_tra )
      stcmd = AddCommand ( cmdname );
   else
      stcmd = AddTranslate ( cmdname );

   stcmd->com_or_xcom = com_or_xcom;
   stcmd->mpatt = strodup ( mpatt );
   stcmd->value = ( rlen > 0 )? strodup ( rpatt ) : NULL;
}

/* ConvertPatterns()
 * Converts result pattern in #command and #translate to inner format
 */

void ConvertPatterns ( char *mpatt, int mlen, char *rpatt, int rlen )
{
   int i = 0, ipos, ifou;
   int explen,rmlen;
   char exppatt[MAX_NAME], expreal[5] = "\1  0";
   char lastchar = '@', exptype;
   char *ptr;

   while ( *(mpatt+i) != '\0' )
   {
     if ( *(mpatt+i) == '<' )
     {  /* Drag match marker, determine it type */
       explen = 0; ipos = i; i++; exptype = '0';
       if ( *(mpatt+i) == '*' )
         { exptype = '3'; i++; }
       else if ( *(mpatt+i) == '(' )
         { exptype = '4'; i++; }
       while ( *(mpatt+i) != '>' )
       {
         if ( *(mpatt+i) == ',' )
         {
           exptype = '1';
           while ( *(mpatt+i) != '>' ) i++;
           break;
         }
         else if ( *(mpatt+i) == ':' )
           { exptype = '2'; break; }
         *(exppatt+explen++) = *(mpatt+i++);
       }
       if ( exptype == '3' )
       {
         if ( *(exppatt+explen-1) == '*' ) explen--;
         else
           GenError( _szPErrors, 'P', ERR_PATTERN_DEFINITION, NULL, NULL );
       }
       else if ( exptype == '4' )
       {
         if ( *(exppatt+explen-1) == ')' ) explen--;
         else
           GenError( _szPErrors, 'P', ERR_PATTERN_DEFINITION, NULL, NULL );
       }
       rmlen = i - ipos + 1;
          /* Convert match marker into inner format */
       lastchar = (char) ( (unsigned int)lastchar + 1 );
       expreal[1] = lastchar;
       expreal[2] = exptype;
       pp_Stuff ( expreal, mpatt+ipos, 4, rmlen, mlen );
       mlen += 4 - rmlen; i += 4 - rmlen;

          /* Look for appropriate result markers */
       ptr = rpatt;
       while ( (ifou = pp_strAt( exppatt, explen, ptr, rlen-(ptr-rpatt) )) > 0 )
       {
          /* Convert result marker into inner format */
         ptr += ifou;
         if ( *(ptr-2) == '<' && *(ptr+explen-1) == '>' &&
               *(ptr-3) != '\\'  && *(ptr+explen-2) != '\\' )  /* <...> */
         {
           if ( *(ptr-3) == '#' && *(ptr-4) != '\\' )          /* #<...> */
             { exptype = '1'; ptr -= 3; rmlen = explen+3; }
           else
             { exptype = '0'; ptr -= 2; rmlen = explen+2; }
         }
         else if ( *(ptr-3) == '<' && *(ptr+explen) == '>' &&
               *(ptr-4) != '\\' && *(ptr+explen-1) != '\\' )   /* < ... > */
         {
           ptr -= 2;
           if ( *ptr == '\"' ) exptype = '2';
           else if ( *ptr == '(' ) exptype = '3';
           else if ( *ptr == '{' ) exptype = '4';
           else if ( *ptr == '.' ) exptype = '5';
           ptr--;
           rmlen = explen+4;
         }
         else continue;
         expreal[2] = exptype;
         pp_Stuff ( expreal, ptr, 4, rmlen, rlen );
         rlen += 4 - rmlen;
       }
     }
     i++;
   }
}

COMMANDS* AddCommand ( char *cmdname )
{
   COMMANDS *stcmd;

   stcmd = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
   stcmd->last = topCommand;
   topCommand = stcmd;
   stcmd->name = strodup ( cmdname );
   return stcmd;
}

COMMANDS* AddTranslate ( char *traname )
{
   COMMANDS *sttra;

   sttra = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
   sttra->last = topTranslate;
   topTranslate = sttra;
   sttra->name = strodup ( traname );
   return sttra;
}

int ParseExpression( char* sLine, char* sOutLine )
{
   char sToken[MAX_NAME];
   char *ptri, *ptro, *ptrb;
   int lenToken, i, ipos, isdvig, lens;
   int ifou;
   int rezDef, rezTra, rezCom, kolpass = 0;
   DEFINES *stdef;
   COMMANDS *stcmd;

   do
   {
      strotrim ( sLine );
      rezDef = 0; rezTra = 0; rezCom = 0;
      isdvig = 0;
      do
      {
         ptro = sOutLine;
         ptri = sLine + isdvig;
         ipos = md_strAt( ";", 1, ptri, FALSE );
         if ( ipos > 0 ) *(ptri+ipos-1) = '\0';
         SKIPTABSPACES( ptri );
         if ( *ptri == '#' )
         {
            ParseDirective( ptri+1 );
            if ( ipos > 0 ) *( sLine + isdvig + ipos - 1 ) = ';';
            lens = strolen( sLine+isdvig );
            pp_Stuff ( " ", sLine+isdvig, 0, (ipos)? ipos:lens, lens );
            if( ipos > 0 ) ipos = 1;
         }
         else
         {   /* Look for macros from #define      */
            while ( ( lenToken = NextName( &ptri, sToken ) ) > 0 )
            if ( (stdef=DefSearch(sToken)) != NULL )
            {
               ptrb = ptri - lenToken;
               if ( ( i = WorkDefine ( &ptri, ptro, stdef ) ) >= 0 )
               {
                  rezDef++;
                  lens = strolen( ptrb );
                  if ( ipos > 0 )
                  {
                     *(ptrb+lens) = ';';
                     lens += strolen( ptrb+lens+1 );
                  }
                  pp_Stuff ( ptro, ptrb, i, ptri-ptrb, lens+1 );
                  if ( ipos > 0 )
                  {
                     ipos += i - (ptri-ptrb);
                     *(sLine + isdvig + ipos - 1) = '\0';
                  }
                  ptri += i - (ptri-ptrb);
               }
            }

          /* Look for definitions from #translate    */
            stcmd = topTranslate;
            while( stcmd != NULL )
            {
               ptri = sLine + isdvig;
               lenToken = strolen(stcmd->name);
               while( ( ifou = md_strAt( stcmd->name, lenToken,
                                                 ptri, 0 )) > 0 )
               {
                  ptri += ifou -1;
                  if( (i = WorkTranslate( ptri+lenToken, ptro, stcmd, &lens )) >= 0 )
                  {
                     lens += lenToken;
                     while ( lens > 0 &&
                             (*(ptri+lens-1)==' ' || *(ptri+lens-1)=='\t') )
                        lens--;
                     if ( ipos > 0 )  *(sLine+isdvig+ipos-1) = ';';
                     pp_Stuff( ptro, ptri, i, lens, strolen(ptri) );
                     rezTra = 1;
                     if ( ipos > 0 )
                     {
                        ipos += i - lens;
                        *(sLine+isdvig+ipos-1) = '\0';
                     }
                     ptri += i;
                  }
                  else
                     ptri += lenToken;
               }
               stcmd = stcmd->last;
            }

               /* Look for definitions from #command      */
            if ( kolpass < 3 )
            {
               ptri = sLine + isdvig;
               SKIPTABSPACES( ptri );
               if ( ISNAME(*ptri) )
                  NextName( &ptri, sToken );
               else
               {
                  i = 0;
                  while ( *ptri != ' ' && *ptri != '\t' && *ptri != '\0' &&
                        *ptri != '\"' && *ptri != '\'' && *ptri != '(' &&
                        !ISNAME(*ptri) )
                  {
                     *(sToken+i) = *ptri++;
                     i++;
                  }
                  *(sToken+i) = '\0';
               }
               SKIPTABSPACES( ptri );

               if ( ( *ptri == '\0' || ( *ptri != '=' &&
                  (!IsInStr(*ptri,":/*+-") || *(ptri+1) != '=') &&
                  ( *ptri != '+' || *(ptri+1) != '+' ) &&
                  ( *ptri != '-' || *(ptri+1) != '-' ) &&
                  ( *ptri != '-' || *(ptri+1) != '>' ) ) )
                  && ( stcmd = ComSearch(sToken,NULL) ) != NULL )
               {
                  ptro = sOutLine;
                  i = WorkCommand( ptri, ptro, stcmd );
                  ptri = sLine + isdvig;
                  if ( ipos > 0 ) *(ptri+ipos-1) = ';';
                  if ( i >= 0 )
                  {
                     if ( isdvig + ipos > 0 )
                     {
                        lens = strolen( sLine+isdvig );
                        pp_Stuff ( ptro, sLine+isdvig, i, (ipos)? ipos-1:lens, lens );
                        if( ipos > 0 ) ipos = i + 1;
                     }
                     else
                        memcpy ( sLine, sOutLine, i+1);
                  }
                  rezCom = 1;
               }
               else if ( ipos > 0 ) *(sLine+isdvig+ipos-1) = ';';
            }
            else if ( ipos > 0 )
               *(sLine+isdvig+ipos-1) = ';';
         }
         isdvig += ipos;
      }
      while ( ipos != 0 );
      kolpass++;
      if( kolpass > 20 && rezDef )
         GenError( _szPErrors, 'P', ERR_RECURSE, NULL, NULL );
   }
   while ( rezDef || rezTra || rezCom );

   return 0;
}

int WorkDefine ( char** ptri, char* ptro, DEFINES *stdef )
{
   int npars, lens;
   char *ptr;

   if ( stdef->npars < 0 )
      lens = strocpy( ptro,stdef->value );
   else
   {
      SKIPTABSPACES( *ptri );
      if ( **ptri == '(' )
      {
         npars = 0; ptr = *ptri;
         do
         {
            ptr++;
            if ( NextParm( &ptr, NULL ) > 0 ) npars++;
         }
         while ( *ptr != ')' && *ptr != '\0' );
         if ( *ptr == ')' && stdef->npars == npars )
            lens = WorkPseudoF( ptri, ptro, stdef );
         else return -1;
      }
      else return -1;
   }
   return lens;
}

int WorkPseudoF ( char** ptri, char* ptro, DEFINES *stdef )
{
   char parfict[MAX_NAME], *ptrreal;
   char *ptrb;
   int ipos, ifou, ibeg;
   int lenfict, lenreal, lenres;

   lenres = strocpy( ptro, stdef->value );  /* Copying value of macro to destination string  */

   if ( stdef->pars )
   {
      ipos = 0; ibeg = 0;
      do                               /* Parsing through parameters */
      {                                /* in macro definition        */
         if ( *(stdef->pars+ipos) == ',' || *(stdef->pars+ipos) == '\0' )
         {
            *(parfict+ipos-ibeg) = '\0';
            lenfict = ipos - ibeg;

            if ( **ptri != ')' )
            {
               (*ptri)++;             /* Get next real parameter */
               SKIPTABSPACES( *ptri );
               ptrreal = *ptri;
               lenreal = NextParm( ptri, NULL);

               ptrb = ptro;
               while ( (ifou = pp_strAt( parfict, lenfict, ptrb, lenres-(ptrb-ptro) )) > 0 )
               {
                  ptrb = ptrb+ifou-1;
                  if ( !ISNAME(*(ptrb-1)) && !ISNAME(*(ptrb+lenfict)) )
                  {
                     pp_Stuff ( ptrreal, ptrb, lenreal, lenfict, lenres );
                     lenres += lenreal - lenfict;
                     ptrb += lenreal;
                  }
                  else ptrb++;
               }
               ibeg = ipos+1;
            }
         }
         else *(parfict+ipos-ibeg) = *(stdef->pars+ipos);
         if ( *(stdef->pars+ipos) == '\0' ) break;
         ipos++;
      }
      while ( 1 );
   }
   else  while ( **ptri != ')' ) (*ptri)++;
   (*ptri)++;
   return lenres;
}

int WorkCommand ( char* ptri, char* ptro, COMMANDS *stcmd )
{
   int rez;
   int lenres;
   char *ptrmp;
   char *sToken = stcmd->name;

   do
   {
      lenres = strocpy ( ptro, stcmd->value );   /* Copying result pattern */
      ptrmp = stcmd->mpatt;                      /* Pointer to a match pattern */
      Repeate = 0;
      groupchar = '@';
      rez = CommandStuff ( ptrmp, ptri, ptro, &lenres, TRUE, stcmd->com_or_xcom );

      stcmd = stcmd->last;
      if ( rez < 0 && stcmd != NULL ) stcmd = ComSearch(sToken, stcmd);
   }
   while ( rez < 0 && stcmd != NULL );

   *(ptro+lenres) = '\0';
   if ( rez >= 0 ) return lenres;
   return -1;
}

int WorkTranslate ( char* ptri, char* ptro, COMMANDS *sttra, int *lens )
{
   int rez;
   int lenres;
   char *ptrmp;
   char *sToken = sttra->name;

   do
   {
      lenres = strocpy ( ptro, sttra->value );
      ptrmp = sttra->mpatt;
      Repeate = 0;
      groupchar = '@';
      rez = CommandStuff ( ptrmp, ptri, ptro, &lenres, FALSE, sttra->com_or_xcom );

      sttra = sttra->last;
      if ( rez < 0 && sttra != NULL ) sttra = TraSearch(sToken, sttra);
   }
   while ( rez < 0 && sttra != NULL );

   *(ptro+lenres) = '\0';
   if ( rez >= 0 )
   {
      *lens = rez;
      return lenres;
   }
   return -1;
}

int CommandStuff ( char *ptrmp, char *inputLine, char * ptro, int *lenres, int com_or_tra, int com_or_xcom )
{
  int endTranslation = FALSE, ipos;
  char *lastopti[2], *strtopti = NULL, *strtptri = NULL;
  char *ptri = inputLine, *ptr, tmpname[MAX_NAME];;

  numBrackets = 0;
  SKIPTABSPACES( ptri );
  if ( ptrmp == NULL ) { if ( *ptri != '\0' ) return -1; }
  else
    while ( *ptri != '\0' && !endTranslation )
    {
      SKIPTABSPACES( ptrmp );
      if( *ptrmp == '[' && !numBrackets && !strtopti )
         strtopti = ptrmp;
      if( !numBrackets && strtopti && strtptri != ptri && ISNAME( *ptri ) )
      {
         strtptri = ptri;
         ptrmp = strtopti;
         ptr = ptri;
         ipos = NextName( &ptr, tmpname );
         ipos = md_strAt( tmpname, ipos, strtopti, TRUE );
         if( ipos && TestOptional( strtopti, strtopti+ipos-2 ) )
         {
            ptr = strtopti+ipos-2;
            while( *ptr != '[' && *ptr != ']' ) ptr--;
            if( *ptr != ']' )
               ptrmp = ptr;
         }
      }
      switch ( *ptrmp ) {
       case '[':
         numBrackets++;
         aIsRepeate[ Repeate ] = 0;
         lastopti[Repeate++] = ptrmp;
         ptrmp++;
         break;
       case ']':
         if( Repeate )
         {
            Repeate--;
            if( aIsRepeate[ Repeate ] )
            {
               if( ISNAME(*ptri) )
               {
                  ptr = ptri;
                  ipos = NextName( &ptr, tmpname );
                  ipos = md_strAt( tmpname, ipos, ptrmp, TRUE );
                  if( ipos && TestOptional( ptrmp+1, ptrmp+ipos-2 ) )
                  {
                     ptrmp = lastopti[Repeate];
                     ptrmp++;
                     Repeate++;
                     SkipOptional( &ptrmp );
                     numBrackets++;
                     ptrmp++;
                     strtptri = ptri;
                  }
                  else
                     ptrmp = lastopti[Repeate];
               }
               else
                  ptrmp = lastopti[Repeate];
            }
            else ptrmp++;
            numBrackets--;
         }
         else { numBrackets--; ptrmp++; }
         break;
       case ',':
         if( !numBrackets ) strtopti = NULL;
         if ( *ptri == ',' ) { ptrmp++; ptri++; }
         else
         {
           if ( numBrackets )
              SkipOptional( &ptrmp );
           else return -1;
         }
         break;
       case '\1':  /*  Match marker */
         if( !numBrackets ) strtopti = NULL;
         if ( !WorkMarkers( &ptrmp, &ptri, ptro, lenres ) )
         {
           if ( numBrackets )
              SkipOptional( &ptrmp );
           else return -1;
         }
         break;
       case '\0':
         if ( com_or_tra )
           return -1;
         else endTranslation = TRUE;
         break;
       default:    /*   Key word    */
         if( !numBrackets ) strtopti = NULL;
         ptr = ptri;
         if ( *ptri == ',' || truncmp( &ptri, &ptrmp, !com_or_xcom ) )
         {
           ptri = ptr;
           if ( numBrackets )
              SkipOptional( &ptrmp );
           else return -1;
         }
      }
      SKIPTABSPACES( ptri );
    };

  if ( *ptrmp != '\0' )
  {
    if ( Repeate ) { Repeate = 0; ptrmp = lastopti[0]; }
    numBrackets = 0;
    do
    {
      SKIPTABSPACES( ptrmp );
      if ( *ptrmp != '\0' )
        switch ( *ptrmp ) {
          case '[':
            ptrmp++;
            SkipOptional( &ptrmp );
            ptrmp++;
            break;
          case ']': ptrmp++; break;
          default:
            return -1;
        }
    }
    while ( *ptrmp != '\0' );
  }
  SearnRep( "\1","",0,ptro,lenres);

  *(ptro + *lenres) = '\0';
  *lenres = RemoveSlash( ptro );   /* Removing '\' from result string */
  if ( com_or_tra ) return 1; else return (ptri-inputLine);
}

int RemoveSlash( char *stroka )
{
  char *ptr = stroka;
  int State = STATE_INIT, lDirective = FALSE;
  int lenres = strolen( stroka );

  while( *ptr != '\0' )
  {
    switch( State ) {
      case STATE_INIT:
        if( *ptr != ' ' && *ptr != '\t' ) State = STATE_NORMAL;
        if( *ptr == '#' )  lDirective = TRUE;
      case STATE_NORMAL:
        if( *ptr == '\'' )  State = STATE_QUOTE1;
        else if( *ptr == '\"' )  State = STATE_QUOTE2;
        else if( *ptr == ';' )
        {
          State = STATE_INIT;
          lDirective = FALSE;
        }
        else if( !lDirective )
        {
          if( *ptr == '\\' && ( *(ptr+1) == '[' || *(ptr+1) == ']' ||
               *(ptr+1) == '{' || *(ptr+1) == '}' || *(ptr+1) == '<' ||
               *(ptr+1) == '>' ) )
          {
            pp_Stuff ( "", ptr, 0, 1, lenres - (ptr - stroka) );
            lenres--;
          }
        }
        break;
      case STATE_QUOTE1:
        if( *ptr == '\'' )  State = STATE_NORMAL;
        break;
      case STATE_QUOTE2:
        if( *ptr == '\"' )  State = STATE_NORMAL;
        break;
    }
    ptr++;
  }
  return lenres;
}

int WorkMarkers( char **ptrmp, char **ptri, char *ptro, int *lenres )
{
 char expreal[MAX_NAME], exppatt[MAX_NAME];
 int lenreal = 0, maxlenreal = STR_SIZE, lenpatt;
 int rezrestr, ipos;
 char *ptr, *ptrtemp;

         /* Copying a match pattern to 'exppatt' */
  lenpatt = stroncpy ( exppatt, *ptrmp, 4 );
  *ptrmp += 4;
  SKIPTABSPACES ( *ptrmp );
  if ( **ptri == ',' )
  {
    if ( numBrackets )
    {
//      SearnRep( exppatt,"",0,ptro,lenres);
      return 0;
    }
  }
  ptrtemp = *ptrmp;
  if ( *(exppatt+2) != '2' && *ptrtemp == ']' )
  {
    ptrtemp++;
    SKIPTABSPACES ( ptrtemp );
  }
  if ( *(exppatt+2) != '2' && *ptrtemp != '\1' && *ptrtemp != ',' &&
        *ptrtemp != '[' && *ptrtemp != ']' && *ptrtemp != '\0' )
  {
   lenreal = strincpy ( expreal, ptrtemp );
   if ( (ipos = md_strAt( expreal, lenreal, *ptri, TRUE )) > 0 )
   {
    if ( ptrtemp > *ptrmp )
    {
      if ( ipos == 1 )
      {
        if ( numBrackets )
        {
//          SearnRep( exppatt,"",0,ptro,lenres);
          return 0;
        }
      }
      else
      {
        maxlenreal = ipos - 1;
        lenreal = 0;
      }
    }
    else
    {
      lenreal = stroncpy( expreal, *ptri, ipos-1 );
      if ( ipos > 1 && isExpres ( expreal ) )
         *ptri += lenreal;
      else
      {
        if ( numBrackets )
        {
//          SearnRep( exppatt,"",0,ptro,lenres);
          return 0;
        }
      }
    }
   }
   else
   {
     if ( numBrackets )
     {
//       SearnRep( exppatt,"",0,ptro,lenres);
       return 0;
     }
     else lenreal = 0;
   }
  }

  if ( *(exppatt+2) == '4' )       /*  ----  extended match marker  */
  {
    if ( !lenreal ) lenreal = getExpReal ( expreal, ptri, FALSE, maxlenreal );
    SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else if ( *(exppatt+2) == '3' )  /*  ----  wild match marker  */
  {
    lenreal = strocpy ( expreal, *ptri );
    *ptri += lenreal;
    SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else if ( *(exppatt+2) == '2' )  /*  ---- restricted match marker  */
  {
   while ( **ptrmp != '>' ) *(exppatt+lenpatt++) = *((*ptrmp)++);
   *(exppatt+lenpatt) = '\0';
   (*ptrmp)++;

   ptr = exppatt + 4;
   rezrestr = 0;
   while ( *ptr != '\0' )
   {
     if ( *ptr == '&' )
     {
       if ( **ptri == '&' )
       {
         rezrestr = 1;
         /*  (*ptri)++; */
         lenreal = getExpReal ( expreal, ptri, FALSE, maxlenreal );
         SearnRep( exppatt,expreal,lenreal,ptro,lenres);
         break;
       }
       else ptr++;
     }
     else
     {
       SKIPTABSPACES( ptr );
           /* Comparing real parameter and restriction value */
       ptrtemp = ptr;
       if ( !strincmp ( *ptri, &ptr ) )
       {
         lenreal = stroncpy( expreal, *ptri, (ptr-ptrtemp) );
         *ptri += lenreal;
         SearnRep( exppatt,expreal,lenreal,ptro,lenres);
         rezrestr = 1;
         break;
       }
       else
       {
          while ( *ptr != ',' && *ptr != '\0' ) ptr++;
          if ( *ptr == ',' ) ptr++;
       }
     }
   }
   if ( rezrestr == 0 )
   {  /* If restricted match marker doesn't correspond to real parameter */
      if ( numBrackets )
      {
//        SearnRep( exppatt,"",0,ptro,lenres);
        return 0;
      }
      else return 0;
   }
  }
  else if ( *(exppatt+2) == '1' )  /*  ---- list match marker  */
  {
     if ( !lenreal ) lenreal = getExpReal ( expreal, ptri, TRUE, maxlenreal );
     SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else                             /*  ---- regular match marker  */
  {
               /* Copying a real expression to 'expreal' */
     if ( !lenreal ) lenreal = getExpReal ( expreal, ptri, FALSE, maxlenreal );
     SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  return 1;
}

int getExpReal ( char *expreal, char **ptri, int prlist, int maxrez )
{
 int lens = 0;
 char *sZnaki = "+-=><*/$.&:#%!^";
 int State;
 int StBr1 = 0, StBr2 = 0, StBr3 = 0;
 int rez = 0;

 SKIPTABSPACES ( *ptri );
 State = (**ptri=='\'' || **ptri=='\"')? STATE_EXPRES:STATE_ID;
 while ( **ptri != '\0' && !rez && lens < maxrez )
 {
  switch ( State ) {
    case STATE_QUOTE1:
     if(**ptri=='\'')
      State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
     break;
    case STATE_QUOTE2:
     if(**ptri=='\"')
      State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
     break;
    case STATE_BRACKET:
     if ( **ptri == '\'' ) State = STATE_QUOTE1;
     else if ( **ptri == '\"' ) State = STATE_QUOTE2;
     else if ( **ptri == '(' ) StBr1++;
     else if ( **ptri == '[' ) StBr2++;
     else if ( **ptri == '{' ) StBr3++;
     else if ( **ptri == ')' )
     { StBr1--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     else if ( **ptri == ']' )
     { StBr2--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     else if ( **ptri == '}' )
     { StBr3--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     break;
    case STATE_ID:
    case STATE_ID_END:
     if ( ( (ISNAME(**ptri) || **ptri=='\\') && State == STATE_ID_END ) ||
          **ptri==',' || **ptri=='\'' || **ptri=='\"' || **ptri==')' )
     {
      if ( **ptri == ',' )
      {
       if ( !prlist ) rez = 1;
       State = STATE_EXPRES;
      }
      else rez = 1;
     }
     else if ( IsInStr( **ptri, sZnaki ) )
     {
      State = STATE_EXPRES;
     }
     else if ( **ptri == '(' )
     {
      State = STATE_BRACKET;
      StBr1 = 1;
     }
     else if ( **ptri == '[' )
     {
      State = STATE_BRACKET;
      StBr2 = 1;
     }
     else if ( **ptri == '{' )
     {
      State = STATE_BRACKET;
      StBr3 = 1;
     }
     else if ( **ptri == ' ' ) State = STATE_ID_END;
     break;
    case STATE_EXPRES:
    case STATE_EXPRES_ID:
     if ( **ptri == '\'' ) State = STATE_QUOTE1;
     else if ( **ptri == '\"' ) State = STATE_QUOTE2;
     else if ( ISNAME(**ptri) ) State = STATE_EXPRES_ID;
     else if ( **ptri == ' ' && State == STATE_EXPRES_ID ) State = STATE_ID_END;
     else if ( **ptri == '(' ) { StBr1++; State = STATE_BRACKET; }
     else if ( **ptri == '[' ) { StBr2++; State = STATE_BRACKET; }
     else if ( **ptri == '{' ) { StBr3++; State = STATE_BRACKET; }
     else if ( **ptri == ',' ) { if ( !prlist ) rez = 1; State = STATE_EXPRES; }
     else State = STATE_EXPRES;
     break;
  }
  if ( !rez )
  {
   if ( expreal != NULL ) *expreal++ = **ptri;
   (*ptri)++;
   lens++;
  }
 }
 if ( expreal != NULL )
 {
  if ( *(expreal-1) == ' ' ) { expreal--; lens--; };
  *expreal = '\0';
 }
 return lens;
}

int isExpres ( char* stroka )
{
 if ( strolen ( stroka ) > getExpReal ( NULL, &stroka, FALSE, STR_SIZE ) )
  return 0;
 else
  return 1;
}

int TestOptional( char *ptr1, char *ptr2 )
{
   int nbr = 0, flagname = 0, statevar = 0;

   while( ptr1 <= ptr2 )
   {
      if( *ptr1 == '[' ) nbr++;
      else if( *ptr1 == ']' )
      {
         if( nbr )
         {
            nbr--;
            flagname = 0;
         }
         else return 0;
      }
      else if( *ptr1 == '\1' && *(ptr1+2) == '2' && nbr ) statevar = 1;
      else if( *ptr1 == '>' && statevar ) statevar = 0;
      else if( *ptr1 != ' ' && *ptr1 != '\t' && !statevar )
      {
         if( nbr ) flagname = 1;
         else return 0;
      }
      ptr1++;
   }
/*   if( !flagname )
      while( *ptr1 != ']' )
      {
         if( *ptr1 == '[' || *ptr1 == '\0' ) return 0;
         ptr1++;
      } */
   return !flagname;
}

void SkipOptional( char** ptri )
{
   int nbr = 0;

   while ( **ptri != ']' || nbr )
   {
      switch ( **ptri ) {
      case '[':  nbr++; break;
      case ']':  nbr--; break;
      case '\1':
         (*ptri) += 3;
         if ( *(*ptri-1) == '2' )
            while ( **ptri != '>' ) (*ptri)++;
         break;
      }
      (*ptri)++;
   }
   if ( **ptri == ']' && numBrackets > 0 )
   {
     if ( Repeate ) Repeate--;
     numBrackets--; (*ptri)++;
   }
}

void SearnRep( char *exppatt,char *expreal,int lenreal,char *ptro, int *lenres)
{
   int ifou, isdvig = 0, rezs;
   int kolmarkers;
   int lennew, i;
   char lastchar = '0';
   char expnew[MAX_NAME];
   char *ptr, *ptr2, *ptrOut = ptro;

   while ( (ifou = pp_strAt( exppatt, (*(exppatt+1))? 2:1, ptrOut, *lenres-isdvig )) > 0 )
   {
      rezs = 0;
      ptr = ptrOut + ifou - 2;
      kolmarkers = 0;
      while ( ptr >= ptrOut )
      {
         if ( (*ptr == '[' || *ptr == ']') && *(ptr-1) != '\\' ) break;
         if  ( *ptr == '\1' ) kolmarkers++;
         ptr--;
      }
      if ( *ptr == '[' )
      {
         if( Repeate ) aIsRepeate[ Repeate - 1 ]++;
         ptr2 = ptrOut + ifou + 3;
         while ( *ptr2 != ']' && *(ptr-1) != '\\' )
         {
            if ( *ptr2 == '\1' ) kolmarkers++;
            ptr2++;
         }

         if ( Repeate && lenreal && kolmarkers && lastchar != '0' &&
                                             *(ptrOut + ifou + 2) == '0' )
         {
            isdvig += ifou;
            rezs = 1;
         }
         else
         {
            if ( lenreal == 0 )
            {
               if( numBrackets >= 2 )
               {
                  isdvig += ifou;
                  continue;
               }
               else
               {
                  pp_Stuff ( "", ptr, 0, ptr2-ptr+1, *lenres-(ptr-ptro) );
                  *lenres -= ptr2-ptr+1;
                  isdvig = ptr - ptro;
                  rezs = 1;
               }
            }
            else
            {
               lennew = ptr2-ptr-1;

               memcpy ( expnew, ptr+1, lennew );
               *(expnew + lennew++) = ' ';
               *(expnew + lennew) = '\0';
               while ( (i = pp_strAt( exppatt, 2, expnew, lennew )) > 0 )
               lennew += ReplacePattern ( exppatt[2], expreal, lenreal, expnew+i-1, lennew );
               if ( kolmarkers )
               {
                  groupchar = (char) ( (unsigned int)groupchar + 1 );
                  for ( i=0; i<lennew; i++ )
                     if ( *(expnew+i) == '\1' )
                     {
                        *(expnew+i+3) = groupchar;
                        i += 4;
                     }
               }
               pp_Stuff ( expnew, ptr, lennew, 0, *lenres-(ptr-ptro)+1 );
               *lenres += lennew;
               isdvig = ptr - ptro + (ptr2-ptr-1) + lennew;
               rezs = 1;
            }
         }
      }
      if ( !rezs )
      {
         if ( *(ptrOut + ifou + 2) != '0' && *(exppatt+1) )
         {
            if ( lastchar == '0' ) lastchar = *(ptrOut + ifou + 2);
            if ( lastchar != *(ptrOut + ifou + 2) )
            {
               isdvig += ifou + 3;
               ptrOut = ptro + isdvig;
               continue;
            }
         }
         *lenres += ReplacePattern ( exppatt[2], expreal, lenreal,
                             ptrOut + ifou - 1, *lenres-isdvig-ifou+1 );
         isdvig += ifou;
      }
      ptrOut = ptro + isdvig;
   }
}

int ReplacePattern ( char patttype, char *expreal, int lenreal, char *ptro, int lenres )
{
   int rmlen = lenreal, ifou, lenitem, i;
   char sQuotes[ 4 ] = "\"\",";

     switch ( *(ptro+2) ) {
      case '0':  /* Regular result marker  */
         pp_Stuff ( expreal, ptro, lenreal, 4, lenres );
         break;
      case '1':  /* Dumb stringify result marker  */
         pp_rQuotes( expreal, sQuotes );
         pp_Stuff ( sQuotes, ptro, 2, 4, lenres );
         if ( lenreal )
          pp_Stuff ( expreal, ptro+1, lenreal, 0, lenres );
         rmlen = lenreal + 2;
         break;
      case '2':  /* Normal stringify result marker  */
         if ( !lenreal )
          pp_Stuff ( "", ptro, 0, 4, lenres );
         else if ( patttype == '1' )          /* list match marker */
         {
            pp_Stuff ( "", ptro, 0, 4, lenres );
            lenres -= 4;
            rmlen = 0;
            do
            {
               ifou = pp_strAt( ",", 1, expreal, lenreal );
               lenitem = (ifou)? ifou-1:lenreal;
               if( *expreal != '\0' )
               {
                  i = (ifou)? 3:2;
                  pp_rQuotes( expreal, sQuotes );
                  pp_Stuff ( sQuotes, ptro, i, 0, lenres );
                  pp_Stuff ( expreal, ptro+1, lenitem, 0, lenres+i );
                  ptro += i + lenitem;
                  rmlen += i + lenitem;
               }
               expreal += ifou;
               lenreal -= ifou;
            }
            while ( ifou > 0 );
         }
         else
         {
          pp_rQuotes( expreal, sQuotes );
          pp_Stuff ( sQuotes, ptro, 2, 4, lenres );
          pp_Stuff ( expreal, ptro+1, lenreal, 0, lenres );
          rmlen = lenreal + 2;
         }
         break;
      case '3':  /* Smart stringify result marker  */
         if ( patttype == '1' )          /* list match marker */
         {
            pp_Stuff ( "", ptro, 0, 4, lenres );
            lenres -= 4;
            rmlen = 0;
            do
            {
               ifou = pp_strAt( ",", 1, expreal, lenreal );
               lenitem = (ifou)? ifou-1:lenreal;
               if( *expreal != '\0' )
               {
                  if ( !lenitem || *expreal == '(' || *expreal == '&' ||
                   (*expreal=='\"' && *(expreal+lenitem-1)=='\"') ||
                   (*expreal == '\'' && *(expreal+lenitem-1)=='\'') )
                  {
                     if( ifou ) lenitem++;
                     pp_Stuff ( (*expreal=='&')? expreal+1:expreal, ptro,
                             (*expreal=='&')? lenitem-1:lenitem, 0, lenres );
                  }
                  else
                  {
                     i = (ifou)? 3:2;
                     pp_rQuotes( expreal, sQuotes );
                     pp_Stuff ( sQuotes, ptro, i, 0, lenres );
                     pp_Stuff ( expreal, ptro+1, lenitem, 0, lenres+i );
                     ptro += i;
                     rmlen += i;
                  }
                  ptro += lenitem;
                  rmlen += lenitem;
               }
               expreal += ifou;
               lenreal -= ifou;
            }
            while ( ifou > 0 );
         }
         else if ( !lenreal || *expreal == '(' || *expreal == '&' ||
                 (*expreal=='\"' && *(expreal+lenreal-1)=='\"') ||
                 (*expreal == '\'' && *(expreal+lenreal-1)=='\'') )
           pp_Stuff ( (*expreal=='&')? expreal+1:expreal, ptro,
                   (*expreal=='&')? lenreal-1:lenreal, 4, lenres );
         else
         {
          pp_rQuotes( expreal, sQuotes );
          pp_Stuff ( sQuotes, ptro, 2, 4, lenres );
          pp_Stuff ( expreal, ptro+1, lenreal, 0, lenres );
          rmlen = lenreal + 2;
         }
         break;
      case '4':  /* Blockify result marker  */
         if ( !lenreal )
           pp_Stuff ( expreal, ptro, lenreal, 4, lenres );
         else if ( patttype == '1' )          /* list match marker */
         {
            pp_Stuff ( "", ptro, 0, 4, lenres );
            lenres -= 4;
            rmlen = 0;
            do
            {
               ifou = pp_strAt( ",", 1, expreal, lenreal );
               lenitem = (ifou)? ifou-1:lenreal;
               if( *expreal != '\0' )
               {
                  i = (ifou)? 5:4;
                  pp_Stuff ( "{||},", ptro, i, 0, lenres );
                  pp_Stuff ( expreal, ptro+3, lenitem, 0, lenres+i );
                  ptro += i + lenitem;
                  rmlen += i + lenitem;
               }
               expreal += ifou;
               lenreal -= ifou;
            }
            while ( ifou > 0 );
         }
         else
         {
          pp_Stuff ( "{||}", ptro, 4, 4, lenres );
          pp_Stuff ( expreal, ptro+3, lenreal, 0, lenres );
          rmlen = lenreal + 4;
         }
         break;
      case '5':  /* Logify result marker  */
         rmlen = 3;
         if ( !lenreal )
         {
          pp_Stuff ( ".F.", ptro, 3, 4, lenres );
         }
         else
          pp_Stuff ( ".T.", ptro, 3, 4, lenres );
         break;
     }
     return rmlen - 4;
}

void pp_rQuotes( char *expreal, char *sQuotes )
{
   int lQuote1 = 0, lQuote2 = 0;

   while( *expreal != '\0' )
   {
     if( *expreal == '\"' ) lQuote2 = 1;
     else if( *expreal == '\'' ) lQuote1 = 1;
     expreal++;
   }
   if( lQuote2 )
   {
     if( lQuote1 )
     {
        *sQuotes = '[';
        *(sQuotes+1) = ']';
     }
     else
     {
        *sQuotes = '\'';
        *(sQuotes+1) = '\'';
     }
   }
   else
   {
      *sQuotes = '\"';
      *(sQuotes+1) = '\"';
   }
}

int pp_RdStr(FILE* handl_i,char *buffer,int maxlen,int lDropSpaces,char* sBuffer, int* lenBuffer, int* iBuffer)
{
   int readed = 0;
   int State = 0;
   char cha,cLast='\0';

   if ( *lenBuffer == 0 ) return -1;
   while(1)
   {
      if ( *iBuffer == *lenBuffer )
      {
         if ( (*lenBuffer = fread(sBuffer,1,BUFF_SIZE,handl_i)) < 1 )
            sBuffer[0] = '\n';
         *iBuffer = 0;
      }
      cha = sBuffer[*iBuffer];
      (*iBuffer)++;
      if( cha == '\n' )  break;
      if ( maxlen > 0 )
      {
         switch ( ParseState ) {
         case STATE_COMMENT:
            if ( cha == '/' && cLast == '*' )
            {
               ParseState = STATE_NORMAL;
               cha = ' ';
            }
            cLast = cha;
            break;
         case STATE_QUOTE1: if(cha=='\'') ParseState = STATE_NORMAL; break;
         case STATE_QUOTE2: if(cha=='\"') ParseState = STATE_NORMAL; break;
         default:
            switch ( cha ) {
            case '[': ParseState = STATE_BRACKET; break;
            case ']': ParseState = STATE_NORMAL; break;
            case '\"':
               if( ParseState != STATE_BRACKET ) ParseState = STATE_QUOTE2;
               break;
            case '\'':
               if( ParseState != STATE_BRACKET ) ParseState = STATE_QUOTE1;
               break;
            case '&':
               if ( readed>0 && buffer[readed-1] == '&' ) { maxlen = 0; readed--; }
               break;
            case '/':
               if ( readed>0 && buffer[readed-1] == '/' ) { maxlen = 0; readed--; }
               break;
            case '*':
               if ( readed > 0 && buffer[readed-1] == '/' )
               {
                  ParseState = STATE_COMMENT;
                  readed--;
               }
               else if ( !State ) maxlen = readed = 0;
               break;
            }
         }
         if ( cha != ' ' && cha != '\t' ) State = 1;
         if( lDropSpaces && State ) lDropSpaces = 0;
         if( readed<maxlen && (!lDropSpaces || readed==0) &&
                         ParseState != STATE_COMMENT )
             buffer[readed++]=cha;
      }
   }
   while(--readed >= 0 && ( buffer[readed] == ' ' || buffer[readed] == '\t') );
   readed++;
   buffer[readed]='\0';
   return readed;
}

int pp_WrStr(FILE* handl_o,char *buffer)
{
   int lens = strolen(buffer);
   fwrite(buffer,lens,1,handl_o);
   if ( *(buffer+lens-1) != '\n' )
      fwrite("\n",1,1,handl_o);
   return 0;
}

/* locates a substring in a string */
int pp_strAt(char *szSub, int lSubLen, char *szText, int lLen)
{
   if( lSubLen )
   {
      if( lLen >= lSubLen )
      {
         long lPos = 0, lSubPos = 0;

         while( lPos < lLen && lSubPos < lSubLen )
         {
            if( *(szText + lPos) == *(szSub + lSubPos) )
            {
               lSubPos++;
               lPos++;
            }
            else if( lSubPos )
               lSubPos = 0;
            else
               lPos++;
         }
         return (lSubPos < lSubLen? 0: lPos - lSubLen + 1);
      }
      else
         return 0;
   }
   else
      return 1;
}

int md_strAt(char *szSub, int lSubLen, char *szText, int checkPrth)
{
 int State = STATE_NORMAL;
 long lPos = 0, lSubPos = 0;
 int kolPrth = 0;

   while( *(szText+lPos) != '\0' && lSubPos < lSubLen )
   {
     if( State == STATE_QUOTE1 )
     {
       if ( *(szText+lPos) == '\'' ) State = STATE_NORMAL;
       lPos++;
     }
     else if( State == STATE_QUOTE2 )
     {
       if ( *(szText+lPos) == '\"' ) State = STATE_NORMAL;
       lPos++;
     }
     else
     {
       if ( *(szText+lPos) == '\"' )
       {
         State = STATE_QUOTE2;
         lPos++;
         continue;
       }
       else if ( *(szText+lPos) == '\'' )
       {
         State = STATE_QUOTE1;
         lPos++;
         continue;
       }
       else if ( *(szText+lPos) == '(' )
         kolPrth++;
       else if ( *(szText+lPos) == ')' )
         kolPrth--;
       if( !lSubPos && checkPrth && ( (kolPrth > 1) ||
           (kolPrth == 1 && *(szText+lPos) != '(') || (kolPrth == 0 && *(szText+lPos) == ')')) )
       {
         lPos++;
         continue;
       }

       if( toupper(*(szText + lPos)) == toupper(*(szSub + lSubPos)) )
       {
         lSubPos++;
         lPos++;
         if ( lSubPos >= lSubLen  &&
          ( ( ISNAME(*szSub) && lPos>lSubPos && ISNAME(*(szText+lPos-lSubPos-1)) ) ||
            ( ISNAME(*(szSub+lSubLen-1)) && ISNAME(*(szText+lPos)) ) ) )
          lSubPos = 0;
       }
       else if( lSubPos )  lSubPos = 0;
       else  lPos++;
     }
   }
   return (lSubPos < lSubLen? 0: lPos - lSubLen + 1);
}

int IsInStr ( char symb, char* s )
{
 while ( *s != '\0' ) if ( *s++ == symb ) return 1;
 return 0;
}

void pp_Stuff (char *ptri, char * ptro, int len1, int len2, int lenres )
{
 char *ptr1, *ptr2;
 int i;
  if ( len1 > len2 )
  {
   ptr1 = ptro+lenres;
   ptr2 = ptro + lenres + len1 - len2;
   for ( ; ptr1 >= ptro; ptr1--,ptr2-- ) *ptr2 = *ptr1;
  }
  else
  {
   ptr1 = ptro + len2;
   ptr2 = ptro + len1;
   for ( ; ptr1 <= ptro+lenres; ptr1++,ptr2++ ) *ptr2 = *ptr1;
  }
  ptr2 = ptro;
  for ( i=0; i < len1; i++ ) *ptr2++ = *(ptri+i);
}

int strocpy (char* ptro, char* ptri )
{
 int lens = 0;
 if ( ptri != NULL )
  while ( *ptri != '\0' )
  {
    *ptro++ = *ptri++;
    lens++;
  }
 *ptro = '\0';
 return lens;
}

int stroncpy (char* ptro, char* ptri, int lens )
{
 int i = 0;
 for ( ; i < lens; i++ ) *(ptro+i) = *ptri++;
 i--;
 while ( i > 0 && *(ptro+i) == ' ' ) i--;
 i++;
 *(ptro+i) = '\0';
 return i;
}

int truncmp (char** ptro, char** ptri, int lTrunc )
{
   char *ptrb = *ptro, co, ci;

   for ( ; **ptri != ' ' && **ptri != '\t' && **ptri != ',' && **ptri != '[' && **ptri != ']' &&
       **ptri != '\1' && **ptri != '\0' && toupper(**ptri)==toupper(**ptro);
       (*ptro)++, (*ptri)++ );
   co = *(*ptro-1);
   ci = **ptri;
   if ( ( ( ci == ' ' || ci == ',' || ci == '[' ||
       ci == ']' || ci == '\1' || ci == '\0' ) &&
       ( ( !ISNAME(**ptro) && ISNAME(co) ) ||
       ( !ISNAME(co) ) ) ) )
      return 0;
   else if ( lTrunc && *ptro-ptrb >= 4 && ISNAME(ci) && !ISNAME(**ptro) && ISNAME(co) )
   {
      while( ISNAME(**ptri) ) (*ptri)++;
      return 0;
   }
   return 1;
}
int strincmp (char* ptro, char** ptri )
{
   char co, ci;

   for ( ; **ptri != ',' && **ptri != '[' && **ptri != ']' &&
       **ptri != '\1' && **ptri != '\0' && toupper(**ptri)==toupper(*ptro);
       ptro++, (*ptri)++ );
   co = *(ptro-1);
   ci = **ptri;
   if ( ( ( ci == ' ' || ci == ',' || ci == '[' ||
       ci == ']' || ci == '\1' || ci == '\0' ) &&
       ( ( !ISNAME(*ptro) && ISNAME(co) ) ||
       ( !ISNAME(co) ) ) ) )
      return 0;
   return 1;
}

int strincpy (char* ptro, char* ptri )
{
 int lens = 0;
 for ( ; *ptri != ' ' && *ptri != ',' && *ptri != '[' && *ptri != ']' &&
                   *ptri != '\1' && *ptri != '\0'; ptro++, ptri++, lens++ )
  *ptro = *ptri;
 return lens;
}

char* strodup ( char *stroka )
{
 char *ptr;
 int lens = 0;
  while ( *(stroka+lens) != '\0' ) lens++;
  ptr = ( char * ) hb_xgrab( lens + 1 );
  memcpy( ptr,  stroka, lens+1 );
  *(ptr+lens) = '\0';
  return ptr;
}

int strolen ( char *stroka )
{
 int lens = 0;
  while ( *(stroka+lens) != '\0' ) lens++;
  return lens;
}

void stroupper ( char *stroka )
{
  while ( *stroka != '\0' )
  {
    *stroka = toupper(*stroka);
    stroka++;
  }
}

int strotrim ( char *stroka )
{
 char *ptr = stroka, lastc = '0', curc;
 int lens = 0, State = STATE_NORMAL;
 while ( ( curc = *stroka ) != '\0' )
 {
  if ( State == STATE_QUOTE1 ) { if (curc == '\'') State = STATE_NORMAL; }
  else if ( State == STATE_QUOTE2 ) { if (curc=='\"') State = STATE_NORMAL; }
  else
  {
   if ( curc == '\'' ) State = STATE_QUOTE1;
   else if ( curc == '\"' ) State = STATE_QUOTE2;
   else if ( curc == '\t' ) curc = ' ';
  }
  if ( State != STATE_NORMAL || curc != ' ' ||
      ( curc==' ' && lastc != ' ' && lastc != ',' && lastc != '(' && *(stroka+1)!=',') )
  {
     *ptr++ = curc;
     lastc = curc;
     lens++;
  }
  stroka++;
 }
 *ptr = '\0';
 return lens;
}

int NextWord ( char** sSource, char* sDest, int lLower )
{
 int i = 0;
 SKIPTABSPACES( (*sSource) );
 while ( **sSource != '\0' && **sSource != ' ' && **sSource != '\t' && **sSource != '(')
 {
   *sDest++ = (lLower)? tolower(**sSource):**sSource;
   (*sSource)++;
   i++;
 }
 *sDest = '\0';
 return i;
}

int NextName ( char** sSource, char* sDest )
{
 int lenName = 0, State = STATE_NORMAL;
 while ( **sSource != '\0' && ( !ISNAME(**sSource) || State != STATE_NORMAL ) )
 {
  if ( State == STATE_QUOTE1 )
   { if ( **sSource == '\'' ) State = STATE_NORMAL; }
  else if ( State == STATE_QUOTE2 )
   { if ( **sSource == '\"' ) State = STATE_NORMAL; }
  else if ( **sSource == '\"' ) State = STATE_QUOTE2;
  else if ( **sSource == '\'' ) State = STATE_QUOTE1;

  (*sSource)++;
 }

 while ( **sSource != '\0' && ISNAME(**sSource) )
  {
   *sDest++ = *(*sSource)++;
   lenName++;
  }
 *sDest = '\0';
 return lenName;
}

int NextParm ( char** sSource, char* sDest )
{
   int lenName = 0, State = STATE_NORMAL, StBr = 0;

   SKIPTABSPACES( (*sSource) );
   while ( **sSource != '\0' )
   {
     if ( State == STATE_QUOTE1 )
     {
       if ( **sSource == '\'' ) State = STATE_NORMAL;
     }
     else if ( State == STATE_QUOTE2 )
     {
       if ( **sSource == '\"' ) State = STATE_NORMAL;
     }
     else if ( **sSource == '\"' ) State = STATE_QUOTE2;
     else if ( **sSource == '\'' ) State = STATE_QUOTE1;
     else if ( **sSource == '(' ) StBr++;
     else if ( **sSource == ')' || **sSource == ',' )
     {
       if( !StBr ) break;
       if( **sSource == ')' ) StBr--;
     }

     if ( sDest != NULL ) *sDest++ = **sSource;
     (*sSource)++;
     lenName++;
   }

   if ( sDest != NULL ) *sDest = '\0';
   return lenName;
}

BOOL OpenInclude( char * szFileName, PATHNAMES *pSearch, FILE** fptr, BOOL bStandardOnly )
{
  PHB_FNAME pFileName;
  char szFName[ _POSIX_PATH_MAX ]; /* filename to parse */

  if ( bStandardOnly )
  {
    *fptr = 0;
  }
  else
  {
    pFileName = hb_fsFNameSplit( szFileName );
    pFileName->szPath = _pFileName->szPath;
    hb_fsFNameMerge( szFName, pFileName );
    *fptr = fopen( szFName, "r" );
    hb_xfree( pFileName );
  }

  if ( !*fptr && pSearch )
  {
    pFileName = hb_fsFNameSplit( szFileName );
    pFileName->szName = szFileName;
    pFileName->szExtension = NULL;
    while ( pSearch && !*fptr )
    {
      pFileName->szPath = pSearch->szPath;
      hb_fsFNameMerge( szFName, pFileName );
      *fptr = fopen( szFName, "r" );
      pSearch = pSearch->pNext;
    }
    hb_xfree( pFileName );
  }

  return ( *fptr ? TRUE : FALSE );
}
