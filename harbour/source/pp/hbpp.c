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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    Support for #pragma directive and related functions
 *    See doc/pragma.txt
 *
 * See doc/license.txt for licensing terms.
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
#include <ctype.h>
#include "hbpp.h"
#include "hberrors.h"
#include "compiler.h"

static COMMANDS * AddCommand( char * );                  /* Add new #command to an array  */
static COMMANDS * AddTranslate( char * );                /* Add new #translate to an array  */
static DEFINES *  DefSearch( char *, BOOL * );
static COMMANDS * ComSearch( char *, COMMANDS * );
static COMMANDS * TraSearch( char *, COMMANDS * );

static int    ParseDefine( char * );                        /* Process #define directive */
static int    ParseUndef( char * );                         /* Process #undef directive */
static int    ParseIfdef( char *, int );                    /* Process #ifdef directive */
static void   ParseCommand( char *, BOOL, BOOL );          /* Process #command or #translate directive */
static void   ConvertPatterns( char *, int, char *, int ); /* Converting result pattern in #command and #translate */
static int    WorkDefine( char **, char *, DEFINES * );    /* Replace fragment of code with a #defined result text */
static int    WorkPseudoF( char **, char *, DEFINES * );   /* Replace pseudofunction with a #defined result text */
static int    WorkCommand( char *, char *, COMMANDS * );
static int    WorkTranslate( char *, char *, COMMANDS *, int * );
static int    CommandStuff( char *, char *, char *, int *, BOOL, BOOL );
static int    RemoveSlash( char * );
static int    WorkMarkers( char **, char **, char *, int *, BOOL );
static int    getExpReal( char *, char **, BOOL, int );
static BOOL   isExpres( char * );
static BOOL   TestOptional( char *, char * );
static BOOL   CheckOptional( char *, char *, char *, int *, BOOL, BOOL );
static void   SkipOptional( char ** );

static void   SearnRep( char *, char *, int, char *, int * );
static int    ReplacePattern( char, char *, int, char *, int );
static void   pp_rQuotes( char *, char * );
static int    md_strAt( char *, int, char *, BOOL, BOOL );
static char * PrevSquare( char * , char *, int * );
static int    IsInStr( char, char * );
static int    stroncpy( char *, char *, int );
static int    strincpy( char *, char * );
static BOOL   truncmp( char **, char **, BOOL );
static BOOL   strincmp( char *, char **, BOOL );
static int    strotrim( char * );
static int    NextWord( char **, char *, BOOL );
static int    NextName( char **, char * );
static int    NextParm( char **, char * );
static BOOL   OpenInclude( char *, PATHNAMES *, PHB_FNAME, FILE **, BOOL bStandardOnly, char * );

/* These are related to pragma support */
static int    ParsePragma( char * );
static BOOL   StringToBool( char *, BOOL );
static int    StringToInt( char *, int );
static BOOL   IsOnOffSwitch( char *, BOOL );
static void   DebugPragma( char *, int, BOOL );

static BOOL s_bTracePragma = FALSE;

#define ISNAME( c )  ( isalnum( c ) || ( c ) == '_' || ( c ) > 0x7E )
#define MAX_NAME 255
#define MAX_EXP 1024
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

static int  s_kolAddDefs = 0;
static int  s_ParseState = 0;
static int  s_maxCondCompile = 5;
static int  s_aIsRepeate[ 5 ];
static int  s_Repeate;
static BOOL s_bReplacePat = TRUE;
static int  s_numBrackets;
static char s_groupchar;

int   hb_pp_lInclude = 0;
int * hb_pp_aCondCompile;
int   hb_pp_nCondCompile = 0;

/* Table with parse errors */
char * hb_pp_szErrors[] =
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
  "Freeing a NULL memory pointer",
  "Value out of range in #pragma directive"
};

/* Table with parse warnings */
/* NOTE: The first character stores the warning's level that triggers this
 * warning. The warning's level is set by -w<n> command line option.
 */
char * hb_pp_szWarnings[] =
{
  "3Non directive in include file %s(%s)"
};

int hb_pp_ParseDirective( char * sLine )
{
  char sDirective[MAX_NAME];
  char szInclude[_POSIX_PATH_MAX];
  int i;
  FILE* handl_i;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_ParseDirective(%s)", sLine));

  i = NextName( &sLine, sDirective );
  hb_strupr( sDirective );

  HB_SKIPTABSPACES(sLine);

  if( i == 4 && memcmp( sDirective, "ELSE", 4 ) == 0 )
    {     /* ---  #else  --- */
      if( hb_pp_nCondCompile == 0 )
        hb_compGenError( hb_pp_szErrors, 'F', ERR_DIRECTIVE_ELSE, NULL, NULL );
      else if( hb_pp_nCondCompile == 1 || hb_pp_aCondCompile[hb_pp_nCondCompile-2] )
        hb_pp_aCondCompile[hb_pp_nCondCompile-1] = 1 - hb_pp_aCondCompile[hb_pp_nCondCompile-1];
    }

  else if( i == 5 && memcmp( sDirective, "ENDIF", 5 ) == 0 )
    {     /* --- #endif  --- */
      if( hb_pp_nCondCompile == 0 )
        hb_compGenError( hb_pp_szErrors, 'F', ERR_DIRECTIVE_ENDIF, NULL, NULL );
      else hb_pp_nCondCompile--;
    }

  else if( i == 5 && memcmp( sDirective, "IFDEF", 5 ) == 0 )
    ParseIfdef( sLine, TRUE ); /* --- #ifdef  --- */

  else if( i == 6 && memcmp( sDirective, "IFNDEF", 6 ) == 0 )
    ParseIfdef( sLine, FALSE ); /* --- #ifndef  --- */

  else if( hb_pp_nCondCompile==0 || hb_pp_aCondCompile[hb_pp_nCondCompile-1])
    {
      if( i == 7 && memcmp( sDirective, "INCLUDE", 7 ) == 0 )
        {    /* --- #include --- */
          char cDelimChar;

          if( *sLine != '\"' && *sLine != '\'' && *sLine != '<' )
            hb_compGenError( hb_pp_szErrors, 'F', ERR_WRONG_NAME, NULL, NULL );

          cDelimChar = *sLine;
          if( cDelimChar == '<' )
            cDelimChar = '>';
          else if( cDelimChar == '`' )
            cDelimChar = '\'';

          sLine++; i = 0;
          while( *(sLine+i) != '\0' && *(sLine+i) != cDelimChar ) i++;
          if( *(sLine+i) != cDelimChar )
            hb_compGenError( hb_pp_szErrors, 'F', ERR_WRONG_NAME, NULL, NULL );
          *(sLine+i) = '\0';

          /*   if((handl_i = fopen(sLine, "r")) == NULL) */
          if( OpenInclude( sLine, hb_comp_pIncludePath, hb_comp_pFileName, &handl_i, ( cDelimChar == '>' ), szInclude ) )
          {
            hb_pp_lInclude++;
            hb_pp_Parse(handl_i, 0, szInclude );
            hb_pp_lInclude--;
            fclose(handl_i);
          }
          else
            hb_compGenError( hb_pp_szErrors, 'F', ERR_CANNOT_OPEN, sLine, NULL );
        }

      else if( i == 6 && memcmp( sDirective, "DEFINE", 6 ) == 0 )
        ParseDefine( sLine );   /* --- #define  --- */

      else if( i == 5 && memcmp( sDirective, "UNDEF", 5 ) == 0 )
        ParseUndef( sLine );    /* --- #undef  --- */

      else if( (i == 7 && memcmp( sDirective, "COMMAND", 7 ) == 0) ||
               (i == 8 && memcmp( sDirective, "XCOMMAND", 8 ) == 0) )
                                /* --- #command  --- */
        ParseCommand( sLine, (i==7)? FALSE:TRUE, TRUE );

      else if( (i == 9 && memcmp( sDirective, "TRANSLATE", 9 ) == 0) ||
               (i == 10 && memcmp( sDirective, "XTRANSLATE", 10 ) == 0) )
                                /* --- #translate  --- */
        ParseCommand( sLine, (i==9)? FALSE:TRUE, FALSE );

      else if( i == 6 && memcmp( sDirective, "STDOUT", 6 ) == 0 )
        printf( "%s\n", sLine ); /* --- #stdout  --- */

      else if( i == 5 && memcmp( sDirective, "ERROR", 5 ) == 0 )
        /* --- #error  --- */
        hb_compGenError( hb_pp_szErrors, 'F', ERR_EXPLICIT, sLine, NULL );

      else if( i == 4 && memcmp( sDirective, "LINE", 4 ) == 0 )
        return -1;

      else if( i == 6 && memcmp( sDirective, "PRAGMA", 6 ) == 0 )
         ParsePragma( sLine );   /* --- #pragma  --- */

      else
        hb_compGenError( hb_pp_szErrors, 'F', ERR_WRONG_DIRECTIVE, sDirective, NULL );
    }
  return 0;
}

static int ParseDefine( char * sLine )
{
  char defname[MAX_NAME], pars[MAX_NAME];
  int i, npars = -1;
  DEFINES * lastdef;

  HB_TRACE(HB_TR_DEBUG, ("ParseDefine(%s)", sLine));

  HB_SKIPTABSPACES( sLine );
  if( isalpha( *sLine ) || *sLine == '_' || *sLine > 0x7e )
    {
      NextName( &sLine, defname );
      if( *sLine == '(' ) /* If pseudofunction was found */
        {
          sLine++; i = 0;
          npars = 0;
          while( *sLine != '\0' && *sLine != ')')
            {
              if( *sLine == ',' ) npars++;
              if( *sLine != ' ' && *sLine != '\t' ) *(pars+i++) = *sLine;
              sLine++;
            }
          if( i > 0 ) npars++;
          *(pars+i) = '\0';
          sLine++;
        }
      HB_SKIPTABSPACES(sLine);

      lastdef = hb_pp_AddDefine( defname, ( *sLine == '\0' )? NULL : sLine );

      lastdef->npars = npars;
      lastdef->pars = ( npars <= 0 )? NULL : hb_strdup( pars );
    }
  else
    hb_compGenError( hb_pp_szErrors, 'F', ERR_DEFINE_ABSENT, NULL, NULL );
  return 0;
}

DEFINES * hb_pp_AddDefine( char * defname, char * value )
{
  BOOL isNew;
  DEFINES* stdef = DefSearch( defname, &isNew );

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_AddDefine(%s, %s)", defname, value));

  if( stdef != NULL )
  {
    if( isNew )
    {
       if( stdef->pars ) hb_xfree( stdef->pars );
       if( stdef->value ) hb_xfree( stdef->value );
    }
  }
  else
    {
      stdef = ( DEFINES * ) hb_xgrab( sizeof( DEFINES ) );
      stdef->last = hb_pp_topDefine;
      hb_pp_topDefine = stdef;
      stdef->name = hb_strdup( defname );
      s_kolAddDefs++;
    }
  stdef->value = ( value == NULL )? NULL : hb_strdup( value );
  return stdef;
}

static int ParseUndef( char * sLine )
{
  char defname[MAX_NAME];
  DEFINES* stdef;
  BOOL isNew;

  HB_TRACE(HB_TR_DEBUG, ("ParseUndef(%s)", sLine));

  NextWord( &sLine, defname, FALSE );

  if( ( stdef = DefSearch(defname, &isNew ) ) != NULL )
  {
    if( isNew )
    {
       if( stdef->pars ) hb_xfree( stdef->pars );
       if( stdef->value ) hb_xfree( stdef->value );
       hb_xfree( stdef->name );
    }
    stdef->name = NULL;
  }
  return 0;
}

static int ParseIfdef( char * sLine, int usl )
{
  char defname[ MAX_NAME ];
  DEFINES * stdef;

  HB_TRACE(HB_TR_DEBUG, ("ParseIfdef(%s, %d)", sLine, usl));

  if( hb_pp_nCondCompile==0 || hb_pp_aCondCompile[hb_pp_nCondCompile-1])
    {
      NextWord( &sLine, defname, FALSE );
      if( *defname == '\0' )
        hb_compGenError( hb_pp_szErrors, 'F', ERR_DEFINE_ABSENT, NULL, NULL );
    }
  if( hb_pp_nCondCompile == s_maxCondCompile )
    {
      s_maxCondCompile += 5;
      hb_pp_aCondCompile = (int*)hb_xrealloc( hb_pp_aCondCompile, sizeof( int ) * s_maxCondCompile );
    }
  if( hb_pp_nCondCompile==0 || hb_pp_aCondCompile[hb_pp_nCondCompile-1])
    {
      if( ( (stdef = DefSearch(defname,NULL)) != NULL && usl )
           || ( stdef == NULL && !usl ) ) hb_pp_aCondCompile[hb_pp_nCondCompile] = 1;
      else hb_pp_aCondCompile[hb_pp_nCondCompile] = 0;
    }
  else
    hb_pp_aCondCompile[ hb_pp_nCondCompile ] = 0;

  hb_pp_nCondCompile++;

  return 0;
}

static DEFINES * DefSearch( char * defname, BOOL * isNew )
{
  int kol = 0,j;
  DEFINES * stdef = hb_pp_topDefine;

  HB_TRACE(HB_TR_DEBUG, ("DefSearch(%s)", defname));

  while( stdef != NULL )
    {
      kol++;
      if( stdef->name != NULL )
        {
          for( j=0; *(stdef->name+j) == *(defname+j) &&
                  *(stdef->name+j) != '\0'; j++ );
          if( *(stdef->name+j) == *(defname+j) )
            {
              if( isNew ) *isNew = ( s_kolAddDefs >= kol );
              return stdef;
            }
        }
      stdef = stdef->last;
    }
  return NULL;
}

static COMMANDS * ComSearch( char * cmdname, COMMANDS * stcmdStart )
{
   COMMANDS * stcmd = ( stcmdStart ) ? stcmdStart : hb_pp_topCommand;

   HB_TRACE(HB_TR_DEBUG, ("ComSearch(%s, %p)", cmdname, stcmdStart));

   while( stcmd != NULL )
   {
      int j;

      for( j=0; (*(stcmd->name+j)==toupper(*(cmdname+j))) &&
              (*(stcmd->name+j)!='\0') &&
              ((stcmd->com_or_xcom)? 1:(j<4 || ISNAME(*(cmdname+j+1)))); j++ );
      if( (*(stcmd->name+j)==toupper(*(cmdname+j))) ||
           ( !stcmd->com_or_xcom && j >= 4 && *(stcmd->name+j)!='\0'
             && *(cmdname+j) == '\0' ) )
        break;

      stcmd = stcmd->last;
   }

   return stcmd;
}

static COMMANDS * TraSearch( char * cmdname, COMMANDS * sttraStart )
{
  int j;
  COMMANDS *sttra = ( sttraStart ) ? sttraStart : hb_pp_topTranslate;

  HB_TRACE(HB_TR_DEBUG, ("TraSearch(%s, %p)", cmdname, sttraStart));

  while( sttra != NULL )
    {
      for( j=0; *(sttra->name+j)==toupper(*(cmdname+j)) &&
              *(sttra->name+j)!='\0' &&
              ((sttra->com_or_xcom)? 1:(j<4 || ISNAME(*(cmdname+j+1)))); j++ );
      if( *(sttra->name+j)==toupper(*(cmdname+j)) ||
           ( !sttra->com_or_xcom && j >= 4 &&
             *(sttra->name+j)!='\0' && *(cmdname+j) == '\0' ) )
        break;
      sttra = sttra->last;
    }
  return sttra;
}

static void ParseCommand( char * sLine, BOOL com_or_xcom, BOOL com_or_tra )
{
  static char mpatt[ PATTERN_SIZE ];
  static char rpatt[ PATTERN_SIZE ];

  char cmdname[ MAX_NAME ];
  COMMANDS * stcmd;
  int mlen,rlen;
  int ipos;

  HB_TRACE(HB_TR_DEBUG, ("ParseCommand(%s, $d, $d)", sLine, com_or_xcom, com_or_tra));

  NextWord( &sLine, cmdname, FALSE );
  hb_strupr( cmdname );
  HB_SKIPTABSPACES(sLine);

  if( (ipos = hb_strAt( "=>", 2, sLine, strlen(sLine) )) > 0 )
  {
    stroncpy( mpatt, sLine, ipos-1 );

    RemoveSlash( mpatt );
    mlen = strotrim( mpatt );

    sLine += ipos + 1;
    HB_SKIPTABSPACES(sLine);
    hb_pp_strocpy( rpatt, sLine );
    rlen = strotrim( rpatt );

    ConvertPatterns( mpatt, mlen, rpatt, rlen );

    if( com_or_tra )
      stcmd = AddCommand( cmdname );
    else
      stcmd = AddTranslate( cmdname );

    stcmd->com_or_xcom = com_or_xcom;
    stcmd->mpatt = hb_strdup( mpatt );
    stcmd->value = ( rlen > 0 ) ? hb_strdup( rpatt ) : NULL;
  }
  else
    hb_compGenError( hb_pp_szErrors, 'F', ERR_COMMAND_DEFINITION, NULL, NULL );
}

/* ConvertPatterns()
 * Converts result pattern in #command and #translate to inner format
 */

static void ConvertPatterns( char * mpatt, int mlen, char * rpatt, int rlen )
{
  int i = 0, ipos, ifou;
  int explen, rmlen;
  char exppatt[ MAX_NAME ], expreal[ 5 ] = "\1  0";
  char lastchar = '@', exptype;
  char * ptr;

  HB_TRACE(HB_TR_DEBUG, ("ConvertPatterns(%s, $d, %s, $d)", mpatt, mlen, rpatt, rlen));

  while( *(mpatt+i) != '\0' )
    {
      if( *(mpatt+i) == '<' )
        {  /* Drag match marker, determine it type */
          explen = 0; ipos = i; i++; exptype = '0';
          while( *(mpatt+i) == ' ' || *(mpatt+i) == '\t' ) i++;
          if( *(mpatt+i) == '*' )        /* Wild match marker */
            { exptype = '3'; i++; }
          else if( *(mpatt+i) == '(' )   /* Extended expression match marker */
            { exptype = '4'; i++; }
          while( *(mpatt+i) != '>' )
            {
              if( *(mpatt+i) == ',' )      /* List match marker */
                {
                  exptype = '1';
                  while( *(mpatt+i) != '>' ) i++;
                  break;
                }
              else if( *(mpatt+i) == ':' ) /* Restricted match marker */
                {
                  exptype = '2';
                  *(mpatt+i--) = ' ';
                  break;
                }
              if( *(mpatt+i) != ' ' && *(mpatt+i) != '\t' )
                *(exppatt+explen++) = *(mpatt+i);
              i++;
            }
          if( exptype == '3' )
            {
              if( *(exppatt+explen-1) == '*' ) explen--;
              else
                hb_compGenError( hb_pp_szErrors, 'F', ERR_PATTERN_DEFINITION, NULL, NULL );
            }
          else if( exptype == '4' )
            {
              if( *(exppatt+explen-1) == ')' ) explen--;
              else
                hb_compGenError( hb_pp_szErrors, 'F', ERR_PATTERN_DEFINITION, NULL, NULL );
            }

          rmlen = i - ipos + 1;
          /* Convert match marker into inner format */
          lastchar = (lastchar!='Z') ? ( (char) ( (unsigned int)lastchar + 1 ) ):
                                       'a';
          expreal[1] = lastchar;
          expreal[2] = exptype;
          hb_pp_Stuff( expreal, mpatt+ipos, 4, rmlen, mlen );
          mlen += 4 - rmlen;
          i += 4 - rmlen;

          /* Look for appropriate result markers */
          ptr = rpatt;
          while( (ifou = hb_strAt( exppatt, explen, ptr, rlen-(ptr-rpatt) )) > 0 )
            {
              /* Convert result marker into inner format */
              ptr += ifou;
              if( *(ptr-2) == '<' && *(ptr+explen-1) == '>' &&
                  *(ptr-3) != '\\'  && *(ptr+explen-2) != '\\' )  /* <...> */
                {
                  if( *(ptr-3) == '#' && *(ptr-4) != '\\' )          /* #<...> */
                    { exptype = '1'; ptr -= 3; rmlen = explen+3; }
                  else
                    { exptype = '0'; ptr -= 2; rmlen = explen+2; }
                }
              else if( *(ptr-3) == '<' && *(ptr+explen) == '>' &&
                       *(ptr-4) != '\\' && *(ptr+explen-1) != '\\' )   /* < ... > */
                {
                  ptr -= 2;
                  if( *ptr == '\"' ) exptype = '2';
                  else if( *ptr == '(' ) exptype = '3';
                  else if( *ptr == '{' ) exptype = '4';
                  else if( *ptr == '.' ) exptype = '5';
                  ptr--;
                  rmlen = explen+4;
                }
              else continue;
              expreal[2] = exptype;
              hb_pp_Stuff( expreal, ptr, 4, rmlen, rlen );
              rlen += 4 - rmlen;
            }
        }
      i++;
    }
}

static COMMANDS * AddCommand( char * cmdname )
{
  COMMANDS * stcmd;

  HB_TRACE(HB_TR_DEBUG, ("AddCommand(%s)", cmdname));

  stcmd = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
  stcmd->last = hb_pp_topCommand;
  hb_pp_topCommand = stcmd;
  stcmd->name = hb_strdup( cmdname );
  return stcmd;
}

static COMMANDS* AddTranslate( char * traname )
{
  COMMANDS * sttra;

  HB_TRACE(HB_TR_DEBUG, ("AddTranslate(%s)", traname));

  sttra = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
  sttra->last = hb_pp_topTranslate;
  hb_pp_topTranslate = sttra;
  sttra->name = hb_strdup( traname );
  return sttra;
}

int hb_pp_ParseExpression( char * sLine, char * sOutLine )
{
  char sToken[MAX_NAME];
  char * ptri, * ptro, * ptrb;
  int lenToken, i, ipos, isdvig, lens;
  int ifou;
  int rezDef, rezTra, rezCom, kolpass = 0;
  DEFINES * stdef;
  COMMANDS * stcmd;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_ParseExpression(%s, %s)", sLine, sOutLine));

  do
    {
      strotrim( sLine );
      rezDef = 0; rezTra = 0; rezCom = 0;
      isdvig = 0;
      do
        {
          ptro = sOutLine;
          ptri = sLine + isdvig;
          ipos = md_strAt( ";", 1, ptri, TRUE, FALSE );
          if( ipos > 0 ) *(ptri+ipos-1) = '\0';
          HB_SKIPTABSPACES( ptri );
          if( *ptri == '#' )
            {
              hb_pp_ParseDirective( ptri+1 );
              if( ipos > 0 ) *( sLine + isdvig + ipos - 1 ) = ';';
              lens = strlen( sLine+isdvig );
              hb_pp_Stuff( " ", sLine+isdvig, 0, (ipos)? ipos:lens, lens );
              if( ipos > 0 ) ipos = 1;
            }
          else
            {   /* Look for macros from #define      */
              while( ( lenToken = NextName( &ptri, sToken ) ) > 0 )
                if( (stdef=DefSearch(sToken,NULL)) != NULL )
                  {
                    ptrb = ptri - lenToken;
                    if( ( i = WorkDefine( &ptri, ptro, stdef ) ) >= 0 )
                      {
                        rezDef++;
                        lens = strlen( ptrb );
                        if( ipos > 0 )
                          {
                            *(ptrb+lens) = ';';
                            lens += strlen( ptrb+lens+1 );
                          }
                        hb_pp_Stuff( ptro, ptrb, i, ptri-ptrb, lens+1 );
                        if( ipos > 0 )
                          {
                            ipos += i - (ptri-ptrb);
                            *(sLine + isdvig + ipos - 1) = '\0';
                          }
                        ptri += i - (ptri-ptrb);
                      }
                  }

              /* Look for definitions from #translate    */
              stcmd = hb_pp_topTranslate;
              while( stcmd != NULL )
                {
                  ptri = sLine + isdvig;
                  lenToken = strlen(stcmd->name);
                  while( ( ifou = md_strAt( stcmd->name, lenToken,
                                            ptri, TRUE, FALSE )) > 0 )
                    {
                      ptri += ifou -1;
                      if( (i = WorkTranslate( ptri+lenToken, ptro, stcmd, &lens )) >= 0 )
                        {
                          lens += lenToken;
                          while( lens > 0 &&
                                  (*(ptri+lens-1)==' ' || *(ptri+lens-1)=='\t') )
                            lens--;
                          if( ipos > 0 )  *(sLine+isdvig+ipos-1) = ';';
                          hb_pp_Stuff( ptro, ptri, i, lens, strlen(ptri) );
                          rezTra = 1;
                          if( ipos > 0 )
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
              if( kolpass < 3 )
                {
                  ptri = sLine + isdvig;
                  HB_SKIPTABSPACES( ptri );
                  if( ISNAME( *ptri ) )
                    NextName( &ptri, sToken );
                  else
                    {
                      i = 0;
                      while( *ptri != ' ' && *ptri != '\t' && *ptri != '\0' &&
                             *ptri != '\"' && *ptri != '\'' && *ptri != '(' &&
                              !ISNAME(*ptri) )
                        {
                          *(sToken+i) = *ptri++;
                          i++;
                        }
                      *(sToken+i) = '\0';
                    }
                  HB_SKIPTABSPACES( ptri );

                  if( ( *ptri == '\0' || ( *ptri != '=' &&
                                            (!IsInStr(*ptri,":/*+-") || *(ptri+1) != '=') &&
                                            ( *ptri != '-' || *(ptri+1) != '>' ) ) )
                       && ( stcmd = ComSearch(sToken,NULL) ) != NULL )
                    {
                      ptro = sOutLine;
                      i = WorkCommand( ptri, ptro, stcmd );
                      ptri = sLine + isdvig;
                      if( ipos > 0 ) *(ptri+ipos-1) = ';';
                      if( i >= 0 )
                        {
                          if( isdvig + ipos > 0 )
                            {
                              lens = strlen( sLine+isdvig );
                              hb_pp_Stuff( ptro, sLine+isdvig, i, (ipos)? ipos-1:lens, lens );
                              if( ipos > 0 ) ipos = i + 1;
                            }
                          else
                            memcpy( sLine, sOutLine, i+1);
                        }
                      rezCom = 1;
                    }
                  else if( ipos > 0 ) *(sLine+isdvig+ipos-1) = ';';
                }
              else if( ipos > 0 )
                *(sLine+isdvig+ipos-1) = ';';
            }
          isdvig += ipos;
        }
      while( ipos != 0 );
      kolpass++;
      if( kolpass > 20 && rezDef )
      {
        hb_compGenError( hb_pp_szErrors, 'F', ERR_RECURSE, NULL, NULL );
        break;
      }
    }
  while( rezDef || rezTra || rezCom );

  return 0;
}

static int WorkDefine( char ** ptri, char * ptro, DEFINES * stdef )
{
  int npars, lens;
  char * ptr;

  HB_TRACE(HB_TR_DEBUG, ("WorkDefine(%p, %s, %p)", ptri, ptro, stdef));

  if( stdef->npars < 0 )
    lens = hb_pp_strocpy( ptro,stdef->value );
  else
    {
      HB_SKIPTABSPACES( *ptri );
      if( **ptri == '(' )
        {
          npars = 0; ptr = *ptri;
          do
            {
              ptr++;
              if( NextParm( &ptr, NULL ) > 0 ) npars++;
            }
          while( *ptr != ')' && *ptr != '\0' );
          if( *ptr == ')' && stdef->npars == npars )
            lens = WorkPseudoF( ptri, ptro, stdef );
          else return -1;
        }
      else return -1;
    }
  return lens;
}

static int WorkPseudoF( char ** ptri, char * ptro, DEFINES * stdef )
{
  char parfict[ MAX_NAME ], * ptrreal;
  char * ptrb;
  int ipos, ifou, ibeg;
  int lenfict, lenreal, lenres;

  HB_TRACE(HB_TR_DEBUG, ("WorkPseudoF(%p, %s, %p)", ptri, ptro, stdef));

  lenres = hb_pp_strocpy( ptro, stdef->value );  /* Copying value of macro to destination string  */

  if( stdef->pars )
    {
      ipos = 0; ibeg = 0;
      do                               /* Parsing through parameters */
        {                                /* in macro definition        */
          if( *(stdef->pars+ipos) == ',' || *(stdef->pars+ipos) == '\0' )
            {
              *(parfict+ipos-ibeg) = '\0';
              lenfict = ipos - ibeg;

              if( **ptri != ')' )
                {
                  (*ptri)++;             /* Get next real parameter */
                  HB_SKIPTABSPACES( *ptri );
                  ptrreal = *ptri;
                  lenreal = NextParm( ptri, NULL);

                  ptrb = ptro;
                  while( (ifou = hb_strAt( parfict, lenfict, ptrb, lenres-(ptrb-ptro) )) > 0 )
                    {
                      ptrb = ptrb+ifou-1;
                      if( !ISNAME(*(ptrb-1)) && !ISNAME(*(ptrb+lenfict)) )
                        {
                          hb_pp_Stuff( ptrreal, ptrb, lenreal, lenfict, lenres );
                          lenres += lenreal - lenfict;
                          ptrb += lenreal;
                        }
                      else ptrb++;
                    }
                  ibeg = ipos+1;
                }
            }
          else *(parfict+ipos-ibeg) = *(stdef->pars+ipos);
          if( *(stdef->pars+ipos) == '\0' ) break;
          ipos++;
        }
      while( 1 );
    }
  else  while( **ptri != ')' ) (*ptri)++;
  (*ptri)++;
  return lenres;
}

static int WorkCommand( char * ptri, char * ptro, COMMANDS * stcmd )
{
  int rez;
  int lenres;
  char * ptrmp;
  char * sToken = stcmd->name;

  HB_TRACE(HB_TR_DEBUG, ("WorkCommand(%s, %s, %p)", ptri, ptro, stcmd));

  do
    {
      lenres = hb_pp_strocpy( ptro, stcmd->value );   /* Copying result pattern */
      ptrmp = stcmd->mpatt;                      /* Pointer to a match pattern */
      s_Repeate = 0;
      s_groupchar = '@';
      rez = CommandStuff( ptrmp, ptri, ptro, &lenres, TRUE, stcmd->com_or_xcom );

      stcmd = stcmd->last;
      if( rez < 0 && stcmd != NULL ) stcmd = ComSearch(sToken, stcmd);
    }
  while( rez < 0 && stcmd != NULL );

  *(ptro+lenres) = '\0';
  if( rez >= 0 ) return lenres;
  return -1;
}

static int WorkTranslate( char * ptri, char * ptro, COMMANDS * sttra, int * lens )
{
  int rez;
  int lenres;
  char * ptrmp;
  char * sToken = sttra->name;

  HB_TRACE(HB_TR_DEBUG, ("WorkTranslate(%s, %s, %p, %p)", ptri, ptro, sttra, lens));

  do
    {
      lenres = hb_pp_strocpy( ptro, sttra->value );
      ptrmp = sttra->mpatt;
      s_Repeate = 0;
      s_groupchar = '@';
      rez = CommandStuff( ptrmp, ptri, ptro, &lenres, FALSE, sttra->com_or_xcom );

      sttra = sttra->last;
      if( rez < 0 && sttra != NULL ) sttra = TraSearch(sToken, sttra);
    }
  while( rez < 0 && sttra != NULL );

  *(ptro+lenres) = '\0';
  if( rez >= 0 )
    {
      *lens = rez;
      return lenres;
    }
  return -1;
}

static int CommandStuff( char * ptrmp, char * inputLine, char * ptro, int * lenres, BOOL com_or_tra, BOOL com_or_xcom )
{
  BOOL endTranslation = FALSE;
  int ipos;
  char * lastopti[ 3 ], * strtopti = NULL, * strtptri = NULL;
  char * ptri = inputLine, * ptr, tmpname[ MAX_NAME ];

  HB_TRACE(HB_TR_DEBUG, ("CommandStuff(%s, %s, %s, %p, %d, %d)", ptrmp, inputLine, ptro, lenres, com_or_tra, com_or_xcom));

  s_numBrackets = 0;
  HB_SKIPTABSPACES( ptri );
  if( ptrmp == NULL ) { if( *ptri != '\0' ) return -1; }
  else
    while( *ptri != '\0' && !endTranslation )
      {
        HB_SKIPTABSPACES( ptrmp );
        if( *ptrmp == '[' && !s_numBrackets && !strtopti )
          strtopti = ptrmp;
        if( !s_numBrackets && strtopti && strtptri != ptri && ISNAME( *ptri ) )
          {
            strtptri = ptri;
            ptrmp = strtopti;
            ptr = ptri;
            ipos = NextName( &ptr, tmpname );
            ipos = md_strAt( tmpname, ipos, strtopti, TRUE, TRUE );
            if( ipos && TestOptional( strtopti, strtopti+ipos-2 ) )
              {
                ptr = strtopti+ipos-2;
                ptr = PrevSquare( ptr, strtopti, NULL );
                if( ptr )
                    ptrmp = ptr;
              }
          }
        switch( *ptrmp ) {
        case '[':
          s_numBrackets++;
          s_aIsRepeate[ s_Repeate ] = 0;
          lastopti[s_Repeate++] = ptrmp;
          ptrmp++;
          if( !CheckOptional( ptrmp, ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
            SkipOptional( &ptrmp );
          break;
        case ']':
          if( s_Repeate )
            {
              s_Repeate--;
              if( s_aIsRepeate[ s_Repeate ] )
                {
                  if( ISNAME(*ptri) )
                    {
                      ptr = ptri;
                      ipos = NextName( &ptr, tmpname );
                      ipos = md_strAt( tmpname, ipos, ptrmp, TRUE, TRUE );
                      if( ipos && TestOptional( ptrmp+1, ptrmp+ipos-2 ) )
                        {
                         ptr = PrevSquare( ptrmp+ipos-2, ptrmp+1, NULL );
                         if( !ptr || CheckOptional( ptrmp+1, ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
                           {
                             ptrmp = lastopti[s_Repeate];
                             ptrmp++;
                             s_Repeate++;
                             SkipOptional( &ptrmp );
                             s_numBrackets++;
                             ptrmp++;
                             strtptri = ptri;
                            }
                            else
                               ptrmp = lastopti[s_Repeate];
                        }
                      else
                         ptrmp = lastopti[s_Repeate];
                    }
                  else
                    ptrmp = lastopti[s_Repeate];
                }
              else ptrmp++;
              s_numBrackets--;
            }
          else { s_numBrackets--; ptrmp++; }
          break;
        case ',':
          if( !s_numBrackets ) strtopti = NULL;
          if( *ptri == ',' ) { ptrmp++; ptri++; }
          else
            {
              if( s_numBrackets )
                {
                  SkipOptional( &ptrmp );
                }
              else return -1;
            }
          break;
        case '\1':  /*  Match marker */
          if( !s_numBrackets ) strtopti = NULL;
          if( !WorkMarkers( &ptrmp, &ptri, ptro, lenres, com_or_xcom ) )
            {
              if( s_numBrackets )
                {
                  SkipOptional( &ptrmp );
                }
              else return -1;
            }
          break;
        case '\0':
          if( com_or_tra )
            return -1;
          else endTranslation = TRUE;
          break;
        default:    /*   Key word    */
          if( !s_numBrackets ) strtopti = NULL;
          ptr = ptri;
          if( *ptri == ',' || truncmp( &ptri, &ptrmp, !com_or_xcom ) )
            {
              ptri = ptr;
              if( s_numBrackets )
                {
                  SkipOptional( &ptrmp );
                }
              else return -1;
            }
        }
        HB_SKIPTABSPACES( ptri );
      };

  if( *ptrmp != '\0' )
    {
      if( s_Repeate ) { s_Repeate = 0; ptrmp = lastopti[0]; }
      s_numBrackets = 0;
      do
        {
          HB_SKIPTABSPACES( ptrmp );
          if( *ptrmp != '\0' )
            switch( *ptrmp ) {
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
      while( *ptrmp != '\0' );
    }
  SearnRep( "\1","",0,ptro,lenres);

  *(ptro + *lenres) = '\0';
  *lenres = RemoveSlash( ptro );   /* Removing '\' from result string */
  if( com_or_tra ) return 1; else return (ptri-inputLine);
}

static int RemoveSlash( char * stroka )
{
  char *ptr = stroka;
  int State = STATE_INIT;
  BOOL bDirective = FALSE;
  int lenres = strlen( stroka );

  HB_TRACE(HB_TR_DEBUG, ("RemoveSlash(%s)", stroka));

  while( *ptr != '\0' )
    {
      switch( State ) {
      case STATE_INIT:
        if( *ptr != ' ' && *ptr != '\t' ) State = STATE_NORMAL;
        if( *ptr == '#' )  bDirective = TRUE;
      case STATE_NORMAL:
        if( *ptr == '\'' )  State = STATE_QUOTE1;
        else if( *ptr == '\"' )  State = STATE_QUOTE2;
        else if( *ptr == ';' )
          {
            State = STATE_INIT;
            bDirective = FALSE;
          }
        else if( !bDirective )
          {
            if( *ptr == '\\' && ( *(ptr+1) == '[' || *(ptr+1) == ']' ||
                                  *(ptr+1) == '{' || *(ptr+1) == '}' || *(ptr+1) == '<' ||
                                  *(ptr+1) == '>' || *(ptr+1) == '\'' || *(ptr+1) == '\"' ) )
              {
                hb_pp_Stuff( "", ptr, 0, 1, lenres - (ptr - stroka) );
                lenres--;
                ptr++;
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

static int WorkMarkers( char ** ptrmp, char ** ptri, char * ptro, int * lenres, BOOL com_or_xcom )
{
  static char expreal[ MAX_EXP ];

  char exppatt[ MAX_NAME ];
  int lenreal = 0, maxlenreal = HB_PP_STR_SIZE, lenpatt;
  int rezrestr, ipos;
  char * ptr, * ptrtemp;

  HB_TRACE(HB_TR_DEBUG, ("WorkMarkers(%p, %p, %s, %p)", ptrmp, ptri, ptro, lenres));

  /* Copying a match pattern to 'exppatt' */
  lenpatt = stroncpy( exppatt, *ptrmp, 4 );
  *ptrmp += 4;
  HB_SKIPTABSPACES( *ptrmp );
  if( **ptri == ',' )
    {
      if( s_numBrackets )
        {
          return 0;
        }
    }
  ptrtemp = *ptrmp;
  if( *(exppatt+2) != '2' && *ptrtemp == ']' )
    {
      ptrtemp++;
      HB_SKIPTABSPACES( ptrtemp );
    }
  if( *(exppatt+2) != '2' && *ptrtemp != '\1' && *ptrtemp != ',' &&
       *ptrtemp != '[' && *ptrtemp != ']' && *ptrtemp != '\0' )
    {
      lenreal = strincpy( expreal, ptrtemp );
      if( (ipos = md_strAt( expreal, lenreal, *ptri, TRUE, TRUE )) > 0 )
        {
          if( ptrtemp > *ptrmp )
            {
              if( ipos == 1 )
                {
                  if( s_numBrackets )
                    {
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
              if( ipos > 1 && isExpres( expreal ) )
                *ptri += lenreal;
              else
                {
                  if( s_numBrackets )
                    {
                      return 0;
                    }
                }
            }
        }
      else
        {
          if( s_numBrackets )
            {
              return 0;
            }
          else lenreal = 0;
        }
    }

  if( *(exppatt+2) == '4' )       /*  ----  extended match marker  */
    {
      if( !lenreal ) lenreal = getExpReal( expreal, ptri, FALSE, maxlenreal );
      SearnRep( exppatt,expreal,lenreal,ptro,lenres);
    }
  else if( *(exppatt+2) == '3' )  /*  ----  wild match marker  */
    {
      lenreal = hb_pp_strocpy( expreal, *ptri );
      *ptri += lenreal;
      SearnRep( exppatt,expreal,lenreal,ptro,lenres);
    }
  else if( *(exppatt+2) == '2' )  /*  ---- restricted match marker  */
    {
      while( **ptrmp != '>' ) *(exppatt+lenpatt++) = *((*ptrmp)++);
      *(exppatt+lenpatt) = '\0';
      (*ptrmp)++;

      ptr = exppatt + 4;
      rezrestr = 0;
      while( *ptr != '\0' )
        {
          if( *ptr == '&' )
            {
              if( **ptri == '&' )
                {
                  rezrestr = 1;
                  /*  (*ptri)++; */
                  lenreal = getExpReal( expreal, ptri, FALSE, maxlenreal );
                  SearnRep( exppatt,expreal,lenreal,ptro,lenres);
                  break;
                }
              else ptr++;
            }
          else
            {
              HB_SKIPTABSPACES( ptr );
              /* Comparing real parameter and restriction value */
              ptrtemp = ptr;
              if( !strincmp( *ptri, &ptr, !com_or_xcom ) )
                {
                  lenreal = stroncpy( expreal, *ptri, (ptr-ptrtemp) );
                  *ptri += lenreal;
                  SearnRep( exppatt,expreal,lenreal,ptro,lenres);
                  rezrestr = 1;
                  break;
                }
              else
                {
                  while( *ptr != ',' && *ptr != '\0' ) ptr++;
                  if( *ptr == ',' ) ptr++;
                }
            }
        }
      if( rezrestr == 0 )
        {  /* If restricted match marker doesn't correspond to real parameter */
          if( s_numBrackets )
            {
              return 0;
            }
          else return 0;
        }
    }
  else if( *(exppatt+2) == '1' )  /*  ---- list match marker  */
    {
      if( !lenreal ) lenreal = getExpReal( expreal, ptri, TRUE, maxlenreal );
      SearnRep( exppatt,expreal,lenreal,ptro,lenres);
    }
  else                             /*  ---- regular match marker  */
    {
      /* Copying a real expression to 'expreal' */
      if( !lenreal ) lenreal = getExpReal( expreal, ptri, FALSE, maxlenreal );
      SearnRep( exppatt,expreal,lenreal,ptro,lenres);
    }
  return 1;
}

static int getExpReal( char * expreal, char ** ptri, BOOL prlist, int maxrez )
{
  int lens = 0;
  char * sZnaki = "+-=><*/$.&:#%!^";
  int State;
  int StBr1 = 0, StBr2 = 0, StBr3 = 0;
  BOOL rez = FALSE;

  HB_TRACE(HB_TR_DEBUG, ("getExpReal(%s, %p, %d, %d)", expreal, ptri, prlist, maxrez));

  HB_SKIPTABSPACES( *ptri );
  State = (**ptri=='\'' || **ptri=='\"')? STATE_EXPRES:STATE_ID;
  while( **ptri != '\0' && !rez && lens < maxrez )
    {
      switch( State ) {
      case STATE_QUOTE1:
        if(**ptri=='\'')
          State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
        break;
      case STATE_QUOTE2:
        if(**ptri=='\"')
          State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
        break;
      case STATE_BRACKET:
        if( **ptri == '\'' ) State = STATE_QUOTE1;
        else if( **ptri == '\"' ) State = STATE_QUOTE2;
        else if( **ptri == '(' ) StBr1++;
        else if( **ptri == '[' ) StBr2++;
        else if( **ptri == '{' ) StBr3++;
        else if( **ptri == ')' )
          { StBr1--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
        else if( **ptri == ']' )
          { StBr2--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
        else if( **ptri == '}' )
          { StBr3--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
        break;
      case STATE_ID:
      case STATE_ID_END:
        if( ( (ISNAME(**ptri) || **ptri=='\\') && State == STATE_ID_END ) ||
             **ptri==',' || **ptri=='\'' || **ptri=='\"' || **ptri==')' )
          {
            if( **ptri == ',' )
              {
                if( !prlist ) rez = TRUE;
                State = STATE_EXPRES;
              }
            else rez = TRUE;
          }
        else if( IsInStr( **ptri, sZnaki ) )
          {
            State = STATE_EXPRES;
          }
        else if( **ptri == '(' )
          {
            State = STATE_BRACKET;
            StBr1 = 1;
          }
        else if( **ptri == '[' )
          {
            State = STATE_BRACKET;
            StBr2 = 1;
          }
        else if( **ptri == '{' )
          {
            State = STATE_BRACKET;
            StBr3 = 1;
          }
        else if( **ptri == ' ' ) State = STATE_ID_END;
        break;
      case STATE_EXPRES:
      case STATE_EXPRES_ID:
        if( **ptri == '\'' ) State = STATE_QUOTE1;
        else if( **ptri == '\"' ) State = STATE_QUOTE2;
        else if( ISNAME(**ptri) ) State = STATE_EXPRES_ID;
        else if( **ptri == ' ' )
          {
            if( State == STATE_EXPRES_ID ) State = STATE_ID_END;
            else if( lens > 2 && ( ( *(*ptri-2)=='+' && *(*ptri-1)=='+' ) ||
                                   ( *(*ptri-2)=='-' && *(*ptri-1)=='-' ) ) )
              State = STATE_ID_END;
          }
        else if( **ptri == '(' ) { StBr1++; State = STATE_BRACKET; }
        else if( **ptri == '[' ) { StBr2++; State = STATE_BRACKET; }
        else if( **ptri == '{' ) { StBr3++; State = STATE_BRACKET; }
        else if( **ptri == ',' ) { if ( !prlist ) rez = TRUE; State = STATE_EXPRES; }
        else if( **ptri == '.' && *(*ptri-2) == '.' &&
                 ( *(*ptri-1) == 'T' || *(*ptri-1) == 'F' ||
                   *(*ptri-1) == 't' || *(*ptri-1) == 'f' ) )
          State = STATE_ID_END;
        else State = STATE_EXPRES;
        break;
      }
      if( !rez )
        {
          if( expreal != NULL ) *expreal++ = **ptri;
          (*ptri)++;
          lens++;
        }
    }
  if( expreal != NULL )
    {
      if( *(expreal-1) == ' ' ) { expreal--; lens--; };
      *expreal = '\0';
    }
  return lens;
}

static BOOL isExpres( char * stroka )
{
  HB_TRACE(HB_TR_DEBUG, ("isExpres(%s)", stroka));

  return strlen( stroka ) <= getExpReal( NULL, &stroka, FALSE, HB_PP_STR_SIZE );
}

static BOOL TestOptional( char *ptr1, char *ptr2 )
{
  int nbr = 0;
  BOOL flagname = FALSE;
  int statevar = 0;

  HB_TRACE(HB_TR_DEBUG, ("TestOptional(%s, %s)", ptr1, ptr2));

  while( ptr1 <= ptr2 )
    {
      if( *ptr1 == '[' ) nbr++;
      else if( *ptr1 == ']' )
        {
          if( nbr )
            {
              nbr--;
              flagname = FALSE;
            }
          else return 0;
        }
      else if( *ptr1 == '\1' && *(ptr1+2) == '2' && nbr ) statevar = 1;
      else if( *ptr1 == '>' && statevar ) statevar = 0;
      else if( *ptr1 != ' ' && *ptr1 != '\t' && !statevar )
        {
          if( nbr ) flagname = TRUE;
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

static BOOL CheckOptional( char * ptrmp, char * ptri, char * ptro, int * lenres, BOOL com_or_tra, BOOL com_or_xcom )
{
  int save_numBr = s_numBrackets, save_Repeate = s_Repeate;
  BOOL endTranslation = FALSE;
  BOOL bResult = TRUE;
  char * lastInputptr[ 5 ];
  char * lastopti[ 3 ], *ptr;

  HB_SYMBOL_UNUSED( com_or_tra );

  HB_TRACE(HB_TR_DEBUG, ("CheckOptional(%s, %s, %s, %p, %d, %d)", ptrmp, ptri, ptro, lenres, com_or_tra, com_or_xcom));

  s_bReplacePat = FALSE;
  lastInputptr[s_Repeate] = ptri;
  while( *ptri != '\0' && !endTranslation && bResult )
    {
      HB_SKIPTABSPACES( ptrmp );
      switch( *ptrmp ) {
      case '[':
        s_numBrackets++;
        s_aIsRepeate[ s_Repeate ] = 0;
        lastInputptr[s_Repeate] = ptri;
        lastopti[s_Repeate++] = ptrmp;
        ptrmp++;
        break;
      case ']':
        if( s_numBrackets == save_numBr )
          endTranslation = TRUE;
        else
          {
            if( s_Repeate )
              {
                s_Repeate--;
                ptrmp = lastopti[s_Repeate];
              }
            else ptrmp++;
            s_numBrackets--;
          }
        break;
      case ',':
        if( *ptri == ',' ) { ptrmp++; ptri++; }
        else
          {
            if( s_numBrackets - save_numBr > 0 )
              {
                SkipOptional( &ptrmp );
                ptri = lastInputptr[s_Repeate];
              }
            else bResult = FALSE;
          }
        break;
      case '\1':  /*  Match marker */
        if( !WorkMarkers( &ptrmp, &ptri, ptro, lenres, com_or_xcom ) )
          {
            if( s_numBrackets - save_numBr > 0 )
              {
                SkipOptional( &ptrmp );
                ptri = lastInputptr[s_Repeate];
              }
            else bResult = FALSE;
          }
        break;
      case '\0':
        bResult = FALSE;
      default:    /*   Key word    */
        ptr = ptri;
        if( *ptri == ',' || truncmp( &ptri, &ptrmp, !com_or_xcom ) )
          {
            ptri = ptr;
            if( s_numBrackets - save_numBr > 0 )
              {
                SkipOptional( &ptrmp );
                ptri = lastInputptr[s_Repeate];
              }
            else bResult = FALSE;
          }
      }
      HB_SKIPTABSPACES( ptri );
    };
  if( *ptri == '\0' )
    {
      do
        {
          HB_SKIPTABSPACES( ptrmp );
          if( *ptrmp == '[' )
            {
              ptrmp++;
              SkipOptional( &ptrmp );
            }
          else if( *ptrmp == ']' )
            break;
          else
            {
              bResult = 0;
              break;
            }
        }
      while( 1 );
    }
  s_Repeate = save_Repeate;
  s_numBrackets = save_numBr;
  s_bReplacePat = TRUE;
  return bResult;
}

static void SkipOptional( char ** ptri )
{
  int nbr = 0;

  HB_TRACE(HB_TR_DEBUG, ("SkipOptional(%p)", ptri));

  while( **ptri != ']' || nbr )
    {
      switch( **ptri ) {
      case '[':  nbr++; break;
      case ']':  nbr--; break;
      case '\1':
        (*ptri) += 3;
        if( *(*ptri-1) == '2' )
          while( **ptri != '>' ) (*ptri)++;
        break;
      }
      (*ptri)++;
    }
  if( **ptri == ']' && s_numBrackets > 0 )
    {
      if( s_Repeate ) s_Repeate--;
      s_numBrackets--; (*ptri)++;
    }
}

static void SearnRep( char * exppatt, char * expreal, int lenreal, char * ptro, int * lenres )
{
  static char expnew[ MAX_EXP ];

  int ifou, isdvig = 0;
  BOOL rezs;
  int kolmarkers;
  int lennew, i;
  char lastchar = '0';
  char *ptr, *ptr2, *ptrOut = ptro;

  HB_TRACE(HB_TR_DEBUG, ("SearnRep(%s, %s, %d, %s, %p)", exppatt, expreal, lenreal, ptro, lenres));

  if( *(exppatt+1) == '\0' ) *( ptro + *lenres ) = '\0';
  while( (ifou = md_strAt( exppatt, (*(exppatt+1))? 2:1, ptrOut, FALSE, FALSE )) > 0 )
    {
      rezs = FALSE;
      ptr = ptrOut + ifou - 2;
      kolmarkers = 0;
      ptr = PrevSquare( ptr, ptrOut, &kolmarkers );
      if( ptr )
        {
          if( s_Repeate ) s_aIsRepeate[ s_Repeate - 1 ]++;
          if( !s_bReplacePat ) return;
          ptr2 = ptrOut + ifou + 3;
          while( *ptr2 != ']' || *(ptr2-1) == '\\' )
            {
              if( *ptr2 == '\1' ) kolmarkers++;
              ptr2++;
            }

          if( s_Repeate && lenreal && kolmarkers && lastchar != '0' &&
               *(ptrOut + ifou + 2) == '0' )
            {
              isdvig += ifou;
              rezs = TRUE;
            }
          else
            {
              if( lenreal == 0 )
                {
                  if( s_numBrackets >= 2 )
                    {
                      isdvig += ifou;
                      continue;
                    }
                  else
                    {
                      hb_pp_Stuff( "", ptr, 0, ptr2-ptr+1, *lenres-(ptr-ptro) );
                      *lenres -= ptr2-ptr+1;
                      isdvig = ptr - ptro;
                      rezs = TRUE;
                    }
                }
              else
                {
                  lennew = ptr2-ptr-1;

                  memcpy( expnew, ptr+1, lennew );
                  *(expnew + lennew++) = ' ';
                  *(expnew + lennew) = '\0';
                  while( (i = hb_strAt( exppatt, 2, expnew, lennew )) > 0 )
                    lennew += ReplacePattern( exppatt[2], expreal, lenreal, expnew+i-1, lennew );
                  if( kolmarkers )
                    {
                      s_groupchar = (char) ( (unsigned int)s_groupchar + 1 );
                      for( i=0; i<lennew; i++ )
                        if( *(expnew+i) == '\1' )
                          {
                            *(expnew+i+3) = s_groupchar;
                            i += 4;
                          }
                    }
                  hb_pp_Stuff( expnew, ptr, lennew, 0, *lenres-(ptr-ptro)+1 );
                  *lenres += lennew;
                  isdvig = ptr - ptro + (ptr2-ptr-1) + lennew;
                  rezs = TRUE;
                }
            }
        }
      if( !rezs && s_bReplacePat )
        {
          if( *(ptrOut + ifou + 2) != '0' && *(exppatt+1) )
            {
              if( lastchar == '0' ) lastchar = *(ptrOut + ifou + 2);
              if( lastchar != *(ptrOut + ifou + 2) )
                {
                  isdvig += ifou + 3;
                  ptrOut = ptro + isdvig;
                  continue;
                }
            }
          *lenres += ReplacePattern( exppatt[2], expreal, lenreal,
                                     ptrOut + ifou - 1, *lenres-isdvig-ifou+1 );
          isdvig += ifou - 1;
        }
      else if( !s_bReplacePat ) isdvig += ifou;
      ptrOut = ptro + isdvig;
    }
}

static int ReplacePattern( char patttype, char * expreal, int lenreal, char * ptro, int lenres )
{
  int rmlen = lenreal, ifou, lenitem, i;
  char sQuotes[ 4 ] = "\"\",";

  HB_TRACE(HB_TR_DEBUG, ("ReplacePattern(%c, %s, %d, %s, %p)", patttype, expreal, lenreal, ptro, lenres));

  switch( *(ptro+2) ) {
  case '0':  /* Regular result marker  */
    hb_pp_Stuff( expreal, ptro, lenreal, 4, lenres );
    break;
  case '1':  /* Dumb stringify result marker  */
    pp_rQuotes( expreal, sQuotes );
    hb_pp_Stuff( sQuotes, ptro, 2, 4, lenres );
    if( lenreal )
      hb_pp_Stuff( expreal, ptro+1, lenreal, 0, lenres );
    rmlen = lenreal + 2;
    break;
  case '2':  /* Normal stringify result marker  */
    if( !lenreal )
      hb_pp_Stuff( "", ptro, 0, 4, lenres );
    else if( patttype == '1' )          /* list match marker */
      {
        hb_pp_Stuff( "", ptro, 0, 4, lenres );
        lenres -= 4;
        rmlen = 0;
        do
          {
            ifou = md_strAt( ",", 1, expreal, FALSE, TRUE );
            lenitem = (ifou)? ifou-1:lenreal;
            if( *expreal != '\0' )
              {
                i = (ifou)? 3:2;
                pp_rQuotes( expreal, sQuotes );
                hb_pp_Stuff( sQuotes, ptro, i, 0, lenres );
                hb_pp_Stuff( expreal, ptro+1, lenitem, 0, lenres+i );
                ptro += i + lenitem;
                rmlen += i + lenitem;
              }
            expreal += ifou;
            lenreal -= ifou;
          }
        while( ifou > 0 );
      }
    else
      {
        pp_rQuotes( expreal, sQuotes );
        hb_pp_Stuff( sQuotes, ptro, 2, 4, lenres );
        hb_pp_Stuff( expreal, ptro+1, lenreal, 0, lenres );
        rmlen = lenreal + 2;
      }
    break;
  case '3':  /* Smart stringify result marker  */
    if( patttype == '1' )          /* list match marker */
      {
        hb_pp_Stuff( "", ptro, 0, 4, lenres );
        lenres -= 4;
        rmlen = 0;
        do
          {
            ifou = md_strAt( ",", 1, expreal, FALSE, TRUE );
            lenitem = (ifou)? ifou-1:lenreal;
            if( *expreal != '\0' )
              {
                if( !lenitem || *expreal == '(' || *expreal == '&' ||
                     ( *expreal=='\"' && *(expreal+lenitem-1)=='\"' ) ||
                     ( *expreal == '\'' && *(expreal+lenitem-1)=='\'' ) )
                  {
                    if( ifou ) lenitem++;
                    hb_pp_Stuff( ( *expreal=='&' ) ? expreal + 1 : expreal, ptro,
                              ( *expreal=='&' ) ? lenitem - 1 : lenitem, 0, lenres );
                  }
                else
                  {
                    i = (ifou)? 3:2;
                    pp_rQuotes( expreal, sQuotes );
                    hb_pp_Stuff( sQuotes, ptro, i, 0, lenres );
                    hb_pp_Stuff( expreal, ptro+1, lenitem, 0, lenres+i );
                    ptro += i;
                    rmlen += i;
                  }
                ptro += lenitem;
                rmlen += lenitem;
              }
            expreal += ifou;
            lenreal -= ifou;
          }
        while( ifou > 0 );
      }
    else if( !lenreal || *expreal == '(' || *expreal == '&' ||
             ( *expreal == '\"' && *( expreal + lenreal - 1 ) == '\"' ) ||
             ( *expreal == '\'' && *( expreal + lenreal - 1 ) == '\'' ) )
      hb_pp_Stuff( ( *expreal == '&' ) ? expreal + 1 : expreal, ptro,
                ( *expreal == '&' ) ? lenreal - 1 : lenreal, 4, lenres );
    else
      {
        pp_rQuotes( expreal, sQuotes );
        hb_pp_Stuff( sQuotes, ptro, 2, 4, lenres );
        hb_pp_Stuff( expreal, ptro + 1, lenreal, 0, lenres );
        rmlen = lenreal + 2;
      }
    break;
  case '4':  /* Blockify result marker  */
    if( !lenreal )
      hb_pp_Stuff( expreal, ptro, lenreal, 4, lenres );
    else if( patttype == '1' )          /* list match marker */
      {
        hb_pp_Stuff( "", ptro, 0, 4, lenres );
        lenres -= 4;
        rmlen = 0;
        do
          {
            ifou = md_strAt( ",", 1, expreal, FALSE, TRUE );
            lenitem = (ifou)? ifou-1:lenreal;
            if( *expreal != '\0' )
              {
                i = (ifou)? 5:4;
                hb_pp_Stuff( "{||},", ptro, i, 0, lenres );
                hb_pp_Stuff( expreal, ptro+3, lenitem, 0, lenres+i );
                ptro += i + lenitem;
                rmlen += i + lenitem;
              }
            expreal += ifou;
            lenreal -= ifou;
          }
        while( ifou > 0 );
      }
    else
      {
        hb_pp_Stuff( "{||}", ptro, 4, 4, lenres );
        hb_pp_Stuff( expreal, ptro+3, lenreal, 0, lenres );
        rmlen = lenreal + 4;
      }
    break;
  case '5':  /* Logify result marker  */
    rmlen = 3;
    if( !lenreal )
      {
        hb_pp_Stuff( ".F.", ptro, 3, 4, lenres );
      }
    else
      hb_pp_Stuff( ".T.", ptro, 3, 4, lenres );
    break;
  }
  return rmlen - 4;
}

static void pp_rQuotes( char * expreal, char * sQuotes )
{
  BOOL lQuote1 = FALSE;
  BOOL lQuote2 = FALSE;

  HB_TRACE(HB_TR_DEBUG, ("pp_rQuotes(%s, %s)", expreal, sQuotes));

  while( *expreal != '\0' )
    {
      if( *expreal == '\"' ) lQuote2 = TRUE;
      else if( *expreal == '\'' ) lQuote1 = TRUE;
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

int hb_pp_RdStr( FILE * handl_i, char * buffer, int maxlen, BOOL lDropSpaces, char * sBuffer, int * lenBuffer, int * iBuffer )
{
  int readed = 0;
  int State = 0;
  char cha,cLast='\0';

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_RdStr(%p, %s, %d, %d, %s, %p, %p)", handl_i, buffer, maxlen, lDropSpaces, sBuffer, lenBuffer, iBuffer));

  if( *lenBuffer == 0 ) return -1;
  while(1)
    {
      if( *iBuffer == *lenBuffer )
        {
          if( (*lenBuffer = fread(sBuffer,1,HB_PP_BUFF_SIZE,handl_i)) < 1 )
            sBuffer[0] = '\n';
          *iBuffer = 0;
        }
      cha = sBuffer[ *iBuffer ];
      (*iBuffer)++;
      if( cha == '\n' )  break;
      if( maxlen > 0 )
        {
          switch( s_ParseState ) {
          case STATE_COMMENT:
            if( cha == '/' && cLast == '*' )
              {
                s_ParseState = STATE_NORMAL;
                cha = ' ';
              }
            cLast = cha;
            break;
          case STATE_QUOTE1: if(cha=='\'') s_ParseState = STATE_NORMAL; break;
          case STATE_QUOTE2: if(cha=='\"') s_ParseState = STATE_NORMAL; break;
          default:
            switch( cha ) {
            case '[': s_ParseState = STATE_BRACKET; break;
            case ']': s_ParseState = STATE_NORMAL; break;
            case '\"':
              if( s_ParseState != STATE_BRACKET ) s_ParseState = STATE_QUOTE2;
              break;
            case '\'':
              if( s_ParseState != STATE_BRACKET ) s_ParseState = STATE_QUOTE1;
              break;
            case '&':
              if( readed>0 && buffer[readed-1] == '&' ) { maxlen = 0; readed--; }
              break;
            case '/':
              if( readed>0 && buffer[readed-1] == '/' ) { maxlen = 0; readed--; }
              break;
            case '*':
              if( readed > 0 && buffer[readed-1] == '/' )
                {
                  s_ParseState = STATE_COMMENT;
                  readed--;
                }
              else if( !State ) maxlen = readed = 0;
              break;
            }
          }
          if( cha != ' ' && cha != '\t' ) State = 1;
          if( lDropSpaces && State ) lDropSpaces = 0;
          if( readed<maxlen && (!lDropSpaces || readed==0) &&
              s_ParseState != STATE_COMMENT )
            buffer[readed++]=cha;
        }
    }
  while(--readed >= 0 && ( buffer[readed] == ' ' || buffer[readed] == '\t') );
  readed++;
  buffer[readed]='\0';
  return readed;
}

int hb_pp_WrStr( FILE * handl_o, char * buffer )
{
  int lens = strlen(buffer);

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_WrStr(%p, %s)", handl_o, buffer));

  fwrite(buffer,lens,1,handl_o);
  if( *(buffer+lens-1) != '\n' )
    fwrite("\n",1,1,handl_o);
  return 0;
}

static int md_strAt( char * szSub, int lSubLen, char * szText, BOOL checkword, BOOL checkPrth )
{
  int State = STATE_NORMAL;
  long lPos = 0, lSubPos = 0;
  int kolPrth = 0;
  int lCase = ( *szSub == '\1' )? 0:1;

  HB_TRACE(HB_TR_DEBUG, ("md_strAt(%s, %d, %s, %d, %d)", szSub, lSubLen, szText, checkword, checkPrth));

  while( *(szText+lPos) != '\0' && lSubPos < lSubLen )
    {
      if( State == STATE_QUOTE1 )
        {
          if( *(szText+lPos) == '\'' )
            State = STATE_NORMAL;
          lPos++;
        }
      else if( State == STATE_QUOTE2 )
        {
          if( *(szText+lPos) == '\"' )
            State = STATE_NORMAL;
          lPos++;
        }
      else
        {
          if( *(szText+lPos) == '\"' && ( lPos == 0 || *(szText+lPos-1) != '\\' ) )
            {
              State = STATE_QUOTE2;
              lPos++;
              continue;
            }
          else if( *(szText+lPos) == '\'' && ( lPos == 0 || *(szText+lPos-1) != '\\' ) )
            {
              State = STATE_QUOTE1;
              lPos++;
              continue;
            }
          else if( *(szText+lPos) == '(' )
            kolPrth++;
          else if( *(szText+lPos) == ')' )
            kolPrth--;
          if( !lSubPos && checkPrth && ( (kolPrth > 1) ||
                                         (kolPrth == 1 && *(szText+lPos) != '(') || (kolPrth == 0 && *(szText+lPos) == ')')) )
            {
              lPos++;
              continue;
            }

          if( ( lCase && toupper(*(szText + lPos)) == toupper(*(szSub + lSubPos)) ) ||
              ( !lCase && *(szText + lPos) == *(szSub + lSubPos) ) )
            {
              lSubPos++;
              lPos++;
              if( lSubPos >= lSubLen  && checkword &&
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

static char * PrevSquare( char * ptr, char * bound, int * kolmark )
{
   int State = STATE_NORMAL;

   HB_TRACE(HB_TR_DEBUG, ("PrevSquare(%s, %s, %d)", ptr, bound, *kolmark));
   while( ptr > bound )
   {
      if( State == STATE_QUOTE1 )
      {
         if( *ptr == '\'' )  State = STATE_NORMAL;
      }
      else if( State == STATE_QUOTE2 )
      {
         if( *ptr == '\"' )  State = STATE_NORMAL;
      }
      else
      {
         if( *ptr == '\"' && *(ptr-1) != '\\' ) State = STATE_QUOTE2;
         else if( *ptr == '\'' && *(ptr-1) != '\\' ) State = STATE_QUOTE1;
         else if( kolmark && *ptr == '\1' ) (*kolmark)++;
         else if( ( *ptr == '[' || *ptr == ']' ) && *(ptr-1) != '\\' )
            break;
      }
      ptr--;
   }
   return ( *ptr == '[' && State == STATE_NORMAL )? ptr:NULL;
}

static int IsInStr( char symb, char * s )
{
  HB_TRACE(HB_TR_DEBUG, ("IsInStr(%c, %s)", symb, s));

  while( *s != '\0' ) if( *s++ == symb ) return 1;
  return 0;
}

void hb_pp_Stuff(char *ptri, char * ptro, int len1, int len2, int lenres )
{
  char *ptr1, *ptr2;
  int i;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_Stuff(%s, %s, %d, %d, %d)", ptri, ptro, len1, len2, lenres));

  if( len1 > len2 )
    {
      ptr1 = ptro + lenres;
      ptr2 = ptro + lenres + len1 - len2;
      for( ; ptr1 >= ptro; ptr1--,ptr2-- ) *ptr2 = *ptr1;
    }
  else
    {
      ptr1 = ptro + len2;
      ptr2 = ptro + len1;
      for( ; ptr1 <= ptro+lenres; ptr1++,ptr2++ ) *ptr2 = *ptr1;
    }
  ptr2 = ptro;
  for( i=0; i < len1; i++ ) *ptr2++ = *(ptri+i);
}

int hb_pp_strocpy( char * ptro, char * ptri )
{
  int lens = 0;

  HB_TRACE(HB_TR_DEBUG, ("hb_pp_strocpy(%s, %s)", ptro, ptri));

  if( ptri != NULL )
    while( *ptri != '\0' )
      {
        *ptro++ = *ptri++;
        lens++;
      }
  *ptro = '\0';
  return lens;
}

static int stroncpy( char * ptro, char * ptri, int lens )
{
  int i = 0;

  HB_TRACE(HB_TR_DEBUG, ("stroncpy(%s, %s, %d)", ptro, ptri, lens));

  for( ; i < lens; i++ ) *(ptro+i) = *ptri++;
  i--;
  while( i > 0 && *(ptro+i) == ' ' ) i--;
  i++;
  *(ptro+i) = '\0';
  return i;
}

static BOOL truncmp( char ** ptro, char ** ptri, BOOL lTrunc )
{
  char *ptrb = *ptro, co, ci;

  HB_TRACE(HB_TR_DEBUG, ("truncmp(%p, %p, %d)", ptro, ptri, lTrunc));

  for( ; **ptri != ' ' && **ptri != '\t' && **ptri != ',' && **ptri != '[' && **ptri != ']' &&
          **ptri != '\1' && **ptri != '\0' && toupper(**ptri)==toupper(**ptro);
        (*ptro)++, (*ptri)++ );
  co = *(*ptro-1);
  ci = **ptri;
  if( ( ( ci == ' ' || ci == ',' || ci == '[' ||
          ci == ']' || ci == '\1' || ci == '\0' ) &&
        ( ( !ISNAME(**ptro) && ISNAME(co) ) ||
          ( !ISNAME(co) ) ) ) )
    return FALSE;
  else if( lTrunc && *ptro-ptrb >= 4 && ISNAME(ci) && !ISNAME(**ptro) && ISNAME(co) )
    {
      while( ISNAME(**ptri) ) (*ptri)++;
      return FALSE;
    }
  return TRUE;
}

static BOOL strincmp( char * ptro, char ** ptri, BOOL lTrunc )
{
  char *ptrb = ptro, co, ci;

  HB_TRACE(HB_TR_DEBUG, ("strincmp(%s, %p)", ptro, ptri));

  for( ; **ptri != ',' && **ptri != '[' && **ptri != ']' &&
         **ptri != '\1' && **ptri != '\0' && toupper(**ptri)==toupper(*ptro);
        ptro++, (*ptri)++ );
  co = *(ptro-1);
  ci = **ptri;
  if( ( ( ci == ' ' || ci == ',' || ci == '[' ||
          ci == ']' || ci == '\1' || ci == '\0' ) &&
        ( ( !ISNAME(*ptro) && ISNAME(co) ) ||
          ( !ISNAME(co) ) ) ) )
    return FALSE;
  else if( lTrunc && ptro-ptrb >= 4 && ISNAME(ci) && !ISNAME(*ptro) && ISNAME(co) )
    {
      /*      while( ISNAME(**ptri) ) (*ptri)++; */
      return FALSE;
    }
  return TRUE;
}

static int strincpy( char * ptro, char * ptri )
{
  int lens = 0;

  HB_TRACE(HB_TR_DEBUG, ("strincpy(%s, %s)", ptro, ptri));

  for( ; *ptri != ' ' && *ptri != ',' && *ptri != '[' && *ptri != ']' &&
         *ptri != '\1' && *ptri != '\0'; ptro++, ptri++, lens++ )
    *ptro = *ptri;
  return lens;
}

static int strotrim( char * stroka )
{
  char *ptr = stroka, lastc = '0', curc;
  int lens = 0, State = STATE_NORMAL;

  HB_TRACE(HB_TR_DEBUG, ("strotrim(%s)", stroka));

  while( ( curc = *stroka ) != '\0' )
    {
      if( State == STATE_QUOTE1 ) { if(curc == '\'') State = STATE_NORMAL; }
      else if( State == STATE_QUOTE2 ) { if(curc=='\"') State = STATE_NORMAL; }
      else
        {
          if( curc == '\'' ) State = STATE_QUOTE1;
          else if( curc == '\"' ) State = STATE_QUOTE2;
          else if( curc == '\t' ) curc = ' ';
        }
      if( State != STATE_NORMAL || curc != ' ' ||
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

static int NextWord( char ** sSource, char * sDest, BOOL lLower )
{
  int i = 0;

  HB_TRACE(HB_TR_DEBUG, ("NextWord(%p, %s, %d)", sSource, sDest, lLower));

  HB_SKIPTABSPACES( (*sSource) );
  while( **sSource != '\0' && **sSource != ' ' && **sSource != '\t' && **sSource != '(')
    {
      *sDest++ = (lLower)? tolower(**sSource):**sSource;
      (*sSource)++;
      i++;
    }
  *sDest = '\0';
  return i;
}

static int NextName( char ** sSource, char * sDest )
{
  int lenName = 0, State = STATE_NORMAL;

  HB_TRACE(HB_TR_DEBUG, ("NextName(%p, %s)", sSource, sDest));

  while( **sSource != '\0' && ( !ISNAME(**sSource) || State != STATE_NORMAL ) )
    {
      if( State == STATE_QUOTE1 )
        { if( **sSource == '\'' ) State = STATE_NORMAL; }
      else if( State == STATE_QUOTE2 )
        { if( **sSource == '\"' ) State = STATE_NORMAL; }
      else if( **sSource == '\"' ) State = STATE_QUOTE2;
      else if( **sSource == '\'' ) State = STATE_QUOTE1;

      (*sSource)++;
    }

  while( **sSource != '\0' && ISNAME(**sSource) )
    {
      *sDest++ = *(*sSource)++;
      lenName++;
    }
  *sDest = '\0';
  return lenName;
}

static int NextParm( char ** sSource, char * sDest )
{
  int lenName = 0, State = STATE_NORMAL, StBr = 0;

  HB_TRACE(HB_TR_DEBUG, ("NextParm(%p, %s)", sSource, sDest));

  HB_SKIPTABSPACES( (*sSource) );
  while( **sSource != '\0' )
    {
      if( State == STATE_QUOTE1 )
        {
          if( **sSource == '\'' ) State = STATE_NORMAL;
        }
      else if( State == STATE_QUOTE2 )
        {
          if( **sSource == '\"' ) State = STATE_NORMAL;
        }
      else if( **sSource == '\"' ) State = STATE_QUOTE2;
      else if( **sSource == '\'' ) State = STATE_QUOTE1;
      else if( **sSource == '(' ) StBr++;
      else if( **sSource == ')' || **sSource == ',' )
        {
          if( !StBr ) break;
          if( **sSource == ')' ) StBr--;
        }

      if( sDest != NULL ) *sDest++ = **sSource;
      (*sSource)++;
      lenName++;
    }

  if( sDest != NULL ) *sDest = '\0';
  return lenName;
}

static BOOL OpenInclude( char * szFileName, PATHNAMES * pSearch, PHB_FNAME pMainFileName, FILE ** fptr, BOOL bStandardOnly, char * szInclude )
{
  PHB_FNAME pFileName;

  HB_TRACE(HB_TR_DEBUG, ("OpenInclude(%s, %p, %p, %p, %d)", szFileName, pSearch, pMainFileName, fptr, (int) bStandardOnly));

  if( bStandardOnly )
    {
      *fptr = 0;
      szInclude[ 0 ] = '\0';
    }
  else
    {
      pFileName = hb_fsFNameSplit( szFileName );
      pFileName->szPath = pMainFileName->szPath;
      hb_fsFNameMerge( szInclude, pFileName );
      *fptr = fopen( szInclude, "r" );
      hb_xfree( pFileName );
    }

  if( !*fptr && pSearch )
    {
      pFileName = hb_fsFNameSplit( szFileName );
      pFileName->szName = szFileName;
      pFileName->szExtension = NULL;
      while( pSearch && !*fptr )
        {
          pFileName->szPath = pSearch->szPath;
          hb_fsFNameMerge( szInclude, pFileName );
          *fptr = fopen( szInclude, "r" );
          pSearch = pSearch->pNext;
        }
      hb_xfree( pFileName );
    }

  return ( *fptr ? TRUE : FALSE );
}

/* Size of abreviated pragma commands */
#define PRAGMAS_LEN       8

/* TODO:  Add support for:
          CompileModule /M
          QuietMode     /Q
          RequestLib    /R
          TempPath      /T
          StdHeader     /U
          CheckSyntax   /S
*/

static int ParsePragma( char * sLine )
{
   char pragma[ MAX_NAME ];

   HB_TRACE(HB_TR_DEBUG, ("ParsePragma(%s)", sLine));

   HB_SKIPTABSPACES( sLine );

   NextWord( &sLine, pragma, FALSE );

   if( HB_ISOPTSEP( pragma[ 0 ] ) )
   {
      switch( pragma[ 1 ] )
      {
         case 'a':
         case 'A':
            hb_comp_bAutoMemvarAssume = IsOnOffSwitch( pragma, hb_comp_bAutoMemvarAssume );
            DebugPragma( pragma, -1, hb_comp_bAutoMemvarAssume );
            break;

         case 'b':
         case 'B':
            hb_comp_bDebugInfo = IsOnOffSwitch( pragma, hb_comp_bDebugInfo );
            hb_comp_bLineNumbers = hb_comp_bDebugInfo;
            DebugPragma( pragma, -1, hb_comp_bDebugInfo );
            break;

         case 'e':
         case 'E':

            if( pragma[ 2 ] == 's' ||
                pragma[ 2 ] == 'S' )
            {
               switch( pragma[ 3 ] )
               {
                  case '\0':
                  case '0':
                     hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT;
                     break;

                  case '1':
                     hb_comp_iExitLevel = HB_EXITLEVEL_SETEXIT;
                     break;

                  case '2':
                     hb_comp_iExitLevel = HB_EXITLEVEL_DELTARGET;
                     break;

                  default:
                     hb_compGenError( hb_pp_szErrors, 'F', ERR_PRAGMA_BAD_VALUE, NULL, NULL );
               }
               DebugPragma( pragma, hb_comp_iExitLevel, FALSE );
            }

            break;

         case 'l':
         case 'L':
            hb_comp_bLineNumbers = IsOnOffSwitch( pragma, hb_comp_bLineNumbers );
            DebugPragma( pragma, -1, hb_comp_bLineNumbers );
            break;

         case 'n':
         case 'N':
            hb_comp_bStartProc = IsOnOffSwitch( pragma, hb_comp_bStartProc );
            DebugPragma( pragma, -1, hb_comp_bStartProc );
            break;

         case 'p':
         case 'P':
            hb_comp_bPPO = IsOnOffSwitch( pragma, hb_comp_bPPO );
            DebugPragma( pragma, -1, hb_comp_bPPO );
            break;

         case 'v':
         case 'V':
            hb_comp_bForceMemvars = IsOnOffSwitch( pragma, hb_comp_bForceMemvars );
            DebugPragma( pragma, -1, hb_comp_bForceMemvars );
            break;

         case 'w':
         case 'W':
            if( pragma[ 2 ] != '\0' )
            {
               /* Check for +/- */
               if( pragma[ strlen( pragma ) - 1 ] == '+' ||
                   pragma[ strlen( pragma ) - 1 ] == '-' )
                  hb_comp_iWarnings = IsOnOffSwitch( pragma, hb_comp_iWarnings != 0 ) ? 1 : 0;
               else
               {  
                  /* There is -w<0,1,2,3> probably */
                  hb_comp_iWarnings = pragma[ 2 ] - '0';
                  if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
                     hb_compGenError( hb_pp_szErrors, 'F', ERR_PRAGMA_BAD_VALUE, NULL, NULL );

                  DebugPragma( pragma, -1, hb_comp_iWarnings );
               }
            }
            break;

         case 'z':
         case 'Z':
            hb_comp_bShortCuts = IsOnOffSwitch( pragma, hb_comp_bShortCuts );
            DebugPragma( pragma, -1, hb_comp_bShortCuts );
            break;

         default:
            break;
      }
   }
   else
   {
      char * temp = hb_strupr( pragma );

      if( memcmp( pragma, "AUTOMEMVARS", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bAutoMemvarAssume = StringToBool( temp, hb_comp_bAutoMemvarAssume );
         DebugPragma( pragma, -1, hb_comp_bAutoMemvarAssume );
      }
      else if( memcmp( temp, "DEBUGINFO", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bDebugInfo = StringToBool( temp, hb_comp_bDebugInfo );
         hb_comp_bLineNumbers = hb_comp_bDebugInfo;
         DebugPragma( pragma, -1, hb_comp_bDebugInfo );
      }
      else if( memcmp( temp, "ENABLEWARNINGS", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToBool( temp, hb_comp_iWarnings != 0 ) ? 1 : 0;
         DebugPragma( pragma, hb_comp_iWarnings, FALSE );
      }
      else if( memcmp( temp, "EXITSEVERITY", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iExitLevel = StringToInt( temp, hb_comp_iExitLevel );
         if( hb_comp_iExitLevel < 0 || hb_comp_iExitLevel > 2 )
            hb_compGenError( hb_pp_szErrors, 'F', ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( pragma, hb_comp_iExitLevel, FALSE );
      }
      else if( memcmp( temp, "FORCEMEMVARS", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bForceMemvars = StringToBool( temp, hb_comp_bForceMemvars );
         DebugPragma( pragma, -1, hb_comp_bForceMemvars );
      }
      else if( memcmp( temp, "LINEINFO", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bLineNumbers = StringToBool( temp, hb_comp_bLineNumbers );
         DebugPragma( pragma, -1, hb_comp_bLineNumbers );
      }
      else if( memcmp( temp, "NOSTARTPROC", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bStartProc = StringToBool( temp, hb_comp_bStartProc );
         DebugPragma( pragma, hb_comp_bStartProc, FALSE );
      }
      else if( memcmp( temp, "PREPROCESSING", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bPPO = StringToBool( temp, hb_comp_bPPO );
         DebugPragma( pragma, -1, hb_comp_bPPO );
      }
      else if( memcmp( temp, "SHORTCUTTING", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_bShortCuts = StringToBool( temp, hb_comp_bShortCuts );
         DebugPragma( pragma, -1, hb_comp_bShortCuts );
      }
      else if( memcmp( temp, "WARNINGLEVEL", PRAGMAS_LEN ) == 0 )
      {
         hb_comp_iWarnings = StringToInt( temp, hb_comp_iWarnings );
         if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
            hb_compGenError( hb_pp_szErrors, 'F', ERR_PRAGMA_BAD_VALUE, NULL, NULL );
         DebugPragma( pragma, -1, hb_comp_iWarnings );
      }
      else if( memcmp( temp, "TRACEPRAGMAS", PRAGMAS_LEN ) == 0 )
      {
         s_bTracePragma = StringToBool( temp, s_bTracePragma );
         DebugPragma( pragma, -1, s_bTracePragma );
      }
   }

   return 0;
}

/* Checks for ON/OFF within the string, sets bDefault if not found */
static BOOL StringToBool( char * str, BOOL bDefault )
{
   char * pos;
   BOOL bRet = bDefault;

   pos = strchr( str, '=' );

   if( pos )
   {
      long lPos = pos - str + 1;

      if( str[ lPos ] == 'O' )
      {
         if( strlen( str ) >= 2 &&
             str[ lPos + 1 ] == 'N' )
            bRet = TRUE;
         else if( strlen( str ) >= 3 &&
                  str[ lPos + 1 ] == 'F' &&
                  str[ lPos + 2 ] == 'F' )
            bRet = FALSE;
      }
   }

   return bRet;
}

/* Checks for +/- within the string, sets bDefault if not found */
static BOOL IsOnOffSwitch( char * str, BOOL bDefault )
{
   BOOL bRet = bDefault;
   long lPos = strlen( str ) - 1;

   if( str[ lPos ] == '+' )
      bRet = TRUE;
   else if( str[ lPos ] == '-' )
      bRet = FALSE;

   return bRet;
}

/* Returns value after =, sets iDefault if not found */
static int StringToInt( char * str, int iDefault )
{
   char * pos;
   int iRet = iDefault;

   pos = strchr( str, '=' );

   if( pos )
   {
      long lPos = pos - str + 1;

      if( lPos && str[ lPos ] )
         iRet = str[ lPos ] - '0';
   }

   return iRet;
}

/* This is only to debug pragmas now */
static void DebugPragma( char * szStr, int iValue, BOOL bValue )
{
   if( s_bTracePragma )
   {
      char * ptr = strchr( szStr, '=' );
      char * temp = ( char * ) hb_xgrab( strlen( szStr ) + 1 );
      BOOL bIsSwitch = TRUE;

      * temp = '\0';

      /* strip =... from szStr. Just cosmetic */
      if( ptr )
      {
         int i = 0;

         while( szStr[ i ] != '=' )
            temp[ i ] = szStr[ i++ ];

         temp[ i ] = '\0';
         bIsSwitch = FALSE;
      }

      if( iValue > 0 )
         printf( "#pragma %s set to %i\n", bIsSwitch ? szStr : temp, iValue );
      else
         printf( "#pragma %s is %s\n", bIsSwitch ? szStr : temp, bValue ? "ON" : "OFF" );

      hb_xfree( temp );
   }
}
