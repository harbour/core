/* Harbour Preprocessor , version 0.8
   author - Alexander Kresin             */
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <mem.h>
#include <ctype.h>
#include "harb.h"

typedef struct
{
  char *name;
  char *pars;
  int npars;
  char *value;
} DEFINES;

typedef struct
{
  char *name;
  char *mpatt;
  char *value;
} COMMANDS;

int Hp_Parse( int, int );
int ParseDirective( char* );
int ParseDefine( char* );
DEFINES* AddDefine ( char* );
int ParseUndef( char* );
int ParseIfdef( char*, int);
int ParseCommand( char* );
int ConvertPatterns ( char*, int, char*, int );
void AddCommand ( char * );
COMMANDS* getCommand ( int );
int ParseExpression( char*, char* );
void WorkDefine ( char**, char**, DEFINES*);
int WorkCommand ( char*, char*, char**, int);
void CmdParse ( char *ptri, int aCmdStru[100][2] );
int WorkMarkers( char**, char*, char*, int*, int aCmdStru[100][2], int*, int );
void SkipOptional( char**, char*, int*);

DEFINES* DefSearch(char *);
int ComSearch(char *,int);
void SearnRep( char*,char*,int,char*,int*);
int ReplacePattern ( char, char*, int, char*, int );
int RdStr(int,char *,int,int,char*,int*,int*);
int WrStr(int,char *);
int hb_strAt(char *, int, char*, int);
int IsInStr ( char, char*);
void Stuff (char*, char*, int, int, int);
int strocpy (char*, char* );
int stroncpy (char*, char*, int);
int strocmp (char*, char**);
int strolen ( char* );
char* strodup ( char * );
int NextWord ( char**, char*, int);
int NextName ( char**, char*, char**);

#define isname(c)  (isalnum(c) || c=='_' || (c) > 0x7e)
#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++
#define MAX_NAME 255
#define BUFF_SIZE 2048
#define STR_SIZE 1024
#define FALSE               0
#define TRUE                1

#define STATE_INIT 0
#define STATE_NORMAL 1
#define STATE_COMMENT 2
#define STATE_QUOTE1 3
#define STATE_QUOTE2 4
#define STATE_ID_END 5
#define STATE_ID 6
#define STATE_EXPRES 7
#define STATE_BRACKET 8

#define IT_EXPR 1
#define IT_ID 2
#define IT_COMMA 3
#define IT_ID_OR_EXPR 4

int ParseState = 0;
int lInclude = 0;
int *aCondCompile, nCondCompile = 0, maxCondCompile = 5;
int nline=0;
int Repeate;
char groupchar;

extern DEFINES aDefines[] ;
extern int koldef;
DEFINES *aDefnew ;
int koldefines = 0, maxdefines = 50;

#define INITIAL_ACOM_SIZE 200
extern COMMANDS aCommands[] ;
extern int kolcomm;
COMMANDS *aCommnew ;
int kolcommands = 0, maxcommands = INITIAL_ACOM_SIZE;

int main (int argc,char* argv[])
{
int handl_i,handl_o,i;
char szFileName[ _POSIX_PATH_MAX ];
FILENAME *pFileName =NULL;

 if(argc<2) { printf("File name absent"); return 1; }
 pFileName =SplitFilename( argv[1] );
 if( !pFileName->extension )
   pFileName->extension =".prg";
 MakeFilename( szFileName, pFileName );

 if ((handl_i = open(szFileName, O_RDONLY | O_TEXT)) == -1)
 { printf("Can't open %s",szFileName); return 1; }

 pFileName->extension =".ppo";
 MakeFilename( szFileName, pFileName );
 if ((handl_o = open(szFileName, O_CREAT | O_WRONLY | O_TEXT)) == -1)
 { printf("Can't open %s",szFileName); return 1; }

 aCondCompile = (int*) _xgrab( sizeof(int) * 5 );
 aDefnew = ( DEFINES * ) _xgrab( sizeof(DEFINES) * 50 );
 aCommnew = ( COMMANDS * ) _xgrab( sizeof(COMMANDS) * INITIAL_ACOM_SIZE );

 Hp_Parse(handl_i,handl_o);
 close(handl_i); close(handl_o);
/*
 for (i=0;i<kolcommands;i++)
 {
  printf("\n\"%s\",",aCommands[i].name);
  if (aCommands[i].mpatt !=NULL)   printf("\"%s\",",aCommands[i].mpatt);
   else printf("NULL,");
  if (aCommands[i].value !=NULL)   printf("\n\"%s\",",aCommands[i].value);
   else printf("\nNULL,");
 }
*/
 return 0;
}

int Hp_Parse( int handl_i, int handl_o )
{
 char sBuffer[BUFF_SIZE];           /* File read buffer */
 int iBuffer = 10, lenBuffer = 10;
 char sLine[STR_SIZE], sOutLine[STR_SIZE], *ptr;
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
  }
  else { lContinue = 0; lens=0; }

  if ( *sLine != '\0' && !lContinue )
  {
   ptr = sLine;
   SKIPTABSPACES( ptr );
   if ( *ptr == '#' )
   {
    if ( (rezParse=ParseDirective( ptr+1 )) > 0 )
    {
     if ( !lInclude )
       printf ( "\nError number %u in line %u", rezParse, nline );
     return rezParse;
    }
    *sLine = '\0';
   }
   else
   {
    if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
    {
      if ( (rezParse = ParseExpression( ptr, sOutLine)) > 0 )
      {
       printf ( "\nError number %u in line %u", rezParse, nline );
       return rezParse;
      }
    }
    else *sLine = '\0';
   }
  }

  if(!lInclude)
  {
   if( lContinue ) WrStr(handl_o,"\0");  else WrStr(handl_o,sLine);
  }
 }
 return 0;
}

int ParseDirective( char* sLine )
{
 char sDirective[MAX_NAME];
 int i = 0;
 int handl_i;

 i = NextWord( &sLine, sDirective, TRUE );
 SKIPTABSPACES(sLine);

 if ( i == 4 && memcmp ( sDirective, "else", 4 ) == 0 )
 {     /* ---  #else  --- */
   if ( nCondCompile == 0 ) return 3001;
   else aCondCompile[nCondCompile-1] = 1 - aCondCompile[nCondCompile-1];
 }

 else if ( i == 5 && memcmp ( sDirective, "endif", 5 ) == 0 )
 {     /* --- #endif  --- */
   if ( nCondCompile == 0 ) return 3001; else nCondCompile--;
 }

 else if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
 {
  if ( i == 7 && memcmp ( sDirective, "include", 7 ) == 0 )
  {    /* --- #include --- */
   if ( *sLine != '\"' ) return 1000;
   sLine++; i = 0;
   while ( *(sLine+i) != '\0' && *(sLine+i) != '\"' ) i++;
   if ( *(sLine+i) != '\"' ) return 1000;
   *(sLine+i) = '\0';

   if ((handl_i = open(sLine, O_RDONLY | O_TEXT)) == -1)
    { printf("\nCan't open %s",sLine); return 1001; }

   lInclude++;
   Hp_Parse(handl_i, NULL);
   lInclude--;
   close(handl_i);
  }

  else if ( i == 6 && memcmp ( sDirective, "define", 6 ) == 0 )
   ParseDefine ( sLine );   /* --- #define  --- */

  else if ( i == 5 && memcmp ( sDirective, "undef", 5 ) == 0 )
   ParseUndef ( sLine );    /* --- #undef  --- */

  else if ( i == 5 && memcmp ( sDirective, "ifdef", 5 ) == 0 )
   ParseIfdef ( sLine, TRUE ); /* --- #ifdef  --- */

  else if ( i == 6 && memcmp ( sDirective, "ifndef", 6 ) == 0 )
   ParseIfdef ( sLine, FALSE ); /* --- #ifndef  --- */

  else if ( (i == 7 && memcmp ( sDirective, "command", 7 ) == 0) ||
            (i == 8 && memcmp ( sDirective, "xcommand", 8 ) == 0) )
   ParseCommand ( sLine ); /* --- #command  --- */

  else if ( (i == 9 && memcmp ( sDirective, "translate", 9 ) == 0) ||
            (i == 10 && memcmp ( sDirective, "xtranslate", 10 ) == 0) )
  {                        /* --- #translate  --- */
  }

  else if ( i == 6 && memcmp ( sDirective, "stdout", 6 ) == 0 )
   printf ( "%s", sLine ); /* --- #stdout  --- */

  else if ( i == 5 && memcmp ( sDirective, "error", 5 ) == 0 )
  {                        /* --- #error  --- */
   printf ( " #error: %s", sLine );
   return 2000;
  }
  else return 1;
 }
 return 0;
}

int ParseDefine( char* sLine)
{
 char defname[MAX_NAME], pars[MAX_NAME];
 int i = 0, npars = 0;
 DEFINES *lastdef;
     /* Drag identifier */
 while ( *sLine != '\0' && *sLine != ' ')
 {
  if ( *sLine == '(' ) break;  /* If pseudofunction */
  *(defname+i++) = *sLine++;
 }
 *(defname+i) = '\0';

 if ( *sLine == '(' ) /* If pseudofunction was found */
 {
  sLine++; i = 0;
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

 lastdef = AddDefine ( defname );

 lastdef->value = ( *sLine == '\0' )? NULL : strodup ( sLine );
 lastdef->npars = npars;
 lastdef->pars = ( npars == 0 )? NULL : strodup ( pars );

 return 0;
}

DEFINES* AddDefine ( char* defname )
{
 DEFINES* stdef = DefSearch( defname );

 if ( stdef != NULL )
 {
  if ( stdef->pars != NULL ) _xfree ( stdef->pars );
  _xfree ( stdef->value );
 }
 else
 {
  if ( koldefines == maxdefines )      /* Add new entry to defines table */
  {
   maxdefines += 50;
   aDefnew = (DEFINES *)_xrealloc( aDefnew, sizeof( DEFINES ) * maxdefines );
  }
  stdef = &aDefnew[koldefines++];
  stdef->name = strodup ( defname );
 }
 return stdef;
}

int ParseUndef( char* sLine)
{
 char defname[MAX_NAME];
 DEFINES* stdef;

 NextWord( &sLine, defname, FALSE );

 if ( ( stdef = DefSearch(defname) ) >= 0 )
 {
  _xfree ( stdef->name );
  _xfree ( stdef->pars );
  _xfree ( stdef->value );
  stdef->name = NULL;
/*
  for ( ; i < koldefines-1; i++ )
  {
   aDefines[i].name = aDefines[i+1].name;
   aDefines[i].value = aDefines[i+1].value;
   aDefines[i].pars = aDefines[i+1].pars;
   aDefines[i].npars = aDefines[i+1].npars;
  }
  koldefines--;
*/
 }
 return 0;
}

int ParseIfdef( char* sLine, int usl)
{
 char defname[MAX_NAME];
 DEFINES *stdef;

  NextWord( &sLine, defname, FALSE );
  if ( *defname == '\0' ) return 3000;
  if ( nCondCompile == maxCondCompile )
  {
    maxCondCompile += 5;
    aCondCompile = (int*)_xrealloc( aCondCompile, sizeof( int ) * maxCondCompile );
  }
  if ( ( (stdef = DefSearch(defname)) != NULL && usl )
       || ( stdef == NULL && !usl ) ) aCondCompile[nCondCompile] = 1;
  else aCondCompile[nCondCompile] = 0;
  nCondCompile++;
  return 0;
}

DEFINES* DefSearch(char *defname)
{
 int i,j;
 for ( i=koldefines-1; i>=0; i-- )
 {
  for ( j=0; *(aDefnew[i].name+j)==*(defname+j) &&
             *(aDefnew[i].name+j)!='\0'; j++ );
  if ( *(aDefnew[i].name+j)==*(defname+j) ) return &aDefnew[i];
 }

 for ( i=koldef-1; i>=0; i-- )
 {
  for ( j=0; *(aDefines[i].name+j)==*(defname+j) &&
             *(aDefines[i].name+j)!='\0'; j++ );
  if ( *(aDefines[i].name+j)==*(defname+j) ) return &aDefines[i];
 }
 return NULL;
}

int ComSearch(char *cmdname, int ncmd)
{
 int i,j;
 if ( !ncmd || ncmd > kolcomm )
  for ( i=(ncmd)? ncmd-kolcomm:kolcommands-1; i >= 0; i-- )
  {
   for ( j=0; *(aCommnew[i].name+j)==toupper(*(cmdname+j)) &&
             *(aCommnew[i].name+j)!='\0'; j++ );
   if ( *(aCommnew[i].name+j)==toupper(*(cmdname+j)) ) return kolcomm+i;
  }

 for ( i=(ncmd && ncmd<=kolcomm)? ncmd:kolcomm-1; i >= 0; i-- )
 {
  for ( j=0; *(aCommands[i].name+j)==toupper(*(cmdname+j)) &&
             *(aCommands[i].name+j)!='\0'; j++ );
  if ( *(aCommands[i].name+j)==toupper(*(cmdname+j)) ) break;
 }
 return i;
}

int ParseCommand( char* sLine)
{
 char cmdname[MAX_NAME];
 char mpatt[STR_SIZE], rpatt[STR_SIZE];
 int mlen,rlen;
 int ipos, rez;

 NextWord( &sLine, cmdname, FALSE );
 SKIPTABSPACES(sLine);

 if ( (ipos = hb_strAt( "=>", 2, sLine, strolen(sLine) )) > 0 )
   mlen = stroncpy( mpatt, sLine, ipos-1 );
 else return 4000;  /* Quit, if '=>' absent */

 sLine += ipos + 1;
 SKIPTABSPACES(sLine);
 rlen = strocpy( rpatt, sLine );

 if ( (rez = ConvertPatterns ( mpatt, mlen, rpatt, rlen )) > 0 ) return rez;

 AddCommand ( cmdname );
 aCommnew[kolcommands-1].mpatt = strodup ( mpatt );
 aCommnew[kolcommands-1].value = ( rlen > 0 )? strodup ( rpatt ) : NULL;

 return 0;
}

int ConvertPatterns ( char *mpatt, int mlen, char *rpatt, int rlen )
{
 int i = 0, ipos, ifou;
 int explen,rmlen;
 char exppatt[MAX_NAME], expreal[4] = "\1  0";
 char lastchar = '@', exptype;
 char *ptr;

 while ( *(mpatt+i) != '\0' )
 {
  if ( *(mpatt+i) == '<' )
  {  /* Drag match marker, determine it type */
   explen = 0; ipos = i; i++; exptype = '0';
   if ( *(mpatt+i) == '*' ) { exptype = '3'; i++; }
   else if ( *(mpatt+i) == '(' ) { exptype = '4'; i++; }
   while ( *(mpatt+i) != '>' )
   {
    if ( *(mpatt+i) == ',' )
    {
     exptype = '1';
     while ( *(mpatt+i) != '>' ) i++;
     break;
    }
    else if ( *(mpatt+i) == ':' ) { exptype = '2'; break; }
    *(exppatt+explen++) = *(mpatt+i++);
   }
   if ( exptype == '3' )
    { if ( *(exppatt+explen-1) == '*' ) explen--; else return 4001; }
   else if ( exptype == '4' )
    { if ( *(exppatt+explen-1) == ')' ) explen--; else return 4001; }
   rmlen = i - ipos + 1;
     /* Replace match marker with new marker */
   lastchar = (char) ( (unsigned int)lastchar + 1 );
   expreal[1] = lastchar;
   expreal[2] = exptype;
   Stuff ( expreal, mpatt+ipos, 4, rmlen, mlen );
   mlen += 4 - rmlen; i += 4 - rmlen;

   ptr = rpatt;
   while ( (ifou = hb_strAt( exppatt, explen, ptr, rlen-(ptr-rpatt) )) > 0 )
   {
    ptr += ifou;
    if ( *(ptr-2) == '<' && *(ptr+explen-1) == '>' )
    {
     if ( *(ptr-3) == '#' ) { exptype = '1'; ptr -= 3; rmlen = explen+3; }
     else { exptype = '0'; ptr -= 2; rmlen = explen+2; }
    }
    else if ( *(ptr-3) == '<' && *(ptr+explen) == '>' )
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
    Stuff ( expreal, ptr, 4, rmlen, rlen );
    rlen += 4 - rmlen;
   }
  }
  i++;
 }
 return 0;
}

void AddCommand ( char *cmdname )
{
 if ( kolcommands == maxcommands )
 {
  maxcommands += 50;
  aCommnew = (COMMANDS *)_xrealloc( aCommnew, sizeof( COMMANDS ) * maxcommands );
 }
 aCommnew[kolcommands].name = strodup ( cmdname );
 kolcommands++;
}

COMMANDS* getCommand ( int ndef )
{
 return (ndef>=kolcomm)? &(aCommnew[ndef-kolcomm]):&(aCommands[ndef]);
}

int ParseExpression( char* sLine, char* sOutLine )
{
 char sToken[MAX_NAME];
 char *ptri, *ptro;
 int lenToken,npars,i,ndef;
 int rezDef, rezCom, kolpass = 0;
 int kolused = 0, lastused;
 DEFINES *aUsed[100], *stdef;

 do
 {
  ptri = sLine; ptro = sOutLine;
  rezDef = 0; rezCom = 0;
  lastused = kolused;
   /* Look for macros from #define      */
  while ( ( lenToken = NextName(&ptri, sToken, &ptro) ) > 0 )
   if ( (stdef=DefSearch(sToken)) != NULL )
   {
    for(i=0;i<kolused;i++) if ( aUsed[i] == stdef ) break;
    if ( i < kolused ) { if ( i < lastused ) return 1000; }
    else
     aUsed[kolused++] = stdef;
    if ( stdef->pars == NULL )
    {
     rezDef = 1;
     ptro -= lenToken;
     lenToken = 0;
     while ( *(stdef->value+lenToken) != '\0' )
      *ptro++ = *(stdef->value+lenToken++);
    }
    else
    {
     SKIPTABSPACES( ptri );
     if ( *ptri == '(' )
     {
      npars=0; i = 0;
      while ( *(ptri+i) != ')' && *(ptri+i) != '\0' )
      {
       if ( *(ptri+i) == ',' ) npars++;
       i++;
      }
      if ( stdef->npars == npars + 1 )
      {
       rezDef = 1;
       ptro -= lenToken;
       WorkDefine( &ptri, &ptro, stdef );
      }
     }
     else *ptro++ = ' ';
    }
   }
  *ptro = '\0';
  memcpy ( sLine, sOutLine, ptro - sOutLine + 1);

  /* Look for definitions from #command      */
  if ( !kolpass )
  {
   ptri = sLine; ptro = sOutLine;
//   lenToken = NextWord( &ptri, sToken, FALSE);
   SKIPTABSPACES( ptri );
   if ( isname(*ptri) ) lenToken = NextName( &ptri, sToken, NULL);
   else { *sToken = *ptri++; *(sToken+1) = '\0'; lenToken = 1; }
   if ( (ndef=ComSearch(sToken,0)) >= 0 )
   {
    if ( (i = WorkCommand( sToken, ptri, &ptro, ndef )) > 0 )
     memcpy ( sLine, sOutLine, i+1);
    rezCom = 1;
   }
  }
  kolpass++;
 }
 while ( rezDef || rezCom );

 return 0;
}

void WorkDefine ( char** ptri, char** ptro, DEFINES *stdef )
{
 char parfict[MAX_NAME], parreal[MAX_NAME];
 char *ptrb;
 int ipos = 0, ifou, ibeg;
 int lenfict, lenreal, lenres;

 while ( *(stdef->value+ipos) != '\0' ) /* Copying value of macro */
 {                                              /* to destination string  */
  *(*ptro+ipos) = *(stdef->value+ipos);
  ipos++;
 }
 *(*ptro+ipos) = '\0';
 lenres = ipos;

 ipos = 0; ibeg = 0;
 do                               /* Parsing through parameters */
 {                                /* in macro definition        */
  if ( *(stdef->pars+ipos)==',' || *(stdef->pars+ipos)=='\0' )
  {
   *(parfict+ipos-ibeg) = '\0';
   lenfict = ipos - ibeg;

   if ( **ptri != ')' )
   {
    (*ptri)++; lenreal = 0;           /* Parsing through real parameters */
    while ( **ptri != ',' && **ptri != ')' )
    {
     *(parreal+lenreal++) = **ptri;
     (*ptri)++;
    }
    *(parreal+lenreal) = '\0';

    ptrb = *ptro;
    while ( (ifou = hb_strAt( parfict, lenfict, ptrb, lenres-(ptrb-*ptro) )) > 0 )
    {
     ptrb = ptrb+ifou-1;
     if ( !isname(*(ptrb-1)) && !isname(*(ptrb+lenfict)) )
     {
       Stuff ( parreal, ptrb, lenreal, lenfict, lenres );
       lenres += lenreal - lenfict;
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
 (*ptri)++;
 *ptro += lenres;
}

int WorkCommand ( char* sToken, char* ptri, char** ptro, int ndef )
{
 int aCmdStru[100][2], iItem;
 int rez, nbr;
 int lenres = 0;
 char *lastopti[2];
 char *ptrmp;

 /* Copying result pattern to destination string  */
 lenres = strocpy ( *ptro, getCommand(ndef)->value );

 CmdParse ( ptri, aCmdStru );     /* Parse input string */
 ptrmp = getCommand(ndef)->mpatt;    /* Pointer to a match pattern */
 do
 {
  Repeate = 0;
  groupchar = '@';
  rez = 1; nbr = 0;
  if ( ptrmp == NULL ) rez = ( aCmdStru[0][0] )? 0:1;
  else
   for ( iItem=1; iItem <= aCmdStru[0][0] && rez; iItem++)
   {
     SKIPTABSPACES( ptrmp );
     switch ( *ptrmp ) {
      case '[':
       iItem--;
       nbr++; ptrmp++; lastopti[Repeate] = ptrmp;
       break;
      case ']':
        iItem--;
        if ( Repeate ) { Repeate--; ptrmp = lastopti[Repeate]; }
        else { nbr--; ptrmp++; }
        break;
      case ',':
       if ( aCmdStru[iItem][1] == IT_COMMA ) ptrmp++;
       else
       {
        if ( nbr ) { SkipOptional( &ptrmp, *ptro, &lenres); iItem--; }
        else rez = 0;
       }
       break;
      case '\1':  /*  Match marker */
       rez = WorkMarkers( &ptrmp, ptri, *ptro, &lenres, aCmdStru, &iItem, nbr );
       break;
      case '\0':
       rez = 0;
       break;
      default:    /*   Key word    */
       if ( aCmdStru[iItem][1] == IT_COMMA ||
           strocmp(ptri + aCmdStru[iItem][0], &ptrmp ) )
       {
        if ( nbr ) { SkipOptional( &ptrmp, *ptro, &lenres); iItem--; }
        else rez = 0;
       }
     }
   }

  if ( rez && ptrmp != NULL)
  {
   SKIPTABSPACES( ptrmp );
   if ( *ptrmp == ']' ) { ptrmp++; nbr--; }
   if ( Repeate ) { Repeate = 0; ptrmp = lastopti[0] - 1; }
   do
   {
    SKIPTABSPACES( ptrmp );
    if ( *ptrmp != '\0' )
     switch ( *ptrmp ) {
      case '[':
       ptrmp++;
       SkipOptional( &ptrmp, *ptro, &lenres);
       ptrmp++;
       break;
      case ']': ptrmp++; break;
      default:
       rez = 0;
     }
   }
   while ( *ptrmp != '\0' && rez );
  }

  if ( !rez && (ndef = ComSearch(sToken,ndef-1))>=0 )
  {
   ptrmp = getCommand(ndef)->mpatt; /* Pointer to a match pattern */
               /* Copying result pattern to destination string  */
   lenres = strocpy ( *ptro, getCommand(ndef)->value );
  }
 }
 while ( !rez && ndef >= 0 );

 *(*ptro+lenres) = '\0';
 if ( rez ) return lenres;
 return 0;
}

void CmdParse ( char *ptri, int aCmdStru[100][2] )
{
 char *sZnaki = "+-=><*/$";
 int State = State = STATE_ID_END, StBr1 = 0, StBr2 = 0, StBr3 = 0;
 int i = 0;

 aCmdStru[0][0] = 0;
 while ( *ptri != '\0' )
 {
  switch ( State ) {
    case STATE_QUOTE1:
     if(*ptri=='\'')
      State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
     break;
    case STATE_QUOTE2:
     if(*ptri=='\"')
      State = (StBr1==0 && StBr2==0 && StBr3==0)? STATE_ID_END:STATE_BRACKET;
     break;
    case STATE_BRACKET:
     if ( *ptri == '\'' ) State = STATE_QUOTE1;
     else if ( *ptri == '\"' ) State = STATE_QUOTE2;
     else if ( *ptri == '(' ) StBr1++;
     else if ( *ptri == '[' ) StBr2++;
     else if ( *ptri == '{' ) StBr3++;
     else if ( *ptri == ')' )
     { StBr1--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     else if ( *ptri == ']' )
     { StBr2--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     else if ( *ptri == '}' )
     { StBr3--; if (StBr1==0 && StBr2==0 && StBr3==0) State = STATE_ID_END; }
     break;
    case STATE_ID:
    case STATE_ID_END:
     if ( ( (isname(*ptri) || *ptri=='\\') && State == STATE_ID_END ) ||
          *ptri==',' || *ptri=='\'' || *ptri=='\"')
     {
      if ( !aCmdStru[ aCmdStru[0][0] ][1] ) aCmdStru[ aCmdStru[0][0] ][1] = IT_ID;
      State = STATE_ID;
      aCmdStru[0][0]++;
      aCmdStru[ aCmdStru[0][0] ][0] = i;
      if ( *ptri == ',' )
      {
       State = STATE_ID_END;
       aCmdStru[ aCmdStru[0][0] ][1] = IT_COMMA;
      }
      else if ( *ptri == '\'' )
      {
       State = STATE_QUOTE1;
       aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
      }
      else if ( *ptri == '\"' )
      {
       State = STATE_QUOTE2;
       aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
      }
      else
      {
       State = STATE_ID;
       if ( isdigit(*ptri) ) aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
       else aCmdStru[ aCmdStru[0][0] ][1] = 0;
      }
     }
     else if ( IsInStr( *ptri, sZnaki ) ) State = STATE_EXPRES;
     else if ( *ptri == '(' )
     {
      State = STATE_BRACKET;
      StBr1 = 1;
      aCmdStru[ aCmdStru[0][0] ][1] = IT_ID_OR_EXPR;
      aCmdStru[0][0]++;
      aCmdStru[ aCmdStru[0][0] ][0] = i;
      aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
     }
     else if ( *ptri == '[' )
     {
      State = STATE_BRACKET;
      StBr2 = 1;
      aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
     }
     else if ( *ptri == '{' )
     {
      State = STATE_BRACKET;
      StBr3 = 1;
      aCmdStru[ aCmdStru[0][0] ][1] = IT_ID;
      aCmdStru[0][0]++;
      aCmdStru[ aCmdStru[0][0] ][0] = i;
      aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
     }
     else if ( *ptri == ' ' ) State = STATE_ID_END;
     break;
    case STATE_EXPRES:
     aCmdStru[ aCmdStru[0][0] ][1] = IT_EXPR;
     if ( *ptri == '\'' ) State = STATE_QUOTE1;
     else if ( *ptri == '\"' ) State = STATE_QUOTE2;
     else if ( isname(*ptri) ) State = STATE_ID;
     else if ( *ptri == '(' ) { StBr1++; State = STATE_BRACKET; }
     else if ( *ptri == '[' ) { StBr2++; State = STATE_BRACKET; }
     else if ( *ptri == '{' ) { StBr3++; State = STATE_BRACKET; }
     break;
  }
  ptri++; i++;
 }
 if ( aCmdStru[0][0] && !aCmdStru[ aCmdStru[0][0] ][1] )
     aCmdStru[ aCmdStru[0][0] ][1] = IT_ID;
 aCmdStru[ aCmdStru[0][0]+1 ][0] = i;
}

int WorkMarkers( char **ptrmp, char *ptri, char *ptro, int *lenres, int aCmdStru[100][2], int *iItem, int nbr )
{
 int ifiItem;
 char expreal[MAX_NAME], exppatt[MAX_NAME];
 int lenreal, lenpatt;
 int rezrestr,i;
 char *ptr;

  if ( aCmdStru[*iItem][1] == IT_COMMA )  return 0;
         /* Copying a match pattern to 'exppatt' */
  lenpatt = stroncpy ( exppatt, *ptrmp, 4 );
  *ptrmp += 4;

  ifiItem = *iItem;
  if ( aCmdStru[ifiItem][1] == IT_ID_OR_EXPR )  (*iItem)++;
  if ( *(exppatt+2) == '4' )       /*  ----  extended match marker  */
  {
        /* Copying a real expression to 'expreal' */
    lenreal = stroncpy ( expreal, ptri + aCmdStru[ifiItem][0],
                              aCmdStru[*iItem+1][0]-aCmdStru[ifiItem][0] );
    SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else if ( *(exppatt+2) == '3' )  /*  ----  wild match marker  */
  {
    *iItem = aCmdStru[0][0];
        /* Copying a real expression to 'expreal' */
    lenreal = stroncpy ( expreal, ptri + aCmdStru[ifiItem][0],
                         aCmdStru[*iItem+1][0]-aCmdStru[ifiItem][0] );
    SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else if ( *(exppatt+2) == '2' )  /*  ---- restricted match marker  */
  {
   while ( **ptrmp != '>' && **ptrmp!= '\0' ) *(exppatt+lenpatt++) = *((*ptrmp)++);
   *(exppatt+lenpatt++) = '>';
   *(exppatt+lenpatt) = '\0';
   (*ptrmp)++;
               /* Copying a real expression to 'expreal' */
   lenreal = stroncpy ( expreal, ptri + aCmdStru[ifiItem][0],
                           aCmdStru[*iItem+1][0]-aCmdStru[ifiItem][0] );

   ptr = exppatt + 4;
   rezrestr = 0;
   while ( *ptr != '>' )
   {
     if ( *ptr == '&' )
     {
       if ( *expreal == '&' )
       {
         rezrestr = 1;
         break;
       }
       else ptr++;
     }
     else
     {
       SKIPTABSPACES( ptr );
           /* Comparing real parameter and restriction value */
       for ( i=0; toupper(*ptr) == toupper(*(expreal+i)) && *ptr != ','
                && *ptr != '>' && i < lenreal; i++,ptr++ );
       if ( i == lenreal || *ptr == ',' || *ptr == '>' )
       {
         SearnRep( exppatt,expreal,lenreal,ptro,lenres);
         rezrestr = 1;
         break;
       }
       else
       {
          while ( *ptr != ',' && *ptr != '>' ) ptr++;
          if ( *ptr == ',' ) ptr++;
       }
     }
   }
   if ( rezrestr == 0 )
   {  /* If restricted match marker doesn't correspond to real parameter */
      if ( nbr )
      {
        SearnRep( exppatt,"",0,ptro,lenres);
        (*iItem)--;      /* if was optional, go to the next */
      }
      else return 0;
   }
  }
  else if ( *(exppatt+2) == '1' )  /*  ---- list match marker  */
  {
     while ( *iItem < aCmdStru[0][0] && aCmdStru[*iItem+1][1] == IT_COMMA ) *iItem+=2;
        /* Copying a real expression to 'expreal' */
     lenreal = stroncpy ( expreal, ptri + aCmdStru[ifiItem][0],
                             aCmdStru[*iItem+1][0]-aCmdStru[ifiItem][0] );
     SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  else                             /*  ---- regular match marker  */
  {
               /* Copying a real expression to 'expreal' */
     lenreal = stroncpy ( expreal, ptri + aCmdStru[ifiItem][0],
                          aCmdStru[*iItem+1][0]-aCmdStru[ifiItem][0] );
     SearnRep( exppatt,expreal,lenreal,ptro,lenres);
  }
  return 1;
}

void SkipOptional( char** ptri, char *ptro, int* lenres)
{
 int nbr = 0;
 char exppatt[MAX_NAME];
 int lenpatt;

 while ( isname(**ptri) ) (*ptri)++;
 SKIPTABSPACES( *ptri );
 while ( **ptri != ']' || nbr )
 {
  switch ( **ptri ) {
   case '[':  nbr++; break;
   case ']':  nbr--; break;
   case '\1':
     for ( lenpatt=0; lenpatt<4; lenpatt++ ) *(exppatt+lenpatt) = *((*ptri)++);
     (*ptri)--;
     if ( exppatt[2] == '2' )
     while ( **ptri != '>' ) (*ptri)++;
     SearnRep( exppatt,"",0,ptro,lenres);
     break;
  }
  (*ptri)++;
 }
}

void SearnRep( char *exppatt,char *expreal,int lenreal,char *ptro, int *lenres)
{
 int ifou, isdvig = 0, rezs;
 int kolmarkers;
 int lennew, i;
 char lastchar = '0';
 char expnew[MAX_NAME];
 char *ptr, *ptr2;
   while ( (ifou = hb_strAt( exppatt, 2, ptro+isdvig, *lenres-isdvig )) > 0 )
   {
    rezs = 0;
    ptr = ptro+isdvig+ifou-2;
    kolmarkers = 0;
    while ( ptr >= ptro+isdvig )
    {
     if ( *ptr == '[' || *ptr == ']' ) break;
     if  ( *ptr == '\1' ) kolmarkers++;
     ptr--;
    }
    if ( *ptr == '[' )
    {
     ptr2 = ptro+isdvig+ifou+3;
     while ( *ptr2 != ']' ) { if ( *ptr2 == '\1' ) kolmarkers++; ptr2++; }
     if ( Repeate && lenreal && kolmarkers) return;
     if ( lenreal == 0 )
     {
      Stuff ( "", ptr, 0, ptr2-ptr+1, *lenres-(ptr-ptro) );
      *lenres -= ptr2-ptr+1;
      isdvig = ptr - ptro;
      rezs = 1;
     }
     else
     {
      lennew = ptr2-ptr-1;

      memcpy ( expnew, ptr+1, lennew );
      *(expnew + lennew) = '\0';
      while ( (i = hb_strAt( exppatt, 2, expnew, lennew )) > 0 )
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
      Stuff ( expnew, ptr, lennew, 0, *lenres-(ptr-ptro)+1 );
      *lenres += lennew;
      isdvig = ptr - ptro + (ptr2-ptr-1) + lennew;
      rezs = 1;
      Repeate++;
     }
    }
    if ( !rezs )
    {
     if ( *(ptro+isdvig+ifou+2) != '0' )
     {
      if ( lastchar == '0' ) lastchar = *(ptro+isdvig+ifou+2);
      if ( lastchar != *(ptro+isdvig+ifou+2) )
       { isdvig += ifou + 3; continue; }
     }
     *lenres += ReplacePattern ( exppatt[2], expreal, lenreal,
                             ptro+isdvig+ifou-1, *lenres-isdvig-ifou+1 );
     isdvig += ifou;
    }
   }
}

int ReplacePattern ( char patttype, char *expreal, int lenreal, char *ptro, int lenres )
{
 int rmlen = lenreal;

     switch ( *(ptro+2) ) {
      case '0':  /* Regular result marker  */
         Stuff ( expreal, ptro, lenreal, 4, lenres );
         break;
      case '1':  /* Dumb stringify result marker  */
         Stuff ( "\"\"", ptro, 2, 4, lenres );
         if ( lenreal )
          Stuff ( expreal, ptro+1, lenreal, 0, lenres );
         rmlen = lenreal + 2;
         break;
      case '2':  /* Normal stringify result marker  */
         if ( !lenreal )
          Stuff ( "", ptro, 0, 4, lenres );
         else if ( patttype == '1' )          /* list match marker */
         {
         }
         else
         {
          Stuff ( "\"\"", ptro, 2, 4, lenres );
          Stuff ( expreal, ptro+1, lenreal, 0, lenres );
          rmlen = lenreal + 2;
         }
         break;
      case '3':  /* Smart stringify result marker  */
         if ( patttype == '1' )          /* list match marker */
         {
         }
         else if ( !lenreal || *expreal == '(' )
           Stuff ( expreal, ptro, lenreal, 4, lenres );
         else
         {
          Stuff ( "\"\"", ptro, 2, 4, lenres );
          Stuff ( expreal, ptro+1, lenreal, 0, lenres );
          rmlen = lenreal + 2;
         }
         break;
      case '4':  /* Blockify result marker  */
         if ( !lenreal )
           Stuff ( expreal, ptro, lenreal, 4, lenres );
         else if ( patttype == '1' )          /* list match marker */
         {
         }
         else
         {
          Stuff ( "{||}", ptro, 4, 4, lenres );
          Stuff ( expreal, ptro+3, lenreal, 0, lenres );
          rmlen = lenreal + 4;
         }
         break;
      case '5':  /* Logify result marker  */
         rmlen = 3;
         if ( !lenreal )
         {
          Stuff ( ".F.", ptro, 3, 4, lenres );
         }
         else
          Stuff ( ".T.", ptro, 3, 4, lenres );
         break;
     }
     return rmlen - 4;
}

int RdStr(int handl_i,char *buffer,int maxlen,int lDropSpaces,char* sBuffer, int* lenBuffer, int* iBuffer)
{
int readed = 0;
int State = 0;
char cha,cLast='\0';
  if ( *lenBuffer == 0 ) return -1;
  while(1)
  {
   if ( *iBuffer == *lenBuffer )
   {
    if ( (*lenBuffer = read(handl_i,sBuffer,BUFF_SIZE)) < 1 ) sBuffer[0] = '\n';
    *iBuffer = 0;
   }
   cha = sBuffer[*iBuffer];
   (*iBuffer)++;
   if( cha == '\n' )  break;
   switch ( ParseState ) {
     case STATE_COMMENT:
      if ( cha == '/' && cLast == '*' )
       { ParseState = STATE_NORMAL; cha = ' '; }
      cLast = cha;
      break;
     case STATE_QUOTE1: if(cha=='\'') ParseState = STATE_NORMAL; break;
     case STATE_QUOTE2: if(cha=='\"') ParseState = STATE_NORMAL; break;
     default:
      switch ( cha ) {
       case '\"': ParseState = STATE_QUOTE2; break;
       case '\'': ParseState = STATE_QUOTE1; break;
       case '&': if ( readed>0 && buffer[readed-1] == '&' ) { maxlen = 0; readed--; } break;
       case '/': if ( readed>0 && buffer[readed-1] == '/' ) { maxlen = 0; readed--; } break;
       case 'e':
       case 'E':
        if ( !State &&
             readed>2 && (buffer[readed-1]=='t' || buffer[readed-1]=='T')
                      && (buffer[readed-2]=='o' || buffer[readed-2]=='O')
                      && (buffer[readed-3]=='n' || buffer[readed-2]=='N') )
         maxlen = readed = 0;
         break;
       case '*':
        if ( readed > 0 && buffer[readed-1] == '/' )
         { ParseState = STATE_COMMENT; readed--; }
        else if ( !State ) maxlen = readed = 0;
        break;
      }
   }
   if ( cha != ' ' && cha != '\t' ) State = 1;
   if( lDropSpaces && State ) lDropSpaces = 0;
   if(readed<maxlen && (!lDropSpaces || readed==0) &&
      ParseState != STATE_COMMENT) buffer[readed++]=cha;
  }
  while(--readed >= 0 && ( buffer[readed] == ' ' || buffer[readed] == '\t') );
  readed++;
  buffer[readed]='\0';
  return readed;
}

int WrStr(int handl_o,char *buffer)
{
 while( *buffer != '\0' ) write(handl_o,buffer++,1);
 write(handl_o,"\n",1);
 return 0;
}

/* locates a substring in a string */
int hb_strAt(char *szSub, int lSubLen, char *szText, int lLen)
{
   if( lSubLen )
   {
      if( lLen > lSubLen )
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

int IsInStr ( char symb, char* s )
{
 while ( *s != '\0' ) if ( *s++ == symb ) return 1;
 return 0;
}

void Stuff (char *ptri, char * ptro, int len1, int len2, int lenres )
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

int strocmp (char* ptro, char** ptri )
{
 for ( ; isname(**ptri) && isname(*ptro) && toupper(**ptri)==toupper(*ptro);
     ptro++, (*ptri)++ );
 if ( toupper(**ptri) != toupper(*ptro) ) return 1;
 return 0;
}

char* strodup ( char *stroka )
{
 char *ptr;
 int lens = 0;
  while ( *(stroka+lens) != '\0' ) lens++;
  ptr = ( char * ) _xgrab( lens + 1 );
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

int NextWord ( char** sSource, char* sDest, int lLower )
{
 int i = 0;
 SKIPTABSPACES( (*sSource) );
 while ( **sSource != '\0' && **sSource != ' ')
 {
   *sDest++ = (lLower)? tolower(**sSource):**sSource;
   (*sSource)++;
   i++;
 }
 *sDest = '\0';
 return i;
}

int NextName ( char** sSource, char* sDest, char **sOut )
{
 int i = 0;
 while ( **sSource != '\0' && !isname(**sSource) )
  { if ( sOut !=NULL ) *(*sOut)++ = **sSource; (*sSource)++; }

 while ( **sSource != '\0' && isname(**sSource) )
  {
   if ( sOut !=NULL ) *(*sOut)++ = **sSource;
   *sDest++ = *(*sSource)++; i++;
  }
 *sDest = '\0';
 return i;
}
