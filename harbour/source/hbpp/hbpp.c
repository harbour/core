/* Harbour Preprocessor , version 0.7
   author - Alexander Kresin             */
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <mem.h>
#include <ctype.h>
#include "harb.h"

int Hp_Parse( int, int );
int ParseDirective( char* );
int ParseDefine( char* );
int ParseUndef( char* );
int ParseIfdef( char*, int);
int ParseCommand( char* );
int ParseExpression( char*, char* );
void WorkDefine ( char**, char**, int);
int WorkCommand ( char*, char**, char**, int);
void CmdParse ( char *ptri, int aCmdStru[100][2] );
void SkipOptional( char**, char*, int*);

int DefSearch(char *);
int ComSearch(char *,int);
void SearnRep( char*,int,char*,int,char*,int*);
int RdStr(int,char *,int,int,char*,int*,int*);
int WrStr(int,char *);
int hb_strAt(char *, int, char*, int);
int IsInStr ( char, char*);
void Stuff (char*, char*, int, int, int);
void Stringify( char *, int);
void pEnclose( char *, int, char);
int NextWord ( char**, char*);
int NextName ( char**, char*, char**);

#define isname(c)  (isalnum(c) || c=='_' || (c) > 0x7e)
#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++
#define MAX_NAME 255
#define BUFF_SIZE 2048

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

int ParseState = 0;
int lInclude = 0;
int *aCondCompile, nCondCompile = 0, maxCondCompile = 5;
int nline=0;
int Repeate;

typedef struct
{
  char *name;
  char *pars;
  int npars;
  char *value;
} DEFINES;

DEFINES *aDefines ;
int koldefines = 0, maxdefines = 50;

typedef struct
{
  char *name;
  char *mpatt;
  int npars;
  char *value;
} COMMANDS;

#define INITIAL_ACOM_SIZE 250
COMMANDS *aCommands ;
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
 aDefines = ( DEFINES * ) _xgrab( sizeof(DEFINES) * 50 );
 aCommands = ( COMMANDS * ) _xgrab( sizeof(COMMANDS) * INITIAL_ACOM_SIZE );

 Hp_Parse(handl_i,handl_o);
 close(handl_i); close(handl_o);
/*
 for (i=0;i<kolcommands;i++)
 {
  printf("\n%s",aCommands[i].name);
  if (aCommands[i].mpatt !=NULL)   printf(" /%s/",aCommands[i].mpatt);
  if (aCommands[i].value !=NULL)   printf(" /%s/",aCommands[i].value);
 }
*/
 return 0;
}

int Hp_Parse( int handl_i, int handl_o )
{
 char sBuffer[BUFF_SIZE];
 int iBuffer = 10, lenBuffer = 10;
 char sLine[1024], sOutLine[1024];
 int lChanged;
 int lContinue = 0;
 int lDirective;
 int lens=0;
 int i;
 int rezParse;

 while ( (i=RdStr(handl_i,sLine+lens, ((lDirective)? 1024:256)-lens,lContinue,sBuffer,&lenBuffer,&iBuffer ))>=0 )
 {
  if ( !lInclude ) nline++;
  lens+=i;
  lChanged = 0;

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
   i = 0;
   while ( *(sLine+i) == ' ' || *(sLine+i) == '\t' ) i++;
   if ( *(sLine+i) == '#' )
   {
    lDirective = 1;
    if ( (rezParse=ParseDirective(sLine+i+1)) > 0 )
    {
     if ( !lInclude )
       printf ( "\nError number %u in line %u", rezParse, nline );
     return rezParse;
    }
   }
   else
   {
    lDirective = 0;
    if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
    {
      if ( (rezParse = ParseExpression( sLine+i,sOutLine)) > 0 )
      {
       printf ( "\nError number %u in line %u", rezParse, nline );
       return rezParse;
      }
    }
    else *sLine = '\0';
   }
  }

  if(!lInclude)
   if(lDirective || lContinue) WrStr(handl_o,"\0");
   else if(!lChanged) WrStr(handl_o,sLine);
 }
 return 0;
}

int ParseDirective( char* sLine )
{
 char sDirective[11];
 int i = 0;
 int handl_i;

 while ( *sLine != '\0' && (*sLine == ' ' || *sLine == '\t') ) sLine++;
 while ( *sLine != '\0' && *sLine != ' ' && *sLine != '\t' && i<10 )
  *(sDirective+i++) = tolower(*sLine++);
 *(sDirective+i) = '\0';
 SKIPTABSPACES(sLine);

 if ( i == 4 && memcmp ( sDirective, "else", 4 ) == 0 )
 {
  if ( nCondCompile == 0 ) return 3001;
  else aCondCompile[nCondCompile-1] = 1 - aCondCompile[nCondCompile-1];
 }
 else if ( i == 5 && memcmp ( sDirective, "endif", 5 ) == 0 )
  { if ( nCondCompile == 0 ) return 3001; else nCondCompile--; }
 else if ( nCondCompile==0 || aCondCompile[nCondCompile-1])
 {
  if ( i == 7 && memcmp ( sDirective, "include", 7 ) == 0 )
  {
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
   ParseDefine ( sLine );
  else if ( i == 5 && memcmp ( sDirective, "undef", 5 ) == 0 )
   ParseUndef ( sLine );
  else if ( i == 5 && memcmp ( sDirective, "ifdef", 5 ) == 0 )
   ParseIfdef ( sLine, 1 );
  else if ( i == 6 && memcmp ( sDirective, "ifndef", 6 ) == 0 )
   ParseIfdef ( sLine, 0 );
  else if ( (i == 7 && memcmp ( sDirective, "command", 7 ) == 0) ||
            (i == 8 && memcmp ( sDirective, "xcommand", 8 ) == 0) )
   ParseCommand ( sLine );
  else if ( (i == 9 && memcmp ( sDirective, "translate", 9 ) == 0) ||
            (i == 10 && memcmp ( sDirective, "xtranslate", 10 ) == 0) )
  {
  }
  else if ( i == 6 && memcmp ( sDirective, "stdout", 6 ) == 0 )
   printf ( "%s", sLine );
  else if ( i == 5 && memcmp ( sDirective, "error", 5 ) == 0 )
  {
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
 int i = 0, deflen, parslen, npars = 0;

 while ( *sLine != '\0' && *sLine != ' ')
 {
  if ( *sLine == '(' ) break;
  *(defname+i++) = *sLine++;
 }
 *(defname+i) = '\0';
 deflen = i;

 if ( *sLine == '(' )
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
  parslen = i;
  sLine++;
 }

 if ( ( i = DefSearch(defname) ) >= 0 )
 {
  if ( aDefines[i].pars != NULL ) _xfree ( aDefines[i].pars );
  _xfree ( aDefines[i].value );
 }
 else
 {
  if ( koldefines == maxdefines )
  {
   maxdefines += 50;
   aDefines = (DEFINES *)_xrealloc( aDefines, sizeof( DEFINES ) * maxdefines );
  }
  i = koldefines++;
  aDefines[i].name = ( char * ) _xgrab( deflen + 1 );
  memcpy( aDefines[i].name,defname, deflen );
  aDefines[i].name[deflen] = '\0';
 }
 SKIPTABSPACES(sLine);
 if ( *sLine == '\0' ) aDefines[i].value = NULL;
 else
 {
  deflen = 0;
  while ( *(sLine+deflen) != '\0' ) deflen++;
  aDefines[i].value = ( char * ) _xgrab( deflen + 1 );
  memcpy( aDefines[i].value,  sLine, deflen+1 );
  aDefines[i].value[deflen] = '\0';
 }
 aDefines[i].npars = npars;
 if ( npars > 0 )
 {
  aDefines[i].pars = ( char * ) _xgrab( parslen + 1 );
  memcpy( aDefines[i].pars,  pars, parslen+1 );
 }
 else aDefines[i].pars = NULL;
 return 0;
}

int ParseUndef( char* sLine)
{
 char defname[MAX_NAME];
 int i = 0;

 NextWord( &sLine, defname );

 if ( ( i = DefSearch(defname) ) >= 0 )
 {
  _xfree ( aDefines[i].name );
  _xfree ( aDefines[i].pars );
  _xfree ( aDefines[i].value );
  for ( ; i < koldefines-1; i++ )
  {
   aDefines[i].name = aDefines[i+1].name;
   aDefines[i].value = aDefines[i+1].value;
   aDefines[i].pars = aDefines[i+1].pars;
   aDefines[i].npars = aDefines[i+1].npars;
  }
  koldefines--;
 }
 return 0;
}

int ParseIfdef( char* sLine, int usl)
{
 char defname[MAX_NAME];
 int i;

  NextWord( &sLine, defname );
  if ( *defname == '\0' ) return 3000;
  if ( nCondCompile == maxCondCompile )
  {
    maxCondCompile += 5;
    aCondCompile = (int*)_xrealloc( aCondCompile, sizeof( int ) * maxCondCompile );
  }
  if ( ( (i = DefSearch(defname)) >= 0 && usl )
       || ( i < 0 && !usl ) ) aCondCompile[nCondCompile] = 1;
  else aCondCompile[nCondCompile] = 0;
  nCondCompile++;
  return 0;
}

int DefSearch(char *defname)
{
 int i,j;
 for ( i=0; i<koldefines; i++ )
 {
  for ( j=0; *(aDefines[i].name+j)==*(defname+j) &&
             *(aDefines[i].name+j)!='\0'; j++ );
  if ( *(aDefines[i].name+j)==*(defname+j) ) break;
 }
 if ( i >= koldefines ) return -1;
 return i;
}

int ComSearch(char *cmdname, int ncmd)
{
 int i,j;
 for ( i=(ncmd)? ncmd:kolcommands-1; i >= 0; i-- )
 {
  for ( j=0; *(aCommands[i].name+j)==toupper(*(cmdname+j)) &&
             *(aCommands[i].name+j)!='\0'; j++ );
  if ( *(aCommands[i].name+j)==toupper(*(cmdname+j)) ) break;
 }
 return i;
}

int ParseCommand( char* sLine)
{
 char cmdname[MAX_NAME], pars[MAX_NAME];
 int cmdlen = 0, pattlen, ipos, i;

 while ( *sLine != '\0' && *sLine != ' ')  *(cmdname+cmdlen++) = *sLine++;
 *(cmdname+cmdlen) = '\0';

 SKIPTABSPACES(sLine);
 for ( ipos=0; *(sLine+ipos) != '\0'; ipos++ )
  if ( *(sLine+ipos) == '=' && *(sLine+ipos+1) == '>' ) break;
 if( *(sLine+ipos) == '\0' ) return 4000;

 if ( kolcommands == maxcommands )
 {
  maxcommands += 50;
  aCommands = (COMMANDS *)_xrealloc( aCommands, sizeof( COMMANDS ) * maxcommands );
 }
 aCommands[kolcommands].name = ( char * ) _xgrab( cmdlen + 1 );
 for ( i=0; i<cmdlen; i++ ) *(aCommands[kolcommands].name+i) = toupper(*(cmdname+i));
 aCommands[kolcommands].name[cmdlen] = '\0';

 if ( ipos > 1 )
 {
  pattlen = ipos - 2;
  while ( *(sLine+pattlen) == ' ' ) pattlen--;
  pattlen++;

  aCommands[kolcommands].mpatt = ( char * ) _xgrab( pattlen + 1 );
  memcpy( aCommands[kolcommands].mpatt, sLine, pattlen );
  *(aCommands[kolcommands].mpatt+pattlen) = '\0';
 }
 else aCommands[kolcommands].mpatt = NULL;

 ipos += 2;
 while ( *(sLine+ipos) == ' ' ) ipos++;
 pattlen = 0;
 while ( *(sLine+ipos+pattlen) != '\0' ) pattlen++;

 if ( pattlen > 0 )
 {
  aCommands[kolcommands].value = ( char * ) _xgrab( pattlen + 1 );
  memcpy( aCommands[kolcommands].value, sLine+ipos, pattlen + 1 );
  *(aCommands[kolcommands].value+pattlen) = '\0';
 }
 else aCommands[kolcommands].value = NULL;

 kolcommands++;
 return 0;
}

int ParseExpression( char* sLine, char* sOutLine)
{
 char sToken[MAX_NAME];
 char *ptri, *ptro;
 int lenToken,npars,ndef,i;
 int rezDef;
 int aUsed[100], kolused = 0, lastused;

 do
 {
  ptri = sLine; ptro = sOutLine;
  rezDef = 0; lastused = kolused;
   /* Look for macros from #define      */
  while ( ( lenToken = NextName(&ptri, sToken, &ptro) ) > 0 )
   if ( (ndef=DefSearch(sToken)) >= 0 )
   {
    for(i=0;i<kolused;i++) if ( aUsed[i] == ndef ) break;
    if ( i < kolused ) { if ( i < lastused ) return 1000; }
    else
     aUsed[kolused++] = ndef;
    if ( aDefines[ndef].pars == NULL )
    {
     rezDef = 1;
     ptro -= lenToken;
     lenToken = 0;
     while ( *(aDefines[ndef].value+lenToken) != '\0' )
      *ptro++ = *(aDefines[ndef].value+lenToken++);
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
      if ( aDefines[ndef].npars == npars + 1 )
      {
       rezDef = 1;
       ptro -= lenToken;
       WorkDefine( &ptri, &ptro, ndef );
      }
     }
     else *ptro++ = ' ';
    }
   }
  *ptro = '\0';
  memcpy ( sLine, sOutLine, ptro - sOutLine + 1);
 }
 while ( rezDef );

 /* Look for definitions from #command      */
 ptri = sLine; ptro = sOutLine;
 lenToken = NextWord( &ptri, sToken);
 if ( (ndef=ComSearch(sToken,0)) >= 0 )
 {
  if ( (i = WorkCommand( sToken, &ptri, &ptro, ndef )) > 0 )
   memcpy ( sLine, sOutLine, i+1);
 }
 return 0;
}

void WorkDefine ( char** ptri, char** ptro, int ndef )
{
 char parfict[MAX_NAME], parreal[MAX_NAME];
 char *ptrb, *ptr1, *ptr2;
 int ipos = 0, ifou, ibeg;
 int i;
 int lenfict, lenreal, lenres;

 while ( *(aDefines[ndef].value+ipos) != '\0' ) /* Copying value of macro */
 {                                              /* to destination string  */
  *(*ptro+ipos) = *(aDefines[ndef].value+ipos);
  ipos++;
 }
 *(*ptro+ipos) = '\0';
 lenres = ipos;

 ipos = 0; ibeg = 0;
 do                               /* Parsing through parameters */
 {                                /* in macro definition        */
  if ( *(aDefines[ndef].pars+ipos)==',' || *(aDefines[ndef].pars+ipos)=='\0' )
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
  else *(parfict+ipos-ibeg) = *(aDefines[ndef].pars+ipos);
  if ( *(aDefines[ndef].pars+ipos) == '\0' ) break;
  ipos++;
 }
 while ( 1 );
 (*ptri)++;
 *ptro += lenres;
}

int WorkCommand ( char* sToken, char** ptri, char** ptro, int ndef )
{
 char expreal[MAX_NAME], exppatt[MAX_NAME];
 int lenreal, lenpatt;
 int aCmdStru[100][2], iItem, ifiItem;
 int rez, rezrestr, rezpatt, nbr;
 int lenres = 0;
 int ifou, i;
 char *lastopti;
 char *ptrin, *ptrmp, *ptr;

 while ( *(aCommands[ndef].value+lenres) != '\0' ) /* Copying result pattern */
 {                                                 /* to destination string  */
  *(*ptro+lenres) = *(aCommands[ndef].value+lenres);
  lenres++;
 }

 CmdParse ( *ptri, aCmdStru );     /* Parse input string */
 ptrmp = aCommands[ndef].mpatt;
 do
 {
  Repeate = 0;
  rez = 1; nbr = 0;
  if ( ptrmp == NULL ) rez = ( aCmdStru[0][0] )? 0:1;
  else
   for ( iItem=1; iItem <= aCmdStru[0][0] && rez; iItem++)
   {
    do
    {
      rezpatt = 0;
      SKIPTABSPACES( ptrmp );
      if ( *ptrmp == '[' ) { rezpatt = 1; nbr++; ptrmp++; lastopti = ptrmp; }
      else if ( *ptrmp == ']' )
      {
       rezpatt = 1;
       if ( Repeate ) { Repeate = 0; ptrmp = lastopti; }
       else { nbr--; ptrmp++; }
      }
      else if ( *ptrmp == ',' )
      {
       if ( aCmdStru[iItem][1] == IT_COMMA ) ptrmp++;
       else { rez = 0; break; }
      }
      else if ( *ptrmp == '<' )
      {     /* -------        Match marker            ------- */
       if ( aCmdStru[iItem][1] == IT_COMMA )   { rez = 0; break; }
               /* Copying a match pattern to 'exppatt' */
       lenpatt = 0;
       while ( *ptrmp != '>' && *ptrmp!= '\0' ) *(exppatt+lenpatt++) = *ptrmp++;
       *(exppatt+lenpatt++) = '>';
       *(exppatt+lenpatt) = '\0';
       ptrmp++;
               /* Copying a real expression to 'expreal' */
       lenreal = 0; ptrin = *ptri + aCmdStru[iItem][0];
       while ( lenreal < aCmdStru[iItem+1][0]-aCmdStru[iItem][0] )
        { *(expreal+lenreal) = *(ptrin+lenreal); lenreal++; }
       if ( *(expreal+lenreal) == ' ' )
        { while ( *(expreal+lenreal) == ' ' ) lenreal--;  lenreal++;  }
       *(expreal+lenreal) = '\0';

       if ( *(exppatt+1) == '(' )
       /*  ---- In case of extended match marker  */
       {
        Stringify( expreal, lenreal ); lenreal += 2;
        SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
       }
       else if ( *(exppatt+1) == '*' )
       /*  ---- In case of wild match marker  */
       {
         ifiItem = iItem; iItem = aCmdStru[0][0];
         if ( iItem > ifiItem ) /* Copying a real expression to 'expreal' */
         {
          lenreal = 0; ptrin = *ptri + aCmdStru[ifiItem][0];
          while ( lenreal < aCmdStru[iItem+1][0]-aCmdStru[ifiItem][0] )
           { *(expreal+lenreal) = *(ptrin+lenreal); lenreal++; }
          if ( *(expreal+lenreal) == ' ' )
           { while ( *(expreal+lenreal) == ' ' ) lenreal--;  lenreal++;  }
          *(expreal+lenreal) = '\0';
         }
       }
       else if ( (ifou = hb_strAt(":",1,exppatt,lenpatt)) > 0 )
       /*  ---- In case of restricted match marker  */
       {
        ptr = exppatt + ifou;
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
           pEnclose( exppatt, ifou, '.' ); lenpatt = ifou+2;
           SearnRep( exppatt,lenpatt,".T.",3,*ptro,&lenres);
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
          pEnclose( exppatt, ifou, '.' ); lenpatt = ifou+2;
          SearnRep( exppatt,lenpatt,".F.",3,*ptro,&lenres);
          rezpatt = 1;      /* if was optional, go to the next */
         }
         else { rez = 0; break; } /* else break this rule            */
        }
       }
       else if ( (ifou = hb_strAt(",",1,exppatt,lenpatt)) > 0 )
       /*  ---- In case of list match marker  */
       {
         *(exppatt+ifou-1) = '>';
         *(exppatt+ifou) = '\0';
         lenpatt = ifou;
         ifiItem = iItem;
         while ( iItem < aCmdStru[0][0] && aCmdStru[iItem+1][1] == IT_COMMA ) iItem+=2;
         if ( iItem > ifiItem ) /* Copying a real expression to 'expreal' */
         {
          lenreal = 0; ptrin = *ptri + aCmdStru[ifiItem][0];
          while ( lenreal < aCmdStru[iItem+1][0]-aCmdStru[ifiItem][0] )
           { *(expreal+lenreal) = *(ptrin+lenreal); lenreal++; }
          if ( *(expreal+lenreal) == ' ' )
           { while ( *(expreal+lenreal) == ' ' ) lenreal--;  lenreal++;  }
          *(expreal+lenreal) = '\0';
         }
         SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
       }
       else
       /*  ---- In case of regular match marker  */
       {
                         /* Search for regular result markers */
         SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
                         /* Search for smart stringify  result markers */
         pEnclose( exppatt, lenpatt, '(' ); lenpatt += 2;
         if ( *expreal != '(' )
         { Stringify( expreal, lenreal ); lenreal += 2; }
         SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
                         /* Search for normal stringify  result markers */
         *(exppatt+1) = *(exppatt+lenpatt-2) = '\"';
         if ( *expreal != '\"' )
         { Stringify( expreal, lenreal ); lenreal += 2; }
         SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
                         /* Search for blockify result markers */
         *(exppatt+1) = '{'; *(exppatt+lenpatt-2) = '}';
         *expreal = '{'; *(expreal+lenreal-1) = '}';
         Stuff ( "||", expreal+1, 2, 0, lenreal );
         lenreal += 2;
         SearnRep( exppatt,lenpatt,expreal,lenreal,*ptro,&lenres);
       }
      }     /* ------- End of match marker processing ------- */
      else if ( *ptrmp == '\0' ) { rez = 0; break; }
      else  /* -------           Key word             ------- */
      {
       if ( aCmdStru[iItem][1] != IT_ID )   { rez = 0; break; }
       ptrin = *ptri + aCmdStru[iItem][0];
       for (;isname(*ptrin) && isname(*ptrmp) && toupper(*ptrin)==toupper(*ptrmp); ptrmp++, ptrin++ );
       if ( *ptrmp != *ptrin )
       {
        if ( nbr ) { SkipOptional( &ptrmp, *ptro, &lenres); rezpatt = 1; }
        else { rez = 0; break; }
       }
      }     /* -------   End of key word processing  -------  */
    }
    while ( rezpatt );
   }

  if ( rez && ptrmp != NULL)
  {
   SKIPTABSPACES( ptrmp );
   if ( *ptrmp == ']' ) ptrmp++; nbr--;
   if ( Repeate ) ptrmp = lastopti - 1;
   do
   {
    SKIPTABSPACES( ptrmp );
    if ( *ptrmp != '\0' )
     if ( *ptrmp == '[' )
     {
      ptrmp++;
      SkipOptional( &ptrmp, *ptro, &lenres);
      ptrmp++;
     }
     else if ( *ptrmp=='<' && *(ptrmp+1)=='*' ) break;
     else rez = 0;
   }
   while ( *ptrmp != '\0' && rez );
  }

  if ( !rez && (ndef = ComSearch(sToken,ndef-1))>=0 )
  {
   ptrmp = aCommands[ndef].mpatt;
   ptrin = *ptri;
   lenres = 0;
   while ( *(aCommands[ndef].value+lenres) != '\0' ) /* Copying result pattern */
   {                                                 /* to destination string  */
    *(*ptro+lenres) = *(aCommands[ndef].value+lenres);
    lenres++;
   }
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
     if ( ( isname(*ptri) && State == STATE_ID_END ) ||
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

void SkipOptional( char** ptri, char *ptro, int* lenres)
{
 int nbr = 0, ifou, lCopy;
 char exppatt[MAX_NAME];
 int lenpatt;

 while ( isname(**ptri) ) (*ptri)++;
 SKIPTABSPACES( *ptri );
 while ( **ptri != ']' || nbr )
 {
  if ( **ptri == '[' ) nbr++;
  if ( **ptri == ']' ) nbr--;
  if ( **ptri == '<' )
  {
   lenpatt = 0; lCopy = 1;
   do     /* Drag the match marker ( <...> )  */
   {
    if ( **ptri == ':' ) lCopy = 0;
    if ( lCopy ) *(exppatt+lenpatt++) = **ptri;
    (*ptri)++;
   }
   while ( **ptri != '>' );
   *(exppatt+lenpatt++) = '>';
   *(exppatt+lenpatt) = '\0';
       /* Search for regular result markers */
   SearnRep( exppatt,lenpatt,"",0,ptro,lenres);
       /* Search for logify result markers */
   pEnclose( exppatt, lenpatt, '.' ); lenpatt += 2;
   SearnRep( exppatt,lenpatt,".F.",3,ptro,lenres);
       /* Search for smart stringify result markers */
   *(exppatt+1) = '('; *(exppatt+lenpatt-2) = ')';
   SearnRep( exppatt,lenpatt,"",0,ptro,lenres);
       /* Search for normal stringify result markers */
   *(exppatt+1) = *(exppatt+lenpatt-2) = '\"';
   SearnRep( exppatt,lenpatt,"",0,ptro,lenres);
       /* Search for blockify result markers */
   *(exppatt+1) = '{'; *(exppatt+lenpatt-2) = '}';
   SearnRep( exppatt,lenpatt,"",0,ptro,lenres);
  }
  (*ptri)++;
 }
}

void SearnRep( char *exppatt,int lenpatt,char *expreal,int lenreal,char *ptro, int *lenres)
{
 int ifou, isdvig = 0, rezs;
 char *ptr, *ptr2;
   while ( (ifou = hb_strAt( exppatt, lenpatt, ptro+isdvig, *lenres-isdvig )) > 0 )
   {
    rezs = 0;
    ptr = ptro+isdvig+ifou-2;
    while ( ptr >= ptro+isdvig )
    {
     if ( *ptr == '[' || *ptr == ']' ) break;
     ptr--;
    }
    if ( *ptr == '[' )
    {
     if ( Repeate && lenreal ) return;
     ptr2 = ptro+isdvig+ifou+lenpatt-1;
     while ( *ptr2 != ']' ) ptr2++;
     if ( lenreal == 0 )
     {
      Stuff ( "", ptr, 0, ptr2-ptr+1, *lenres-(ptr-ptro) );
      *lenres -= ptr2-ptr+1;
      isdvig = ptr - ptro;
      rezs = 1;
     }
     else
     {
      char expnew[MAX_NAME];
      int lennew = ptr2-ptr-1, i;

      memcpy ( expnew, ptr+1, lennew );
      *(expnew + lennew) = '\0';
      i = hb_strAt( exppatt, lenpatt, expnew, lennew );
      Stuff ( expreal, expnew+i-1, lenreal, lenpatt, lennew );
      lennew += lenreal - lenpatt;
      Stuff ( expnew, ptr, lennew, 0, *lenres-(ptr-ptro)+1 );
      *lenres += lennew;
      isdvig = ptr - ptro + (ptr2-ptr-1) + lennew;
      rezs = 1;
      Repeate = 1;
     }
    }
    if ( !rezs )
    {
     Stuff ( expreal, ptro+isdvig+ifou-1, lenreal, lenpatt, *lenres-ifou+1 );
     *lenres += lenreal - lenpatt;
     isdvig += ifou +lenreal;
    }
   }
}

int RdStr(int handl_i,char *buffer,int maxlen,int lDropSpaces,char* sBuffer, int* lenBuffer, int* iBuffer)
{
int readed = 0,bytes;
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

void Stringify( char *stroka, int lens )
{
 char *ptr = stroka + lens + 2;

 *ptr-- = '\0';
 *ptr-- = '\"';
 for ( ; ptr > stroka; ptr-- ) *ptr = *(ptr-1);
 *ptr = '\"';
}

void pEnclose( char *stroka, int lens, char symb )
{
 char *ptr = stroka + lens + 2;

 *ptr-- = '\0';
 *ptr-- = '>';
 *ptr-- = (symb=='(')? ')':(symb=='{')? '}':symb;
 for ( ; ptr > stroka+1; ptr-- ) *ptr = *(ptr-1);
 *ptr = symb;
}

int NextWord ( char** sSource, char* sDest )
{
 int i = 0;
 SKIPTABSPACES( (*sSource) );
 while ( **sSource != '\0' && **sSource != ' ')
  { *sDest++ = *(*sSource)++; i++; }
 *sDest = '\0';
 return i;
}

int NextName ( char** sSource, char* sDest, char **sOut )
{
 int i = 0;
 while ( **sSource != '\0' && !isname(**sSource) )
  { if ( *sOut !=NULL ) *(*sOut)++ = **sSource; (*sSource)++; }

 while ( **sSource != '\0' && isname(**sSource) )
  {
   if ( *sOut !=NULL ) *(*sOut)++ = **sSource;
   *sDest++ = *(*sSource)++; i++;
  }
 *sDest = '\0';
 return i;
}
