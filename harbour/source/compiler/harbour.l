%{



/*

   Harbour lex rules.

   Build 21: Spring 99

   Usage: flex -i -oyylex.c harbour.l

   You may find flex.exe at www.harbour-project.org

*/



#include <stdio.h>

#include <stdlib.h>

#include <string.h>

#include <limits.h>

#include "y_tab.h"



void yyerror( char * );

void yyunput( int, char * );

#undef yywrap        /* to implement our own yywrap() funtion to handle EOFs */

int yywrap( void );

#undef YY_INPUT      /* to implement our own YY_INPUT function to manage PRGs without \n at the end */

extern FILE * yyin;  /* currently yacc parsed file */

int yy_lex_input( char *, int );

#define YY_INPUT( buf, result, max_size ) result = yy_lex_input( buf, max_size );

void GenError( int, char*, char * ); /* generic parsing error management function */



#define ERR_NUMERIC_FORMAT      6

#define ERR_STRING_TERMINATOR   7



typedef struct

{

   char * szDefine;

   char * szValue;

   void * pKeys;

   void * pNext;

} _DEFINE, * PDEFINE;



PDEFINE LastDef( PDEFINE pDef );    /* searches for the latest #define */

void Define( char * szDefine );     /* add a new #define symbol */

void DefineKey( char * szKey );     /* add a new key to a #define expression */

void AddDefine( char * szDefine, char * szValue ); /* add a new define from the command line */

PDEFINE FindDef( char * szText );   /* finds a #define */



extern int _iQuiet;

extern int _iRestrictSymbolLength;



PDEFINE pDefs = 0;                  /* support for #defines */

int iLine = 1;

long lNumber = 0;



#define LOOKUP  0

int _iState = LOOKUP;



/*

//Separator     {SpaceTab}|("/ *".*"* /")|"//".*

*/



%}



SpaceTab      [ \t]+

InvalidNumber [0-9]+\.

Number        ([0-9]+)|([0-9]*\.[0-9]+)

HexNumber     0x[0-9A-F]+

Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))

String        (\"(([^\"]*)|([\!]*))\")|(\'(([^\']*)|([\!]*))\')

PseudoFunc    {Identifier}"("+.*")"+



Comment1      [\/][\*]([^\*]|[\*][^\/])*[\*][\/]

Comment2      [\/][\/].*

Comment       ({Comment1}|{Comment2})

Separator     {SpaceTab}|{Comment}



%x            COMMENT3 DEFINE DEFINE_PARAMS DEFINE_EXPR

%x            IFDEF IFNDEF STRING1 STRING2

%x            _NEXT



%%



("//".*)|("&&".*)   ;



^{SpaceTab}*(\*|"NOTE").*  ;

\n{SpaceTab}*(\*|"NOTE").*  ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );



'              BEGIN STRING1;

\"             BEGIN STRING2;



<STRING1>[^'^\n]*   GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0;

<STRING2>[^\"^\n]*  GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0;



<STRING1>[^']*'   { BEGIN 0; yylval.string = strdup( yytext + 1 );

                    yylval.string[ yyleng - 2 ] = 0; return LITERAL; }

<STRING2>[^\"]*\" { BEGIN 0; yylval.string = strdup( yytext + 1 );

                    yylval.string[ yyleng - 2 ] = 0; return LITERAL; }



"/*"           BEGIN COMMENT3;

<COMMENT3>"*/" BEGIN 0;

<COMMENT3>[^"*/"\n]* ;

<COMMENT3>[\/\"]+ ;

<COMMENT3>\n   ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );



"#"{SpaceTab}*"define"      BEGIN DEFINE;

<DEFINE>{Identifier}/{SpaceTab}+   Define( yytext );

<DEFINE>{Identifier}/{SpaceTab}*\n Define( yytext ); BEGIN 0;

<DEFINE>{SpaceTab}          ;

<DEFINE>{Identifier}        LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

<DEFINE>{PseudoFunc}        LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

<DEFINE>{String}            LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

<DEFINE>-?{Number}          LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

<DEFINE>{HexNumber}         LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

<DEFINE>"/*".*"*/"          ;

<DEFINE>{Number}{SpaceTab}*[\(]    yyerror( "Syntax error in #define" );

<DEFINE>{String}{SpaceTab}*[\(]    yyerror( "Syntax error in #define" );

<DEFINE>{Identifier}{SpaceTab}*[\(] Define( yytext ); BEGIN DEFINE_PARAMS;

<DEFINE_PARAMS>{SpaceTab}   ;

<DEFINE_PARAMS>{Identifier} DefineKey( yytext );

<DEFINE_PARAMS>[\,]         ;

<DEFINE_PARAMS>[\)]         BEGIN DEFINE_EXPR;

<DEFINE_EXPR>.*/\n          LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;



"#"{SpaceTab}*"ifdef"          BEGIN IFDEF;

<IFDEF>{Identifier}            if( FindDef( yytext ) ) BEGIN 0;

<IFDEF>"#"{SpaceTab}*"else"\n  ++iLine; BEGIN 0;

<IFDEF>"#"{SpaceTab}*"endif"\n ++iLine; BEGIN 0;

<IFDEF>\n                      ++iLine;

<IFDEF>[\(\),\"\/\.]*          ;

<IFDEF>"#"{SpaceTab}*"ifdef"   ;

<IFDEF>"#"{SpaceTab}*"else"    BEGIN 0;

"#"{SpaceTab}*"else"           BEGIN IFDEF;

"#"{SpaceTab}*"endif"          BEGIN 0;



"#"{SpaceTab}*"ifndef"      BEGIN IFNDEF;

<IFNDEF>{Identifier}        if( ! FindDef( yytext ) ) BEGIN 0;

<IFNDEF>"#endif"\n          ++iLine; BEGIN 0;

<IFNDEF>\n                  ++iLine;



"#"{SpaceTab}*"line".* ;



{SpaceTab}     ;



\n.*           _iState=LOOKUP; yyless( 1 ); ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine ); return '\n';

;\n            _iState=LOOKUP; ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );



"begin"        return BEGINSEQ;

"break"        _iState =BREAK; return BREAK;

"case"         _iState =CASE; return CASE;

"do"           return DO;

"do"{SpaceTab}+"case" return DOCASE;

("do"{SpaceTab}+"while")|"while" return WHILE;

"else"         return ELSE;

"elseif"       _iState =ELSEIF; return ELSEIF;

"end"/[^(]     return END;

"endif"        return ENDIF;

"endcase"      return ENDCASE;

"enddo"        return ENDDO;

"exit"/[\n\;]  return EXITLOOP;

"exit"         return EXIT;

"extern"|"external" _iState =EXTERN; return EXTERN;

"field"/[^(]   _iState =FIELD; return FIELD;

"for"          _iState = FOR; return FOR;

"func"|"funct"|"functi"|"functio"|"function"/[^(] return FUNCTION;

("if"|"iif"){SpaceTab}*/[(]  _iState =IF; return IF;

"if"           _iState =IF; return IF;

"in"           return IN;

"include"      return INCLUDE;

"init"         return INIT;

"local"        _iState =LOCAL; return LOCAL;

"loop"         return LOOP;

"memvar"       _iState =MEMVAR; return MEMVAR;

"next"         BEGIN _NEXT;

<_NEXT>{Separator}*[\n\;] { /* at the end of line */

               BEGIN 0;

               if( _iState == LOOKUP )

               { /* it is first item in the line */

                 ++iLine;

                 return NEXT;

               }

               else

               { /* there is another item in line already */

                 unput( yytext[ yyleng-1 ] );

                 yylval.string = "nExT";

                 return IDENTIFIER;

               }

             }

<_NEXT>{Separator}*[^_a-zA-Z\n\;] { /* there is no identifier after "next" */

               BEGIN 0;

               yylval.string = ((yytext[ yyleng-1 ] =='(')?"NEXT":"nExT");

               unput( yytext[ yyleng-1 ] );

               _iState =IDENTIFIER;

               return IDENTIFIER;

             }

<_NEXT>{Separator}*. { /* an identifier follows NEXT statement */

               BEGIN 0;

               unput( yytext[ yyleng-1 ] );

               _iState =NEXT;

               return NEXT;

             }

<_NEXT>.     {  BEGIN 0; unput( yytext[ yyleng-1 ] ); }

"nil"          return NIL;

"otherwise"    return OTHERWISE;

"parameters"   _iState =PARAMETERS; return PARAMETERS;

"private"      _iState =PRIVATE; return PRIVATE;

"proc"|"procedure" return PROCEDURE;

"public"       _iState =PUBLIC; return PUBLIC;

"qself"{SpaceTab}*[(]{SpaceTab}*[)] return SELF;

"recover"      _iState =RECOVER; return RECOVER;

"retu"|"retur"|"return"  _iState =RETURN; return RETURN;

"sequence"     return SEQUENCE;

"static"       _iState =STATIC; return STATIC;

"step"/[^(]    return STEP;

"to"           return TO;

"using"        return USING;

"with"         return WITH;

"#"            return NE1;

"<>"|"!="      return NE2;

":="           return INASSIGN;

"=="           return EQ;

"++"           _iState =INC; return INC;

"--"           _iState =DEC; return DEC;

"->"           return ALIAS;

"<="           return LE;

">="           return GE;

"+="           return PLUSEQ;

"-="           return MINUSEQ;

"*="           return MULTEQ;

"/="           return DIVEQ;

"^="           return EXPEQ;

"%="           return MODEQ;

"**"|"^"       return POWER;

"."[t|y]"."    return TRUE;

"."[f|n]"."    return FALSE;

".and."        return AND;

".or."         return OR;

"!"|".not."    return NOT;

"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );

[,\;+\-\*\/\{\}\|\#\&\:\<\>\=\[\]\$\%\(\)\@] return yytext[ 0 ];



{InvalidNumber} GenError( ERR_NUMERIC_FORMAT, NULL, NULL );



{Number}       { yylval.dNumber = atof( yytext );

                 if( strchr( yytext, '.' ) )

                    return DOUBLE;

                 else

                 {

                    if( ( double )SHRT_MIN <= yylval.dNumber &&

                        yylval.dNumber <= ( double )SHRT_MAX )

                    {

                       yylval.iNumber = ( int ) yylval.dNumber;

                       return INTEGER;

                    }

                    else if( ( double )LONG_MIN <= yylval.dNumber &&

                             yylval.dNumber <= ( double )LONG_MAX )

                    {

                       yylval.lNumber = ( long ) yylval.dNumber;

                       return INTLONG;

                    }

                    else

                       return DOUBLE;

                 }

               }



{HexNumber}   {  sscanf( yytext, "%lxI", &lNumber );



                 if( ( double )SHRT_MIN <= lNumber &&

                        lNumber <= ( double )SHRT_MAX )

                 {

                    yylval.iNumber = lNumber;

                    return INTEGER;

                 }

                 else if( ( double )LONG_MIN <= lNumber &&

                          lNumber <= ( double )LONG_MAX )

                 {

                    yylval.lNumber = lNumber;

                    return INTLONG;

                 }

                 else

                 {

                    yylval.dNumber = lNumber;

                    return DOUBLE;

                 }

              }



{String}      yylval.string = strdup( yytext + 1 ); yylval.string[ yyleng - 2 ] = 0; return LITERAL;



{Identifier}               {

                              PDEFINE pDef = FindDef( yytext );

                              char * szText; int c;

                              _iState = IDENTIFIER;

                              if( pDef )

                              {

                                 c = strlen( pDef->szValue ) - 1;

                                 szText = pDef->szValue;

                                 while( c >= 0 )

                                    unput( szText[ c-- ] );

                              }

                              else

                              {

                                 if( _iRestrictSymbolLength && strlen( yytext ) > 10 )

                                 {

                                    yytext[ 10 ] = 0;

                                    yyleng = 10;

                                 }

                                 yylval.string = strupr( strdup( yytext ) );

                                 return IDENTIFIER;

                              }

                           }



%%



PDEFINE LastDef( PDEFINE pDef )

{

   while( pDef->pNext )

      pDef = pDef->pNext;



   return pDef;

}



void Define( char * szDefine )

{

   PDEFINE pDef = ( PDEFINE ) malloc( sizeof( _DEFINE ) );



   if( ! pDefs )

      pDefs = pDef;

   else

      LastDef( pDefs )->pNext = pDef;



   pDef->szDefine = strdup( szDefine );

   pDef->szValue  = 0;

   pDef->pKeys    = 0;

   pDef->pNext    = 0;

}



void DefineKey( char * szKey )

{

   PDEFINE pDef = ( PDEFINE ) malloc( sizeof( _DEFINE ) );

   PDEFINE pLast = LastDef( pDefs );



   if( pLast->pKeys )

     LastDef( pLast->pKeys )->pNext = pDef;

   else

     pLast->pKeys = pDef;



   pDef->szDefine = strdup( szKey );

   pDef->szValue  = 0;

   pDef->pKeys    = 0;

   pDef->pNext    = 0;

}



PDEFINE FindDef( char * szText )

{

   PDEFINE pDef = pDefs;



   while( pDef )

   {

      if( ! strcmp( pDef->szDefine, szText ) )

         return pDef;

      else

      {

         if( pDef->pNext )

            pDef = pDef->pNext;

         else

            return 0;

      }

   }

   return 0;

}



void AddDefine( char * szId, char * szValue ) /* adds a new define from command line */

{

   PDEFINE pDefine = FindDef( szId ); /* just incase there are several repited defines */



   if( pDefine )

   {

      if( pDefine->szValue )

      {

         free( pDefine->szValue );

         pDefine->szValue = 0;

      }

      if( szValue )

         pDefine->szValue = strdup( szValue );

   }

   else

   {

      Define( szId );

      if( szValue )

         LastDef( pDefs )->szValue = strdup( szValue );

   }

}



int yy_lex_input( char *buffer, int iBufferSize )

{

   int i;

   static int _ineedLF = 1; /* we want LF as the last character */



   if( ((i = fread(buffer, 1, iBufferSize, yyin)) == 0) && ferror(yyin) )

       YY_FATAL_ERROR( "input in flex scanner failed" );



   if( i == iBufferSize )

     _ineedLF =( buffer[i-1] != '\n' ); /* check if last character is LF */

                                        /* in case it is the last block */

   else if( i < iBufferSize && _ineedLF )

   {

     if( i == 0 )

       buffer[ i++ ] ='\n';

     else if( buffer[ i-1 ] != '\n' )

       buffer[ i++ ] ='\n';

     _ineedLF =0;              /* OK - no more LF */

   }

   return i;

}

