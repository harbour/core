/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_HB_MACRO_YY_MACROY_H_INCLUDED
# define YY_HB_MACRO_YY_MACROY_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int hb_macro_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    NIL = 259,
    NUM_DOUBLE = 260,
    INASSIGN = 261,
    NUM_LONG = 262,
    NUM_DATE = 263,
    TIMESTAMP = 264,
    IIF = 265,
    LITERAL = 266,
    TRUEVALUE = 267,
    FALSEVALUE = 268,
    AND = 269,
    OR = 270,
    NOT = 271,
    EQ = 272,
    NE1 = 273,
    NE2 = 274,
    INC = 275,
    DEC = 276,
    ALIASOP = 277,
    HASHOP = 278,
    SELF = 279,
    LE = 280,
    GE = 281,
    FIELD = 282,
    MACROVAR = 283,
    MACROTEXT = 284,
    PLUSEQ = 285,
    MINUSEQ = 286,
    MULTEQ = 287,
    DIVEQ = 288,
    POWER = 289,
    EXPEQ = 290,
    MODEQ = 291,
    EPSILON = 292,
    POST = 293,
    UNARY = 294,
    PRE = 295
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 140 "macro.y" /* yacc.c:1909  */

   const char * string; /* to hold a string returned by lex */
   int       iNumber;   /* to hold a temporary integer number */
   HB_MAXINT lNumber;   /* to hold a temporary long number */
   void *    pVoid;     /* to hold any memory structure we may need */
   PHB_EXPR  asExpr;
   struct
   {
      const char * string;
      HB_SIZE      length;
   } valChar;
   struct
   {
      int      iNumber; /* to hold a number returned by lex */
   } valInteger;
   struct
   {
      HB_MAXINT lNumber; /* to hold a long number returned by lex */
      HB_UCHAR  bWidth;  /* to hold the width of the value */
   } valLong;
   struct
   {
      double   dNumber; /* to hold a double number returned by lex */
      HB_UCHAR bWidth;  /* to hold the width of the value */
      HB_UCHAR bDec;    /* to hold the number of decimal points in the value */
   } valDouble;
   struct
   {
      long     date;    /* to hold julian date */
      long     time;    /* to hold milliseconds */
   } valTimeStamp;

#line 128 "macroy.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int hb_macro_yyparse (PHB_MACRO pMacro);

#endif /* !YY_HB_MACRO_YY_MACROY_H_INCLUDED  */
