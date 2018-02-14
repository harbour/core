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

#ifndef YY_HB_COMP_YY_HARBOURY_H_INCLUDED
# define YY_HB_COMP_YY_HARBOURY_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int hb_comp_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    FUNCTION = 258,
    PROCEDURE = 259,
    IDENTIFIER = 260,
    RETURN = 261,
    NIL = 262,
    LOCAL = 263,
    STATIC = 264,
    IIF = 265,
    IF = 266,
    ELSE = 267,
    ELSEIF = 268,
    END = 269,
    ENDIF = 270,
    ENDERR = 271,
    LITERAL = 272,
    TRUEVALUE = 273,
    FALSEVALUE = 274,
    NUM_DOUBLE = 275,
    INASSIGN = 276,
    NUM_LONG = 277,
    ANNOUNCE = 278,
    EXTERN = 279,
    DYNAMIC = 280,
    AND = 281,
    OR = 282,
    NOT = 283,
    PUBLIC = 284,
    EQ = 285,
    NE1 = 286,
    NE2 = 287,
    INC = 288,
    DEC = 289,
    ALIASOP = 290,
    DOCASE = 291,
    CASE = 292,
    OTHERWISE = 293,
    ENDCASE = 294,
    ENDDO = 295,
    MEMVAR = 296,
    WHILE = 297,
    LOOP = 298,
    EXIT = 299,
    INIT = 300,
    FOR = 301,
    NEXT = 302,
    TO = 303,
    STEP = 304,
    LE = 305,
    GE = 306,
    FIELD = 307,
    IN = 308,
    PARAMETERS = 309,
    PLUSEQ = 310,
    MINUSEQ = 311,
    MULTEQ = 312,
    DIVEQ = 313,
    POWER = 314,
    EXPEQ = 315,
    MODEQ = 316,
    PRIVATE = 317,
    BEGINSEQ = 318,
    BREAK = 319,
    RECOVER = 320,
    RECOVERUSING = 321,
    ALWAYS = 322,
    ENDSEQ = 323,
    DO = 324,
    WITH = 325,
    SELF = 326,
    LINE = 327,
    MACROVAR = 328,
    MACROTEXT = 329,
    AS_ARRAY = 330,
    AS_BLOCK = 331,
    AS_CHARACTER = 332,
    AS_CLASS = 333,
    AS_DATE = 334,
    AS_LOGICAL = 335,
    AS_NUMERIC = 336,
    AS_OBJECT = 337,
    AS_VARIANT = 338,
    AS_ARRAY_ARRAY = 339,
    AS_BLOCK_ARRAY = 340,
    AS_CHARACTER_ARRAY = 341,
    AS_CLASS_ARRAY = 342,
    AS_DATE_ARRAY = 343,
    AS_LOGICAL_ARRAY = 344,
    AS_NUMERIC_ARRAY = 345,
    AS_OBJECT_ARRAY = 346,
    DECLARE = 347,
    OPTIONAL = 348,
    DECLARE_CLASS = 349,
    DECLARE_MEMBER = 350,
    PROCREQ = 351,
    CBSTART = 352,
    DOIDENT = 353,
    FOREACH = 354,
    DESCEND = 355,
    DOSWITCH = 356,
    ENDSWITCH = 357,
    WITHOBJECT = 358,
    ENDWITH = 359,
    NUM_DATE = 360,
    TIMESTAMP = 361,
    EPSILON = 362,
    HASHOP = 363,
    THREAD_STATIC = 364,
    POST = 365,
    UNARY = 366,
    PRE = 367
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 125 "harbour.y" /* yacc.c:1909  */

   const char * string; /* to hold a string returned by lex */
   int     iNumber;     /* to hold a temporary integer number */
   HB_SIZE sNumber;     /* to hold a temporary HB_SIZE values */
   HB_MAXINT lNumber;   /* to hold a temporary long number */
   HB_BOOL bTrue;
   PHB_EXPR asExpr;
   void * pVoid;        /* to hold any memory structure we may need */
   struct
   {
      HB_MAXINT lNumber;   /* to hold a long number returned by lex */
      HB_UCHAR  bWidth;    /* to hold the width of the value */
   } valLong;
   struct
   {
      double   dNumber;    /* to hold a double number returned by lex */
      HB_UCHAR bWidth;     /* to hold the width of the value */
      HB_UCHAR bDec;       /* to hold the number of decimal points in the value */
   } valDouble;
   struct
   {
      long     date;
      long     time;
   } valTimeStamp;
   struct
   {
      char *   string;
      HB_SIZE  length;
      HB_BOOL  dealloc;
   } valChar;
   struct
   {
      char *   string;
      HB_SIZE  length;
      int      flags;   /* Flag for early {|| &macro} (1) or late {|| &(macro)} (2) binding */
   } asCodeblock;
   PHB_VARTYPE asVarType;

#line 206 "harboury.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int hb_comp_yyparse (PHB_COMP pComp);

#endif /* !YY_HB_COMP_YY_HARBOURY_H_INCLUDED  */
