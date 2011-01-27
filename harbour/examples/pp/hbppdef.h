/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Preprocesor
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_PP_H_
#define HB_PP_H_

#include "hbapi.h"
#include "hbapifs.h"
#include "hberrors.h"

HB_EXTERN_BEGIN

typedef void * PHB_PP_STATE;
typedef void * PHB_PP_TOKEN;
typedef void * PHB_PP_OPEN_FUNC;

struct _DEFINES;
typedef struct _DEFINES
{
   char * name;
   int namelen;
   char * pars;
   int    npars;
   char * value;
   struct _DEFINES * last;
} DEFINES;

struct _COMMANDS;
typedef struct _COMMANDS
{
   int com_or_xcom;
   char * name;
   int namelen;
   char * mpatt;
   char * value;
   struct _COMMANDS * last;
} COMMANDS;

/* #include support */
typedef struct
{
   FILE * handle;               /* handle of the opened file */
   void * pBuffer;              /* file buffer */
   char * yyBuffer;             /* buffer used by yyac */
   int    iBuffer;              /* current position in file buffer */
   int    lenBuffer;            /* current length of data in file buffer */
   char * szFileName;           /* name of the file */
   void * pPrev;                /* pointer to the previous opened file */
   void * pNext;                /* pointer to the next opened file */
   int    iLine;                /* currently processed line number */
} _FILE, * PFILE;               /* structure to hold an opened PRG or CH */

/* structure to control several opened PRGs and CHs */
typedef struct
{
   PFILE pLast;                 /* pointer to the last opened file */
   int   iFiles;                /* number of files currently opened */
} FILES;


#define HB_PP_STR_SIZE  12288
#define HB_PP_BUFF_SIZE 4096

#define HB_SKIPTABSPACES( sptr ) while( *( sptr ) == ' ' || *( sptr ) == '\t' ) ( sptr )++

/* ppcore.c exported functions and variables */

#define HB_INCLUDE_FUNC_( hbFunc ) HB_BOOL hbFunc( char *, HB_PATHNAMES * )
typedef HB_INCLUDE_FUNC_( HB_INCLUDE_FUNC );
typedef HB_INCLUDE_FUNC * HB_INCLUDE_FUNC_PTR;

extern void   hb_pp_SetRules_( HB_INCLUDE_FUNC_PTR hb_compInclude, HB_BOOL hb_comp_bQuiet );
extern int    hb_pp_ReadRules( void );
extern void   hb_pp_Init( void );
extern void   hb_pp_Free( void );
extern void   hb_pp_CloseInclude( void );
extern int    hb_pp_ParseDirective_( char * ); /* Parsing preprocessor directives ( #... ) */
extern int    hb_pp_ParseExpression( char *, char *, HB_BOOL ); /* Parsing a line ( without preprocessor directive ) */
extern int    hb_pp_WrStr( FILE *, char * );
extern int    hb_pp_RdStr( FILE *, char *, int, HB_BOOL, char *, int *, int * );
extern void   hb_pp_Stuff( char *, char *, int, int, int );
extern int    hb_pp_strocpy( char *, char * );
extern DEFINES * hb_pp_AddDefine_( char *, char * );         /* Add new #define to a linked list */

extern PHB_FNAME hb_comp_pFileName;
extern HB_BOOL   hb_comp_bQuiet;
extern HB_BOOL   hb_comp_bStartProc;
extern HB_BOOL   hb_comp_bAutoMemvarAssume;
extern HB_BOOL   hb_comp_bForceMemvars;
extern HB_BOOL   hb_comp_bShortCuts;
extern HB_BOOL   hb_comp_bDebugInfo;
extern HB_BOOL   hb_comp_bLineNumbers;
extern int       hb_comp_iExitLevel;
extern int       hb_comp_iWarnings;
extern int       hb_comp_iLine;
extern int       hb_comp_iLineINLINE;
extern int       hb_comp_iLinePRG;
extern int       hb_pp_lInclude;
extern int *     hb_pp_aCondCompile;
extern int       hb_pp_nCondCompile;
extern const char * hb_pp_szErrors[];
extern const char * hb_pp_szWarnings[];
extern int       hb_pp_nEmptyStrings;
extern int       hb_pp_LastOutLine;
extern int       hb_pp_StreamBlock;
extern HB_BOOL   hb_pp_NestedLiteralString;
extern HB_BOOL   hb_pp_LiteralEscSeq;
extern unsigned int hb_pp_MaxTranslateCycles;
extern HB_PATHNAMES * hb_comp_pIncludePath;
extern FILES  hb_comp_files;
extern HB_BOOL   hb_comp_bPPO;
extern FILE * hb_comp_yyppo;

/* ppcomp.c exported functions */
extern int    hb_pp_Internal_( FILE *, char * );
extern void   hb_pp_InternalFree( void );

#define HB_PP_STREAM_DUMP_C   1 /* pragma BEGINDUMP */
#define HB_PP_STREAM_CLIPPER  2 /* clipper compatible TEXT/ENDTEXT */
#define HB_PP_STREAM_PRG      4 /* TEXT/ENDTEXT lines joined with LF */
#define HB_PP_STREAM_C        8 /* TEXT/ENDTEXT lines joined and ESC seq processed */

extern HB_BOOL   hb_pp_StreamBlockBegin( char *, int );
extern void      hb_pp_BlockEnd( void );

/* pptable.c exported functions and variables */

extern void   hb_pp_Table( void );

extern DEFINES *  hb_pp_topDefine;
extern COMMANDS * hb_pp_topCommand;
extern COMMANDS * hb_pp_topTranslate;

/* pragma.c exported functions */

extern HB_BOOL hb_pp_ParsePragma( char * szline );

/*
 * Errors generated by Harbour preprocessor
 */
#define HB_PP_ERR_CANNOT_OPEN                   1
#define HB_PP_ERR_DIRECTIVE_ELSE                2
#define HB_PP_ERR_DIRECTIVE_ENDIF               3
#define HB_PP_ERR_WRONG_NAME                    4
#define HB_PP_ERR_DEFINE_ABSENT                 5
#define HB_PP_ERR_COMMAND_DEFINITION            6
#define HB_PP_ERR_PATTERN_DEFINITION            7
#define HB_PP_ERR_RECURSE                       8
#define HB_PP_ERR_WRONG_DIRECTIVE               9
#define HB_PP_ERR_EXPLICIT                      10
#define HB_PP_ERR_MEMALLOC                      11
#define HB_PP_ERR_MEMREALLOC                    12
#define HB_PP_ERR_MEMFREE                       13
#define HB_PP_ERR_PRAGMA_BAD_VALUE              14
#define HB_PP_ERR_CANNOT_OPEN_RULES             15
#define HB_PP_ERR_BAD_RULES_FILE_NAME           16
#define HB_PP_ERR_TOO_MANY_INCLUDES             17
#define HB_PP_ERR_BUFFER_OVERFLOW               18
#define HB_PP_ERR_LABEL_MISSING_IN_DEFINE       19
#define HB_PP_ERR_PARE_MISSING_IN_DEFINE        20
#define HB_PP_ERR_LABEL_DUPL_IN_DEFINE          21

#define HB_PP_WARN_DEFINE_REDEF                 1
#define HB_PP_WARN_NO_DIRECTIVES                2

HB_EXTERN_END

#endif /* HB_PP_H_ */
