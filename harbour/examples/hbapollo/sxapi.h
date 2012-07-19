/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

#ifndef __SIXAPIC__
#define __SIXAPIC__

/* NOTE: This hack is needed to suppress 'non-ANSI
         keyword' warnings inside Sde61.h. */
#if defined( __MINGW32__ ) || defined( __BORLANDC__ ) || defined( __WATCOMC__ )
   #define _declspec __declspec
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapirdd.h"
#include "hbset.h"
#include "hbdate.h"
#include "error.ch"
#include "hbvm.h"
#include "hbstack.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#if defined( HB_WITH_APOLLO_VER61 )
   #include "Sde61.h"
#else
   #include "sde7.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ******************************************************* */
/* SDE defines                                             */
/* ******************************************************* */
#define READWRITE                   0
#define READONLY                    1
#define EXCLUSIVE                   2

/* ********* */
/* RDE types */
/* ********* */
#define SDENTX                      1
#define SDEFOX                      2
#define SDENSX                      3
#define SDENSXDBT                   4

/* text file types */
#define COMMA_DELIM                 21
#define SDF_FILE                    22
#define TAB_DELIM                   23
#define SPACE_DELIM                 24
#define USER_DELIM                  25

/* OEM Source types for AppendFrom */
#define OEMNTX                      31
#define OEMFOX                      32
#define OEMNSX                      33

/* Outdated RDE Names */
#define DBFNTX                      SDENTX
#define DBFIDX                      SDEFOX
#define DBFNSX                      SDENSX
#define DBFNSXDBT                   SDENSXDBT

/* *********** */
/* Index Types */
/* *********** */
#define INDEX_STANDARD              1
#define INDEX_STANDARD_UNIQUE       2
#define INDEX_CONDITIONAL           3
#define INDEX_CONDITIONAL_UNIQUE    4

/* ************* */
/*   date types  */
/* ************* */
#define AMERICAN                    0
#define ANSI                        1
#define BRITISH                     2
#define FRENCH                      3
#define GERMAN                      4
#define ITALIAN                     5
#define SPANISH                     6
#define WIN_DEFAULT                 99

/* ************************************ */
/* Data type identifiers for sx_Replace */
/* ************************************ */
#define R_INTEGER                   1
#define R_LONG                      2
#define R_DOUBLE                    8
#define R_JULIAN                    32
#define R_LOGICAL                   128
#define R_CHAR                      1024
#define R_DATESTR                   1056
#define R_MEMO                      3072
#define R_BITMAP                    4096
#define R_BLOBFILE                  8192
#define R_BLOBPTR                   8193
#define R_GENERAL                   8195

/* ******************************** */
/* sx_QueryTest Results             */
/* ******************************** */
#define OPTIMIZE_NONE               0
#define OPTIMIZE_PART               1
#define OPTIMIZE_FULL               2

/* ******************************** */
/* sx_EvalTest Results              */
/* ******************************** */
#define EVAL_CHARACTER              1
#define EVAL_NUMERIC                2
#define EVAL_LOGICAL                3

/* ******************************** */
/* sx_Index(tag) iOptions           */
/* ******************************** */
#define IDX_NONE                    0
#define IDX_UNIQUE                  1
#define IDX_EMPTY                   2

/* ******************************** */
/* sx_ErrorLevel uiErrorLevels      */
/* ******************************** */
#define ERRLEVEL_NONE               0
#define ERRLEVEL_FATAL              1
#define ERRLEVEL_STANDARD           2

/* ***************************************** */
/* RYO HB_BOOL Operations for RYOFilterActivate */
/* ***************************************** */
#define RYOFILTER_NEW               1
#define RYOFILTER_AND               2
#define RYOFILTER_OR                3
#define RYOFILTER_XOR               4
#define RYOFILTER_ANDNOT            5
#define RYOFILTER_ORNOT             6
#define RYOFILTER_XORNOT            7

/* ***************************************** */
/* Collation rule type                       */
/* ***************************************** */
#define ALPHABETICAL                0     /* usual linguistic */
#define SPELLING                    1     /* == Duden */
#define EXPANDING                   2     /* additonal groups coalltion rule */
#define MACHINE                     3     /* simple value ordering */

/* ***************************************** */
/* Collation rule order                      */
/* ***************************************** */
#define DEFAULT_SET                 0     /* ALPHABETICAL or duden or expanding default */

/* ******************************** */
/* sx_SysProp Constants             */
/* ******************************** */

/* Global Task Information */
/* Gets should always be even numbers */
#define SDE_SP_GETSOFTSEEK          1000  /* Get the softseek flag */
#define SDE_SP_SETSOFTSEEK          1001  /* Set the softseek flag */
#define SDE_SP_GETEXACT             1002  /* Get the extact flag */
#define SDE_SP_SETEXACT             1003  /* Set the extact flag */
#define SDE_SP_GETDELETED           1006  /* Get the deleted flag */
#define SDE_SP_PUTOBUFFER           1007  /* Write the optimistic buffer on commit */
#define SDE_SP_GETOBUFFER           1008  /* Get the optimistic buffer flag */
#define SDE_SP_SETOBUFFER           1009  /* Set the optimistic buffer flag */
#define SDE_SP_GETSTRINGTYPE        1010  /* Get the stringtype flag */
#define SDE_SP_SETSTRINGTYPE        1011  /* Set the stringtype flag */
#define SDE_SP_GETDISABLEAUTO       1012  /* Get the disable auto open flag */
#define SDE_SP_SETDISABLEAUTO       1013  /* Set the disable auto open flag */
#define SDE_SP_SETOEMCOLLATE        1101  /* Set the collation sequence for OEM tables. */
#define SDE_SP_GETOEMCOLLATE        1111  /* Get the collation sequence for OEM tables. */
#define SDE_SP_SETCHRCOLLATE        1102  /* Set the collation sequence for Win tables. */
#define SDE_SP_GETCHRCOLLATE        1122  /* Get the collation sequence for Win tables. */
#define SDE_SP_SETLGTRCOLLATE       1103  /* Set the ligatures collation dimmension */
#define SDE_SP_GETLGTRCOLLATE       1133  /* Get the ligatures collation dimmension */
#define SDE_SP_SETSPECIALCOLLATE    1108  /* Set the international collation like DUDEN collate flag */
#define SDE_SP_GETSPECIALCOLLATE    1109  /* Set the international collation like DUDEN collate flag */
#define SDE_SP_GETLANGUAGECOLLATE   1110  /* Get language, according to collation done */
#define SDE_SP_GETDUDENCOLLATE      1104  /* get the German DUDEN collate flag */
#define SDE_SP_SETDUDENCOLLATE      1105  /* set the German DUDEN collate flag */
#define SDE_SP_GETLIMITCASECONV     1106  /* limit case conv to A-Z, a-z if HB_TRUE */
#define SDE_SP_SETLIMITCASECONV     1107  /* limit case conv to A-Z, a-z if HB_TRUE */

/* Behavior settings which bridge the differences between 1.40 and 2.00 */
#define SDE_SP_GETADDQUERY          1300  /* Get the AddQueryFlag */
#define SDE_SP_SETADDQUERY          1301  /* Set the AddQueryFlag */
#define SDE_SP_GETUSECONDITIONAL    1302  /* Get the bUseConditional flag */
#define SDE_SP_SETUSECONDITIONAL    1303  /* Get the bUseConditional flag */
#define SDE_SP_SETWRITEBLOBHDR      1305  /* Set the bWriteBlobHdr */
#define SDE_SP_GETQUERYRELAXFLAG    1306  /* Get flag that dictates rules of query */
#define SDE_SP_SETQUERYRELAXFLAG    1307  /* Set flag that dictates rules of query */

/* WorkArea information */
#define SDE_SP_GETDRIVER            2000  /* Get the active driver */
#define SDE_SP_SETSTRDEFLEN         2001  /* Set the default lenght for STR, if 2nd parameter is absent and field lenght zero */
#define SDE_SP_SETSTRDEFDEC         2002  /* Set the default decimals for STR, if 3d parameter is absent and field lenght zero */
#define SDE_SP_SETDEFAPPEND         2003  /* Set default behavior for ordering ordering for non-unique key like FOX/Clipper */
#define SDE_SP_SETMEMOMIXED         2004  /* Set pure Clipper's memo for NSX driver */
#define SDE_SP_BDESPECIFIC          2005  /* Set the treatment of LIKE operator in accoring to BDE */
#define SDE_SP_DBASEDATEHEADER      2006  /* Set the using of DBF header in according to DbaseIII+ specification */
#define SDE_SP_SETAUTOPAD           2007
#define SDE_SP_GETAUTOPAD           2008

/* Index information for current workarea */
#define SDE_SP_GETINDEXCOUNT        3000  /* Get the number of indexes */
#define SDE_SP_GETDESCENDING        3002  /* Get the descending flag */
#define SDE_SP_GETEMPTY             3004  /* Get the empty index flag */
#define NIL                         '\0'

/* Verify Types */
#define VBEG                        1  /* verify at begining of string only */
#define VEND                        2  /* verify at end of string only */
#define VAND                        3  /* verify all tokens in target */
#define VONE                        4  /* don't tokenize target */

/* FTS File Open Modes */
#define FTS_SHARE                   0x0         /* SHARE */
#define FTS_EXCL                    0x1         /* EXCLUSIVE */
#define FTS_RDONLY                  0x2         /* READ-ONLY */

#define FTSifdelete                 FTSisdelete /* permit old name */

/* FTS Error Codes */
#define FTS_CREATEFAIL              -1
#define FTS_MEMERR                  -2
#define FTS_NULLPTR                 -3
#define FTS_BADSEEK                 -4
#define FTS_BADREAD                 -5
#define FTS_BADWRITE                -6
#define FTS_RECBOUND                -7
#define FTS_ISDELETED               -8
#define FTS_NOTDELETED              -9
#define FTS_OPENERR                 -10
#define FTS_INTERR                  -11
#define FTS_NORECS                  -13
#define FTS_BADPARMS                -16
#define FTS_NOMOREHANDLES           -17
#define FTS_BADHANDLE               -18
#define FTS_BADIHANDLE              -19
#define FTS_LOCKFAILED              -20
#define FTS_NOMORELOCKS             -21
#define FTS_CANNOTUNLOCK            -22
#define FTS_BADCOMMIT               -23

#define SX_DUMMY_NUMBER             9999

typedef struct SX_DBOPENINFO
{
   HB_USHORT uiArea;
   const char * cFilename;
   const char * cAlias;
   HB_BOOL fShared;
   HB_BOOL fReadonly;
   HB_USHORT iRDEType;
   HB_USHORT iMode;
   const char * cRDD;
   HB_USHORT iCommitLevel;
   HB_USHORT iRecSize;
   HB_USHORT iFieldCount;
   PHB_ITEM aFieldInfo;
} SX_DBOPENINFO;

extern LONG         _sx_SysProp( WORD uiSysItem, PVOID vpData );
extern char *       _sx_randomname( const char * szPrefix );
extern PHB_ITEM     _sx_FieldNames( void );
extern int          _sx_CheckRDD( const char * sSetDefault );
extern HB_BOOL      _sx_Eval( PHB_ITEM pItem );
extern HB_BOOL      _sx_Used( VOID );
extern HB_BOOL      _sx_SetCentury( VOID );
extern HB_BOOL      _sx_CopyStructure( PBYTE cpFileName, PHB_ITEM paFields );
extern char *       _sx_insertchar( char * strbuf, char chrtoins, HB_ISIZ pos );
extern char *       _sx_ltrim( char * string );
extern char *       _sx_rtrim( char * string );
extern char *       _sx_alltrim( char * string );
extern char *       _sx_padl( char * strbuf, char chrtofill, HB_SIZE len );
extern char *       _sx_padr( char * strbuf, char chrtofill, HB_SIZE len );
extern char *       _sx_upper( char * string );
extern char *       _sx_strcat( char * dest, const char * src, ... );
extern char *       _sx_AutoAlias( const char * cpFileName );
extern PHB_ITEM     _sx_DbStruct( VOID );
extern WORD         _sx_select( PHB_ITEM vParam );
extern int          _sx_CheckOpenMode( const char * sxOpenMode );
extern char *       _sx_GetDateValue( PBYTE cFieldName );
extern void         _sx_SetDBFInfo( int iOpenedArea, const char * szAlias,
                                    int iOpenMode, int iRDEType );
extern void         _sx_DelOpenInfo( const char * szAlias );
extern const char * _sx_CheckFileExt( const char * szFileName );
extern PHB_ITEM     _sx_GetAlias( void );

#if defined( __SXAPI_INIT )
   int               i_sxApi_MemoBlock_Size;    /* Default Memo Block Size */
   int               i_sxApi_Error_Level;       /* Default ErrorLevel */
   int               i_sxApi_RDD_Default;       /* Default RDD Driver */
   HB_BOOL              bSetTrimmedON       = HB_FALSE;
   PHB_ITEM          Opened_DBF_Property = NULL;
#else
   extern int        i_sxApi_MemoBlock_Size;    /* Default Memo Block Size */
   extern int        i_sxApi_Error_Level;       /* Default ErrorLevel */
   extern int        i_sxApi_RDD_Default;       /* Default RDD Driver */
   extern HB_BOOL    bSetTrimmedON;
   extern PHB_ITEM   Opened_DBF_Property;
#endif

#define AMERICAN        0  /* MM/DD/YY */
#define ANSI            1  /* YY.MM.DD */
#define BRITISH         2  /* DD/MM/YY */
#define FRENCH          3  /* DD/MM/YY */
#define GERMAN          4  /* DD.MM.YY */
#define ITALIAN         5  /* DD-MM-YY */
#define SPANISH         6  /* DD-MM-YY */

#ifdef __cplusplus
}
#endif

#endif
