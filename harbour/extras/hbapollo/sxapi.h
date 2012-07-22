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

#define _declspec(x) /* ugly hack */
#if defined( HB_WITH_APOLLO_VER61 )
   #include "Sde61.h"
#else
   #include "sde7.h"
#endif
#undef _declspec

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

HB_EXTERN_BEGIN

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
   HB_BOOL           bSetTrimmedON       = HB_FALSE;
   PHB_ITEM          Opened_DBF_Property = NULL;
#else
   extern int        i_sxApi_MemoBlock_Size;    /* Default Memo Block Size */
   extern int        i_sxApi_Error_Level;       /* Default ErrorLevel */
   extern int        i_sxApi_RDD_Default;       /* Default RDD Driver */
   extern HB_BOOL    bSetTrimmedON;
   extern PHB_ITEM   Opened_DBF_Property;
#endif

HB_EXTERN_END

#define AMERICAN        0  /* MM/DD/YY */
#define ANSI            1  /* YY.MM.DD */
#define BRITISH         2  /* DD/MM/YY */
#define FRENCH          3  /* DD/MM/YY */
#define GERMAN          4  /* DD.MM.YY */
#define ITALIAN         5  /* DD-MM-YY */
#define SPANISH         6  /* DD-MM-YY */

#endif
