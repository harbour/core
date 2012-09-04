/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Advantage Database Server RDD
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

#include "hbapirdd.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#if !defined( WIN32 ) && defined( HB_OS_WIN )
   #define WIN32
#endif
#if !defined( unix ) && defined( HB_OS_UNIX )
   #define unix
#endif
#if !defined( x64 ) && defined( HB_ARCH_64BIT )
   #define x64
#endif
#if defined( __WATCOMC__ ) || defined( __LCC__ ) || ( defined( __MINGW32__ ) && ! defined( _declspec ) )
   #define _declspec( dllexport ) __declspec( dllexport )
#endif

#include "ace.h"

/* Autodetect ACE version. */
#if   defined( ADS_GET_FORMAT_WEB )
   #define _ADS_LIB_VERSION 1100 /* or upper */
#elif defined( ADS_GET_UTF8 )
   #define _ADS_LIB_VERSION 1010
#elif defined( ADS_DEFAULT_SQL_TIMEOUT )
   #define _ADS_LIB_VERSION 1000
#elif defined( DANISH_ADS_CS_AS_1252 )
   #define _ADS_LIB_VERSION 910
#elif defined( ADS_NOTIFICATION_CONNECTION )
   #define _ADS_LIB_VERSION 900
#elif defined( ADS_UDP_IP_CONNECTION )
   #define _ADS_LIB_VERSION 810
#elif defined( ADS_REPLICATION_CONNECTION )
   #define _ADS_LIB_VERSION 800
#elif defined( ADS_NOT_AUTO_OPEN )
   #define _ADS_LIB_VERSION 710
#elif defined( ADS_FTS_INDEX_ORDER )
   #define _ADS_LIB_VERSION 700
#elif defined( ADS_COMPRESS_ALWAYS )
   #define _ADS_LIB_VERSION 620
#elif defined( ADS_USER_DEFINED )
   #define _ADS_LIB_VERSION 611
#else
   #define _ADS_LIB_VERSION 500
#endif

/* Make sure to not allow a manual override requesting
   a higher version than the one of ACE. [vszakats] */
#if !defined( ADS_LIB_VERSION )
   #define ADS_LIB_VERSION _ADS_LIB_VERSION
#elif ADS_LIB_VERSION > _ADS_LIB_VERSION
   #undef ADS_LIB_VERSION
   #define ADS_LIB_VERSION _ADS_LIB_VERSION
#endif

HB_EXTERN_BEGIN

#if ADS_LIB_VERSION >= 600
/* NOTE: Undocumented ACE function. */
UNSIGNED32 ENTRYPOINT AdsDeleteFile( ADSHANDLE hConnection, UNSIGNED8 * pucFileName );
#endif

/*
 *  ADS WORKAREA
 *  --------
 *  The Workarea Structure of Advantage Database Server RDD
 *
 */

typedef struct _ADSAREA_
{
   AREA area;

   /*
    *  ADS's additions to the workarea structure
    *
    *  Warning: The above section MUST match WORKAREA exactly!  Any
    *  additions to the structure MUST be added below, as in this
    *  example.
    */

   LPDBRELINFO lpdbPendingRel;    /* Pointer to parent rel struct */

   char *    szDataFileName;      /* Name of data file */
   HB_ULONG  ulRecordLen;         /* Size of record */
   HB_ULONG  ulRecNo;             /* Current record */
   HB_BYTE * pRecord;             /* Buffer of record data */
   HB_ULONG  maxFieldLen;         /* Max field length in table record */

   HB_BOOL   fPositioned;         /* HB_TRUE if we are not at phantom record */
   HB_BOOL   fShared;             /* Shared file */
   HB_BOOL   fReadonly;           /* Read only file */
   HB_BOOL   fFLocked;            /* HB_TRUE if file is locked */

   int       iFileType;           /* adt/cdx/ntx/vfp */

   ADSHANDLE hTable;
   ADSHANDLE hOrdCurrent;
   ADSHANDLE hStatement;
} ADSAREA;

typedef ADSAREA * ADSAREAP;

#define SELF_RESETREL( p )    if( (p)->lpdbPendingRel ) \
                              { \
                                 if( (p)->lpdbPendingRel->isScoped && \
                                    !(p)->lpdbPendingRel->isOptimized ) \
                                    SELF_FORCEREL( &(p)->area ); \
                                 else \
                                    (p)->lpdbPendingRel = NULL; \
                              }


#define HB_RDD_ADS_VERSION_STRING "ADS RDD 1.4"

#if defined( HB_OS_WIN )
#  define ADS_USE_OEM_TRANSLATION
#else
#  undef ADS_USE_OEM_TRANSLATION
#endif

#define HB_ADS_PARCONNECTION( n )      ( ( ADSHANDLE ) hb_parnintdef( n, hb_ads_getConnection() ) )
#define HB_ADS_RETCONNECTION( h )      hb_retnint( h )
#define HB_ADS_GETCONNECTION( p )      ( ( hb_itemType( p ) & HB_IT_NUMERIC ) ? ( ADSHANDLE ) hb_itemGetNInt( p ) : hb_ads_getConnection() )
#define HB_ADS_PUTCONNECTION( p, h )   hb_itemPutNInt( ( p ), ( ADSHANDLE ) ( h ) )
#define HB_ADS_DEFCONNECTION( h, s )   hb_ads_defConnection( ( h ), ( s ) )

extern int        hb_ads_iFileType; /* current global setting */
extern int        hb_ads_iLockType;
extern int        hb_ads_iCheckRights;
extern int        hb_ads_iCharType;
extern HB_BOOL    hb_ads_bTestRecLocks;

extern ADSHANDLE  hb_ads_getConnection( void );
extern ADSHANDLE  hb_ads_defConnection( ADSHANDLE hConnect, const char * szName );
extern void       hb_ads_setConnection( ADSHANDLE hConnect );

extern HB_ERRCODE hb_adsCloseCursor( ADSAREAP pArea );
extern ADSAREAP   hb_adsGetWorkAreaPointer( void );

#ifdef ADS_USE_OEM_TRANSLATION
   extern HB_BOOL hb_ads_bOEM;
   extern char *  hb_adsOemToAnsi( const char * pcString, HB_SIZE ulLen );
   extern char *  hb_adsAnsiToOem( const char * pcString, HB_SIZE ulLen );
   extern void    hb_adsOemAnsiFree( char * pcString );

   /* NOTE: Undocumented ACE function. */
   UNSIGNED32 ENTRYPOINT AdsSetFieldRaw( ADSHANDLE   hObj,
                                         UNSIGNED8 * pucFldName,
                                         UNSIGNED8 * pucBuf,
                                         UNSIGNED32  ulLen );

   /* NOTE: Undocumented ACE function. */
   UNSIGNED32 ENTRYPOINT AdsGetFieldRaw( ADSHANDLE    hTbl,
                                         UNSIGNED8 *  pucFldName,
                                         UNSIGNED8 *  pucBuf,
                                         UNSIGNED32 * pulLen );

#else
#  define hb_adsOemToAnsi( s, l )     ( ( char * ) ( s ) )
#  define hb_adsAnsiToOem( s, l )     ( ( char * ) ( s ) )
#  define hb_adsOemAnsiFree( s )
#endif

HB_EXTERN_END
