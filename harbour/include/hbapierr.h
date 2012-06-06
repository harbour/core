/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Error API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef HB_APIERR_H_
#define HB_APIERR_H_

#include "hbapi.h"
#include "error.ch"

HB_EXTERN_BEGIN

/* Error codes (returned from hb_errLaunch()) */

#define E_BREAK                         0xFFFF
#define E_RETRY                         1
#define E_DEFAULT                       0

/* Error flags */

#define EF_NONE                         0
#define EF_CANRETRY                     1
#define EF_CANSUBSTITUTE                2
#define EF_CANDEFAULT                   4

/* oError:Severity */

/* ... defined in error.ch */

/* oError:SubSystem (commonly used) */

#define HB_ERR_SS_BASE                  "BASE"
#define HB_ERR_SS_TERMINAL              "TERM"
#define HB_ERR_SS_DBCMD                 "DBCMD"

/* oError:GenCode */

/* ... defined in extend.ch */

/* Internal error numbers */

#define HB_ERR_IE_NOT_ENOUGH_MEM        1024
#define HB_ERR_IE_ERR_RECOV_FAIL        1025
#define HB_ERR_IE_UNREC_ERROR           1026
#define HB_ERR_IE_GENERIC               1027

#define HB_ERR_ARGS_BASEPARAMS          0xFFFFFFFF
#define HB_ERR_ARGS_SELFPARAMS          0xFFFFFFFE

#define HB_ERR_FUNCNAME                 ( ( const char * ) ( HB_PTRDIFF ) 1 )

/* Standard API */

extern HB_EXPORT PHB_ITEM     hb_errGetCargo          ( PHB_ITEM pError );
extern HB_EXPORT PHB_ITEM     hb_errGetArgs           ( PHB_ITEM pError );
extern HB_EXPORT const char * hb_errGetDescription    ( PHB_ITEM pError );
extern HB_EXPORT const char * hb_errGetFileName       ( PHB_ITEM pError );
extern HB_EXPORT HB_USHORT    hb_errGetFlags          ( PHB_ITEM pError );
extern HB_EXPORT HB_ERRCODE   hb_errGetGenCode        ( PHB_ITEM pError );
extern HB_EXPORT const char * hb_errGetOperation      ( PHB_ITEM pError );
extern HB_EXPORT HB_ERRCODE   hb_errGetOsCode         ( PHB_ITEM pError );
extern HB_EXPORT HB_USHORT    hb_errGetSeverity       ( PHB_ITEM pError );
extern HB_EXPORT HB_ERRCODE   hb_errGetSubCode        ( PHB_ITEM pError );
extern HB_EXPORT const char * hb_errGetSubSystem      ( PHB_ITEM pError );
extern HB_EXPORT HB_USHORT    hb_errGetTries          ( PHB_ITEM pError );
extern HB_EXPORT HB_USHORT    hb_errLaunch            ( PHB_ITEM pError );
extern HB_EXPORT PHB_ITEM     hb_errNew               ( void );
extern HB_EXPORT PHB_ITEM     hb_errPutCargo          ( PHB_ITEM pError, PHB_ITEM pCargo );
extern HB_EXPORT PHB_ITEM     hb_errPutArgsArray      ( PHB_ITEM pError, PHB_ITEM pArgs );
extern HB_EXPORT PHB_ITEM     hb_errPutArgs           ( PHB_ITEM pError, HB_ULONG ulArgCount, ... );
extern HB_EXPORT PHB_ITEM     hb_errPutDescription    ( PHB_ITEM pError, const char * szDescription );
extern HB_EXPORT PHB_ITEM     hb_errPutFileName       ( PHB_ITEM pError, const char * szFileName );
extern HB_EXPORT PHB_ITEM     hb_errPutFlags          ( PHB_ITEM pError, HB_USHORT uiFlags );
extern HB_EXPORT PHB_ITEM     hb_errPutGenCode        ( PHB_ITEM pError, HB_ERRCODE uiGenCode );
extern HB_EXPORT PHB_ITEM     hb_errPutOperation      ( PHB_ITEM pError, const char * szOperation );
extern HB_EXPORT PHB_ITEM     hb_errPutOsCode         ( PHB_ITEM pError, HB_ERRCODE uiOsCode );
extern HB_EXPORT PHB_ITEM     hb_errPutSeverity       ( PHB_ITEM pError, HB_USHORT uiSeverity );
extern HB_EXPORT PHB_ITEM     hb_errPutSubCode        ( PHB_ITEM pError, HB_ERRCODE uiSubCode );
extern HB_EXPORT PHB_ITEM     hb_errPutSubSystem      ( PHB_ITEM pError, const char * szSubSystem );
extern HB_EXPORT PHB_ITEM     hb_errPutTries          ( PHB_ITEM pError, HB_USHORT uiTries );
extern HB_EXPORT void         hb_errRelease           ( PHB_ITEM pError );

/* Harbour additions */

extern void     hb_errInit              ( void );
extern void     hb_errExit              ( void );

extern HB_EXPORT PHB_ITEM  hb_errLaunchSubst( PHB_ITEM pError );

extern HB_EXPORT PHB_ITEM  hb_errRT_New( HB_USHORT uiSeverity,
                                         const char * szSubSystem,
                                         HB_ERRCODE errGenCode,
                                         HB_ERRCODE errSubCode,
                                         const char * szDescription,
                                         const char * szOperation,
                                         HB_ERRCODE uiOsCode,
                                         HB_USHORT uiFlags );

extern HB_EXPORT PHB_ITEM  hb_errRT_New_Subst( HB_USHORT uiSeverity,
                                         const char * szSubSystem,
                                         HB_ERRCODE errGenCode,
                                         HB_ERRCODE errSubCode,
                                         const char * szDescription,
                                         const char * szOperation,
                                         HB_ERRCODE uiOsCode,
                                         HB_USHORT uiFlags );

extern HB_EXPORT PHB_ITEM  hb_errRT_SubstParams( const char * szSubSystem, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation );

extern HB_EXPORT PHB_ITEM hb_errRT_FileError( PHB_ITEM pError, const char * szSubSystem,
                                              HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                                              const char * szFileName );

extern HB_EXPORT HB_USHORT hb_errRT_BASE        ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ULONG ulArgCount, ... );
extern HB_EXPORT HB_USHORT hb_errRT_BASE_Ext1   ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE uiOsCode, HB_USHORT uiFlags, HB_ULONG ulArgCount, ... );
extern HB_EXPORT PHB_ITEM  hb_errRT_BASE_Subst  ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ULONG ulArgCount, ... );
extern HB_EXPORT void      hb_errRT_BASE_SubstR ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ULONG ulArgCount, ... );
extern HB_EXPORT HB_USHORT hb_errRT_TERM        ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE uiOSCode, HB_USHORT uiFlags );
extern HB_EXPORT HB_USHORT hb_errRT_DBCMD       ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation );
extern HB_EXPORT HB_USHORT hb_errRT_DBCMD_Ext   ( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_USHORT uiFlags );

extern HB_EXPORT void      hb_errInternal       ( HB_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 ) HB_NORETURN_ATTR;
extern           void      hb_errInternalRaw    ( HB_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 );

/* Low-level error handling */
struct HB_ERROR_INFO_;   /* forward declaration */
#define HB_ERROR_HANDLE( hbfunc )   PHB_ITEM hbfunc( struct HB_ERROR_INFO_ * ErrorInfo )
typedef HB_ERROR_HANDLE( HB_ERROR_HANDLER );
typedef HB_ERROR_HANDLER * HB_ERROR_HANDLER_PTR;

typedef struct HB_ERROR_INFO_
{
   HB_ERROR_HANDLER_PTR Func;
   PHB_ITEM Error;
   void * Cargo;
   struct HB_ERROR_INFO_ * Previous;
   PHB_ITEM ErrorBlock;
} HB_ERROR_INFO, * HB_ERROR_INFO_PTR;

/* set/get current error handler */
extern HB_EXPORT HB_ERROR_INFO_PTR  hb_errorHandler( HB_ERROR_INFO_PTR pNewHandler );

/* current errorblock item */
extern HB_EXPORT PHB_ITEM hb_errorBlock( void );

HB_EXTERN_END

#endif /* HB_APIERR_H_ */
