/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Error API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_APIERR_H_
#define HB_APIERR_H_

#include "hbapi.h"
#include "error.ch"

#if defined(__cplusplus)
//extern "C" {
#endif

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

/* ... defined in extend.ch */

/* oError:SubSystem (commonly used) */

#define HB_ERR_SS_BASE                  "BASE"
#define HB_ERR_SS_TERMINAL              "TERM"
#define HB_ERR_SS_DBCMD                 "DBCMD"
#define HB_ERR_SS_TOOLS                 "TOOLS"

/* oError:GenCode */

/* ... defined in extend.ch */

/* Internal error numbers */

#define HB_ERR_IE_NOT_ENOUGH_MEM        1024
#define HB_ERR_IE_ERR_RECOV_FAIL        1025
#define HB_ERR_IE_UNREC_ERROR           1026
#define HB_ERR_IE_GENERIC               1027

/* Standard API */

extern char *   hb_errGetDescription    ( PHB_ITEM pError );
extern char *   hb_errGetFileName       ( PHB_ITEM pError );
extern USHORT   hb_errGetFlags          ( PHB_ITEM pError );
extern USHORT   hb_errGetGenCode        ( PHB_ITEM pError );
extern char *   hb_errGetOperation      ( PHB_ITEM pError );
extern USHORT   hb_errGetOsCode         ( PHB_ITEM pError );
extern USHORT   hb_errGetSeverity       ( PHB_ITEM pError );
extern USHORT   hb_errGetSubCode        ( PHB_ITEM pError );
extern char *   hb_errGetSubSystem      ( PHB_ITEM pError );
extern USHORT   hb_errGetTries          ( PHB_ITEM pError );
extern USHORT   hb_errLaunch            ( PHB_ITEM pError );
extern PHB_ITEM hb_errNew               ( void );
extern PHB_ITEM hb_errPutArgs           ( PHB_ITEM pError, USHORT uiArgCount, ... );
extern PHB_ITEM hb_errPutDescription    ( PHB_ITEM pError, char * szDescription );
extern PHB_ITEM hb_errPutFileName       ( PHB_ITEM pError, char * szFileName );
extern PHB_ITEM hb_errPutFlags          ( PHB_ITEM pError, USHORT uiFlags );
extern PHB_ITEM hb_errPutGenCode        ( PHB_ITEM pError, USHORT uiGenCode );
extern PHB_ITEM hb_errPutOperation      ( PHB_ITEM pError, char * szOperation );
extern PHB_ITEM hb_errPutOsCode         ( PHB_ITEM pError, USHORT uiOsCode );
extern PHB_ITEM hb_errPutSeverity       ( PHB_ITEM pError, USHORT uiSeverity );
extern PHB_ITEM hb_errPutSubCode        ( PHB_ITEM pError, USHORT uiSubCode );
extern PHB_ITEM hb_errPutSubSystem      ( PHB_ITEM pError, char * szSubSystem );
extern PHB_ITEM hb_errPutTries          ( PHB_ITEM pError, USHORT uiTries );
extern void     hb_errRelease           ( PHB_ITEM pError );

/* Harbour additions */

extern void     hb_errInit              ( void );
extern void     hb_errExit              ( void );

extern PHB_ITEM hb_errLaunchSubst       ( PHB_ITEM pError );

extern PHB_ITEM hb_errRT_New( USHORT uiSeverity, char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags );

extern PHB_ITEM hb_errRT_New_Subst( USHORT uiSeverity, char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags );

extern USHORT   hb_errRT_BASE           ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );
extern USHORT   hb_errRT_BASE_Ext1      ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOsCode, USHORT uiFlags );
extern PHB_ITEM hb_errRT_BASE_Subst     ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );
extern USHORT   hb_errRT_TERM           ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOSCode, USHORT uiFlags );
extern USHORT   hb_errRT_DBCMD          ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );
extern USHORT   hb_errRT_TOOLS          ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );

extern void     hb_errInternal          ( ULONG ulIntCode, char * szText, char * szPar1, char * szPar2 );

/* Low-level error handling */
struct HB_ERROR_INFO_;   /* forward declaration */
#define HB_ERROR_HANDLE( hbfunc )   HB_ITEM_PTR hbfunc( struct HB_ERROR_INFO_ * ErrorInfo )
typedef HB_ERROR_HANDLE( HB_ERROR_HANDLER );
typedef HB_ERROR_HANDLER *HB_ERROR_HANDLER_PTR;

typedef struct HB_ERROR_INFO_
{
   HB_ERROR_HANDLER_PTR Func;
   HB_ITEM_PTR Error;
   void * Cargo;
   struct HB_ERROR_INFO_ *Previous;
   HB_ITEM_PTR ErrorBlock;
} HB_ERROR_INFO, *HB_ERROR_INFO_PTR;

/*  set/get current error handler */
extern HB_ERROR_INFO_PTR hb_errorHandler( HB_ERROR_INFO_PTR );

#if defined(__cplusplus)
//}
#endif

#endif /* HB_APIERR_H_ */
