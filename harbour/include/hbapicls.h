/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Harbour class API
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

#ifndef HB_APICLS_H_
#define HB_APICLS_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

#define HB_OO_OP_PLUS         0
#define HB_OO_OP_MINUS        1
#define HB_OO_OP_MULT         2
#define HB_OO_OP_DIVIDE       3
#define HB_OO_OP_MOD          4
#define HB_OO_OP_POWER        5
#define HB_OO_OP_INC          6
#define HB_OO_OP_DEC          7
#define HB_OO_OP_EQUAL        8
#define HB_OO_OP_EXACTEQUAL   9
#define HB_OO_OP_NOTEQUAL     10
#define HB_OO_OP_LESS         11
#define HB_OO_OP_LESSEQUAL    12
#define HB_OO_OP_GREATER      13
#define HB_OO_OP_GREATEREQUAL 14
#define HB_OO_OP_ASSIGN       15
#define HB_OO_OP_INSTRING     16
#define HB_OO_OP_NOT          17
#define HB_OO_OP_AND          18
#define HB_OO_OP_OR           19
#define HB_OO_OP_ARRAYINDEX   20
#define HB_OO_OP_ENUMNEXT     21
#define HB_OO_OP_ENUMPREV     22
#define HB_OO_OP_ENUMINDEX    23
#define HB_OO_OP_ENUMBASE     24
#define HB_OO_OP_ENUMVALUE    25

#define HB_OO_MAX_OPERATOR    25

/* class management */
extern void       hb_clsInit( void );           /* initialize Classy/OO system at HVM startup */
extern void       hb_clsReleaseAll( void );     /* releases all defined classes */
extern void       hb_clsIsClassRef( void );     /* classes.c - mark all class internals as used */
/* has this function to be public? */
extern BOOL       hb_clsIsParent( USHORT uiClass, char * szParentName ); /* is a class handle inherited from szParentName Class ? */

/* object management */
#ifdef _HB_API_INTERNAL_
extern PHB_SYMB   hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pSymMsg, BOOL * pfPopSuper ); /* returns the method pointer of an object class */
extern void       hb_objPopSuperCast( PHB_ITEM pObject ); /* clean super casting if necessary */
#endif

extern BOOL       hb_objHasOperator( PHB_ITEM pObject, USHORT uiOperator );
extern BOOL       hb_objOperatorCall( USHORT uiOperator, HB_ITEM_PTR pResult, PHB_ITEM pObject, PHB_ITEM pMsgArg );

extern USHORT     hb_objGetClass( PHB_ITEM pItem );      /* get object class handle */
extern char *     hb_objGetClsName( PHB_ITEM pObject );  /* retrieves an object class name */
extern char *     hb_objGetRealClsName( PHB_ITEM pObject, char * szString  ); /* retrieves an object class name for a specific message */

extern BOOL       hb_objHasMsg( PHB_ITEM pObject, char * szString ); /* returns TRUE/FALSE whether szString is an existing message for object */
extern BOOL       hb_objHasMessage( PHB_ITEM pObject, PHB_DYNS pMessage );
extern void       hb_objSendMsg( PHB_ITEM pObj, char *sMsg, ULONG ulArg, ... );

#ifndef HB_NO_PROFILER
/* profiler for object management */
extern BOOL       hb_bProfiler;                       /* profiler activity status */
extern void *     hb_mthRequested( void );            /* profiler from classes.c */
extern void       hb_mthAddTime( void *, ULONG );     /* profiler from classes.c */
#endif

HB_EXTERN_END

#endif /* HB_APICLS_H_ */
