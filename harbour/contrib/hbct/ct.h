/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   header for CT3 functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#ifndef _CT_H
#define _CT_H  1

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"
#include "error.ch"

#include "ctstr.h"
#include "ctset.h"

#include "cterror.ch"

#define CT_SUBSYSTEM            "CT"

#define CT_ARGERR_WHOCARES      ES_WHOCARES
#define CT_ARGERR_WARNING       ES_WARNING
#define CT_ARGERR_ERROR         ES_ERROR
#define CT_ARGERR_CATASTROPHIC  ES_CATASTROPHIC
#define CT_ARGERR_IGNORE        -1

HB_EXTERN_BEGIN

/* ct_charlist() modes */
#define CT_CHARLIST_CHARLIST    0
#define CT_CHARLIST_CHARNOLIST  1
#define CT_CHARLIST_CHARHIST    2
#define CT_CHARLIST_CHARSLIST   3

extern void ct_charlist( int iMode );

/* ct_charop() modes */
#define CT_CHAROP_CHARADD       0
#define CT_CHAROP_CHARSUB       1  /* new: character subtraction */
#define CT_CHAROP_CHARAND       2
#define CT_CHAROP_CHARNOT       3
#define CT_CHAROP_CHAROR        4
#define CT_CHAROP_CHARXOR       5
#define CT_CHAROP_CHARSHL       6  /* new: shift left */
#define CT_CHAROP_CHARSHR       7  /* new: shift right */
#define CT_CHAROP_CHARRLL       8  /* new: left rotation */
#define CT_CHAROP_CHARRLR       9  /* new: right rotation */

extern void ct_charop( int iMode );

extern HB_BOOL ct_numParam( int iParam, HB_MAXINT * plNum );

/* CT subsystem error throwing functions */
extern HB_USHORT ct_error( HB_USHORT uiSeverity, HB_ERRCODE ulGenCode, HB_ERRCODE ulSubCode,
                           const char * szDescription, const char * szOperation, HB_ERRCODE uiOsCode, HB_USHORT uiFlags, HB_ULONG uiArgCount, ... );

extern PHB_ITEM ct_error_subst( HB_USHORT uiSeverity, HB_ERRCODE ulGenCode, HB_ERRCODE ulSubCode,
                                const char * szDescription, const char * szOperation, HB_ERRCODE uiOsCode, HB_USHORT uiFlags, HB_ULONG uiArgCount, ... );

/* set argument error behaviour */
extern void ct_setargerrormode( int iMode );
extern int ct_getargerrormode( void );

HB_EXTERN_END

#endif
