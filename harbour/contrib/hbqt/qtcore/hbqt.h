/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#ifndef __HBQT_H
#define __HBQT_H

#include "hbapi.h"
#include "hbapistr.h"

#if defined( HB_OS_OS2 )
#  define OS2EMX_PLAIN_CHAR
#  define INCL_BASE
#  define INCL_PM
#  include <os2.h>
#endif

#include <QtCore/qglobal.h>
#include <QtCore/QEvent>
#include <QtCore/QStringList>

#if !( QT_VERSION >= 0x040500 )
#  error QT library version 4.5.0 or upper is required for hbqt.
#endif

#define HBQT_GC_FUNC( hbfunc )   void hbfunc( void * Cargo ) /* callback function for cleaning garbage memory pointer */
typedef HBQT_GC_FUNC( HBQT_GC_FUNC_ );
typedef HBQT_GC_FUNC_ * PHBQT_GC_FUNC;

typedef struct
{
   void * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   HB_U32 type;
} HBQT_GC_T;

typedef void ( * PHBQT_SLOT_FUNC )( PHB_ITEM * codeblock, void ** arguments, QStringList pList );
typedef void * ( * PHBQT_EVENT_FUNC )( void * pObj, bool bNew );

HB_EXTERN_BEGIN

extern HB_EXPORT void hbqt_events_register_createobj( QEvent::Type eventtype, QByteArray szCreateObj, PHBQT_EVENT_FUNC pCallback );
extern HB_EXPORT void hbqt_events_unregister_createobj( QEvent::Type eventtype );
extern HB_EXPORT void hbqt_slots_register_callback( QByteArray sig, PHBQT_SLOT_FUNC pCallback );
extern HB_EXPORT void hbqt_slots_unregister_callback( QByteArray sig );
extern HB_EXPORT void * hbqt_gcpointer( int iParam );
extern HB_EXPORT void * hbqt_detachgcpointer( int iParam );
extern HB_EXPORT const HB_GC_FUNCS * hbqt_gcFuncs( void );
extern HB_EXPORT void * hbqt_pPtrFromObj( int iParam );
extern HB_EXPORT int hbqt_isObjectType( int iParam, HB_U32 iType ); /* check if parameter iParam is EXACTLY of type iType */
extern HB_EXPORT void hbqt_errRT_ARG( void );
extern HB_EXPORT PHB_ITEM hbqt_defineClassBegin( const char * szClsName, PHB_ITEM s_oClass, const char * szParentClsStr );
extern HB_EXPORT void hbqt_defineClassEnd( PHB_ITEM s_oClass, PHB_ITEM oClass );
extern HB_EXPORT void * hbqt_getqtptr( void );
extern HB_EXPORT PHB_ITEM hbqt_create_object( void * pObject, const char * pszObjectName );
extern HB_EXPORT PHB_ITEM hbqt_create_objectGC( void * pObject, const char * pszObjectName );
extern HB_EXPORT void hbqt_itemPushReturn( void * ptr, PHB_ITEM pSelf );
extern HB_EXPORT void hbqt_set_pptr( void * ptr, PHB_ITEM pSelf );
extern HB_EXPORT HBQT_GC_T * hbqt_getObjectGC( int iParam ); /* returns a pointer to the HBQT_GC_T area */
extern HB_EXPORT void hbqt_addDeleteList( PHB_ITEM item ); /* populate a list of PHB_ITEM to delete at exit time */

HB_EXTERN_END

#define hbqt_par_QString( n )                       ( ( QString ) hb_parcx( n ) )
#define hbqt_par_uchar( n )                         ( ( uchar * ) hb_parcx( n ) )
#define hbqt_par_QRgb( n )                          ( hb_parnint( n ) )
#define hbqt_par_Bool( n )                          ( hb_parl( n ) )
#define hbqt_par_char( n )                          ( hb_parcx( n ) )

#endif /* __HBQT_H */
