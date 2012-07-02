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
#include "hbthread.h"

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

typedef void ( * PHBQT_SLOT_FUNC )( PHB_ITEM * codeblock, void ** arguments, QStringList pList );
typedef void * ( * PHBQT_EVENT_FUNC )( void * pObj, bool bNew );
typedef void ( * PHBQT_DEL_FUNC )( void * pObj, int iFlags );

#define HBQT_BIT_NONE                             0
#define HBQT_BIT_OWNER                            1
#define HBQT_BIT_QOBJECT                          2
#define HBQT_BIT_CONSTRUCTOR                      4
#define HBQT_BIT_DESTRUCTOR                       8
#define HBQT_BIT_QPOINTER                         16

HB_EXTERN_BEGIN

HB_EXPORT void      hbqt_events_register_createobj( QEvent::Type eventtype, QByteArray szCreateObj );
HB_EXPORT void      hbqt_events_unregister_createobj( QEvent::Type eventtype );
HB_EXPORT void      hbqt_slots_register_callback( QByteArray sig, PHBQT_SLOT_FUNC pCallback );
HB_EXPORT void      hbqt_slots_unregister_callback( QByteArray sig );

HB_EXPORT void *    hbqt_par_ptr( int iParam );
HB_EXPORT void      hbqt_par_detach_ptrGC( int iParam );
HB_EXPORT HB_BOOL   hbqt_par_isDerivedFrom( int iParam, const char * pszClsName ); /* check if parameter iParam is class or subclass of szClsName */
HB_EXPORT HB_BOOL   hbqt_obj_isDerivedFrom( PHB_ITEM pItem, const char * pszClsName ); /* check if parameter iParam is class or subclass of szClsName */
HB_EXPORT void *    hbqt_get_ptr( PHB_ITEM pObj );

HB_EXPORT void      hbqt_errRT_ARG( void );
HB_EXPORT PHB_ITEM  hbqt_defineClassBegin( const char * pszClsName, PHB_ITEM s_oClass, const char * pszParentClsStr );
HB_EXPORT void      hbqt_defineClassEnd( PHB_ITEM s_oClass, PHB_ITEM oClass );

HB_EXPORT PHB_ITEM  hbqt_bindGetHbObject( PHB_ITEM pItem, void * qtObject, const char * szClassFunc, PHBQT_DEL_FUNC pDelete, int iFlags );
HB_EXPORT PHB_ITEM  hbqt_bindSetHbObject( PHB_ITEM pItem, void * qtObject, const char * szClassName, PHBQT_DEL_FUNC pDelFunc, int iFlags );
HB_EXPORT PHB_ITEM  hbqt_bindGetHbObjectByQtObject( void * qtObject );
HB_EXPORT void *    hbqt_bindGetQtObject( PHB_ITEM pObject );
HB_EXPORT void      hbqt_bindSetOwner( void * qtObject, HB_BOOL fOwner );
HB_EXPORT void      hbqt_bindDestroyHbObject( PHB_ITEM pObject );
HB_EXPORT void      hbqt_bindDestroyQtObject( void * qtObject, QObject * obj );

HB_EXPORT void      hbqt_bindAddChild( PHB_ITEM pObject, PHB_ITEM pChild );
HB_EXPORT void      hbqt_bindDelChild( PHB_ITEM pObject, PHB_ITEM pChild );

HB_EXPORT void      hbqt_bindAddSlot( PHB_ITEM pSenderObject, int iSignalid, PHB_ITEM pCode );
HB_EXPORT void      hbqt_bindDelSlot( PHB_ITEM pSenderObject, int iSignalid, PHB_ITEM pCode );
HB_EXPORT PHB_ITEM  hbqt_bindGetSlots( PHB_ITEM pSenderObject, int iSignalid );

HB_EXPORT void      hbqt_bindAddEvent( PHB_ITEM pSenderObject, int iEventId, PHB_ITEM pCode );
HB_EXPORT void      hbqt_bindDelEvent( PHB_ITEM pSenderObject, int iEventId, PHB_ITEM pCode );
HB_EXPORT PHB_ITEM  hbqt_bindGetEvents( PHB_ITEM pSenderObject, int iEventId );

HB_EXTERN_END

#define hbqt_par_uchar( n )                         ( ( uchar * ) hb_parcx( n ) )
#define hbqt_par_QRgb( n )                          ( hb_parnint( n ) )
#define hbqt_par_Bool( n )                          ( hb_parl( n ) )
#define hbqt_par_char( n )                          ( hb_parcx( n ) )

#endif /* __HBQT_H */
