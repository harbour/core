/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ActivationReason { Unknown, Context, DoubleClick, Trigger, MiddleClick }
 *  enum MessageIcon { NoIcon, Information, Warning, Critical }
 */

#include <QtCore/QPointer>

#include <QtGui/QSystemTrayIcon>


/*
 * QSystemTrayIcon ( QObject * parent = 0 )
 * QSystemTrayIcon ( const QIcon & icon, QObject * parent = 0 )
 * ~QSystemTrayIcon ()
 */

typedef struct
{
   QPointer< QSystemTrayIcon > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSystemTrayIcon;

HBQT_GC_FUNC( hbqt_gcRelease_QSystemTrayIcon )
{
   QSystemTrayIcon  * ph = NULL ;
   HBQT_GC_T_QSystemTrayIcon * p = ( HBQT_GC_T_QSystemTrayIcon * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSystemTrayIcon   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSystemTrayIcon   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSystemTrayIcon          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSystemTrayIcon    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSystemTrayIcon    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSystemTrayIcon( void * pObj, bool bNew )
{
   HBQT_GC_T_QSystemTrayIcon * p = ( HBQT_GC_T_QSystemTrayIcon * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSystemTrayIcon ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSystemTrayIcon >( ( QSystemTrayIcon * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSystemTrayIcon;
   p->type = HBQT_TYPE_QSystemTrayIcon;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSystemTrayIcon  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSystemTrayIcon", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSYSTEMTRAYICON )
{
   QSystemTrayIcon * pObj = NULL;

   pObj =  new QSystemTrayIcon( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSystemTrayIcon( ( void * ) pObj, true ) );
}

/*
 * QMenu * contextMenu () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_CONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->contextMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_CONTEXTMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->contextMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * QRect geometry () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_GEOMETRY )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ISVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * void setContextMenu ( QMenu * menu )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETCONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setContextMenu( hbqt_par_QMenu( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SETCONTEXTMENU FP=( p )->setContextMenu( hbqt_par_QMenu( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & tip )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETTOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setToolTip( QSystemTrayIcon::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SETTOOLTIP FP=( p )->setToolTip( QSystemTrayIcon::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void showMessage ( const QString & title, const QString & message, MessageIcon icon = Information, int millisecondsTimeoutHint = 10000 )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOWMESSAGE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->showMessage( QSystemTrayIcon::tr( hb_parc( 2 ) ), QSystemTrayIcon::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QSystemTrayIcon::MessageIcon ) hb_parni( 4 ) : ( QSystemTrayIcon::MessageIcon ) QSystemTrayIcon::Information ), hb_parnidef( 5, 10000 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SHOWMESSAGE FP=( p )->showMessage( QSystemTrayIcon::tr( hb_parc( 2 ) ), QSystemTrayIcon::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QSystemTrayIcon::MessageIcon ) hb_parni( 4 ) : ( QSystemTrayIcon::MessageIcon ) QSystemTrayIcon::Information ), hb_parnidef( 5, 10000 ) ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_TOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool isSystemTrayAvailable ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ISSYSTEMTRAYAVAILABLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->isSystemTrayAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_ISSYSTEMTRAYAVAILABLE FP=hb_retl( ( p )->isSystemTrayAvailable() ); p is NULL" ) );
   }
}

/*
 * bool supportsMessages ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SUPPORTSMESSAGES )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->supportsMessages() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SUPPORTSMESSAGES FP=hb_retl( ( p )->supportsMessages() ); p is NULL" ) );
   }
}

/*
 * void hide ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_HIDE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->hide();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_HIDE FP=( p )->hide(); p is NULL" ) );
   }
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void show ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOW )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->show();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSYSTEMTRAYICON_SHOW FP=( p )->show(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
