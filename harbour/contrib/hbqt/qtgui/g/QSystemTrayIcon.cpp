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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
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

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
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

   pObj = new QSystemTrayIcon( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSystemTrayIcon( ( void * ) pObj, true ) );
}

/*
 * QMenu * contextMenu () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_CONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->contextMenu(), false ) );
   }
}

/*
 * QRect geometry () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_GEOMETRY )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ISVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retl( ( p )->isVisible() );
   }
}

/*
 * void setContextMenu ( QMenu * menu )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETCONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      ( p )->setContextMenu( hbqt_par_QMenu( 2 ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
   }
}

/*
 * void setToolTip ( const QString & tip )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETTOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void showMessage ( const QString & title, const QString & message, MessageIcon icon = Information, int millisecondsTimeoutHint = 10000 )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOWMESSAGE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( QSystemTrayIcon::MessageIcon ) hb_parni( 4 ) : ( QSystemTrayIcon::MessageIcon ) QSystemTrayIcon::Information ), hb_parnidef( 5, 10000 ) );
      hb_strfree( pText );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QSYSTEMTRAYICON_TOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
   }
}

/*
 * bool isSystemTrayAvailable ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_ISSYSTEMTRAYAVAILABLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retl( ( p )->isSystemTrayAvailable() );
   }
}

/*
 * bool supportsMessages ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SUPPORTSMESSAGES )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsMessages() );
   }
}

/*
 * void hide ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_HIDE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      ( p )->hide();
   }
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SETVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      ( p )->setVisible( hb_parl( 2 ) );
   }
}

/*
 * void show ()
 */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOW )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      ( p )->show();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
