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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QAbstractButton>


/*
 * QAbstractButton ( QWidget * parent = 0 )
 * ~QAbstractButton ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QAbstractButton > pq;
} QGC_POINTER_QAbstractButton;

QT_G_FUNC( hbqt_gcRelease_QAbstractButton )
{
   HB_SYMBOL_UNUSED( Cargo );
}

void * hbqt_gcAllocate_QAbstractButton( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractButton * p = ( QGC_POINTER_QAbstractButton * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractButton ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractButton;

   if( bNew )
   {
      new( & p->pq ) QPointer< QAbstractButton >( ( QAbstractButton * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QAbstractButton            ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTBUTTON )
{
}

/*
 * bool autoExclusive () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOEXCLUSIVE )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->autoExclusive() );
}

/*
 * bool autoRepeat () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEAT )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->autoRepeat() );
}

/*
 * int autoRepeatDelay () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATDELAY )
{
   hb_retni( hbqt_par_QAbstractButton( 1 )->autoRepeatDelay() );
}

/*
 * int autoRepeatInterval () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATINTERVAL )
{
   hb_retni( hbqt_par_QAbstractButton( 1 )->autoRepeatInterval() );
}

/*
 * QButtonGroup * group () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_GROUP )
{
   hb_retptrGC( hbqt_gcAllocate_QButtonGroup( hbqt_par_QAbstractButton( 1 )->group(), false ) );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_ICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QAbstractButton( 1 )->icon() ), true ) );
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_ICONSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QAbstractButton( 1 )->iconSize() ), true ) );
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKABLE )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isCheckable() );
}

/*
 * bool isChecked () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKED )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isChecked() );
}

/*
 * bool isDown () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_ISDOWN )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isDown() );
}

/*
 * void setAutoExclusive ( bool )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOEXCLUSIVE )
{
   hbqt_par_QAbstractButton( 1 )->setAutoExclusive( hb_parl( 2 ) );
}

/*
 * void setAutoRepeat ( bool )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEAT )
{
   hbqt_par_QAbstractButton( 1 )->setAutoRepeat( hb_parl( 2 ) );
}

/*
 * void setAutoRepeatDelay ( int )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATDELAY )
{
   hbqt_par_QAbstractButton( 1 )->setAutoRepeatDelay( hb_parni( 2 ) );
}

/*
 * void setAutoRepeatInterval ( int )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATINTERVAL )
{
   hbqt_par_QAbstractButton( 1 )->setAutoRepeatInterval( hb_parni( 2 ) );
}

/*
 * void setCheckable ( bool )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKABLE )
{
   hbqt_par_QAbstractButton( 1 )->setCheckable( hb_parl( 2 ) );
}

/*
 * void setDown ( bool )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETDOWN )
{
   hbqt_par_QAbstractButton( 1 )->setDown( hb_parl( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETICON )
{
   hbqt_par_QAbstractButton( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setShortcut ( const QKeySequence & key )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETSHORTCUT )
{
   hbqt_par_QAbstractButton( 1 )->setShortcut( *hbqt_par_QKeySequence( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETTEXT )
{
   hbqt_par_QAbstractButton( 1 )->setText( QAbstractButton::tr( hb_parc( 2 ) ) );
}

/*
 * QKeySequence shortcut () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_SHORTCUT )
{
   hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( hbqt_par_QAbstractButton( 1 )->shortcut() ), true ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QABSTRACTBUTTON_TEXT )
{
   hb_retc( hbqt_par_QAbstractButton( 1 )->text().toAscii().data() );
}

/*
 * void animateClick ( int msec = 100 )
 */
HB_FUNC( QT_QABSTRACTBUTTON_ANIMATECLICK )
{
   hbqt_par_QAbstractButton( 1 )->animateClick( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 100 ) );
}

/*
 * void click ()
 */
HB_FUNC( QT_QABSTRACTBUTTON_CLICK )
{
   hbqt_par_QAbstractButton( 1 )->click();
}

/*
 * void setChecked ( bool )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKED )
{
   hbqt_par_QAbstractButton( 1 )->setChecked( hb_parl( 2 ) );
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QABSTRACTBUTTON_SETICONSIZE )
{
   hbqt_par_QAbstractButton( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void toggle ()
 */
HB_FUNC( QT_QABSTRACTBUTTON_TOGGLE )
{
   hbqt_par_QAbstractButton( 1 )->toggle();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
