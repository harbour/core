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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

/*
 *  enum Direction { TopToBottom, BottomToTop }
 */

#include <QtCore/QPointer>

#include <QtGui/QProgressBar>


/*
 * QProgressBar ( QWidget * parent = 0 )
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QProgressBar > pq;
} QGC_POINTER_QProgressBar;

QT_G_FUNC( hbqt_gcRelease_QProgressBar )
{
   QGC_POINTER_QProgressBar * p = ( QGC_POINTER_QProgressBar * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QProgressBar                 p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QProgressBar                ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QProgressBar * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QProgressBar * ) p->ph )->~QProgressBar();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QProgressBar * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QProgressBar                Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QProgressBar                Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QProgressBar                Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QProgressBar( void * pObj )
{
   QGC_POINTER_QProgressBar * p = ( QGC_POINTER_QProgressBar * ) hb_gcAllocate( sizeof( QGC_POINTER_QProgressBar ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QProgressBar;
   new( & p->pq ) QPointer< QProgressBar >( ( QProgressBar * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QProgressBar                %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QPROGRESSBAR )
{
   void * pObj = NULL;

   pObj = ( QProgressBar* ) new QProgressBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressBar( pObj ) );
}
/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QPROGRESSBAR_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QProgressBar( 1 )->alignment() );
}

/*
 * QString format () const
 */
HB_FUNC( QT_QPROGRESSBAR_FORMAT )
{
   hb_retc( hbqt_par_QProgressBar( 1 )->format().toAscii().data() );
}

/*
 * bool invertedAppearance ()
 */
HB_FUNC( QT_QPROGRESSBAR_INVERTEDAPPEARANCE )
{
   hb_retl( hbqt_par_QProgressBar( 1 )->invertedAppearance() );
}

/*
 * bool isTextVisible () const
 */
HB_FUNC( QT_QPROGRESSBAR_ISTEXTVISIBLE )
{
   hb_retl( hbqt_par_QProgressBar( 1 )->isTextVisible() );
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QPROGRESSBAR_MAXIMUM )
{
   hb_retni( hbqt_par_QProgressBar( 1 )->maximum() );
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QPROGRESSBAR_MINIMUM )
{
   hb_retni( hbqt_par_QProgressBar( 1 )->minimum() );
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QPROGRESSBAR_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QProgressBar( 1 )->orientation() );
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QPROGRESSBAR_SETALIGNMENT )
{
   hbqt_par_QProgressBar( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setFormat ( const QString & format )
 */
HB_FUNC( QT_QPROGRESSBAR_SETFORMAT )
{
   hbqt_par_QProgressBar( 1 )->setFormat( QProgressBar::tr( hb_parc( 2 ) ) );
}

/*
 * void setInvertedAppearance ( bool invert )
 */
HB_FUNC( QT_QPROGRESSBAR_SETINVERTEDAPPEARANCE )
{
   hbqt_par_QProgressBar( 1 )->setInvertedAppearance( hb_parl( 2 ) );
}

/*
 * void setTextDirection ( QProgressBar::Direction textDirection )
 */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTDIRECTION )
{
   hbqt_par_QProgressBar( 1 )->setTextDirection( ( QProgressBar::Direction ) hb_parni( 2 ) );
}

/*
 * void setTextVisible ( bool visible )
 */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTVISIBLE )
{
   hbqt_par_QProgressBar( 1 )->setTextVisible( hb_parl( 2 ) );
}

/*
 * virtual QString text () const
 */
HB_FUNC( QT_QPROGRESSBAR_TEXT )
{
   hb_retc( hbqt_par_QProgressBar( 1 )->text().toAscii().data() );
}

/*
 * QProgressBar::Direction textDirection ()
 */
HB_FUNC( QT_QPROGRESSBAR_TEXTDIRECTION )
{
   hb_retni( ( QProgressBar::Direction ) hbqt_par_QProgressBar( 1 )->textDirection() );
}

/*
 * int value () const
 */
HB_FUNC( QT_QPROGRESSBAR_VALUE )
{
   hb_retni( hbqt_par_QProgressBar( 1 )->value() );
}

/*
 * void reset ()
 */
HB_FUNC( QT_QPROGRESSBAR_RESET )
{
   hbqt_par_QProgressBar( 1 )->reset();
}

/*
 * void setMaximum ( int maximum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETMAXIMUM )
{
   hbqt_par_QProgressBar( 1 )->setMaximum( hb_parni( 2 ) );
}

/*
 * void setMinimum ( int minimum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETMINIMUM )
{
   hbqt_par_QProgressBar( 1 )->setMinimum( hb_parni( 2 ) );
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QPROGRESSBAR_SETORIENTATION )
{
   hbqt_par_QProgressBar( 1 )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETRANGE )
{
   hbqt_par_QProgressBar( 1 )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setValue ( int value )
 */
HB_FUNC( QT_QPROGRESSBAR_SETVALUE )
{
   hbqt_par_QProgressBar( 1 )->setValue( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
