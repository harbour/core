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

#include <QtCore/QPointer>

#include <QtGui/QProgressDialog>


/*
 * QProgressDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * QProgressDialog ( const QString & labelText, const QString & cancelButtonText, int minimum, int maximum, QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QProgressDialog ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QProgressDialog > pq;
} QGC_POINTER_QProgressDialog;

QT_G_FUNC( release_QProgressDialog )
{
   QGC_POINTER_QProgressDialog * p = ( QGC_POINTER_QProgressDialog * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QProgressDialog              p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QProgressDialog             ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QProgressDialog * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QProgressDialog * ) p->ph )->~QProgressDialog();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QProgressDialog * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QProgressDialog             Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QProgressDialog             Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QProgressDialog             Object Allready deleted!" ) );
   }
}

void * gcAllocate_QProgressDialog( void * pObj )
{
   QGC_POINTER_QProgressDialog * p = ( QGC_POINTER_QProgressDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QProgressDialog ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QProgressDialog;
   new( & p->pq ) QPointer< QProgressDialog >( ( QProgressDialog * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QProgressDialog             %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QPROGRESSDIALOG )
{
   void * pObj = NULL;

   pObj = new QProgressDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QProgressDialog( pObj ) );
}
/*
 * bool autoClose () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_AUTOCLOSE )
{
   hb_retl( hbqt_par_QProgressDialog( 1 )->autoClose() );
}

/*
 * bool autoReset () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_AUTORESET )
{
   hb_retl( hbqt_par_QProgressDialog( 1 )->autoReset() );
}

/*
 * QString labelText () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_LABELTEXT )
{
   hb_retc( hbqt_par_QProgressDialog( 1 )->labelText().toAscii().data() );
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MAXIMUM )
{
   hb_retni( hbqt_par_QProgressDialog( 1 )->maximum() );
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUM )
{
   hb_retni( hbqt_par_QProgressDialog( 1 )->minimum() );
}

/*
 * int minimumDuration () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUMDURATION )
{
   hb_retni( hbqt_par_QProgressDialog( 1 )->minimumDuration() );
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QPROGRESSDIALOG_OPEN )
{
   hbqt_par_QProgressDialog( 1 )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
}

/*
 * void setAutoClose ( bool close )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTOCLOSE )
{
   hbqt_par_QProgressDialog( 1 )->setAutoClose( hb_parl( 2 ) );
}

/*
 * void setAutoReset ( bool reset )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTORESET )
{
   hbqt_par_QProgressDialog( 1 )->setAutoReset( hb_parl( 2 ) );
}

/*
 * void setBar ( QProgressBar * bar )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETBAR )
{
   hbqt_par_QProgressDialog( 1 )->setBar( hbqt_par_QProgressBar( 2 ) );
}

/*
 * void setCancelButton ( QPushButton * cancelButton )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTON )
{
   hbqt_par_QProgressDialog( 1 )->setCancelButton( hbqt_par_QPushButton( 2 ) );
}

/*
 * void setLabel ( QLabel * label )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABEL )
{
   hbqt_par_QProgressDialog( 1 )->setLabel( hbqt_par_QLabel( 2 ) );
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_SIZEHINT )
{
   hb_retptrGC( gcAllocate_QSize( new QSize( hbqt_par_QProgressDialog( 1 )->sizeHint() ) ) );
}

/*
 * int value () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_VALUE )
{
   hb_retni( hbqt_par_QProgressDialog( 1 )->value() );
}

/*
 * bool wasCanceled () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_WASCANCELED )
{
   hb_retl( hbqt_par_QProgressDialog( 1 )->wasCanceled() );
}

/*
 * void cancel ()
 */
HB_FUNC( QT_QPROGRESSDIALOG_CANCEL )
{
   hbqt_par_QProgressDialog( 1 )->cancel();
}

/*
 * void reset ()
 */
HB_FUNC( QT_QPROGRESSDIALOG_RESET )
{
   hbqt_par_QProgressDialog( 1 )->reset();
}

/*
 * void setCancelButtonText ( const QString & cancelButtonText )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTONTEXT )
{
   hbqt_par_QProgressDialog( 1 )->setCancelButtonText( hbqt_par_QString( 2 ) );
}

/*
 * void setLabelText ( const QString & text )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABELTEXT )
{
   hbqt_par_QProgressDialog( 1 )->setLabelText( hbqt_par_QString( 2 ) );
}

/*
 * void setMaximum ( int maximum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMAXIMUM )
{
   hbqt_par_QProgressDialog( 1 )->setMaximum( hb_parni( 2 ) );
}

/*
 * void setMinimum ( int minimum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUM )
{
   hbqt_par_QProgressDialog( 1 )->setMinimum( hb_parni( 2 ) );
}

/*
 * void setMinimumDuration ( int ms )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUMDURATION )
{
   hbqt_par_QProgressDialog( 1 )->setMinimumDuration( hb_parni( 2 ) );
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETRANGE )
{
   hbqt_par_QProgressDialog( 1 )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setValue ( int progress )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETVALUE )
{
   hbqt_par_QProgressDialog( 1 )->setValue( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
