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
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QProgressDialog > pq;
} QGC_POINTER_QProgressDialog;

QT_G_FUNC( hbqt_gcRelease_QProgressDialog )
{
   QGC_POINTER_QProgressDialog * p = ( QGC_POINTER_QProgressDialog * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QProgressDialog   /.\\   pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QProgressDialog * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QProgressDialog   \\./   pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QProgressDialog          pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QProgressDialog    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QProgressDialog    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProgressDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QProgressDialog * p = ( QGC_POINTER_QProgressDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QProgressDialog ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProgressDialog;

   if( bNew )
   {
      new( & p->pq ) QPointer< QProgressDialog >( ( QProgressDialog * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QProgressDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QProgressDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPROGRESSDIALOG )
{
   void * pObj = NULL;

   pObj = new QProgressDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressDialog( pObj, true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QProgressDialog( 1 )->sizeHint() ), true ) );
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
   hbqt_par_QProgressDialog( 1 )->setCancelButtonText( QProgressDialog::tr( hb_parc( 2 ) ) );
}

/*
 * void setLabelText ( const QString & text )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABELTEXT )
{
   hbqt_par_QProgressDialog( 1 )->setLabelText( QProgressDialog::tr( hb_parc( 2 ) ) );
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
