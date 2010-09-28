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
 *  Constructed[ 24/24 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QProgressDialog>


/*
 * QProgressDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * QProgressDialog ( const QString & labelText, const QString & cancelButtonText, int minimum, int maximum, QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QProgressDialog ()
 */

typedef struct
{
   QPointer< QProgressDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QProgressDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QProgressDialog )
{
   QProgressDialog  * ph = NULL ;
   HBQT_GC_T_QProgressDialog * p = ( HBQT_GC_T_QProgressDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProgressDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProgressDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QProgressDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QProgressDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QProgressDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProgressDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QProgressDialog * p = ( HBQT_GC_T_QProgressDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QProgressDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QProgressDialog >( ( QProgressDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProgressDialog;
   p->type = HBQT_TYPE_QProgressDialog;

   if( bNew )
   {
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
   QProgressDialog * pObj = NULL;

   pObj = new QProgressDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressDialog( ( void * ) pObj, true ) );
}

/*
 * bool autoClose () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_AUTOCLOSE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retl( ( p )->autoClose() );
   }
}

/*
 * bool autoReset () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_AUTORESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retl( ( p )->autoReset() );
   }
}

/*
 * QString labelText () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_LABELTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->labelText().toUtf8().data() );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MAXIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retni( ( p )->maximum() );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retni( ( p )->minimum() );
   }
}

/*
 * int minimumDuration () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUMDURATION )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retni( ( p )->minimumDuration() );
   }
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QPROGRESSDIALOG_OPEN )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
   }
}

/*
 * void setAutoClose ( bool close )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTOCLOSE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setAutoClose( hb_parl( 2 ) );
   }
}

/*
 * void setAutoReset ( bool reset )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTORESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setAutoReset( hb_parl( 2 ) );
   }
}

/*
 * void setBar ( QProgressBar * bar )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETBAR )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setBar( hbqt_par_QProgressBar( 2 ) );
   }
}

/*
 * void setCancelButton ( QPushButton * cancelButton )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTON )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setCancelButton( hbqt_par_QPushButton( 2 ) );
   }
}

/*
 * void setLabel ( QLabel * label )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABEL )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setLabel( hbqt_par_QLabel( 2 ) );
   }
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_SIZEHINT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_VALUE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retni( ( p )->value() );
   }
}

/*
 * bool wasCanceled () const
 */
HB_FUNC( QT_QPROGRESSDIALOG_WASCANCELED )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      hb_retl( ( p )->wasCanceled() );
   }
}

/*
 * void cancel ()
 */
HB_FUNC( QT_QPROGRESSDIALOG_CANCEL )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->cancel();
   }
}

/*
 * void reset ()
 */
HB_FUNC( QT_QPROGRESSDIALOG_RESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->reset();
   }
}

/*
 * void setCancelButtonText ( const QString & cancelButtonText )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTONTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCancelButtonText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setLabelText ( const QString & text )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABELTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setLabelText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setMaximum ( int maximum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMAXIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setMaximum( hb_parni( 2 ) );
   }
}

/*
 * void setMinimum ( int minimum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setMinimum( hb_parni( 2 ) );
   }
}

/*
 * void setMinimumDuration ( int ms )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUMDURATION )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setMinimumDuration( hb_parni( 2 ) );
   }
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETRANGE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setValue ( int progress )
 */
HB_FUNC( QT_QPROGRESSDIALOG_SETVALUE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      ( p )->setValue( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
