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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum PrintDialogOption
 *  flags PrintDialogOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QPrintDialog>


/*
 * QPrintDialog ( QPrinter * printer, QWidget * parent = 0 )
 * QPrintDialog ( QWidget * parent = 0 )
 * ~QPrintDialog ()
 */

typedef struct
{
   QPointer< QPrintDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPrintDialog;

QT_G_FUNC( hbqt_gcRelease_QPrintDialog )
{
   QPrintDialog  * ph = NULL ;
   QGC_POINTER_QPrintDialog * p = ( QGC_POINTER_QPrintDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPrintDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPrintDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QPrintDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPrintDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPrintDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrintDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QPrintDialog * p = ( QGC_POINTER_QPrintDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QPrintDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPrintDialog >( ( QPrintDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintDialog;
   p->type = HBQT_TYPE_QPrintDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPrintDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPrintDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPRINTDIALOG )
{
   QPrintDialog * pObj = NULL;

   pObj =  new QPrintDialog ( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPrintDialog( ( void * ) pObj, true ) );
}

/*
 * virtual void done ( int result )
 */
HB_FUNC( QT_QPRINTDIALOG_DONE )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->done( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_DONE FP=( p )->done( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QPRINTDIALOG_OPEN )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_OPEN FP=( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) ); p is NULL" ) );
   }
}

/*
 * PrintDialogOptions options () const
 */
HB_FUNC( QT_QPRINTDIALOG_OPTIONS )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retni( ( QPrintDialog::PrintDialogOptions ) ( p )->options() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_OPTIONS FP=hb_retni( ( QPrintDialog::PrintDialogOptions ) ( p )->options() ); p is NULL" ) );
   }
}

/*
 * QPrinter * printer () const
 */
HB_FUNC( QT_QPRINTDIALOG_PRINTER )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_PRINTER FP=hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) ); p is NULL" ) );
   }
}

/*
 * QPrinter * printer ()
 */
HB_FUNC( QT_QPRINTDIALOG_PRINTER_1 )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_PRINTER_1 FP=hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( PrintDialogOption option, bool on = true )
 */
HB_FUNC( QT_QPRINTDIALOG_SETOPTION )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_SETOPTION FP=( p )->setOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOptions ( PrintDialogOptions options )
 */
HB_FUNC( QT_QPRINTDIALOG_SETOPTIONS )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setOptions( ( QPrintDialog::PrintDialogOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_SETOPTIONS FP=( p )->setOptions( ( QPrintDialog::PrintDialogOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QPRINTDIALOG_SETVISIBLE )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool testOption ( PrintDialogOption option ) const
 */
HB_FUNC( QT_QPRINTDIALOG_TESTOPTION )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTDIALOG_TESTOPTION FP=hb_retl( ( p )->testOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
