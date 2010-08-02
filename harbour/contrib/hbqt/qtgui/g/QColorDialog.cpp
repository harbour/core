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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorDialogOption { ShowAlphaChannel, NoButtons, DontUseNativeDialog }
 *  flags ColorDialogOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QColorDialog>

/*
 * QColorDialog ( QWidget * parent = 0 )
 * QColorDialog ( const QColor & initial, QWidget * parent = 0 )
 * ~QColorDialog ()
 */

typedef struct
{
   QPointer< QColorDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QColorDialog;

QT_G_FUNC( hbqt_gcRelease_QColorDialog )
{
   QColorDialog  * ph = NULL ;
   QGC_POINTER_QColorDialog * p = ( QGC_POINTER_QColorDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QColorDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QColorDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QColorDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QColorDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QColorDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QColorDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QColorDialog * p = ( QGC_POINTER_QColorDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QColorDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QColorDialog >( ( QColorDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QColorDialog;
   p->type = HBQT_TYPE_QColorDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QColorDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QColorDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCOLORDIALOG )
{
   QColorDialog * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
   {
      pObj =  new QColorDialog( hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj =  new QColorDialog( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QColorDialog( ( void * ) pObj, true ) );
}

/*
 * QColor currentColor () const
 */
HB_FUNC( QT_QCOLORDIALOG_CURRENTCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->currentColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_CURRENTCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->currentColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * void open ()
 */
HB_FUNC( QT_QCOLORDIALOG_OPEN )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->open();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_OPEN FP=( p )->open(); p is NULL" ) );
   }
}

/*
 * ColorDialogOptions options () const
 */
HB_FUNC( QT_QCOLORDIALOG_OPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retni( ( QColorDialog::ColorDialogOptions ) ( p )->options() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_OPTIONS FP=hb_retni( ( QColorDialog::ColorDialogOptions ) ( p )->options() ); p is NULL" ) );
   }
}

/*
 * QColor selectedColor () const
 */
HB_FUNC( QT_QCOLORDIALOG_SELECTEDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->selectedColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SELECTEDCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->selectedColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentColor ( const QColor & color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCURRENTCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setCurrentColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETCURRENTCOLOR FP=( p )->setCurrentColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( ColorDialogOption option, bool on = true )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETOPTION FP=( p )->setOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOptions ( ColorDialogOptions options )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setOptions( ( QColorDialog::ColorDialogOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETOPTIONS FP=( p )->setOptions( ( QColorDialog::ColorDialogOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QCOLORDIALOG_SETVISIBLE )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool testOption ( ColorDialogOption option ) const
 */
HB_FUNC( QT_QCOLORDIALOG_TESTOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_TESTOPTION FP=hb_retl( ( p )->testOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QRgb customColor ( int index )
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retnl( ( p )->customColor( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_CUSTOMCOLOR FP=hb_retnl( ( p )->customColor( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int customCount ()
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOUNT )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retni( ( p )->customCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_CUSTOMCOUNT FP=hb_retni( ( p )->customCount() ); p is NULL" ) );
   }
}

/*
 * QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ), QColorDialog::tr( hb_parc( 4 ) ), ( QColorDialog::ColorDialogOptions ) hb_parni( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_GETCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ), QColorDialog::tr( hb_parc( 4 ) ), ( QColorDialog::ColorDialogOptions ) hb_parni( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR_1 )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_GETCOLOR_1 FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCustomColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setCustomColor( hb_parni( 2 ), hb_parnl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETCUSTOMCOLOR FP=( p )->setCustomColor( hb_parni( 2 ), hb_parnl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setStandardColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETSTANDARDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setStandardColor( hb_parni( 2 ), hb_parnl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOLORDIALOG_SETSTANDARDCOLOR FP=( p )->setStandardColor( hb_parni( 2 ), hb_parnl( 3 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
