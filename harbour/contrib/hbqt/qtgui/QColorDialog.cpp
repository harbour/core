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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QColorDialog > pq;
} QGC_POINTER_QColorDialog;

QT_G_FUNC( hbqt_gcRelease_QColorDialog )
{
   QGC_POINTER_QColorDialog * p = ( QGC_POINTER_QColorDialog * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QColorDialog * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QColorDialog               ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QColorDialog               ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QColorDialog                Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QColorDialog                Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QColorDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QColorDialog * p = ( QGC_POINTER_QColorDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QColorDialog ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QColorDialog;

   if( bNew )
   {
      new( & p->pq ) QPointer< QColorDialog >( ( QColorDialog * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QColorDialog               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QCOLORDIALOG )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
   {
      pObj = ( QColorDialog* ) new QColorDialog( hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = ( QColorDialog* ) new QColorDialog( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QColorDialog( pObj, true ) );
}
/*
 * QColor currentColor () const
 */
HB_FUNC( QT_QCOLORDIALOG_CURRENTCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QColorDialog( 1 )->currentColor() ), true ) );
}

/*
 * void open ()
 */
HB_FUNC( QT_QCOLORDIALOG_OPEN )
{
   hbqt_par_QColorDialog( 1 )->open();
}

/*
 * ColorDialogOptions options () const
 */
HB_FUNC( QT_QCOLORDIALOG_OPTIONS )
{
   hb_retni( ( QColorDialog::ColorDialogOptions ) hbqt_par_QColorDialog( 1 )->options() );
}

/*
 * QColor selectedColor () const
 */
HB_FUNC( QT_QCOLORDIALOG_SELECTEDCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QColorDialog( 1 )->selectedColor() ), true ) );
}

/*
 * void setCurrentColor ( const QColor & color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCURRENTCOLOR )
{
   hbqt_par_QColorDialog( 1 )->setCurrentColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setOption ( ColorDialogOption option, bool on = true )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTION )
{
   hbqt_par_QColorDialog( 1 )->setOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setOptions ( ColorDialogOptions options )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTIONS )
{
   hbqt_par_QColorDialog( 1 )->setOptions( ( QColorDialog::ColorDialogOptions ) hb_parni( 2 ) );
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QCOLORDIALOG_SETVISIBLE )
{
   hbqt_par_QColorDialog( 1 )->setVisible( hb_parl( 2 ) );
}

/*
 * bool testOption ( ColorDialogOption option ) const
 */
HB_FUNC( QT_QCOLORDIALOG_TESTOPTION )
{
   hb_retl( hbqt_par_QColorDialog( 1 )->testOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ) ) );
}

/*
 * QRgb customColor ( int index )
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOLOR )
{
   hb_retnl( hbqt_par_QColorDialog( 1 )->customColor( hb_parni( 2 ) ) );
}

/*
 * int customCount ()
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOUNT )
{
   hb_retni( hbqt_par_QColorDialog( 1 )->customCount() );
}

/*
 * QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QColorDialog( 1 )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ), QColorDialog::tr( hb_parc( 4 ) ), ( QColorDialog::ColorDialogOptions ) hb_parni( 5 ) ) ), true ) );
}

/*
 * QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QColorDialog( 1 )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ) ) ), true ) );
}

/*
 * void setCustomColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCUSTOMCOLOR )
{
   hbqt_par_QColorDialog( 1 )->setCustomColor( hb_parni( 2 ), hb_parnl( 3 ) );
}

/*
 * void setStandardColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETSTANDARDCOLOR )
{
   hbqt_par_QColorDialog( 1 )->setStandardColor( hb_parni( 2 ), hb_parnl( 3 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
