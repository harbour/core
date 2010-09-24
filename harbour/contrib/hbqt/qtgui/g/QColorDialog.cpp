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
 *  enum ColorDialogOption { ShowAlphaChannel, NoButtons, DontUseNativeDialog }
 *  flags ColorDialogOptions
 */

/*
 *  Constructed[ 15/15 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void open ( QObject * receiver, const char * member )
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QColorDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QColorDialog )
{
   QColorDialog  * ph = NULL ;
   HBQT_GC_T_QColorDialog * p = ( HBQT_GC_T_QColorDialog * ) Cargo;

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
   HBQT_GC_T_QColorDialog * p = ( HBQT_GC_T_QColorDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QColorDialog ), hbqt_gcFuncs() );

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
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->currentColor() ), true ) );
   }
}

/*
 * void open ()
 */
HB_FUNC( QT_QCOLORDIALOG_OPEN )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->open();
   }
}

/*
 * ColorDialogOptions options () const
 */
HB_FUNC( QT_QCOLORDIALOG_OPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retni( ( QColorDialog::ColorDialogOptions ) ( p )->options() );
   }
}

/*
 * QColor selectedColor () const
 */
HB_FUNC( QT_QCOLORDIALOG_SELECTEDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->selectedColor() ), true ) );
   }
}

/*
 * void setCurrentColor ( const QColor & color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCURRENTCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setCurrentColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void setOption ( ColorDialogOption option, bool on = true )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setOptions ( ColorDialogOptions options )
 */
HB_FUNC( QT_QCOLORDIALOG_SETOPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setOptions( ( QColorDialog::ColorDialogOptions ) hb_parni( 2 ) );
   }
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QCOLORDIALOG_SETVISIBLE )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setVisible( hb_parl( 2 ) );
   }
}

/*
 * bool testOption ( ColorDialogOption option ) const
 */
HB_FUNC( QT_QCOLORDIALOG_TESTOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retl( ( p )->testOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ) ) );
   }
}

/*
 * QRgb customColor ( int index )
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retnl( ( p )->customColor( hb_parni( 2 ) ) );
   }
}

/*
 * int customCount ()
 */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOUNT )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retni( ( p )->customCount() );
   }
}

/*
 * QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ), hb_parstr_utf8( 4, &pText, NULL ), ( QColorDialog::ColorDialogOptions ) hb_parni( 5 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 )
 */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR_1 )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ) ) ), true ) );
   }
}

/*
 * void setCustomColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETCUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setCustomColor( hb_parni( 2 ), hb_parnl( 3 ) );
   }
}

/*
 * void setStandardColor ( int index, QRgb color )
 */
HB_FUNC( QT_QCOLORDIALOG_SETSTANDARDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      ( p )->setStandardColor( hb_parni( 2 ), hb_parnl( 3 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
