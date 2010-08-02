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
#include "hbqtdesigner.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum MoveMode { MoveAnchor, KeepAnchor }
 *  enum MoveOperation { NoMove, Start, End, Next, ..., Down }
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerFormWindowCursorInterface>


/*
 * ~QDesignerFormWindowCursorInterface ()
 *
 */

typedef struct
{
   QDesignerFormWindowCursorInterface * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerFormWindowCursorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerFormWindowCursorInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormWindowCursorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDesignerFormWindowCursorInterface * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowCursorInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowCursorInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerFormWindowCursorInterface", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerFormWindowCursorInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE )
{
   //hb_retptr( new QDesignerFormWindowCursorInterface() );
}

/*
 * virtual QWidget * current () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_CURRENT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->current(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_CURRENT FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->current(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormWindowInterface * formWindow () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_FORMWINDOW )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_FORMWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual bool hasSelection () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_HASSELECTION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->hasSelection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_HASSELECTION FP=hb_retl( ( p )->hasSelection() ); p is NULL" ) );
   }
}

/*
 * bool isWidgetSelected ( QWidget * widget ) const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_ISWIDGETSELECTED )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->isWidgetSelected( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_ISWIDGETSELECTED FP=hb_retl( ( p )->isWidgetSelected( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool movePosition ( MoveOperation operation, MoveMode mode = MoveAnchor ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_MOVEPOSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->movePosition( ( QDesignerFormWindowCursorInterface::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_MOVEPOSITION FP=hb_retl( ( p )->movePosition( ( QDesignerFormWindowCursorInterface::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) ) ); p is NULL" ) );
   }
}

/*
 * virtual int position () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_POSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_POSITION FP=hb_retni( ( p )->position() ); p is NULL" ) );
   }
}

/*
 * virtual void resetWidgetProperty ( QWidget * widget, const QString & name ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_RESETWIDGETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      ( p )->resetWidgetProperty( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_RESETWIDGETPROPERTY FP=( p )->resetWidgetProperty( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual QWidget * selectedWidget ( int index ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGET )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->selectedWidget( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->selectedWidget( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual int selectedWidgetCount () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGETCOUNT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->selectedWidgetCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGETCOUNT FP=hb_retni( ( p )->selectedWidgetCount() ); p is NULL" ) );
   }
}

/*
 * virtual void setPosition ( int position, MoveMode mode = MoveAnchor ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPOSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      ( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPOSITION FP=( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) ); p is NULL" ) );
   }
}

/*
 * virtual void setProperty ( const QString & name, const QVariant & value ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      ( p )->setProperty( hbqt_par_QString( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPROPERTY FP=( p )->setProperty( hbqt_par_QString( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setWidgetProperty ( QWidget * widget, const QString & name, const QVariant & value ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETWIDGETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      ( p )->setWidgetProperty( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), *hbqt_par_QVariant( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETWIDGETPROPERTY FP=( p )->setWidgetProperty( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), *hbqt_par_QVariant( 4 ) ); p is NULL" ) );
   }
}

/*
 * virtual QWidget * widget ( int index ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGET )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual int widgetCount () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGETCOUNT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->widgetCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGETCOUNT FP=hb_retni( ( p )->widgetCount() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
