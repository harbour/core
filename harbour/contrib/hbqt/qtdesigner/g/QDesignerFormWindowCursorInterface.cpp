/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtdesigner.h"

#if QT_VERSION >= 0x040500

/*
 *  enum MoveMode { MoveAnchor, KeepAnchor }
 *  enum MoveOperation { NoMove, Start, End, Next, ..., Down }
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
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
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerFormWindowCursorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDesignerFormWindowCursorInterface * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowCursorInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowCursorInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerFormWindowCursorInterface() );
}

/* virtual QWidget * current () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_CURRENT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->current(), false ) );
}

/* virtual QDesignerFormWindowInterface * formWindow () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_FORMWINDOW )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow(), false ) );
}

/* virtual bool hasSelection () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_HASSELECTION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->hasSelection() );
}

/* bool isWidgetSelected ( QWidget * widget ) const */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_ISWIDGETSELECTED )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->isWidgetSelected( hbqt_par_QWidget( 2 ) ) );
}

/* virtual bool movePosition ( MoveOperation operation, MoveMode mode = MoveAnchor ) = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_MOVEPOSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retl( ( p )->movePosition( ( QDesignerFormWindowCursorInterface::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) ) );
}

/* virtual int position () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_POSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->position() );
}

/* virtual void resetWidgetProperty ( QWidget * widget, const QString & name ) = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_RESETWIDGETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->resetWidgetProperty( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual QWidget * selectedWidget ( int index ) const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGET )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->selectedWidget( hb_parni( 2 ) ), false ) );
}

/* virtual int selectedWidgetCount () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SELECTEDWIDGETCOUNT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->selectedWidgetCount() );
}

/* virtual void setPosition ( int position, MoveMode mode = MoveAnchor ) = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPOSITION )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      ( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QDesignerFormWindowCursorInterface::MoveMode ) hb_parni( 3 ) : ( QDesignerFormWindowCursorInterface::MoveMode ) QDesignerFormWindowCursorInterface::MoveAnchor ) );
}

/* virtual void setProperty ( const QString & name, const QVariant & value ) = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setProperty( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QVariant( 3 ) );
      hb_strfree( pText );
   }
}

/* virtual void setWidgetProperty ( QWidget * widget, const QString & name, const QVariant & value ) = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_SETWIDGETPROPERTY )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWidgetProperty( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), *hbqt_par_QVariant( 4 ) );
      hb_strfree( pText );
   }
}

/* virtual QWidget * widget ( int index ) const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGET )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
}

/* virtual int widgetCount () const = 0 */
HB_FUNC( QT_QDESIGNERFORMWINDOWCURSORINTERFACE_WIDGETCOUNT )
{
   QDesignerFormWindowCursorInterface * p = hbqt_par_QDesignerFormWindowCursorInterface( 1 );
   if( p )
      hb_retni( ( p )->widgetCount() );
}


#endif /* #if QT_VERSION >= 0x040500 */
