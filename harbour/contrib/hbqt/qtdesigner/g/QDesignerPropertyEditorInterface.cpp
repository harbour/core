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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerPropertyEditorInterface>


/*
 * QDesignerPropertyEditorInterface ( QWidget * parent, Qt::WindowFlags flags = 0 )
 * ~QDesignerPropertyEditorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerPropertyEditorInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerPropertyEditorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerPropertyEditorInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerPropertyEditorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerPropertyEditorInterface * p = ( HBQT_GC_T_QDesignerPropertyEditorInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerPropertyEditorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerPropertyEditorInterface >( ( QDesignerPropertyEditorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerPropertyEditorInterface;
   p->type = HBQT_TYPE_QDesignerPropertyEditorInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerPropertyEditorInterface() );
}

/* virtual QDesignerFormEditorInterface * core () const */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_CORE )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
}

/* virtual QString currentPropertyName () const = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_CURRENTPROPERTYNAME )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      hb_retstr_utf8( ( p )->currentPropertyName().toUtf8().data() );
}

/* virtual bool isReadOnly () const = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_ISREADONLY )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
}

/* virtual QObject * object () const = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_OBJECT )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->object(), false ) );
}

/* virtual void setObject ( QObject * object ) = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETOBJECT )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      ( p )->setObject( hbqt_par_QObject( 2 ) );
}

/* virtual void setPropertyValue ( const QString & name, const QVariant & value, bool changed = true ) = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETPROPERTYVALUE )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPropertyValue( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QVariant( 3 ), hb_parl( 4 ) );
      hb_strfree( pText );
   }
}

/* virtual void setReadOnly ( bool readOnly ) = 0 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETREADONLY )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
