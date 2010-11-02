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
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //const QItemEditorFactory * defaultFactory ()
 */

#include <QtCore/QPointer>

#include <QtGui/QItemEditorFactory>


/*
 * QItemEditorFactory ()
 * virtual ~QItemEditorFactory ()
 *
 */

typedef struct
{
   QItemEditorFactory * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QItemEditorFactory;

HBQT_GC_FUNC( hbqt_gcRelease_QItemEditorFactory )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QItemEditorFactory * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QItemEditorFactory( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QItemEditorFactory * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemEditorFactory;
   p->type = HBQT_TYPE_QItemEditorFactory;

   return p;
}

HB_FUNC( QT_QITEMEDITORFACTORY )
{
   QItemEditorFactory * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QItemEditorFactory() ;
   }
   else
   {
      pObj = new QItemEditorFactory( *hbqt_par_QItemEditorFactory( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QItemEditorFactory( ( void * ) pObj, true ) );
}

/* virtual QWidget * createEditor ( QVariant::Type type, QWidget * parent ) const */
HB_FUNC( QT_QITEMEDITORFACTORY_CREATEEDITOR )
{
   QItemEditorFactory * p = hbqt_par_QItemEditorFactory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createEditor( ( QVariant::Type ) hb_parni( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
}

/* void registerEditor ( QVariant::Type type, QItemEditorCreatorBase * creator ) */
HB_FUNC( QT_QITEMEDITORFACTORY_REGISTEREDITOR )
{
   QItemEditorFactory * p = hbqt_par_QItemEditorFactory( 1 );
   if( p )
      ( p )->registerEditor( ( QVariant::Type ) hb_parni( 2 ), hbqt_par_QItemEditorCreatorBase( 3 ) );
}

/* virtual QByteArray valuePropertyName ( QVariant::Type type ) const */
HB_FUNC( QT_QITEMEDITORFACTORY_VALUEPROPERTYNAME )
{
   QItemEditorFactory * p = hbqt_par_QItemEditorFactory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->valuePropertyName( ( QVariant::Type ) hb_parni( 2 ) ) ), true ) );
}

/* void setDefaultFactory ( QItemEditorFactory * factory ) */
HB_FUNC( QT_QITEMEDITORFACTORY_SETDEFAULTFACTORY )
{
   QItemEditorFactory * p = hbqt_par_QItemEditorFactory( 1 );
   if( p )
      ( p )->setDefaultFactory( hbqt_par_QItemEditorFactory( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
