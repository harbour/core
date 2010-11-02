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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QItemEditorCreatorBase>


/*
 * QItemEditorCreatorBase ()
 * virtual ~QItemEditorCreatorBase ()
 *
 */

typedef struct
{
   QItemEditorCreatorBase * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QItemEditorCreatorBase;

HBQT_GC_FUNC( hbqt_gcRelease_QItemEditorCreatorBase )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QItemEditorCreatorBase( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QItemEditorCreatorBase * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemEditorCreatorBase;
   p->type = HBQT_TYPE_QItemEditorCreatorBase;

   return p;
}

HB_FUNC( QT_QITEMEDITORCREATORBASE )
{

}

/* virtual QWidget * createWidget ( QWidget * parent ) const = 0 */
HB_FUNC( QT_QITEMEDITORCREATORBASE_CREATEWIDGET )
{
   QItemEditorCreatorBase * p = hbqt_par_QItemEditorCreatorBase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createWidget( hbqt_par_QWidget( 2 ) ), false ) );
}

/* virtual QByteArray valuePropertyName () const = 0 */
HB_FUNC( QT_QITEMEDITORCREATORBASE_VALUEPROPERTYNAME )
{
   QItemEditorCreatorBase * p = hbqt_par_QItemEditorCreatorBase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->valuePropertyName() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
