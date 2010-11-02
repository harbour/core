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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerWidgetBoxInterface>


/*
 * QDesignerWidgetBoxInterface ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QDesignerWidgetBoxInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerWidgetBoxInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerWidgetBoxInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerWidgetBoxInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerWidgetBoxInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerWidgetBoxInterface * p = ( HBQT_GC_T_QDesignerWidgetBoxInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerWidgetBoxInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerWidgetBoxInterface >( ( QDesignerWidgetBoxInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerWidgetBoxInterface;
   p->type = HBQT_TYPE_QDesignerWidgetBoxInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERWIDGETBOXINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerWidgetBoxInterface() );
}

/* virtual QString fileName () const = 0 */
HB_FUNC( QT_QDESIGNERWIDGETBOXINTERFACE_FILENAME )
{
   QDesignerWidgetBoxInterface * p = hbqt_par_QDesignerWidgetBoxInterface( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* virtual bool load () = 0 */
HB_FUNC( QT_QDESIGNERWIDGETBOXINTERFACE_LOAD )
{
   QDesignerWidgetBoxInterface * p = hbqt_par_QDesignerWidgetBoxInterface( 1 );
   if( p )
      hb_retl( ( p )->load() );
}

/* virtual bool save () = 0 */
HB_FUNC( QT_QDESIGNERWIDGETBOXINTERFACE_SAVE )
{
   QDesignerWidgetBoxInterface * p = hbqt_par_QDesignerWidgetBoxInterface( 1 );
   if( p )
      hb_retl( ( p )->save() );
}

/* virtual void setFileName ( const QString & fileName ) = 0 */
HB_FUNC( QT_QDESIGNERWIDGETBOXINTERFACE_SETFILENAME )
{
   QDesignerWidgetBoxInterface * p = hbqt_par_QDesignerWidgetBoxInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
