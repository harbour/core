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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QWindowsStyle>


/*
 * QWindowsStyle ()
 * ~QWindowsStyle ()
 *
 */

typedef struct
{
   QPointer< QWindowsStyle > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWindowsStyle;

HBQT_GC_FUNC( hbqt_gcRelease_QWindowsStyle )
{
   HBQT_GC_T_QWindowsStyle * p = ( HBQT_GC_T_QWindowsStyle * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWindowsStyle * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWindowsStyle( void * pObj, bool bNew )
{
   HBQT_GC_T_QWindowsStyle * p = ( HBQT_GC_T_QWindowsStyle * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWindowsStyle ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWindowsStyle >( ( QWindowsStyle * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWindowsStyle;
   p->type = HBQT_TYPE_QWindowsStyle;

   return p;
}

HB_FUNC( QT_QWINDOWSSTYLE )
{
   QWindowsStyle * pObj = NULL;

   pObj = new QWindowsStyle() ;

   hb_retptrGC( hbqt_gcAllocate_QWindowsStyle( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
