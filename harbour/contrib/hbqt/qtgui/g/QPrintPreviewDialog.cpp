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

#include <QtGui/QPrintPreviewDialog>


/*
 * QPrintPreviewDialog ( QPrinter * printer, QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * QPrintPreviewDialog ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QPrintPreviewDialog ()
 */

typedef struct
{
   QPointer< QPrintPreviewDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrintPreviewDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QPrintPreviewDialog )
{
   HBQT_GC_T_QPrintPreviewDialog * p = ( HBQT_GC_T_QPrintPreviewDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QPrintPreviewDialog * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPrintPreviewDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QPrintPreviewDialog * p = ( HBQT_GC_T_QPrintPreviewDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPrintPreviewDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPrintPreviewDialog >( ( QPrintPreviewDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintPreviewDialog;
   p->type = HBQT_TYPE_QPrintPreviewDialog;

   return p;
}

HB_FUNC( QT_QPRINTPREVIEWDIALOG )
{
   QPrintPreviewDialog * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISPOINTER( 2 ) )
      pObj = new QPrintPreviewDialog( hbqt_par_QPrinter( 1 ), hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ) ;
   else
      pObj = new QPrintPreviewDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPrintPreviewDialog( ( void * ) pObj, true ) );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QPRINTPREVIEWDIALOG_OPEN )
{
   QPrintPreviewDialog * p = hbqt_par_QPrintPreviewDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* QPrinter * printer () */
HB_FUNC( QT_QPRINTPREVIEWDIALOG_PRINTER )
{
   QPrintPreviewDialog * p = hbqt_par_QPrintPreviewDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
