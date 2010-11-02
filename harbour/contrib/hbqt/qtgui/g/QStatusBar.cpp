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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStatusBar>


/*
 * QStatusBar ( QWidget * parent = 0 )
 * virtual ~QStatusBar ()
 */

typedef struct
{
   QPointer< QStatusBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStatusBar;

HBQT_GC_FUNC( hbqt_gcRelease_QStatusBar )
{
   QStatusBar  * ph = NULL;
   HBQT_GC_T_QStatusBar * p = ( HBQT_GC_T_QStatusBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QStatusBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QStatusBar * p = ( HBQT_GC_T_QStatusBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStatusBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStatusBar >( ( QStatusBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStatusBar;
   p->type = HBQT_TYPE_QStatusBar;

   return p;
}

HB_FUNC( QT_QSTATUSBAR )
{
   QStatusBar * pObj = NULL;

   pObj = new QStatusBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStatusBar( ( void * ) pObj, true ) );
}

/* void addPermanentWidget ( QWidget * widget, int stretch = 0 )   [*D=1*] */
HB_FUNC( QT_QSTATUSBAR_ADDPERMANENTWIDGET )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addPermanentWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
   }
}

/* void addWidget ( QWidget * widget, int stretch = 0 )   [*D=1*] */
HB_FUNC( QT_QSTATUSBAR_ADDWIDGET )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
   }
}

/* QString currentMessage () const */
HB_FUNC( QT_QSTATUSBAR_CURRENTMESSAGE )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->currentMessage().toUtf8().data() );
}

/* int insertPermanentWidget ( int index, QWidget * widget, int stretch = 0 ) */
HB_FUNC( QT_QSTATUSBAR_INSERTPERMANENTWIDGET )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      hb_retni( ( p )->insertPermanentWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ) ) );
}

/* int insertWidget ( int index, QWidget * widget, int stretch = 0 ) */
HB_FUNC( QT_QSTATUSBAR_INSERTWIDGET )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      hb_retni( ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ) ) );
}

/* bool isSizeGripEnabled () const */
HB_FUNC( QT_QSTATUSBAR_ISSIZEGRIPENABLED )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      hb_retl( ( p )->isSizeGripEnabled() );
}

/* void removeWidget ( QWidget * widget ) */
HB_FUNC( QT_QSTATUSBAR_REMOVEWIDGET )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      ( p )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/* void setSizeGripEnabled ( bool ) */
HB_FUNC( QT_QSTATUSBAR_SETSIZEGRIPENABLED )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      ( p )->setSizeGripEnabled( hb_parl( 2 ) );
}

/* void clearMessage () */
HB_FUNC( QT_QSTATUSBAR_CLEARMESSAGE )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
      ( p )->clearMessage();
}

/* void showMessage ( const QString & message, int timeout = 0 ) */
HB_FUNC( QT_QSTATUSBAR_SHOWMESSAGE )
{
   QStatusBar * p = hbqt_par_QStatusBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
