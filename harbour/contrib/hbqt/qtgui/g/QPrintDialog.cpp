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
 *  enum PrintDialogOption
 *  flags PrintDialogOptions
 */

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // void addButton ( QPushButton * button )
 *  //QPrinter * printer () const
 *  // void setPrinter ( QPrinter * printer, bool pickupSettings = false )
 */

#include <QtCore/QPointer>

#include <QtGui/QPrintDialog>


/*
 * QPrintDialog ( QPrinter * printer, QWidget * parent = 0 )
 * QPrintDialog ( QWidget * parent = 0 )
 * ~QPrintDialog ()
 */

typedef struct
{
   QPointer< QPrintDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrintDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QPrintDialog )
{
   HBQT_GC_T_QPrintDialog * p = ( HBQT_GC_T_QPrintDialog * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QPrintDialog * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrintDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QPrintDialog * p = ( HBQT_GC_T_QPrintDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPrintDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPrintDialog >( ( QPrintDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintDialog;
   p->type = HBQT_TYPE_QPrintDialog;

   return p;
}

HB_FUNC( QT_QPRINTDIALOG )
{
   QPrintDialog * pObj = NULL;

   pObj = new QPrintDialog ( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPrintDialog( ( void * ) pObj, true ) );
}

/* virtual void done ( int result ) */
HB_FUNC( QT_QPRINTDIALOG_DONE )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->done( hb_parni( 2 ) );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QPRINTDIALOG_OPEN )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* PrintDialogOptions options () const */
HB_FUNC( QT_QPRINTDIALOG_OPTIONS )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retni( ( QPrintDialog::PrintDialogOptions ) ( p )->options() );
}

/* QPrinter * printer () */
HB_FUNC( QT_QPRINTDIALOG_PRINTER )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
}

/* void setOption ( PrintDialogOption option, bool on = true ) */
HB_FUNC( QT_QPRINTDIALOG_SETOPTION )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( PrintDialogOptions options ) */
HB_FUNC( QT_QPRINTDIALOG_SETOPTIONS )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setOptions( ( QPrintDialog::PrintDialogOptions ) hb_parni( 2 ) );
}

/* virtual void setVisible ( bool visible ) */
HB_FUNC( QT_QPRINTDIALOG_SETVISIBLE )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* bool testOption ( PrintDialogOption option ) const */
HB_FUNC( QT_QPRINTDIALOG_TESTOPTION )
{
   QPrintDialog * p = hbqt_par_QPrintDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QPrintDialog::PrintDialogOption ) hb_parni( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
