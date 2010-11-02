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
 *  enum PageSetupDialogOption { DontUseSheet }
 *  flags PageSetupDialogOptions
 */

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPageSetupDialog>


/*
 * QPageSetupDialog ( QPrinter * printer, QWidget * parent = 0 )
 * QPageSetupDialog ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QPageSetupDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPageSetupDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QPageSetupDialog )
{
   HBQT_GC_T_QPageSetupDialog * p = ( HBQT_GC_T_QPageSetupDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QPageSetupDialog * ph = p->ph;
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

void * hbqt_gcAllocate_QPageSetupDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QPageSetupDialog * p = ( HBQT_GC_T_QPageSetupDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPageSetupDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPageSetupDialog >( ( QPageSetupDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPageSetupDialog;
   p->type = HBQT_TYPE_QPageSetupDialog;

   return p;
}

HB_FUNC( QT_QPAGESETUPDIALOG )
{
   QPageSetupDialog * pObj = NULL;

   if( hb_pcount() >= 2 )
      pObj = new QPageSetupDialog( hbqt_par_QPrinter( 1 ), hbqt_par_QWidget( 1 ) ) ;
   else
      pObj = new QPageSetupDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPageSetupDialog( ( void * ) pObj, true ) );
}

/* virtual int exec () */
HB_FUNC( QT_QPAGESETUPDIALOG_EXEC )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      hb_retni( ( p )->exec() );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QPAGESETUPDIALOG_OPEN )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* PageSetupDialogOptions options () const */
HB_FUNC( QT_QPAGESETUPDIALOG_OPTIONS )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      hb_retni( ( QPageSetupDialog::PageSetupDialogOptions ) ( p )->options() );
}

/* QPrinter * printer () */
HB_FUNC( QT_QPAGESETUPDIALOG_PRINTER )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
}

/* void setOption ( PageSetupDialogOption option, bool on = true ) */
HB_FUNC( QT_QPAGESETUPDIALOG_SETOPTION )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      ( p )->setOption( ( QPageSetupDialog::PageSetupDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( PageSetupDialogOptions options ) */
HB_FUNC( QT_QPAGESETUPDIALOG_SETOPTIONS )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      ( p )->setOptions( ( QPageSetupDialog::PageSetupDialogOptions ) hb_parni( 2 ) );
}

/* virtual void setVisible ( bool visible ) */
HB_FUNC( QT_QPAGESETUPDIALOG_SETVISIBLE )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* bool testOption ( PageSetupDialogOption option ) const */
HB_FUNC( QT_QPAGESETUPDIALOG_TESTOPTION )
{
   QPageSetupDialog * p = hbqt_par_QPageSetupDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QPageSetupDialog::PageSetupDialogOption ) hb_parni( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
