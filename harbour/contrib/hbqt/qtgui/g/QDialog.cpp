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
 *  enum DialogCode { Accepted, Rejected }
 */

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include  <QtGui/QDialog>


/*
 * QDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QDialog ()
 */

typedef struct
{
   QPointer< QDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QDialog )
{
   QDialog  * ph = NULL;
   HBQT_GC_T_QDialog * p = ( HBQT_GC_T_QDialog * ) Cargo;

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

void * hbqt_gcAllocate_QDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QDialog * p = ( HBQT_GC_T_QDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDialog >( ( QDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDialog;
   p->type = HBQT_TYPE_QDialog;

   return p;
}

HB_FUNC( QT_QDIALOG )
{
   QDialog * pObj = NULL;

   pObj = new QDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDialog( ( void * ) pObj, true ) );
}

/* bool isSizeGripEnabled () const */
HB_FUNC( QT_QDIALOG_ISSIZEGRIPENABLED )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      hb_retl( ( p )->isSizeGripEnabled() );
}

/* int result () const */
HB_FUNC( QT_QDIALOG_RESULT )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      hb_retni( ( p )->result() );
}

/* void setModal ( bool modal ) */
HB_FUNC( QT_QDIALOG_SETMODAL )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->setModal( hb_parl( 2 ) );
}

/* void setResult ( int i ) */
HB_FUNC( QT_QDIALOG_SETRESULT )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->setResult( hb_parni( 2 ) );
}

/* void setSizeGripEnabled ( bool ) */
HB_FUNC( QT_QDIALOG_SETSIZEGRIPENABLED )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->setSizeGripEnabled( hb_parl( 2 ) );
}

/* virtual void accept () */
HB_FUNC( QT_QDIALOG_ACCEPT )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->accept();
}

/* virtual void done ( int r ) */
HB_FUNC( QT_QDIALOG_DONE )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->done( hb_parni( 2 ) );
}

/* int exec () */
HB_FUNC( QT_QDIALOG_EXEC )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      hb_retni( ( p )->exec() );
}

/* void open () */
HB_FUNC( QT_QDIALOG_OPEN )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->open();
}

/* virtual void reject () */
HB_FUNC( QT_QDIALOG_REJECT )
{
   QDialog * p = hbqt_par_QDialog( 1 );
   if( p )
      ( p )->reject();
}


#endif /* #if QT_VERSION >= 0x040500 */
