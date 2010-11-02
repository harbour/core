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
 *  Constructed[ 24/24 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QProgressDialog>


/*
 * QProgressDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * QProgressDialog ( const QString & labelText, const QString & cancelButtonText, int minimum, int maximum, QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QProgressDialog ()
 */

typedef struct
{
   QPointer< QProgressDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QProgressDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QProgressDialog )
{
   HBQT_GC_T_QProgressDialog * p = ( HBQT_GC_T_QProgressDialog * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QProgressDialog * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProgressDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QProgressDialog * p = ( HBQT_GC_T_QProgressDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QProgressDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QProgressDialog >( ( QProgressDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProgressDialog;
   p->type = HBQT_TYPE_QProgressDialog;

   return p;
}

HB_FUNC( QT_QPROGRESSDIALOG )
{
   QProgressDialog * pObj = NULL;

   pObj = new QProgressDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressDialog( ( void * ) pObj, true ) );
}

/* bool autoClose () const */
HB_FUNC( QT_QPROGRESSDIALOG_AUTOCLOSE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retl( ( p )->autoClose() );
}

/* bool autoReset () const */
HB_FUNC( QT_QPROGRESSDIALOG_AUTORESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retl( ( p )->autoReset() );
}

/* QString labelText () const */
HB_FUNC( QT_QPROGRESSDIALOG_LABELTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retstr_utf8( ( p )->labelText().toUtf8().data() );
}

/* int maximum () const */
HB_FUNC( QT_QPROGRESSDIALOG_MAXIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
}

/* int minimum () const */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
}

/* int minimumDuration () const */
HB_FUNC( QT_QPROGRESSDIALOG_MINIMUMDURATION )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retni( ( p )->minimumDuration() );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QPROGRESSDIALOG_OPEN )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* void setAutoClose ( bool close ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTOCLOSE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setAutoClose( hb_parl( 2 ) );
}

/* void setAutoReset ( bool reset ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETAUTORESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setAutoReset( hb_parl( 2 ) );
}

/* void setBar ( QProgressBar * bar ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETBAR )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setBar( hbqt_par_QProgressBar( 2 ) );
}

/* void setCancelButton ( QPushButton * cancelButton ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTON )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setCancelButton( hbqt_par_QPushButton( 2 ) );
}

/* void setLabel ( QLabel * label ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABEL )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setLabel( hbqt_par_QLabel( 2 ) );
}

/* virtual QSize sizeHint () const */
HB_FUNC( QT_QPROGRESSDIALOG_SIZEHINT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
}

/* int value () const */
HB_FUNC( QT_QPROGRESSDIALOG_VALUE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retni( ( p )->value() );
}

/* bool wasCanceled () const */
HB_FUNC( QT_QPROGRESSDIALOG_WASCANCELED )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      hb_retl( ( p )->wasCanceled() );
}

/* void cancel () */
HB_FUNC( QT_QPROGRESSDIALOG_CANCEL )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->cancel();
}

/* void reset () */
HB_FUNC( QT_QPROGRESSDIALOG_RESET )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->reset();
}

/* void setCancelButtonText ( const QString & cancelButtonText ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETCANCELBUTTONTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCancelButtonText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setLabelText ( const QString & text ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETLABELTEXT )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setLabelText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setMaximum ( int maximum ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETMAXIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
}

/* void setMinimum ( int minimum ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUM )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
}

/* void setMinimumDuration ( int ms ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETMINIMUMDURATION )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setMinimumDuration( hb_parni( 2 ) );
}

/* void setRange ( int minimum, int maximum ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETRANGE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setValue ( int progress ) */
HB_FUNC( QT_QPROGRESSDIALOG_SETVALUE )
{
   QProgressDialog * p = hbqt_par_QProgressDialog( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
