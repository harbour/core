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
 *  Constructed[ 17/17 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QWizardPage>


/* QWizardPage ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QWizardPage > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWizardPage;

HBQT_GC_FUNC( hbqt_gcRelease_QWizardPage )
{
   HBQT_GC_T_QWizardPage * p = ( HBQT_GC_T_QWizardPage * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWizardPage * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWizardPage( void * pObj, bool bNew )
{
   HBQT_GC_T_QWizardPage * p = ( HBQT_GC_T_QWizardPage * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWizardPage ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWizardPage >( ( QWizardPage * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWizardPage;
   p->type = HBQT_TYPE_QWizardPage;

   return p;
}

HB_FUNC( QT_QWIZARDPAGE )
{
   QWizardPage * pObj = NULL;

   pObj = new QWizardPage() ;

   hb_retptrGC( hbqt_gcAllocate_QWizardPage( ( void * ) pObj, true ) );
}

/* QString buttonText ( QWizard::WizardButton which ) const */
HB_FUNC( QT_QWIZARDPAGE_BUTTONTEXT )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retstr_utf8( ( p )->buttonText( ( QWizard::WizardButton ) hb_parni( 2 ) ).toUtf8().data() );
}

/* virtual void cleanupPage () */
HB_FUNC( QT_QWIZARDPAGE_CLEANUPPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      ( p )->cleanupPage();
}

/* virtual void initializePage () */
HB_FUNC( QT_QWIZARDPAGE_INITIALIZEPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      ( p )->initializePage();
}

/* bool isCommitPage () const */
HB_FUNC( QT_QWIZARDPAGE_ISCOMMITPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retl( ( p )->isCommitPage() );
}

/* virtual bool isComplete () const */
HB_FUNC( QT_QWIZARDPAGE_ISCOMPLETE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retl( ( p )->isComplete() );
}

/* bool isFinalPage () const */
HB_FUNC( QT_QWIZARDPAGE_ISFINALPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retl( ( p )->isFinalPage() );
}

/* virtual int nextId () const */
HB_FUNC( QT_QWIZARDPAGE_NEXTID )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retni( ( p )->nextId() );
}

/* QPixmap pixmap ( QWizard::WizardPixmap which ) const */
HB_FUNC( QT_QWIZARDPAGE_PIXMAP )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ) ) ), true ) );
}

/* void setButtonText ( QWizard::WizardButton which, const QString & text ) */
HB_FUNC( QT_QWIZARDPAGE_SETBUTTONTEXT )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      void * pText;
      ( p )->setButtonText( ( QWizard::WizardButton ) hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setCommitPage ( bool commitPage ) */
HB_FUNC( QT_QWIZARDPAGE_SETCOMMITPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      ( p )->setCommitPage( hb_parl( 2 ) );
}

/* void setFinalPage ( bool finalPage ) */
HB_FUNC( QT_QWIZARDPAGE_SETFINALPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      ( p )->setFinalPage( hb_parl( 2 ) );
}

/* void setPixmap ( QWizard::WizardPixmap which, const QPixmap & pixmap ) */
HB_FUNC( QT_QWIZARDPAGE_SETPIXMAP )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      ( p )->setPixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* void setSubTitle ( const QString & subTitle ) */
HB_FUNC( QT_QWIZARDPAGE_SETSUBTITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSubTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTitle ( const QString & title ) */
HB_FUNC( QT_QWIZARDPAGE_SETTITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString subTitle () const */
HB_FUNC( QT_QWIZARDPAGE_SUBTITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retstr_utf8( ( p )->subTitle().toUtf8().data() );
}

/* QString title () const */
HB_FUNC( QT_QWIZARDPAGE_TITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}

/* virtual bool validatePage () */
HB_FUNC( QT_QWIZARDPAGE_VALIDATEPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
      hb_retl( ( p )->validatePage() );
}


#endif /* #if QT_VERSION >= 0x040500 */
