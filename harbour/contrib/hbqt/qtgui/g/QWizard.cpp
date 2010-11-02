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
 *  enum WizardButton { BackButton, NextButton, CommitButton, FinishButton, ..., Stretch }
 *  enum WizardOption { IndependentPages, IgnoreSubTitles, ExtendedWatermarkPixmap, NoDefaultButton, ..., HaveCustomButton3 }
 *  enum WizardPixmap { WatermarkPixmap, LogoPixmap, BannerPixmap, BackgroundPixmap }
 *  enum WizardStyle { ClassicStyle, ModernStyle, MacStyle, AeroStyle }
 *  flags WizardOptions
 */

/*
 *  Constructed[ 35/36 [ 97.22% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setButtonLayout ( const QList<WizardButton> & layout )
 */

#include <QtCore/QPointer>

#include <QtGui/QWizard>
#include <QtCore/QVariant>


/*
 * QWizard ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QWizard ()
 */

typedef struct
{
   QPointer< QWizard > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWizard;

HBQT_GC_FUNC( hbqt_gcRelease_QWizard )
{
   HBQT_GC_T_QWizard * p = ( HBQT_GC_T_QWizard * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWizard * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWizard( void * pObj, bool bNew )
{
   HBQT_GC_T_QWizard * p = ( HBQT_GC_T_QWizard * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWizard ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWizard >( ( QWizard * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWizard;
   p->type = HBQT_TYPE_QWizard;

   return p;
}

HB_FUNC( QT_QWIZARD )
{
   QWizard * pObj = NULL;

   pObj = new QWizard( hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWizard( ( void * ) pObj, true ) );
}

/* int addPage ( QWizardPage * page ) */
HB_FUNC( QT_QWIZARD_ADDPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( p )->addPage( hbqt_par_QWizardPage( 2 ) ) );
}

/* QAbstractButton * button ( WizardButton which ) const */
HB_FUNC( QT_QWIZARD_BUTTON )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( ( QWizard::WizardButton ) hb_parni( 2 ) ), false ) );
}

/* QString buttonText ( WizardButton which ) const */
HB_FUNC( QT_QWIZARD_BUTTONTEXT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retstr_utf8( ( p )->buttonText( ( QWizard::WizardButton ) hb_parni( 2 ) ).toUtf8().data() );
}

/* int currentId () const */
HB_FUNC( QT_QWIZARD_CURRENTID )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( p )->currentId() );
}

/* QWizardPage * currentPage () const */
HB_FUNC( QT_QWIZARD_CURRENTPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWizardPage( ( p )->currentPage(), false ) );
}

/* QVariant field ( const QString & name ) const */
HB_FUNC( QT_QWIZARD_FIELD )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->field( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* bool hasVisitedPage ( int id ) const */
HB_FUNC( QT_QWIZARD_HASVISITEDPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retl( ( p )->hasVisitedPage( hb_parni( 2 ) ) );
}

/* virtual int nextId () const */
HB_FUNC( QT_QWIZARD_NEXTID )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( p )->nextId() );
}

/* WizardOptions options () const */
HB_FUNC( QT_QWIZARD_OPTIONS )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( QWizard::WizardOptions ) ( p )->options() );
}

/* QWizardPage * page ( int id ) const */
HB_FUNC( QT_QWIZARD_PAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWizardPage( ( p )->page( hb_parni( 2 ) ), false ) );
}

/* QList<int> pageIds () const */
HB_FUNC( QT_QWIZARD_PAGEIDS )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->pageIds() ), true ) );
}

/* QPixmap pixmap ( WizardPixmap which ) const */
HB_FUNC( QT_QWIZARD_PIXMAP )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ) ) ), true ) );
}

/* void removePage ( int id ) */
HB_FUNC( QT_QWIZARD_REMOVEPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->removePage( hb_parni( 2 ) );
}

/* void setButton ( WizardButton which, QAbstractButton * button ) */
HB_FUNC( QT_QWIZARD_SETBUTTON )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setButton( ( QWizard::WizardButton ) hb_parni( 2 ), hbqt_par_QAbstractButton( 3 ) );
}

/* void setButtonText ( WizardButton which, const QString & text ) */
HB_FUNC( QT_QWIZARD_SETBUTTONTEXT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
   {
      void * pText;
      ( p )->setButtonText( ( QWizard::WizardButton ) hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setDefaultProperty ( const char * className, const char * property, const char * changedSignal ) */
HB_FUNC( QT_QWIZARD_SETDEFAULTPROPERTY )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setDefaultProperty( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ), ( const char * ) hb_parc( 4 ) );
}

/* void setField ( const QString & name, const QVariant & value ) */
HB_FUNC( QT_QWIZARD_SETFIELD )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
   {
      void * pText;
      ( p )->setField( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QVariant( 3 ) );
      hb_strfree( pText );
   }
}

/* void setOption ( WizardOption option, bool on = true ) */
HB_FUNC( QT_QWIZARD_SETOPTION )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setOption( ( QWizard::WizardOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( WizardOptions options ) */
HB_FUNC( QT_QWIZARD_SETOPTIONS )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setOptions( ( QWizard::WizardOptions ) hb_parni( 2 ) );
}

/* void setPage ( int id, QWizardPage * page ) */
HB_FUNC( QT_QWIZARD_SETPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setPage( hb_parni( 2 ), hbqt_par_QWizardPage( 3 ) );
}

/* void setPixmap ( WizardPixmap which, const QPixmap & pixmap ) */
HB_FUNC( QT_QWIZARD_SETPIXMAP )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setPixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* void setStartId ( int id ) */
HB_FUNC( QT_QWIZARD_SETSTARTID )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setStartId( hb_parni( 2 ) );
}

/* void setSubTitleFormat ( Qt::TextFormat format ) */
HB_FUNC( QT_QWIZARD_SETSUBTITLEFORMAT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setSubTitleFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/* void setTitleFormat ( Qt::TextFormat format ) */
HB_FUNC( QT_QWIZARD_SETTITLEFORMAT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setTitleFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/* void setWizardStyle ( WizardStyle style ) */
HB_FUNC( QT_QWIZARD_SETWIZARDSTYLE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->setWizardStyle( ( QWizard::WizardStyle ) hb_parni( 2 ) );
}

/* int startId () const */
HB_FUNC( QT_QWIZARD_STARTID )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( p )->startId() );
}

/* Qt::TextFormat subTitleFormat () const */
HB_FUNC( QT_QWIZARD_SUBTITLEFORMAT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( Qt::TextFormat ) ( p )->subTitleFormat() );
}

/* bool testOption ( WizardOption option ) const */
HB_FUNC( QT_QWIZARD_TESTOPTION )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QWizard::WizardOption ) hb_parni( 2 ) ) );
}

/* Qt::TextFormat titleFormat () const */
HB_FUNC( QT_QWIZARD_TITLEFORMAT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( Qt::TextFormat ) ( p )->titleFormat() );
}

/* virtual bool validateCurrentPage () */
HB_FUNC( QT_QWIZARD_VALIDATECURRENTPAGE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retl( ( p )->validateCurrentPage() );
}

/* QList<int> visitedPages () const */
HB_FUNC( QT_QWIZARD_VISITEDPAGES )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->visitedPages() ), true ) );
}

/* WizardStyle wizardStyle () const */
HB_FUNC( QT_QWIZARD_WIZARDSTYLE )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      hb_retni( ( QWizard::WizardStyle ) ( p )->wizardStyle() );
}

/* void back () */
HB_FUNC( QT_QWIZARD_BACK )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->back();
}

/* void next () */
HB_FUNC( QT_QWIZARD_NEXT )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->next();
}

/* void restart () */
HB_FUNC( QT_QWIZARD_RESTART )
{
   QWizard * p = hbqt_par_QWizard( 1 );
   if( p )
      ( p )->restart();
}


#endif /* #if QT_VERSION >= 0x040500 */
