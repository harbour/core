/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum WizardButton { BackButton, NextButton, CommitButton, FinishButton, ..., Stretch }
 *  enum WizardOption { IndependentPages, IgnoreSubTitles, ExtendedWatermarkPixmap, NoDefaultButton, ..., HaveCustomButton3 }
 *  enum WizardPixmap { WatermarkPixmap, LogoPixmap, BannerPixmap, BackgroundPixmap }
 *  enum WizardStyle { ClassicStyle, ModernStyle, MacStyle, AeroStyle }
 *  flags WizardOptions
 */

/*
 *  Constructed[ 33/36 [ 91.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<int> pageIds () const
 *  void setButtonLayout ( const QList<WizardButton> & layout )
 *  QList<int> visitedPages () const
 */

#include <QtCore/QPointer>

#include <QtGui/QWizard>
#include <QtCore/QVariant>


/*
 * QWizard ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QWizard ()
 */

QT_G_FUNC( release_QWizard )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QWizard" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QWizard * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QWizard" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QWIZARD )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   QPointer< QWizard > pObj = NULL;

   pObj = new QWizard( hbqt_par_QWidget( 2 ) ) ;

   p->ph = pObj;
   p->func = release_QWizard;

   hb_retptrGC( p );
}
/*
 * int addPage ( QWizardPage * page )
 */
HB_FUNC( QT_QWIZARD_ADDPAGE )
{
   hb_retni( hbqt_par_QWizard( 1 )->addPage( hbqt_par_QWizardPage( 2 ) ) );
}

/*
 * QAbstractButton * button ( WizardButton which ) const
 */
HB_FUNC( QT_QWIZARD_BUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QWizard( 1 )->button( ( QWizard::WizardButton ) hb_parni( 2 ) ) );
}

/*
 * QString buttonText ( WizardButton which ) const
 */
HB_FUNC( QT_QWIZARD_BUTTONTEXT )
{
   hb_retc( hbqt_par_QWizard( 1 )->buttonText( ( QWizard::WizardButton ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * int currentId () const
 */
HB_FUNC( QT_QWIZARD_CURRENTID )
{
   hb_retni( hbqt_par_QWizard( 1 )->currentId() );
}

/*
 * QWizardPage * currentPage () const
 */
HB_FUNC( QT_QWIZARD_CURRENTPAGE )
{
   hb_retptr( ( QWizardPage* ) hbqt_par_QWizard( 1 )->currentPage() );
}

/*
 * QVariant field ( const QString & name ) const
 */
HB_FUNC( QT_QWIZARD_FIELD )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QWizard( 1 )->field( hbqt_par_QString( 2 ) ) ), release_QVariant ) );
}

/*
 * bool hasVisitedPage ( int id ) const
 */
HB_FUNC( QT_QWIZARD_HASVISITEDPAGE )
{
   hb_retl( hbqt_par_QWizard( 1 )->hasVisitedPage( hb_parni( 2 ) ) );
}

/*
 * virtual int nextId () const
 */
HB_FUNC( QT_QWIZARD_NEXTID )
{
   hb_retni( hbqt_par_QWizard( 1 )->nextId() );
}

/*
 * WizardOptions options () const
 */
HB_FUNC( QT_QWIZARD_OPTIONS )
{
   hb_retni( ( QWizard::WizardOptions ) hbqt_par_QWizard( 1 )->options() );
}

/*
 * QWizardPage * page ( int id ) const
 */
HB_FUNC( QT_QWIZARD_PAGE )
{
   hb_retptr( ( QWizardPage* ) hbqt_par_QWizard( 1 )->page( hb_parni( 2 ) ) );
}

/*
 * QPixmap pixmap ( WizardPixmap which ) const
 */
HB_FUNC( QT_QWIZARD_PIXMAP )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPixmap( hbqt_par_QWizard( 1 )->pixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ) ) ), release_QPixmap ) );
}

/*
 * void removePage ( int id )
 */
HB_FUNC( QT_QWIZARD_REMOVEPAGE )
{
   hbqt_par_QWizard( 1 )->removePage( hb_parni( 2 ) );
}

/*
 * void setButton ( WizardButton which, QAbstractButton * button )
 */
HB_FUNC( QT_QWIZARD_SETBUTTON )
{
   hbqt_par_QWizard( 1 )->setButton( ( QWizard::WizardButton ) hb_parni( 2 ), hbqt_par_QAbstractButton( 3 ) );
}

/*
 * void setButtonText ( WizardButton which, const QString & text )
 */
HB_FUNC( QT_QWIZARD_SETBUTTONTEXT )
{
   hbqt_par_QWizard( 1 )->setButtonText( ( QWizard::WizardButton ) hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setDefaultProperty ( const char * className, const char * property, const char * changedSignal )
 */
HB_FUNC( QT_QWIZARD_SETDEFAULTPROPERTY )
{
   hbqt_par_QWizard( 1 )->setDefaultProperty( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ) );
}

/*
 * void setField ( const QString & name, const QVariant & value )
 */
HB_FUNC( QT_QWIZARD_SETFIELD )
{
   hbqt_par_QWizard( 1 )->setField( hbqt_par_QString( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setOption ( WizardOption option, bool on = true )
 */
HB_FUNC( QT_QWIZARD_SETOPTION )
{
   hbqt_par_QWizard( 1 )->setOption( ( QWizard::WizardOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setOptions ( WizardOptions options )
 */
HB_FUNC( QT_QWIZARD_SETOPTIONS )
{
   hbqt_par_QWizard( 1 )->setOptions( ( QWizard::WizardOptions ) hb_parni( 2 ) );
}

/*
 * void setPage ( int id, QWizardPage * page )
 */
HB_FUNC( QT_QWIZARD_SETPAGE )
{
   hbqt_par_QWizard( 1 )->setPage( hb_parni( 2 ), hbqt_par_QWizardPage( 3 ) );
}

/*
 * void setPixmap ( WizardPixmap which, const QPixmap & pixmap )
 */
HB_FUNC( QT_QWIZARD_SETPIXMAP )
{
   hbqt_par_QWizard( 1 )->setPixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * void setStartId ( int id )
 */
HB_FUNC( QT_QWIZARD_SETSTARTID )
{
   hbqt_par_QWizard( 1 )->setStartId( hb_parni( 2 ) );
}

/*
 * void setSubTitleFormat ( Qt::TextFormat format )
 */
HB_FUNC( QT_QWIZARD_SETSUBTITLEFORMAT )
{
   hbqt_par_QWizard( 1 )->setSubTitleFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/*
 * void setTitleFormat ( Qt::TextFormat format )
 */
HB_FUNC( QT_QWIZARD_SETTITLEFORMAT )
{
   hbqt_par_QWizard( 1 )->setTitleFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/*
 * void setWizardStyle ( WizardStyle style )
 */
HB_FUNC( QT_QWIZARD_SETWIZARDSTYLE )
{
   hbqt_par_QWizard( 1 )->setWizardStyle( ( QWizard::WizardStyle ) hb_parni( 2 ) );
}

/*
 * int startId () const
 */
HB_FUNC( QT_QWIZARD_STARTID )
{
   hb_retni( hbqt_par_QWizard( 1 )->startId() );
}

/*
 * Qt::TextFormat subTitleFormat () const
 */
HB_FUNC( QT_QWIZARD_SUBTITLEFORMAT )
{
   hb_retni( ( Qt::TextFormat ) hbqt_par_QWizard( 1 )->subTitleFormat() );
}

/*
 * bool testOption ( WizardOption option ) const
 */
HB_FUNC( QT_QWIZARD_TESTOPTION )
{
   hb_retl( hbqt_par_QWizard( 1 )->testOption( ( QWizard::WizardOption ) hb_parni( 2 ) ) );
}

/*
 * Qt::TextFormat titleFormat () const
 */
HB_FUNC( QT_QWIZARD_TITLEFORMAT )
{
   hb_retni( ( Qt::TextFormat ) hbqt_par_QWizard( 1 )->titleFormat() );
}

/*
 * virtual bool validateCurrentPage ()
 */
HB_FUNC( QT_QWIZARD_VALIDATECURRENTPAGE )
{
   hb_retl( hbqt_par_QWizard( 1 )->validateCurrentPage() );
}

/*
 * WizardStyle wizardStyle () const
 */
HB_FUNC( QT_QWIZARD_WIZARDSTYLE )
{
   hb_retni( ( QWizard::WizardStyle ) hbqt_par_QWizard( 1 )->wizardStyle() );
}

/*
 * void back ()
 */
HB_FUNC( QT_QWIZARD_BACK )
{
   hbqt_par_QWizard( 1 )->back();
}

/*
 * void next ()
 */
HB_FUNC( QT_QWIZARD_NEXT )
{
   hbqt_par_QWizard( 1 )->next();
}

/*
 * void restart ()
 */
HB_FUNC( QT_QWIZARD_RESTART )
{
   hbqt_par_QWizard( 1 )->restart();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
