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

#include <QtCore/QPointer>

#include <QtGui/QWizardPage>


/* QWizardPage ( QWidget * parent = 0 )
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QWizardPage > pq;
} QGC_POINTER_QWizardPage;

QT_G_FUNC( release_QWizardPage )
{
   QGC_POINTER_QWizardPage * p = ( QGC_POINTER_QWizardPage * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QWizardPage                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QWizardPage                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QWizardPage * ) p->ph )->~QWizardPage();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QWizardPage                 Object deleted!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  YES release_QWizardPage                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QWizardPage                 Object Name Missing!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  NO  release_QWizardPage" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QWizardPage                 Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QWizardPage" );
      #endif
   }
}

void * gcAllocate_QWizardPage( void * pObj )
{
   QGC_POINTER_QWizardPage * p = ( QGC_POINTER_QWizardPage * ) hb_gcAllocate( sizeof( QGC_POINTER_QWizardPage ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QWizardPage;
   new( & p->pq ) QPointer< QWizardPage >( ( QWizardPage * ) pObj );
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QWizardPage                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QWIZARDPAGE )
{
   void * pObj = NULL;

   pObj = new QWizardPage() ;

   hb_retptrGC( gcAllocate_QWizardPage( pObj ) );
}
/*
 * QString buttonText ( QWizard::WizardButton which ) const
 */
HB_FUNC( QT_QWIZARDPAGE_BUTTONTEXT )
{
   hb_retc( hbqt_par_QWizardPage( 1 )->buttonText( ( QWizard::WizardButton ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * virtual void cleanupPage ()
 */
HB_FUNC( QT_QWIZARDPAGE_CLEANUPPAGE )
{
   hbqt_par_QWizardPage( 1 )->cleanupPage();
}

/*
 * virtual void initializePage ()
 */
HB_FUNC( QT_QWIZARDPAGE_INITIALIZEPAGE )
{
   hbqt_par_QWizardPage( 1 )->initializePage();
}

/*
 * bool isCommitPage () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISCOMMITPAGE )
{
   hb_retl( hbqt_par_QWizardPage( 1 )->isCommitPage() );
}

/*
 * virtual bool isComplete () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISCOMPLETE )
{
   hb_retl( hbqt_par_QWizardPage( 1 )->isComplete() );
}

/*
 * bool isFinalPage () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISFINALPAGE )
{
   hb_retl( hbqt_par_QWizardPage( 1 )->isFinalPage() );
}

/*
 * virtual int nextId () const
 */
HB_FUNC( QT_QWIZARDPAGE_NEXTID )
{
   hb_retni( hbqt_par_QWizardPage( 1 )->nextId() );
}

/*
 * QPixmap pixmap ( QWizard::WizardPixmap which ) const
 */
HB_FUNC( QT_QWIZARDPAGE_PIXMAP )
{
   hb_retptrGC( gcAllocate_QPixmap( new QPixmap( hbqt_par_QWizardPage( 1 )->pixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ) ) ) ) );
}

/*
 * void setButtonText ( QWizard::WizardButton which, const QString & text )
 */
HB_FUNC( QT_QWIZARDPAGE_SETBUTTONTEXT )
{
   hbqt_par_QWizardPage( 1 )->setButtonText( ( QWizard::WizardButton ) hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setCommitPage ( bool commitPage )
 */
HB_FUNC( QT_QWIZARDPAGE_SETCOMMITPAGE )
{
   hbqt_par_QWizardPage( 1 )->setCommitPage( hb_parl( 2 ) );
}

/*
 * void setFinalPage ( bool finalPage )
 */
HB_FUNC( QT_QWIZARDPAGE_SETFINALPAGE )
{
   hbqt_par_QWizardPage( 1 )->setFinalPage( hb_parl( 2 ) );
}

/*
 * void setPixmap ( QWizard::WizardPixmap which, const QPixmap & pixmap )
 */
HB_FUNC( QT_QWIZARDPAGE_SETPIXMAP )
{
   hbqt_par_QWizardPage( 1 )->setPixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * void setSubTitle ( const QString & subTitle )
 */
HB_FUNC( QT_QWIZARDPAGE_SETSUBTITLE )
{
   hbqt_par_QWizardPage( 1 )->setSubTitle( hbqt_par_QString( 2 ) );
}

/*
 * void setTitle ( const QString & title )
 */
HB_FUNC( QT_QWIZARDPAGE_SETTITLE )
{
   hbqt_par_QWizardPage( 1 )->setTitle( hbqt_par_QString( 2 ) );
}

/*
 * QString subTitle () const
 */
HB_FUNC( QT_QWIZARDPAGE_SUBTITLE )
{
   hb_retc( hbqt_par_QWizardPage( 1 )->subTitle().toAscii().data() );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWIZARDPAGE_TITLE )
{
   hb_retc( hbqt_par_QWizardPage( 1 )->title().toAscii().data() );
}

/*
 * virtual bool validatePage ()
 */
HB_FUNC( QT_QWIZARDPAGE_VALIDATEPAGE )
{
   hb_retl( hbqt_par_QWizardPage( 1 )->validatePage() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
