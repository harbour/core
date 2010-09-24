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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

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
   QWizardPage  * ph = NULL ;
   HBQT_GC_T_QWizardPage * p = ( HBQT_GC_T_QWizardPage * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWizardPage   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWizardPage   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QWizardPage          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWizardPage    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWizardPage    :    Object not created with new=true", ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWizardPage  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWizardPage", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWIZARDPAGE )
{
   QWizardPage * pObj = NULL;

   pObj = new QWizardPage() ;

   hb_retptrGC( hbqt_gcAllocate_QWizardPage( ( void * ) pObj, true ) );
}

/*
 * QString buttonText ( QWizard::WizardButton which ) const
 */
HB_FUNC( QT_QWIZARDPAGE_BUTTONTEXT )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->buttonText( ( QWizard::WizardButton ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * virtual void cleanupPage ()
 */
HB_FUNC( QT_QWIZARDPAGE_CLEANUPPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      ( p )->cleanupPage();
   }
}

/*
 * virtual void initializePage ()
 */
HB_FUNC( QT_QWIZARDPAGE_INITIALIZEPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      ( p )->initializePage();
   }
}

/*
 * bool isCommitPage () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISCOMMITPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retl( ( p )->isCommitPage() );
   }
}

/*
 * virtual bool isComplete () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISCOMPLETE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retl( ( p )->isComplete() );
   }
}

/*
 * bool isFinalPage () const
 */
HB_FUNC( QT_QWIZARDPAGE_ISFINALPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retl( ( p )->isFinalPage() );
   }
}

/*
 * virtual int nextId () const
 */
HB_FUNC( QT_QWIZARDPAGE_NEXTID )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retni( ( p )->nextId() );
   }
}

/*
 * QPixmap pixmap ( QWizard::WizardPixmap which ) const
 */
HB_FUNC( QT_QWIZARDPAGE_PIXMAP )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * void setButtonText ( QWizard::WizardButton which, const QString & text )
 */
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

/*
 * void setCommitPage ( bool commitPage )
 */
HB_FUNC( QT_QWIZARDPAGE_SETCOMMITPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      ( p )->setCommitPage( hb_parl( 2 ) );
   }
}

/*
 * void setFinalPage ( bool finalPage )
 */
HB_FUNC( QT_QWIZARDPAGE_SETFINALPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      ( p )->setFinalPage( hb_parl( 2 ) );
   }
}

/*
 * void setPixmap ( QWizard::WizardPixmap which, const QPixmap & pixmap )
 */
HB_FUNC( QT_QWIZARDPAGE_SETPIXMAP )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      ( p )->setPixmap( ( QWizard::WizardPixmap ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
   }
}

/*
 * void setSubTitle ( const QString & subTitle )
 */
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

/*
 * void setTitle ( const QString & title )
 */
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

/*
 * QString subTitle () const
 */
HB_FUNC( QT_QWIZARDPAGE_SUBTITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->subTitle().toUtf8().data() );
   }
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWIZARDPAGE_TITLE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
   }
}

/*
 * virtual bool validatePage ()
 */
HB_FUNC( QT_QWIZARDPAGE_VALIDATEPAGE )
{
   QWizardPage * p = hbqt_par_QWizardPage( 1 );
   if( p )
   {
      hb_retl( ( p )->validatePage() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
