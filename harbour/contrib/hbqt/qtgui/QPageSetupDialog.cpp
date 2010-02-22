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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum PageSetupDialogOption { DontUseSheet }
 *  flags PageSetupDialogOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QPageSetupDialog>


/*
 * QPageSetupDialog ( QPrinter * printer, QWidget * parent = 0 )
 * QPageSetupDialog ( QWidget * parent = 0 )
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QPageSetupDialog > pq;
} QGC_POINTER_QPageSetupDialog;

QT_G_FUNC( hbqt_gcRelease_QPageSetupDialog )
{
   QGC_POINTER_QPageSetupDialog * p = ( QGC_POINTER_QPageSetupDialog * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QPageSetupDialog   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QPageSetupDialog * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QPageSetupDialog   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QPageSetupDialogph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QPageSetupDialog    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QPageSetupDialog    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPageSetupDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QPageSetupDialog * p = ( QGC_POINTER_QPageSetupDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QPageSetupDialog ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPageSetupDialog;

   if( bNew )
   {
      new( & p->pq ) QPointer< QPageSetupDialog >( ( QPageSetupDialog * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPageSetupDialog           ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPAGESETUPDIALOG )
{
   void * pObj = NULL;

   if( hb_pcount() >= 2 )
      pObj = ( QPageSetupDialog* ) new QPageSetupDialog( hbqt_par_QPrinter( 1 ), hbqt_par_QWidget( 1 ) ) ;
   else
      pObj = ( QPageSetupDialog* ) new QPageSetupDialog( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPageSetupDialog( pObj, true ) );
}

/*
 * virtual int exec ()
 */
HB_FUNC( QT_QPAGESETUPDIALOG_EXEC )
{
   hb_retni( hbqt_par_QPageSetupDialog( 1 )->exec() );
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QPAGESETUPDIALOG_OPEN )
{
   hbqt_par_QPageSetupDialog( 1 )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
}

/*
 * PageSetupDialogOptions options () const
 */
HB_FUNC( QT_QPAGESETUPDIALOG_OPTIONS )
{
   hb_retni( ( QPageSetupDialog::PageSetupDialogOptions ) hbqt_par_QPageSetupDialog( 1 )->options() );
}

/*
 * QPrinter * printer ()
 */
HB_FUNC( QT_QPAGESETUPDIALOG_PRINTER )
{
   hb_retptrGC( hbqt_gcAllocate_QPrinter( hbqt_par_QPageSetupDialog( 1 )->printer(), false ) );
}

/*
 * void setOption ( PageSetupDialogOption option, bool on = true )
 */
HB_FUNC( QT_QPAGESETUPDIALOG_SETOPTION )
{
   hbqt_par_QPageSetupDialog( 1 )->setOption( ( QPageSetupDialog::PageSetupDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setOptions ( PageSetupDialogOptions options )
 */
HB_FUNC( QT_QPAGESETUPDIALOG_SETOPTIONS )
{
   hbqt_par_QPageSetupDialog( 1 )->setOptions( ( QPageSetupDialog::PageSetupDialogOptions ) hb_parni( 2 ) );
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QPAGESETUPDIALOG_SETVISIBLE )
{
   hbqt_par_QPageSetupDialog( 1 )->setVisible( hb_parl( 2 ) );
}

/*
 * bool testOption ( PageSetupDialogOption option ) const
 */
HB_FUNC( QT_QPAGESETUPDIALOG_TESTOPTION )
{
   hb_retl( hbqt_par_QPageSetupDialog( 1 )->testOption( ( QPageSetupDialog::PageSetupDialogOption ) hb_parni( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
