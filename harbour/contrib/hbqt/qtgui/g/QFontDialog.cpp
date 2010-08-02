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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FontDialogOption { NoButtons, DontUseNativeDialog }
 *  flags FontDialogOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QFontDialog>


/*
 * QFontDialog ( QWidget * parent = 0 )
 * QFontDialog ( const QFont & initial, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QFontDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFontDialog;

QT_G_FUNC( hbqt_gcRelease_QFontDialog )
{
   QFontDialog  * ph = NULL ;
   QGC_POINTER_QFontDialog * p = ( QGC_POINTER_QFontDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFontDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFontDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFontDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QFontDialog * p = ( QGC_POINTER_QFontDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QFontDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFontDialog >( ( QFontDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontDialog;
   p->type = HBQT_TYPE_QFontDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTDIALOG )
{
   QFontDialog * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontDialog( hbqt_par_QWidget( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontDialog( *hbqt_par_QFont( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = new QFontDialog( 0 ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontDialog( ( void * ) pObj, true ) );
}

/*
 * QFont currentFont () const
 */
HB_FUNC( QT_QFONTDIALOG_CURRENTFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_CURRENTFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) ); p is NULL" ) );
   }
}

/*
 * FontDialogOptions options () const
 */
HB_FUNC( QT_QFONTDIALOG_OPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retni( ( QFontDialog::FontDialogOptions ) ( p )->options() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_OPTIONS FP=hb_retni( ( QFontDialog::FontDialogOptions ) ( p )->options() ); p is NULL" ) );
   }
}

/*
 * QFont selectedFont () const
 */
HB_FUNC( QT_QFONTDIALOG_SELECTEDFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->selectedFont() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_SELECTEDFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->selectedFont() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentFont ( const QFont & font )
 */
HB_FUNC( QT_QFONTDIALOG_SETCURRENTFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_SETCURRENTFONT FP=( p )->setCurrentFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( FontDialogOption option, bool on = true )
 */
HB_FUNC( QT_QFONTDIALOG_SETOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_SETOPTION FP=( p )->setOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOptions ( FontDialogOptions options )
 */
HB_FUNC( QT_QFONTDIALOG_SETOPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setOptions( ( QFontDialog::FontDialogOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_SETOPTIONS FP=( p )->setOptions( ( QFontDialog::FontDialogOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool testOption ( FontDialogOption option ) const
 */
HB_FUNC( QT_QFONTDIALOG_TESTOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_TESTOPTION FP=hb_retl( ( p )->testOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title, FontDialogOptions options )
 */
HB_FUNC( QT_QFONTDIALOG_GETFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), QFontDialog::tr( hb_parc( 5 ) ), ( QFontDialog::FontDialogOptions ) hb_parni( 6 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_GETFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), QFontDialog::tr( hb_parc( 5 ) ), ( QFontDialog::FontDialogOptions ) hb_parni( 6 ) ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const char * name )
 */
HB_FUNC( QT_QFONTDIALOG_GETFONT_1 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hbqt_par_char( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_GETFONT_1 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hbqt_par_char( 5 ) ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title )
 */
HB_FUNC( QT_QFONTDIALOG_GETFONT_2 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), QFontDialog::tr( hb_parc( 5 ) ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_GETFONT_2 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), QFontDialog::tr( hb_parc( 5 ) ) ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QFont getFont ( bool * ok, const QFont & initial, QWidget * parent = 0 )
 */
HB_FUNC( QT_QFONTDIALOG_GETFONT_3 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_GETFONT_3 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QFont getFont ( bool * ok, QWidget * parent = 0 )
 */
HB_FUNC( QT_QFONTDIALOG_GETFONT_4 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, hbqt_par_QWidget( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDIALOG_GETFONT_4 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, hbqt_par_QWidget( 3 ) ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
