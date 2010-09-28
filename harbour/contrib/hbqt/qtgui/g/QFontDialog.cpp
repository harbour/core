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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FontDialogOption { NoButtons, DontUseNativeDialog }
 *  flags FontDialogOptions
 */

/*
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // void open ( QObject * receiver, const char * member )
 *  // QFont getFont ( bool * ok, QWidget * parent, const char * name )
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QFontDialog )
{
   QFontDialog  * ph = NULL ;
   HBQT_GC_T_QFontDialog * p = ( HBQT_GC_T_QFontDialog * ) Cargo;

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
   HBQT_GC_T_QFontDialog * p = ( HBQT_GC_T_QFontDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFontDialog ), hbqt_gcFuncs() );

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
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
   }
}

/*
 * FontDialogOptions options () const
 */
HB_FUNC( QT_QFONTDIALOG_OPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      hb_retni( ( QFontDialog::FontDialogOptions ) ( p )->options() );
   }
}

/*
 * QFont selectedFont () const
 */
HB_FUNC( QT_QFONTDIALOG_SELECTEDFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->selectedFont() ), true ) );
   }
}

/*
 * void setCurrentFont ( const QFont & font )
 */
HB_FUNC( QT_QFONTDIALOG_SETCURRENTFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * void setOption ( FontDialogOption option, bool on = true )
 */
HB_FUNC( QT_QFONTDIALOG_SETOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      ( p )->setOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setOptions ( FontDialogOptions options )
 */
HB_FUNC( QT_QFONTDIALOG_SETOPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      ( p )->setOptions( ( QFontDialog::FontDialogOptions ) hb_parni( 2 ) );
   }
}

/*
 * bool testOption ( FontDialogOption option ) const
 */
HB_FUNC( QT_QFONTDIALOG_TESTOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
   {
      hb_retl( ( p )->testOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ) ) );
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
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hb_parstr_utf8( 5, &pText, NULL ), ( QFontDialog::FontDialogOptions ) hb_parni( 6 ) ) ), true ) );
      hb_strfree( pText );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hbqt_par_char( 5 ) ) ), true ) );
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
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hb_parstr_utf8( 5, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, hbqt_par_QWidget( 3 ) ) ), true ) );
   }

   hb_stornl( iOk, 2 );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
