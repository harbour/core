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
 *  enum FontFilter { AllFonts, ScalableFonts, NonScalableFonts, MonospacedFonts, ProportionalFonts }
 *  flags FontFilters
 */

#include <QtCore/QPointer>

#include <QtGui/QFontComboBox>


/*
 * QFontComboBox ( QWidget * parent = 0 )
 * ~QFontComboBox ()
 */

typedef struct
{
   QPointer< QFontComboBox > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFontComboBox;

QT_G_FUNC( hbqt_gcRelease_QFontComboBox )
{
   QFontComboBox  * ph = NULL ;
   QGC_POINTER_QFontComboBox * p = ( QGC_POINTER_QFontComboBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFontComboBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFontComboBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFontComboBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontComboBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontComboBox    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontComboBox( void * pObj, bool bNew )
{
   QGC_POINTER_QFontComboBox * p = ( QGC_POINTER_QFontComboBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QFontComboBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFontComboBox >( ( QFontComboBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontComboBox;
   p->type = HBQT_TYPE_QFontComboBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontComboBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontComboBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTCOMBOBOX )
{
   QFontComboBox * pObj = NULL;

   pObj = ( QFontComboBox * ) new QFontComboBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFontComboBox( ( void * ) pObj, true ) );
}

/*
 * QFont currentFont () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_CURRENTFONT )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_CURRENTFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) ); p is NULL" ) );
   }
}

/*
 * FontFilters fontFilters () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_FONTFILTERS )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retni( ( QFontComboBox::FontFilters ) ( p )->fontFilters() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_FONTFILTERS FP=hb_retni( ( QFontComboBox::FontFilters ) ( p )->fontFilters() ); p is NULL" ) );
   }
}

/*
 * void setFontFilters ( FontFilters filters )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETFONTFILTERS )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setFontFilters( ( QFontComboBox::FontFilters ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_SETFONTFILTERS FP=( p )->setFontFilters( ( QFontComboBox::FontFilters ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWritingSystem ( QFontDatabase::WritingSystem script )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETWRITINGSYSTEM )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setWritingSystem( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_SETWRITINGSYSTEM FP=( p )->setWritingSystem( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QFontDatabase::WritingSystem writingSystem () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_WRITINGSYSTEM )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retni( ( QFontDatabase::WritingSystem ) ( p )->writingSystem() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_WRITINGSYSTEM FP=hb_retni( ( QFontDatabase::WritingSystem ) ( p )->writingSystem() ); p is NULL" ) );
   }
}

/*
 * void setCurrentFont ( const QFont & font )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETCURRENTFONT )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTCOMBOBOX_SETCURRENTFONT FP=( p )->setCurrentFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
