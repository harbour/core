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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QFontComboBox > pq;
} QGC_POINTER_QFontComboBox;

QT_G_FUNC( hbqt_gcRelease_QFontComboBox )
{
   QGC_POINTER_QFontComboBox * p = ( QGC_POINTER_QFontComboBox * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontComboBox   /.\\   pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QFontComboBox * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontComboBox   \\./   pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFontComboBox          pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontComboBox    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontComboBox    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontComboBox( void * pObj, bool bNew )
{
   QGC_POINTER_QFontComboBox * p = ( QGC_POINTER_QFontComboBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QFontComboBox ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontComboBox;

   if( bNew )
   {
      new( & p->pq ) QPointer< QFontComboBox >( ( QFontComboBox * ) pObj );
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
   void * pObj = NULL;

   pObj = ( QFontComboBox * ) new QFontComboBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFontComboBox( pObj, true ) );
}

/*
 * QFont currentFont () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_CURRENTFONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QFontComboBox( 1 )->currentFont() ), true ) );
}

/*
 * FontFilters fontFilters () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_FONTFILTERS )
{
   hb_retni( ( QFontComboBox::FontFilters ) hbqt_par_QFontComboBox( 1 )->fontFilters() );
}

/*
 * void setFontFilters ( FontFilters filters )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETFONTFILTERS )
{
   hbqt_par_QFontComboBox( 1 )->setFontFilters( ( QFontComboBox::FontFilters ) hb_parni( 2 ) );
}

/*
 * void setWritingSystem ( QFontDatabase::WritingSystem script )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETWRITINGSYSTEM )
{
   hbqt_par_QFontComboBox( 1 )->setWritingSystem( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) );
}

/*
 * QFontDatabase::WritingSystem writingSystem () const
 */
HB_FUNC( QT_QFONTCOMBOBOX_WRITINGSYSTEM )
{
   hb_retni( ( QFontDatabase::WritingSystem ) hbqt_par_QFontComboBox( 1 )->writingSystem() );
}

/*
 * void setCurrentFont ( const QFont & font )
 */
HB_FUNC( QT_QFONTCOMBOBOX_SETCURRENTFONT )
{
   hbqt_par_QFontComboBox( 1 )->setCurrentFont( *hbqt_par_QFont( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
