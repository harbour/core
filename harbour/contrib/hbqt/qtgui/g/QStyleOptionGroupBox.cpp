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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionGroupBox>


/*
 * QStyleOptionGroupBox ()
 * QStyleOptionGroupBox ( const QStyleOptionGroupBox & other )
 */

typedef struct
{
   QStyleOptionGroupBox * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionGroupBox;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionGroupBox )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStyleOptionGroupBox   /.\\", p->ph ) );
         delete ( ( QStyleOptionGroupBox * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStyleOptionGroupBox   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStyleOptionGroupBox    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStyleOptionGroupBox    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionGroupBox( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionGroupBox * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionGroupBox;
   p->type = HBQT_TYPE_QStyleOptionGroupBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStyleOptionGroupBox", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStyleOptionGroupBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLEOPTIONGROUPBOX )
{
   QStyleOptionGroupBox * pObj = NULL;

   pObj =  new QStyleOptionGroupBox() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionGroupBox( ( void * ) pObj, true ) );
}

/*
 * QStyleOptionFrameV2::FrameFeatures features
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_FEATURES )
{
   hb_retni( ( QStyleOptionFrameV2::FrameFeatures ) hbqt_par_QStyleOptionGroupBox( 1 )->features );
}

/*
 * int lineWidth
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_LINEWIDTH )
{
   hb_retni( hbqt_par_QStyleOptionGroupBox( 1 )->lineWidth );
}

/*
 * int midLineWidth
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_MIDLINEWIDTH )
{
   hb_retni( hbqt_par_QStyleOptionGroupBox( 1 )->midLineWidth );
}

/*
 * QString text
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXT )
{
   hb_retstr_utf8( hbqt_par_QStyleOptionGroupBox( 1 )->text.toUtf8().data() );
}

/*
 * Qt::Alignment textAlignment
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXTALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStyleOptionGroupBox( 1 )->textAlignment );
}

/*
 * QColor textColor
 */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXTCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QStyleOptionGroupBox( 1 )->textColor ), true ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
