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
 *  enum OptionType { SO_Button, SO_ComboBox, SO_Complex, SO_Default, ..., SO_Q3ListViewItem }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOption>


/*
 * QStyleOption ( int version = QStyleOption::Version, int type = SO_Default )
 * QStyleOption ( const QStyleOption & other )
 * ~QStyleOption ()
 */

typedef struct
{
   QStyleOption * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOption;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOption )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStyleOption   /.\\", p->ph ) );
         delete ( ( QStyleOption * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStyleOption   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStyleOption    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStyleOption    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOption( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOption * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOption;
   p->type = HBQT_TYPE_QStyleOption;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStyleOption", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStyleOption", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLEOPTION )
{
   QStyleOption * pObj = NULL;

   pObj =  new QStyleOption() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOption( ( void * ) pObj, true ) );
}

/*
 * void initFrom ( const QWidget * widget )
 */
HB_FUNC( QT_QSTYLEOPTION_INITFROM )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
   {
      ( p )->initFrom( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * Qt::LayoutDirection direction
 */
HB_FUNC( QT_QSTYLEOPTION_DIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QStyleOption( 1 )->direction );
}

/*
 * QFontMetrics fontMetrics
 */
HB_FUNC( QT_QSTYLEOPTION_FONTMETRICS )
{
   hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( hbqt_par_QStyleOption( 1 )->fontMetrics ), true ) );
}

/*
 * QPalette palette
 */
HB_FUNC( QT_QSTYLEOPTION_PALETTE )
{
   hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( hbqt_par_QStyleOption( 1 )->palette ), true ) );
}

/*
 * QRect rect
 */
HB_FUNC( QT_QSTYLEOPTION_RECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QStyleOption( 1 )->rect ), true ) );
}

/*
 * QStyle::State state
 */
HB_FUNC( QT_QSTYLEOPTION_STATE )
{
   hb_retni( ( QStyle::State ) hbqt_par_QStyleOption( 1 )->state );
}

/*
 * int type
 */
HB_FUNC( QT_QSTYLEOPTION_TYPE )
{
   hb_retni( hbqt_par_QStyleOption( 1 )->type );
}

/*
 * int version
 */
HB_FUNC( QT_QSTYLEOPTION_VERSION )
{
   hb_retni( hbqt_par_QStyleOption( 1 )->version );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
