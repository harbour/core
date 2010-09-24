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
 *  enum Policy { Fixed, Minimum, Maximum, Preferred, ..., Ignored }
 *  enum PolicyFlag { GrowFlag, ExpandFlag, ShrinkFlag, IgnoreFlag }
 *  enum ControlType { DefaultType, ButtonBox, CheckBox, ComboBox, ..., ToolButton }
 *  flags ControlTypes
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSizePolicy>


/*
 * QSizePolicy ()
 * QSizePolicy ( Policy horizontal, Policy vertical )
 * QSizePolicy ( Policy horizontal, Policy vertical, ControlType type )
 */

typedef struct
{
   QSizePolicy * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSizePolicy;

HBQT_GC_FUNC( hbqt_gcRelease_QSizePolicy )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QSizePolicy   /.\\", p->ph ) );
         delete ( ( QSizePolicy * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QSizePolicy   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSizePolicy    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSizePolicy    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizePolicy( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSizePolicy * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizePolicy;
   p->type = HBQT_TYPE_QSizePolicy;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSizePolicy", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSizePolicy", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSIZEPOLICY )
{
   QSizePolicy * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSizePolicy( ( QSizePolicy::Policy ) hb_parni( 1 ), ( QSizePolicy::Policy ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QSizePolicy( *hbqt_par_QSizePolicy( 1 ) ) ;
   }
   else
   {
      pObj = new QSizePolicy() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSizePolicy( ( void * ) pObj, true ) );
}

/*
 * ControlType controlType () const
 */
HB_FUNC( QT_QSIZEPOLICY_CONTROLTYPE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( QSizePolicy::ControlType ) ( p )->controlType() );
   }
}

/*
 * Qt::Orientations expandingDirections () const
 */
HB_FUNC( QT_QSIZEPOLICY_EXPANDINGDIRECTIONS )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
   }
}

/*
 * bool hasHeightForWidth () const
 */
HB_FUNC( QT_QSIZEPOLICY_HASHEIGHTFORWIDTH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retl( ( p )->hasHeightForWidth() );
   }
}

/*
 * Policy horizontalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( QSizePolicy::Policy ) ( p )->horizontalPolicy() );
   }
}

/*
 * int horizontalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( p )->horizontalStretch() );
   }
}

/*
 * void setControlType ( ControlType type )
 */
HB_FUNC( QT_QSIZEPOLICY_SETCONTROLTYPE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setControlType( ( QSizePolicy::ControlType ) hb_parni( 2 ) );
   }
}

/*
 * void setHeightForWidth ( bool dependent )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHEIGHTFORWIDTH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setHeightForWidth( hb_parl( 2 ) );
   }
}

/*
 * void setHorizontalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setHorizontalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
   }
}

/*
 * void setHorizontalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setHorizontalStretch( ( char ) hb_parni( 2 ) );
   }
}

/*
 * void setVerticalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setVerticalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
   }
}

/*
 * void setVerticalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->setVerticalStretch( ( char ) hb_parni( 2 ) );
   }
}

/*
 * void transpose ()
 */
HB_FUNC( QT_QSIZEPOLICY_TRANSPOSE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      ( p )->transpose();
   }
}

/*
 * Policy verticalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( QSizePolicy::Policy ) ( p )->verticalPolicy() );
   }
}

/*
 * int verticalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
   {
      hb_retni( ( p )->verticalStretch() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
