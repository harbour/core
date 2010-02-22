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
 *  enum Policy { Fixed, Minimum, Maximum, Preferred, ..., Ignored }
 *  enum PolicyFlag { GrowFlag, ExpandFlag, ShrinkFlag, IgnoreFlag }
 *  enum ControlType { DefaultType, ButtonBox, CheckBox, ComboBox, ..., ToolButton }
 *  flags ControlTypes
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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QSizePolicy;

QT_G_FUNC( hbqt_gcRelease_QSizePolicy )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QSizePolicy   /.\\    ph=%p", p->ph ) );
         delete ( ( QSizePolicy * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QSizePolicy   \\./    ph=%p", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QSizePolicy    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QSizePolicy    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizePolicy( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizePolicy;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QSizePolicy                ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QSIZEPOLICY )
{
   void * pObj = NULL;

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

   hb_retptrGC( hbqt_gcAllocate_QSizePolicy( pObj, true ) );
}

/*
 * ControlType controlType () const
 */
HB_FUNC( QT_QSIZEPOLICY_CONTROLTYPE )
{
   hb_retni( ( QSizePolicy::ControlType ) hbqt_par_QSizePolicy( 1 )->controlType() );
}

/*
 * Qt::Orientations expandingDirections () const
 */
HB_FUNC( QT_QSIZEPOLICY_EXPANDINGDIRECTIONS )
{
   hb_retni( ( Qt::Orientations ) hbqt_par_QSizePolicy( 1 )->expandingDirections() );
}

/*
 * bool hasHeightForWidth () const
 */
HB_FUNC( QT_QSIZEPOLICY_HASHEIGHTFORWIDTH )
{
   hb_retl( hbqt_par_QSizePolicy( 1 )->hasHeightForWidth() );
}

/*
 * Policy horizontalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALPOLICY )
{
   hb_retni( ( QSizePolicy::Policy ) hbqt_par_QSizePolicy( 1 )->horizontalPolicy() );
}

/*
 * int horizontalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALSTRETCH )
{
   hb_retni( hbqt_par_QSizePolicy( 1 )->horizontalStretch() );
}

/*
 * void setControlType ( ControlType type )
 */
HB_FUNC( QT_QSIZEPOLICY_SETCONTROLTYPE )
{
   hbqt_par_QSizePolicy( 1 )->setControlType( ( QSizePolicy::ControlType ) hb_parni( 2 ) );
}

/*
 * void setHeightForWidth ( bool dependent )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHEIGHTFORWIDTH )
{
   hbqt_par_QSizePolicy( 1 )->setHeightForWidth( hb_parl( 2 ) );
}

/*
 * void setHorizontalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALPOLICY )
{
   hbqt_par_QSizePolicy( 1 )->setHorizontalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/*
 * void setHorizontalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALSTRETCH )
{
   hbqt_par_QSizePolicy( 1 )->setHorizontalStretch( ( char ) hb_parni( 2 ) );
}

/*
 * void setVerticalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALPOLICY )
{
   hbqt_par_QSizePolicy( 1 )->setVerticalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/*
 * void setVerticalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALSTRETCH )
{
   hbqt_par_QSizePolicy( 1 )->setVerticalStretch( ( char ) hb_parni( 2 ) );
}

/*
 * void transpose ()
 */
HB_FUNC( QT_QSIZEPOLICY_TRANSPOSE )
{
   hbqt_par_QSizePolicy( 1 )->transpose();
}

/*
 * Policy verticalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALPOLICY )
{
   hb_retni( ( QSizePolicy::Policy ) hbqt_par_QSizePolicy( 1 )->verticalPolicy() );
}

/*
 * int verticalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALSTRETCH )
{
   hb_retni( hbqt_par_QSizePolicy( 1 )->verticalStretch() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
