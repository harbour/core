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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGroupBox>


/*
 * QGroupBox ( QWidget * parent = 0 )
 * QGroupBox ( const QString & title, QWidget * parent = 0 )
 * ~QGroupBox ()
 */

typedef struct
{
   QPointer< QGroupBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGroupBox;

HBQT_GC_FUNC( hbqt_gcRelease_QGroupBox )
{
   QGroupBox  * ph = NULL ;
   HBQT_GC_T_QGroupBox * p = ( HBQT_GC_T_QGroupBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGroupBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGroupBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QGroupBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGroupBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGroupBox    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGroupBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QGroupBox * p = ( HBQT_GC_T_QGroupBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGroupBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGroupBox >( ( QGroupBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGroupBox;
   p->type = HBQT_TYPE_QGroupBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGroupBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGroupBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGROUPBOX )
{
   QGroupBox * pObj = NULL;

   pObj = ( QGroupBox * ) new QGroupBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QGroupBox( ( void * ) pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QGROUPBOX_ALIGNMENT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   }
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QGROUPBOX_ISCHECKABLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      hb_retl( ( p )->isCheckable() );
   }
}

/*
 * bool isChecked () const
 */
HB_FUNC( QT_QGROUPBOX_ISCHECKED )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      hb_retl( ( p )->isChecked() );
   }
}

/*
 * bool isFlat () const
 */
HB_FUNC( QT_QGROUPBOX_ISFLAT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      hb_retl( ( p )->isFlat() );
   }
}

/*
 * void setAlignment ( int alignment )
 */
HB_FUNC( QT_QGROUPBOX_SETALIGNMENT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      ( p )->setAlignment( hb_parni( 2 ) );
   }
}

/*
 * void setCheckable ( bool checkable )
 */
HB_FUNC( QT_QGROUPBOX_SETCHECKABLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      ( p )->setCheckable( hb_parl( 2 ) );
   }
}

/*
 * void setFlat ( bool flat )
 */
HB_FUNC( QT_QGROUPBOX_SETFLAT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      ( p )->setFlat( hb_parl( 2 ) );
   }
}

/*
 * void setTitle ( const QString & title )
 */
HB_FUNC( QT_QGROUPBOX_SETTITLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString title () const
 */
HB_FUNC( QT_QGROUPBOX_TITLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
   }
}

/*
 * void setChecked ( bool checked )
 */
HB_FUNC( QT_QGROUPBOX_SETCHECKED )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      ( p )->setChecked( hb_parl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
