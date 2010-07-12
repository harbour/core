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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QPicture>


/*
 * QPicture ( int formatVersion = -1 )
 * QPicture ( const QPicture & pic )
 * ~QPicture ()
 */

typedef struct
{
   QPicture * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPicture;

QT_G_FUNC( hbqt_gcRelease_QPicture )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPicture   /.\\", p->ph ) );
         delete ( ( QPicture * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPicture   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPicture    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPicture    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPicture( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QPicture * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPicture;
   p->type = HBQT_TYPE_QPicture;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPicture", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPicture", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPICTURE )
{
   QPicture * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QPicture( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPicture( *hbqt_par_QPicture( 1 ) ) ;
   }
   else
   {
      pObj = new QPicture() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPicture( ( void * ) pObj, true ) );
}

/*
 * QRect boundingRect () const
 */
HB_FUNC( QT_QPICTURE_BOUNDINGRECT )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * const char * data () const
 */
HB_FUNC( QT_QPICTURE_DATA )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retc( ( p )->data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_DATA FP=hb_retc( ( p )->data() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QPICTURE_ISNULL )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool load ( const QString & fileName, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_LOAD )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_LOAD FP=hb_retl( ( p )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool load ( QIODevice * dev, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_LOAD_1 )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->load( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_LOAD_1 FP=hb_retl( ( p )->load( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool play ( QPainter * painter )
 */
HB_FUNC( QT_QPICTURE_PLAY )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->play( hbqt_par_QPainter( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_PLAY FP=hb_retl( ( p )->play( hbqt_par_QPainter( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool save ( const QString & fileName, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_SAVE )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_SAVE FP=hb_retl( ( p )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool save ( QIODevice * dev, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_SAVE_1 )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_SAVE_1 FP=hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setBoundingRect ( const QRect & r )
 */
HB_FUNC( QT_QPICTURE_SETBOUNDINGRECT )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      ( p )->setBoundingRect( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_SETBOUNDINGRECT FP=( p )->setBoundingRect( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setData ( const char * data, uint size )
 */
HB_FUNC( QT_QPICTURE_SETDATA )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      ( p )->setData( hbqt_par_char( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_SETDATA FP=( p )->setData( hbqt_par_char( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * uint size () const
 */
HB_FUNC( QT_QPICTURE_SIZE )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retni( ( p )->size() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPICTURE_SIZE FP=hb_retni( ( p )->size() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
