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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QAbstractButton *> buttons () const
 */

#include <QtCore/QPointer>

#include <QtGui/QButtonGroup>


/*
 * QButtonGroup ( QObject * parent = 0 )
 * ~QButtonGroup ()
 */

QT_G_FUNC( release_QButtonGroup )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QButtonGroup                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QButtonGroup * ) ph )->~QButtonGroup();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QButtonGroup" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QButtonGroup" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QBUTTONGROUP )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QButtonGroup > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QButtonGroup                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QButtonGroup* ) new QButtonGroup( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = ( QButtonGroup* ) new QButtonGroup() ;
   }

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QButtonGroup;

   hb_retptrGC( p );
}
/*
 * void addButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON )
{
   hbqt_par_QButtonGroup( 1 )->addButton( hbqt_par_QAbstractButton( 2 ) );
}

/*
 * void addButton ( QAbstractButton * button, int id )
 */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON_1 )
{
   hbqt_par_QButtonGroup( 1 )->addButton( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
}

/*
 * QAbstractButton * button ( int id ) const
 */
HB_FUNC( QT_QBUTTONGROUP_BUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QButtonGroup( 1 )->button( hb_parni( 2 ) ) );
}

/*
 * QAbstractButton * checkedButton () const
 */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDBUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QButtonGroup( 1 )->checkedButton() );
}

/*
 * int checkedId () const
 */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDID )
{
   hb_retni( hbqt_par_QButtonGroup( 1 )->checkedId() );
}

/*
 * bool exclusive () const
 */
HB_FUNC( QT_QBUTTONGROUP_EXCLUSIVE )
{
   hb_retl( hbqt_par_QButtonGroup( 1 )->exclusive() );
}

/*
 * int id ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QBUTTONGROUP_ID )
{
   hb_retni( hbqt_par_QButtonGroup( 1 )->id( hbqt_par_QAbstractButton( 2 ) ) );
}

/*
 * void removeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QBUTTONGROUP_REMOVEBUTTON )
{
   hbqt_par_QButtonGroup( 1 )->removeButton( hbqt_par_QAbstractButton( 2 ) );
}

/*
 * void setExclusive ( bool )
 */
HB_FUNC( QT_QBUTTONGROUP_SETEXCLUSIVE )
{
   hbqt_par_QButtonGroup( 1 )->setExclusive( hb_parl( 2 ) );
}

/*
 * void setId ( QAbstractButton * button, int id )
 */
HB_FUNC( QT_QBUTTONGROUP_SETID )
{
   hbqt_par_QButtonGroup( 1 )->setId( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
