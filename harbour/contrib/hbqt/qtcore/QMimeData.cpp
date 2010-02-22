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
 *  Constructed[ 19/21 [ 90.48% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setUrls ( const QList<QUrl> & urls )
 *  QList<QUrl> urls () const
 */

#include <QtCore/QPointer>

#include <QtCore/QMimeData>
#include <QtCore/QStringList>


/* QMimeData ()
 * ~QMimeData ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QMimeData > pq;
} QGC_POINTER_QMimeData;

QT_G_FUNC( hbqt_gcRelease_QMimeData )
{
   QGC_POINTER_QMimeData * p = ( QGC_POINTER_QMimeData * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMimeData   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QMimeData * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMimeData   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QMimeDataph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QMimeData    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QMimeData    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMimeData( void * pObj, bool bNew )
{
   QGC_POINTER_QMimeData * p = ( QGC_POINTER_QMimeData * ) hb_gcAllocate( sizeof( QGC_POINTER_QMimeData ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMimeData;

   if( bNew )
   {
      new( & p->pq ) QPointer< QMimeData >( ( QMimeData * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QMimeData                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QMIMEDATA )
{
   void * pObj = NULL;

   pObj = new QMimeData() ;

   hb_retptrGC( hbqt_gcAllocate_QMimeData( pObj, true ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMIMEDATA_CLEAR )
{
   hbqt_par_QMimeData( 1 )->clear();
}

/*
 * QVariant colorData () const
 */
HB_FUNC( QT_QMIMEDATA_COLORDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QMimeData( 1 )->colorData() ), true ) );
}

/*
 * QByteArray data ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_DATA )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QMimeData( 1 )->data( QMimeData::tr( hb_parc( 2 ) ) ) ), true ) );
}

/*
 * virtual QStringList formats () const
 */
HB_FUNC( QT_QMIMEDATA_FORMATS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QMimeData( 1 )->formats() ), true ) );
}

/*
 * bool hasColor () const
 */
HB_FUNC( QT_QMIMEDATA_HASCOLOR )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasColor() );
}

/*
 * virtual bool hasFormat ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_HASFORMAT )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasFormat( QMimeData::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool hasHtml () const
 */
HB_FUNC( QT_QMIMEDATA_HASHTML )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasHtml() );
}

/*
 * bool hasImage () const
 */
HB_FUNC( QT_QMIMEDATA_HASIMAGE )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasImage() );
}

/*
 * bool hasText () const
 */
HB_FUNC( QT_QMIMEDATA_HASTEXT )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasText() );
}

/*
 * bool hasUrls () const
 */
HB_FUNC( QT_QMIMEDATA_HASURLS )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasUrls() );
}

/*
 * QString html () const
 */
HB_FUNC( QT_QMIMEDATA_HTML )
{
   hb_retc( hbqt_par_QMimeData( 1 )->html().toAscii().data() );
}

/*
 * QVariant imageData () const
 */
HB_FUNC( QT_QMIMEDATA_IMAGEDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QMimeData( 1 )->imageData() ), true ) );
}

/*
 * void removeFormat ( const QString & mimeType )
 */
HB_FUNC( QT_QMIMEDATA_REMOVEFORMAT )
{
   hbqt_par_QMimeData( 1 )->removeFormat( QMimeData::tr( hb_parc( 2 ) ) );
}

/*
 * void setColorData ( const QVariant & color )
 */
HB_FUNC( QT_QMIMEDATA_SETCOLORDATA )
{
   hbqt_par_QMimeData( 1 )->setColorData( *hbqt_par_QVariant( 2 ) );
}

/*
 * void setData ( const QString & mimeType, const QByteArray & data )
 */
HB_FUNC( QT_QMIMEDATA_SETDATA )
{
   hbqt_par_QMimeData( 1 )->setData( QMimeData::tr( hb_parc( 2 ) ), *hbqt_par_QByteArray( 3 ) );
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QMIMEDATA_SETHTML )
{
   hbqt_par_QMimeData( 1 )->setHtml( QMimeData::tr( hb_parc( 2 ) ) );
}

/*
 * void setImageData ( const QVariant & image )
 */
HB_FUNC( QT_QMIMEDATA_SETIMAGEDATA )
{
   hbqt_par_QMimeData( 1 )->setImageData( *hbqt_par_QVariant( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QMIMEDATA_SETTEXT )
{
   hbqt_par_QMimeData( 1 )->setText( QMimeData::tr( hb_parc( 2 ) ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QMIMEDATA_TEXT )
{
   hb_retc( hbqt_par_QMimeData( 1 )->text().toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
