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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 21/22 [ 95.45% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setUrls ( const QList<QUrl> & urls )
 */

#include <QtCore/QPointer>

#include <QtCore/QMimeData>
#include <QtCore/QStringList>
#include <QtCore/QUrl>

/* QMimeData ()
 * ~QMimeData ()
 */

typedef struct
{
   QPointer< QMimeData > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMimeData;

HBQT_GC_FUNC( hbqt_gcRelease_QMimeData )
{
   QMimeData  * ph = NULL ;
   HBQT_GC_T_QMimeData * p = ( HBQT_GC_T_QMimeData * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMimeData   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMimeData   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMimeData          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMimeData    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMimeData    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMimeData( void * pObj, bool bNew )
{
   HBQT_GC_T_QMimeData * p = ( HBQT_GC_T_QMimeData * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMimeData ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMimeData >( ( QMimeData * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMimeData;
   p->type = HBQT_TYPE_QMimeData;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMimeData  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMimeData", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMIMEDATA )
{
   QMimeData * pObj = NULL;

   pObj = new QMimeData() ;

   hb_retptrGC( hbqt_gcAllocate_QMimeData( ( void * ) pObj, true ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMIMEDATA_CLEAR )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * QVariant colorData () const
 */
HB_FUNC( QT_QMIMEDATA_COLORDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->colorData() ), true ) );
   }
}

/*
 * QByteArray data ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_DATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->data( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QStringList formats () const
 */
HB_FUNC( QT_QMIMEDATA_FORMATS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->formats() ), true ) );
   }
}

/*
 * bool hasColor () const
 */
HB_FUNC( QT_QMIMEDATA_HASCOLOR )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retl( ( p )->hasColor() );
   }
}

/*
 * virtual bool hasFormat ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_HASFORMAT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->hasFormat( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool hasHtml () const
 */
HB_FUNC( QT_QMIMEDATA_HASHTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retl( ( p )->hasHtml() );
   }
}

/*
 * bool hasImage () const
 */
HB_FUNC( QT_QMIMEDATA_HASIMAGE )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retl( ( p )->hasImage() );
   }
}

/*
 * bool hasText () const
 */
HB_FUNC( QT_QMIMEDATA_HASTEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retl( ( p )->hasText() );
   }
}

/*
 * bool hasUrls () const
 */
HB_FUNC( QT_QMIMEDATA_HASURLS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retl( ( p )->hasUrls() );
   }
}

/*
 * QString html () const
 */
HB_FUNC( QT_QMIMEDATA_HTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->html().toUtf8().data() );
   }
}

/*
 * QVariant imageData () const
 */
HB_FUNC( QT_QMIMEDATA_IMAGEDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->imageData() ), true ) );
   }
}

/*
 * void removeFormat ( const QString & mimeType )
 */
HB_FUNC( QT_QMIMEDATA_REMOVEFORMAT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeFormat( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setColorData ( const QVariant & color )
 */
HB_FUNC( QT_QMIMEDATA_SETCOLORDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      ( p )->setColorData( *hbqt_par_QVariant( 2 ) );
   }
}

/*
 * void setData ( const QString & mimeType, const QByteArray & data )
 */
HB_FUNC( QT_QMIMEDATA_SETDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setData( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QByteArray( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QMIMEDATA_SETHTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setImageData ( const QVariant & image )
 */
HB_FUNC( QT_QMIMEDATA_SETIMAGEDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      ( p )->setImageData( *hbqt_par_QVariant( 2 ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QMIMEDATA_SETTEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QMIMEDATA_TEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * QList<QUrl> urls () const
 */
HB_FUNC( QT_QMIMEDATA_URLS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QUrl>( ( p )->urls() ), true ) );
   }
}

/*
 * QStringList * hbUrlList() const
 */
HB_FUNC( QT_QMIMEDATA_HBURLLIST )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      QStringList strList;
      foreach ( QUrl url, ( p )->urls() )
      {
         strList << ( QString ) url.toString().toAscii().data();
      }
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( strList ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
