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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QStringRef>


/*
 * QStringRef ()
 * QStringRef ( const QString * string )
 * QStringRef ( const QStringRef & other )
 * QStringRef ( const QString * string, int position, int length )
 * ~QStringRef ()
 */

typedef struct
{
   QStringRef * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStringRef;

HBQT_GC_FUNC( hbqt_gcRelease_QStringRef )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStringRef   /.\\", p->ph ) );
         delete ( ( QStringRef * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStringRef   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStringRef    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStringRef    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStringRef( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStringRef * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStringRef;
   p->type = HBQT_TYPE_QStringRef;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStringRef", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStringRef", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTRINGREF )
{
   QStringRef * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QStringRef( ( const QString *) hb_parcx( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QStringRef( * hbqt_par_QStringRef( 1 ) ) ;
   }
   else if( hb_pcount() == 3  && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = new QStringRef( ( const QString *) hb_parcx( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ;
   }
   else
   {
      pObj = new QStringRef() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QStringRef( ( void * ) pObj, true ) );
}

/*
 * const QChar at ( int position ) const
 */
HB_FUNC( QT_QSTRINGREF_AT )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->at( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QSTRINGREF_CLEAR )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * int compare ( const QString & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_QSTRINGREF_COMPARE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QStringRef & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_QSTRINGREF_COMPARE_1 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * const QChar * constData () const
 */
HB_FUNC( QT_QSTRINGREF_CONSTDATA )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->constData() ) ), true ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QSTRINGREF_COUNT )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * const QChar * data () const
 */
HB_FUNC( QT_QSTRINGREF_DATA )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->data() ) ), true ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QSTRINGREF_ISEMPTY )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QSTRINGREF_ISNULL )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QSTRINGREF_LENGTH )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->length() );
   }
}

/*
 * int localeAwareCompare ( const QString & other ) const
 */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int localeAwareCompare ( const QStringRef & other ) const
 */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_1 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ) ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QSTRINGREF_POSITION )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->position() );
   }
}

/*
 * int size () const
 */
HB_FUNC( QT_QSTRINGREF_SIZE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->size() );
   }
}

/*
 * const QChar * unicode () const
 */
HB_FUNC( QT_QSTRINGREF_UNICODE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->unicode() ) ), true ) );
   }
}

/*
 * int compare ( const QStringRef & s1, const QString & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_QSTRINGREF_COMPARE_2 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QStringRef & s1, const QStringRef & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_QSTRINGREF_COMPARE_3 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), *hbqt_par_QStringRef( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int localeAwareCompare ( const QStringRef & s1, const QString & s2 )
 */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_2 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int localeAwareCompare ( const QStringRef & s1, const QStringRef & s2 )
 */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_3 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ), *hbqt_par_QStringRef( 3 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
