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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtGui/QTextImageFormat>


/*
 * QTextImageFormat ()
 *
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT )
{
   hb_retptr( ( QTextImageFormat* ) new QTextImageFormat() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_DESTROY )
{

}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_HEIGHT )
{
   hb_retnd( hbqt_par_QTextImageFormat( 1 )->height() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_ISVALID )
{
   hb_retl( hbqt_par_QTextImageFormat( 1 )->isValid() );
}

/*
 * QString name () const
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_NAME )
{
   hb_retc( hbqt_par_QTextImageFormat( 1 )->name().toAscii().data() );
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETHEIGHT )
{
   hbqt_par_QTextImageFormat( 1 )->setHeight( hb_parnd( 2 ) );
}

/*
 * void setName ( const QString & name )
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETNAME )
{
   hbqt_par_QTextImageFormat( 1 )->setName( hbqt_par_QString( 2 ) );
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETWIDTH )
{
   hbqt_par_QTextImageFormat( 1 )->setWidth( hb_parnd( 2 ) );
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTIMAGEFORMAT_WIDTH )
{
   hb_retnd( hbqt_par_QTextImageFormat( 1 )->width() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
