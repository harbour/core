/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"
#include "hbthread.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include <QWidget>
#include <QString>
#include <QList>
#include <QKeyEvent>
#include <QAction>
#include <QObject>
#include <QEvent>
#include <QMessageBox>
#include <QFileDialog>

#include <QBitArray>
#include <QBitmap>
#include <QDate>
#include <QHttpRequestHeader>
#include <QTextCursor>
#include <QTextDocumentFragment>
#include <QTextLine>
#include <QWebHistoryItem>
#include <QWebSecurityOrigin>
#include <QWebHitTestResult>
#include <QLocale>
#include <QModelIndex>

/*----------------------------------------------------------------------*/

HB_GARBAGE_FUNC( Q_release )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;
   if( p && p->ph )
   {
      p->func( p->ph );
   }
}

void * hbqt_gcpointer( int iParam )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( Q_release, iParam );

   #if 0
   if( p && p->ph )
      return p->ph;
   hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
   #else
   return ( p && p->ph ? p->ph : hb_parptr( iParam ) );
   #endif
}

void * hbqt_ptrTOgcpointer( void * ptr, QT_G_FUNC_PTR func )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   p->ph = ptr;
   p->func = func;
   return p;
}

int hbqt_getIdByName( QString name )
{
   return 0;
}
/*----------------------------------------------------------------------*/

#endif                  // #if QT_VERSION >= 0x040500



