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
#include "hbstack.h"
#include "hbthread.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

//#define __debug__
#if defined(__debug__)
    #include <windows.h>   //////////////////////////////////////////
    static char str[50];   //////////////////////////////////////////
#endif

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

int hbqt_getIdByName( QString obj )
{
   if(      obj == ( QString ) "QBitArray"              )
      return 1;
   else if( obj == ( QString ) "QBitmap"                )
      return 2;
   else if( obj == ( QString ) "QBrush"                 )
      return 3;
   else if( obj == ( QString ) "QByteArray"             )
      return 4;
   else if( obj == ( QString ) "QColor"                 )
      return 5;
   else if( obj == ( QString ) "QCursor"                )
      return 6;
   else if( obj == ( QString ) "QDate"                  )
      return 7;
   else if( obj == ( QString ) "QDateTime"              )
      return 8;
   else if( obj == ( QString ) "QDir"                   )
      return 9;
   else if( obj == ( QString ) "QFileInfoList"          )
      return 10;
   else if( obj == ( QString ) "QFont"                  )
      return 11;
   else if( obj == ( QString ) "QFontInfo"              )
      return 12;
   else if( obj == ( QString ) "QFontMetrics"           )
      return 13;
   else if( obj == ( QString ) "QGradientStops"         )
      return 14;
   else if( obj == ( QString ) "QHttpRequestHeader"     )
      return 15;
   else if( obj == ( QString ) "QHttpResponseHeader"    )
      return 16;
   else if( obj == ( QString ) "QIcon"                  )
      return 17;
   else if( obj == ( QString ) "QImage"                 )
      return 18;
   else if( obj == ( QString ) "QKeySequence"           )
      return 19;
   else if( obj == ( QString ) "QLine"                  )
      return 20;
   else if( obj == ( QString ) "QLineF"                 )
      return 21;
   else if( obj == ( QString ) "QLocale"                )
      return 22;
   else if( obj == ( QString ) "QMatrix"                )
      return 23;
   else if( obj == ( QString ) "QModelIndex"            )
      return 24;
   else if( obj == ( QString ) "QObjectList"            )
      return 25;
   else if( obj == ( QString ) "QPainterPath"           )
      return 26;
   else if( obj == ( QString ) "QPalette"               )
      return 27;
   else if( obj == ( QString ) "QPen"                   )
      return 28;
   else if( obj == ( QString ) "QPixmap"                )
      return 29;
   else if( obj == ( QString ) "QPointF"                )
      return 30;
   else if( obj == ( QString ) "QRect"                  )
      return 31;
   else if( obj == ( QString ) "QRectF"                 )
      return 32;
   else if( obj == ( QString ) "QRegExp"                )
      return 33;
   else if( obj == ( QString ) "QRegion"                )
      return 34;
   else if( obj == ( QString ) "QSize"                  )
      return 35;
   else if( obj == ( QString ) "QSizeF"                 )
      return 36;
   else if( obj == ( QString ) "QSizePolicy"            )
      return 37;
   else if( obj == ( QString ) "QStringList"            )
      return 38;
   else if( obj == ( QString ) "QTextBlockFormat"       )
      return 39;
   else if( obj == ( QString ) "QTextCharFormat"        )
      return 40;
   else if( obj == ( QString ) "QTextCursor"            )
      return 41;
   else if( obj == ( QString ) "QTextDocumentFragment"  )
      return 42;
   else if( obj == ( QString ) "QTextFormat"            )
      return 43;
   else if( obj == ( QString ) "QTextFrameFormat"       )
      return 44;
   else if( obj == ( QString ) "QTextImageFormat"       )
      return 45;
   else if( obj == ( QString ) "QTextLength"            )
      return 46;
   else if( obj == ( QString ) "QTextLine"              )
      return 47;
   else if( obj == ( QString ) "QTextListFormat"        )
      return 48;
   else if( obj == ( QString ) "QTextOption"            )
      return 49;
   else if( obj == ( QString ) "QTextTableCellFormat"   )
      return 50;
   else if( obj == ( QString ) "QTextTableFormat"       )
      return 51;
   else if( obj == ( QString ) "QTime"                  )
      return 52;
   else if( obj == ( QString ) "QTransform"             )
      return 53;
   else if( obj == ( QString ) "QUrl"                   )
      return 54;
   else if( obj == ( QString ) "QVariant"               )
      return 55;
   else if( obj == ( QString ) "QWebHistoryItem"        )
      return 56;
   else if( obj == ( QString ) "QWebHitTestResult"      )
      return 57;
   else if( obj == ( QString ) "QWebSecurityOrigin"     )
      return 58;
   else if( obj == ( QString ) "QWidgetList"            )
      return 59;
   else
      return 0;
}

HB_GARBAGE_FUNC( Q_release )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;
   if( p && p->ph )
   {

#if defined(__debug__)
   hb_snprintf( str, sizeof( str ), "Q_release  0  %i", p->type );  OutputDebugString( str );
#endif

      switch( p->type )
      {
      case 1001:
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();

#if defined(__debug__)
   hb_snprintf( str, sizeof( str ), "Q_release  1  %s", m->className() );  OutputDebugString( str );
#endif
         /*  It is strange that I receive some objects with className as "QObject",
          *  which in-fact are never created as such in .prg code. And releasing
          *  such object is
          */
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete  ( ( QObject             * ) p->ph ) ;
         }
         break;
      }
      case 1:
         delete  ( ( QBitArray              * ) p->ph ) ;
         break;
      case 2:
         delete  ( ( QBitmap                * ) p->ph ) ;
         break;
      case 3:
         delete  ( ( QBrush                 * ) p->ph ) ;
         break;
      case 4:
         delete  ( ( QByteArray             * ) p->ph ) ;
         break;
      case 5:
         delete  ( ( QColor                 * ) p->ph ) ;
         break;
      case 6:
         delete  ( ( QCursor                * ) p->ph ) ;
         break;
      case 7:
         delete  ( ( QDate                  * ) p->ph ) ;
         break;
      case 8:
         delete  ( ( QDateTime              * ) p->ph ) ;
         break;
      case 9:
         delete  ( ( QDir                   * ) p->ph ) ;
         break;
      case 10:
         delete  ( ( QFileInfoList          * ) p->ph ) ;
         break;
      case 11:
         delete  ( ( QFont                  * ) p->ph ) ;
         break;
      case 12:
         delete  ( ( QFontInfo              * ) p->ph ) ;
         break;
      case 13:
         delete  ( ( QFontMetrics           * ) p->ph ) ;
         break;
      case 14:
         delete  ( ( QGradientStops         * ) p->ph ) ;
         break;
      case 15:
         delete  ( ( QHttpRequestHeader     * ) p->ph ) ;
         break;
      case 16:
         delete  ( ( QHttpResponseHeader    * ) p->ph ) ;
         break;
      case 17:
         delete  ( ( QIcon                  * ) p->ph ) ;
         break;
      case 18:
         delete  ( ( QImage                 * ) p->ph ) ;
         break;
      case 19:
         delete  ( ( QKeySequence           * ) p->ph ) ;
         break;
      case 20:
         delete  ( ( QLine                  * ) p->ph ) ;
         break;
      case 21:
         delete  ( ( QLineF                 * ) p->ph ) ;
         break;
      case 22:
         delete  ( ( QLocale                * ) p->ph ) ;
         break;
      case 23:
         delete  ( ( QMatrix                * ) p->ph ) ;
         break;
      case 24:
         delete  ( ( QModelIndex            * ) p->ph ) ;
         break;
      case 25:
         delete  ( ( QObjectList            * ) p->ph ) ;
         break;
      case 26:
         delete  ( ( QPainterPath           * ) p->ph ) ;
         break;
      case 27:
         delete  ( ( QPalette               * ) p->ph ) ;
         break;
      case 28:
         delete  ( ( QPen                   * ) p->ph ) ;
         break;
      case 29:
         delete  ( ( QPixmap                * ) p->ph ) ;
         break;
      case 30:
         delete  ( ( QPointF                * ) p->ph ) ;
         break;
      case 31:
         delete  ( ( QRect                  * ) p->ph ) ;
         break;
      case 32:
         delete  ( ( QRectF                 * ) p->ph ) ;
         break;
      case 33:
         delete  ( ( QRegExp                * ) p->ph ) ;
         break;
      case 34:
         delete  ( ( QRegion                * ) p->ph ) ;
         break;
      case 35:
         delete  ( ( QSize                  * ) p->ph ) ;
         break;
      case 36:
         delete  ( ( QSizeF                 * ) p->ph ) ;
         break;
      case 37:
         delete  ( ( QSizePolicy            * ) p->ph ) ;
         break;
      case 38:
         delete  ( ( QStringList            * ) p->ph ) ;
         break;
      case 39:
         delete  ( ( QTextBlockFormat       * ) p->ph ) ;
         break;
      case 40:
         delete  ( ( QTextCharFormat        * ) p->ph ) ;
         break;
      case 41:
         delete  ( ( QTextCursor            * ) p->ph ) ;
         break;
      case 42:
         delete  ( ( QTextDocumentFragment  * ) p->ph ) ;
         break;
      case 43:
         delete  ( ( QTextFormat            * ) p->ph ) ;
         break;
      case 44:
         delete  ( ( QTextFrameFormat       * ) p->ph ) ;
         break;
      case 45:
         delete  ( ( QTextImageFormat       * ) p->ph ) ;
         break;
      case 46:
         delete  ( ( QTextLength            * ) p->ph ) ;
         break;
      case 47:
         delete  ( ( QTextLine              * ) p->ph ) ;
         break;
      case 48:
         delete  ( ( QTextListFormat        * ) p->ph ) ;
         break;
      case 49:
         delete  ( ( QTextOption            * ) p->ph ) ;
         break;
      case 50:
         delete  ( ( QTextTableCellFormat   * ) p->ph ) ;
         break;
      case 51:
         delete  ( ( QTextTableFormat       * ) p->ph ) ;
         break;
      case 52:
         delete  ( ( QTime                  * ) p->ph ) ;
         break;
      case 53:
         delete  ( ( QTransform             * ) p->ph ) ;
         break;
      case 54:
         delete  ( ( QUrl                   * ) p->ph ) ;
         break;
      case 55:
         delete  ( ( QVariant               * ) p->ph ) ;
         break;
      case 56:
         delete  ( ( QWebHistoryItem        * ) p->ph ) ;
         break;
      case 57:
         delete  ( ( QWebHitTestResult      * ) p->ph ) ;
         break;
      case 58:
         delete  ( ( QWebSecurityOrigin     * ) p->ph ) ;
         break;
      case 59:
         delete  ( ( QWidgetList            * ) p->ph ) ;
         break;
      }
      p->ph = NULL;
      p = NULL;
   }
#if defined(__debug__)
   hb_snprintf( str, sizeof( str ), "======================" );  OutputDebugString( str );
#endif
}

void * hbqt_gcpointer( int iParam )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( Q_release, iParam );

   return ( p && p->ph ? p->ph : hb_parptr( iParam ) );
}

void * hbqt_ptrTOgcpointer( void * ptr )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   p->ph = ptr;
   return p;
}

/*----------------------------------------------------------------------*/

#endif                  // #if QT_VERSION >= 0x040500



