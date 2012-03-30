/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2011 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *    Adopted to new GT API
 *
 * www - http://www.harbour-project.org
 *
 * See COPYING for licensing terms.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "gtqtc.h"

#define WM_MY_UPDATE_CARET         1700

static  int             s_GtId;
static  HB_GT_FUNCS     SuperTable;
#define HB_GTSUPER      (&SuperTable)
#define HB_GTID_PTR     (&s_GtId)

#define HB_GTWVT_GET(p) ( ( PHB_GTWVT ) HB_GTLOCAL( p ) )

static  HB_CRITICAL_NEW( s_wvtMtx );
#define HB_WVT_LOCK     hb_threadEnterCriticalSection( &s_wvtMtx );
#define HB_WVT_UNLOCK   hb_threadLeaveCriticalSection( &s_wvtMtx );

static  PHB_GTWVT       s_wvtWindows[ WVT_MAX_WINDOWS ];
static  int             s_wvtCount = 0;

/*----------------------------------------------------------------------*/
#if 0
static void DebugIt( char* text, int iVal, int iVal2 )
{
   char   buf[ 100 ];
   wsprintf( buf, text, iVal, iVal2 );
   OutputDebugString( buf );

#if 0
   HB_SYMBOL_UNUSED( text );
   HB_SYMBOL_UNUSED( iVal );
   HB_SYMBOL_UNUSED( iVal2 );
#endif
}
#endif
/*----------------------------------------------------------------------*/

static bool hb_gt_wvt_Alloc( PHB_GTWVT pWVT )
{
   bool fOK = HB_FALSE;

   HB_WVT_LOCK

   if( s_wvtCount < WVT_MAX_WINDOWS )
   {
      int iPos = 0;
      do
      {
         if( s_wvtWindows[ iPos ] == NULL )
         {
            s_wvtWindows[ iPos ] = pWVT;
            pWVT->iHandle = iPos;
            fOK = HB_TRUE;
            ++s_wvtCount;
            break;
         }
         ++iPos;
      }
      while( iPos < WVT_MAX_WINDOWS );
   }
   HB_WVT_UNLOCK

   return fOK;
}

static void hb_gt_wvt_Free( PHB_GTWVT pWVT )
{
   if( pWVT->qWnd->_drawingArea->_basicTimer->isActive() )
   {
      pWVT->qWnd->_drawingArea->_basicTimer->stop();
   }
   if( pWVT->qEventLoop )
   {
      pWVT->qEventLoop->exit();
   }

   pWVT->qWnd->~MainWindow();

   s_wvtWindows[ pWVT->iHandle ] = NULL;
#if 0
   if( --s_wvtCount == 0 )
   {
      Qapplication::quit();
   }
#endif
   hb_xfree( pWVT );
}

static PHB_GTWVT hb_gt_wvt_New( PHB_GT pGT, int iCmdShow )
{
   PHB_GTWVT pWVT;

   pWVT = ( PHB_GTWVT ) hb_xgrab( sizeof( HB_GTWVT ) );
   memset( pWVT, 0, sizeof( HB_GTWVT ) );
   pWVT->pGT = pGT;

   if( !hb_gt_wvt_Alloc( pWVT ) )
   {
      hb_xfree( pWVT );
      return NULL;
   }

   pWVT->iCmdShow          = iCmdShow;
   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   /* THESE are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.setX( WVT_DEFAULT_FONT_WIDTH );
   pWVT->PTEXTSIZE.setY( WVT_DEFAULT_FONT_HEIGHT );
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = 0;
   pWVT->fontQuality       = 0;
   hb_strncpy( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, sizeof( pWVT->fontFace ) - 1 );

   pWVT->CaretExist        = HB_FALSE;
   pWVT->CaretHidden       = HB_TRUE;
   pWVT->CaretSize         = 0;
   pWVT->CaretWidth        = 0;
   pWVT->MousePos.setX( 0 );
   pWVT->MousePos.setY( 0 );
   pWVT->MouseMove         = HB_TRUE;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLast           = 0;

   pWVT->CenterWindow      = HB_TRUE;        /* Default is to always display window in centre of screen */
   pWVT->CodePage          = 255;         /* GetACP(); - set code page to default system */

   pWVT->AltF4Close        = HB_FALSE;
   pWVT->fInit             = HB_FALSE;
   pWVT->bMaximized        = HB_FALSE;
   pWVT->bBeingMarked      = HB_FALSE;
   pWVT->bBeginMarked      = HB_FALSE;

   pWVT->pszSelectCopy     = hb_strdup( "Mark and Copy" );
   pWVT->bSelectCopy       = HB_TRUE;
   pWVT->bResizable        = HB_TRUE;
   pWVT->bClosable         = HB_TRUE;

   pWVT->ResizeMode        = HB_GTI_RESIZEMODE_FONT;

   pWVT->bResizing         = HB_FALSE;
   pWVT->bAlreadySizing    = HB_FALSE;

#ifndef HB_CDP_SUPPORT_OFF
   pWVT->hostCDP    = hb_vmCDP();
#if defined( UNICODE )
   pWVT->inCDP      = hb_vmCDP();
   pWVT->boxCDP     = hb_cdpFind( "EN" );
#else
   {
      int i;
      for( i = 0; i < 256; ++i )
         pWVT->chrTransTbl[ i ] = pWVT->keyTransTbl[ i ] = ( HB_BYTE ) i;
   }
#endif
#endif

   return pWVT;
}

static void hb_gt_wvt_SetWindowFlags( PHB_GTWVT pWVT, Qt::WindowFlags flags )
{
   pWVT->qWnd->setWindowFlags( flags );
   #if 1
   QPoint pos = pWVT->qWnd->pos();
   if( pos.x() < 0 )
      pos.setX( 0 );
   if( pos.y() < 0 )
      pos.setY( 0 );
   pWVT->qWnd->move( pos );
   #endif
   pWVT->qWnd->setFocus();
   pWVT->qWnd->_drawingArea->setFocus( Qt::MouseFocusReason );
   pWVT->qWnd->show();
}

static int hb_gt_wvt_FireEvent( PHB_GTWVT pWVT, int nEvent )
{
   int nResult = 0; /* Unhandled */

   if( pWVT->pGT->pNotifierBlock )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pEvent = hb_itemPutNI( NULL, nEvent );
         nResult = hb_itemGetNI( hb_vmEvalBlockV( ( PHB_ITEM ) pWVT->pGT->pNotifierBlock, 1, pEvent ) );
         hb_itemRelease( pEvent );
         hb_vmRequestRestore();
      }
   }
   return nResult;
}

#if 0    /*   I M P O R T A N T   */
static QPoint hb_gt_wvt_QGetXYFromColRow( PHB_GTWVT pWVT, int col, int row )
{
   QPoint xy;
   xy.setX( col * pWVT->PTEXTSIZE.x() );
   xy.setY( row * pWVT->PTEXTSIZE.y() );
   return xy;
}
#endif

static QRect hb_gt_wvt_QGetXYFromColRowRect( PHB_GTWVT pWVT, QRect colrow )
{
   QRect xy;
   xy.setLeft( colrow.left() * pWVT->PTEXTSIZE.x() );
   xy.setTop( colrow.top()  * pWVT->PTEXTSIZE.y() );
   xy.setRight( ( colrow.right()  + 1 ) * pWVT->PTEXTSIZE.x() );
   xy.setBottom( ( colrow.bottom() + 1 ) * pWVT->PTEXTSIZE.y() );
   return xy;
}

static void hb_gt_wvt_QUpdateCaret( PHB_GTWVT pWVT )
{
   int iRow, iCol, iStyle, iCaretSize;

   /* Restore previous cell value */
   pWVT->qWnd->_drawingArea->displayCell( pWVT->qWnd->_drawingArea->_crtLastRow, pWVT->qWnd->_drawingArea->_crtLastCol );

   HB_GTSELF_GETSCRCURSOR( pWVT->pGT, &iRow, &iCol, &iStyle );

   if( iRow < 0 || iCol < 0 || iRow >= pWVT->ROWS || iCol >= pWVT->COLS )
   {
      iCaretSize = 0;
   }
   else switch( iStyle )
   {
      case SC_INSERT:
         iCaretSize = pWVT->PTEXTSIZE.y() / 2;
         break;
      case SC_SPECIAL1:
         iCaretSize = pWVT->PTEXTSIZE.y() * 3 / 4;
         break;
      case SC_SPECIAL2:
         iCaretSize = pWVT->PTEXTSIZE.y();
         break;
      case SC_NORMAL:
         iCaretSize = pWVT->PTEXTSIZE.y() / 4;
         break;
      default:
         iCaretSize = 0;
         break;
   }

   if( iCaretSize == 0 )
   {
      if( pWVT->CaretExist && !pWVT->CaretHidden )
      {
         pWVT->qWnd->_drawingArea->hideCaret();
         pWVT->CaretHidden = HB_TRUE;
      }
   }
   else
   {
      if( iCaretSize != pWVT->CaretSize || pWVT->PTEXTSIZE.x() != pWVT->CaretWidth ||
          !pWVT->CaretExist )
      {
         pWVT->CaretSize  = iCaretSize;
         pWVT->CaretWidth = pWVT->PTEXTSIZE.x();
         pWVT->CaretExist = pWVT->qWnd->_drawingArea->createCaret( pWVT->PTEXTSIZE.x(), pWVT->CaretSize );
      }
      if( pWVT->CaretExist )
      {
         pWVT->qWnd->_drawingArea->setCaretPos( iCol, iRow );
         pWVT->qWnd->_drawingArea->showCaret();
         pWVT->CaretHidden = HB_FALSE;
      }
   }
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_wvt_AddCharToInputQueue( PHB_GTWVT pWVT, int iKey )
{
   int iPos = pWVT->keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      /* Clipper strips repeated mouse movemnt - let's do the same */
      if( pWVT->keyLast == iKey && pWVT->keyPointerIn != pWVT->keyPointerOut )
         return;
   }
   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   pWVT->Keys[ iPos ] = pWVT->keyLast = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
      iPos = 0;
   if( iPos != pWVT->keyPointerOut )
      pWVT->keyPointerIn = iPos;
}

static bool hb_gt_wvt_GetCharFromInputQueue( PHB_GTWVT pWVT, int * iKey )
{
   if( pWVT && pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
      {
         pWVT->keyPointerOut = 0;
      }
      return HB_TRUE;
   }
   *iKey = 0;
   return HB_FALSE;
}

int hb_gt_wvt_getKbdState( void )
{
   int iKbdState = 0;
   Qt::KeyboardModifiers kbState = QApplication::keyboardModifiers();

   if( kbState & Qt::ShiftModifier   ) iKbdState |= HB_GTI_KBD_SHIFT;
   if( kbState & Qt::ControlModifier ) iKbdState |= HB_GTI_KBD_CTRL;
   if( kbState & Qt::AltModifier     ) iKbdState |= HB_GTI_KBD_ALT;
   #if 0  /* No equivalents available in QT */
   if( kbState[VK_LWIN    ] & 0x80 ) iKbdState |= HB_GTI_KBD_LWIN;
   if( kbState[VK_RWIN    ] & 0x80 ) iKbdState |= HB_GTI_KBD_RWIN;
   if( kbState[VK_APPS    ] & 0x80 ) iKbdState |= HB_GTI_KBD_MENU;
   if( kbState[VK_SCROLL  ] & 0x01 ) iKbdState |= HB_GTI_KBD_SCROLOCK;
   if( kbState[VK_NUMLOCK ] & 0x01 ) iKbdState |= HB_GTI_KBD_NUMLOCK;
   if( kbState[VK_CAPITAL ] & 0x01 ) iKbdState |= HB_GTI_KBD_CAPSLOCK;
   if( kbState[VK_INSERT  ] & 0x01 ) iKbdState |= HB_GTI_KBD_INSERT;
   #endif

   return iKbdState;
}
#if 0
void hb_gt_wvt_setKbdState( int iKbdState )
{
   HB_BYTE kbState[256];

   GetKeyboardState( kbState );

   kbState[VK_SHIFT  ] = ( iKbdState & HB_GTI_KBD_SHIFT    ) ? 0x80 : 0;
   kbState[VK_CONTROL] = ( iKbdState & HB_GTI_KBD_CTRL     ) ? 0x80 : 0;
   kbState[VK_MENU   ] = ( iKbdState & HB_GTI_KBD_ALT      ) ? 0x80 : 0;
   kbState[VK_LWIN   ] = ( iKbdState & HB_GTI_KBD_LWIN     ) ? 0x80 : 0;
   kbState[VK_RWIN   ] = ( iKbdState & HB_GTI_KBD_RWIN     ) ? 0x80 : 0;
   kbState[VK_APPS   ] = ( iKbdState & HB_GTI_KBD_MENU     ) ? 0x80 : 0;
   kbState[VK_SCROLL ] = ( iKbdState & HB_GTI_KBD_SCROLOCK ) ? 0x01 : 0;
   kbState[VK_NUMLOCK] = ( iKbdState & HB_GTI_KBD_NUMLOCK  ) ? 0x01 : 0;
   kbState[VK_CAPITAL] = ( iKbdState & HB_GTI_KBD_CAPSLOCK ) ? 0x01 : 0;
   kbState[VK_INSERT ] = ( iKbdState & HB_GTI_KBD_INSERT   ) ? 0x01 : 0;

   SetKeyboardState( kbState );
}
#endif

#if 0
static int hb_gt_wvt_key_ansi_to_oem( int c )
{
   HB_BYTE pszAnsi[ 2 ];
   HB_BYTE pszOem[ 2 ];

   pszAnsi[ 0 ] = ( CHAR ) c;
   pszAnsi[ 1 ] = 0;
//   CharToOemBuffA( ( LPCSTR ) pszAnsi, ( LPSTR ) pszOem, 1 );
   return * pszOem;
}
#endif

static void hb_gt_wvt_QResetWindowSize( PHB_GTWVT pWVT )
{
   pWVT->qWnd->_drawingArea->resetWindowSize();
   pWVT->qWnd->setWindowSize();
}

static bool hb_gt_wvt_QSetWindowSize( PHB_GTWVT pWVT, int iRows, int iCols )
{
   if( HB_GTSELF_RESIZE( pWVT->pGT, iRows, iCols ) )
   {
      pWVT->ROWS = ( HB_USHORT ) iRows;
      pWVT->COLS = ( HB_USHORT ) iCols;

      pWVT->qWnd->_drawingArea->_iROWS = iRows;
      pWVT->qWnd->_drawingArea->_iCOLS = iCols;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static bool hb_gt_wvt_QInitWindow( PHB_GTWVT pWVT, int iRow, int iCol )
{
   bool fRet = hb_gt_wvt_QSetWindowSize( pWVT, iRow, iCol );
   hb_gt_wvt_QResetWindowSize( pWVT );
   return fRet;
}

static QPoint hb_gt_wvt_QGetColRowFromXY( PHB_GTWVT pWVT, int x, int y )
{
   QPoint colrow;
   colrow.setX( x / pWVT->PTEXTSIZE.x() );
   colrow.setY( y / pWVT->PTEXTSIZE.y() );
   return colrow;
}

static QRect hb_gt_wvt_QGetColRowFromXYRect( PHB_GTWVT pWVT, QRect xy )
{
   QRect colrow;
   colrow.setLeft( xy.left() / pWVT->PTEXTSIZE.x() );
   colrow.setTop( xy.top() / pWVT->PTEXTSIZE.y() );
   colrow.setRight( xy.right() / pWVT->PTEXTSIZE.x() -
                   ( xy.right() % pWVT->PTEXTSIZE.x() ? 0 : 1 ) ); /* Adjust for when rectangle */
   colrow.setBottom( xy.bottom() / pWVT->PTEXTSIZE.y() -
                   ( xy.bottom() % pWVT->PTEXTSIZE.y() ? 0 : 1 ) ); /* EXACTLY overlaps characters */
   return colrow;
}

#if 0
static bool hb_gt_wvt_QValidWindowSize( int rows, int cols, QFont *qFont, int iWidth )
{
   //QDesktopWidget *desk = new QDesktopWidget();
   //int maxWidth = desk->width();
   //int maxHeight = desk->height();

   HB_SYMBOL_UNUSED( rows   );
   HB_SYMBOL_UNUSED( cols   );
   HB_SYMBOL_UNUSED( qFont  );
   HB_SYMBOL_UNUSED( iWidth );

   return HB_TRUE;
}
#endif

static void hb_gt_wvt_QCenterWindow( PHB_GTWVT pWVT )
{
   int iDTWidth  = QDesktopWidget().screenGeometry( QDesktopWidget().primaryScreen() ).right();
   int iDTHeight = QDesktopWidget().screenGeometry( QDesktopWidget().primaryScreen() ).bottom();
   int iWidth    = pWVT->qWnd->width();
   int iHeight   = pWVT->qWnd->height();
   pWVT->qWnd->move( ( iDTWidth - iWidth ) / 2, ( iDTHeight - iHeight ) / 2 );
}

static bool hb_gt_wvt_CreateConsoleWindow( PHB_GTWVT pWVT )
{
   pWVT->qWnd = new MainWindow();
   if( !pWVT->qWnd )
      hb_errInternal( 10001, "Failed to create QTC window", NULL, NULL );

   pWVT->qWnd->pGT               = pWVT->pGT;
   pWVT->qWnd->_drawingArea->pGT  = pWVT->pGT;

   hb_gt_wvt_QInitWindow( pWVT, pWVT->ROWS, pWVT->COLS );

   /* Set icon */
   #if 0
   if( pWVT->hIcon )
   {
      pWVT->qWnd->setWindowIcon( pWVT->hIcon );
   }
   #endif

   /* Set default window title */
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGV()[ 0 ] );
      pWVT->qWnd->setWindowTitle( "Harbour-QT Console" );
      pWVT->qWnd->_drawingArea->resetWindowSize();
      pWVT->qWnd->setWindowSize();
      hb_xfree( pFileName );
   }

   return HB_TRUE;
}

/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void hb_gt_wvt_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   int          iCmdShow = 0;
   PHB_GTWVT    pWVT = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   pWVT = hb_gt_wvt_New( pGT, iCmdShow );
   if( !pWVT )
   {
      hb_errInternal( 10001, "Maximum number of QTC windows reached, cannot create another one", NULL, NULL );
   }
   HB_GTLOCAL( pGT ) = ( void * ) pWVT;
   if( ! pWVT->qEventLoop )  /* To be activated for stand alone console */
   {
      pWVT->qEventLoop = new QEventLoop();
   }
   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
   HB_GTSELF_SEMICOLD( pGT );
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Exit(%p)", pGT));

   pWVT = HB_GTWVT_GET( pGT );
   HB_GTSUPER_EXIT( pGT );

   if( pWVT )
   {
      hb_gt_wvt_Free( pWVT );
   }
}

/* ********************************************************************** */

static void hb_gt_wvt_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT && pWVT->qWnd )
   {
      QRect rect;
      /* Fill in values */
      rect.setTop( iRow );
      rect.setBottom( iRow );
      rect.setLeft( iCol );
      rect.setRight( iCol + iSize - 1 );
      /* convert in pixel coordinates */
      rect = hb_gt_wvt_QGetXYFromColRowRect( pWVT, rect );
      /* Schedule a Repaint Event */
      pWVT->qWnd->_drawingArea->redrawBuffer( rect );
      pWVT->qWnd->_drawingArea->update( rect );
   }
}

/* ********************************************************************** */

static void hb_gt_wvt_Refresh( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_Refresh(%p)", pGT) );

   HB_GTSUPER_REFRESH( pGT );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( !pWVT->fInit )
      {
         pWVT->fInit = HB_TRUE;

         #if 1
         hb_gt_wvt_CreateConsoleWindow( pWVT );
         #endif

         if( pWVT->CenterWindow )
         {
            hb_gt_wvt_QCenterWindow( pWVT );
         }
         pWVT->qWnd->setFocus();
//         pWVT->qWnd->_drawingArea->setFocus();
         pWVT->qWnd->show();
         pWVT->qWnd->update();
         pWVT->qWnd->_drawingArea->setFocus();
      }
      hb_gt_wvt_QUpdateCaret( pWVT );
   }
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   PHB_GTWVT pWVT;
   bool fResult = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_SetMode(%p,%d,%d)", pGT, iRow, iCol ) );

   pWVT = HB_GTWVT_GET( pGT );

   if( iRow <= WVT_MAX_ROWS && iCol <= WVT_MAX_COLS )
   {
      if( pWVT->qWnd ) /* Is the window already open */
      {
         /*
          * make sure that the mode selected along with the current
          * font settings will fit in the window
          */
         fResult = hb_gt_wvt_QInitWindow( pWVT, iRow, iCol );
         if( pWVT->CenterWindow )
         {
            hb_gt_wvt_QCenterWindow( pWVT );
         }
         HB_GTSELF_REFRESH( pGT );
      }
   }
   return fResult;
}

/* ********************************************************************** */

static const char * hb_gt_wvt_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Multi-Platform QT based GUI console (QTC)";
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTWVT pWVT;
   int  c = 0;
   bool fKey = HB_FALSE;

   //HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );
   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( pWVT->qEventLoop ) /* Is the window already open */
      {
         pWVT->qEventLoop->processEvents( QEventLoop::AllEvents );
         hb_releaseCPU();
      }
      else
      {
         QApplication::processEvents( QEventLoop::AllEvents );
         hb_releaseCPU();
      }
      fKey = hb_gt_wvt_GetCharFromInputQueue( pWVT, &c );
   }
   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );

   /* Not exactly what this function is supposed to do, but ... */
   QApplication::beep();
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_SetDispCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox )
{
   HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox );

#  if defined( UNICODE )
   /*
    * We are displaying text in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP || !*pszHostCDP )
      pszHostCDP = hb_cdpID();

   if( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
      {
         PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

         pWVT->hostCDP = cdpHost;
         pWVT->boxCDP = fBox ? cdpHost : hb_cdpFind( "EN" );
      }
   }
#  else
   {
      PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
      PHB_CODEPAGE cdpTerm, cdpHost;
      int i;

      if( !pszHostCDP )
         pszHostCDP = hb_cdpID();

      if( !pszTermCDP )
         pszTermCDP = pszHostCDP;

      cdpTerm = hb_cdpFind( pszTermCDP );
      cdpHost = hb_cdpFind( pszHostCDP );

      for( i = 0; i < 256; i++ )
      {
         pWVT->chrTransTbl[ i ] = ( HB_BYTE )
                           hb_cdpTranslateDispChar( i, cdpHost, cdpTerm );
      }
   }
#  endif

   return HB_TRUE;
}

static HB_BOOL hb_gt_wvt_SetKeyCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP );

   /*
    * We are receiving WM_CHAR events in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP || !*pszHostCDP )
      pszHostCDP = hb_cdpID();

   if( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
         HB_GTWVT_GET( pGT )->inCDP = cdpHost;
   }

   return HB_TRUE;
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_IsPresent(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

static void hb_gt_wvt_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTWVT pWVT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol));

   pWVT = HB_GTWVT_GET( pGT );
   *piRow = pWVT->MousePos.y();
   *piCol = pWVT->MousePos.x();
}

static HB_BOOL hb_gt_wvt_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_SYMBOL_UNUSED( iButton );
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_mouse_ButtonState(%p,%i)", pGT, iButton) );

   HB_SYMBOL_UNUSED( pGT );

   switch( iButton )
   {
   case 0:
      return ( QApplication::mouseButtons() & Qt::LeftButton );
   case 1:
      return ( QApplication::mouseButtons() & Qt::RightButton );
   case 2:
      return ( QApplication::mouseButtons() & Qt::MidButton );
   }
   return HB_FALSE;
}

static int hb_gt_wvt_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_mouse_CountButton(%p)", pGT) );

   HB_SYMBOL_UNUSED( pGT );

   //return GetSystemMetrics( SM_CMOUSEBUTTONS );
   return 2;
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTWVT pWVT;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pWVT = HB_GTWVT_GET( pGT );

   switch( iType )
   {
      case HB_GTI_FULLSCREEN:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;
#if 0
      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;
#endif
      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y() );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            pWVT->fontHeight = iVal;
            if( pWVT->qWnd )
            {
               hb_gt_wvt_QResetWindowSize( pWVT );
               HB_GTSELF_REFRESH( pGT );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x() );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            /* store font status for next operation on fontsize */
            pWVT->fontWidth = iVal;
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, pWVT->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
            hb_strncpy( pWVT->fontFace, hb_itemGetCPtr( pInfo->pNewVal ), sizeof( pWVT->fontFace ) - 1 );
         break;

      case HB_GTI_FONTWEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWeight );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontWeight = hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_FONTQUALITY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontQuality );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontQuality = hb_itemGetNI( pInfo->pNewVal ) ;
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y() * pWVT->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, ( HB_USHORT ) ( iVal / pWVT->PTEXTSIZE.y() ), pWVT->COLS );
         }
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x() * pWVT->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, pWVT->ROWS, ( HB_USHORT ) ( iVal / pWVT->PTEXTSIZE.x() ) );
         }
         break;

      case HB_GTI_DESKTOPWIDTH:
      {
         QDesktopWidget *qDesk = new QDesktopWidget();
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, qDesk->width() );
         break;
      }
      case HB_GTI_DESKTOPHEIGHT:
      {
         QDesktopWidget *qDesk = new QDesktopWidget();
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, qDesk->height() );
         break;
      }
      case HB_GTI_DESKTOPCOLS:
      {
         QDesktopWidget *qDesk = new QDesktopWidget();
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, qDesk->width() / pWVT->PTEXTSIZE.x() );
         break;
      }
      case HB_GTI_DESKTOPROWS:
      {
         QDesktopWidget *qDesk = new QDesktopWidget();
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, qDesk->height() / pWVT->PTEXTSIZE.y() );
         break;
      }
      case HB_GTI_WINTITLE:
         if( pWVT->qWnd )
         {
            pInfo->pResult = hb_itemPutStrUTF8( pInfo->pResult, pWVT->qWnd->windowTitle().toUtf8().data() );
            if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            {
               void * pText01 = NULL;
               pWVT->qWnd->setWindowTitle( hb_itemGetStrUTF8( pInfo->pNewVal, &pText01, NULL ) );
               hb_strfree( pText01 );
            }
         }
         break;

      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->CodePage );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->CodePage )
            {
               pWVT->CodePage = iVal;
            }
         }
         break;

      case HB_GTI_BOXCP:
         pInfo->pResult = hb_itemPutC( pInfo->pResult,
                                       pWVT->boxCDP ? pWVT->boxCDP->id : NULL );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            PHB_CODEPAGE cdpBox = hb_cdpFind( hb_itemGetCPtr( pInfo->pNewVal ) );
            if( cdpBox )
               pWVT->boxCDP = cdpBox;
         }
         break;

      case HB_GTI_ICONFILE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            if( pWVT->qWnd )
               pWVT->qWnd->setWindowIcon( QIcon( QString( hb_itemGetCPtr( pInfo->pNewVal ) ) ) );
         }
         break;

      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->COLS );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->ROWS );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_wvt_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            // hb_gt_wvt_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, QApplication::cursorFlashTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            QApplication::setCursorFlashTime( hb_itemGetNI( pInfo->pNewVal ) );
         }
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            QClipboard *cb = QApplication::clipboard();
            cb->setText( QString( hb_itemGetCPtr( pInfo->pNewVal ) ) );
         }
         else
         {
            QClipboard *cb = QApplication::clipboard();
            pInfo->pResult = hb_itemPutC( pInfo->pResult, cb->text().toLatin1().data() );
         }

         break;

      case HB_GTI_SCREENSIZE:
         int iX, iY;

         if( !pInfo->pResult )
         {
            pInfo->pResult = hb_itemNew( NULL );
         }
         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 2, pWVT->PTEXTSIZE.y() * pWVT->ROWS );
         hb_arraySetNI( pInfo->pResult, 1, pWVT->PTEXTSIZE.x() * pWVT->COLS );
         iY = hb_arrayGetNI( pInfo->pNewVal,2 );
         iX = hb_arrayGetNI( pInfo->pNewVal,1 );

         if( iY  > 0 )
         {
            bool bOldCentre = pWVT->CenterWindow;
            pWVT->CenterWindow = pWVT->bMaximized ? HB_TRUE : HB_FALSE;
            HB_GTSELF_SETMODE( pGT, ( HB_USHORT ) ( iY / pWVT->PTEXTSIZE.y() ), ( HB_USHORT ) ( iX / pWVT->PTEXTSIZE.x() ) );
            pWVT->CenterWindow = bOldCentre;
         }
         break;

      case HB_GTI_RESIZABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bResizable );
         if( pInfo->pNewVal )
         {
            bool bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bResizable )
            {
               pWVT->bResizable = bNewValue;
               if( pWVT->qWnd )
               {
                  Qt::WindowFlags flags = pWVT->qWnd->windowFlags();
                  if( pWVT->bResizable )
                  {
                     flags = flags | Qt::WindowMaximizeButtonHint;
                     pWVT->qWnd->setMinimumWidth( 0 );
                     pWVT->qWnd->setMaximumWidth( QDesktopWidget().width() );
                     pWVT->qWnd->setMinimumHeight( 50 );
                     pWVT->qWnd->setMaximumHeight( QDesktopWidget().height() );
                  }
                  else
                  {
                     flags = flags &~ Qt::WindowMaximizeButtonHint;
                     pWVT->qWnd->setMinimumWidth( pWVT->qWnd->width() );
                     pWVT->qWnd->setMaximumWidth( pWVT->qWnd->width() );
                     pWVT->qWnd->setMinimumHeight( pWVT->qWnd->height() );
                     pWVT->qWnd->setMaximumHeight( pWVT->qWnd->height() );
                  }
                  hb_gt_wvt_SetWindowFlags( pWVT, flags );
               }
            }
         }
         break;

      case HB_GTI_SELECTCOPY:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bSelectCopy );

         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            pInfo->pResult = hb_itemPutC( pInfo->pResult, pWVT->pszSelectCopy );
            if( hb_itemGetCLen( pInfo->pNewVal ) )
            {
               #if 0
               HMENU hSysMenu = pWVT->hWnd ? GetSystemMenu( pWVT->hWnd, HB_FALSE ) : NULL;
               if( hSysMenu || !pWVT->hWnd )
               {
                  if( pWVT->pszSelectCopy )
                     hb_xfree( pWVT->pszSelectCopy );
                  pWVT->pszSelectCopy = hb_strdup( hb_itemGetCPtr( pInfo->pNewVal ) );
                  pWVT->bSelectCopy = HB_TRUE;
               }
               #endif
               pWVT->pszSelectCopy = hb_strdup( hb_itemGetCPtr( pInfo->pNewVal ) );
               pWVT->bSelectCopy = HB_TRUE;
            }
         }
         else if( pInfo->pNewVal )
         {
            bool bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bSelectCopy )
            {
               if( pWVT->qWnd )
               {
                  #if 0
                  HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, HB_FALSE );
                  if( hSysMenu )
                  {
                     EnableMenuItem( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND | ( bNewValue ? MF_ENABLED : MF_GRAYED ) );
                     pWVT->bSelectCopy = bNewValue;
                  }
                  #endif
                  pWVT->bSelectCopy = bNewValue;
               }
               else
                  pWVT->bSelectCopy = bNewValue;
            }
         }
         break;

      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bClosable );
         if( pInfo->pNewVal )
         {
            bool bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bClosable )
            {
               pWVT->bClosable = bNewValue;
               if( pWVT->qWnd )
               {
                  Qt::WindowFlags flags = pWVT->qWnd->windowFlags();
                  if( pWVT->bClosable )
                     flags |= Qt::WindowCloseButtonHint;
                  else
                     flags = flags &~ Qt::WindowCloseButtonHint;

                  hb_gt_wvt_SetWindowFlags( pWVT, flags );
               }
            }
         }
         break;

      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            int iIndex = hb_itemGetNI( pInfo->pNewVal );
            if( iIndex >= 0 && iIndex < 16 )
            {
               pInfo->pResult = hb_itemPutNL( pInfo->pResult, pWVT->qWnd->_drawingArea->_COLORS[ iIndex ] );
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  if( pWVT->qWnd )
                  {
                     pWVT->qWnd->_drawingArea->_COLORS[ iIndex ] = hb_itemGetNL( pInfo->pNewVal2 );
                     HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
                  }
               }
            }
         }
         else
         {
            int i;
            if( !pInfo->pResult )
            {
               pInfo->pResult = hb_itemNew( NULL );
            }
            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 0; i < 16; i++ )
            {
               hb_arraySetNL( pInfo->pResult, i + 1, pWVT->qWnd->_drawingArea->_COLORS[ i ] );
            }
            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
            {
               if( hb_arrayLen( pInfo->pNewVal ) == 16 )
               {
                  for( i = 0; i < 16; i++ )
                  {
                     pWVT->qWnd->_drawingArea->_COLORS[ i ] = hb_arrayGetNL( pInfo->pNewVal, i + 1 );
                  }
                  if( pWVT->qWnd )
                     HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
               }
            }
         }
         break;

      case HB_GTI_RESIZEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->ResizeMode );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            pWVT->ResizeMode = hb_itemGetNI( pInfo->pNewVal );
         }
         break;

      case HB_GTI_SETPOS_XY:
      case HB_GTI_SETPOS_ROWCOL:
         if( pWVT->qWnd && ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) &&
                           ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC ) )
         {
            int x, y;

            if( iType == HB_GTI_SETPOS_ROWCOL )
            {
               y = hb_itemGetNI( pInfo->pNewVal ) * pWVT->PTEXTSIZE.y();
               x = hb_itemGetNI( pInfo->pNewVal2 ) * pWVT->PTEXTSIZE.x();
            }
            else
            {
               x = hb_itemGetNI( pInfo->pNewVal );
               y = hb_itemGetNI( pInfo->pNewVal2 );
            }
            pWVT->qWnd->move( x,y );
         }
         break;

      case HB_GTI_WIDGET:
         //pInfo->pResult = hb_itemPutPtrGC( pInfo->pResult, hbqt_gcAllocate_QWidget( pWVT->qWnd, false ) );
         pInfo->pResult = hb_itemNew( hbqt_create_objectGC( hbqt_gcAllocate_QMainWindow( pWVT->qWnd, false ), "HB_QMAINWINDOW" ) );
         break;

      case HB_GTI_DRAWINGAREA:
         //pInfo->pResult = hb_itemPutPtrGC( pInfo->pResult, hbqt_gcAllocate_QMainWindow( pWVT->qWnd->_drawingArea, false ) );
         pInfo->pResult = hb_itemNew( hbqt_create_objectGC( hbqt_gcAllocate_QWidget( pWVT->qWnd->_drawingArea, false ), "HB_QWIDGET" ) );
         break;

      case HB_GTI_EVENTLOOP:

         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* ********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                 = hb_gt_wvt_Init;
   pFuncTable->Exit                 = hb_gt_wvt_Exit;
   pFuncTable->SetMode              = hb_gt_wvt_SetMode;
   pFuncTable->Redraw               = hb_gt_wvt_Redraw;
   pFuncTable->Refresh              = hb_gt_wvt_Refresh;
   pFuncTable->Version              = hb_gt_wvt_Version;
   pFuncTable->Tone                 = hb_gt_wvt_Tone;
   pFuncTable->Info                 = hb_gt_wvt_Info;
   pFuncTable->SetDispCP            = hb_gt_wvt_SetDispCP;
   pFuncTable->SetKeyCP             = hb_gt_wvt_SetKeyCP;
   pFuncTable->ReadKey              = hb_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent       = hb_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos          = hb_gt_wvt_mouse_GetPos;
   pFuncTable->MouseButtonState     = hb_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton     = hb_gt_wvt_mouse_CountButton;

   return HB_TRUE;
}

/* ********************************************************************** */

static const HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                                   hb_gt_FuncInit,
                                   HB_GTSUPER,
                                   HB_GTID_PTR };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

/* ********************************************************************** */

DrawingArea::DrawingArea( QWidget *parent )
    : QWidget( parent )
{
   setAttribute( Qt::WA_StaticContents );
   setAttribute( Qt::WA_PaintOnScreen );

   _COLORS[ 0] = BLACK;
   _COLORS[ 1] = BLUE;
   _COLORS[ 2] = GREEN;
   _COLORS[ 3] = CYAN;
   _COLORS[ 4] = RED;
   _COLORS[ 5] = MAGENTA;
   _COLORS[ 6] = BROWN;
   _COLORS[ 7] = WHITE;
   _COLORS[ 8] = LIGHT_GRAY;
   _COLORS[ 9] = BRIGHT_BLUE;
   _COLORS[10] = BRIGHT_GREEN;
   _COLORS[11] = BRIGHT_CYAN;
   _COLORS[12] = BRIGHT_RED;
   _COLORS[13] = BRIGHT_MAGENTA;
   _COLORS[14] = YELLOW;
   _COLORS[15] = BRIGHT_WHITE;

   _iROWS = 25;
   _iCOLS = 80;

   setFocusPolicy( Qt::StrongFocus );
   setMouseTracking( HB_TRUE );

   setAttribute( Qt::WA_InputMethodEnabled, true );

   /* Important but give it a thought */
   //setAttribute(Qt::WA_OpaquePaintEvent);

   _bCaretOn   = HB_TRUE;
   _bBlinking  = HB_FALSE;
   _basicTimer = new QBasicTimer();

   _bFirst     = HB_TRUE;
   _bSizing    = HB_FALSE;
   _bCopying   = HB_FALSE;

   _image      = new QImage();

   _crtLastRow = 0;
   _crtLastCol = 0;

   _rCopying.setRect( -1, -1, -1, -1 );
}

void DrawingArea::copyTextOnClipboard( void )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   QRect rectRC = hb_gt_wvt_QGetColRowFromXYRect( pWVT, _rCopying );
   QRect rectXY = hb_gt_wvt_QGetXYFromColRowRect( pWVT, rectRC    );

   repaint( rectXY );

   /* Post to Clipboard */
   int left   = rectRC.left();
   int top    = rectRC.top();
   int right  = rectRC.right();
   int bottom = rectRC.bottom();

   HB_ULONG ulSize = ( ( bottom - top + 1 ) * ( right - left + 1 + 2 ) );
   char * sBuffer = ( char * ) hb_xgrab( ulSize + 1 );

   int j, irow, icol;
   for( j = 0, irow = top; irow <= bottom; irow++ )
   {
      for( icol = left; icol <= right; icol++ )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         if( !HB_GTSELF_GETSCRCHAR( pWVT->pGT, irow, icol, &iColor, &bAttr, &usChar ) )
            break;

         sBuffer[ j++ ] = ( char ) usChar;
      }

      sBuffer[ j++ ] = '\r';
      sBuffer[ j++ ] = '\n';
   }
   sBuffer[ j ] = '\0';

   if( j > 0 )
   {
      QClipboard *cb = QApplication::clipboard();
      cb->setText( QString( sBuffer ) );
   }
   hb_xfree( sBuffer );

   _rCopying.setRect( -1, -1, -1, -1 );
   _rCopyingP.setRect( -1, -1, -1, -1 );
}

void DrawingArea::paintCopyOperation( void )
{
   if( _rCopying.left() == -1 )
      return;

   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   QRect rect;

   if( _rCopying.right() < _rCopyingP.right() || _rCopying.bottom() < _rCopyingP.bottom() )
   {
      _bCopying = false;
      rect = hb_gt_wvt_QGetColRowFromXYRect( pWVT, _rCopyingP );
      rect = hb_gt_wvt_QGetXYFromColRowRect( pWVT, rect );
      repaint( rect );
      _bCopying = true;
   }
   rect = hb_gt_wvt_QGetColRowFromXYRect( pWVT, _rCopying );
   rect = hb_gt_wvt_QGetXYFromColRowRect( pWVT, rect );
   repaint( rect );

   _rCopyingP.setRight( _rCopying.right() );
   _rCopyingP.setBottom( _rCopying.bottom() );
}

void DrawingArea::redrawBuffer( const QRect & rect )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   QPainter painter( _image );
   QFont font( _qFont, painter.device() );
   painter.setFont( font );
   painter.setBackgroundMode( Qt::OpaqueMode );

   HB_USHORT usChar;
   HB_BYTE   bAttr, bOldAttr = 0;
   int       iCol, iRow, len, iTop, startCol;
   int       bColor, bOldColor = 0;
   char      text[ WVT_MAX_COLS ];
   QRect     rcRect = hb_gt_wvt_QGetColRowFromXYRect( pWVT, rect );

   for( iRow = rcRect.top(); iRow <= rcRect.bottom(); ++iRow )
   {
      iCol    = startCol = rcRect.left();
      len     = 0;
      iTop    = ( iRow * _fontHeight ) + _fontAscent;
      text[0] = '\0';

      while( iCol <= rcRect.right() )
      {
         if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &bColor, &bAttr, &usChar ) )
            break;

         #if defined( UNICODE )
         usChar = hb_cdpGetU16Disp( bAttr & HB_GT_ATTR_BOX ? pWVT->boxCDP : pWVT->hostCDP, ( HB_BYTE ) usChar );
         #else
         usChar = pWVT->chrTransTbl[ usChar & 0xFF ];
         #endif
#if 1
         if( bAttr & HB_GT_ATTR_BOX )
         {
            drawBoxCharacter( &painter, usChar, bColor, iCol * _fontWidth, iRow * _fontHeight );
         }
         /* Hack to let it know if character is a Line character */
         else if( usChar >= 170 && usChar <= 223 )
         {
            drawBoxCharacter( &painter, usChar, bColor, iCol * _fontWidth, iRow * _fontHeight );
            bAttr = HB_GT_ATTR_BOX;
         }
#endif
         if( len == 0 )
         {
            bOldAttr  = bAttr;
            bOldColor = bColor;
         }
         else if( bColor != bOldColor || bAttr != bOldAttr )
         {
            text[ len ] = '\0';
            if( ! bOldAttr & HB_GT_ATTR_BOX )
            {
               painter.setPen( QPen( _COLORS[ bOldColor & 0x0F ] ) );
               painter.setBackground( QBrush( _COLORS[ bOldColor >> 4 ] ) );
               painter.drawText( QPoint( startCol * _fontWidth, iTop ), QString( text ) );
            }
            bOldColor = bColor;
            bOldAttr  = bAttr;
            startCol  = iCol;
            len       = 0;
         }
         text[ len++ ] = ( char ) usChar;
         iCol++;
      }
      if( len > 0 )
      {
         text[ len ] = '\0';
         if( ! bOldAttr & HB_GT_ATTR_BOX )
         {
            painter.setPen( QPen( _COLORS[ bOldColor & 0x0F ] ) );
            painter.setBackground( QBrush( _COLORS[ bOldColor >> 4 ] ) );
            painter.drawText( QPoint( startCol * _fontWidth, iTop ), QString( text ) );
         }
      }
   }
}

void DrawingArea::paintEvent( QPaintEvent * event )
{
   QPainter painter( this );

   if( _bCopying )
   {
      _image->invertPixels();
      painter.drawImage( event->rect(), *_image, event->rect() );
      _image->invertPixels();
   }
   else
   {
      painter.drawImage( event->rect(), *_image, event->rect() );
   }
}

bool DrawingArea::createCaret( int iWidth, int iHeight )
{
   _crtWidth  = iWidth;
   _crtHeight = iHeight;
//HB_TRACE( HB_TR_ALWAYS, ( "bool DrawingArea::createCaret() %i %i %i %i", _crtLastRow, _crtLastCol, iWidth, iHeight ) );
   _bCaretOn = HB_TRUE;
   _bBlinking = HB_FALSE;
   displayCell( _crtLastRow, _crtLastCol );
   displayBlock( _crtLastRow, _crtLastCol );
   if( ! _basicTimer->isActive() )
   {
      _basicTimer->start( 500, this );
   }
   return( HB_TRUE );
}
void DrawingArea::hideCaret( void )
{
//HB_TRACE( HB_TR_ALWAYS, ( "bool DrawingArea::hideCaret() %i %i", _crtLastRow, _crtLastCol ) );
   _bCaretOn = HB_FALSE;
   displayCell( _crtLastRow, _crtLastCol );
}
void DrawingArea::showCaret( void )
{
//HB_TRACE( HB_TR_ALWAYS, ( "bool DrawingArea::showCaret() %i %i", _crtLastRow, _crtLastCol ) );
   if( ! _basicTimer->isActive() )
   {
      _basicTimer->start( 500, this );
   }
   displayBlock( _crtLastRow, _crtLastCol );
   _bCaretOn = HB_TRUE;
}
void DrawingArea::destroyCaret( void )
{
   _basicTimer->stop();
   _bCaretOn = HB_FALSE;
   displayCell( _crtLastRow, _crtLastCol );
//HB_TRACE( HB_TR_ALWAYS, ( "void DrawingArea::destroyCaret( void )" ) );
}
void DrawingArea::setCaretPos( int iCol, int iRow )
{
   _crtLastCol = iCol;
   _crtLastRow = iRow;
}
void DrawingArea::displayCell( int iRow, int iCol )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   QPainter painter( _image );
   painter.setBackgroundMode( Qt::OpaqueMode );
   QFont font( _qFont, painter.device() );
   painter.setFont( font );

   HB_USHORT usChar;
   HB_BYTE   bAttr;
   int       bColor = 0;

   if( HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &bColor, &bAttr, &usChar ) )
   {
      #if defined( UNICODE )
      usChar = hb_cdpGetU16Disp( bAttr & HB_GT_ATTR_BOX ? pWVT->boxCDP : pWVT->hostCDP, ( HB_BYTE ) usChar );
      #else
      usChar = pWVT->chrTransTbl[ usChar & 0xFF ];
      #endif

      painter.setPen( QPen( _COLORS[ bColor & 0x0F ] ) );
      painter.setBackground( QBrush( _COLORS[ bColor >> 4 ] ) );
      painter.drawText( QPoint( iCol * _fontWidth, ( iRow * _fontHeight ) + _fontAscent ), QString( usChar ) );
   }
   /* We need immediate painting */
   repaint( QRect( iCol * _fontWidth, iRow * _fontHeight, _fontWidth, _fontHeight ) );
}
void DrawingArea::displayBlock( int iRow, int iCol )
{
   QPainter painter( _image );
   #if 0
   painter.fillRect( QRect( iCol*_fontWidth, iRow * _fontHeight + ( _fontHeight - _crtHeight ),
                                     _fontWidth, _crtHeight ), qRgb( 255,255,255 ) );
   #else
   painter.setCompositionMode( QPainter::RasterOp_SourceXorDestination );
   painter.fillRect( QRect( iCol*_fontWidth, iRow*_fontHeight+(_fontHeight-_crtHeight),
                                     _fontWidth, _crtHeight ), QBrush( qRgb( 255,255,255 ) ) );
   #endif
   /* We need immediate painting */
   repaint( QRect( iCol * _fontWidth, iRow * _fontHeight, _fontWidth, _fontHeight ) );
}
void DrawingArea::timerEvent( QTimerEvent *event )
{
   if( event->timerId() == _basicTimer->timerId() )
   {
      if( _bCaretOn )
      {
         if( _bBlinking )
         {
            _bBlinking = HB_FALSE;
            displayCell( _crtLastRow, _crtLastCol );
         }
         else
         {
            _bBlinking = HB_TRUE;
            displayBlock( _crtLastRow, _crtLastCol );
         }
      }
   }
   else
   {
      QWidget::timerEvent( event );
   }
}

void DrawingArea::resetWindowSize( void )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   QPainter painter( this );

   _iROWS = pWVT->ROWS;
   _iCOLS = pWVT->COLS;

   _qFont = QFont();
   _qFont.setFamily( pWVT->fontFace );
   _qFont.setPixelSize( pWVT->fontHeight );
   _qFont.setFixedPitch( HB_TRUE );
   _qFont        = QFont( _qFont, painter.device() );
   QFontMetrics fontMetrics( _qFont );
   _fontHeight   = fontMetrics.height();
   _fontWidth    = fontMetrics.averageCharWidth();
   _fontAscent   = fontMetrics.ascent();
   _wndWidth     = _fontWidth * _iCOLS;
   _wndHeight    = _fontHeight * _iROWS;

   pWVT->PTEXTSIZE.setX( _fontWidth );
   pWVT->PTEXTSIZE.setY( _fontHeight );

   pWVT->fontWidth  = _fontWidth;
   pWVT->fontHeight = _fontHeight;

   resizeImage( QSize( _wndWidth, _wndHeight ) );
   _image->fill( qRgb( 198,198,198 ) );
   setFont( _qFont );
   setFocus( Qt::OtherFocusReason );
   update();
}

void DrawingArea::resizeImage( const QSize &newSize )
{
   if( _image->size() == newSize )
      return;

   QImage *newImage = new QImage( newSize, QImage::Format_RGB32 );
   newImage->fill( qRgb( 255, 255, 255 ) );
   QPainter painter( newImage );
   painter.drawImage( QPoint( 0,0 ), *_image );
   /* Cleanup Memory */
   _image->~QImage();
   /* Assign new image */
   _image = newImage;
}

void DrawingArea::resizeEvent( QResizeEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   _bSizing = HB_TRUE;

   int iW  = width();
   int iH  = height();

   if( _bFirst )
   {
      _bFirst = HB_FALSE;
      QWidget::resizeEvent( event );
   }
   else
   {
      if( _wndWidth != iW || _wndHeight != iH )
      {
         if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_ROWS )
         {
            int iRows  = iH / _fontHeight;
            int iCols  = iW / _fontWidth;
            _wndWidth  = _fontWidth * iCols;
            _wndHeight = _fontHeight * iRows;

            if( hb_gt_wvt_QSetWindowSize( pWVT, iRows, iCols ) )
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_RESIZE );
               hb_gt_wvt_FireEvent( pWVT, HB_GTE_RESIZED );
               resizeImage( QSize( _wndWidth, _wndHeight ) );
               redrawBuffer( _image->rect() );
            }
         }
         else
         {
            int iFH  = iH / ( _iROWS );
            int iStr = _qFont.stretch();
            int fac  = iStr + ( ( iW - _wndWidth ) / _iCOLS );

            QFontMetrics fmm( _qFont );

            QPainter painter( this );
            _qFont = QFont( _qFont, painter.device() );
            _qFont.setPointSize( 0 );
            _qFont.setPixelSize( iFH-3 );  /* 3 in cases this is not the exact value but still... */
            _qFont.setStretch( fac );
            QFontMetrics fm( _qFont );
            _fontHeight  = fm.height();
            _fontWidth   = fm.averageCharWidth();
            _fontAscent  = fm.ascent();
            _wndWidth    = _fontWidth * _iCOLS;
            _wndHeight   = _fontHeight * _iROWS;

            pWVT->PTEXTSIZE.setX( _fontWidth );
            pWVT->PTEXTSIZE.setY( _fontHeight );

            pWVT->fontWidth  = _fontWidth;
            pWVT->fontHeight = _fontHeight;

            resizeImage( QSize( _wndWidth, _wndHeight ) );
            redrawBuffer( _image->rect() );
            hb_gt_wvt_FireEvent( pWVT, HB_GTE_RESIZED );
         }
      }
      else
      {
         QWidget::resizeEvent( event );
      }
   }
}

void DrawingArea::moveEvent( QMoveEvent *event )
{
   QWidget::moveEvent( event );
}

void DrawingArea::focusInEvent( QFocusEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   hb_gt_wvt_QUpdateCaret( pWVT );
   /* We can fire this event but cannot fire OUT event, message loop gets confused */
   //hb_gt_wvt_FireEvent( pWVT, HB_GTE_SETFOCUS );
   QWidget::focusInEvent( event );
}

void DrawingArea::focusOutEvent( QFocusEvent *event )
{
//HB_TRACE( HB_TR_ALWAYS, ( "void DrawingArea::focusOutEvent( QFocusEvent *event )" ) );
//   this->hideCaret();           /* Disableing for the time being */
   HB_SYMBOL_UNUSED( event );
   /* Either of IN or OUT messagess */
   /* hb_gt_wvt_FireEvent( pWVT, HB_GTE_KILLFOCUS ); */
   QWidget::focusOutEvent( event );
}

void DrawingArea::keyReleaseEvent( QKeyEvent *event )
{
   HB_SYMBOL_UNUSED( event );
   QWidget::keyReleaseEvent( event );
}

void hb_gt_wvt_QSetMousePos( PHB_GTWVT pWVT, int x, int y )
{
   QPoint colrow = hb_gt_wvt_QGetColRowFromXY( pWVT, x, y );

   pWVT->MousePos.setY( colrow.y() );
   pWVT->MousePos.setX( colrow.x() );
}

void DrawingArea::wheelEvent( QWheelEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   int c = 0;

   switch( event->orientation() )
   {
   case Qt::Vertical:
      if( event->delta() < 0 )
         c = K_MWBACKWARD;
      else
         c = K_MWFORWARD;
      break;
   case Qt::Horizontal:
   default:
      QWidget::wheelEvent( event );
      return;
   }
   if( c != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, c );
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
   }
}

void DrawingArea::mouseDoubleClickEvent( QMouseEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   int c = 0;

   switch( event->button() )
   {
   case Qt::LeftButton:
      c = K_LDBLCLK;
      break;
   case Qt::RightButton:
      c = K_RDBLCLK;;
      break;
   case Qt::MidButton:
      c = K_MDBLCLK;;
      break;
   case Qt::MouseButtonMask:
   case Qt::XButton1:
   case Qt::XButton2:
   case Qt::NoButton:
      QWidget::mouseDoubleClickEvent( event );
      return;
   }
   if( c != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, c );
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
   }
}

void DrawingArea::mouseMoveEvent( QMouseEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   int c = K_MOUSEMOVE;

   if( event->buttons() & Qt::LeftButton )
   {
      if( !_bCopying )
      {
         _bCopying = HB_TRUE;
         _rCopying.setLeft( event->x() );
         _rCopying.setTop( event->y() );
         _rCopying.setRight( event->x() );
         _rCopying.setBottom( event->y() );

         _rCopyingP.setLeft( event->x() );
         _rCopyingP.setTop( event->y() );
         _rCopyingP.setRight( event->x() );
         _rCopyingP.setBottom( event->y() );
      }
      if( _bCopying )
      {
         _rCopying.setRight( event->x() );
         _rCopying.setBottom( event->y() );
         paintCopyOperation();
      }
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
      return;
   }

#if defined( __HB_GTWVT_GEN_K_MMDOWN_EVENTS )
   if( event->buttons() & Qt::LeftButton )
      c = K_MMLEFTDOWN;
   else if( event->buttons() & Qt::RightButton )
      c = K_MMRIGHTDOWN;
   else if( event->buttons() & Qt::MidButton )
      c = K_MMMIDDLEDOWN;
#endif
   if( c != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, c );
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
   }
}

void DrawingArea::mousePressEvent( QMouseEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   int c = 0;

   if( _bSizing )
   {
      _bSizing = HB_FALSE;
      pWVT->qWnd->setWindowSize();
   }

   switch( event->button() )
   {
   case Qt::LeftButton:
      c = K_LBUTTONDOWN;
      break;
   case Qt::RightButton:
      c = K_RBUTTONDOWN;
      break;
   case Qt::MidButton:
      c = K_MBUTTONDOWN;
      break;
   case Qt::MouseButtonMask:
   case Qt::XButton1:
   case Qt::XButton2:
   case Qt::NoButton:
      QWidget::mousePressEvent( event );
      return;
   }
   if( c != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, c );
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
   }
}

void DrawingArea::mouseReleaseEvent( QMouseEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
   int c = 0;

   if( _bSizing )
   {
      _bSizing = HB_FALSE;
      pWVT->qWnd->setWindowSize();
   }
   if( _bCopying )
   {
      _bCopying = false;
      copyTextOnClipboard();
   }

   switch( event->button() )
   {
   case Qt::LeftButton:
      c = K_LBUTTONUP;
      break;
   case Qt::RightButton:
      c = K_RBUTTONUP;
      break;
   case Qt::MidButton:
      c = K_MBUTTONUP;
      break;
   case Qt::MouseButtonMask:
   case Qt::XButton1:
   case Qt::XButton2:
   case Qt::NoButton:
      QWidget::mouseReleaseEvent( event );
      return;
   }
   if( c != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, c );
      hb_gt_wvt_QSetMousePos( pWVT, event->x(), event->y() );
   }
}

bool DrawingArea::event( QEvent *event )
{
   if( _bSizing && ( event->type() == QEvent::Enter || event->type() == QEvent::Leave ) )
   {
      _bSizing = HB_FALSE;
      PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );
      pWVT->qWnd->setWindowSize();
   }
   return( QWidget::event( event ) );
}

static void hb_gt_wvt_QTranslateKey( PHB_GTWVT pWVT, Qt::KeyboardModifiers kbm, int key, int shiftkey, int altkey, int controlkey )
{
   if( kbm & Qt::AltModifier )
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   else
   {
      if( kbm & Qt::ControlModifier )
         hb_gt_wvt_AddCharToInputQueue( pWVT, controlkey );
      else
      {
         if( kbm & Qt::ShiftModifier )
            hb_gt_wvt_AddCharToInputQueue( pWVT, shiftkey );
         else
            hb_gt_wvt_AddCharToInputQueue( pWVT, key );
      }
   }
}

static void hb_gt_wvt_QTranslateKeyDigit( PHB_GTWVT pWVT, Qt::KeyboardModifiers kbm, int key, int altkey )
{
   if( kbm & Qt::AltModifier )
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   else
      hb_gt_wvt_AddCharToInputQueue( pWVT, key );
}

static void hb_gt_wvt_QTranslateKeyAlpha( PHB_GTWVT pWVT, Qt::KeyboardModifiers kbm, int key, int shiftkey, int altkey, int controlkey, QString text )
{
   HB_SYMBOL_UNUSED( key );
   HB_SYMBOL_UNUSED( shiftkey );
   HB_SYMBOL_UNUSED( controlkey );

   if( kbm & Qt::AltModifier )
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   else
   {
      if( kbm & Qt::ControlModifier )
         hb_gt_wvt_AddCharToInputQueue( pWVT, controlkey );
      else
         hb_gt_wvt_AddCharToInputQueue( pWVT, ( int ) *text.toLatin1().data() );
   }
}

static void hb_gt_wvt_QTranslateKeyKP( PHB_GTWVT pWVT, Qt::KeyboardModifiers kbm,
                                       int key, int shiftkey, int altkey, int controlkey,
                                       int keyKP, int shiftkeyKP, int altkeyKP, int controlkeyKP )
{
   if( kbm & Qt::KeypadModifier )
   {
      key        = keyKP;
      shiftkey   = shiftkeyKP;
      altkey     = altkeyKP;
      controlkey = controlkeyKP;
   }

   if( kbm & Qt::AltModifier )
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   else
   {
      if( kbm & Qt::ControlModifier )
         hb_gt_wvt_AddCharToInputQueue( pWVT, controlkey );
      else
      {
         if( kbm & Qt::ShiftModifier )
            hb_gt_wvt_AddCharToInputQueue( pWVT, shiftkey );
         else
            hb_gt_wvt_AddCharToInputQueue( pWVT, key );
      }
   }
}

void DrawingArea::keyPressEvent( QKeyEvent *event )
{
   int  c = 0;
   Qt::KeyboardModifiers kbm = event->modifiers();
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   switch( event->key() )
   {
   case Qt::Key_Escape          :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_ESC, K_ESC, K_ALT_ESC, K_ESC );
      return;
   case Qt::Key_Tab             :
   case Qt::Key_Backtab         :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
      return;
   case Qt::Key_Backspace       :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
      return;
   case Qt::Key_Return          :
   case Qt::Key_Enter           : /* Typically located on the keypad. */
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_ENTER, K_SH_ENTER, K_ALT_ENTER, K_CTRL_ENTER );
      return;
   case Qt::Key_Insert          :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_INS, K_SH_INS, K_ALT_INS, K_CTRL_INS );
      return;
   case Qt::Key_Delete          :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_DEL, K_SH_DEL, K_ALT_DEL, K_CTRL_DEL );
      return;
   case Qt::Key_Home            :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_HOME, K_SH_HOME, K_ALT_HOME, K_CTRL_HOME );
      return;
   case Qt::Key_End             :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_END, K_SH_END, K_ALT_END, K_CTRL_END );
      return;
   case Qt::Key_Left            :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_LEFT, K_SH_LEFT, K_ALT_LEFT, K_CTRL_LEFT );
      return;
   case Qt::Key_Up              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_UP, K_SH_UP, K_ALT_UP, K_CTRL_UP );
      return;
   case Qt::Key_Right           :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
      return;
   case Qt::Key_Down            :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_DOWN, K_SH_DOWN, K_ALT_DOWN, K_CTRL_DOWN );
      return;
   case Qt::Key_PageUp          :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_PGUP, K_SH_PGUP, K_ALT_PGUP, K_CTRL_PGUP );
      return;
   case Qt::Key_PageDown        :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_PGDN, K_SH_PGDN, K_ALT_PGDN, K_CTRL_PGDN );
      return;
   case Qt::Key_F1              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1 );
      return;
   case Qt::Key_F2              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2 );
      return;
   case Qt::Key_F3              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3 );
      return;
   case Qt::Key_F4              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
      return;
   case Qt::Key_F5              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5 );
      return;
   case Qt::Key_F6              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6 );
      return;
   case Qt::Key_F7              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7 );
      return;
   case Qt::Key_F8              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8 );
      return;
   case Qt::Key_F9              :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9 );
      return;
   case Qt::Key_F10             :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10 );
      return;
   case Qt::Key_F11             :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11 );
      return;
   case Qt::Key_F12             :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12 );
      return;
   case Qt::Key_Space           :
      c = ' '                ;  break;
   case Qt::Key_Exclam          :
      c = '!'                ;  break;
   case Qt::Key_QuoteDbl        :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, '"', '"', K_ALT_QUOTE, '"' );
      return;
   case Qt::Key_NumberSign      :
      c = '#'                ;  break;
   case Qt::Key_Dollar          :
      c = '$'                ;  break;
   case Qt::Key_Percent         :
      c = '%'                ;  break;
   case Qt::Key_Ampersand       :
      c = '&'                ;  break;
   case Qt::Key_Apostrophe      :
      c = Qt::Key_Apostrophe ;  break;
   case Qt::Key_ParenLeft       :
      c = '('                ;  break;
   case Qt::Key_ParenRight      :
      c = ')'                ;  break;
   case Qt::Key_Comma           :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, ',', ',', K_ALT_COMMA, ',' );
      return;
   case Qt::Key_Period          :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, '.', '.', K_ALT_PERIOD, '.' );
      return;
   case Qt::Key_Asterisk        :
      hb_gt_wvt_QTranslateKeyKP( pWVT, kbm, '*', '*', '*', '*', '*', '*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
      return;
   case Qt::Key_Plus            :
      hb_gt_wvt_QTranslateKeyKP( pWVT, kbm, '+', '+', '+', '+', '+', '+', KP_ALT_PLUS, KP_CTRL_PLUS );
      return;
   case Qt::Key_Minus           :
      hb_gt_wvt_QTranslateKeyKP( pWVT, kbm, '-', '-', '-', '-', '-', '-', KP_ALT_MINUS, KP_CTRL_MINUS );
      return;
   case Qt::Key_Slash           :
      hb_gt_wvt_QTranslateKeyKP( pWVT, kbm, '/', '/', K_ALT_SLASH, '/', '/', '/', KP_ALT_SLASH, KP_CTRL_SLASH );
      return;
   case Qt::Key_0               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 0, K_ALT_0 );
      return;
   case Qt::Key_1               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 1, K_ALT_1 );
      return;
   case Qt::Key_2               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 2, K_ALT_2 );
      return;
   case Qt::Key_3               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 3, K_ALT_3 );
      return;
   case Qt::Key_4               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 4, K_ALT_4 );
      return;
   case Qt::Key_5               :
      hb_gt_wvt_QTranslateKeyKP( pWVT, kbm, '5', '5', K_ALT_5, '5', '5', '5', KP_ALT_5, KP_CTRL_5 );
      break;
   case Qt::Key_6               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 6, K_ALT_6 );
      return;
   case Qt::Key_7               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 7, K_ALT_7 );
      return;
   case Qt::Key_8               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 8, K_ALT_8 );
      return;
   case Qt::Key_9               :
      hb_gt_wvt_QTranslateKeyDigit( pWVT, kbm, 9, K_ALT_9 );
      return;
   case Qt::Key_Colon           :
      c = ':'                ;  break;
   case Qt::Key_Semicolon       :
      c = ';'                ;  break;
   case Qt::Key_Less            :
      c = '<'                ;  break;
   case Qt::Key_Equal           :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, '=', '=', K_ALT_EQUALS, '=' );
      return;
   case Qt::Key_Greater         :
      c = '>'                ;  break;
   case Qt::Key_Question        :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, '?', '?', '?', K_CTRL_QUESTION );
      return;
   case Qt::Key_At              :
      c = '@'                ;  break;
   case Qt::Key_A               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'A', 'a', K_ALT_A, K_CTRL_A, event->text() );
      return;
   case Qt::Key_B               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'B', 'b', K_ALT_B, K_CTRL_B, event->text() );
      return;
   case Qt::Key_C               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'C', 'c', K_ALT_C, K_CTRL_C, event->text() );
      return;
   case Qt::Key_D               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'D', 'd', K_ALT_D, K_CTRL_D, event->text() );
      return;
   case Qt::Key_E               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'E', 'e', K_ALT_E, K_CTRL_E, event->text() );
      return;
   case Qt::Key_F               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'F', 'f', K_ALT_F, K_CTRL_F, event->text() );
      return;
   case Qt::Key_G               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'G', 'g', K_ALT_G, K_CTRL_G, event->text() );
      return;
   case Qt::Key_H               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'H', 'h', K_ALT_H, K_CTRL_H, event->text() );
      return;
   case Qt::Key_I               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'I', 'i', K_ALT_I, K_CTRL_I, event->text() );
      return;
   case Qt::Key_J               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'J', 'j', K_ALT_J, K_CTRL_J, event->text() );
      return;
   case Qt::Key_K               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'K', 'k', K_ALT_K, K_CTRL_K, event->text() );
      return;
   case Qt::Key_L               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'L', 'l', K_ALT_L, K_CTRL_L, event->text() );
      return;
   case Qt::Key_M               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'M', 'm', K_ALT_M, K_CTRL_M, event->text() );
      return;
   case Qt::Key_N               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'N', 'n', K_ALT_N, K_CTRL_N, event->text() );
      return;
   case Qt::Key_O               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'O', 'o', K_ALT_O, K_CTRL_O, event->text() );
      return;
   case Qt::Key_P               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'P', 'p', K_ALT_P, K_CTRL_P, event->text() );
      return;
   case Qt::Key_Q               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'Q', 'q', K_ALT_Q, K_CTRL_Q, event->text() );
      return;
   case Qt::Key_R               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'R', 'r', K_ALT_R, K_CTRL_R, event->text() );
      return;
   case Qt::Key_S               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'S', 's', K_ALT_S, K_CTRL_S, event->text() );
      return;
   case Qt::Key_T               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'T', 't', K_ALT_T, K_CTRL_T, event->text() );
      return;
   case Qt::Key_U               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'U', 'u', K_ALT_U, K_CTRL_U, event->text() );
      return;
   case Qt::Key_V               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'V', 'v', K_ALT_V, K_CTRL_V, event->text() );
      return;
   case Qt::Key_W               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'W', 'w', K_ALT_W, K_CTRL_W, event->text() );
      return;
   case Qt::Key_X               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'X', 'x', K_ALT_X, K_CTRL_X, event->text() );
      return;
   case Qt::Key_Y               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'Y', 'y', K_ALT_Y, K_CTRL_Y, event->text() );
      return;
   case Qt::Key_Z               :
      hb_gt_wvt_QTranslateKeyAlpha( pWVT, kbm, 'Z', 'z', K_ALT_Z, K_CTRL_Z, event->text() );
      return;
   case Qt::Key_BracketLeft     :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, '[', '[', K_ALT_OSB, '[' );
      return;
   case Qt::Key_Backslash       :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, Qt::Key_Backslash, Qt::Key_Backslash, K_ALT_BACKSLASH, Qt::Key_Backslash );
      return;
   case Qt::Key_BracketRight    :
      hb_gt_wvt_QTranslateKey( pWVT, kbm, ']', ']', K_ALT_CSB, ']' );
      return;
   case Qt::Key_AsciiCircum     :
      c = '^'                ;  break;
   case Qt::Key_Underscore      :
      c = '_'                ;  break;
   case Qt::Key_QuoteLeft       :
      c = '`'                ;  break;
   case Qt::Key_BraceLeft       :
      c = '{'                ;  break;
   case Qt::Key_Bar             :
      c = '|'                ;  break;
   case Qt::Key_BraceRight      :
      c = '}'                ;  break;
   case Qt::Key_AsciiTilde      :
      c = '~'                ;  break;
#if 0
   case Qt::Key_Shift           :
      c = K_                 ;  break;
   case Qt::Key_Control         : /* On Mac OS X, this corresponds to the Command keys. */
      c = K_                 ;  break;
   case Qt::Key_Meta            : /* On Mac OS X, this corresponds to the Control keys. On Windows keyboards, this key is mapped to the Windows key. */
      c = K_                 ;  break;
   case Qt::Key_Alt             :
      c = K_                 ;  break;
   case Qt::Key_AltGr           : /* On Windows, when the KeyDown event for this key is sent, the Ctrl+Alt modifiers are also set. */
      c = K_                 ;  break;
   case Qt::Key_CapsLock        :
      c = K_                 ;  break;
   case Qt::Key_NumLock         :
      c = K_                 ;  break;
   case Qt::Key_ScrollLock      :
      c = K_                 ;  break;
   case Qt::Key_Super_L         :
      c = K_                 ;  break;
   case Qt::Key_Super_R         :
      c = K_                 ;  break;
   case Qt::Key_Menu            :
      c = K_                 ;  break;
   case Qt::Key_Hyper_L         :
      c = K_                 ;  break;
   case Qt::Key_Hyper_R         :
      c = K_                 ;  break;
   case Qt::Key_Help            :
      c = K_                 ;  break;
   case Qt::Key_Direction_L     :
      c = K_                 ;  break;
   case Qt::Key_Direction_R     :
      c = K_                 ;  break;
   case Qt::Key_Pause           :
      c = K_PAUSE            ;  break;
   case Qt::Key_Print           :
      c = K_                 ;  break;
   case Qt::Key_SysReq          :
      c = K_                 ;  break;
   case Qt::Key_Clear           :
      c = K_                 ;  break;
#endif
   default                      :
      if( ( event->key() >= 0 ) && ( event->key() <= 255 ) )
         c = event->key();
      else
      {
         QWidget::keyPressEvent( event );
         return ;
      }
   }
   hb_gt_wvt_AddCharToInputQueue( pWVT, c );
}

/*----------------------------------------------------------------------*/
/*
 *                           Class MainWindow
 */
/*----------------------------------------------------------------------*/

MainWindow::MainWindow()
{
   Qt::WindowFlags flags = Qt::WindowCloseButtonHint    | Qt::WindowMaximizeButtonHint |
                           Qt::WindowMinimizeButtonHint | Qt::WindowSystemMenuHint     |
                           Qt::CustomizeWindowHint      | Qt::WindowTitleHint          |
                           Qt::Window ;
   setWindowFlags( flags );
   setFocusPolicy( Qt::StrongFocus );

   _drawingArea = new DrawingArea();
   setCentralWidget( _drawingArea );
}

MainWindow::~MainWindow( void )
{
   delete this->_drawingArea;
}

DrawingArea::~DrawingArea( void )
{
   _image->~QImage();
   _basicTimer->~QBasicTimer();
}

void MainWindow::closeEvent( QCloseEvent *event )
{
   PHB_GTWVT pWVT = HB_GTWVT_GET( pGT );

   if( s_wvtCount > 1 && pWVT->iHandle == 0 )
   {
      event->ignore();
   }
   else
   {
      if( hb_gt_wvt_FireEvent( pWVT, HB_GTE_CLOSE ) == 1 )
      {
         event->ignore();
      }
      else
      {
         event->accept();
         hb_gt_wvt_AddCharToInputQueue( pWVT, K_ESC );
      }
   }
}

void MainWindow::setWindowSize( void )
{
   resize( _drawingArea->_wndWidth, _drawingArea->_wndHeight );
}

void DrawingArea::drawBoxCharacter( QPainter *painter, HB_USHORT usChar, HB_BYTE bColor, int x, int y )
{
   /* Common to all drawing operations except characters */
   int iGap  = 2;
   int iMidY = y + _fontHeight / 2;
   int iMidX = x + _fontWidth / 2;
   int iEndY = y + _fontHeight;
   int iEndX = x + _fontWidth;
   int x1,x2,y1,y2;

   /* painter->setPen( QPen( QBrush( _COLORS[ bColor & 0x0F ] ),1 ) ); */
   painter->setPen( QPen( _COLORS[ bColor & 0x0F ] ) );
   painter->setBackground( QBrush( _COLORS[ bColor >> 4 ] ) );
   painter->fillRect( x, y, _fontWidth, _fontHeight, QBrush( _COLORS[ bColor >> 4 ] ) );

   switch( usChar )
   {
   /*  ---------------------------------------------------------------------  */
   /*                                  B_SINGLE                               */
   /*  ---------------------------------------------------------------------  */
   case 196:      /* --            */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      break;
   case 179:      /* |            */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   case 191:      /* top right    */
      painter->drawLine( x, iMidY, iMidX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, iMidY, iMidX, iEndY );                   /* Vert       */
      break;
   case 217:      /* bottom right  */
      painter->drawLine( x, iMidY, iMidX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iMidY );                       /* Vert       */
      break;
   case 218:      /* top left      */
      painter->drawLine( iMidX, iMidY, iEndX, iMidY );                   /* Horz       */
      painter->drawLine( iMidX, iMidY, iMidX, iEndY );                   /* Vert       */
      break;
   case 192:      /* bottom left   */
      painter->drawLine( iMidX, iMidY, iEndX, iMidY );                   /* Horz       */
      painter->drawLine( iMidX, iMidY, iMidX, y     );                   /* Vert       */
      break;
   case 193:      /* bottom mid up */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iMidY );                       /* Vert       */
      break;
   case 194:      /* top mid down  */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, iMidY, iMidX, iEndY );                   /* Vert       */
      break;
   case 195:      /* middle left   */
      painter->drawLine( iMidX, iMidY, iEndX, iMidY );                   /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   case 180:      /* middle right  */
      painter->drawLine( x, iMidY, iMidX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   case 197:      /* middle cross  */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   /*  ---------------------------------------------------------------------  */
   /*                             B_DOUBLE_SINGLE                             */
   /*  ---------------------------------------------------------------------  */
   case 205:      /* --            */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      break;
   #if 0
   case 179:      /* |            */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   #endif
   case 184:      /* top right    */
      painter->drawLine( x, iMidY-iGap, iMidX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, iMidY-iGap, iMidX, iEndY );              /* Vert       */
      break;
   case 190:      /* bottom right  */
      painter->drawLine( x, iMidY-iGap, iMidX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iMidY+iGap );                  /* Vert       */
      break;
   case 213:      /* top left      */
      painter->drawLine( iMidX, iMidY-iGap, iEndX, iMidY-iGap );         /* Horz       */
      painter->drawLine( iMidX, iMidY+iGap, iEndX, iMidY+iGap );         /* Horz       */
      painter->drawLine( iMidX, iMidY-iGap, iMidX, iEndY );              /* Vert       */
      break;
   case 212:      /* bottom left   */
      painter->drawLine( iMidX, iMidY-iGap, iEndX, iMidY-iGap );         /* Horz       */
      painter->drawLine( iMidX, iMidY+iGap, iEndX, iMidY+iGap );         /* Horz       */
      painter->drawLine( iMidX, iMidY+iGap, iMidX, y     );              /* Vert       */
      break;
   case 207:      /* bottom mid up */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iMidY-iGap );                  /* Vert       */
      break;
   case 209:      /* top mid down  */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, iMidY+iGap, iMidX, iEndY );              /* Vert       */
      break;
   case 198:      /* middle left   */
      painter->drawLine( iMidX, iMidY-iGap, iEndX, iMidY-iGap );         /* Horz       */
      painter->drawLine( iMidX, iMidY+iGap, iEndX, iMidY+iGap );         /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   case 181:      /* middle right  */
      painter->drawLine( x, iMidY-iGap, iMidX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iEndY );                       /* Vert       */
      break;
   case 216:      /* middle cross  */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      painter->drawLine( iMidX, y, iMidX, iMidY-iGap );                  /* Vert       */
      painter->drawLine( iMidX, iMidY+iGap, iMidX, iEndY );              /* Vert       */
      break;
   /*  ---------------------------------------------------------------------  */
   /*                                 B_DOUBLE                                */
   /* ----------------------------------------------------------------------- */
   #if 0
   case 205:      /* --            */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      break;
   #endif
   case 186:      /* |             */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      break;
   case 187:      /* top right     */
      painter->drawLine( x, iMidY-iGap, iMidX+iGap, iMidY-iGap );        /* Horz U     */
      painter->drawLine( x, iMidY+iGap, iMidX-iGap, iMidY+iGap );        /* Horz L     */
      painter->drawLine( iMidX+iGap, iMidY-iGap, iMidX+iGap, iEndY );    /* Vert I     */
      painter->drawLine( iMidX-iGap, iMidY+iGap, iMidX-iGap, iEndY );    /* Vert O     */
      break;
   case 188:      /* bottom right  */
      x1 = iMidX-iGap, x2 = iMidX+iGap, y1 = iMidY-iGap, y2 = iMidY+iGap;
      painter->drawLine( x, y1, x1, y1 );
      painter->drawLine( x, y2, x2, y2 );
      painter->drawLine( x1, y, x1, y1 );
      painter->drawLine( x2, y, x2, y2 );
      break;
   case 201:      /* top left      */
      x1 = iMidX-iGap, x2 = iMidX+iGap, y1 = iMidY-iGap, y2 = iMidY+iGap;
      painter->drawLine( x1, y1, iEndX, y1 );
      painter->drawLine( x2, y2, iEndY, y2 );
      painter->drawLine( x1, y1, x1, iEndY );
      painter->drawLine( x2, y2, x2, iEndY );
      break;
   case 200:      /* bottom left   */
      x1 = iMidX-iGap, x2 = iMidX+iGap, y1 = iMidY-iGap, y2 = iMidY+iGap;
      painter->drawLine( x1, y2, iEndX, y2 );
      painter->drawLine( x2, y1, iEndX, y1 );
      painter->drawLine( x1, y, x1, y2     );
      painter->drawLine( x2, y, x2, y1     );
      break;
   case 202:      /* bottom mid up */
      painter->drawLine( x, iMidY+iGap, iEndX, iMidY+iGap );             /* Horz       */
      painter->drawLine( x, iMidY-iGap, iMidX-iGap, iMidY-iGap );        /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY-iGap, iEndX, iMidY-iGap );    /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iMidY-iGap );        /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iMidY-iGap );        /* Vert       */
      break;
   case 203:      /* top mid down  */
      painter->drawLine( x, iMidY-iGap, iEndX, iMidY-iGap );             /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX-iGap, iMidY+iGap );        /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iEndX, iMidY+iGap );    /* Horz       */
      painter->drawLine( iMidX-iGap, iMidY+iGap, iMidX-iGap, iEndY );    /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iMidX+iGap, iEndY );    /* Vert       */
      break;
   case 204:      /* middle left   */
      painter->drawLine( iMidX+iGap, iMidY-iGap, iEndX, iMidY-iGap );    /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iEndX, iMidY+iGap );    /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iMidY-iGap );        /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iMidX+iGap, iEndY );    /* Vert       */
      break;
   case 185:      /* middle right  */
      painter->drawLine( x, iMidY-iGap, iMidX-iGap, iMidY-iGap );        /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX-iGap, iMidY+iGap );        /* Horz       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iMidY-iGap );        /* Vert       */
      painter->drawLine( iMidX-iGap, iMidY+iGap, iMidX-iGap, iEndY );    /* Vert       */
      break;
   case 206:      /* middle cross  */
      painter->drawLine( x, iMidY-iGap, iMidX-iGap, iMidY-iGap );        /* Horz       */
      painter->drawLine( x, iMidY+iGap, iMidX-iGap, iMidY+iGap );        /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY-iGap, iEndX, iMidY-iGap );    /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iEndX, iMidY+iGap );    /* Horz       */

      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iMidY-iGap );        /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iMidY-iGap );        /* Vert       */
      painter->drawLine( iMidX-iGap, iMidY+iGap, iMidX-iGap, iEndY );    /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY+iGap, iMidX+iGap, iEndY );    /* Vert       */
      break;
   /*  ---------------------------------------------------------------------  */
   /*                            B_SINGLE_DOUBLE                              */
   /*  ---------------------------------------------------------------------  */
   #if 0
   case 196:      /* --            */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      break;
   #endif
   #if 0
   case 186:      /* |            */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      break;
   #endif
   case 183:      /* top right    */
      painter->drawLine( x, iMidY, iMidX+iGap, iMidY );                  /* Horz       */
      painter->drawLine( iMidX-iGap, iMidY, iMidX-iGap, iEndY );         /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY, iMidX+iGap, iEndY );         /* Vert       */
      break;
   case 189:      /* bottom right  */
      painter->drawLine( x, iMidY, iMidX+iGap, iMidY );                  /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iMidY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iMidY );             /* Vert       */
      break;
   case 214:      /* top left      */
      painter->drawLine( iMidX-iGap, iMidY, iEndX, iMidY );              /* Horz       */
      painter->drawLine( iMidX-iGap, iMidY, iMidX-iGap, iEndY );         /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY, iMidX+iGap, iEndY );         /* Vert       */
      break;
   case 211:      /* bottom left   */
      painter->drawLine( iMidX-iGap, iMidY, iEndX, iMidY );              /* Horz       */
      painter->drawLine( iMidX-iGap, iMidY, iMidX-iGap, y     );         /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY, iMidX+iGap, y     );         /* Vert       */
      break;
   case 208:      /* bottom mid up */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iMidY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iMidY );             /* Vert       */
      break;
   case 210:      /* top mid down  */
      painter->drawLine( x, iMidY, iEndX, iMidY );                       /* Horz       */
      painter->drawLine( iMidX-iGap, iMidY, iMidX-iGap, iEndY );         /* Vert       */
      painter->drawLine( iMidX+iGap, iMidY, iMidX+iGap, iEndY );         /* Vert       */
      break;
   case 199:      /* middle left   */
      painter->drawLine( iMidX+iGap, iMidY, iEndX, iMidY );              /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      break;
   case 182:      /* middle right  */
      painter->drawLine( x, iMidY, iMidX-iGap, iMidY );                  /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      break;
   case 215:      /* middle cross  */
      painter->drawLine( x, iMidY, iMidX-iGap, iMidY );                  /* Horz       */
      painter->drawLine( iMidX+iGap, iMidY, iEndX, iMidY  );             /* Horz       */
      painter->drawLine( iMidX-iGap, y, iMidX-iGap, iEndY );             /* Vert       */
      painter->drawLine( iMidX+iGap, y, iMidX+iGap, iEndY );             /* Vert       */
      break;
   /*  ---------------------------------------------------------------------  */
   /*                           B_THIN     B_FAT                              */
   /*  ---------------------------------------------------------------------  */
   case 219:      /* Full Column  */
      painter->fillRect( x, y, _fontWidth, _fontHeight, _COLORS[ bColor & 0x0F ] );
      break;
   case 223:      /* Upper Half Column  */
      painter->fillRect( x, y, _fontWidth, _fontHeight, _COLORS[ bColor >> 4 ] );
      painter->fillRect( x, y, _fontWidth, _fontWidth, _COLORS[ bColor & 0x0F ] );
      break;
   case 220:      /* Lower Half Half Column  */
      painter->fillRect( x, y, _fontWidth, _fontHeight, _COLORS[ bColor >> 4 ] );
      painter->fillRect( x, y+_fontHeight-_fontWidth, _fontWidth, _fontWidth, _COLORS[ bColor & 0x0F ] );
      break;
   default:
      painter->drawText( QPoint( x,y+_fontAscent ), QString( usChar ) );
      break;
   }
}
/*----------------------------------------------------------------------*/
#if 0
static bool hb_gt_wvt_KeyEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
            default:
            {
               else if( iScanCode == 70 ) /* Ctrl_Break key OR Scroll Lock Key */
               {
                  if( bCtrl )  /* Not scroll lock */
                  {
                     hb_gt_wvt_AddCharToInputQueue( pWVT, HB_BREAK_FLAG ); /* Pretend Alt+C pressed */
                  }
               }
            }
   }
}
#endif
/*----------------------------------------------------------------------*/
#if 0
static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   if( pWVT ) switch( message )
   {
      case WM_QUERYENDSESSION: /* Closing down computer */
         hb_vmRequestQuit();
         return 0;

      case WM_ENTERIDLE:
         hb_idleState();
         return 0;

      case WM_SYSCOMMAND:
         switch( wParam )
         {
            case SC_MAXIMIZE:
            {
               /* Disable "maximize" button */
               hb_gt_wvt_FireEvent( pWVT, HB_GTE_RESIZED );
               if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_ROWS )
                  hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_RESIZE );
            }

            case SYS_EV_MARK:
            {
               pWVT->bBeginMarked = HB_TRUE;
            }
         }
   }
}
#endif
/*----------------------------------------------------------------------*/
