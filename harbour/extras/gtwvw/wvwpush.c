/*
 * $Id$
 */

/*
 * Video subsystem for Win32 using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw pushbutton/ combobox functions
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004/01/26 08:14:07 vouchcac Exp =
 *
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://harbour-project.org
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

#include "hbgtwvw.h"

/*WVW_PBcreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset,;
 *              nStretchBitmap, lMap3Dcolors)
 *create pushbutton for window nWinNum
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nBottom: row of bottom/right corner (in character unit) defaults==nTop
 *nRight: col of bottom/right corner (in character unit) defaults==??
 *cText: caption, default == ""
 *
 *
 *cImage: bitmap file name, can be supplied as nImage: bitmap resource id
 *
 *nStretchBitmap: a number between 0 and 1 (inclusive) as a factor to
 *                stretch the bitmap.
 *                1.0: bitmap covers the whole button
 *                0.5: bitmap covers 50% of button
 *                0: bitmap is not stretch
 *               (default is 1)
 *
 *lMap3Dcolors: defaults to .f.
 *           if .t. the following color mapping will be performed:
 *              RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *              RGB(128,128,128) --> COLOR_3DSHADOW
 *              RGB(223,223,223) --> COLOR_3DLIGHT
 *           This might be desirable to have transparent effect.
 *           LIMITATION: this will work on 256 colored bitmaps only
 *
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of pushbutton.
 *         defaults for pushbutton: {-2,-2,+2,+2}
 *
 *bBlock:  codeblock to execute on every BN_CLICK event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nPBid  : pushbutton id
 *
 *returns control id of newly created pushbutton of windows nWinNum
 *returns 0 if failed
 *
 *example:
 */

HB_FUNC( WVW_PBCREATE)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = ( USHORT ) hb_parni( 2 ),
            usLeft   = ( USHORT ) hb_parni( 3 ),
            usBottom = ( USHORT ) hb_parni( 4 ),
            usRight  = ( USHORT ) hb_parni( 5 );
   LPCTSTR  lpszCaption = HB_ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = HB_ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = HB_ISNUM(7) ? (UINT) hb_parni(7) : 0;
   double   dStretch = !HB_ISNIL(10) ? hb_parnd(10) : 1;
   BOOL     bMap3Dcolors = HB_ISLOG(11) ? (BOOL) hb_parl(11) : FALSE;

   if (!HB_ISBLOCK(8))
   {
     hb_retnl(0);
     return;
   }

   iOffTop    = !HB_ISNIL( 9 ) ? hb_parvni( 9,1 ) : -2 ;
   iOffLeft   = !HB_ISNIL( 9 ) ? hb_parvni( 9,2 ) : -2 ;
   iOffBottom = !HB_ISNIL( 9 ) ? hb_parvni( 9,3 ) : +2 ;
   iOffRight  = !HB_ISNIL( 9 ) ? hb_parvni( 9,4 ) : +2;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_PUSHBUTTON );
   hb_retnl( (LONG) uiPBid );
}

/*WVW_PBdestroy( [nWinNum], nPBid )
 *destroy button nPBid for window nWinNum
 */
HB_FUNC( WVW_PBDESTROY)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   UINT uiPBid = (UINT) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = NULL;

   while (pcd)
   {
     if (pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON && pcd->uiCtrlid == uiPBid)
     {
       break;
     }
     pcdPrev = pcd;
     pcd = pcd->pNext;
   }

   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_PBsetFocus( [nWinNum], nButtonId )
 *set the focus to button nButtonId in window nWinNum
 */
HB_FUNC( WVW_PBSETFOCUS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (hWndPB)
  {

    hb_retl( SetFocus(hWndPB) != NULL );
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_PBisFocused( [nWinNum], nPBid )
 *returns .t. if the focus is on button nPBid in window nWinNum
 */
HB_FUNC( WVW_PBISFOCUSED )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  hb_retl((HWND) GetFocus() == hWndPB);
}

/*WVW_PBenable( [nWinNum], nButtonId, [lToggle] )
 *enable/disable button nButtonId on window nWinNum
 *(lToggle defaults to .t., ie. enabling the button)
 *return previous state of the button (TRUE:enabled FALSE:disabled)
 *(if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_PBENABLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
  WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (hWndPB)
  {
    hb_retl( EnableWindow(hWndPB, bEnable)==0 );

    if (!bEnable)
    {
       SetFocus( pWindowData->hWnd );
    }
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_PBsetcodeblock( [nWinNum], nPBid, bBlock )
 *assign (new) codeblock bBlock to button nPBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_PBSETCODEBLOCK )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WVW_DATA * pData=  hb_getWvwData( ) ;
   UINT uiPBid = (UINT) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, uiPBid);
   PHB_ITEM phiCodeBlock = hb_param( 3, HB_IT_BLOCK );
   BOOL bOldSetting = pData->s_bRecurseCBlock;

   if (!phiCodeBlock || pcd==NULL || pcd->bBusy)
   {

     /*
     if (!HB_ISBLOCK(3))
     {
        MessageBox( NULL, TEXT( "Codeblock Expected" ),
                    pData->szAppName, MB_ICONERROR );
     }

     if (pcd==NULL)
     {
        MessageBox( NULL, TEXT( "Control Data not Found" ),
                    pData->szAppName, MB_ICONERROR );
     }

     if (pcd->bBusy)
     {
        MessageBox( NULL, TEXT( "Codeblock is busy" ),
                    pData->szAppName, MB_ICONERROR );
     }
     */

     hb_retl( FALSE );
     return;
   }

   pData->s_bRecurseCBlock = FALSE;
   pcd->bBusy = TRUE;

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

   pcd->bBusy = FALSE;
   pData->s_bRecurseCBlock = bOldSetting;

   hb_retl( TRUE );
}

/*WVW_PBsetstyle( [nWinNum], nPBid, nStyle )
 *assign new style nStyle to button nPBid for window nWinNum
 *typical usage: nStyle==BS_DEFPUSHBUTTON (==01) to turn the button
 *                                               into default push button
 *                                               (thick border)
 *                       BS_PUSHBUTTON    (==00) to turn the button
 *                                               into regular push button
 *
 *using other styles like BS_MULTILINE may also be usefull,
 *but I haven't tried that
 *
 * this function always return .t.
 */
HB_FUNC( WVW_PBSETSTYLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT  uiPBid = (UINT) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   ULONG ulStyle = (ULONG) ( HB_ISNIL( 3 ) ? 0  : hb_parni( 3 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, uiPBid);

   if (pcd->hWndCtrl)
   {
      SendMessage(pcd->hWndCtrl, BM_SETSTYLE, (WPARAM) ulStyle, (LPARAM) TRUE);
   }

   hb_retl( TRUE );
}

/*WVW_PBSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 *this will initialize font for ALL pushbuttons in window nWinNum
 *(including ones created later on)
 */
HB_FUNC( WVW_PBSETFONT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WVW_DATA * pData=  hb_getWvwData( ) ;
   BOOL  retval = TRUE;

   pData->s_lfPB.lfHeight         = HB_ISNIL( 3 ) ? pWindowData->fontHeight - 2 : hb_parnl( 3 );
   pData->s_lfPB.lfWidth          = HB_ISNIL( 4 ) ? pData->s_lfPB.lfWidth : hb_parni( 4 );
   pData->s_lfPB.lfEscapement     = 0;
   pData->s_lfPB.lfOrientation    = 0;
   pData->s_lfPB.lfWeight         = HB_ISNIL( 5 ) ? pData->s_lfPB.lfWeight : hb_parni( 5 );
   pData->s_lfPB.lfItalic         = HB_ISNIL( 7 ) ? pData->s_lfPB.lfItalic    : ( BYTE )hb_parl( 7 );
   pData->s_lfPB.lfUnderline      = HB_ISNIL( 8 ) ? pData->s_lfPB.lfUnderline : ( BYTE )hb_parl( 8 );
   pData->s_lfPB.lfStrikeOut      = HB_ISNIL( 9 ) ? pData->s_lfPB.lfStrikeOut : ( BYTE )hb_parl( 9 );
   pData->s_lfPB.lfCharSet        = DEFAULT_CHARSET;

   pData->s_lfPB.lfQuality        = HB_ISNIL( 6 ) ? pData->s_lfPB.lfQuality : ( BYTE )hb_parni( 6 );
   pData->s_lfPB.lfPitchAndFamily = FF_DONTCARE;
   if ( HB_ISCHAR( 2 ) )
   {
      strcpy( pData->s_lfPB.lfFaceName, hb_parcx( 2 ) );
   }

   if (pWindowData->hPBfont)
   {
      HFONT hOldFont = pWindowData->hPBfont;
      HFONT hFont = CreateFontIndirect( &pData->s_lfPB );
      if (hFont)
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while (pcd)
         {
           if ((pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON) &&
               ((HFONT) SendMessage( pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont)
              )
           {
              SendMessage( pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);
           }

           pcd = pcd->pNext;
         }

         pWindowData->hPBfont = hFont;
         DeleteObject( (HFONT) hOldFont );

      }
      else
      {
         retval = FALSE;
      }
   }

   hb_retl( retval );

}


/* PUSHBUTTON ends                                                   */



/* COMBOBOX begins (experimental)                                    */


/*WVW_CBcreate( [nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
 *                         nReserved, nKbdType, aOffset, hControl )
 *create combobox (drop-down list, no editbox) for window nWinNum
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nWidth: width of combobox (in character unit)
 *aText: array of drop-down list members, default = {"empty"}
 *       eg. {"yes","no"}
 *bBlock: codeblock to execute on these events:
 *         event=CBN_SELCHANGE(1): user changes selection
                        (not executed if selection
                        is changed programmatically)
 *         event=CBN_SETFOCUS
 *         event=CBN_KILLFOCUS
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCBid  : combobox id
 *         nType  : event type (CBN_SELCHANGE/CBN_SETFOCUS/CBN_KILLFOCUS supported)
 *         nIndex : index of the selected list item (0 based)
 *nListLines: number of lines for list items, default = 3
 *            (will be automatically truncated if it's > len(aText))
 *nReserved: reserved for future (this parm is now ignored)
 *
 *nKbdType: WVW_CB_KBD_STANDARD (0): similar to standard windows convention
 *            ENTER/ESC: will kill focus from combobox
 *          WVW_CB_KBD_CLIPPER (1):
 *            ENTER: drop (show) the list box
 *            UP/DOWN/TAB/SHIFTTAB/ESC: kill focus
 *default is WVW_CB_KBD_STANDARD (0)
 *
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of combobox.
 *         defaults: {-2,-2,+2,+2}
 *         NOTES: the third element (y2) is actually ignored.
 *                height of combobox is always 1 char height
 *                (see also wvw_cbSetFont())
 *
 *returns control id of newly created combobox of windows nWinNum
 *returns 0 if failed
 *
 *example:
 */

HB_FUNC( WVW_CBCREATE)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData =hb_gt_wvw_GetWindowsData( usWinNum );
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndCB;
   WVW_DATA * pData=  hb_getWvwData( ) ;
//   LONG cnt;
   LONG numofchars;
   LONG avgwidth;
   LONG LongComboWidth,NewLongComboWidth;
//   RECT r;
   HFONT hFont = hb_gt_wvwGetFont( pWindowData->fontFace, 10, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );

   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;

   UINT uiCBid;
   USHORT   usWidth  = ( USHORT )hb_parni( 4 );
   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = usTop,
            usRight  = usLeft + usWidth - 1;
   USHORT   usNumElement = (USHORT) (HB_ISARRAY(5) ? hb_arrayLen( hb_param(5, HB_IT_ARRAY) ) : 0);
   USHORT   usListLines  = (USHORT) (HB_ISNUM(7) ? hb_parni(7) : 3);
   BYTE     byCharHeight = hb_wvw_LineHeight(pWindowData);

   /* in the future combobox type might be selectable by 8th parameter */
   int      iStyle = CBS_DROPDOWNLIST | WS_VSCROLL;
   BYTE     bKbdType = HB_ISNUM(9) ? (BYTE)hb_parni(9) : (BYTE)WVW_CB_KBD_STANDARD;


   if (pWindowData->hCBfont==NULL)
   {
      pWindowData->hCBfont = CreateFontIndirect( &pData->s_lfCB );
      if (pWindowData->hCBfont==NULL)
      {
        hb_retnl(0);
        return;
      }
   }

   LongComboWidth = 0;
   iOffTop    = !HB_ISNIL( 10 ) ? hb_parvni( 10,1 ) : 0;
   iOffLeft   = !HB_ISNIL( 10 ) ? hb_parvni( 10,2 ) : 0;

   iOffBottom = usListLines;
   iOffRight  = !HB_ISNIL( 10 ) ? hb_parvni( 10,4 ) : 0;

   if (hb_gt_wvw_GetMainCoordMode())
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + (iOffBottom * byCharHeight);
   iRight  = xy.x - 1 + iOffRight;

   uiCBid = LastControlId(usWinNum, WVW_CONTROL_COMBOBOX);
   if (uiCBid==0)
   {
     uiCBid = WVW_ID_BASE_COMBOBOX;
   }
   else
   {
     uiCBid++;
   }

   InitCommonControls();

   hWndCB = CreateWindowEx(
       0L,
       "COMBOBOX",
       (LPSTR) NULL,
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,
       iLeft,
       iTop,
       iRight-iLeft+1,
       iBottom-iTop+1,
       hWndParent,
       (HMENU) uiCBid,
       hb_getWvwData()->hInstance,
       (LPVOID) NULL
   );

   if(hWndCB)
   {
     RECT rXB = { 0 }, rOffXB = { 0 };
     WNDPROC OldProc;
     USHORT i;
     TCHAR szDefault[] = TEXT( "empty" );

     SendMessage(
       (HWND) hWndCB,
       WM_SETREDRAW,
       (WPARAM) TRUE,
       (LPARAM) 0
     );

     if (usNumElement==0)
     {
       if (SendMessage((HWND) hWndCB,
                    CB_ADDSTRING,
                    (WPARAM) 0,
                    (LPARAM) (LPCTSTR) szDefault
                  ) < 0)
       {
         /* ignore failure */

       }
     }
     else
     {
       for(i=1; i<=usNumElement; i++)
       {
         if (SendMessage((HWND) hWndCB,
                      CB_ADDSTRING,
                      (WPARAM) 0,
                      (LPARAM) (LPCTSTR) hb_parvcx( 5, i)
                    ) < 0)
         {
           /* ignore failure */

         }
         else
         {
            numofchars =SendMessage((HWND) hWndCB,CB_GETLBTEXTLEN,i-1,0);
            if ( numofchars > LongComboWidth )
                LongComboWidth=numofchars;

         }
       }
     }

     SendMessage(
       (HWND) hWndCB,
       CB_SETCURSEL,
       (WPARAM) 0,
       (LPARAM) 0
     );

     SendMessage(
       (HWND) hWndCB,
       CB_SETEXTENDEDUI,
       (WPARAM) TRUE,
       (LPARAM) 0
     );
     avgwidth =GetFontDialogUnits(hWndParent,hFont);
     NewLongComboWidth = (LongComboWidth -2) *avgwidth;
     SendMessage(
       (HWND) hWndCB,
       CB_SETDROPPEDWIDTH,
       (WPARAM)  NewLongComboWidth+100,//LongComboWidth+100
       (LPARAM) 0
     );

     rXB.top = usTop;     rXB.left= usLeft;
     rXB.bottom=usBottom; rXB.right =usRight;
     rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;

     rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

     AddControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, uiCBid, (PHB_ITEM) hb_param( 6, HB_IT_BLOCK ), rXB, rOffXB, (byte) bKbdType);

     OldProc = (WNDPROC) SetWindowLongPtr (hWndCB,
                                        GWLP_WNDPROC, (LONG_PTR) hb_gt_wvwCBProc) ;

     StoreControlProc(usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, OldProc);

     SendMessage( hWndCB, WM_SETFONT, (WPARAM) pWindowData->hCBfont, (LPARAM) TRUE);
     hb_stornl((LONG)hWndCB,11);

     hb_retnl( (LONG) uiCBid );
   }
   else
   {

     hb_retnl( (LONG) 0 );
   }
}

/*WVW_CBdestroy( [nWinNum], nCBid )
 *destroy combobox nCBid for window nWinNum
 */
HB_FUNC( WVW_CBDESTROY)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData =hb_gt_wvw_GetWindowsData( usWinNum );
   UINT uiCBid = (UINT) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = (CONTROL_DATA *) NULL;

   while (pcd)
   {
     if (pcd->byCtrlClass == WVW_CONTROL_COMBOBOX && pcd->uiCtrlid == uiCBid)
     {
       break;
     }

     pcdPrev = pcd;
     pcd = pcd->pNext;
   }
   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_CBsetFocus( [nWinNum], nComboId )
 *set the focus to combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBSETFOCUS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  if (hWndCB)
  {

    hb_retl( SetFocus(hWndCB) != NULL );
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_CBisFocused( [nWinNum], nComboId )
 *returns .t. if the focus is on combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBISFOCUSED )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  hb_retl((HWND) GetFocus() == hWndCB);
}

/*WVW_CBenable( [nWinNum], nComboId, [lEnable] )
 *enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .t., ie. enabling the combobox)
 *return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
  WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  if (hWndCB)
  {
    hb_retl( EnableWindow(hWndCB, bEnable)==0 );

    if (!bEnable)
    {
       SetFocus( pWindowData->hWnd );
    }
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_CBsetcodeblock( [nWinNum], nCBid, bBlock )
 *assign (new) codeblock bBlock to combobox nCBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = (UINT) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   PHB_ITEM phiCodeBlock = hb_param( 3, HB_IT_BLOCK );
   WVW_DATA * pData=  hb_getWvwData( ) ;
   BOOL bOldSetting = pData->s_bRecurseCBlock;

   if (!phiCodeBlock || pcd==NULL || pcd->bBusy)
   {
     hb_retl( FALSE );
     return;
   }

   pData->s_bRecurseCBlock = FALSE;
   pcd->bBusy = TRUE;

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

   pcd->bBusy = FALSE;
   pData->s_bRecurseCBlock = bOldSetting;

   hb_retl( TRUE );
}

/*WVW_CBSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 *this will initialize font for ALL comboboxes in window nWinNum
 *(including ones created later on)
 *
 *TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_CBSETFONT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData =hb_gt_wvw_GetWindowsData( usWinNum );
   WVW_DATA * pData=  hb_getWvwData( ) ;

   BOOL  retval = TRUE;

   pData->s_lfCB.lfHeight         = HB_ISNIL( 3 ) ? pWindowData->fontHeight - 2 : hb_parnl( 3 );
   pData->s_lfCB.lfWidth          = HB_ISNIL( 4 ) ? pData->s_lfCB.lfWidth : hb_parni( 4 );
   pData->s_lfCB.lfEscapement     = 0;
   pData->s_lfCB.lfOrientation    = 0;
   pData->s_lfCB.lfWeight         = HB_ISNIL( 5 ) ? pData->s_lfCB.lfWeight : hb_parni( 5 );
   pData->s_lfCB.lfItalic         = HB_ISNIL( 7 ) ? pData->s_lfCB.lfItalic    : ( BYTE )hb_parl( 7 );
   pData->s_lfCB.lfUnderline      = HB_ISNIL( 8 ) ? pData->s_lfCB.lfUnderline : ( BYTE )hb_parl( 8 );
   pData->s_lfCB.lfStrikeOut      = HB_ISNIL( 9 ) ? pData->s_lfCB.lfStrikeOut : ( BYTE )hb_parl( 9 );
   pData->s_lfCB.lfCharSet        = DEFAULT_CHARSET;

   pData->s_lfCB.lfQuality        = HB_ISNIL( 6 ) ? pData->s_lfCB.lfQuality : ( BYTE )hb_parni( 6 );
   pData->s_lfCB.lfPitchAndFamily = FF_DONTCARE;
   if ( HB_ISCHAR( 2 ) )
   {
      strcpy( pData->s_lfCB.lfFaceName, hb_parcx( 2 ) );
   }

   if (pWindowData->hCBfont)
   {
      HFONT hOldFont = pWindowData->hCBfont;
      HFONT hFont = CreateFontIndirect( &pData->s_lfCB );
      if (hFont)
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while (pcd)
         {
           if ((pcd->byCtrlClass == WVW_CONTROL_COMBOBOX) &&
               ((HFONT) SendMessage( pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont)
              )
           {
              SendMessage( pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);

           }

           pcd = pcd->pNext;
         }

         pWindowData->hCBfont = hFont;
         DeleteObject( (HFONT) hOldFont );

      }
      else
      {
         retval = FALSE;
      }
   }

   hb_retl( retval );

}

/*WVW_CBsetIndex( [nWinNum], nCBid, nIndex )
 *set current selection of combobox nCBid in window nWinNum to nIndex
 *(nIndex is 0 based)
 *returns .t. if successful.
 *
 *NOTE: the better name to this function should be WVW_CBsetCurSel()
 *      but that name is already used.
 *      (WVW_CBsetcursel() and WVW_CBaddstring() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   int iIndex = hb_parni(3);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   BOOL  retval;

   if (pcd==NULL || iIndex < 0)
   {
     hb_retl(FALSE);
     return;
   }

   retval = (SendMessage((HWND) pcd->hWndCtrl,
                         CB_SETCURSEL,
                         (WPARAM) iIndex,
                         (LPARAM) 0
                         )==iIndex);
   hb_retl(retval);
}

/*WVW_CBgetIndex( [nWinNum], nCBid )
 *get current selection of combobox nCBid in window nWinNum
 *return nIndex (0 based)
 *returns CB_ERR (-1) if none selected
 *
 *NOTE: the better name to this function should be WVW_CBgetCurSel()
 *      but that name is potentially misleading to WVW_CBsetCursel
 *      which is not our family of WVW_CB* functions
 *      (WVW_CBsetcursel() and WVW_CBaddstring() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBGETINDEX )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int retval;

   if (pcd==NULL)
   {
     hb_retni(CB_ERR);
     return;
   }

   retval = SendMessage((HWND) pcd->hWndCtrl,
                        CB_GETCURSEL,
                        (WPARAM) 0,
                        (LPARAM) 0
                        );
   hb_retni(retval);
}

/*WVW_CBfindString( [nWinNum], nCBid, cString )
 *find index of cString in combobox nCBid in window nWinNum
 *returns index of cString (0 based)
 *returns CB_ERR (-1) if string not found
 *
 *NOTE:case insensitive
 */
HB_FUNC( WVW_CBFINDSTRING )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int retval;

   if (pcd==NULL)
   {
     hb_retni(CB_ERR);
     return;
   }

   retval = SendMessage((HWND) pcd->hWndCtrl,
                         CB_FINDSTRING,
                         (WPARAM) -1,
                         (LPARAM) (LPCSTR) hb_parcx(3)
                         );
   hb_retni(retval);
}

/*WVW_cbGetCurText( [nWinNum], nCBid )
 *get current selected cString in combobox nCBid in window nWinNum
 *returns "" if none selected
 *
 */
HB_FUNC( WVW_CBGETCURTEXT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int iCurSel,iTextLen;
   LPTSTR lptstr = NULL;

   if (pcd==NULL)
   {
     hb_retclen(lptstr,0);
     return;
   }

   iCurSel = SendMessage((HWND) pcd->hWndCtrl,
                        CB_GETCURSEL,
                        (WPARAM) 0,
                        (LPARAM) 0
                        );
   iTextLen = SendMessage((HWND) pcd->hWndCtrl,
                          CB_GETLBTEXTLEN,
                          (WPARAM) iCurSel,
                          (LPARAM) 0
                          );
   if (iTextLen==CB_ERR)
   {
     hb_retclen(lptstr, 0);
     return;
   }

   lptstr = ( char* ) hb_xgrab( iTextLen+1 );
   if (SendMessage((HWND) pcd->hWndCtrl,
                   CB_GETLBTEXT,
                   (WPARAM) iCurSel,
                   (LPARAM) lptstr
                   )==CB_ERR)
   {
     hb_retclen(lptstr, 0);
   }
   else
   {
     hb_retc(lptstr);
   }
   hb_xfree( lptstr );
}

/*WVW_cbIsDropped( [nWinNum], nCBid )
 *get current dropped state of combobox nCBid in window nWinNum
 *returns .t. if listbox is being shown, otherwise .f.
 *Also returns .f. if nCBid not valid
 */
HB_FUNC( WVW_CBISDROPPED )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   BOOL bDropped;

   if (pcd==NULL)
   {
      hb_retl(FALSE);
      return;
   }

   bDropped = SendMessage( (HWND) pcd->hWndCtrl,
                           CB_GETDROPPEDSTATE,
                           (WPARAM) 0,
                           (LPARAM) 0
                          );
   hb_retl(bDropped);
}


/* COMBOBOX ends (experimental)                                    */

