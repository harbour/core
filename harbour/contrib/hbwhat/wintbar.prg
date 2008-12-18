/*
 * $Id$
 */

//----------------------------------------------------------------------//
// hbwhat
// Tollbar class
//----------------------------------------------------------------------//

#include "common.ch"
#include "winuser.ch"
#include "hbclass.ch"
#include "commctrl.ch"
#include "debug.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "hbwhat.ch"

//----------------------------------------------------------------------//

pragma pack(4)

typedef struct _RECT { ;
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT

typedef struct tagNMHDR {;
    HWND hwndFrom;
    UINT idFrom;
    UINT code;
} NMHDR

typedef struct _TBBUTTON {;
    int iBitmap;
    int idCommand;
    BYTE fsState;
    BYTE fsStyle;
    DWORD dwData;
    int iString;
} TBBUTTON, NEAR* PTBBUTTON, FAR* LPTBBUTTON

typedef struct tagNMTOOLBAR {;
    NMHDR   hdr;
    int     iItem;
    TBBUTTON tbButton;
    int     cchText;
    LPSTR   pszText;
    RECT    rcButton;
} NMTOOLBAR, FAR* LPNMTOOLBAR



typedef struct tagTBADDBITMAP {;
        HINSTANCE       hInst;
        UINT            nID;
} TBADDBITMAP, *LPTBADDBITMAP

typedef struct {;
    NMHDR     hdr;        // required for all WM_NOTIFY messages
    LPTSTR    lpszText;   // see below
    char      szText[80]; // buffer for tool tip text
    HINSTANCE hinst;      // see below
    UINT      uflags;     // flag indicating how to interpret the idFrom member of the NMHDR structure that is included in the structure
} TOOLTIPTEXT, FAR *LPTOOLTIPTEXT

/*
typedef struct tagNMTBHOTITEM {;
    NMHDR hdr;
    int idOld;
    int idNew;
    DWORD dwFlags;
} NMTBHOTITEM, FAR *LPNMTBHOTITEM
*/

// based on cToolBar  class
//----------------------------------------------------------------------//
CLASS WHT_TOOLBAR

   DATA abuttons
   DATA aBitmaps
   DATA hParent
   DATA hWnd
   DATA nId
   DATA nStyle
   DATA Created
   DATA aText
   DATA hBmp
   DATA nProc
   DATA aTips
   DATA nBtn HIDDEN
   DATA aMenus

   METHOD Init() CONSTRUCTOR

   METHOD AddButton
   METHOD AddBitmap
   METHOD AddString
   METHOD Create
   METHOD createbuttons
   METHOD tbProc

   METHOD setsizes(xBtn,yBtn,xImg,yImg )
   METHOD setheight(nHeight )
   METHOD loadbitmap
   METHOD setbitmap
   METHOD setbuttons
   METHOD commandtoindex
   METHOD GetItemId
   METHOD getitemrect
   METHOD getbuttonstyle
   METHOD getbuttoninfo
   METHOD setbuttoninfo
   METHOD getbuttontext
   METHOD setbuttontext
   METHOD gettollbarctrl
   METHOD disable
   METHOD enable
   METHOD disableall
   METHOD enableall
   METHOD CheckButton
   METHOD IsButtonChecked
   METHOD AddMenu

   ENDCLASS
//----------------------------------------------------------------------//
METHOD Init()

   VWN_InitCommonControlsEx( ICC_BAR_CLASSES )

   ::aButtons:={}
   ::aTips   :={}
   ::nStyle  :=0
   ::nId     :=0
   ::Created := .F.
   ::aText   :={}
   ::aBitmaps:={}
   ::aMenus  :={}

   RETURN(self)
//----------------------------------------------------------------------//
METHOD AddBitmap(hInst, nhIdBmp, nButtons)
   LOCAL tbab IS TBADDBITMAP

   DEFAULT nButtons TO 1

   tbab:hInst := hInst
   tbab:nId   := nhIdBmp

   AADD(::aBitmaps,{tbab,nButtons})
   IF ::created
     VWN_SendMessage(::hWnd,TB_ADDBITMAP,nButtons,tbab:value)
   ENDIF

   RETURN(1)
//----------------------------------------------------------------------//
METHOD AddButton(nIndex, nId, nState, nStyle, ncargo, nString, cText, cToolTip )
   LOCAL tbb IS TBBUTTON

   HB_SYMBOL_UNUSED( cText )

   tbb:ibitmap   :=IFNIL( nIndex,-1,nIndex )
   tbb:idCommand :=nId // must be supplied
   tbb:fsState   :=IFNIL( nState,TBSTATE_ENABLED,nState )
   tbb:fsStyle   :=IFNIL( nStyle,TBSTYLE_BUTTON,nStyle )
   tbb:dwData    :=IFNIL( ncargo,0,nCargo )
   tbb:iString   :=IFNIL( nString,0,nString )

   AADD( ::aButtons,tbb )
   AADD( ::aTips,cToolTip )
   IF ::Created
      VWN_SendMessage( ::hWnd,TB_ADDBUTTONS,1,tbb:value )
   Endif

   RETURN(self)
//----------------------------------------------------------------------//
METHOD AddMenu(nButton, nMenuId, cMenuText )
   AADD( ::aMenus,{nButton, nMenuId, cMenuText })
   return( self )
//----------------------------------------------------------------------//
METHOD addstring( cText )

   IF ::created
      VWN_SendMessage( ::hWnd,TB_ADDSTRING,0,cText )
   Else
      AADD( ::aText,cText )
   Endif

   RETURN( self )
//----------------------------------------------------------------------//
//  HWND CreateToolbarEx( HWND hwnd, DWORD ws, UINT wID,int nBitmaps,HINSTANCE hBMInst,
//                        UINT wBMID,LPCTBBUTTON lpButtons,int iNumButtons,int dxButton,
//                        int dyButton, int dxBitmap, int dyBitmap,UINT uStructSize );
//----------------------------------------------------------------------//
METHOD Create(hParent,nStyle,nId,nImg,hBMInst,nBMId,xBtn,yBtn,xBmp,yBmp)
   LOCAL cButtons:=""
   LOCAL tbb IS TBBUTTON
   LOCAL i


   ::hParent := hParent
   ::nStyle  := IFNIL( nStyle,TBSTYLE_FLAT+WS_CHILD+WS_VISIBLE,nStyle )
   ::nId     := IFNIL( nId,0,nId)

   if ISNIL(hBMInst) .AND. ISNIL(nBMId)
     hBMInst:=HINST_COMMCTRL
     nBMId  :=IDB_STD_LARGE_COLOR
   endif

   FOR i:=1 TO LEN(::aButtons)
      cButtons+=::aButtons[i]:Value
   NEXT

// ::hWnd:=CreateWindowEx(0,TOOLBARCLASSNAME,"",::nStyle,0,0,300,30,::hParent,::nId)


   ::hWnd := VWN_CreateToolBarEx(::hParent,::nStyle,::nId,nImg,hBMInst,nBMId,cButtons,LEN(::aButtons),;
                                  xbtn,yBtn,xBmp,yBmp, tbb:sizeof())

   ::nProc := WHT_SetProcedure( ::hParent,{|hWnd, nMsg,nwParam,nlParam| HB_SYMBOL_UNUSED( hWnd ), ;
                                  ::tbProc(nMsg,nwParam,nlParam)},{WM_NOTIFY})


//   SendMessage(::hWnd,TB_BUTTONSTRUCTSIZE,::aButtons[1]:sizeof,0)

 //  FOR i:=1 TO LEN(::aBitmaps)
 //    SendMessage(::hWnd,TB_ADDBITMAP,::aBitmaps[i,2],::aBitmaps[i,1]:value)
 //  NEXT

   VWN_SendMessage( ::hwnd,TB_SETEXTENDEDSTYLE,0,TBSTYLE_EX_DRAWDDARROWS )

   //SendMessage(::hWnd,TB_ADDBUTTONS,LEN(::aButtons),cButtons)

   FOR i := 1 to LEN( ::aText )
      VWN_SendMessage( ::hWnd,TB_ADDSTRING,0,::aText[ i ] )
   NEXT

   ::Created := .T.

   RETURN( ::hWnd )
//----------------------------------------------------------------------//
#undef TBN_DROPDOWN
#undef TTN_NEEDTEXT
#undef TBN_QUERYINSERT
#undef TBN_QUERYDELETE
#undef TBN_GETBUTTONINFO

#DEFINE TBN_DROPDOWN      0
#DEFINE TTN_NEEDTEXT      1
#DEFINE TBN_QUERYINSERT   2
#DEFINE TBN_QUERYDELETE   3
#DEFINE TBN_GETBUTTONINFO 4
//----------------------------------------------------------------------//
METHOD tbProc(nMsg,nwParam,nlParam)
   LOCAL Hdr
   LOCAL Ttt
   LOCAL nmt
   LOCAL hMenu,aRect, aPoint
   LOCAL n,x

   DO CASE
   CASE nMsg == WM_NOTIFY
     Hdr IS NMHDR
     Hdr:Buffer( WHT_Peek(nlParam,Hdr:sizeof))

     DO CASE
     CASE Hdr:code==TBN_DROPDOWN
         Nmt IS NMTOOLBAR
         nmt:buffer( WHT_Peek(nlParam,nmt:sizeof))
         IF ASCAN( ::aMenus,{|a| a[1]==nmt:iItem}) > 0
            ::nBtn:=nmt:iItem
            hMenu := VWN_CreatePopupMenu()
            FOR x:=1 TO len(::aMenus)
                IF ::aMenus[x][1] == nmt:iItem
                   IF ::aMenus[x][3] == "-".and. ::aMenus[x][2] == 0
                      VWN_AppendMenu( hMenu, MF_SEPARATOR )
                     ELSE
                      VWN_AppendMenu( hMenu, MF_ENABLED + MF_STRING,::aMenus[x][2],::aMenus[x][3])
                   ENDIF
                ENDIF
            NEXT

            x := ASCAN( ::aButtons,{|btn| btn:idCommand==nmt:iItem } )
            aRect := VWN_GetToolBarItemRect( ::hWnd,x-1 )
            aPoint := { aRect[ 1 ],aRect[ 4 ] }
            VWN_ClientToScreen( ::hParent, @aPoint )
            VWN_TrackPopupMenu( hMenu, TPM_LEFTALIGN+TPM_TOPALIGN, aPoint[1]+9, aPoint[2], 0, ::hWnd )
            VWN_DestroyMenu( hMenu )
            RETURN 0
         end
     CASE Hdr:code==TTN_NEEDTEXT
       IF (n := ASCAN(::aButtons,{|btn| btn:idCommand==Hdr:idFrom})) > 0
         Ttt IS TOOLTIPTEXT
         Ttt:Buffer( WHT_Peek(nlParam,Ttt:sizeof))
         Ttt:lpszText:=::aTips[n] //"ID:"+STR(Hdr:IdFrom)
         WHT_Poke(nlParam,Ttt:value,Ttt:sizeof)
       ENDIF

     CASE Hdr:code==TBN_QUERYINSERT
         RETURN(1)
     CASE Hdr:code==TBN_QUERYDELETE
         RETURN(1)
     CASE Hdr:code==TBN_GETBUTTONINFO
         Nmt IS NMTOOLBAR
         nmt:buffer( WHT_Peek(nlParam,nmt:sizeof))
         /*
         int     iItem;      // cmd
         TBBUTTON tbButton;
         int     cchText;    // str len
         LPSTR   pszText;    // btn text
         RECT    rcButton;   // new (!)
         */
         RETURN(1)

     ENDCASE
   ENDCASE

   RETURN( VWN_CallWindowProc( ::nProc,::hParent,nMsg,nwParam,nlParam ) )
//----------------------------------------------------------------------//
METHOD CreateButtons()
   LOCAL i

   FOR i:=1 TO LEN( ::aBitmaps )
   NEXT

   RETURN(NIL)
//----------------------------------------------------------------------//
METHOD SetSizes( xBtn,yBtn,xImg,yImg )

   HB_SYMBOL_UNUSED( xBtn )
   HB_SYMBOL_UNUSED( yBtn )
   HB_SYMBOL_UNUSED( xImg )
   HB_SYMBOL_UNUSED( yImg )

   RETURN(self)
//----------------------------------------------------------------------//
METHOD SetHeight(nHeight )

   HB_SYMBOL_UNUSED( nHeight )

   RETURN(self)
//----------------------------------------------------------------------//
METHOD loadbitmap
   RETURN(self)
//----------------------------------------------------------------------//
METHOD setbitmap
   RETURN(self)
//----------------------------------------------------------------------//
METHOD setbuttons
   RETURN(self)
//----------------------------------------------------------------------//
METHOD commandtoindex
   RETURN(self)
//----------------------------------------------------------------------//
METHOD GetItemId
   RETURN(self)
//----------------------------------------------------------------------//
METHOD getitemrect(nIndex)
   RETURN( VWN_GetToolbarItemRect( ::hWnd,nIndex ) )
//----------------------------------------------------------------------//
METHOD getbuttonstyle
   RETURN(self)
//----------------------------------------------------------------------//
METHOD getbuttoninfo
   RETURN(self)
//----------------------------------------------------------------------//
METHOD setbuttoninfo
   RETURN(self)
//----------------------------------------------------------------------//
METHOD getbuttontext
   RETURN(self)
//----------------------------------------------------------------------//
METHOD setbuttontext
   RETURN(self)
//----------------------------------------------------------------------//
METHOD gettollbarctrl
   RETURN(self)
//----------------------------------------------------------------------//
METHOD disable(nBtn)
   VWN_SendMessage( ::hWnd,TB_ENABLEBUTTON,nBtn,0 )
   RETURN( SELF )
//----------------------------------------------------------------------//
METHOD enable(nBtn,lFlag)
   DEFAULT lFlag TO .T.
   VWN_SendMessage(::hWnd,TB_ENABLEBUTTON,nBtn,iif(lFlag,1,0))
   RETURN(SELF)
//----------------------------------------------------------------------//
METHOD DisableAll()
   AEVAL(::aButtons,{|btn| ::disable( btn:idCommand )})
   return(self)
//----------------------------------------------------------------------//
METHOD EnableAll()
   AEVAL(::aButtons,{|btn| ::enable( btn:idCommand )})
   return(self)
//----------------------------------------------------------------------//
METHOD CheckButton(nBtn,lFlag)
   DEFAULT lFlag TO !::IsButtonChecked(nBtn)
   VWN_SendMessage( ::hWnd,TB_CHECKBUTTON,nBtn,iif( lFlag,1,0 ) )
   RETURN(SELF)
//----------------------------------------------------------------------//
METHOD IsButtonChecked(nBtn)
   RETURN( IIF( VWN_SendMessage(::hWnd,TB_ISBUTTONCHECKED,nBtn,0 )==0,.F.,.T. ) )
//----------------------------------------------------------------------//
