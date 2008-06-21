/*
 * $Id$
 */

// What32.Lib
// Rebar class

#include "common.ch"

#Include "winuser.ch"
#include "hbclass.ch"

#Include "wintypes.ch"
#Include "cstruct.ch"

pragma pack(4)

#Include "winstruc.ch"

#Include 'what32.ch'
#Include "commctrl.ch"

#Include 'debug.ch'

typedef struct tagREBARINFO;
{;
    UINT        cbSize;
    UINT        fMask;
    HIMAGELIST  himl;
}   REBARINFO, FAR *LPREBARINFO

typedef struct tagREBARBANDINFOA;
{;
    UINT        cbSize;
    UINT        fMask;
    UINT        fStyle;
    COLORREF    clrFore;
    COLORREF    clrBack;
    LPSTR       lpText;
    UINT        cch;
    int         iImage;
    HWND        hwndChild;
    UINT        cxMinChild;
    UINT        cyMinChild;
    UINT        cx;
    HBITMAP     hbmBack;
    UINT        wID;
    UINT        cyChild;
    UINT        cyMaxChild;
    UINT        cyIntegral;
    UINT        cxIdeal;
    LPARAM      lParam;
    UINT        cxHeader;
}   REBARBANDINFO, FAR *LPREBARBANDINFOA;

typedef struct tagNMREBARCHILDSIZE;
{;
    NMHDR hdr;
    UINT uBand;
    UINT wID;
    RECT rcChild;
    RECT rcBand;
} NMREBARCHILDSIZE, *LPNMREBARCHILDSIZE

typedef struct tagNMREBAR;
{;
    NMHDR   hdr;
    DWORD   dwMask;           // RBNM_*
    UINT    uBand;
    UINT    fStyle;
    UINT    wID;
    LPARAM  lParam;
} NMREBAR, *LPNMREBAR

typedef struct tagNMRBAUTOSIZE;
{;
    NMHDR hdr;
    BOOL fChanged;
    RECT rcTarget;
    RECT rcActual;
} NMRBAUTOSIZE, *LPNMRBAUTOSIZE

typedef struct tagNMREBARCHEVRON;
{;
    NMHDR hdr;
    UINT uBand;
    UINT wID;
    LPARAM lParam;
    RECT rc;
    LPARAM lParamNM;
} NMREBARCHEVRON, *LPNMREBARCHEVRON

typedef struct _RB_HITTESTINFO;
{;
    POINT pt;
    UINT flags;
    int iBand;
} RBHITTESTINFO, FAR *LPRBHITTESTINFO


*-----------------------------------------------------------------------------*

CLASS REBAR

  DATA hWnd
  DATA hParent
  DATA nStyle
  DATA nProc


  METHOD INIT() Constructor
  METHOD Create()
  METHOD AddBand()
  METHOD rbProc()
  METHOD GetHeight()
  ACCESS height INLINE ::GetHeight()
ENDCLASS

METHOD GetHeight()
LOCAL aRect:=GetWindowRect(::hWnd)
return aRect[4]-aRect[2]

*-----------------------------------------------------------------------------*

METHOD INIT()
   InitCommonControlsEx(ICC_COOL_CLASSES)


RETURN SELF


*-----------------------------------------------------------------------------*

METHOD create(hParent,nStyle)

//   LOCAL rbi IS REBARINFO

   ::hParent:=hParent
   ::nStyle :=IFNIL(nStyle,WS_VISIBLE+WS_BORDER+WS_CHILD+WS_CLIPCHILDREN+;
                           WS_CLIPSIBLINGS+RBS_VARHEIGHT+RBS_BANDBORDERS+;
                           CCS_NODIVIDER+CCS_NOPARENTALIGN+CCS_TOP,nStyle)

   ::hWnd := CreateWindowEx(WS_EX_TOOLWINDOW,;
                           REBARCLASSNAME,;
                           "",;
                           ::nStyle,;
                           0,0,200,100,;
                           hParent,;
                           1,;
                           hInstance(),;
                           0)


  ::nProc:=SetProcedure(::hParent,{|hWnd, nMsg,nwParam,nlParam| HB_SYMBOL_UNUSED( hWnd ), ::rbProc(nMsg,nwParam,nlParam)},{WM_SIZE})




  // rbi:cbSize := rbi:sizeof()  // Required when using this struct.
  // rbi:fMask  := 0
  // rbi:himl   := 0

   SendMessage(::hWnd, RB_SETBKCOLOR, 0, GetSysColor(COLOR_BTNFACE))
  // view SendMessage(::hWnd, RB_SETBARINFO, 0, rbi:value)

   return self


*-----------------------------------------------------------------------------*

METHOD rbProc(nMsg,nwParam,nlParam)
   LOCAL acRect
   LOCAL aRect
   DO CASE
   CASE nMsg==WM_SIZE
     acRect:=GetClientRect(::hParent)
     aRect:=GetWindowRect(::hWnd)
     MoveWindow(::hWnd,0,0,acRect[3],aRect[4]-aRect[2],.t.)
   ENDCASE
RETURN CallWindowProc(::nProc,::hParent,nMsg,nwParam,nlParam)




*-----------------------------------------------------------------------------*
METHOD addband(nMask,nStyle,hChild,cxMin,cyMin,cx,cText,hBmp,nPos)

   LOCAL rbBand IS REBARBANDINFO
   LOCAL aRect:=GetWindowRect(hChild)

   HB_SYMBOL_UNUSED( nPos )

   rbBand:Reset()

   // Initialize structure members that most bands will share.
   rbBand:cbSize := rbBand:sizeof()  // Required

   rbBand:fMask  := IFNIL(nMask,RBBIM_TEXT +; //RBBIM_BACKGROUND +;
                                RBBIM_STYLE +RBBIM_CHILDSIZE+;
                                RBBIM_SIZE+RBBIM_CHILD,nMask)

   rbBand:fStyle     := IFNIL(nStyle,RBBS_GRIPPERALWAYS+RBBS_NOVERT/*+RBBS_CHILDEDGE*/,nStyle)// + RBBS_FIXEDBMP
   rbBand:hwndChild  := IFNIL(hChild,0,hChild)
   rbBand:cxMinChild := IFNIL(cxMin,aRect[3]-aRect[1],cxMin)
   rbBand:cyMinChild := IFNIL(cyMin,aRect[4]-aRect[2],cyMin)
   rbBand:cx         := IFNIL(cx,GetClientRect(::hParent)[3],cx)
   rbBand:lpText     := IFNIL(cText,"Test",cText)
   rbBand:hbmBack    := IFNIL(hBmp,0,hBmp) //LoadBitmap(hInstance(), "IDB_BACKGRND"),hBmp)


  // view rbBand,aRect,LoadBitmap(hInstance(), "IDB_BACKGRND"), rbBand:value

   // Add the band
   RETURN SendMessage(::hWnd, RB_INSERTBAND, -1, rbBand:value ) != 0
