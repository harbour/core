/*
 * $Id$
 */

#Define WIN_WANT_VER4
#define WIN_WANT_ALL
#Include "winuser.ch"
#include "hbclass.ch"
//#Include 'debug.ch'
#Include "commctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#include "wingdi.ch"


// move that structure to WinStruc.ch

typedef struct {;
    HWND hwndFrom;
    UINT idFrom;
    UINT code;
} NMHDR

*-----------------------------------------------------------------------------*

CLASS TabControl

   DATA hParent
   DATA hTab AS  NUMERIC
   DATA Tabs AS  ARRAY HIDDEN
   DATA Dlgs AS  ARRAY HIDDEN
   DATA Procs AS ARRAY HIDDEN
   DATA nCurSel
   DATA nProc
   DATA nId
   DATA Cargo

   METHOD New() CONSTRUCTOR
   METHOD TabProc()
   METHOD Add()
   METHOD Insert()
   METHOD Delete()
   METHOD Configure()
   METHOD AdjustRect()
   METHOD DeleteAll()
   METHOD DeselectAll()
   METHOD GetCurFocus()
   METHOD GetCurSel()
   METHOD GetExtendedStyle()
   METHOD GetImageList()
   METHOD GetItem()
   METHOD GetItemCount()
   METHOD GetItemRect()
   METHOD GetRowCount()
   METHOD GetToolTips()
   METHOD GetUnicodeFormat()
   METHOD HighlightItem()
   METHOD HitTest()
   METHOD RemoveImage()
   METHOD SetCurFocus()
   METHOD SetCurSel()
   METHOD SetExtendedStyle()
   METHOD SetImageList()
   METHOD SetItem()
   METHOD SetItemExtra()
   METHOD SetItemSize()
   METHOD SetMinTabWidth()
   METHOD SetPadding()
   METHOD SetToolTips()
   METHOD SetUnicodeFormat()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( hDlg, nL, nT, nW, nH, nStyle, nSel,nId )
   ::hParent:=hDlg
   ::Tabs:={}
   ::Dlgs:={}
   ::Procs:={}
   ::nId:=nId
   ::nCurSel:=IF(nSel==NIL,1,nSel)
   ::hTab:=TabCtrl_Create( hDlg, nL, nT, nW, nH, nStyle,nId)
   ::nProc:=SetProcedure( hDlg, {|hDlg,nMsg,nwParam,nlParam|;
            ::TabProc(hDlg,nMsg,nwParam,nlParam)} , {WM_NOTIFY} )
RETURN Self

*-----------------------------------------------------------------------------*

static FUNCTION _TempPageProc(nMsg)
   IF nMsg==WM_CTLCOLORDLG
      return(GetStockObject(NULL_BRUSH))
   END

   RETURN(0)

*-----------------------------------------------------------------------------*
#define TCN_SELCHANGE 0

METHOD TabProc(hDlg, nMsg, nwParam, nlParam)

   LOCAL tnhdr

   LOCAL n,nSel
   LOCAL lVisible
   LOCAL lEnabled
   LOCAL nLen

   IF nMsg==WM_NOTIFY

      tnhdr IS NMHDR
      tnhdr:Buffer( peek(nlParam,tnhdr:sizeof() ) )

      IF tnhdr:code==TCN_SELCHANGE

        nSel:=TabCtrl_GetCurSel( ::hTab )+1
        IF ::nCursel <> nSel
           ShowWindow(::Tabs[::nCurSel], SW_HIDE)
           ::nCurSel:=nSel
           ShowWindow(::Tabs[::nCurSel], SW_SHOW)
           /*
           IF ::nCurSel > 0

              //nLen:=len(::Tabs[::nCurSel])
              nLen:=if(EMPTY(::Tabs[::nCurSel]),0,len(::Tabs[::nCurSel]))
              FOR n:=1 TO nLen
                 ::Tabs[::nCurSel,n,2]:=isWindowEnabled(::Tabs[::nCurSel,n,1])
                 ::Tabs[::nCurSel,n,3]:=isWindowVisible(::Tabs[::nCurSel,n,1])
                 ShowWindow(::Tabs[::nCurSel,n,1],SW_HIDE)
                 EnableWindow(::Tabs[::nCurSel,n,1],.F.)
              NEXT
           ENDIF

           ::nCurSel:=nSel
           nLen:=if(EMPTY(::Tabs[::nCurSel]),0,len(::Tabs[::nCurSel]))

           FOR n:=1 TO nLen
              IF ::Tabs[::nCurSel,n,2]
                 EnableWindow(::Tabs[::nCurSel,n,1],.T.)
              ENDIF
              IF ::Tabs[::nCurSel,n,3]
                 ShowWindow(::Tabs[::nCurSel,n,1],SW_SHOW)
              ENDIF
           NEXT
           */

        ENDIF

      ENDIF

   ENDIF

   Return( CallWindowProc(::nProc, hDlg, nMsg, nwParam, nlParam) )

*-----------------------------------------------------------------------------*

METHOD Add(cText,cRes,bProc,nImgPos)
LOCAL hTab

   IF (hTab:=TabCtrl_AddItem(::hTab,cText,nImgPos)) > -1
      AADD(::Dlgs,cRes)
      AADD(::Tabs,NIL )
      AADD(::Procs,bProc)
   ENDIF

   RETURN(hTab)

*-----------------------------------------------------------------------------*

METHOD Insert(nPos,cText,cRes,bProc,nImgPos)

   if TabCtrl_InsertItem(::hTab,cText,nPos,nImgpos) > -1
      AINS(::Dlgs,nPos,cRes,.T.)
      aIns(::Tabs,nPos,NIL,.T.)
      AINS(::Procs,nPos,bProc,.T.)
      RETURN(.T.)
   ENDIF

   return(.F.)

*-----------------------------------------------------------------------------*

METHOD Delete(nPos)

   Local nCount:=LEN(::Tabs)
   if nPos > 0 .and. nPos <= nCount
      IF nPos <= ::nCurSel     // verify !!!!!
        ::nCurSel--
      ENDIF
      TabCtrl_DeleteItem(nPos-1)
      ADel(::Dlgs,nPos,.t.)
      if isWindow(::Tabs[nPos])
         DestroyWindow(::Tabs[nPos])
      endif
      ADel(::Tabs,nPos,.t.)
      ADEL(::Procs,nPos,.T.)
      return(.T.)
   Endif

   RETURN(.F.)

*-----------------------------------------------------------------------------*

METHOD Configure()

   LOCAL aTab :=GetClientRect(::hTab)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL i
   LOCAL aPt
   LOCAL bBlock

   aPt:={aTab[1],aTab[2]}
   ClientToScreen(::hTab   ,aPt)
   ScreenToClient(::hParent,aPt)
   aTab[1]:=aPt[1]
   aTab[2]:=aPt[2]

   aPt:={aTab[3],aTab[4]}
   ClientToScreen(::hTab   ,aPt)
   ScreenToClient(::hParent,aPt)
   aTab[3]:=aPt[1]
   aTab[4]:=aPt[2]


   IF LEN(::Tabs) > 0
      acRect:=TabCtrl_GetItemRect(::hTab,0)
      FOR i:=1 TO LEN(::Tabs)-1
         aTemp:=TabCtrl_GetItemRect(::hTab,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   aPt:={acRect[1],acRect[2]}
   ClientToScreen(::hTab   ,aPt)
   ScreenToClient(::hParent,aPt)
   acRect[1]:=aPt[1]
   acRect[2]:=aPt[2]
   /*
   aPt:={acRect[3],acRect[4]}
   ClientToScreen(::hTab   ,aPt)
   ScreenToClient(::hParent,aPt)
   acRect[3]:=aPt[1]
   acRect[4]:=aPt[2]
   */
   FOR i:=1 TO LEN(::Dlgs)
      IF ::Dlgs[i] != NIL .AND. EMPTY(::Tabs[i])

         hCtrl:=CreatePage(::Dlgs[i],::hParent,::Procs, i )
         ::Tabs[i]:=hCtrl
         MoveWindow( hCtrl, acRect[1]+4, acRect[2]+acRect[4]+4, aTab[3]-aTab[1]-8, aTab[4]-(acRect[4]+acRect[2])- 8, .F. )
         IF i<>::nCurSel
            ShowWindow(hCtrl,SW_HIDE)
         ENDIF
      ENDIF
   NEXT

   RETURN(self)

*-----------------------------------------------------------------------------*
Static Function CreatePage(acRes,hParent,aProcs, i)
   Local bBlock:=IF(valtype( aProcs[i])== "B", aProcs[i], {|nMsg| _TempPageProc(nMsg)} )
   RETURN CreateDialog( , acRes, hParent, bBlock )

*-----------------------------------------------------------------------------*
METHOD AdjustRect(lDisplay,aRect)

   TabCtrl_AdjustRect(::hTab,lDisplay,@aRect)

   RETURN(aRect)

*-----------------------------------------------------------------------------*
METHOD DeleteAll()

  Local lRet:=TabCtrl_DeleteAllItems(::hTab)

  AEVAL(::Tabs,{|hWnd| IF(isWindow(hWnd),DestroyWindow(hWnd),)})
  ::Tabs:={}
  ::aDlg:={}
  ::Procs:={}
  ::nCurSel:=0

  RETURN(lRet)

*-----------------------------------------------------------------------------*
METHOD DeselectAll(lExcludeFocus)

   TabCtrl_DeselectAll(::hTab,lExcludeFocus)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD GetCurFocus()

  RETURN TabCtrl_GetCurFocus(::hTab )+1

*-----------------------------------------------------------------------------*
METHOD GetCurSel()

   RETURN TabCtrl_GetCurSel(::hTab)+1

*-----------------------------------------------------------------------------*
METHOD GetExtendedStyle()

   RETURN TabCtrl_GetExtendedStyle(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetImageList()

   RETURN NIL //TabCtrl_GetImageList(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetItem(nItem,ptrItem)

   RETURN TabCtrl_GetItem(::hTab,nItem-1,@ptrItem)

*-----------------------------------------------------------------------------*
METHOD GetItemCount()

   RETURN TabCtrl_GetItemCount(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetItemRect(nItem)

   RETURN TabCtrl_GetItemRect(::hTab,nItem-1)

*-----------------------------------------------------------------------------*
METHOD GetRowCount()

   RETURN TabCtrl_GetRowCount(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetToolTips()

   RETURN TabCtrl_GetToolTips(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetUnicodeFormat()

   RETURN TabCtrl_GetUnicodeFormat(::hTab)

*-----------------------------------------------------------------------------*
METHOD HighlightItem(nItem,nHighlight)

   RETURN TabCtrl_HighlightItem(::hTab,nItem-1,nHighlight)

*-----------------------------------------------------------------------------*
METHOD HitTest(nPtrHitTestInfo)

   RETURN TabCtrl_HitTest(::hTab,nPtrHitTestInfo) + 1

*-----------------------------------------------------------------------------*
METHOD RemoveImage(nImageIndex)

   RETURN TabCtrl_RemoveImage(::hTab, nImageIndex-1)

*-----------------------------------------------------------------------------*
METHOD SetCurFocus(nItem)

   TabCtrl_SetCurFocus(::hTab, nItem-1)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetCurSel(nItem)

   RETURN TabCtrl_SetCurSel(::hTab, nItem-1) + 1

*-----------------------------------------------------------------------------*
METHOD SetExtendedStyle(nExStyle)

   RETURN TabCtrl_SetExtendedStyle(::hTab,nExStyle)

*-----------------------------------------------------------------------------*
METHOD SetImageList(hImageList)

   RETURN TabCtrl_SetImageList(::hTab, hImageList)

*-----------------------------------------------------------------------------*
METHOD SetItem(nItem, cText)

   RETURN TabCtrl_SetItem(::hTab, nItem-1, cText )

*-----------------------------------------------------------------------------*
METHOD SetItemExtra(nBytes)

   RETURN TabCtrl_SetItemExtra(::hTab, nBytes)

*-----------------------------------------------------------------------------*
METHOD SetItemSize(x,y)

   RETURN TabCtrl_SetItemSize(::hTab, x, y )

*-----------------------------------------------------------------------------*
METHOD SetMinTabWidth(dx)

   RETURN TabCtrl_SetMinTabWidth( ::hTab, dx )

*-----------------------------------------------------------------------------*
METHOD SetPadding( cx, cy )

   TabCtrl_SetPadding( ::hTab, cx, cy )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetToolTips( hToolTips )

   TabCtrl_SetToolTips( ::hTab, hToolTips )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetUnicodeFormat( lUnicode )

   RETURN TabCtrl_SetUnicodeFormat( ::hTab, lUnicode )

*-----------------------------------------------------------------------------*




/*
*-----------------------------------------------------------------------------*

METHOD Configure()


   LOCAL aTab :=GetClientRect(::hTab)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL cRes
   LOCAL i

   IF LEN(::Tabs) > 0
      acRect:=TabCtrl_GetItemRect(::hTab,1)
      FOR i:=2 TO LEN(::Tabs)
         aTemp:=TabCtrl_GetItemRect(::hTab,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   FOR i:=1 TO LEN(::Dlgs)
      aWnd:={}
      IF (cRes:=::Dlgs[i]) != NIL .AND. EMPTY(::Tabs[i])
         hCtrl :=CreateDialog(,cRes, ::hTab,{|| _TempPageProc()})
         MoveWindow(hCtrl,4,acRect[2]+acRect[4]+4,aTab[3]-8,aTab[4]-acRect[2]-acRect[4]-6,.F.)
         aWnd:=TransferChildren(::hParent,hCtrl,i==::nCurSel)
         DestroyWindow(hCtrl)
         ::Tabs[i]:=ACLONE(aWnd)
      ENDIF
   NEXT

   RETURN(self)

*-----------------------------------------------------------------------------*

Function TransferChildren(hDlg,hPage,lShow)

   LOCAL aChildren:={}
   LOCAL aRect
   LOCAL aPt
   LOCAL lVisible
   LOCAL lEnabled
   LOCAL cClass
   LOCAL cText
   LOCAL nStyle
   LOCAL nExStyle
   LOCAL nId
   LOCAL hNewWnd
   LOCAL hWnd:=GetWindow(hPage,GW_CHILD)

   DO WHILE !EMPTY(hWnd)
      aRect:=GetWindowRect(hWnd)
      aPt:={aRect[1],aRect[2]}
      ScreenToClient(hDlg,aPt)
      aRect[3]-=aRect[1]
      aRect[4]-=aRect[2]
      aRect[1]:=aPt[1]
      aRect[2]:=aPt[2]
      cClass:=GetClassName(hWnd)
      cText:=GetWindowText(hWnd)
      nStyle:=GetWindowLong(hWnd,GWL_STYLE)
      nExStyle:=GetWindowLong(hWnd,GWL_EXSTYLE)
      nId:=GetWindowLong(hWnd,GWL_ID)
      hNewWnd:=CreateWindowEx(nExStyle,cClass,cText,nStyle,;
               aRect[1],aRect[2],aRect[3],aRect[4],hDlg,nId)
      SendMEssage(hNewWnd,WM_SETFONT,SendMessage(hWnd,WM_GETFONT,0,0), 0 )
      lVisible:=AND(nStyle,WS_VISIBLE)==WS_VISIBLE
      lEnabled:=isWindowEnabled(hWnd)
      AADD(aChildren,{hNewWnd,lEnabled,lVisible})
      IF !lShow
         ShowWindow(hNewWnd,SW_HIDE)
         EnableWindow(hNewWnd,.F.)
      Endif
      hWnd:=GetWindow(hWnd,GW_HWNDNEXT)
   ENDDO

   RETURN(aChildren)

*-----------------------------------------------------------------------------*

*/




















