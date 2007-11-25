/*
 * $Id$
 */

* Automatically translated from commctrl.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.


/*****************************************************************************\
*                                                                             *
* commctrl.h - - Interface for the Windows Common Controls                    *
*                                                                             *
* Version 1.2                                                                 *
*                                                                             *
* Copyright (c) 1991-1998, Microsoft Corp.      All rights reserved.          *
*                                                                             *
\*****************************************************************************/


#ifndef _INC_COMMCTRL
  //P_O_Push
  #define _INC_COMMCTRL

  #ifndef _HRESULT_DEFINED
    #define _HRESULT_DEFINED
  #endif // _HRESULT_DEFINED

  //
  // Define API decoration for direct importing of DLL references.
  //
  #ifndef WINCOMMCTRLAPI
    #define WINCOMMCTRLAPI
  #endif // WINCOMMCTRLAPI

  //
  // For compilers that don't support nameless unions
  //
  #ifndef DUMMYUNIONNAME
    #ifdef NONAMELESSUNION
      #define DUMMYUNIONNAME   u
      #define DUMMYUNIONNAME2  u2
      #define DUMMYUNIONNAME3  u3
      #define DUMMYUNIONNAME4  u4
      #define DUMMYUNIONNAME5  u5
    #else
      #define DUMMYUNIONNAME
      #define DUMMYUNIONNAME2
      #define DUMMYUNIONNAME3
      #define DUMMYUNIONNAME4
      #define DUMMYUNIONNAME5
    #endif
  #endif // DUMMYUNIONNAME

  //
  // Users of this header may define any number of these constants to avoid
  // the definitions of each functional group.
  //
  //    NOTOOLBAR    Customizable bitmap-button toolbar control.
  //    NOUPDOWN     Up and Down arrow increment/decrement control.
  //    NOSTATUSBAR  Status bar control.
  //    NOMENUHELP   APIs to help manage menus, especially with a status bar.
  //    NOTRACKBAR   Customizable column-width tracking control.
  //    NODRAGLIST   APIs to make a listbox source and sink drag&drop actions.
  //    NOPROGRESS   Progress gas gauge.
  //    NOHOTKEY     HotKey control
  //    NOHEADER     Header bar control.
  //    NOIMAGEAPIS  ImageList apis.
  //    NOLISTVIEW   ListView control.
  //    NOTREEVIEW   TreeView control.
  //    NOTABCONTROL Tab control.
  //    NOANIMATE    Animate control.
  //
  //=============================================================================



                  // size of this structure
                   // flags indicating which classes to be initialized

  #define ICC_LISTVIEW_CLASSES          1 // listview, header
  #define ICC_TREEVIEW_CLASSES          2 // treeview, tooltips
  #define ICC_BAR_CLASSES               4 // toolbar, statusbar, trackbar, tooltips
  #define ICC_TAB_CLASSES               8 // tab, tooltips
  #define ICC_UPDOWN_CLASS             16 // updown
  #define ICC_PROGRESS_CLASS           32 // progress
  #define ICC_HOTKEY_CLASS             64 // hotkey
  #define ICC_ANIMATE_CLASS           128 // animate
  #define ICC_WIN95_CLASSES           255
  #define ICC_DATE_CLASSES            256 // month picker, date picker, time picker, updown
  #define ICC_USEREX_CLASSES          512 // comboex
  #define ICC_COOL_CLASSES           1024 // rebar (coolbar) control
  #define ICC_INTERNET_CLASSES       2048
  #define ICC_PAGESCROLLER_CLASS       4096   // page scroller
  #define ICC_NATIVEFNTCTL_CLASS       8192   // native font control

  #define ODT_HEADER              100
  #define ODT_TAB                 101
  #define ODT_LISTVIEW            102


  //====== Ranges for control message IDs =======================================

  #define LVM_FIRST                 4096      // ListView messages
  #define TV_FIRST                  4352      // TreeView messages
  #define HDM_FIRST                 4608      // Header messages
  #define TCM_FIRST                 4864      // Tab control messages

  #define PGM_FIRST                 5120      // Pager control messages
  #define CCM_FIRST                 8192      // Common control shared messages
  #define CCM_LAST                (CCM_FIRST +   512)


  #define CCM_SETBKCOLOR          (CCM_FIRST + 1) // lParam is bkColor


           // highlight color
              // shadow color


  #define CCM_SETCOLORSCHEME      (CCM_FIRST + 2) // lParam is color scheme
  #define CCM_GETCOLORSCHEME      (CCM_FIRST + 3) // fills in COLORSCHEME pointed to by lParam
  #define CCM_GETDROPTARGET       (CCM_FIRST + 4)
  #define CCM_SETUNICODEFORMAT    (CCM_FIRST + 5)
  #define CCM_GETUNICODEFORMAT    (CCM_FIRST + 6)

  #define COMCTL32_VERSION  5
  #define CCM_SETVERSION          (CCM_FIRST +   7)
  #define CCM_GETVERSION          (CCM_FIRST +   8)
  #define CCM_SETNOTIFYWINDOW     (CCM_FIRST +   9) // wParam == hwndParent.

  // for tooltips
  #define INFOTIPSIZE 1024

  //====== WM_NOTIFY Macros =====================================================

  #define HANDLE_WM_NOTIFY(hwnd, wParam, lParam, fn) \

  #define FORWARD_WM_NOTIFY(hwnd, idFrom, pnmhdr, fn) \


  //====== Generic WM_NOTIFY notification codes =================================

  #define NM_OUTOFMEMORY          (NM_FIRST-1)
  #define NM_CLICK                (NM_FIRST-2)    // uses NMCLICK struct
  #define NM_DBLCLK               (NM_FIRST-3)
  #define NM_RETURN               (NM_FIRST-4)
  #define NM_RCLICK               (NM_FIRST-5)    // uses NMCLICK struct
  #define NM_RDBLCLK              (NM_FIRST-6)
  #define NM_SETFOCUS             (NM_FIRST-7)
  #define NM_KILLFOCUS            (NM_FIRST-8)
  #define NM_CUSTOMDRAW           (NM_FIRST-12)
  #define NM_HOVER                (NM_FIRST-13)
  #define NM_NCHITTEST            (NM_FIRST-14)   // uses NMMOUSE struct
  #define NM_KEYDOWN              (NM_FIRST-15)   // uses NMKEY struct
  #define NM_RELEASEDCAPTURE      (NM_FIRST-16)
  #define NM_SETCURSOR            (NM_FIRST-17)   // uses NMMOUSE struct
  #define NM_CHAR                 (NM_FIRST-18)   // uses NMCHAR struct
  #define NM_TOOLTIPSCREATED      (NM_FIRST-19)   // notify of when the tooltips window is create
  #define NM_LDOWN                (NM_FIRST-20)
  #define NM_RDOWN                (NM_FIRST-21)

  #ifndef CCSIZEOF_STRUCT
    #define CCSIZEOF_STRUCT(structname, member)  (((int)((LPBYTE)(&((structname*)0)->member) - ((LPBYTE)((structname*)0)))) + sizeof(((structname*)0)->member))
  #endif

  //====== Generic WM_NOTIFY notification structures ============================

  //====== WM_NOTIFY codes (NMHDR.code values) ==================================

  #define NM_FIRST                (0U-  0U)       // generic to all controls
  #define NM_LAST                 (0U- 99U)

  #define LVN_FIRST               (0U-100U)       // listview
  #define LVN_LAST                (0U-199U)

  // Property sheet reserved      (0U-200U) -  (0U-299U) - see prsht.h

  #define HDN_FIRST               (0U-300U)       // header
  #define HDN_LAST                (0U-399U)

  #define TVN_FIRST               (0U-400U)       // treeview
  #define TVN_LAST                (0U-499U)

  #define TTN_FIRST               (0U-520U)       // tooltips
  #define TTN_LAST                (0U-549U)

  #define TCN_FIRST               (0U-550U)       // tab control
  #define TCN_LAST                (0U-580U)

  // Shell reserved               (0U-580U) -  (0U-589U)

  #define CDN_FIRST               (0U-601U)       // common dialog (new)
  #define CDN_LAST                (0U-699U)

  #define TBN_FIRST               (0U-700U)       // toolbar
  #define TBN_LAST                (0U-720U)

  #define UDN_FIRST               (0U-721)        // updown
  #define UDN_LAST                (0U-740)
  #define MCN_FIRST               (0U-750U)       // monthcal
  #define MCN_LAST                (0U-759U)

  #define DTN_FIRST               (0U-760U)       // datetimepick
  #define DTN_LAST                (0U-799U)

  #define CBEN_FIRST              (0U-800U)       // combo box ex
  #define CBEN_LAST               (0U-830U)

  #define RBN_FIRST               (0U-831U)       // rebar
  #define RBN_LAST                (0U-859U)

  #define IPN_FIRST               (0U-860U)       // internet address
  #define IPN_LAST                (0U-879U)       // internet address

  #define SBN_FIRST               (0U-880U)       // status bar
  #define SBN_LAST                (0U-899U)

  #define PGN_FIRST               (0U-900U)       // Pager Control
  #define PGN_LAST                (0U-950U)


  #ifndef WMN_FIRST
    #define WMN_FIRST               (0U-1000U)
    #define WMN_LAST                (0U-1200U)
  #endif

  #define MSGF_COMMCTRL_BEGINDRAG      16896
  #define MSGF_COMMCTRL_SIZEHEADER     16897
  #define MSGF_COMMCTRL_DRAGSELECT     16898
  #define MSGF_COMMCTRL_TOOLBARCUST    16899

  //==================== CUSTOM DRAW ==========================================


  // custom draw return flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  #define CDRF_DODEFAULT          0x00000000
  #define CDRF_NEWFONT            0x00000002
  #define CDRF_SKIPDEFAULT        0x00000004


  #define CDRF_NOTIFYPOSTPAINT            16
  #define CDRF_NOTIFYITEMDRAW             32
  #define CDRF_NOTIFYSUBITEMDRAW          32  // flags are the same, we can distinguish by context
  #define CDRF_NOTIFYPOSTERASE            64

  // drawstage flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  #define CDDS_PREPAINT                    1
  #define CDDS_POSTPAINT                   2
  #define CDDS_PREERASE                    3
  #define CDDS_POSTERASE                   4
  // the 0x000010000 bit means it's individual item specific
  #define CDDS_ITEM                    65536
  #define CDDS_ITEMPREPAINT       (CDDS_ITEM + CDDS_PREPAINT)
  #define CDDS_ITEMPOSTPAINT      (CDDS_ITEM + CDDS_POSTPAINT)
  #define CDDS_ITEMPREERASE       (CDDS_ITEM + CDDS_PREERASE)
  #define CDDS_ITEMPOSTERASE      (CDDS_ITEM + CDDS_POSTERASE)
  #define CDDS_SUBITEM                131072


  // itemState flags
  #define CDIS_SELECTED            1
  #define CDIS_GRAYED              2
  #define CDIS_DISABLED            4
  #define CDIS_CHECKED             8
  #define CDIS_FOCUS              16
  #define CDIS_DEFAULT            32
  #define CDIS_HOT                64
  #define CDIS_MARKED            128
  #define CDIS_INDETERMINATE     256


       // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set




  //====== IMAGE APIS ===========================================================

  #ifndef NOIMAGEAPIS

    #define CLR_NONE                 4294967295
    #define CLR_DEFAULT              4278190080



    #define ILC_MASK                     1
    #define ILC_COLOR                    0
    #define ILC_COLORDDB               254
    #define ILC_COLOR4                   4
    #define ILC_COLOR8                   8
    #define ILC_COLOR16                 16
    #define ILC_COLOR24                 24
    #define ILC_COLOR32                 32
    #define ILC_PALETTE               2048      // (not implemented)



    //#define     ImageList_AddIcon(himl, hicon) ImageList_ReplaceIcon(himl, -1, hicon)

    #define ILD_NORMAL                   0
    #define ILD_TRANSPARENT              1
    #define ILD_MASK                    16
    #define ILD_IMAGE                   32
    #define ILD_ROP                     64
    #define ILD_BLEND25                  2
    #define ILD_BLEND50                  4
    #define ILD_OVERLAYMASK           3840
    #define INDEXTOOVERLAYMASK(i)   ((i) << 8)

    #define ILD_SELECTED            ILD_BLEND50
    #define ILD_FOCUS               ILD_BLEND25
    #define ILD_BLEND               ILD_BLEND50
    #define CLR_HILIGHT             CLR_DEFAULT



    #ifdef UNICODE
      #define ImageList_LoadImage     ImageList_LoadImageW
    #else
      #define ImageList_LoadImage     ImageList_LoadImageA
    #endif

    #define ILCF_MOVE   (         0)
    #define ILCF_SWAP   (         1)


    #define     ImageList_RemoveAll(himl) ImageList_Remove(himl, -1)
    #define     ImageList_ExtractIcon(hi, himl, i) ImageList_GetIcon(himl, i, 0)
    #define     ImageList_LoadBitmap(hi, lpbmp, cx, cGrow, crMask) ImageList_LoadImage(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, 0)
  #endif

  //====== HEADER CONTROL =======================================================

  #ifndef NOHEADER

    #define WC_HEADERA              "SysHeader32"
    #define WC_HEADERW              L"SysHeader32"

    #ifdef UNICODE
      #define WC_HEADER               WC_HEADERW
    #else
      #define WC_HEADER               WC_HEADERA
    #endif

    // begin_r_commctrl

    #define HDS_HORZ                     0
    #define HDS_BUTTONS                  2
    #define HDS_HOTTRACK                 4
    #define HDS_HIDDEN                   8

    #define HDS_DRAGDROP                64
    #define HDS_FULLDRAG               128
    #define HDS_FILTERBAR              256

    // end_r_commctrl


    #define HDFT_ISSTRING            0      // HD_ITEM.pvFilter points to a HD_TEXTFILTER
    #define HDFT_ISNUMBER            1      // HD_ITEM.pvFilter points to a INT

    #define HDFT_HASNOVALUE      32768      // clear the filter, by setting this bit

    #ifdef UNICODE
      #define HD_TEXTFILTER HD_TEXTFILTERW
      #define HDTEXTFILTER HD_TEXTFILTERW
      #define LPHD_TEXTFILTER LPHD_TEXTFILTERW
      #define LPHDTEXTFILTER LPHD_TEXTFILTERW
    #else
      #define HD_TEXTFILTER HD_TEXTFILTERA
      #define HDTEXTFILTER HD_TEXTFILTERA
      #define LPHD_TEXTFILTER LPHD_TEXTFILTERA
      #define LPHDTEXTFILTER LPHD_TEXTFILTERA
    #endif



    //#define HDITEMW  HD_ITEMW
    //#define HDITEMA  HD_ITEMA
    //#define HD_ITEM HDITEM


    #define HDITEMA_V1_SIZE CCSIZEOF_STRUCT(HDITEMA, lParam)
    #define HDITEMW_V1_SIZE CCSIZEOF_STRUCT(HDITEMW, lParam)


    //#ifdef UNICODE
    //#define HDITEM HDITEMW
    //#define LPHDITEM LPHDITEMW
    //#define HDITEM_V1_SIZE HDITEMW_V1_SIZE
    //#else
    //#define HDITEM HDITEMA
    //#define LPHDITEM LPHDITEMA
    //#define HDITEM_V1_SIZE HDITEMA_V1_SIZE
    //#endif


    #define HDI_WIDTH                    1
    #define HDI_HEIGHT              HDI_WIDTH
    #define HDI_TEXT                     2
    #define HDI_FORMAT                   4
    #define HDI_LPARAM                   8
    #define HDI_BITMAP                  16
    #define HDI_IMAGE                   32
    #define HDI_DI_SETITEM              64
    #define HDI_ORDER                  128
    #define HDI_FILTER                 256

    #define HDF_LEFT                0
    #define HDF_RIGHT               1
    #define HDF_CENTER              2
    #define HDF_JUSTIFYMASK              3
    #define HDF_RTLREADING          4

    #define HDF_OWNERDRAW            32768
    #define HDF_STRING               16384
    #define HDF_BITMAP                8192
    #define HDF_BITMAP_ON_RIGHT       4096
    #define HDF_IMAGE                 2048

    #define HDM_GETITEMCOUNT        (HDM_FIRST + 0)
    //#define Header_GetItemCount(hwndHD) \


    #define HDM_INSERTITEMA         (HDM_FIRST + 1)
    #define HDM_INSERTITEMW         (HDM_FIRST + 10)

    #ifdef UNICODE
      #define HDM_INSERTITEM          HDM_INSERTITEMW
    #else
      #define HDM_INSERTITEM          HDM_INSERTITEMA
    #endif

    //#define Header_InsertItem(hwndHD, i, phdi) \


    #define HDM_DELETEITEM          (HDM_FIRST + 2)
    //#define Header_DeleteItem(hwndHD, i) \


    #define HDM_GETITEMA            (HDM_FIRST + 3)
    #define HDM_GETITEMW            (HDM_FIRST + 11)

    #ifdef UNICODE
      #define HDM_GETITEM             HDM_GETITEMW
    #else
      #define HDM_GETITEM             HDM_GETITEMA
    #endif

    //#define Header_GetItem(hwndHD, i, phdi) \


    #define HDM_SETITEMA            (HDM_FIRST + 4)
    #define HDM_SETITEMW            (HDM_FIRST + 12)

    #ifdef UNICODE
      #define HDM_SETITEM             HDM_SETITEMW
    #else
      #define HDM_SETITEM             HDM_SETITEMA
    #endif

    //#define Header_SetItem(hwndHD, i, phdi) \


    #define HD_LAYOUT  HDLAYOUT


    #define HDM_LAYOUT              (HDM_FIRST + 5)
    //#define Header_Layout(hwndHD, playout) \


    #define HHT_NOWHERE                  1
    #define HHT_ONHEADER                 2
    #define HHT_ONDIVIDER                4
    #define HHT_ONDIVOPEN                8
    #define HHT_ONFILTER                16
    #define HHT_ONFILTERBUTTON          32
    #define HHT_ABOVE                  256
    #define HHT_BELOW                  512
    #define HHT_TORIGHT               1024
    #define HHT_TOLEFT                2048

    #define HD_HITTESTINFO HDHITTESTINFO


    #define HDM_HITTEST             (HDM_FIRST + 6)


    #define HDM_GETITEMRECT         (HDM_FIRST + 7)
    //#define Header_GetItemRect(hwnd, iItem, lprc) \


    #define HDM_SETIMAGELIST        (HDM_FIRST + 8)
    ////#define Header_SetImageList(hwnd, himl) \


    #define HDM_GETIMAGELIST        (HDM_FIRST + 9)
    //#define Header_GetImageList(hwnd) \


    #define HDM_ORDERTOINDEX        (HDM_FIRST + 15)
    //#define Header_OrderToIndex(hwnd, i) \


    #define HDM_CREATEDRAGIMAGE     (HDM_FIRST + 16)  // wparam = which item (by index)
    //#define Header_CreateDragImage(hwnd, i) \


    #define HDM_GETORDERARRAY       (HDM_FIRST + 17)
    //#define Header_GetOrderArray(hwnd, iCount, lpi) \


    #define HDM_SETORDERARRAY       (HDM_FIRST + 18)
    //#define Header_SetOrderArray(hwnd, iCount, lpi) \

    // lparam = int array of size HDM_GETITEMCOUNT
    // the array specifies the order that all items should be displayed.
    // e.g.  { 2, 0, 1}
    // says the index 2 item should be shown in the 0ths position
    //      index 0 should be shown in the 1st position
    //      index 1 should be shown in the 2nd position


    #define HDM_SETHOTDIVIDER          (HDM_FIRST + 19)
    //#define Header_SetHotDivider(hwnd, fPos, dw) \

    // convenience message for external dragdrop
    // wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
    //              position or the index of which divider to hotlight
    // lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)


    #define HDM_SETBITMAPMARGIN          (HDM_FIRST + 20)
    //#define Header_SetBitmapMargin(hwnd, iWidth) \


    #define HDM_GETBITMAPMARGIN          (HDM_FIRST + 21)
    //#define Header_GetBitmapMargin(hwnd) \



    #define HDM_SETUNICODEFORMAT   CCM_SETUNICODEFORMAT
    //#define Header_SetUnicodeFormat(hwnd, fUnicode)  \


    #define HDM_GETUNICODEFORMAT   CCM_GETUNICODEFORMAT
    //#define Header_GetUnicodeFormat(hwnd)  \


    #define HDM_SETFILTERCHANGETIMEOUT  (HDM_FIRST+22)
    //#define Header_SetFilterChangeTimeout(hwnd, i) \


    #define HDM_EDITFILTER          (HDM_FIRST+23)
    //#define Header_EditFilter(hwnd, i, fDiscardChanges) \


    // Clear filter takes -1 as a column value to indicate that all
    // the filter should be cleared.  When this happens you will
    // only receive a single filter changed notification.

    #define HDM_CLEARFILTER         (HDM_FIRST+24)
    //#define Header_ClearFilter(hwnd, i) \

    //#define Header_ClearAllFilters(hwnd) \


    #define HDN_ITEMCHANGINGA           (HDN_FIRST-0)
    #define HDN_ITEMCHANGINGW       (HDN_FIRST-20)
    #define HDN_ITEMCHANGEDA        (HDN_FIRST-1)
    #define HDN_ITEMCHANGEDW        (HDN_FIRST-21)
    #define HDN_ITEMCLICKA          (HDN_FIRST-2)
    #define HDN_ITEMCLICKW          (HDN_FIRST-22)
    #define HDN_ITEMDBLCLICKA       (HDN_FIRST-3)
    #define HDN_ITEMDBLCLICKW       (HDN_FIRST-23)
    #define HDN_DIVIDERDBLCLICKA    (HDN_FIRST-5)
    #define HDN_DIVIDERDBLCLICKW    (HDN_FIRST-25)
    #define HDN_BEGINTRACKA         (HDN_FIRST-6)
    #define HDN_BEGINTRACKW         (HDN_FIRST-26)
    #define HDN_ENDTRACKA           (HDN_FIRST-7)
    #define HDN_ENDTRACKW           (HDN_FIRST-27)
    #define HDN_TRACKA              (HDN_FIRST-8)
    #define HDN_TRACKW              (HDN_FIRST-28)
    #define HDN_GETDISPINFOA        (HDN_FIRST-9)
    #define HDN_GETDISPINFOW        (HDN_FIRST-29)
    #define HDN_BEGINDRAG           (HDN_FIRST-10)
    #define HDN_ENDDRAG             (HDN_FIRST-11)
    #define HDN_FILTERCHANGE        (HDN_FIRST-12)
    #define HDN_FILTERBTNCLICK      (HDN_FIRST-13)

    #ifdef UNICODE
      #define HDN_ITEMCHANGING         HDN_ITEMCHANGINGW
      #define HDN_ITEMCHANGED          HDN_ITEMCHANGEDW
      #define HDN_ITEMCLICK            HDN_ITEMCLICKW
      #define HDN_ITEMDBLCLICK         HDN_ITEMDBLCLICKW
      #define HDN_DIVIDERDBLCLICK      HDN_DIVIDERDBLCLICKW
      #define HDN_BEGINTRACK           HDN_BEGINTRACKW
      #define HDN_ENDTRACK             HDN_ENDTRACKW
      #define HDN_TRACK                HDN_TRACKW
      #define HDN_GETDISPINFO          HDN_GETDISPINFOW
    #else
      #define HDN_ITEMCHANGING         HDN_ITEMCHANGINGA
      #define HDN_ITEMCHANGED          HDN_ITEMCHANGEDA
      #define HDN_ITEMCLICK            HDN_ITEMCLICKA
      #define HDN_ITEMDBLCLICK         HDN_ITEMDBLCLICKA
      #define HDN_DIVIDERDBLCLICK      HDN_DIVIDERDBLCLICKA
      #define HDN_BEGINTRACK           HDN_BEGINTRACKA
      #define HDN_ENDTRACK             HDN_ENDTRACKA
      #define HDN_TRACK                HDN_TRACKA
      #define HDN_GETDISPINFO          HDN_GETDISPINFOA
    #endif


    //#define HD_NOTIFYA              NMHEADERA
    //#define HD_NOTIFYW              NMHEADERW
    #define HD_NOTIFY               NMHEADER


    //#ifdef UNICODE
    //#define NMHEADER                NMHEADERW
    //#define LPNMHEADER              LPNMHEADERW
    //#else
    //#define NMHEADER                NMHEADERA
    //#define LPNMHEADER              LPNMHEADERA
    //#endif


    #ifdef UNICODE
      #define NMHDDISPINFO            NMHDDISPINFOW
      #define LPNMHDDISPINFO          LPNMHDDISPINFOW
    #else
      #define NMHDDISPINFO            NMHDDISPINFOA
      #define LPNMHDDISPINFO          LPNMHDDISPINFOA
    #endif

  #endif      // NOHEADER


  //====== TOOLBAR CONTROL ======================================================

  #ifndef NOTOOLBAR

    #ifdef _WIN32
      #define TOOLBARCLASSNAMEW       L"ToolbarWindow32"
      #define TOOLBARCLASSNAMEA       "ToolbarWindow32"

      #ifdef  UNICODE
        #define TOOLBARCLASSNAME        TOOLBARCLASSNAMEW
      #else
        #define TOOLBARCLASSNAME        TOOLBARCLASSNAMEA
      #endif

    #else
      #define TOOLBARCLASSNAME        "ToolbarWindow"
    #endif



    #define CMB_MASKED                 2
    #define TBSTATE_CHECKED            1
    #define TBSTATE_PRESSED            2
    #define TBSTATE_ENABLED            4
    #define TBSTATE_HIDDEN             8
    #define TBSTATE_INDETERMINATE     16
    #define TBSTATE_WRAP              32
    #define TBSTATE_ELLIPSES          64
    #define TBSTATE_MARKED           128

    #define TBSTYLE_BUTTON               0  // obsolete; use BTNS_BUTTON instead
    #define TBSTYLE_SEP                  1  // obsolete; use BTNS_SEP instead
    #define TBSTYLE_CHECK                2  // obsolete; use BTNS_CHECK instead
    #define TBSTYLE_GROUP                4  // obsolete; use BTNS_GROUP instead
    #define TBSTYLE_CHECKGROUP      (TBSTYLE_GROUP + TBSTYLE_CHECK)     // obsolete; use BTNS_CHECKGROUP instead
    #define TBSTYLE_DROPDOWN             8  // obsolete; use BTNS_DROPDOWN instead
    #define TBSTYLE_AUTOSIZE            16  // obsolete; use BTNS_AUTOSIZE instead
    #define TBSTYLE_NOPREFIX            32  // obsolete; use BTNS_NOPREFIX instead

    #define TBSTYLE_TOOLTIPS           256
    #define TBSTYLE_WRAPABLE           512
    #define TBSTYLE_ALTDRAG           1024
    #define TBSTYLE_FLAT              2048
    #define TBSTYLE_LIST              4096
    #define TBSTYLE_CUSTOMERASE       8192
    #define TBSTYLE_REGISTERDROP     16384
    #define TBSTYLE_TRANSPARENT      32768
    #define TBSTYLE_EX_DRAWDDARROWS          1

    #define BTNS_BUTTON     TBSTYLE_BUTTON      // 0x0000
    #define BTNS_SEP        TBSTYLE_SEP         // 0x0001
    #define BTNS_CHECK      TBSTYLE_CHECK       // 0x0002
    #define BTNS_GROUP      TBSTYLE_GROUP       // 0x0004
    #define BTNS_CHECKGROUP TBSTYLE_CHECKGROUP  // (TBSTYLE_GROUP | TBSTYLE_CHECK)
    #define BTNS_DROPDOWN   TBSTYLE_DROPDOWN    // 0x0008
    #define BTNS_AUTOSIZE   TBSTYLE_AUTOSIZE    // 0x0010; automatically calculate the cx of the button
    #define BTNS_NOPREFIX   TBSTYLE_NOPREFIX    // 0x0020; this button should not have accel prefix
    #define BTNS_SHOWTEXT       64              // ignored unless TBSTYLE_EX_MIXEDBUTTONS is set
    #define BTNS_WHOLEDROPDOWN     128          // draw drop-down arrow, but without split arrow section

    #define TBSTYLE_EX_MIXEDBUTTONS              8
    #define TBSTYLE_EX_HIDECLIPPEDBUTTONS           16  // don't show partially obscured buttons


    // Custom Draw Structure


                       // For drawing lines on buttons
                        // For drawing lines on buttons

                      // Color of text
                      // Color of text bk when marked. (only if TBSTATE_MARKED)
             // Color of text when highlighted
                   // Background of the button
              // 3D highlight
         // In conjunction with fHighlightHotTrack
                                       // will cause button to highlight like a menu
                           // Rect for text


    // Toolbar custom draw return flags
    #define TBCDRF_NOEDGES                   65536  // Don't draw button edges
    #define TBCDRF_HILITEHOTTRACK           131072  // Use color of the button bk when hottracked
    #define TBCDRF_NOOFFSET                 262144  // Don't offset button if pressed
    #define TBCDRF_NOMARK                   524288  // Don't draw default highlight of image/text for TBSTATE_MARKED
    #define TBCDRF_NOETCHEDEFFECT          1048576  // Don't draw etched effect for disabled items

    #define TBCDRF_BLENDICON               2097152  // Use ILD_BLEND50 on the icon image


    #define TB_ENABLEBUTTON         (WM_USER + 1)
    #define TB_CHECKBUTTON          (WM_USER + 2)
    #define TB_PRESSBUTTON          (WM_USER + 3)
    #define TB_HIDEBUTTON           (WM_USER + 4)
    #define TB_INDETERMINATE        (WM_USER + 5)
    #define TB_MARKBUTTON           (WM_USER + 6)
    #define TB_ISBUTTONENABLED      (WM_USER + 9)
    #define TB_ISBUTTONCHECKED      (WM_USER + 10)
    #define TB_ISBUTTONPRESSED      (WM_USER + 11)
    #define TB_ISBUTTONHIDDEN       (WM_USER + 12)
    #define TB_ISBUTTONINDETERMINATE (WM_USER + 13)
    #define TB_ISBUTTONHIGHLIGHTED  (WM_USER + 14)
    #define TB_SETSTATE             (WM_USER + 17)
    #define TB_GETSTATE             (WM_USER + 18)
    #define TB_ADDBITMAP            (WM_USER + 19)


    #define HINST_COMMCTRL          ((HINSTANCE)-1)
    #define IDB_STD_SMALL_COLOR     0
    #define IDB_STD_LARGE_COLOR     1
    #define IDB_VIEW_SMALL_COLOR    4
    #define IDB_VIEW_LARGE_COLOR    5
    #define IDB_HIST_SMALL_COLOR    8
    #define IDB_HIST_LARGE_COLOR    9

    // icon indexes for standard bitmap

    #define STD_CUT                 0
    #define STD_COPY                1
    #define STD_PASTE               2
    #define STD_UNDO                3
    #define STD_REDOW               4
    #define STD_DELETE              5
    #define STD_FILENEW             6
    #define STD_FILEOPEN            7
    #define STD_FILESAVE            8
    #define STD_PRINTPRE            9
    #define STD_PROPERTIES          10
    #define STD_HELP                11
    #define STD_FIND                12
    #define STD_REPLACE             13
    #define STD_PRINT               14

    // icon indexes for standard view bitmap

    #define VIEW_LARGEICONS         0
    #define VIEW_SMALLICONS         1
    #define VIEW_LIST               2
    #define VIEW_DETAILS            3
    #define VIEW_SORTNAME           4
    #define VIEW_SORTSIZE           5
    #define VIEW_SORTDATE           6
    #define VIEW_SORTTYPE           7
    #define VIEW_PARENTFOLDER       8
    #define VIEW_NETCONNECT         9
    #define VIEW_NETDISCONNECT      10
    #define VIEW_NEWFOLDER          11
    #define VIEW_VIEWMENU           12

    #define HIST_BACK               0
    #define HIST_FORWARD            1
    #define HIST_FAVORITES          2
    #define HIST_ADDTOFAVORITES     3
    #define HIST_VIEWTREE           4

    #define TB_ADDBUTTONSA          (WM_USER + 20)
    #define TB_INSERTBUTTONA        (WM_USER + 21)

    #define TB_DELETEBUTTON         (WM_USER + 22)
    #define TB_GETBUTTON            (WM_USER + 23)
    #define TB_BUTTONCOUNT          (WM_USER + 24)
    #define TB_COMMANDTOINDEX       (WM_USER + 25)



    #ifdef UNICODE
      #define TBSAVEPARAMS            TBSAVEPARAMSW
      #define LPTBSAVEPARAMS          LPTBSAVEPARAMSW
    #else
      #define TBSAVEPARAMS            TBSAVEPARAMSA
      #define LPTBSAVEPARAMS          LPTBSAVEPARAMSA
    #endif


    #define TB_SAVERESTOREA         (WM_USER + 26)
    #define TB_SAVERESTOREW         (WM_USER + 76)
    #define TB_CUSTOMIZE            (WM_USER + 27)
    #define TB_ADDSTRINGA           (WM_USER + 28)
    #define TB_ADDSTRINGW           (WM_USER + 77)
    #define TB_GETITEMRECT          (WM_USER + 29)
    #define TB_BUTTONSTRUCTSIZE     (WM_USER + 30)
    #define TB_SETBUTTONSIZE        (WM_USER + 31)
    #define TB_SETBITMAPSIZE        (WM_USER + 32)
    #define TB_AUTOSIZE             (WM_USER + 33)
    #define TB_GETTOOLTIPS          (WM_USER + 35)
    #define TB_SETTOOLTIPS          (WM_USER + 36)
    #define TB_SETPARENT            (WM_USER + 37)
    #define TB_SETROWS              (WM_USER + 39)
    #define TB_GETROWS              (WM_USER + 40)
    #define TB_SETCMDID             (WM_USER + 42)
    #define TB_CHANGEBITMAP         (WM_USER + 43)
    #define TB_GETBITMAP            (WM_USER + 44)
    #define TB_GETBUTTONTEXTA       (WM_USER + 45)
    #define TB_GETBUTTONTEXTW       (WM_USER + 75)
    #define TB_REPLACEBITMAP        (WM_USER + 46)
    #define TB_SETINDENT            (WM_USER + 47)
    #define TB_SETIMAGELIST         (WM_USER + 48)
    #define TB_GETIMAGELIST         (WM_USER + 49)
    #define TB_LOADIMAGES           (WM_USER + 50)
    #define TB_GETRECT              (WM_USER + 51) // wParam is the Cmd instead of index
    #define TB_SETHOTIMAGELIST      (WM_USER + 52)
    #define TB_GETHOTIMAGELIST      (WM_USER + 53)
    #define TB_SETDISABLEDIMAGELIST (WM_USER + 54)
    #define TB_GETDISABLEDIMAGELIST (WM_USER + 55)
    #define TB_SETSTYLE             (WM_USER + 56)
    #define TB_GETSTYLE             (WM_USER + 57)
    #define TB_GETBUTTONSIZE        (WM_USER + 58)
    #define TB_SETBUTTONWIDTH       (WM_USER + 59)
    #define TB_SETMAXTEXTROWS       (WM_USER + 60)
    #define TB_GETTEXTROWS          (WM_USER + 61)

    #ifdef UNICODE
      #define TB_GETBUTTONTEXT        TB_GETBUTTONTEXTW
      #define TB_SAVERESTORE          TB_SAVERESTOREW
      #define TB_ADDSTRING            TB_ADDSTRINGW
    #else
      #define TB_GETBUTTONTEXT        TB_GETBUTTONTEXTA
      #define TB_SAVERESTORE          TB_SAVERESTOREA
      #define TB_ADDSTRING            TB_ADDSTRINGA
    #endif

    #define TB_GETOBJECT            (WM_USER + 62)  // wParam == IID, lParam void **ppv
    #define TB_GETHOTITEM           (WM_USER + 71)
    #define TB_SETHOTITEM           (WM_USER + 72)  // wParam == iHotItem
    #define TB_SETANCHORHIGHLIGHT   (WM_USER + 73)  // wParam == TRUE/FALSE
    #define TB_GETANCHORHIGHLIGHT   (WM_USER + 74)
    #define TB_MAPACCELERATORA      (WM_USER + 78)  // wParam == ch, lParam int * pidBtn


    #define TBIMHT_AFTER               1 // TRUE = insert After iButton, otherwise before
    #define TBIMHT_BACKGROUND          2 // TRUE iff missed buttons completely

    #define TB_GETINSERTMARK        (WM_USER + 79)  // lParam == LPTBINSERTMARK
    #define TB_SETINSERTMARK        (WM_USER + 80)  // lParam == LPTBINSERTMARK
    #define TB_INSERTMARKHITTEST    (WM_USER + 81)  // wParam == LPPOINT lParam == LPTBINSERTMARK
    #define TB_MOVEBUTTON           (WM_USER + 82)
    #define TB_GETMAXSIZE           (WM_USER + 83)  // lParam == LPSIZE
    #define TB_SETEXTENDEDSTYLE     (WM_USER + 84)  // For TBSTYLE_EX_*
    #define TB_GETEXTENDEDSTYLE     (WM_USER + 85)  // For TBSTYLE_EX_*
    #define TB_GETPADDING           (WM_USER + 86)
    #define TB_SETPADDING           (WM_USER + 87)
    #define TB_SETINSERTMARKCOLOR   (WM_USER + 88)
    #define TB_GETINSERTMARKCOLOR   (WM_USER + 89)

    #define TB_SETCOLORSCHEME       CCM_SETCOLORSCHEME  // lParam is color scheme
    #define TB_GETCOLORSCHEME       CCM_GETCOLORSCHEME      // fills in COLORSCHEME pointed to by lParam

    #define TB_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    #define TB_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT

    #define TB_MAPACCELERATORW      (WM_USER + 90)  // wParam == ch, lParam int * pidBtn
    #ifdef UNICODE
      #define TB_MAPACCELERATOR       TB_MAPACCELERATORW
    #else
      #define TB_MAPACCELERATOR       TB_MAPACCELERATORA
    #endif

    #define TBBF_LARGE                   1

    #define TB_GETBITMAPFLAGS       (WM_USER + 41)

    #define TBIF_IMAGE                       1
    #define TBIF_TEXT                        2
    #define TBIF_STATE                       4
    #define TBIF_STYLE                       8
    #define TBIF_LPARAM                     16
    #define TBIF_COMMAND                    32
    #define TBIF_SIZE                       64

    #define TBIF_BYINDEX            2147483648 // this specifies that the wparam in Get/SetButtonInfo is an index, not id


    #ifdef UNICODE
      #define TBBUTTONINFO TBBUTTONINFOW
      #define LPTBBUTTONINFO LPTBBUTTONINFOW
    #else
      #define TBBUTTONINFO TBBUTTONINFOA
      #define LPTBBUTTONINFO LPTBBUTTONINFOA
    #endif


    // BUTTONINFO APIs do NOT support the string pool.
    #define TB_GETBUTTONINFOW        (WM_USER + 63)
    #define TB_SETBUTTONINFOW        (WM_USER + 64)
    #define TB_GETBUTTONINFOA        (WM_USER + 65)
    #define TB_SETBUTTONINFOA        (WM_USER + 66)
    #ifdef UNICODE
      #define TB_GETBUTTONINFO        TB_GETBUTTONINFOW
      #define TB_SETBUTTONINFO        TB_SETBUTTONINFOW
    #else
      #define TB_GETBUTTONINFO        TB_GETBUTTONINFOA
      #define TB_SETBUTTONINFO        TB_SETBUTTONINFOA
    #endif


    #define TB_INSERTBUTTONW        (WM_USER + 67)
    #define TB_ADDBUTTONSW          (WM_USER + 68)

    #define TB_HITTEST              (WM_USER + 69)

    // New post Win95/NT4 for InsertButton and AddButton.  if iString member
    // is a pointer to a string, it will be handled as a string like listview
    // (although LPSTR_TEXTCALLBACK is not supported).
    #ifdef UNICODE
      #define TB_INSERTBUTTON         TB_INSERTBUTTONW
      #define TB_ADDBUTTONS           TB_ADDBUTTONSW
    #else
      #define TB_INSERTBUTTON         TB_INSERTBUTTONA
      #define TB_ADDBUTTONS           TB_ADDBUTTONSA
    #endif

    #define TB_SETDRAWTEXTFLAGS     (WM_USER + 70)  // wParam == mask lParam == bit values

    #define TB_GETSTRINGW           (WM_USER + 91)
    #define TB_GETSTRINGA           (WM_USER + 92)
    #ifdef UNICODE
      #define TB_GETSTRING            TB_GETSTRINGW
    #else
      #define TB_GETSTRING            TB_GETSTRINGA
    #endif




    #define TBN_GETBUTTONINFOA      (TBN_FIRST-0)
    #define TBN_BEGINDRAG           (TBN_FIRST-1)
    #define TBN_ENDDRAG             (TBN_FIRST-2)
    #define TBN_BEGINADJUST         (TBN_FIRST-3)
    #define TBN_ENDADJUST           (TBN_FIRST-4)
    #define TBN_RESET               (TBN_FIRST-5)
    #define TBN_QUERYINSERT         (TBN_FIRST-6)
    #define TBN_QUERYDELETE         (TBN_FIRST-7)
    #define TBN_TOOLBARCHANGE       (TBN_FIRST-8)
    #define TBN_CUSTHELP            (TBN_FIRST-9)
    #define TBN_DROPDOWN            (TBN_FIRST - 10)
    #define TBN_GETOBJECT           (TBN_FIRST - 12)

    // Structure for TBN_HOTITEMCHANGE notification
    //


                  // HICF_*


    // Hot item change flags
    #define HICF_OTHER                   0
    #define HICF_MOUSE                   1          // Triggered by mouse
    #define HICF_ARROWKEYS               2          // Triggered by arrow keys
    #define HICF_ACCELERATOR             4          // Triggered by accelerator
    #define HICF_DUPACCEL                8          // This accelerator is not unique
    #define HICF_ENTERING               16          // idOld is invalid
    #define HICF_LEAVING                32          // idNew is invalid
    #define HICF_RESELECT               64          // hot item reselected
    #define HICF_LMOUSE                128          // left mouse button selected
    #define HICF_TOGGLEDROPDOWN        256          // Toggle button's dropdown state


    #define TBN_HOTITEMCHANGE       (TBN_FIRST - 13)
    #define TBN_DRAGOUT             (TBN_FIRST - 14) // this is sent when the user clicks down on a button then drags off the button
    #define TBN_DELETINGBUTTON      (TBN_FIRST - 15) // uses TBNOTIFY
    #define TBN_GETDISPINFOA        (TBN_FIRST - 16) // This is sent when the  toolbar needs  some display information
    #define TBN_GETDISPINFOW        (TBN_FIRST - 17) // This is sent when the  toolbar needs  some display information
    #define TBN_GETINFOTIPA         (TBN_FIRST - 18)
    #define TBN_GETINFOTIPW         (TBN_FIRST - 19)
    #define TBN_GETBUTTONINFOW      (TBN_FIRST - 20)
    #define TBN_RESTORE             (TBN_FIRST - 21)
    #define TBN_SAVE                (TBN_FIRST - 22)
    #define TBN_INITCUSTOMIZE       (TBN_FIRST - 23)
    #define    TBNRF_HIDEHELP                1
    #define    TBNRF_ENDCUSTOMIZE            2



    #ifdef UNICODE
      #define TBN_GETINFOTIP          TBN_GETINFOTIPW
      #define NMTBGETINFOTIP          NMTBGETINFOTIPW
      #define LPNMTBGETINFOTIP        LPNMTBGETINFOTIPW
    #else
      #define TBN_GETINFOTIP          TBN_GETINFOTIPA
      #define NMTBGETINFOTIP          NMTBGETINFOTIPA
      #define LPNMTBGETINFOTIP        LPNMTBGETINFOTIPA
    #endif

    #define TBNF_IMAGE                       1
    #define TBNF_TEXT                        2
    #define TBNF_DI_SETITEM          268435456


            // [in] Specifies the values requested .[out] Client ask the data to be set for future use
           // [in] id of button we're requesting info for
         // [in] lParam of button
              // [out] image index
           // [out] new text for item
             // [in] size of buffer pointed to by pszText


             //[in] Specifies the values requested .[out] Client ask the data to be set for future use
           // [in] id of button we're requesting info for
         // [in] lParam of button
              // [out] image index
          // [out] new text for item
             // [in] size of buffer pointed to by pszText


    #ifdef UNICODE
      #define TBN_GETDISPINFO       TBN_GETDISPINFOW
      #define NMTBDISPINFO          NMTBDISPINFOW
      #define LPNMTBDISPINFO        LPNMTBDISPINFOW
    #else
      #define TBN_GETDISPINFO       TBN_GETDISPINFOA
      #define NMTBDISPINFO          NMTBDISPINFOA
      #define LPNMTBDISPINFO        LPNMTBDISPINFOA
    #endif

    // Return codes for TBN_DROPDOWN
    #define TBDDRET_DEFAULT         0
    #define TBDDRET_NODEFAULT       1
    #define TBDDRET_TREATPRESSED    2       // Treat as a standard press button



    #ifdef UNICODE
      #define TBN_GETBUTTONINFO       TBN_GETBUTTONINFOW
    #else
      #define TBN_GETBUTTONINFO       TBN_GETBUTTONINFOA
    #endif

    #define TBNOTIFYA NMTOOLBARA
    #define TBNOTIFYW NMTOOLBARW
    #define LPTBNOTIFYA LPNMTOOLBARA
    #define LPTBNOTIFYW LPNMTOOLBARW

    #define TBNOTIFY       NMTOOLBAR
    #define LPTBNOTIFY     LPNMTOOLBAR


    #ifdef UNICODE
      #define NMTOOLBAR               NMTOOLBARW
      #define LPNMTOOLBAR             LPNMTOOLBARW
    #else
      #define NMTOOLBAR               NMTOOLBARA
      #define LPNMTOOLBAR             LPNMTOOLBARA
    #endif

  #endif      // NOTOOLBAR


  //====== REBAR CONTROL ========================================================

  #ifndef NOREBAR

    #define REBARCLASSNAMEW         L"ReBarWindow32"
    #define REBARCLASSNAMEA         "ReBarWindow32"

    #ifdef  UNICODE
      #define REBARCLASSNAME          REBARCLASSNAMEW
    #else
      #define REBARCLASSNAME          REBARCLASSNAMEA
    #endif


    #define RBIM_IMAGELIST           1

    // begin_r_commctrl

    #define RBS_TOOLTIPS           256
    #define RBS_VARHEIGHT          512
    #define RBS_BANDBORDERS       1024
    #define RBS_FIXEDORDER        2048
    #define RBS_REGISTERDROP      4096
    #define RBS_AUTOSIZE          8192
    #define RBS_VERTICALGRIPPER  16384  // this always has the vertical gripper (default for horizontal mode)
    #define RBS_DBLCLKTOGGLE     32768


    // end_r_commctrl



    #define RBBS_BREAK                   1  // break to new line
    #define RBBS_FIXEDSIZE               2  // band can't be sized
    #define RBBS_CHILDEDGE               4  // edge around top & bottom of child window
    #define RBBS_HIDDEN                  8  // don't show
    #define RBBS_NOVERT                 16  // don't show when vertical
    #define RBBS_FIXEDBMP               32  // bitmap doesn't move during band resize
    #define RBBS_VARIABLEHEIGHT         64  // allow autosizing of this child vertically
    #define RBBS_GRIPPERALWAYS         128  // always show the gripper
    #define RBBS_NOGRIPPER             256  // never show the gripper
    #define RBBS_USECHEVRON            512  // display drop-down button for this band if it's sized smaller than ideal width
    #define RBBS_HIDETITLE            1024  // keep band title hidden

    #define RBBIM_STYLE                  1
    #define RBBIM_COLORS                 2
    #define RBBIM_TEXT                   4
    #define RBBIM_IMAGE                  8
    #define RBBIM_CHILD                 16
    #define RBBIM_CHILDSIZE             32
    #define RBBIM_SIZE                  64
    #define RBBIM_BACKGROUND           128
    #define RBBIM_ID                   256
    #define RBBIM_IDEALSIZE            512
    #define RBBIM_LPARAM              1024
    #define RBBIM_HEADERSIZE          2048  // control the size of the header


    #define REBARBANDINFOA_V3_SIZE CCSIZEOF_STRUCT(REBARBANDINFOA, wID)
    #define REBARBANDINFOW_V3_SIZE CCSIZEOF_STRUCT(REBARBANDINFOW, wID)


    #ifdef UNICODE
      #define REBARBANDINFO       REBARBANDINFOW
      #define LPREBARBANDINFO     LPREBARBANDINFOW
      #define LPCREBARBANDINFO    LPCREBARBANDINFOW
      #define REBARBANDINFO_V3_SIZE REBARBANDINFOW_V3_SIZE
    #else
      #define REBARBANDINFO       REBARBANDINFOA
      #define LPREBARBANDINFO     LPREBARBANDINFOA
      #define LPCREBARBANDINFO    LPCREBARBANDINFOA
      #define REBARBANDINFO_V3_SIZE REBARBANDINFOA_V3_SIZE
    #endif

    #define RB_INSERTBANDA  (WM_USER +  1)
    #define RB_DELETEBAND   (WM_USER +  2)
    #define RB_GETBARINFO   (WM_USER +  3)
    #define RB_SETBARINFO   (WM_USER +  4)
    #define RB_SETBANDINFOA (WM_USER +  6)
    #define RB_SETPARENT    (WM_USER +  7)
    #define RB_HITTEST      (WM_USER +  8)
    #define RB_GETRECT      (WM_USER +  9)
    #define RB_INSERTBANDW  (WM_USER +  10)
    #define RB_SETBANDINFOW (WM_USER +  11)
    #define RB_GETBANDCOUNT (WM_USER +  12)
    #define RB_GETROWCOUNT  (WM_USER +  13)
    #define RB_GETROWHEIGHT (WM_USER +  14)
    #define RB_IDTOINDEX    (WM_USER +  16) // wParam == id
    #define RB_GETTOOLTIPS  (WM_USER +  17)
    #define RB_SETTOOLTIPS  (WM_USER +  18)
    #define RB_SETBKCOLOR   (WM_USER +  19) // sets the default BK color
    #define RB_GETBKCOLOR   (WM_USER +  20) // defaults to CLR_NONE
    #define RB_SETTEXTCOLOR (WM_USER +  21)
    #define RB_GETTEXTCOLOR (WM_USER +  22) // defaults to 0x00000000
    #define RB_SIZETORECT   (WM_USER +  23) // resize the rebar/break bands and such to this rect (lparam)

    #define RB_SETCOLORSCHEME   CCM_SETCOLORSCHEME  // lParam is color scheme
    #define RB_GETCOLORSCHEME   CCM_GETCOLORSCHEME  // fills in COLORSCHEME pointed to by lParam

    #ifdef UNICODE
      #define RB_INSERTBAND   RB_INSERTBANDW
      #define RB_SETBANDINFO   RB_SETBANDINFOW
    #else
      #define RB_INSERTBAND   RB_INSERTBANDA
      #define RB_SETBANDINFO   RB_SETBANDINFOA
    #endif

    // for manual drag control
    // lparam == cursor pos
           // -1 means do it yourself.
           // -2 means use what you had saved before
    #define RB_BEGINDRAG    (WM_USER + 24)
    #define RB_ENDDRAG      (WM_USER + 25)
    #define RB_DRAGMOVE     (WM_USER + 26)
    #define RB_GETBARHEIGHT (WM_USER + 27)
    #define RB_GETBANDINFOW (WM_USER + 28)
    #define RB_GETBANDINFOA (WM_USER + 29)

    #ifdef UNICODE
      #define RB_GETBANDINFO   RB_GETBANDINFOW
    #else
      #define RB_GETBANDINFO   RB_GETBANDINFOA
    #endif

    #define RB_MINIMIZEBAND (WM_USER + 30)
    #define RB_MAXIMIZEBAND (WM_USER + 31)

    #define RB_GETDROPTARGET (CCM_GETDROPTARGET)

    #define RB_GETBANDBORDERS (WM_USER + 34)  // returns in lparam = lprc the amount of edges added to band wparam

    #define RB_SHOWBAND     (WM_USER + 35)      // show/hide band
    #define RB_SETPALETTE   (WM_USER + 37)
    #define RB_GETPALETTE   (WM_USER + 38)
    #define RB_MOVEBAND     (WM_USER + 39)

    #define RB_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    #define RB_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT


    #define RB_PUSHCHEVRON  (WM_USER + 43)

    #define RBN_HEIGHTCHANGE    (RBN_FIRST - 0)

    #define RBN_GETOBJECT       (RBN_FIRST - 1)
    #define RBN_LAYOUTCHANGED   (RBN_FIRST - 2)
    #define RBN_AUTOSIZE        (RBN_FIRST - 3)
    #define RBN_BEGINDRAG       (RBN_FIRST - 4)
    #define RBN_ENDDRAG         (RBN_FIRST - 5)
    #define RBN_DELETINGBAND    (RBN_FIRST - 6)     // Uses NMREBAR
    #define RBN_DELETEDBAND     (RBN_FIRST - 7)     // Uses NMREBAR
    #define RBN_CHILDSIZE       (RBN_FIRST - 8)

    #define RBN_CHEVRONPUSHED   (RBN_FIRST - 10)


    #define RBN_MINMAX          (RBN_FIRST - 21)


                  // RBNM_*


    // Mask flags for NMREBAR
    #define RBNM_ID                  1
    #define RBNM_STYLE               2
    #define RBNM_LPARAM              4



    #define RBHT_NOWHERE         1
    #define RBHT_CAPTION         2
    #define RBHT_CLIENT          3
    #define RBHT_GRABBER         4
    #define RBHT_CHEVRON         8

  #endif      // NOREBAR

  //====== TOOLTIPS CONTROL =====================================================

  #ifndef NOTOOLTIPS

    #define TOOLTIPS_CLASSW         L"tooltips_class32"
    #define TOOLTIPS_CLASSA         "tooltips_class32"

    #ifdef UNICODE
      #define TOOLTIPS_CLASS          TOOLTIPS_CLASSW
    #else
      #define TOOLTIPS_CLASS          TOOLTIPS_CLASSA
    #endif

    //#define LPTOOLINFOA   LPTTTOOLINFOA
    //#define LPTOOLINFOW   LPTTTOOLINFOW
    //#define TOOLINFOA       TTTOOLINFOA
    //#define TOOLINFOW       TTTOOLINFOW

    //#define LPTOOLINFO    LPTTTOOLINFO
    //#define TOOLINFO        TTTOOLINFO

    #define TTTOOLINFOA_V1_SIZE CCSIZEOF_STRUCT(TTTOOLINFOA, lpszText)
    #define TTTOOLINFOW_V1_SIZE CCSIZEOF_STRUCT(TTTOOLINFOW, lpszText)


    #ifdef UNICODE
      #define TOOLINFO              TOOLINFOW
      #define PTOOLINFO             PTOOLINFOW
      #define LPTOOLINFO            LPTOOLINFOW
      #define TOOLINFO_V1_SIZE TOOLINFOW_V1_SIZE
    #else
      #define PTOOLINFO               PTOOLINFOA
      #define TOOLINFO              TOOLINFOA
      #define LPTOOLINFO            LPTOOLINFOA
      #define TTTOOLINFO_V1_SIZE TTTOOLINFOA_V1_SIZE
    #endif

    // begin_r_commctrl

    #define TTS_ALWAYSTIP              1
    #define TTS_NOPREFIX               2
    #define TTS_NOANIMATE             16
    #define TTS_NOFADE                32
    #define TTS_BALLOON               64
    #define TTS_CLOSE                128

    // end_r_commctrl

    #define TTF_IDISHWND                 1

    // Use this to center around trackpoint in trackmode
    // -OR- to center around tool in normal mode.
    // Use TTF_ABSOLUTE to place the tip exactly at the track coords when
    // in tracking mode.  TTF_ABSOLUTE can be used in conjunction with TTF_CENTERTIP
    // to center the tip absolutely about the track point.

    #define TTF_CENTERTIP                2
    #define TTF_RTLREADING               4
    #define TTF_SUBCLASS                16
    #define TTF_TRACK                   32
    #define TTF_ABSOLUTE               128
    #define TTF_TRANSPARENT            256
    #define TTF_DI_SETITEM           32768       // valid only on the TTN_NEEDTEXT callback

    #define TTDT_AUTOMATIC          0
    #define TTDT_RESHOW             1
    #define TTDT_AUTOPOP            2
    #define TTDT_INITIAL            3

    // ToolTip Icons (Set with TTM_SETTITLE)
    #define TTI_NONE                0
    #define TTI_INFO                1
    #define TTI_WARNING             2
    #define TTI_ERROR               3

    // Tool Tip Messages
    #define TTM_ACTIVATE            (WM_USER + 1)
    #define TTM_SETDELAYTIME        (WM_USER + 3)
    #define TTM_ADDTOOLA            (WM_USER + 4)
    #define TTM_ADDTOOLW            (WM_USER + 50)
    #define TTM_DELTOOLA            (WM_USER + 5)
    #define TTM_DELTOOLW            (WM_USER + 51)
    #define TTM_NEWTOOLRECTA        (WM_USER + 6)
    #define TTM_NEWTOOLRECTW        (WM_USER + 52)
    #define TTM_RELAYEVENT          (WM_USER + 7)

    #define TTM_GETTOOLINFOA        (WM_USER + 8)
    #define TTM_GETTOOLINFOW        (WM_USER + 53)

    #define TTM_SETTOOLINFOA        (WM_USER + 9)
    #define TTM_SETTOOLINFOW        (WM_USER + 54)

    #define TTM_HITTESTA            (WM_USER +10)
    #define TTM_HITTESTW            (WM_USER +55)
    #define TTM_GETTEXTA            (WM_USER +11)
    #define TTM_GETTEXTW            (WM_USER +56)
    #define TTM_UPDATETIPTEXTA      (WM_USER +12)
    #define TTM_UPDATETIPTEXTW      (WM_USER +57)
    #define TTM_GETTOOLCOUNT        (WM_USER +13)
    #define TTM_ENUMTOOLSA          (WM_USER +14)
    #define TTM_ENUMTOOLSW          (WM_USER +58)
    #define TTM_GETCURRENTTOOLA     (WM_USER + 15)
    #define TTM_GETCURRENTTOOLW     (WM_USER + 59)
    #define TTM_WINDOWFROMPOINT     (WM_USER + 16)
    #define TTM_TRACKACTIVATE       (WM_USER + 17)  // wParam = TRUE/FALSE start end  lparam = LPTOOLINFO
    #define TTM_TRACKPOSITION       (WM_USER + 18)  // lParam = dwPos
    #define TTM_SETTIPBKCOLOR       (WM_USER + 19)
    #define TTM_SETTIPTEXTCOLOR     (WM_USER + 20)
    #define TTM_GETDELAYTIME        (WM_USER + 21)
    #define TTM_GETTIPBKCOLOR       (WM_USER + 22)
    #define TTM_GETTIPTEXTCOLOR     (WM_USER + 23)
    #define TTM_SETMAXTIPWIDTH      (WM_USER + 24)
    #define TTM_GETMAXTIPWIDTH      (WM_USER + 25)
    #define TTM_SETMARGIN           (WM_USER + 26)  // lParam = lprc
    #define TTM_GETMARGIN           (WM_USER + 27)  // lParam = lprc
    #define TTM_POP                 (WM_USER + 28)
    #define TTM_UPDATE              (WM_USER + 29)
    #define TTM_GETBUBBLESIZE       (WM_USER + 30)
    #define TTM_ADJUSTRECT          (WM_USER + 31)
    #define TTM_SETTITLEA           (WM_USER + 32)  // wParam = TTI_*, lParam = char* szTitle
    #define TTM_SETTITLEW           (WM_USER + 33)  // wParam = TTI_*, lParam = wchar* szTitle
    #define TTM_POPUP               (WM_USER + 34)


    #ifdef UNICODE
      #define TTM_ADDTOOL             TTM_ADDTOOLW
      #define TTM_DELTOOL             TTM_DELTOOLW
      #define TTM_NEWTOOLRECT         TTM_NEWTOOLRECTW
      #define TTM_GETTOOLINFO         TTM_GETTOOLINFOW
      #define TTM_SETTOOLINFO         TTM_SETTOOLINFOW
      #define TTM_HITTEST             TTM_HITTESTW
      #define TTM_GETTEXT             TTM_GETTEXTW
      #define TTM_UPDATETIPTEXT       TTM_UPDATETIPTEXTW
      #define TTM_ENUMTOOLS           TTM_ENUMTOOLSW
      #define TTM_GETCURRENTTOOL      TTM_GETCURRENTTOOLW
      #define TTM_SETTITLE            TTM_SETTITLEW
    #else
      #define TTM_ADDTOOL             TTM_ADDTOOLA
      #define TTM_DELTOOL             TTM_DELTOOLA
      #define TTM_NEWTOOLRECT         TTM_NEWTOOLRECTA
      #define TTM_GETTOOLINFO         TTM_GETTOOLINFOA
      #define TTM_SETTOOLINFO         TTM_SETTOOLINFOA
      #define TTM_HITTEST             TTM_HITTESTA
      #define TTM_GETTEXT             TTM_GETTEXTA
      #define TTM_UPDATETIPTEXT       TTM_UPDATETIPTEXTA
      #define TTM_ENUMTOOLS           TTM_ENUMTOOLSA
      #define TTM_GETCURRENTTOOL      TTM_GETCURRENTTOOLA
      #define TTM_SETTITLE            TTM_SETTITLEA
    #endif


    #define LPHITTESTINFOW    LPTTHITTESTINFOW
    #define LPHITTESTINFOA    LPTTHITTESTINFOA

    #define LPHITTESTINFO     LPTTHITTESTINFO


    #ifdef UNICODE
      #define TTHITTESTINFO           TTHITTESTINFOW
      #define LPTTHITTESTINFO         LPTTHITTESTINFOW
    #else
      #define TTHITTESTINFO           TTHITTESTINFOA
      #define LPTTHITTESTINFO         LPTTHITTESTINFOA
    #endif

    #define TTN_GETDISPINFOA        (TTN_FIRST - 0)
    #define TTN_GETDISPINFOW        (TTN_FIRST - 10)
    #define TTN_SHOW                (TTN_FIRST - 1)
    #define TTN_POP                 (TTN_FIRST - 2)

    #ifdef UNICODE
      #define TTN_GETDISPINFO         TTN_GETDISPINFOW
    #else
      #define TTN_GETDISPINFO         TTN_GETDISPINFOA
    #endif

    #define TTN_NEEDTEXT            TTN_GETDISPINFO
    #define TTN_NEEDTEXTA           TTN_GETDISPINFOA
    #define TTN_NEEDTEXTW           TTN_GETDISPINFOW

    //#define TOOLTIPTEXTW NMTTDISPINFOW
    //#define TOOLTIPTEXTA NMTTDISPINFOA
    //#define LPTOOLTIPTEXTA LPNMTTDISPINFOA
    //#define LPTOOLTIPTEXTW LPNMTTDISPINFOW

    //#define TOOLTIPTEXT    NMTTDISPINFO
    //#define LPTOOLTIPTEXT  LPNMTTDISPINFO

    #define NMTTDISPINFOA_V1_SIZE CCSIZEOF_STRUCT(NMTTDISPINFOA, uFlags)
    #define NMTTDISPINFOW_V1_SIZE CCSIZEOF_STRUCT(NMTTDISPINFOW, uFlags)


    #ifdef UNICODE
      #define NMTTDISPINFO            NMTTDISPINFOW
      #define LPNMTTDISPINFO          LPNMTTDISPINFOW
      #define NMTTDISPINFO_V1_SIZE NMTTDISPINFOW_V1_SIZE
    #else
      #define NMTTDISPINFO            NMTTDISPINFOA
      #define LPNMTTDISPINFO          LPNMTTDISPINFOA
      #define NMTTDISPINFO_V1_SIZE NMTTDISPINFOA_V1_SIZE
    #endif

  #endif      // NOTOOLTIPS


  //====== STATUS BAR CONTROL ===================================================

  #ifndef NOSTATUSBAR

    // begin_r_commctrl

    #define SBARS_SIZEGRIP             256
    #define SBARS_TOOLTIPS            2048

    // this is a status bar flag, preference to SBARS_TOOLTIPS
    #define SBT_TOOLTIPS              2048

    // end_r_commctrl


    //#ifdef UNICODE
    //#define CreateStatusWindow      CreateStatusWindowW
    //#define DrawStatusText          DrawStatusTextW
    //#else
    //#define CreateStatusWindow      CreateStatusWindowA
    //#define DrawStatusText          DrawStatusTextA
    //#endif

    #define STATUSCLASSNAMEW        L"msctls_statusbar32"
    #define STATUSCLASSNAMEA        "msctls_statusbar32"

    #ifdef UNICODE
      #define STATUSCLASSNAME         STATUSCLASSNAMEW
    #else
      #define STATUSCLASSNAME         STATUSCLASSNAMEA
    #endif

    #define SB_SETTEXTA             (WM_USER+1)
    #define SB_SETTEXTW             (WM_USER+11)
    #define SB_GETTEXTA             (WM_USER+2)
    #define SB_GETTEXTW             (WM_USER+13)
    #define SB_GETTEXTLENGTHA       (WM_USER+3)
    #define SB_GETTEXTLENGTHW       (WM_USER+12)

    #ifdef UNICODE
      #define SB_GETTEXT              SB_GETTEXTW
      #define SB_SETTEXT              SB_SETTEXTW
      #define SB_GETTEXTLENGTH        SB_GETTEXTLENGTHW
      #define SB_SETTIPTEXT           SB_SETTIPTEXTW
      #define SB_GETTIPTEXT           SB_GETTIPTEXTW
    #else
      #define SB_GETTEXT              SB_GETTEXTA
      #define SB_SETTEXT              SB_SETTEXTA
      #define SB_GETTEXTLENGTH        SB_GETTEXTLENGTHA
      #define SB_SETTIPTEXT           SB_SETTIPTEXTA
      #define SB_GETTIPTEXT           SB_GETTIPTEXTA
    #endif


    #define SB_SETPARTS             (WM_USER+4)
    #define SB_GETPARTS             (WM_USER+6)
    #define SB_GETBORDERS           (WM_USER+7)
    #define SB_SETMINHEIGHT         (WM_USER+8)
    #define SB_SIMPLE               (WM_USER+9)
    #define SB_GETRECT              (WM_USER+10)
    #define SB_ISSIMPLE             (WM_USER+14)
    #define SB_SETICON              (WM_USER+15)
    #define SB_SETTIPTEXTA          (WM_USER+16)
    #define SB_SETTIPTEXTW          (WM_USER+17)
    #define SB_GETTIPTEXTA          (WM_USER+18)
    #define SB_GETTIPTEXTW          (WM_USER+19)
    #define SB_GETICON              (WM_USER+20)
    #define SB_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    #define SB_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT

    #define SBT_OWNERDRAW              4096
    #define SBT_NOBORDERS               256
    #define SBT_POPOUT                  512
    #define SBT_RTLREADING             1024
    #define SBT_NOTABPARSING           2048

    #define SB_SETBKCOLOR           CCM_SETBKCOLOR      // lParam = bkColor

    /// status bar notifications
    #define SBN_SIMPLEMODECHANGE    (SBN_FIRST - 0)

    // refers to the data saved for simple mode
    #define SB_SIMPLEID     255

  #endif      // NOSTATUSBAR

  //====== MENU HELP ============================================================

  #ifndef NOMENUHELP

    #define MINSYSCOMMAND   SC_SIZE

  #endif


  //====== TRACKBAR CONTROL =====================================================

  #ifndef NOTRACKBAR

    #define TRACKBAR_CLASSA         "msctls_trackbar32"
    #define TRACKBAR_CLASSW         L"msctls_trackbar32"

    #ifdef UNICODE
      #define  TRACKBAR_CLASS         TRACKBAR_CLASSW
    #else
      #define  TRACKBAR_CLASS         TRACKBAR_CLASSA
    #endif

    // begin_r_commctrl

    #define TBS_AUTOTICKS                1
    #define TBS_VERT                     2
    #define TBS_HORZ                     0
    #define TBS_TOP                      4
    #define TBS_BOTTOM                   0
    #define TBS_LEFT                     4
    #define TBS_RIGHT                    0
    #define TBS_BOTH                     8
    #define TBS_NOTICKS                 16
    #define TBS_ENABLESELRANGE          32
    #define TBS_FIXEDLENGTH             64
    #define TBS_NOTHUMB                128
    #define TBS_TOOLTIPS               256
    #define TBS_REVERSED               512  // Accessibility hint: the smaller number (usually the min value) means "high" and the larger number (usually the max value) means "low"

    // end_r_commctrl

    #define TBM_GETPOS              (WM_USER)
    #define TBM_GETRANGEMIN         (WM_USER+1)
    #define TBM_GETRANGEMAX         (WM_USER+2)
    #define TBM_GETTIC              (WM_USER+3)
    #define TBM_SETTIC              (WM_USER+4)
    #define TBM_SETPOS              (WM_USER+5)
    #define TBM_SETRANGE            (WM_USER+6)
    #define TBM_SETRANGEMIN         (WM_USER+7)
    #define TBM_SETRANGEMAX         (WM_USER+8)
    #define TBM_CLEARTICS           (WM_USER+9)
    #define TBM_SETSEL              (WM_USER+10)
    #define TBM_SETSELSTART         (WM_USER+11)
    #define TBM_SETSELEND           (WM_USER+12)
    #define TBM_GETPTICS            (WM_USER+14)
    #define TBM_GETTICPOS           (WM_USER+15)
    #define TBM_GETNUMTICS          (WM_USER+16)
    #define TBM_GETSELSTART         (WM_USER+17)
    #define TBM_GETSELEND           (WM_USER+18)
    #define TBM_CLEARSEL            (WM_USER+19)
    #define TBM_SETTICFREQ          (WM_USER+20)
    #define TBM_SETPAGESIZE         (WM_USER+21)
    #define TBM_GETPAGESIZE         (WM_USER+22)
    #define TBM_SETLINESIZE         (WM_USER+23)
    #define TBM_GETLINESIZE         (WM_USER+24)
    #define TBM_GETTHUMBRECT        (WM_USER+25)
    #define TBM_GETCHANNELRECT      (WM_USER+26)
    #define TBM_SETTHUMBLENGTH      (WM_USER+27)
    #define TBM_GETTHUMBLENGTH      (WM_USER+28)
    #define TBM_SETTOOLTIPS         (WM_USER+29)
    #define TBM_GETTOOLTIPS         (WM_USER+30)
    #define TBM_SETTIPSIDE          (WM_USER+31)
    // TrackBar Tip Side flags
    #define TBTS_TOP                0
    #define TBTS_LEFT               1
    #define TBTS_BOTTOM             2
    #define TBTS_RIGHT              3

    #define TBM_SETBUDDY            (WM_USER+32) // wparam = BOOL fLeft; (or right)
    #define TBM_GETBUDDY            (WM_USER+33) // wparam = BOOL fLeft; (or right)
    #define TBM_SETUNICODEFORMAT    CCM_SETUNICODEFORMAT
    #define TBM_GETUNICODEFORMAT    CCM_GETUNICODEFORMAT


    #define TB_LINEUP               0
    #define TB_LINEDOWN             1
    #define TB_PAGEUP               2
    #define TB_PAGEDOWN             3
    #define TB_THUMBPOSITION        4
    #define TB_THUMBTRACK           5
    #define TB_TOP                  6
    #define TB_BOTTOM               7
    #define TB_ENDTRACK             8


    // custom draw item specs
    #define TBCD_TICS         1
    #define TBCD_THUMB        2
    #define TBCD_CHANNEL      3

  #endif // trackbar

  //====== DRAG LIST CONTROL ====================================================

  #ifndef NODRAGLIST

    #define DL_BEGINDRAG            (WM_USER+133)
    #define DL_DRAGGING             (WM_USER+134)
    #define DL_DROPPED              (WM_USER+135)
    #define DL_CANCELDRAG           (WM_USER+136)

    #define DL_CURSORSET            0
    #define DL_STOPCURSOR           1
    #define DL_COPYCURSOR           2
    #define DL_MOVECURSOR           3

    #define DRAGLISTMSGSTRING       TEXT("commctrl_DragListMsg")

  #endif //nodraglist


  //====== UPDOWN CONTROL =======================================================

  #ifndef NOUPDOWN

    #define UPDOWN_CLASSA           "msctls_updown32"
    #define UPDOWN_CLASSW           L"msctls_updown32"

    #ifdef UNICODE
      #define  UPDOWN_CLASS           UPDOWN_CLASSW
    #else
      #define  UPDOWN_CLASS           UPDOWN_CLASSA
    #endif


    #define UD_MAXVAL                32767
    #define UD_MINVAL               (-UD_MAXVAL)

    // begin_r_commctrl

    #define UDS_WRAP                     1
    #define UDS_SETBUDDYINT              2
    #define UDS_ALIGNRIGHT               4
    #define UDS_ALIGNLEFT                8
    #define UDS_AUTOBUDDY               16
    #define UDS_ARROWKEYS               32
    #define UDS_HORZ                    64
    #define UDS_NOTHOUSANDS            128
    #define UDS_HOTTRACK               256

    // end_r_commctrl

    #define UDM_SETRANGE            (WM_USER+101)
    #define UDM_GETRANGE            (WM_USER+102)
    #define UDM_SETPOS              (WM_USER+103)
    #define UDM_GETPOS              (WM_USER+104)
    #define UDM_SETBUDDY            (WM_USER+105)
    #define UDM_GETBUDDY            (WM_USER+106)
    #define UDM_SETACCEL            (WM_USER+107)
    #define UDM_GETACCEL            (WM_USER+108)
    #define UDM_SETBASE             (WM_USER+109)
    #define UDM_GETBASE             (WM_USER+110)
    #define UDM_SETRANGE32          (WM_USER+111)
    #define UDM_GETRANGE32          (WM_USER+112) // wParam & lParam are LPINT
    #define UDM_SETUNICODEFORMAT    CCM_SETUNICODEFORMAT
    #define UDM_GETUNICODEFORMAT    CCM_GETUNICODEFORMAT
    #define UDM_SETPOS32            (WM_USER+113)
    #define UDM_GETPOS32            (WM_USER+114)


    #define NM_UPDOWN      NMUPDOWN
    #define LPNM_UPDOWN  LPNMUPDOWN

    #define UDN_DELTAPOS            (UDN_FIRST - 1)

  #endif  // NOUPDOWN


  //====== PROGRESS CONTROL =====================================================

  #ifndef NOPROGRESS

    #define PROGRESS_CLASSA         "msctls_progress32"
    #define PROGRESS_CLASSW         L"msctls_progress32"

    #ifdef UNICODE
      #define  PROGRESS_CLASS         PROGRESS_CLASSW
    #else
      #define  PROGRESS_CLASS         PROGRESS_CLASSA
    #endif

    // begin_r_commctrl

    #define PBS_SMOOTH                 1
    #define PBS_VERTICAL               4

    // end_r_commctrl

    #define PBM_SETRANGE            (WM_USER+1)
    #define PBM_SETPOS              (WM_USER+2)
    #define PBM_DELTAPOS            (WM_USER+3)
    #define PBM_SETSTEP             (WM_USER+4)
    #define PBM_STEPIT              (WM_USER+5)
    #define PBM_SETRANGE32          (WM_USER+6)  // lParam = high, wParam = low


    #define PBM_GETRANGE            (WM_USER+7)  // wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
    #define PBM_GETPOS              (WM_USER+8)
    #define PBM_SETBARCOLOR         (WM_USER+9)             // lParam = bar color
    #define PBM_SETBKCOLOR          CCM_SETBKCOLOR  // lParam = bkColor

  #endif  // NOPROGRESS


  //====== HOTKEY CONTROL =======================================================

  #ifndef NOHOTKEY

    #define HOTKEYF_SHIFT              1
    #define HOTKEYF_CONTROL            2
    #define HOTKEYF_ALT                4
    #define HOTKEYF_EXT                8

    #define HKCOMB_NONE                  1
    #define HKCOMB_S                     2
    #define HKCOMB_C                     4
    #define HKCOMB_A                     8
    #define HKCOMB_SC                   16
    #define HKCOMB_SA                   32
    #define HKCOMB_CA                   64
    #define HKCOMB_SCA                 128


    #define HKM_SETHOTKEY           (WM_USER+1)
    #define HKM_GETHOTKEY           (WM_USER+2)
    #define HKM_SETRULES            (WM_USER+3)

    #define HOTKEY_CLASSA           "msctls_hotkey32"
    #define HOTKEY_CLASSW           L"msctls_hotkey32"

    #ifdef UNICODE
      #define HOTKEY_CLASS            HOTKEY_CLASSW
    #else
      #define HOTKEY_CLASS            HOTKEY_CLASSA
    #endif

  #endif  // NOHOTKEY

  // begin_r_commctrl

  //====== COMMON CONTROL STYLES ================================================

  #define CCS_TOP                           1
  #define CCS_NOMOVEY                       2
  #define CCS_BOTTOM                        3
  #define CCS_NORESIZE                      4
  #define CCS_NOPARENTALIGN                 8
  #define CCS_ADJUSTABLE                   32
  #define CCS_NODIVIDER                    64
  #define CCS_VERT                        128
  #define CCS_LEFT                (CCS_VERT + CCS_TOP)
  #define CCS_RIGHT               (CCS_VERT + CCS_BOTTOM)
  #define CCS_NOMOVEX             (CCS_VERT + CCS_NOMOVEY)

  // end_r_commctrl

  //====== LISTVIEW CONTROL =====================================================

  #ifndef NOLISTVIEW

    #define WC_LISTVIEWA            "SysListView32"
    #define WC_LISTVIEWW            L"SysListView32"

    #ifdef UNICODE
      #define WC_LISTVIEW             WC_LISTVIEWW
    #else
      #define WC_LISTVIEW             WC_LISTVIEWA
    #endif

    // begin_r_commctrl

    #define LVS_ICON                     0
    #define LVS_REPORT                   1
    #define LVS_SMALLICON                2
    #define LVS_LIST                     3
    #define LVS_TYPEMASK                 3
    #define LVS_SINGLESEL                4
    #define LVS_SHOWSELALWAYS            8
    #define LVS_SORTASCENDING           16
    #define LVS_SORTDESCENDING          32
    #define LVS_SHAREIMAGELISTS         64
    #define LVS_NOLABELWRAP            128
    #define LVS_AUTOARRANGE            256
    #define LVS_EDITLABELS             512
    #define LVS_OWNERDATA             4096
    #define LVS_NOSCROLL              8192

    #define LVS_TYPESTYLEMASK        64512

    #define LVS_ALIGNTOP                 0
    #define LVS_ALIGNLEFT             2048
    #define LVS_ALIGNMASK             3072

    #define LVS_OWNERDRAWFIXED        1024
    #define LVS_NOCOLUMNHEADER       16384
    #define LVS_NOSORTHEADER         32768

    // end_r_commctrl

    #define LVM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    //#define ListView_SetUnicodeFormat(hwnd, fUnicode)  \


    #define LVM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
    //#define ListView_GetUnicodeFormat(hwnd)  \


    #define LVM_GETBKCOLOR          (LVM_FIRST + 0)
    //#define ListView_GetBkColor(hwnd)  \


    #define LVM_SETBKCOLOR          (LVM_FIRST + 1)
    //#define ListView_SetBkColor(hwnd, clrBk) \


    #define LVM_GETIMAGELIST        (LVM_FIRST + 2)
    //#define ListView_GetImageList(hwnd, iImageList) \


    #define LVSIL_NORMAL            0
    #define LVSIL_SMALL             1
    #define LVSIL_STATE             2

    #define LVM_SETIMAGELIST        (LVM_FIRST + 3)
    //#define ListView_SetImageList(hwnd, himl, iImageList) \


    #define LVM_GETITEMCOUNT        (LVM_FIRST + 4)
    //#define ListView_GetItemCount(hwnd) \


    #define LVIF_TEXT                    1
    #define LVIF_IMAGE                   2
    #define LVIF_PARAM                   4
    #define LVIF_STATE                   8
    #define LVIF_INDENT                 16
    #define LVIF_NORECOMPUTE          2048

    #define LVIS_FOCUSED                 1
    #define LVIS_SELECTED                2
    #define LVIS_CUT                     4
    #define LVIS_DROPHILITED             8
    #define LVIS_ACTIVATING             32

    #define LVIS_OVERLAYMASK          3840
    #define LVIS_STATEIMAGEMASK      61440

    #define INDEXTOSTATEIMAGEMASK(i) ((i) << 12)

    #define I_INDENTCALLBACK        (-1)
    #define LV_ITEMA LVITEMA
    #define LV_ITEMW LVITEMW

    #define LV_ITEM LVITEM

    #define LVITEMA_V1_SIZE CCSIZEOF_STRUCT(LVITEMA, lParam)
    #define LVITEMW_V1_SIZE CCSIZEOF_STRUCT(LVITEMW, lParam)


    #ifdef UNICODE
      #define LVITEM    LVITEMW
      #define LPLVITEM  LPLVITEMW
      #define LVITEM_V1_SIZE LVITEMW_V1_SIZE
    #else
      #define LVITEM    LVITEMA
      #define LPLVITEM  LPLVITEMA
      #define LVITEM_V1_SIZE LVITEMA_V1_SIZE
    #endif


    #define LPSTR_TEXTCALLBACKW     ((LPWSTR)-1L)
    #define LPSTR_TEXTCALLBACKA     ((LPSTR)-1L)
    #ifdef UNICODE
      #define LPSTR_TEXTCALLBACK      LPSTR_TEXTCALLBACKW
    #else
      #define LPSTR_TEXTCALLBACK      LPSTR_TEXTCALLBACKA
    #endif

    #define I_IMAGECALLBACK         (-1)
    #define I_IMAGENONE             (-2)

    #define LVM_GETITEMA            (LVM_FIRST + 5)
    #define LVM_GETITEMW            (LVM_FIRST + 75)
    #ifdef UNICODE
      #define LVM_GETITEM             LVM_GETITEMW
    #else
      #define LVM_GETITEM             LVM_GETITEMA
    #endif

    //#define ListView_GetItem(hwnd, pitem) \


    #define LVM_SETITEMA            (LVM_FIRST + 6)
    #define LVM_SETITEMW            (LVM_FIRST + 76)
    #ifdef UNICODE
      #define LVM_SETITEM             LVM_SETITEMW
    #else
      #define LVM_SETITEM             LVM_SETITEMA
    #endif

    //#define ListView_SetItem(hwnd, pitem) \


    #define LVM_INSERTITEMA         (LVM_FIRST + 7)
    #define LVM_INSERTITEMW         (LVM_FIRST + 77)
    #ifdef UNICODE
      #define LVM_INSERTITEM          LVM_INSERTITEMW
    #else
      #define LVM_INSERTITEM          LVM_INSERTITEMA
    #endif
    //#define ListView_InsertItem(hwnd, pitem)   \


    #define LVM_DELETEITEM          (LVM_FIRST + 8)
    //#define ListView_DeleteItem(hwnd, i) \


    #define LVM_DELETEALLITEMS      (LVM_FIRST + 9)
    //#define ListView_DeleteAllItems(hwnd) \


    #define LVM_GETCALLBACKMASK     (LVM_FIRST + 10)
    //#define ListView_GetCallbackMask(hwnd) \


    #define LVM_SETCALLBACKMASK     (LVM_FIRST + 11)
    //#define ListView_SetCallbackMask(hwnd, mask) \


    #define LVNI_ALL                     0
    #define LVNI_FOCUSED                 1
    #define LVNI_SELECTED                2
    #define LVNI_CUT                     4
    #define LVNI_DROPHILITED             8

    #define LVNI_ABOVE                 256
    #define LVNI_BELOW                 512
    #define LVNI_TOLEFT               1024
    #define LVNI_TORIGHT              2048


    #define LVM_GETNEXTITEM         (LVM_FIRST + 12)
    //#define ListView_GetNextItem(hwnd, i, flags) \


    #define LVFI_PARAM                   1
    #define LVFI_STRING                  2
    #define LVFI_PARTIAL                 8
    #define LVFI_WRAP                   32
    #define LVFI_NEARESTXY              64

    #define LV_FINDINFOA    LVFINDINFOA
    #define LV_FINDINFOW    LVFINDINFOW

    #define LV_FINDINFO  LVFINDINFO


    #ifdef UNICODE
      #define  LVFINDINFO            LVFINDINFOW
    #else
      #define  LVFINDINFO            LVFINDINFOA
    #endif

    #define LVM_FINDITEMA           (LVM_FIRST + 13)
    #define LVM_FINDITEMW           (LVM_FIRST + 83)
    #ifdef UNICODE
      #define  LVM_FINDITEM           LVM_FINDITEMW
    #else
      #define  LVM_FINDITEM           LVM_FINDITEMA
    #endif

    //#define ListView_FindItem(hwnd, iStart, plvfi) \


    #define LVIR_BOUNDS             0
    #define LVIR_ICON               1
    #define LVIR_LABEL              2
    #define LVIR_SELECTBOUNDS       3


    #define LVM_GETITEMRECT         (LVM_FIRST + 14)
    //#define ListView_GetItemRect(hwnd, i, prc, code) \


    #define LVM_SETITEMPOSITION     (LVM_FIRST + 15)
    //#define ListView_SetItemPosition(hwndLV, i, x, y) \


    #define LVM_GETITEMPOSITION     (LVM_FIRST + 16)
    //#define ListView_GetItemPosition(hwndLV, i, ppt) \


    #define LVM_GETSTRINGWIDTHA     (LVM_FIRST + 17)
    #define LVM_GETSTRINGWIDTHW     (LVM_FIRST + 87)
    #ifdef UNICODE
      #define  LVM_GETSTRINGWIDTH     LVM_GETSTRINGWIDTHW
    #else
      #define  LVM_GETSTRINGWIDTH     LVM_GETSTRINGWIDTHA
    #endif

    //#define ListView_GetStringWidth(hwndLV, psz) \


    #define LVHT_NOWHERE                 1
    #define LVHT_ONITEMICON              2
    #define LVHT_ONITEMLABEL             4
    #define LVHT_ONITEMSTATEICON         8
    #define LVHT_ONITEM             (LVHT_ONITEMICON + LVHT_ONITEMLABEL + LVHT_ONITEMSTATEICON)

    #define LVHT_ABOVE                   8
    #define LVHT_BELOW                  16
    #define LVHT_TORIGHT                32
    #define LVHT_TOLEFT                 64

    #define LV_HITTESTINFO LVHITTESTINFO

    #define LVHITTESTINFO_V1_SIZE CCSIZEOF_STRUCT(LVHITTESTINFO, iItem)


    #define LVM_HITTEST             (LVM_FIRST + 18)
    //#define ListView_HitTest(hwndLV, pinfo) \


    #define LVM_ENSUREVISIBLE       (LVM_FIRST + 19)
    //#define ListView_EnsureVisible(hwndLV, i, fPartialOK) \


    #define LVM_SCROLL              (LVM_FIRST + 20)
    //#define ListView_Scroll(hwndLV, dx, dy) \


    #define LVM_REDRAWITEMS         (LVM_FIRST + 21)
    //#define ListView_RedrawItems(hwndLV, iFirst, iLast) \


    #define LVA_DEFAULT                  0
    #define LVA_ALIGNLEFT                1
    #define LVA_ALIGNTOP                 2
    #define LVA_SNAPTOGRID               5

    #define LVM_ARRANGE             (LVM_FIRST + 22)
    //#define ListView_Arrange(hwndLV, code) \


    #define LVM_EDITLABELA          (LVM_FIRST + 23)
    #define LVM_EDITLABELW          (LVM_FIRST + 118)
    #ifdef UNICODE
      #define LVM_EDITLABEL           LVM_EDITLABELW
    #else
      #define LVM_EDITLABEL           LVM_EDITLABELA
    #endif

    //#define ListView_EditLabel(hwndLV, i) \


    #define LVM_GETEDITCONTROL      (LVM_FIRST + 24)
    //#define ListView_GetEditControl(hwndLV) \


    #define LV_COLUMNA      LVCOLUMNA
    #define LV_COLUMNW      LVCOLUMNW

    #define LV_COLUMN       LVCOLUMN

    #define LVCOLUMNA_V1_SIZE CCSIZEOF_STRUCT(LVCOLUMNA, iSubItem)
    #define LVCOLUMNW_V1_SIZE CCSIZEOF_STRUCT(LVCOLUMNW, iSubItem)


    #ifdef UNICODE
      #define  LVCOLUMN               LVCOLUMNW
      #define  LPLVCOLUMN             LPLVCOLUMNW
      #define LVCOLUMN_V1_SIZE LVCOLUMNW_V1_SIZE
    #else
      #define  LVCOLUMN               LVCOLUMNA
      #define  LPLVCOLUMN             LPLVCOLUMNA
      #define LVCOLUMN_V1_SIZE LVCOLUMNA_V1_SIZE
    #endif


    #define LVCF_FMT                     1
    #define LVCF_WIDTH                   2
    #define LVCF_TEXT                    4
    #define LVCF_SUBITEM                 8
    #define LVCF_IMAGE                  16
    #define LVCF_ORDER                  32

    #define LVCFMT_LEFT                  0
    #define LVCFMT_RIGHT                 1
    #define LVCFMT_CENTER                2
    #define LVCFMT_JUSTIFYMASK           3

    #define LVCFMT_IMAGE              2048
    #define LVCFMT_BITMAP_ON_RIGHT    4096
    #define LVCFMT_COL_HAS_IMAGES    32768

    #define LVM_GETCOLUMNA          (LVM_FIRST + 25)
    #define LVM_GETCOLUMNW          (LVM_FIRST + 95)
    #ifdef UNICODE
      #define  LVM_GETCOLUMN          LVM_GETCOLUMNW
    #else
      #define  LVM_GETCOLUMN          LVM_GETCOLUMNA
    #endif

    //#define ListView_GetColumn(hwnd, iCol, pcol) \


    #define LVM_SETCOLUMNA          (LVM_FIRST + 26)
    #define LVM_SETCOLUMNW          (LVM_FIRST + 96)
    #ifdef UNICODE
      #define  LVM_SETCOLUMN          LVM_SETCOLUMNW
    #else
      #define  LVM_SETCOLUMN          LVM_SETCOLUMNA
    #endif

    //#define ListView_SetColumn(hwnd, iCol, pcol) \


    #define LVM_INSERTCOLUMN        (LVM_FIRST + 27)
    #define LVM_INSERTCOLUMNW       (LVM_FIRST + 97)

    //#define ListView_InsertColumn(hwnd, iCol, pcol) \


    #define LVM_DELETECOLUMN        (LVM_FIRST + 28)
    //#define ListView_DeleteColumn(hwnd, iCol) \


    #define LVM_GETCOLUMNWIDTH      (LVM_FIRST + 29)
    //#define ListView_GetColumnWidth(hwnd, iCol) \


    #define LVSCW_AUTOSIZE              -1
    #define LVSCW_AUTOSIZE_USEHEADER    -2
    #define LVM_SETCOLUMNWIDTH          (LVM_FIRST + 30)

    //#define ListView_SetColumnWidth(hwnd, iCol, cx) \


    #define LVM_GETHEADER               (LVM_FIRST + 31)
    //#define ListView_GetHeader(hwnd)\

    #define LVM_CREATEDRAGIMAGE     (LVM_FIRST + 33)
    //#define ListView_CreateDragImage(hwnd, i, lpptUpLeft) \

    #define LVM_GETVIEWRECT         (LVM_FIRST + 34)
    //#define ListView_GetViewRect(hwnd, prc) \

    #define LVM_GETTEXTCOLOR        (LVM_FIRST + 35)
    //#define ListView_GetTextColor(hwnd)  \

    #define LVM_SETTEXTCOLOR        (LVM_FIRST + 36)
    //#define ListView_SetTextColor(hwnd, clrText) \

    #define LVM_GETTEXTBKCOLOR      (LVM_FIRST + 37)
    //#define ListView_GetTextBkColor(hwnd)  \

    #define LVM_SETTEXTBKCOLOR      (LVM_FIRST + 38)
    //#define ListView_SetTextBkColor(hwnd, clrTextBk) \

    #define LVM_GETTOPINDEX         (LVM_FIRST + 39)
    //#define ListView_GetTopIndex(hwndLV) \

    #define LVM_GETCOUNTPERPAGE     (LVM_FIRST + 40)
    //#define ListView_GetCountPerPage(hwndLV) \

    #define LVM_GETORIGIN           (LVM_FIRST + 41)
    //#define ListView_GetOrigin(hwndLV, ppt) \

    #define LVM_UPDATE              (LVM_FIRST + 42)
    //#define ListView_Update(hwndLV, i) \

    #define LVM_SETITEMSTATE        (LVM_FIRST + 43)
    //#define ListView_SetItemState(hwndLV, i, data, mask) \

    //#define ListView_SetCheckState(hwndLV, i, fCheck) \

    #define LVM_GETITEMSTATE        (LVM_FIRST + 44)
    //#define ListView_GetItemState(hwndLV, i, mask) \


    //#define ListView_GetCheckState(hwndLV, i) \

    #define LVM_GETITEMTEXTA        (LVM_FIRST + 45)
    #define LVM_GETITEMTEXTW        (LVM_FIRST + 115)

    #ifdef UNICODE
      #define  LVM_GETITEMTEXT        LVM_GETITEMTEXTW
    #else
      #define  LVM_GETITEMTEXT        LVM_GETITEMTEXTA
    #endif

    //#define ListView_GetItemText(hwndLV, i, iSubItem_, pszText_, cchTextMax_) \


    #define LVM_SETITEMTEXTA        (LVM_FIRST + 46)
    #define LVM_SETITEMTEXTW        (LVM_FIRST + 116)

    #ifdef UNICODE
      #define  LVM_SETITEMTEXT        LVM_SETITEMTEXTW
    #else
      #define  LVM_SETITEMTEXT        LVM_SETITEMTEXTA
    #endif

    //#define ListView_SetItemText(hwndLV, i, iSubItem_, pszText_) \


    // these flags only apply to LVS_OWNERDATA listviews in report or list mode
    #define LVSICF_NOINVALIDATEALL           1
    #define LVSICF_NOSCROLL                  2

    #define LVM_SETITEMCOUNT        (LVM_FIRST + 47)
    //#define ListView_SetItemCount(hwndLV, cItems) \


    //#define ListView_SetItemCountEx(hwndLV, cItems, dwFlags) \


    #define LVM_SORTITEMS           (LVM_FIRST + 48)
    //#define ListView_SortItems(hwndLV, _pfnCompare, _lPrm) \


    #define LVM_SETITEMPOSITION32   (LVM_FIRST + 49)
    //#define ListView_SetItemPosition32(hwndLV, i, x0, y0) \


    #define LVM_GETSELECTEDCOUNT    (LVM_FIRST + 50)
    //#define ListView_GetSelectedCount(hwndLV) \


    #define LVM_GETITEMSPACING      (LVM_FIRST + 51)
    //#define ListView_GetItemSpacing(hwndLV, fSmall) \


    #define LVM_GETISEARCHSTRINGA   (LVM_FIRST + 52)
    #define LVM_GETISEARCHSTRINGW   (LVM_FIRST + 117)

    #ifdef UNICODE
      #define LVM_GETISEARCHSTRING    LVM_GETISEARCHSTRINGW
    #else
      #define LVM_GETISEARCHSTRING    LVM_GETISEARCHSTRINGA
    #endif

    //#define ListView_GetISearchString(hwndLV, lpsz) \


    #define LVM_SETICONSPACING      (LVM_FIRST + 53)
    // -1 for cx and cy means we'll use the default (system settings)
    // 0 for cx or cy means use the current setting (allows you to change just one param)
    //#define ListView_SetIconSpacing(hwndLV, cx, cy) \


    #define LVM_SETEXTENDEDLISTVIEWSTYLE (LVM_FIRST + 54)   // optional wParam == mask
    //#define ListView_SetExtendedListViewStyle(hwndLV, dw)\

    //#define ListView_SetExtendedListViewStyleEx(hwndLV, dwMask, dw)\

    #define LVM_GETEXTENDEDLISTVIEWSTYLE (LVM_FIRST + 55)
    //#define ListView_GetExtendedListViewStyle(hwndLV)\


    #define LVS_EX_GRIDLINES                 1
    #define LVS_EX_SUBITEMIMAGES             2
    #define LVS_EX_CHECKBOXES                4
    #define LVS_EX_TRACKSELECT               8
    #define LVS_EX_HEADERDRAGDROP           16
    #define LVS_EX_FULLROWSELECT            32 // applies to report mode only
    #define LVS_EX_ONECLICKACTIVATE         64
    #define LVS_EX_TWOCLICKACTIVATE        128
    #define LVS_EX_FLATSB                  256
    #define LVS_EX_REGIONAL                512
    #define LVS_EX_INFOTIP                1024 // listview does InfoTips for you
    #define LVS_EX_UNDERLINEHOT           2048
    #define LVS_EX_UNDERLINECOLD          4096
    #define LVS_EX_MULTIWORKAREAS         8192

    #define LVS_EX_LABELTIP              16384 // listview unfolds partly hidden labels if it does not have infotip text

    #define LVM_GETSUBITEMRECT      (LVM_FIRST + 56)
    //#define ListView_GetSubItemRect(hwnd, iItem, iSubItem, code, prc) \


    #define LVM_SUBITEMHITTEST      (LVM_FIRST + 57)
    //#define ListView_SubItemHitTest(hwnd, plvhti) \


    #define LVM_SETCOLUMNORDERARRAY (LVM_FIRST + 58)
    //#define ListView_SetColumnOrderArray(hwnd, iCount, pi) \


    #define LVM_GETCOLUMNORDERARRAY (LVM_FIRST + 59)
    //#define ListView_GetColumnOrderArray(hwnd, iCount, pi) \


    #define LVM_SETHOTITEM  (LVM_FIRST + 60)
    //#define ListView_SetHotItem(hwnd, i) \


    #define LVM_GETHOTITEM  (LVM_FIRST + 61)
    //#define ListView_GetHotItem(hwnd) \


    #define LVM_SETHOTCURSOR  (LVM_FIRST + 62)
    //#define ListView_SetHotCursor(hwnd, hcur) \


    #define LVM_GETHOTCURSOR  (LVM_FIRST + 63)
    //#define ListView_GetHotCursor(hwnd) \


    #define LVM_APPROXIMATEVIEWRECT (LVM_FIRST + 64)
    //#define ListView_ApproximateViewRect(hwnd, iWidth, iHeight, iCount) \

    #define LV_MAX_WORKAREAS         16
    #define LVM_SETWORKAREAS         (LVM_FIRST + 65)
    //#define ListView_SetWorkAreas(hwnd, nWorkAreas, prc) \


    #define LVM_GETWORKAREAS        (LVM_FIRST + 70)
    //#define ListView_GetWorkAreas(hwnd, nWorkAreas, prc) \


    #define LVM_GETNUMBEROFWORKAREAS  (LVM_FIRST + 73)
    //#define ListView_GetNumberOfWorkAreas(hwnd, pnWorkAreas) \


    #define LVM_GETSELECTIONMARK    (LVM_FIRST + 66)
    //#define ListView_GetSelectionMark(hwnd) \


    #define LVM_SETSELECTIONMARK    (LVM_FIRST + 67)
    //#define ListView_SetSelectionMark(hwnd, i) \


    #define LVM_SETHOVERTIME        (LVM_FIRST + 71)
    //#define ListView_SetHoverTime(hwndLV, dwHoverTimeMs)\


    #define LVM_GETHOVERTIME        (LVM_FIRST + 72)
    //#define ListView_GetHoverTime(hwndLV)\


    #define LVM_SETTOOLTIPS       (LVM_FIRST + 74)
    //#define ListView_SetToolTips(hwndLV, hwndNewHwnd)\


    #define LVM_GETTOOLTIPS       (LVM_FIRST + 78)
    //#define ListView_GetToolTips(hwndLV)\


    #define LVM_SORTITEMSEX          (LVM_FIRST + 81)
    //#define ListView_SortItemsEx(hwndLV, _pfnCompare, _lPrm) \


                     // LVBKIF_*


                     // LVBKIF_*


    #define LVBKIF_SOURCE_NONE               0
    #define LVBKIF_SOURCE_HBITMAP            1
    #define LVBKIF_SOURCE_URL                2
    #define LVBKIF_SOURCE_MASK               3
    #define LVBKIF_STYLE_NORMAL              0
    #define LVBKIF_STYLE_TILE               16
    #define LVBKIF_STYLE_MASK               16

    #define LVM_SETBKIMAGEA         (LVM_FIRST + 68)
    #define LVM_SETBKIMAGEW         (LVM_FIRST + 138)
    #define LVM_GETBKIMAGEA         (LVM_FIRST + 69)
    #define LVM_GETBKIMAGEW         (LVM_FIRST + 139)

    #ifdef UNICODE
      #define LVBKIMAGE               LVBKIMAGEW
      #define LPLVBKIMAGE             LPLVBKIMAGEW
      #define LVM_SETBKIMAGE          LVM_SETBKIMAGEW
      #define LVM_GETBKIMAGE          LVM_GETBKIMAGEW
    #else
      #define LVBKIMAGE               LVBKIMAGEA
      #define LPLVBKIMAGE             LPLVBKIMAGEA
      #define LVM_SETBKIMAGE          LVM_SETBKIMAGEA
      #define LVM_GETBKIMAGE          LVM_GETBKIMAGEA
    #endif


    //#define ListView_SetBkImage(hwnd, plvbki) \


    //#define ListView_GetBkImage(hwnd, plvbki) \

    #define LPNM_LISTVIEW   LPNMLISTVIEW
    #define NM_LISTVIEW     NMLISTVIEW


    // NMITEMACTIVATE is used instead of NMLISTVIEW in IE >= 0x400
    // therefore all the fields are the same except for extra uKeyFlags
    // they are used to store key flags at the time of the single click with
    // delayed activation - because by the time the timer goes off a user may
    // not hold the keys (shift, ctrl) any more


    // key flags stored in uKeyFlags
    #define LVKF_ALT            1
    #define LVKF_CONTROL        2
    #define LVKF_SHIFT          4


    #define NMLVCUSTOMDRAW_V3_SIZE CCSIZEOF_STRUCT(NMLVCUSTOMDRW, clrTextBk)


    #define LPNM_CACHEHINT  LPNMLVCACHEHINT
    #define PNM_CACHEHINT   LPNMLVCACHEHINT
    #define NM_CACHEHINT    NMLVCACHEHINT


    #define PNM_FINDITEMA   LPNMLVFINDITEMA
    #define LPNM_FINDITEMA  LPNMLVFINDITEMA
    #define NM_FINDITEMA    NMLVFINDITEMA

    #define PNM_FINDITEMW   LPNMLVFINDITEMW
    #define LPNM_FINDITEMW  LPNMLVFINDITEMW
    #define NM_FINDITEMW    NMLVFINDITEMW

    #ifdef UNICODE
      #define PNM_FINDITEM    PNM_FINDITEMW
      #define LPNM_FINDITEM   LPNM_FINDITEMW
      #define NM_FINDITEM     NM_FINDITEMW
      #define NMLVFINDITEM    NMLVFINDITEMW
      #define LPNMLVFINDITEM  LPNMLVFINDITEMW
    #else
      #define PNM_FINDITEM    PNM_FINDITEMA
      #define LPNM_FINDITEM   LPNM_FINDITEMA
      #define NM_FINDITEM     NM_FINDITEMA
      #define NMLVFINDITEM    NMLVFINDITEMA
      #define LPNMLVFINDITEM  LPNMLVFINDITEMA
    #endif


    #define PNM_ODSTATECHANGE   LPNMLVODSTATECHANGE
    #define LPNM_ODSTATECHANGE  LPNMLVODSTATECHANGE
    #define NM_ODSTATECHANGE    NMLVODSTATECHANGE


    #define LVN_ITEMCHANGING        (LVN_FIRST-0)
    #define LVN_ITEMCHANGED         (LVN_FIRST-1)
    #define LVN_INSERTITEM          (LVN_FIRST-2)
    #define LVN_DELETEITEM          (LVN_FIRST-3)
    #define LVN_DELETEALLITEMS      (LVN_FIRST-4)
    #define LVN_BEGINLABELEDITA     (LVN_FIRST-5)
    #define LVN_BEGINLABELEDITW     (LVN_FIRST-75)
    #define LVN_ENDLABELEDITA       (LVN_FIRST-6)
    #define LVN_ENDLABELEDITW       (LVN_FIRST-76)
    #define LVN_COLUMNCLICK         (LVN_FIRST-8)
    #define LVN_BEGINDRAG           (LVN_FIRST-9)
    #define LVN_BEGINRDRAG          (LVN_FIRST-11)

    #define LVN_ODCACHEHINT         (LVN_FIRST-13)
    #define LVN_ODFINDITEMA         (LVN_FIRST-52)
    #define LVN_ODFINDITEMW         (LVN_FIRST-79)

    #define LVN_ITEMACTIVATE        (LVN_FIRST-14)
    #define LVN_ODSTATECHANGED      (LVN_FIRST-15)

    #ifdef UNICODE
      #define LVN_ODFINDITEM          LVN_ODFINDITEMW
    #else
      #define LVN_ODFINDITEM          LVN_ODFINDITEMA
    #endif


    #define LVN_HOTTRACK            (LVN_FIRST-21)

    #define LVN_GETDISPINFOA        (LVN_FIRST-50)
    #define LVN_GETDISPINFOW        (LVN_FIRST-77)
    #define LVN_SETDISPINFOA        (LVN_FIRST-51)
    #define LVN_SETDISPINFOW        (LVN_FIRST-78)

    #ifdef UNICODE
      #define LVN_BEGINLABELEDIT      LVN_BEGINLABELEDITW
      #define LVN_ENDLABELEDIT        LVN_ENDLABELEDITW
      #define LVN_GETDISPINFO         LVN_GETDISPINFOW
      #define LVN_SETDISPINFO         LVN_SETDISPINFOW
    #else
      #define LVN_BEGINLABELEDIT      LVN_BEGINLABELEDITA
      #define LVN_ENDLABELEDIT        LVN_ENDLABELEDITA
      #define LVN_GETDISPINFO         LVN_GETDISPINFOA
      #define LVN_SETDISPINFO         LVN_SETDISPINFOA
    #endif


    #define LVIF_DI_SETITEM           4096

    #define LV_DISPINFOA    NMLVDISPINFOA
    #define LV_DISPINFOW    NMLVDISPINFOW

    #define LV_DISPINFO     NMLVDISPINFO


    #ifdef UNICODE
      #define  NMLVDISPINFO           NMLVDISPINFOW
    #else
      #define  NMLVDISPINFO           NMLVDISPINFOA
    #endif

    #define LVN_KEYDOWN             (LVN_FIRST-55)

    #define LV_KEYDOWN              NMLVKEYDOWN

    #define LVN_MARQUEEBEGIN        (LVN_FIRST-56)


    // NMLVGETINFOTIPA.dwFlag values

    #define LVGIT_UNFOLDED       1

    #define LVN_GETINFOTIPA          (LVN_FIRST-57)
    #define LVN_GETINFOTIPW          (LVN_FIRST-58)

    #ifdef UNICODE
      #define LVN_GETINFOTIP          LVN_GETINFOTIPW
      #define NMLVGETINFOTIP          NMLVGETINFOTIPW
      #define LPNMLVGETINFOTIP        LPNMLVGETINFOTIPW
    #else
      #define LVN_GETINFOTIP          LVN_GETINFOTIPA
      #define NMLVGETINFOTIP          NMLVGETINFOTIPA
      #define LPNMLVGETINFOTIP        LPNMLVGETINFOTIPA
    #endif

  #endif // NOLISTVIEW

  //====== TREEVIEW CONTROL =====================================================

  #ifndef NOTREEVIEW

    #define WC_TREEVIEWA            "SysTreeView32"
    #define WC_TREEVIEWW            L"SysTreeView32"

    #ifdef UNICODE
      #define  WC_TREEVIEW            WC_TREEVIEWW
    #else
      #define  WC_TREEVIEW            WC_TREEVIEWA
    #endif

    // begin_r_commctrl

    #define TVS_HASBUTTONS               1
    #define TVS_HASLINES                 2
    #define TVS_LINESATROOT              4
    #define TVS_EDITLABELS               8
    #define TVS_DISABLEDRAGDROP         16
    #define TVS_SHOWSELALWAYS           32
    #define TVS_RTLREADING              64

    #define TVS_NOTOOLTIPS             128
    #define TVS_CHECKBOXES             256
    #define TVS_TRACKSELECT            512
    #define TVS_SINGLEEXPAND          1024
    #define TVS_INFOTIP               2048
    #define TVS_FULLROWSELECT         4096
    #define TVS_NOSCROLL              8192
    #define TVS_NONEVENHEIGHT        16384
    #define TVS_NOHSCROLL            32768  // TVS_NOSCROLL overrides this

    // end_r_commctrl


    #define TVIF_TEXT                    1
    #define TVIF_IMAGE                   2
    #define TVIF_PARAM                   4
    #define TVIF_STATE                   8
    #define TVIF_HANDLE                 16
    #define TVIF_SELECTEDIMAGE          32
    #define TVIF_CHILDREN               64
    #define TVIF_INTEGRAL              128
    #define TVIS_SELECTED                2
    #define TVIS_CUT                     4
    #define TVIS_DROPHILITED             8
    #define TVIS_BOLD                   16
    #define TVIS_EXPANDED               32
    #define TVIS_EXPANDEDONCE           64
    #define TVIS_EXPANDPARTIAL         128

    #define TVIS_OVERLAYMASK          3840
    #define TVIS_STATEIMAGEMASK      61440
    #define TVIS_USERMASK            61440

    #define I_CHILDRENCALLBACK  (-1)

    #define LPTV_ITEMW              LPTVITEMW
    #define LPTV_ITEMA              LPTVITEMA
    #define TV_ITEMW                TVITEMW
    #define TV_ITEMA                TVITEMA

    #define LPTV_ITEM               LPTVITEM
    #define TV_ITEM                 TVITEM


    #ifdef UNICODE
      #define  TVITEM                 TVITEMW
      #define  LPTVITEM               LPTVITEMW
    #else
      #define  TVITEM                 TVITEMA
      #define  LPTVITEM               LPTVITEMA
    #endif

    #define TVI_ROOT  (-0x10000)
    #define TVI_FIRST (-0x0FFFF)
    #define TVI_LAST  (-0x0FFFE)
    #define TVI_SORT  (-0x0FFFD)

    #define LPTV_INSERTSTRUCTA      LPTVINSERTSTRUCTA
    #define LPTV_INSERTSTRUCTW      LPTVINSERTSTRUCTW
    #define TV_INSERTSTRUCTA        TVINSERTSTRUCTA
    #define TV_INSERTSTRUCTW        TVINSERTSTRUCTW

    #define TV_INSERTSTRUCT         TVINSERTSTRUCT
    #define LPTV_INSERTSTRUCT       LPTVINSERTSTRUCT


    #define TVINSERTSTRUCTA_V1_SIZE CCSIZEOF_STRUCT(TVINSERTSTRUCTA, item)
    #define TVINSERTSTRUCTW_V1_SIZE CCSIZEOF_STRUCT(TVINSERTSTRUCTW, item)



    #ifdef UNICODE
      #define  TVINSERTSTRUCT         TVINSERTSTRUCTW
      #define  LPTVINSERTSTRUCT       LPTVINSERTSTRUCTW
      #define TVINSERTSTRUCT_V1_SIZE TVINSERTSTRUCTW_V1_SIZE
    #else
      #define  TVINSERTSTRUCT         TVINSERTSTRUCTA
      #define  LPTVINSERTSTRUCT       LPTVINSERTSTRUCTA
      #define TVINSERTSTRUCT_V1_SIZE TVINSERTSTRUCTA_V1_SIZE
    #endif

    #define TVM_INSERTITEMA         (TV_FIRST + 0)
    #define TVM_INSERTITEMW         (TV_FIRST + 50)
    #ifdef UNICODE
      #define  TVM_INSERTITEM         TVM_INSERTITEMW
    #else
      #define  TVM_INSERTITEM         TVM_INSERTITEMA
    #endif

    #define TreeView_InsertItem(hwnd, lpis) \


    #define TVM_DELETEITEM          (TV_FIRST + 1)
    #define TreeView_DeleteItem(hwnd, hitem) \


    #define TreeView_DeleteAllItems(hwnd) \


    #define TVM_EXPAND              (TV_FIRST + 2)
    #define TreeView_Expand(hwnd, hitem, code) \


    #define TVE_COLLAPSE                 1
    #define TVE_EXPAND                   2
    #define TVE_TOGGLE                   3
    #define TVE_EXPANDPARTIAL        16384
    #define TVE_COLLAPSERESET        32768


    #define TVM_GETITEMRECT         (TV_FIRST + 4)
    #define TreeView_GetItemRect(hwnd, hitem, prc, code) \


    #define TVM_GETCOUNT            (TV_FIRST + 5)
    #define TreeView_GetCount(hwnd) \


    #define TVM_GETINDENT           (TV_FIRST + 6)
    #define TreeView_GetIndent(hwnd) \


    #define TVM_SETINDENT           (TV_FIRST + 7)
    #define TreeView_SetIndent(hwnd, indent) \


    #define TVM_GETIMAGELIST        (TV_FIRST + 8)
    #define TreeView_GetImageList(hwnd, iImage) \


    #define TVSIL_NORMAL            0
    #define TVSIL_STATE             2


    #define TVM_SETIMAGELIST        (TV_FIRST + 9)
    #define TreeView_SetImageList(hwnd, himl, iImage) \


    #define TVM_GETNEXTITEM         (TV_FIRST + 10)
    #define TreeView_GetNextItem(hwnd, hitem, code) \


    #define TVGN_ROOT                    0
    #define TVGN_NEXT                    1
    #define TVGN_PREVIOUS                2
    #define TVGN_PARENT                  3
    #define TVGN_CHILD                   4
    #define TVGN_FIRSTVISIBLE            5
    #define TVGN_NEXTVISIBLE             6
    #define TVGN_PREVIOUSVISIBLE         7
    #define TVGN_DROPHILITE              8
    #define TVGN_CARET                   9
    #define TVGN_LASTVISIBLE            10

    #define TreeView_GetChild(hwnd, hitem)          TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD)
    #define TreeView_GetNextSibling(hwnd, hitem)    TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT)
    #define TreeView_GetPrevSibling(hwnd, hitem)    TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUS)
    #define TreeView_GetParent(hwnd, hitem)         TreeView_GetNextItem(hwnd, hitem, TVGN_PARENT)
    #define TreeView_GetFirstVisible(hwnd)          TreeView_GetNextItem(hwnd, NULL,  TVGN_FIRSTVISIBLE)
    #define TreeView_GetNextVisible(hwnd, hitem)    TreeView_GetNextItem(hwnd, hitem, TVGN_NEXTVISIBLE)
    #define TreeView_GetPrevVisible(hwnd, hitem)    TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUSVISIBLE)
    #define TreeView_GetSelection(hwnd)             TreeView_GetNextItem(hwnd, NULL,  TVGN_CARET)
    #define TreeView_GetDropHilight(hwnd)           TreeView_GetNextItem(hwnd, NULL,  TVGN_DROPHILITE)
    #define TreeView_GetRoot(hwnd)                  TreeView_GetNextItem(hwnd, NULL,  TVGN_ROOT)
    #define TreeView_GetLastVisible(hwnd)          TreeView_GetNextItem(hwnd, NULL,  TVGN_LASTVISIBLE)


    #define TVM_SELECTITEM          (TV_FIRST + 11)
    #define TreeView_Select(hwnd, hitem, code) \


    #define TreeView_SelectItem(hwnd, hitem)            TreeView_Select(hwnd, hitem, TVGN_CARET)
    #define TreeView_SelectDropTarget(hwnd, hitem)      TreeView_Select(hwnd, hitem, TVGN_DROPHILITE)
    #define TreeView_SelectSetFirstVisible(hwnd, hitem) TreeView_Select(hwnd, hitem, TVGN_FIRSTVISIBLE)

    #define TVM_GETITEMA            (TV_FIRST + 12)
    #define TVM_GETITEMW            (TV_FIRST + 62)

    #ifdef UNICODE
      #define  TVM_GETITEM            TVM_GETITEMW
    #else
      #define  TVM_GETITEM            TVM_GETITEMA
    #endif

    #define TreeView_GetItem(hwnd, pitem) \


    #define TVM_SETITEMA            (TV_FIRST + 13)
    #define TVM_SETITEMW            (TV_FIRST + 63)

    #ifdef UNICODE
      #define  TVM_SETITEM            TVM_SETITEMW
    #else
      #define  TVM_SETITEM            TVM_SETITEMA
    #endif

    #define TreeView_SetItem(hwnd, pitem) \


    #define TVM_EDITLABELA          (TV_FIRST + 14)
    #define TVM_EDITLABELW          (TV_FIRST + 65)
    #ifdef UNICODE
      #define TVM_EDITLABEL           TVM_EDITLABELW
    #else
      #define TVM_EDITLABEL           TVM_EDITLABELA
    #endif

    #define TreeView_EditLabel(hwnd, hitem) \


    #define TVM_GETEDITCONTROL      (TV_FIRST + 15)
    #define TreeView_GetEditControl(hwnd) \


    #define TVM_GETVISIBLECOUNT     (TV_FIRST + 16)
    #define TreeView_GetVisibleCount(hwnd) \


    #define TVM_HITTEST             (TV_FIRST + 17)


    #define LPTV_HITTESTINFO   LPTVHITTESTINFO
    #define   TV_HITTESTINFO     TVHITTESTINFO

    #define TVHT_NOWHERE                 1
    #define TVHT_ONITEMICON              2
    #define TVHT_ONITEMLABEL             4
    #define TVHT_ONITEM             (TVHT_ONITEMICON + TVHT_ONITEMLABEL + TVHT_ONITEMSTATEICON)
    #define TVHT_ONITEMINDENT            8
    #define TVHT_ONITEMBUTTON           16
    #define TVHT_ONITEMRIGHT            32
    #define TVHT_ONITEMSTATEICON        64

    #define TVHT_ABOVE                 256
    #define TVHT_BELOW                 512
    #define TVHT_TORIGHT              1024
    #define TVHT_TOLEFT               2048


    #define TVM_CREATEDRAGIMAGE     (TV_FIRST + 18)
    #define TreeView_CreateDragImage(hwnd, hitem) \


    #define TVM_SORTCHILDREN        (TV_FIRST + 19)
    #define TreeView_SortChildren(hwnd, hitem, recurse) \


    #define TVM_ENSUREVISIBLE       (TV_FIRST + 20)
    #define TreeView_EnsureVisible(hwnd, hitem) \


    #define TVM_SORTCHILDRENCB      (TV_FIRST + 21)
    #define TreeView_SortChildrenCB(hwnd, psort, recurse) \


    #define TVM_ENDEDITLABELNOW     (TV_FIRST + 22)
    #define TreeView_EndEditLabelNow(hwnd, fCancel) \


    #define TVM_GETISEARCHSTRINGA   (TV_FIRST + 23)
    #define TVM_GETISEARCHSTRINGW   (TV_FIRST + 64)

    #ifdef UNICODE
      #define TVM_GETISEARCHSTRING     TVM_GETISEARCHSTRINGW
    #else
      #define TVM_GETISEARCHSTRING     TVM_GETISEARCHSTRINGA
    #endif

    #define TVM_SETTOOLTIPS         (TV_FIRST + 24)
    #define TreeView_SetToolTips(hwnd,  hwndTT) \

    #define TVM_GETTOOLTIPS         (TV_FIRST + 25)
    #define TreeView_GetToolTips(hwnd) \

    #define TreeView_GetISearchString(hwndTV, lpsz) \


    #define TVM_SETINSERTMARK       (TV_FIRST + 26)
    #define TreeView_SetInsertMark(hwnd, hItem, fAfter) \


    #define TVM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    #define TreeView_SetUnicodeFormat(hwnd, fUnicode)  \


    #define TVM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
    #define TreeView_GetUnicodeFormat(hwnd)  \

    #define TVM_SETITEMHEIGHT         (TV_FIRST + 27)
    #define TreeView_SetItemHeight(hwnd,  iHeight) \

    #define TVM_GETITEMHEIGHT         (TV_FIRST + 28)
    #define TreeView_GetItemHeight(hwnd) \


    #define TVM_SETBKCOLOR              (TV_FIRST + 29)
    #define TreeView_SetBkColor(hwnd, clr) \


    #define TVM_SETTEXTCOLOR              (TV_FIRST + 30)
    #define TreeView_SetTextColor(hwnd, clr) \


    #define TVM_GETBKCOLOR              (TV_FIRST + 31)
    #define TreeView_GetBkColor(hwnd) \


    #define TVM_GETTEXTCOLOR              (TV_FIRST + 32)
    #define TreeView_GetTextColor(hwnd) \


    #define TVM_SETSCROLLTIME              (TV_FIRST + 33)
    #define TreeView_SetScrollTime(hwnd, uTime) \


    #define TVM_GETSCROLLTIME              (TV_FIRST + 34)
    #define TreeView_GetScrollTime(hwnd) \


    #define TVM_SETINSERTMARKCOLOR              (TV_FIRST + 37)
    #define TreeView_SetInsertMarkColor(hwnd, clr) \

    #define TVM_GETINSERTMARKCOLOR              (TV_FIRST + 38)
    #define TreeView_GetInsertMarkColor(hwnd) \


    // tvm_?etitemstate only uses mask, state and stateMask.
    // so unicode or ansi is irrelevant.
    #define TreeView_SetItemState(hwndTV, hti, data, _mask) \


    #define TreeView_SetCheckState(hwndTV, hti, fCheck) \


    #define TVM_GETITEMSTATE        (TV_FIRST + 39)
    #define TreeView_GetItemState(hwndTV, hti, mask) \


    #define TreeView_GetCheckState(hwndTV, hti) \


    #define TVM_SETLINECOLOR            (TV_FIRST + 40)
    #define TreeView_SetLineColor(hwnd, clr) \


    #define TVM_GETLINECOLOR            (TV_FIRST + 41)
    #define TreeView_GetLineColor(hwnd) \


    #define LPTV_SORTCB    LPTVSORTCB
    #define   TV_SORTCB      TVSORTCB


    #define LPNM_TREEVIEWA          LPNMTREEVIEWA
    #define LPNM_TREEVIEWW          LPNMTREEVIEWW
    #define NM_TREEVIEWW            NMTREEVIEWW
    #define NM_TREEVIEWA            NMTREEVIEWA

    #define LPNM_TREEVIEW           LPNMTREEVIEW
    #define NM_TREEVIEW             NMTREEVIEW


    #ifdef UNICODE
      #define  NMTREEVIEW             NMTREEVIEWW
      #define  LPNMTREEVIEW           LPNMTREEVIEWW
    #else
      #define  NMTREEVIEW             NMTREEVIEWA
      #define  LPNMTREEVIEW           LPNMTREEVIEWA
    #endif


    #define TVN_SELCHANGINGA        (TVN_FIRST-1)
    #define TVN_SELCHANGINGW        (TVN_FIRST-50)
    #define TVN_SELCHANGEDA         (TVN_FIRST-2)
    #define TVN_SELCHANGEDW         (TVN_FIRST-51)

    #define TVC_UNKNOWN                  0
    #define TVC_BYMOUSE                  1
    #define TVC_BYKEYBOARD               2

    #define TVN_GETDISPINFOA        (TVN_FIRST-3)
    #define TVN_GETDISPINFOW        (TVN_FIRST-52)
    #define TVN_SETDISPINFOA        (TVN_FIRST-4)
    #define TVN_SETDISPINFOW        (TVN_FIRST-53)

    #define TVIF_DI_SETITEM           4096

    #define TV_DISPINFOA            NMTVDISPINFOA
    #define TV_DISPINFOW            NMTVDISPINFOW

    #define TV_DISPINFO             NMTVDISPINFO


    #ifdef UNICODE
      #define NMTVDISPINFO            NMTVDISPINFOW
      #define LPNMTVDISPINFO          LPNMTVDISPINFOW
    #else
      #define NMTVDISPINFO            NMTVDISPINFOA
      #define LPNMTVDISPINFO          LPNMTVDISPINFOA
    #endif

    #define TVN_ITEMEXPANDINGA      (TVN_FIRST-5)
    #define TVN_ITEMEXPANDINGW      (TVN_FIRST-54)
    #define TVN_ITEMEXPANDEDA       (TVN_FIRST-6)
    #define TVN_ITEMEXPANDEDW       (TVN_FIRST-55)
    #define TVN_BEGINDRAGA          (TVN_FIRST-7)
    #define TVN_BEGINDRAGW          (TVN_FIRST-56)
    #define TVN_BEGINRDRAGA         (TVN_FIRST-8)
    #define TVN_BEGINRDRAGW         (TVN_FIRST-57)
    #define TVN_DELETEITEMA         (TVN_FIRST-9)
    #define TVN_DELETEITEMW         (TVN_FIRST-58)
    #define TVN_BEGINLABELEDITA     (TVN_FIRST-10)
    #define TVN_BEGINLABELEDITW     (TVN_FIRST-59)
    #define TVN_ENDLABELEDITA       (TVN_FIRST-11)
    #define TVN_ENDLABELEDITW       (TVN_FIRST-60)
    #define TVN_KEYDOWN             (TVN_FIRST-12)

    #define TVN_GETINFOTIPA         (TVN_FIRST-13)
    #define TVN_GETINFOTIPW         (TVN_FIRST-14)
    #define TVN_SINGLEEXPAND        (TVN_FIRST-15)

    #define TVNRET_DEFAULT          0
    #define TVNRET_SKIPOLD          1
    #define TVNRET_SKIPNEW          2


    #define TV_KEYDOWN      NMTVKEYDOWN


    #ifdef UNICODE
      #define TVN_SELCHANGING         TVN_SELCHANGINGW
      #define TVN_SELCHANGED          TVN_SELCHANGEDW
      #define TVN_GETDISPINFO         TVN_GETDISPINFOW
      #define TVN_SETDISPINFO         TVN_SETDISPINFOW
      #define TVN_ITEMEXPANDING       TVN_ITEMEXPANDINGW
      #define TVN_ITEMEXPANDED        TVN_ITEMEXPANDEDW
      #define TVN_BEGINDRAG           TVN_BEGINDRAGW
      #define TVN_BEGINRDRAG          TVN_BEGINRDRAGW
      #define TVN_DELETEITEM          TVN_DELETEITEMW
      #define TVN_BEGINLABELEDIT      TVN_BEGINLABELEDITW
      #define TVN_ENDLABELEDIT        TVN_ENDLABELEDITW
    #else
      #define TVN_SELCHANGING         TVN_SELCHANGINGA
      #define TVN_SELCHANGED          TVN_SELCHANGEDA
      #define TVN_GETDISPINFO         TVN_GETDISPINFOA
      #define TVN_SETDISPINFO         TVN_SETDISPINFOA
      #define TVN_ITEMEXPANDING       TVN_ITEMEXPANDINGA
      #define TVN_ITEMEXPANDED        TVN_ITEMEXPANDEDA
      #define TVN_BEGINDRAG           TVN_BEGINDRAGA
      #define TVN_BEGINRDRAG          TVN_BEGINRDRAGA
      #define TVN_DELETEITEM          TVN_DELETEITEMA
      #define TVN_BEGINLABELEDIT      TVN_BEGINLABELEDITA
      #define TVN_ENDLABELEDIT        TVN_ENDLABELEDITA
    #endif

    #define NMTVCUSTOMDRAW_V3_SIZE CCSIZEOF_STRUCT(NMTVCUSTOMDRAW, clrTextBk)

    // for tooltips


    #ifdef UNICODE
      #define TVN_GETINFOTIP          TVN_GETINFOTIPW
      #define NMTVGETINFOTIP          NMTVGETINFOTIPW
      #define LPNMTVGETINFOTIP        LPNMTVGETINFOTIPW
    #else
      #define TVN_GETINFOTIP          TVN_GETINFOTIPA
      #define NMTVGETINFOTIP          NMTVGETINFOTIPA
      #define LPNMTVGETINFOTIP        LPNMTVGETINFOTIPA
    #endif

    // treeview's customdraw return meaning don't draw images.  valid on CDRF_NOTIFYITEMPREPAINT
    #define TVCDRF_NOIMAGES              65536

  #endif      // NOTREEVIEW


  #ifndef NOUSEREXCONTROLS

    ////////////////////  ComboBoxEx ////////////////////////////////


    #define WC_COMBOBOXEXW         L"ComboBoxEx32"
    #define WC_COMBOBOXEXA         "ComboBoxEx32"

    #ifdef UNICODE
      #define WC_COMBOBOXEX          WC_COMBOBOXEXW
    #else
      #define WC_COMBOBOXEX          WC_COMBOBOXEXA
    #endif

    #define CBEIF_TEXT                       1
    #define CBEIF_IMAGE                      2
    #define CBEIF_SELECTEDIMAGE              4
    #define CBEIF_OVERLAY                    8
    #define CBEIF_INDENT                    16
    #define CBEIF_LPARAM                    32

    #define CBEIF_DI_SETITEM         268435456


    #ifdef UNICODE
      #define COMBOBOXEXITEM            COMBOBOXEXITEMW
      #define PCOMBOBOXEXITEM           PCOMBOBOXEXITEMW
      #define PCCOMBOBOXEXITEM          PCCOMBOBOXEXITEMW
    #else
      #define COMBOBOXEXITEM            COMBOBOXEXITEMA
      #define PCOMBOBOXEXITEM           PCOMBOBOXEXITEMA
      #define PCCOMBOBOXEXITEM          PCCOMBOBOXEXITEMA
    #endif

    #define CBEM_INSERTITEMA        (WM_USER + 1)
    #define CBEM_SETIMAGELIST       (WM_USER + 2)
    #define CBEM_GETIMAGELIST       (WM_USER + 3)
    #define CBEM_GETITEMA           (WM_USER + 4)
    #define CBEM_SETITEMA           (WM_USER + 5)
    #define CBEM_DELETEITEM         CB_DELETESTRING
    #define CBEM_GETCOMBOCONTROL    (WM_USER + 6)
    #define CBEM_GETEDITCONTROL     (WM_USER + 7)
    #define CBEM_SETEXSTYLE         (WM_USER + 8)  // use  SETEXTENDEDSTYLE instead
    #define CBEM_SETEXTENDEDSTYLE   (WM_USER + 14)   // lparam == new style, wParam (optional) == mask
    #define CBEM_GETEXSTYLE         (WM_USER + 9) // use GETEXTENDEDSTYLE instead
    #define CBEM_GETEXTENDEDSTYLE   (WM_USER + 9)
    #define CBEM_SETUNICODEFORMAT   CCM_SETUNICODEFORMAT
    #define CBEM_GETUNICODEFORMAT   CCM_GETUNICODEFORMAT
    #define CBEM_HASEDITCHANGED     (WM_USER + 10)
    #define CBEM_INSERTITEMW        (WM_USER + 11)
    #define CBEM_SETITEMW           (WM_USER + 12)
    #define CBEM_GETITEMW           (WM_USER + 13)

    #ifdef UNICODE
      #define CBEM_INSERTITEM         CBEM_INSERTITEMW
      #define CBEM_SETITEM            CBEM_SETITEMW
      #define CBEM_GETITEM            CBEM_GETITEMW
    #else
      #define CBEM_INSERTITEM         CBEM_INSERTITEMA
      #define CBEM_SETITEM            CBEM_SETITEMA
      #define CBEM_GETITEM            CBEM_GETITEMA
    #endif

    #define CBES_EX_NOEDITIMAGE                   1
    #define CBES_EX_NOEDITIMAGEINDENT             2
    #define CBES_EX_PATHWORDBREAKPROC             4
    #define CBES_EX_NOSIZELIMIT                   8
    #define CBES_EX_CASESENSITIVE                16


    #ifdef UNICODE
      #define NMCOMBOBOXEX            NMCOMBOBOXEXW
      #define PNMCOMBOBOXEX           PNMCOMBOBOXEXW
      #define CBEN_GETDISPINFO        CBEN_GETDISPINFOW
    #else
      #define NMCOMBOBOXEX            NMCOMBOBOXEXA
      #define PNMCOMBOBOXEX           PNMCOMBOBOXEXA
      #define CBEN_GETDISPINFO        CBEN_GETDISPINFOA
    #endif

    #define CBEN_GETDISPINFOA        (CBEN_FIRST - 0)
    #define CBEN_INSERTITEM          (CBEN_FIRST - 1)
    #define CBEN_DELETEITEM          (CBEN_FIRST - 2)
    #define CBEN_BEGINEDIT           (CBEN_FIRST - 4)
    #define CBEN_ENDEDITA            (CBEN_FIRST - 5)
    #define CBEN_ENDEDITW            (CBEN_FIRST - 6)

    #define CBEN_GETDISPINFOW        (CBEN_FIRST - 7)

    #define CBEN_DRAGBEGINA                  (CBEN_FIRST - 8)
    #define CBEN_DRAGBEGINW                  (CBEN_FIRST - 9)

    #ifdef UNICODE
      #define CBEN_DRAGBEGIN CBEN_DRAGBEGINW
    #else
      #define CBEN_DRAGBEGIN CBEN_DRAGBEGINA
    #endif

    // lParam specifies why the endedit is happening
    #ifdef UNICODE
      #define CBEN_ENDEDIT CBEN_ENDEDITW
    #else
      #define CBEN_ENDEDIT CBEN_ENDEDITA
    #endif

    #define CBENF_KILLFOCUS         1
    #define CBENF_RETURN            2
    #define CBENF_ESCAPE            3
    #define CBENF_DROPDOWN          4

    #define CBEMAXSTRLEN 260

    // CBEN_DRAGBEGIN sends this information ...


    #ifdef UNICODE
      #define  NMCBEDRAGBEGIN NMCBEDRAGBEGINW
      #define  LPNMCBEDRAGBEGIN LPNMCBEDRAGBEGINW
      #define  PNMCBEDRAGBEGIN PNMCBEDRAGBEGINW
    #else
      #define  NMCBEDRAGBEGIN NMCBEDRAGBEGINA
      #define  LPNMCBEDRAGBEGIN LPNMCBEDRAGBEGINA
      #define  PNMCBEDRAGBEGIN PNMCBEDRAGBEGINA
    #endif

    // CBEN_ENDEDIT sends this information...
    // fChanged if the user actually did anything
    // iNewSelection gives what would be the new selection unless the notify is failed
    //                      iNewSelection may be CB_ERR if there's no match


    #ifdef UNICODE
      #define  NMCBEENDEDIT NMCBEENDEDITW
      #define  LPNMCBEENDEDIT LPNMCBEENDEDITW
      #define  PNMCBEENDEDIT PNMCBEENDEDITW
    #else
      #define  NMCBEENDEDIT NMCBEENDEDITA
      #define  LPNMCBEENDEDIT LPNMCBEENDEDITA
      #define  PNMCBEENDEDIT PNMCBEENDEDITA
    #endif

  #endif //NOUSEREXCONTROLS

  //====== TAB CONTROL ==========================================================

  #ifndef NOTABCONTROL

    #define WC_TABCONTROLA          "SysTabControl32"
    #define WC_TABCONTROLW          L"SysTabControl32"

    #ifdef UNICODE
      #define  WC_TABCONTROL          WC_TABCONTROLW
    #else
      #define  WC_TABCONTROL          WC_TABCONTROLA
    #endif

    // begin_r_commctrl

    #define TCS_SCROLLOPPOSITE           1   // assumes multiline tab
    #define TCS_BOTTOM                   2
    #define TCS_RIGHT                    2
    #define TCS_MULTISELECT              4  // allow multi-select in button mode
    #define TCS_FLATBUTTONS              8
    #define TCS_FORCEICONLEFT           16
    #define TCS_FORCELABELLEFT          32
    #define TCS_HOTTRACK                64
    #define TCS_VERTICAL               128
    #define TCS_TABS                     0
    #define TCS_BUTTONS                256
    #define TCS_SINGLELINE               0
    #define TCS_MULTILINE              512
    #define TCS_RIGHTJUSTIFY             0
    #define TCS_FIXEDWIDTH            1024
    #define TCS_RAGGEDRIGHT           2048
    #define TCS_FOCUSONBUTTONDOWN     4096
    #define TCS_OWNERDRAWFIXED        8192
    #define TCS_TOOLTIPS             16384
    #define TCS_FOCUSNEVER           32768

    // end_r_commctrl

    // EX styles for use with TCM_SETEXTENDEDSTYLE
    #define TCS_EX_FLATSEPARATORS            1
    #define TCS_EX_REGISTERDROP              2


    #define TCM_GETIMAGELIST        (TCM_FIRST + 2)
    //#define TabCtrl_GetImageList(hwnd) \


    #define TCM_SETIMAGELIST        (TCM_FIRST + 3)
    //#define TabCtrl_SetImageList(hwnd, himl) \


    #define TCM_GETITEMCOUNT        (TCM_FIRST + 4)
    //#define TabCtrl_GetItemCount(hwnd) \


    #define TCIF_TEXT                    1
    #define TCIF_IMAGE                   2
    #define TCIF_RTLREADING              4
    #define TCIF_PARAM                   8
    #define TCIF_STATE                  16


    #define TCIS_BUTTONPRESSED           1
    #define TCIS_HIGHLIGHTED             2

    #define TC_ITEMHEADERA         TCITEMHEADERA
    #define TC_ITEMHEADERW         TCITEMHEADERW
    #define TC_ITEMHEADER          TCITEMHEADER


    #ifdef UNICODE
      #define  TCITEMHEADER          TCITEMHEADERW
      #define  LPTCITEMHEADER        LPTCITEMHEADERW
    #else
      #define  TCITEMHEADER          TCITEMHEADERA
      #define  LPTCITEMHEADER        LPTCITEMHEADERA
    #endif


    #define TC_ITEMA                TCITEMA
    #define TC_ITEMW                TCITEMW
    #define TC_ITEM                 TCITEM


    #ifdef UNICODE
      #define  TCITEM                 TCITEMW
      #define  LPTCITEM               LPTCITEMW
    #else
      #define  TCITEM                 TCITEMA
      #define  LPTCITEM               LPTCITEMA
    #endif


    #define TCM_GETITEMA            (TCM_FIRST + 5)
    #define TCM_GETITEMW            (TCM_FIRST + 60)

    #ifdef UNICODE
      #define TCM_GETITEM             TCM_GETITEMW
    #else
      #define TCM_GETITEM             TCM_GETITEMA
    #endif

    //#define TabCtrl_GetItem(hwnd, iItem, pitem) \


    #define TCM_SETITEMA            (TCM_FIRST + 6)
    #define TCM_SETITEMW            (TCM_FIRST + 61)

    #ifdef UNICODE
      #define TCM_SETITEM             TCM_SETITEMW
    #else
      #define TCM_SETITEM             TCM_SETITEMA
    #endif

    //#define TabCtrl_SetItem(hwnd, iItem, pitem) \


    #define TCM_INSERTITEMA         (TCM_FIRST + 7)
    #define TCM_INSERTITEMW         (TCM_FIRST + 62)

    #ifdef UNICODE
      #define TCM_INSERTITEM          TCM_INSERTITEMW
    #else
      #define TCM_INSERTITEM          TCM_INSERTITEMA
    #endif

    //#define TabCtrl_InsertItem(hwnd, iItem, pitem)   \


    #define TCM_DELETEITEM          (TCM_FIRST + 8)
    //#define TabCtrl_DeleteItem(hwnd, i) \


    #define TCM_DELETEALLITEMS      (TCM_FIRST + 9)
    //#define TabCtrl_DeleteAllItems(hwnd) \


    #define TCM_GETITEMRECT         (TCM_FIRST + 10)
    //#define TabCtrl_GetItemRect(hwnd, i, prc) \


    #define TCM_GETCURSEL           (TCM_FIRST + 11)
    //#define TabCtrl_GetCurSel(hwnd) \


    #define TCM_SETCURSEL           (TCM_FIRST + 12)
    //#define TabCtrl_SetCurSel(hwnd, i) \


    #define TCHT_NOWHERE                 1
    #define TCHT_ONITEMICON              2
    #define TCHT_ONITEMLABEL             4
    #define TCHT_ONITEM             (TCHT_ONITEMICON + TCHT_ONITEMLABEL)

    #define LPTC_HITTESTINFO        LPTCHITTESTINFO
    #define TC_HITTESTINFO          TCHITTESTINFO

    #define TCM_HITTEST             (TCM_FIRST + 13)
    //#define TabCtrl_HitTest(hwndTC, pinfo) \


    #define TCM_SETITEMEXTRA        (TCM_FIRST + 14)
    //#define TabCtrl_SetItemExtra(hwndTC, cb) \


    #define TCM_ADJUSTRECT          (TCM_FIRST + 40)
    //#define TabCtrl_AdjustRect(hwnd, bLarger, prc) \


    #define TCM_SETITEMSIZE         (TCM_FIRST + 41)
    //#define TabCtrl_SetItemSize(hwnd, x, y) \


    #define TCM_REMOVEIMAGE         (TCM_FIRST + 42)
    //#define TabCtrl_RemoveImage(hwnd, i) \


    #define TCM_SETPADDING          (TCM_FIRST + 43)
    //#define TabCtrl_SetPadding(hwnd,  cx, cy) \


    #define TCM_GETROWCOUNT         (TCM_FIRST + 44)
    //#define TabCtrl_GetRowCount(hwnd) \


    #define TCM_GETTOOLTIPS         (TCM_FIRST + 45)
    //#define TabCtrl_GetToolTips(hwnd) \


    #define TCM_SETTOOLTIPS         (TCM_FIRST + 46)
    //#define TabCtrl_SetToolTips(hwnd, hwndTT) \


    #define TCM_GETCURFOCUS         (TCM_FIRST + 47)
    //#define TabCtrl_GetCurFocus(hwnd) \


    #define TCM_SETCURFOCUS         (TCM_FIRST + 48)
    //#define TabCtrl_SetCurFocus(hwnd, i) \


    #define TCM_SETMINTABWIDTH      (TCM_FIRST + 49)
    //#define TabCtrl_SetMinTabWidth(hwnd, x) \


    #define TCM_DESELECTALL         (TCM_FIRST + 50)
    //#define TabCtrl_DeselectAll(hwnd, fExcludeFocus)\

    #define TCM_HIGHLIGHTITEM       (TCM_FIRST + 51)
    //#define TabCtrl_HighlightItem(hwnd, i, fHighlight) \


    #define TCM_SETEXTENDEDSTYLE    (TCM_FIRST + 52)  // optional wParam == mask
    //#define TabCtrl_SetExtendedStyle(hwnd, dw)\


    #define TCM_GETEXTENDEDSTYLE    (TCM_FIRST + 53)
    //#define TabCtrl_GetExtendedStyle(hwnd)\


    #define TCM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    //#define TabCtrl_SetUnicodeFormat(hwnd, fUnicode)  \


    #define TCM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
    //#define TabCtrl_GetUnicodeFormat(hwnd)  \


    #define TCN_KEYDOWN             (TCN_FIRST - 0)

    #define TC_KEYDOWN              NMTCKEYDOWN

    #define TCN_SELCHANGE           (TCN_FIRST - 1)
    #define TCN_SELCHANGING         (TCN_FIRST - 2)
    #define TCN_GETOBJECT           (TCN_FIRST - 3)
    #define TCN_FOCUSCHANGE         (TCN_FIRST - 4)

  #endif      // NOTABCONTROL


  //====== ANIMATE CONTROL ======================================================

  #ifndef NOANIMATE

    #define ANIMATE_CLASSW          L"SysAnimate32"
    #define ANIMATE_CLASSA          "SysAnimate32"

    #ifdef UNICODE
      #define ANIMATE_CLASS           ANIMATE_CLASSW
    #else
      #define ANIMATE_CLASS           ANIMATE_CLASSA
    #endif

    // begin_r_commctrl

    #define ACS_CENTER                   1
    #define ACS_TRANSPARENT              2
    #define ACS_AUTOPLAY                 4
    #define ACS_TIMER                    8  // don't use threads... use timers

    // end_r_commctrl

    #define ACM_OPENA               (WM_USER+100)
    #define ACM_OPENW               (WM_USER+103)

    #ifdef UNICODE
      #define ACM_OPEN                ACM_OPENW
    #else
      #define ACM_OPEN                ACM_OPENA
    #endif

    #define ACM_PLAY                (WM_USER+101)
    #define ACM_STOP                (WM_USER+102)


    #define ACN_START               1
    #define ACN_STOP                2


    #define Animate_Create(hwndP, id, dwStyle, hInstance)   \


    #define Animate_Open(hwnd, szName)          (BOOL)SNDMSG(hwnd, ACM_OPEN, 0, (LPARAM)(LPTSTR)(szName))
    #define Animate_OpenEx(hwnd, hInst, szName) (BOOL)SNDMSG(hwnd, ACM_OPEN, (WPARAM)(hInst), (LPARAM)(LPTSTR)(szName))
    #define Animate_Play(hwnd, from, to, rep)   (BOOL)SNDMSG(hwnd, ACM_PLAY, (WPARAM)(rep), (LPARAM)MAKELONG(from, to))
    #define Animate_Stop(hwnd)                  (BOOL)SNDMSG(hwnd, ACM_STOP, 0, 0)
    #define Animate_Close(hwnd)                 Animate_Open(hwnd, NULL)
    #define Animate_Seek(hwnd, frame)           Animate_Play(hwnd, frame, frame, 1)

  #endif      // NOANIMATE

  //====== MONTHCAL CONTROL ======================================================

  #ifndef NOMONTHCAL

    #define MONTHCAL_CLASSW          L"SysMonthCal32"
    #define MONTHCAL_CLASSA          "SysMonthCal32"

    #ifdef UNICODE
      #define MONTHCAL_CLASS           MONTHCAL_CLASSW
    #else
      #define MONTHCAL_CLASS           MONTHCAL_CLASSA
    #endif

    // bit-packed array of "bold" info for a month
    // if a bit is on, that day is drawn bold


    #define MCM_FIRST             4096

    // BOOL MonthCal_GetCurSel(HWND hmc, LPSYSTEMTIME pst)
    //   returns FALSE if MCS_MULTISELECT
    //   returns TRUE and sets *pst to the currently selected date otherwise
    #define MCM_GETCURSEL       (MCM_FIRST + 1)
    #define MonthCal_GetCurSel(hmc, pst)    (BOOL)SNDMSG(hmc, MCM_GETCURSEL, 0, (LPARAM)(pst))

    // BOOL MonthCal_SetCurSel(HWND hmc, LPSYSTEMTIME pst)
    //   returns FALSE if MCS_MULTISELECT
    //   returns TURE and sets the currently selected date to *pst otherwise
    #define MCM_SETCURSEL       (MCM_FIRST + 2)
    #define MonthCal_SetCurSel(hmc, pst)    (BOOL)SNDMSG(hmc, MCM_SETCURSEL, 0, (LPARAM)(pst))

    // DWORD MonthCal_GetMaxSelCount(HWND hmc)
    //   returns the maximum number of selectable days allowed
    #define MCM_GETMAXSELCOUNT  (MCM_FIRST + 3)
    #define MonthCal_GetMaxSelCount(hmc)    (DWORD)SNDMSG(hmc, MCM_GETMAXSELCOUNT, 0, 0L)

    // BOOL MonthCal_SetMaxSelCount(HWND hmc, UINT n)
    //   sets the max number days that can be selected iff MCS_MULTISELECT
    #define MCM_SETMAXSELCOUNT  (MCM_FIRST + 4)
    #define MonthCal_SetMaxSelCount(hmc, n) (BOOL)SNDMSG(hmc, MCM_SETMAXSELCOUNT, (WPARAM)(n), 0L)

    // BOOL MonthCal_GetSelRange(HWND hmc, LPSYSTEMTIME rgst)
    //   sets rgst[0] to the first day of the selection range
    //   sets rgst[1] to the last day of the selection range
    #define MCM_GETSELRANGE     (MCM_FIRST + 5)
    #define MonthCal_GetSelRange(hmc, rgst) SNDMSG(hmc, MCM_GETSELRANGE, 0, (LPARAM)(rgst))

    // BOOL MonthCal_SetSelRange(HWND hmc, LPSYSTEMTIME rgst)
    //   selects the range of days from rgst[0] to rgst[1]
    #define MCM_SETSELRANGE     (MCM_FIRST + 6)
    #define MonthCal_SetSelRange(hmc, rgst) SNDMSG(hmc, MCM_SETSELRANGE, 0, (LPARAM)(rgst))

    // DWORD MonthCal_GetMonthRange(HWND hmc, DWORD gmr, LPSYSTEMTIME rgst)
    //   if rgst specified, sets rgst[0] to the starting date and
    //      and rgst[1] to the ending date of the the selectable (non-grayed)
    //      days if GMR_VISIBLE or all the displayed days (including grayed)
    //      if GMR_DAYSTATE.
    //   returns the number of months spanned by the above range.
    #define MCM_GETMONTHRANGE   (MCM_FIRST + 7)
    #define MonthCal_GetMonthRange(hmc, gmr, rgst)  (DWORD)SNDMSG(hmc, MCM_GETMONTHRANGE, (WPARAM)(gmr), (LPARAM)(rgst))

    // BOOL MonthCal_SetDayState(HWND hmc, int cbds, DAYSTATE *rgds)
    //   cbds is the count of DAYSTATE items in rgds and it must be equal
    //   to the value returned from MonthCal_GetMonthRange(hmc, GMR_DAYSTATE, NULL)
    //   This sets the DAYSTATE bits for each month (grayed and non-grayed
    //   days) displayed in the calendar. The first bit in a month's DAYSTATE
    //   corresponts to bolding day 1, the second bit affects day 2, etc.
    #define MCM_SETDAYSTATE     (MCM_FIRST + 8)
    #define MonthCal_SetDayState(hmc, cbds, rgds)   SNDMSG(hmc, MCM_SETDAYSTATE, (WPARAM)(cbds), (LPARAM)(rgds))

    // BOOL MonthCal_GetMinReqRect(HWND hmc, LPRECT prc)
    //   sets *prc the minimal size needed to display one month
    //   To display two months, undo the AdjustWindowRect calculation already done to
    //   this rect, double the width, and redo the AdjustWindowRect calculation --
    //   the monthcal control will display two calendars in this window (if you also
    //   double the vertical size, you will get 4 calendars)
    //   NOTE: if you want to gurantee that the "Today" string is not clipped,
    //   get the MCM_GETMAXTODAYWIDTH and use the max of that width and this width
    #define MCM_GETMINREQRECT   (MCM_FIRST + 9)
    #define MonthCal_GetMinReqRect(hmc, prc)        SNDMSG(hmc, MCM_GETMINREQRECT, 0, (LPARAM)(prc))

    // set colors to draw control with -- see MCSC_ bits below
    #define MCM_SETCOLOR            (MCM_FIRST + 10)
    #define MonthCal_SetColor(hmc, iColor, clr) SNDMSG(hmc, MCM_SETCOLOR, iColor, clr)

    #define MCM_GETCOLOR            (MCM_FIRST + 11)
    #define MonthCal_GetColor(hmc, iColor) SNDMSG(hmc, MCM_GETCOLOR, iColor, 0)

    #define MCSC_BACKGROUND   0   // the background color (between months)
    #define MCSC_TEXT         1   // the dates
    #define MCSC_TITLEBK      2   // background of the title
    #define MCSC_TITLETEXT    3
    #define MCSC_MONTHBK      4   // background within the month cal
    #define MCSC_TRAILINGTEXT 5   // the text color of header & trailing days

    // set what day is "today"   send NULL to revert back to real date
    #define MCM_SETTODAY    (MCM_FIRST + 12)
    #define MonthCal_SetToday(hmc, pst)             SNDMSG(hmc, MCM_SETTODAY, 0, (LPARAM)(pst))

    // get what day is "today"
    // returns BOOL for success/failure
    #define MCM_GETTODAY    (MCM_FIRST + 13)
    #define MonthCal_GetToday(hmc, pst)             (BOOL)SNDMSG(hmc, MCM_GETTODAY, 0, (LPARAM)(pst))

    // determine what pinfo->pt is over
    #define MCM_HITTEST          (MCM_FIRST + 14)
    #define MonthCal_HitTest(hmc, pinfo) \


              // out param


    #define MCHT_TITLE                           65536
    #define MCHT_CALENDAR                       131072
    #define MCHT_TODAYLINK                      196608

    #define MCHT_NEXT                         16777216   // these indicate that hitting
    #define MCHT_PREV                         33554432  // here will go to the next/prev month

    #define MCHT_NOWHERE                             0

    #define MCHT_TITLEBK                    (MCHT_TITLE)
    #define MCHT_TITLEMONTH                 (MCHT_TITLE +      1)
    #define MCHT_TITLEYEAR                  (MCHT_TITLE +      2)
    #define MCHT_TITLEBTNNEXT               (MCHT_TITLE + MCHT_NEXT +      3)
    #define MCHT_TITLEBTNPREV               (MCHT_TITLE + MCHT_PREV +      3)

    #define MCHT_CALENDARBK                 (MCHT_CALENDAR)
    #define MCHT_CALENDARDATE               (MCHT_CALENDAR +      1)
    #define MCHT_CALENDARDATENEXT           (MCHT_CALENDARDATE + MCHT_NEXT)
    #define MCHT_CALENDARDATEPREV           (MCHT_CALENDARDATE + MCHT_PREV)
    #define MCHT_CALENDARDAY                (MCHT_CALENDAR +      2)
    #define MCHT_CALENDARWEEKNUM            (MCHT_CALENDAR +      3)

    // set first day of week to iDay:
    // 0 for Monday, 1 for Tuesday, ..., 6 for Sunday
    // -1 for means use locale info
    #define MCM_SETFIRSTDAYOFWEEK (MCM_FIRST + 15)
    #define MonthCal_SetFirstDayOfWeek(hmc, iDay) \


    // DWORD result...  low word has the day.  high word is bool if this is app set
    // or not (FALSE == using locale info)
    #define MCM_GETFIRSTDAYOFWEEK (MCM_FIRST + 16)
    #define MonthCal_GetFirstDayOfWeek(hmc) \


    // DWORD MonthCal_GetRange(HWND hmc, LPSYSTEMTIME rgst)
    //   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
    //   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
    //   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit
    #define MCM_GETRANGE (MCM_FIRST + 17)
    #define MonthCal_GetRange(hmc, rgst) \


    // BOOL MonthCal_SetRange(HWND hmc, DWORD gdtr, LPSYSTEMTIME rgst)
    //   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
    //   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
    //   returns TRUE on success, FALSE on error (such as invalid parameters)
    #define MCM_SETRANGE (MCM_FIRST + 18)
    #define MonthCal_SetRange(hmc, gd, rgst) \


    // int MonthCal_GetMonthDelta(HWND hmc)
    //   returns the number of months one click on a next/prev button moves by
    #define MCM_GETMONTHDELTA (MCM_FIRST + 19)
    #define MonthCal_GetMonthDelta(hmc) \


    // int MonthCal_SetMonthDelta(HWND hmc, int n)
    //   sets the month delta to n. n==0 reverts to moving by a page of months
    //   returns the previous value of n.
    #define MCM_SETMONTHDELTA (MCM_FIRST + 20)
    #define MonthCal_SetMonthDelta(hmc, n) \


    // DWORD MonthCal_GetMaxTodayWidth(HWND hmc, LPSIZE psz)
    //   sets *psz to the maximum width/height of the "Today" string displayed
    //   at the bottom of the calendar (as long as MCS_NOTODAY is not specified)
    #define MCM_GETMAXTODAYWIDTH (MCM_FIRST + 21)
    #define MonthCal_GetMaxTodayWidth(hmc) \


    #define MCM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
    #define MonthCal_SetUnicodeFormat(hwnd, fUnicode)  \


    #define MCM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
    #define MonthCal_GetUnicodeFormat(hwnd)  \

    // MCN_SELCHANGE is sent whenever the currently displayed date changes
    // via month change, year change, keyboard navigation, prev/next button
    //


         // this must be first, so we don't break WM_NOTIFY


    #define MCN_SELCHANGE       (MCN_FIRST + 1)

    // MCN_GETDAYSTATE is sent for MCS_DAYSTATE controls whenever new daystate
    // information is needed (month or year scroll) to draw bolding information.
    // The app must fill in cDayState months worth of information starting from
    // stStart date. The app may fill in the array at prgDayState or change
    // prgDayState to point to a different array out of which the information
    // will be copied. (similar to tooltips)
    //


         // this must be first, so we don't break WM_NOTIFY


        // points to cDayState MONTHDAYSTATEs


    #define MCN_GETDAYSTATE     (MCN_FIRST + 3)

    // MCN_SELECT is sent whenever a selection has occured (via mouse or keyboard)
    //


    #define MCN_SELECT          (MCN_FIRST + 4)


    // begin_r_commctrl

    #define MCS_DAYSTATE             1
    #define MCS_MULTISELECT          2
    #define MCS_WEEKNUMBERS          4
    #define MCS_NOTODAYCIRCLE        8
    #define MCS_NOTODAY             16


    // end_r_commctrl

    #define GMR_VISIBLE     0       // visible portion of display
    #define GMR_DAYSTATE    1       // above plus the grayed out parts of
                                 // partially displayed months

  #endif // NOMONTHCAL


  //====== DATETIMEPICK CONTROL ==================================================

  #ifndef NODATETIMEPICK

    #define DATETIMEPICK_CLASSW          L"SysDateTimePick32"
    #define DATETIMEPICK_CLASSA          "SysDateTimePick32"

    #ifdef UNICODE
      #define DATETIMEPICK_CLASS           DATETIMEPICK_CLASSW
    #else
      #define DATETIMEPICK_CLASS           DATETIMEPICK_CLASSA
    #endif

    #define DTM_FIRST          4096

    // DWORD DateTimePick_GetSystemtime(HWND hdp, LPSYSTEMTIME pst)
    //   returns GDT_NONE if "none" is selected (DTS_SHOWNONE only)
    //   returns GDT_VALID and modifies *pst to be the currently selected value
    #define DTM_GETSYSTEMTIME   (DTM_FIRST + 1)
    #define DateTime_GetSystemtime(hdp, pst)    (DWORD)SNDMSG(hdp, DTM_GETSYSTEMTIME, 0, (LPARAM)(pst))

    // BOOL DateTime_SetSystemtime(HWND hdp, DWORD gd, LPSYSTEMTIME pst)
    //   if gd==GDT_NONE, sets datetimepick to None (DTS_SHOWNONE only)
    //   if gd==GDT_VALID, sets datetimepick to *pst
    //   returns TRUE on success, FALSE on error (such as bad params)
    #define DTM_SETSYSTEMTIME   (DTM_FIRST + 2)
    #define DateTime_SetSystemtime(hdp, gd, pst)    (BOOL)SNDMSG(hdp, DTM_SETSYSTEMTIME, (WPARAM)(gd), (LPARAM)(pst))

    // DWORD DateTime_GetRange(HWND hdp, LPSYSTEMTIME rgst)
    //   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
    //   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
    //   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit
    #define DTM_GETRANGE (DTM_FIRST + 3)
    #define DateTime_GetRange(hdp, rgst)  (DWORD)SNDMSG(hdp, DTM_GETRANGE, 0, (LPARAM)(rgst))

    // BOOL DateTime_SetRange(HWND hdp, DWORD gdtr, LPSYSTEMTIME rgst)
    //   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
    //   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
    //   returns TRUE on success, FALSE on error (such as invalid parameters)
    #define DTM_SETRANGE (DTM_FIRST + 4)
    //#define DateTime_SetRange(hdp, gd, rgst)  (BOOL)SNDMSG(hdp, DTM_SETRANGE, (WPARAM)(gd), (LPARAM)(rgst))

    // BOOL DateTime_SetFormat(HWND hdp, LPCTSTR sz)
    //   sets the display formatting string to sz (see GetDateFormat and GetTimeFormat for valid formatting chars)
    //   NOTE: 'X' is a valid formatting character which indicates that the application
    //   will determine how to display information. Such apps must support DTN_WMKEYDOWN,
    //   DTN_FORMAT, and DTN_FORMATQUERY.
    #define DTM_SETFORMATA (DTM_FIRST + 5)
    #define DTM_SETFORMATW (DTM_FIRST + 50)

    #ifdef UNICODE
      #define DTM_SETFORMAT       DTM_SETFORMATW
    #else
      #define DTM_SETFORMAT       DTM_SETFORMATA
    #endif

    #define DateTime_SetFormat(hdp, sz)  (BOOL)SNDMSG(hdp, DTM_SETFORMAT, 0, (LPARAM)(sz))


    #define DTM_SETMCCOLOR    (DTM_FIRST + 6)
    #define DateTime_SetMonthCalColor(hdp, iColor, clr) SNDMSG(hdp, DTM_SETMCCOLOR, iColor, clr)

    #define DTM_GETMCCOLOR    (DTM_FIRST + 7)
    #define DateTime_GetMonthCalColor(hdp, iColor) SNDMSG(hdp, DTM_GETMCCOLOR, iColor, 0)

    // HWND DateTime_GetMonthCal(HWND hdp)
    //   returns the HWND of the MonthCal popup window. Only valid
    // between DTN_DROPDOWN and DTN_CLOSEUP notifications.
    #define DTM_GETMONTHCAL   (DTM_FIRST + 8)
    #define DateTime_GetMonthCal(hdp) (HWND)SNDMSG(hdp, DTM_GETMONTHCAL, 0, 0)

    #define DTM_SETMCFONT     (DTM_FIRST + 9)
    #define DateTime_SetMonthCalFont(hdp, hfont, fRedraw) SNDMSG(hdp, DTM_SETMCFONT, (WPARAM)(hfont), (LPARAM)(fRedraw))

    #define DTM_GETMCFONT     (DTM_FIRST + 10)
    #define DateTime_GetMonthCalFont(hdp) SNDMSG(hdp, DTM_GETMCFONT, 0, 0)

    // begin_r_commctrl

    #define DTS_UPDOWN               1 // use UPDOWN instead of MONTHCAL
    #define DTS_SHOWNONE             2 // allow a NONE selection
    #define DTS_SHORTDATEFORMAT      0 // use the short date format (app must forward WM_WININICHANGE messages)
    #define DTS_LONGDATEFORMAT       4 // use the long date format (app must forward WM_WININICHANGE messages)
    #define DTS_SHORTDATECENTURYFORMAT     12// short date format with century (app must forward WM_WININICHANGE messages)
    #define DTS_TIMEFORMAT           9 // use the time format (app must forward WM_WININICHANGE messages)
    #define DTS_APPCANPARSE         16 // allow user entered strings (app MUST respond to DTN_USERSTRING)
    #define DTS_RIGHTALIGN          32 // right-align popup instead of left-align it

    // end_r_commctrl

    #define DTN_DATETIMECHANGE  (DTN_FIRST + 1) // the systemtime has changed


           // GDT_VALID or GDT_NONE
                // valid iff dwFlags==GDT_VALID


    #define DTN_USERSTRINGA  (DTN_FIRST + 2) // the user has entered a string
    #define DTN_USERSTRINGW  (DTN_FIRST + 15)


         // string user entered
                    // app fills this in
               // GDT_VALID or GDT_NONE


         // string user entered
                    // app fills this in
               // GDT_VALID or GDT_NONE


    #ifdef UNICODE
      #define DTN_USERSTRING          DTN_USERSTRINGW
      #define NMDATETIMESTRING        NMDATETIMESTRINGW
      #define LPNMDATETIMESTRING      LPNMDATETIMESTRINGW
    #else
      #define DTN_USERSTRING          DTN_USERSTRINGA
      #define NMDATETIMESTRING        NMDATETIMESTRINGA
      #define LPNMDATETIMESTRING      LPNMDATETIMESTRINGA
    #endif

    #define DTN_WMKEYDOWNA  (DTN_FIRST + 3) // modify keydown on app format field (X)
    #define DTN_WMKEYDOWNW  (DTN_FIRST + 16)


         // virtual key code of WM_KEYDOWN which MODIFIES an X field
        // format substring
               // current systemtime, app should modify based on key


         // virtual key code of WM_KEYDOWN which MODIFIES an X field
        // format substring
               // current systemtime, app should modify based on key


    #ifdef UNICODE
      #define DTN_WMKEYDOWN           DTN_WMKEYDOWNW
      #define NMDATETIMEWMKEYDOWN     NMDATETIMEWMKEYDOWNW
      #define LPNMDATETIMEWMKEYDOWN   LPNMDATETIMEWMKEYDOWNW
    #else
      #define DTN_WMKEYDOWN           DTN_WMKEYDOWNA
      #define NMDATETIMEWMKEYDOWN     NMDATETIMEWMKEYDOWNA
      #define LPNMDATETIMEWMKEYDOWN   LPNMDATETIMEWMKEYDOWNA
    #endif

    #define DTN_FORMATA  (DTN_FIRST + 4) // query display for app format field (X)
    #define DTN_FORMATW  (DTN_FIRST + 17)


          // format substring
              // current systemtime
          // string to display
         // buffer pszDisplay originally points at


          // format substring
              // current systemtime
         // string to display
        // buffer pszDisplay originally points at


    #ifdef UNICODE
      #define DTN_FORMAT             DTN_FORMATW
      #define NMDATETIMEFORMAT        NMDATETIMEFORMATW
      #define LPNMDATETIMEFORMAT      LPNMDATETIMEFORMATW
    #else
      #define DTN_FORMAT             DTN_FORMATA
      #define NMDATETIMEFORMAT        NMDATETIMEFORMATA
      #define LPNMDATETIMEFORMAT      LPNMDATETIMEFORMATA
    #endif

    #define DTN_FORMATQUERYA  (DTN_FIRST + 5) // query formatting info for app format field (X)
    #define DTN_FORMATQUERYW (DTN_FIRST + 18)


         // format substring
               // max bounding rectangle app will use for this format string


        // format substring
               // max bounding rectangle app will use for this format string


    #ifdef UNICODE
      #define DTN_FORMATQUERY         DTN_FORMATQUERYW
      #define NMDATETIMEFORMATQUERY   NMDATETIMEFORMATQUERYW
      #define LPNMDATETIMEFORMATQUERY LPNMDATETIMEFORMATQUERYW
    #else
      #define DTN_FORMATQUERY         DTN_FORMATQUERYA
      #define NMDATETIMEFORMATQUERY   NMDATETIMEFORMATQUERYA
      #define LPNMDATETIMEFORMATQUERY LPNMDATETIMEFORMATQUERYA
    #endif

    #define DTN_DROPDOWN    (DTN_FIRST + 6) // MonthCal has dropped down
    #define DTN_CLOSEUP     (DTN_FIRST + 7) // MonthCal is popping up

    #define GDTR_MIN          1
    #define GDTR_MAX          2

    #define GDT_ERROR    -1
    #define GDT_VALID    0
    #define GDT_NONE     1

  #endif // NODATETIMEPICK


  #ifndef NOIPADDRESS

    ///////////////////////////////////////////////
    ///    IP Address edit control

    // Messages sent to IPAddress controls

    #define IPM_CLEARADDRESS (WM_USER+100) // no parameters
    #define IPM_SETADDRESS   (WM_USER+101) // lparam = TCP/IP address
    #define IPM_GETADDRESS   (WM_USER+102) // lresult = # of non black fields.  lparam = LPDWORD for TCP/IP address
    #define IPM_SETRANGE (WM_USER+103) // wparam = field, lparam = range
    #define IPM_SETFOCUS (WM_USER+104) // wparam = field
    #define IPM_ISBLANK  (WM_USER+105) // no parameters

    #define WC_IPADDRESSW           L"SysIPAddress32"
    #define WC_IPADDRESSA           "SysIPAddress32"

    #ifdef UNICODE
      #define WC_IPADDRESS          WC_IPADDRESSW
    #else
      #define WC_IPADDRESS          WC_IPADDRESSA
    #endif

    #define IPN_FIELDCHANGED                (IPN_FIRST - 0)


    // The following is a useful macro for passing the range values in the
    // IPM_SETRANGE message.

    #define MAKEIPRANGE(low, high)    ((LPARAM)(WORD)(((BYTE)(high) << 8) + (BYTE)(low)))

    // And this is a useful macro for making the IP Address to be passed
    // as a LPARAM.

    #define MAKEIPADDRESS(b1,b2,b3,b4)  ((LPARAM)(((DWORD)(b1)<<24)+((DWORD)(b2)<<16)+((DWORD)(b3)<<8)+((DWORD)(b4))))

    // Get individual number
    #define FIRST_IPADDRESS(x)  ((x>>24) *  255)
    #define SECOND_IPADDRESS(x) ((x>>16) *  255)
    #define THIRD_IPADDRESS(x)  ((x>>8) *  255)
    #define FOURTH_IPADDRESS(x) (x *  255)

  #endif // NOIPADDRESS


  //---------------------------------------------------------------------------------------
  //---------------------------------------------------------------------------------------
  ///  ====================== Pager Control =============================
  //---------------------------------------------------------------------------------------
  //---------------------------------------------------------------------------------------

  #ifndef NOPAGESCROLLER

    //Pager Class Name
    #define WC_PAGESCROLLERW           L"SysPager"
    #define WC_PAGESCROLLERA           "SysPager"

    #ifdef UNICODE
      #define WC_PAGESCROLLER          WC_PAGESCROLLERW
    #else
      #define WC_PAGESCROLLER          WC_PAGESCROLLERA
    #endif


    //---------------------------------------------------------------------------------------
    // Pager Control Styles
    //---------------------------------------------------------------------------------------
    // begin_r_commctrl

    #define PGS_VERT                         0
    #define PGS_HORZ                         1
    #define PGS_AUTOSCROLL                   2
    #define PGS_DRAGNDROP                    4

    // end_r_commctrl


    //---------------------------------------------------------------------------------------
    // Pager Button State
    //---------------------------------------------------------------------------------------
    //The scroll can be in one of the following control State
    #define  PGF_INVISIBLE   0      // Scroll button is not visible
    #define  PGF_NORMAL      1      // Scroll button is in normal state
    #define  PGF_GRAYED      2      // Scroll button is in grayed state
    #define  PGF_DEPRESSED   4      // Scroll button is in depressed state
    #define  PGF_HOT         8      // Scroll button is in hot state


    // The following identifiers specifies the button control
    #define PGB_TOPORLEFT       0
    #define PGB_BOTTOMORRIGHT   1

    //---------------------------------------------------------------------------------------
    // Pager Control  Messages
    //---------------------------------------------------------------------------------------
    #define PGM_SETCHILD            (PGM_FIRST + 1)  // lParam == hwnd
    #define Pager_SetChild(hwnd, hwndChild) \


    #define PGM_RECALCSIZE          (PGM_FIRST + 2)
    #define Pager_RecalcSize(hwnd) \


    #define PGM_FORWARDMOUSE        (PGM_FIRST + 3)
    #define Pager_ForwardMouse(hwnd, bForward) \


    #define PGM_SETBKCOLOR          (PGM_FIRST + 4)
    #define Pager_SetBkColor(hwnd, clr) \


    #define PGM_GETBKCOLOR          (PGM_FIRST + 5)
    #define Pager_GetBkColor(hwnd) \


    #define PGM_SETBORDER          (PGM_FIRST + 6)
    #define Pager_SetBorder(hwnd, iBorder) \


    #define PGM_GETBORDER          (PGM_FIRST + 7)
    #define Pager_GetBorder(hwnd) \


    #define PGM_SETPOS              (PGM_FIRST + 8)
    #define Pager_SetPos(hwnd, iPos) \


    #define PGM_GETPOS              (PGM_FIRST + 9)
    #define Pager_GetPos(hwnd) \


    #define PGM_SETBUTTONSIZE       (PGM_FIRST + 10)
    #define Pager_SetButtonSize(hwnd, iSize) \


    #define PGM_GETBUTTONSIZE       (PGM_FIRST + 11)
    #define Pager_GetButtonSize(hwnd) \


    #define PGM_GETBUTTONSTATE      (PGM_FIRST + 12)
    #define Pager_GetButtonState(hwnd, iButton) \


    #define PGM_GETDROPTARGET       CCM_GETDROPTARGET
    #define Pager_GetDropTarget(hwnd, ppdt) \

    //---------------------------------------------------------------------------------------
    //Pager Control Notification Messages
    //---------------------------------------------------------------------------------------


    // PGN_SCROLL Notification Message

    #define PGN_SCROLL          (PGN_FIRST-1)

    #define PGF_SCROLLUP        1
    #define PGF_SCROLLDOWN      2
    #define PGF_SCROLLLEFT      4
    #define PGF_SCROLLRIGHT     8


    //Keys down
    #define PGK_SHIFT           1
    #define PGK_CONTROL         2
    #define PGK_MENU            4


    // This structure is sent along with PGN_SCROLL notifications


                   // Specifies which keys are down when this notification is send
                 // Contains Parent Window Rect
                     // Scrolling Direction
                    // Horizontal scroll position
                    // Vertical scroll position
                  // [in/out] Amount to scroll


    // PGN_CALCSIZE Notification Message

    #define PGN_CALCSIZE        (PGN_FIRST-2)

    #define PGF_CALCWIDTH       1
    #define PGF_CALCHEIGHT      2

  #endif // NOPAGESCROLLER

  ////======================  End Pager Control ==========================================

  //
  // === Native Font Control ===
  //
  #ifndef NONATIVEFONTCTL

    //NativeFont Class Name
    #define WC_NATIVEFONTCTLW           L"NativeFontCtl"
    #define WC_NATIVEFONTCTLA           "NativeFontCtl"

    #ifdef UNICODE
      #define WC_NATIVEFONTCTL          WC_NATIVEFONTCTLW
    #else
      #define WC_NATIVEFONTCTL          WC_NATIVEFONTCTLA
    #endif

    // begin_r_commctrl

    // style definition
    #define NFS_EDIT                     1
    #define NFS_STATIC                   2
    #define NFS_LISTCOMBO                4
    #define NFS_BUTTON                   8
    #define NFS_ALL                     16
    #define NFS_USEFONTASSOC            32

    // end_r_commctrl

  #endif // NONATIVEFONTCTL
  // === End Native Font Control ===

  //
  // === MUI APIs ===
  //
  //====== TrackMouseEvent  =====================================================

  #ifndef NOTRACKMOUSEEVENT

    //
    // If the messages for TrackMouseEvent have not been defined then define them
    // now.
    //
    #ifndef WM_MOUSEHOVER
      #define WM_MOUSEHOVER                      673
      #define WM_MOUSELEAVE                      675
    #endif

    //
    // If the TRACKMOUSEEVENT structure and associated flags havent been declared
    // then declare them now.
    //
    #ifndef TME_HOVER
      #define TME_HOVER       0x00000001
      #define TME_LEAVE       0x00000002
      #define TME_NONCLIENT   0x00000010
      #define TME_QUERY       0x40000000
      #define TME_CANCEL      0x80000000

      #define HOVER_DEFAULT   0xFFFFFFFF

    #endif // !TME_HOVER

    //
    // Declare _TrackMouseEvent.  This API tries to use the window manager's
    // implementation of TrackMouseEvent if it is present, otherwise it emulates.
    //

  #endif // !NOTRACKMOUSEEVENT

  //====== Flat Scrollbar APIs=========================================
  #ifndef NOFLATSBAPIS

    #define WSB_PROP_CYVSCROLL            1
    #define WSB_PROP_CXHSCROLL            2
    #define WSB_PROP_CYHSCROLL            4
    #define WSB_PROP_CXVSCROLL            8
    #define WSB_PROP_CXHTHUMB            16
    #define WSB_PROP_CYVTHUMB            32
    #define WSB_PROP_VBKGCOLOR           64
    #define WSB_PROP_HBKGCOLOR          128
    #define WSB_PROP_VSTYLE             256
    #define WSB_PROP_HSTYLE             512
    #define WSB_PROP_WINSTYLE          1024
    #define WSB_PROP_PALETTE           2048
    #define WSB_PROP_MASK              4095

    #define FSB_FLAT_MODE           2
    #define FSB_ENCARTA_MODE        1
    #define FSB_REGULAR_MODE        0


    #define FlatSB_GetScrollPropPtr  FlatSB_GetScrollProp


    #define FlatSB_SetScrollPropPtr FlatSB_SetScrollProp

  #endif  //  NOFLATSBAPIS


  //P_O_Pop
#endif  // _INC_COMMCTRL
