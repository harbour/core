/* Pritpal Bedi <bedipritpal@hotmail.com> */

#include "inkey.ch"
#include "hbgtinfo.ch"

#define ID_BTN_OK                  1
#define ID_MLE                     10
#define ID_CHK_SATIS               11
#define ID_EDT_TIME                51
#define ID_LST_LIST                13
#define ID_CMB_COMBO               31
#define ID_RDO_XH                  21
#define ID_RDO_CLIP                22
#define ID_RDO_XBASE               23
#define ID_EDT_TEXT                14
#define ID_EDT_NUMB                15
#define ID_STA_TEXT                71
#define ID_STA_IMAGE               72
#define ID_ICO_VOUCH               81

#define ID_GRP_COMP                113

#define ID_MNU_FILE                201
#define ID_MNU_CONTROL             202

THREAD STATIC t_aSlides := {}

FUNCTION DynWinDialog( nInfo )

   LOCAL hDlg, aDlg, nStyle, cDlgIcon, lOnTop, hMenu

   LOCAL nTimerTicks

   nStyle := DS_SETFONT + WIN_WS_VISIBLE + WIN_WS_POPUP + WIN_WS_CAPTION + WIN_WS_SYSMENU + WIN_WS_THICKFRAME + WIN_WS_MINIMIZEBOX

   aDlg := wvt_MakeDlgTemplate( 1, 4, 21, 60, { 0, 0, 0, 0 },  ;
      "Dialog First [ " + hb_ntos( nInfo ) + " ] " + ;
      iif( nInfo % 2 == 0, "Modeless", "Modal" ), " Dialog !", nStyle )

   // Multi line edit control
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + ES_AUTOVSCROLL + ES_MULTILINE + ;
      ES_WANTRETURN + WIN_WS_BORDER  + WIN_WS_VSCROLL
   aDlg   := wvt_AddDlgItem( aDlg,  1, 2, 15, 35, {}, ID_MLE       , "EDIT"   , nStyle, /* cText, nHelpId, nExStyle */ )

   // Two Horz and Vert Lines
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_ETCHEDVERT
   aDlg   := wvt_AddDlgItem( aDlg, 1, 39,  16, 1, {}, 111          , "STATIC" , nStyle )
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_ETCHEDHORZ
   aDlg   := wvt_AddDlgItem( aDlg, 17, 2,  1, 56, {}, 112          , "STATIC" , nStyle )

   // Icon
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_ICON //+ SS_CENTERIMAGE
   aDlg   := wvt_AddDlgItem( aDlg, 18, 2, 2, 6, {}, ID_ICO_VOUCH  , "STATIC" , nStyle, "" )
#if 0
   // Bitmap
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_BITMAP + SS_REALSIZEIMAGE
   aDlg   := wvt_AddDlgItem( aDlg, 18, 41, 2,8, { -3, 0, 3 }, ID_STA_IMAGE, "STATIC" , nStyle, "" )
#endif
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + BS_AUTOCHECKBOX
   aDlg   := wvt_AddDlgItem( aDlg, 18, 15,  1, 10, {}, ID_CHK_SATIS , "BUTTON" , nStyle, "Satisfied?" )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + ES_RIGHT + ES_READONLY
   aDlg   := wvt_AddDlgItem( aDlg, 18, 30,  1,  7, { 3 }, ID_EDT_TIME , "EDIT" , nStyle, "" )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + LBS_NOTIFY + WIN_WS_VSCROLL + WIN_WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg,  1, 41,  4, 17, {}, ID_LST_LIST  , "LISTBOX", nStyle, "ListBox"  )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg,  4, 41,  1, 17, { 3, 0, 0, 0 }, -1    , "STATIC" , nStyle, "Degree"     )
   nStyle := WIN_WS_VISIBLE + WIN_WS_TABSTOP + CBS_DROPDOWNLIST + WIN_WS_BORDER + WIN_WS_VSCROLL
   aDlg   := wvt_AddDlgItem( aDlg,  5, 41,  6, 17, {}, ID_CMB_COMBO , "COMBOBOX" , nStyle, "Combo" )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + BS_GROUPBOX
   aDlg   := wvt_AddDlgItem( aDlg,  7, 41,  4, 17, { 0, 0, 4, 0 }, ID_GRP_COMP, "BUTTON" , nStyle, "Compiler" )
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + BS_AUTORADIOBUTTON
   aDlg   := wvt_AddDlgItem( aDlg,  8, 43,  1, 14, {}, ID_RDO_XH    , "BUTTON" , nStyle, "Harbour"  )
   aDlg   := wvt_AddDlgItem( aDlg,  9, 43,  1, 14, {}, ID_RDO_CLIP  , "BUTTON" , nStyle, "Cl*pper"  )
   aDlg   := wvt_AddDlgItem( aDlg, 10, 43,  1, 14, {}, ID_RDO_XBASE , "BUTTON" , nStyle, "Xbase++"  )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg, 12, 41, 1, 17, { 3, 0, 0, 0 }, ID_STA_TEXT, "STATIC" , nStyle, "Scrollable Text" )
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + ES_AUTOHSCROLL + WIN_WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg, 13, 41, 1, 17, {}, ID_EDT_TEXT  , "EDIT"   , nStyle, "This is Text Field" )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg, 14, 41, 1, 17, { 3, 0, 0, 0 }, -1, "STATIC" , nStyle, "Right Justified Numerics" )
   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + ES_AUTOHSCROLL + ES_NUMBER + ES_RIGHT + WIN_WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg, 15, 41, 1, 17, {}, ID_EDT_NUMB  , "EDIT"   , nStyle, "1234567" )

   nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_TABSTOP + BS_PUSHBUTTON
   aDlg   := wvt_AddDlgItem( aDlg, 18, 50, 1,  8, { -3, 0, 3, 0 }, ID_BTN_OK, "BUTTON" , nStyle, "OK" )

   hMenu  := wapi_CreateMenu()
   wapi_AppendMenu( hMenu, WIN_MF_STRING + WIN_MF_ENABLED, ID_MNU_FILE   , "File"     )
   wapi_AppendMenu( hMenu, WIN_MF_STRING + WIN_MF_ENABLED, ID_MNU_CONTROL, "Controls" )

   lOnTop      := .F.
   cDlgIcon    := "v_notes.ico"
#if 0
   nTimerTicks := 1000  // 1 second
#else
   nTimerTicks := NIL
#endif

   IF nInfo == 2
      // Modal Dialog
      hDlg := wvt_DialogBox( aDlg, @DynDlgProc(), hb_gtInfo( HB_GTI_WINHANDLE ) )
   ELSE
      // Modeless Dialog
      hDlg := wvt_CreateDialog( aDlg, lOnTop, @DynDlgProc(), cDlgIcon, nTimerTicks, hMenu )
   ENDIF

   RETURN hDlg

STATIC FUNCTION DynDlgProc( hDlg, nMsg, wParam, lParam )

   LOCAL lClicked, cPrompt, nIndex, hFont, aHFonts

   SWITCH nMsg

   CASE WIN_WM_TIMER
      wapi_SetDlgItemText( hDlg, ID_EDT_TIME, Time() )
      EXIT

   CASE WIN_WM_COMMAND

      DO CASE
      CASE wParam == ID_CHK_SATIS
         lClicked := ( wapi_IsDlgButtonChecked( hDlg, ID_CHK_SATIS ) == 1 )
         wapi_MessageBox( hDlg, iif( lClicked, "Satisfied", "UnSatisfied" ), "CheckBoxStatus" )

      CASE wParam == ID_RDO_XH
         wapi_MessageBox( hDlg, "Harbour", "Compiler" )

      CASE wParam == ID_RDO_CLIP
         wapi_MessageBox( hDlg, "Cl*pper", "Compiler" )

      CASE wParam == ID_RDO_XBASE
         wapi_MessageBox( hDlg, "Xbase++", "Compiler" )

      CASE wParam == ID_MNU_FILE
         wapi_MessageBox( hDlg, "Execute Menu Action!", "File" )

      CASE wParam == ID_MNU_CONTROL
         wapi_MessageBox( hDlg, "Controls are from Windows!", "Controls" )

      CASE wapi_LOWORD( wParam ) == ID_LST_LIST
         IF wapi_HIWORD( wParam ) == LBN_SELCHANGE
            nIndex  := wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETCURSEL, 0, 0 )
            cPrompt := Space( 20 )
            wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETTEXT, nIndex, @cPrompt )
            wapi_MessageBox( hDlg, cPrompt, "ListBox" )
         ENDIF

      CASE wapi_LOWORD( wParam ) == ID_CMB_COMBO
         IF wapi_HIWORD( wParam ) == CBN_SELCHANGE
            nIndex  := wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETCURSEL, 0, 0 )
            cPrompt := Space( 20 )
            wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETLBTEXT, nIndex, @cPrompt )
            wapi_MessageBox( hDlg, cPrompt, "Combo Box" )
         ENDIF

      ENDCASE
      EXIT

   CASE WIN_WM_CTLCOLOREDIT
      DO CASE
      CASE wapi_GetDlgItem( hDlg, ID_MLE ) == wvg_n2p( lParam )
         wapi_SetTextColor( wvg_n2p( wParam ), WIN_RGB( 0, 0, 255 ) )
         wapi_SetBkColor( wvg_n2p( wParam ), WIN_RGB( 255, 255, 200 ) )
         RETURN EVENT_UNHANDLED
      CASE wapi_GetDlgItem( hDlg, ID_EDT_TEXT ) == wvg_n2p( lParam )
         wapi_SetTextColor( wvg_n2p( wParam ), WIN_RGB( 255, 255, 255 ) )
         wapi_SetBkColor( wvg_n2p( wParam ), WIN_RGB( 10, 200, 45 ) )
         RETURN EVENT_UNHANDLED
      ENDCASE
      EXIT

   CASE WIN_WM_CTLCOLORSTATIC
#if 0
      IF wapi_GetDlgItem( hDlg, ID_STA_TEXT ) == wvg_n2p( lParam )
         wapi_SetTextColor( wvg_n2p( wParam ), WIN_RGB( 255, 255, 255 ) )
         wapi_SetBkColor( wvg_n2p( wParam ), WIN_RGB( 0, 0, 0 ) )
         RETURN EVENT_UNHANDLED
      ENDIF
#endif
      EXIT

   CASE WIN_WM_INITDIALOG
      wvg_SetTimer( hDlg, 5001, 1000 ) // 1 sec

      IF Empty( aHFonts := SetFonts() )
         IF ! Empty( hFont := wvt_CreateFont( "Times New Roman", 18 ) )
            SetFonts( hFont )
         ENDIF
      ENDIF

      IF Len( aHFonts ) > 0
         wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_MLE ), WIN_WM_SETFONT, ahFonts[ 1 ], 0 )
      ENDIF

      IF Empty( SetIcons() )
         SetIcons( wvg_LoadIcon( "vr_1.ico" ) )
      ENDIF
      IF ! Empty( SetIcons() )
         wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_ICO_VOUCH ), STM_SETIMAGE, WIN_IMAGE_ICON, SetIcons()[ 1 ] )
      ENDIF

#if 0
      IF t_hImage == NIL
         t_hImage := wvg_LoadImage( "vouch1.bmp", 2 )
      ENDIF
      IF t_hImage != NIL .AND. ! Empty( t_hImage )
         wapi_SendMessage( wapi_GetDlgItem( hDlg, ID_STA_IMAGE ), STM_SETIMAGE, WIN_IMAGE_BITMAP, t_hImage )
      ENDIF
#endif
      wapi_SetDlgItemText( hDlg, ID_MLE, GetEditText() )
      wapi_CheckDlgButton( hDlg, ID_CHK_SATIS, .T. )

      wapi_CheckRadioButton( hDlg, ID_RDO_XH, ID_RDO_XBASE, ID_RDO_XH )

      wvt_LBAddString( hDlg, ID_LST_LIST, "Harbour"   )
      wvt_LBAddString( hDlg, ID_LST_LIST, "Gtwvt"     )
      wvt_LBAddString( hDlg, ID_LST_LIST, "Wvtgui"    )
      wvt_LBAddString( hDlg, ID_LST_LIST, "Modeless"  )
      wvt_LBAddString( hDlg, ID_LST_LIST, "Dialogs"   )
      wvt_LBAddString( hDlg, ID_LST_LIST, "WVT"       )

      wvt_LBSetCurSel( hDlg, ID_LST_LIST, 1 )

      wvt_CBAddString( hDlg, ID_CMB_COMBO, "First"    )
      wvt_CBAddString( hDlg, ID_CMB_COMBO, "Second"   )
      wvt_CBAddString( hDlg, ID_CMB_COMBO, "Third"    )
      wvt_CBAddString( hDlg, ID_CMB_COMBO, "Fourth"   )
      wvt_CBAddString( hDlg, ID_CMB_COMBO, "Fifth"    )

      wvt_CBSetCurSel( hDlg, ID_CMB_COMBO, 1 )

      wvg_InvalidateRect( hDlg )

      EXIT

   CASE WIN_WM_DESTROY
      // Do whatevert you want to do with cText
      // Each box will retrieve its own text.
      /* cText := */
      wapi_GetDlgItemText( hDlg, ID_MLE )
      EXIT
   ENDSWITCH

   RETURN EVENT_HANDLED

STATIC FUNCTION GetEditText()
   RETURN ;
      "Welcome in the Wonderful World of Harbour!" + ;
      hb_eol() + hb_eol() + ;
      "When Peter Rees first published GTWVT, a Windows " + ;
      "Terminal Driver, on 22 Dec 2003, everybody took it " + ;
      "lightly, except for me, as I was aware that what " + ;
      "wonderful contribution to Harbour he has made, " + ;
      "what immense possibilities he has opened for Harbour " + ;
      "developers, what limitations he has cleared for Cl*pper " + ;
      "savvy user base." + ;
      hb_eol() + hb_eol() + ;
      "With a little effort I could extend GTWVT " + ;
      "to give it a GUI look. I also tried to give it " + ;
      "an event driven functionality, and up came Wvt*Classes." + ;
      hb_eol() + hb_eol() + ;
      "And yet another feather is added in the cap of GTWVT " + ;
      "as it is now capable of firing modeless dialogs like the one " + ;
      "you are viewing. These dialogs can be constructed dynamically ( Courtesy hbwhat ) " + ;
      "at run time or can be one of resources. At present 20 such dialogs " + ;
      "can be active at any given time. Also note that dialogs created " + ;
      "dynamically respect Top, Left, Rows, Cols coordinates, which is an " + ;
      "undisputed productivity boost!" + ;
      hb_eol() + hb_eol() + ;
      "Enjoy!" + hb_eol() + ;
      "Pritpal Bedi, INDIA"

FUNCTION DlgSlideShow()

   LOCAL hDlg, aDlg, nStyle

   t_aSlides := { "vouch1.bmp", "v_notes.ico", "2000.gif", "v_lock.bmp", "v_help.ico" }

   nStyle := DS_SETFONT + WIN_WS_VISIBLE + WIN_WS_POPUP + WIN_WS_CAPTION + WIN_WS_SYSMENU + WIN_WS_THICKFRAME + WIN_WS_MINIMIZEBOX

   aDlg   := wvt_MakeDlgTemplate( 0, 0, 20, 40, {}, "Slide Show", nStyle )

   hDlg   := wvt_CreateDialog( aDlg, .F., @DlgSlideShowProc(), "vr_1.ico", 5000 )

   RETURN hDlg

STATIC FUNCTION DlgSlideShowProc( hDlg, nMsg, wParam, lParam )

   THREAD STATIC t_nSlide := 1

   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )

   SWITCH nMsg

   CASE WIN_WM_INITDIALOG
      DrawSlide( hDlg, t_nSlide )
      EXIT

   CASE WIN_WM_PAINT
      DrawSlide( hDlg, t_nSlide )
      EXIT

   CASE WIN_WM_TIMER
      t_nSlide++
      IF t_nSlide > Len( t_aSlides )
         t_nSlide := 1
      ENDIF
      DrawSlide( hDlg, t_nSlide )

      EXIT
   ENDSWITCH

   RETURN EVENT_HANDLED

STATIC PROCEDURE DrawSlide( hDlg, nSlide )

   LOCAL hDC   := wvg_GetDC( hDlg )
   LOCAL aRect := wvg_GetClientRect( hDlg )

   wapi_Rectangle( hDC, aRect[ 1 ] + 10, aRect[ 2 ] + 10, aRect[ 3 ] - 10, aRect[ 4 ] - 10 )
   wvg_DrawImage( hDC, aRect[ 1 ] + 10, aRect[ 2 ] + 10, aRect[ 3 ] - aRect[ 1 ] - 20, ;
      aRect[ 4 ] - aRect[ 2 ] - 20, t_aSlides[ nSlide ] )

   wvg_ReleaseDC( hDlg, hDC )

   RETURN
