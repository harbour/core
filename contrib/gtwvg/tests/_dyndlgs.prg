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

   LOCAL hDlg, aDlg, nStyle, cDlgIcon, cDlgProc, lOnTop, hMenu

// LOCAL bDlgProc
// LOCAL nTimerTicks

   nStyle := DS_SETFONT + WS_VISIBLE + WS_POPUP + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX

   aDlg := wvt_MakeDlgTemplate( 1, 4, 21, 60, { 0, 0, 0, 0 },  ;
      "Dialog First [ " + hb_ntos( nInfo ) + " ] " + ;
      iif( nInfo % 2 == 0, "Modeless", "Modal" ), " Dialog !", nStyle )

   // Multi line edit control
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOVSCROLL + ES_MULTILINE + ;
      ES_WANTRETURN + WS_BORDER  + WS_VSCROLL
   aDlg   := wvt_AddDlgItem( aDlg,  1, 2, 15, 35, {}, ID_MLE       , "EDIT"   , nStyle, /* cText, nHelpId, nExStyle */ )

   // Two Horz and Vert Lines
   nStyle := WS_CHILD + WS_VISIBLE + SS_ETCHEDVERT
   aDlg   := wvt_AddDlgItem( aDlg, 1, 39,  16, 1, {}, 111          , "STATIC" , nStyle )
   nStyle := WS_CHILD + WS_VISIBLE + SS_ETCHEDHORZ
   aDlg   := wvt_AddDlgItem( aDlg, 17, 2,  1, 56, {}, 112          , "STATIC" , nStyle )

   // Icon
   nStyle := WS_CHILD + WS_VISIBLE + SS_ICON //+ SS_CENTERIMAGE
   aDlg   := wvt_AddDlgItem( aDlg, 18, 2, 2, 6, {}, ID_ICO_VOUCH  , "STATIC" , nStyle, "" )
/*
   // Bitmap
   nStyle := WS_CHILD + WS_VISIBLE + SS_BITMAP + SS_REALSIZEIMAGE
   aDlg   := wvt_AddDlgItem( aDlg, 18, 41, 2,8, { -3, 0, 3 }, ID_STA_IMAGE, "STATIC" , nStyle, "" )
*/
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTOCHECKBOX
   aDlg   := wvt_AddDlgItem( aDlg, 18, 15,  1, 10, {}, ID_CHK_SATIS , "BUTTON" , nStyle, "Satisfied?" )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_RIGHT + ES_READONLY
   aDlg   := wvt_AddDlgItem( aDlg, 18, 30,  1,  7, { 3 }, ID_EDT_TIME , "EDIT" , nStyle, "" )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_NOTIFY + WS_VSCROLL + WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg,  1, 41,  4, 17, {}, ID_LST_LIST  , "LISTBOX", nStyle, "ListBox"  )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg,  4, 41,  1, 17, { 3, 0, 0, 0 }, -1    , "STATIC" , nStyle, "Degree"     )
   nStyle := WS_VISIBLE + WS_TABSTOP + CBS_DROPDOWNLIST + WS_BORDER + WS_VSCROLL
   aDlg   := wvt_AddDlgItem( aDlg,  5, 41,  6, 17, {}, ID_CMB_COMBO , "COMBOBOX" , nStyle, "Combo" )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_GROUPBOX
   aDlg   := wvt_AddDlgItem( aDlg,  7, 41,  4, 17, { 0, 0, 4, 0 }, ID_GRP_COMP, "BUTTON" , nStyle, "Compiler" )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTORADIOBUTTON
   aDlg   := wvt_AddDlgItem( aDlg,  8, 43,  1, 14, {}, ID_RDO_XH    , "BUTTON" , nStyle, "Harbour"  )
   aDlg   := wvt_AddDlgItem( aDlg,  9, 43,  1, 14, {}, ID_RDO_CLIP  , "BUTTON" , nStyle, "Clipper"  )
   aDlg   := wvt_AddDlgItem( aDlg, 10, 43,  1, 14, {}, ID_RDO_XBASE , "BUTTON" , nStyle, "Xbase++"  )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg, 12, 41, 1, 17, { 3, 0, 0, 0 }, ID_STA_TEXT, "STATIC" , nStyle, "Scrollable Text"    )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOHSCROLL + WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg, 13, 41, 1, 17, {}, ID_EDT_TEXT  , "EDIT"   , nStyle, "This is Text Field" )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := wvt_AddDlgItem( aDlg, 14, 41, 1, 17, { 3, 0, 0, 0 }, -1, "STATIC" , nStyle, "Right Justified Numerics" )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOHSCROLL + ES_NUMBER + ES_RIGHT + WS_BORDER
   aDlg   := wvt_AddDlgItem( aDlg, 15, 41, 1, 17, {}, ID_EDT_NUMB  , "EDIT"   , nStyle, "1234567" )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_PUSHBUTTON
   aDlg   := wvt_AddDlgItem( aDlg, 18, 50, 1,  8, { -3, 0, 3, 0 }, ID_BTN_OK, "BUTTON" , nStyle, "OK" )

   hMenu  := Wvt_CreateMenu()
   Wvt_AppendMenu( hMenu, MF_STRING + MF_ENABLED, ID_MNU_FILE   , "File"     )
   Wvt_AppendMenu( hMenu, MF_STRING + MF_ENABLED, ID_MNU_CONTROL, "Controls" )

   lOnTop      := .F.
   cDlgProc    := "DynDlgProc"
// bDlgProc    := {| a, b, c, d | DYNDLGPROC( a, b, c, d ) }
   cDlgIcon    := "v_notes.ico"
// nTimerTicks := 1000  // 1 second

   IF nInfo == 2
      // Modal Dialog
      //hDlg := wvt_DialogBox( aDlg, bDlgProc, Wvt_GetWindowHandle() )
      hDlg := wvt_DialogBox( aDlg, cDlgProc, Wvt_GetWindowHandle() )
   ELSE
      // Modeless Dialog
      hDlg := wvt_CreateDialog( aDlg, lOnTop, cDlgProc, cDlgIcon, /*nTimerTicks*/, hMenu )

      // Using Function name.
      //hDlg  := wvt_CreateDialog( aDlg, lOnTop, cDlgProc, cDlgIcon, nTimerTicks, hMenu, lModal )
   ENDIF

   RETURN hDlg

FUNCTION DynDlgProc( hDlg, nMsg, wParam, lParam )

   LOCAL lClicked, cPrompt, nIndex, hFont, aHFonts

   SWITCH nMsg

   CASE WM_TIMER
      wvg_SetDlgItemText( hDlg, ID_EDT_TIME, Time() )
      EXIT

   CASE WM_COMMAND
      DO CASE

      CASE wParam == ID_CHK_SATIS
         lClicked := ( wvg_IsDlgButtonChecked( hDlg, ID_CHK_SATIS ) == 1 )
         wvg_MessageBox( hDlg, iif( lClicked, "Satisfied", "UnSatisfied" ), "CheckBoxStatus" )

      CASE wParam == ID_RDO_XH
         wvg_MessageBox( hDlg, "Harbour", "Compiler" )

      CASE wParam == ID_RDO_CLIP
         wvg_MessageBox( hDlg, "Clipper", "Compiler" )

      CASE wParam == ID_RDO_XBASE
         wvg_MessageBox( hDlg, "Xbase++", "Compiler" )

      CASE wParam == ID_MNU_FILE
         wvg_MessageBox( hDlg, "Execute Menu Action!", "File" )

      CASE wParam == ID_MNU_CONTROL
         wvg_MessageBox( hDlg, "Controls are from Windows!", "Controls" )

      CASE wvg_LOWORD( wParam ) == ID_LST_LIST
         IF wvg_HIWORD( wParam ) == LBN_SELCHANGE
            nIndex  := wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETCURSEL, 0, 0 )
            cPrompt := Space( 20 )
            wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETTEXT, nIndex, @cPrompt )
            wvg_MessageBox( hDlg, cPrompt, "ListBox" )
         ENDIF

      CASE wvg_LOWORD( wParam ) == ID_CMB_COMBO
         IF wvg_HIWORD( wParam ) == CBN_SELCHANGE
            nIndex  := wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETCURSEL, 0, 0 )
            cPrompt := Space( 20 )
            wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETLBTEXT, nIndex, @cPrompt )
            wvg_MessageBox( hDlg, cPrompt, "Combo Box" )
         ENDIF

      ENDCASE
      EXIT

   CASE WM_CTLCOLOREDIT
      IF wvg_GetDlgItem( hDlg, ID_MLE ) == lParam
         wvg_SetTextColor( wParam, RGB( 0, 0, 255 ) )
         wvg_SetBkColor( wParam, RGB( 255, 255, 200 ) )
         RETURN 1
      ELSEIF wvg_GetDlgItem( hDlg, ID_EDT_TEXT ) == lParam
         wvg_SetTextColor( wParam, RGB( 255, 255, 255 ) )
         wvg_SetBkColor( wParam, RGB( 10, 200, 45 ) )
         RETURN 1
      ENDIF
      EXIT

   CASE WM_CTLCOLORSTATIC
      IF wvg_GetDlgItem( hDlg, ID_STA_TEXT ) == lParam
         wvg_SetTextColor( wParam, RGB( 255, 255, 255 ) )
         RETURN 1
      ENDIF
      EXIT

   CASE WM_INITDIALOG
      wvg_SetTimer( hDlg, 5001, 1000 ) // 1 sec

      IF Empty( aHFonts := SetFonts() )
         IF ( hFont := wvt_CreateFont( "Times New Roman", 18 ) ) != 0
            SetFonts( hFont )
         ENDIF
      ENDIF

      IF Len( aHFonts ) > 0
         wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_MLE ), WM_SETFONT, ahFonts[ 1 ], 0 )
      ENDIF

      IF Empty( SetIcons() )
         SetIcons( wvg_LoadIcon( "vr_1.ico" ) )
      ENDIF
      IF ! Empty( SetIcons() )
         wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_ICO_VOUCH ), STM_SETIMAGE, IMAGE_ICON, SetIcons()[ 1 ] )
      ENDIF

      /*
      IF t_hImage == NIL
         t_hImage := wvg_LoadImage( "vouch1.bmp", 2 )
      ENDIF
      IF t_hImage != NIL .AND. t_hImage != 0
         wvg_SendMessage( wvg_GetDlgItem( hDlg, ID_STA_IMAGE ), STM_SETIMAGE, IMAGE_BITMAP, t_hImage )
      ENDIF
      */
      wvg_SetDlgItemText( hDlg, ID_MLE, GetEditText() )
      wvg_CheckDlgButton( hDlg, ID_CHK_SATIS, .T.           )

      wvg_CheckRadioButton( hDlg, ID_RDO_XH, ID_RDO_XBASE, ID_RDO_XH )

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

   CASE WM_DESTROY
      // Do whatever you want to do with cText
      // Each box will retrieve its own text.
#if 0
      cText := ""
#endif
      wvg_GetDlgItemText( hDlg, ID_MLE )
      EXIT
   ENDSWITCH

   RETURN 0

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

   nStyle  := DS_SETFONT + WS_VISIBLE + WS_POPUP + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX

   aDlg    := wvt_MakeDlgTemplate( 0, 0, 20, 40, {}, "Slide Show", nStyle )

   hDlg    := wvt_CreateDialog( aDlg, .F., "DlgSlideShowProc", "vr_1.ico", 5000 )

   RETURN hDlg

FUNCTION DlgSlideShowProc( hDlg, nMsg, wParam, lParam )

   THREAD STATIC t_nSlide := 1

   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )

   SWITCH nMsg

   CASE WM_INITDIALOG
      DrawSlide( hDlg, t_nSlide )
      EXIT

   CASE WM_PAINT
      DrawSlide( hDlg, t_nSlide )
      EXIT

   CASE WM_TIMER
      t_nSlide++
      IF t_nSlide > Len( t_aSlides )
         t_nSlide := 1
      ENDIF
      DrawSlide( hDlg, t_nSlide )

      EXIT
   ENDSWITCH

   RETURN 0

STATIC PROCEDURE DrawSlide( hDlg, nSlide )

   LOCAL hDC, aRect

   hDC   := wvg_GetDC( hDlg )
   aRect := wvg_GetClientRect( hDlg )

   win_Rectangle( hDC, aRect[ 1 ] + 10, aRect[ 2 ] + 10, aRect[ 3 ] - 10, aRect[ 4 ] - 10 )
   wvg_DrawImage( hDC, aRect[ 1 ] + 10, aRect[ 2 ] + 10, aRect[ 3 ] - aRect[ 1 ] - 20, ;
      aRect[ 4 ] - aRect[ 2 ] - 20, t_aSlides[ nSlide ] )

   wvg_ReleaseDC( hDlg, hDC )

   RETURN
