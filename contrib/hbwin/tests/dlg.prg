/*
 * This example demonstrates how to work with some features of the native Windows
 * API. The following function displays a dialog created with an external
 * resource editor.
 * 2010-06-10 - 00:16:41 - [vailtom]
 */

#require "hbwin"

// Some constants
#define IDD_DIALOG1         101
#define IDC_BUTTON1         4001
#define IDC_STATIC1         1002
#define IDC_EDIT1           1003
#define IDC_COMBO1          1006

#define WM_INITDIALOG       0x0110
#define WM_COMMAND          0x0111
#define WM_SYSCOMMAND       0x0112
#define WM_CLOSE            0x0010
#define MB_ICONASTERISK     0x00000040

// Main entry point
PROCEDURE Main()

   IF Empty( win_LoadResource( IDD_DIALOG1, WIN_RT_DIALOG ) )
      wapi_MessageBox( , "Resources not linked. Use 'hbmk2 dlg.hbp' to link resources at build time." )
   ELSE
      wapi_DialogBoxParam( 0, IDD_DIALOG1, 0, @DialogFunc() )
   ENDIF

   RETURN

// Main function to control the user interaction
STATIC FUNCTION DialogFunc( hWnd, nMessage, wParam, lParam, wPHigh, wPLow )

   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )
   HB_SYMBOL_UNUSED( wPHigh )

   SWITCH nMessage
   CASE WM_INITDIALOG

      wapi_SetDlgItemText( hWnd, IDC_STATIC1, "Hi! " + Time() )
      wapi_SetDlgItemText( hWnd, IDC_EDIT1, "Harbour" )

      wapi_ComboBox_AddString( wapi_GetDlgItem( hWnd, IDC_COMBO1 ), "Architect" )
      wapi_ComboBox_AddString( wapi_GetDlgItem( hWnd, IDC_COMBO1 ), "Engineer" )
      wapi_ComboBox_AddString( wapi_GetDlgItem( hWnd, IDC_COMBO1 ), "Project or Program Administrator" )
      wapi_ComboBox_AddString( wapi_GetDlgItem( hWnd, IDC_COMBO1 ), "Software Designer" )
      wapi_ComboBox_AddString( wapi_GetDlgItem( hWnd, IDC_COMBO1 ), "Other" )

      wapi_SetFocus( hWnd, wapi_GetDlgItem( hWnd, IDC_EDIT1 ) )
      RETURN .T.

   CASE WM_CLOSE
      wapi_EndDialog( hWnd, 3 )
      RETURN .T.

   CASE WM_COMMAND
      SWITCH wPLow
      CASE IDC_BUTTON1
         wapi_MessageBox( 0, "Hello [" + wapi_GetDlgItemText( hWnd, IDC_EDIT1 ) + "]!" + Chr( 13 ) + "How are you?", "Hi!", MB_ICONASTERISK )
         RETURN .T.
      ENDSWITCH
   ENDSWITCH

   RETURN .F.
