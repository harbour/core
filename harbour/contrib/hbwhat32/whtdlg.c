/*
 * $Id$
 */


// WHAT32
// Dialog functions


/*
 * Some parts Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * with author's permission granted on 27 MAy 2002
   Last change:  WN   27 May 2002   10:23 am
 */


#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"



//-----------------------------------------------------------------------------

HB_FUNC( ENDDIALOG )
{
  EndDialog( (HWND) HB_PARWH(1) , hb_parni(2) );
}

//-----------------------------------------------------------------------------

HB_FUNC( GETDLGITEM )
{
   HWND hWnd = GetDlgItem(
                 (HWND) HB_PARWH( 1 ), // handle of dialog box
                 hb_parni( 2 )           // identifier of control
               );
   HB_RETWH( hWnd );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetNextDlgGroupItem( IN HWND hDlg, IN HWND hCtl, IN BOOL bPrevious);


HB_FUNC( GETNEXTDLGGROUPITEM )
{
   HB_RETWH( GetNextDlgGroupItem( (HWND) HB_PARWH( 1 ),
                                         (HWND) HB_PARWH( 2 ),
                                         hb_parl( 3 )
                                       ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetNextDlgTabItem( IN HWND hDlg, IN HWND hCtl, IN BOOL bPrevious);


HB_FUNC( GETNEXTDLGTABITEM )
{
   HB_RETWH( GetNextDlgTabItem( (HWND) HB_PARWH( 1 ),
                                       (HWND) HB_PARWH( 2 ),
                                       hb_parl( 3 )
                                     ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetDlgCtrlID( IN HWND hWnd);


HB_FUNC( GETDLGCTRLID )
{
   hb_retni( GetDlgCtrlID( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI long WINAPI GetDialogBaseUnits(VOID);


HB_FUNC( GETDIALOGBASEUNITS )
{
   hb_retnl( (LONG) GetDialogBaseUnits(  ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetDlgItemInt( IN HWND hDlg, IN int nIDDlgItem, IN UINT uValue, IN BOOL bSigned);


HB_FUNC( SETDLGITEMINT )
{
   hb_retl( SetDlgItemInt( (HWND) HB_PARWH( 1 ),
                           hb_parni( 2 )       ,
                           (UINT) hb_parni( 3 ),
                           hb_parl( 4 )
                         ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( SETDLGITEMTEXT )
{
    SetDlgItemText(
       (HWND) HB_PARWH( 1 ),  // handle of dialog box
       hb_parni( 2 ),           // identifier of control
       (LPCTSTR) hb_parcx( 3 )    // text to set
    );
}

//-----------------------------------------------------------------------------

// modified API

HB_FUNC( GETDLGITEMTEXT )  // GETDLGITMTEXT
{
   USHORT iLen = ( USHORT ) SendMessage( GetDlgItem( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 )+1 ;
   char *cText = (char*) hb_xgrab( iLen );

   GetDlgItemText(
                  (HWND) HB_PARWH( 1 ),   // handle of dialog box
                  hb_parni( 2 ),             // identifier of control
                  (LPTSTR) cText,         // address of buffer for text
                  iLen                    // maximum size of string
                 );

   hb_retc( cText );
   hb_xfree( cText );
}

//-----------------------------------------------------------------------------
// optional max length

// NOT WIN API !!!

HB_FUNC( GETLBITEMTEXT )  // GETDLGITMTEXT
{
   USHORT iLen = ISNIL(3) ? 255 : ( USHORT ) hb_parni( 3 );
   char * cText = ( char * ) hb_xgrab( iLen + 1 );

   SendMessage(
                  (HWND) HB_PARWH( 1 ),    // handle of dialog box
                  LB_GETTEXT,
                  (WPARAM) hb_parni( 2 ), // item number
                  (LPARAM) (LPCSTR) cText           // address of buffer for text
              );

   hb_retc( cText );
   hb_xfree( cText );
}


//-----------------------------------------------------------------------------

HB_FUNC( CHECKDLGBUTTON )
{
    hb_retl( CheckDlgButton(
                             (HWND) HB_PARWH( 1 ), // handle of dialog box
                             hb_parni( 2 ),          // identifier of control
                             ISNUM(3) ? hb_parni(3) : (UINT) hb_parl(3) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton(
                              (HWND) HB_PARWH( 1 ),   // handle of dialog box
                              hb_parni( 2 ),         // identifier of first radio button in group
                              hb_parni( 3 ),         // identifier of last radio button in group
                              hb_parni( 4 )          // identifier of radio button to select
                             ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( ISDLGBUTTONCHECKED )
{
  hb_retni( IsDlgButtonChecked(
                               (HWND) HB_PARWH( 1 ),       // handle of dialog box
                               hb_parni( 2 )               // button identifier
                               ) );


}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI DlgDirListA( IN HWND hDlg, IN OUT LPSTR lpPathSpec, IN int nIDListBox, IN int nIDStaticPath, IN UINT uFileType);


HB_FUNC( DLGDIRLIST )
{

   char *cText = (char*) hb_xgrab( MAX_PATH+1 );
  // cText = hb_parcx(2);

   hb_retni( DlgDirList( (HWND) HB_PARWH( 1 ),
                         (LPSTR) cText       ,
                         hb_parni( 3 )       ,
                         hb_parni( 4 )       ,
                         (UINT) hb_parni( 5 )
                       ) );

   hb_storc(cText,2);
   hb_xfree(cText);

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DlgDirSelectExA( IN HWND hDlg, OUT LPSTR lpString, IN int nCount, IN int nIDListBox);


HB_FUNC( DLGDIRSELECTEX )
{

   USHORT iLen = ISNIL(3) ? MAX_PATH : ( USHORT ) hb_parni( 3 );
   char *cText = (char*) hb_xgrab( iLen+1 );

   hb_retl( DlgDirSelectEx( (HWND) HB_PARWH( 1 ),
                            (LPSTR) cText       ,
                            iLen                ,
                            hb_parni( 4 )
                          ) );

   hb_storc(cText, 2 );
   hb_xfree(cText);
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI DlgDirListComboBoxA( IN HWND hDlg, IN OUT LPSTR lpPathSpec, IN int nIDComboBox, IN int nIDStaticPath, IN UINT uFiletype);


HB_FUNC( DLGDIRLISTCOMBOBOX )
{
   char *cText = (char*) hb_xgrab( MAX_PATH+1 );
   //cText = hb_parcx(2);

   hb_retni( DlgDirListComboBox( (HWND) HB_PARWH( 1 ),
                                 (LPSTR) cText       ,
                                 hb_parni( 3 )       ,
                                 hb_parni( 4 )       ,
                                 (UINT) hb_parni( 5 )
                               ) );
   hb_storc(cText, 2 );
   hb_xfree(cText);

}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DlgDirSelectComboBoxExA( IN HWND hDlg, OUT LPSTR lpString, IN int nCount, IN int nIDComboBox);


HB_FUNC( DLGDIRSELECTCOMBOBOXEX )
{

   USHORT iLen = ISNIL(3) ? MAX_PATH : ( USHORT ) hb_parni( 3 );
   char *cText = (char*) hb_xgrab( iLen+1 );

   hb_retl( DlgDirSelectComboBoxEx( (HWND) HB_PARWH( 1 ),
                                    (LPSTR) cText       ,
                                    iLen                ,
                                    hb_parni( 4 )
                                  ) );

   hb_storc(cText, 2 );
   hb_xfree(cText);

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI MapDialogRect( IN HWND hDlg, IN OUT LPRECT lpRect);

/* Call as
Local aSrc := { 11 , 22 , 15 , 63 )
MAPDIALOGRECT(nDlg,@aSrc)
*/

HB_FUNC( MAPDIALOGRECT )
{
   RECT lpRect ;
   PHB_ITEM pArray;
   if (ISARRAY(2) )
      {
      pArray=hb_param( 2, HB_IT_ARRAY );
      lpRect.left   = hb_arrayGetNL( pArray , 1 );
      lpRect.top    = hb_arrayGetNL( pArray , 2 );
      lpRect.right  = hb_arrayGetNL( pArray , 3 );
      lpRect.bottom = hb_arrayGetNL( pArray , 4 );

      hb_retl( MapDialogRect( (HWND) HB_PARWH( 1 ), &lpRect ) );
      hb_stornl( lpRect.left   , 2 , 1 );
      hb_stornl( lpRect.top    , 2 , 2 );
      hb_stornl( lpRect.right  , 2 , 3 );
      hb_stornl( lpRect.bottom , 2 , 4 );
   }
   else
      hb_retl(FALSE);


}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetComboBoxInfo( IN HWND hwndCombo, OUT PCOMBOBOXINFO pcbi );

/*

HB_FUNC( GETCOMBOBOXINFO )
{
   PCOMBOBOXINFO pcbi      ;

   hb_retl( GetComboBoxInfo( (HWND) HB_PARWH( 1 ), pcbi ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetAltTabInfoA( IN HWND hwnd, IN int iItem, OUT PALTTABINFO pati, OUT LPSTR pszItemText, IN UINT cchItemText );

/*

HB_FUNC( GETALTTABINFO )
{
   PALTTABINFO pati        ;

   // Your code goes here

   hb_retl( GetAltTabInfo( (HWND) HB_PARWH( 1 ),
                           hb_parni( 2 )       ,
                           pati                ,
                           (LPSTR) hb_parcx( 4 ),
                           (UINT) hb_parni( 5 )
                         ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetListBoxInfo( IN HWND hwnd );

/*
HB_FUNC( GETLISTBOXINFO )
{
   hb_retnl( (LONG) GetListBoxInfo( (HWND) HB_PARWH( 1 ) ) );
}
*/

HB_FUNC( GETLISTBOXINFO )
{
HINSTANCE h    = LoadLibraryEx( "user32.dll", NULL, 0);
DWORD dwGLIRet = 0 ;
HWND  hWnd     = (HWND) HB_PARWH( 1 );

if( h ){
    typedef DWORD (WINAPI *xdwGetListBoxInfo)( HWND hWnd );
    xdwGetListBoxInfo pfnGLI = (xdwGetListBoxInfo)
    GetProcAddress( h, "GetListBoxInfo");
    if( pfnGLI ){
        dwGLIRet = (DWORD) pfnGLI( hWnd );
    }
    FreeLibrary( h );
}

   hb_retl( (ULONG) dwGLIRet );
}


//----------------------------------------------------------------------------//
