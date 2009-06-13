/*
 * $Id$
 */

// hbwhat

// Common dialogs


#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commdlg.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//extern __DlgProc ;

/*
(HWND) hdlg,      // handle to the dialog box window
                         (UINT) uiMsg      // message identifier
                         (WPARAM) wParam,  // message parameter
                         (LPARAM) lParam   // message parameter
                        );

*/
//----------------------------------------------------------------------------
// DWORD CommDlgExtendedError(VOID)

HB_FUNC( VWN_COMMDLGEXTENDEDERROR )
{
  hb_retnl( CommDlgExtendedError() );
}

//---------------------------------------------------------------------

//Syntax: ChooseFont( cf:value) -> structure buffer, or NIL

HB_FUNC( VWN_CHOOSEFONT )
{
  CHOOSEFONT *cf =  (CHOOSEFONT * ) hb_parc( 1 );
                       //hb_param( 1, HB_IT_STRING )->item.asString.value;

  cf->lStructSize = sizeof(CHOOSEFONT);

  if (ChooseFont( cf ) )
      hb_retclen( (char *) cf, sizeof( CHOOSEFONT ));
}

//----------------------------------------------------------------------------

// to be called called via FindText (in wincdlg.prg)

HB_FUNC( VWN__FINDTEXT )
{
   FINDREPLACE fr ;

   fr.lStructSize = sizeof( fr );

   fr.hwndOwner        = (HWND) HB_PARWH( 1 );
   fr.hInstance        = (HINSTANCE) HB_PARWH( 2 );
   fr.Flags            = (DWORD)  hb_parnl( 3 );
   fr.lpstrFindWhat    = (LPTSTR) hb_parcx( 4 );
   fr.lpstrReplaceWith = NULL ;
   fr.wFindWhatLen     = (WORD) hb_parclen(4);
   fr.wReplaceWithLen  = 0 ;
   fr.lCustData        = 0 ;
//   fr.lpfnHook         = HB_ISNIL(5) ? NULL : __DlgProc ;
   fr.lpTemplateName   = NULL ;


   HB_RETWH( FindText( &fr ) );
}

//----------------------------------------------------------------------------

// to be called called via FindText (in wincdlg.prg)

HB_FUNC( VWN__REPLACETEXT )
{
   FINDREPLACE fr ;

   fr.lStructSize = sizeof( fr );

   fr.hwndOwner        = (HWND) HB_PARWH( 1 );
   fr.hInstance        = (HINSTANCE) HB_PARWH( 2 );
   fr.Flags            = (DWORD)  hb_parnl( 3 );
   fr.lpstrFindWhat    = (LPTSTR) hb_parcx( 4 )  ;
   fr.lpstrReplaceWith = (LPTSTR) hb_parcx( 5 )  ;
   fr.wFindWhatLen     = (WORD) hb_parclen( 4 );
   fr.wReplaceWithLen  = (WORD) hb_parclen( 5 );
   fr.lCustData        = 0 ;
//   fr.lpfnHook         = HB_ISNIL(5) ? NULL : __DlgProc ;
   fr.lpTemplateName   = NULL ;

   HB_RETWH( FindText( &fr ) );
}


//----------------------------------------------------------------------------

HB_FUNC( VWN_PRINTDLG )
{

   PRINTDLG *pd  = ( PRINTDLG * ) hb_parc( 1 );
                       //hb_param( 1, HB_IT_STRING )->item.asString.value;

   pd->lStructSize = sizeof(PRINTDLG);

   if ( PrintDlg( pd ) )
   {
      hb_storclen( (char*) pd, sizeof(PRINTDLG), 1 );
      hb_retl(TRUE);
   }
   else
     hb_retl(FALSE);
}

//----------------------------------------------------------------------------

//NT

/*
HB_FUNC( VWN_PRINTDLGEX )
{

   PRINTDLGEX *pd  = ( PRINTDLGEX * ) hb_param( 1, HB_IT_STRING )->item.asString.value;

   pd->lStructSize = sizeof(PRINTDLGEX);

   if ( PrintDlgEx( pd ) )
   {
      hb_storclen( (char*) pd, sizeof(PRINTDLGEX), 1 );
      hb_retl(TRUE);
   }
   else
     hb_retl(FALSE);
}
*/

//----------------------------------------------------------------------------

HB_FUNC( VWN_PAGESETUPDLG )
{

   PAGESETUPDLG *psd = (PAGESETUPDLG * ) hb_parc( 1 );
                        //hb_param( 1, HB_IT_STRING )->item.asString.value;

   psd->lStructSize = sizeof(PAGESETUPDLG);

   if ( PageSetupDlg( psd ) )
   {
      hb_storclen( (char*) psd, sizeof(PAGESETUPDLG), 1 );
      hb_retl(TRUE);
   }
   else
   {
     hb_retl(FALSE);
   }
}


//----------------------------------------------------------------------------

// nColor := ChooseColor( [hParentWnd],[nInitColor],[aCustColors[16]],[nFlags])

HB_FUNC( VWN_CHOOSECOLOR )
{
   CHOOSECOLOR cc ;
   COLORREF crCustClr[16] ;
   int i ;

   for( i = 0 ; i <16 ; i++ )
     crCustClr[i] = (ISARRAY(3) ? ( COLORREF ) hb_parnl(3,i+1) : RGB(0,0,0));
                                    // GetSysColor(COLOR_BTNFACE));

   cc.lStructSize    = sizeof( CHOOSECOLOR );
   cc.hwndOwner      = HB_ISNIL(1) ? GetActiveWindow():(HWND) HB_PARWH(1);
   cc.rgbResult      = (COLORREF)ISNIL(2) ?  0 : hb_parnl(2);
   cc.lpCustColors   = crCustClr ;
   cc.Flags          = (WORD) (ISNIL(4) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hb_parnl(4) );
   if ( ChooseColorA( &cc ) )
   {
      hb_retnl( cc.rgbResult );
   }
}


//----------------------------------------------------------------------------

// to be called via GetOpenFileName in wincdlg.prg

HB_FUNC( VWN__GETOPENFILENAME )
{
   OPENFILENAME ofn;
   char *szFileName = ( char * ) hb_xgrab( hb_parclen( 2 ) + 1 );

   hb_strncpy( szFileName, hb_parcx( 2 ), hb_parclen( 2 ) );

   ZeroMemory( &ofn, sizeof(ofn) );
   ofn.hInstance       = GetModuleHandle(NULL)  ;
   ofn.lStructSize     = sizeof(ofn);
   ofn.hwndOwner       = (ISNIL  (1) ? GetActiveWindow() : (HWND) HB_PARWH(1));
   ofn.lpstrTitle      = hb_parc (3);
   ofn.lpstrFilter     = hb_parc (4);
   ofn.Flags           = (ISNIL  (5) ? OFN_EXPLORER : hb_parnl(5) );
   ofn.lpstrInitialDir = hb_parc (6);
   ofn.lpstrDefExt     = hb_parc (7);
   ofn.nFilterIndex    = hb_parni(8);
   ofn.lpstrFile       = szFileName;
   ofn.nMaxFile        = hb_parcsiz(2);

   if( GetOpenFileName( &ofn ) )
   {
      hb_stornl( ofn.nFilterIndex, 8 );
      hb_storclen( szFileName, hb_parcsiz(2), 2 );
      hb_xfree( szFileName );
      hb_retc( ( char * ) ofn.lpstrFile );
   }
   else
   {
      hb_retc( NULL );
   }
}



//----------------------------------------------------------------------------

// to be called via GetSaveFileName in wincdlg.prg

HB_FUNC( VWN__GETSAVEFILENAME )
{
    OPENFILENAME ofn;
    char szFileName[ MAX_PATH + 1 ];
    hb_strncpy( szFileName, hb_parc( 2 ), sizeof( szFileName ) - 1 );
    ZeroMemory( &ofn, sizeof( ofn ) );
    ofn.hInstance       = GetModuleHandle(NULL);
    ofn.lStructSize     = sizeof(ofn);
    ofn.hwndOwner       = ISNIL   (1)  ? GetActiveWindow() : (HWND) HB_PARWH(1);
    ofn.lpstrTitle      = hb_parc (3);
    ofn.lpstrFilter     = hb_parc (4);
    ofn.Flags           = (ISNIL  (5) ? OFN_FILEMUSTEXIST|OFN_EXPLORER : hb_parnl(4) );
    ofn.lpstrInitialDir = hb_parc (6);
    ofn.lpstrDefExt     = hb_parc (7);
    ofn.nFilterIndex    = hb_parni(8);
    ofn.lpstrFile       = szFileName;
    ofn.nMaxFile        = MAX_PATH;
    if(GetSaveFileName(&ofn))
     {
      hb_stornl(ofn.nFilterIndex , 8 );
      hb_retc( ofn.lpstrFile );
     }
    else
     {
      hb_retc( NULL );
   }
}

//----------------------------------------------------------------------------

//SYNTAX: SHBrowseForFolder([<hWnd>],[<cTitle>],<nFlags>,[<nFolderType>])

HB_FUNC( VWN_SHBROWSEFORFOLDER )
{
   HWND hwnd = ISNIL   (1)  ? GetActiveWindow() : (HWND) HB_PARWH(1);
   BROWSEINFO BrowseInfo;
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH + 1 );
   LPITEMIDLIST pidlBrowse;

   SHGetSpecialFolderLocation(hwnd, HB_ISNIL(4) ? CSIDL_DRIVES : hb_parni(4), &pidlBrowse);
   BrowseInfo.hwndOwner = hwnd;
   BrowseInfo.pidlRoot = pidlBrowse;
   BrowseInfo.pszDisplayName = lpBuffer;
   BrowseInfo.lpszTitle = ISNIL (2) ?  "Select a Folder" : hb_parcx(2);
   BrowseInfo.ulFlags = hb_parni(3);
   BrowseInfo.lpfn = NULL;
   BrowseInfo.lParam = 1;
   BrowseInfo.iImage = 0;
   pidlBrowse = SHBrowseForFolder(&BrowseInfo);

   if ( pidlBrowse )
   {
     SHGetPathFromIDList(pidlBrowse,lpBuffer);
     hb_retc( lpBuffer );
   }
   else
   {
     hb_retc( NULL );
   }

   hb_xfree( lpBuffer);
}
