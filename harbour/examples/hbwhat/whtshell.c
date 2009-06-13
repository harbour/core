/*
 * $Id$
 */

// hbwhat
// Shell API

#undef _WIN32_WINNT
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
// SHSTDAPI_(UINT) DragQueryFileA(HDROP,UINT,LPSTR,UINT);


HB_FUNC( VWN_DRAGQUERYFILE )
{

  char *cFile ;
  UINT iRet   ;

  if ( hb_parni( 4 ) > 0  )
    cFile = (char*) hb_xgrab( hb_parni(4) + 1 );
  else
    cFile = (char*) hb_xgrab( strlen( hb_parcx(3) ) + 1 );


  iRet = DragQueryFile( (HDROP) HB_PARWH( 1 ),
                        (UINT) hb_parni( 2 ) ,
                        hb_parni(4) > 0 ? cFile : NULL ,
                        (UINT) hb_parni( 4 )
                      );

   if (hb_parni( 4 ) > 0)
   {
      hb_storclen( cFile, iRet, 3 );
      hb_xfree( cFile );
   }
   hb_retni( iRet );
 }


//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) DragQueryPoint(HDROP,LPPOINT);


HB_FUNC( VWN_DRAGQUERYPOINT )
{
   POINT lpPoInt ;
   BOOL lRet ;
   lRet = DragQueryPoint( (HDROP) HB_PARWH( 1 ),(LPPOINT) &lpPoInt )  ;
   if (ISBYREF( 2 ) ){
      hb_stornl(2,lpPoInt.x,1);
      hb_stornl(2,lpPoInt.y,2);
   }
   hb_retl( lRet );

}


//-----------------------------------------------------------------------------
// SHSTDAPI_(void) DragFinish(HDROP);


HB_FUNC( VWN_DRAGFINISH )
{
   DragFinish( (HDROP) HB_PARWH( 1 ) );
}

//-----------------------------------------------------------------------------
// SHSTDAPI_(void) DragAcceptFiles(HWND,BOOL);


HB_FUNC( VWN_DRAGACCEPTFILES )
{
   DragAcceptFiles( (HWND) HB_PARWH( 1 ), hb_parl( 2 ) );
}

//-----------------------------------------------------------------------------
// SHSTDAPI_(HINSTANCE) ShellExecuteA(HWND hwnd, LPCSTR lpOperation, LPCSTR lpFile, LPCSTR lpParameters, LPCSTR lpDirectory, INT nShowCmd);


HB_FUNC( VWN_SHELLEXECUTE )
{
   HB_RETWH( ShellExecute( (HWND) HB_PARWH( 1 )     ,
                                  (LPCSTR) hb_parcx( 2 )    ,
                                  (LPCSTR) hb_parcx( 3 )    ,
                                  HB_ISNIL(4) ? NULL : (LPCSTR) hb_parcx( 4 )    ,
                                  (LPCSTR) hb_parcx( 5 )    ,
                                   hb_parni( 6 )
                                 ) );
}

//-----------------------------------------------------------------------------
// SHSTDAPI_(HINSTANCE) FindExecutableA(LPCSTR lpFile, LPCSTR lpDirectory, LPSTR lpResult);

HB_FUNC( VWN_FINDEXECUTABLE )
{

  char cBuffer[MAX_PATH];
  HINSTANCE hInst ;


   hInst = FindExecutable( (LPCSTR) hb_parcx( 1 )    ,
                           (LPCSTR) hb_parcx( 2 )    ,
                           (LPSTR)  cBuffer
                         );

   HB_RETWH( hInst);

   if( ( HB_PTRDIFF ) hInst > 32 )
      hb_storc( cBuffer, 3 );
}


//-----------------------------------------------------------------------------
// SHSTDAPI_(LPWSTR *) CommandLineToArgvW(LPCWSTR lpCmdLine, int*pNumArgs);

// no info

/*
HB_FUNC( VWN_COMMANDLINETOARGVW )
{
   intpNumArgs Intpnumargs ;

   // Your code goes here

// () CommandLineToArgvW( (LPWSTR) hb_parcx( 1 ) ,
                          (LPCWSTR) hb_parcx( 2 ),
                          &Intpnumargs
                        ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(INT) ShellAboutA(HWND hWnd, LPCSTR szApp, LPCSTR szOtherStuff, HICON hIcon);

HB_FUNC( VWN_SHELLABOUT )
{
   hb_retni( ShellAbout( (HWND) HB_PARWH(1),
                         (LPCSTR) hb_parcx(2),
                         (LPCSTR) hb_parcx(3),
                         (ISNIL(4) ? NULL : (HICON) HB_PARWH(4) )
                       ) );
}


//-----------------------------------------------------------------------------
// SHSTDAPI_(UINT) SHAppBarMessage(DWORD dwMessage, PAPPBARDATA pData);

/*

HB_FUNC( VWN_SHAPPBARMESSAGE )
{
   PAPPBARDATA pData     ;

   // Your code goes here

// () SHAppBarMessage( (DWORD) hb_parnl( 1 ), pData ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(DWORD) DoEnvironmentSubstA(LPSTR szString, UINT cchString);

#if !defined(__WATCOMC__) && !defined(__MINGW32__) && !defined(__CYGWIN__)
HB_FUNC( VWN_DOENVIRONMENTSUBST )
{
   hb_retnl((LONG) DoEnvironmentSubst( (LPSTR) hb_parcx( 1 ) ,
                                       (UINT) hb_parni( 2 )
                                     ) );
}
#endif
//-----------------------------------------------------------------------------
// SHSTDAPI_(UINT) ExtractIconExA(LPCSTR lpszFile, int nIconIndex, HICON *phiconLarge, HICON *phiconSmall, UINT nIcons);

/*
HB_FUNC( VWN_EXTRACTICONEX )
{
   HICON iLarge;
   HICON iSmall;
   UINT  nIcons=hb_parni(5);

   hb_retni( ExtractIconEx( (LPCSTR) hb_parcx( 1 ),
                            hb_parni( 2 )        ,
                            &iLarge              ,
                            &iSmall              ,
                            nIcons               ,
                           ) );


       // unfinished


}
*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(int) SHFileOperationA(LPSHFILEOPSTRUCTA lpFileOp);

HB_FUNC( VWN_SHFILEOPERATION )
{
   SHFILEOPSTRUCT *sfo = (SHFILEOPSTRUCT *) hb_parc( 1 ); //hb_param(1, HB_IT_STRING)->item.asString.value;
   hb_retni( SHFileOperation( sfo ) );
}

//-----------------------------------------------------------------------------
// SHSTDAPI_(void) SHFreeNameMappings(HANDLE hNameMappings);

HB_FUNC( VWN_SHFREENAMEMAPPINGS )
{
   SHFreeNameMappings( (HANDLE) HB_PARWH( 1 ) );
}

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) ShellExecuteExA(LPSHELLEXECUTEINFOA lpExecInfo);

// uses structure

HB_FUNC( VWN_SHELLEXECUTEEX )
{
   SHELLEXECUTEINFO *ExecInfo = (SHELLEXECUTEINFO *) hb_parc( 1 ); //hb_param(1, HB_IT_STRING)->item.asString.value;
   hb_retl( ShellExecuteEx( ExecInfo ) );
}



//-----------------------------------------------------------------------------
// SHSTDAPI_(void) WinExecErrorA(HWND hwnd, int error, LPCSTR lpstrFileName, LPCSTR lpstrTitle);
/*


// NT only ?
// ????

HB_FUNC( VWN_WINEXECERROR )
{
   WinExecError( (HWND) HB_PARWH( 1 ) ,
                 hb_parni( 2 )        ,
                 (LPCSTR) hb_parcx( 3 ),
                 (LPCSTR) hb_parcx( 4 )
                  );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) SHCreateProcessAsUserW(PSHCREATEPROCESSINFOW pscpi);

/*
HB_FUNC( VWN_SHCREATEPROCESSASUSERW )
{
   PSHCREATEPROCESSINFOW pscpi ;

   // Your code goes here

   hb_retl( SHCreateProcessAsUserW( pscpi ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI SHQueryRecycleBinA(LPCSTR pszRootPath, LPSHQUERYRBINFO pSHQueryRBInfo);

/*

// check the prototype !!!

HB_FUNC( VWN_SHQUERYRECYCLEBIN )
{
   LPSHQUERYRBINFO pSHQueryRBInfo ;

   // Your code goes here

// (SHSTDAPI) SHQueryRecycleBin( (LPCSTR) hb_parcx( 1 ), pSHQueryRBInfo ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI SHEmptyRecycleBinA(HWND hwnd, LPCSTR pszRootPath, DWORD dwFlags);

// check the prototype


#if(WINVER >= 0x0500)

HB_FUNC( VWN_SHEMPTYRECYCLEBIN )
{
   hb_retnl(  SHEmptyRecycleBin( (HWND) HB_PARWH( 1 ) ,
                                 (LPCSTR) hb_parcx( 2 ),
                                 (DWORD) hb_parnl( 3 )
                                ) );
}

#endif

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) Shell_NotifyIconA(DWORD dwMessage, PNOTIFYICONDATAA lpData);

//uses structure

HB_FUNC( VWN_SHELL_NOTIFYICON )
{
   NOTIFYICONDATA * Data =  (NOTIFYICONDATA * ) hb_parc( 2 ); //hb_param(2, HB_IT_STRING)->item.asString.value;
   hb_retl( Shell_NotifyIcon( (DWORD) hb_parnl( 1 ), Data ) );
}



//-----------------------------------------------------------------------------
// SHSTDAPI_(DWORD_PTR) SHGetFileInfoA(LPCSTR pszPath, DWORD dwFileAttributes, SHFILEINFOA *psfi, UINT cbFileInfo, UINT uFlags);

/*

HB_FUNC( VWN_SHGETFILEINFO )
{
   SHFILEINFOA psfi             ;

   // Your code goes here

// hb_retnl( SHGetFileInfo( (LPCSTR) hb_parcx( 1 )    ,
                            (DWORD) hb_parnl( 2 )    ,
                            &psfi                    ,
                            (UINT) hb_parni( 4 )     ,
                            (UINT) hb_parni( 5 )
                          ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) SHGetDiskFreeSpaceExA(LPCSTR pszDirectoryName, ULARGE_INTEGER* pulFreeBytesAvailableToCaller, ULARGE_INTEGER* pulTotalNumberOfBytes, ULARGE_INTEGER* pulTotalNumberOfFreeBytes);

/*

HB_FUNC( VWN_SHGETDISKFREESPACEEX )
{
   ULARGE_INTEGER pulFreeBytesAvailableToCaller ;
   ULARGE_INTEGER pulTotalNumberOfBytes         ;
   ULARGE_INTEGER pulTotalNumberOfFreeBytes     ;

   // Your code goes here

// () SHGetDiskFreeSpaceEx( (LPCSTR) hb_parcx( 1 )         ,
                            &pulFreeBytesAvailableToCaller,
                            &pulTotalNumberOfBytes        ,
                            &pulTotalNumberOfFreeBytes
                          ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) SHGetNewLinkInfoA(LPCSTR pszLinkTo, LPCSTR pszDir, LPSTR pszName, BOOL *pfMustCopy, UINT uFlags);

/*
HB_FUNC( VWN_SHGETNEWLINKINFO )
{
   hb_retl( SHGetNewLinkInfo( (LPCSTR) hb_parcx( 1 ),
                             (LPCSTR) hb_parcx( 2 ),
                             (LPSTR) hb_parcx( 3 ) ,
                             hb_parl( 4 )         ,
                             (UINT) hb_parni( 5 )
                           ) );
}

*/

//-----------------------------------------------------------------------------
// SHSTDAPI_(BOOL) SHInvokePrinterCommandA(HWND hwnd, UINT uAction, LPCSTR lpBuf1, LPCSTR lpBuf2, BOOL fModal);

#if(WINVER >= 0x0500)

HB_FUNC( VWN_SHINVOKEPRINTERCOMMAND )
{
   hb_retl( SHInvokePrinterCommand( (HWND) HB_PARWH( 1 ) ,
                                    (UINT) hb_parni( 2 ) ,
                                    (LPCSTR) hb_parcx( 3 ),
                                    (LPCSTR) hb_parcx( 4 ),
                                     hb_parl( 5 )
                                   ) );
}

#endif

//-----------------------------------------------------------------------------
// SHSTDAPI SHLoadNonloadedIconOverlayIdentifiers(void);

// verify the prototype

/*
HB_FUNC( VWN_SHLOADNONLOADEDICONOVERLAYIDENTIFIERS )
{
// (SHSTDAPI) SHLoadNonloadedIconOverlayIdentifiers(  ) );
}
*/

//-----------------------------------------------------------------------------
// End.
