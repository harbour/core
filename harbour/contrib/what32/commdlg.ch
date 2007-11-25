/*
 * $Id$
 */

* Automatically translated from commdlg.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/************************************************************************
*                                                                       *
*   commdlg.h -- This module defines the 32-Bit Common Dialog APIs      *
*                                                                       *
*   Copyright (c) 1992-1999, Microsoft Corp. All rights reserved.       *
*                                                                       *
************************************************************************/

#ifndef _COMMDLG_H

  #define _COMMDLG_H

  #define OPENFILENAME_SIZE_VERSION_400A  CDSIZEOF_STRUCT(OPENFILENAMEA,lpTemplateName)
  #define OPENFILENAME_SIZE_VERSION_400W  CDSIZEOF_STRUCT(OPENFILENAMEW,lpTemplateName)

  #ifdef UNICODE
  #define OPENFILENAME_SIZE_VERSION_400  OPENFILENAME_SIZE_VERSION_400W
  #else
  #define OPENFILENAME_SIZE_VERSION_400  OPENFILENAME_SIZE_VERSION_400A
  #endif // !UNICODE


  #define OFN_READONLY                          1
  #define OFN_OVERWRITEPROMPT                   2
  #define OFN_HIDEREADONLY                      4
  #define OFN_NOCHANGEDIR                       8
  #define OFN_SHOWHELP                         16
  #define OFN_ENABLEHOOK                       32
  #define OFN_ENABLETEMPLATE                   64
  #define OFN_ENABLETEMPLATEHANDLE            128
  #define OFN_NOVALIDATE                      256
  #define OFN_ALLOWMULTISELECT                512
  #define OFN_EXTENSIONDIFFERENT             1024
  #define OFN_PATHMUSTEXIST                  2048
  #define OFN_FILEMUSTEXIST                  4096
  #define OFN_CREATEPROMPT                   8192
  #define OFN_SHAREAWARE                    16384
  #define OFN_NOREADONLYRETURN              32768
  #define OFN_NOTESTFILECREATE              65536
  #define OFN_NONETWORKBUTTON              131072
  #define OFN_NOLONGNAMES                  262144     // force no long names for 4.x modules
  #define OFN_EXPLORER                     524288     // new look commdlg
  #define OFN_NODEREFERENCELINKS          1048576
  #define OFN_LONGNAMES                   2097152     // force long names for 3.x modules
  #define OFN_ENABLEINCLUDENOTIFY         4194304     // send include message to callback
  #define OFN_ENABLESIZING                8388608
  #define OFN_DONTADDTORECENT            33554432
  #define OFN_FORCESHOWHIDDEN           268435456    // Show All files including System and hidden files



  // Return values for the registered message sent to the hook function
  // when a sharing violation occurs.  OFN_SHAREFALLTHROUGH allows the
  // filename to be accepted, OFN_SHARENOWARN rejects the name but puts
  // up no warning (returned when the app has already put up a warning
  // message), and OFN_SHAREWARN puts up the default warning message
  // for sharing violations.
  //
  // Note:  Undefined return values map to OFN_SHAREWARN, but are
  //        reserved for future use.

  #define OFN_SHAREFALLTHROUGH     2
  #define OFN_SHARENOWARN          1
  #define OFN_SHAREWARN            0

  // Notifications from Open or Save dialog
  #define CDN_INITDONE            (CDN_FIRST -      0)
  #define CDN_SELCHANGE           (CDN_FIRST -      1)
  #define CDN_FOLDERCHANGE        (CDN_FIRST -      2)
  #define CDN_SHAREVIOLATION      (CDN_FIRST -      3)
  #define CDN_HELP                (CDN_FIRST -      4)
  #define CDN_FILEOK              (CDN_FIRST -      5)
  #define CDN_TYPECHANGE          (CDN_FIRST -      6)
  #define CDN_INCLUDEITEM         (CDN_FIRST -      7)


  #define CDM_FIRST       (WM_USER + 100)
  #define CDM_LAST        (WM_USER + 200)

  // Messages to query information from the Open or Save dialogs

  // lParam = pointer to text buffer that gets filled in
  // wParam = max number of characters of the text buffer (including NULL)
  // return = < 0 if error; number of characters needed (including NULL)
  #define CDM_GETSPEC             (CDM_FIRST +      0)

  // lParam = pointer to text buffer that gets filled in
  // wParam = max number of characters of the text buffer (including NULL)
  // return = < 0 if error; number of characters needed (including NULL)
  #define CDM_GETFILEPATH         (CDM_FIRST +      1)

  // lParam = pointer to text buffer that gets filled in
  // wParam = max number of characters of the text buffer (including NULL)
  // return = < 0 if error; number of characters needed (including NULL)
  #define CDM_GETFOLDERPATH       (CDM_FIRST +      2)

  // lParam = pointer to ITEMIDLIST buffer that gets filled in
  // wParam = size of the ITEMIDLIST buffer
  // return = < 0 if error; length of buffer needed
  #define CDM_GETFOLDERIDLIST     (CDM_FIRST +      3)


  // lParam = pointer to a string
  // wParam = ID of control to change
  // return = not used
  #define CDM_SETCONTROLTEXT      (CDM_FIRST +      4)


  // lParam = not used
  // wParam = ID of control to change
  // return = not used
  #define CDM_HIDECONTROL         (CDM_FIRST +      5)


  // lParam = pointer to default extension (no dot)
  // wParam = not used
  // return = not used
  #define CDM_SETDEFEXT           (CDM_FIRST +      6)


  #define CC_RGBINIT                        1
  #define CC_FULLOPEN                       2
  #define CC_PREVENTFULLOPEN                4
  #define CC_SHOWHELP                       8
  #define CC_ENABLEHOOK                    16
  #define CC_ENABLETEMPLATE                32
  #define CC_ENABLETEMPLATEHANDLE          64
  #define CC_SOLIDCOLOR                   128
  #define CC_ANYCOLOR                     256


  #define FR_DOWN                                  1
  #define FR_WHOLEWORD                             2
  #define FR_MATCHCASE                             4
  #define FR_FINDNEXT                              8
  #define FR_REPLACE                              16
  #define FR_REPLACEALL                           32
  #define FR_DIALOGTERM                           64
  #define FR_SHOWHELP                            128
  #define FR_ENABLEHOOK                          256
  #define FR_ENABLETEMPLATE                      512
  #define FR_NOUPDOWN                           1024
  #define FR_NOMATCHCASE                        2048
  #define FR_NOWHOLEWORD                        4096
  #define FR_ENABLETEMPLATEHANDLE               8192
  #define FR_HIDEUPDOWN                        16384
  #define FR_HIDEMATCHCASE                     32768
  #define FR_HIDEWHOLEWORD                     65536
  #define FR_RAW                              131072
  #define FR_MATCHDIAC                     536870912
  #define FR_MATCHKASHIDA                 1073741824
  #define FR_MATCHALEFHAMZA               2147483648


  #define CF_SCREENFONTS                      1
  #define CF_PRINTERFONTS                     2
  #define CF_BOTH                    (CF_SCREENFONTS + CF_PRINTERFONTS)
  #define CF_SHOWHELP                          4
  #define CF_ENABLEHOOK                        8
  #define CF_ENABLETEMPLATE                   16
  #define CF_ENABLETEMPLATEHANDLE             32
  #define CF_INITTOLOGFONTSTRUCT              64
  #define CF_USESTYLE                        128
  #define CF_EFFECTS                         256
  #define CF_APPLY                           512
  #define CF_ANSIONLY                       1024
  #define CF_SCRIPTSONLY             CF_ANSIONLY
  #define CF_NOVECTORFONTS                  2048
  #define CF_NOOEMFONTS              CF_NOVECTORFONTS
  #define CF_NOSIMULATIONS                  4096
  #define CF_LIMITSIZE                      8192
  #define CF_FIXEDPITCHONLY                16384
  #define CF_WYSIWYG                       32768 // must also have CF_SCREENFONTS & CF_PRINTERFONTS
  #define CF_FORCEFONTEXIST                65536
  #define CF_SCALABLEONLY                 131072
  #define CF_TTONLY                       262144
  #define CF_NOFACESEL                    524288
  #define CF_NOSTYLESEL                  1048576
  #define CF_NOSIZESEL                   2097152
  #define CF_SELECTSCRIPT                4194304
  #define CF_NOSCRIPTSEL                 8388608
  #define CF_NOVERTFONTS                16777216

  // these are extra nFontType bits that are added to what is returned to the
  // EnumFonts callback routine

  #define SIMULATED_FONTTYPE     32768
  #define PRINTER_FONTTYPE       16384
  #define SCREEN_FONTTYPE         8192
  #define BOLD_FONTTYPE            256
  #define ITALIC_FONTTYPE          512
  #define REGULAR_FONTTYPE        1024

  // EnumFonts callback routine only uses these bits, so we can use the rest

  // #define RASTER_FONTTYPE     0x001
  // #define DEVICE_FONTTYPE     0x002
  // #define TRUETYPE_FONTTYPE   0x004

  #ifdef WINNT
  #define PS_OPENTYPE_FONTTYPE    65536
  #define TT_OPENTYPE_FONTTYPE   131072
  #define TYPE1_FONTTYPE         262144
  #endif

  #define WM_CHOOSEFONT_GETLOGFONT      (WM_USER + 1)
  #define WM_CHOOSEFONT_SETLOGFONT      (WM_USER + 101)
  #define WM_CHOOSEFONT_SETFLAGS        (WM_USER + 102)

  // strings used to obtain unique window message for communication
  // between dialog and caller

  #define LBSELCHSTRINGA  "commdlg_LBSelChangedNotify"
  #define SHAREVISTRINGA  "commdlg_ShareViolation"
  #define FILEOKSTRINGA   "commdlg_FileNameOK"
  #define COLOROKSTRINGA  "commdlg_ColorOK"
  #define SETRGBSTRINGA   "commdlg_SetRGBColor"
  #define HELPMSGSTRINGA  "commdlg_help"
  #define FINDMSGSTRINGA  "commdlg_FindReplace"

  #define LBSELCHSTRINGW  L"commdlg_LBSelChangedNotify"
  #define SHAREVISTRINGW  L"commdlg_ShareViolation"
  #define FILEOKSTRINGW   L"commdlg_FileNameOK"
  #define COLOROKSTRINGW  L"commdlg_ColorOK"
  #define SETRGBSTRINGW   L"commdlg_SetRGBColor"
  #define HELPMSGSTRINGW  L"commdlg_help"
  #define FINDMSGSTRINGW  L"commdlg_FindReplace"

  #define LBSELCHSTRING  LBSELCHSTRINGW
  #define SHAREVISTRING  SHAREVISTRINGW
  #define FILEOKSTRING   FILEOKSTRINGW
  #define COLOROKSTRING  COLOROKSTRINGW
  #define SETRGBSTRING   SETRGBSTRINGW
  #define HELPMSGSTRING  HELPMSGSTRINGW
  #define FINDMSGSTRING  FINDMSGSTRINGW

  // HIWORD values for lParam of commdlg_LBSelChangeNotify message
  #define CD_LBSELNOITEMS -1
  #define CD_LBSELCHANGE   0
  #define CD_LBSELSUB      1
  #define CD_LBSELADD      2



  //-------------------------------------------------------------------------
  //
  //  IPrintDialogCallback Interface
  //
  //  IPrintDialogCallback::InitDone()
  //    This function is called by PrintDlgEx when the system has finished
  //    initializing the main page of the print dialog.  This function
  //    should return S_OK if it has processed the action or S_FALSE to let
  //    PrintDlgEx perform the default action.
  //
  //  IPrintDialogCallback::SelectionChange()
  //    This function is called by PrintDlgEx when a selection change occurs
  //    in the list view that displays the currently installed printers.
  //    This function should return S_OK if it has processed the action or
  //    S_FALSE to let PrintDlgEx perform the default action.
  //
  //  IPrintDialogCallback::HandleMessage(hDlg, uMsg, wParam, lParam, pResult)
  //    This function is called by PrintDlgEx when a message is sent to the
  //    child window of the main page of the print dialog.  This function
  //    should return S_OK if it has processed the action or S_FALSE to let
  //    PrintDlgEx perform the default action.
  //
  //  IObjectWithSite::SetSite(punkSite)
  //    IPrintDialogCallback usually paired with IObjectWithSite.
  //    Provides the IUnknown pointer of the site to QI for the
  //    IPrintDialogServices interface.
  //
  //-------------------------------------------------------------------------



  //-------------------------------------------------------------------------
  //
  //  IPrintDialogServices Interface
  //
  //  IPrintDialogServices::GetCurrentDevMode(pDevMode, pcbSize)
  //    Returns the DEVMODE structure for the currently selected printer.
  //
  //  IPrintDialogServices::GetCurrentPrinterName(pPrinterName, pcchSize)
  //    Returns the printer name for the currently selected printer.
  //
  //  IPrintDialogServices::GetCurrentPortName(pPortName, pcchSize)
  //    Returns the port name for the currently selected printer.
  //
  //-------------------------------------------------------------------------



  //
  //  Flags for PrintDlg and PrintDlgEx.
  //
  #define PD_ALLPAGES                             0
  #define PD_SELECTION                            1
  #define PD_PAGENUMS                             2
  #define PD_NOSELECTION                          4
  #define PD_NOPAGENUMS                           8
  #define PD_COLLATE                             16
  #define PD_PRINTTOFILE                         32
  #define PD_PRINTSETUP                          64
  #define PD_NOWARNING                          128
  #define PD_RETURNDC                           256
  #define PD_RETURNIC                           512
  #define PD_RETURNDEFAULT                     1024
  #define PD_SHOWHELP                          2048
  #define PD_ENABLEPRINTHOOK                   4096
  #define PD_ENABLESETUPHOOK                   8192
  #define PD_ENABLEPRINTTEMPLATE              16384
  #define PD_ENABLESETUPTEMPLATE              32768
  #define PD_ENABLEPRINTTEMPLATEHANDLE        65536
  #define PD_ENABLESETUPTEMPLATEHANDLE       131072
  #define PD_USEDEVMODECOPIES                262144
  #define PD_USEDEVMODECOPIESANDCOLLATE      262144
  #define PD_DISABLEPRINTTOFILE              524288
  #define PD_HIDEPRINTTOFILE                1048576
  #define PD_NONETWORKBUTTON                2097152
  #define PD_CURRENTPAGE                    4194304
  #define PD_NOCURRENTPAGE                  8388608
  #define PD_EXCLUSIONFLAGS                16777216
  #define PD_USELARGETEMPLATE             268435456


  //
  //  Exclusion flags for PrintDlgEx.
  //
  #define PD_EXCL_COPIESANDCOLLATE       (DM_COPIES + DM_COLLATE)


  //
  //  Define the start page for the print dialog when using PrintDlgEx.
  //
  #define START_PAGE_GENERAL             4294967295


  //
  //  Result action ids for PrintDlgEx.
  //
  #define PD_RESULT_CANCEL               0
  #define PD_RESULT_PRINT                1
  #define PD_RESULT_APPLY                2


  //
  //  Device Names structure for PrintDlg and PrintDlgEx.
  //


  #define DN_DEFAULTPRN           1


  #define WM_PSD_PAGESETUPDLG     (WM_USER  )
  #define WM_PSD_FULLPAGERECT     (WM_USER+1)
  #define WM_PSD_MINMARGINRECT    (WM_USER+2)
  #define WM_PSD_MARGINRECT       (WM_USER+3)
  #define WM_PSD_GREEKTEXTRECT    (WM_USER+4)
  #define WM_PSD_ENVSTAMPRECT     (WM_USER+5)
  #define WM_PSD_YAFULLPAGERECT   (WM_USER+6)


  #define PSD_DEFAULTMINMARGINS                      0 // default (printer's)
  #define PSD_INWININIINTLMEASURE                    0 // 1st of 4 possible

  #define PSD_MINMARGINS                             1 // use caller's
  #define PSD_MARGINS                                2 // use caller's
  #define PSD_INTHOUSANDTHSOFINCHES                  4 // 2nd of 4 possible
  #define PSD_INHUNDREDTHSOFMILLIMETERS              8 // 3rd of 4 possible
  #define PSD_DISABLEMARGINS                        16
  #define PSD_DISABLEPRINTER                        32
  #define PSD_NOWARNING                            128 // must be same as PD_*
  #define PSD_DISABLEORIENTATION                   256
  #define PSD_RETURNDEFAULT                       1024 // must be same as PD_*
  #define PSD_DISABLEPAPER                         512
  #define PSD_SHOWHELP                            2048 // must be same as PD_*
  #define PSD_ENABLEPAGESETUPHOOK                 8192 // must be same as PD_*
  #define PSD_ENABLEPAGESETUPTEMPLATE            32768 // must be same as PD_*
  #define PSD_ENABLEPAGESETUPTEMPLATEHANDLE     131072 // must be same as PD_*
  #define PSD_ENABLEPAGEPAINTHOOK               262144
  #define PSD_DISABLEPAGEPAINTING               524288
  #define PSD_NONETWORKBUTTON                  2097152 // must be same as PD_*


  // SHBrowseForFolder()

  // Browsing for directory.
  #define BIF_RETURNONLYFSDIRS   0x0001  // For finding a folder to start document searching
  #define BIF_DONTGOBELOWDOMAIN  0x0002  // For starting the Find Computer
  #define BIF_STATUSTEXT         0x0004   // Top of the dialog has 2 lines of text for BROWSEINFO.lpszTitle and one line if
                                          // this flag is set.  Passing the message BFFM_SETSTATUSTEXTA to the hwnd can set the
                                          // rest of the text.  This is not used with BIF_USENEWUI and BROWSEINFO.lpszTitle gets
                                          // all three lines of text.
  #define BIF_RETURNFSANCESTORS  0x0008
  #define BIF_EDITBOX            0x0010   // Add an editbox to the dialog
  #define BIF_VALIDATE           0x0020   // insist on valid result (or CANCEL)

  #define BIF_NEWDIALOGSTYLE     0x0040   // Use the new dialog layout with the ability to resize
                                          // Caller needs to call OleInitialize() before using this API

  #define BIF_USENEWUI           (BIF_NEWDIALOGSTYLE | BIF_EDITBOX)

  #define BIF_BROWSEINCLUDEURLS  0x0080   // Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)

  #define BIF_BROWSEFORCOMPUTER  0x1000  // Browsing for Computers.
  #define BIF_BROWSEFORPRINTER   0x2000  // Browsing for Printers
  #define BIF_BROWSEINCLUDEFILES 0x4000  // Browsing for Everything
  #define BIF_SHAREABLE          0x8000  // sharable resources displayed (remote shares, requires BIF_USENEWUI)

  // message from browser
  #define BFFM_INITIALIZED        1
  #define BFFM_SELCHANGED         2
  #define BFFM_VALIDATEFAILED    3   // lParam:szPath ret:1(cont),0(EndDialog)

  // messages to browser
  #define BFFM_SETSTATUSTEXT      (WM_USER + 100)
  #define BFFM_ENABLEOK           (WM_USER + 101)
  #define BFFM_SETSELECTION       (WM_USER + 102)



  //-------------------------------------------------------------------------
  //
  // SHGetSpecialFolderLocation
  //
  //  Caller should use SHGetMalloc to obtain an allocator that can free the pidl
  //
  //
  //-------------------------------------------------------------------------
  //
  // registry entries for special paths are kept in :
  #define REGSTR_PATH_SPECIAL_FOLDERS    REGSTR_PATH_EXPLORER TEXT("\\Shell Folders")


  #define CSIDL_DESKTOP                   0x0000        // <desktop>
  #define CSIDL_INTERNET                  0x0001        // Internet Explorer (icon on desktop)
  #define CSIDL_PROGRAMS                  0x0002        // Start Menu\Programs
  #define CSIDL_CONTROLS                  0x0003        // My Computer\Control Panel
  #define CSIDL_PRINTERS                  0x0004        // My Computer\Printers
  #define CSIDL_PERSONAL                  0x0005        // My Documents
  #define CSIDL_FAVORITES                 0x0006        // <user name>\Favorites
  #define CSIDL_STARTUP                   0x0007        // Start Menu\Programs\Startup
  #define CSIDL_RECENT                    0x0008        // <user name>\Recent
  #define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
  #define CSIDL_BITBUCKET                 0x000a        // <desktop>\Recycle Bin
  #define CSIDL_STARTMENU                 0x000b        // <user name>\Start Menu
  #define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
  #define CSIDL_DRIVES                    0x0011        // My Computer
  #define CSIDL_NETWORK                   0x0012        // Network Neighborhood
  #define CSIDL_NETHOOD                   0x0013        // <user name>\nethood
  #define CSIDL_FONTS                     0x0014        // windows\fonts
  #define CSIDL_TEMPLATES                 0x0015
  #define CSIDL_COMMON_STARTMENU          0x0016        // All Users\Start Menu
  #define CSIDL_COMMON_PROGRAMS           0X0017        // All Users\Programs
  #define CSIDL_COMMON_STARTUP            0x0018        // All Users\Startup
  #define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
  #define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
  #define CSIDL_PRINTHOOD                 0x001b        // <user name>\PrintHood
  #define CSIDL_LOCAL_APPDATA             0x001c        // <user name>\Local Settings\Applicaiton Data (non roaming)
  #define CSIDL_ALTSTARTUP                0x001d        // non localized startup
  #define CSIDL_COMMON_ALTSTARTUP         0x001e        // non localized common startup
  #define CSIDL_COMMON_FAVORITES          0x001f
  #define CSIDL_INTERNET_CACHE            0x0020
  #define CSIDL_COOKIES                   0x0021
  #define CSIDL_HISTORY                   0x0022
  #define CSIDL_COMMON_APPDATA            0x0023        // All Users\Application Data
  #define CSIDL_WINDOWS                   0x0024        // GetWindowsDirectory()
  #define CSIDL_SYSTEM                    0x0025        // GetSystemDirectory()
  #define CSIDL_PROGRAM_FILES             0x0026        // C:\Program Files
  #define CSIDL_MYPICTURES                0x0027        // C:\Program Files\My Pictures
  #define CSIDL_PROFILE                   0x0028        // USERPROFILE
  #define CSIDL_SYSTEMX86                 0x0029        // x86 system directory on RISC
  #define CSIDL_PROGRAM_FILESX86          0x002a        // x86 C:\Program Files on RISC
  #define CSIDL_PROGRAM_FILES_COMMON      0x002b        // C:\Program Files\Common
  #define CSIDL_PROGRAM_FILES_COMMONX86   0x002c        // x86 Program Files\Common on RISC
  #define CSIDL_COMMON_TEMPLATES          0x002d        // All Users\Templates
  #define CSIDL_COMMON_DOCUMENTS          0x002e        // All Users\Documents
  #define CSIDL_COMMON_ADMINTOOLS         0x002f        // All Users\Start Menu\Programs\Administrative Tools
  #define CSIDL_ADMINTOOLS                0x0030        // <user name>\Start Menu\Programs\Administrative Tools
  #define CSIDL_CONNECTIONS               0x0031        // Network and Dial-up Connections

  #define CSIDL_FLAG_CREATE               0x8000        // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
  #define CSIDL_FLAG_DONT_VERIFY          0x4000        // combine with CSIDL_ value to return an unverified folder path
  #define CSIDL_FLAG_MASK                 0xFF00        // mask for all possible flag values

#endif
