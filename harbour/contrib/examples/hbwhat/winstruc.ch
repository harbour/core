/*
 * $Id$
 */


// hbwhat
// structures


#Include "wintypes.ch"

#ifndef MAX_PATH
  #define MAX_PATH 260
#endif


/* POINT */

typedef struct tagPOINT {;
    LONG x;
    LONG y;
} POINT

/* RECT */

typedef struct _RECT { ;
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT

typedef struct tagNMHDR {;
    HWND hwndFrom;
    UINT idFrom;
    UINT code;
} NMHDR;



/* CreateFile */

typedef struct _SECURITY_ATTRIBUTES { ;
    DWORD  nLength;
    LPVOID lpSecurityDescriptor;
    BOOL   bInheritHandle;
} SECURITY_ATTRIBUTES

/* register class */

typedef struct _WNDCLASS { ;
    UINT    style;
    WNDPROC lpfnWndProc;
    int     cbClsExtra;
    int     cbWndExtra;
    HANDLE  hInstance;
    HICON   hIcon;
    HCURSOR hCursor;
    HBRUSH  hbrBackground;
    LPCTSTR lpszMenuName;
    LPCTSTR lpszClassName;
} WNDCLASS

/* Bitmap Header Definition */

typedef struct tagBITMAP { ;
     LONG        bmType;
     LONG        bmWidth;
     LONG        bmHeight;
     LONG        bmWidthBytes;
     WORD        bmPlanes;
     WORD        bmBitsPixel;
     LPVOID      bmBits;
} BITMAP, *PBITMAP, NEAR *NPBITMAP, FAR *LPBITMAP

typedef struct tagBITMAPINFOHEADER{; // bmih
    DWORD  biSize;
    LONG   biWidth;
    LONG   biHeight;
    WORD   biPlanes;
    WORD   biBitCount;
    DWORD  biCompression;
    DWORD  biSizeImage;
    LONG   biXPelsPerMeter;
    LONG   biYPelsPerMeter;
    DWORD  biClrUsed;
    DWORD  biClrImportant;
} BITMAPINFOHEADER

/* DIBSECTION */

typedef struct tagDIBSECTION { ;
    BITMAP              dsBm;
    BITMAPINFOHEADER    dsBmih;
    DWORD               dsBitfields[3];
    HANDLE              dshSection;
    DWORD               dsOffset;
} DIBSECTION

/* BRUSH */

typedef struct tagLOGBRUSH { ;
   UINT     lbStyle;
   COLORREF lbColor;
   LONG     lbHatch;
} LOGBRUSH

/* PEN */

typedef struct tagLOGPEN { ;
    UINT     lopnStyle;
    POINT    lopnWidth;
    COLORREF lopnColor;
} LOGPEN

/* ext LOGPEN */



typedef struct tagEXTLOGPEN { ;
    UINT     elpPenStyle;
    UINT     elpWidth;
    UINT     elpBrushStyle;
    COLORREF elpColor;
    LONG     elpHatch;
    DWORD    elpNumEntries;
    DWORD    elpStyleEntry[1];
} EXTLOGPEN

/* LOFFONT */



typedef struct tagLOGFONT { ;
   LONG lfHeight;
   LONG lfWidth;
   LONG lfEscapement;
   LONG lfOrientation;
   LONG lfWeight;
   BYTE lfItalic;
   BYTE lfUnderline;
   BYTE lfStrikeOut;
   BYTE lfCharSet;
   BYTE lfOutPrecision;
   BYTE lfClipPrecision;
   BYTE lfQuality;
   BYTE lfPitchAndFamily;
   TCHAR lfFaceName[32];
} LOGFONT


/* choose font */

typedef struct tagCHOOSEFONT {  ;
    DWORD        lStructSize;
    HWND         hwndOwner;
    HDC          hDC;
    LOGFONT*     lpLogFont;
    INT          iPointSize;
    DWORD        Flags;
    DWORD        rgbColors;
    LPARAM       lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCTSTR      lpTemplateName;
    HINSTANCE    hInstance;
    LPTSTR       lpszStyle;
    WORD         nFontType;
    WORD         ___MISSING_ALIGNMENT__;
    INT          nSizeMin;
    INT          nSizeMax;
} CHOOSEFONT


/* FINDREPLACE */

typedef struct tagFR { ;
    DWORD        lStructSize;
    HWND         hwndOwner;
    HINSTANCE    hInstance;
    DWORD        Flags;
    LPTSTR        lpstrFindWhat;
    LPTSTR        lpstrReplaceWith;
    WORD         wFindWhatLen;
    WORD         wReplaceWithLen;
    LPARAM        lCustData;
    LPFRHOOKPROC lpfnHook;
    LPCTSTR       lpTemplateName;
} FINDREPLACE

/* PAGESETUPDLG */

typedef struct tagPSD { ;
    DWORD           lStructSize;
    HWND            hwndOwner;
    HGLOBAL         hDevMode;
    HGLOBAL         hDevNames;
    DWORD           Flags;
    POINT           ptPaperSize;
    RECT            rtMinMargin;
    RECT            rtMargin;
    HINSTANCE       hInstance;
    LPARAM          lCustData;
    LPPAGESETUPHOOK lpfnPageSetupHook;
    LPPAGEPAINTHOOK lpfnPagePaintHook;
    LPCTSTR         lpPageSetupTemplateName;
    HGLOBAL         hPageSetupTemplate;
} PAGESETUPDLG, * LPPAGESETUPDLG

/* OPENFILENAME */

typedef struct tagOFN {;
    DWORD         lStructSize;
    HWND          hwndOwner;
    HINSTANCE     hInstance;
    LPCTSTR       lpstrFilter;
    LPTSTR        lpstrCustomFilter;
    DWORD         nMaxCustFilter;
    DWORD         nFilterIndex;
    LPTSTR        lpstrFile;
    DWORD         nMaxFile;
    LPTSTR        lpstrFileTitle;
    DWORD         nMaxFileTitle;
    LPCTSTR       lpstrInitialDir;
    LPCTSTR       lpstrTitle;
    DWORD         Flags;
    WORD          nFileOffset;
    WORD          nFileExtension;
    LPCTSTR       lpstrDefExt;
    DWORD         lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCTSTR       lpTemplateName;
} OPENFILENAME

/* BROWSEINFO */

/*
typedef struct _browseinfo {;
    HWND hwndOwner;
    LPCITEMIDLIST pidlRoot;
    LPSTR pszDisplayName;
    LPCSTR lpszTitle;
    UINT ulFlags;
    BFFCALLBACK lpfn;
    LPARAM lParam;
    int iImage;
} BROWSEINFO, *PBROWSEINFO, *LPBROWSEINFO
*/


/*
typedef struct tagMOUSEINPUT {;
    LONG    dx;
    LONG    dy;
    DWORD   mouseData;
    DWORD   dwFlags;
    DWORD   time;
    DWORD   dwExtraInfo;
} MOUSEINPUT, *PMOUSEINPUT, FAR* LPMOUSEINPUT


typedef struct tagKEYBDINPUT {;
    WORD    wVk;
    WORD    wScan;
    DWORD   dwFlags;
    DWORD   time;
    DWORD   dwExtraInfo;
} KEYBDINPUT, *PKEYBDINPUT, FAR* LPKEYBDINPUT


typedef struct tagHARDWAREINPUT {;
    DWORD   uMsg;
    WORD    wParamL;
    WORD    wParamH;
    DWORD   dwExtraInfo;
} HARDWAREINPUT, *PHARDWAREINPUT, FAR* LPHARDWAREINPUT


typedef struct tagINPUT { ;
    DWORD   type;
    union
    {
        MOUSEINPUT      mi;
        KEYBDINPUT      ki;
        HARDWAREINPUT   hi;
    };
} INPUT, *PINPUT, FAR* LPINPUT
*/



/* PAINTSTRUCT */

typedef struct tagPAINTSTRUCT {;
    HDC  hdc;
    BOOL fErase;
    RECT rcPaint;
    BOOL fRestore;
    BOOL fIncUpdate;
    BYTE rgbReserved[32];
} PAINTSTRUCT


/* drawitem struct */

typedef struct tagDRAWITEMSTRUCT {;
    UINT  CtlType;
    UINT  CtlID;
    UINT  itemID;
    UINT  itemAction;
    UINT  itemState;
    HWND  hwndItem;
    HDC   hDC;
    RECT  rcItem;
    DWORD itemData;
} DRAWITEMSTRUCT

/* memory status */

typedef struct _MEMORYSTATUS {;
    DWORD dwLength;
    DWORD dwMemoryLoad;
    DWORD dwTotalPhys;
    DWORD dwAvailPhys;
    DWORD dwTotalPageFile;
    DWORD dwAvailPageFile;
    DWORD dwTotalVirtual;
    DWORD dwAvailVirtual;
} MEMORYSTATUS, *LPMEMORYSTATUS


/* TRACKMOUSEEVENT */

typedef struct tagTRACKMOUSEEVENT { ;
    DWORD cbSize;
    DWORD dwFlags;
    HWND  hwndTrack;
    DWORD dwHoverTime;
} TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT


/* MSG */

typedef struct tagMSG { ;
    HWND   hwnd;
    UINT   message;
    WPARAM wParam;
    LPARAM lParam;
    DWORD  time;
    POINT  pt;
} MSG

 /* scrollinfo */

typedef struct tagSCROLLINFO { ;
    UINT cbSize;
    UINT fMask;
    int  nMin;
    int  nMax;
    UINT nPage;
    int  nPos;
    int  nTrackPos;
}   SCROLLINFO;

/* APPBARDATA */

typedef struct _AppBarData {;
    DWORD  cbSize;
    HWND   hWnd;
    UINT   uCallbackMessage;
    UINT   uEdge;
    RECT   rc;
    LPARAM lParam;
} APPBARDATA, *PAPPBARDATA

/*shellexecuteinfo */

typedef struct _SHELLEXECUTEINFO{;
    DWORD cbSize;
    ULONG fMask;
    HWND hwnd;
    LPCTSTR lpVerb;
    LPCTSTR lpFile;
    LPCTSTR lpParameters;
    LPCTSTR lpDirectory;
    int nShow;
    HINSTANCE hInstApp;
    LPVOID lpIDList;
    LPCSTR lpClass;
    HKEY hkeyClass;
    DWORD dwHotKey;
    HANDLE hIcon;
    HANDLE hProcess;
} SHELLEXECUTEINFO, FAR *LPSHELLEXECUTEINFO


/* SHFILEINFO */
/*
typedef struct _SHFILEINFO{ ;
    HICON hIcon;
    int   iIcon;
    DWORD dwAttributes;
    char  szDisplayName[MAX_PATH];
    char  szTypeName[80];
} SHFILEINFO
*/

/* notifyicondata */

typedef struct _NOTIFYICONDATA { ;
    DWORD cbSize;
    HWND hWnd;
    UINT uID;
    UINT uFlags;
    UINT uCallbackMessage;
    HICON hIcon;
    char szTip[64];
} NOTIFYICONDATA, *PNOTIFYICONDATA

/* SYSTEMTIME */

typedef struct _SYSTEMTIME {;
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
} SYSTEMTIME


/* textmetric */

typedef struct tagTEXTMETRIC { ;
    LONG tmHeight;
    LONG tmAscent;
    LONG tmDescent;
    LONG tmInternalLeading;
    LONG tmExternalLeading;
    LONG tmAveCharWidth;
    LONG tmMaxCharWidth;
    LONG tmWeight;
    LONG tmOverhang;
    LONG tmDigitizedAspectX;
    LONG tmDigitizedAspectY;
    BCHAR tmFirstChar;
    BCHAR tmLastChar;
    BCHAR tmDefaultChar;
    BCHAR tmBreakChar;
    BYTE tmItalic;
    BYTE tmUnderlined;
    BYTE tmStruckOut;
    BYTE tmPitchAndFamily;
    BYTE tmCharSet;
} TEXTMETRIC


/* MDI: CLIENTCREATESTRUCT */

typedef struct tagCLIENTCREATESTRUCT {;
    HANDLE hWindowMenu;
    UINT   idFirstChild;
} CLIENTCREATESTRUCT


/* DRAWPATRECT */

typedef struct _DRAWPATRECT {;
        POINT ptPosition;
        POINT ptSize;
        WORD  wStyle;
        WORD  wPattern;
} DRAWPATRECT, *PDRAWPATRECT




