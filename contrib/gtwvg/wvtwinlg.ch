/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

/* Legacy constants. Use hbwin.ch instead. */

#ifdef HB_LEGACY_LEVEL5
/* Deprecated. Use WIN_* equivalent instead. */
#xtranslate MAKELONG( <nLow>, <nHigh> )   => WIN_MAKELONG( <nLow>, <nHigh> )
#xtranslate MAKELPARAM( <nLow>, <nHigh> ) => WIN_MAKELPARAM( <nLow>, <nHigh> )
#ifndef RGB
#define RGB( nR, nG, nB )  WIN_RGB( nR, nG, nB )
#endif
#endif

// Menu Manipulation Constants
#define MF_INSERT                                 WIN_MF_INSERT
#define MF_CHANGE                                 WIN_MF_CHANGE
#define MF_APPEND                                 WIN_MF_APPEND
#define MF_DELETE                                 WIN_MF_DELETE
#define MF_REMOVE                                 WIN_MF_REMOVE

#define MF_BYCOMMAND                              WIN_MF_BYCOMMAND
#define MF_BYPOSITION                             WIN_MF_BYPOSITION

#define MF_SEPARATOR                              WIN_MF_SEPARATOR

#define MF_ENABLED                                WIN_MF_ENABLED
#define MF_GRAYED                                 WIN_MF_GRAYED
#define MF_DISABLED                               WIN_MF_DISABLED

#define MF_UNCHECKED                              WIN_MF_UNCHECKED
#define MF_CHECKED                                WIN_MF_CHECKED
#define MF_USECHECKBITMAPS                        WIN_MF_USECHECKBITMAPS

#define MF_STRING                                 WIN_MF_STRING
#define MF_BITMAP                                 WIN_MF_BITMAP
#define MF_OWNERDRAW                              WIN_MF_OWNERDRAW

#define MF_POPUP                                  WIN_MF_POPUP
#define MF_MENUBARBREAK                           WIN_MF_MENUBARBREAK
#define MF_MENUBREAK                              WIN_MF_MENUBREAK

#define MF_UNHILITE                               WIN_MF_UNHILITE
#define MF_HILITE                                 WIN_MF_HILITE

/* - */

#define TPM_LEFTBUTTON                            WIN_TPM_LEFTBUTTON
#define TPM_RIGHTBUTTON                           WIN_TPM_RIGHTBUTTON

#define TPM_LEFTALIGN                             WIN_TPM_LEFTALIGN
#define TPM_CENTERALIGN                           WIN_TPM_CENTERALIGN
#define TPM_RIGHTALIGN                            WIN_TPM_RIGHTALIGN

#define TPM_TOPALIGN                              WIN_TPM_TOPALIGN
#define TPM_VCENTERALIGN                          WIN_TPM_VCENTERALIGN
#define TPM_BOTTOMALIGN                           WIN_TPM_BOTTOMALIGN

#define TPM_HORIZONTAL                            WIN_TPM_HORIZONTAL   /* Horz alignment matters more */
#define TPM_VERTICAL                              WIN_TPM_VERTICAL     /* Vert alignment matters more */
#define TPM_NONOTIFY                              WIN_TPM_NONOTIFY     /* Don't send any notification msgs */
#define TPM_RETURNCMD                             WIN_TPM_RETURNCMD

/* ShowWindow() Commands */
#define SW_HIDE                                   WIN_SW_HIDE
#define SW_SHOWNORMAL                             WIN_SW_SHOWNORMAL
#define SW_NORMAL                                 WIN_SW_NORMAL
#define SW_SHOWMINIMIZED                          WIN_SW_SHOWMINIMIZED
#define SW_SHOWMAXIMIZED                          WIN_SW_SHOWMAXIMIZED
#define SW_MAXIMIZE                               WIN_SW_MAXIMIZE
#define SW_SHOWNOACTIVATE                         WIN_SW_SHOWNOACTIVATE
#define SW_SHOW                                   WIN_SW_SHOW
#define SW_MINIMIZE                               WIN_SW_MINIMIZE
#define SW_SHOWMINNOACTIVE                        WIN_SW_SHOWMINNOACTIVE
#define SW_SHOWNA                                 WIN_SW_SHOWNA
#define SW_RESTORE                                WIN_SW_RESTORE
#define SW_SHOWDEFAULT                            WIN_SW_SHOWDEFAULT
#define SW_FORCEMINIMIZE                          WIN_SW_FORCEMINIMIZE
#define SW_MAX                                    WIN_SW_MAX

/* Window Styles */
#define WS_OVERLAPPED                             WIN_WS_OVERLAPPED
#define WS_TABSTOP                                WIN_WS_TABSTOP
#define WS_MAXIMIZEBOX                            WIN_WS_MAXIMIZEBOX
#define WS_MINIMIZEBOX                            WIN_WS_MINIMIZEBOX
#define WS_GROUP                                  WIN_WS_GROUP
#define WS_THICKFRAME                             WIN_WS_THICKFRAME
#define WS_SYSMENU                                WIN_WS_SYSMENU
#define WS_HSCROLL                                WIN_WS_HSCROLL
#define WS_VSCROLL                                WIN_WS_VSCROLL
#define WS_DLGFRAME                               WIN_WS_DLGFRAME
#define WS_BORDER                                 WIN_WS_BORDER
#define WS_MAXIMIZE                               WIN_WS_MAXIMIZE
#define WS_CLIPCHILDREN                           WIN_WS_CLIPCHILDREN
#define WS_CLIPSIBLINGS                           WIN_WS_CLIPSIBLINGS
#define WS_DISABLED                               WIN_WS_DISABLED
#define WS_VISIBLE                                WIN_WS_VISIBLE
#define WS_MINIMIZE                               WIN_WS_MINIMIZE
#define WS_CHILD                                  WIN_WS_CHILD
#define WS_POPUP                                  WIN_WS_POPUP
#define WS_TILED                                  WIN_WS_TILED
#define WS_ICONIC                                 WIN_WS_ICONIC
#define WS_SIZEBOX                                WIN_WS_SIZEBOX
#define WS_TILEDWINDOW                            WIN_WS_TILEDWINDOW
#define WS_CAPTION                                WIN_WS_CAPTION
#define WS_OVERLAPPEDWINDOW                       WIN_WS_OVERLAPPEDWINDOW

/* window styles (extended) */
#define WS_EX_DLGMODALFRAME                       WIN_WS_EX_DLGMODALFRAME
#define WS_EX_NOPARENTNOTIFY                      WIN_WS_EX_NOPARENTNOTIFY
#define WS_EX_TOPMOST                             WIN_WS_EX_TOPMOST
#define WS_EX_ACCEPTFILES                         WIN_WS_EX_ACCEPTFILES
#define WS_EX_TRANSPARENT                         WIN_WS_EX_TRANSPARENT
#define WS_EX_MDICHILD                            WIN_WS_EX_MDICHILD
#define WS_EX_TOOLWINDOW                          WIN_WS_EX_TOOLWINDOW
#define WS_EX_WINDOWEDGE                          WIN_WS_EX_WINDOWEDGE
#define WS_EX_CLIENTEDGE                          WIN_WS_EX_CLIENTEDGE
#define WS_EX_CONTEXTHELP                         WIN_WS_EX_CONTEXTHELP

#define WS_EX_RIGHT                               WIN_WS_EX_RIGHT
#define WS_EX_LEFT                                WIN_WS_EX_LEFT
#define WS_EX_RTLREADING                          WIN_WS_EX_RTLREADING
#define WS_EX_LTRREADING                          WIN_WS_EX_LTRREADING
#define WS_EX_LEFTSCROLLBAR                       WIN_WS_EX_LEFTSCROLLBAR
#define WS_EX_RIGHTSCROLLBAR                      WIN_WS_EX_RIGHTSCROLLBAR

#define WS_EX_CONTROLPARENT                       WIN_WS_EX_CONTROLPARENT
#define WS_EX_STATICEDGE                          WIN_WS_EX_STATICEDGE
#define WS_EX_APPWINDOW                           WIN_WS_EX_APPWINDOW

#define WS_EX_OVERLAPPEDWINDOW                    WIN_WS_EX_OVERLAPPEDWINDOW
#define WS_EX_PALETTEWINDOW                       WIN_WS_EX_PALETTEWINDOW

#define WS_EX_LAYERED                             WIN_WS_EX_LAYERED
#define WS_EX_NOINHERITLAYOUT                     WIN_WS_EX_NOINHERITLAYOUT
#define WS_EX_LAYOUTRTL                           WIN_WS_EX_LAYOUTRTL
#define WS_EX_NOACTIVATE                          WIN_WS_EX_NOACTIVATE

/* - */

#define SC_SIZE                                   WIN_SC_SIZE
#define SC_MOVE                                   WIN_SC_MOVE
#define SC_MINIMIZE                               WIN_SC_MINIMIZE
#define SC_MAXIMIZE                               WIN_SC_MAXIMIZE
#define SC_NEXTWINDOW                             WIN_SC_NEXTWINDOW
#define SC_PREVWINDOW                             WIN_SC_PREVWINDOW
#define SC_CLOSE                                  WIN_SC_CLOSE
#define SC_VSCROLL                                WIN_SC_VSCROLL
#define SC_HSCROLL                                WIN_SC_HSCROLL
#define SC_MOUSEMENU                              WIN_SC_MOUSEMENU
#define SC_KEYMENU                                WIN_SC_KEYMENU
#define SC_ARRANGE                                WIN_SC_ARRANGE
#define SC_RESTORE                                WIN_SC_RESTORE
#define SC_TASKLIST                               WIN_SC_TASKLIST
#define SC_SCREENSAVE                             WIN_SC_SCREENSAVE
#define SC_HOTKEY                                 WIN_SC_HOTKEY
#define SC_DEFAULT                                WIN_SC_DEFAULT
#define SC_MONITORPOWER                           WIN_SC_MONITORPOWER
#define SC_CONTEXTHELP                            WIN_SC_CONTEXTHELP
#define SC_SEPARATOR                              WIN_SC_SEPARATOR

/* Dialog Box Command IDs */
#define IDOK                                      WIN_IDOK
#define IDCANCEL                                  WIN_IDCANCEL
#define IDABORT                                   WIN_IDABORT
#define IDRETRY                                   WIN_IDRETRY
#define IDIGNORE                                  WIN_IDIGNORE
#define IDYES                                     WIN_IDYES
#define IDNO                                      WIN_IDNO
#define IDTRYAGAIN                                WIN_IDTRYAGAIN
#define IDCONTINUE                                WIN_IDCONTINUE

/* MessageBox() Flags */
#define MB_OK                                     WIN_MB_OK
#define MB_OKCANCEL                               WIN_MB_OKCANCEL
#define MB_ABORTRETRYIGNORE                       WIN_MB_ABORTRETRYIGNORE
#define MB_YESNOCANCEL                            WIN_MB_YESNOCANCEL
#define MB_YESNO                                  WIN_MB_YESNO
#define MB_RETRYCANCEL                            WIN_MB_RETRYCANCEL
#define MB_CANCELTRYCONTINUE                      WIN_MB_CANCELTRYCONTINUE

#define MB_ICONHAND                               WIN_MB_ICONHAND
#define MB_ICONQUESTION                           WIN_MB_ICONQUESTION
#define MB_ICONEXCLAMATION                        WIN_MB_ICONEXCLAMATION
#define MB_ICONASTERISK                           WIN_MB_ICONASTERISK
#define MB_USERICON                               WIN_MB_USERICON
#define MB_ICONWARNING                            WIN_MB_ICONWARNING
#define MB_ICONERROR                              WIN_MB_ICONERROR
#define MB_ICONINFORMATION                        WIN_MB_ICONINFORMATION
#define MB_ICONSTOP                               WIN_MB_ICONSTOP

#define MB_DEFBUTTON1                             WIN_MB_DEFBUTTON1
#define MB_DEFBUTTON2                             WIN_MB_DEFBUTTON2
#define MB_DEFBUTTON3                             WIN_MB_DEFBUTTON3
#define MB_DEFBUTTON4                             WIN_MB_DEFBUTTON4

#define MB_APPLMODAL                              WIN_MB_APPLMODAL
#define MB_SYSTEMMODAL                            WIN_MB_SYSTEMMODAL
#define MB_TASKMODAL                              WIN_MB_TASKMODAL
#define MB_HELP                                   WIN_MB_HELP

#define MB_NOFOCUS                                WIN_MB_NOFOCUS
#define MB_SETFOREGROUND                          WIN_MB_SETFOREGROUND
#define MB_DEFAULT_DESKTOP_ONLY                   WIN_MB_DEFAULT_DESKTOP_ONLY

#define MB_TOPMOST                                WIN_MB_TOPMOST
#define MB_RIGHT                                  WIN_MB_RIGHT
#define MB_RTLREADING                             WIN_MB_RTLREADING

#define MB_TYPEMASK                               WIN_MB_TYPEMASK
#define MB_ICONMASK                               WIN_MB_ICONMASK
#define MB_DEFMASK                                WIN_MB_DEFMASK
#define MB_MODEMASK                               WIN_MB_MODEMASK
#define MB_MISCMASK                               WIN_MB_MISCMASK

/* - */

#define IMAGE_BITMAP                              WIN_IMAGE_BITMAP
#define IMAGE_ICON                                WIN_IMAGE_ICON
#define IMAGE_CURSOR                              WIN_IMAGE_CURSOR
#define IMAGE_ENHMETAFILE                         WIN_IMAGE_ENHMETAFILE

/* DrawText() Format Flags */
#define DT_TOP                                    WIN_DT_TOP
#define DT_LEFT                                   WIN_DT_LEFT
#define DT_CENTER                                 WIN_DT_CENTER
#define DT_RIGHT                                  WIN_DT_RIGHT
#define DT_VCENTER                                WIN_DT_VCENTER
#define DT_BOTTOM                                 WIN_DT_BOTTOM
#define DT_WORDBREAK                              WIN_DT_WORDBREAK
#define DT_SINGLELINE                             WIN_DT_SINGLELINE
#define DT_EXPANDTABS                             WIN_DT_EXPANDTABS
#define DT_TABSTOP                                WIN_DT_TABSTOP
#define DT_NOCLIP                                 WIN_DT_NOCLIP
#define DT_EXTERNALLEADING                        WIN_DT_EXTERNALLEADING
#define DT_CALCRECT                               WIN_DT_CALCRECT
#define DT_NOPREFIX                               WIN_DT_NOPREFIX
#define DT_INTERNAL                               WIN_DT_INTERNAL
#define DT_EDITCONTROL                            WIN_DT_EDITCONTROL
#define DT_PATH_ELLIPSIS                          WIN_DT_PATH_ELLIPSIS
#define DT_END_ELLIPSIS                           WIN_DT_END_ELLIPSIS
#define DT_MODIFYSTRING                           WIN_DT_MODIFYSTRING
#define DT_RTLREADING                             WIN_DT_RTLREADING
#define DT_WORD_ELLIPSIS                          WIN_DT_WORD_ELLIPSIS
#define DT_NOFULLWIDTHCHARBREAK                   WIN_DT_NOFULLWIDTHCHARBREAK
#define DT_HIDEPREFIX                             WIN_DT_HIDEPREFIX
#define DT_PREFIXONLY                             WIN_DT_PREFIXONLY

/* Brush Styles */
#define BS_SOLID                                  WIN_BS_SOLID
#define BS_NULL                                   WIN_BS_NULL
#define BS_HOLLOW                                 WIN_BS_HOLLOW
#define BS_HATCHED                                WIN_BS_HATCHED
#define BS_PATTERN                                WIN_BS_PATTERN
#define BS_INDEXED                                WIN_BS_INDEXED
#define BS_DIBPATTERN                             WIN_BS_DIBPATTERN
#define BS_DIBPATTERNPT                           WIN_BS_DIBPATTERNPT
#define BS_PATTERN8X8                             WIN_BS_PATTERN8X8
#define BS_DIBPATTERN8X8                          WIN_BS_DIBPATTERN8X8
#define BS_MONOPATTERN                            WIN_BS_MONOPATTERN

// Hatch Styles
#define HS_HORIZONTAL                             WIN_HS_HORIZONTAL
#define HS_VERTICAL                               WIN_HS_VERTICAL
#define HS_FDIAGONAL                              WIN_HS_FDIAGONAL
#define HS_BDIAGONAL                              WIN_HS_BDIAGONAL
#define HS_CROSS                                  WIN_HS_CROSS
#define HS_DIAGCROSS                              WIN_HS_DIAGCROSS

// Pen Styles
#define PS_SOLID                                  WIN_PS_SOLID
#define PS_DASH                                   WIN_PS_DASH
#define PS_DOT                                    WIN_PS_DOT
#define PS_DASHDOT                                WIN_PS_DASHDOT
#define PS_DASHDOTDOT                             WIN_PS_DASHDOTDOT
#define PS_NULL                                   WIN_PS_NULL
#define PS_INSIDEFRAME                            WIN_PS_INSIDEFRAME
#define PS_USERSTYLE                              WIN_PS_USERSTYLE
#define PS_ALTERNATE                              WIN_PS_ALTERNATE
#define PS_STYLE_MASK                             WIN_PS_STYLE_MASK

// font weight values
#define FW_DONTCARE                               WIN_FW_DONTCARE
#define FW_THIN                                   WIN_FW_THIN
#define FW_EXTRALIGHT                             WIN_FW_EXTRALIGHT
#define FW_ULTRALIGHT                             WIN_FW_ULTRALIGHT
#define FW_LIGHT                                  WIN_FW_LIGHT
#define FW_NORMAL                                 WIN_FW_NORMAL
#define FW_REGULAR                                WIN_FW_REGULAR
#define FW_MEDIUM                                 WIN_FW_MEDIUM
#define FW_SEMIBOLD                               WIN_FW_SEMIBOLD
#define FW_DEMIBOLD                               WIN_FW_DEMIBOLD
#define FW_BOLD                                   WIN_FW_BOLD
#define FW_EXTRABOLD                              WIN_FW_EXTRABOLD
#define FW_ULTRABOLD                              WIN_FW_ULTRABOLD
#define FW_HEAVY                                  WIN_FW_HEAVY
#define FW_BLACK                                  WIN_FW_BLACK

// font quality values
#define DEFAULT_QUALITY                           WIN_DEFAULT_QUALITY
#define DRAFT_QUALITY                             WIN_DRAFT_QUALITY
#define PROOF_QUALITY                             WIN_PROOF_QUALITY
#define NONANTIALIASED_QUALITY                    WIN_NONANTIALIASED_QUALITY
#define ANTIALIASED_QUALITY                       WIN_ANTIALIASED_QUALITY

#define ANSI_CHARSET                              WIN_ANSI_CHARSET
#define DEFAULT_CHARSET                           WIN_DEFAULT_CHARSET
#define SYMBOL_CHARSET                            WIN_SYMBOL_CHARSET
#define MAC_CHARSET                               WIN_MAC_CHARSET
#define SHIFTJIS_CHARSET                          WIN_SHIFTJIS_CHARSET
#define HANGEUL_CHARSET                           WIN_HANGEUL_CHARSET
#define HANGUL_CHARSET                            WIN_HANGUL_CHARSET
#define JOHAB_CHARSET                             WIN_JOHAB_CHARSET
#define GB2312_CHARSET                            WIN_GB2312_CHARSET
#define CHINESEBIG5_CHARSET                       WIN_CHINESEBIG5_CHARSET
#define GREEK_CHARSET                             WIN_GREEK_CHARSET
#define TURKISH_CHARSET                           WIN_TURKISH_CHARSET
#define VIETNAMESE_CHARSET                        WIN_VIETNAMESE_CHARSET
#define HEBREW_CHARSET                            WIN_HEBREW_CHARSET
#define ARABIC_CHARSET                            WIN_ARABIC_CHARSET
#define BALTIC_CHARSET                            WIN_BALTIC_CHARSET
#define RUSSIAN_CHARSET                           WIN_RUSSIAN_CHARSET
#define THAI_CHARSET                              WIN_THAI_CHARSET
#define EASTEUROPE_CHARSET                        WIN_EASTEUROPE_CHARSET
#define OEM_CHARSET                               WIN_OEM_CHARSET

/* Window field offsets for GetWindowLong() */
#define GWL_WNDPROC                               WIN_GWLP_WNDPROC
#define GWL_HINSTANCE                             WIN_GWLP_HINSTANCE
#define GWL_HWNDPARENT                            WIN_GWLP_HWNDPARENT
#define GWL_ID                                    WIN_GWL_ID
#define GWL_STYLE                                 WIN_GWL_STYLE
#define GWL_EXSTYLE                               WIN_GWL_EXSTYLE
#define GWL_USERDATA                              WIN_GWLP_USERDATA
#define DWL_MSGRESULT                             WIN_DWLP_MSGRESULT
#define DWL_DLGPROC                               WIN_DWLP_DLGPROC
#define DWL_USER                                  WIN_DWLP_USER

/* Virtual Key Codes */
#define VK_LBUTTON                                WIN_VK_LBUTTON
#define VK_RBUTTON                                WIN_VK_RBUTTON
#define VK_CANCEL                                 WIN_VK_CANCEL
#define VK_MBUTTON                                WIN_VK_MBUTTON
#define VK_BACK                                   WIN_VK_BACK
#define VK_TAB                                    WIN_VK_TAB
#define VK_CLEAR                                  WIN_VK_CLEAR
#define VK_RETURN                                 WIN_VK_RETURN
#define VK_SHIFT                                  WIN_VK_SHIFT
#define VK_CONTROL                                WIN_VK_CONTROL
#define VK_MENU                                   WIN_VK_MENU
#define VK_PAUSE                                  WIN_VK_PAUSE
#define VK_CAPITAL                                WIN_VK_CAPITAL
#define VK_ESCAPE                                 WIN_VK_ESCAPE
#define VK_SPACE                                  WIN_VK_SPACE
#define VK_PRIOR                                  WIN_VK_PRIOR
#define VK_NEXT                                   WIN_VK_NEXT
#define VK_END                                    WIN_VK_END
#define VK_HOME                                   WIN_VK_HOME
#define VK_LEFT                                   WIN_VK_LEFT
#define VK_UP                                     WIN_VK_UP
#define VK_RIGHT                                  WIN_VK_RIGHT
#define VK_DOWN                                   WIN_VK_DOWN
#define VK_SELECT                                 WIN_VK_SELECT
#define VK_PRINT                                  WIN_VK_PRINT
#define VK_EXECUTE                                WIN_VK_EXECUTE
#define VK_SNAPSHOT                               WIN_VK_SNAPSHOT
#define VK_INSERT                                 WIN_VK_INSERT
#define VK_DELETE                                 WIN_VK_DELETE
#define VK_HELP                                   WIN_VK_HELP
#define VK_NUMPAD0                                WIN_VK_NUMPAD0
#define VK_NUMPAD1                                WIN_VK_NUMPAD1
#define VK_NUMPAD2                                WIN_VK_NUMPAD2
#define VK_NUMPAD3                                WIN_VK_NUMPAD3
#define VK_NUMPAD4                                WIN_VK_NUMPAD4
#define VK_NUMPAD5                                WIN_VK_NUMPAD5
#define VK_NUMPAD6                                WIN_VK_NUMPAD6
#define VK_NUMPAD7                                WIN_VK_NUMPAD7
#define VK_NUMPAD8                                WIN_VK_NUMPAD8
#define VK_NUMPAD9                                WIN_VK_NUMPAD9
#define VK_MULTIPLY                               WIN_VK_MULTIPLY
#define VK_ADD                                    WIN_VK_ADD
#define VK_SEPARATOR                              WIN_VK_SEPARATOR
#define VK_SUBTRACT                               WIN_VK_SUBTRACT
#define VK_DECIMAL                                WIN_VK_DECIMAL
#define VK_DIVIDE                                 WIN_VK_DIVIDE
#define VK_F1                                     WIN_VK_F1
#define VK_F2                                     WIN_VK_F2
#define VK_F3                                     WIN_VK_F3
#define VK_F4                                     WIN_VK_F4
#define VK_F5                                     WIN_VK_F5
#define VK_F6                                     WIN_VK_F6
#define VK_F7                                     WIN_VK_F7
#define VK_F8                                     WIN_VK_F8
#define VK_F9                                     WIN_VK_F9
#define VK_F10                                    WIN_VK_F10
#define VK_F11                                    WIN_VK_F11
#define VK_F12                                    WIN_VK_F12
#define VK_F13                                    WIN_VK_F13
#define VK_F14                                    WIN_VK_F14
#define VK_F15                                    WIN_VK_F15
#define VK_F16                                    WIN_VK_F16
#define VK_F17                                    WIN_VK_F17
#define VK_F18                                    WIN_VK_F18
#define VK_F19                                    WIN_VK_F19
#define VK_F20                                    WIN_VK_F20
#define VK_F21                                    WIN_VK_F21
#define VK_F22                                    WIN_VK_F22
#define VK_F23                                    WIN_VK_F23
#define VK_F24                                    WIN_VK_F24
#define VK_NUMLOCK                                WIN_VK_NUMLOCK
#define VK_SCROLL                                 WIN_VK_SCROLL

/* File Open/Save Dialog Constants */
#define OFN_READONLY                              WIN_OFN_READONLY
#define OFN_OVERWRITEPROMPT                       WIN_OFN_OVERWRITEPROMPT
#define OFN_HIDEREADONLY                          WIN_OFN_HIDEREADONLY
#define OFN_NOCHANGEDIR                           WIN_OFN_NOCHANGEDIR
#define OFN_SHOWHELP                              WIN_OFN_SHOWHELP
#define OFN_ENABLEHOOK                            WIN_OFN_ENABLEHOOK
#define OFN_ENABLETEMPLATE                        WIN_OFN_ENABLETEMPLATE
#define OFN_ENABLETEMPLATEHANDLE                  WIN_OFN_ENABLETEMPLATEHANDLE
#define OFN_NOVALIDATE                            WIN_OFN_NOVALIDATE
#define OFN_ALLOWMULTISELECT                      WIN_OFN_ALLOWMULTISELECT
#define OFN_EXTENSIONDIFFERENT                    WIN_OFN_EXTENSIONDIFFERENT
#define OFN_PATHMUSTEXIST                         WIN_OFN_PATHMUSTEXIST
#define OFN_FILEMUSTEXIST                         WIN_OFN_FILEMUSTEXIST
#define OFN_CREATEPROMPT                          WIN_OFN_CREATEPROMPT
#define OFN_SHAREAWARE                            WIN_OFN_SHAREAWARE
#define OFN_NOREADONLYRETURN                      WIN_OFN_NOREADONLYRETURN
#define OFN_NOTESTFILECREATE                      WIN_OFN_NOTESTFILECREATE
#define OFN_NONETWORKBUTTON                       WIN_OFN_NONETWORKBUTTON
#define OFN_NOLONGNAMES                           WIN_OFN_NOLONGNAMES
#define OFN_EXPLORER                              WIN_OFN_EXPLORER
#define OFN_NODEREFERENCELINKS                    WIN_OFN_NODEREFERENCELINKS
#define OFN_LONGNAMES                             WIN_OFN_LONGNAMES
#define OFN_ENABLEINCLUDENOTIFY                   WIN_OFN_ENABLEINCLUDENOTIFY
#define OFN_ENABLESIZING                          WIN_OFN_ENABLESIZING
#define OFN_DONTADDTORECENT                       WIN_OFN_DONTADDTORECENT
#define OFN_FORCESHOWHIDDEN                       WIN_OFN_FORCESHOWHIDDEN

#define HWND_TOP                                  WIN_HWND_TOP
#define HWND_BOTTOM                               WIN_HWND_BOTTOM
#define HWND_TOPMOST                              WIN_HWND_TOPMOST
#define HWND_NOTOPMOST                            WIN_HWND_NOTOPMOST

#define SWP_NOSIZE                                WIN_SWP_NOSIZE
#define SWP_NOMOVE                                WIN_SWP_NOMOVE
#define SWP_NOZORDER                              WIN_SWP_NOZORDER
#define SWP_NOREDRAW                              WIN_SWP_NOREDRAW
#define SWP_NOACTIVATE                            WIN_SWP_NOACTIVATE
#define SWP_FRAMECHANGED                          WIN_SWP_FRAMECHANGED     /* The frame changed: send WM_NCCALCSIZE */
#define SWP_SHOWWINDOW                            WIN_SWP_SHOWWINDOW
#define SWP_HIDEWINDOW                            WIN_SWP_HIDEWINDOW
#define SWP_NOCOPYBITS                            WIN_SWP_NOCOPYBITS
#define SWP_NOOWNERZORDER                         WIN_SWP_NOOWNERZORDER    /* Don't do owner Z ordering */
#define SWP_NOSENDCHANGING                        WIN_SWP_NOSENDCHANGING   /* Don't send WM_WINDOWPOSCHANGING */

/* Window Messages */
#define WM_CREATE                                 WIN_WM_CREATE
#define WM_DESTROY                                WIN_WM_DESTROY
#define WM_MOVE                                   WIN_WM_MOVE
#define WM_SIZE                                   WIN_WM_SIZE
#define WM_ACTIVATE                               WIN_WM_ACTIVATE
#define WM_SETFOCUS                               WIN_WM_SETFOCUS
#define WM_KILLFOCUS                              WIN_WM_KILLFOCUS
#define WM_ENABLE                                 WIN_WM_ENABLE
#define WM_SETREDRAW                              WIN_WM_SETREDRAW
#define WM_SETTEXT                                WIN_WM_SETTEXT
#define WM_GETTEXT                                WIN_WM_GETTEXT
#define WM_GETTEXTLENGTH                          WIN_WM_GETTEXTLENGTH
#define WM_PAINT                                  WIN_WM_PAINT
#define WM_CLOSE                                  WIN_WM_CLOSE
#define WM_QUIT                                   WIN_WM_QUIT
#define WM_ERASEBKGND                             WIN_WM_ERASEBKGND
#define WM_SYSCOLORCHANGE                         WIN_WM_SYSCOLORCHANGE
#define WM_SHOWWINDOW                             WIN_WM_SHOWWINDOW
#define WM_WININICHANGE                           WIN_WM_WININICHANGE
#define WM_DEVMODECHANGE                          WIN_WM_DEVMODECHANGE
#define WM_ACTIVATEAPP                            WIN_WM_ACTIVATEAPP
#define WM_FONTCHANGE                             WIN_WM_FONTCHANGE
#define WM_TIMECHANGE                             WIN_WM_TIMECHANGE
#define WM_CANCELMODE                             WIN_WM_CANCELMODE
#define WM_SETCURSOR                              WIN_WM_SETCURSOR
#define WM_MOUSEACTIVATE                          WIN_WM_MOUSEACTIVATE
#define WM_CHILDACTIVATE                          WIN_WM_CHILDACTIVATE
#define WM_QUEUESYNC                              WIN_WM_QUEUESYNC
#define WM_GETMINMAXINFO                          WIN_WM_GETMINMAXINFO

#define WM_PAINTICON                              WIN_WM_PAINTICON
#define WM_ICONERASEBKGND                         WIN_WM_ICONERASEBKGND
#define WM_NEXTDLGCTL                             WIN_WM_NEXTDLGCTL
#define WM_SPOOLERSTATUS                          WIN_WM_SPOOLERSTATUS
#define WM_DRAWITEM                               WIN_WM_DRAWITEM
#define WM_MEASUREITEM                            WIN_WM_MEASUREITEM
#define WM_DELETEITEM                             WIN_WM_DELETEITEM
#define WM_VKEYTOITEM                             WIN_WM_VKEYTOITEM
#define WM_CHARTOITEM                             WIN_WM_CHARTOITEM
#define WM_SETFONT                                WIN_WM_SETFONT
#define WM_GETFONT                                WIN_WM_GETFONT
#define WM_SETHOTKEY                              WIN_WM_SETHOTKEY
#define WM_GETHOTKEY                              WIN_WM_GETHOTKEY
#define WM_QUERYDRAGICON                          WIN_WM_QUERYDRAGICON
#define WM_COMPAREITEM                            WIN_WM_COMPAREITEM
#define WM_GETOBJECT                              WIN_WM_GETOBJECT
#define WM_COMPACTING                             WIN_WM_COMPACTING
#define WM_COMMNOTIFY                             WIN_WM_COMMNOTIFY
#define WM_WINDOWPOSCHANGING                      WIN_WM_WINDOWPOSCHANGING
#define WM_WINDOWPOSCHANGED                       WIN_WM_WINDOWPOSCHANGED
#define WM_POWER                                  WIN_WM_POWER
#define WM_NOTIFY                                 WIN_WM_NOTIFY
#define WM_INPUTLANGCHANGEREQUEST                 WIN_WM_INPUTLANGCHANGEREQUEST
#define WM_INPUTLANGCHANGE                        WIN_WM_INPUTLANGCHANGE
#define WM_TCARD                                  WIN_WM_TCARD
#define WM_HELP                                   WIN_WM_HELP
#define WM_USERCHANGED                            WIN_WM_USERCHANGED
#define WM_NOTIFYFORMAT                           WIN_WM_NOTIFYFORMAT

#define WM_CONTEXTMENU                            WIN_WM_CONTEXTMENU
#define WM_STYLECHANGING                          WIN_WM_STYLECHANGING
#define WM_STYLECHANGED                           WIN_WM_STYLECHANGED
#define WM_DISPLAYCHANGE                          WIN_WM_DISPLAYCHANGE
#define WM_GETICON                                WIN_WM_GETICON
#define WM_SETICON                                WIN_WM_SETICON

#define WM_NCCREATE                               WIN_WM_NCCREATE
#define WM_NCDESTROY                              WIN_WM_NCDESTROY
#define WM_NCCALCSIZE                             WIN_WM_NCCALCSIZE
#define WM_NCHITTEST                              WIN_WM_NCHITTEST
#define WM_NCPAINT                                WIN_WM_NCPAINT
#define WM_NCACTIVATE                             WIN_WM_NCACTIVATE
#define WM_GETDLGCODE                             WIN_WM_GETDLGCODE

#define WM_NCMOUSEMOVE                            WIN_WM_NCMOUSEMOVE
#define WM_NCLBUTTONDOWN                          WIN_WM_NCLBUTTONDOWN
#define WM_NCLBUTTONUP                            WIN_WM_NCLBUTTONUP
#define WM_NCLBUTTONDBLCLK                        WIN_WM_NCLBUTTONDBLCLK
#define WM_NCRBUTTONDOWN                          WIN_WM_NCRBUTTONDOWN
#define WM_NCRBUTTONUP                            WIN_WM_NCRBUTTONUP
#define WM_NCRBUTTONDBLCLK                        WIN_WM_NCRBUTTONDBLCLK
#define WM_NCMBUTTONDOWN                          WIN_WM_NCMBUTTONDOWN
#define WM_NCMBUTTONUP                            WIN_WM_NCMBUTTONUP
#define WM_NCMBUTTONDBLCLK                        WIN_WM_NCMBUTTONDBLCLK

#define WM_KEYDOWN                                WIN_WM_KEYDOWN
#define WM_KEYUP                                  WIN_WM_KEYUP

#define WM_KEYFIRST                               WIN_WM_KEYFIRST
#define WM_CHAR                                   WIN_WM_CHAR
#define WM_DEADCHAR                               WIN_WM_DEADCHAR
#define WM_SYSKEYDOWN                             WIN_WM_SYSKEYDOWN
#define WM_SYSKEYUP                               WIN_WM_SYSKEYUP
#define WM_SYSCHAR                                WIN_WM_SYSCHAR
#define WM_SYSDEADCHAR                            WIN_WM_SYSDEADCHAR
#define WM_KEYLAST                                WIN_WM_KEYLAST

#define WM_INITDIALOG                             WIN_WM_INITDIALOG
#define WM_COMMAND                                WIN_WM_COMMAND
#define WM_SYSCOMMAND                             WIN_WM_SYSCOMMAND
#define WM_TIMER                                  WIN_WM_TIMER
#define WM_HSCROLL                                WIN_WM_HSCROLL
#define WM_VSCROLL                                WIN_WM_VSCROLL

#define WM_INITMENUPOPUP                          WIN_WM_INITMENUPOPUP
#define WM_MENUSELECT                             WIN_WM_MENUSELECT
#define WM_MENUCOMMAND                            WIN_WM_MENUCOMMAND

#define WM_CTLCOLORMSGBOX                         WIN_WM_CTLCOLORMSGBOX
#define WM_CTLCOLOREDIT                           WIN_WM_CTLCOLOREDIT
#define WM_CTLCOLORLISTBOX                        WIN_WM_CTLCOLORLISTBOX
#define WM_CTLCOLORBTN                            WIN_WM_CTLCOLORBTN
#define WM_CTLCOLORDLG                            WIN_WM_CTLCOLORDLG
#define WM_CTLCOLORSCROLLBAR                      WIN_WM_CTLCOLORSCROLLBAR
#define WM_CTLCOLORSTATIC                         WIN_WM_CTLCOLORSTATIC

#define WM_MOUSEFIRST                             WIN_WM_MOUSEFIRST
#define WM_MOUSEMOVE                              WIN_WM_MOUSEMOVE
#define WM_LBUTTONDOWN                            WIN_WM_LBUTTONDOWN
#define WM_LBUTTONUP                              WIN_WM_LBUTTONUP
#define WM_LBUTTONDBLCLK                          WIN_WM_LBUTTONDBLCLK
#define WM_RBUTTONDOWN                            WIN_WM_RBUTTONDOWN
#define WM_RBUTTONUP                              WIN_WM_RBUTTONUP
#define WM_RBUTTONDBLCLK                          WIN_WM_RBUTTONDBLCLK
#define WM_MBUTTONDOWN                            WIN_WM_MBUTTONDOWN
#define WM_MBUTTONUP                              WIN_WM_MBUTTONUP
#define WM_MBUTTONDBLCLK                          WIN_WM_MBUTTONDBLCLK
#define WM_MOUSEWHEEL                             WIN_WM_MOUSEWHEEL
#define WM_XBUTTONDOWN                            WIN_WM_XBUTTONDOWN
#define WM_XBUTTONUP                              WIN_WM_XBUTTONUP
#define WM_XBUTTONDBLCLK                          WIN_WM_XBUTTONDBLCLK
#define WM_MOUSEHOVER                             WIN_WM_MOUSEHOVER
#define WM_MOUSELEAVE                             WIN_WM_MOUSELEAVE

#define WM_PARENTNOTIFY                           WIN_WM_PARENTNOTIFY
#define WM_ENTERMENULOOP                          WIN_WM_ENTERMENULOOP
#define WM_EXITMENULOOP                           WIN_WM_EXITMENULOOP

#define WM_NEXTMENU                               WIN_WM_NEXTMENU
#define WM_SIZING                                 WIN_WM_SIZING
#define WM_CAPTURECHANGED                         WIN_WM_CAPTURECHANGED
#define WM_MOVING                                 WIN_WM_MOVING
#define WM_POWERBROADCAST                         WIN_WM_POWERBROADCAST

#define WM_MDICREATE                              WIN_WM_MDICREATE
#define WM_MDIDESTROY                             WIN_WM_MDIDESTROY
#define WM_MDIACTIVATE                            WIN_WM_MDIACTIVATE
#define WM_MDIRESTORE                             WIN_WM_MDIRESTORE
#define WM_MDINEXT                                WIN_WM_MDINEXT
#define WM_MDIMAXIMIZE                            WIN_WM_MDIMAXIMIZE
#define WM_MDITILE                                WIN_WM_MDITILE
#define WM_MDICASCADE                             WIN_WM_MDICASCADE
#define WM_MDIICONARRANGE                         WIN_WM_MDIICONARRANGE
#define WM_MDIGETACTIVE                           WIN_WM_MDIGETACTIVE

#define WM_MDISETMENU                             WIN_WM_MDISETMENU
#define WM_ENTERSIZEMOVE                          WIN_WM_ENTERSIZEMOVE
#define WM_EXITSIZEMOVE                           WIN_WM_EXITSIZEMOVE
#define WM_DROPFILES                              WIN_WM_DROPFILES
#define WM_MDIREFRESHMENU                         WIN_WM_MDIREFRESHMENU

#define WM_CUT                                    WIN_WM_CUT
#define WM_COPY                                   WIN_WM_COPY
#define WM_PASTE                                  WIN_WM_PASTE
#define WM_CLEAR                                  WIN_WM_CLEAR
#define WM_UNDO                                   WIN_WM_UNDO
#define WM_RENDERFORMAT                           WIN_WM_RENDERFORMAT
#define WM_RENDERALLFORMATS                       WIN_WM_RENDERALLFORMATS
#define WM_DESTROYCLIPBOARD                       WIN_WM_DESTROYCLIPBOARD
#define WM_DRAWCLIPBOARD                          WIN_WM_DRAWCLIPBOARD
#define WM_PAINTCLIPBOARD                         WIN_WM_PAINTCLIPBOARD
#define WM_VSCROLLCLIPBOARD                       WIN_WM_VSCROLLCLIPBOARD
#define WM_SIZECLIPBOARD                          WIN_WM_SIZECLIPBOARD
#define WM_ASKCBFORMATNAME                        WIN_WM_ASKCBFORMATNAME
#define WM_CHANGECBCHAIN                          WIN_WM_CHANGECBCHAIN
#define WM_HSCROLLCLIPBOARD                       WIN_WM_HSCROLLCLIPBOARD
#define WM_QUERYNEWPALETTE                        WIN_WM_QUERYNEWPALETTE
#define WM_PALETTEISCHANGING                      WIN_WM_PALETTEISCHANGING
#define WM_PALETTECHANGED                         WIN_WM_PALETTECHANGED
#define WM_HOTKEY                                 WIN_WM_HOTKEY

#define WM_USER                                   WIN_WM_USER

#define WM_CHOOSEFONT_GETLOGFONT                  WIN_WM_CHOOSEFONT_GETLOGFONT
#define WM_CHOOSEFONT_SETLOGFONT                  WIN_WM_CHOOSEFONT_SETLOGFONT
#define WM_CHOOSEFONT_SETFLAGS                    WIN_WM_CHOOSEFONT_SETFLAGS

/* WIN_WM_SETICON / WIN_WM_GETICON Type Codes */
#define ICON_SMALL                                WIN_ICON_SMALL
#define ICON_BIG                                  WIN_ICON_BIG

/* Stock Logical Objects */
#define WHITE_BRUSH                               WIN_WHITE_BRUSH
#define LTGRAY_BRUSH                              WIN_LTGRAY_BRUSH
#define GRAY_BRUSH                                WIN_GRAY_BRUSH
#define DKGRAY_BRUSH                              WIN_DKGRAY_BRUSH
#define BLACK_BRUSH                               WIN_BLACK_BRUSH
#define NULL_BRUSH                                WIN_NULL_BRUSH
#define HOLLOW_BRUSH                              WIN_HOLLOW_BRUSH
#define WHITE_PEN                                 WIN_WHITE_PEN
#define BLACK_PEN                                 WIN_BLACK_PEN
#define NULL_PEN                                  WIN_NULL_PEN
#define OEM_FIXED_FONT                            WIN_OEM_FIXED_FONT
#define ANSI_FIXED_FONT                           WIN_ANSI_FIXED_FONT
#define ANSI_VAR_FONT                             WIN_ANSI_VAR_FONT
#define SYSTEM_FONT                               WIN_SYSTEM_FONT
#define DEVICE_DEFAULT_FONT                       WIN_DEVICE_DEFAULT_FONT
#define DEFAULT_PALETTE                           WIN_DEFAULT_PALETTE
#define SYSTEM_FIXED_FONT                         WIN_SYSTEM_FIXED_FONT
#define DEFAULT_GUI_FONT                          WIN_DEFAULT_GUI_FONT

// flags (CHOOSECOLOR structure)
#define CC_RGBINIT                                WIN_CC_RGBINIT
#define CC_FULLOPEN                               WIN_CC_FULLOPEN
#define CC_PREVENTFULLOPEN                        WIN_CC_PREVENTFULLOPEN
#define CC_SHOWHELP                               WIN_CC_SHOWHELP
#define CC_ENABLEHOOK                             WIN_CC_ENABLEHOOK
#define CC_ENABLETEMPLATE                         WIN_CC_ENABLETEMPLATE
#define CC_ENABLETEMPLATEHANDLE                   WIN_CC_ENABLETEMPLATEHANDLE
#define CC_SOLIDCOLOR                             WIN_CC_SOLIDCOLOR
#define CC_ANYCOLOR                               WIN_CC_ANYCOLOR

#define DRIVERVERSION                             WIN_DRIVERVERSION
#define TECHNOLOGY                                WIN_TECHNOLOGY
#define HORZSIZE                                  WIN_HORZSIZE
#define VERTSIZE                                  WIN_VERTSIZE
#define HORZRES                                   WIN_HORZRES
#define VERTRES                                   WIN_VERTRES
#define BITSPIXEL                                 WIN_BITSPIXEL
#define PLANES                                    WIN_PLANES
#define NUMBRUSHES                                WIN_NUMBRUSHES
#define NUMPENS                                   WIN_NUMPENS
#define NUMMARKERS                                WIN_NUMMARKERS
#define NUMFONTS                                  WIN_NUMFONTS
#define NUMCOLORS                                 WIN_NUMCOLORS
#define PDEVICESIZE                               WIN_PDEVICESIZE
#define CURVECAPS                                 WIN_CURVECAPS
#define LINECAPS                                  WIN_LINECAPS
#define POLYGONALCAPS                             WIN_POLYGONALCAPS
#define TEXTCAPS                                  WIN_TEXTCAPS
#define CLIPCAPS                                  WIN_CLIPCAPS
#define RASTERCAPS                                WIN_RASTERCAPS
#define ASPECTX                                   WIN_ASPECTX
#define ASPECTY                                   WIN_ASPECTY
#define ASPECTXY                                  WIN_ASPECTXY
#define LOGPIXELSX                                WIN_LOGPIXELSX
#define LOGPIXELSY                                WIN_LOGPIXELSY
#define SIZEPALETTE                               WIN_SIZEPALETTE
#define NUMRESERVED                               WIN_NUMRESERVED
#define COLORRES                                  WIN_COLORRES
#define PHYSICALWIDTH                             WIN_PHYSICALWIDTH
#define PHYSICALHEIGHT                            WIN_PHYSICALHEIGHT
#define PHYSICALOFFSETX                           WIN_PHYSICALOFFSETX
#define PHYSICALOFFSETY                           WIN_PHYSICALOFFSETY
#define SCALINGFACTORX                            WIN_SCALINGFACTORX
#define SCALINGFACTORY                            WIN_SCALINGFACTORY

#define TRANSPARENT                               WIN_TRANSPARENT
#define OPAQUE                                    WIN_OPAQUE

#define TA_NOUPDATECP                             WIN_TA_NOUPDATECP
#define TA_UPDATECP                               WIN_TA_UPDATECP
#define TA_LEFT                                   WIN_TA_LEFT
#define TA_RIGHT                                  WIN_TA_RIGHT
#define TA_CENTER                                 WIN_TA_CENTER
#define TA_TOP                                    WIN_TA_TOP
#define TA_BOTTOM                                 WIN_TA_BOTTOM
#define TA_BASELINE                               WIN_TA_BASELINE
#define TA_RTLREADING                             WIN_TA_RTLREADING
#define TA_MASK                                   WIN_TA_MASK

#define CTLCOLOR_MSGBOX                           WIN_CTLCOLOR_MSGBOX
#define CTLCOLOR_EDIT                             WIN_CTLCOLOR_EDIT
#define CTLCOLOR_LISTBOX                          WIN_CTLCOLOR_LISTBOX
#define CTLCOLOR_BTN                              WIN_CTLCOLOR_BTN
#define CTLCOLOR_DLG                              WIN_CTLCOLOR_DLG
#define CTLCOLOR_SCROLLBAR                        WIN_CTLCOLOR_SCROLLBAR
#define CTLCOLOR_STATIC                           WIN_CTLCOLOR_STATIC
#define CTLCOLOR_MAX                              WIN_CTLCOLOR_MAX
#define COLOR_SCROLLBAR                           WIN_COLOR_SCROLLBAR
#define COLOR_BACKGROUND                          WIN_COLOR_BACKGROUND
#define COLOR_ACTIVECAPTION                       WIN_COLOR_ACTIVECAPTION
#define COLOR_INACTIVECAPTION                     WIN_COLOR_INACTIVECAPTION
#define COLOR_MENU                                WIN_COLOR_MENU
#define COLOR_WINDOW                              WIN_COLOR_WINDOW
#define COLOR_WINDOWFRAME                         WIN_COLOR_WINDOWFRAME
#define COLOR_MENUTEXT                            WIN_COLOR_MENUTEXT
#define COLOR_WINDOWTEXT                          WIN_COLOR_WINDOWTEXT
#define COLOR_CAPTIONTEXT                         WIN_COLOR_CAPTIONTEXT
#define COLOR_ACTIVEBORDER                        WIN_COLOR_ACTIVEBORDER
#define COLOR_INACTIVEBORDER                      WIN_COLOR_INACTIVEBORDER
#define COLOR_APPWORKSPACE                        WIN_COLOR_APPWORKSPACE
#define COLOR_HIGHLIGHT                           WIN_COLOR_HIGHLIGHT
#define COLOR_HIGHLIGHTTEXT                       WIN_COLOR_HIGHLIGHTTEXT
#define COLOR_BTNFACE                             WIN_COLOR_BTNFACE
#define COLOR_BTNSHADOW                           WIN_COLOR_BTNSHADOW
#define COLOR_GRAYTEXT                            WIN_COLOR_GRAYTEXT
#define COLOR_BTNTEXT                             WIN_COLOR_BTNTEXT
#define COLOR_INACTIVECAPTIONTEXT                 WIN_COLOR_INACTIVECAPTIONTEXT
#define COLOR_BTNHIGHLIGHT                        WIN_COLOR_BTNHIGHLIGHT
#define COLOR_3DDKSHADOW                          WIN_COLOR_3DDKSHADOW
#define COLOR_3DLIGHT                             WIN_COLOR_3DLIGHT
#define COLOR_INFOTEXT                            WIN_COLOR_INFOTEXT
#define COLOR_INFOBK                              WIN_COLOR_INFOBK
#define COLOR_HOTLIGHT                            WIN_COLOR_HOTLIGHT
#define COLOR_GRADIENTACTIVECAPTION               WIN_COLOR_GRADIENTACTIVECAPTION
#define COLOR_GRADIENTINACTIVECAPTION             WIN_COLOR_GRADIENTINACTIVECAPTION
#define COLOR_DESKTOP                             WIN_COLOR_DESKTOP
#define COLOR_3DFACE                              WIN_COLOR_3DFACE
#define COLOR_3DSHADOW                            WIN_COLOR_3DSHADOW
#define COLOR_3DHIGHLIGHT                         WIN_COLOR_3DHIGHLIGHT
#define COLOR_3DHILIGHT                           WIN_COLOR_3DHILIGHT
#define COLOR_BTNHILIGHT                          WIN_COLOR_BTNHILIGHT

#define SND_SYNC                                  WIN_SND_SYNC
#define SND_ASYNC                                 WIN_SND_ASYNC
#define SND_NODEFAULT                             WIN_SND_NODEFAULT
#define SND_MEMORY                                WIN_SND_MEMORY
#define SND_LOOP                                  WIN_SND_LOOP
#define SND_NOSTOP                                WIN_SND_NOSTOP
#define SND_FILENAME                              WIN_SND_FILENAME
#define SND_RESOURCE                              WIN_SND_RESOURCE
#define SND_PURGE                                 WIN_SND_PURGE
#define SND_APPLICATION                           WIN_SND_APPLICATION
#define SND_ALIAS_START                           WIN_SND_ALIAS_START
