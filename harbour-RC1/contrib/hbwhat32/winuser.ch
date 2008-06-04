/*
 * $Id$
 */

* Automatically translated from winuser.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/****************************************************************************
*                                                                           *
* winuser.h -- USER procedure declarations, constant definitions and macros *
*                                                                           *
* Copyright (c) 1985 - 1999, Microsoft Corporation                          *
*                                                                           *
****************************************************************************/


#ifndef _WINUSER_
  //P_O_Push
  #define _WINUSER_


  //
  // Define API decoration for direct importing of DLL references.
  //


  //#define IS_INTRESOURCE(_r) (((ULONG_PTR)(_r) >> 16) == 0)
  //#define MAKEINTRESOURCEA(i) (LPSTR)((ULONG_PTR)((WORD)(i)))
  //#define MAKEINTRESOURCEW(i) (LPWSTR)((ULONG_PTR)((WORD)(i)))
  //#ifdef UNICODE
  //#define MAKEINTRESOURCE  MAKEINTRESOURCEW
  //#else
  //#define MAKEINTRESOURCE  MAKEINTRESOURCEA
  //#endif // !UNICODE

  #ifndef NORESOURCE

    /*
    * Predefined Resource Types
    */
    #define RT_CURSOR       1
    #define RT_BITMAP       2
    #define RT_ICON         3
    #define RT_MENU         4
    #define RT_DIALOG       5
    #define RT_STRING       6
    #define RT_FONTDIR      7
    #define RT_FONT         8
    #define RT_ACCELERATOR  9
    #define RT_RCDATA       10
    #define RT_GROUP_CURSOR 12
    #define RT_GROUP_ICON   14
    #define RT_VERSION      16
    #define RT_DLGINCLUDE   17

    //#define RT_CURSOR           MAKEINTRESOURCE(1)
    //#define RT_BITMAP           MAKEINTRESOURCE(2)
    //#define RT_ICON             MAKEINTRESOURCE(3)
    //#define RT_MENU             MAKEINTRESOURCE(4)
    //#define RT_DIALOG           MAKEINTRESOURCE(5)
    //#define RT_STRING           MAKEINTRESOURCE(6)
    //#define RT_FONTDIR          MAKEINTRESOURCE(7)
    //#define RT_FONT             MAKEINTRESOURCE(8)
    //#define RT_ACCELERATOR      MAKEINTRESOURCE(9)
    //#define RT_RCDATA           MAKEINTRESOURCE(10)

    #define RT_MESSAGETABLE     MAKEINTRESOURCE(11)

    #define DIFFERENCE          11
    //#define RT_GROUP_CURSOR MAKEINTRESOURCE((ULONG_PTR)RT_CURSOR + DIFFERENCE)
    //#define RT_GROUP_ICON   MAKEINTRESOURCE((ULONG_PTR)RT_ICON + DIFFERENCE)
    //#define RT_VERSION      MAKEINTRESOURCE(16)
    //#define RT_DLGINCLUDE   MAKEINTRESOURCE(17)
    //#if (WINVER >=   1024)
    #define RT_PLUGPLAY     MAKEINTRESOURCE(19)
    #define RT_VXD          MAKEINTRESOURCE(20)
    #define RT_ANICURSOR    MAKEINTRESOURCE(21)
    #define RT_ANIICON      MAKEINTRESOURCE(22)
    //#endif // WINVER >= 0x0400
    #define RT_HTML         MAKEINTRESOURCE(23)

  #endif // !NORESOURCE


  //#ifdef UNICODE
  //#define wvsprintf  wvsprintfW
  //#else
  //#define wvsprintf  wvsprintfA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define wsprintf  wsprintfW
  //#else
  //#define wsprintf  wsprintfA
  //#endif // !UNICODE


  /*
  * SPI_SETDESKWALLPAPER defined constants
  */
  #define SETWALLPAPER_DEFAULT    ((LPWSTR)-1)

  #ifndef NOSCROLL

    /*
    * Scroll Bar Constants
    */
    #define SB_HORZ             0
    #define SB_VERT             1
    #define SB_CTL              2
    #define SB_BOTH             3

    /*
    * Scroll Bar Commands
    */
    #define SB_LINEUP           0
    #define SB_LINELEFT         0
    #define SB_LINEDOWN         1
    #define SB_LINERIGHT        1
    #define SB_PAGEUP           2
    #define SB_PAGELEFT         2
    #define SB_PAGEDOWN         3
    #define SB_PAGERIGHT        3
    #define SB_THUMBPOSITION    4
    #define SB_THUMBTRACK       5
    #define SB_TOP              6
    #define SB_LEFT             6
    #define SB_BOTTOM           7
    #define SB_RIGHT            7
    #define SB_ENDSCROLL        8

  #endif // !NOSCROLL

  #ifndef NOSHOWWINDOW

    /*
    * ShowWindow() Commands
    */
    #define SW_HIDE             0
    #define SW_SHOWNORMAL       1
    #define SW_NORMAL           1
    #define SW_SHOWMINIMIZED    2
    #define SW_SHOWMAXIMIZED    3
    #define SW_MAXIMIZE         3
    #define SW_SHOWNOACTIVATE   4
    #define SW_SHOW             5
    #define SW_MINIMIZE         6
    #define SW_SHOWMINNOACTIVE  7
    #define SW_SHOWNA           8
    #define SW_RESTORE          9
    #define SW_SHOWDEFAULT      10
    #define SW_FORCEMINIMIZE    11
    #define SW_MAX              11

    /*
    * Old ShowWindow() Commands
    */
    #define HIDE_WINDOW         0
    #define SHOW_OPENWINDOW     1
    #define SHOW_ICONWINDOW     2
    #define SHOW_FULLSCREEN     3
    #define SHOW_OPENNOACTIVATE 4

    /*
    * Identifiers for the WM_SHOWWINDOW message
    */
    #define SW_PARENTCLOSING    1
    #define SW_OTHERZOOM        2
    #define SW_PARENTOPENING    3
    #define SW_OTHERUNZOOM      4


  #endif // !NOSHOWWINDOW

  //#if(WINVER >=   1280)
  /*
  * AnimateWindow() Commands
  */
  #define AW_HOR_POSITIVE                      1
  #define AW_HOR_NEGATIVE                      2
  #define AW_VER_POSITIVE                      4
  #define AW_VER_NEGATIVE                      8
  #define AW_CENTER                           16
  #define AW_HIDE                          65536
  #define AW_ACTIVATE                     131072
  #define AW_SLIDE                        262144
  #define AW_BLEND                        524288

  //#endif // WINVER >= 0x0500


  /*
  * WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags
  */
  #define KF_EXTENDED          256
  #define KF_DLGMODE          2048
  #define KF_MENUMODE         4096
  #define KF_ALTDOWN          8192
  #define KF_REPEAT          16384
  #define KF_UP              32768

  #ifndef NOVIRTUALKEYCODES

    /*
    * Virtual Keys, Standard Set
    */
    #define VK_LBUTTON           1
    #define VK_RBUTTON           2
    #define VK_CANCEL            3
    #define VK_MBUTTON           4    // NOT contiguous with L & RBUTTON

    //#if(_WIN32_WINNT >=   1280)
    #define VK_XBUTTON1          5    // NOT contiguous with L & RBUTTON
    #define VK_XBUTTON2          6    // NOT contiguous with L & RBUTTON
    //#endif // _WIN32_WINNT >= 0x0500

    /*
    * 0x07 : unassigned
    */

    #define VK_BACK              8
    #define VK_TAB               9

    /*
    * 0x0A - 0x0B : reserved
    */

    #define VK_CLEAR            12
    #define VK_RETURN           13

    #define VK_SHIFT            16
    #define VK_CONTROL          17
    #define VK_MENU             18
    #define VK_PAUSE            19
    #define VK_CAPITAL          20

    #define VK_KANA             21
    #define VK_HANGEUL          21  // old name - should be here for compatibility
    #define VK_HANGUL           21
    #define VK_JUNJA            23
    #define VK_FINAL            24
    #define VK_HANJA            25
    #define VK_KANJI            25

    #define VK_ESCAPE           27

    #define VK_CONVERT          28
    #define VK_NONCONVERT       29
    #define VK_ACCEPT           30
    #define VK_MODECHANGE       31

    #define VK_SPACE            32
    #define VK_PRIOR            33
    #define VK_NEXT             34
    #define VK_END              35
    #define VK_HOME             36
    #define VK_LEFT             37
    #define VK_UP               38
    #define VK_RIGHT            39
    #define VK_DOWN             40
    #define VK_SELECT           41
    #define VK_PRINT            42
    #define VK_EXECUTE          43
    #define VK_SNAPSHOT         44
    #define VK_INSERT           45
    #define VK_DELETE           46
    #define VK_HELP             47

    /*
    * VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
    * 0x40 : unassigned
    * VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
    */

    #define VK_LWIN             91
    #define VK_RWIN             92
    #define VK_APPS             93

    /*
    * 0x5E : reserved
    */

    #define VK_SLEEP            95

    #define VK_NUMPAD0          96
    #define VK_NUMPAD1          97
    #define VK_NUMPAD2          98
    #define VK_NUMPAD3          99
    #define VK_NUMPAD4         100
    #define VK_NUMPAD5         101
    #define VK_NUMPAD6         102
    #define VK_NUMPAD7         103
    #define VK_NUMPAD8         104
    #define VK_NUMPAD9         105
    #define VK_MULTIPLY        106
    #define VK_ADD             107
    #define VK_SEPARATOR       108
    #define VK_SUBTRACT        109
    #define VK_DECIMAL         110
    #define VK_DIVIDE          111
    #define VK_F1              112
    #define VK_F2              113
    #define VK_F3              114
    #define VK_F4              115
    #define VK_F5              116
    #define VK_F6              117
    #define VK_F7              118
    #define VK_F8              119
    #define VK_F9              120
    #define VK_F10             121
    #define VK_F11             122
    #define VK_F12             123
    #define VK_F13             124
    #define VK_F14             125
    #define VK_F15             126
    #define VK_F16             127
    #define VK_F17             128
    #define VK_F18             129
    #define VK_F19             130
    #define VK_F20             131
    #define VK_F21             132
    #define VK_F22             133
    #define VK_F23             134
    #define VK_F24             135

    /*
    * 0x88 - 0x8F : unassigned
    */

    #define VK_NUMLOCK         144
    #define VK_SCROLL          145

    /*
    * NEC PC-9800 kbd definitions
    */
    #define VK_OEM_NEC_EQUAL   146   // '=' key on numpad

    /*
    * Fujitsu/OASYS kbd definitions
    */
    #define VK_OEM_FJ_JISHO    146   // 'Dictionary' key
    #define VK_OEM_FJ_MASSHOU  147   // 'Unregister word' key
    #define VK_OEM_FJ_TOUROKU  148   // 'Register word' key
    #define VK_OEM_FJ_LOYA     149   // 'Left OYAYUBI' key
    #define VK_OEM_FJ_ROYA     150   // 'Right OYAYUBI' key

    /*
    * 0x97 - 0x9F : unassigned
    */

    /*
    * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
    * Used only as parameters to GetAsyncKeyState() and GetKeyState().
    * No other API or message will distinguish left and right keys in this way.
    */
    #define VK_LSHIFT          160
    #define VK_RSHIFT          161
    #define VK_LCONTROL        162
    #define VK_RCONTROL        163
    #define VK_LMENU           164
    #define VK_RMENU           165

    //#if(_WIN32_WINNT >=   1280)
    #define VK_BROWSER_BACK         166
    #define VK_BROWSER_FORWARD      167
    #define VK_BROWSER_REFRESH      168
    #define VK_BROWSER_STOP         169
    #define VK_BROWSER_SEARCH       170
    #define VK_BROWSER_FAVORITES    171
    #define VK_BROWSER_HOME         172

    #define VK_VOLUME_MUTE          173
    #define VK_VOLUME_DOWN          174
    #define VK_VOLUME_UP            175
    #define VK_MEDIA_NEXT_TRACK     176
    #define VK_MEDIA_PREV_TRACK     177
    #define VK_MEDIA_STOP           178
    #define VK_MEDIA_PLAY_PAUSE     179
    #define VK_LAUNCH_MAIL          180
    #define VK_LAUNCH_MEDIA_SELECT  181
    #define VK_LAUNCH_APP1          182
    #define VK_LAUNCH_APP2          183

    //#endif // _WIN32_WINNT >= 0x0500

    /*
    * 0xB8 - 0xB9 : reserved
    */

    #define VK_OEM_1           186   // ';:' for US
    #define VK_OEM_PLUS        187   // '+' any country
    #define VK_OEM_COMMA       188   // ',' any country
    #define VK_OEM_MINUS       189   // '-' any country
    #define VK_OEM_PERIOD      190   // '.' any country
    #define VK_OEM_2           191   // '/?' for US
    #define VK_OEM_3           192   // '`~' for US

    /*
    * 0xC1 - 0xD7 : reserved
    */

    /*
    * 0xD8 - 0xDA : unassigned
    */

    #define VK_OEM_4           219  //  '[{' for US
    #define VK_OEM_5           220  //  '\|' for US
    #define VK_OEM_6           221  //  ']}' for US
    #define VK_OEM_7           222  //  ''"' for US
    #define VK_OEM_8           223

    /*
    * 0xE0 : reserved
    */

    /*
    * Various extended or enhanced keyboards
    */
    #define VK_OEM_AX          225  //  'AX' key on Japanese AX kbd
    #define VK_OEM_102         226  //  "<>" or "\|" on RT 102-key kbd.
    #define VK_ICO_HELP        227  //  Help key on ICO
    #define VK_ICO_00          228  //  00 key on ICO

    //#if(WINVER >=   1024)
    #define VK_PROCESSKEY      229
    //#endif // WINVER >= 0x0400

    #define VK_ICO_CLEAR       230


    //#if(_WIN32_WINNT >=   1280)
    #define VK_PACKET          231
    //#endif // _WIN32_WINNT >= 0x0500

    /*
    * 0xE8 : unassigned
    */

    /*
    * Nokia/Ericsson definitions
    */
    #define VK_OEM_RESET       233
    #define VK_OEM_JUMP        234
    #define VK_OEM_PA1         235
    #define VK_OEM_PA2         236
    #define VK_OEM_PA3         237
    #define VK_OEM_WSCTRL      238
    #define VK_OEM_CUSEL       239
    #define VK_OEM_ATTN        240
    #define VK_OEM_FINISH      241
    #define VK_OEM_COPY        242
    #define VK_OEM_AUTO        243
    #define VK_OEM_ENLW        244
    #define VK_OEM_BACKTAB     245

    #define VK_ATTN            246
    #define VK_CRSEL           247
    #define VK_EXSEL           248
    #define VK_EREOF           249
    #define VK_PLAY            250
    #define VK_ZOOM            251
    #define VK_NONAME          252
    #define VK_PA1             253
    #define VK_OEM_CLEAR       254

    /*
    * 0xFF : reserved
    */

  #endif // !NOVIRTUALKEYCODES

  #ifndef NOWH

    /*
    * SetWindowsHook() codes
    */
    #define WH_MIN              (-1)
    #define WH_MSGFILTER        (-1)
    #define WH_JOURNALRECORD    0
    #define WH_JOURNALPLAYBACK  1
    #define WH_KEYBOARD         2
    #define WH_GETMESSAGE       3
    #define WH_CALLWNDPROC      4
    #define WH_CBT              5
    #define WH_SYSMSGFILTER     6
    #define WH_MOUSE            7
    //#if defined(_WIN32_WINDOWS)
    #define WH_HARDWARE         8
    //#endif
    #define WH_DEBUG            9
    #define WH_SHELL           10
    #define WH_FOREGROUNDIDLE  11
    //#if(WINVER >=   1024)
    #define WH_CALLWNDPROCRET  12
    //#endif // WINVER >= 0x0400

    //#if (_WIN32_WINNT >=   1024)
    #define WH_KEYBOARD_LL     13
    #define WH_MOUSE_LL        14
    //#endif // (_WIN32_WINNT >= 0x0400)

    //#if(WINVER >=   1024)
    //#if (_WIN32_WINNT >=   1024)
    //#define WH_MAX             14
    //#else
    //#define WH_MAX             12
    //#endif // (_WIN32_WINNT >= 0x0400)
    //#else
    #define WH_MAX             11
    //#endif

    #define WH_MINHOOK         WH_MIN
    #define WH_MAXHOOK         WH_MAX

    /*
    * Hook Codes
    */
    #define HC_ACTION           0
    #define HC_GETNEXT          1
    #define HC_SKIP             2
    #define HC_NOREMOVE         3
    #define HC_NOREM            HC_NOREMOVE
    #define HC_SYSMODALON       4
    #define HC_SYSMODALOFF      5

    /*
    * CBT Hook Codes
    */
    #define HCBT_MOVESIZE       0
    #define HCBT_MINMAX         1
    #define HCBT_QS             2
    #define HCBT_CREATEWND      3
    #define HCBT_DESTROYWND     4
    #define HCBT_ACTIVATE       5
    #define HCBT_CLICKSKIPPED   6
    #define HCBT_KEYSKIPPED     7
    #define HCBT_SYSCOMMAND     8
    #define HCBT_SETFOCUS       9

    /*
    * HCBT_CREATEWND parameters pointed to by lParam
    */


    /*
    * HCBT_CREATEWND parameters pointed to by lParam
    */


    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE

    /*
    * HCBT_ACTIVATE structure pointed to by lParam
    */


    /*
    * WH_MSGFILTER Filter Proc Codes
    */
    #define MSGF_DIALOGBOX      0
    #define MSGF_MESSAGEBOX     1
    #define MSGF_MENU           2
    #define MSGF_SCROLLBAR      5
    #define MSGF_NEXTWINDOW     6
    #define MSGF_MAX            8                       // unused
    #define MSGF_USER           4096

    /*
    * Shell support
    */
    #define HSHELL_WINDOWCREATED        1
    #define HSHELL_WINDOWDESTROYED      2
    #define HSHELL_ACTIVATESHELLWINDOW  3

    //#if(WINVER >=   1024)
    #define HSHELL_WINDOWACTIVATED      4
    #define HSHELL_GETMINRECT           5
    #define HSHELL_REDRAW               6
    #define HSHELL_TASKMAN              7
    #define HSHELL_LANGUAGE             8
    //#endif // WINVER >= 0x0400
    //#if(_WIN32_WINNT >=   1280)
    #define HSHELL_ACCESSIBILITYSTATE   11
    #define HSHELL_APPCOMMAND           12
    //#endif // _WIN32_WINNT >= 0x0500


    //#if(_WIN32_WINNT >=   1280)
    //// wparam for HSHELL_ACCESSIBILITYSTATE
    #define    ACCESS_STICKYKEYS                 1
    #define    ACCESS_FILTERKEYS                 2
    #define    ACCESS_MOUSEKEYS                  3
    //
    //// cmd for HSHELL_APPCOMMAND and WM_APPCOMMAND
    #define APPCOMMAND_BROWSER_BACKWARD       1
    #define APPCOMMAND_BROWSER_FORWARD        2
    #define APPCOMMAND_BROWSER_REFRESH        3
    #define APPCOMMAND_BROWSER_STOP           4
    #define APPCOMMAND_BROWSER_SEARCH         5
    #define APPCOMMAND_BROWSER_FAVORITES      6
    #define APPCOMMAND_BROWSER_HOME           7
    #define APPCOMMAND_VOLUME_MUTE            8
    #define APPCOMMAND_VOLUME_DOWN            9
    #define APPCOMMAND_VOLUME_UP              10
    #define APPCOMMAND_MEDIA_NEXTTRACK        11
    #define APPCOMMAND_MEDIA_PREVIOUSTRACK    12
    #define APPCOMMAND_MEDIA_STOP             13
    #define APPCOMMAND_MEDIA_PLAY_PAUSE       14
    #define APPCOMMAND_LAUNCH_MAIL            15
    #define APPCOMMAND_LAUNCH_MEDIA_SELECT    16
    #define APPCOMMAND_LAUNCH_APP1            17
    #define APPCOMMAND_LAUNCH_APP2            18
    #define APPCOMMAND_BASS_DOWN              19
    #define APPCOMMAND_BASS_BOOST             20
    #define APPCOMMAND_BASS_UP                21
    #define APPCOMMAND_TREBLE_DOWN            22
    #define APPCOMMAND_TREBLE_UP              23

    #define FAPPCOMMAND_MOUSE  32768
    #define FAPPCOMMAND_KEY   0
    #define FAPPCOMMAND_OEM     4096
    #define FAPPCOMMAND_MASK   61440

    #define GET_APPCOMMAND_LPARAM(lParam) ((short)(HIWORD(lParam) * ~FAPPCOMMAND_MASK))
    #define GET_DEVICE_LPARAM(lParam)     ((WORD)(HIWORD(lParam) * FAPPCOMMAND_MASK))
    #define GET_MOUSEORKEY_LPARAM         GET_DEVICE_LPARAM
    #define GET_FLAGS_LPARAM(lParam)      (LOWORD(lParam))
    #define GET_KEYSTATE_LPARAM(lParam)   GET_FLAGS_LPARAM(lParam)
    //#endif // _WIN32_WINNT >= 0x0500


    /*
    * Message Structure used in Journaling
    */


    /*
    * Message structure used by WH_CALLWNDPROC
    */


    //#if(WINVER >=   1024)
    ///*
    // * Message structure used by WH_CALLWNDPROCRET
    // */
    //
    //
    //#endif // WINVER >= 0x0400

    //#if (_WIN32_WINNT >=   1024)

    /*
    * Low level hook flags
    */

    #define LLKHF_EXTENDED       (KF_EXTENDED >> 8)
    #define LLKHF_INJECTED               16
    #define LLKHF_ALTDOWN        (KF_ALTDOWN >> 8)
    #define LLKHF_UP             (KF_UP >> 8)

    #define LLMHF_INJECTED                1

    /*
    * Structure used by WH_KEYBOARD_LL
    */


    /*
    * Structure used by WH_MOUSE_LL
    */


    //#endif // (_WIN32_WINNT >= 0x0400)

    /*
    * Structure used by WH_DEBUG
    */


    /*
    * Structure used by WH_MOUSE
    */


    //#if(_WIN32_WINNT >=   1280)
    //#ifdef __cplusplus
    //
    //
    //#else // ndef __cplusplus
    //
    //
    //#endif
    //#endif // _WIN32_WINNT >= 0x0500
    //
    //#if(WINVER >=   1024)
    ///*
    // * Structure used by WH_HARDWARE
    // */
    //
    //
    //#endif // WINVER >= 0x0400
  #endif // !NOWH

  /*
  * Keyboard Layout API
  */
  #define HKL_PREV            0
  #define HKL_NEXT            1


  #define KLF_ACTIVATE                 1
  #define KLF_SUBSTITUTE_OK            2
  #define KLF_REORDER                  8
  //#if(WINVER >=   1024)
  #define KLF_REPLACELANG             16
  #define KLF_NOTELLSHELL            128
  //#endif // WINVER >= 0x0400
  #define KLF_SETFORPROCESS          256
  //#if(_WIN32_WINNT >=   1280)
  #define KLF_SHIFTLOCK            65536
  #define KLF_RESET           1073741824
  //#endif // _WIN32_WINNT >= 0x0500


  //#if(WINVER >=   1280)
  ///*
  // * Bits in wParam of WM_INPUTLANGCHANGEREQUEST message
  // */
  #define INPUTLANGCHANGE_SYSCHARSET      1
  #define INPUTLANGCHANGE_FORWARD         2
  #define INPUTLANGCHANGE_BACKWARD        4
  //#endif // WINVER >= 0x0500

  /*
  * Size of KeyboardLayoutName (number of characters), including nul terminator
  */
  #define KL_NAMELENGTH       9


  //#ifdef UNICODE
  //#define LoadKeyboardLayout  LoadKeyboardLayoutW
  //#else
  //#define LoadKeyboardLayout  LoadKeyboardLayoutA
  //#endif // !UNICODE


  //#if(WINVER >=   1024)
  //
  //
  //#else
  //
  //
  //#endif // WINVER >= 0x0400
  //
  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400


  //#ifdef UNICODE
  //#define GetKeyboardLayoutName  GetKeyboardLayoutNameW
  //#else
  //#define GetKeyboardLayoutName  GetKeyboardLayoutNameA
  //#endif // !UNICODE

  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400
  //
  //#if(WINVER >=   1280)
  //
  //
  ///*
  // * Values for resolution parameter of GetMouseMovePointsEx
  // */
  #define GMMP_USE_DISPLAY_POINTS          1
  #define GMMP_USE_HIGH_RESOLUTION_POINTS  2
  //
  //
  //#endif // WINVER >= 0x0500

  #ifndef NODESKTOP

    /*
    * Desktop-specific access flags
    */
    #define DESKTOP_READOBJECTS               1
    #define DESKTOP_CREATEWINDOW              2
    #define DESKTOP_CREATEMENU                4
    #define DESKTOP_HOOKCONTROL               8
    #define DESKTOP_JOURNALRECORD            16
    #define DESKTOP_JOURNALPLAYBACK          32
    #define DESKTOP_ENUMERATE                64
    #define DESKTOP_WRITEOBJECTS            128
    #define DESKTOP_SWITCHDESKTOP           256

    /*
    * Desktop-specific control flags
    */
    #define DF_ALLOWOTHERACCOUNTHOOK          1

    #ifdef _WINGDI_

      #ifndef NOGDI

        //#ifdef UNICODE
        //#define CreateDesktop  CreateDesktopW
        //#else
        //#define CreateDesktop  CreateDesktopA
        //#endif // !UNICODE

      #endif // NOGDI
    #endif // _WINGDI_


    //#ifdef UNICODE
    //#define OpenDesktop  OpenDesktopW
    //#else
    //#define OpenDesktop  OpenDesktopA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define EnumDesktops  EnumDesktopsW
    //#else
    //#define EnumDesktops  EnumDesktopsA
    //#endif // !UNICODE


  #endif  // !NODESKTOP

  #ifndef NOWINDOWSTATION

    /*
    * Windowstation-specific access flags
    */
    #define WINSTA_ENUMDESKTOPS               1
    #define WINSTA_READATTRIBUTES             2
    #define WINSTA_ACCESSCLIPBOARD            4
    #define WINSTA_CREATEDESKTOP              8
    #define WINSTA_WRITEATTRIBUTES           16
    #define WINSTA_ACCESSGLOBALATOMS         32
    #define WINSTA_EXITWINDOWS               64
    #define WINSTA_ENUMERATE                256
    #define WINSTA_READSCREEN               512

    /*
    * Windowstation-specific attribute flags
    */
    #define WSF_VISIBLE                       1


    //#ifdef UNICODE
    //#define CreateWindowStation  CreateWindowStationW
    //#else
    //#define CreateWindowStation  CreateWindowStationA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define OpenWindowStation  OpenWindowStationW
    //#else
    //#define OpenWindowStation  OpenWindowStationA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define EnumWindowStations  EnumWindowStationsW
    //#else
    //#define EnumWindowStations  EnumWindowStationsA
    //#endif // !UNICODE

  #endif  // !NOWINDOWSTATION

  #ifndef NOSECURITY

    #define UOI_FLAGS       1
    #define UOI_NAME        2
    #define UOI_TYPE        3
    #define UOI_USER_SID    4


    //#ifdef UNICODE
    //#define GetUserObjectInformation  GetUserObjectInformationW
    //#else
    //#define GetUserObjectInformation  GetUserObjectInformationA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define SetUserObjectInformation  SetUserObjectInformationW
    //#else
    //#define SetUserObjectInformation  SetUserObjectInformationA
    //#endif // !UNICODE

  #endif  // !NOSECURITY

  //#if(WINVER >=   1024)
  //
  //
  //    // Win 3.x
  //
  //
  //    // Win 4.0
  //
  //
  //    // Win 3.x
  //
  //
  //    // Win 4.0
  //
  //
  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE
  //#endif // WINVER >= 0x0400


  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE


  #ifndef NOMSG

    /*
    * Message structure
    */


    //#ifdef _MAC
    //
    //#endif


    #define POINTSTOPOINT(pt, pts)                          \

    /*
    #define POINTTOPOINTS(pt)      (MAKELONG((short)((pt).x), (short)((pt).y)))
    #define MAKEWPARAM(l, h)      ((WPARAM)(DWORD)MAKELONG(l, h))
    #define MAKELPARAM(l, h)      ((LPARAM)(DWORD)MAKELONG(l, h))
    #define MAKELRESULT(l, h)     ((LRESULT)(DWORD)MAKELONG(l, h))
    */

  #endif // !NOMSG

  #ifndef NOWINOFFSETS

    /*
    * Window field offsets for GetWindowLong()
    */
    #define GWL_WNDPROC         (-4)
    #define GWL_HINSTANCE       (-6)
    #define GWL_HWNDPARENT      (-8)
    #define GWL_STYLE           (-16)
    #define GWL_EXSTYLE         (-20)
    #define GWL_USERDATA        (-21)
    #define GWL_ID              (-12)

    //#ifdef _WIN64
    //
    //
    //#endif // _WIN64

    #define GWLP_WNDPROC        (-4)
    #define GWLP_HINSTANCE      (-6)
    #define GWLP_HWNDPARENT     (-8)
    #define GWLP_USERDATA       (-21)
    #define GWLP_ID             (-12)

    /*
    * Class field offsets for GetClassLong()
    */
    #define GCL_MENUNAME        (-8)
    #define GCL_HBRBACKGROUND   (-10)
    #define GCL_HCURSOR         (-12)
    #define GCL_HICON           (-14)
    #define GCL_HMODULE         (-16)
    #define GCL_CBWNDEXTRA      (-18)
    #define GCL_CBCLSEXTRA      (-20)
    #define GCL_WNDPROC         (-24)
    #define GCL_STYLE           (-26)
    #define GCW_ATOM            (-32)

    //#if(WINVER >=   1024)
    #define GCL_HICONSM         (-34)
    //#endif // WINVER >= 0x0400

    //#ifdef _WIN64
    //
    //
    //#endif // _WIN64

    #define GCLP_MENUNAME       (-8)
    #define GCLP_HBRBACKGROUND  (-10)
    #define GCLP_HCURSOR        (-12)
    #define GCLP_HICON          (-14)
    #define GCLP_HMODULE        (-16)
    #define GCLP_WNDPROC        (-24)
    #define GCLP_HICONSM        (-34)

  #endif // !NOWINOFFSETS

  #ifndef NOWINMESSAGES

    /*
    * Window Messages
    */

    #define WM_NULL                              0
    #define WM_CREATE                            1
    #define WM_DESTROY                           2
    #define WM_MOVE                              3
    #define WM_SIZE                              5

    #define WM_ACTIVATE                          6
    /*
    * WM_ACTIVATE state values
    */
    #define     WA_INACTIVE     0
    #define     WA_ACTIVE       1
    #define     WA_CLICKACTIVE  2

    #define WM_SETFOCUS                          7
    #define WM_KILLFOCUS                         8
    #define WM_ENABLE                           10
    #define WM_SETREDRAW                        11
    #define WM_SETTEXT                          12
    #define WM_GETTEXT                          13
    #define WM_GETTEXTLENGTH                    14
    #define WM_PAINT                            15
    #define WM_CLOSE                            16

    #ifndef _WIN32_WCE
      #define WM_QUERYENDSESSION                  17
      #define WM_QUERYOPEN                        19
      #define WM_ENDSESSION                       22
    #endif

    #define WM_QUIT                             18
    #define WM_ERASEBKGND                       20
    #define WM_SYSCOLORCHANGE                   21
    #define WM_SHOWWINDOW                       24
    #define WM_WININICHANGE                     26
    //#if(WINVER >=   1024)
    #define WM_SETTINGCHANGE                WM_WININICHANGE
    //#endif // WINVER >= 0x0400


    #define WM_DEVMODECHANGE                    27
    #define WM_ACTIVATEAPP                      28
    #define WM_FONTCHANGE                       29
    #define WM_TIMECHANGE                       30
    #define WM_CANCELMODE                       31
    #define WM_SETCURSOR                        32
    #define WM_MOUSEACTIVATE                    33
    #define WM_CHILDACTIVATE                    34
    #define WM_QUEUESYNC                        35

    #define WM_GETMINMAXINFO                    36
    /*
    * Struct pointed to by WM_GETMINMAXINFO lParam
    */


    #define WM_PAINTICON                        38
    #define WM_ICONERASEBKGND                   39
    #define WM_NEXTDLGCTL                       40
    #define WM_SPOOLERSTATUS                    42
    #define WM_DRAWITEM                         43
    #define WM_MEASUREITEM                      44
    #define WM_DELETEITEM                       45
    #define WM_VKEYTOITEM                       46
    #define WM_CHARTOITEM                       47
    #define WM_SETFONT                          48
    #define WM_GETFONT                          49
    #define WM_SETHOTKEY                        50
    #define WM_GETHOTKEY                        51
    #define WM_QUERYDRAGICON                    55
    #define WM_COMPAREITEM                      57
    //#if(WINVER >=   1280)
    //#ifndef _WIN32_WCE
    #define WM_GETOBJECT                        61
    //#endif
    //#endif // WINVER >= 0x0500
    #define WM_COMPACTING                       65
    #define WM_COMMNOTIFY                       68  // no longer suported
    #define WM_WINDOWPOSCHANGING                70
    #define WM_WINDOWPOSCHANGED                 71

    #define WM_POWER                            72
    /*
    * wParam for WM_POWER window message and DRV_POWER driver notification
    */
    #define PWR_OK              1
    #define PWR_FAIL            (-1)
    #define PWR_SUSPENDREQUEST  1
    #define PWR_SUSPENDRESUME   2
    #define PWR_CRITICALRESUME  3

    #define WM_COPYDATA                         74
    #define WM_CANCELJOURNAL                    75


    /*
    * lParam of WM_COPYDATA message points to...
    */


    //#if(WINVER >=   1024)
    //
    //
    //#endif // WINVER >= 0x0400
    //
    //
    //#if(WINVER >=   1024)
    #define WM_NOTIFY                           78
    #define WM_INPUTLANGCHANGEREQUEST           80
    #define WM_INPUTLANGCHANGE                  81
    #define WM_TCARD                            82
    #define WM_HELP                             83
    #define WM_USERCHANGED                      84
    #define WM_NOTIFYFORMAT                     85

    #define NFR_ANSI                             1
    #define NFR_UNICODE                          2
    #define NF_QUERY                             3
    #define NF_REQUERY                           4

    #define WM_CONTEXTMENU                     123
    #define WM_STYLECHANGING                   124
    #define WM_STYLECHANGED                    125
    #define WM_DISPLAYCHANGE                   126
    #define WM_GETICON                         127
    #define WM_SETICON                         128
    //#endif // WINVER >= 0x0400

    #define WM_NCCREATE                        129
    #define WM_NCDESTROY                       130
    #define WM_NCCALCSIZE                      131
    #define WM_NCHITTEST                       132
    #define WM_NCPAINT                         133
    #define WM_NCACTIVATE                      134
    #define WM_GETDLGCODE                      135
    //#ifndef _WIN32_WCE
    //#define WM_SYNCPAINT                       136
    //#endif
    #define WM_NCMOUSEMOVE                     160
    #define WM_NCLBUTTONDOWN                   161
    #define WM_NCLBUTTONUP                     162
    #define WM_NCLBUTTONDBLCLK                 163
    #define WM_NCRBUTTONDOWN                   164
    #define WM_NCRBUTTONUP                     165
    #define WM_NCRBUTTONDBLCLK                 166
    #define WM_NCMBUTTONDOWN                   167
    #define WM_NCMBUTTONUP                     168
    #define WM_NCMBUTTONDBLCLK                 169


    //#if(_WIN32_WINNT >=   1280)
    #define WM_NCXBUTTONDOWN                   171
    #define WM_NCXBUTTONUP                     172
    #define WM_NCXBUTTONDBLCLK                 173
    //#endif // _WIN32_WINNT >= 0x0500


    #define WM_KEYFIRST                        256
    #define WM_KEYDOWN                         256
    #define WM_KEYUP                           257
    #define WM_CHAR                            258
    #define WM_DEADCHAR                        259
    #define WM_SYSKEYDOWN                      260
    #define WM_SYSKEYUP                        261
    #define WM_SYSCHAR                         262
    #define WM_SYSDEADCHAR                     263
    #define WM_KEYLAST                         264

    //#if(WINVER >=   1024)
    #define WM_IME_STARTCOMPOSITION            269
    #define WM_IME_ENDCOMPOSITION              270
    #define WM_IME_COMPOSITION                 271
    #define WM_IME_KEYLAST                     271
    //#endif // WINVER >= 0x0400

    #define WM_INITDIALOG                      272
    #define WM_COMMAND                         273
    #define WM_SYSCOMMAND                      274
    #define WM_TIMER                           275
    #define WM_HSCROLL                         276
    #define WM_VSCROLL                         277
    #define WM_INITMENU                        278
    #define WM_INITMENUPOPUP                   279
    #define WM_MENUSELECT                      287
    #define WM_MENUCHAR                        288
    #define WM_ENTERIDLE                       289
    //#if(WINVER >=   1280)
    //#ifndef _WIN32_WCE
    #define WM_MENURBUTTONUP                   290
    #define WM_MENUDRAG                        291
    #define WM_MENUGETOBJECT                   292
    #define WM_UNINITMENUPOPUP                 293
    #define WM_MENUCOMMAND                     294
    //
    //#ifndef _WIN32_WCE
    //#if(_WIN32_WINNT >=   1280)
    #define WM_CHANGEUISTATE                   295
    #define WM_UPDATEUISTATE                   296
    #define WM_QUERYUISTATE                    297
    //
    ///*
    // * LOWORD(wParam) values in WM_*UISTATE*
    // */
    //#define UIS_SET         1
    //#define UIS_CLEAR       2
    //#define UIS_INITIALIZE  3
    //
    ///*
    // * HIWORD(wParam) values in WM_*UISTATE*
    // */
    //#define UISF_HIDEFOCUS     1
    //#define UISF_HIDEACCEL     2
    //#endif // _WIN32_WINNT >= 0x0500
    //#endif
    //
    //#endif
    //#endif // WINVER >= 0x0500


    #define WM_CTLCOLORMSGBOX                  306
    #define WM_CTLCOLOREDIT                    307
    #define WM_CTLCOLORLISTBOX                 308
    #define WM_CTLCOLORBTN                     309
    #define WM_CTLCOLORDLG                     310
    #define WM_CTLCOLORSCROLLBAR               311
    #define WM_CTLCOLORSTATIC                  312


    #define WM_MOUSEFIRST                      512
    #define WM_MOUSEMOVE                       512
    #define WM_LBUTTONDOWN                     513
    #define WM_LBUTTONUP                       514
    #define WM_LBUTTONDBLCLK                   515
    #define WM_RBUTTONDOWN                     516
    #define WM_RBUTTONUP                       517
    #define WM_RBUTTONDBLCLK                   518
    #define WM_MBUTTONDOWN                     519
    #define WM_MBUTTONUP                       520
    #define WM_MBUTTONDBLCLK                   521
    //#if (_WIN32_WINNT >=   1024) .OR. (_WIN32_WINDOWS >   1024)
    #define WM_MOUSEWHEEL                      522
    //#endif
    //#if (_WIN32_WINNT >=   1280)
    #define WM_XBUTTONDOWN                     523
    #define WM_XBUTTONUP                       524
    #define WM_XBUTTONDBLCLK                   525
    //#endif
    //#if (_WIN32_WINNT >=   1280)
    //#define WM_MOUSELAST                       525
    //
    //#define WM_MOUSELAST                       522
    //#else
    #define WM_MOUSELAST                       521
    //#endif // (_WIN32_WINNT >= 0x0500)


    //#if(_WIN32_WINNT >=   1024)
    //// Value for rolling one detent

    #define WHEEL_DELTA                     120
    #define GET_WHEEL_DELTA_WPARAM(wParam)  HIWORD(wParam)

    //
    //// Setting to scroll one page for SPI_GET/SETWHEELSCROLLLINES
    //#define WHEEL_PAGESCROLL                (UINT_MAX)
    //#endif // _WIN32_WINNT >= 0x0400
    //
    //#if(_WIN32_WINNT >=   1280)
    //#define GET_KEYSTATE_WPARAM(wParam)     (LOWORD(wParam))
    #define GET_NCHITTEST_WPARAM(wParam)    ((short)LOWORD(wParam))
    #define GET_XBUTTON_WPARAM(wParam)      (HIWORD(wParam))
    //
    //// XButton values are WORD flags
    //#define XBUTTON1           1
    //#define XBUTTON2           2
    //// Were there to be an XBUTTON3, it's value would be 0x0004
    //#endif // _WIN32_WINNT >= 0x0500

    #define WM_PARENTNOTIFY                    528
    #define WM_ENTERMENULOOP                   529
    #define WM_EXITMENULOOP                    530

    //#if(WINVER >=   1024)
    #define WM_NEXTMENU                        531
    #define WM_SIZING                          532
    #define WM_CAPTURECHANGED                  533
    #define WM_MOVING                          534
    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1024)


    #define WM_POWERBROADCAST                  536

    //#ifndef _WIN32_WCE
    //#define PBT_APMQUERYSUSPEND                  0
    //#define PBT_APMQUERYSTANDBY                  1
    //
    //#define PBT_APMQUERYSUSPENDFAILED            2
    //#define PBT_APMQUERYSTANDBYFAILED            3
    //
    //#define PBT_APMSUSPEND                       4
    //#define PBT_APMSTANDBY                       5
    //
    //#define PBT_APMRESUMECRITICAL                6
    //#define PBT_APMRESUMESUSPEND                 7
    //#define PBT_APMRESUMESTANDBY                 8
    //
    //#define PBTF_APMRESUMEFROMFAILURE                1
    //
    //#define PBT_APMBATTERYLOW                    9
    //#define PBT_APMPOWERSTATUSCHANGE            10
    //
    //#define PBT_APMOEMEVENT                     11
    //#define PBT_APMRESUMEAUTOMATIC              18
    //#endif
    //
    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1024)
    #define WM_DEVICECHANGE                    537
    //#endif // WINVER >= 0x0400

    #define WM_MDICREATE                       544
    #define WM_MDIDESTROY                      545
    #define WM_MDIACTIVATE                     546
    #define WM_MDIRESTORE                      547
    #define WM_MDINEXT                         548
    #define WM_MDIMAXIMIZE                     549
    #define WM_MDITILE                         550
    #define WM_MDICASCADE                      551
    #define WM_MDIICONARRANGE                  552
    #define WM_MDIGETACTIVE                    553


    #define WM_MDISETMENU                      560
    #define WM_ENTERSIZEMOVE                   561
    #define WM_EXITSIZEMOVE                    562
    #define WM_DROPFILES                       563
    #define WM_MDIREFRESHMENU                  564


    //#if(WINVER >=   1024)
    #define WM_IME_SETCONTEXT                  641
    #define WM_IME_NOTIFY                      642
    #define WM_IME_CONTROL                     643
    #define WM_IME_COMPOSITIONFULL             644
    #define WM_IME_SELECT                      645
    #define WM_IME_CHAR                        646
    //#endif // WINVER >= 0x0400
    //#if(WINVER >=   1280)
    #define WM_IME_REQUEST                     648
    //#endif // WINVER >= 0x0500
    //#if(WINVER >=   1024)
    #define WM_IME_KEYDOWN                     656
    #define WM_IME_KEYUP                       657
    //#endif // WINVER >= 0x0400

    //#if((_WIN32_WINNT >=   1024) .OR. (WINVER >=   1280))
    #define WM_MOUSEHOVER                      673
    #define WM_MOUSELEAVE                      675
    //#endif
    //#if(WINVER >=   1280)
    #define WM_NCMOUSEHOVER                    672
    #define WM_NCMOUSELEAVE                    674
    //#endif // WINVER >= 0x0500

    #define WM_CUT                             768
    #define WM_COPY                            769
    #define WM_PASTE                           770
    #define WM_CLEAR                           771
    #define WM_UNDO                            772
    #define WM_RENDERFORMAT                    773
    #define WM_RENDERALLFORMATS                774
    #define WM_DESTROYCLIPBOARD                775
    #define WM_DRAWCLIPBOARD                   776
    #define WM_PAINTCLIPBOARD                  777
    #define WM_VSCROLLCLIPBOARD                778
    #define WM_SIZECLIPBOARD                   779
    #define WM_ASKCBFORMATNAME                 780
    #define WM_CHANGECBCHAIN                   781
    #define WM_HSCROLLCLIPBOARD                782
    #define WM_QUERYNEWPALETTE                 783
    #define WM_PALETTEISCHANGING               784
    #define WM_PALETTECHANGED                  785
    #define WM_HOTKEY                          786

    //#if(WINVER >=   1024)
    #define WM_PRINT                           791
    #define WM_PRINTCLIENT                     792
    //#endif // WINVER >= 0x0400

    //#if(_WIN32_WINNT >=   1280)
    #define WM_APPCOMMAND                      793
    //#endif // _WIN32_WINNT >= 0x0500

    //#if(WINVER >=   1024)

    #define WM_HANDHELDFIRST                   856
    #define WM_HANDHELDLAST                    863

    #define WM_AFXFIRST                        864
    #define WM_AFXLAST                         895
    //#endif // WINVER >= 0x0400

    #define WM_PENWINFIRST                     896
    #define WM_PENWINLAST                      911


    //#if(WINVER >=   1024)
    #define WM_APP                           32768
    //#endif // WINVER >= 0x0400


    /*
    * NOTE: All Message Numbers below 0x0400 are RESERVED.
    *
    * Private Window Messages Start Here:
    */
    #define WM_USER                           1024

    //#if(WINVER >=   1024)

    //  wParam for WM_SIZING message
    #define WMSZ_LEFT           1
    #define WMSZ_RIGHT          2
    #define WMSZ_TOP            3
    #define WMSZ_TOPLEFT        4
    #define WMSZ_TOPRIGHT       5
    #define WMSZ_BOTTOM         6
    #define WMSZ_BOTTOMLEFT     7
    #define WMSZ_BOTTOMRIGHT    8
    //#endif // WINVER >= 0x0400

    #ifndef NONCMESSAGES

      /*
      * WM_NCHITTEST and MOUSEHOOKSTRUCT Mouse Position Codes
      */
      #define HTERROR             (-2)
      #define HTTRANSPARENT       (-1)
      #define HTNOWHERE           0
      #define HTCLIENT            1
      #define HTCAPTION           2
      #define HTSYSMENU           3
      #define HTGROWBOX           4
      #define HTSIZE              HTGROWBOX
      #define HTMENU              5
      #define HTHSCROLL           6
      #define HTVSCROLL           7
      #define HTMINBUTTON         8
      #define HTMAXBUTTON         9
      #define HTLEFT              10
      #define HTRIGHT             11
      #define HTTOP               12
      #define HTTOPLEFT           13
      #define HTTOPRIGHT          14
      #define HTBOTTOM            15
      #define HTBOTTOMLEFT        16
      #define HTBOTTOMRIGHT       17
      #define HTBORDER            18
      #define HTREDUCE            HTMINBUTTON
      #define HTZOOM              HTMAXBUTTON
      #define HTSIZEFIRST         HTLEFT
      #define HTSIZELAST          HTBOTTOMRIGHT
      //#if(WINVER >=   1024)
      #define HTOBJECT            19
      #define HTCLOSE             20
      #define HTHELP              21
      //#endif // WINVER >= 0x0400

      /*
      * SendMessageTimeout values
      */
      #define SMTO_NORMAL              0
      #define SMTO_BLOCK               1
      #define SMTO_ABORTIFHUNG         2
      //#if(WINVER >=   1280)
      #define SMTO_NOTIMEOUTIFNOTHUNG      8
      //#endif // WINVER >= 0x0500
    #endif // !NONCMESSAGES

    /*
    * WM_MOUSEACTIVATE Return Codes
    */
    #define MA_ACTIVATE         1
    #define MA_ACTIVATEANDEAT   2
    #define MA_NOACTIVATE       3
    #define MA_NOACTIVATEANDEAT 4

    /*
    * WM_SETICON / WM_GETICON Type Codes
    */
    #define ICON_SMALL          0
    #define ICON_BIG            1


    //#ifdef UNICODE
    //#define RegisterWindowMessage  RegisterWindowMessageW
    //#else
    //#define RegisterWindowMessage  RegisterWindowMessageA
    //#endif // !UNICODE


    /*
    * WM_SIZE message wParam values
    */
    #define SIZE_RESTORED       0
    #define SIZE_MINIMIZED      1
    #define SIZE_MAXIMIZED      2
    #define SIZE_MAXSHOW        3
    #define SIZE_MAXHIDE        4

    /*
    * Obsolete constant names
    */
    #define SIZENORMAL          SIZE_RESTORED
    #define SIZEICONIC          SIZE_MINIMIZED
    #define SIZEFULLSCREEN      SIZE_MAXIMIZED
    #define SIZEZOOMSHOW        SIZE_MAXSHOW
    #define SIZEZOOMHIDE        SIZE_MAXHIDE

    /*
    * WM_WINDOWPOSCHANGING/CHANGED struct pointed to by lParam
    */


    /*
    * WM_NCCALCSIZE parameter structure
    */


    /*
    * WM_NCCALCSIZE "window valid rect" return values
    */
    #define WVR_ALIGNTOP            16
    #define WVR_ALIGNLEFT           32
    #define WVR_ALIGNBOTTOM         64
    #define WVR_ALIGNRIGHT         128
    #define WVR_HREDRAW            256
    #define WVR_VREDRAW            512
    #define WVR_REDRAW         (WVR_HREDRAW + \

    #define WVR_VALIDRECTS        1024


    #ifndef NOKEYSTATES

      /*
      * Key State Masks for Mouse Messages
      */
      #define MK_LBUTTON               1
      #define MK_RBUTTON               2
      #define MK_SHIFT                 4
      #define MK_CONTROL               8
      #define MK_MBUTTON              16
      //#if(_WIN32_WINNT >=   1280)
      #define MK_XBUTTON1             32
      #define MK_XBUTTON2             64
      //#endif // _WIN32_WINNT >= 0x0500

    #endif // !NOKEYSTATES


    //#if(_WIN32_WINNT >=   1024)
    #ifndef NOTRACKMOUSEEVENT

      #define TME_HOVER                1
      #define TME_LEAVE                2
      //#if(WINVER >=   1280)
      #define TME_NONCLIENT           16
      //#endif // WINVER >= 0x0500
      #define TME_QUERY       1073741824
      #define TME_CANCEL      2147483648


      #define HOVER_DEFAULT   4294967295
      //#endif // _WIN32_WINNT >= 0x0400

      //#if(_WIN32_WINNT >=   1024)
      //
      //
      //#endif // _WIN32_WINNT >= 0x0400
      //
      //#if(_WIN32_WINNT >=   1024)

    #endif // !NOTRACKMOUSEEVENT
    //#endif // _WIN32_WINNT >= 0x0400


  #endif // !NOWINMESSAGES

  #ifndef NOWINSTYLES


    /*
    * Window Styles
    */
    #define WS_OVERLAPPED                 0
    #define WS_CLOSE             4294967296
    #define WS_POPUP             2147483648
    #define WS_CHILD             1073741824
    #define WS_MINIMIZE           536870912
    #define WS_VISIBLE            268435456
    #define WS_DISABLED           134217728
    #define WS_CLIPSIBLINGS        67108864
    #define WS_CLIPCHILDREN        33554432
    #define WS_MAXIMIZE            16777216
    #define WS_CAPTION             12582912     // WS_BORDER | WS_DLGFRAME
    #define WS_BORDER               8388608
    #define WS_DLGFRAME             4194304
    #define WS_VSCROLL              2097152
    #define WS_HSCROLL              1048576
    #define WS_SYSMENU               524288
    #define WS_THICKFRAME            262144
    #define WS_GROUP                 131072
    #define WS_TABSTOP                65536

    #define WS_MINIMIZEBOX           131072
    #define WS_MAXIMIZEBOX            65536


    #define WS_TILED            WS_OVERLAPPED
    #define WS_ICONIC           WS_MINIMIZE
    #define WS_SIZEBOX          WS_THICKFRAME
    #define WS_TILEDWINDOW      WS_OVERLAPPEDWINDOW

    /*
    * Common Window Styles
    */
    #define WS_OVERLAPPEDWINDOW (WS_OVERLAPPED     + ;
                                WS_CAPTION        + ;
                                WS_SYSMENU        + ;
                                WS_THICKFRAME     + ;
                                WS_MINIMIZEBOX    + ;
                                WS_MAXIMIZEBOX)

    #define WS_POPUPWINDOW      (WS_POPUP          + ;
                                WS_BORDER         + ;
                                WS_SYSMENU)

    #define WS_CHILDWINDOW      (WS_CHILD)

    /*
    * Extended Window Styles
    */
    #define WS_EX_DLGMODALFRAME               1
    #define WS_EX_NOPARENTNOTIFY              4
    #define WS_EX_TOPMOST                     8
    #define WS_EX_ACCEPTFILES                16
    #define WS_EX_TRANSPARENT                32
    //#if(WINVER >=   1024)
    #define WS_EX_MDICHILD                   64
    #define WS_EX_TOOLWINDOW                128
    #define WS_EX_WINDOWEDGE                256
    #define WS_EX_CLIENTEDGE                512
    #define WS_EX_CONTEXTHELP              1024

    #define WS_EX_RIGHT                    4096
    #define WS_EX_LEFT                        0
    #define WS_EX_RTLREADING               8192
    #define WS_EX_LTRREADING                  0
    #define WS_EX_LEFTSCROLLBAR           16384
    #define WS_EX_RIGHTSCROLLBAR              0

    #define WS_EX_CONTROLPARENT           65536
    #define WS_EX_STATICEDGE             131072
    #define WS_EX_APPWINDOW              262144


    #define WS_EX_OVERLAPPEDWINDOW  (WS_EX_WINDOWEDGE + WS_EX_CLIENTEDGE)
    #define WS_EX_PALETTEWINDOW     (WS_EX_WINDOWEDGE + WS_EX_TOOLWINDOW + WS_EX_TOPMOST)

    //#endif // WINVER >= 0x0400

    //#if(_WIN32_WINNT >=   1280)
    #define WS_EX_LAYERED               524288
    //#endif // _WIN32_WINNT >= 0x0500


    //#if(WINVER >=   1280)
    #define WS_EX_NOINHERITLAYOUT       1048576 // Disable inheritence of mirroring by children
    #define WS_EX_LAYOUTRTL             4194304 // Right to left mirroring
    //#endif // WINVER >= 0x0500
    //
    //#if(_WIN32_WINNT >=   1280)
    #define WS_EX_NOACTIVATE          134217728
    //#endif // _WIN32_WINNT >= 0x0500


    /*
    * Class styles
    */
    #define CS_VREDRAW               1
    #define CS_HREDRAW               2
    #define CS_DBLCLKS               8
    #define CS_OWNDC                32
    #define CS_CLASSDC              64
    #define CS_PARENTDC            128
    #define CS_NOCLOSE             512
    #define CS_SAVEBITS           2048
    #define CS_BYTEALIGNCLIENT    4096
    #define CS_BYTEALIGNWINDOW    8192
    #define CS_GLOBALCLASS       16384

    #define CS_IME                   65536

  #endif // !NOWINSTYLES

  //#if(WINVER >=   1024)
  // WM_PRINT flags
  #define PRF_CHECKVISIBLE              1
  #define PRF_NONCLIENT                 2
  #define PRF_CLIENT                    4
  #define PRF_ERASEBKGND                8
  #define PRF_CHILDREN                 16
  #define PRF_OWNED                    32

  // 3D border styles
  #define BDR_RAISEDOUTER      1
  #define BDR_SUNKENOUTER      2
  #define BDR_RAISEDINNER      4
  #define BDR_SUNKENINNER      8

  #define BDR_OUTER       (BDR_RAISEDOUTER + BDR_SUNKENOUTER)
  #define BDR_INNER       (BDR_RAISEDINNER + BDR_SUNKENINNER)
  #define BDR_RAISED      (BDR_RAISEDOUTER + BDR_RAISEDINNER)
  #define BDR_SUNKEN      (BDR_SUNKENOUTER + BDR_SUNKENINNER)


  #define EDGE_RAISED     (BDR_RAISEDOUTER + BDR_RAISEDINNER)
  #define EDGE_SUNKEN     (BDR_SUNKENOUTER + BDR_SUNKENINNER)
  #define EDGE_ETCHED     (BDR_SUNKENOUTER + BDR_RAISEDINNER)
  #define EDGE_BUMP       (BDR_RAISEDOUTER + BDR_SUNKENINNER)

  // Border flags
  #define BF_LEFT              1
  #define BF_TOP               2
  #define BF_RIGHT             4
  #define BF_BOTTOM            8

  #define BF_TOPLEFT      (BF_TOP + BF_LEFT)
  #define BF_TOPRIGHT     (BF_TOP + BF_RIGHT)
  #define BF_BOTTOMLEFT   (BF_BOTTOM + BF_LEFT)
  #define BF_BOTTOMRIGHT  (BF_BOTTOM + BF_RIGHT)
  #define BF_RECT         (BF_LEFT + BF_TOP + BF_RIGHT + BF_BOTTOM)

  #define BF_DIAGONAL         16

  // For diagonal lines, the BF_RECT flags specify the end point of the
  // vector bounded by the rectangle parameter.
  #define BF_DIAGONAL_ENDTOPRIGHT     (BF_DIAGONAL + BF_TOP + BF_RIGHT)
  #define BF_DIAGONAL_ENDTOPLEFT      (BF_DIAGONAL + BF_TOP + BF_LEFT)
  #define BF_DIAGONAL_ENDBOTTOMLEFT   (BF_DIAGONAL + BF_BOTTOM + BF_LEFT)
  #define BF_DIAGONAL_ENDBOTTOMRIGHT  (BF_DIAGONAL + BF_BOTTOM + BF_RIGHT)


  #define BF_MIDDLE         2048  // Fill in the middle
  #define BF_SOFT           4096  // For softer buttons
  #define BF_ADJUST         8192  // Calculate the space left over
  #define BF_FLAT          16384  // For flat rather than 3D borders
  #define BF_MONO          32768  // For monochrome borders


  // flags for DrawFrameControl

  #define DFC_CAPTION             1
  #define DFC_MENU                2
  #define DFC_SCROLL              3
  #define DFC_BUTTON              4
  //#if(WINVER >=   1280)
  #define DFC_POPUPMENU           5
  //#endif // WINVER >= 0x0500

  #define DFCS_CAPTIONCLOSE            0
  #define DFCS_CAPTIONMIN              1
  #define DFCS_CAPTIONMAX              2
  #define DFCS_CAPTIONRESTORE          3
  #define DFCS_CAPTIONHELP             4

  #define DFCS_MENUARROW               0
  #define DFCS_MENUCHECK               1
  #define DFCS_MENUBULLET              2
  #define DFCS_MENUARROWRIGHT          4
  #define DFCS_SCROLLUP                0
  #define DFCS_SCROLLDOWN              1
  #define DFCS_SCROLLLEFT              2
  #define DFCS_SCROLLRIGHT             3
  #define DFCS_SCROLLCOMBOBOX          5
  #define DFCS_SCROLLSIZEGRIP          8
  #define DFCS_SCROLLSIZEGRIPRIGHT     16

  #define DFCS_BUTTONCHECK             0
  #define DFCS_BUTTONRADIOIMAGE        1
  #define DFCS_BUTTONRADIOMASK         2
  #define DFCS_BUTTONRADIO             4
  #define DFCS_BUTTON3STATE            8
  #define DFCS_BUTTONPUSH             16

  #define DFCS_INACTIVE              256
  #define DFCS_PUSHED                512
  #define DFCS_CHECKED              1024

  //#if(WINVER >=   1280)
  #define DFCS_TRANSPARENT          2048
  #define DFCS_HOT                  4096
  //#endif // WINVER >= 0x0500

  #define DFCS_ADJUSTRECT           8192
  #define DFCS_FLAT                16384
  #define DFCS_MONO                32768


  // flags for DrawCaption
  #define DC_ACTIVE                1
  #define DC_SMALLCAP              2
  #define DC_ICON                  4
  #define DC_TEXT                  8
  #define DC_INBUTTON             16
  //#if(WINVER >=   1280)
  #define DC_GRADIENT             32
  //#endif // WINVER >= 0x0500


  #define IDANI_OPEN          1


  //#endif // WINVER >= 0x0400

  #ifndef NOCLIPBOARD

    /*
    * Predefined Clipboard Formats
    */
    #define CF_TEXT             1
    #define CF_BITMAP           2
    #define CF_METAFILEPICT     3
    #define CF_SYLK             4
    #define CF_DIF              5
    #define CF_TIFF             6
    #define CF_OEMTEXT          7
    #define CF_DIB              8
    #define CF_PALETTE          9
    #define CF_PENDATA          10
    #define CF_RIFF             11
    #define CF_WAVE             12
    #define CF_UNICODETEXT      13
    #define CF_ENHMETAFILE      14
    //#if(WINVER >=   1024)
    #define CF_HDROP            15
    #define CF_LOCALE           16
    //#endif // WINVER >= 0x0400
    //#if(WINVER >=   1280)
    #define CF_DIBV5            17
    //#endif // WINVER >= 0x0500

    //#if(WINVER >=   1280)
    //#define CF_MAX              18
    //
    //#define CF_MAX              17
    //#else
    #define CF_MAX              15
    //#endif

    #define CF_OWNERDISPLAY        128
    #define CF_DSPTEXT             129
    #define CF_DSPBITMAP           130
    #define CF_DSPMETAFILEPICT     131
    #define CF_DSPENHMETAFILE      142

    /*
    * "Private" formats don't get GlobalFree()'d
    */
    #define CF_PRIVATEFIRST        512
    #define CF_PRIVATELAST         767

    /*
    * "GDIOBJ" formats do get DeleteObject()'d
    */
    #define CF_GDIOBJFIRST         768
    #define CF_GDIOBJLAST         1023

  #endif // !NOCLIPBOARD

  /*
  * Defines for the fVirt field of the Accelerator table structure.
  */
  #define FVIRTKEY     1         // Assumed to be == TRUE
  #define FNOINVERT    2
  #define FSHIFT       4
  #define FCONTROL     8
  #define FALT        16


  //#ifndef _MAC
  //                   // Also called the flags field
  //
  //
  //#else
  //                   // Also called the flags field
  //
  //
  //#endif
  //
  //
  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE


  //#ifdef _MAC
  //
  //#endif


  #define WPF_SETMINPOSITION               1
  #define WPF_RESTORETOMAXIMIZED           2
  //#if(_WIN32_WINNT >=   1280)
  #define WPF_ASYNCWINDOWPLACEMENT         4
  //#endif // _WIN32_WINNT >= 0x0500

  //#if(WINVER >=   1024)
  //
  //
  //             // NM_ code
  //
  //
  //#endif // WINVER >= 0x0400


  /*
  * Owner draw control types
  */
  #define ODT_MENU        1
  #define ODT_LISTBOX     2
  #define ODT_COMBOBOX    3
  #define ODT_BUTTON      4
  //#if(WINVER >=   1024)
  #define ODT_STATIC      5
  //#endif // WINVER >= 0x0400

  /*
  * Owner draw actions
  */
  #define ODA_DRAWENTIRE       1
  #define ODA_SELECT           2
  #define ODA_FOCUS            4

  /*
  * Owner draw state
  */
  #define ODS_SELECTED         1
  #define ODS_GRAYED           2
  #define ODS_DISABLED         4
  #define ODS_CHECKED          8
  #define ODS_FOCUS           16
  //#if(WINVER >=   1024)
  #define ODS_DEFAULT             32
  #define ODS_COMBOBOXEDIT      4096
  //#endif // WINVER >= 0x0400
  //#if(WINVER >=   1280)
  #define ODS_HOTLIGHT            64
  #define ODS_INACTIVE           128
  //#if(_WIN32_WINNT >=   1280)
  #define ODS_NOACCEL            256
  #define ODS_NOFOCUSRECT        512
  //#endif // _WIN32_WINNT >= 0x0500
  //#endif // WINVER >= 0x0500

  /*
  * MEASUREITEMSTRUCT for ownerdraw
  */


  /*
  * DRAWITEMSTRUCT for ownerdraw
  */


  /*
  * DELETEITEMSTRUCT for ownerdraw
  */


  /*
  * COMPAREITEMSTUCT for ownerdraw sorting
  */


  #ifndef NOMSG

    /*
    * Message Function Templates
    */


    //#ifdef UNICODE
    //#define GetMessage  GetMessageW
    //#else
    //#define GetMessage  GetMessageA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define DispatchMessage  DispatchMessageW
    //#else
    //#define DispatchMessage  DispatchMessageA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define PeekMessage  PeekMessageW
    //#else
    //#define PeekMessage  PeekMessageA
    //#endif // !UNICODE

    /*
    * PeekMessage() Options
    */
    #define PM_NOREMOVE              0
    #define PM_REMOVE                1
    #define PM_NOYIELD               2
    //#if(WINVER >=   1280)
    #define PM_QS_INPUT         (QS_INPUT << 16)
    #define PM_QS_POSTMESSAGE   ((QS_POSTMESSAGE + QS_HOTKEY + QS_TIMER) << 16)
    #define PM_QS_PAINT         (QS_PAINT << 16)
    #define PM_QS_SENDMESSAGE   (QS_SENDMESSAGE << 16)
    //#endif // WINVER >= 0x0500

  #endif // !NOMSG


  #define MOD_ALT              1
  #define MOD_CONTROL          2
  #define MOD_SHIFT            4
  #define MOD_WIN              8


  #define IDHOT_SNAPWINDOW        (-1)    // SHIFT-PRINTSCRN
  #define IDHOT_SNAPDESKTOP       (-2)    // PRINTSCRN

  #ifdef WIN_INTERNAL
     #ifndef LSTRING
       #define NOLSTRING
     #endif // LSTRING

     #ifndef LFILEIO
       #define NOLFILEIO
     #endif // LFILEIO
  #endif // WIN_INTERNAL

  //#if(WINVER >=   1024)

  #define ENDSESSION_LOGOFF    2147483648
  //#endif // WINVER >= 0x0400

  #define EWX_LOGOFF          0
  #define EWX_SHUTDOWN                 1
  #define EWX_REBOOT                   2
  #define EWX_FORCE                    4
  #define EWX_POWEROFF                 8
  //#if(_WIN32_WINNT >=   1280)
  #define EWX_FORCEIFHUNG             16
  //#endif // _WIN32_WINNT >= 0x0500


  #define ExitWindows(dwReserved, Code) ExitWindowsEx(EWX_LOGOFF, 4294967295)


  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400


  //#ifdef UNICODE
  //#define SendMessage  SendMessageW
  //#else
  //#define SendMessage  SendMessageA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define SendMessageTimeout  SendMessageTimeoutW
  //#else
  //#define SendMessageTimeout  SendMessageTimeoutA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define SendNotifyMessage  SendNotifyMessageW
  //#else
  //#define SendNotifyMessage  SendNotifyMessageA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define SendMessageCallback  SendMessageCallbackW
  //#else
  //#define SendMessageCallback  SendMessageCallbackA
  //#endif // !UNICODE
  //
  ////#if(WINVER >=   1024)
  ////
  ////#if defined(_WIN32_WINNT)
  //
  //
  //#ifdef UNICODE
  //#define BroadcastSystemMessage  BroadcastSystemMessageW
  //#else
  //#define BroadcastSystemMessage  BroadcastSystemMessageA
  //#endif // !UNICODE

  // The Win95 version isn't A/W decorated


  //#endif

  //Broadcast Special Message Recipient list
  #define BSM_ALLCOMPONENTS                0
  #define BSM_VXDS                         1
  #define BSM_NETDRIVER                    2
  #define BSM_INSTALLABLEDRIVERS           4
  #define BSM_APPLICATIONS                 8
  #define BSM_ALLDESKTOPS                 16

  //Broadcast Special Message Flags
  #define BSF_QUERY                        1
  #define BSF_IGNORECURRENTTASK            2
  #define BSF_FLUSHDISK                    4
  #define BSF_NOHANG                       8
  #define BSF_POSTMESSAGE                 16
  #define BSF_FORCEIFHUNG                 32
  #define BSF_NOTIMEOUTIFNOTHUNG          64
  //#if(_WIN32_WINNT >=   1280)
  #define BSF_ALLOWSFW                   128
  #define BSF_SENDNOTIFYMESSAGE          256
  //#endif // _WIN32_WINNT >= 0x0500

  #define BROADCAST_QUERY_DENY         1112363332  // Return this value to deny a query.
  //#endif // WINVER >= 0x0400

  // RegisterDeviceNotification

  //#if(WINVER >=   1280)
  //
  //
  #define DEVICE_NOTIFY_WINDOW_HANDLE              0
  #define DEVICE_NOTIFY_SERVICE_HANDLE             1
  //
  //
  //#ifdef UNICODE
  //#define RegisterDeviceNotification  RegisterDeviceNotificationW
  //#else
  //#define RegisterDeviceNotification  RegisterDeviceNotificationA
  //#endif // !UNICODE
  //
  //
  //#endif // WINVER >= 0x0500


  //#ifdef UNICODE
  //#define PostMessage  PostMessageW
  //#else
  //#define PostMessage  PostMessageA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define PostThreadMessage  PostThreadMessageW
  //#else
  //#define PostThreadMessage  PostThreadMessageA
  //#endif // !UNICODE

  #define PostAppMessageA(idThread, wMsg, wParam, lParam)\

  #define PostAppMessageW(idThread, wMsg, wParam, lParam)\

  //#ifdef UNICODE
  //#define PostAppMessage  PostAppMessageW
  //#else
  //#define PostAppMessage  PostAppMessageA
  //#endif // !UNICODE

  /*
  * Special HWND value for use with PostMessage() and SendMessage()
  */
  #define HWND_BROADCAST  65535

  //#if(WINVER >=   1280)
  #define HWND_MESSAGE     -3
  //#endif // WINVER >= 0x0500


  //#ifndef _MAC
  //
  //
  //#else
  //
  //
  //#endif
  //
  //
  //#ifndef _MAC
  //
  //
  //#else
  //
  //
  //#endif


  //#ifdef UNICODE
  //#define DefWindowProc  DefWindowProcW
  //#else
  //#define DefWindowProc  DefWindowProcA
  //#endif // !UNICODE


  //#ifdef STRICT

    //#ifdef UNICODE
    //#define CallWindowProc  CallWindowProcW
    //#else
    //#define CallWindowProc  CallWindowProcA
    //#endif // !UNICODE

  //#else // !STRICT

    //#ifdef UNICODE
    //#define CallWindowProc  CallWindowProcW
    //#else
    //#define CallWindowProc  CallWindowProcA
    //#endif // !UNICODE

  //#endif // !STRICT


  //#if(WINVER >=   1280)
  //
  //
  ///*
  // * InSendMessageEx return value
  // */
  #define ISMEX_NOSEND               0
  #define ISMEX_SEND                 1
  #define ISMEX_NOTIFY               2
  #define ISMEX_CALLBACK             4
  #define ISMEX_REPLIED              8
  //#endif // WINVER >= 0x0500


  //#ifdef UNICODE
  //#define RegisterClass  RegisterClassW
  //#else
  //#define RegisterClass  RegisterClassA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define UnregisterClass  UnregisterClassW
  //#else
  //#define UnregisterClass  UnregisterClassA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetClassInfo  GetClassInfoW
  //#else
  //#define GetClassInfo  GetClassInfoA
  //#endif // !UNICODE

  //#if(WINVER >=   1024)


  //#ifdef UNICODE
  //#define RegisterClassEx  RegisterClassExW
  //#else
  //#define RegisterClassEx  RegisterClassExA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetClassInfoEx  GetClassInfoExW
  //#else
  //#define GetClassInfoEx  GetClassInfoExA
  //#endif // !UNICODE

  //#endif // WINVER >= 0x0400

  //#define CW_USEDEFAULT       ((int)2147483648)
  #define CW_USEDEFAULT       2147483648

  /*
  * Special value for CreateWindow, et al.
  */
  #define HWND_DESKTOP        ((HWND)0)


  //#ifdef UNICODE
  //#define CreateWindowEx  CreateWindowExW
  //#else
  //#define CreateWindowEx  CreateWindowExA
  //#endif // !UNICODE



  //#ifdef UNICODE
  //#define CreateWindowW(lpClassName, lpWindowName, dwStyle, x, y,\
  //#else
  //#define CreateWindowA(lpClassName, lpWindowName, dwStyle, x, y,\
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define CreateWindow  CreateWindowW
  //#else
  //#define CreateWindow  CreateWindowA
  //#endif // !UNICODE


  //#if(WINVER >=   1280)
  //
  //
  //#endif // WINVER >= 0x0500
  //
  //#if(_WIN32_WINNT >=   1280)
  //#if defined(_WINGDI_) .AND. !defined (NOGDI)
  //
  //
  //#endif
  //
  //
  #define LWA_COLORKEY                     1
  #define LWA_ALPHA                        2
  //
  //
  //#define ULW_COLORKEY                     1
  //#define ULW_ALPHA                        2
  //#define ULW_OPAQUE                       4
  //
  //#endif // _WIN32_WINNT >= 0x0500

  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400
  //
  //
  //#if(WINVER >=   1280)
  //
  //
  //#define FLASHW_STOP         0
  //#define FLASHW_CAPTION               1
  //#define FLASHW_TRAY                  2
  //#define FLASHW_ALL          (FLASHW_CAPTION + FLASHW_TRAY)
  //#define FLASHW_TIMER                 4
  //#define FLASHW_TIMERNOFG            12
  //
  //#endif // WINVER >= 0x0500


  //#ifndef NODEFERWINDOWPOS

  //#endif // !NODEFERWINDOWPOS


  /*
  * SetWindowPos Flags
  */
  #define SWP_NOSIZE               1
  #define SWP_NOMOVE               2
  #define SWP_NOZORDER             4
  #define SWP_NOREDRAW             8
  #define SWP_NOACTIVATE          16
  #define SWP_FRAMECHANGED        32  // The frame changed: send WM_NCCALCSIZE
  #define SWP_SHOWWINDOW          64
  #define SWP_HIDEWINDOW         128
  #define SWP_NOCOPYBITS         256
  #define SWP_NOOWNERZORDER      512  // Don't do owner Z ordering
  #define SWP_NOSENDCHANGING    1024  // Don't send WM_WINDOWPOSCHANGING

  #define SWP_DRAWFRAME       SWP_FRAMECHANGED
  #define SWP_NOREPOSITION    SWP_NOOWNERZORDER

  //#if(WINVER >=   1024)
  #define SWP_DEFERERASE        8192
  #define SWP_ASYNCWINDOWPOS   16384
  //#endif // WINVER >= 0x0400


  #define HWND_TOP        (0)
  #define HWND_BOTTOM     (1)
  #define HWND_TOPMOST    (-1)
  #define HWND_NOTOPMOST  (-2)

  #ifndef NOCTLMGR

    /*
    * WARNING:
    * The following structures must NOT be DWORD padded because they are
    * followed by strings, etc that do not have to be DWORD aligned.
    */
    //#include "pshpack2.ch"

    /*
    * original NT 32 bit dialog template:
    */


    //#ifdef UNICODE
    //
    //#else
    //
    //#endif // UNICODE
    //
    //
    //#ifdef UNICODE
    //
    //#else
    //
    //#endif // UNICODE
    //
    ///*
    // * 32 bit Dialog item template.
    // */
    //
    //
    //#ifdef UNICODE
    //
    //#else
    //
    //#endif // UNICODE
    //
    //
    //#ifdef UNICODE
    //
    //#else
    //
    //#endif // UNICODE
    //
    //
    //#include "poppack.ch" // Resume normal packing


    //#ifdef UNICODE
    //#define CreateDialogParam  CreateDialogParamW
    //#else
    //#define CreateDialogParam  CreateDialogParamA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define CreateDialogIndirectParam  CreateDialogIndirectParamW
    //#else
    //#define CreateDialogIndirectParam  CreateDialogIndirectParamA
    //#endif // !UNICODE
    //
    //#define CreateDialogA(hInstance, lpName, hWndParent, lpDialogFunc) \
    //
    //#define CreateDialogW(hInstance, lpName, hWndParent, lpDialogFunc) \
    //
    //#ifdef UNICODE
    //#define CreateDialog  CreateDialogW
    //#else
    //#define CreateDialog  CreateDialogA
    //#endif // !UNICODE
    //
    //#define CreateDialogIndirectA(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#define CreateDialogIndirectW(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#ifdef UNICODE
    //#define CreateDialogIndirect  CreateDialogIndirectW
    //#else
    //#define CreateDialogIndirect  CreateDialogIndirectA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define DialogBoxParam  DialogBoxParamW
    //#else
    //#define DialogBoxParam  DialogBoxParamA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define DialogBoxIndirectParam  DialogBoxIndirectParamW
    //#else
    //#define DialogBoxIndirectParam  DialogBoxIndirectParamA
    //#endif // !UNICODE
    //
    //#define DialogBoxA(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#define DialogBoxW(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#ifdef UNICODE
    //#define DialogBox  DialogBoxW
    //#else
    //#define DialogBox  DialogBoxA
    //#endif // !UNICODE
    //
    //#define DialogBoxIndirectA(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#define DialogBoxIndirectW(hInstance, lpTemplate, hWndParent, lpDialogFunc) \
    //
    //#ifdef UNICODE
    //#define DialogBoxIndirect  DialogBoxIndirectW
    //#else
    //#define DialogBoxIndirect  DialogBoxIndirectA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define SetDlgItemText  SetDlgItemTextW
    //#else
    //#define SetDlgItemText  SetDlgItemTextA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define GetDlgItemText  GetDlgItemTextW
    //#else
    //#define GetDlgItemText  GetDlgItemTextA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define SendDlgItemMessage  SendDlgItemMessageW
    //#else
    //#define SendDlgItemMessage  SendDlgItemMessageA
    //#endif // !UNICODE
    //
    //
    ////#ifndef _MAC
    ////
    ////
    ////#else
    ////
    ////
    ////#endif
    ////
    ////
    ////#ifndef _MAC
    ////
    ////
    ////#else
    ////
    ////
    ////#endif
    //
    //
    //#ifdef UNICODE
    //#define DefDlgProc  DefDlgProcW
    //#else
    //#define DefDlgProc  DefDlgProcA
    //#endif // !UNICODE

    /*
    * Window extra byted needed for private dialog classes.
    */
    //#ifndef _MAC
    #define DLGWINDOWEXTRA 30
    //#else
    //#define DLGWINDOWEXTRA 48
    //#endif

  #endif // !NOCTLMGR

  #ifndef NOMSG

    //#ifdef UNICODE
    //#define CallMsgFilter  CallMsgFilterW
    //#else
    //#define CallMsgFilter  CallMsgFilterA
    //#endif // !UNICODE

  #endif // !NOMSG

  #ifndef NOCLIPBOARD

    /*
    * Clipboard Manager Functions
    */


    //#if(WINVER >=   1280)
    //
    //
    //#endif // WINVER >= 0x0500


    //#ifdef UNICODE
    //#define RegisterClipboardFormat  RegisterClipboardFormatW
    //#else
    //#define RegisterClipboardFormat  RegisterClipboardFormatA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define GetClipboardFormatName  GetClipboardFormatNameW
    //#else
    //#define GetClipboardFormatName  GetClipboardFormatNameA
    //#endif // !UNICODE

  #endif // !NOCLIPBOARD

  /*
  * Character Translation Routines
  */


  //#ifdef UNICODE
  //#define CharToOem  CharToOemW
  //#else
  //#define CharToOem  CharToOemA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define OemToChar  OemToCharW
  //#else
  //#define OemToChar  OemToCharA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharToOemBuff  CharToOemBuffW
  //#else
  //#define CharToOemBuff  CharToOemBuffA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define OemToCharBuff  OemToCharBuffW
  //#else
  //#define OemToCharBuff  OemToCharBuffA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharUpper  CharUpperW
  //#else
  //#define CharUpper  CharUpperA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharUpperBuff  CharUpperBuffW
  //#else
  //#define CharUpperBuff  CharUpperBuffA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharLower  CharLowerW
  //#else
  //#define CharLower  CharLowerA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharLowerBuff  CharLowerBuffW
  //#else
  //#define CharLowerBuff  CharLowerBuffA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharNext  CharNextW
  //#else
  //#define CharNext  CharNextA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CharPrev  CharPrevW
  //#else
  //#define CharPrev  CharPrevA
  //#endif // !UNICODE

  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400

  /*
  * Compatibility defines for character translation routines
  */
  #define AnsiToOem CharToOemA
  #define OemToAnsi OemToCharA
  #define AnsiToOemBuff CharToOemBuffA
  #define OemToAnsiBuff OemToCharBuffA
  #define AnsiUpper CharUpperA
  #define AnsiUpperBuff CharUpperBuffA
  #define AnsiLower CharLowerA
  #define AnsiLowerBuff CharLowerBuffA
  #define AnsiNext CharNextA
  #define AnsiPrev CharPrevA

  #ifndef  NOLANGUAGE

    /*
    * Language dependent Routines
    */


    //#ifdef UNICODE
    //#define IsCharAlpha  IsCharAlphaW
    //#else
    //#define IsCharAlpha  IsCharAlphaA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define IsCharAlphaNumeric  IsCharAlphaNumericW
    //#else
    //#define IsCharAlphaNumeric  IsCharAlphaNumericA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define IsCharUpper  IsCharUpperW
    //#else
    //#define IsCharUpper  IsCharUpperA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define IsCharLower  IsCharLowerW
    //#else
    //#define IsCharLower  IsCharLowerA
    //#endif // !UNICODE
    //
  #endif  // !NOLANGUAGE

  //
  //
  //#ifdef UNICODE
  //#define GetKeyNameText  GetKeyNameTextW
  //#else
  //#define GetKeyNameText  GetKeyNameTextA
  //#endif // !UNICODE
  //
  //
  ////#if(WINVER >=   1024)
  ////
  ////
  ////#endif // WINVER >= 0x0400
  //
  //
  //#ifdef UNICODE
  //#define VkKeyScan  VkKeyScanW
  //#else
  //#define VkKeyScan  VkKeyScanA
  //#endif // !UNICODE
  //
  ////#if(WINVER >=   1024)
  //
  //
  //#ifdef UNICODE
  //#define VkKeyScanEx  VkKeyScanExW
  //#else
  //#define VkKeyScanEx  VkKeyScanExA
  //#endif // !UNICODE
  //#endif // WINVER >= 0x0400
  #define KEYEVENTF_EXTENDEDKEY      1
  #define KEYEVENTF_KEYUP            2
  //#if(_WIN32_WINNT >=   1280)
  #define KEYEVENTF_UNICODE          4
  #define KEYEVENTF_SCANCODE         8
  //#endif // _WIN32_WINNT >= 0x0500


  #define MOUSEEVENTF_MOVE             1 // mouse move
  #define MOUSEEVENTF_LEFTDOWN         2 // left button down
  #define MOUSEEVENTF_LEFTUP           4 // left button up
  #define MOUSEEVENTF_RIGHTDOWN        8 // right button down
  #define MOUSEEVENTF_RIGHTUP         16 // right button up
  #define MOUSEEVENTF_MIDDLEDOWN      32 // middle button down
  #define MOUSEEVENTF_MIDDLEUP        64 // middle button up
  #define MOUSEEVENTF_XDOWN          128 // x button down
  #define MOUSEEVENTF_XUP            256 // x button down
  #define MOUSEEVENTF_WHEEL         2048 // wheel button rolled
  #define MOUSEEVENTF_VIRTUALDESK  16384 // map to entire virtual desktop
  #define MOUSEEVENTF_ABSOLUTE     32768 // absolute move


  //#if (_WIN32_WINNT >   1024)


  #define INPUT_MOUSE     0
  #define INPUT_KEYBOARD  1
  #define INPUT_HARDWARE  2


          // number of input in the array
          // array of inputs
          // sizeof(INPUT)

  //#endif // (_WIN32_WINNT > 0x0400)

  //#if(_WIN32_WINNT >=   1280)
  //
  //
  //#endif // _WIN32_WINNT >= 0x0500


  //#ifdef UNICODE
  //#define MapVirtualKey  MapVirtualKeyW
  //#else
  //#define MapVirtualKey  MapVirtualKeyA
  //#endif // !UNICODE
  //
  ////#if(WINVER >=   1024)
  //
  //
  //#ifdef UNICODE
  //#define MapVirtualKeyEx  MapVirtualKeyExW
  //#else
  //#define MapVirtualKeyEx  MapVirtualKeyExA
  //#endif // !UNICODE
  //#endif // WINVER >= 0x0400


  #define MWMO_WAITALL             1
  #define MWMO_ALERTABLE           2
  #define MWMO_INPUTAVAILABLE      4

  /*
  * Queue status flags for GetQueueStatus() and MsgWaitForMultipleObjects()
  */
  #define QS_KEY                   1
  #define QS_MOUSEMOVE             2
  #define QS_MOUSEBUTTON           4
  #define QS_POSTMESSAGE           8
  #define QS_TIMER                16
  #define QS_PAINT                32
  #define QS_SENDMESSAGE          64
  #define QS_HOTKEY              128
  #define QS_ALLPOSTMESSAGE      256

  #define QS_MOUSE           (QS_MOUSEMOVE     + \


  #define QS_INPUT           (QS_MOUSE         + \


  #define QS_ALLEVENTS       (QS_INPUT         + \


  #define QS_ALLINPUT        (QS_INPUT         + \


  /*
  * Windows Functions
  */


  //#ifdef UNICODE
  //#define LoadAccelerators  LoadAcceleratorsW
  //#else
  //#define LoadAccelerators  LoadAcceleratorsA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CreateAcceleratorTable  CreateAcceleratorTableW
  //#else
  //#define CreateAcceleratorTable  CreateAcceleratorTableA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define CopyAcceleratorTable  CopyAcceleratorTableW
  //#else
  //#define CopyAcceleratorTable  CopyAcceleratorTableA
  //#endif // !UNICODE

  #ifndef NOMSG

    //#ifdef UNICODE
    //#define TranslateAccelerator  TranslateAcceleratorW
    //#else
    //#define TranslateAccelerator  TranslateAcceleratorA
    //#endif // !UNICODE

  #endif // !NOMSG

  #ifndef NOSYSMETRICS

    /*
    * GetSystemMetrics() codes
    */

    #define SM_CXSCREEN             0
    #define SM_CYSCREEN             1
    #define SM_CXVSCROLL            2
    #define SM_CYHSCROLL            3
    #define SM_CYCAPTION            4
    #define SM_CXBORDER             5
    #define SM_CYBORDER             6
    #define SM_CXDLGFRAME           7
    #define SM_CYDLGFRAME           8
    #define SM_CYVTHUMB             9
    #define SM_CXHTHUMB             10
    #define SM_CXICON               11
    #define SM_CYICON               12
    #define SM_CXCURSOR             13
    #define SM_CYCURSOR             14
    #define SM_CYMENU               15
    #define SM_CXFULLSCREEN         16
    #define SM_CYFULLSCREEN         17
    #define SM_CYKANJIWINDOW        18
    #define SM_MOUSEPRESENT         19
    #define SM_CYVSCROLL            20
    #define SM_CXHSCROLL            21
    #define SM_DEBUG                22
    #define SM_SWAPBUTTON           23
    #define SM_RESERVED1            24
    #define SM_RESERVED2            25
    #define SM_RESERVED3            26
    #define SM_RESERVED4            27
    #define SM_CXMIN                28
    #define SM_CYMIN                29
    #define SM_CXSIZE               30
    #define SM_CYSIZE               31
    #define SM_CXFRAME              32
    #define SM_CYFRAME              33
    #define SM_CXMINTRACK           34
    #define SM_CYMINTRACK           35
    #define SM_CXDOUBLECLK          36
    #define SM_CYDOUBLECLK          37
    #define SM_CXICONSPACING        38
    #define SM_CYICONSPACING        39
    #define SM_MENUDROPALIGNMENT    40
    #define SM_PENWINDOWS           41
    #define SM_DBCSENABLED          42
    #define SM_CMOUSEBUTTONS        43

    //#if(WINVER >=   1024)
    #define SM_CXFIXEDFRAME           SM_CXDLGFRAME  // ;win40 name change
    #define SM_CYFIXEDFRAME           SM_CYDLGFRAME  // ;win40 name change
    #define SM_CXSIZEFRAME            SM_CXFRAME     // ;win40 name change
    #define SM_CYSIZEFRAME            SM_CYFRAME     // ;win40 name change

    #define SM_SECURE               44
    #define SM_CXEDGE               45
    #define SM_CYEDGE               46
    #define SM_CXMINSPACING         47
    #define SM_CYMINSPACING         48
    #define SM_CXSMICON             49
    #define SM_CYSMICON             50
    #define SM_CYSMCAPTION          51
    #define SM_CXSMSIZE             52
    #define SM_CYSMSIZE             53
    #define SM_CXMENUSIZE           54
    #define SM_CYMENUSIZE           55
    #define SM_ARRANGE              56
    #define SM_CXMINIMIZED          57
    #define SM_CYMINIMIZED          58
    #define SM_CXMAXTRACK           59
    #define SM_CYMAXTRACK           60
    #define SM_CXMAXIMIZED          61
    #define SM_CYMAXIMIZED          62
    #define SM_NETWORK              63
    #define SM_CLEANBOOT            67
    #define SM_CXDRAG               68
    #define SM_CYDRAG               69
    //#endif // WINVER >= 0x0400
    #define SM_SHOWSOUNDS           70
    //#if(WINVER >=   1024)
    #define SM_CXMENUCHECK          71   // Use instead of GetMenuCheckMarkDimensions()!
    #define SM_CYMENUCHECK          72
    #define SM_SLOWMACHINE          73
    #define SM_MIDEASTENABLED       74
    //#endif // WINVER >= 0x0400

    //#if (WINVER >=   1280) .OR. (_WIN32_WINNT >=   1024)
    #define SM_MOUSEWHEELPRESENT    75
    //#endif
    //#if(WINVER >=   1280)
    #define SM_XVIRTUALSCREEN       76
    #define SM_YVIRTUALSCREEN       77
    #define SM_CXVIRTUALSCREEN      78
    #define SM_CYVIRTUALSCREEN      79
    #define SM_CMONITORS            80
    #define SM_SAMEDISPLAYFORMAT    81
    //#endif // WINVER >= 0x0500
    //#if(_WIN32_WINNT >=   1280)
    #define SM_IMMENABLED           82
    //#endif // _WIN32_WINNT >= 0x0500
    //
    //#if (WINVER <   1280) .AND. (!defined(_WIN32_WINNT) .OR. (_WIN32_WINNT <   1024))
    //#define SM_CMETRICS             76
    //#else
    #define SM_CMETRICS             83
    //#endif

    //#if(WINVER >=   1280)
    #define SM_REMOTESESSION          4096
    //#endif // WINVER >= 0x0500

  #endif // !NOSYSMETRICS

  #ifndef NOMENUS

    //#ifdef UNICODE
    //#define LoadMenu  LoadMenuW
    //#else
    //#define LoadMenu  LoadMenuA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define LoadMenuIndirect  LoadMenuIndirectW
    //#else
    //#define LoadMenuIndirect  LoadMenuIndirectA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define ChangeMenu  ChangeMenuW
    //#else
    //#define ChangeMenu  ChangeMenuA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define GetMenuString  GetMenuStringW
    //#else
    //#define GetMenuString  GetMenuStringA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define InsertMenu  InsertMenuW
    //#else
    //#define InsertMenu  InsertMenuA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define AppendMenu  AppendMenuW
    //#else
    //#define AppendMenu  AppendMenuA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define ModifyMenu  ModifyMenuW
    //#else
    //#define ModifyMenu  ModifyMenuA
    //#endif // !UNICODE


    //#if(WINVER >=   1024)
    // return codes for WM_MENUCHAR
    #define MNC_IGNORE  0
    #define MNC_CLOSE   1
    #define MNC_EXECUTE 2
    #define MNC_SELECT  3


            // Size of structure
         // Screen coordinates of rectangle to exclude when positioning


    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1280)
    //
    #define MNS_NOCHECK         2147483648
    #define MNS_MODELESS        1073741824
    #define MNS_DRAGDROP         536870912
    #define MNS_AUTODISMISS      268435456
    #define MNS_NOTIFYBYPOS      134217728
    #define MNS_CHECKORBMP        67108864

    #define MIM_MAXHEIGHT                        1
    #define MIM_BACKGROUND                       2
    #define MIM_HELPID                           4
    #define MIM_MENUDATA                         8
    #define MIM_STYLE                           16
    #define MIM_APPLYTOSUBMENUS         2147483648


    /*
    * WM_MENUDRAG return values.
    */
    #define MND_CONTINUE       0
    #define MND_ENDMENU        1


    /*
    * MENUGETOBJECTINFO dwFlags values
    */
    #define MNGOF_TOPGAP                  1
    #define MNGOF_BOTTOMGAP               2

    /*
    * WM_MENUGETOBJECT return values
    */
    #define MNGO_NOINTERFACE              0
    #define MNGO_NOERROR                  1
    //#endif // WINVER >= 0x0500

    //#if(WINVER >=   1024)
    #define MIIM_STATE                1
    #define MIIM_ID                   2
    #define MIIM_SUBMENU              4
    #define MIIM_CHECKMARKS           8
    #define MIIM_TYPE                16
    #define MIIM_DATA                32
    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1280)
    #define MIIM_STRING              64
    #define MIIM_BITMAP             128
    #define MIIM_FTYPE              256

    #define HBMMENU_CALLBACK            ((HBITMAP) -1)
    #define HBMMENU_SYSTEM              ((HBITMAP)  1)
    #define HBMMENU_MBAR_RESTORE        ((HBITMAP)  2)
    #define HBMMENU_MBAR_MINIMIZE       ((HBITMAP)  3)
    #define HBMMENU_MBAR_CLOSE          ((HBITMAP)  5)
    #define HBMMENU_MBAR_CLOSE_D        ((HBITMAP)  6)
    #define HBMMENU_MBAR_MINIMIZE_D     ((HBITMAP)  7)
    #define HBMMENU_POPUP_CLOSE         ((HBITMAP)  8)
    #define HBMMENU_POPUP_RESTORE       ((HBITMAP)  9)
    #define HBMMENU_POPUP_MAXIMIZE      ((HBITMAP) 10)
    #define HBMMENU_POPUP_MINIMIZE      ((HBITMAP) 11)
    //#endif // WINVER >= 0x0500

    //#if(WINVER >=   1024)


                // used if MIIM_TYPE (4.0) or MIIM_FTYPE (>4.0)
               // used if MIIM_STATE
                  // used if MIIM_ID
             // used if MIIM_SUBMENU
          // used if MIIM_CHECKMARKS
        // used if MIIM_CHECKMARKS
          // used if MIIM_DATA
           // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
                  // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    //#if(WINVER >=   1280)
    //          // used if MIIM_BITMAP
    //#endif // WINVER >= 0x0500
    //
    //
    //             // used if MIIM_TYPE (4.0) or MIIM_FTYPE (>4.0)
    //            // used if MIIM_STATE
    //               // used if MIIM_ID
    //          // used if MIIM_SUBMENU
    //       // used if MIIM_CHECKMARKS
    //     // used if MIIM_CHECKMARKS
    //       // used if MIIM_DATA
    //        // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    //               // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    //#if(WINVER >=   1280)
    //          // used if MIIM_BITMAP
    //#endif // WINVER >= 0x0500
    //
    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE
    //
    //
    //#ifdef UNICODE
    //
    //#else
    //
    //#endif // UNICODE


    //#ifdef UNICODE
    //#define InsertMenuItem  InsertMenuItemW
    //#else
    //#define InsertMenuItem  InsertMenuItemA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define GetMenuItemInfo  GetMenuItemInfoW
    //#else
    //#define GetMenuItemInfo  GetMenuItemInfoA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define SetMenuItemInfo  SetMenuItemInfoW
    //#else
    //#define SetMenuItemInfo  SetMenuItemInfoA
    //#endif // !UNICODE


    #define GMDI_USEDISABLED          1
    #define GMDI_GOINTOPOPUPS         2


    //#endif // WINVER >= 0x0400

    /*
    * Flags for TrackPopupMenu
    #define TPM_LEFTBUTTON  0x0000L
    #define TPM_RIGHTBUTTON 0x0002L
    #define TPM_LEFTALIGN   0x0000L
    #define TPM_CENTERALIGN 0x0004L
    #define TPM_RIGHTALIGN  0x0008L

    #define TPM_TOPALIGN        0x0000L
    #define TPM_VCENTERALIGN    0x0010L
    #define TPM_BOTTOMALIGN     0x0020L

    #define TPM_HORIZONTAL      0x0000L
    #define TPM_VERTICAL        0x0040L
    #define TPM_NONOTIFY        0x0080L
    #define TPM_RETURNCMD       0x0100L

    #define TPM_RECURSE         0x0001L
    #define TPM_HORPOSANIMATION 0x0400L
    #define TPM_HORNEGANIMATION 0x0800L
    #define TPM_VERPOSANIMATION 0x1000L
    #define TPM_VERNEGANIMATION 0x2000L

    #define TPM_NOANIMATION     0x4000L
    */



    /*
    * Flags for TrackPopupMenu
    */
    #define TPM_LEFTBUTTON        0
    #define TPM_RIGHTBUTTON       2
    #define TPM_LEFTALIGN         0
    #define TPM_CENTERALIGN       4
    #define TPM_RIGHTALIGN        8

    #define TPM_TOPALIGN              0
    #define TPM_VCENTERALIGN         16
    #define TPM_BOTTOMALIGN          32

    #define TPM_HORIZONTAL            0     // Horz alignment matters more
    #define TPM_VERTICAL             64     // Vert alignment matters more
    #define TPM_NONOTIFY            128     // Don't send any notification msgs
    #define TPM_RETURNCMD           256

    #define TPM_RECURSE               1
    #define TPM_HORPOSANIMATION    1024
    #define TPM_HORNEGANIMATION    2048
    #define TPM_VERPOSANIMATION    4096
    #define TPM_VERNEGANIMATION    8192

    #define TPM_NOANIMATION       16384

  #endif // !NOMENUS


  //#if(WINVER >=   1024)
  //
  // Drag-and-drop support
  // Obsolete - use OLE instead
  //


  #define DOF_EXECUTABLE       32769      // wFmt flags
  #define DOF_DOCUMENT         32770
  #define DOF_DIRECTORY        32771
  #define DOF_MULTIPLE         32772
  #define DOF_PROGMAN              1
  #define DOF_SHELLDATA            2

  #define DO_DROPFILE          1162627398
  #define DO_PRINTFILE         1414419024


  //#endif // WINVER >= 0x0400


  #ifndef NODRAWTEXT

    /*
    * DrawText() Format Flags
    */
    #define DT_TOP                               0
    #define DT_LEFT                              0
    #define DT_CENTER                            1
    #define DT_RIGHT                             2
    #define DT_VCENTER                           4
    #define DT_BOTTOM                            8
    #define DT_WORDBREAK                        16
    #define DT_SINGLELINE                       32
    #define DT_EXPANDTABS                       64
    #define DT_TABSTOP                         128
    #define DT_NOCLIP                          256
    #define DT_EXTERNALLEADING                 512
    #define DT_CALCRECT                       1024
    #define DT_NOPREFIX                       2048
    #define DT_INTERNAL                       4096

    //#if(WINVER >=   1024)
    #define DT_EDITCONTROL                    8192
    #define DT_PATH_ELLIPSIS                 16384
    #define DT_END_ELLIPSIS                  32768
    #define DT_MODIFYSTRING                  65536
    #define DT_RTLREADING                   131072
    #define DT_WORD_ELLIPSIS                262144
    //#if(WINVER >=   1280)
    #define DT_NOFULLWIDTHCHARBREAK         524288
    //#if(_WIN32_WINNT >=   1280)
    #define DT_HIDEPREFIX                  1048576
    #define DT_PREFIXONLY                  2097152
    //#endif // _WIN32_WINNT >= 0x0500
    //#endif // WINVER >= 0x0500


    //#endif // WINVER >= 0x0400


    //#ifdef UNICODE
    //#define DrawText  DrawTextW
    //#else
    //#define DrawText  DrawTextA
    //#endif // !UNICODE


    //#if(WINVER >=   1024)


    //#ifdef UNICODE
    //#define DrawTextEx  DrawTextExW
    //#else
    //#define DrawTextEx  DrawTextExA
    //#endif // !UNICODE
    //#endif // WINVER >= 0x0400

  #endif // !NODRAWTEXT


  //#ifdef UNICODE
  //#define GrayString  GrayStringW
  //#else
  //#define GrayString  GrayStringA
  //#endif // !UNICODE

  //#if(WINVER >=   1024)
  // Monolithic state-drawing routine
  // Image type
  #define DST_COMPLEX          0
  #define DST_TEXT             1
  #define DST_PREFIXTEXT       2
  #define DST_ICON             3
  #define DST_BITMAP           4

  // State type
  #define DSS_NORMAL           0
  #define DSS_UNION           16  // Gray string appearance
  #define DSS_DISABLED        32
  #define DSS_MONO           128
  //#if(_WIN32_WINNT >=   1280)
  #define DSS_HIDEPREFIX     512
  #define DSS_PREFIXONLY    1024
  //#endif // _WIN32_WINNT >= 0x0500
  #define DSS_RIGHT        32768


  //#ifdef UNICODE
  //#define DrawState  DrawStateW
  //#else
  //#define DrawState  DrawStateA
  //#endif // !UNICODE
  //#endif // WINVER >= 0x0400


  //#ifdef UNICODE
  //#define TabbedTextOut  TabbedTextOutW
  //#else
  //#define TabbedTextOut  TabbedTextOutA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetTabbedTextExtent  GetTabbedTextExtentW
  //#else
  //#define GetTabbedTextExtent  GetTabbedTextExtentA
  //#endif // !UNICODE


  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400
  //
  //
  //#if(_WIN32_WINNT >=   1280)
  //
  //
  #define ASFW_ANY    ((DWORD)-1)
  //
  //
  #define LSFW_LOCK       1
  #define LSFW_UNLOCK     2
  //
  //#endif // _WIN32_WINNT >= 0x0500


  /*
  * GetDCEx() flags
  */
  #define DCX_WINDOW                     1
  #define DCX_CACHE                      2
  #define DCX_NORESETATTRS               4
  #define DCX_CLIPCHILDREN               8
  #define DCX_CLIPSIBLINGS              16
  #define DCX_PARENTCLIP                32
  #define DCX_EXCLUDERGN                64
  #define DCX_INTERSECTRGN             128
  #define DCX_EXCLUDEUPDATE            256
  #define DCX_INTERSECTUPDATE          512
  #define DCX_LOCKWINDOWUPDATE        1024

  #define DCX_VALIDATE             2097152


  /*
  * RedrawWindow() flags
  */
  #define RDW_INVALIDATE               1
  #define RDW_INTERNALPAINT            2
  #define RDW_ERASE                    4

  #define RDW_VALIDATE                 8
  #define RDW_NOINTERNALPAINT         16
  #define RDW_NOERASE                 32

  #define RDW_NOCHILDREN              64
  #define RDW_ALLCHILDREN            128

  #define RDW_UPDATENOW              256
  #define RDW_ERASENOW               512

  #define RDW_FRAME                 1024
  #define RDW_NOFRAME               2048


  /*
  * LockWindowUpdate API
  */


  #define SW_SCROLLCHILDREN        1  // Scroll children within *lprcScroll.
  #define SW_INVALIDATE            2  // Invalidate after scrolling
  #define SW_ERASE                 4  // If SW_INVALIDATE, don't send WM_ERASEBACKGROUND
  //#if(WINVER >=   1280)
  #define SW_SMOOTHSCROLL         16  // Use smooth scrolling
  //#endif // WINVER >= 0x0500

  #ifndef NOSCROLL

    /*
    * EnableScrollBar() flags
    */
    #define ESB_ENABLE_BOTH          0
    #define ESB_DISABLE_BOTH         3

    #define ESB_DISABLE_LEFT         1
    #define ESB_DISABLE_RIGHT        2

    #define ESB_DISABLE_UP           1
    #define ESB_DISABLE_DOWN         2

    #define ESB_DISABLE_LTUP    ESB_DISABLE_LEFT
    #define ESB_DISABLE_RTDN    ESB_DISABLE_RIGHT

  #endif  // !NOSCROLL


  //#ifdef UNICODE
  //#define SetProp  SetPropW
  //#else
  //#define SetProp  SetPropA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetProp  GetPropW
  //#else
  //#define GetProp  GetPropA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define RemoveProp  RemovePropW
  //#else
  //#define RemoveProp  RemovePropA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define EnumPropsEx  EnumPropsExW
  //#else
  //#define EnumPropsEx  EnumPropsExA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define EnumProps  EnumPropsW
  //#else
  //#define EnumProps  EnumPropsA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define SetWindowText  SetWindowTextW
  //#else
  //#define SetWindowText  SetWindowTextA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetWindowText  GetWindowTextW
  //#else
  //#define GetWindowText  GetWindowTextA
  //#endif // !UNICODE


  //#ifdef UNICODE
  //#define GetWindowTextLength  GetWindowTextLengthW
  //#else
  //#define GetWindowTextLength  GetWindowTextLengthA
  //#endif // !UNICODE


  //#if(WINVER >=   1024)
  #define HELPINFO_WINDOW         1
  #define HELPINFO_MENUITEM       2
       // Structure pointed to by lParam of WM_HELP

                  // Size in bytes of this struct
            // Either HELPINFO_WINDOW or HELPINFO_MENUITEM
                 // Control Id or a Menu item Id.
             // hWnd of control or hMenu.
           // Context Id associated with this item
                // Mouse Position in screen co-ordinates


  //#endif // WINVER >= 0x0400


  #ifndef NOMB

    /*
    * MessageBox() Flags
    */
    #define MB_OK                                 0
    #define MB_OKCANCEL                           1
    #define MB_ABORTRETRYIGNORE                   2
    #define MB_YESNOCANCEL                        3
    #define MB_YESNO                              4
    #define MB_RETRYCANCEL                        5
    //#if(WINVER >=   1280)
    #define MB_CANCELTRYCONTINUE                  6
    //#endif // WINVER >= 0x0500


    #define MB_ICONHAND                          16
    #define MB_ICONQUESTION                      32
    #define MB_ICONEXCLAMATION                   48
    #define MB_ICONASTERISK                      64

    //#if(WINVER >=   1024)
    #define MB_USERICON                         128
    #define MB_ICONWARNING              MB_ICONEXCLAMATION
    #define MB_ICONERROR                MB_ICONHAND
    //#endif // WINVER >= 0x0400

    #define MB_ICONINFORMATION          MB_ICONASTERISK
    #define MB_ICONSTOP                 MB_ICONHAND

    #define MB_DEFBUTTON1                         0
    #define MB_DEFBUTTON2                       256
    #define MB_DEFBUTTON3                       512
    //#if(WINVER >=   1024)
    #define MB_DEFBUTTON4                       768
    //#endif // WINVER >= 0x0400

    #define MB_APPLMODAL                          0
    #define MB_SYSTEMMODAL                     4096
    #define MB_TASKMODAL                       8192
    //#if(WINVER >=   1024)
    #define MB_HELP                           16384 // Help Button
    //#endif // WINVER >= 0x0400

    #define MB_NOFOCUS                        32768
    #define MB_SETFOREGROUND                  65536
    #define MB_DEFAULT_DESKTOP_ONLY          131072

    //#if(WINVER >=   1024)
    #define MB_TOPMOST                       262144
    #define MB_RIGHT                         524288
    #define MB_RTLREADING                   1048576


    //#endif // WINVER >= 0x0400

    //#ifdef _WIN32_WINNT
    //#if (_WIN32_WINNT >=   1024)
    //#define MB_SERVICE_NOTIFICATION              2097152
    //#else
    //#define MB_SERVICE_NOTIFICATION               262144
    //#endif
    #define MB_SERVICE_NOTIFICATION_NT3X          262144
    //#endif

    #define MB_TYPEMASK                          15
    #define MB_ICONMASK                         240
    #define MB_DEFMASK                         3840
    #define MB_MODEMASK                       12288
    #define MB_MISCMASK                       49152


    //#ifdef UNICODE
    //#define MessageBox  MessageBoxW
    //#else
    //#define MessageBox  MessageBoxA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define MessageBoxEx  MessageBoxExW
    //#else
    //#define MessageBoxEx  MessageBoxExA
    //#endif // !UNICODE

    //#if(WINVER >=   1024)
    //
    //
    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE


    //#ifdef UNICODE
    //#define MessageBoxIndirect  MessageBoxIndirectW
    //#else
    //#define MessageBoxIndirect  MessageBoxIndirectA
    //#endif // !UNICODE
    //#endif // WINVER >= 0x0400

  #endif // !NOMB


  //#if(WINVER >=   1024)
  #define CWP_ALL                  0
  #define CWP_SKIPINVISIBLE        1
  #define CWP_SKIPDISABLED         2
  #define CWP_SKIPTRANSPARENT      4


  //#endif // WINVER >= 0x0400

  #ifndef NOCOLOR

    /*
    * Color Types
    */
    #define CTLCOLOR_MSGBOX         0
    #define CTLCOLOR_EDIT           1
    #define CTLCOLOR_LISTBOX        2
    #define CTLCOLOR_BTN            3
    #define CTLCOLOR_DLG            4
    #define CTLCOLOR_SCROLLBAR      5
    #define CTLCOLOR_STATIC         6
    #define CTLCOLOR_MAX            7

    #define COLOR_SCROLLBAR         0
    #define COLOR_BACKGROUND        1
    #define COLOR_ACTIVECAPTION     2
    #define COLOR_INACTIVECAPTION   3
    #define COLOR_MENU              4
    #define COLOR_WINDOW            5
    #define COLOR_WINDOWFRAME       6
    #define COLOR_MENUTEXT          7
    #define COLOR_WINDOWTEXT        8
    #define COLOR_CAPTIONTEXT       9
    #define COLOR_ACTIVEBORDER      10
    #define COLOR_INACTIVEBORDER    11
    #define COLOR_APPWORKSPACE      12
    #define COLOR_HIGHLIGHT         13
    #define COLOR_HIGHLIGHTTEXT     14
    #define COLOR_BTNFACE           15
    #define COLOR_BTNSHADOW         16
    #define COLOR_GRAYTEXT          17
    #define COLOR_BTNTEXT           18
    #define COLOR_INACTIVECAPTIONTEXT 19
    #define COLOR_BTNHIGHLIGHT      20

    //#if(WINVER >=   1024)
    #define COLOR_3DDKSHADOW        21
    #define COLOR_3DLIGHT           22
    #define COLOR_INFOTEXT          23
    #define COLOR_INFOBK            24
    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1280)
    #define COLOR_HOTLIGHT          26
    #define COLOR_GRADIENTACTIVECAPTION 27
    #define COLOR_GRADIENTINACTIVECAPTION 28
    //#endif // WINVER >= 0x0500

    //#if(WINVER >=   1024)
    #define COLOR_DESKTOP           COLOR_BACKGROUND
    #define COLOR_3DFACE            COLOR_BTNFACE
    #define COLOR_3DSHADOW          COLOR_BTNSHADOW
    #define COLOR_3DHIGHLIGHT       COLOR_BTNHIGHLIGHT
    #define COLOR_3DHILIGHT         COLOR_BTNHIGHLIGHT
    #define COLOR_BTNHILIGHT        COLOR_BTNHIGHLIGHT
    //#endif // WINVER >= 0x0400


    //#if(WINVER >=   1024)
    //
    //
    //#endif // WINVER >= 0x0400

  #endif // !NOCOLOR


  #ifndef NOWINOFFSETS

    //#ifdef UNICODE
    //#define GetWindowLong  GetWindowLongW
    //#else
    //#define GetWindowLong  GetWindowLongA
    //#endif // !UNICODE


    //#ifdef UNICODE
    //#define SetWindowLong  SetWindowLongW
    //#else
    //#define SetWindowLong  SetWindowLongA
    //#endif // !UNICODE

    //#ifdef _WIN64
    //
    //
    //#ifdef UNICODE
    //#define GetWindowLongPtr  GetWindowLongPtrW
    //#else
    //#define GetWindowLongPtr  GetWindowLongPtrA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define SetWindowLongPtr  SetWindowLongPtrW
    //#else
    //#define SetWindowLongPtr  SetWindowLongPtrA
    //#endif // !UNICODE
    //
    //#else  // _WIN64

    //#define GetWindowLongPtrA   GetWindowLongA
    //#define GetWindowLongPtrW   GetWindowLongW
    //#ifdef UNICODE
    //#define GetWindowLongPtr  GetWindowLongPtrW
    //#else
    //#define GetWindowLongPtr  GetWindowLongPtrA
    //#endif // !UNICODE

    //#define SetWindowLongPtrA   SetWindowLongA
    //#define SetWindowLongPtrW   SetWindowLongW
    //#ifdef UNICODE
    //#define SetWindowLongPtr  SetWindowLongPtrW
    //#else
    //#define SetWindowLongPtr  SetWindowLongPtrA
    //#endif // !UNICODE

    //#endif // _WIN64


    //#ifdef UNICODE
    //#define GetClassLong  GetClassLongW
    //#else
    //#define GetClassLong  GetClassLongA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define SetClassLong  SetClassLongW
    //#else
    //#define SetClassLong  SetClassLongA
    //#endif // !UNICODE

    //#ifdef _WIN64
    //
    //
    //#ifdef UNICODE
    //#define GetClassLongPtr  GetClassLongPtrW
    //#else
    //#define GetClassLongPtr  GetClassLongPtrA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define SetClassLongPtr  SetClassLongPtrW
    //#else
    //#define SetClassLongPtr  SetClassLongPtrA
    //#endif // !UNICODE
    //
    //#else  // _WIN64

    //#define GetClassLongPtrA    GetClassLongA
    //#define GetClassLongPtrW    GetClassLongW
    //#ifdef UNICODE
    //#define GetClassLongPtr  GetClassLongPtrW
    //#else
    //#define GetClassLongPtr  GetClassLongPtrA
    //#endif // !UNICODE
    //
    //#define SetClassLongPtrA    SetClassLongA
    //#define SetClassLongPtrW    SetClassLongW
    //#ifdef UNICODE
    //#define SetClassLongPtr  SetClassLongPtrW
    //#else
    //#define SetClassLongPtr  SetClassLongPtrA
    //#endif // !UNICODE

    //#endif // _WIN64

  #endif // !NOWINOFFSETS

  //#if(WINVER >=   1280)
  //
  //
  //#endif // WINVER >= 0x0500


  //#ifdef UNICODE
  //#define FindWindow  FindWindowW
  //#else
  //#define FindWindow  FindWindowA
  //#endif // !UNICODE

  //#if(WINVER >=   1024)


  //#ifdef UNICODE
  //#define FindWindowEx  FindWindowExW
  //#else
  //#define FindWindowEx  FindWindowExA
  //#endif // !UNICODE

  //#endif // WINVER >= 0x0400


  #define EnumTaskWindows(hTask, lpfn, lParam) EnumThreadWindows(HandleToUlong(hTask), lpfn, lParam)


  //#ifdef UNICODE
  //#define GetClassName  GetClassNameW
  //#else
  //#define GetClassName  GetClassNameA
  //#endif // !UNICODE


  #define GetNextWindow(hWnd, wCmd) GetWindow(hWnd, wCmd)
  #define GetSysModalWindow() (NULL)
  #define SetSysModalWindow(hWnd) (NULL)


  #define GetWindowTask(hWnd) \


  /*
  * GetWindow() Constants
  */
  #define GW_HWNDFIRST        0
  #define GW_HWNDLAST         1
  #define GW_HWNDNEXT         2
  #define GW_HWNDPREV         3
  #define GW_OWNER            4
  #define GW_CHILD            5
  //#if(WINVER <=   1024)
  #define GW_MAX              5
  //#else
  //#define GW_ENABLEDPOPUP     6
  //#define GW_MAX              6
  //#endif


  #ifndef NOWH

    //#ifdef STRICT
    //
    //
    ////#ifdef UNICODE
    ////#define SetWindowsHook  SetWindowsHookW
    ////#else
    ////#define SetWindowsHook  SetWindowsHookA
    ////#endif // !UNICODE
    //
    //#else // !STRICT
    //
    //
    ////#ifdef UNICODE
    ////#define SetWindowsHook  SetWindowsHookW
    ////#else
    ////#define SetWindowsHook  SetWindowsHookA
    ////#endif // !UNICODE
    //
    //#endif // !STRICT


    //#ifdef UNICODE
    //#define SetWindowsHookEx  SetWindowsHookExW
    //#else
    //#define SetWindowsHookEx  SetWindowsHookExA
    //#endif // !UNICODE


    /*
    * Macros for source-level compatibility with old functions.
    */
    #ifdef STRICT
      #define DefHookProc(nCode, wParam, lParam, phhk) CallNextHookEx(*phhk, nCode, wParam, lParam)
    #else
      #define DefHookProc(nCode, wParam, lParam, phhk) CallNextHookEx((HHOOK)*phhk, nCode, wParam, lParam)
    #endif /* STRICT */

  #endif // !NOWH


  #ifndef NOMENUS

    // ;win40  -- A lot of MF_* flags have been renamed as MFT_* and MFS_* flags
    /*
    * Menu flags for Add/Check/EnableMenuItem()
    */
    #define MF_INSERT                     0
    #define MF_CHANGE                   128
    #define MF_APPEND                   256
    #define MF_DELETE                   512
    #define MF_REMOVE                  4096

    #define MF_BYCOMMAND                  0
    #define MF_BYPOSITION              1024

    #define MF_SEPARATOR               2048

    #define MF_ENABLED                    0
    #define MF_GRAYED                     1
    #define MF_DISABLED                   2

    #define MF_UNCHECKED                  0
    #define MF_CHECKED                    8
    #define MF_USECHECKBITMAPS          512

    #define MF_STRING                     0
    #define MF_BITMAP                     4
    #define MF_OWNERDRAW                256

    #define MF_POPUP                     16
    #define MF_MENUBARBREAK              32
    #define MF_MENUBREAK                 64

    #define MF_UNHILITE                   0
    #define MF_HILITE                   128

    //#if(WINVER >=   1024)
    #define MF_DEFAULT                 4096
    //#endif // WINVER >= 0x0400
    #define MF_SYSMENU                 8192
    #define MF_HELP                   16384
    //#if(WINVER >=   1024)
    #define MF_RIGHTJUSTIFY           16384
    //#endif // WINVER >= 0x0400

    #define MF_MOUSESELECT            32768
    //#if(WINVER >=   1024)
    #define MF_END                      128  // Obsolete -- only used by old RES files
    //#endif // WINVER >= 0x0400


    //#if(WINVER >=   1024)
    #define MFT_STRING          MF_STRING
    #define MFT_BITMAP          MF_BITMAP
    #define MFT_MENUBARBREAK    MF_MENUBARBREAK
    #define MFT_MENUBREAK       MF_MENUBREAK
    #define MFT_OWNERDRAW       MF_OWNERDRAW
    #define MFT_RADIOCHECK              512
    #define MFT_SEPARATOR       MF_SEPARATOR
    #define MFT_RIGHTORDER             8192
    #define MFT_RIGHTJUSTIFY    MF_RIGHTJUSTIFY

    // Menu flags for Add/Check/EnableMenuItem()
    #define MFS_GRAYED                    3
    #define MFS_DISABLED        MFS_GRAYED
    #define MFS_CHECKED         MF_CHECKED
    #define MFS_HILITE          MF_HILITE
    #define MFS_ENABLED         MF_ENABLED
    #define MFS_UNCHECKED       MF_UNCHECKED
    #define MFS_UNHILITE        MF_UNHILITE
    #define MFS_DEFAULT         MF_DEFAULT
    //#endif // WINVER >= 0x0400


    //#if(WINVER >=   1024)
    //
    //
    //#endif // WINVER >= 0x0400

    /*
    * Menu item resource format
    */


           // version 0


    //#define MF_END                     128

  #endif // !NOMENUS


  #ifndef NOSYSCOMMANDS

    /*
    * System Menu Command Values
    */
    #define SC_SIZE          61440
    #define SC_MOVE          61456
    #define SC_MINIMIZE      61472
    #define SC_MAXIMIZE      61488
    #define SC_NEXTWINDOW    61504
    #define SC_PREVWINDOW    61520
    #define SC_CLOSE         61536
    #define SC_VSCROLL       61552
    #define SC_HSCROLL       61568
    #define SC_MOUSEMENU     61584
    #define SC_KEYMENU       61696
    #define SC_ARRANGE       61712
    #define SC_RESTORE       61728
    #define SC_TASKLIST      61744
    #define SC_SCREENSAVE    61760
    #define SC_HOTKEY        61776
    //#if(WINVER >=   1024)
    #define SC_DEFAULT       61792
    #define SC_MONITORPOWER  61808
    #define SC_CONTEXTHELP   61824
    #define SC_SEPARATOR     61455
    //#endif // WINVER >= 0x0400
    /*
    * Obsolete names
    */
    #define SC_ICON         SC_MINIMIZE
    #define SC_ZOOM         SC_MAXIMIZE

  #endif // !NOSYSCOMMANDS

  /*
  * Resource Loading Routines
  */


  //#ifdef UNICODE
  //#define LoadBitmap  LoadBitmapW
  //#else
  //#define LoadBitmap  LoadBitmapA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define LoadCursor  LoadCursorW
  //#else
  //#define LoadCursor  LoadCursorA
  //#endif // !UNICODE
  //
  //
  //#ifdef UNICODE
  //#define LoadCursorFromFile  LoadCursorFromFileW
  //#else
  //#define LoadCursorFromFile  LoadCursorFromFileA
  //#endif // !UNICODE


  //#ifndef _MAC
  #define CopyCursor(pcur) ((HCURSOR)CopyIcon((HICON)(pcur)))
  //#else
  //
  //
  //#endif

  /*
  * Standard Cursor IDs
  */
  #define IDC_ARROW           32512
  #define IDC_IBEAM           32513
  #define IDC_WAIT            32514
  #define IDC_CROSS           32515
  #define IDC_UPARROW         32516
  #define IDC_SIZE            32640  // OBSOLETE: use IDC_SIZEALL
  #define IDC_ICON            32641  // OBSOLETE: use IDC_ARROW
  #define IDC_SIZENWSE        32642
  #define IDC_SIZENESW        32643
  #define IDC_SIZEWE          32644
  #define IDC_SIZENS          32645
  #define IDC_SIZEALL         32646
  #define IDC_NO              32648 //not in win3.1
  //#if(WINVER >=   1280)
  #define IDC_HAND            32649
  //#endif // WINVER >= 0x0500
  #define IDC_APPSTARTING     32650 //not in win3.1
  //#if(WINVER >=   1024)
  #define IDC_HELP            32651
  //#endif // WINVER >= 0x0400


  //#ifdef UNICODE
  //#define LoadIcon  LoadIconW
  //#else
  //#define LoadIcon  LoadIconA
  //#endif // !UNICODE


  //#if(WINVER >=   1024)
  //
  //
  //#endif // WINVER >= 0x0400
  //
  //
  //#if(WINVER >=   1024)
  //
  //
  //// Icon/Cursor header
  //
  //
  //#endif // WINVER >= 0x0400

  #define IMAGE_BITMAP        0
  #define IMAGE_ICON          1
  #define IMAGE_CURSOR        2
  //#if(WINVER >=   1024)
  #define IMAGE_ENHMETAFILE   3

  #define LR_DEFAULTCOLOR          0
  #define LR_MONOCHROME            1
  #define LR_COLOR                 2
  #define LR_COPYRETURNORG         4
  #define LR_COPYDELETEORG         8
  #define LR_LOADFROMFILE         16
  #define LR_LOADTRANSPARENT      32
  #define LR_DEFAULTSIZE          64
  #define LR_VGACOLOR            128
  #define LR_LOADMAP3DCOLORS    4096
  #define LR_CREATEDIBSECTION   8192
  #define LR_COPYFROMRESOURCE  16384
  #define LR_SHARED            32768


  //#ifdef UNICODE
  //#define LoadImage  LoadImageW
  //#else
  //#define LoadImage  LoadImageA
  //#endif // !UNICODE


  #define DI_MASK              1
  #define DI_IMAGE             2
  #define DI_NORMAL            3
  #define DI_COMPAT            4
  #define DI_DEFAULTSIZE       8


  //#endif // WINVER >= 0x0400


  //#if(WINVER >=   1024)
  #define RES_ICON    1
  #define RES_CURSOR  2
  //#endif // WINVER >= 0x0400


  #ifdef OEMRESOURCE

    /*
    * OEM Resource Ordinal Numbers
    */
    #define OBM_CLOSE           32754
    #define OBM_UPARROW         32753
    #define OBM_DNARROW         32752
    #define OBM_RGARROW         32751
    #define OBM_LFARROW         32750
    #define OBM_REDUCE          32749
    #define OBM_ZOOM            32748
    #define OBM_RESTORE         32747
    #define OBM_REDUCED         32746
    #define OBM_ZOOMD           32745
    #define OBM_RESTORED        32744
    #define OBM_UPARROWD        32743
    #define OBM_DNARROWD        32742
    #define OBM_RGARROWD        32741
    #define OBM_LFARROWD        32740
    #define OBM_MNARROW         32739
    #define OBM_COMBO           32738
    #define OBM_UPARROWI        32737
    #define OBM_DNARROWI        32736
    #define OBM_RGARROWI        32735
    #define OBM_LFARROWI        32734

    #define OBM_OLD_CLOSE       32767
    #define OBM_SIZE            32766
    #define OBM_OLD_UPARROW     32765
    #define OBM_OLD_DNARROW     32764
    #define OBM_OLD_RGARROW     32763
    #define OBM_OLD_LFARROW     32762
    #define OBM_BTSIZE          32761
    #define OBM_CHECK           32760
    #define OBM_CHECKBOXES      32759
    #define OBM_BTNCORNERS      32758
    #define OBM_OLD_REDUCE      32757
    #define OBM_OLD_ZOOM        32756
    #define OBM_OLD_RESTORE     32755


    #define OCR_NORMAL          32512
    #define OCR_IBEAM           32513
    #define OCR_WAIT            32514
    #define OCR_CROSS           32515
    #define OCR_UP              32516
    #define OCR_SIZE            32640   // OBSOLETE: use OCR_SIZEALL
    #define OCR_ICON            32641   // OBSOLETE: use OCR_NORMAL
    #define OCR_SIZENWSE        32642
    #define OCR_SIZENESW        32643
    #define OCR_SIZEWE          32644
    #define OCR_SIZENS          32645
    #define OCR_SIZEALL         32646
    #define OCR_ICOCUR          32647   // OBSOLETE: use OIC_WINLOGO
    #define OCR_NO              32648
    //#if(WINVER >=   1280)
    #define OCR_HAND            32649
    //#endif // WINVER >= 0x0500
    //#if(WINVER >=   1024)
    #define OCR_APPSTARTING     32650
    //#endif // WINVER >= 0x0400


    #define OIC_SAMPLE          32512
    #define OIC_HAND            32513
    #define OIC_QUES            32514
    #define OIC_BANG            32515
    #define OIC_NOTE            32516
    //#if(WINVER >=   1024)
    #define OIC_WINLOGO         32517
    #define OIC_WARNING         OIC_BANG
    #define OIC_ERROR           OIC_HAND
    #define OIC_INFORMATION     OIC_NOTE
    //#endif // WINVER >= 0x0400

  #endif // OEMRESOURCE

  #define ORD_LANGDRIVER    1     /* The ordinal number for the entry point of
                                 ** language drivers.
                                 */

  //#ifndef NOICONS

  /*
  * Standard Icon IDs
  */
  //#ifdef RC_INVOKED
  #define IDI_APPLICATION     32512
  #define IDI_HAND            32513
  #define IDI_QUESTION        32514
  #define IDI_EXCLAMATION     32515
  #define IDI_ASTERISK        32516
  //#if(WINVER >=   1024)
  #define IDI_WINLOGO         32517
  //#endif // WINVER >= 0x0400
  //#else
  //#define IDI_APPLICATION     MAKEINTRESOURCE(32512)
  //#define IDI_HAND            MAKEINTRESOURCE(32513)
  //#define IDI_QUESTION        MAKEINTRESOURCE(32514)
  //#define IDI_EXCLAMATION     MAKEINTRESOURCE(32515)
  //#define IDI_ASTERISK        MAKEINTRESOURCE(32516)
  ////#if(WINVER >=   1024)
  //#define IDI_WINLOGO         MAKEINTRESOURCE(32517)
  //#endif // WINVER >= 0x0400
  //#endif // RC_INVOKED

  //#if(WINVER >=   1024)
  #define IDI_WARNING     IDI_EXCLAMATION
  #define IDI_ERROR       IDI_HAND
  #define IDI_INFORMATION IDI_ASTERISK
  //#endif // WINVER >= 0x0400


  //#endif // !NOICONS


  //#ifdef UNICODE
  //#define LoadString  LoadStringW
  //#else
  //#define LoadString  LoadStringA
  //#endif // !UNICODE


  /*
  * Dialog Box Command IDs
  */
  #define IDOK                1
  #define IDCANCEL            2
  #define IDABORT             3
  #define IDRETRY             4
  #define IDIGNORE            5
  #define IDYES               6
  #define IDNO                7
  //#if(WINVER >=   1024)
  #define IDCLOSE         8
  #define IDHELP          9
  //#endif // WINVER >= 0x0400

  //#if(WINVER >=   1280)
  #define IDTRYAGAIN      10
  #define IDCONTINUE      11
  //#endif // WINVER >= 0x0500


  #ifndef NOCTLMGR

    /*
    * Control Manager Structures and Definitions
    */

    #ifndef NOWINSTYLES

      /*
      * Edit Control Styles
      */
      #define ES_LEFT                   0
      #define ES_CENTER                 1
      #define ES_RIGHT                  2
      #define ES_MULTILINE              4
      #define ES_UPPERCASE              8
      #define ES_LOWERCASE             16
      #define ES_PASSWORD              32
      #define ES_AUTOVSCROLL           64
      #define ES_AUTOHSCROLL          128
      #define ES_NOHIDESEL            256
      #define ES_OEMCONVERT          1024
      #define ES_READONLY            2048
      #define ES_WANTRETURN          4096
      //#if(WINVER >=   1024)
      #define ES_NUMBER              8192
      //#endif // WINVER >= 0x0400

    #endif // !NOWINSTYLES

    /*
    * Edit Control Notification Codes
    */
    #define EN_SETFOCUS            256
    #define EN_KILLFOCUS           512
    #define EN_CHANGE              768
    #define EN_UPDATE             1024
    #define EN_ERRSPACE           1280
    #define EN_MAXTEXT            1281
    #define EN_HSCROLL            1537
    #define EN_VSCROLL            1538

    //#if(_WIN32_WINNT >=   1280)
    #define EN_ALIGN_LTR_EC       1792
    #define EN_ALIGN_RTL_EC       1793
    //#endif // _WIN32_WINNT >= 0x0500

    //#if(WINVER >=   1024)
    // Edit control EM_SETMARGIN parameters
    #define EC_LEFTMARGIN            1
    #define EC_RIGHTMARGIN           2
    #define EC_USEFONTINFO       65535
    //#endif // WINVER >= 0x0400

    //#if(WINVER >=   1280)
    //// wParam of EM_GET/SETIMESTATUS
    #define EMSIS_COMPOSITIONSTRING             1

    // lParam for EMSIS_COMPOSITIONSTRING
    #define EIMES_GETCOMPSTRATONCE              1
    #define EIMES_CANCELCOMPSTRINFOCUS          2
    #define EIMES_COMPLETECOMPSTRKILLFOCUS      4
    //#endif // WINVER >= 0x0500

    #ifndef NOWINMESSAGES

      /*
      * Edit Control Messages
      */
      #define EM_GETSEL                  176
      #define EM_SETSEL                  177
      #define EM_GETRECT                 178
      #define EM_SETRECT                 179
      #define EM_SETRECTNP               180
      #define EM_SCROLL                  181
      #define EM_LINESCROLL              182
      #define EM_SCROLLCARET             183
      #define EM_GETMODIFY               184
      #define EM_SETMODIFY               185
      #define EM_GETLINECOUNT            186
      #define EM_LINEINDEX               187
      #define EM_SETHANDLE               188
      #define EM_GETHANDLE               189
      #define EM_GETTHUMB                190
      #define EM_LINELENGTH              193
      #define EM_REPLACESEL              194
      #define EM_GETLINE                 196
      #define EM_LIMITTEXT               197
      #define EM_CANUNDO                 198
      #define EM_UNDO                    199
      #define EM_FMTLINES                200
      #define EM_LINEFROMCHAR            201
      #define EM_SETTABSTOPS             203
      #define EM_SETPASSWORDCHAR         204
      #define EM_EMPTYUNDOBUFFER         205
      #define EM_GETFIRSTVISIBLELINE     206
      #define EM_SETREADONLY             207
      #define EM_SETWORDBREAKPROC        208
      #define EM_GETWORDBREAKPROC        209
      #define EM_GETPASSWORDCHAR         210
      //#if(WINVER >=   1024)
      #define EM_SETMARGINS              211
      #define EM_GETMARGINS              212
      #define EM_SETLIMITTEXT         EM_LIMITTEXT   // ;win40 Name change
      #define EM_GETLIMITTEXT            213
      #define EM_POSFROMCHAR             214
      #define EM_CHARFROMPOS             215
      //#endif // WINVER >= 0x0400

      //#if(WINVER >=   1280)
      #define EM_SETIMESTATUS            216
      #define EM_GETIMESTATUS            217
      //#endif // WINVER >= 0x0500

    #endif // !NOWINMESSAGES

    /*
    * EDITWORDBREAKPROC code values
    */
    #define WB_LEFT            0
    #define WB_RIGHT           1
    #define WB_ISDELIMITER     2


    /*
    * Button Control Styles
    */
    #define BS_PUSHBUTTON                 0
    #define BS_DEFPUSHBUTTON              1
    #define BS_CHECKBOX                   2
    #define BS_AUTOCHECKBOX               3
    #define BS_RADIOBUTTON                4
    #define BS_3STATE                     5
    #define BS_AUTO3STATE                 6
    #define BS_GROUPBOX                   7
    #define BS_USERBUTTON                 8
    #define BS_AUTORADIOBUTTON            9
    #define BS_OWNERDRAW                 11
    #define BS_LEFTTEXT                  32
    //#if(WINVER >=   1024)
    #define BS_TEXT                       0
    #define BS_ICON                      64
    #define BS_BITMAP                   128
    #define BS_LEFT                     256
    #define BS_RIGHT                    512
    #define BS_CENTER                   768
    #define BS_TOP                     1024
    #define BS_BOTTOM                  2048
    #define BS_VCENTER                 3072
    #define BS_PUSHLIKE                4096
    #define BS_MULTILINE               8192
    #define BS_NOTIFY                 16384
    #define BS_FLAT                   32768
    #define BS_RIGHTBUTTON      BS_LEFTTEXT
    //#endif // WINVER >= 0x0400

    /*
    * User Button Notification Codes
    */
    #define BN_CLICKED          0
    #define BN_PAINT            1
    #define BN_HILITE           2
    #define BN_UNHILITE         3
    #define BN_DISABLE          4
    #define BN_DOUBLECLICKED    5
    //#if(WINVER >=   1024)
    #define BN_PUSHED           BN_HILITE
    #define BN_UNPUSHED         BN_UNHILITE
    #define BN_DBLCLK           BN_DOUBLECLICKED
    #define BN_SETFOCUS         6
    #define BN_KILLFOCUS        7
    //#endif // WINVER >= 0x0400

    /*
    * Button Control Messages
    */
    #define BM_GETCHECK           240
    #define BM_SETCHECK           241
    #define BM_GETSTATE           242
    #define BM_SETSTATE           243
    #define BM_SETSTYLE           244
    //#if(WINVER >=   1024)
    #define BM_CLICK              245
    #define BM_GETIMAGE           246
    #define BM_SETIMAGE           247

    #define BST_UNCHECKED           0
    #define BST_CHECKED             1
    #define BST_INDETERMINATE       2
    #define BST_PUSHED              4
    #define BST_FOCUS               8
    //#endif // WINVER >= 0x0400

    /*
    * Static Control Constants
    */
    #define SS_LEFT                       0
    #define SS_CENTER                     1
    #define SS_RIGHT                      2
    #define SS_ICON                       3
    #define SS_BLACKRECT                  4
    #define SS_GRAYRECT                   5
    #define SS_WHITERECT                  6
    #define SS_BLACKFRAME                 7
    #define SS_GRAYFRAME                  8
    #define SS_WHITEFRAME                 9
    #define SS_USERITEM                  10
    #define SS_SIMPLE                    11
    #define SS_LEFTNOWORDWRAP            12
    //#if(WINVER >=   1024)
    #define SS_OWNERDRAW                 13
    #define SS_BITMAP                    14
    #define SS_ENHMETAFILE               15
    #define SS_ETCHEDHORZ                16
    #define SS_ETCHEDVERT                17
    #define SS_ETCHEDFRAME               18
    #define SS_TYPEMASK                  31
    //#endif // WINVER >= 0x0400
    #define SS_NOPREFIX                 128 // Don't do "&" character translation
    //#if(WINVER >=   1024)
    #define SS_NOTIFY                   256
    #define SS_CENTERIMAGE              512
    #define SS_RIGHTJUST               1024
    #define SS_REALSIZEIMAGE           2048
    #define SS_SUNKEN                  4096
    #define SS_ENDELLIPSIS            16384
    #define SS_PATHELLIPSIS           32768
    #define SS_WORDELLIPSIS           49152
    #define SS_ELLIPSISMASK           49152
    //#endif // WINVER >= 0x0400


    #ifndef NOWINMESSAGES
      /*
      * Static Control Mesages
      */
      #define STM_SETICON            368
      #define STM_GETICON            369
      //#if(WINVER >=   1024)
      #define STM_SETIMAGE           370
      #define STM_GETIMAGE           371
      #define STN_CLICKED         0
      #define STN_DBLCLK          1
      #define STN_ENABLE          2
      #define STN_DISABLE         3
      //#endif // WINVER >= 0x0400
      #define STM_MSGMAX             372
    #endif // !NOWINMESSAGES

    /*
    * Dialog window class
    */
    #define WC_DIALOG       (MAKEINTATOM( 32770))

    /*
    * Get/SetWindowWord/Long offsets for use with WC_DIALOG windows
    */
    #define DWL_MSGRESULT   0
    #define DWL_DLGPROC     4
    #define DWL_USER        8

    //#ifdef _WIN64
    //
    //
    //#endif // _WIN64

    #define DWLP_MSGRESULT  0
    #define DWLP_DLGPROC    DWLP_MSGRESULT + sizeof(LRESULT)
    #define DWLP_USER       DWLP_DLGPROC + sizeof(DLGPROC)

    /*
    * Dialog Manager Routines
    */

    #ifndef NOMSG
      //#ifdef UNICODE
      //#define IsDialogMessage  IsDialogMessageW
      //#else
      //#define IsDialogMessage  IsDialogMessageA
      //#endif // !UNICODE
    #endif // !NOMSG


    //#ifdef UNICODE
    //#define DlgDirList  DlgDirListW
    //#else
    //#define DlgDirList  DlgDirListA
    //#endif // !UNICODE

    /*
    * DlgDirList, DlgDirListComboBox flags values
    */
    #define DDL_READWRITE            0
    #define DDL_READONLY             1
    #define DDL_HIDDEN               2
    #define DDL_SYSTEM               4
    #define DDL_DIRECTORY           16
    #define DDL_ARCHIVE             32

    #define DDL_POSTMSGS          8192
    #define DDL_DRIVES           16384
    #define DDL_EXCLUSIVE        32768


    //#ifdef UNICODE
    //#define DlgDirSelectEx  DlgDirSelectExW
    //#else
    //#define DlgDirSelectEx  DlgDirSelectExA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define DlgDirListComboBox  DlgDirListComboBoxW
    //#else
    //#define DlgDirListComboBox  DlgDirListComboBoxA
    //#endif // !UNICODE
    //
    //
    //#ifdef UNICODE
    //#define DlgDirSelectComboBoxEx  DlgDirSelectComboBoxExW
    //#else
    //#define DlgDirSelectComboBoxEx  DlgDirSelectComboBoxExA
    //#endif // !UNICODE


    /*
    * Dialog Styles
    */
    #define DS_ABSALIGN             1
    #define DS_SYSMODAL             2
    #define DS_LOCALEDIT           32   // Edit items get Local storage.
    #define DS_SETFONT             64   // User specified font for Dlg controls
    #define DS_MODALFRAME         128   // Can be combined with WS_CAPTION
    #define DS_NOIDLEMSG           256  // WM_ENTERIDLE message will not be sent
    #define DS_SETFOREGROUND       512  // not in win3.1


    //#if(WINVER >=   1024)
    #define DS_3DLOOK                 4
    #define DS_FIXEDSYS               8
    #define DS_NOFAILCREATE          16
    #define DS_CONTROL             1024
    #define DS_CENTER              2048
    #define DS_CENTERMOUSE         4096
    #define DS_CONTEXTHELP         8192

    #define DS_SHELLFONT        (DS_SETFONT + DS_FIXEDSYS)
    //#endif // WINVER >= 0x0400


    #define DM_GETDEFID         (WM_USER+0)
    #define DM_SETDEFID         (WM_USER+1)

    //#if(WINVER >=   1024)
    #define DM_REPOSITION       (WM_USER+2)
    //#endif // WINVER >= 0x0400
    /*
    * Returned in HIWORD() of DM_GETDEFID result if msg is supported
    */
    #define DC_HASDEFID          21323

    /*
    * Dialog Codes
    */
    #define DLGC_WANTARROWS          1      // Control wants arrow keys
    #define DLGC_WANTTAB             2      // Control wants tab keys
    #define DLGC_WANTALLKEYS         4      // Control wants all keys
    #define DLGC_WANTMESSAGE         4      // Pass message to control
    #define DLGC_HASSETSEL           8      // Understands EM_SETSEL message
    #define DLGC_DEFPUSHBUTTON      16      // Default pushbutton
    #define DLGC_UNDEFPUSHBUTTON     32     // Non-default pushbutton
    #define DLGC_RADIOBUTTON        64      // Radio button
    #define DLGC_WANTCHARS         128      // Want WM_CHAR messages
    #define DLGC_STATIC            256      // Static item: don't include
    #define DLGC_BUTTON           8192      // Button item: can be checked

    #define LB_CTLCODE          0L

    /*
    * Listbox Return Values
    */
    #define LB_OKAY             0
    #define LB_ERR              (-1)
    #define LB_ERRSPACE         (-2)

    /*
    **  The idStaticPath parameter to DlgDirList can have the following values
    **  ORed if the list box should show other details of the files along with
    **  the name of the files;
    */
                                     // all other details also will be returned


    /*
    * Listbox Notification Codes
    */
    #define LBN_ERRSPACE        (-2)
    #define LBN_SELCHANGE       1
    #define LBN_DBLCLK          2
    #define LBN_SELCANCEL       3
    #define LBN_SETFOCUS        4
    #define LBN_KILLFOCUS       5


    #ifndef NOWINMESSAGES

      /*
      * Listbox messages
      */
      #define LB_ADDSTRING               384
      #define LB_INSERTSTRING            385
      #define LB_DELETESTRING            386
      #define LB_SELITEMRANGEEX          387
      #define LB_RESETCONTENT            388
      #define LB_SETSEL                  389
      #define LB_SETCURSEL               390
      #define LB_GETSEL                  391
      #define LB_GETCURSEL               392
      #define LB_GETTEXT                 393
      #define LB_GETTEXTLEN              394
      #define LB_GETCOUNT                395
      #define LB_SELECTSTRING            396
      #define LB_DIR                     397
      #define LB_GETTOPINDEX             398
      #define LB_FINDSTRING              399
      #define LB_GETSELCOUNT             400
      #define LB_GETSELITEMS             401
      #define LB_SETTABSTOPS             402
      #define LB_GETHORIZONTALEXTENT     403
      #define LB_SETHORIZONTALEXTENT     404
      #define LB_SETCOLUMNWIDTH          405
      #define LB_ADDFILE                 406
      #define LB_SETTOPINDEX             407
      #define LB_GETITEMRECT             408
      #define LB_GETITEMDATA             409
      #define LB_SETITEMDATA             410
      #define LB_SELITEMRANGE            411
      #define LB_SETANCHORINDEX          412
      #define LB_GETANCHORINDEX          413
      #define LB_SETCARETINDEX           414
      #define LB_GETCARETINDEX           415
      #define LB_SETITEMHEIGHT           416
      #define LB_GETITEMHEIGHT           417
      #define LB_FINDSTRINGEXACT         418
      #define LB_SETLOCALE               421
      #define LB_GETLOCALE               422
      #define LB_SETCOUNT                423
      //#if(WINVER >=   1024)
      #define LB_INITSTORAGE             424
      #define LB_ITEMFROMPOINT           425
      //#endif // WINVER >= 0x0400
      //#if(_WIN32_WCE >=   1024)
      //#define LB_MULTIPLEADDSTRING       433
      //#endif

      //#if(_WIN32_WCE >=   1024)
      //#define LB_MSGMAX                  433
      //
      //#define LB_MSGMAX                  432
      //#else
      #define LB_MSGMAX                  424
      //#endif

    #endif // !NOWINMESSAGES

    #ifndef NOWINSTYLES

      /*
      * Listbox Styles
      */
      #define LBS_NOTIFY                  1
      #define LBS_SORT                    2
      #define LBS_NOREDRAW                4
      #define LBS_MULTIPLESEL             8
      #define LBS_OWNERDRAWFIXED         16
      #define LBS_OWNERDRAWVARIABLE      32
      #define LBS_HASSTRINGS             64
      #define LBS_USETABSTOPS           128
      #define LBS_NOINTEGRALHEIGHT      256
      #define LBS_MULTICOLUMN           512
      #define LBS_WANTKEYBOARDINPUT    1024
      #define LBS_EXTENDEDSEL          2048
      #define LBS_DISABLENOSCROLL      4096
      #define LBS_NODATA               8192
      //#if(WINVER >=   1024)
      #define LBS_NOSEL               16384
      //#endif // WINVER >= 0x0400
      #define LBS_STANDARD          (LBS_NOTIFY + LBS_SORT + WS_VSCROLL + WS_BORDER)

    #endif // !NOWINSTYLES


    /*
    * Combo Box return Values
    */
    #define CB_OKAY             0
    #define CB_ERR              (-1)
    #define CB_ERRSPACE         (-2)


    /*
    * Combo Box Notification Codes
    */
    #define CBN_ERRSPACE        (-1)
    #define CBN_SELCHANGE       1
    #define CBN_DBLCLK          2
    #define CBN_SETFOCUS        3
    #define CBN_KILLFOCUS       4
    #define CBN_EDITCHANGE      5
    #define CBN_EDITUPDATE      6
    #define CBN_DROPDOWN        7
    #define CBN_CLOSEUP         8
    #define CBN_SELENDOK        9
    #define CBN_SELENDCANCEL    10

    #ifndef NOWINSTYLES

      /*
      * Combo Box styles
      */
      #define CBS_SIMPLE                  1
      #define CBS_DROPDOWN                2
      #define CBS_DROPDOWNLIST            3
      #define CBS_OWNERDRAWFIXED         16
      #define CBS_OWNERDRAWVARIABLE      32
      #define CBS_AUTOHSCROLL            64
      #define CBS_OEMCONVERT            128
      #define CBS_SORT                  256
      #define CBS_HASSTRINGS            512
      #define CBS_NOINTEGRALHEIGHT     1024
      #define CBS_DISABLENOSCROLL      2048
      //#if(WINVER >=   1024)
      #define CBS_UPPERCASE              8192
      #define CBS_LOWERCASE             16384
      //#endif // WINVER >= 0x0400

    #endif  // !NOWINSTYLES


    /*
    * Combo Box messages
    */
    #ifndef NOWINMESSAGES
      #define CB_GETEDITSEL                  320
      #define CB_LIMITTEXT                   321
      #define CB_SETEDITSEL                  322
      #define CB_ADDSTRING                   323
      #define CB_DELETESTRING                324
      #define CB_DIR                         325
      #define CB_GETCOUNT                    326
      #define CB_GETCURSEL                   327
      #define CB_GETLBTEXT                   328
      #define CB_GETLBTEXTLEN                329
      #define CB_INSERTSTRING                330
      #define CB_RESETCONTENT                331
      #define CB_FINDSTRING                  332
      #define CB_SELECTSTRING                333
      #define CB_SETCURSEL                   334
      #define CB_SHOWDROPDOWN                335
      #define CB_GETITEMDATA                 336
      #define CB_SETITEMDATA                 337
      #define CB_GETDROPPEDCONTROLRECT       338
      #define CB_SETITEMHEIGHT               339
      #define CB_GETITEMHEIGHT               340
      #define CB_SETEXTENDEDUI               341
      #define CB_GETEXTENDEDUI               342
      #define CB_GETDROPPEDSTATE             343
      #define CB_FINDSTRINGEXACT             344
      #define CB_SETLOCALE                   345
      #define CB_GETLOCALE                   346
      //#if(WINVER >=   1024)
      #define CB_GETTOPINDEX                 347
      #define CB_SETTOPINDEX                 348
      #define CB_GETHORIZONTALEXTENT         349
      #define CB_SETHORIZONTALEXTENT         350
      #define CB_GETDROPPEDWIDTH             351
      #define CB_SETDROPPEDWIDTH             352
      #define CB_INITSTORAGE                 353
      //#if(_WIN32_WCE >=   1024)
      //#define CB_MULTIPLEADDSTRING           355
      //#endif
      //#endif // WINVER >= 0x0400
      //#if(_WIN32_WCE >=   1024)
      //#define CB_MSGMAX                      355
      //
      //#define CB_MSGMAX                      354
      //#else
      #define CB_MSGMAX                      347
      //#endif
    #endif  // !NOWINMESSAGES


    #ifndef NOWINSTYLES

      /*
      * Scroll Bar Styles
      */
      #define SBS_HORZ                          0
      #define SBS_VERT                          1
      #define SBS_TOPALIGN                      2
      #define SBS_LEFTALIGN                     2
      #define SBS_BOTTOMALIGN                   4
      #define SBS_RIGHTALIGN                    4
      #define SBS_SIZEBOXTOPLEFTALIGN           2
      #define SBS_SIZEBOXBOTTOMRIGHTALIGN       4
      #define SBS_SIZEBOX                       8
      //#if(WINVER >=   1024)
      //#define SBS_SIZEGRIP                     16
      //#endif // WINVER >= 0x0400

    #endif // !NOWINSTYLES

    /*
    * Scroll bar messages
    */
    #ifndef NOWINMESSAGES
      #define SBM_SETPOS                     224 //not in win3.1
      #define SBM_GETPOS                     225 //not in win3.1
      #define SBM_SETRANGE                   226 //not in win3.1
      #define SBM_SETRANGEREDRAW             230 //not in win3.1
      #define SBM_GETRANGE                   227 //not in win3.1
      #define SBM_ENABLE_ARROWS              228 //not in win3.1
      //#if(WINVER >=   1024)
      #define SBM_SETSCROLLINFO              233
      #define SBM_GETSCROLLINFO              234

      #define SIF_RANGE                1
      #define SIF_PAGE                 2
      #define SIF_POS                  4
      #define SIF_DISABLENOSCROLL      8
      #define SIF_TRACKPOS            16
      #define SIF_ALL             (SIF_RANGE + SIF_PAGE + SIF_POS + SIF_TRACKPOS)


      //#endif // WINVER >= 0x0400

    #endif // !NOWINMESSAGES

  #endif // !NOCTLMGR

  #ifndef NOMDI

    /*
    * MDI client style bits
    */
    #define MDIS_ALLCHILDSTYLES         1

    /*
    * wParam Flags for WM_MDITILE and WM_MDICASCADE messages.
    */
    #define MDITILE_VERTICAL            0 //not in win3.1
    #define MDITILE_HORIZONTAL          1 //not in win3.1
    #define MDITILE_SKIPDISABLED        2 //not in win3.1
    //#if(_WIN32_WINNT >=   1280)
    #define MDITILE_ZORDER              4
    //#endif // _WIN32_WINNT >= 0x0500


               // app-defined stuff


               // app-defined stuff

    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE


    //#ifdef UNICODE
    //#define DefFrameProc  DefFrameProcW
    //#else
    //#define DefFrameProc  DefFrameProcA
    //#endif // !UNICODE


    //#ifndef _MAC
    //
    //
    //#else
    //
    //
    //#endif
    //
    //
    //#ifndef _MAC
    //
    //
    //#else
    //
    //
    //#endif


    //#ifdef UNICODE
    //#define DefMDIChildProc  DefMDIChildProcW
    //#else
    //#define DefMDIChildProc  DefMDIChildProcA
    //#endif // !UNICODE

    #ifndef NOMSG


    #endif // !NOMSG


    //#ifdef UNICODE
    //#define CreateMDIWindow  CreateMDIWindowW
    //#else
    //#define CreateMDIWindow  CreateMDIWindowA
    //#endif // !UNICODE

    //#if(WINVER >=   1024)
    //
    //
    //#endif // WINVER >= 0x0400
  #endif // !NOMDI

  //***** Help support *******************************************************

  #ifndef NOHELP

    //#ifndef _MAC
    //
    //#else
    //
    //#endif
    //
    //
    //#ifndef _MAC
    //
    //#else
    //
    //#endif
    //
    //
    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE
    //
    //
    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE
    //

    /*
    * Commands to pass to WinHelp()
    */
    #define HELP_CONTEXT            1  // Display topic in ulTopic
    #define HELP_QUIT               2  // Terminate help
    #define HELP_INDEX              3  // Display index
    #define HELP_CONTENTS           3
    #define HELP_HELPONHELP         4  // Display help on using help
    #define HELP_SETINDEX           5  // Set current Index for multi index help
    #define HELP_SETCONTENTS        5
    #define HELP_CONTEXTPOPUP       8
    #define HELP_FORCEFILE          9
    #define HELP_KEY              257  // Display topic for keyword in offabData
    #define HELP_COMMAND          258
    #define HELP_PARTIALKEY       261
    #define HELP_MULTIKEY         513
    #define HELP_SETWINPOS        515
    //#if(WINVER >=   1024)
    #define HELP_CONTEXTMENU      10
    #define HELP_FINDER           11
    #define HELP_WM_HELP          12
    #define HELP_SETPOPUP_POS     13

    #define HELP_TCARD               32768
    #define HELP_TCARD_DATA             16
    #define HELP_TCARD_OTHER_CALLER     17

    // These are in winhelp.h in Win95.
    #define IDH_NO_HELP                     28440
    #define IDH_MISSING_CONTEXT             28441 // Control doesn't have matching help context
    #define IDH_GENERIC_HELP_BUTTON         28442 // Property sheet help button
    #define IDH_OK                          28443
    #define IDH_CANCEL                      28444
    #define IDH_HELP                        28445

    //#endif // WINVER >= 0x0400


    //#ifdef UNICODE
    //#define WinHelp  WinHelpW
    //#else
    //#define WinHelp  WinHelpA
    //#endif // !UNICODE

  #endif // !NOHELP

  //#if(WINVER >=   1280)
  //
  #define GR_GDIOBJECTS     0       // Count of GDI objects
  #define GR_USEROBJECTS    1       // Count of USER objects
  //
  //
  //#endif // WINVER >= 0x0500


  #ifndef NOSYSPARAMSINFO

    /*
    * Parameter for SystemParametersInfo()
    */

    #define SPI_GETBEEP                 1
    #define SPI_SETBEEP                 2
    #define SPI_GETMOUSE                3
    #define SPI_SETMOUSE                4
    #define SPI_GETBORDER               5
    #define SPI_SETBORDER               6
    #define SPI_GETKEYBOARDSPEED       10
    #define SPI_SETKEYBOARDSPEED       11
    #define SPI_LANGDRIVER             12
    #define SPI_ICONHORIZONTALSPACING  13
    #define SPI_GETSCREENSAVETIMEOUT   14
    #define SPI_SETSCREENSAVETIMEOUT   15
    #define SPI_GETSCREENSAVEACTIVE    16
    #define SPI_SETSCREENSAVEACTIVE    17
    #define SPI_GETGRIDGRANULARITY     18
    #define SPI_SETGRIDGRANULARITY     19
    #define SPI_SETDESKWALLPAPER       20
    #define SPI_SETDESKPATTERN         21
    #define SPI_GETKEYBOARDDELAY       22
    #define SPI_SETKEYBOARDDELAY       23
    #define SPI_ICONVERTICALSPACING    24
    #define SPI_GETICONTITLEWRAP       25
    #define SPI_SETICONTITLEWRAP       26
    #define SPI_GETMENUDROPALIGNMENT   27
    #define SPI_SETMENUDROPALIGNMENT   28
    #define SPI_SETDOUBLECLKWIDTH      29
    #define SPI_SETDOUBLECLKHEIGHT     30
    #define SPI_GETICONTITLELOGFONT    31
    #define SPI_SETDOUBLECLICKTIME     32
    #define SPI_SETMOUSEBUTTONSWAP     33
    #define SPI_SETICONTITLELOGFONT    34
    #define SPI_GETFASTTASKSWITCH      35
    #define SPI_SETFASTTASKSWITCH      36
    //#if(WINVER >=   1024)
    #define SPI_SETDRAGFULLWINDOWS     37
    #define SPI_GETDRAGFULLWINDOWS     38
    #define SPI_GETNONCLIENTMETRICS    41
    #define SPI_SETNONCLIENTMETRICS    42
    #define SPI_GETMINIMIZEDMETRICS    43
    #define SPI_SETMINIMIZEDMETRICS    44
    #define SPI_GETICONMETRICS         45
    #define SPI_SETICONMETRICS         46
    #define SPI_SETWORKAREA            47
    #define SPI_GETWORKAREA            48
    #define SPI_SETPENWINDOWS          49

    #define SPI_GETHIGHCONTRAST        66
    #define SPI_SETHIGHCONTRAST        67
    #define SPI_GETKEYBOARDPREF        68
    #define SPI_SETKEYBOARDPREF        69
    #define SPI_GETSCREENREADER        70
    #define SPI_SETSCREENREADER        71
    #define SPI_GETANIMATION           72
    #define SPI_SETANIMATION           73
    #define SPI_GETFONTSMOOTHING       74
    #define SPI_SETFONTSMOOTHING       75
    #define SPI_SETDRAGWIDTH           76
    #define SPI_SETDRAGHEIGHT          77
    #define SPI_SETHANDHELD            78
    #define SPI_GETLOWPOWERTIMEOUT     79
    #define SPI_GETPOWEROFFTIMEOUT     80
    #define SPI_SETLOWPOWERTIMEOUT     81
    #define SPI_SETPOWEROFFTIMEOUT     82
    #define SPI_GETLOWPOWERACTIVE      83
    #define SPI_GETPOWEROFFACTIVE      84
    #define SPI_SETLOWPOWERACTIVE      85
    #define SPI_SETPOWEROFFACTIVE      86
    #define SPI_SETCURSORS             87
    #define SPI_SETICONS               88
    #define SPI_GETDEFAULTINPUTLANG    89
    #define SPI_SETDEFAULTINPUTLANG    90
    #define SPI_SETLANGTOGGLE          91
    #define SPI_GETWINDOWSEXTENSION    92
    #define SPI_SETMOUSETRAILS         93
    #define SPI_GETMOUSETRAILS         94
    #define SPI_SETSCREENSAVERRUNNING  97
    #define SPI_SCREENSAVERRUNNING     SPI_SETSCREENSAVERRUNNING
    //#endif // WINVER >= 0x0400
    #define SPI_GETFILTERKEYS          50
    #define SPI_SETFILTERKEYS          51
    #define SPI_GETTOGGLEKEYS          52
    #define SPI_SETTOGGLEKEYS          53
    #define SPI_GETMOUSEKEYS           54
    #define SPI_SETMOUSEKEYS           55
    #define SPI_GETSHOWSOUNDS          56
    #define SPI_SETSHOWSOUNDS          57
    #define SPI_GETSTICKYKEYS          58
    #define SPI_SETSTICKYKEYS          59
    #define SPI_GETACCESSTIMEOUT       60
    #define SPI_SETACCESSTIMEOUT       61
    //#if(WINVER >=   1024)
    #define SPI_GETSERIALKEYS          62
    #define SPI_SETSERIALKEYS          63
    //#endif // WINVER >= 0x0400
    #define SPI_GETSOUNDSENTRY         64
    #define SPI_SETSOUNDSENTRY         65
    //#if(_WIN32_WINNT >=   1024)
    #define SPI_GETSNAPTODEFBUTTON     95
    #define SPI_SETSNAPTODEFBUTTON     96
    //#endif // _WIN32_WINNT >= 0x0400
    //#if (_WIN32_WINNT >=   1024) .OR. (_WIN32_WINDOWS >   1024)
    #define SPI_GETMOUSEHOVERWIDTH     98
    #define SPI_SETMOUSEHOVERWIDTH     99
    #define SPI_GETMOUSEHOVERHEIGHT   100
    #define SPI_SETMOUSEHOVERHEIGHT   101
    #define SPI_GETMOUSEHOVERTIME     102
    #define SPI_SETMOUSEHOVERTIME     103
    #define SPI_GETWHEELSCROLLLINES   104
    #define SPI_SETWHEELSCROLLLINES   105
    #define SPI_GETMENUSHOWDELAY      106
    #define SPI_SETMENUSHOWDELAY      107


    #define SPI_GETSHOWIMEUI          110
    #define SPI_SETSHOWIMEUI          111
    //#endif


    //#if(WINVER >=   1280)
    #define SPI_GETMOUSESPEED         112
    #define SPI_SETMOUSESPEED         113
    #define SPI_GETSCREENSAVERRUNNING 114
    #define SPI_GETDESKWALLPAPER      115

    //#endif // WINVER >= 0x0500


    //#if(WINVER >=   1280)
    #define SPI_GETACTIVEWINDOWTRACKING           4096
    #define SPI_SETACTIVEWINDOWTRACKING           4097
    #define SPI_GETMENUANIMATION                  4098
    #define SPI_SETMENUANIMATION                  4099
    #define SPI_GETCOMBOBOXANIMATION              4100
    #define SPI_SETCOMBOBOXANIMATION              4101
    #define SPI_GETLISTBOXSMOOTHSCROLLING         4102
    #define SPI_SETLISTBOXSMOOTHSCROLLING         4103
    #define SPI_GETGRADIENTCAPTIONS               4104
    #define SPI_SETGRADIENTCAPTIONS               4105
    #define SPI_GETKEYBOARDCUES                   4106
    #define SPI_SETKEYBOARDCUES                   4107
    #define SPI_GETMENUUNDERLINES               SPI_GETKEYBOARDCUES
    #define SPI_SETMENUUNDERLINES               SPI_SETKEYBOARDCUES
    #define SPI_GETACTIVEWNDTRKZORDER             4108
    #define SPI_SETACTIVEWNDTRKZORDER             4109
    #define SPI_GETHOTTRACKING                    4110
    #define SPI_SETHOTTRACKING                    4111
    #define SPI_GETMENUFADE                       4114
    #define SPI_SETMENUFADE                       4115
    #define SPI_GETSELECTIONFADE                  4116
    #define SPI_SETSELECTIONFADE                  4117
    #define SPI_GETTOOLTIPANIMATION               4118
    #define SPI_SETTOOLTIPANIMATION               4119
    #define SPI_GETTOOLTIPFADE                    4120
    #define SPI_SETTOOLTIPFADE                    4121
    #define SPI_GETCURSORSHADOW                   4122
    #define SPI_SETCURSORSHADOW                   4123

    #define SPI_GETUIEFFECTS                      4158
    #define SPI_SETUIEFFECTS                      4159

    #define SPI_GETFOREGROUNDLOCKTIMEOUT          8192
    #define SPI_SETFOREGROUNDLOCKTIMEOUT          8193
    #define SPI_GETACTIVEWNDTRKTIMEOUT            8194
    #define SPI_SETACTIVEWNDTRKTIMEOUT            8195
    #define SPI_GETFOREGROUNDFLASHCOUNT           8196
    #define SPI_SETFOREGROUNDFLASHCOUNT           8197
    #define SPI_GETCARETWIDTH                     8198
    #define SPI_SETCARETWIDTH                     8199
    //#endif // WINVER >= 0x0500

    /*
    * Flags
    */
    #define SPIF_UPDATEINIFILE         1
    #define SPIF_SENDWININICHANGE      2
    #define SPIF_SENDCHANGE       SPIF_SENDWININICHANGE


    #define METRICS_USEDEFAULT -1

    #ifdef _WINGDI_
      #ifndef NOGDI

      #endif // NOGDI
    #endif // _WINGDI_

    #define ARW_BOTTOMLEFT                    0
    #define ARW_BOTTOMRIGHT                   1
    #define ARW_TOPLEFT                       2
    #define ARW_TOPRIGHT                      3
    #define ARW_STARTMASK                     3
    #define ARW_STARTRIGHT                    1
    #define ARW_STARTTOP                      2

    #define ARW_LEFT                          0
    #define ARW_RIGHT                         0
    #define ARW_UP                            4
    #define ARW_DOWN                          4
    #define ARW_HIDE                          8


    #ifdef _WINGDI_
      #ifndef NOGDI
        #ifdef UNICODE
        #else
        #endif // UNICODE
      #endif // NOGDI
    #endif // _WINGDI_


    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE

    // flags for SERIALKEYS dwFlags field
    #define SERKF_SERIALKEYSON           1
    #define SERKF_AVAILABLE              2
    #define SERKF_INDICATOR              4


    //#ifdef UNICODE
    //
    //
    //#else
    //
    //
    //#endif // UNICODE

    // flags for HIGHCONTRAST dwFlags field
    #define HCF_HIGHCONTRASTON           1
    #define HCF_AVAILABLE                2
    #define HCF_HOTKEYACTIVE             4
    #define HCF_CONFIRMHOTKEY            8
    #define HCF_HOTKEYSOUND             16
    #define HCF_INDICATOR               32
    #define HCF_HOTKEYAVAILABLE         64

    // Flags for ChangeDisplaySettings
    #define CDS_UPDATEREGISTRY           1
    #define CDS_TEST                     2
    #define CDS_FULLSCREEN               4
    #define CDS_GLOBAL                   8
    #define CDS_SET_PRIMARY             16
    #define CDS_VIDEOPARAMETERS         32
    #define CDS_RESET           1073741824
    #define CDS_NORESET          268435456

    //#include "tvout.ch"

    // Return values for ChangeDisplaySettings
    #define DISP_CHANGE_SUCCESSFUL       0
    #define DISP_CHANGE_RESTART          1
    #define DISP_CHANGE_FAILED          -1
    #define DISP_CHANGE_BADMODE         -2
    #define DISP_CHANGE_NOTUPDATED      -3
    #define DISP_CHANGE_BADFLAGS        -4
    #define DISP_CHANGE_BADPARAM        -5

    #ifdef _WINGDI_
      #ifndef NOGDI
        //#ifdef UNICODE
        //#define ChangeDisplaySettings  ChangeDisplaySettingsW
        //#else
        //#define ChangeDisplaySettings  ChangeDisplaySettingsA
        //#endif // !UNICODE
        //
        //
        //#ifdef UNICODE
        //#define ChangeDisplaySettingsEx  ChangeDisplaySettingsExW
        //#else
        //#define ChangeDisplaySettingsEx  ChangeDisplaySettingsExA
        //#endif // !UNICODE

        #define ENUM_CURRENT_SETTINGS       ((DWORD)-1)
        #define ENUM_REGISTRY_SETTINGS      ((DWORD)-2)


        //#ifdef UNICODE
        //#define EnumDisplaySettings  EnumDisplaySettingsW
        //#else
        //#define EnumDisplaySettings  EnumDisplaySettingsA
        //#endif // !UNICODE

        //#if(WINVER >=   1280)
        //
        //
        //#ifdef UNICODE
        //#define EnumDisplaySettingsEx  EnumDisplaySettingsExW
        //#else
        //#define EnumDisplaySettingsEx  EnumDisplaySettingsExA
        //#endif // !UNICODE
        //
        //// Flags for EnumDisplaySettingsEx
        #define EDS_RAWMODE                            2
        //
        //
        //#ifdef UNICODE
        //#define EnumDisplayDevices  EnumDisplayDevicesW
        //#else
        //#define EnumDisplayDevices  EnumDisplayDevicesA
        //#endif // !UNICODE
        //#endif // WINVER >= 0x0500

      #endif // NOGDI
    #endif // _WINGDI_


    //#ifdef UNICODE
    //#define SystemParametersInfo  SystemParametersInfoW
    //#else
    //#define SystemParametersInfo  SystemParametersInfoA
    //#endif // !UNICODE

  #endif  // !NOSYSPARAMSINFO

  /*
  * Accessibility support
  */


                 // Acceptance Delay
                // Delay Until Repeat
               // Repeat Rate
               // Debounce Time


  /*
  * FILTERKEYS dwFlags field
  */
  #define FKF_FILTERKEYSON             1
  #define FKF_AVAILABLE                2
  #define FKF_HOTKEYACTIVE             4
  #define FKF_CONFIRMHOTKEY            8
  #define FKF_HOTKEYSOUND             16
  #define FKF_INDICATOR               32
  #define FKF_CLICKON                 64


  /*
  * STICKYKEYS dwFlags field
  */
  #define SKF_STICKYKEYSON             1
  #define SKF_AVAILABLE                2
  #define SKF_HOTKEYACTIVE             4
  #define SKF_CONFIRMHOTKEY            8
  #define SKF_HOTKEYSOUND             16
  #define SKF_INDICATOR               32
  #define SKF_AUDIBLEFEEDBACK         64
  #define SKF_TRISTATE               128
  #define SKF_TWOKEYSOFF             256
  //#if(_WIN32_WINNT >=   1280)
  #define SKF_LALTLATCHED        268435456
  #define SKF_LCTLLATCHED         67108864
  #define SKF_LSHIFTLATCHED       16777216
  #define SKF_RALTLATCHED        536870912
  #define SKF_RCTLLATCHED        134217728
  #define SKF_RSHIFTLATCHED       33554432
  #define SKF_LWINLATCHED       1073741824
  #define SKF_RWINLATCHED       2147483648
  #define SKF_LALTLOCKED           1048576
  #define SKF_LCTLLOCKED            262144
  #define SKF_LSHIFTLOCKED           65536
  #define SKF_RALTLOCKED           2097152
  #define SKF_RCTLLOCKED            524288
  #define SKF_RSHIFTLOCKED          131072
  #define SKF_LWINLOCKED           4194304
  #define SKF_RWINLOCKED           8388608
  //#endif // _WIN32_WINNT >= 0x0500


  /*
  * MOUSEKEYS dwFlags field
  */
  #define MKF_MOUSEKEYSON              1
  #define MKF_AVAILABLE                2
  #define MKF_HOTKEYACTIVE             4
  #define MKF_CONFIRMHOTKEY            8
  #define MKF_HOTKEYSOUND             16
  #define MKF_INDICATOR               32
  #define MKF_MODIFIERS               64
  #define MKF_REPLACENUMBERS         128
  //#if(_WIN32_WINNT >=   1280)
  #define MKF_LEFTBUTTONSEL    268435456
  #define MKF_RIGHTBUTTONSEL   536870912
  #define MKF_LEFTBUTTONDOWN    16777216
  #define MKF_RIGHTBUTTONDOWN   33554432
  #define MKF_MOUSEMODE       2147483648
  //#endif // _WIN32_WINNT >= 0x0500


  /*
  * ACCESSTIMEOUT dwFlags field
  */
  #define ATF_TIMEOUTON                1
  #define ATF_ONOFFFEEDBACK            2

  // values for SOUNDSENTRY iFSGrafEffect field
  #define SSGF_NONE       0
  #define SSGF_DISPLAY    3

  // values for SOUNDSENTRY iFSTextEffect field
  #define SSTF_NONE       0
  #define SSTF_CHARS      1
  #define SSTF_BORDER     2
  #define SSTF_DISPLAY    3

  // values for SOUNDSENTRY iWindowsEffect field
  #define SSWF_NONE     0
  #define SSWF_TITLE    1
  #define SSWF_WINDOW   2
  #define SSWF_DISPLAY  3
  #define SSWF_CUSTOM   4


  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE

  /*
  * SOUNDSENTRY dwFlags field
  */
  #define SSF_SOUNDSENTRYON            1
  #define SSF_AVAILABLE                2
  #define SSF_INDICATOR                4


  /*
  * TOGGLEKEYS dwFlags field
  */
  #define TKF_TOGGLEKEYSON             1
  #define TKF_AVAILABLE                2
  #define TKF_HOTKEYACTIVE             4
  #define TKF_CONFIRMHOTKEY            8
  #define TKF_HOTKEYSOUND             16
  #define TKF_INDICATOR               32

  /*
  * Set debug level
  */


  /*
  * SetLastErrorEx() types.
  */

  #define SLE_ERROR                1
  #define SLE_MINORERROR           2
  #define SLE_WARNING              3


  //#if(WINVER >=   1280)
  //
  ///*
  // * Multimonitor API.
  // */
  //
  #define MONITOR_DEFAULTTONULL                0
  #define MONITOR_DEFAULTTOPRIMARY             1
  #define MONITOR_DEFAULTTONEAREST             2
  //
  //
  //#define MONITORINFOF_PRIMARY                 1
  //
  //#ifndef CCHDEVICENAME
  //#define CCHDEVICENAME 32
  //#endif


  //#ifdef __cplusplus
  //
  //
  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE
  //#else // ndef __cplusplus
  //
  //
  //#ifdef UNICODE
  //
  //
  //#else
  //
  //
  //#endif // UNICODE
  //#endif
  //

  //#ifdef UNICODE
  //#define GetMonitorInfo  GetMonitorInfoW
  //#else
  //#define GetMonitorInfo  GetMonitorInfoA
  //#endif // !UNICODE


  #ifndef NOWINABLE

    /*
    * WinEvents - Active Accessibility hooks
    */


    /*
    * dwFlags for SetWinEventHook
    */
    #define WINEVENT_OUTOFCONTEXT        0  // Events are ASYNC
    #define WINEVENT_SKIPOWNTHREAD       1  // Don't call back for events on installer's thread
    #define WINEVENT_SKIPOWNPROCESS      2  // Don't call back for events on installer's process
    #define WINEVENT_INCONTEXT           4  // Events are SYNC, this causes your dll to be injected into every process


    /*
    * idObject values for WinEventProc and NotifyWinEvent
    */

    /*
    * hwnd + idObject can be used with OLEACC.DLL's OleGetObjectFromWindow()
    * to get an interface pointer to the container.  indexChild is the item
    * within the container in question.  Setup a VARIANT with vt VT_I4 and
    * lVal the indexChild and pass that in to all methods.  Then you
    * are raring to go.
    */


    /*
    * Common object IDs (cookies, only for sending WM_GETOBJECT to get at the
    * thing in question).  Positive IDs are reserved for apps (app specific),
    * negative IDs are system things and are global, 0 means "just little old
    * me".
    */
    #define     CHILDID_SELF        0
    #define     INDEXID_OBJECT      0
    #define     INDEXID_CONTAINER   0

    /*
    * Reserved IDs for system objects
    */
    #define     OBJID_WINDOW                 0
    #define     OBJID_SYSMENU       4294967295
    #define     OBJID_TITLEBAR      4294967294
    #define     OBJID_MENU          4294967293
    #define     OBJID_CLIENT        4294967292
    #define     OBJID_VSCROLL       4294967291
    #define     OBJID_HSCROLL       4294967290
    #define     OBJID_SIZEGRIP      4294967289
    #define     OBJID_CARET         4294967288
    #define     OBJID_CURSOR        4294967287
    #define     OBJID_ALERT         4294967286
    #define     OBJID_SOUND         4294967285
    #define     OBJID_QUERYCLASSNAMEIDX 4294967284
    #define     OBJID_NATIVEOM      4294967280

    /*
    * EVENT DEFINITION
    */
    #define EVENT_MIN                    1
    #define EVENT_MAX           2147483647


    /*
    *  EVENT_SYSTEM_SOUND
    *  Sent when a sound is played.  Currently nothing is generating this, we
    *  this event when a system sound (for menus, etc) is played.  Apps
    *  generate this, if accessible, when a private sound is played.  For
    *  example, if Mail plays a "New Mail" sound.
    *
    *  System Sounds:
    *  (Generated by PlaySoundEvent in USER itself)
    *      hwnd            is NULL
    *      idObject        is OBJID_SOUND
    *      idChild         is sound child ID if one
    *  App Sounds:
    *  (PlaySoundEvent won't generate notification; up to app)
    *      hwnd + idObject gets interface pointer to Sound object
    *      idChild identifies the sound in question
    *  are going to be cleaning up the SOUNDSENTRY feature in the control panel
    *  and will use this at that time.  Applications implementing WinEvents
    *  are perfectly welcome to use it.  Clients of IAccessible* will simply
    *  turn around and get back a non-visual object that describes the sound.
    */
    #define EVENT_SYSTEM_SOUND                   1

    /*
    * EVENT_SYSTEM_ALERT
    * System Alerts:
    * (Generated by MessageBox() calls for example)
    *      hwnd            is hwndMessageBox
    *      idObject        is OBJID_ALERT
    * App Alerts:
    * (Generated whenever)
    *      hwnd+idObject gets interface pointer to Alert
    */
    #define EVENT_SYSTEM_ALERT                   2

    /*
    * EVENT_SYSTEM_FOREGROUND
    * Sent when the foreground (active) window changes, even if it is changing
    * to another window in the same thread as the previous one.
    *      hwnd            is hwndNewForeground
    *      idObject        is OBJID_WINDOW
    *      idChild    is INDEXID_OBJECT
    */
    #define EVENT_SYSTEM_FOREGROUND              3

    /*
    * Menu
    *      hwnd            is window (top level window or popup menu window)
    *      idObject        is ID of control (OBJID_MENU, OBJID_SYSMENU, OBJID_SELF for popup)
    *      idChild         is CHILDID_SELF
    *
    * EVENT_SYSTEM_MENUSTART
    * EVENT_SYSTEM_MENUEND
    * For MENUSTART, hwnd+idObject+idChild refers to the control with the menu bar,
    *  or the control bringing up the context menu.
    *
    * Sent when entering into and leaving from menu mode (system, app bar, and
    * track popups).
    */
    #define EVENT_SYSTEM_MENUSTART               4
    #define EVENT_SYSTEM_MENUEND                 5

    /*
    * EVENT_SYSTEM_MENUPOPUPSTART
    * EVENT_SYSTEM_MENUPOPUPEND
    * Sent when a menu popup comes up and just before it is taken down.  Note
    * that for a call to TrackPopupMenu(), a client will see EVENT_SYSTEM_MENUSTART
    * followed almost immediately by EVENT_SYSTEM_MENUPOPUPSTART for the popup
    * being shown.
    *
    * For MENUPOPUP, hwnd+idObject+idChild refers to the NEW popup coming up, not the
    * parent item which is hierarchical.  You can get the parent menu/popup by
    * asking for the accParent object.
    */
    #define EVENT_SYSTEM_MENUPOPUPSTART          6
    #define EVENT_SYSTEM_MENUPOPUPEND            7


    /*
    * EVENT_SYSTEM_CAPTURESTART
    * EVENT_SYSTEM_CAPTUREEND
    * Sent when a window takes the capture and releases the capture.
    */
    #define EVENT_SYSTEM_CAPTURESTART            8
    #define EVENT_SYSTEM_CAPTUREEND              9

    /*
    * Move Size
    * EVENT_SYSTEM_MOVESIZESTART
    * EVENT_SYSTEM_MOVESIZEEND
    * Sent when a window enters and leaves move-size dragging mode.
    */
    #define EVENT_SYSTEM_MOVESIZESTART          10
    #define EVENT_SYSTEM_MOVESIZEEND            11

    /*
    * Context Help
    * EVENT_SYSTEM_CONTEXTHELPSTART
    * EVENT_SYSTEM_CONTEXTHELPEND
    * Sent when a window enters and leaves context sensitive help mode.
    */
    #define EVENT_SYSTEM_CONTEXTHELPSTART       12
    #define EVENT_SYSTEM_CONTEXTHELPEND         13

    /*
    * Drag & Drop
    * EVENT_SYSTEM_DRAGDROPSTART
    * EVENT_SYSTEM_DRAGDROPEND
    * Send the START notification just before going into drag&drop loop.  Send
    * the END notification just after canceling out.
    * Note that it is up to apps and OLE to generate this, since the system
    * doesn't know.  Like EVENT_SYSTEM_SOUND, it will be a while before this
    * is prevalent.
    */
    #define EVENT_SYSTEM_DRAGDROPSTART          14
    #define EVENT_SYSTEM_DRAGDROPEND            15

    /*
    * Dialog
    * Send the START notification right after the dialog is completely
    *  initialized and visible.  Send the END right before the dialog
    *  is hidden and goes away.
    * EVENT_SYSTEM_DIALOGSTART
    * EVENT_SYSTEM_DIALOGEND
    */
    #define EVENT_SYSTEM_DIALOGSTART            16
    #define EVENT_SYSTEM_DIALOGEND              17

    /*
    * EVENT_SYSTEM_SCROLLING
    * EVENT_SYSTEM_SCROLLINGSTART
    * EVENT_SYSTEM_SCROLLINGEND
    * Sent when beginning and ending the tracking of a scrollbar in a window,
    * and also for scrollbar controls.
    */
    #define EVENT_SYSTEM_SCROLLINGSTART         18
    #define EVENT_SYSTEM_SCROLLINGEND           19

    /*
    * Alt-Tab Window
    * Send the START notification right after the switch window is initialized
    * and visible.  Send the END right before it is hidden and goes away.
    * EVENT_SYSTEM_SWITCHSTART
    * EVENT_SYSTEM_SWITCHEND
    */
    #define EVENT_SYSTEM_SWITCHSTART            20
    #define EVENT_SYSTEM_SWITCHEND              21

    /*
    * EVENT_SYSTEM_MINIMIZESTART
    * EVENT_SYSTEM_MINIMIZEEND
    * Sent when a window minimizes and just before it restores.
    */
    #define EVENT_SYSTEM_MINIMIZESTART          22
    #define EVENT_SYSTEM_MINIMIZEEND            23


    /*
    * Object events
    *
    * The system AND apps generate these.  The system generates these for
    * real windows.  Apps generate these for objects within their window which
    * act like a separate control, e.g. an item in a list view.
    *
    * When the system generate them, dwParam2 is always WMOBJID_SELF.  When
    * apps generate them, apps put the has-meaning-to-the-app-only ID value
    * in dwParam2.
    * For all events, if you want detailed accessibility information, callers
    * should
    *      * Call AccessibleObjectFromWindow() with the hwnd, idObject parameters
    *          of the event, and IID_IAccessible as the REFIID, to get back an
    *          IAccessible* to talk to
    *      * Initialize and fill in a VARIANT as VT_I4 with lVal the idChild
    *          parameter of the event.
    *      * If idChild isn't zero, call get_accChild() in the container to see
    *          if the child is an object in its own right.  If so, you will get
    *          back an IDispatch* object for the child.  You should release the
    *          parent, and call QueryInterface() on the child object to get its
    *          IAccessible*.  Then you talk directly to the child.  Otherwise,
    *          if get_accChild() returns you nothing, you should continue to
    *          use the child VARIANT.  You will ask the container for the properties
    *          of the child identified by the VARIANT.  In other words, the
    *          child in this case is accessible but not a full-blown object.
    *          Like a button on a titlebar which is 'small' and has no children.
    */

    /*
    * For all EVENT_OBJECT events,
    *      hwnd is the dude to Send the WM_GETOBJECT message to (unless NULL,
    *          see above for system things)
    *      idObject is the ID of the object that can resolve any queries a
    *          client might have.  It's a way to deal with windowless controls,
    *          controls that are just drawn on the screen in some larger parent
    *          window (like SDM), or standard frame elements of a window.
    *      idChild is the piece inside of the object that is affected.  This
    *          allows clients to access things that are too small to have full
    *          blown objects in their own right.  Like the thumb of a scrollbar.
    *          The hwnd/idObject pair gets you to the container, the dude you
    *          probably want to talk to most of the time anyway.  The idChild
    *          can then be passed into the acc properties to get the name/value
    *          of it as needed.
    *
    * Example #1:
    *      System propagating a listbox selection change
    *      EVENT_OBJECT_SELECTION
    *          hwnd == listbox hwnd
    *          idObject == OBJID_WINDOW
    *          idChild == new selected item, or CHILDID_SELF if
    *              nothing now selected within container.
    *      Word '97 propagating a listbox selection change
    *          hwnd == SDM window
    *          idObject == SDM ID to get at listbox 'control'
    *          idChild == new selected item, or CHILDID_SELF if
    *              nothing
    *
    * Example #2:
    *      System propagating a menu item selection on the menu bar
    *      EVENT_OBJECT_SELECTION
    *          hwnd == top level window
    *          idObject == OBJID_MENU
    *          idChild == ID of child menu bar item selected
    *
    * Example #3:
    *      System propagating a dropdown coming off of said menu bar item
    *      EVENT_OBJECT_CREATE
    *          hwnd == popup item
    *          idObject == OBJID_WINDOW
    *          idChild == CHILDID_SELF
    *
    * Example #4:
    *
    * For EVENT_OBJECT_REORDER, the object referred to by hwnd/idObject is the
    * PARENT container in which the zorder is occurring.  This is because if
    * one child is zordering, all of them are changing their relative zorder.
    */
    #define EVENT_OBJECT_CREATE                  32768  // hwnd + ID + idChild is created item
    #define EVENT_OBJECT_DESTROY                 32769  // hwnd + ID + idChild is destroyed item
    #define EVENT_OBJECT_SHOW                    32770  // hwnd + ID + idChild is shown item
    #define EVENT_OBJECT_HIDE                    32771  // hwnd + ID + idChild is hidden item
    #define EVENT_OBJECT_REORDER                 32772  // hwnd + ID + idChild is parent of zordering children
    /*
    * NOTE:
    * Minimize the number of notifications!
    *
    * When you are hiding a parent object, obviously all child objects are no
    * longer visible on screen.  They still have the same "visible" status,
    * but are not truly visible.  Hence do not send HIDE notifications for the
    * children also.  One implies all.  The same goes for SHOW.
    */


    #define EVENT_OBJECT_FOCUS                   32773  // hwnd + ID + idChild is focused item
    #define EVENT_OBJECT_SELECTION               32774  // hwnd + ID + idChild is selected item (if only one), or idChild is OBJID_WINDOW if complex
    #define EVENT_OBJECT_SELECTIONADD            32775  // hwnd + ID + idChild is item added
    #define EVENT_OBJECT_SELECTIONREMOVE         32776  // hwnd + ID + idChild is item removed
    #define EVENT_OBJECT_SELECTIONWITHIN         32777  // hwnd + ID + idChild is parent of changed selected items

    /*
    * NOTES:
    * There is only one "focused" child item in a parent.  This is the place
    * keystrokes are going at a given moment.  Hence only send a notification
    * about where the NEW focus is going.  A NEW item getting the focus already
    * implies that the OLD item is losing it.
    *
    * SELECTION however can be multiple.  Hence the different SELECTION
    * notifications.  Here's when to use each:
    *
    * (1) Send a SELECTION notification in the simple single selection
    *     case (like the focus) when the item with the selection is
    *     merely moving to a different item within a container.  hwnd + ID
    *     is the container control, idChildItem is the new child with the
    *     selection.
    *
    * (2) Send a SELECTIONADD notification when a new item has simply been added
    *     to the selection within a container.  This is appropriate when the
    *     number of newly selected items is very small.  hwnd + ID is the
    *     container control, idChildItem is the new child added to the selection.
    *
    * (3) Send a SELECTIONREMOVE notification when a new item has simply been
    *     removed from the selection within a container.  This is appropriate
    *     when the number of newly selected items is very small, just like
    *     SELECTIONADD.  hwnd + ID is the container control, idChildItem is the
    *     new child removed from the selection.
    *
    * (4) Send a SELECTIONWITHIN notification when the selected items within a
    *     control have changed substantially.  Rather than propagate a large
    *     number of changes to reflect removal for some items, addition of
    *     others, just tell somebody who cares that a lot happened.  It will
    *     be faster an easier for somebody watching to just turn around and
    *     query the container control what the new bunch of selected items
    *     are.
    */

    #define EVENT_OBJECT_STATECHANGE             32778  // hwnd + ID + idChild is item w/ state change
    /*
    * Examples of when to send an EVENT_OBJECT_STATECHANGE include
    *      * It is being enabled/disabled (USER does for windows)
    *      * It is being pressed/released (USER does for buttons)
    *      * It is being checked/unchecked (USER does for radio/check buttons)
    */
    #define EVENT_OBJECT_LOCATIONCHANGE          32779  // hwnd + ID + idChild is moved/sized item

    /*
    * Note:
    * A LOCATIONCHANGE is not sent for every child object when the parent
    * changes shape/moves.  Send one notification for the topmost object
    * that is changing.  For example, if the user resizes a top level window,
    * USER will generate a LOCATIONCHANGE for it, but not for the menu bar,
    * title bar, scrollbars, etc.  that are also changing shape/moving.
    *
    * In other words, it only generates LOCATIONCHANGE notifications for
    * real windows that are moving/sizing.  It will not generate a LOCATIONCHANGE
    * for every non-floating child window when the parent moves (the children are
    * logically moving also on screen, but not relative to the parent).
    *
    * Now, if the app itself resizes child windows as a result of being
    * sized, USER will generate LOCATIONCHANGEs for those dudes also because
    * it doesn't know better.
    *
    * Note also that USER will generate LOCATIONCHANGE notifications for two
    * non-window sys objects:
    *      (1) System caret
    *      (2) Cursor
    */

    #define EVENT_OBJECT_NAMECHANGE              32780  // hwnd + ID + idChild is item w/ name change
    #define EVENT_OBJECT_DESCRIPTIONCHANGE       32781  // hwnd + ID + idChild is item w/ desc change
    #define EVENT_OBJECT_VALUECHANGE             32782  // hwnd + ID + idChild is item w/ value change
    #define EVENT_OBJECT_PARENTCHANGE            32783  // hwnd + ID + idChild is item w/ new parent
    #define EVENT_OBJECT_HELPCHANGE              32784  // hwnd + ID + idChild is item w/ help change
    #define EVENT_OBJECT_DEFACTIONCHANGE         32785  // hwnd + ID + idChild is item w/ def action change
    #define EVENT_OBJECT_ACCELERATORCHANGE       32786  // hwnd + ID + idChild is item w/ keybd accel change

    /*
    * Child IDs
    */

    /*
    * System Sounds (idChild of system SOUND notification)
    */
    #define SOUND_SYSTEM_STARTUP            1
    #define SOUND_SYSTEM_SHUTDOWN           2
    #define SOUND_SYSTEM_BEEP               3
    #define SOUND_SYSTEM_ERROR              4
    #define SOUND_SYSTEM_QUESTION           5
    #define SOUND_SYSTEM_WARNING            6
    #define SOUND_SYSTEM_INFORMATION        7
    #define SOUND_SYSTEM_MAXIMIZE           8
    #define SOUND_SYSTEM_MINIMIZE           9
    #define SOUND_SYSTEM_RESTOREUP          10
    #define SOUND_SYSTEM_RESTOREDOWN        11
    #define SOUND_SYSTEM_APPSTART           12
    #define SOUND_SYSTEM_FAULT              13
    #define SOUND_SYSTEM_APPEND             14
    #define SOUND_SYSTEM_MENUCOMMAND        15
    #define SOUND_SYSTEM_MENUPOPUP          16
    #define CSOUND_SYSTEM                   16

    /*
    * System Alerts (indexChild of system ALERT notification)
    */
    #define ALERT_SYSTEM_INFORMATIONAL      1       // MB_INFORMATION
    #define ALERT_SYSTEM_WARNING            2       // MB_WARNING
    #define ALERT_SYSTEM_ERROR              3       // MB_ERROR
    #define ALERT_SYSTEM_QUERY              4       // MB_QUESTION
    #define ALERT_SYSTEM_CRITICAL           5       // HardSysErrBox
    #define CALERT_SYSTEM                   6


    #define GUI_CARETBLINKING            1
    #define GUI_INMOVESIZE               2
    #define GUI_INMENUMODE               4
    #define GUI_SYSTEMMENUMODE           8
    #define GUI_POPUPMENUMODE           16


    //#ifdef UNICODE
    //#define GetWindowModuleFileName  GetWindowModuleFileNameW
    //#else
    //#define GetWindowModuleFileName  GetWindowModuleFileNameA
    //#endif // !UNICODE

    #ifndef NO_STATE_FLAGS
      #define STATE_SYSTEM_UNAVAILABLE                 1  // Disabled
      #define STATE_SYSTEM_SELECTED                    2
      #define STATE_SYSTEM_FOCUSED                     4
      #define STATE_SYSTEM_PRESSED                     8
      #define STATE_SYSTEM_CHECKED                    16
      #define STATE_SYSTEM_MIXED                      32  // 3-state checkbox or toolbar button
      #define STATE_SYSTEM_INDETERMINATE      STATE_SYSTEM_MIXED
      #define STATE_SYSTEM_READONLY                   64
      #define STATE_SYSTEM_HOTTRACKED                128
      #define STATE_SYSTEM_DEFAULT                   256
      #define STATE_SYSTEM_EXPANDED                  512
      #define STATE_SYSTEM_COLLAPSED                1024
      #define STATE_SYSTEM_BUSY                     2048
      #define STATE_SYSTEM_FLOATING                 4096  // Children "owned" not "contained" by parent
      #define STATE_SYSTEM_MARQUEED                 8192
      #define STATE_SYSTEM_ANIMATED                16384
      #define STATE_SYSTEM_INVISIBLE               32768
      #define STATE_SYSTEM_OFFSCREEN               65536
      #define STATE_SYSTEM_SIZEABLE               131072
      #define STATE_SYSTEM_MOVEABLE               262144
      #define STATE_SYSTEM_SELFVOICING            524288
      #define STATE_SYSTEM_FOCUSABLE             1048576
      #define STATE_SYSTEM_SELECTABLE            2097152
      #define STATE_SYSTEM_LINKED                4194304
      #define STATE_SYSTEM_TRAVERSED             8388608
      #define STATE_SYSTEM_MULTISELECTABLE      16777216  // Supports multiple selection
      #define STATE_SYSTEM_EXTSELECTABLE        33554432  // Supports extended selection
      #define STATE_SYSTEM_ALERT_LOW            67108864  // This information is of low priority
      #define STATE_SYSTEM_ALERT_MEDIUM        134217728  // This information is of medium priority
      #define STATE_SYSTEM_ALERT_HIGH          268435456  // This information is of high priority
      #define STATE_SYSTEM_PROTECTED           536870912  // access to this is restricted
      #define STATE_SYSTEM_VALID              1073741823
    #endif

    #define CCHILDREN_TITLEBAR              5
    #define CCHILDREN_SCROLLBAR             5



    /* version */
     #define VER_PLATFORM_WIN32S                                          0
    #define VER_PLATFORM_WIN32_WINDOWS                          1
    #define VER_PLATFORM_WIN32_NT                                       2

    #define VER_SERVER_NT                                     0x80000000
    #define VER_WORKSTATION_NT                  0x40000000

    #define VER_NT_WORKSTATION                  0x00000001
    #define VER_NT_DOMAIN_CONTROLLER            0x00000002
    #define VER_NT_SERVER                       0x00000003

    #define VER_SUITE_SMALLBUSINESS             0x00000001
    #define VER_SUITE_ENTERPRISE                0x00000002
    #define VER_SUITE_BACKOFFICE                0x00000004
    #define VER_SUITE_COMMUNICATIONS            0x00000008
    #define VER_SUITE_TERMINAL                  0x00000010
    #define VER_SUITE_SMALLBUSINESS_RESTRICTED  0x00000020
    #define VER_SUITE_EMBEDDEDNT                0x00000040
    #define VER_SUITE_DATACENTER                0x00000080
    #define VER_SUITE_SINGLEUSERTS              0x00000100
    #define VER_SUITE_PERSONAL                  0x00000200
    #define VER_SUITE_BLADE                     0x00000400


    /*
    * Information about the global cursor.
    */


    #define CURSOR_SHOWING              1


    /*
    * Window information snapshot
    */


    #define WS_ACTIVECAPTION         1


    /*
    * Titlebar information.
    */


    /*
    * Menubar information
    */


                 // rect of bar, popup, item
                 // real menu handle of bar, popup
              // hwnd of item submenu if one
         // bar, popup has the focus
            // item has the focus


    /*
    * Scrollbar information
    */


    /*
    * Combobox information
    */


    /*
    * The "real" ancestor window
    */
    #define     GA_PARENT       1
    #define     GA_ROOT         2
    #define     GA_ROOTOWNER    3


    /*
    * This gets the REAL child window at the point.  If it is in the dead
    * space of a group box, it will try a sibling behind it.  But static
    * fields will get returned.  In other words, it is kind of a cross between
    * ChildWindowFromPointEx and WindowFromPoint.
    */


    /*
    * This gets the name of the window TYPE, not class.  This allows us to
    * recognize ThunderButton32 et al.
    */


    /*
    * This gets the name of the window TYPE, not class.  This allows us to
    * recognize ThunderButton32 et al.
    */


    //#ifdef UNICODE
    //#define RealGetWindowClass  RealGetWindowClassW
    //#else
    //#define RealGetWindowClass  RealGetWindowClassA
    //#endif // !UNICODE

    /*
    * Alt-Tab Switch window information.
    */


    //#ifdef UNICODE
    //#define GetAltTabInfo  GetAltTabInfoW
    //#else
    //#define GetAltTabInfo  GetAltTabInfoA
    //#endif // !UNICODE

    /*
    * Listbox information.
    * Returns the number of items per row.
    */

  #endif // NOWINABLE
  //#endif // WINVER >= 0x0500


  //#if(_WIN32_WINNT >=   1280)
  //
  //
  //#endif // _WIN32_WINNT >= 0x0500
  //
  //#if(_WIN32_WINNT >=   1280)
  //
  //
  //#endif // _WIN32_WINNT >= 0x0500
  //
  //
  //#ifdef __cplusplus
  //
  //#endif  // __cplusplus

 //P_O_Pop
#endif // !_WINUSER_
