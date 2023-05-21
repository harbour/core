/*
    gtnap bindings to use in Harbour programs
    TODO: More info
*/

/* Return codes for NAP_WINDOW_MODAL() */

#define NAP_MODAL_ESC               1

#define NAP_MODAL_ENTER             2

#define NAP_MODAL_X_BUTTON          3

#define NAP_MODAL_BUTTON_AUTOCLOSE  1000    /* + index of button >= 1 */

#define NAP_MODAL_HOTKEY_AUTOCLOSE  2000    /* + vkey_t use NAP_INKEY(ret - NAP_MODAL_HOTKEY_AUTOCLOSE)*/

#define NAP_MODAL_IMAGE_AUTOCLOSE   3000    /* + index of image >= 1 */

#define NAP_MODAL_MENU_AUTOCLOSE    4000    /* + index of option >= 1 */

#define NAP_MODAL_TEXT_CONFIRM      5000

#define NAP_MAX_IMAGES              50

#define NAP_MAX_BUTTONS             50

#define NAP_MAX_VKEY                112


// // https://nappgui.com/en/draw2d/draw2d.html

// // enum fstyle_t
// #define ekNAP_FSTYLE_NORMAL         0
// #define ekNAP_FSTYLE_BOLD           1
// #define ekNAP_FSTYLE_ITALIC         2
// #define ekNAP_FSTYLE_STRIKEOUT      4
// #define ekNAP_FSTYLE_UNDERLINE      8
// #define ekNAP_FSTYLE_SUBSCRIPT      16
// #define ekNAP_FSTYLE_SUPSCRIPT      32
// #define ekNAP_FSTYLE_PIXELS         0
// #define ekNAP_FSTYLE_POINTS         64

// // enum align_t
// #define ekNAP_ALIGN_LEFT            1
// #define ekNAP_ALIGN_TOP             (ekNAP_ALIGN_LEFT)
// #define ekNAP_ALIGN_CENTER          (ekNAP_ALIGN_LEFT + 1)
// #define ekNAP_ALIGN_RIGHT           (ekNAP_ALIGN_LEFT + 2)
// #define ekNAP_ALIGN_BOTTOM          (ekNAP_ALIGN_LEFT + 2)
// #define ekNAP_ALIGN_JUSTIFY         (ekNAP_ALIGN_LEFT + 3)

// // https://nappgui.com/en/gui/gui.html

// // enum orient_t
// #define ekNAP_ORIENT_HORIZONTAL     1
// #define ekNAP_ORIENT_VERTICAL       (ekNAP_ORIENT_HORIZONTAL + 1)

// // enum state_t
// #define ekNAP_STATE_OFF             0
// #define ekNAP_STATE_ON              (ekNAP_STATE_OFF + 1)
// #define ekNAP_STATE_MIXED           (ekNAP_STATE_OFF + 2)

// // enum scale_t
// #define ekNAP_SCALE_AUTO            1
// #define ekNAP_SCALE_SNONE           (ekNAP_SCALE_AUTO + 1)
// #define ekNAP_SCALE_ASPECT          (ekNAP_SCALE_AUTO + 2)
// #define ekNAP_SCALE_ASPECTDW        (ekNAP_SCALE_AUTO + 3)

// // enum window_flag_t
// #define ekNAP_WINDOW_FLAG            0
// #define ekNAP_WINDOW_EDGE            1
// #define ekNAP_WINDOW_TITLE           2
// #define ekNAP_WINDOW_MAX             4
// #define ekNAP_WINDOW_MIN             8
// #define ekNAP_WINDOW_CLOSE           16
// #define ekNAP_WINDOW_RES             32
// #define ekNAP_WINDOW_RETURN          64
// #define ekNAP_WINDOW_ESC             128
// #define ekNAP_WINDOW_STD             (ekNAP_WINDOW_TITLE + ekNAP_WINDOW_MIN + ekNAP_WINDOW_CLOSE)
// #define ekNAP_WINDOW_SRES            (ekNAP_WINDOW_STD + ekNAP_WINDOW_MAX + ekNAP_WINDOW_RES)

// // enum event_t
// #define ekNAP_EVLABEL               0x400
// #define ekNAP_EVBUTTON              (ekNAP_EVLABEL + 1)
// #define ekNAP_EVPOPUP               (ekNAP_EVLABEL + 2)
// #define ekNAP_EVLISTBOX             (ekNAP_EVLABEL + 3)
// #define ekNAP_EVSLIDER              (ekNAP_EVLABEL + 4)
// #define ekNAP_EVUPDOWN              (ekNAP_EVLABEL + 5)
// #define ekNAP_EVTXTFILTER           (ekNAP_EVLABEL + 6)
// #define ekNAP_EVTXTCHANGE           (ekNAP_EVLABEL + 7)
// #define ekNAP_EVFOCUS               (ekNAP_EVLABEL + 8)
// #define ekNAP_EVMENU                (ekNAP_EVLABEL + 9)
// #define ekNAP_EVDRAW                (ekNAP_EVLABEL + 10)
// #define ekNAP_EVRESIZE              (ekNAP_EVLABEL + 11)
// #define ekNAP_EVENTER               (ekNAP_EVLABEL + 12)
// #define ekNAP_EVEXIT                (ekNAP_EVLABEL + 13)
// #define ekNAP_EVMOVED               (ekNAP_EVLABEL + 14)
// #define ekNAP_EVDOWN                (ekNAP_EVLABEL + 15)
// #define ekNAP_EVUP                  (ekNAP_EVLABEL + 16)
// #define ekNAP_EVCLICK               (ekNAP_EVLABEL + 17)
// #define ekNAP_EVDRAG                (ekNAP_EVLABEL + 18)
// #define ekNAP_EVWHEEL               (ekNAP_EVLABEL + 19)
// #define ekNAP_EVKEYDOWN             (ekNAP_EVLABEL + 20)
// #define ekNAP_EVKEYUP               (ekNAP_EVLABEL + 21)
// #define ekNAP_EVWNDMOVED            (ekNAP_EVLABEL + 22)
// #define ekNAP_EVWNDSIZING           (ekNAP_EVLABEL + 23)
// #define ekNAP_EVWNDSIZE             (ekNAP_EVLABEL + 24)
// #define ekNAP_EVWNDCLOSE            (ekNAP_EVLABEL + 25)
// #define ekNAP_EVCOLOR               (ekNAP_EVLABEL + 26)
// #define ekNAP_EVTHEME               (ekNAP_EVLABEL + 27)
// #define ekNAP_EVOBJCHANGE           (ekNAP_EVLABEL + 28)

// // enum vkey_t
// #define ekNAP_KEY_UNDEF             0
// #define ekNAP_KEY_A                 1
// #define ekNAP_KEY_S                 2
// #define ekNAP_KEY_D                 3
// #define ekNAP_KEY_F                 4
// #define ekNAP_KEY_H                 5
// #define ekNAP_KEY_G                 6
// #define ekNAP_KEY_Z                 7
// #define ekNAP_KEY_X                 8
// #define ekNAP_KEY_C                 9
// #define ekNAP_KEY_V                 10
// #define ekNAP_KEY_BSLASH            11
// #define ekNAP_KEY_B                 12
// #define ekNAP_KEY_Q                 13
// #define ekNAP_KEY_W                 14
// #define ekNAP_KEY_E                 15
// #define ekNAP_KEY_R                 16
// #define ekNAP_KEY_Y                 17
// #define ekNAP_KEY_T                 18
// #define ekNAP_KEY_1                 19
// #define ekNAP_KEY_2                 20
// #define ekNAP_KEY_3                 21
// #define ekNAP_KEY_4                 22
// #define ekNAP_KEY_6                 23
// #define ekNAP_KEY_5                 24
// #define ekNAP_KEY_9                 25
// #define ekNAP_KEY_7                 26
// #define ekNAP_KEY_8                 27
// #define ekNAP_KEY_0                 28
// #define ekNAP_KEY_RCURLY            29
// #define ekNAP_KEY_O                 30
// #define ekNAP_KEY_U                 31
// #define ekNAP_KEY_LCURLY            32
// #define ekNAP_KEY_I                 33
// #define ekNAP_KEY_P                 34
// #define ekNAP_KEY_RETURN            35
// #define ekNAP_KEY_L                 36
// #define ekNAP_KEY_J                 37
// #define ekNAP_KEY_SEMICOLON         38
// #define ekNAP_KEY_K                 39
// #define ekNAP_KEY_QUEST             40
// #define ekNAP_KEY_COMMA             41
// #define ekNAP_KEY_MINUS             42
// #define ekNAP_KEY_N                 43
// #define ekNAP_KEY_M                 44
// #define ekNAP_KEY_PERIOD            45
// #define ekNAP_KEY_TAB               46
// #define ekNAP_KEY_SPACE             47
// #define ekNAP_KEY_GTLT              48
// #define ekNAP_KEY_BACK              49
// #define ekNAP_KEY_ESCAPE            50
// #define ekNAP_KEY_F17               51
// #define ekNAP_KEY_NUMDECIMAL        52
// #define ekNAP_KEY_NUMMULT           53
// #define ekNAP_KEY_NUMADD            54
// #define ekNAP_KEY_NUMLOCK           55
// #define ekNAP_KEY_NUMDIV            56
// #define ekNAP_KEY_NUMRET            57
// #define ekNAP_KEY_NUMMINUS          58
// #define ekNAP_KEY_F18               59
// #define ekNAP_KEY_F19               60
// #define ekNAP_KEY_NUMEQUAL          61
// #define ekNAP_KEY_NUM0              62
// #define ekNAP_KEY_NUM1              63
// #define ekNAP_KEY_NUM2              64
// #define ekNAP_KEY_NUM3              65
// #define ekNAP_KEY_NUM4              66
// #define ekNAP_KEY_NUM5              67
// #define ekNAP_KEY_NUM6              68
// #define ekNAP_KEY_NUM7              69
// #define ekNAP_KEY_NUM8              70
// #define ekNAP_KEY_NUM9              71
// #define ekNAP_KEY_F5                72
// #define ekNAP_KEY_F6                73
// #define ekNAP_KEY_F7                74
// #define ekNAP_KEY_F3                75
// #define ekNAP_KEY_F8                76
// #define ekNAP_KEY_F9                77
// #define ekNAP_KEY_F11               78
// #define ekNAP_KEY_F13               79
// #define ekNAP_KEY_F16               80
// #define ekNAP_KEY_F14               81
// #define ekNAP_KEY_F10               82
// #define ekNAP_KEY_F12               83
// #define ekNAP_KEY_F15               84
// #define ekNAP_KEY_PAGEUP            85
// #define ekNAP_KEY_HOME              86
// #define ekNAP_KEY_SUPR              87
// #define ekNAP_KEY_F4                88
// #define ekNAP_KEY_PAGEDOWN          89
// #define ekNAP_KEY_F2                90
// #define ekNAP_KEY_END               91
// #define ekNAP_KEY_F1                92
// #define ekNAP_KEY_LEFT              93
// #define ekNAP_KEY_RIGHT             94
// #define ekNAP_KEY_DOWN              95
// #define ekNAP_KEY_UP                96
// #define ekNAP_KEY_LSHIFT            97
// #define ekNAP_KEY_RSHIFT            98
// #define ekNAP_KEY_LCTRL             99
// #define ekNAP_KEY_RCTRL             100
// #define ekNAP_KEY_LALT              101
// #define ekNAP_KEY_RALT              102
// #define ekNAP_KEY_INSERT            103
// #define ekNAP_KEY_EXCLAM            104
// #define ekNAP_KEY_MENU              105
// #define ekNAP_KEY_LWIN              106
// #define ekNAP_KEY_RWIN              107
// #define ekNAP_KEY_CAPS              108
// #define ekNAP_KEY_TILDE             109
// #define ekNAP_KEY_GRAVE             110
// #define ekNAP_KEY_PLUS              111

// // enum mkey_t
// #define ekNAP_MKEY_NONE             0
// #define ekNAP_MKEY_SHIFT            1
// #define ekNAP_MKEY_CONTROL          2
// #define ekNAP_MKEY_ALT              4
// #define ekNAP_MKEY_COMMAND          8


