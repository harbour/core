/*
    gtnap bindings to use in Harbour programs
    TODO: More info
*/

// https://nappgui.com/en/draw2d/draw2d.html

// enum fstyle_t
#define ekNAP_FSTYLE_NORMAL         0
#define ekNAP_FSTYLE_BOLD           1
#define ekNAP_FSTYLE_ITALIC         2
#define ekNAP_FSTYLE_STRIKEOUT      4
#define ekNAP_FSTYLE_UNDERLINE      8
#define ekNAP_FSTYLE_SUBSCRIPT      16
#define ekNAP_FSTYLE_SUPSCRIPT      32
#define ekNAP_FSTYLE_PIXELS         0
#define ekNAP_FSTYLE_POINTS         64

// enum align_t
#define ekNAP_ALIGN_LEFT            1
#define ekNAP_ALIGN_TOP             (ekNAP_ALIGN_LEFT)
#define ekNAP_ALIGN_CENTER          (ekNAP_ALIGN_LEFT + 1)
#define ekNAP_ALIGN_RIGHT           (ekNAP_ALIGN_LEFT + 2)
#define ekNAP_ALIGN_BOTTOM          (ekNAP_ALIGN_LEFT + 2)
#define ekNAP_ALIGN_JUSTIFY         (ekNAP_ALIGN_LEFT + 3)

// https://nappgui.com/en/gui/gui.html

// enum orient_t
#define ekNAP_ORIENT_HORIZONTAL     1
#define ekNAP_ORIENT_VERTICAL       (ekNAP_ORIENT_HORIZONTAL + 1)

// enum state_t
#define ekNAP_STATE_OFF             0
#define ekNAP_STATE_ON              (ekNAP_STATE_OFF + 1)
#define ekNAP_STATE_MIXED           (ekNAP_STATE_OFF + 2)

// enum scale_t
#define ekNAP_SCALE_AUTO            1
#define ekNAP_SCALE_SNONE           (ekNAP_SCALE_AUTO + 1)
#define ekNAP_SCALE_ASPECT          (ekNAP_SCALE_AUTO + 2)
#define ekNAP_SCALE_ASPECTDW        (ekNAP_SCALE_AUTO + 3)

// enum window_flag_t
#define ekNAP_WINDOW_FLAG            0
#define ekNAP_WINDOW_EDGE            1
#define ekNAP_WINDOW_TITLE           2
#define ekNAP_WINDOW_MAX             4
#define ekNAP_WINDOW_MIN             8
#define ekNAP_WINDOW_CLOSE           16
#define ekNAP_WINDOW_RES             32
#define ekNAP_WINDOW_RETURN          64
#define ekNAP_WINDOW_ESC             128
#define ekNAP_WINDOW_STD             26 //(ekNAP_WINDOW_TITLE | ekNAP_WINDOW_MIN | ekNAP_WINDOW_CLOSE)
#define ekNAP_WINDOW_SRES            (ekNAP_WINDOW_STD | ekNAP_WINDOW_MAX | ekNAP_WINDOW_RES)

// enum event_t
#define ekNAP_EVLABEL               0x400
#define ekNAP_EVBUTTON              (ekNAP_EVLABEL + 1)
#define ekNAP_EVPOPUP               (ekNAP_EVLABEL + 2)
#define ekNAP_EVLISTBOX             (ekNAP_EVLABEL + 3)
#define ekNAP_EVSLIDER              (ekNAP_EVLABEL + 4)
#define ekNAP_EVUPDOWN              (ekNAP_EVLABEL + 5)
#define ekNAP_EVTXTFILTER           (ekNAP_EVLABEL + 6)
#define ekNAP_EVTXTCHANGE           (ekNAP_EVLABEL + 7)
#define ekNAP_EVFOCUS               (ekNAP_EVLABEL + 8)
#define ekNAP_EVMENU                (ekNAP_EVLABEL + 9)
#define ekNAP_EVDRAW                (ekNAP_EVLABEL + 10)
#define ekNAP_EVRESIZE              (ekNAP_EVLABEL + 11)
#define ekNAP_EVENTER               (ekNAP_EVLABEL + 12)
#define ekNAP_EVEXIT                (ekNAP_EVLABEL + 13)
#define ekNAP_EVMOVED               (ekNAP_EVLABEL + 14)
#define ekNAP_EVDOWN                (ekNAP_EVLABEL + 15)
#define ekNAP_EVUP                  (ekNAP_EVLABEL + 16)
#define ekNAP_EVCLICK               (ekNAP_EVLABEL + 17)
#define ekNAP_EVDRAG                (ekNAP_EVLABEL + 18)
#define ekNAP_EVWHEEL               (ekNAP_EVLABEL + 19)
#define ekNAP_EVKEYDOWN             (ekNAP_EVLABEL + 20)
#define ekNAP_EVKEYUP               (ekNAP_EVLABEL + 21)
#define ekNAP_EVWNDMOVED            (ekNAP_EVLABEL + 22)
#define ekNAP_EVWNDSIZING           (ekNAP_EVLABEL + 23)
#define ekNAP_EVWNDSIZE             (ekNAP_EVLABEL + 24)
#define ekNAP_EVWNDCLOSE            (ekNAP_EVLABEL + 25)
#define ekNAP_EVCOLOR               (ekNAP_EVLABEL + 26)
#define ekNAP_EVTHEME               (ekNAP_EVLABEL + 27)
#define ekNAP_EVOBJCHANGE           (ekNAP_EVLABEL + 28)

