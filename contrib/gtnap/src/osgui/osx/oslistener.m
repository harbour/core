/*
 * NAppGUI Cross-platform C SDK
 * 2015-2023 Francisco Garcia Collado
 * MIT Licence
 * https://nappgui.com/en/legal/license.html
 *
 * File: oslistener.m
 *
 */

/* View listeners */

#include "oslistener.inl"
#include "osgui.inl"
#include <core/event.h>
#include <sewer/bmem.h>
#include <sewer/cassert.h>

#if !defined (__MACOS__)
#error This file is only for OSX
#endif

/*---------------------------------------------------------------------------*/

static const unsigned short i_kVIRTUAL_KEY[] =
{
    UINT16_MAX,                 /* ekKEY_UNDEF      = 0 */
    kVK_ANSI_A,                 /* ekKEY_A          = 1 */
    kVK_ANSI_S,                 /* ekKEY_S          = 2 */
    kVK_ANSI_D,                 /* ekKEY_D          = 3 */
    kVK_ANSI_F,                 /* ekKEY_F          = 4 */
    kVK_ANSI_H,                 /* ekKEY_H          = 5 */
    kVK_ANSI_G,                 /* ekKEY_G          = 6 */
    kVK_ANSI_Z,                 /* ekKEY_Z          = 7 */
    kVK_ANSI_X,                 /* ekKEY_X          = 8 */
    kVK_ANSI_C,                 /* ekKEY_C          = 9 */

    kVK_ANSI_V,                 /* ekKEY_V          = 10 */
    0xA,                        /* ekKEY_BSLASH     = 11 */
    kVK_ANSI_B,                 /* ekKEY_B          = 12 */
    kVK_ANSI_Q,                 /* ekKEY_Q          = 13 */
    kVK_ANSI_W,                 /* ekKEY_W          = 14 */
    kVK_ANSI_E,                 /* ekKEY_E          = 15 */
    kVK_ANSI_R,                 /* ekKEY_R          = 16 */
    kVK_ANSI_Y,                 /* ekKEY_Y          = 17 */
    kVK_ANSI_T,                 /* ekKEY_T          = 18 */
    kVK_ANSI_1,                 /* ekKEY_1          = 19 */

    kVK_ANSI_2,                 /* ekKEY_2          = 20 */
    kVK_ANSI_3,                 /* ekKEY_3          = 21 */
    kVK_ANSI_4,                 /* ekKEY_4          = 22 */
    kVK_ANSI_6,                 /* ekKEY_6          = 23 */
    kVK_ANSI_5,                 /* ekKEY_5          = 24 */
    kVK_ANSI_9,                 /* ekKEY_9          = 25 */
    kVK_ANSI_7,                 /* ekKEY_7          = 26 */
    kVK_ANSI_8,                 /* ekKEY_8          = 27 */
    kVK_ANSI_0,                 /* ekKEY_0          = 28 */
    kVK_ANSI_Backslash,         /* ekKEY_RCURLY     = 29 */

    kVK_ANSI_O,                 /* ekKEY_O          = 30 */
    kVK_ANSI_U,                 /* ekKEY_U          = 31 */
    kVK_ANSI_Quote,             /* ekKEY_LCURLY     = 32 */
    kVK_ANSI_I,                 /* ekKEY_I          = 33 */
    kVK_ANSI_P,                 /* ekKEY_P          = 34 */
    kVK_Return,                 /* ekKEY_RETURN     = 35 */
    kVK_ANSI_L,                 /* ekKEY_L          = 36 */
    kVK_ANSI_J,                 /* ekKEY_J          = 37 */
    10000,                      /* ekKEY_SEMICOLON  = 38 */
    kVK_ANSI_K,                 /* ekKEY_K          = 39 */

    kVK_ANSI_Minus,             /* ekKEY_QUEST      = 40 */
    kVK_ANSI_Comma,             /* ekKEY_COMMA      = 41 */
    kVK_ANSI_Slash,             /* ekKEY_MINUS      = 42 */
    kVK_ANSI_N,                 /* ekKEY_N          = 43 */
    kVK_ANSI_M,                 /* ekKEY_M          = 44 */
    kVK_ANSI_Period,            /* ekKEY_PERIOD     = 45 */
    kVK_Tab,                    /* ekKEY_TAB        = 46 */
    kVK_Space,                  /* ekKEY_SPACE      = 47 */
    kVK_ANSI_Grave,             /* ekKEY_GTLT       = 48 */
    kVK_Delete,                 /* ekKEY_BACK       = 49 */

    kVK_Escape,                 /* ekKEY_ESCAPE     = 50 */
    kVK_F17,                    /* ekKEY_F17        = 51 */
    kVK_ANSI_KeypadDecimal,     /* ekKEY_NUMDECIMAL = 52 */
    kVK_ANSI_KeypadMultiply,    /* ekKEY_NUMMULT    = 53 */
    kVK_ANSI_KeypadPlus,        /* ekKEY_NUMADD     = 54 */
    kVK_ANSI_KeypadClear,       /* ekKEY_NUMLOCK    = 55 */
    kVK_ANSI_KeypadDivide,      /* ekKEY_NUMDIV     = 56 */
    kVK_ANSI_KeypadEnter,       /* ekKEY_NUMRET     = 57 */
    kVK_ANSI_KeypadMinus,       /* ekKEY_NUMMINUS   = 58 */
    kVK_F18,                    /* ekKEY_F18        = 59 */

    kVK_F19,                    /* ekKEY_F19        = 60 */
    kVK_ANSI_KeypadEquals,      /* ekKEY_NUMEQUAL   = 61 */
    kVK_ANSI_Keypad0,           /* ekKEY_NUM0       = 62 */
    kVK_ANSI_Keypad1,           /* ekKEY_NUM1       = 63 */
    kVK_ANSI_Keypad2,           /* ekKEY_NUM2       = 64 */
    kVK_ANSI_Keypad3,           /* ekKEY_NUM3       = 65 */
    kVK_ANSI_Keypad4,           /* ekKEY_NUM4       = 66 */
    kVK_ANSI_Keypad5,           /* ekKEY_NUM5       = 67 */
    kVK_ANSI_Keypad6,           /* ekKEY_NUM6       = 68 */
    kVK_ANSI_Keypad7,           /* ekKEY_NUM7       = 69 */

    kVK_ANSI_Keypad8,           /* ekKEY_NUM8       = 70 */
    kVK_ANSI_Keypad9,           /* ekKEY_NUM9       = 71 */
    kVK_F5,                     /* ekKEY_F5         = 72 */
    kVK_F6,                     /* ekKEY_F6         = 73 */
    kVK_F7,                     /* ekKEY_F7         = 74 */
    kVK_F3,                     /* ekKEY_F3         = 75 */
    kVK_F8,                     /* ekKEY_F8         = 76 */
    kVK_F9,                     /* ekKEY_F9         = 77 */
    kVK_F11,                    /* ekKEY_F11        = 78 */
    kVK_F13,                    /* ekKEY_F13        = 79 */

    kVK_F16,                    /* ekKEY_F16        = 80 */
    kVK_F14,                    /* ekKEY_F14        = 81 */
    kVK_F10,                    /* ekKEY_F10        = 82 */
    kVK_F12,                    /* ekKEY_F12        = 83 */
    kVK_F15,                    /* ekKEY_F15        = 84 */
    kVK_PageUp,                 /* ekKEY_PAGEUP     = 85 */
    kVK_Home,                   /* ekKEY_HOME       = 86 */
    kVK_ForwardDelete,          /* ekKEY_SUPR       = 87 */
    kVK_F4,                     /* ekKEY_F4         = 88 */
    kVK_PageDown,               /* ekKEY_PAGEDOWN   = 89 */

    kVK_F2,                     /* ekKEY_F2         = 90 */
    kVK_End,                    /* ekKEY_END        = 91 */
    kVK_F1,                     /* ekKEY_F1         = 92 */
    kVK_LeftArrow,              /* ekKEY_LEFT       = 93 */
    kVK_RightArrow,             /* ekKEY_RIGHT      = 94 */
    kVK_DownArrow,              /* ekKEY_DOWN       = 95 */
    kVK_UpArrow,                /* ekKEY_UP         = 96 */
    kVK_Shift,                  /* ekKEY_LSHIFT     = 97 */
    kVK_RightShift,             /* ekKEY_RSHIFT     = 98 */
    kVK_Control,                /* ekKEY_LCTRL      = 99 */
    kVK_RightControl,           /* ekKEY_RCTRL      = 100 */

    kVK_Option,                 /* ekKEY_LALT       = 101 */
    kVK_RightOption,            /* ekKEY_RALT       = 102 */
    kVK_Help,                   /* ekKEY_INSERT     = 103 */
    kVK_ANSI_Equal,             /* ekKEY_EXCLAM     = 104 */
    0x6E,                       /* ekKEY_MENU       = 105 */
    kVK_Command,                /* ekKEY_LWIN       = 106 */
    kVK_RightCommand,           /* ekKEY_RWIN       = 107 */
    kVK_CapsLock,               /* ekKEY_CAPS       = 108 */
    kVK_ANSI_Semicolon,         /* ekKEY_TILDE      = 109 */
    kVK_ANSI_LeftBracket,       /* ekKEY_GRAVE      = 110 */
    kVK_ANSI_RightBracket       /* ekKEY_PLUS       = 111 */
};

/*---------------------------------------------------------------------------*/

void _oslistener_init(ViewListeners *listeners)
{
    cassert_no_null(listeners);
    bmem_zero(listeners, ViewListeners);
    //listeners->is_dirty = YES;
    listeners->is_enabled = YES;
}

/*---------------------------------------------------------------------------*/

void _oslistener_release(ViewListeners *listeners)
{
    cassert_no_null(listeners);
    listener_destroy(&listeners->OnDraw);
    listener_destroy(&listeners->OnEnter);
    listener_destroy(&listeners->OnExit);
    listener_destroy(&listeners->OnMoved);
    listener_destroy(&listeners->OnDown);
    listener_destroy(&listeners->OnUp);
    listener_destroy(&listeners->OnClick);
    listener_destroy(&listeners->OnDrag);
    listener_destroy(&listeners->OnWheel);
    listener_destroy(&listeners->OnKeyUp);
    listener_destroy(&listeners->OnKeyDown);
}

/*---------------------------------------------------------------------------*/

void _oslistener_set_enabled(ViewListeners *listeners, bool_t is_enabled)
{
    cassert_no_null(listeners);
    listeners->is_enabled = (is_enabled == TRUE) ? YES : NO;
}

/*---------------------------------------------------------------------------*/

static void i_mouse_position_in_view_coordinates(const NSView *view, const NSPoint mouse_location_in_window, real32_t *x, real32_t *y)
{
    NSPoint local_point;
    cassert_no_null(view);
    cassert_no_null(x);
    cassert_no_null(y);
    local_point = [view convertPoint:mouse_location_in_window fromView:nil];
    *x = (real32_t)local_point.x;
    *y = (real32_t)local_point.y;
    /* Note: The y coordinate in the returned point starts from a base of 1, not 0. */
    /* *y -= 1.f; */
    // *y = (real32_t)[view frame].size.height - *y - 1.f;
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_enter(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled && listeners->OnEnter != NULL)
    {
        EvMouse params;
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &params.x, &params.y);
        params.button = ENUM_MAX(gui_mouse_t);
        params.count = 0;
        listener_event(listeners->OnEnter, ekGUI_EVENT_ENTER, (OSView*)view, &params, NULL, OSView, EvMouse, void);
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_exit(const NSView *view, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled && listeners->OnExit != NULL)
        listener_event(listeners->OnExit, ekGUI_EVENT_EXIT, (OSView*)view, NULL, NULL, OSView, void, void);
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_moved(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled && listeners->OnMoved != NULL)
    {
        real32_t x, y;
        //NSSize size = [view frame].size;
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &x, &y);
        //if (x >= 0.f && x < size.width && y >= 0.f && y < size.height)
        {
            EvMouse params;
            params.x = x;
            params.y = y;
            params.button = ENUM_MAX(gui_mouse_t);
            params.count = 0;
            listener_event(listeners->OnMoved, ekGUI_EVENT_MOVED, (OSView*)view, &params, NULL, OSView, EvMouse, void);
        }
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_down(const NSView *view, NSEvent *theEvent, const gui_mouse_t button, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled && listeners->OnDown != NULL)
    {
        EvMouse params;
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &params.x, &params.y);
        params.button = button;
        params.count = 0;
        listener_event(listeners->OnDown, ekGUI_EVENT_DOWN, (OSView*)view, &params, NULL, OSView, EvMouse, void);
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_up(const NSView *view, NSEvent *theEvent, const gui_mouse_t button, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled == YES)
    {
        real32_t x, y;
        cassert_no_null(theEvent);
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &x, &y);

        if (listeners->OnUp != NULL)
        {
            EvMouse params;
            params.x = x;
            params.y = y;
            params.button = button;
            params.count = 0;
            listener_event(listeners->OnUp, ekGUI_EVENT_UP, (OSView*)view, &params, NULL, OSView, EvMouse, void);
        }

        if (listeners->OnClick != NULL)
        {
            EvMouse params;
            params.x = x;
            params.y = y;
            params.button = button;
            params.count = (uint32_t)[theEvent clickCount];
            listener_event(listeners->OnClick, ekGUI_EVENT_CLICK, (OSView*)view, &params, NULL, OSView, EvMouse, void);
        }
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_dragged2(const NSView *view, NSEvent *theEvent, const gui_mouse_t button, Listener *OnDrag_listener)
{
    if (OnDrag_listener != NULL)
    {
        EvMouse params;
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &params.x, &params.y);
        params.button = button;
        params.count = 0;
        listener_event(OnDrag_listener, ekGUI_EVENT_DRAG, (OSView*)view, &params, NULL, OSView, EvMouse, void);
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_mouse_dragged(const NSView *view, NSEvent *theEvent, const gui_mouse_t button, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled == YES)
        _oslistener_mouse_dragged2(view, theEvent, button, listeners->OnDrag);
}

/*---------------------------------------------------------------------------*/

void _oslistener_scroll_whell(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->OnWheel != NULL && listeners->is_enabled == YES)
    {
        EvWheel params;
        cassert_no_null(theEvent);
        i_mouse_position_in_view_coordinates(view, [theEvent locationInWindow], &params.x, &params.y);
        params.dx = (real32_t)[theEvent deltaX];
        params.dy = (real32_t)[theEvent deltaY];
        params.dz = (real32_t)[theEvent deltaZ];
        listener_event(listeners->OnWheel, ekGUI_EVENT_WHEEL, (OSView*)view, &params, NULL, OSView, EvWheel, void);
    }
}

/*---------------------------------------------------------------------------*/

static __INLINE void i_launch_key_event(const NSView *view, const gui_event_t evtype, vkey_t virtual_key_code, Listener *OnKey)
{
    EvKey params;
    cassert_no_null(OnKey);
    params.key = virtual_key_code;
    listener_event(OnKey, evtype, (OSView*)view, &params, NULL, OSView, EvKey, void);
}

/*---------------------------------------------------------------------------*/

static void i_process_key_event(const NSView *view, NSEvent *theEvent, const gui_event_t evtype, Listener *OnKey)
{
    vkey_t virtual_key_code = ekKEY_UNDEF;
    register unsigned short keycode;
    register uint32_t i, n = sizeof(i_kVIRTUAL_KEY) / sizeof(unsigned short);
    cassert_no_null(theEvent);
    keycode = [theEvent keyCode];

    for (i = 0; i < n; ++i)
    {
        if(i_kVIRTUAL_KEY[i] == keycode)
        {
            virtual_key_code = (vkey_t)i;
            break;
        }
    }

    if (virtual_key_code != ekKEY_UNDEF)
        i_launch_key_event(view, evtype, virtual_key_code, OnKey);
}

/*---------------------------------------------------------------------------*/

void _oslistener_key_down(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled == YES && listeners->OnKeyDown != NULL)
        i_process_key_event(view, theEvent, ekGUI_EVENT_KEYDOWN, listeners->OnKeyDown);
}

/*---------------------------------------------------------------------------*/

void _oslistener_key_up(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    if (listeners->is_enabled == YES && listeners->OnKeyUp != NULL)
        i_process_key_event(view, theEvent, ekGUI_EVENT_KEYUP, listeners->OnKeyUp);
}

/*---------------------------------------------------------------------------*/

static __INLINE void i_modifier_flags(
                            NSUInteger flags,
                            bool_t *rshift, bool_t *rctrl, bool_t *rcommand, bool_t *ralt,
                            bool_t *lshift, bool_t *lctrl, bool_t *lcommand, bool_t *lalt)
{
    *rshift = ((flags & 131332) == 131332) ? TRUE : FALSE;
    *rctrl = ((flags & 270592) == 270592) ? TRUE : FALSE;
    *rcommand = ((flags & 1048848) == 1048848) ? TRUE : FALSE;
    *ralt = ((flags & 524608) == 524608) ? TRUE : FALSE;
    *lshift = ((flags & 131330) == 131330) ? TRUE : FALSE;
    *lctrl = ((flags & 262401) == 262401) ? TRUE : FALSE;
    *lcommand = ((flags & 1048840) == 1048840) ? TRUE : FALSE;
    *lalt = ((flags & 524576) == 524576) ? TRUE : FALSE;
}

/*---------------------------------------------------------------------------*/

static __INLINE void i_flags_event(const NSView *view, const bool_t press, const bool_t prevpress, const vkey_t key, Listener *OnKeyDown, Listener *OnKeyUp)
{
    if (press != prevpress)
    {
        if (press == TRUE)
        {
            if (OnKeyDown != NULL)
                i_launch_key_event(view, ekGUI_EVENT_KEYDOWN, key, OnKeyDown);
        }
        else
        {
            if (OnKeyUp != NULL)
                i_launch_key_event(view, ekGUI_EVENT_KEYUP, key, OnKeyUp);
        }
    }
}

/*---------------------------------------------------------------------------*/

void _oslistener_key_flags_changed(const NSView *view, NSEvent *theEvent, ViewListeners *listeners)
{
    cassert_no_null(listeners);
    cassert_no_null(theEvent);
    if (listeners->is_enabled == YES)
    {
        NSUInteger flags;
        bool_t rshift, rctrl, rcommand, ralt;
        bool_t lshift, lctrl, lcommand, lalt;
        bool_t prshift, prctrl, prcommand, pralt;
        bool_t plshift, plctrl, plcommand, plalt;
        flags = [theEvent modifierFlags];
        i_modifier_flags(flags, &rshift, &rctrl, &rcommand, &ralt, &lshift, &lctrl, &lcommand, &lalt);
        i_modifier_flags(listeners->modifier_flags, &prshift, &prctrl, &prcommand, &pralt, &plshift, &plctrl, &plcommand, &plalt);
        i_flags_event(view, rshift, prshift, ekKEY_RSHIFT, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, lshift, plshift, ekKEY_LSHIFT, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, rctrl, prctrl, ekKEY_RCTRL, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, lctrl, plctrl, ekKEY_LCTRL, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, rcommand, prcommand, ekKEY_RWIN, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, lcommand, plcommand, ekKEY_LWIN, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, ralt, pralt, ekKEY_RALT, listeners->OnKeyDown, listeners->OnKeyUp);
        i_flags_event(view, lalt, plalt, ekKEY_LALT, listeners->OnKeyDown, listeners->OnKeyUp);
        listeners->modifier_flags = flags;
    }
}

