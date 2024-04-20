/*
    This is part of gtnap
    TODO: More info
    Commit - 2
*/

#include "nap_debugger.inl"
#include <deblib/deblib.h>
#include "inkey.ch"
#include "setcurs.ch"
#include "nappgui.h"

struct _gtnap_debugger_t
{
    Proc *proc;
    Stream *stream;
};

/*---------------------------------------------------------------------------*/

GtNapDebugger *nap_debugger_create(const char_t *path, const uint32_t nrows, const uint32_t ncols)
{
    GtNapDebugger *debug = heap_new(GtNapDebugger);
    uint32_t ip = bsocket_url_ip("localhost", NULL);
    String *cmd = str_cpath("%s %d %d", path, nrows, ncols);
    Socket *socket = NULL;

    debug->proc = bproc_exec(tc(cmd), NULL);
    bthread_sleep(100);

    socket = bsocket_connect(ip, 3555, 0, NULL);
    if (socket != NULL)
        debug->stream = stm_socket(socket);

    str_destroy(&cmd);
    return debug;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_destroy(GtNapDebugger **debug)
{
    cassert_no_null(debug);
    cassert_no_null(*debug);

    if ((*debug)->stream != NULL)
    {
        deblib_close((*debug)->stream);
        stm_close(&(*debug)->stream);
    }

    heap_delete(debug, GtNapDebugger);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_scroll(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_scroll(debug->stream, top, left, bottom, right, row, col, codepoint, color);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_box(GtNapDebugger *debug, const uint32_t top, const uint32_t left, const uint32_t bottom, const uint32_t right, const uint32_t color)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_box(debug->stream, top, left, bottom, right, color);
}

/*---------------------------------------------------------------------------*/

static cursor_t i_cursor(const uint32_t style)
{
    switch( style ) {
    case SC_NONE:
        return ekCURSOR_NONE;
    case SC_NORMAL:
        return ekCURSOR_NORMAL;
    case SC_INSERT:
        return ekCURSOR_INSERT;
    case SC_SPECIAL1:
        return ekCURSOR_SPECIAL1;
    case SC_SPECIAL2:
        return ekCURSOR_SPECIAL2;
    cassert_default();
    }

    return ekCURSOR_NORMAL;
}

/*---------------------------------------------------------------------------*/

void nap_debugger_cursor(GtNapDebugger *debug, const uint32_t style)
{
    cassert_no_null(debug);
    if (debug->stream != NULL) 
    {
        cursor_t cursor = i_cursor(style);
        deblib_send_cursor(debug->stream, cursor);
    }
}

/*---------------------------------------------------------------------------*/

void nap_debugger_set_pos(GtNapDebugger *debug, const uint32_t row, const uint32_t col)
{
    cassert_no_null(debug);
    if (debug->stream != NULL) 
        deblib_send_set_pos(debug->stream, row, col);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_putchar(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t codepoint, const uint32_t color, const byte_t attrib)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_putchar(debug->stream, row, col, codepoint, color, attrib);
}

/*---------------------------------------------------------------------------*/

void nap_debugger_puttext(GtNapDebugger *debug, const uint32_t row, const uint32_t col, const uint32_t color, const char_t *utf8)
{
    cassert_no_null(debug);
    if (debug->stream != NULL)
        deblib_send_puttext(debug->stream, row, col, color, utf8);
}

/*---------------------------------------------------------------------------*/

static __INLINE int32_t i_key_letter(int32_t ctrl, int32_t alt, int32_t shift, int32_t normal, uint32_t modifiers)
{
    if (modifiers & ekMKEY_CONTROL)
        return ctrl;
    if (modifiers & ekMKEY_ALT)
        return alt;
    if (modifiers & ekMKEY_SHIFT)
        return shift;
    else
        return normal;
}

/*---------------------------------------------------------------------------*/

static __INLINE int32_t i_key_function(int32_t ctrl, int32_t alt, int32_t shift, int32_t normal, uint32_t modifiers)
{
    if (modifiers & ekMKEY_CONTROL)
        return ctrl;
    if (modifiers & ekMKEY_ALT)
        return alt;
    if (modifiers & ekMKEY_SHIFT)
        return shift;
    else
        return normal;
}

/*---------------------------------------------------------------------------*/

static int32_t i_vkey_to_hb(const vkey_t vkey, const uint32_t modifiers)
{
    unref(modifiers);
    switch(vkey) {
    case ekKEY_LEFT:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_LEFT;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_LEFT;
        return K_LEFT;

    case ekKEY_RIGHT:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_RIGHT;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_RIGHT;
        return K_RIGHT;

    case ekKEY_DOWN:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_DOWN;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_DOWN;
        return K_DOWN;

    case ekKEY_UP:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_UP;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_UP;
        return K_UP;

    case ekKEY_HOME:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_HOME;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_HOME;
        return K_HOME;

    case ekKEY_END:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_END;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_END;
        return K_END;

    case ekKEY_PAGEUP:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_PGUP;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_PGUP;
        return K_PGUP;

    case ekKEY_PAGEDOWN:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_PGDN;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_PGDN;
        return K_PGDN;

    case ekKEY_RETURN:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_RETURN;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_RETURN;
        return K_RETURN;

    case ekKEY_SPACE:
        return K_SPACE;

    case ekKEY_ESCAPE:
        return K_ESC;

    case ekKEY_QUEST:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_QUESTION;
        return '?';

    case ekKEY_0:
    case ekKEY_NUM0:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_0;
        return '0';

    case ekKEY_1:
    case ekKEY_NUM1:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_1;
        return '1';

    case ekKEY_2:
    case ekKEY_NUM2:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_2;
        return '2';

    case ekKEY_3:
    case ekKEY_NUM3:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_3;
        return '3';

    case ekKEY_4:
    case ekKEY_NUM4:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_4;
        return '4';

    case ekKEY_5:
    case ekKEY_NUM5:
        if (modifiers & ekMKEY_CONTROL)
            return KP_CTRL_5;
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_5;
        return '5'; /* KP_CENTER */

    case ekKEY_6:
    case ekKEY_NUM6:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_6;
        return '6';

    case ekKEY_7:
    case ekKEY_NUM7:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_7;
        return '7';

    case ekKEY_8:
    case ekKEY_NUM8:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_8;
        return '8';

    case ekKEY_9:
    case ekKEY_NUM9:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_9;
        return '9';

    case ekKEY_NUMDECIMAL:
        return '.';

    case ekKEY_NUMRET:
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_ENTER;
        return K_RETURN;

    case ekKEY_NUMDIV:
        if (modifiers & ekMKEY_CONTROL)
            return KP_CTRL_SLASH;
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_SLASH;
        return '/';

    case ekKEY_NUMMULT:
        if (modifiers & ekMKEY_CONTROL)
            return KP_CTRL_ASTERISK;
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_ASTERISK;
        return '*';

    case ekKEY_NUMADD:
        if (modifiers & ekMKEY_CONTROL)
            return KP_CTRL_PLUS;
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_PLUS;
        return '+';

    case ekKEY_NUMMINUS:
        if (modifiers & ekMKEY_CONTROL)
            return KP_CTRL_MINUS;
        if (modifiers & ekMKEY_ALT)
            return KP_ALT_MINUS;
        return '-';

    case ekKEY_INSERT:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_INS;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_INS;
        return K_INS;

    case ekKEY_SUPR:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_DEL;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_DEL;
        return K_DEL;

    case ekKEY_BACK:
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_BS;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_BS;
        if (modifiers & ekMKEY_SHIFT)
            return K_SH_BS;
        return K_BS;

    case ekKEY_TAB:
        if ((modifiers & ekMKEY_CONTROL) && (modifiers & ekMKEY_SHIFT))
            return K_CTRL_SH_TAB;
        if (modifiers & ekMKEY_CONTROL)
            return K_CTRL_TAB;
        if (modifiers & ekMKEY_SHIFT)
            return K_SH_TAB;
        if (modifiers & ekMKEY_ALT)
            return K_ALT_TAB;
        else
            return K_TAB;

    case ekKEY_BSLASH:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_BACKSLASH;
        return '\\';

    case ekKEY_MINUS:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_MINUS;
        return '-';

    case ekKEY_COMMA:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_COMMA;
        return ',';

    case ekKEY_PERIOD:
        if (modifiers & ekMKEY_ALT)
            return K_ALT_PERIOD;
        return '.';

    case ekKEY_F1:
        return i_key_function(K_CTRL_F1, K_ALT_F1, K_SH_F1, K_F1, modifiers);

    case ekKEY_F2:
        return i_key_function(K_CTRL_F2, K_ALT_F2, K_SH_F2, K_F2, modifiers);

    case ekKEY_F3:
        return i_key_function(K_CTRL_F3, K_ALT_F3, K_SH_F3, K_F3, modifiers);

    case ekKEY_F4:
        return i_key_function(K_CTRL_F4, K_ALT_F4, K_SH_F4, K_F4, modifiers);

    case ekKEY_F5:
        return i_key_function(K_CTRL_F5, K_ALT_F5, K_SH_F5, K_F5, modifiers);

    case ekKEY_F6:
        return i_key_function(K_CTRL_F6, K_ALT_F6, K_SH_F6, K_F6, modifiers);

    case ekKEY_F7:
        return i_key_function(K_CTRL_F7, K_ALT_F7, K_SH_F7, K_F7, modifiers);

    case ekKEY_F8:
        return i_key_function(K_CTRL_F8, K_ALT_F8, K_SH_F8, K_F8, modifiers);

    case ekKEY_F9:
        return i_key_function(K_CTRL_F9, K_ALT_F9, K_SH_F9, K_F9, modifiers);

    case ekKEY_F10:
        return i_key_function(K_CTRL_F10, K_ALT_F10, K_SH_F10, K_F10, modifiers);

    case ekKEY_F11:
        return i_key_function(K_CTRL_F11, K_ALT_F11, K_SH_F11, K_F11, modifiers);

    case ekKEY_F12:
        return i_key_function(K_CTRL_F12, K_ALT_F12, K_SH_F12, K_F12, modifiers);

    case ekKEY_A:
        return i_key_letter(K_CTRL_A, K_ALT_A, 'A', 'a', modifiers);

    case ekKEY_B:
        return i_key_letter(K_CTRL_B, K_ALT_B, 'B', 'b', modifiers);

    case ekKEY_C:
        return i_key_letter(K_CTRL_C, K_ALT_C, 'C', 'c', modifiers);

    case ekKEY_D:
        return i_key_letter(K_CTRL_D, K_ALT_D, 'D', 'd', modifiers);

    case ekKEY_E:
        return i_key_letter(K_CTRL_E, K_ALT_E, 'E', 'e', modifiers);

    case ekKEY_F:
        return i_key_letter(K_CTRL_F, K_ALT_F, 'F', 'f', modifiers);

    case ekKEY_G:
        return i_key_letter(K_CTRL_G, K_ALT_G, 'G', 'g', modifiers);

    case ekKEY_H:
        return i_key_letter(K_CTRL_H, K_ALT_H, 'H', 'h', modifiers);

    case ekKEY_I:
        return i_key_letter(K_CTRL_I, K_ALT_I, 'I', 'i', modifiers);

    case ekKEY_J:
        return i_key_letter(K_CTRL_J, K_ALT_J, 'J', 'j', modifiers);

    case ekKEY_K:
        return i_key_letter(K_CTRL_K, K_ALT_K, 'K', 'k', modifiers);

    case ekKEY_L:
        return i_key_letter(K_CTRL_L, K_ALT_L, 'L', 'l', modifiers);

    case ekKEY_M:
        return i_key_letter(K_CTRL_M, K_ALT_M, 'M', 'm', modifiers);

    case ekKEY_N:
        return i_key_letter(K_CTRL_N, K_ALT_N, 'N', 'n', modifiers);

    case ekKEY_O:
        return i_key_letter(K_CTRL_O, K_ALT_O, 'O', 'o', modifiers);

    case ekKEY_P:
        return i_key_letter(K_CTRL_P, K_ALT_P, 'P', 'p', modifiers);

    case ekKEY_Q:
        return i_key_letter(K_CTRL_Q, K_ALT_Q, 'Q', 'q', modifiers);

    case ekKEY_R:
        return i_key_letter(K_CTRL_R, K_ALT_R, 'R', 'r', modifiers);

    case ekKEY_S:
        return i_key_letter(K_CTRL_S, K_ALT_S, 'S', 's', modifiers);

    case ekKEY_T:
        return i_key_letter(K_CTRL_T, K_ALT_T, 'T', 't', modifiers);

    case ekKEY_U:
        return i_key_letter(K_CTRL_U, K_ALT_U, 'U', 'u', modifiers);

    case ekKEY_V:
        return i_key_letter(K_CTRL_V, K_ALT_V, 'V', 'v', modifiers);

    case ekKEY_W:
        return i_key_letter(K_CTRL_W, K_ALT_W, 'W', 'w', modifiers);

    case ekKEY_X:
        return i_key_letter(K_CTRL_X, K_ALT_X, 'X', 'x', modifiers);

    case ekKEY_Y:
        return i_key_letter(K_CTRL_Y, K_ALT_Y, 'Y', 'y', modifiers);

    case ekKEY_Z:
        return i_key_letter(K_CTRL_Z, K_ALT_Z, 'Z', 'z', modifiers);
    }

    /* 
     * Non-procesed inkey codes 
     * K_CTRL_PRTSCR
     */
    return 0;
}

/*---------------------------------------------------------------------------*/

int32_t nap_debugger_read_key(GtNapDebugger *debug)
{
    int32_t key = 0;
    cassert_no_null(debug);
    if (debug->stream != NULL)
    {
        vkey_t vkey = ENUM_MAX(vkey_t);
        uint32_t modifiers = UINT32_MAX;
        deblib_read_key(debug->stream, &vkey, &modifiers);
        key = i_vkey_to_hb(vkey, modifiers);

        ///* TODO: Convert to Harbour key value */
        //log_printf("VKEY: %d", vkey);
        //log_printf("MODIFIERS: %d", modifiers);

       
        //if (vkey == ENUM_MAX(vkey_t))
        //    key = 0;
    }

    return key;
}
