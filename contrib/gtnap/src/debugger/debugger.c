/* NAppGUI Hello World */

#include <deblib/deblib.h>
#include <nappgui.h>

typedef struct _bufchar_t BufChar;
typedef struct _app_t App;

struct _bufchar_t
{
    char_t utf8[5];
    uint32_t color;
    byte_t attrib;
};

struct _app_t
{
    Window *window;
    Font *font;
    TextView *text;
    View *view;
    bool_t alive;
    bool_t print_log;
    Mutex *mutex;
    Thread *protocol_thread;
    BufChar *text_buffer;
    KeyBuf *keyboard_buffer;
    uint32_t ncols;
    uint32_t nrows;
    real32_t cell_width;
    real32_t cell_height;
    cursor_t cursor_type;
    uint32_t cursor_row;
    uint32_t cursor_col;
    bool_t cursor_draw;
    real64_t last_redraw;
};

static const real64_t i_BLINK_INTERVAL = 0.4;

static color_t i_COLORS[16];

#define COL_BLACK           0
#define COL_BLUE            1
#define COL_GREEN           2
#define COL_CYAN            3
#define COL_RED             4
#define COL_MAGENTA         5
#define COL_BROWN           6
#define COL_WHITE           7
#define COL_LIGHT_GRAY      8
#define COL_BRIGHT_BLUE     9
#define COL_BRIGHT_GREEN    10
#define COL_BRIGHT_CYAN     11
#define COL_BRIGHT_RED      12
#define COL_BRIGHT_MAGENTA  13
#define COL_YELLOW          14
#define COL_BRIGHT_WHITE    15

static void i_init_colors(void)
{
    /* In dark mode, black and white are inverted */
    //color_t DARK_COL_BLACK = color_html("#FCFCFC");
    //color_t DARK_COL_BLUE = color_html("#1D99F3");
    //color_t DARK_COL_GREEN = color_html("#11D116");
    //color_t DARK_COL_CYAN = color_html("#1ABC9C");
    //color_t DARK_COL_RED = color_html("#ED1515");
    //color_t DARK_COL_MAGENTA = color_html("#9B59B6");
    //color_t DARK_COL_BROWN = color_html("#F67400");
    //color_t DARK_COL_WHITE = color_html("#232627");
    //color_t DARK_COL_LIGHT_GRAY = color_html("#FFFFFF");
    //color_t DARK_COL_BRIGHT_BLUE = color_html("#3DAEE9");
    //color_t DARK_COL_BRIGHT_GREEN = color_html("#1CDC9A");
    //color_t DARK_COL_BRIGHT_CYAN = color_html("#3DAEE9");
    //color_t DARK_COL_BRIGHT_RED = color_html("#C0392B");
    //color_t DARK_COL_BRIGHT_MAGENTA = color_html("#8E44AD");
    //color_t DARK_COL_YELLOW = color_html("#FDBC4B");
    //color_t DARK_COL_BRIGHT_WHITE = color_html("#7F8C8D");

    color_t LIGHT_COL_BLACK = color_html("#000000");
    color_t LIGHT_COL_BLUE = color_html("#135CD0");
    color_t LIGHT_COL_GREEN = color_html("#328A5D");
    color_t LIGHT_COL_CYAN = color_html("#33C3C1");
    color_t LIGHT_COL_RED = color_html("#F8282A");
    color_t LIGHT_COL_MAGENTA = color_html("#9F00BD");
    color_t LIGHT_COL_BROWN = color_html("#FA701D");
    color_t LIGHT_COL_WHITE = color_html("#B3B3B3");
    color_t LIGHT_COL_LIGHT_GRAY = color_html("#555753");
    color_t LIGHT_COL_BRIGHT_BLUE = color_html("#1670FF");
    color_t LIGHT_COL_BRIGHT_GREEN = color_html("#2CC631");
    color_t LIGHT_COL_BRIGHT_CYAN = color_html("#00FFFF");
    color_t LIGHT_COL_BRIGHT_RED = color_html("#FB0416");
    color_t LIGHT_COL_BRIGHT_MAGENTA = color_html("#E900B0");
    color_t LIGHT_COL_YELLOW = color_html("#FDD727");
    color_t LIGHT_COL_BRIGHT_WHITE = color_html("#EEEEEC");

        i_COLORS[COL_BLACK] = LIGHT_COL_BLACK;
    i_COLORS[COL_BLUE] = LIGHT_COL_BLUE;
    i_COLORS[COL_GREEN] = LIGHT_COL_GREEN;
    i_COLORS[COL_CYAN] = LIGHT_COL_CYAN;
    i_COLORS[COL_RED] = LIGHT_COL_RED;
    i_COLORS[COL_MAGENTA] = LIGHT_COL_MAGENTA;
    i_COLORS[COL_BROWN] = LIGHT_COL_BROWN;
    i_COLORS[COL_WHITE] = LIGHT_COL_WHITE;
    i_COLORS[COL_LIGHT_GRAY] = LIGHT_COL_LIGHT_GRAY;
    i_COLORS[COL_BRIGHT_BLUE] = LIGHT_COL_BRIGHT_BLUE;
    i_COLORS[COL_BRIGHT_GREEN] = LIGHT_COL_BRIGHT_GREEN;
    i_COLORS[COL_BRIGHT_CYAN] = LIGHT_COL_BRIGHT_CYAN;
    i_COLORS[COL_BRIGHT_RED] = LIGHT_COL_BRIGHT_RED;
    i_COLORS[COL_BRIGHT_MAGENTA] = LIGHT_COL_BRIGHT_MAGENTA;
    i_COLORS[COL_YELLOW] = LIGHT_COL_YELLOW;
    i_COLORS[COL_BRIGHT_WHITE] = LIGHT_COL_BRIGHT_WHITE;

    //i_COLORS[COL_BLACK] = gui_alt_color(LIGHT_COL_BLACK, DARK_COL_BLACK);
    //i_COLORS[COL_BLUE] = gui_alt_color(LIGHT_COL_BLUE, DARK_COL_BLUE);
    //i_COLORS[COL_GREEN] = gui_alt_color(LIGHT_COL_GREEN, DARK_COL_GREEN);
    //i_COLORS[COL_CYAN] = gui_alt_color(LIGHT_COL_CYAN, DARK_COL_CYAN);
    //i_COLORS[COL_RED] = gui_alt_color(LIGHT_COL_RED, DARK_COL_RED);
    //i_COLORS[COL_MAGENTA] = gui_alt_color(LIGHT_COL_MAGENTA, DARK_COL_MAGENTA);
    //i_COLORS[COL_BROWN] = gui_alt_color(LIGHT_COL_BROWN, DARK_COL_BROWN);
    //i_COLORS[COL_WHITE] = gui_alt_color(LIGHT_COL_WHITE, DARK_COL_WHITE);
    //i_COLORS[COL_LIGHT_GRAY] = gui_alt_color(LIGHT_COL_LIGHT_GRAY, DARK_COL_LIGHT_GRAY);
    //i_COLORS[COL_BRIGHT_BLUE] = gui_alt_color(LIGHT_COL_BRIGHT_BLUE, DARK_COL_BRIGHT_BLUE);
    //i_COLORS[COL_BRIGHT_GREEN] = gui_alt_color(LIGHT_COL_BRIGHT_GREEN, DARK_COL_BRIGHT_GREEN);
    //i_COLORS[COL_BRIGHT_CYAN] = gui_alt_color(LIGHT_COL_BRIGHT_CYAN, DARK_COL_BRIGHT_CYAN);
    //i_COLORS[COL_BRIGHT_RED] = gui_alt_color(LIGHT_COL_BRIGHT_RED, DARK_COL_BRIGHT_RED);
    //i_COLORS[COL_BRIGHT_MAGENTA] = gui_alt_color(LIGHT_COL_BRIGHT_MAGENTA, DARK_COL_BRIGHT_MAGENTA);
    //i_COLORS[COL_YELLOW] = gui_alt_color(LIGHT_COL_YELLOW, DARK_COL_YELLOW);
    //i_COLORS[COL_BRIGHT_WHITE] = gui_alt_color(LIGHT_COL_BRIGHT_WHITE, DARK_COL_BRIGHT_WHITE);
}

/*---------------------------------------------------------------------------*/

static uint16_t kSERVER_PORT = 3555;
static uint32_t i_DEFAULT_COLS = 80;
static uint32_t i_DEFAULT_ROWS = 25;

/*---------------------------------------------------------------------------*/

static void i_update_text_buffer(App *app, const uint32_t nrows, const uint32_t ncols)
{
    cassert_no_null(app);
    if (app->text_buffer != NULL)
        heap_delete_n(&app->text_buffer, app->nrows * app->ncols, BufChar);

    app->nrows = nrows;
    app->ncols = ncols;
    app->text_buffer = heap_new_n0(app->nrows * app->ncols, BufChar);

    if (app->view != NULL)
        view_size(app->view, s2df(app->ncols * app->cell_width, app->nrows * app->cell_height));

    if (app->window != NULL)
        window_update(app->window);
}

/*---------------------------------------------------------------------------*/

static void i_OnDraw(App *app, Event *e)
{
    const EvDraw *p = event_params(e, EvDraw);
    BufChar *bchar = NULL;
    uint32_t i, j;
    cassert_no_null(app);
    bchar = app->text_buffer;

    draw_font(p->ctx, app->font);
    for (i = 0; i < app->nrows; ++i)
    {
        real32_t y = i * app->cell_height;
        for (j = 0; j < app->ncols; ++j, ++bchar)
        {
            real32_t x = j * app->cell_width;
            if (bchar->utf8[0] != 0)
            {
                int fore = bchar->color & 0x000F;
                int back = ( bchar->color & 0x00F0 ) >> 4;
                color_t cfore = i_COLORS[fore];
                color_t cback = i_COLORS[back];
                draw_text_color(p->ctx, cfore);
                draw_fill_color(p->ctx, cback);
                draw_rect(p->ctx, ekFILL, x + 2, y, app->cell_width + 1, app->cell_height + 1);
                draw_text(p->ctx, bchar->utf8, x, y);
            }
        }
    }

    if (app->cursor_draw == TRUE)
    {
        if (app->cursor_row < app->nrows && app->cursor_col < app->ncols)
        {
            real32_t x = app->cursor_col * app->cell_width;
            real32_t y = app->cursor_row * app->cell_height;
            color_t cfore = i_COLORS[COL_WHITE];
            color_t cback = i_COLORS[COL_BLACK];
            bchar = app->text_buffer + (app->cursor_row * app->ncols) + app->cursor_col;
            draw_fill_color(p->ctx, cback);
            draw_rect(p->ctx, ekFILL, x+1, y, app->cell_width + 1, app->cell_height + 1);

            if (bchar->utf8[0] != 0)
            {
                draw_text_color(p->ctx, cfore);
                draw_text(p->ctx, bchar->utf8, x, y);
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static Panel *i_panel(App *app)
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(2, 1);
    View *view = view_create();
    TextView *text = textview_create();
    view_OnDraw(view, listener(app, i_OnDraw, App));
    app->font = font_monospace(font_regular_size(), 0);
    app->text = text;
    app->view = view;
    font_extents(app->font, "OOOO", -1, &app->cell_width, &app->cell_height);
    app->cell_width /= 4;
    layout_view(layout, view, 0, 0);
    layout_textview(layout, text, 1, 0);
    layout_tabstop(layout, 0, 0, TRUE);
    layout_tabstop(layout, 1, 0, FALSE);
    layout_hsize(layout, 1, 400);
    panel_layout(panel, layout);
    return panel;
}

/*---------------------------------------------------------------------------*/

static void i_OnClose(App *app, Event *e)
{
    osapp_finish();
    unref(app);
    unref(e);
}

/*---------------------------------------------------------------------------*/

static void i_log(App *app, String **str)
{
    cassert_no_null(app);
    cassert_no_null(str);
    if (app->print_log == TRUE)
    {
        bmutex_lock(app->mutex);
        textview_writef(app->text, tc(*str));
        textview_writef(app->text, "\n");
        bmutex_unlock(app->mutex);
    }
    str_destroy(str);
}

/*---------------------------------------------------------------------------*/

static void i_set_size(App *app, const DebMsg *msg)
{
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_SET_SIZE Rows: %d, Cols: %d", msg->row, msg->col);
        i_log(app, &log);
    }

    //if (app->nrows != msg->row || app->ncols != msg->col)
    //{
    //    i_update_text_buffer(app, msg->row, msg->col);
    //    view_update(app->view);
    //}
}

/*---------------------------------------------------------------------------*/

static void i_scroll(App *app, const DebMsg *msg)
{
    uint32_t i, j;
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_SCROLL Top: %d, Left: %d, Bottom: %d, Right: %d, Char: '%s', Color: %d", msg->top, msg->left, msg->bottom, msg->right, msg->utf8, msg->color);
        i_log(app, &log);
    }

    bmutex_lock(app->mutex);
    for (i = msg->top; i <= msg->bottom; ++i)
    {
        for (j = msg->left; j <= msg->right; ++j)
        {
            BufChar *bchar = app->text_buffer + (i * app->ncols + j);
            cassert(i < app->nrows);
            cassert(j < app->ncols);
            str_copy_c(bchar->utf8, sizeof(bchar->utf8), msg->utf8);
            bchar->color = msg->color;
            bchar->attrib = 0;
        }
    }
    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_box(App *app, const DebMsg *msg)
{
    uint32_t i, j;
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_BOX Top: %d, Left: %d, Bottom: %d, Right: %d, Color: %d", msg->top, msg->left, msg->bottom, msg->right, msg->color);
        i_log(app, &log);
    }

    bmutex_lock(app->mutex);
    for (i = msg->top + 1; i <= msg->bottom - 1; ++i)
    {
        /* Left edge */
        {
            BufChar *bchar = app->text_buffer + (i * app->ncols + msg->left);
            cassert(i < app->nrows);
            str_copy_c(bchar->utf8, sizeof(bchar->utf8), "│");
            bchar->color = msg->color;
            bchar->attrib = 0;
        }

        /* Right edge */
        {
            BufChar *bchar = app->text_buffer + (i * app->ncols + msg->right);
            cassert(i < app->nrows);
            str_copy_c(bchar->utf8, sizeof(bchar->utf8), "│");
            bchar->color = msg->color;
            bchar->attrib = 0;
        }
    }

    for (j = msg->left + 1; j <= msg->right - 1; ++j)
    {
        /* Top edge */
        {
            BufChar *bchar = app->text_buffer + (msg->top * app->ncols + j);
            cassert(j < app->ncols);
            str_copy_c(bchar->utf8, sizeof(bchar->utf8), "─");
            bchar->color = msg->color;
            bchar->attrib = 0;
        }

        /* Bottom edge */
        {
            BufChar *bchar = app->text_buffer + (msg->bottom * app->ncols + j);
            cassert(j < app->ncols);
            str_copy_c(bchar->utf8, sizeof(bchar->utf8), "─");
            bchar->color = msg->color;
            bchar->attrib = 0;
        }
    }

    /* Top-left corner */
    {
        BufChar *bchar = app->text_buffer + (msg->top * app->ncols + msg->left);
        str_copy_c(bchar->utf8, sizeof(bchar->utf8), "┌");
        bchar->color = msg->color;
        bchar->attrib = 0;
    }

    /* Top-right corner */
    {
        BufChar *bchar = app->text_buffer + (msg->top * app->ncols + msg->right);
        str_copy_c(bchar->utf8, sizeof(bchar->utf8), "┐");
        bchar->color = msg->color;
        bchar->attrib = 0;
    }

    /* Bottom-left corner */
    {
        BufChar *bchar = app->text_buffer + (msg->bottom * app->ncols + msg->left);
        str_copy_c(bchar->utf8, sizeof(bchar->utf8), "└");
        bchar->color = msg->color;
        bchar->attrib = 0;
    }

    /* Bottom-right corner */
    {
        BufChar *bchar = app->text_buffer + (msg->bottom * app->ncols + msg->right);
        str_copy_c(bchar->utf8, sizeof(bchar->utf8), "┘");
        bchar->color = msg->color;
        bchar->attrib = 0;
    }

    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_cursor(App *app, const DebMsg *msg)
{
    cursor_t cursor = ENUM_MAX(cursor_t);
    cassert_no_null(app);
    cassert_no_null(msg);

    cursor = (cursor_t)msg->attrib;

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_CURSOR Type: %s", deblib_cursor_str(cursor));
        i_log(app, &log);
    }

    bmutex_lock(app->mutex);
    app->cursor_type = cursor;
    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_set_pos(App *app, const DebMsg *msg)
{
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_SET_POS: (%d, %d)", msg->row, msg->col);
        i_log(app, &log);
    }

    bmutex_lock(app->mutex);
    app->cursor_row = msg->row;
    app->cursor_col = msg->col;
    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_putchar(App *app, const DebMsg *msg)
{
    BufChar *bchar = NULL;
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_PUTCHAR Row: %d, Col: %d, Char: '%s', Color: %d, Attrib: %d", msg->row, msg->col, msg->utf8, msg->color, msg->attrib);
        i_log(app, &log);
    }

    cassert(msg->row < app->nrows);
    cassert(msg->col < app->ncols);
    bmutex_lock(app->mutex);
    bchar = app->text_buffer + (msg->row * app->ncols + msg->col);
    str_copy_c(bchar->utf8, sizeof(bchar->utf8), msg->utf8);
    bchar->color = msg->color;
    bchar->attrib = msg->attrib;
    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_puttext(App *app, const DebMsg *msg)
{
    BufChar *bchar = NULL;
    const char_t *text = NULL;
    uint32_t codepoint, nbytes, col;
    cassert_no_null(app);
    cassert_no_null(msg);
    cassert(msg->row < app->nrows);
    
    if (app->print_log == TRUE)
    {
        String *log = str_printf("ekMSG_PUTTEXT Row: %d, Col: %d, Color: %d, Text: '%s'", msg->row, msg->col, msg->color, msg->utf8);
        i_log(app, &log);
    }

    col = msg->col;
    text = msg->utf8;
    bmutex_lock(app->mutex);
    bchar = app->text_buffer + (msg->row * app->ncols + msg->col);
    codepoint = unicode_to_u32b(text, ekUTF8, &nbytes);

    while(codepoint != 0)
    {
        uint32_t nb = unicode_to_char(codepoint, bchar->utf8, ekUTF8);
        cassert_unref(nb == nbytes, nbytes);
        cassert_unref(col < app->ncols, col);
        bchar->utf8[nb] = 0;
        bchar->color = msg->color;
        bchar->attrib = 0;
        text += nb;
        bchar += 1;
        col += 1;
        codepoint = unicode_to_u32b(text, ekUTF8, &nbytes);
    }
    bmutex_unlock(app->mutex);
}

/*---------------------------------------------------------------------------*/

static void i_read_key(App *app, DebMsg *msg)
{
    uint32_t i = 0;
    cassert_no_null(app);
    cassert_no_null(msg);
    msg->key = ENUM_MAX(vkey_t);
    msg->modifiers = 0;

    for (i = 0; i < kKEY_MAX; ++i)
    {
        if (keybuf_pressed(app->keyboard_buffer, (vkey_t)i) == TRUE)
        {
            switch ((vkey_t)i) {
            case ekKEY_LSHIFT:
            case ekKEY_RSHIFT:
                msg->modifiers |= ekMKEY_SHIFT;
                break;
            case ekKEY_LCTRL:
            case ekKEY_RCTRL:
                msg->modifiers |= ekMKEY_CONTROL;
                break;
            case ekKEY_LALT:
            case ekKEY_RALT:
                msg->modifiers |= ekMKEY_ALT;
                break;
            case ekKEY_LWIN:
            case ekKEY_RWIN:
                msg->modifiers |= ekMKEY_COMMAND;
                break;
            default:
                if (msg->key == ENUM_MAX(vkey_t))
                    msg->key = (vkey_t)i;
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_unknown(App *app, const DebMsg *msg)
{
    cassert_no_null(app);
    cassert_no_null(msg);

    if (app->print_log == TRUE)
    {
        String *log = str_printf("Unknown msg: %d", msg->type);
        i_log(app, &log);
    }
}

/*---------------------------------------------------------------------------*/

static uint32_t i_protocol_thread(App *app)
{
    Socket *server_sock = NULL;
    heap_start_mt();

    server_sock = bsocket_server(kSERVER_PORT, 32, NULL);

    if (server_sock != NULL)
    {
        Socket *income_sock = bsocket_accept(server_sock, 0, NULL);
        if (income_sock != NULL)
        {
            Stream *stm = stm_socket(income_sock);
            if (stm != NULL)
            {
                uint32_t ip;
                uint16_t port;
                bsocket_remote_ip(income_sock, &ip, &port);

                if (app->print_log == TRUE)
                {
                    String *log = str_printf("Incomming connect from: %s:%d", bsocket_ip_str(ip), port);
                    i_log(app, &log);
                }

                for(;;)
                {
                    DebMsg msg;
                    deblib_recv_message(stm, &msg);
                    
                    switch (msg.type) {
                    case ekMSG_SET_SIZE:
                        i_set_size(app, &msg);
                        break;
                    case ekMSG_SCROLL:
                        i_scroll(app, &msg);
                        break;
                    case ekMSG_BOX:
                        i_box(app, &msg);
                        break;
                    case ekMSG_CURSOR:
                        i_cursor(app, &msg);
                        break;
                    case ekMSG_SET_POS:
                        i_set_pos(app, &msg);
                        break;
                    case ekMSG_PUTCHAR:
                        i_putchar(app, &msg);
                        break;
                    case ekMSG_PUTTEXT:
                        i_puttext(app, &msg);
                        break;
                    case ekMSG_READ_KEY:
                        i_read_key(app, &msg);
                        stm_write_enum(stm, msg.key, vkey_t);
                        stm_write_u32(stm, msg.modifiers);
                        break;
                    default:
                        i_unknown(app, &msg);
                    }

                    if (msg.type == 12000)
                        break;
                }

                stm_close(&stm);
            }

            bsocket_close(&income_sock);
        }

        bsocket_close(&server_sock);
    }

    heap_end_mt();
    return 0;
}

/*---------------------------------------------------------------------------*/

static App *i_app(void)
{
    App *app = heap_new0(App);
    uint32_t nrows = UINT32_MAX, ncols = UINT32_MAX;
    uint32_t argc = osapp_argc();

    /* Parse arguments */
    if (argc == 3)
    {
        char_t argv[128];
        bool_t err1, err2;
        osapp_argv(1, argv, sizeof(argv));
        nrows = str_to_u32(argv, 10, &err1);
        osapp_argv(2, argv, sizeof(argv));
        ncols = str_to_u32(argv, 10, &err2);
        if (err1 == TRUE || err2 == TRUE)
        {
            nrows = UINT32_MAX;
            ncols = UINT32_MAX;
        }
    }

    if (ncols == UINT32_MAX || nrows == UINT32_MAX)
    {
        nrows = i_DEFAULT_ROWS;
        ncols = i_DEFAULT_COLS;
    }

    i_update_text_buffer(app, nrows, ncols);
    app->keyboard_buffer = keybuf_create();
    app->cursor_type = ekCURSOR_NONE;
    app->cursor_row = UINT32_MAX;
    app->cursor_col = UINT32_MAX;
    app->cursor_draw = FALSE;
    app->last_redraw = -1;

    return app;
}

/*---------------------------------------------------------------------------*/

static App *i_create(void)
{
    App *app = i_app();
    Panel *panel = i_panel(app);
    app->print_log = FALSE;
    log_file("C:\\Users\\Fran\\Desktop\\debugger_log.txt");
    i_init_colors();
    view_size(app->view, s2df(app->ncols * app->cell_width, app->nrows * app->cell_height));
    view_keybuf(app->view, app->keyboard_buffer);
    log_printf("APP: Rows: %d Cols: %d", app->nrows, app->ncols);
    app->window = window_create(ekWINDOW_STD);
    window_panel(app->window, panel);
    window_title(app->window, "GTNap Debugger");
    window_origin(app->window, v2df(500, 200));
    window_OnClose(app->window, listener(app, i_OnClose, App));
    window_show(app->window);
    app->mutex = bmutex_create();
    app->protocol_thread = bthread_create(i_protocol_thread, app, App);
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_update(App *app, const real64_t prtime, const real64_t ctime)
{
    cassert_no_null(app);
    unref(prtime);
    unref(ctime);
    app->cursor_draw = !app->cursor_draw;
    view_update(app->view);
}

/*---------------------------------------------------------------------------*/

static void i_destroy(App **app)
{
    cassert_no_null(app);
    cassert_no_null(*app);
    keybuf_destroy(&(*app)->keyboard_buffer);
    window_destroy(&(*app)->window);
    heap_delete(app, App);
}

/*---------------------------------------------------------------------------*/

#include "osmain.h"
osmain_sync(i_BLINK_INTERVAL, i_create, i_destroy, i_update, "", App)
