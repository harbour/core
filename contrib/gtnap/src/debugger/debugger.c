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
    uint32_t ncols;
    uint32_t nrows;
    real32_t cell_width;
    real32_t cell_height;
};

/*---------------------------------------------------------------------------*/

static uint16_t kSERVER_PORT = 3555;
static uint32_t i_DEFAULT_COLS = 110;
static uint32_t i_DEFAULT_ROWS = 35;

/*---------------------------------------------------------------------------*/

static void i_update_text_buffer(App *app, const uint32_t nrows, const uint32_t ncols)
{
    cassert_no_null(app);
    if (app->text_buffer != NULL)
        heap_delete_n(&app->text_buffer, app->nrows * app->ncols, BufChar);

    app->nrows = nrows;
    app->ncols = ncols;
    app->text_buffer = heap_new_n0(app->nrows * app->ncols, BufChar);
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
    draw_font(p->ctx, app->font);

    /* Access to text_buffer in mutual exclusion
     * Can be modified by secondary (socket listen thread) 
     */
   // bmutex_lock(app->mutex);
    bchar = app->text_buffer;
    //log_printf("i_OnDraw()");

    for (i = 0; i < app->nrows; ++i)
    {
        real32_t y = i * app->cell_height;
        for (j = 0; j < app->ncols; ++j, ++bchar)
        {
            real32_t x = j * app->cell_width;
            if (bchar->utf8[0] != 0)
            {
                draw_text(p->ctx, bchar->utf8, x, y);
                //log_printf("(%d, %d) %s", j, i, bchar->utf8);
            }
        }
    }
    //bmutex_unlock(app->mutex);
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
    i_update_text_buffer(app, i_DEFAULT_ROWS, i_DEFAULT_COLS);


    layout_view(layout, view, 0, 0);
    layout_textview(layout, text, 1, 0);
    layout_hsize(layout, 1, 400);
    //layout_margin(layout, 5);
    //layout_vmargin(layout, 0, 5);
    //layout_vmargin(layout, 1, 5);
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
    bmutex_lock(app->mutex);
    if (app->print_log == TRUE)
    {
        textview_writef(app->text, tc(*str));
        textview_writef(app->text, "\n");
    }
    bmutex_unlock(app->mutex);
    str_destroy(str);
}

/*---------------------------------------------------------------------------*/

static void i_set_size(App *app, const DebMsg *msg)
{
    String *log = NULL;
    uint32_t nrows, ncols;
    cassert_no_null(app);
    cassert_no_null(msg);
    nrows = msg->row;
    ncols = msg->col;
    log = str_printf("ekMSG_SET_SIZE Rows: %d, Cols: %d", nrows, ncols);
    i_log(app, &log);

    //if (app->nrows != nrows || app->ncols != ncols)
    //{
    //    i_update_text_buffer(app, nrows, ncols);
    //    view_update(app->view);
    //}
}

/*---------------------------------------------------------------------------*/

static void i_putchar(App *app, const DebMsg *msg)
{
    String *log = NULL;
    BufChar *bchar = NULL;
    cassert_no_null(app);
    cassert_no_null(msg);
    log = str_printf("ekMSG_PUTCHAR Row: %d, Col: %d, Char: '%s', Color: %d, Attrib: %d", msg->row, msg->col, msg->utf8, msg->color, msg->attrib);
    i_log(app, &log);
    cassert(msg->row < app->nrows);
    cassert(msg->col < app->ncols);
    bmutex_lock(app->mutex);
    bchar = app->text_buffer + (msg->row * app->ncols + msg->col);
    str_copy_c(bchar->utf8, sizeof(bchar->utf8), msg->utf8);
    bchar->color = msg->color;
    bchar->attrib = msg->attrib;
    bmutex_unlock(app->mutex);
    view_update(app->view);
}

/*---------------------------------------------------------------------------*/

static void i_puttext(App *app, const DebMsg *msg)
{
    String *log = NULL;
    BufChar *bchar = NULL;
    const char_t *text = NULL;
    uint32_t codepoint, nbytes, col;
    cassert_no_null(app);
    cassert_no_null(msg);
    log = str_printf("ekMSG_PUTTEXT Row: %d, Col: %d, Color: %d, Text: '%s'", msg->row, msg->col, msg->color, msg->utf8);
    i_log(app, &log);
    col = msg->col;
    //log_printf("ROW: %d", msg->row);
    cassert(msg->row < app->nrows);
    text = msg->utf8;
    bmutex_lock(app->mutex);
    bchar = app->text_buffer + (msg->row * app->ncols + msg->col);
    codepoint = unicode_to_u32b(text, ekUTF8, &nbytes);

    while(codepoint != 0)
    {
        uint32_t nb = unicode_to_char(codepoint, bchar->utf8, ekUTF8);
        cassert_unref(nb == nbytes, nbytes);
        //log_printf("COL: %d", col);
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
    view_update(app->view);
}

/*---------------------------------------------------------------------------*/

static void i_unknown(App *app, const DebMsg *msg)
{
    String *log = NULL;
    cassert_no_null(app);
    cassert_no_null(msg);
    log = str_printf("Unknown msg: %d", msg->type);
    i_log(app, &log);
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
                String *str = NULL;
                bsocket_remote_ip(income_sock, &ip, &port);
                str = str_printf("Incomming connect from: %s:%d", bsocket_ip_str(ip), port);
                i_log(app, &str);
                for(;;)
                {
                    DebMsg msg;
                    deblib_recv_message(stm, &msg);
                    
                    switch (msg.type) {
                    case ekMSG_SET_SIZE:
                        i_set_size(app, &msg);
                        break;
                    case ekMSG_PUTCHAR:
                        i_putchar(app, &msg);
                        break;
                    case ekMSG_PUTTEXT:
                        i_puttext(app, &msg);
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

static App *i_create(void)
{
    App *app = heap_new0(App);
    Panel *panel = i_panel(app);
    app->print_log = TRUE;
    log_file("C:\\Users\\Fran\\Desktop\\debugger_log.txt");
    app->window = window_create(ekWINDOW_STD);
    window_panel(app->window, panel);
    window_title(app->window, "GTNap Debugger");
    window_origin(app->window, v2df(500, 200));
    window_OnClose(app->window, listener(app, i_OnClose, App));
    window_show(app->window);
    app->mutex = bmutex_create();
    app->protocol_thread = bthread_create(i_protocol_thread, app, App);
    //i_protocol_thread(app);
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_destroy(App **app)
{
    window_destroy(&(*app)->window);
    heap_delete(app, App);
}

/*---------------------------------------------------------------------------*/

#include "osmain.h"
osmain(i_create, i_destroy, "", App)