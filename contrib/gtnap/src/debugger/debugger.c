/* NAppGUI Hello World */

#include <deblib/deblib.h>
#include <nappgui.h>

typedef struct _app_t App;

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
    uint32_t num_cols;
    uint32_t num_rows;
    real32_t cell_width;
    real32_t cell_height;
};

/*---------------------------------------------------------------------------*/

static uint16_t kSERVER_PORT = 3555;
static uint32_t i_DEFAULT_COLS = 78;
static uint32_t i_DEFAULT_ROWS = 23;

/*---------------------------------------------------------------------------*/

static Panel *i_panel(App *app)
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(2, 1);
    View *view = view_create();
    TextView *text = textview_create();
    app->font = font_monospace(20, 0);
    app->text = text;
    app->view = view;
    font_extents(app->font, "OOOO", -1, &app->cell_width, &app->cell_height);
    app->cell_width /= 4;
    app->num_cols = i_DEFAULT_COLS;
    app->num_rows = i_DEFAULT_ROWS;
    view_size(view, s2df(app->num_cols * app->cell_width, app->num_rows * app->cell_height));
    layout_view(layout, view, 0, 0);
    layout_textview(layout, text, 1, 0);
    layout_hsize(layout, 1, 200);
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
    nrows = msg->p0;
    ncols = msg->p1;
    log = str_printf("ekMSG_SET_SIZE Rows: %d, Cols: %d", nrows, ncols);
    i_log(app, &log);

    if (app->num_rows != nrows || app->num_cols != ncols)
    {
        app->num_rows = nrows;
        app->num_cols = ncols;
        view_size(app->view, s2df(app->num_cols * app->cell_width, app->num_rows * app->cell_height));
        window_update(app->window);
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
                    cassert_default();
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