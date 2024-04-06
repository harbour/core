/* NAppGUI Hello World */

#include <nappgui.h>

typedef struct _app_t App;

struct _app_t
{
    Window *window;
    TextView *text;
    uint32_t clicks;

    bool_t alive;
    Mutex *mutex;
    Thread *protocol_thread;
};

/*---------------------------------------------------------------------------*/

uint16_t kSERVER_PORT = 3555;

/*---------------------------------------------------------------------------*/

static void i_OnButton(App *app, Event *e)
{
    textview_printf(app->text, "Button click (%d)\n", app->clicks);
    app->clicks += 1;
    unref(e);
}

/*---------------------------------------------------------------------------*/

static Panel *i_panel(App *app)
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(1, 3);
    Label *label = label_create();
    Button *button = button_push();
    TextView *text = textview_create();
    app->text = text;
    label_text(label, "Hello!, I'm a label");
    button_text(button, "Click Me!");
    button_OnClick(button, listener(app, i_OnButton, App));
    layout_label(layout, label, 0, 0);
    layout_button(layout, button, 0, 1);
    layout_textview(layout, text, 0, 2);
    layout_hsize(layout, 0, 250);
    layout_vsize(layout, 2, 100);
    layout_margin(layout, 5);
    layout_vmargin(layout, 0, 5);
    layout_vmargin(layout, 1, 5);
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
                for(;;)
                {
                    uint16_t cmd = stm_read_u16(stm);
                    bmutex_lock(app->mutex);
                    textview_printf(app->text, "CMD: (%d)\n", cmd);
                    bmutex_unlock(app->mutex);

                    if (cmd == 10000)
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