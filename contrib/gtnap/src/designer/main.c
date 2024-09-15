/* NAppGUI Designer */

#include <nappgui.h>
#include "res_designer.h"

typedef struct _app_t App;

struct _app_t
{
    Window *window;
    TextView *text;
    uint32_t clicks;
};

/*---------------------------------------------------------------------------*/

static void i_OnButton(App *app, Event *e)
{
    textview_printf(app->text, "Button click (%d)\n", app->clicks);
    app->clicks += 1;
    unref(e);
}

/*---------------------------------------------------------------------------*/

static Layout *i_tools_layout(App *app, ResPack *pack)
{
    Layout *layout = layout_create(8, 1);
    Button *button1 = button_flat();
    Button *button2 = button_flat();
    Button *button3 = button_flat();
    Button *button4 = button_flat();
    Button *button5 = button_flat();
    Button *button6 = button_flat();
    Button *button7 = button_flat();
    Button *button8 = button_flat();
    button_image(button1, image_from_resource(pack, FOLDER32_PNG));
    button_image(button2, image_from_resource(pack, DISK32_PNG));
    button_image(button3, image_from_resource(pack, EDIT32_PNG));
    button_image(button4, image_from_resource(pack, SEARCH32_PNG));
    button_image(button5, image_from_resource(pack, PLUS24_PNG));
    button_image(button6, image_from_resource(pack, PLUS24_PNG));
    button_image(button7, image_from_resource(pack, ERROR24_PNG));
    button_image(button8, image_from_resource(pack, ERROR24_PNG));
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    layout_button(layout, button3, 2, 0);
    layout_button(layout, button4, 3, 0);
    layout_button(layout, button5, 4, 0);
    layout_button(layout, button6, 5, 0);
    layout_button(layout, button7, 6, 0);
    layout_button(layout, button8, 7, 0);
    unref(app);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_main_layout(App *app, ResPack *pack)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_tools_layout(app, pack);
    Layout *layout3 = i_middle_layout(app);
    Layout *layout4 = i_statusbar_layout(app);
    layout_layout(layout1, layout2, 0, 0);
    layout_layout(layout1, layout3, 0, 1);
    layout_layout(layout1, layout4, 0, 2);

    /* 
     * All the vertical expansion will be done in the middle layout
     * tools_layout (top) and statusbar_layout (bottom) will preserve 
     * the 'natural' height 
     */
    layout_vexpand(layout1, 1);

    /* A vertical margins between middle and (controls, info) */
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);

    /* A border margin for all layout edges */
    layout_margin(layout1, 5);

    return layout1;
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

static App *i_create(void)
{
    App *app = heap_new0(App);
    ResPack *pack = res_designer_respack("");
    Panel *panel = i_panel(app);
    app->window = window_create(ekWINDOW_STD);
    window_panel(app->window, panel);
    window_title(app->window, "Hello, World!");
    window_origin(app->window, v2df(500, 200));
    window_OnClose(app->window, listener(app, i_OnClose, App));
    window_show(app->window);
    respack_destroy(&pack);
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_destroy(App **app)
{
    window_destroy(&(*app)->window);
    heap_delete(app, App);
}

/*---------------------------------------------------------------------------*/

static void i_update(App *app, const real64_t prtime, const real64_t ctime)
{
    unref(app);
    unref(prtime);
    unref(ctime);
}

/*---------------------------------------------------------------------------*/

#include "osmain.h"
osmain_sync(0.1, i_create, i_destroy, i_update, "", App)
