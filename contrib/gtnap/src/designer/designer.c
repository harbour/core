/* NAppGUI Designer main */

#include <nappgui.h>
#include "res_designer.h"
#include "dlabel.h"
#include "dlayout.h"
#include "dform.h"

typedef struct _app_t App;

struct _app_t
{
    Window *window;
    Label *status_label;
    Label *cells_label;
    Progress *progress;

    View *canvas;
    Layout *canvas_layout;
    Layout *main_layout;

    /* GUI Bindings */
    widget_t swidget;
    Image *add_icon;

    /* Editing forms */
    DForm *form;
};

/*---------------------------------------------------------------------------*/

static void i_dbind(void)
{
    /* Registration of editable structures */
    dbind_enum(celltype_t, ekCELL_TYPE_EMPTY, "");
    dbind_enum(celltype_t, ekCELL_TYPE_LABEL, "");
    dbind_enum(celltype_t, ekCELL_TYPE_LAYOUT, "");
    dbind_enum(align_t, ekLEFT, "");
    dbind_enum(align_t, ekTOP, "");
    dbind_enum(align_t, ekCENTER, "");
    dbind_enum(align_t, ekRIGHT, "");
    dbind_enum(align_t, ekBOTTOM, "");
    dbind_enum(align_t, ekJUSTIFY, "");
    dbind(DLabel, String*, text);
    dbind(DColumn, real32_t, margin_right);
    dbind(DRow, real32_t, margin_bottom);
    dbind(DCell, celltype_t, type);
    dbind(DCell, align_t, halign);
    dbind(DCell, align_t, valign);
    dbind(DLayout, String*, name);
    dbind(DLayout, real32_t, margin_left);
    dbind(DLayout, real32_t, margin_top);
    dbind(DLayout, ArrSt(DColumn)*, cols);
    dbind(DLayout, ArrSt(DRow)*, rows);
    dbind(DLayout, ArrSt(DCell)*, cells);

    /* Don't move, we must first declare the inner struct */
    dbind(DCellContent, DLabel*, label);
    dbind(DCellContent, DLayout*, layout);
    dbind(DCell, DCellContent, content);

    /* GUI */
    dbind_enum(widget_t, ekWIDGET_SELECT, "");
    dbind_enum(widget_t, ekWIDGET_GRID_LAYOUT, "");
    dbind_enum(widget_t, ekWIDGET_LABEL, "");
    dbind_enum(widget_t, ekWIDGET_BUTTON, "");
    dbind_enum(widget_t, ekWIDGET_CHECKBOX, "");
    dbind_enum(widget_t, ekWIDGET_EDITBOX, "");
    dbind(App, widget_t, swidget);
}

/*---------------------------------------------------------------------------*/

static Layout *i_tools_layout(App *app, ResPack *pack)
{
    Layout *layout = layout_create(9, 1);
    Button *button1 = button_flat();
    Button *button2 = button_flat();
    Button *button3 = button_flat();
    Button *button4 = button_flat();
    Button *button5 = button_flat();
    Button *button6 = button_flat();
    Button *button7 = button_flat();
    Button *button8 = button_flat();
    button_image(button1, image_from_resource(pack, FOLDER24_PNG));
    button_image(button2, image_from_resource(pack, DISK24_PNG));
    button_image(button3, image_from_resource(pack, EDIT24_PNG));
    button_image(button4, image_from_resource(pack, SEARCH24_PNG));
    button_image(button5, image_from_resource(pack, PLUS24_PNG));
    button_image(button6, image_from_resource(pack, PLUS24_PNG));
    button_image(button7, image_from_resource(pack, ERROR24_PNG));
    button_image(button8, image_from_resource(pack, ERROR24_PNG));
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    layout_button(layout, button3, 2, 0);
    layout_button(layout, button4, 3, 0);
    layout_button(layout, button5, 5, 0);
    layout_button(layout, button6, 6, 0);
    layout_button(layout, button7, 7, 0);
    layout_button(layout, button8, 8, 0);
    layout_hexpand(layout, 4);
    unref(app);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_widgets_layout(App *app)
{
    Layout *layout = layout_create(1, 6);
    Button *radio1 = button_radio();
    Button *radio2 = button_radio();
    Button *radio3 = button_radio();
    Button *radio4 = button_radio();
    Button *radio5 = button_radio();
    Button *radio6 = button_radio();
    button_text(radio1, "Select");
    button_text(radio2, "Grid layout");
    button_text(radio3, "Label");
    button_text(radio4, "Button");
    button_text(radio5, "Checkbox");
    button_text(radio6, "Editbox");
    layout_button(layout, radio1, 0, 0);
    layout_button(layout, radio2, 0, 1);
    layout_button(layout, radio3, 0, 2);
    layout_button(layout, radio4, 0, 3);
    layout_button(layout, radio5, 0, 4);
    layout_button(layout, radio6, 0, 5);
    layout_vmargin(layout, 0, 5);
    layout_vmargin(layout, 1, 5);
    layout_vmargin(layout, 2, 5);
    layout_vmargin(layout, 3, 5);
    layout_vmargin(layout, 4, 5);    
    unref(app);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_left_layout(App *app)
{
    Layout *layout1 = layout_create(1, 5);
    Layout *layout2 = i_widgets_layout(app);
    Label *label1 = label_create();
    Label *label2 = label_create();
    ListBox *list1 = listbox_create();
    label_text(label1, "Forms");
    label_text(label2, "Widgets");
    listbox_add_elem(list1, "Form 1", NULL);
    listbox_add_elem(list1, "Form 2", NULL);
    listbox_add_elem(list1, "Form 3", NULL);
    listbox_add_elem(list1, "Form 4", NULL);
    listbox_add_elem(list1, "Form 5", NULL);
    listbox_add_elem(list1, "Table", NULL);
    listbox_add_elem(list1, "View", NULL);
    listbox_add_elem(list1, "Edit 1", NULL);
    listbox_add_elem(list1, "Edit 2", NULL);
    listbox_select(list1, 0, TRUE);

    /* Natural size of listboxes */
    listbox_size(list1, s2df(150, 100));

    layout_label(layout1, label1, 0, 0);
    layout_label(layout1, label2, 0, 2);
    layout_listbox(layout1, list1, 0, 1);
    layout_layout(layout1, layout2, 0, 3);

    layout_valign(layout1, 0, 3, ekTOP);
    layout_vmargin(layout1, 1, 5);
    layout_vmargin(layout1, 2, 5);
    layout_vexpand2(layout1, 1, 4, .75f);

    cell_dbind(layout_cell(layout1, 0, 3), App, widget_t, swidget);
    return layout1;
}

/*---------------------------------------------------------------------------*/

static Layout *i_right_layout(App *app)
{
    Layout *layout = layout_create(1, 4);
    Label *label1 = label_create();
    Label *label2 = label_create();
    TableView *table = tableview_create();
    label_text(label1, "Object inspector");
    label_text(label2, "Property editor");
    tableview_new_column_text(table);
    tableview_size(table, s2df(150, 200));
    tableview_column_width(table, 0, 120);
    tableview_update(table);
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 2);
    layout_tableview(layout, table, 0, 1);
    layout_vexpand2(layout, 1, 3, .5f);
    layout_vmargin(layout, 1, 5.f);
    unref(app);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnDraw(App *app, Event *e)
{
    const EvDraw *p = event_params(e, EvDraw);
    cassert_no_null(app);
    //Font *font = font_system(30, 0);
    draw_clear(p->ctx, kCOLOR_YELLOW);

    if (app->form != NULL)
    {
        dform_draw(app->form, app->swidget, app->add_icon, p->ctx);
    }
    //draw_font(p->ctx, font);
    //draw_text(p->ctx, "--> CANVAS <--", 0, 0);
    //font_destroy(&font);
}

/*---------------------------------------------------------------------------*/

static void i_OnMove(App *app, Event *e)
{
    const EvMouse *p = event_params(e, EvMouse);
    cassert_no_null(app);
    if (app->form != NULL)
    {
        if (dform_OnMove(app->form, p->x, p->y) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnExit(App *app, Event *e)
{
    cassert_no_null(app);
    unref(e);
    if (app->form != NULL)
    {
        if (dform_OnExit(app->form) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnClick(App *app, Event *e)
{
    cassert_no_null(app);
    if (app->form != NULL)
    {
        const EvMouse *p = event_params(e, EvMouse);
        if (dform_OnClick(app->form, app->window, app->canvas, app->swidget, p->x, p->y, p->button) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnSize(App *app, Event *e)
{       
    cassert_no_null(app);
    unref(e);
    if (app->form != NULL)
        dform_synchro_visual(app->form);
}

/*---------------------------------------------------------------------------*/

static Layout *i_canvas_layout(App *app)
{
    Layout *layout = layout_create(1, 1);
    View *view = view_scroll();
    view_size(view, s2df(450, 200));
    view_OnDraw(view, listener(app, i_OnDraw, App));
    view_OnMove(view, listener(app, i_OnMove, App));
    view_OnExit(view, listener(app, i_OnExit, App));
    view_OnClick(view, listener(app, i_OnClick, App));
    view_OnSize(view, listener(app, i_OnSize, App));
    layout_view(layout, view, 0, 0);
    app->canvas = view;
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_middle_layout(App *app)
{
    Layout *layout1 = layout_create(3, 1);
    Layout *layout2 = i_left_layout(app);
    Layout *layout3 = i_canvas_layout(app);
    Layout *layout4 = i_right_layout(app);
    layout_layout(layout1, layout2, 0, 0);
    layout_layout(layout1, layout3, 1, 0);
    layout_layout(layout1, layout4, 2, 0);

    /* A small horizontal margin between view cell and list (left) table (right) layouts */
    layout_hmargin(layout1, 0, 3);
    layout_hmargin(layout1, 1, 3);

    /* All the horizontal expansion will be done in the middle cell (view)
       list_layout (left) and table_layout (right) will preserve the 'natural' width */
    layout_hexpand(layout1, 1);
    app->canvas_layout = layout3;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static Layout *i_statusbar_layout(App *app)
{
    Layout *layout = layout_create(4, 1);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Progress *progress = progress_create();

    label_align(label2, ekRIGHT);

    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 3, 0);
    layout_progress(layout, progress, 1, 0);
    layout_hmargin(layout, 0, 10);

    /* All the horizontal expansion will be done in empty column-cell(2) */
    layout_hexpand(layout, 2);

    label_text(label1, "status-1");
    label_text(label2, "status-2");

    /* Keep the controls for futher updates */
    app->status_label = label1;
    app->cells_label = label2;
    app->progress = progress;
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
    // layout_halign(layout1, 0, 0, ekJUSTIFY);
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

static Panel *i_panel(App *app, ResPack *pack)
{
    Panel *panel = panel_create();
    Layout *layout = i_main_layout(app, pack);
    app->main_layout = layout;
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

static App *i_app(ResPack *pack)
{
    App *app = heap_new0(App);
    i_dbind();
    app->swidget = ekWIDGET_SELECT;
    app->add_icon = image_copy(image_from_resource(pack, PLUS16_PNG));
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_init_forms(App *app)
{
    cassert_no_null(app);
    cassert(app->form == NULL);
    app->form = dform_first_example();
    dform_compose(app->form);
    dform_synchro_visual(app->form);
}

/*---------------------------------------------------------------------------*/

static App *i_create(void)
{
    ResPack *pack = res_designer_respack("");
    App *app = i_app(pack);
    Panel *panel = i_panel(app, pack);
    app->window = window_create(ekWINDOW_STDRES);
    window_panel(app->window, panel);
    window_title(app->window, "GTNAP Designer");
    window_origin(app->window, v2df(500, 200));
    window_OnClose(app->window, listener(app, i_OnClose, App));
    window_show(app->window);
    layout_dbind(app->main_layout, NULL, App);
    layout_dbind_obj(app->main_layout, app, App);
    respack_destroy(&pack);
    i_init_forms(app);
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_destroy(App **app)
{
    cassert_no_null(app);
    cassert_no_null(*app);
    image_destroy(&(*app)->add_icon);
    dform_destroy(&(*app)->form);
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
