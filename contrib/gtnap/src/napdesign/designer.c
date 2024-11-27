/* NAppGUI Designer Application */

#include <nform/nform.h>
#include <nappgui.h>
#include "res_designer.h"
#include "dlayout.h"
#include "dform.h"
#include "dialogs.h"
#include "propedit.h"
#include "inspect.h"

struct _desiger_t
{
    Window *window;
    widget_t swidget;
    String *folder_path;
    ArrPt(DForm) *forms;
    uint32_t sel_form;

    ListBox *form_list;
    Label *status_label;
    Label *cells_label;
    Progress *progress;
    View *canvas;
    Panel *inspect;
    Panel *propedit;
    Layout *widgets_layout;
    Cell *widgets_cell;
    Cell *open_form_cell;
    Cell *save_form_cell;
    Cell *run_form_cell;
    Cell *add_form_cell;
    Cell *remove_form_cell;
    Cell *rename_form_cell;
    Image *add_icon;
};

DeclPt(DForm);

/*---------------------------------------------------------------------------*/

static const char_t *i_FILE_EXT = "nfm";
static const char_t *i_SAVE_MARK = "• ";

/*---------------------------------------------------------------------------*/

static void i_dbind(void)
{
    dbind_enum(widget_t, ekWIDGET_SELECT, "");
    dbind_enum(widget_t, ekWIDGET_GRID_LAYOUT, "");
    dbind_enum(widget_t, ekWIDGET_LABEL, "");
    dbind_enum(widget_t, ekWIDGET_BUTTON, "");
    dbind_enum(widget_t, ekWIDGET_CHECKBOX, "");
    dbind_enum(widget_t, ekWIDGET_EDITBOX, "");
    dbind(Designer, widget_t, swidget);
}

/*---------------------------------------------------------------------------*/

static void i_destroy_form_opt(DForm **form)
{
    cassert_no_null(form);
    if (*form != NULL)
        dform_destroy(form);
}

/*---------------------------------------------------------------------------*/

static void i_need_save_mark(ListBox *listbox, const uint32_t pos, const bool_t needs_save)
{
    const char_t *name = listbox_text(listbox, pos);
    bool_t with_bullet = str_is_prefix(name, i_SAVE_MARK);
    if (needs_save != with_bullet)
    {
        if (needs_save == TRUE)
        {
            String *nname = str_printf("%s%s", i_SAVE_MARK, name);
            listbox_set_elem(listbox, pos, tc(nname), NULL);
            str_destroy(&nname);
        }
        else
        {
            name += str_len_c(i_SAVE_MARK);
            listbox_set_elem(listbox, pos, name, NULL);            
        }
    }
}

/*---------------------------------------------------------------------------*/

static bool_t i_need_save(Designer *app)
{
    cassert_no_null(app);
    cassert(arrpt_size(app->forms, DForm) == listbox_count(app->form_list));
    arrpt_foreach(form, app->forms, DForm)
        if (form != NULL && dform_need_save(form) == TRUE)
            return TRUE;
    arrpt_end()
    return FALSE;
}

/*---------------------------------------------------------------------------*/

static void i_update_form_controls(Designer *app, const bool_t enable)
{
    bool_t enable_save = FALSE;
    bool_t enable_run = FALSE;
    bool_t enable_remove = FALSE;
    bool_t enable_rename = FALSE;
    cassert_no_null(app);

    if (enable == TRUE)
    {
        enable_save = i_need_save(app);

        if (app->sel_form != UINT32_MAX)
        {
            enable_run = TRUE;
            enable_remove = TRUE;
            enable_rename = TRUE;
        }
    }

    arrpt_foreach(form, app->forms, DForm)
        bool_t need_save = FALSE;
        if (form != NULL)
            need_save = dform_need_save(form);
        i_need_save_mark(app->form_list, form_i, need_save);
    arrpt_end()

    /*cell_enabled(app->open_form_cell, enable);*/
    cell_enabled(app->save_form_cell, enable_save);
    cell_enabled(app->run_form_cell, enable_run);
    cell_enabled(app->add_form_cell, enable);
    cell_enabled(app->remove_form_cell, enable_remove);
    cell_enabled(app->rename_form_cell, enable_rename);
    cell_enabled(app->widgets_cell, enable);
}

/*---------------------------------------------------------------------------*/

static void i_open_form(Designer *app, const uint32_t index)
{
    DForm *form = NULL;
    cassert_no_null(app);
    if (index != UINT32_MAX)
    {
        form = arrpt_get(app->forms, index, DForm);
        if (form == NULL)
        {
            const char_t *name = listbox_text(app->form_list, index);
            String *path = str_cpath("%s/%s.%s", tc(app->folder_path), name, i_FILE_EXT);
            Stream *stm = NULL;
            DForm **forms = arrpt_all(app->forms, DForm);
            cassert(str_is_prefix(name, i_SAVE_MARK) == FALSE);
            stm = stm_from_file(tc(path), NULL);
            if (stm != NULL)
            {
                form = dform_read(stm, app);
                stm_close(&stm);            
            }

            forms[index] = form;
            str_destroy(&path);
        }

        if (form != NULL)
        {
            dform_compose(form);
            dform_set(form, app->inspect, app->propedit);
        }
    }

    view_update(app->canvas);
}

/*---------------------------------------------------------------------------*/

static void i_init_forms(Designer *app, const char_t *path)
{
    ArrSt(DirEntry) *files = NULL;
    ferror_t err = ekFNOPATH;
    cassert_no_null(app);
    if (str_empty_c(path) == FALSE)
        files = hfile_dir_list(path, FALSE, &err);

    arrpt_clear(app->forms, i_destroy_form_opt, DForm);
    listbox_clear(app->form_list);

    if (err == ekFOK)
    {
        uint32_t n = UINT32_MAX;

        str_upd(&app->folder_path, path);
        arrst_foreach(file, files, DirEntry)
            String *fil = NULL;
            String *ext = NULL;
            str_split_pathext(tc(file->name), NULL, &fil, &ext);
            if (str_equ_c(tc(ext), i_FILE_EXT) == TRUE)
            {
                listbox_add_elem(app->form_list, tc(fil), NULL);
                arrpt_append(app->forms, NULL, DForm);
            }

            str_destroy(&fil);
            str_destroy(&ext);
        arrst_end()

        n = arrpt_size(app->forms, DForm);
        if (n > 0)
        {
            if (app->sel_form > n)
                app->sel_form = 0;
            i_open_form(app, app->sel_form);
            listbox_select(app->form_list, app->sel_form, TRUE);
        }
        else
        {
            app->sel_form = UINT32_MAX;
        }

        i_update_form_controls(app, TRUE);

        {
            Button *button = cell_button(app->open_form_cell);
            String *tooltip = str_printf("Open forms folder (%s)", tc(app->folder_path));
            button_tooltip(button, tc(tooltip));
            str_destroy(&tooltip);
        }
    }
    else
    {
        Button *button = cell_button(app->open_form_cell);
        i_update_form_controls(app, FALSE);
        button_tooltip(button, "Open forms folder (No path selected)");
    }

    view_update(app->canvas);
    arrst_destopt(&files, hfile_dir_entry_remove, DirEntry);
}

/*---------------------------------------------------------------------------*/

static void i_save_forms(Designer *app)
{
    cassert_no_null(app);
    arrpt_foreach(form, app->forms, DForm)
        bool_t need_save = FALSE;
        if (form != NULL)
            need_save = dform_need_save(form);
        
        if (need_save == TRUE)
        {
            const char_t *name = listbox_text(app->form_list, form_i);
            String *path = NULL;
            Stream *stm = NULL;
            
            if (str_is_prefix(name, i_SAVE_MARK) == TRUE)
                name += str_len_c(i_SAVE_MARK);

            path = str_cpath("%s/%s.%s", tc(app->folder_path), name, i_FILE_EXT);
            stm = stm_to_file(tc(path), NULL);
            if (stm != NULL)
            {
                dform_write(stm, form);
                stm_close(&stm);
            }

            str_destroy(&path);
        }
    arrpt_end()
}

/*---------------------------------------------------------------------------*/

static void i_OnOpenFormsClick(Designer *app, Event *e)
{
    bool_t can_open = TRUE;
    cassert_no_null(app);
    unref(e);
    if (i_need_save(app) == TRUE)
    {
        uint8_t ret = dialog_unsaved_changes(app->window);
        if (ret == 1)
            i_save_forms(app);
        else if (ret == 2)
            can_open = FALSE;
    }

    if (can_open == TRUE)
    {
        const char_t *ftype = "..DIR..";
        const char_t *folder = comwin_open_file(app->window, &ftype, 1, tc(app->folder_path));
        if (folder != NULL)
            i_init_forms(app, folder);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnSaveFormsClick(Designer *app, Event *e)
{
    i_save_forms(app);
    i_update_form_controls(app, TRUE);
    unref(e);
}

/*---------------------------------------------------------------------------*/

static void i_OnSimulateClick(Designer *app, Event *e)
{
    cassert_no_null(app);
    unref(e);
    if (app->sel_form != UINT32_MAX)
    {
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        dform_simulate(form, app->window);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnRemoveClick(Designer *app, Event *e)
{
    const char_t *name = NULL;
    cassert_no_null(app);
    unref(e);
    name = listbox_text(app->form_list, app->sel_form);
    if (str_is_prefix(name, i_SAVE_MARK) == TRUE)
        name += str_len_c(i_SAVE_MARK);

    if (dialog_remove_form(app->window, name) == TRUE)
    {
        String *path = str_cpath("%s/%s.%s", tc(app->folder_path), name, i_FILE_EXT);
        bool_t removed = TRUE;

        if (hfile_exists(tc(path), NULL) == TRUE)
            removed = bfile_delete(tc(path), NULL);

        if (removed == TRUE)
        {
            uint32_t n = UINT32_MAX;
            listbox_del_elem(app->form_list, app->sel_form);
            arrpt_delete(app->forms, app->sel_form, i_destroy_form_opt, DForm);
            n = arrpt_size(app->forms, DForm);

            if (n > 0)
            {
                if (app->sel_form >= n)
                    app->sel_form = n - 1;
                listbox_select(app->form_list, app->sel_form, TRUE);
            }
            else
            {
                app->sel_form = UINT32_MAX;
            }

            i_open_form(app, app->sel_form);
            i_update_form_controls(app, TRUE);
        }

        str_destroy(&path);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnAddFormClick(Designer *app, Event *e)
{
    String *fname = NULL;
    cassert_no_null(app);
    unref(e);
    fname = dialog_form_name(app->window, NULL);
    if (str_empty(fname) == FALSE)
    {
        uint32_t n = listbox_count(app->form_list);
        DForm *form = dform_empty(app);
        dform_compose(form);
        cassert(n == arrpt_size(app->forms, DForm));
        listbox_add_elem(app->form_list, tc(fname), NULL);
        listbox_select(app->form_list, n, TRUE);
        arrpt_append(app->forms, form, DForm);
        app->sel_form = n;
        i_update_form_controls(app, TRUE);
        view_update(app->canvas);
    }

    str_destroy(&fname);
}

/*---------------------------------------------------------------------------*/

static Layout *i_tools_layout(Designer *app, ResPack *pack)
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
    cassert_no_null(app);
    button_image(button1, image_from_resource(pack, FOLDER24_PNG));
    button_image(button2, image_from_resource(pack, DISK24_PNG));
    button_image(button3, image_from_resource(pack, PLUS24_PNG));
    button_image(button4, image_from_resource(pack, EDIT24_PNG));
    button_image(button5, image_from_resource(pack, SEARCH24_PNG));
    button_image(button6, image_from_resource(pack, ERROR24_PNG));
    button_image(button7, image_from_resource(pack, PLUS24_PNG));
    button_image(button8, image_from_resource(pack, ERROR24_PNG));
    button_OnClick(button1, listener(app, i_OnOpenFormsClick, Designer));
    button_OnClick(button2, listener(app, i_OnSaveFormsClick, Designer));
    button_OnClick(button3, listener(app, i_OnAddFormClick, Designer));
    button_OnClick(button5, listener(app, i_OnSimulateClick, Designer));
    button_OnClick(button6, listener(app, i_OnRemoveClick, Designer));
    button_tooltip(button1, "Open forms folder");
    button_tooltip(button2, "Save all forms");
    button_tooltip(button3, "Add new form");
    button_tooltip(button4, "Rename form");
    button_tooltip(button5, "Simulate current form");
    button_tooltip(button6, "Remove current form");
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    layout_button(layout, button3, 2, 0);
    layout_button(layout, button4, 3, 0);
    layout_button(layout, button5, 4, 0);
    layout_button(layout, button6, 5, 0);
    layout_button(layout, button7, 7, 0);
    layout_button(layout, button8, 8, 0);
    layout_hexpand(layout, 6);
    app->open_form_cell = layout_cell(layout, 0, 0);
    app->save_form_cell = layout_cell(layout, 1, 0);
    app->add_form_cell = layout_cell(layout, 2, 0);
    app->rename_form_cell = layout_cell(layout, 3, 0);
    app->run_form_cell = layout_cell(layout, 4, 0);
    app->remove_form_cell = layout_cell(layout, 5, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_widgets_layout(Designer *app)
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

static void i_OnFormSelect(Designer *app, Event *e)
{
    const EvButton *p = event_params(e, EvButton);
    cassert_no_null(app);
    app->sel_form = p->index;
    i_open_form(app, app->sel_form);
    i_update_form_controls(app, TRUE);
}

/*---------------------------------------------------------------------------*/

static Layout *i_left_layout(Designer *app)
{
    Layout *layout1 = layout_create(1, 5);
    Layout *layout2 = i_widgets_layout(app);
    Label *label1 = label_create();
    Label *label2 = label_create();
    ListBox *list1 = listbox_create();
    cassert_no_null(app);
    label_text(label1, "Forms");
    label_text(label2, "Widgets");
    listbox_size(list1, s2df(150, 100));
    listbox_OnSelect(list1, listener(app, i_OnFormSelect, Designer));
    layout_label(layout1, label1, 0, 0);
    layout_label(layout1, label2, 0, 2);
    layout_listbox(layout1, list1, 0, 1);
    layout_layout(layout1, layout2, 0, 3);
    layout_valign(layout1, 0, 3, ekTOP);
    layout_vmargin(layout1, 1, 5);
    layout_vmargin(layout1, 2, 5);
    layout_vexpand2(layout1, 1, 4, .75f);
    cell_dbind(layout_cell(layout1, 0, 3), Designer, widget_t, swidget);
    app->form_list = list1;
    app->widgets_cell = layout_cell(layout1, 0, 3);
    app->widgets_layout = layout2;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static Layout *i_right_layout(Designer *app)
{
    Layout *layout = layout_create(1, 4);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Panel *panel1 = inspect_create(app);
    Panel *panel2 = propedit_create(app);
    cassert_no_null(app);
    label_text(label1, "Object inspector");
    label_text(label2, "Property editor");
    panel_size(panel1, s2df(150, 200));
    panel_size(panel2, s2df(150, 200));
    layout_label(layout, label1, 0, 0);
    layout_panel(layout, panel1, 0, 1);
    layout_label(layout, label2, 0, 2);
    layout_panel(layout, panel2, 0, 3);
    layout_vmargin(layout, 1, 5);
    layout_vmargin(layout, 2, 5);
    layout_vexpand(layout, 3);
    layout_vmargin(layout, 1, 5.f);
    app->inspect = panel1;
    app->propedit = panel2;
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnDraw(Designer *app, Event *e)
{
    const EvDraw *p = event_params(e, EvDraw);
    cassert_no_null(app);
    draw_clear(p->ctx, kCOLOR_YELLOW);
    if (app->sel_form != UINT32_MAX)
    {
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        dform_draw(form, app->swidget, app->add_icon, p->ctx);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnMove(Designer *app, Event *e)
{
    const EvMouse *p = event_params(e, EvMouse);
    cassert_no_null(app);
    if (app->sel_form != UINT32_MAX)
    {
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        if (dform_OnMove(form, p->x, p->y) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnExit(Designer *app, Event *e)
{
    cassert_no_null(app);
    unref(e);
    if (app->sel_form != UINT32_MAX)
    {
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        if (dform_OnExit(form) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnClick(Designer *app, Event *e)
{
    cassert_no_null(app);
    if (app->sel_form != UINT32_MAX)
    {
        const EvMouse *p = event_params(e, EvMouse);
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        if (dform_OnClick(form, app->window, app->inspect, app->propedit, app->swidget, p->x, p->y, p->button) == TRUE)
            view_update(app->canvas);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnSize(Designer *app, Event *e)
{
    unref(app);
    unref(e);
}

/*---------------------------------------------------------------------------*/

static Layout *i_canvas_layout(Designer *app)
{
    Layout *layout = layout_create(1, 1);
    View *view = view_scroll();
    view_size(view, s2df(450, 200));
    view_OnDraw(view, listener(app, i_OnDraw, Designer));
    view_OnMove(view, listener(app, i_OnMove, Designer));
    view_OnExit(view, listener(app, i_OnExit, Designer));
    view_OnClick(view, listener(app, i_OnClick, Designer));
    view_OnSize(view, listener(app, i_OnSize, Designer));
    layout_view(layout, view, 0, 0);
    app->canvas = view;
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_middle_layout(Designer *app)
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
    return layout1;
}

/*---------------------------------------------------------------------------*/

static Layout *i_statusbar_layout(Designer *app)
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

static Layout *i_main_layout(Designer *app, ResPack *pack)
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

static Panel *i_panel(Designer *app, ResPack *pack)
{
    Panel *panel = panel_create();
    Layout *layout = i_main_layout(app, pack);
    panel_layout(panel, layout);
    return panel;
}

/*---------------------------------------------------------------------------*/

static void i_OnHotKey(Designer *app, Event *e)
{
    const EvKey *p = event_params(e, EvKey);
    cassert_no_null(app);
    if (p->key == ekKEY_SUPR)
    {
        if (app->sel_form != UINT32_MAX)
        {
            DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
            if (dform_OnSupr(form, app->inspect, app->propedit) == TRUE)
                view_update(app->canvas);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_save_config(const Designer *app)
{
    String *cfile = hfile_appdata("config.bin");
    Stream *stm = stm_to_file(tc(cfile), NULL);
    cassert_no_null(app);
    if (stm != NULL)
    {
        stm_write_enum(stm, app->swidget, widget_t);
        str_write(stm, app->folder_path);
        stm_write_u32(stm, app->sel_form);
        stm_close(&stm);
    }

    str_destroy(&cfile);
}

/*---------------------------------------------------------------------------*/

static void i_load_config(Designer *app)
{
    String *cfile = hfile_appdata("config.bin");
    Stream *stm = stm_from_file(tc(cfile), NULL);
    bool_t ok = FALSE;
    cassert_no_null(app);
    cassert(app->folder_path == NULL);
    if (stm != NULL)
    {
        app->swidget = stm_read_enum(stm, widget_t);
        app->folder_path = str_read(stm);
        app->sel_form = stm_read_u32(stm);
        ok = stm_state(stm) == ekSTOK;
        stm_close(&stm);
    }

    if (ok == FALSE)
    {
        app->swidget = ekWIDGET_SELECT;
        str_upd(&app->folder_path, "");
        app->sel_form = UINT32_MAX;
    }

    str_destroy(&cfile);
}

/*---------------------------------------------------------------------------*/

static void i_OnClose(Designer *app, Event *e)
{
    i_save_config(app);
    osapp_finish();
    unref(e);
}

/*---------------------------------------------------------------------------*/

static Designer *i_app(ResPack *pack)
{
    Designer *app = heap_new0(Designer);
    nform_start();
    i_dbind();
    dialog_dbind();
    i_load_config(app);
    app->forms = arrpt_create(DForm);
    app->add_icon = image_copy(image_from_resource(pack, PLUS16_PNG));
    return app;
}

/*---------------------------------------------------------------------------*/

static Designer *i_create(void)
{
    ResPack *pack = res_designer_respack("");
    Designer *app = i_app(pack);
    Panel *panel = i_panel(app, pack);
    app->window = window_create(ekWINDOW_STDRES);
    window_panel(app->window, panel);
    window_title(app->window, "GTNAP Designer");
    window_origin(app->window, v2df(500, 200));
    window_OnClose(app->window, listener(app, i_OnClose, Designer));
    window_hotkey(app->window, ekKEY_SUPR, 0, listener(app, i_OnHotKey, Designer));
    window_show(app->window);
    layout_dbind(app->widgets_layout, NULL, Designer);
    layout_dbind_obj(app->widgets_layout, app, Designer);
    respack_destroy(&pack);
    i_init_forms(app, tc(app->folder_path));
    return app;
}

/*---------------------------------------------------------------------------*/

static void i_destroy(Designer **app)
{
    cassert_no_null(app);
    cassert_no_null(*app);
    str_destroy(&(*app)->folder_path);
    image_destroy(&(*app)->add_icon);
    arrpt_destroy(&(*app)->forms, i_destroy_form_opt, DForm);
    window_destroy(&(*app)->window);
    nform_finish();
    heap_delete(app, Designer);
}

/*---------------------------------------------------------------------------*/

static void i_update(Designer *app, const real64_t prtime, const real64_t ctime)
{
    unref(app);
    unref(prtime);
    unref(ctime);
}

/*---------------------------------------------------------------------------*/

void designer_need_save(Designer *app)
{
    i_update_form_controls(app, TRUE);
}

/*---------------------------------------------------------------------------*/

void designer_canvas_update(Designer *app)
{
    cassert_no_null(app);
    view_update(app->canvas);
}

/*---------------------------------------------------------------------------*/

void designer_inspect_update(Designer *app)
{
    cassert_no_null(app);
    inspect_update(app->inspect);
}

/*---------------------------------------------------------------------------*/

void designer_inspect_select(Designer *app, const uint32_t row)
{
    cassert_no_null(app);
    if (app->sel_form != UINT32_MAX)
    {
        DForm *form = arrpt_get(app->forms, app->sel_form, DForm);
        dform_inspect_select(form, app->propedit, row);
    }
}

/*---------------------------------------------------------------------------*/

#include "osmain.h"
osmain_sync(0.1, i_create, i_destroy, i_update, "", Designer)
