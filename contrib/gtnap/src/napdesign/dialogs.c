/* Dialog boxes */

#include "dialogs.h"
#include <nform/flayout.h>
#include <gui/button.h>
#include <gui/cell.h>
#include <gui/edit.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/panel.h>
#include <gui/popup.h>
#include <gui/updown.h>
#include <gui/window.h>
#include <core/dbind.h>
#include <core/event.h>
#include <core/strings.h>
#include <sewer/cassert.h>

typedef struct _dialoglayout_t DialogLayout;
typedef struct _dialogdata_t DialogData;

struct _dialoglayout_t
{
    uint32_t ncols;
    uint32_t nrows;
};

struct _dialogdata_t
{
    Button *defbutton;
    Window *window;
    Edit *edit;
};

#define BUTTON_OK 1000
#define BUTTON_CANCEL 1001
#define BUTTON_SAVE 1002
#define BUTTON_NO_SAVE 1003

/*---------------------------------------------------------------------------*/

void dialog_dbind(void)
{
    dbind(DialogLayout, uint32_t, ncols);
    dbind(DialogLayout, uint32_t, nrows);
    dbind_range(DialogLayout, uint32_t, ncols, 1, 20);
    dbind_range(DialogLayout, uint32_t, nrows, 1, 20);
}

/*---------------------------------------------------------------------------*/

static void i_center_window(const Window *parent, Window *window)
{
    V2Df p1 = window_get_origin(parent);
    S2Df s1 = window_get_size(parent);
    S2Df s2 = window_get_size(window);
    V2Df p2;
    p2.x = p1.x + (s1.width - s2.width) / 2;
    p2.y = p1.y + (s1.height - s2.height) / 2;
    window_origin(window, p2);
}

/*---------------------------------------------------------------------------*/

static void i_OnClick(DialogData *data, Event *e)
{
    Button *button = event_sender(e, Button);
    uint32_t tag = button_get_tag(button);
    cassert_no_null(data);
    window_stop_modal(data->window, tag);
}

/*---------------------------------------------------------------------------*/

static Layout *i_ok_cancel(DialogData *data, const bool_t ok_default)
{
    Layout *layout = layout_create(2, 1);
    Button *button1 = button_push();
    Button *button2 = button_push();
    cassert_no_null(data);
    button_text(button1, "Ok");
    button_text(button2, "Cancel");
    button_tag(button1, BUTTON_OK);
    button_tag(button2, BUTTON_CANCEL);
    button_OnClick(button1, listener(data, i_OnClick, DialogData));
    button_OnClick(button2, listener(data, i_OnClick, DialogData));
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    if (ok_default == TRUE)
        data->defbutton = button1;
    else
        data->defbutton = button2;
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_ok(DialogData *data)
{
    Layout *layout = layout_create(1, 1);
    Button *button1 = button_push();
    cassert_no_null(data);
    button_text(button1, "Ok");
    button_tag(button1, BUTTON_OK);
    button_OnClick(button1, listener(data, i_OnClick, DialogData));
    layout_button(layout, button1, 0, 0);
    data->defbutton = button1;
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_save_buttons(DialogData *data)
{
    Layout *layout = layout_create(3, 1);
    Button *button1 = button_push();
    Button *button2 = button_push();
    Button *button3 = button_push();
    cassert_no_null(data);
    button_text(button1, "Save changes");
    button_text(button2, "NO save changes");
    button_text(button3, "Cancel");
    button_tag(button1, BUTTON_SAVE);
    button_tag(button2, BUTTON_NO_SAVE);
    button_tag(button3, BUTTON_CANCEL);
    button_OnClick(button1, listener(data, i_OnClick, DialogData));
    button_OnClick(button2, listener(data, i_OnClick, DialogData));
    button_OnClick(button3, listener(data, i_OnClick, DialogData));
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    layout_button(layout, button3, 2, 0);
    data->defbutton = button1;
    return layout;
}

/*---------------------------------------------------------------------------*/

String *dialog_form_name(Window *parent, const char_t *name)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    uint32_t ret = 0;
    String *fname = NULL;
    data.window = window;
    data.edit = edit;
    if (str_empty_c(name) == TRUE)
    {
        caption = str_c("Set name for new form");
    }
    else
    {
        caption = str_c("Change the name of the form");
        edit_text(edit, name);
    }
    label_text(label1, tc(caption));
    label_text(label2, "Name:");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret == BUTTON_OK)
        fname = str_c(edit_get_text(data.edit));
    else
        fname = str_c("");

    window_destroy(&window);
    str_destroy(&caption);
    return fname;
}

/*---------------------------------------------------------------------------*/

FLabel *dialog_new_label(Window *parent, const DSelect *sel)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    FLabel *flabel = dbind_create(FLabel);
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->flayout);
    caption = str_printf("New Label widget in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->flayout->name));
    label_text(label1, tc(caption));
    label_text(label2, "Text:");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    cell_dbind(layout_cell(layout2, 1, 0), FLabel, String*, text);
    layout_dbind(layout1, NULL, FLabel);
    layout_dbind_obj(layout1, flabel, FLabel);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret != BUTTON_OK)
        dbind_destroy(&flabel, FLabel);

    window_destroy(&window);
    str_destroy(&caption);
    return flabel;
}

/*---------------------------------------------------------------------------*/

FButton *dialog_new_button(Window *parent, const DSelect *sel)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    FButton *fbutton = dbind_create(FButton);
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->flayout);
    caption = str_printf("New Button widget in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->flayout->name));
    label_text(label1, tc(caption));
    label_text(label2, "Text:");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    cell_dbind(layout_cell(layout2, 1, 0), FButton, String*, text);
    layout_dbind(layout1, NULL, FButton);
    layout_dbind_obj(layout1, fbutton, FButton);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret != BUTTON_OK)
        dbind_destroy(&fbutton, FButton);

    window_destroy(&window);
    str_destroy(&caption);
    return fbutton;
}

/*---------------------------------------------------------------------------*/

FCheck *dialog_new_check(Window *parent, const DSelect *sel)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    FCheck *fcheck = dbind_create(FCheck);
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->flayout);
    caption = str_printf("New Checkbox widget in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->flayout->name));
    label_text(label1, tc(caption));
    label_text(label2, "Text:");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    cell_dbind(layout_cell(layout2, 1, 0), FCheck, String*, text);
    layout_dbind(layout1, NULL, FCheck);
    layout_dbind_obj(layout1, fcheck, FCheck);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret != BUTTON_OK)
        dbind_destroy(&fcheck, FCheck);

    window_destroy(&window);
    str_destroy(&caption);
    return fcheck;
}

/*---------------------------------------------------------------------------*/

FEdit *dialog_new_edit(Window *parent, const DSelect *sel)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 5);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Button *button1 = button_check();
    Button *button2 = button_check();
    PopUp *popup = popup_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    FEdit *fedit = dbind_create(FEdit);
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->flayout);
    caption = str_printf("New Editbox widget in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->flayout->name));
    label_text(label1, tc(caption));
    label_text(label2, "Text align");
    button_text(button1, "Passmode");
    button_text(button2, "Autosel");
    layout_label(layout1, label1, 0, 0);
    layout_button(layout1, button1, 0, 1);
    layout_button(layout1, button2, 0, 2);
    layout_label(layout2, label2, 0, 0);
    layout_popup(layout2, popup, 1, 0);
    layout_layout(layout1, layout2, 0, 3);
    layout_layout(layout1, layout3, 0, 4);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    cell_dbind(layout_cell(layout1, 0, 1), FEdit, bool_t, passmode);
    cell_dbind(layout_cell(layout1, 0, 2), FEdit, bool_t, autosel);
    cell_dbind(layout_cell(layout2, 1, 0), FEdit, halign_t, text_align);
    layout_dbind(layout1, NULL, FEdit);
    layout_dbind_obj(layout1, fedit, FEdit);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret != BUTTON_OK)
        dbind_destroy(&fedit, FEdit);

    window_destroy(&window);
    str_destroy(&caption);
    return fedit;
}

/*---------------------------------------------------------------------------*/

static Layout *i_grid_layout(void)
{
    Layout *layout = layout_create(3, 2);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit1 = edit_create();
    Edit *edit2 = edit_create();
    UpDown *updown1 = updown_create();
    UpDown *updown2 = updown_create();
    label_text(label1, "Columns");
    label_text(label2, "Rows");
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_edit(layout, edit1, 1, 0);
    layout_edit(layout, edit2, 1, 1);
    layout_updown(layout, updown1, 2, 0);
    layout_updown(layout, updown2, 2, 1);
    cell_dbind(layout_cell(layout, 1, 0), DialogLayout, uint32_t, ncols);
    cell_dbind(layout_cell(layout, 2, 0), DialogLayout, uint32_t, ncols);
    cell_dbind(layout_cell(layout, 1, 1), DialogLayout, uint32_t, nrows);
    cell_dbind(layout_cell(layout, 2, 1), DialogLayout, uint32_t, nrows);
    layout_dbind(layout, NULL, DialogLayout);
    return layout;
}

/*---------------------------------------------------------------------------*/

FLayout *dialog_new_layout(Window *parent, const DSelect *sel)
{
    DialogData data;
    DialogLayout diag;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_grid_layout();
    Layout *layout3 = i_ok_cancel(&data, TRUE);
    Label *label = label_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    FLayout *flayout = NULL;
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->flayout);
    caption = str_printf("New Grid Layout in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->flayout->name));
    label_text(label, tc(caption));
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_vmargin(layout1, 0, 5);
    layout_vmargin(layout1, 1, 5);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    diag.ncols = 3;
    diag.nrows = 2;
    layout_dbind_obj(layout2, &diag, DialogLayout);
    ret = window_modal(window, parent);

    if (ret == BUTTON_OK)
    {
        cassert(diag.ncols > 0);
        cassert(diag.nrows > 0);
        flayout = flayout_create(diag.ncols, diag.nrows);
    }

    window_destroy(&window);
    str_destroy(&caption);
    return flayout;
}

/*---------------------------------------------------------------------------*/

uint8_t dialog_unsaved_changes(Window *parent)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_save_buttons(&data);
    Label *label = label_multiline();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    uint32_t ret = 0;
    data.window = window;
    label_text(label, "There are unsaved changes in your current forms.\nChange will be lost if you open another project folder.\nDo you want to save your changes?");
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, 5);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);
    window_destroy(&window);

    /* Save changes */
    if (ret == BUTTON_SAVE)
        return 1;
    /* Don't save changes */
    else if (ret == BUTTON_NO_SAVE)
        return 0;
    /* Cancel action */
    else
        return 2;
}

/*---------------------------------------------------------------------------*/

bool_t dialog_remove_form(Window *parent, const char_t *name)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_ok_cancel(&data, FALSE);
    Label *label = label_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    uint32_t ret = 0;
    data.window = window;
    caption = str_printf("Do you really want to delete the '%s' form?", name);
    label_text(label, tc(caption));
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, 5);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);
    window_destroy(&window);
    str_destroy(&caption);

    if (ret == BUTTON_OK)
        return TRUE;

    return FALSE;
}

/*---------------------------------------------------------------------------*/

void dialog_name_already_exists(Window *parent, const char_t *name)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_ok(&data);
    Label *label = label_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    data.window = window;
    caption = str_printf("Form name '%s' already exists", name);
    label_text(label, tc(caption));
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, 5);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    window_modal(window, parent);
    window_destroy(&window);
    str_destroy(&caption);
}
