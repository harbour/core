/* Dialog boxes */

#include "dialogs.h"
#include "dlabel.h"
#include "dlayout.h"
#include <gui/button.h>
#include <gui/cell.h>
#include <gui/edit.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/panel.h>
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
    Edit *edit;
    Button *defbutton;
    Window *window;
};

#define BUTTON_OK   1000
#define BUTTON_CANCEL   1001

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

static Layout *i_ok_cancel(DialogData *data)
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
    data->defbutton = button1;
    return layout;
}

/*---------------------------------------------------------------------------*/

DLabel *dialog_new_label(Window *parent, const DSelect *sel)
{
    DialogData data;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel(&data);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    DLabel *dlabel = NULL;
    uint32_t ret = 0;
    data.edit = edit;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->layout);
    caption = str_printf("New Label widget in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->layout->name));
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
    window_panel(window, panel);
    window_defbutton(window, data.defbutton);
    i_center_window(parent, window);
    ret = window_modal(window, parent);

    if (ret == BUTTON_OK)
    {
        const char_t *text = edit_get_text(data.edit);
        dlabel = dlabel_create();
        dlabel_text(dlabel, text);
    }

    window_destroy(&window);
    str_destroy(&caption);    
    return dlabel;
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

DLayout *dialog_new_layout(Window *parent, const DSelect *sel)
{
    DialogData data;
    DialogLayout diag;
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_grid_layout();
    Layout *layout3 = i_ok_cancel(&data);
    Label *label = label_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD | ekWINDOW_ESC);
    String *caption = NULL;
    DLayout *dlayout = NULL;
    uint32_t ret = 0;
    data.window = window;
    cassert_no_null(sel);
    cassert_no_null(sel->layout);
    caption = str_printf("New Grid Layout in (%d, %d) of '%s'", sel->col, sel->row, tc(sel->layout->name));
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
        uint32_t i, j;
        cassert(diag.ncols > 0);
        cassert(diag.nrows > 0);
        dlayout = dlayout_create(diag.ncols, diag.nrows);
        /* Make it editable from dialog */
        dlayout_margin_top(dlayout, 5);
        dlayout_margin_bottom(dlayout, 5);
        dlayout_margin_left(dlayout, 5);
        dlayout_margin_right(dlayout, 5);
        for (i = 0; i < diag.ncols - 1; ++i)
            dlayout_margin_col(dlayout, i, 5);
        for (j = 0; j < diag.nrows - 1; ++j)
            dlayout_margin_row(dlayout, j, 5);
    }

    window_destroy(&window);
    str_destroy(&caption);    
    return dlayout;
}
