/* Dialog boxes */

#include "dialogs.h"
#include "dlabel.h"
#include <gui/button.h>
#include <gui/edit.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/panel.h>
#include <gui/window.h>
#include <core/event.h>
#include <core/strings.h>
#include <sewer/cassert.h>

typedef struct _dialogdata_t DialogData;

struct _dialogdata_t
{
    Edit *edit;
    Button *defbutton;
    Window *window;
};

#define BUTTON_OK   1000
#define BUTTON_CANCEL   1001

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
