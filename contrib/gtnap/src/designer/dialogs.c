/* Dialog boxes */

#include "dialogs.h"
#include <gui/button.h>
#include <gui/edit.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/panel.h>
#include <gui/window.h>

/*---------------------------------------------------------------------------*/

static Layout *i_ok_cancel(void)
{
    Layout *layout = layout_create(2, 1);
    Button *button1 = button_push();
    Button *button2 = button_push();
    button_text(button1, "Ok");
    button_text(button2, "Cancel");
    layout_button(layout, button1, 0, 0);
    layout_button(layout, button2, 1, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

bool_t dialog_new_label(Window *parent, const V2Df origin, String **text)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Layout *layout3 = i_ok_cancel();
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    Panel *panel = panel_create();
    Window *window = window_create(ekWINDOW_STD);
    label_text(label1, "Create a new Label widget");
    label_text(label2, "Text:");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    panel_layout(panel, layout1);
    window_panel(window, panel);
    window_origin(window, origin);
    window_modal(window, parent);
    window_destroy(&window);
    return TRUE;
}
