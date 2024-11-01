/* Property editor */

#include "propedit.h"
#include "designer.h"
#include "dlayout.h"
#include "dlabel.h"
#include "dform.h"
#include <gui/gui.h>
#include <gui/cell.h>
#include <gui/edit.h>
#include <gui/label.h>
//#include <gui/labelh.h>
#include <gui/layout.h>
//#include <gui/layouth.h>
//#include <gui/cell.h>
#include <gui/panel.h>
#include <gui/view.h>
//#include <gui/drawctrl.inl>
//#include <geom2d/r2d.h>
//#include <geom2d/v2d.h>
//#include <draw2d/color.h>
//#include <draw2d/draw.h>
//#include <draw2d/drawg.h>
//#include <draw2d/image.h>
#include <core/event.h>
#include <core/heap.h>
//#include <core/dbind.h>
#include <core/strings.h>
//#include <sewer/bmem.h>
#include <sewer/cassert.h>

typedef struct _propdata_t PropData;

struct _propdata_t
{
    DSelect sel;
    Designer *app;
    DForm *form;
    Layout *label_layout;
};

/*---------------------------------------------------------------------------*/

static Layout *i_no_sel_layout(void)
{
    Layout *layout = layout_create(1, 1);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_empty_cell_layout(void)
{
    Layout *layout = layout_create(1, 1);
    Label *label = label_create();
    label_text(label, "Empty cell");
    layout_label(layout, label, 0, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnLabelNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);

    if (evbind_modify(e, DLabel, String*, name) == TRUE)
    {
        designer_inspect_update(data->app);
    }
    else if (evbind_modify(e, DLabel, String*, text) == TRUE)
    {
        dform_synchro_cell_text(data->form, &data->sel);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

//static void i_OnLabelName(PropData *data, Event *e)
//{
//    unref(data);
//    unref(e);
//}
//
///*---------------------------------------------------------------------------*/
//
//static void i_OnLabelText(PropData *data, Event *e)
//{
//    unref(data);
//    unref(e);
//}

/*---------------------------------------------------------------------------*/

static Layout *i_label_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 2);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    Edit *edit1 = edit_create();
    Edit *edit2 = edit_create();
    cassert_no_null(data);
    label_text(label1, "Name");
    label_text(label2, "Text");
    label_text(label3, "Label properties");
    //edit_OnFilter(edit1, listener(data, i_OnLabelName, PropData));
    //edit_OnFilter(edit2, listener(data, i_OnLabelText, PropData));
    layout_label(layout2, label1, 0, 0);
    layout_label(layout2, label2, 0, 1);
    layout_edit(layout2, edit1, 1, 0);
    layout_edit(layout2, edit2, 1, 1);
    layout_label(layout1, label3, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, 3);
    layout_hmargin(layout2, 0, 5);
    layout_hexpand(layout2, 1);
    layout_vexpand(layout1, 2);
    cell_dbind(layout_cell(layout2, 1, 0), DLabel, String*, name);
    cell_dbind(layout_cell(layout2, 1, 1), DLabel, String*, text);
    layout_dbind(layout1, listener(data, i_OnLabelNotify, PropData), DLabel);
    data->label_layout = layout1;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static PropData *i_data(Designer *app)
{
    PropData *data = heap_new0(PropData);
    data->app = app;
    return data;
}

/*---------------------------------------------------------------------------*/

static void i_destroy_data(PropData **data)
{
    heap_delete(data, PropData);
}

/*---------------------------------------------------------------------------*/

Panel *propedit_create(Designer *app)
{
    PropData *data = i_data(app);
    Layout *layout0 = i_no_sel_layout();
    Layout *layout1 = i_empty_cell_layout();
    Layout *layout2 = i_label_layout(data);
    Panel *panel = panel_create();
    panel_layout(panel, layout0);
    panel_layout(panel, layout1);
    panel_layout(panel, layout2);
    panel_data(panel, &data, i_destroy_data, PropData);
    panel_visible_layout(panel, 0);
    return panel;
}

/*---------------------------------------------------------------------------*/

void propedit_set(Panel *panel, DForm *form, const DSelect *sel)
{
    PropData *data = panel_get_data(panel, PropData);
    cassert_no_null(sel);
    data->form = form;
    data->sel = *sel;
    if (sel->layout == NULL)
    {
        panel_visible_layout(panel, 0);
    }
    else if (sel->elem == ekLAYELEM_CELL)
    {
        DCell *cell = dlayout_cell(sel);
        if (cell->type == ekCELL_TYPE_EMPTY)
        {
            panel_visible_layout(panel, 1);
        }
        else if (cell->type == ekCELL_TYPE_LABEL)
        {
            layout_dbind_obj(data->label_layout, cell->content.label, DLabel);
            panel_visible_layout(panel, 2);
        }
        else
        {
            panel_visible_layout(panel, 0);
        }
    }
    else
    {
        /* Still unimplemented */
        panel_visible_layout(panel, 0);
    }

    panel_update(panel);
}
