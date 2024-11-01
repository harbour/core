/* Property editor */

#include "propedit.h"
#include "designer.h"
#include "dlayout.h"
#include "dlabel.h"
#include "dform.h"
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
    Edit *edit;
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

static void i_OnFilter(PropData *data, Event *e)
{
    const EvText *p = event_params(e, EvText);
    cassert_no_null(data);
    dform_update_cell_text(data->form, &data->sel, p->text);
    dform_compose(data->form);
    designer_canvas_update(data->app);
}

/*---------------------------------------------------------------------------*/

static Layout *i_label_layout(PropData *data)
{
    Layout *layout = layout_create(2, 1);
    Label *label = label_create();
    Edit *edit = edit_create();
    cassert_no_null(data);
    label_text(label, "Text");
    edit_OnFilter(edit, listener(data, i_OnFilter, PropData));
    layout_label(layout, label, 0, 0);
    layout_edit(layout, edit, 1, 0);
    layout_hmargin(layout, 0, 5);
    layout_hexpand(layout, 1);
    data->edit = edit;
    return layout;
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
            const char_t *text = tc(cell->content.label->text);
            edit_text(data->edit, text);
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
