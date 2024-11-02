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
#include <gui/popup.h>
#include <gui/view.h>
#include <gui/updown.h>
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
    Panel *cell_panel;
    Layout *layout_layout;
    Layout *cell_layout;
    Layout *label_layout;
};

/*---------------------------------------------------------------------------*/

static const real32_t i_GRID_HMARGIN = 5;
static const real32_t i_HEADER_VMARGIN = 3;

/*---------------------------------------------------------------------------*/

static Layout *i_no_sel_layout(void)
{
    Layout *layout = layout_create(1, 1);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_margin_layout(void)
{
    Layout *layout = layout_create(3, 4);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    Label *label4 = label_create();
    Edit *edit1 = edit_create();
    Edit *edit2 = edit_create();
    Edit *edit3 = edit_create();
    Edit *edit4 = edit_create();
    UpDown *updown1 = updown_create();
    UpDown *updown2 = updown_create();
    UpDown *updown3 = updown_create();
    UpDown *updown4 = updown_create();
    label_text(label1, "Top");
    label_text(label2, "Left");
    label_text(label3, "Bottom");
    label_text(label4, "Right");
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_label(layout, label3, 0, 2);
    layout_label(layout, label4, 0, 3);
    layout_edit(layout, edit1, 1, 0);
    layout_edit(layout, edit2, 1, 1);
    layout_edit(layout, edit3, 1, 2);
    layout_edit(layout, edit4, 1, 3);
    layout_updown(layout, updown1, 2, 0);
    layout_updown(layout, updown2, 2, 1);
    layout_updown(layout, updown3, 2, 2);
    layout_updown(layout, updown4, 2, 3);
    layout_hexpand(layout, 1);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    cell_dbind(layout_cell(layout, 1, 0), DLayout, real32_t, margin_left);
    cell_dbind(layout_cell(layout, 1, 1), DLayout, real32_t, margin_top);
    cell_dbind(layout_cell(layout, 1, 2), DLayout, real32_t, margin_right);
    cell_dbind(layout_cell(layout, 1, 3), DLayout, real32_t, margin_bottom);
    cell_dbind(layout_cell(layout, 2, 0), DLayout, real32_t, margin_left);
    cell_dbind(layout_cell(layout, 2, 1), DLayout, real32_t, margin_top);
    cell_dbind(layout_cell(layout, 2, 2), DLayout, real32_t, margin_right);
    cell_dbind(layout_cell(layout, 2, 3), DLayout, real32_t, margin_bottom);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnLayoutNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);

    //if (evbind_modify(e, DLabel, String*, name) == TRUE)
    //{
    //    designer_inspect_update(data->app);
    //}
    //else if (evbind_modify(e, DLabel, String*, text) == TRUE)
    //{
    //    dform_synchro_cell_text(data->form, &data->sel);
    //    dform_compose(data->form);
    //    designer_canvas_update(data->app);
    //}
}

/*---------------------------------------------------------------------------*/

static Layout *i_layout_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = i_margin_layout();
    Label *label = label_create();
    cassert_no_null(data);
    label_text(label, "Layout properties");
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_vexpand(layout1, 2);
    layout_dbind(layout1, listener(data, i_OnLayoutNotify, PropData), DLayout);
    data->layout_layout = layout1;
    return layout1;
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
    if (evbind_modify(e, DLabel, String*, text) == TRUE)
    {
        dform_synchro_cell_text(data->form, &data->sel);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_label_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    cassert_no_null(data);
    label_text(label1, "Label properties");
    label_text(label2, "Text");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_hmargin(layout2, 0, i_GRID_HMARGIN);
    layout_hexpand(layout2, 1);
    layout_vexpand(layout1, 2);
    cell_dbind(layout_cell(layout2, 1, 0), DLabel, String*, text);
    layout_dbind(layout1, listener(data, i_OnLabelNotify, PropData), DLabel);
    data->label_layout = layout1;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static void i_OnCellNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DCell, String*, name) == TRUE)
    {
        designer_inspect_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_cell_props_layout(void)
{
    Layout *layout = layout_create(2, 3);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    Edit *edit = edit_create();
    PopUp *popup1 = popup_create();
    PopUp *popup2 = popup_create();
    label_text(label1, "Name");
    label_text(label2, "HAlign");
    label_text(label3, "VAlign");
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_label(layout, label3, 0, 2);
    layout_edit(layout, edit, 1, 0);
    layout_popup(layout, popup1, 1, 1);
    layout_popup(layout, popup2, 1, 2);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    layout_hexpand(layout, 1);
    cell_dbind(layout_cell(layout, 1, 0), DCell, String*, name);
    cell_dbind(layout_cell(layout, 1, 1), DCell, halign_t, halign);
    cell_dbind(layout_cell(layout, 1, 2), DCell, valign_t, valign);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Panel *i_cell_content_panel(PropData *data)
{
    Layout *layout1 = i_empty_cell_layout();
    Layout *layout2 = i_label_layout(data);
    Panel *panel = panel_create();
    cassert_no_null(data);
    panel_layout(panel, layout1);
    panel_layout(panel, layout2);
    panel_visible_layout(panel, 0);
    data->cell_panel = panel;
    return panel;
}

/*---------------------------------------------------------------------------*/

static Layout *i_cell_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 4);
    Layout *layout2 = i_cell_props_layout();
    Label *label = label_create();
    Panel *panel = i_cell_content_panel(data);
    label_text(label, "Cell properties");
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_panel(layout1, panel, 0, 2);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_vmargin(layout1, 1, i_HEADER_VMARGIN);
    layout_vexpand(layout1, 3);
    layout_dbind(layout1, listener(data, i_OnCellNotify, PropData), DCell);
    data->cell_layout = layout1;
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
    Layout *layout1 = i_layout_layout(data);
    Layout *layout2 = i_cell_layout(data);
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
        /* i_no_sel_layout */
        panel_visible_layout(panel, 0);
    }
    else if (sel->elem != ekLAYELEM_CELL)
    {
        /* i_layout_layout */
        layout_dbind_obj(data->layout_layout, sel->layout, DLayout);
        panel_visible_layout(panel, 1);
    }
    else
    {
        /* i_cell_layout */
        DCell *cell = dlayout_cell_sel(sel);
        panel_visible_layout(panel, 2);
        layout_dbind_obj(data->cell_layout, cell, DCell);
        if (cell->type == ekCELL_TYPE_EMPTY)
        {
            panel_visible_layout(data->cell_panel, 0);
        }
        else if (cell->type == ekCELL_TYPE_LABEL)
        {
            layout_dbind_obj(data->label_layout, cell->content.label, DLabel);
            panel_visible_layout(data->cell_panel, 1);
        }
        else
        {
            cassert(FALSE);
            panel_visible_layout(data->cell_panel, 0);
        }

        panel_update(data->cell_panel);
    }

    panel_update(panel);
}
