/* Property editor */

#include "propedit.h"
#include "designer.h"
#include "dlayout.h"
#include "dlabel.h"
#include "dform.h"
#include <gui/gui.h>
#include <gui/cell.h>
#include <gui/button.h>
#include <gui/edit.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/panel.h>
#include <gui/popup.h>
#include <gui/view.h>
#include <gui/updown.h>
#include <core/event.h>
#include <core/heap.h>
#include <core/strings.h>
#include <sewer/bstd.h>
#include <sewer/cassert.h>

typedef struct _propdata_t PropData;

struct _propdata_t
{
    DSelect sel;
    Designer *app;
    DForm *form;
    Panel *cell_panel;
    Layout *layout_layout;
    Layout *column_layout;
    Layout *row_layout;
    Layout *cell_layout;
    Layout *label_layout;
    Layout *button_layout;
    Layout *check_layout;
    Layout *edit_layout;
    Cell *column_margin_cell;
    Cell *row_margin_cell;
    Label *layout_geom_label;
    Label *cell_geom_label;
    PopUp *column_popup;
    PopUp *row_popup;
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

static Layout *i_value_updown_layout(void)
{
    Layout *layout = layout_create(2, 1);
    Edit *edit = edit_create();
    UpDown *updown = updown_create();
    layout_edit(layout, edit, 0, 0);
    layout_updown(layout, updown, 1, 0);
    layout_hexpand(layout, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_margin_layout(PropData *data)
{
    Layout *layout = layout_create(2, 6);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    Label *label4 = label_create();
    Label *label5 = label_create();
    Label *label6 = label_create();
    Label *label7 = label_create();
    Edit *edit = edit_create();
    Layout *val1 = i_value_updown_layout();
    Layout *val2 = i_value_updown_layout();
    Layout *val3 = i_value_updown_layout();
    Layout *val4 = i_value_updown_layout();
    cassert_no_null(data);
    label_text(label1, "Geom");
    label_text(label2, "Name");
    label_text(label3, "Top");
    label_text(label4, "Left");
    label_text(label5, "Bottom");
    label_text(label6, "Right");
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_label(layout, label3, 0, 2);
    layout_label(layout, label4, 0, 3);
    layout_label(layout, label5, 0, 4);
    layout_label(layout, label6, 0, 5);
    layout_label(layout, label7, 1, 0);
    layout_edit(layout, edit, 1, 1);
    layout_layout(layout, val1, 1, 2);
    layout_layout(layout, val2, 1, 3);
    layout_layout(layout, val3, 1, 4);
    layout_layout(layout, val4, 1, 5);
    layout_halign(layout, 1, 0, ekJUSTIFY);
    layout_hexpand(layout, 1);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    data->layout_geom_label = label7;
    cell_dbind(layout_cell(layout, 1, 1), DLayout, String *, name);
    cell_dbind(layout_cell(layout, 1, 2), DLayout, real32_t, margin_top);
    cell_dbind(layout_cell(layout, 1, 3), DLayout, real32_t, margin_left);
    cell_dbind(layout_cell(layout, 1, 4), DLayout, real32_t, margin_bottom);
    cell_dbind(layout_cell(layout, 1, 5), DLayout, real32_t, margin_right);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_set_column_obj(PropData *data, const uint32_t col)
{
    DColumn *column = NULL;
    uint32_t ncols = 0;
    cassert_no_null(data);
    column = dlayout_column(data->sel.layout, col);
    ncols = dlayout_ncols(data->sel.layout);
    layout_dbind_obj(data->column_layout, column, DColumn);
    cell_enabled(data->column_margin_cell, col < ncols - 1);
}

/*---------------------------------------------------------------------------*/

static void i_set_row_obj(PropData *data, const uint32_t row)
{
    DRow *drow = NULL;
    uint32_t nrows = 0;
    cassert_no_null(data);
    drow = dlayout_row(data->sel.layout, row);
    nrows = dlayout_nrows(data->sel.layout);
    layout_dbind_obj(data->row_layout, drow, DRow);
    cell_enabled(data->row_margin_cell, row < nrows - 1);
}

/*---------------------------------------------------------------------------*/

static void i_OnColumnSelect(PropData *data, Event *e)
{
    const EvButton *p = event_params(e, EvButton);
    cassert_no_null(data);
    i_set_column_obj(data, p->index);
}

/*---------------------------------------------------------------------------*/

static void i_OnRowSelect(PropData *data, Event *e)
{
    const EvButton *p = event_params(e, EvButton);
    cassert_no_null(data);
    i_set_row_obj(data, p->index);
}

/*---------------------------------------------------------------------------*/

static Layout *i_column_layout(PropData *data)
{
    Layout *layout = layout_create(2, 3);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    PopUp *popup = popup_create();
    Layout *val1 = i_value_updown_layout();
    Layout *val2 = i_value_updown_layout();
    cassert_no_null(data);
    label_text(label1, "Column");
    label_text(label2, "CRight");
    label_text(label3, "FWidth");
    popup_OnSelect(popup, listener(data, i_OnColumnSelect, PropData));
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_label(layout, label3, 0, 2);
    layout_popup(layout, popup, 1, 0);
    layout_layout(layout, val1, 1, 1);
    layout_layout(layout, val2, 1, 2);
    layout_hexpand(layout, 1);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    data->column_popup = popup;
    data->column_margin_cell = layout_cell(layout, 1, 1);
    cell_dbind(layout_cell(layout, 1, 1), DColumn, real32_t, margin_right);
    cell_dbind(layout_cell(layout, 1, 2), DColumn, real32_t, forced_width);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Layout *i_row_layout(PropData *data)
{
    Layout *layout = layout_create(2, 2);
    Label *label1 = label_create();
    Label *label2 = label_create();
    PopUp *popup = popup_create();
    Layout *val1 = i_value_updown_layout();
    cassert_no_null(data);
    label_text(label1, "Row");
    label_text(label2, "RBottom");
    popup_OnSelect(popup, listener(data, i_OnRowSelect, PropData));
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_popup(layout, popup, 1, 0);
    layout_layout(layout, val1, 1, 1);
    layout_hexpand(layout, 1);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    data->row_popup = popup;
    data->row_margin_cell = layout_cell(layout, 1, 1);
    cell_dbind(layout_cell(layout, 1, 1), DRow, real32_t, margin_bottom);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnLayoutNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DLayout, String *, name) == TRUE)
    {
        designer_inspect_update(data->app);
    }
    else if (evbind_modify(e, DLayout, real32_t, margin_left) == TRUE || evbind_modify(e, DLayout, real32_t, margin_top) == TRUE || evbind_modify(e, DLayout, real32_t, margin_right) == TRUE || evbind_modify(e, DLayout, real32_t, margin_bottom) == TRUE)
    {
        DLayout *dlayout = evbind_object(e, DLayout);
        cassert(dlayout == data->sel.layout);
        dform_synchro_layout_margin(data->form, dlayout);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnColumnNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DColumn, real32_t, margin_right) == TRUE)
    {
        DColumn *dcolumn = evbind_object(e, DColumn);
        uint32_t col = popup_get_selected(data->column_popup);
        dform_synchro_column_margin(data->form, data->sel.layout, dcolumn, col);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
    else if (evbind_modify(e, DColumn, real32_t, forced_width) == TRUE)
    {
        DColumn *dcolumn = evbind_object(e, DColumn);
        uint32_t col = popup_get_selected(data->column_popup);
        dform_synchro_column_width(data->form, data->sel.layout, dcolumn, col);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnRowNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DRow, real32_t, margin_bottom) == TRUE)
    {
        DRow *drow = evbind_object(e, DRow);
        uint32_t row = popup_get_selected(data->row_popup);
        dform_synchro_row_margin(data->form, data->sel.layout, drow, row);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_layout_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 5);
    Layout *layout2 = i_margin_layout(data);
    Layout *layout3 = i_column_layout(data);
    Layout *layout4 = i_row_layout(data);
    Label *label = label_create();
    cassert_no_null(data);
    label_text(label, "Layout properties");
    layout_label(layout1, label, 0, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_layout(layout1, layout3, 0, 2);
    layout_layout(layout1, layout4, 0, 3);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_vexpand(layout1, 4);
    layout_dbind(layout2, listener(data, i_OnLayoutNotify, PropData), DLayout);
    layout_dbind(layout3, listener(data, i_OnColumnNotify, PropData), DColumn);
    layout_dbind(layout4, listener(data, i_OnRowNotify, PropData), DRow);
    data->layout_layout = layout2;
    data->column_layout = layout3;
    data->row_layout = layout4;
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

static Layout *i_layout_cell_layout(void)
{
    Layout *layout = layout_create(1, 1);
    Label *label = label_create();
    label_text(label, "Layout cell");
    layout_label(layout, label, 0, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

static void i_OnLabelNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DLabel, String *, text) == TRUE)
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
    cell_dbind(layout_cell(layout2, 1, 0), DLabel, String *, text);
    layout_dbind(layout1, listener(data, i_OnLabelNotify, PropData), DLabel);
    data->label_layout = layout1;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static void i_OnButtonNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DButton, String *, text) == TRUE)
    {
        dform_synchro_cell_text(data->form, &data->sel);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_button_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    cassert_no_null(data);
    label_text(label1, "Button properties");
    label_text(label2, "Text");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_hmargin(layout2, 0, i_GRID_HMARGIN);
    layout_hexpand(layout2, 1);
    layout_vexpand(layout1, 2);
    cell_dbind(layout_cell(layout2, 1, 0), DButton, String *, text);
    layout_dbind(layout1, listener(data, i_OnButtonNotify, PropData), DButton);
    data->button_layout = layout1;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static void i_OnCheckNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DCheck, String *, text) == TRUE)
    {
        dform_synchro_cell_text(data->form, &data->sel);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_check_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 3);
    Layout *layout2 = layout_create(2, 1);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Edit *edit = edit_create();
    cassert_no_null(data);
    label_text(label1, "CheckBox properties");
    label_text(label2, "Text");
    layout_label(layout1, label1, 0, 0);
    layout_label(layout2, label2, 0, 0);
    layout_edit(layout2, edit, 1, 0);
    layout_layout(layout1, layout2, 0, 1);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_hmargin(layout2, 0, i_GRID_HMARGIN);
    layout_hexpand(layout2, 1);
    layout_vexpand(layout1, 2);
    cell_dbind(layout_cell(layout2, 1, 0), DCheck, String *, text);
    layout_dbind(layout1, listener(data, i_OnCheckNotify, PropData), DCheck);
    data->check_layout = layout1;
    return layout1;
}

/*---------------------------------------------------------------------------*/

static void i_OnEditNotify(PropData *data, Event *e)
{
    cassert_no_null(data);
    cassert(event_type(e) == ekGUI_EVENT_OBJCHANGE);
    if (evbind_modify(e, DEdit, bool_t, passmode) == TRUE
    || evbind_modify(e, DEdit, bool_t, autosel) == TRUE
    || evbind_modify(e, DEdit, halign_t, text_align) == TRUE)
    {
        dform_synchro_edit(data->form, &data->sel);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_edit_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 5);
    Layout *layout2 = layout_create(2, 1);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Button *button1 = button_check();
    Button *button2 = button_check();
    PopUp *popup = popup_create();
    cassert_no_null(data);
    label_text(label1, "EditBox properties");
    label_text(label2, "Text align");
    button_text(button1, "Passmode");
    button_text(button2, "Autosel");
    layout_label(layout1, label1, 0, 0);
    layout_button(layout1, button1, 0, 1);
    layout_button(layout1, button2, 0, 2);
    layout_label(layout2, label2, 0, 0);
    layout_popup(layout2, popup, 1, 0);
    layout_layout(layout1, layout2, 0, 3);
    layout_vmargin(layout1, 0, i_HEADER_VMARGIN);
    layout_hmargin(layout2, 0, i_GRID_HMARGIN);
    layout_hexpand(layout2, 1);
    layout_vexpand(layout1, 4);
    cell_dbind(layout_cell(layout1, 0, 1), DEdit, bool_t, passmode);
    cell_dbind(layout_cell(layout1, 0, 2), DEdit, bool_t, autosel);
    cell_dbind(layout_cell(layout2, 1, 0), DEdit, halign_t, text_align);
    layout_dbind(layout1, listener(data, i_OnEditNotify, PropData), DEdit);
    data->edit_layout = layout1;
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
    else if (evbind_modify(e, DCell, halign_t, halign) == TRUE)
    {
        DCell *dcell = evbind_object(e, DCell);
        dform_synchro_cell_halign(data->form, data->sel.layout, dcell, data->sel.col, data->sel.row);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
    else if (evbind_modify(e, DCell, valign_t, valign) == TRUE)
    {
        DCell *dcell = evbind_object(e, DCell);
        dform_synchro_cell_valign(data->form, data->sel.layout, dcell, data->sel.col, data->sel.row);
        dform_compose(data->form);
        designer_canvas_update(data->app);
    }
}

/*---------------------------------------------------------------------------*/

static Layout *i_cell_props_layout(PropData *data)
{
    Layout *layout = layout_create(2, 4);
    Label *label1 = label_create();
    Label *label2 = label_create();
    Label *label3 = label_create();
    Label *label4 = label_create();
    Label *label5 = label_create();
    Edit *edit = edit_create();
    PopUp *popup1 = popup_create();
    PopUp *popup2 = popup_create();
    cassert_no_null(data);
    label_text(label1, "Coord");
    label_text(label2, "Name");
    label_text(label3, "HAlign");
    label_text(label4, "VAlign");
    layout_label(layout, label1, 0, 0);
    layout_label(layout, label2, 0, 1);
    layout_label(layout, label3, 0, 2);
    layout_label(layout, label4, 0, 3);
    layout_label(layout, label5, 1, 0);
    layout_edit(layout, edit, 1, 1);
    layout_popup(layout, popup1, 1, 2);
    layout_popup(layout, popup2, 1, 3);
    layout_halign(layout, 1, 0, ekJUSTIFY);
    layout_hmargin(layout, 0, i_GRID_HMARGIN);
    layout_hexpand(layout, 1);
    data->cell_geom_label = label5;
    cell_dbind(layout_cell(layout, 1, 1), DCell, String *, name);
    cell_dbind(layout_cell(layout, 1, 2), DCell, halign_t, halign);
    cell_dbind(layout_cell(layout, 1, 3), DCell, valign_t, valign);
    return layout;
}

/*---------------------------------------------------------------------------*/

static Panel *i_cell_content_panel(PropData *data)
{
    Layout *layout1 = i_empty_cell_layout();
    Layout *layout2 = i_layout_cell_layout();
    Layout *layout3 = i_label_layout(data);
    Layout *layout4 = i_button_layout(data);
    Layout *layout5 = i_check_layout(data);
    Layout *layout6 = i_edit_layout(data);
    Panel *panel = panel_create();
    cassert_no_null(data);
    panel_layout(panel, layout1);
    panel_layout(panel, layout2);
    panel_layout(panel, layout3);
    panel_layout(panel, layout4);
    panel_layout(panel, layout5);
    panel_layout(panel, layout6);
    panel_visible_layout(panel, 0);
    data->cell_panel = panel;
    return panel;
}

/*---------------------------------------------------------------------------*/

static Layout *i_cell_layout(PropData *data)
{
    Layout *layout1 = layout_create(1, 4);
    Layout *layout2 = i_cell_props_layout(data);
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
    /* i_no_sel_layout */
    if (sel->layout == NULL)
    {
        panel_visible_layout(panel, 0);
    }
    /* i_layout_layout */
    else if (sel->elem != ekLAYELEM_CELL)
    {
        char_t text[64];
        uint32_t ncols = dlayout_ncols(sel->layout);
        uint32_t nrows = dlayout_nrows(sel->layout);
        bstd_sprintf(text, sizeof(text), "%d cols x %d rows", ncols, nrows);
        label_text(data->layout_geom_label, text);

        /* Column selector */
        {
            uint32_t i, col = 0;
            popup_clear(data->column_popup);

            for (i = 0; i < ncols; ++i)
            {
                bstd_sprintf(text, sizeof(text), "%d", i);
                popup_add_elem(data->column_popup, text, NULL);
            }

            if (sel->elem == ekLAYELEM_MARGIN_COLUMN)
                col = sel->col;

            popup_selected(data->column_popup, col);
            i_set_column_obj(data, col);
        }

        /* Row selector */
        {
            uint32_t j, row = 0;
            popup_clear(data->row_popup);

            for (j = 0; j < nrows; ++j)
            {
                bstd_sprintf(text, sizeof(text), "%d", j);
                popup_add_elem(data->row_popup, text, NULL);
            }

            if (sel->elem == ekLAYELEM_MARGIN_ROW)
                row = sel->row;

            popup_selected(data->row_popup, row);
            i_set_row_obj(data, row);
        }

        layout_dbind_obj(data->layout_layout, sel->layout, DLayout);
        panel_visible_layout(panel, 1);
    }
    /* i_cell_layout */
    else
    {
        char_t text[64];
        DCell *cell = dlayout_cell_sel(sel);
        bstd_sprintf(text, sizeof(text), "(%d,%d)", sel->col, sel->row);
        label_text(data->cell_geom_label, text);
        panel_visible_layout(panel, 2);
        layout_dbind_obj(data->cell_layout, cell, DCell);
        if (cell->type == ekCELL_TYPE_EMPTY)
        {
            panel_visible_layout(data->cell_panel, 0);
        }
        else if (cell->type == ekCELL_TYPE_LAYOUT)
        {
            panel_visible_layout(data->cell_panel, 1);
        }
        else if (cell->type == ekCELL_TYPE_LABEL)
        {
            layout_dbind_obj(data->label_layout, cell->content.label, DLabel);
            panel_visible_layout(data->cell_panel, 2);
        }
        else if (cell->type == ekCELL_TYPE_BUTTON)
        {
            layout_dbind_obj(data->button_layout, cell->content.button, DButton);
            panel_visible_layout(data->cell_panel, 3);
        }
        else if (cell->type == ekCELL_TYPE_CHECK)
        {
            layout_dbind_obj(data->check_layout, cell->content.check, DCheck);
            panel_visible_layout(data->cell_panel, 4);
        }
        else if (cell->type == ekCELL_TYPE_EDIT)
        {
            layout_dbind_obj(data->edit_layout, cell->content.edit, DEdit);
            panel_visible_layout(data->cell_panel, 5);
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
