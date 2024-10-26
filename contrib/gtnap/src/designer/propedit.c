/* Property editor */

#include "propedit.h"
#include <gui/edit.h>
#include <gui/label.h>
//#include <gui/labelh.h>
#include <gui/layout.h>
//#include <gui/layouth.h>
//#include <gui/cell.h>
#include <gui/panel.h>
//#include <gui/drawctrl.inl>
//#include <geom2d/r2d.h>
//#include <geom2d/v2d.h>
//#include <draw2d/color.h>
//#include <draw2d/draw.h>
//#include <draw2d/drawg.h>
//#include <draw2d/image.h>
#include <core/heap.h>
//#include <core/dbind.h>
//#include <core/strings.h>
//#include <sewer/bmem.h>
#include <sewer/cassert.h>

typedef struct _paneldata_t PanelData;

struct _paneldata_t
{
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

static Layout *i_label_layout(PanelData *data)
{
    Layout *layout = layout_create(2, 1);
    Label *label = label_create();
    Edit *edit = edit_create();
    cassert_no_null(data);
    label_text(label, "Text: ");
    layout_label(layout, label, 0, 0);
    layout_edit(layout, edit, 1, 0);
    return layout;
}

/*---------------------------------------------------------------------------*/

static PanelData *i_data(void)
{
    PanelData *data = heap_new0(PanelData);
    return data;
}

/*---------------------------------------------------------------------------*/

static void i_destroy_data(PanelData **data)
{
    heap_delete(data, PanelData);
}

/*---------------------------------------------------------------------------*/

Panel *propedit_create(void)
{
    PanelData *data = i_data();
    Layout *layout0 = i_no_sel_layout();
    Layout *layout1 = i_empty_cell_layout();
    Layout *layout2 = i_label_layout(data);
    Panel *panel = panel_create();
    panel_layout(panel, layout0);
    panel_layout(panel, layout1);
    panel_layout(panel, layout2);
    panel_data(panel, &data, i_destroy_data, PanelData);
    panel_visible_layout(panel, 0);
    return panel;
}
