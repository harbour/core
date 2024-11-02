/* Object inspector */

#include "inspect.h"
#include "designer.h"
#include "dform.h"
//#include "dlayout.h"
//#include "dlabel.h"
//#include "dform.h"
//#include <gui/edit.h>
//#include <gui/label.h>
////#include <gui/labelh.h>
#include <gui/layout.h>
////#include <gui/layouth.h>
////#include <gui/cell.h>
#include <gui/panel.h>
//#include <gui/view.h>
#include <gui/tableview.h>
////#include <gui/drawctrl.inl>
////#include <geom2d/r2d.h>
#include <geom2d/s2d.h>
////#include <draw2d/color.h>
////#include <draw2d/draw.h>
////#include <draw2d/drawg.h>
////#include <draw2d/image.h>
#include <core/arrst.h>
#include <core/event.h>
#include <core/heap.h>
//#include <core/dbind.h>
//#include <core/strings.h>
//#include <sewer/bmem.h>
#include <sewer/cassert.h>

typedef struct _inspectdata_t InspectData;

struct _inspectdata_t
{
    Designer *app;
    DForm *form;
    TableView *table;
    uint32_t nrows;
};

/*---------------------------------------------------------------------------*/

static void i_destroy_data(InspectData **data)
{
    heap_delete(data, InspectData);
}

/*---------------------------------------------------------------------------*/

static InspectData *i_data(Designer *app)
{
    InspectData *data = heap_new0(InspectData);
    data->app = app;
    return data;
}

/*---------------------------------------------------------------------------*/

static void i_OnTableData(InspectData *data, Event *e)
{
    uint32_t etype = event_type(e);
    cassert_no_null(data);

    switch (etype)
    {
    case ekGUI_EVENT_TBL_BEGIN:
        break;

    case ekGUI_EVENT_TBL_END:
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        if (data->form != NULL)
            data->nrows = dform_selpath_size(data->form);
        else
            data->nrows = 0;
        *n = data->nrows;
        break;
    }

    case ekGUI_EVENT_TBL_CELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = dform_selpath_caption(data->form, pos->col, pos->row);
        cell->align = ekLEFT;
        break;
    }

    default:
        break;
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnSelect(InspectData *data, Event *e)
{
    const EvTbSel *p = event_params(e, EvTbSel);
    uint32_t sel = 0;
    cassert(arrst_size(p->sel, uint32_t) == 1);
    sel = *arrst_first_const(p->sel, uint32_t);
    designer_inspect_select(data->app, sel);
}

/*---------------------------------------------------------------------------*/

Panel *inspect_create(Designer *app)
{
    InspectData *data = i_data(app);
    Layout *layout = layout_create(1, 1);
    TableView *table = tableview_create();
    Panel *panel = panel_create();
    data->table = table;
    tableview_new_column_text(table);
    tableview_new_column_text(table);
    tableview_column_width(table, 0, 80);
    tableview_column_width(table, 1, 80);
    tableview_header_title(table, 0, "Object");
    tableview_header_title(table, 1, "Type");
    tableview_OnData(table, listener(data, i_OnTableData, InspectData));
    tableview_OnSelect(table, listener(data, i_OnSelect, InspectData));
    tableview_update(table);
    layout_tableview(layout, table, 0, 0);
    panel_layout(panel, layout);
    panel_data(panel, &data, i_destroy_data, InspectData);
    return panel;
}

/*---------------------------------------------------------------------------*/

void inspect_set(Panel *panel, DForm *form)
{
    InspectData *data = panel_get_data(panel, InspectData);
    cassert_no_null(data);
    data->form = form;
    tableview_update(data->table);
    if (data->nrows > 0)
    {
        uint32_t sel = data->nrows - 1;
        tableview_select(data->table, &sel, 1);
        tableview_focus_row(data->table, sel, ekBOTTOM);

    }
}

/*---------------------------------------------------------------------------*/

void inspect_update(Panel *panel)
{
    InspectData *data = panel_get_data(panel, InspectData);
    cassert_no_null(data);
    tableview_update(data->table);
}
