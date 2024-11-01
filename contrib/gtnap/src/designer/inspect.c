/* Object inspector */

#include "inspect.h"
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
//#include <core/event.h>
#include <core/heap.h>
//#include <core/dbind.h>
//#include <core/strings.h>
//#include <sewer/bmem.h>
#include <sewer/cassert.h>

typedef struct _inspectdata_t InspectData;

struct _inspectdata_t
{
    View *canvas;
    DForm *form;
};

/*---------------------------------------------------------------------------*/

static void i_destroy_data(InspectData **data)
{
    heap_delete(data, InspectData);
}

/*---------------------------------------------------------------------------*/

static InspectData *i_data(View *canvas)
{
    InspectData *data = heap_new0(InspectData);
    data->canvas = canvas;
    return data;
}

/*---------------------------------------------------------------------------*/

Panel *inspect_create(View *canvas)
{
    InspectData *data = i_data(canvas);
    Layout *layout = layout_create(1, 1);
    TableView *table = tableview_create();
    Panel *panel = panel_create();
    tableview_new_column_text(table);
    tableview_column_width(table, 0, 120);
    layout_tableview(layout, table, 0, 0);
    panel_layout(panel, layout);
    panel_data(panel, &data, i_destroy_data, InspectData);
    return panel;
}

/*---------------------------------------------------------------------------*/

void inspect_set(Panel *panel, DForm *form, const ArrSt(DSelect) *selpath)
{
    unref(panel);
    unref(form);
    unref(selpath);
}
