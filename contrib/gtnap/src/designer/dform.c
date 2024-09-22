/* Design form */

#include "dform.h"
#include "dlayout.h"
#include <gui/panel.h>
#include <geom2d/v2d.h>
#include <core/dbind.h>
#include <core/heap.h>
#include <sewer/cassert.h>

struct _dform_t
{
    DLayout *dlayout;
    Layout *layout;
    Panel *panel;
};

/*---------------------------------------------------------------------------*/

DForm *dform_first_example(void)
{
    DForm *form = heap_new0(DForm);
    DLayout *layout1 = dlayout_create(2, 6);
    DLayout *layout2 = dlayout_create(1, 4);
    DLayout *layout3 = dlayout_create(2, 1);
    dlayout_add_layout(layout3, layout1, 0, 0);
    dlayout_add_layout(layout3, layout2, 1, 0);
    dlayout_margin_col(layout1, 0, 5);
    dlayout_margin_row(layout1, 0, 5);
    dlayout_margin_row(layout1, 1, 5);
    dlayout_margin_row(layout1, 2, 5);
    dlayout_margin_row(layout1, 3, 5);
    dlayout_margin_row(layout1, 4, 5);
    dlayout_margin_row(layout2, 0, 5);
    dlayout_margin_row(layout2, 1, 5);
    dlayout_margin_row(layout2, 2, 5);
    dlayout_margin_left(layout3, 10);
    dlayout_margin_top(layout3, 10);
    dlayout_margin_right(layout3, 10);
    dlayout_margin_bottom(layout3, 10);
    dlayout_margin_col(layout3, 0, 5);
    form->dlayout = layout3;
    return form;
}

/*---------------------------------------------------------------------------*/

void dform_destroy(DForm **form)
{
    cassert_no_null(form);
    cassert_no_null(*form);
    dlayout_destroy(&(*form)->dlayout);
    /* Layout and Panel will be destroyed in host window */
    heap_delete(form, DForm);
}

/*---------------------------------------------------------------------------*/

Panel *dform_panel(DForm *form)
{
    cassert_no_null(form);
    cassert(form->layout == NULL);
    cassert(form->panel == NULL);
    form->layout = dlayout_gui_layout(form->dlayout);
    form->panel = panel_create();
    panel_layout(form->panel, form->layout);
    return form->panel;
}

/*---------------------------------------------------------------------------*/

void dform_synchro_visual(DForm *form)
{
    cassert_no_null(form);
    dlayout_synchro_visual(form->dlayout, form->layout, kV2D_ZEROf);
}

/*---------------------------------------------------------------------------*/

void dform_draw(const DForm *form, DCtx *ctx)
{
    cassert_no_null(form);
    dlayout_draw(form->dlayout, ctx);
}
