/* Design form */

#include "dform.h"
#include "dlayout.h"
#include "dlabel.h"
#include <gui/panel.h>
#include <gui/panel.inl>
#include <geom2d/v2d.h>
#include <geom2d/s2d.h>
#include <core/dbind.h>
#include <core/heap.h>
#include <sewer/cassert.h>

struct _dform_t
{
    DLayout *dlayout;
    Layout *layout;
    DSelect select;
    Panel *panel;
};

/*---------------------------------------------------------------------------*/

DForm *dform_first_example(void)
{
    DForm *form = heap_new0(DForm);
    DLayout *layout1 = dlayout_create(2, 6);
    DLayout *layout2 = dlayout_create(1, 4);
    DLayout *layout3 = dlayout_create(2, 1);
    //DLabel *label = dlabel_create();
    //dlabel_text(label, "This is a label");
    //dlayout_add_label(layout1, label, 0, 0);
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

    if ((*form)->panel != NULL)
    {
        cassert((*form)->layout != NULL);
        _panel_destroy(&(*form)->panel);
    }

    heap_delete(form, DForm);
}

/*---------------------------------------------------------------------------*/

void dform_compose(DForm *form)
{
    S2Df fsize = kS2D_ZEROf;
    cassert_no_null(form);
    cassert(form->layout == NULL);
    cassert(form->panel == NULL);
    form->layout = dlayout_gui_layout(form->dlayout);
    form->panel = panel_create();
    panel_layout(form->panel, form->layout);
    _panel_compose(form->panel, NULL, &fsize);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_visual(DForm *form)
{
    cassert_no_null(form);
    dlayout_synchro_visual(form->dlayout, form->layout, kV2D_ZEROf);
}

/*---------------------------------------------------------------------------*/

static bool_t i_sel_equ(const DSelect *sel1, const DSelect *sel2)
{
    cassert_no_null(sel1);
    cassert_no_null(sel2);
    if (sel1->layout != sel2->layout)
        return FALSE;

    if (sel1->layout == NULL && sel2->layout == NULL)
        return TRUE;

    if (sel1->elem == sel2->elem 
        && sel1->col == sel2->col
        && sel1->row == sel2->row)
        return TRUE;
    else
        return FALSE;
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnMove(DForm *form, const real32_t mouse_x, const real32_t mouse_y)
{
    DSelect sel;
    bool_t equ = TRUE;
    dlayout_elem_at_pos(form->dlayout, mouse_x, mouse_y, &sel);
    equ = i_sel_equ(&form->select, &sel);
    form->select = sel;
    return !equ;
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnExit(DForm *form)
{
    DSelect sel;
    bool_t equ = TRUE;
    sel.layout = NULL;
    sel.elem = ENUM_MAX(layelem_t);
    sel.col = UINT32_MAX;
    sel.row = UINT32_MAX;
    equ = i_sel_equ(&form->select, &sel);
    form->select = sel;
    return !equ;
}

/*---------------------------------------------------------------------------*/

void dform_draw(const DForm *form, DCtx *ctx)
{
    cassert_no_null(form);
    dlayout_draw(form->dlayout, &form->select, ctx);
}
