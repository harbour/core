/* Design form */

#include "dform.h"
#include "dcell.h"
#include "dlayout.h"
#include "dlabel.h"
#include "dialogs.h"
#include "inspect.h"
#include "propedit.h"
#include <gui/guicontrol.h>
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/layouth.h>
#include <gui/panel.h>
#include <gui/panel.inl>
#include <gui/window.h>
#include <geom2d/v2d.h>
#include <geom2d/s2d.h>
#include <core/arrst.h>
#include <core/dbind.h>
#include <core/heap.h>
#include <core/strings.h>
#include <sewer/bstd.h>
#include <sewer/cassert.h>

struct _dform_t
{
    DLayout *dlayout;
    Layout *layout;
    DSelect hover;
    DSelect sel;
    ArrSt(DSelect) *temp_path;
    ArrSt(DSelect) *sel_path;
    Panel *panel;
    uint32_t layout_id;
    uint32_t cell_id;
};

/*---------------------------------------------------------------------------*/

static void i_layout_obj_names(DForm *form, DLayout *dlayout)
{
    cassert_no_null(form);

    {
        char_t name[64];
        bstd_sprintf(name, sizeof(name), "layout%d", form->layout_id);
        dlayout_name(dlayout, name);
        form->layout_id += 1;
    }

    {
        uint32_t i, ncols = dlayout_ncols(dlayout);
        uint32_t j, nrows = dlayout_nrows(dlayout);
        for (j = 0; j < nrows; ++j)
        {
            for (i = 0; i < ncols; ++i)
            {
                char_t name[64];
                DCell *dcell = dlayout_cell(dlayout, i, j);
                bstd_sprintf(name, sizeof(name), "cell%d", form->cell_id);
                dcell_name(dcell, name);
                form->cell_id += 1;
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

//DForm *dform_first_example(void)
//{
//    DForm *form = heap_new0(DForm);
//    DLayout *layout1 = dlayout_create(3, 3);
//    dlayout_margin_col(layout1, 0, 5);
//    dlayout_margin_col(layout1, 1, 5);
//    dlayout_margin_row(layout1, 0, 5);
//    dlayout_margin_row(layout1, 1, 5);
//    dlayout_margin_left(layout1, 10);
//    dlayout_margin_top(layout1, 10);
//    dlayout_margin_right(layout1, 10);
//    dlayout_margin_bottom(layout1, 10);
//    form->dlayout = layout1;
//    form->temp_path = arrst_create(DSelect);
//    form->sel_path = arrst_create(DSelect);
//    i_set_layout_name(form, layout1);
//    return form;
//}

/*---------------------------------------------------------------------------*/

DForm *dform_first_example(void)
{
    DForm *form = heap_new0(DForm);
    DLayout *layout1 = dlayout_create(2, 6);
    DLayout *layout2 = dlayout_create(1, 4);
    DLayout *layout3 = dlayout_create(2, 1);
    DLabel *label1 = dlabel_create();
    DLabel *label2 = dlabel_create();
    dlabel_text(label1, "This is a label");
    dlabel_text(label2, "Other");
    dlayout_add_label(layout1, label1, 0, 0);
    dlayout_add_label(layout1, label2, 0, 1);
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
    form->temp_path = arrst_create(DSelect);
    form->sel_path = arrst_create(DSelect);
    i_layout_obj_names(form, layout1);
    i_layout_obj_names(form, layout2);
    i_layout_obj_names(form, layout3);
    return form;
}

/*---------------------------------------------------------------------------*/

void dform_destroy(DForm **form)
{
    cassert_no_null(form);
    cassert_no_null(*form);
    dlayout_destroy(&(*form)->dlayout);
    arrst_destroy(&(*form)->temp_path, NULL, DSelect);
    arrst_destroy(&(*form)->sel_path, NULL, DSelect);
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
    /*
    * 1) Recompute the GUI form for update lastest changes.
    * 2) Synchro design form with GUI controls and sizes.
    */
    S2Df fsize = kS2D_ZEROf;
    cassert_no_null(form);
    if (form->layout == NULL)
    {
        cassert(form->panel == NULL);
        form->layout = dlayout_gui_layout(form->dlayout);
        form->panel = panel_create();
        panel_layout(form->panel, form->layout);
    }

    _panel_compose(form->panel, NULL, &fsize);
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

static void i_elem_at_mouse(DLayout *dlayout, const real32_t mouse_x, const real32_t mouse_y, ArrSt(DSelect) *selpath, DSelect *sel)
{
    cassert_no_null(sel);
    arrst_clear(selpath, NULL, DSelect);
    dlayout_elem_at_pos(dlayout, mouse_x, mouse_y, selpath);
    if (arrst_size(selpath, DSelect) > 0)
    {
        *sel = *arrst_last(selpath, DSelect);
    }
    else
    {
        sel->layout = NULL;
        sel->elem = ENUM_MAX(layelem_t);
        sel->col = UINT32_MAX;
        sel->row = UINT32_MAX;
    }
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnMove(DForm *form, const real32_t mouse_x, const real32_t mouse_y)
{
    DSelect hover;
    bool_t equ = TRUE;
    i_elem_at_mouse(form->dlayout, mouse_x, mouse_y, form->temp_path, &hover);
    equ = i_sel_equ(&form->hover, &hover);
    form->hover = hover;
    return !equ;
}

/*---------------------------------------------------------------------------*/

static align_t i_halign(const halign_t halign)
{
    switch(halign) {
    case ekHALIGN_LEFT:
        return ekLEFT;
    case ekHALIGN_CENTER:
        return ekCENTER;
    case ekHALIGN_RIGHT:
        return ekRIGHT;
    case ekHALIGN_JUSTIFY:
        return ekJUSTIFY;
    cassert_default();
    }
    return ekLEFT;
}

/*---------------------------------------------------------------------------*/

static align_t i_valign(const valign_t valign)
{
    switch(valign) {
    case ekVALIGN_TOP:
        return ekTOP;
    case ekVALIGN_CENTER:
        return ekCENTER;
    case ekVALIGN_BOTTOM:
        return ekBOTTOM;
    case ekVALIGN_JUSTIFY:
        return ekJUSTIFY;
    cassert_default();
    }
    return ekTOP;
}

/*---------------------------------------------------------------------------*/

static void i_synchro_cell_props(const DLayout *dlayout, Layout *layout, const uint32_t col, const uint32_t row)
{
    const DCell *dcell = dlayout_ccell(dlayout, col, row);
    align_t halign = i_halign(dcell->halign);
    align_t valign = i_valign(dcell->valign);
    layout_halign(layout, col, row, halign);
    layout_valign(layout, col, row, valign);
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnClick(DForm *form, Window *window, Panel *inspect, Panel *propedit, const widget_t widget, const real32_t mouse_x, const real32_t mouse_y, const gui_mouse_t button)
{
    cassert_no_null(form);
    if (button == ekGUI_MOUSE_LEFT)
    {
        DSelect sel;
        i_elem_at_mouse(form->dlayout, mouse_x, mouse_y, form->sel_path, &sel);
        inspect_set(inspect, form);
        if (dlayout_empty_cell(&sel) == TRUE)
        {
            switch(widget) {
            case ekWIDGET_SELECT:
                break;

            case ekWIDGET_LABEL:
            {
                DLabel *dlabel = dialog_new_label(window, &sel);
                if (dlabel != NULL)
                {
                    Layout *layout = dlayout_search_layout(form->dlayout, form->layout, sel.layout);
                    Label *label = label_create();
                    label_text(label, tc(dlabel->text));
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    dlayout_add_label(sel.layout, dlabel, sel.col, sel.row);
                    layout_label(layout, label, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout, layout, sel.col, sel.row);
                    dform_compose(form);
                    propedit_set(propedit, form, &sel);
                    inspect_set(inspect, form);
                    form->sel = sel;
                    return TRUE;
                }
                else
                {
                    return FALSE;
                }
            }

            case ekWIDGET_GRID_LAYOUT:
            {
                DLayout *dsublayout = dialog_new_layout(window, &sel);
                if (dsublayout != NULL)
                {
                    Layout *layout = dlayout_search_layout(form->dlayout, form->layout, sel.layout);
                    Layout *sublayout = dlayout_gui_layout(dsublayout);
                    i_layout_obj_names(form, dsublayout);
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    dlayout_add_layout(sel.layout, dsublayout, sel.col, sel.row);
                    layout_layout(layout, sublayout, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout, layout, sel.col, sel.row);
                    dform_compose(form);
                    propedit_set(propedit, form, &sel);
                    inspect_set(inspect, form);
                    form->sel = sel;
                    return TRUE;
                }
                else
                {
                    return FALSE;
                }
            }


            default:
                break;
            }
        }

        /* No new component added, just select */
        {
            bool_t equ = i_sel_equ(&form->sel, &sel);
            propedit_set(propedit, form, &sel);
            form->sel = sel;
            return !equ;
        }
    }
    else
    {
        return FALSE;
    }
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
    equ = i_sel_equ(&form->hover, &sel);
    form->hover = sel;
    return !equ;
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_text(DForm *form, const DSelect *sel)
{
    DCell *cell = dlayout_cell_sel(sel);
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(cell);
    layout = dlayout_search_layout(form->dlayout, form->layout, sel->layout);

    if (cell->type == ekCELL_TYPE_LABEL)
    {
        Label *label = layout_get_label(layout, sel->col, sel->row);
        label_text(label, tc(cell->content.label->text));
    }
    else
    {
        /* At the moment, only Label can update the text */
        cassert(FALSE);
    }
}

/*---------------------------------------------------------------------------*/

void dform_synchro_layout_margin(DForm *form, const DLayout *dlayout)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(dlayout);
    layout = dlayout_search_layout(form->dlayout, form->layout, dlayout);
    layout_margin4(layout, dlayout->margin_top, dlayout->margin_right, dlayout->margin_bottom, dlayout->margin_left);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_column_margin(DForm *form, const DLayout *dlayout, const DColumn *dcolumn, const uint32_t col)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(dcolumn);
    cassert(dlayout_column(cast(dlayout, DLayout), col) == dcolumn);
    layout = dlayout_search_layout(form->dlayout, form->layout, dlayout);
    layout_hmargin(layout, col, dcolumn->margin_right);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_row_margin(DForm *form, const DLayout *dlayout, const DRow *drow, const uint32_t row)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(drow);
    cassert(dlayout_row(cast(dlayout, DLayout), row) == drow);
    layout = dlayout_search_layout(form->dlayout, form->layout, dlayout);
    layout_vmargin(layout, row, drow->margin_bottom);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_halign(DForm *form, const DLayout *dlayout, const DCell *cell, const uint32_t col, const uint32_t row)
{
    Layout *layout = NULL;
    align_t align = ENUM_MAX(align_t);
    cassert_no_null(form);
    cassert_no_null(cell);
    cassert(dlayout_cell(cast(dlayout, DLayout), col, row) == cell);
    layout = dlayout_search_layout(form->dlayout, form->layout, dlayout);
    align = i_halign(cell->halign);
    layout_halign(layout, col, row, align);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_valign(DForm *form, const DLayout *dlayout, const DCell *cell, const uint32_t col, const uint32_t row)
{
    Layout *layout = NULL;
    align_t align = ENUM_MAX(align_t);
    cassert_no_null(form);
    cassert_no_null(cell);
    cassert(dlayout_cell(cast(dlayout, DLayout), col, row) == cell);
    layout = dlayout_search_layout(form->dlayout, form->layout, dlayout);
    align = i_valign(cell->valign);
    layout_valign(layout, col, row, align);
}

/*---------------------------------------------------------------------------*/

void dform_draw(const DForm *form, const widget_t swidget, const Image *add_icon, DCtx *ctx)
{
    cassert_no_null(form);
    dlayout_draw(form->dlayout, form->layout, &form->hover, &form->sel, swidget, add_icon, ctx);
}

/*---------------------------------------------------------------------------*/

uint32_t dform_selpath_size(const DForm *form)
{
    uint32_t n = 0;
    cassert_no_null(form);
    n = arrst_size(form->sel_path, DSelect);    
    if (n > 0)
    {
        const DSelect *last = arrst_last_const(form->sel_path, DSelect);
        cassert(last->layout != NULL);
        if (last->elem == ekLAYELEM_CELL)
            return n * 2;
        else
            return (n - 1) * 2 + 1;
    }
    
    return 0;
}

/*---------------------------------------------------------------------------*/

const char_t *dform_selpath_caption(const DForm *form, const uint32_t col, const uint32_t row)
{
    const DSelect *sel = NULL;
    cassert_no_null(form);
    sel = arrst_get_const(form->sel_path, row / 2, DSelect);
    cassert(col <= 1);

    /* Even rows == layout */
    if (row % 2 == 0)
    {
        if (col == 0)
            return tc(sel->layout->name);
        else
            return "Layout";
    }
    /* Odd rows == cell */
    else
    {
        const DCell *cell = dlayout_cell_sel(sel);
        if (col == 0)
        {
            return tc(cell->name);
        }
        else
        {
            cassert(col == 1);
            switch(cell->type)
            {
            case ekCELL_TYPE_EMPTY:
                return "EmptyCell";
            case ekCELL_TYPE_LABEL:
                return "LabelCell";
            case ekCELL_TYPE_LAYOUT:
                return "LayoutCell";
            cassert_default();
            }
        }
    }

    return "";
}

/*---------------------------------------------------------------------------*/

void dform_inspect_select(DForm *form, Panel *propedit, const uint32_t row)
{
    const DSelect *sel = NULL;
    cassert_no_null(form);
    sel = arrst_get_const(form->sel_path, row / 2, DSelect);

    /* Even rows == layout */
    if (row % 2 == 0)
    {
        DSelect propsel;
        propsel.layout = sel->layout;
        propsel.elem = ekLAYELEM_MARGIN_LEFT;
        propsel.col = UINT32_MAX;
        propsel.row = UINT32_MAX;
        form->sel = propsel;
        propedit_set(propedit, form, &propsel);
    }
    /* Odd rows == cell */
    else
    {
        cassert(sel->elem == ekLAYELEM_CELL);
        form->sel = *sel;
        propedit_set(propedit, form, sel);
    }
}

