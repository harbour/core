/* Design form */

#include "dform.h"
#include "dlayout.h"
#include "dialogs.h"
#include "inspect.h"
#include "propedit.h"
#include <nform/flabel.h>
#include <nform/flayout.h>
#include <gui/guicontrol.h>
#include <gui/button.h>
#include <gui/edit.h>
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
    Window *window;
    DSelect hover;
    DSelect sel;
    ArrSt(DSelect) *temp_path;
    ArrSt(DSelect) *sel_path;
    uint32_t layout_id;
    uint32_t cell_id;
};

/*---------------------------------------------------------------------------*/

static real32_t i_EMPTY_CELL_WIDTH = 40;
static real32_t i_EMPTY_CELL_HEIGHT = 20;

/*---------------------------------------------------------------------------*/

static void i_layout_obj_names(DForm *form, FLayout *flayout)
{
    cassert_no_null(form);

    {
        char_t name[64];
        bstd_sprintf(name, sizeof(name), "layout%d", form->layout_id);
        str_upd(&flayout->name, name);
        form->layout_id += 1;
    }

    {
        uint32_t i, ncols = flayout_ncols(flayout);
        uint32_t j, nrows = flayout_nrows(flayout);
        for (j = 0; j < nrows; ++j)
        {
            for (i = 0; i < ncols; ++i)
            {
                char_t name[64];
                FCell *fcell = flayout_cell(flayout, i, j);
                bstd_sprintf(name, sizeof(name), "cell%d", form->cell_id);
                str_upd(&fcell->name, name);
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
    FLabel *label1 = flabel_create();
    FLabel *label2 = flabel_create();
    str_upd(&label1->text, "This is a label");
    str_upd(&label2->text, "Other");
    flayout_add_label(layout1->flayout, label1, 0, 0);
    flayout_add_label(layout1->flayout, label2, 0, 1);
    dlayout_add_layout(layout3, layout1, 0, 0);
    dlayout_add_layout(layout3, layout2, 1, 0);
    flayout_margin_col(layout1->flayout, 0, 5);
    flayout_margin_row(layout1->flayout, 0, 5);
    flayout_margin_row(layout1->flayout, 1, 5);
    flayout_margin_row(layout1->flayout, 2, 5);
    flayout_margin_row(layout1->flayout, 3, 5);
    flayout_margin_row(layout1->flayout, 4, 5);
    flayout_margin_row(layout2->flayout, 0, 5);
    flayout_margin_row(layout2->flayout, 1, 5);
    flayout_margin_row(layout2->flayout, 2, 5);
    flayout_margin_left(layout3->flayout, 10);
    flayout_margin_top(layout3->flayout, 10);
    flayout_margin_right(layout3->flayout, 10);
    flayout_margin_bottom(layout3->flayout, 10);
    flayout_margin_col(layout3->flayout, 0, 5);
    form->dlayout = layout3;
    form->temp_path = arrst_create(DSelect);
    form->sel_path = arrst_create(DSelect);
    i_layout_obj_names(form, layout1->flayout);
    i_layout_obj_names(form, layout2->flayout);
    i_layout_obj_names(form, layout3->flayout);
    return form;
}

/*---------------------------------------------------------------------------*/

DForm *dform_empty(void)
{
    DForm *form = heap_new0(DForm);
    form->dlayout = dlayout_create(1, 1);
    form->temp_path = arrst_create(DSelect);
    form->sel_path = arrst_create(DSelect);
    i_layout_obj_names(form, form->dlayout->flayout);
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
    if ((*form)->window != NULL)
    {
        cassert((*form)->layout != NULL);
        window_destroy(&(*form)->window);
    }

    heap_delete(form, DForm);
}

/*---------------------------------------------------------------------------*/

void dform_compose(DForm *form)
{
    cassert_no_null(form);
    if (form->layout == NULL)
    {
        Panel *panel = panel_create();
        cassert(form->window == NULL);
        cassert_no_null(form->dlayout);
        form->layout = flayout_to_gui(form->dlayout->flayout, i_EMPTY_CELL_WIDTH, i_EMPTY_CELL_HEIGHT);
        panel_layout(panel, form->layout);
        form->window = window_create(ekWINDOW_STD);
        window_panel(form->window, panel);
    }

    window_update(form->window);
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

static void i_synchro_cell_props(const FLayout *flayout, Layout *layout, const uint32_t col, const uint32_t row)
{
    const FCell *dcell = flayout_ccell(flayout, col, row);
    align_t halign = i_halign(dcell->halign);
    align_t valign = i_valign(dcell->valign);
    layout_halign(layout, col, row, halign);
    layout_valign(layout, col, row, valign);
}

/*---------------------------------------------------------------------------*/

static Layout *i_gui_search(const FLayout *layout, Layout *glayout, const FLayout *wanted)
{
    cassert_no_null(layout);
    if (layout != wanted)
    {
        const FCell *cell = arrst_all_const(layout->cells, FCell);
        uint32_t ncols = arrst_size(layout->cols, FColumn);
        uint32_t nrows = arrst_size(layout->rows, FRow);
        uint32_t i, j;

        for (j = 0; j < nrows; ++j)
        {
            for (i = 0; i < ncols; ++i)
            {
                if (cell->type == ekCELL_TYPE_LAYOUT)
                {
                    FLayout *sublayout = cell->widget.layout;
                    Layout *subglayout = layout_get_layout(cast(glayout, Layout), i, j);
                    Layout *fglayout = i_gui_search(sublayout, subglayout, wanted);
                    if (fglayout != NULL)
                        return fglayout;
                }

                cell += 1;
            }
        }

        return NULL;
    }
    else
    {
        cassert(flayout_ncols(layout) == layout_ncols(glayout));
        cassert(flayout_nrows(layout) == layout_nrows(glayout));
        return glayout;
    }
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnClick(DForm *form, Window *window, Panel *inspect, Panel *propedit, const widget_t widget, const real32_t mouse_x, const real32_t mouse_y, const gui_mouse_t mbutton)
{
    cassert_no_null(form);
    if (mbutton == ekGUI_MOUSE_LEFT)
    {
        DSelect sel;
        i_elem_at_mouse(form->dlayout, mouse_x, mouse_y, form->sel_path, &sel);
        inspect_set(inspect, form);
        if (dlayout_empty_cell(&sel) == TRUE)
        {
            cassert_no_null(form->dlayout);
            switch(widget) {
            case ekWIDGET_SELECT:
                break;

            case ekWIDGET_LABEL:
            {
                FLabel *flabel = dialog_new_label(window, &sel);
                if (flabel != NULL)
                {
                    Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, sel.layout->flayout);
                    Label *label = label_create();
                    label_text(label, tc(flabel->text));
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    flayout_add_label(sel.layout->flayout, flabel, sel.col, sel.row);
                    layout_label(layout, label, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout->flayout, layout, sel.col, sel.row);
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

            case ekWIDGET_BUTTON:
            {
                FButton *fbutton = dialog_new_button(window, &sel);
                if (fbutton != NULL)
                {
                    Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, sel.layout->flayout);
                    Button *button = button_push();
                    button_text(button, tc(fbutton->text));
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    flayout_add_button(sel.layout->flayout, fbutton, sel.col, sel.row);
                    layout_button(layout, button, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout->flayout, layout, sel.col, sel.row);
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

            case ekWIDGET_CHECKBOX:
            {
                FCheck *fcheck = dialog_new_check(window, &sel);
                if (fcheck != NULL)
                {
                    Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, sel.layout->flayout);
                    Button *check = button_check();
                    button_text(check, tc(fcheck->text));
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    flayout_add_check(sel.layout->flayout, fcheck, sel.col, sel.row);
                    layout_button(layout, check, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout->flayout, layout, sel.col, sel.row);
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

            case ekWIDGET_EDITBOX:
            {
                FEdit *fedit = dialog_new_edit(window, &sel);
                if (fedit != NULL)
                {
                    Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, sel.layout->flayout);
                    Edit *edit = edit_create();
                    align_t align = i_halign(fedit->text_align);
                    edit_passmode(edit, fedit->passmode);
                    edit_autoselect(edit, fedit->autosel);
                    edit_align(edit, align);
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    flayout_add_edit(sel.layout->flayout, fedit, sel.col, sel.row);
                    layout_edit(layout, edit, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout->flayout, layout, sel.col, sel.row);
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
                    Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, sel.layout->flayout);
                    Layout *sublayout = flayout_to_gui(dsublayout->flayout, i_EMPTY_CELL_WIDTH, i_EMPTY_CELL_HEIGHT);
                    i_layout_obj_names(form, dsublayout->flayout);
                    dlayout_remove_cell(sel.layout, sel.col, sel.row);
                    layout_remove_cell(layout, sel.col, sel.row);
                    dlayout_add_layout(sel.layout, dsublayout, sel.col, sel.row);
                    layout_layout(layout, sublayout, sel.col, sel.row);
                    i_synchro_cell_props(sel.layout->flayout, layout, sel.col, sel.row);
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

bool_t dform_OnSupr(DForm *form, Panel *inspect, Panel *propedit)
{
    cassert_no_null(form);
    if (form->sel.layout != NULL && form->sel.elem == ekLAYELEM_CELL)
    {
        if (dlayout_empty_cell(&form->sel) == FALSE)
        {
            Layout *layout = i_gui_search(form->dlayout->flayout, form->layout, form->sel.layout->flayout);
            Cell *cell = layout_cell(layout, form->sel.col, form->sel.row);
            dlayout_remove_cell(form->sel.layout, form->sel.col, form->sel.row);
            layout_remove_cell(layout, form->sel.col, form->sel.row);
            cell_force_size(cell, i_EMPTY_CELL_WIDTH, i_EMPTY_CELL_HEIGHT);
            i_synchro_cell_props(form->sel.layout->flayout, layout, form->sel.col, form->sel.row);
            dform_compose(form);
            propedit_set(propedit, form, &form->sel);
            inspect_set(inspect, form);
            return TRUE;
        }
    }

    return FALSE;
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_text(DForm *form, const DSelect *sel)
{
    FCell *cell = dlayout_sel_fcell(sel);
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(cell);
    layout = i_gui_search(form->dlayout->flayout, form->layout, sel->layout->flayout);

    if (cell->type == ekCELL_TYPE_LABEL)
    {
        Label *label = layout_get_label(layout, sel->col, sel->row);
        label_text(label, tc(cell->widget.label->text));
    }
    else if (cell->type == ekCELL_TYPE_BUTTON)
    {
        Button *button = layout_get_button(layout, sel->col, sel->row);
        button_text(button, tc(cell->widget.button->text));
    }
    else if (cell->type == ekCELL_TYPE_CHECK)
    {
        Button *button = layout_get_button(layout, sel->col, sel->row);
        button_text(button, tc(cell->widget.check->text));
    }
    else
    {
        cassert(FALSE);
    }
}

/*---------------------------------------------------------------------------*/

void dform_synchro_edit(DForm *form, const DSelect *sel)
{
    FCell *cell = dlayout_sel_fcell(sel);
    Layout *layout = NULL;
    Edit *edit = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(cell);
    cassert(cell->type == ekCELL_TYPE_EDIT);
    layout = i_gui_search(form->dlayout->flayout, form->layout, sel->layout->flayout);
    edit = layout_get_edit(layout, sel->col, sel->row);
    edit_passmode(edit, cell->widget.edit->passmode);
    edit_autoselect(edit, cell->widget.edit->autosel);
    edit_align(edit, i_halign(cell->widget.edit->text_align));
}

/*---------------------------------------------------------------------------*/

void dform_synchro_layout_margin(DForm *form, const FLayout *flayout)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(flayout);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    layout_margin4(layout, flayout->margin_top, flayout->margin_right, flayout->margin_bottom, flayout->margin_left);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_column_margin(DForm *form, const FLayout *flayout, const FColumn *fcol, const uint32_t col)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(fcol);
    cassert(flayout_column(cast(flayout, FLayout), col) == fcol);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    layout_hmargin(layout, col, fcol->margin_right);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_column_width(DForm *form, const FLayout *flayout, const FColumn *fcol, const uint32_t col)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(fcol);
    cassert(flayout_column(cast(flayout, FLayout), col) == fcol);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    layout_hsize(layout, col, fcol->forced_width);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_row_margin(DForm *form, const FLayout *flayout, const FRow *frow, const uint32_t row)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(frow);
    cassert(flayout_row(cast(flayout, FLayout), row) == frow);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    layout_vmargin(layout, row, frow->margin_bottom);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_row_height(DForm *form, const FLayout *flayout, const FRow *frow, const uint32_t row)
{
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(frow);
    cassert(flayout_row(cast(flayout, FLayout), row) == frow);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    layout_vsize(layout, row, frow->forced_height);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_halign(DForm *form, const FLayout *flayout, const FCell *fcell, const uint32_t col, const uint32_t row)
{
    Layout *layout = NULL;
    align_t align = ENUM_MAX(align_t);
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(fcell);
    cassert(flayout_cell(cast(flayout, FLayout), col, row) == fcell);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    align = i_halign(fcell->halign);
    layout_halign(layout, col, row, align);
}

/*---------------------------------------------------------------------------*/

void dform_synchro_cell_valign(DForm *form, const FLayout *flayout, const FCell *fcell, const uint32_t col, const uint32_t row)
{
    Layout *layout = NULL;
    align_t align = ENUM_MAX(align_t);
    cassert_no_null(form);
    cassert_no_null(form->dlayout);
    cassert_no_null(fcell);
    cassert(flayout_cell(cast(flayout, FLayout), col, row) == fcell);
    layout = i_gui_search(form->dlayout->flayout, form->layout, flayout);
    align = i_valign(fcell->valign);
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
    cassert_no_null(sel);
    cassert_no_null(sel->layout);
    cassert_no_null(sel->layout->flayout);

    /* Even rows == layout */
    if (row % 2 == 0)
    {
        if (col == 0)
            return tc(sel->layout->flayout->name);
        else
            return "Layout";
    }
    /* Odd rows == cell */
    else
    {
        const FCell *cell = dlayout_sel_fcell(sel);
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
            case ekCELL_TYPE_BUTTON:
                return "ButtonCell";
            case ekCELL_TYPE_CHECK:
                return "CheckBoxCell";
            case ekCELL_TYPE_EDIT:
                return "EditBoxCell";
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

/*---------------------------------------------------------------------------*/
/* Unify with 'dialogs' code */
static void i_center_window(const Window *parent, Window *window)
{
    V2Df p1 = window_get_origin(parent);
    S2Df s1 = window_get_size(parent);
    S2Df s2 = window_get_size(window);
    V2Df p2;
    p2.x = p1.x + (s1.width - s2.width) / 2;
    p2.y = p1.y + (s1.height - s2.height) / 2;
    window_origin(window, p2);
}

/*---------------------------------------------------------------------------*/

void dform_simulate(DForm *form, Window *window)
{
    cassert_no_null(form);
    if (form->window != NULL)
    {
        i_center_window(window, form->window);
        window_modal(form->window, window);
    }
}
