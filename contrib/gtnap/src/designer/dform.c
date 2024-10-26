/* Design form */

#include "dform.h"
#include "dlayout.h"
#include "dlabel.h"
#include "dialogs.h"
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
#include <core/dbind.h>
#include <core/heap.h>
#include <core/strings.h>
#include <sewer/cassert.h>

struct _dform_t
{
    DLayout *dlayout;
    Layout *layout;
    DSelect hover;
    DSelect sel;
    Panel *panel;
};

/*---------------------------------------------------------------------------*/

DForm *dform_first_example(void)
{
    DForm *form = heap_new0(DForm);
    DLayout *layout1 = dlayout_create(2, 6);
    DLayout *layout2 = dlayout_create(1, 4);
    DLayout *layout3 = dlayout_create(2, 1);
    DLabel *label1 = dlabel_create();
    DLabel *label2 = dlabel_create();
    dlayout_set_name(layout1, "Left grid");
    dlayout_set_name(layout2, "Right grid");
    dlayout_set_name(layout3, "Main grid");
    dlabel_text(label1, "This is a label");
    dlabel_text(label2, "And other");
    dlayout_add_label(layout1, label1, 0, 0);
    dlayout_add_label(layout1, label2, 1, 0);
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

bool_t dform_OnMove(DForm *form, const real32_t mouse_x, const real32_t mouse_y)
{
    DSelect hover;
    bool_t equ = TRUE;
    dlayout_elem_at_pos(form->dlayout, mouse_x, mouse_y, &hover);
    equ = i_sel_equ(&form->hover, &hover);
    form->hover = hover;
    return !equ;
}

/*---------------------------------------------------------------------------*/

bool_t dform_OnClick(DForm *form, Window *window, Panel *propedit, const widget_t widget, const real32_t mouse_x, const real32_t mouse_y, const gui_mouse_t button)
{
    cassert_no_null(form);
    if (button == ekGUI_MOUSE_LEFT)
    {
        DSelect sel;
        dlayout_elem_at_pos(form->dlayout, mouse_x, mouse_y, &sel);
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
                    /*
                    * 1) Add the label into design layout.
                    * 2) Add the label into real GUI layout.
                    * 3) Disable the empty cell 'forced' size
                    * 4) Update the GUI panel to recompute the GUI.
                    * 5) Synchro design layout with real GUI layout.
                    * 6) Update the drawing (return TRUE).
                    */
                    Layout *layout = dlayout_search_layout(form->dlayout, form->layout, sel.layout);
                    Label *label = label_create();
                    Cell *cell = layout_cell(layout, sel.col, sel.row);
                    S2Df fsize;
                    label_text(label, tc(dlabel->text));
                    dlayout_add_label(sel.layout, dlabel, sel.col, sel.row);
                    layout_label(layout, label, sel.col, sel.row);
                    //--------------------------- TO form_compose()
                    cell_force_size(cell, 0, 0);
                    _panel_compose(form->panel, NULL, &fsize);
                    dlayout_synchro_visual(form->dlayout, form->layout, kV2D_ZEROf);
                    propedit_set(propedit, form, &sel);
                    //----------------------------
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

void dform_update_cell_text(DForm *form, const DSelect *sel, const char_t *text)
{
    DCell *cell = dlayout_cell(sel);
    Layout *layout = NULL;
    cassert_no_null(form);
    cassert_no_null(cell);
    layout = dlayout_search_layout(form->dlayout, form->layout, sel->layout);

    if (cell->type == ekCELL_TYPE_LABEL)
    {
        Label *label = layout_get_label(layout, sel->col, sel->row);
        dlabel_text(cell->content.label, text);
        label_text(label, text);
    }
    else
    {
        /* At the moment, only Label can update the text */
        cassert(FALSE);
    }
}

/*---------------------------------------------------------------------------*/

void dform_draw(const DForm *form, const widget_t swidget, const Image *add_icon, DCtx *ctx)
{
    cassert_no_null(form);
    dlayout_draw(form->dlayout, form->layout, &form->hover, &form->sel, swidget, add_icon, ctx);
}
