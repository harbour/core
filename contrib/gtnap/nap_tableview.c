/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"
#include "hbapi.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CREATE )
{
    TableView *view = tableview_create();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SIZE )
{
    TableView *view = (TableView*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    tableview_size(view, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_GRID )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t hlines = (bool_t)hb_parl(2);
    bool_t vlines = (bool_t)hb_parl(3);
    tableview_grid(view, hlines, vlines);
}

/*---------------------------------------------------------------------------*/

static void i_OnTableNotify(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);

    switch(etype) {
    case ekEVTBLNROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = hb_gtnap_area_row_count(gtarea);
        break;
    }

    case ekEVTBLCELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = hb_gtnap_area_eval_field(gtarea, pos->col + 1, pos->row + 1, &cell->align);
        break;
    }
    }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_BIND_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    GtNapArea *gtarea = hb_gtnap_new_area(view);
    tableview_OnNotify(view, listener(gtarea, i_OnTableNotify, GtNapArea));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_COLUMN_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    const char_t *title = hb_gtnap_parText(2);
    real32_t width = (real32_t)hb_parnd(3);
    align_t align = (align_t)hb_parni(4);
    PHB_ITEM codeBlock = hb_param(5, HB_IT_BLOCK);
    GtNapArea *gtarea = hb_gtnap_get_area(view);
    hb_gtnap_area_add_column(gtarea, title, width, align, codeBlock);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_UPDATE_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    tableview_update(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SELECTED )
{
    TableView *view = (TableView*)hb_parptr(1);
    const ArrSt(uint32_t) *selected = tableview_selected(view);
    uint32_t n = arrst_size(selected, uint32_t);
    hb_reta(n);
    arrst_foreach_const(v, selected, uint32_t)
        hb_storvnl((long)(*v + 1), -1, v_i + 1);
    arrst_end();
}
