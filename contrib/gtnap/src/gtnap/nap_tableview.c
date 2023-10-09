/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "hbapi.h"
#include <core/arrst.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    bool_t multisel = (bool_t)hb_parl(6);
    bool_t autoclose = (bool_t)hb_parl(7);
    bool_t in_scroll = (bool_t)hb_parl(8);
    uint32_t id = hb_gtnap_tableview(wid, top, left, bottom, right, multisel, autoclose, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_COLUMN )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t width = hb_parni(3);
    HB_ITEM *head_block = hb_param(4, HB_IT_BLOCK);
    HB_ITEM *eval_block = hb_param(5, HB_IT_BLOCK);
    hb_gtnap_tableview_column(wid, id, width, head_block, eval_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SCROLL )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t horizontal = (bool_t)hb_parl(3);
    bool_t vertical = (bool_t)hb_parl(4);
    hb_gtnap_tableview_scroll(wid, id, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_GRID )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t hlines = (bool_t)hb_parl(3);
    bool_t vlines = (bool_t)hb_parl(4);
    hb_gtnap_tableview_grid(wid, id, hlines, vlines);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_HEADER )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t visible = (bool_t)hb_parl(3);
    hb_gtnap_tableview_header(wid, id, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_FREEZE )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t col_id = hb_parni(3);
    hb_gtnap_tableview_freeze(wid, id, col_id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_BIND_AREA )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    HB_ITEM *while_block = hb_param(3, HB_IT_BLOCK);
    hb_gtnap_tableview_bind_area(wid, id, while_block);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_BIND_DATA )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t num_rows = hb_parni(3);
    hb_gtnap_tableview_bind_data(wid, id, num_rows);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_DESELECT_ALL )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    hb_gtnap_tableview_deselect_all(wid, id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SELECT_ROW )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t row_id = hb_parni(3);
    cassert(row_id > 0);
    hb_gtnap_tableview_select_row(wid, id, row_id - 1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_TOGGLE_ROW )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t row_id = hb_parni(3);
    cassert(row_id > 0);
    hb_gtnap_tableview_toggle_row(wid, id, row_id - 1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SELECTED_ROWS )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    const ArrSt(uint32_t) *selected = hb_gtnap_tableview_selected_rows(wid, id);
    uint32_t n = arrst_size(selected, uint32_t);
    hb_reta(n);
    arrst_foreach_const(v, selected, uint32_t)
        hb_storvnl((long)(*v + 1), -1, v_i + 1);
    arrst_end();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_FOCUS_ROW )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t row = hb_gtnap_tableview_focus_row(wid, id) + 1;
    hb_retni(row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_RECNO_FROM_ROW )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t row_id = hb_parni(3);
    uint32_t recno;
    cassert(row_id > 0);
    recno = hb_gtnap_tableview_recno_from_row(wid, id, row_id - 1);
    hb_retni(recno);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_ROW_FROM_RECNO )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t recno = hb_parni(3);
    uint32_t row;
    row = hb_gtnap_tableview_row_from_recno(wid, id, recno);
    hb_retni(row + 1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_REFRESH_CURRENT )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    hb_gtnap_tableview_refresh_current(wid, id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_REFRESH_ALL )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    hb_gtnap_tableview_refresh_all(wid, id);
}
