/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nap_tableview.inl"
#include "hbapi.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW )
{
    uint32_t wid = hb_parni(1);
    bool_t multisel = (bool_t)hb_parl(2);
    int32_t top = hb_parni(3);
    int32_t left = hb_parni(4);
    int32_t bottom = hb_parni(5);
    int32_t right = hb_parni(6);
    bool_t in_scroll = (bool_t)hb_parl(7);
    uint32_t id = hb_gtnap_tableview(wid, multisel, top, left, bottom, right, in_scroll);
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

HB_FUNC( NAP_TABLEVIEW_SCROLL2 )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t horizontal = (bool_t)hb_parl(3);
    bool_t vertical = (bool_t)hb_parl(4);
    hb_gtnap_tableview_scroll(wid, id, horizontal, vertical);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_GRID2 )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    bool_t hlines = (bool_t)hb_parl(3);
    bool_t vlines = (bool_t)hb_parl(4);
    hb_gtnap_tableview_grid(wid, id, hlines, vlines);
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

HB_FUNC( NAP_TABLEVIEW_SELECT2 )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    HB_ITEM *selection = hb_param(3, HB_IT_BLOCK);
    hb_gtnap_tableview_select(wid, id, selection);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_REFRESH_ALL )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    hb_gtnap_tableview_refresh_all(wid, id);
}




















/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CREATE )
{
    TableView *view = tableview_create();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SCROLL )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t hor = (bool_t)hb_parl(2);
    bool_t ver = (bool_t)hb_parl(3);
    tableview_scroll_visible(view, hor, ver);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_FONT )
{
    TableView *view = (TableView*)hb_parptr(1);
    tableview_font(view, hb_gtnap_font());
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

HB_FUNC( NAP_TABLEVIEW_COLUMN_FREEZE )
{
    TableView *view = (TableView*)hb_parptr(1);
    uint32_t last_column_id = hb_parni(2);
    tableview_column_freeze(view, last_column_id - 1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_HEADER_VISIBLE )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t visible = (bool_t)hb_parl(2);
    tableview_header_visible(view, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_HEADER_CLICKABLE )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t clickable = (bool_t)hb_parl(2);
    tableview_header_clickable(view, clickable);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_HEADER_RESIZABLE )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t resizable = (bool_t)hb_parl(2);
    tableview_header_resizable(view, resizable);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_MULTISEL )
{
    TableView *view = (TableView*)hb_parptr(1);
    bool_t multisel = (bool_t)hb_parl(2);
    bool_t preserve = (bool_t)hb_parl(3);
    tableview_multisel(view, multisel, preserve);
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

static void i_OnTableData(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);

    switch(etype) {
    case ekGUI_EVENT_TBL_BEGIN:
        hb_gtnap_area_cache_cur_db_row(gtarea);
        break;

    case ekGUI_EVENT_TBL_END:
        hb_gtnap_area_restore_cur_db_row(gtarea);
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = hb_gtnap_area_row_count(gtarea);
        break;
    }

    case ekGUI_EVENT_TBL_CELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = hb_gtnap_area_eval_field(gtarea, pos->col + 1, pos->row, &cell->align);

        // Table column automatic width based on cell content
        hb_gtnap_cualib_column_width(gtarea, pos->col, cell->text);
        break;
    }

    default:
        break;
    }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_BIND_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    GtNapArea *gtarea = hb_gtnap_cualib_tableview_area(view, 2);
    tableview_OnData(view, listener(gtarea, i_OnTableData, GtNapArea));
}

/*---------------------------------------------------------------------------*/

static void i_OnTableDataVector(GtNapVector *gtvect, Event *e)
{
    uint32_t etype = event_type(e);

    switch(etype) {
    case ekGUI_EVENT_TBL_BEGIN:
        break;

    case ekGUI_EVENT_TBL_END:
        break;

    case ekGUI_EVENT_TBL_NROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = hb_gtnap_vector_items_count(gtvect);
        break;
    }

    case ekGUI_EVENT_TBL_CELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = hb_gtnap_vector_eval_field(gtvect, pos->col, pos->row);
        break;
    }

    default:
        break;
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnVectorSelect(GtNapVector *gtvect, Event *e)
{
    const EvTbSel *p = event_params(e, EvTbSel);
    hb_gtnap_cualib_vector_selection(p->sel);
    unref(gtvect);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_BIND_VETOR )
{
    TableView *view = (TableView*)hb_parptr(1);
    GtNapVector *gtvect = hb_gtnap_cualib_tableview_vector(view);
    tableview_OnData(view, listener(gtvect, i_OnTableDataVector, GtNapVector));
    tableview_OnSelect(view, listener(gtvect, i_OnVectorSelect, GtNapVector));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_VECTOR_ADD )
{
    TableView *view = (TableView*)hb_parptr(1);
    String *text = hb_gtnap_parstr(2);
    PHB_ITEM codeBlock = hb_param(3, HB_IT_BLOCK);
    uint32_t hotkey_pos = hb_parni(4);
    hb_gtnap_cualib_tableview_vector_add_item(view, text, codeBlock, hotkey_pos);

    // MenuVert *menu = panel_get_data(panel, MenuVert);
    // MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
    // opt->text = text;
    // opt->codeBlock = codeBlock ? hb_itemNew(codeBlock) : NULL;
    // opt->hoykey_pos = (hotkey_pos == 0) ? UINT32_MAX : hotkey_pos - 1;
    // opt->hotkey = ENUM_MAX(vkey_t);
    // opt->hotmodif = UINT32_MAX;
    // log_printf("Added option '%s' to MenuVert", tc(text));
    // if (opt->hoykey_pos != UINT32_MAX)
    // {
    //     uint32_t cp = i_codepoint(tc(opt->text), opt->hoykey_pos);

    //     // ASCII uppercase
    //     if (cp >= 65 && cp <= 90)
    //     {
    //         opt->hotkey = KEY_ASCII_TABLE[cp - 65];
    //         //opt->hotmodif = ekMKEY_SHIFT;
    //         // Hotkeys doesn't use SHIFT
    //         opt->hotmodif = 0;
    //     }

    //     // ASCII lowercase
    //     if (cp >= 97 && cp <= 122)
    //     {
    //         opt->hotkey = KEY_ASCII_TABLE[cp - 97];
    //         opt->hotmodif = 0;
    //     }

    //     // ASCII numbers
    //     if (cp >= 48 && cp <= 57)
    //     {
    //         opt->hotkey = KEY_ASCII_NUMBERS[cp - 48];
    //         opt->hotmodif = 0;
    //     }

    //     if (opt->hotkey != ENUM_MAX(vkey_t))
    //     {
    //         Window *window = hb_gtnap_cualib_current_window();
    //         window_hotkey(window, opt->hotkey, opt->hotmodif, listener(menu, i_OnHotKey, MenuVert));
    //     }
    // }
}

//NAP_TABLEVIEW_CUALIB_VECTOR_ADD(V_TableView, V_Opcoes[N_Cont,_OPCAO_TEXTO_TRATADO], V_Opcoes[N_Cont,_OPCAO_BLOCO_ACAO], V_Opcoes[N_Cont,_OPCAO_COL_DESTAQUE])


/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_COLUMN_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    const char_t *title = hb_gtnap_parText(2);
    PHB_ITEM codeBlock = hb_param(3, HB_IT_BLOCK);
    uint32_t width = hb_parni(4);
    hb_gtnap_cualib_tableview_area_add_column(view, title, FALSE, width, codeBlock);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_COLUMN_VECTOR )
{
    TableView *view = (TableView*)hb_parptr(1);
    //const char_t *title = hb_gtnap_parText(2);
    PHB_ITEM codeBlock = hb_param(2, HB_IT_BLOCK);
    uint32_t width = hb_parni(3);
    hb_gtnap_cualib_tableview_vector_add_column(view/*, title, FALSE*/, width, codeBlock);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_UPDATE )
{
    TableView *view = (TableView*)hb_parptr(1);
    //const ArrSt(uint32_t) *sel = NULL;
    tableview_update(view);
    // sel = tableview_selected(view);
    // if (arrst_size(sel, uint32_t) == 0)
    // {
    //     uint32_t row = 0;
    //     tableview_select(view, &row, 1);
    // }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SELECT )
{
    TableView *view = (TableView*)hb_parptr(1);
    ArrSt(uint32_t) *rows = arrst_create(uint32_t);

    if (HB_ISNUM(2))
    {
        uint32_t row = hb_parni(2) - 1;
        arrst_append(rows, row, uint32_t);
    }
    else if (HB_ISARRAY(2))
    {
        uint32_t i, n = hb_parinfa(2, 0);
        for (i = 0; i < n; ++i)
        {
            uint32_t row = hb_parvni(2, i + 1) - 1;
            log_printf("Added %d row to selection", row);
            arrst_append(rows, row, uint32_t);
        }
    }

    tableview_select(view, arrst_all_const(rows, uint32_t), arrst_size(rows, uint32_t));
    arrst_destroy(&rows, NULL, uint32_t);

    {
        const ArrSt(uint32_t) *sel = tableview_selected(view);
        log_printf("There are %d selected rows", arrst_size(sel, uint32_t));
    }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_DESELECT )
{
    TableView *view = (TableView*)hb_parptr(1);
    ArrSt(uint32_t) *rows = arrst_create(uint32_t);

    if (HB_ISNUM(2))
    {
        uint32_t row = hb_parni(2) - 1;
        arrst_append(rows, row, uint32_t);
    }
    else if (HB_ISARRAY(2))
    {
        uint32_t i, n = hb_parinfa(2, 0);
        for (i = 0; i < n; ++i)
        {
            uint32_t row = hb_parvni(2, i + 1) - 1;
            arrst_append(rows, row, uint32_t);
        }
    }

    tableview_deselect(view, arrst_all_const(rows, uint32_t), arrst_size(rows, uint32_t));
    arrst_destroy(&rows, NULL, uint32_t);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_DESELECT_ALL )
{
    TableView *view = (TableView*)hb_parptr(1);
    tableview_deselect_all(view);
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

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_REFRESH_ALL )
{
    hb_gtnap_cualib_tableview_refresh_all();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_REFRESH_CURRENT )
{
    hb_gtnap_cualib_tableview_refresh_current();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_SINGLE_ROW_SELECTED )
{
    uint32_t recno = hb_gtnap_cualib_tableview_select_single_row();
    hb_retni(recno);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_SELECT_MULTIPLE_ROW )
{
    ArrSt(uint32_t) *recs = hb_gtnap_cualib_tableview_select_multiple_row();
    uint32_t n = arrst_size(recs, uint32_t);
    hb_reta(n);
    arrst_foreach_const(v, recs, uint32_t)
        hb_storvnl((long)(*v), -1, v_i + 1);
    arrst_end();
    arrst_destroy(&recs, NULL, uint32_t);
}

/*---------------------------------------------------------------------------*/

// HB_FUNC( NAP_TABLEVIEW_CUALIB_ON_SELECT_CHANGE )
// {
//     hb_gtnap_cualib_tableview_OnSelect(1);
// }


/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CUALIB_ON_SINGLE_SELECT_CHANGE )
{
    hb_gtnap_cualib_tableview_On_Single_Select_Change();
}
