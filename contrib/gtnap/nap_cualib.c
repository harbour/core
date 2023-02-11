/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_SETUP )
{
    const char_t *title = hb_gtnap_parText(1);
    uint32_t qt_lin = hb_parni(2);
    uint32_t qt_col = hb_parni(3);
    PHB_ITEM codeBlock_begin = hb_param(4, HB_IT_BLOCK);
    hb_gtnap_cualib_setup(title, qt_lin, qt_col, codeBlock_begin);
    //str_destroy(&title);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_INIT_LOG )
{
    hb_gtnap_cualib_init_log();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( WVW_SETLINESPACING )
{
    // FRAN TODO
//    uint32_t byOldLineSpacing = hb_gtnap_cualib_linespacing();

//    if( ! HB_ISNIL( 1 ) && HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= 40 ) //&& /*nobody is crazy enough to use > 40 */
//        //fmod( hb_parnd( 1 ), 2 ) == 0 )
//        hb_gtnap_cualib_set_linespacing(hb_parni( 1 ));

   hb_retni( 4 );
}

/*---------------------------------------------------------------------------*/

HB_FUNC( WVW_SETDEFLINESPACING )
{
   uint32_t byOldLineSpacing = hb_gtnap_cualib_linespacing();

   if( ! HB_ISNIL( 1 ) && HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= 40 ) //&& /*nobody is crazy enough to use > 40 */
       //fmod( hb_parnd( 1 ), 2 ) == 0 )
       hb_gtnap_cualib_set_linespacing(hb_parni( 1 ));

   hb_retni( byOldLineSpacing );
}

/*---------------------------------------------------------------------------*/

HB_FUNC( WVW_SETMAINCOORD )
{
    // FRAN TODO
//    BOOL bOldMainCoordMode = s_pWvwData->s_bMainCoordMode;

//    if( ! HB_ISNIL( 1 ) )
//    {
//       s_pWvwData->s_bMainCoordMode = hb_parl( 1 );

//       if( ! s_pWvwData->s_bMainCoordMode )
//          hb_gt_wvwSetCurWindow( s_pWvwData->s_usNumWindows - 1 );
//       else
//          hb_gt_wvwSetCurWindow( 0 );
//    }

//    hb_retl( bOldMainCoordMode );

   hb_retni( TRUE );
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW )
{
    int32_t N_LinIni = hb_parni(1);
    int32_t N_ColIni = hb_parni(2);
    int32_t N_LinFin = hb_parni(3);
    int32_t N_ColFin = hb_parni(4);
    String *C_Cabec = hb_gtnap_cualib_parText(5);
    bool_t close_return = (bool_t)hb_parl(6);
    bool_t close_esc = (bool_t)hb_parl(7);
    bool_t minimize_button = (bool_t)hb_parl(8);
    uint32_t id = hb_gtnap_cualib_window(N_LinIni, N_ColIni, N_LinFin, N_ColFin, tc(C_Cabec), close_return, close_esc, minimize_button);
    str_destroy(&C_Cabec);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_ENTER_TABSTOP )
{
    hb_gtnap_cualib_window_enter_tabstop();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_ARROWS_TABSTOP )
{
    hb_gtnap_cualib_window_arrows_tabstop();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_STOPS_LAST_EDIT )
{
    hb_gtnap_cualib_window_stops_last_edit();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_WINDOW_SCROLL_PANEL )
{
    int32_t N_LinIni = hb_parni(1);
    int32_t N_ColIni = hb_parni(2);
    int32_t N_LinFin = hb_parni(3);
    int32_t N_ColFin = hb_parni(4);
    hb_gtnap_cualib_window_scroll_panel(N_LinIni, N_ColIni, N_LinFin, N_ColFin);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_ERROR_DATA )
{
    hb_gtnap_cualib_error_data(1);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_LAUNCH_MODAL )
{
    uint32_t ret = hb_gtnap_cualib_launch_modal(1, 2);
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_DESTROY_WINDOW )
{
    hb_gtnap_cualib_destroy_window();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_MENUVERT )
{
    Panel *panel = (Panel*)hb_parptr(1);
    int32_t nTop = hb_parni(2);
    int32_t nLeft = hb_parni(3);
    int32_t nBottom = hb_parni(4);
    int32_t nRight = hb_parni(5);
    hb_gtnap_cualib_menuvert(panel, nTop, nLeft, nBottom, nRight);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TABLEVIEW )
{
    TableView *view = (TableView*)hb_parptr(1);
    uint32_t nTop = hb_parni(2);
    uint32_t nLeft = hb_parni(3);
    uint32_t nBottom = hb_parni(4);
    uint32_t nRight = hb_parni(5);
    hb_gtnap_cualib_tableview(view, nTop, nLeft, nBottom, nRight);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TEXTVIEW )
{
    TextView *view = (TextView*)hb_parptr(1);
    uint32_t nTop = hb_parni(2);
    uint32_t nLeft = hb_parni(3);
    uint32_t nBottom = hb_parni(4);
    uint32_t nRight = hb_parni(5);
    hb_gtnap_cualib_textview(view, nTop, nLeft, nBottom, nRight);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_IMAGE )
{
    const char_t *pathname = hb_gtnap_parText(1);
    uint32_t nTop = hb_parni(3);
    uint32_t nLeft = hb_parni(4);
    uint32_t nBottom = hb_parni(5);
    uint32_t nRight = hb_parni(6);
    bool_t autoclose = (bool_t)hb_parl(7);
    hb_gtnap_cualib_image(pathname, 2, nTop, nLeft, nBottom, nRight, autoclose);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_BUTTON )
{
    const char_t *text = hb_gtnap_parText(1);
    uint32_t nTag = hb_parni(3);
    int32_t nTop = hb_parni(4);
    int32_t nLeft = hb_parni(5);
    int32_t nBottom = hb_parni(6);
    int32_t nRight = hb_parni(7);
    bool_t autoclose = (bool_t)hb_parl(8);
    hb_gtnap_cualib_button(text, 2, nTag, nTop, nLeft, nBottom, nRight, autoclose);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_LABEL )
{
    uint32_t nLin = hb_parni(1);
    uint32_t nCol = hb_parni(2);
    const char_t *text = hb_gtnap_parText(3);
    bool_t background = (bool_t)hb_parl(4);
    bool_t in_scroll = (bool_t)hb_parl(5);
    hb_gtnap_cualib_label(text, nLin, nCol, background, in_scroll);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_EDIT )
{
    uint32_t nLin = hb_parni(1);
    uint32_t nCol = hb_parni(2);
    uint32_t nSize = hb_parni(3);
    // FRAN!!!! Cache of text data
    String *text = str_c(hb_gtnap_parText(4));
    const char_t *type = hb_gtnap_parText(5);
    bool_t editable = (bool_t)hb_parl(6);
    bool_t in_scroll = (bool_t)hb_parl(7);
    hb_gtnap_cualib_edit(tc(text), nLin, nCol, nSize, type, editable, in_scroll);
    str_destroy(&text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR )
{
    uint32_t nPixelsImage = hb_parni(1);
    hb_gtnap_cualib_toolbar(nPixelsImage);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR_BUTTON )
{
    String *pathname = str_c(hb_gtnap_parText(1));
    String *tooltip = str_c(hb_gtnap_parText(2));
    hb_gtnap_cualib_toolbar_button(tc(pathname), tc(tooltip));
    str_destroy(&pathname);
    str_destroy(&tooltip);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TOOLBAR_SEPARATOR )
{
    hb_gtnap_cualib_toolbar_separator();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_HOTKEY )
{
    int32_t key = hb_parni(1);
    bool_t autoclose = (bool_t)hb_parl(3);
    hb_gtnap_cualib_hotkey(key, 2, autoclose);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_CURRENT_TABLEVIEW )
{
    TableView *view = hb_gtnap_cualib_current_tableview();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_CURRENT_MENUVERT )
{
    Panel *menuvert = hb_gtnap_cualib_current_menuvert();
    hb_retptr(menuvert);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_IS_SELECTED )
{
    bool_t sel = hb_gtnap_cualib_current_row_selected();
    hb_retl(sel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_VETOR_SELECT )
{
    ArrSt(uint32_t) *rows = arrst_create(uint32_t);

    if (HB_ISNUM(1))
    {
        uint32_t row = hb_parni(1) - 1;
        arrst_append(rows, row, uint32_t);
    }
    else if (HB_ISARRAY(1))
    {
        uint32_t i, n = hb_parinfa(1, 0);
        for (i = 0; i < n; ++i)
        {
            uint32_t row = hb_parvni(1, i + 1) - 1;
            log_printf("Added %d row to selection", row);
            arrst_append(rows, row, uint32_t);
        }
    }

    hb_gtnap_cualib_vector_selection(rows);

    arrst_destroy(&rows, NULL, uint32_t);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_TEXTVIEW_WRITE )
{
    TextView *view = (TextView*)hb_parptr(1);
    String *text = hb_gtnap_cualib_parText(2);
    textview_writef(view, tc(text));
    str_destroy(&text);
}
