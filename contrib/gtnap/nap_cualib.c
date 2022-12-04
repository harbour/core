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
    uint32_t N_LinIni = hb_parni(1);
    uint32_t N_ColIni = hb_parni(2);
    uint32_t N_LinFin = hb_parni(3);
    uint32_t N_ColFin = hb_parni(4);
    String *C_Cabec = hb_gtnap_cualib_parText(5);
    bool_t close_return = (bool_t)hb_parl(6);
    bool_t close_esc = (bool_t)hb_parl(7);
    bool_t minimize_button = (bool_t)hb_parl(8);
    uint32_t id = hb_gtnap_cualib_window(N_LinIni, N_ColIni, N_LinFin, N_ColFin, tc(C_Cabec), close_return, close_esc, minimize_button);
    str_destroy(&C_Cabec);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_LAUNCH_MODAL )
{
    uint32_t ret = hb_gtnap_cualib_launch_modal();
    hb_retni(ret);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_DESTROY_WINDOW )
{
    hb_gtnap_cualib_destroy_window();
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_IMAGE )
{
    const char_t *pathname = hb_gtnap_parText(1);
    uint32_t nTop = hb_parni(2);
    uint32_t nLeft = hb_parni(3);
    uint32_t nBottom = hb_parni(4);
    uint32_t nRight = hb_parni(5);
    hb_gtnap_cualib_image(pathname, nTop, nLeft, nBottom, nRight);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_BUTTON )
{
    const char_t *text = hb_gtnap_parText(1);
    uint32_t nTop = hb_parni(2);
    uint32_t nLeft = hb_parni(3);
    uint32_t nBottom = hb_parni(4);
    uint32_t nRight = hb_parni(5);
    hb_gtnap_cualib_button(text, nTop, nLeft, nBottom, nRight);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_LABEL )
{
    uint32_t nLin = hb_parni(1);
    uint32_t nCol = hb_parni(2);
    const char_t *text = hb_gtnap_parText(3);
    hb_gtnap_cualib_label(text, nLin, nCol);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_HOTKEY )
{
    int32_t key = hb_parni(1);
    hb_gtnap_cualib_hotkey(key, 2);
}

