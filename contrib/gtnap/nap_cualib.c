/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_CUALIB_SETUP )
{
    const char_t *title = hb_gtnap_cualib_parText(1);
    uint32_t qt_lin = hb_parni(2);
    uint32_t qt_col = hb_parni(3);
    PHB_ITEM codeBlock_begin = hb_param(4, HB_IT_BLOCK);
    hb_gtnap_cualib_setup(title, qt_lin, qt_col, codeBlock_begin);
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

HB_FUNC( NAP_CUALIB_MODAL_WINDOW )
{
    uint32_t N_LinIni = hb_parni(1);
    uint32_t N_ColIni = hb_parni(2);
    uint32_t N_LinFin = hb_parni(3);
    uint32_t N_ColFin = hb_parni(4);
    const char_t *C_Cabec = hb_gtnap_cualib_parText(5);
    hb_gtnap_cualib_modal_window(N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec);
}