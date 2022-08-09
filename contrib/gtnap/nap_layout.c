/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTCREATE )
{
    uint32_t nCols = hb_parni(1);
    uint32_t nRows = hb_parni(2);
    Layout *layout = layout_create(nCols, nRows);
    HB_RETHANDLE(layout);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTLABEL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Label *label = (Label*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_label(layout, label, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTBUTTON )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Button *button = (Button*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_button(layout, button, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTEDIT )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Edit *edit = (Edit*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_edit(layout, edit, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTLAYOUT )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Layout *sublayout = (Layout*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_layout(layout, sublayout, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTHMARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_hmargin(layout, col, margin);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTVMARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_vmargin(layout, row, margin);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUTMARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    real32_t mall = (real32_t)hb_parnd(2);
    layout_margin(layout, mall);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

