/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_CREATE )
{
    uint32_t nCols = hb_parni(1);
    uint32_t nRows = hb_parni(2);
    Layout *layout = layout_create(nCols, nRows);
    HB_RETHANDLE(layout);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_CELL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Cell *cell = layout_cell(layout, col, row);
    HB_RETHANDLE(cell);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LABEL )
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

HB_FUNC( NAP_LAYOUT_BUTTON )
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

HB_FUNC( NAP_LAYOUT_POPUP )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    PopUp *popup = (PopUp*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_popup(layout, popup, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_EDIT )
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

HB_FUNC( NAP_LAYOUT_COMBO )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Combo *combo = (Combo*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_combo(layout, combo, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LISTBOX )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    ListBox *list = (ListBox*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_listbox(layout, list, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_UPDOWN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    UpDown *updown = (UpDown*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_updown(layout, updown, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SLIDER )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Slider *slider = (Slider*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_slider(layout, slider, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PROGRESS )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Progress *progress = (Progress*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_progress(layout, progress, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    View *view = (View*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_view(layout, view, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TEXTVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    TextView *view = (TextView*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_textview(layout, view, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_IMAGEVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    ImageView *view = (ImageView*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_imageview(layout, view, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABLEVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    TableView *view = (TableView*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_tableview(layout, view, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SPLITVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    SplitView *view = (SplitView*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_splitview(layout, view, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PANEL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    Panel *panel = (Panel*)HB_PARHANDLE(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_panel(layout, panel, col, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LAYOUT )
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

HB_FUNC( NAP_LAYOUT_GET_LABEL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Label *label = layout_get_label(layout, col, row);
    HB_RETHANDLE(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_BUTTON )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Button *button = layout_get_button(layout, col, row);
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_POPUP )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    PopUp *popup = layout_get_popup(layout, col, row);
    HB_RETHANDLE(popup);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_EDIT )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Edit *edit = layout_get_edit(layout, col, row);
    HB_RETHANDLE(edit);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_COMBO )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Combo *combo = layout_get_combo(layout, col, row);
    HB_RETHANDLE(combo);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_LISTBOX )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    ListBox *list = layout_get_listbox(layout, col, row);
    HB_RETHANDLE(list);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_UPDOWN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    UpDown *updown = layout_get_updown(layout, col, row);
    HB_RETHANDLE(updown);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_SLIDER )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Slider *slider = layout_get_slider(layout, col, row);
    HB_RETHANDLE(slider);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_PROGRESS )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Progress *progress = layout_get_progress(layout, col, row);
    HB_RETHANDLE(progress);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_VIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    View *view = layout_get_view(layout, col, row);
    HB_RETHANDLE(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_TEXTVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    TextView *view = layout_get_textview(layout, col, row);
    HB_RETHANDLE(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_IMAGEVIEW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    ImageView *view = layout_get_imageview(layout, col, row);
    HB_RETHANDLE(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_PANEL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Panel *panel = layout_get_panel(layout, col, row);
    HB_RETHANDLE(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABORDER )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t order = (orient_t)hb_parni(2);
    layout_taborder(layout, order);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABSTOP )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    bool_t tabstop = (bool_t)hb_parl(4);
    layout_tabstop(layout, col, row, tabstop);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HSIZE )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    real32_t width = (real32_t)hb_parnd(3);
    layout_hsize(layout, col, width);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VSIZE )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row = hb_parni(2);
    real32_t height = (real32_t)hb_parnd(3);
    layout_vsize(layout, row, height);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HMARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_hmargin(layout, col, margin);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VMARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_vmargin(layout, row, margin);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    layout_hexpand(layout, col);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND2 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col1 = hb_parni(2);
    uint32_t col2 = hb_parni(3);
    real32_t exp = (real32_t)hb_parnd(4);
    layout_hexpand2(layout, col1, col2, exp);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND3 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col1 = hb_parni(2);
    uint32_t col2 = hb_parni(3);
    uint32_t col3 = hb_parni(4);
    real32_t exp1 = (real32_t)hb_parnd(5);
    real32_t exp2 = (real32_t)hb_parnd(6);
    layout_hexpand3(layout, col1, col2, col3, exp1, exp2);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row = hb_parni(2);
    layout_vexpand(layout, row);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND2 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row1 = hb_parni(2);
    uint32_t row2 = hb_parni(3);
    real32_t exp = (real32_t)hb_parnd(4);
    layout_vexpand2(layout, row1, row2, exp);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND3 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row1 = hb_parni(2);
    uint32_t row2 = hb_parni(3);
    uint32_t row3 = hb_parni(4);
    real32_t exp1 = (real32_t)hb_parnd(5);
    real32_t exp2 = (real32_t)hb_parnd(6);
    layout_vexpand3(layout, row1, row2, row3, exp1, exp2);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HALIGN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    align_t align = (align_t)hb_parni(4);
    layout_halign(layout, col, row, align);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VALIGN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    align_t align = (align_t)hb_parni(4);
    layout_valign(layout, col, row, align);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SHOW_COL )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t col = hb_parni(2);
    bool_t visible = (bool_t)hb_parl(3);
    layout_show_col(layout, col, visible);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SHOW_ROW )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    uint32_t row = hb_parni(2);
    bool_t visible = (bool_t)hb_parl(3);
    layout_show_row(layout, row, visible);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    real32_t mall = (real32_t)hb_parnd(2);
    layout_margin(layout, mall);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN2 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    real32_t mtb = (real32_t)hb_parnd(2);
    real32_t mlr = (real32_t)hb_parnd(3);
    layout_margin2(layout, mtb, mlr);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN4 )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    real32_t mt = (real32_t)hb_parnd(2);
    real32_t mr = (real32_t)hb_parnd(3);
    real32_t mb = (real32_t)hb_parnd(4);
    real32_t ml = (real32_t)hb_parnd(5);
    layout_margin4(layout, mt, mr, mb, ml);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_BGCOLOR )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    color_t color = (color_t)hb_parni(2);
    layout_bgcolor(layout, color);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SKCOLOR )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    color_t color = (color_t)hb_parni(2);
    layout_skcolor(layout, color);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_UPDATE )
{
    Layout *layout = (Layout*)HB_PARHANDLE(1);
    layout_update(layout);
    hb_retl(TRUE);
    return;
}

/*---------------------------------------------------------------------------*/
