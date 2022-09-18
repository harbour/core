/*
    This is part of gtnap
    TODO: More info
*/

// NAppGUI-Layout wrapper for Harbour
// https://nappgui.com/en/gui/layout.html
#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_CREATE )
{
    uint32_t nCols = hb_parni(1);
    uint32_t nRows = hb_parni(2);
    Layout *layout = layout_create(nCols, nRows);
    hb_retptr(layout);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_CELL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Cell *cell = layout_cell(layout, col, row);
    hb_retptr(cell);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LABEL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Label *label = (Label*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_label(layout, label, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_BUTTON )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Button *button = (Button*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_button(layout, button, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_POPUP )
{
    Layout *layout = (Layout*)hb_parptr(1);
    PopUp *popup = (PopUp*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_popup(layout, popup, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_EDIT )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Edit *edit = (Edit*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_edit(layout, edit, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_COMBO )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Combo *combo = (Combo*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_combo(layout, combo, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LISTBOX )
{
    Layout *layout = (Layout*)hb_parptr(1);
    ListBox *list = (ListBox*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_listbox(layout, list, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_UPDOWN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    UpDown *updown = (UpDown*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_updown(layout, updown, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SLIDER )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Slider *slider = (Slider*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_slider(layout, slider, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PROGRESS )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Progress *progress = (Progress*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_progress(layout, progress, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    View *view = (View*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_view(layout, view, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TEXTVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    TextView *view = (TextView*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_textview(layout, view, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_IMAGEVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    ImageView *view = (ImageView*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_imageview(layout, view, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABLEVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    TableView *view = (TableView*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_tableview(layout, view, col, row);
    layout_tabstop(layout, col, row, TRUE);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SPLITVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    SplitView *view = (SplitView*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_splitview(layout, view, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PANEL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Panel *panel = (Panel*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_panel(layout, panel, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_LAYOUT )
{
    Layout *layout = (Layout*)hb_parptr(1);
    Layout *sublayout = (Layout*)hb_parptr(2);
    uint32_t col = hb_parni(3);
    uint32_t row = hb_parni(4);
    layout_layout(layout, sublayout, col, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_LABEL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Label *label = layout_get_label(layout, col, row);
    hb_retptr(label);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_BUTTON )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Button *button = layout_get_button(layout, col, row);
    hb_retptr(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_POPUP )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    PopUp *popup = layout_get_popup(layout, col, row);
    hb_retptr(popup);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_EDIT )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Edit *edit = layout_get_edit(layout, col, row);
    hb_retptr(edit);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_COMBO )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Combo *combo = layout_get_combo(layout, col, row);
    hb_retptr(combo);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_LISTBOX )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    ListBox *list = layout_get_listbox(layout, col, row);
    hb_retptr(list);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_UPDOWN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    UpDown *updown = layout_get_updown(layout, col, row);
    hb_retptr(updown);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_SLIDER )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Slider *slider = layout_get_slider(layout, col, row);
    hb_retptr(slider);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_PROGRESS )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Progress *progress = layout_get_progress(layout, col, row);
    hb_retptr(progress);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_VIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    View *view = layout_get_view(layout, col, row);
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_TEXTVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    TextView *view = layout_get_textview(layout, col, row);
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_IMAGEVIEW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    ImageView *view = layout_get_imageview(layout, col, row);
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_GET_PANEL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    Panel *panel = layout_get_panel(layout, col, row);
    hb_retptr(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABORDER )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t order = (orient_t)hb_parni(2);
    layout_taborder(layout, order);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_TABSTOP )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    bool_t tabstop = (bool_t)hb_parl(4);
    layout_tabstop(layout, col, row, tabstop);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HSIZE )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    real32_t width = (real32_t)hb_parnd(3);
    layout_hsize(layout, col, width);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VSIZE )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row = hb_parni(2);
    real32_t height = (real32_t)hb_parnd(3);
    layout_vsize(layout, row, height);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HMARGIN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_hmargin(layout, col, margin);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VMARGIN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row = hb_parni(2);
    real32_t margin = (real32_t)hb_parnd(3);
    layout_vmargin(layout, row, margin);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    layout_hexpand(layout, col);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND2 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col1 = hb_parni(2);
    uint32_t col2 = hb_parni(3);
    real32_t exp = (real32_t)hb_parnd(4);
    layout_hexpand2(layout, col1, col2, exp);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HEXPAND3 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col1 = hb_parni(2);
    uint32_t col2 = hb_parni(3);
    uint32_t col3 = hb_parni(4);
    real32_t exp1 = (real32_t)hb_parnd(5);
    real32_t exp2 = (real32_t)hb_parnd(6);
    layout_hexpand3(layout, col1, col2, col3, exp1, exp2);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row = hb_parni(2);
    layout_vexpand(layout, row);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND2 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row1 = hb_parni(2);
    uint32_t row2 = hb_parni(3);
    real32_t exp = (real32_t)hb_parnd(4);
    layout_vexpand2(layout, row1, row2, exp);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VEXPAND3 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row1 = hb_parni(2);
    uint32_t row2 = hb_parni(3);
    uint32_t row3 = hb_parni(4);
    real32_t exp1 = (real32_t)hb_parnd(5);
    real32_t exp2 = (real32_t)hb_parnd(6);
    layout_vexpand3(layout, row1, row2, row3, exp1, exp2);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_HALIGN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    align_t align = (align_t)hb_parni(4);
    layout_halign(layout, col, row, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_VALIGN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    align_t align = (align_t)hb_parni(4);
    layout_valign(layout, col, row, align);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SHOW_COL )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    bool_t visible = (bool_t)hb_parl(3);
    layout_show_col(layout, col, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SHOW_ROW )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t row = hb_parni(2);
    bool_t visible = (bool_t)hb_parl(3);
    layout_show_row(layout, row, visible);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN )
{
    Layout *layout = (Layout*)hb_parptr(1);
    real32_t mall = (real32_t)hb_parnd(2);
    layout_margin(layout, mall);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN2 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    real32_t mtb = (real32_t)hb_parnd(2);
    real32_t mlr = (real32_t)hb_parnd(3);
    layout_margin2(layout, mtb, mlr);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_MARGIN4 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    real32_t mt = (real32_t)hb_parnd(2);
    real32_t mr = (real32_t)hb_parnd(3);
    real32_t mb = (real32_t)hb_parnd(4);
    real32_t ml = (real32_t)hb_parnd(5);
    layout_margin4(layout, mt, mr, mb, ml);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PADDING )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    real32_t pall = (real32_t)hb_parnd(4);
    Cell *cell = layout_cell(layout, col, row);
    cell_padding(cell, pall);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_PADDING4 )
{
    Layout *layout = (Layout*)hb_parptr(1);
    uint32_t col = hb_parni(2);
    uint32_t row = hb_parni(3);
    real32_t pt = (real32_t)hb_parnd(4);
    real32_t pr = (real32_t)hb_parnd(5);
    real32_t pb = (real32_t)hb_parnd(6);
    real32_t pl = (real32_t)hb_parnd(7);
    Cell *cell = layout_cell(layout, col, row);
    cell_padding4(cell, pt, pr, pb, pl);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_BGCOLOR )
{
    Layout *layout = (Layout*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    layout_bgcolor(layout, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_SKCOLOR )
{
    Layout *layout = (Layout*)hb_parptr(1);
    color_t color = (color_t)hb_parni(2);
    layout_skcolor(layout, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LAYOUT_UPDATE )
{
    Layout *layout = (Layout*)hb_parptr(1);
    layout_update(layout);
}

/*---------------------------------------------------------------------------*/
