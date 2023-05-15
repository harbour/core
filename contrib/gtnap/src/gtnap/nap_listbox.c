/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_CREATE )
{
    ListBox *listbox = listbox_create();
    hb_retptr(listbox);
}

/*---------------------------------------------------------------------------*/

//static void i_OnSelect(GtNapCallback *callback, Event *e)
//{
//    hb_gtnap_callback(callback, e);
//}

/*---------------------------------------------------------------------------*/

//HB_FUNC( NAP_LISTBOX_ONSELECT )
//{
//    ListBox *listbox = (ListBox*)hb_parptr(1);
//    Listener *listener = hb_gtnap_comp_listener(2, (GuiComponent*)listbox, i_OnSelect);
//    listbox_OnSelect(listbox, listener);
//}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_SIZE )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    listbox_size(listbox, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_CHECKBOX )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    bool_t show = (bool_t)hb_parl(2);
    listbox_checkbox(listbox, show);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_MULTISEL )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    bool_t multisel = (bool_t)hb_parl(2);
    listbox_multisel(listbox, multisel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_ADD_ELEM )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    //Image *image = hb_gtnap_parImage(3);
    listbox_add_elem(listbox, text, NULL);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_SET_ELEM )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    const char_t *text = hb_gtnap_parText(3);
    Image *image = hb_gtnap_parImage(4);
    listbox_set_elem(listbox, index - 1, text, image);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_CLEAR )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    listbox_clear(listbox);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_COLOR )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    color_t color = (color_t)hb_parni(3);
    listbox_color(listbox, index - 1, color);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_SELECT )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    bool_t select = (bool_t)hb_parl(3);
    listbox_select(listbox, index - 1, select);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_CHECK )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    bool_t check = (bool_t)hb_parl(3);
    listbox_check(listbox, index - 1, check);
    view_update((View*)listbox);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_COUNT )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t n = listbox_count(listbox);
    hb_retni(n);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_TEXT )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    const char_t *text = listbox_text(listbox, index - 1);
    hb_retc(text);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_SELECTED )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    bool_t sel = listbox_selected(listbox, index - 1);
    hb_retl(sel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_LISTBOX_CHECKED )
{
    ListBox *listbox = (ListBox*)hb_parptr(1);
    uint32_t index = hb_parni(2);
    bool_t check = listbox_checked(listbox, index - 1);
    hb_retl(check);
}
