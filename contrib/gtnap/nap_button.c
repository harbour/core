/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_PUSH )
{
    Button *button = button_push();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_CHECK )
{
    Button *button = button_check();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_CHECK3 )
{
    Button *button = button_check3();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_RADIO )
{
    Button *button = button_radio();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_FLAT )
{
    Button *button = button_flat();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_FLATGLE )
{
    Button *button = button_flatgle();
    HB_RETHANDLE(button);
}

/*---------------------------------------------------------------------------*/

static void i_OnButtonClick(void *idp, Event *e)
{
    hb_gt_nap_callback(idp, e);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_ONCLICK )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    button_OnClick(button, hb_gt_nap_listener(2, i_OnButtonClick));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_TEXT )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    button_text(button, hb_get_nap_text(2));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_TEXT_ALT )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    button_text_alt(button, hb_get_nap_text(2));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_TOOLTIP )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    button_tooltip(button, hb_get_nap_text(2));
}

// Images & Fonts not supported yet!

/*---------------------------------------------------------------------------*/

// void button_font(Button *button, const Font *font);

/*---------------------------------------------------------------------------*/

// void button_image(Button *button, const Image *image);

/*---------------------------------------------------------------------------*/

// void button_image_alt(Button *button, const Image *image);

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_STATE )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    state_t state = (state_t)hb_parni(2);
    button_state(button, state);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_BUTTON_GET_STATE )
{
    Button *button = (Button*)HB_PARHANDLE(1);
    state_t state = button_get_state(button);
    hb_retni((int)state);
}

/*---------------------------------------------------------------------------*/
