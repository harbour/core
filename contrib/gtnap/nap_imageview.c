/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGEVIEW_CREATE )
{
    ImageView *view = imageview_create();
    imageview_scale(view, ekAUTO);
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGEVIEW_SIZE )
{
    ImageView *view = (ImageView*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    imageview_size(view, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGEVIEW_SCALE )
{
    ImageView *view = (ImageView*)hb_parptr(1);
    scale_t scale = (scale_t)hb_parni(2);
    imageview_scale(view, scale);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGEVIEW_IMAGE )
{
    ImageView *view = (ImageView*)hb_parptr(1);
    Image *image = hb_parImage(2);
    imageview_image(view, image);
}

/*---------------------------------------------------------------------------*/
