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

HB_FUNC( NAP_IMAGEVIEW_IMAGE )
{
    ImageView *view = (ImageView*)hb_parptr(1);
    Image *image = hb_parImage(2);//(Image*)hb_parptr(2);
    imageview_image(view, image);
    //image_destroy(&image);
}

/*---------------------------------------------------------------------------*/
