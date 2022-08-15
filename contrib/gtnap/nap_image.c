/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGE_FROM_FILE )
{
    const char_t *pathname = hb_get_nap_text(1);
    String *cpath = str_cpath("%s", pathname);
    ferror_t error;
    Image *image = image_from_file(tc(cpath), &error);
    // log_printf("PathName: %s", tc(cpath));
    // log_printf("Image: %p", image);
    str_destroy(&cpath);
    hb_retImage(image);
    //hb_retptr(image);
}
