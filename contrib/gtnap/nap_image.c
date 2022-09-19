/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_IMAGE_FROM_FILE )
{
    const char_t *pathname = hb_get_nap_text(1);
    String *cpath = str_cpath("%s", pathname);
    Image *image = image_from_file(tc(cpath), NULL);
    str_destroy(&cpath);
    hb_retImage(image);
}
