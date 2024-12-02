/* NAppGUI forms */

#include "napforms.h"
#include <nform/nform.h>
#include <osbs/log.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>

static uint32_t i_NUM_USERS = 0;

/*---------------------------------------------------------------------------*/

static void i_napforms_atexit(void)
{
    if (i_NUM_USERS != 0)
        log_printf("Error! napforms is not properly closed (%d)\n", i_NUM_USERS);
}

/*---------------------------------------------------------------------------*/

void napforms_start(void)
{
    if (i_NUM_USERS == 0)
    {
        nform_start();
        blib_atexit(i_napforms_atexit);
    }

    i_NUM_USERS += 1;
}

/*---------------------------------------------------------------------------*/

void napforms_finish(void)
{
    cassert(i_NUM_USERS > 0);
    if (i_NUM_USERS == 1)
    {
        nform_finish();
    }

    i_NUM_USERS -= 1;
}
