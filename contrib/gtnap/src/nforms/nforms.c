/* NAppGUI forms */

#include "nforms.h"
#include <nflib/nflib.h>
#include <osbs/log.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>

static uint32_t i_NUM_USERS = 0;

/*---------------------------------------------------------------------------*/

static void i_nforms_atexit(void)
{
    if (i_NUM_USERS != 0)
        log_printf("Error! nforms is not properly closed (%d)\n", i_NUM_USERS);
}

/*---------------------------------------------------------------------------*/

void nforms_start(void)
{
    if (i_NUM_USERS == 0)
    {
        nflib_start();
        blib_atexit(i_nforms_atexit);
    }

    i_NUM_USERS += 1;
}

/*---------------------------------------------------------------------------*/

void nforms_finish(void)
{
    cassert(i_NUM_USERS > 0);
    if (i_NUM_USERS == 1)
    {
        nflib_finish();
    }

    i_NUM_USERS -= 1;
}
