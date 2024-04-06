/* Debugger library */

#ifndef __DEBLIB_HXX__
#define __DEBLIB_HXX__

#include <core/core.hxx>

typedef struct _debmsg_t DebMsg;

typedef enum _msg_type_t
{
    ekMSG_SET_SIZE = 10000
} msg_type_t;

struct _debmsg_t
{
    msg_type_t type;
    uint32_t p0;
    uint32_t p1;
};

#endif
