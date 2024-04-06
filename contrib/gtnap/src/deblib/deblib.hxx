/* Debugger library */

#ifndef __DEBLIB_HXX__
#define __DEBLIB_HXX__

#include <core/core.hxx>

#define MAX_UTF8_SIZE   2048

typedef struct _debmsg_t DebMsg;

typedef enum _msg_type_t
{
    ekMSG_SET_SIZE = 10000,
    ekMSG_PUTCHAR
} msg_type_t;

struct _debmsg_t
{
    msg_type_t type;
    uint32_t row;
    uint32_t col;
    uint32_t color;
    byte_t attrib;
    char_t utf8[MAX_UTF8_SIZE];    
};

#endif
