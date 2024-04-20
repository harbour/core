/* Debugger library */

#ifndef __DEBLIB_HXX__
#define __DEBLIB_HXX__

#include <core/core.hxx>

#define MAX_UTF8_SIZE   2048

typedef struct _debmsg_t DebMsg;

typedef enum _msg_type_t
{
    ekMSG_SET_SIZE = 10000,
    ekMSG_SCROLL,
    ekMSG_BOX,
    ekMSG_CURSOR,
    ekMSG_SET_POS,
    ekMSG_PUTCHAR,
    ekMSG_PUTTEXT
} msg_type_t;

typedef enum _cursor_t
{
    ekCURSOR_NONE = 1,
    ekCURSOR_NORMAL,
    ekCURSOR_INSERT,
    ekCURSOR_SPECIAL1,
    ekCURSOR_SPECIAL2
} cursor_t;

struct _debmsg_t
{
    msg_type_t type;
    uint32_t top;
    uint32_t left;
    uint32_t bottom;
    uint32_t right;
    uint32_t row;
    uint32_t col;
    uint32_t color;
    byte_t attrib;
    char_t utf8[MAX_UTF8_SIZE];
};

#endif
