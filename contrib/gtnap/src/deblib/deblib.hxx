/* Debugger library */

#ifndef __DEBLIB_HXX__
#define __DEBLIB_HXX__

#include <draw2d/draw2d.hxx>

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
    ekMSG_PUTTEXT,
    ekMSG_SAVE,
    ekMSG_REST,
    ekMSG_READ_KEY,
    ekMSG_CLOSE
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
    vkey_t key;
    uint32_t modifiers;
    char_t utf8[MAX_UTF8_SIZE];
};

#define COL_BLACK           0
#define COL_BLUE            1
#define COL_GREEN           2
#define COL_CYAN            3
#define COL_RED             4
#define COL_MAGENTA         5
#define COL_BROWN           6
#define COL_WHITE           7
#define COL_LIGHT_GRAY      8
#define COL_BRIGHT_BLUE     9
#define COL_BRIGHT_GREEN    10
#define COL_BRIGHT_CYAN     11
#define COL_BRIGHT_RED      12
#define COL_BRIGHT_MAGENTA  13
#define COL_YELLOW          14
#define COL_BRIGHT_WHITE    15

#endif
