/*
 * $Id$
 */

#ifndef _SET_H
#define _SET_H

#include <types.h>

HARBOUR SET (void);
HARBOUR __SETCENTURY (void);
void HB_init_set (void);
void ReleaseSets (void);

typedef enum
{
   SC_NONE = 0,            /* None */
   SC_NORMAL = 1,          /* Underline */
   SC_INSERT = 2,          /* Lower half block */
   SC_SPECIAL1 = 3,        /* Full block */
   SC_SPECIAL2 = 4         /* Upper half block */
} HB_cursor_enum;

typedef enum
{
   INKEY_MOVE = 1,         /* Mouse Events */
   INKEY_LDOWN = 2,        /* Mouse Left Click Down */
   INKEY_LUP = 4,          /* Mouse Left Click Up */
   INKEY_RDOWN = 8,        /* Mouse Right Click Down */
   INKEY_RUP = 16,         /* Mouse Right Click Up */
   INKEY_KEYBOARD = 128,   /* Keyboard Events */
   INKEY_ALL = 159         /* All Mouse and Keyboard Events */
} HB_inkey_enum;

typedef enum
{
   _SET_ALTERNATE   = 1,
   _SET_ALTFILE     = 2,
   _SET_BELL        = 3,
   _SET_CANCEL      = 4,
   _SET_COLOR       = 5,
   _SET_CONFIRM     = 6,
   _SET_CONSOLE     = 7,
   _SET_CURSOR      = 8,
   _SET_DATEFORMAT  = 9,
   _SET_DEBUG       = 10,
   _SET_DECIMALS    = 11,
   _SET_DEFAULT     = 12,
   _SET_DELETED     = 13,
   _SET_DELIMCHARS  = 14,
   _SET_DELIMITERS  = 15,
   _SET_DEVICE      = 16,
   _SET_EPOCH       = 17,
   _SET_ESCAPE      = 18,
   _SET_EVENTMASK   = 19,
   _SET_EXACT       = 20,
   _SET_EXCLUSIVE   = 21,
   _SET_EXIT        = 22,
   _SET_EXTRA       = 23,
   _SET_EXTRAFILE   = 24,
   _SET_FIXED       = 25,
   _SET_INSERT      = 26,
   _SET_INTENSITY   = 27,
   _SET_MARGIN      = 28,
   _SET_MCENTER     = 29,
   _SET_MESSAGE     = 30,
   _SET_PATH        = 31,
   _SET_PRINTER     = 32,
   _SET_PRINTFILE   = 33,
   _SET_SCOREBOARD  = 34,
   _SET_SCROLLBREAK = 35,
   _SET_SOFTSEEK    = 36,
   _SET_TYPEAHEAD   = 37,
   _SET_UNIQUE      = 38,
   _SET_WRAP        = 39
} HB_set_enum;

typedef struct
{
   BOOL _SET_ALTERNATE;   /* Logical */
   char *_SET_ALTFILE;    /* Character */
   BOOL _SET_BELL;        /* Logical */
   BOOL _SET_CANCEL;      /* Logical */
   char *_SET_COLOR;      /* Character */
   BOOL _SET_CONFIRM;     /* Logical */
   BOOL _SET_CONSOLE;     /* Logical */
   HB_cursor_enum  _SET_CURSOR;      /* Numeric */
   char *_SET_DATEFORMAT; /* Character */
   BOOL _SET_DEBUG;       /* Logical */
   int  _SET_DECIMALS;    /* Numeric */
   char *_SET_DEFAULT;    /* Character */
   BOOL _SET_DELETED;     /* Logical */
   char *_SET_DELIMCHARS; /* Character */
   BOOL _SET_DELIMITERS;  /* Logical */
   char * _SET_DEVICE;    /* Character */
   int  _SET_EPOCH;       /* Numeric */
   BOOL _SET_ESCAPE;      /* Logical */
   HB_inkey_enum  _SET_EVENTMASK;   /* Numeric */
   BOOL _SET_EXACT;       /* Logical */
   BOOL _SET_EXCLUSIVE;   /* Logical */
   BOOL _SET_EXIT;        /* Logical */
   BOOL _SET_EXTRA;       /* Logical */   /* QUESTION: What does this do? */
   char *_SET_EXTRAFILE;  /* Character */ /* QUESTION: What does this do? */
   BOOL _SET_FIXED;       /* Logical */
   BOOL _SET_INSERT;      /* Logical */
   BOOL _SET_INTENSITY;   /* Logical */
   int  _SET_MARGIN;      /* Numeric */
   BOOL _SET_MCENTER;     /* Logical */
   int  _SET_MESSAGE;     /* Numeric */
   char *_SET_PATH;       /* Character */
   BOOL _SET_PRINTER;     /* Logical */
   char *_SET_PRINTFILE;  /* Character */
   BOOL _SET_SCOREBOARD;  /* Logcial */
   int  _SET_SCROLLBREAK; /* Logical */ /* QUESTION: What does this do? */
   BOOL _SET_SOFTSEEK;    /* Logical */
   int  _SET_TYPEAHEAD;   /* Numeric */
   BOOL _SET_UNIQUE;      /* Logical */
   BOOL _SET_WRAP;        /* Logical */
}  HB_set_struct;

extern HB_set_struct HB_set;
extern BOOL HB_set_century;

#endif
