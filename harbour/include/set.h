/*
 * $Id$
 */

#ifndef SET_H_
#define SET_H_

#include <types.h>

HARBOUR HB_SET (void);
HARBOUR HB_SETCENTURY (void);
HARBOUR HB_SETFIXED (void);
void InitializeSets (void);
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
   HB_SET_ALTERNATE   = 1,
   HB_SET_ALTFILE     = 2,
   HB_SET_BELL        = 3,
   HB_SET_CANCEL      = 4,
   HB_SET_COLOR       = 5,
   HB_SET_CONFIRM     = 6,
   HB_SET_CONSOLE     = 7,
   HB_SET_CURSOR      = 8,
   HB_SET_DATEFORMAT  = 9,
   HB_SET_DEBUG       = 10,
   HB_SET_DECIMALS    = 11,
   HB_SET_DEFAULT     = 12,
   HB_SET_DELETED     = 13,
   HB_SET_DELIMCHARS  = 14,
   HB_SET_DELIMITERS  = 15,
   HB_SET_DEVICE      = 16,
   HB_SET_EPOCH       = 17,
   HB_SET_ESCAPE      = 18,
   HB_SET_EVENTMASK   = 19,
   HB_SET_EXACT       = 20,
   HB_SET_EXCLUSIVE   = 21,
   HB_SET_EXIT        = 22,
   HB_SET_EXTRA       = 23,
   HB_SET_EXTRAFILE   = 24,
   HB_SET_FIXED       = 25,
   HB_SET_INSERT      = 26,
   HB_SET_INTENSITY   = 27,
   HB_SET_MARGIN      = 28,
   HB_SET_MCENTER     = 29,
   HB_SET_MESSAGE     = 30,
   HB_SET_PATH        = 31,
   HB_SET_PRINTER     = 32,
   HB_SET_PRINTFILE   = 33,
   HB_SET_SCOREBOARD  = 34,
   HB_SET_SCROLLBREAK = 35,
   HB_SET_SOFTSEEK    = 36,
   HB_SET_TYPEAHEAD   = 37,
   HB_SET_UNIQUE      = 38,
   HB_SET_WRAP        = 39
} HB_set_enum;

typedef struct
{
   BOOL HB_SET_ALTERNATE;   /* Logical */
   char *HB_SET_ALTFILE;    /* Character */
   BOOL HB_SET_BELL;        /* Logical */
   BOOL HB_SET_CANCEL;      /* Logical */
   char *HB_SET_COLOR;      /* Character */
   BOOL HB_SET_CONFIRM;     /* Logical */
   BOOL HB_SET_CONSOLE;     /* Logical */
   HB_cursor_enum  HB_SET_CURSOR;      /* Numeric */
   char *HB_SET_DATEFORMAT; /* Character */
   int  HB_SET_DEBUG;       /* Numeric */
   int  HB_SET_DECIMALS;    /* Numeric */
   char *HB_SET_DEFAULT;    /* Character */
   BOOL HB_SET_DELETED;     /* Logical */
   char *HB_SET_DELIMCHARS; /* Character */
   BOOL HB_SET_DELIMITERS;  /* Logical */
   char * HB_SET_DEVICE;    /* Character */
   int  HB_SET_EPOCH;       /* Numeric */
   BOOL HB_SET_ESCAPE;      /* Logical */
   HB_inkey_enum  HB_SET_EVENTMASK;   /* Numeric */
   BOOL HB_SET_EXACT;       /* Logical */
   BOOL HB_SET_EXCLUSIVE;   /* Logical */
   BOOL HB_SET_EXIT;        /* Logical */
   BOOL HB_SET_EXTRA;       /* Logical */   /* QUESTION: What does this do? */
   char *HB_SET_EXTRAFILE;  /* Character */ /* QUESTION: What does this do? */
   BOOL HB_SET_FIXED;       /* Logical */
   BOOL HB_SET_INSERT;      /* Logical */
   BOOL HB_SET_INTENSITY;   /* Logical */
   int  HB_SET_MARGIN;      /* Numeric */
   BOOL HB_SET_MCENTER;     /* Logical */
   int  HB_SET_MESSAGE;     /* Numeric */
   char *HB_SET_PATH;       /* Character */
   BOOL HB_SET_PRINTER;     /* Logical */
   char *HB_SET_PRINTFILE;  /* Character */
   BOOL HB_SET_SCOREBOARD;  /* Logcial */
   int  HB_SET_SCROLLBREAK; /* Logical */ /* QUESTION: What does this do? */
   BOOL HB_SET_SOFTSEEK;    /* Logical */
   int  HB_SET_TYPEAHEAD;   /* Numeric */
   BOOL HB_SET_UNIQUE;      /* Logical */
   BOOL HB_SET_WRAP;        /* Logical */
}  HB_set_struct;

extern HB_set_struct hb_set;
extern BOOL hb_set_century;
extern BOOL hb_set_fixed;
extern int hb_set_althan;
extern int hb_set_printhan;

#endif  /* SET_H_ */
