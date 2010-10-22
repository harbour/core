/* wrap_ow.c */

#include <mem.h>
#include <dos.h>

#include "irqwrap.h"

/*
IRQ wrappers for OpenWatcom
*/

typedef void interrupt (*IRQ_ISR)(void);
extern TIRQWrapper OldIRQVectors[16];

#define IRQWRAP(x)\
void interrupt IRQWrap##x(void)\
{\
  if (IRQHandlers[##x]())\
    ((IRQ_ISR)OldIRQVectors[##x])();\
}

void interrupt IRQWrap(void)
{
}

IRQWRAP(0);
IRQWRAP(1);
IRQWRAP(2);
IRQWRAP(3);
IRQWRAP(4);
IRQWRAP(5);
IRQWRAP(6);
IRQWRAP(7);
IRQWRAP(8);
IRQWRAP(9);
IRQWRAP(10);
IRQWRAP(11);
IRQWRAP(12);
IRQWRAP(13);
IRQWRAP(14);
IRQWRAP(15);

void interrupt IRQWrap_End(void)
{
}

TIRQWrapper IRQWrappers[16] =
{
  (TIRQWrapper)IRQWrap0,
  (TIRQWrapper)IRQWrap1,
  (TIRQWrapper)IRQWrap2,
  (TIRQWrapper)IRQWrap3,
  (TIRQWrapper)IRQWrap4,
  (TIRQWrapper)IRQWrap5,
  (TIRQWrapper)IRQWrap6,
  (TIRQWrapper)IRQWrap7,
  (TIRQWrapper)IRQWrap8,
  (TIRQWrapper)IRQWrap9,
  (TIRQWrapper)IRQWrap10,
  (TIRQWrapper)IRQWrap11,
  (TIRQWrapper)IRQWrap12,
  (TIRQWrapper)IRQWrap13,
  (TIRQWrapper)IRQWrap14,
  (TIRQWrapper)IRQWrap15
};

TIRQHandler IRQHandlers[16] =
{
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};
