/* irqwrap.h */

/*
How many stacks to allocate for the irq wrappers. You could
probably get away with fewer of these, if you want to save memory and
you are feeling brave...
*/
#define IRQ_STACKS      8

extern void *IRQStacks[IRQ_STACKS];

typedef int (*TIRQHandler)(void);
typedef void (*TIRQWrapper)(void);

extern TIRQHandler IRQHandlers[16];
extern TIRQWrapper IRQWrappers[16];

