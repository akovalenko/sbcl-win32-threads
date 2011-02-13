#ifndef FPU_X87_INCLUDED
#define FPU_X87_INCLUDED

typedef struct memfloat80 {
    unsigned char bytes[10];
} memfloat80;

typedef struct x87_env {
    unsigned short control, _padcontrol;
    unsigned short status, _padstatus;
    unsigned short tags, _padtags;
    unsigned long eip;
    unsigned short cs, _padcs;
    unsigned long data;
    unsigned short ds, _padds;
} x87_env;



typedef struct x87_full_state {
    x87_env env;
    memfloat80 regs[8];
} x87_full_state;


/* ------------------------------------------------------------ */


#define _Frob_x87_get(op,type)                  \
    static inline type x87_##op()               \
    {                                           \
        __typeof__(type) result;                \
        asm(#op " %0":"=m"(result));            \
        return result;                          \
    }

_Frob_x87_get(fstcw,unsigned short)

_Frob_x87_get(fnstcw,unsigned short)

_Frob_x87_get(fnstenv,x87_env)

_Frob_x87_get(fstenv,x87_env)

_Frob_x87_get(fnsave,x87_full_state)

_Frob_x87_get(fsave,x87_full_state)

#undef _Frob_x87_get

#define _Frob_x87_set(op,type)                                  \
    static inline void x87_##op(__typeof__(type) new_value)     \
    { asm(#op " %0": :"m"(new_value)); }                        \


_Frob_x87_set(fldcw, unsigned short)

_Frob_x87_set(frstor, x87_full_state)

_Frob_x87_set(fldenv, x87_env)

#undef _Frob_x87_set

#define _Frob_x87(op) static inline void x87_##op() { asm(#op); }

_Frob_x87(fnclex)
_Frob_x87(fclex)
_Frob_x87(fninit)
_Frob_x87(finit)
_Frob_x87(fwait)
_Frob_x87(fincstp)
_Frob_x87(fdecstp)

#undef _Frob_x87

#endif  /* FPU_X87_INCLUDED */
