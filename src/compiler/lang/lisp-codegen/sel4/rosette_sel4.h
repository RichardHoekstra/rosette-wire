/* rosette_sel4.h -- the seed of a raw rosette-sel4-cap layer: invoke seL4 capabilities
 * DIRECTLY via libsel4 syscalls, bypassing Microkit's ppcall/notify wrappers.
 * (microkit_ppcall is just seL4_Call(BASE_ENDPOINT_CAP + ch, ...); we call the
 * capability ourselves with our own message-register marshalling.) */
#include <stdint.h>
#include <microkit.h>          /* brings in <sel4/sel4.h> and BASE_ENDPOINT_CAP */

/* one-argument, one-result synchronous call straight to the seL4 endpoint cap */
static inline seL4_Word rosette_cap_call1(microkit_channel ch, seL4_Word arg)
{
    seL4_SetMR(0, arg);
    seL4_Call(BASE_ENDPOINT_CAP + ch, seL4_MessageInfo_new(0, 0, 0, 1));
    return seL4_GetMR(0);
}
