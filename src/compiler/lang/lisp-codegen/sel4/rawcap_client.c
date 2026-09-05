/* CLIENT: drives the seL4 endpoint capability with a RAW seL4_Call -- no
 * microkit_ppcall.  This is rosette issuing a seL4 invocation on a capability. */
#include "rosette_sel4.h"
#define CH 0
static void put_int(int v){char b[16];int i=0;if(v<0){microkit_dbg_putc('-');v=-v;}
    if(v==0){microkit_dbg_putc('0');return;}while(v>0){b[i++]='0'+(v%10);v/=10;}while(i>0)microkit_dbg_putc(b[--i]);}
void init(void){
    microkit_dbg_puts("rawclient: seL4_Call on the endpoint cap DIRECTLY (no microkit_ppcall)\n");
    seL4_Word r = rosette_cap_call1(CH, 9);
    microkit_dbg_puts("rawclient: raw cap invocation returned 9*9 = "); put_int((int)r);
    microkit_dbg_puts("  (expect 81)\n");
}
void notified(microkit_channel ch){(void)ch;}
microkit_msginfo protected(microkit_channel ch, microkit_msginfo mi){(void)ch;return mi;}
