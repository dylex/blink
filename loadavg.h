#ifndef LOADAVG_H
#define LOADAVG_H

typedef unsigned short loadavg_t;
extern loadavg_t Loadavg[3]; /* fixed-point 0.01 */
void loadavg_init();

#endif
