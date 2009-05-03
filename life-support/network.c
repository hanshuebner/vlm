/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support */

#include "std.h"

#if defined(OS_OSF)
#include "network-osf.c"

#elif defined(OS_LINUX)
#include "network-linux.c"
#include "network-tun-linux.c"

#elif defined(OS_DARWIN)
#include "network-darwin.c"
#endif
