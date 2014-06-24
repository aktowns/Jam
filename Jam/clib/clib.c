#include "clib.h"
#include <uv.h>
#include <stdio.h>

void jam_init(int argc) {
	uv_loop_t *loop = uv_loop_new();
	uv_run(loop, UV_RUN_DEFAULT);
}




//JamObj println(JamObj obj) {
//	puts(obj);
//}