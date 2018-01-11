src/gproc.erl:: include/gproc.hrl src/gproc_int.hrl; @touch $@
src/gproc_bcast.erl:: src/gproc_int.hrl; @touch $@
src/gproc_dist.erl:: include/gproc.hrl src/gproc_int.hrl src/gproc_trace.hrl; @touch $@
src/gproc_lib.erl:: include/gproc.hrl src/gproc_int.hrl; @touch $@

COMPILE_FIRST +=
