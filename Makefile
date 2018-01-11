PROJECT = torrer
PROJECT_DESCRIPTION = Torrent client
PROJECT_VERSION = 0.1.0

DEPS = lager gproc gun
LOCAL_DEPS = mnesia crypto

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}' +debug_info

ERLC_OPTS += $(ERLC_COMPILE_OPTS)

include erlang.mk
