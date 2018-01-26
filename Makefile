PROJECT = torrer
PROJECT_DESCRIPTION = Torrent client
PROJECT_VERSION = 0.1.0

DEPS = lager gproc gun cowboy erlydtl
LOCAL_DEPS = sasl mnesia crypto

dep_cowboy_commit = master

DEP_PLUGINS = cowboy



ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}' +debug_info

ERLC_OPTS += $(ERLC_COMPILE_OPTS)

include erlang.mk
