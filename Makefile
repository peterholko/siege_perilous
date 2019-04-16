# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Use the same settings for compiling releases as well as for testing
ERLC_OPTS= $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)

PROJECT = sp
DEPS = lager cowboy jsx parsexml pqueue
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_jsx = git https://github.com/talentdeficit/jsx master
dep_parsexml = git https://github.com/maxlapshin/parsexml master
dep_pqueue = git https://github.com/okeuday/pqueue master

include erlang.mk
