# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Use the same settings for compiling releases as well as for testing
ERLC_OPTS= $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)

PROJECT = sp
DEPS = lager cowboy jiffy  
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_jiffy = git https://github.com/davisp/jiffy master

include erlang.mk
