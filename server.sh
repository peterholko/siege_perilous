#!/bin/bash
erl -sname sp -pz ebin deps/log4erl/ebin deps/map_port/ebin -s server start
