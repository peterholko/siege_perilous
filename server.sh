#!/bin/bash
erl -sname sp -pz ebin deps/log4erl/ebin deps/jiffy/ebin -s server start
