#!/bin/bash

ln -sf build-arc/std.arc.so std.arc.so
echo '"new"' | LD_LIBRARY_PATH=build-arc ./bootstrap.arc.run
ln -sf new/std.arc.so std.arc.so
echo '"new2"' | LD_LIBRARY_PATH=new ./bootstrap.arc.run
ln -sf new2/std.arc.do std.arc.so
