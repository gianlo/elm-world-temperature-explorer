#!/bin/sh

SOURCE=$1
TARGET=$2

echo Building \'$SOURCE\' to \'$TARGET\'
elm make $SOURCE --optimize --output=$TARGET
echo Uglifying \'$TARGET\'
uglifyjs $TARGET --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$TARGET && uglifyjs $TARGET --mangle --output=$TARGET
echo Gzipping \'$TARGET\'
gzip $TARGET
mv ${TARGET}.gz $TARGET

