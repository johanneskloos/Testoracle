#! /bin/sh

cat testoracle.itarget |grep '^[^#].*_unit' |while read test; do ./$test; done
