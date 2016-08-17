#! /bin/bash

# Copyright © 2016 by DNV GL SE

# Task  : Call INTEL FORTRAN compiler

# Author: Berthold Höllmann <berthold.hoellmann@dnvgl.com>

# ID: $Id$
author="$Author$"
date="$Date$"
version="$Revision$"

if [ -f /usr/local/fitools/bin/ifort101 ] ; then

    MKL_DIR=/usr/local/fitools/opt/intel/mkl/10.2
    IFCBASE=/usr/local/fitools/opt/intel/x86_64/fc/10.1.022

    INTEL_LICENSE_FILE=/usr/local/fitools/opt/intel/licenses
    export INTEL_LICENSE_FILE
else
    MKL_DIR=/opt/intel/mkl/10.2.1.017
    IFCBASE=/opt/intel/fce/10.1.022
fi

MKL_SETUP=$MKL_DIR/tools/environment/mklvarsem64t.sh

IFCLIB=$IFCBASE/lib
if [ -f $IFCBASE/bin/ifortvars.sh ] ; then
    . $IFCBASE/bin/ifortvars.sh
fi

if [ -f $MKL_SETUP ] ; then
    . $MKL_SETUP
fi

set -e
ifort "$@"

# Local Variables:
# mode: shell-script
# coding: utf-8
# compile-command: "make -C ../.."
# End:
