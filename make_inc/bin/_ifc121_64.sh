#! /bin/bash

# Copyright © 2016 by DNV GL SE

# Task  : Call INTEL FORTRAN compiler

# Author: Berthold Höllmann <berthold.hoellmann@dnvgl.com>

# ID: $Id$
author="$Author$"
date="$Date$"
version="$Revision$"

if [ -d /usr/local/fitools/opt/intel/12.1 ] ; then
    IFC_BASE=/usr/local/fitools/opt/intel/12.1

    INTEL_LICENSE_FILE=/usr/local/fitools/opt/intel/licenses
    export INTEL_LICENSE_FILE
else
    IFC_BASE=/opt/intel/composer_xe_2011_sp1.13.367
fi

set -e
. $IFC_BASE/bin/compilervars.sh intel64
ifort "$@"

# Local Variables:
# mode: shell-script
# coding: utf-8
# compile-command: "make -C ../.."
# End:
