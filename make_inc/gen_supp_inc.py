#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""
Copyright (C) 2006 by Germanischer Lloyd AG

======================================================================
Task      Generate list of supported settings for actual architecture.
----------------------------------------------------------------------
Author    Berthold Höllmann <hoel@GL-Group.com>
Project   ans2bmf
======================================================================
"""

#  CVSID: $Id: gen_supp_inc.py 398 2009-08-28 09:02:16Z hoel $
__author__ = ("2006 Germanischer Lloyd (author: $Author: hoel $) " +
              "hoel@GL-Group.com")
__date__ = "$Date: 2009-08-28 11:02:16 +0200 (Fr, 28 Aug 2009) $"
__version__ = "$Revision: 398 $"[10:-1]
__package_info__ = """ """

import os
import subprocess
import sys


class gen_supp_inc(object):
    # matrix of ANSYS version/output file formats supported
    # The meaning of the value is as follows:
    #
    #     0: variant not supported
    #
    #     1: ASCII output supported
    #
    #     2: BMF outout supported
    #
    #     4: HDF output supported
    #
    ASCII = 1
    BMF = 2
    HDF = 4
    formatmap = {
        #None:  'NONE',
        ASCII: 'ASCII',
        BMF:   'BMF',
        HDF:   'HDF',
    }
    # dictionary mapping operating system/platform to default ANSYS
    # version and default output format.
    _archs = (
        (('LINUXIA32'), (100, BMF)),
        (('LINIA32'),   (120, BMF)),
        (('LINOP64'),   (110, ASCII)),
        (('LINEM64T'),  (110, ASCII)),
        (('LINX64'),    (120, ASCII)),
        (('SunOS', 'sun4u'),  (None, None)),
        )
    archs = dict(_archs)
    _outMatrix = {
    #         L          L            L      L      L            S
    #         i          i            i      i      i            u
    #         n          n            n      n      n            n
    #         u          u            u      u      u            O
    #         x          x            x      x      x            S
    #
    #         i          i            x      E      X            s
    #         6          6            8      M      6            u
    #         8          8            6      6      4            n
    #         6          6            _      4                   4
    #         LINUXIA32  LINIA32      6      T                   u
    #         <=100      >=110        4
    #    70: (False,    (False,       False, False, False,       False, ),
    #    71: (ASCII|BMF, False,       False, False, False,       False, ),
    #    80: (ASCII|BMF, False,       False, False, False,       False, ),
    #    81: (ASCII|BMF, False,       False, False, False,       False, ),
    #    90: (ASCII|BMF, False,       ASCII, False, False,       False, ),
        100: (False,     ASCII | BMF, ASCII, ASCII, False,       False, ),
        110: (False,     ASCII | BMF, ASCII, ASCII, False,       False, ),
        120: (False,     ASCII | BMF, False, False, ASCII | BMF, False, ),
        121: (False,     ASCII | BMF, False, False, ASCII | BMF, False, ),
        130: (False,     ASCII | BMF, False, False, ASCII | BMF, False, ),
        }
    outMatrix = {}
    for key, supp in _outMatrix.iteritems():
        outMatrix[key] = dict(zip((a[0] for a in _archs), supp))

    def __init__(self):
        ansys_revn = os.environ.get("ANSYS_REVN", "120")
        self.system = subprocess.Popen(
            "source /ansys_inc/v%s/ansys/bin/anssh.ini ; echo $SYS" %
            (ansys_revn,),
            stdout=subprocess.PIPE, shell=True).communicate()[0].strip()

    def __call__(self):
        self.gen_makefile()

    def get_formats(self, formats):
        """Generate list of supported output file formats supported on
described in the 'formats' argument. 'formats' is an `or`ed
combination of the allowed values 'ASCII', 'BMF', and 'HDF'.

Order of returned values sets preferred output format first.
"""
        res = []
        if formats & self.BMF:
            res.append('BMF')
        if formats & self.HDF:
            res.append('HDF')
        if formats & self.ASCII:
            res.append('ASCII')
        return res

    def gen_makefile(self):
        makefile = open("make_%s.inc" % self.system, 'w')
        makefile.write("""# -*- makefile -*-
# make settings for ANSYS extensions on %s
# auto generated by %s
""" % (self.system, sys.argv[0]))
        # setting default ANSYS version and output format
        makefile.write("""# default ANSYS version and output format
# define ANSYS version to use
ANSMAJOR = %d
ANSMINOR = %d
FORMAT = %s
# define ANSYS version to use
ANSVER = $(ANSMAJOR)$(ANSMINOR)
ANSDVER = $(ANSMAJOR).$(ANSMINOR)
""" % (self.archs[self.system][0] // 10,
       self.archs[self.system][0] % 10,
       self.formatmap[self.archs[self.system][1]]))

        # compiler flags for various architectures
        makefile.write(
            "\ninclude ../make_inc/make_$(SYSTEM)_ans$(ANSVER).inc\n")

        makefile.write("\ncomplete:\n")
        for ansver in self.outMatrix:
            formats = self.get_formats(self.outMatrix[ansver][self.system])
            if formats:
                system = subprocess.Popen(
                    "source /ansys_inc/v%d/ansys/bin/anssh.ini ; echo $SYS" %
                    (ansver,),
                    stdout=subprocess.PIPE,
                    shell=True).communicate()[0].strip()
                for format in formats:
                    makefile.write(
                        ("\tmake ntest ANSMAJOR=%d ANSMINOR=%d FORMAT=%s "
                         "SYSTEM=%s\n") %
                        (ansver // 10, ansver % 10, format, system))

        makefile.write("\ninstall_all:\n")
        for ansver in self.outMatrix:
            formats = self.get_formats(self.outMatrix[ansver][self.system])
            if formats:
                system = subprocess.Popen(
                    "source /ansys_inc/v%d/ansys/bin/anssh.ini ; echo $SYS" %
                    (ansver,),
                    stdout=subprocess.PIPE,
                    shell=True).communicate()[0].strip()
                makefile.write(
                    ("\tmake install ANSMAJOR=%d ANSMINOR=%d FORMAT=%s "
                     "SYSTEM=%s\n") %
                     (ansver // 10, ansver % 10, formats[0], system))
        makefile.write("\tcd ../doc ; make install\n")

        makefile.close()

        return

if __name__ == "__main__":
    gen_supp_inc()()

# Local Variables:
# compile-command:"make -C .."
# End: