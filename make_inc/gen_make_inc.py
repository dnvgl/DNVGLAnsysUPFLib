#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""
Copyright (C) 2005 by Germanischer Lloyd AG

======================================================================
Task      Generate Makefile includes from ANSYS default Makefiles for
          shared libraries.
----------------------------------------------------------------------
Author Berthold Höllmann <hoel@GL-Group.com> Project PyANSYS
======================================================================
"""

#  CVSID: $Id: gen_make_inc.py 414 2012-08-15 13:37:21Z hoel $
__author__ = ("2005 Germanischer Lloyd (author: $Author: hoel $) " +
              "hoel@GL-Group.com")
__date__ = "$Date: 2012-08-15 15:37:21 +0200 (Mi, 15. Aug 2012) $"
__version__ = "$Revision: 414 $"[10:-1]
__package_info__ = """ """

import os
import re
import subprocess
import sys


class GenMakeInc(object):
    _notSupp = (None, None, None)
    _ifc70 = ('false', '')  # ('ifc70', '_ifc')
    _ifc71 = ('ifc71', '-parallel')
    _ifc80 = ('ifort80', '-parallel')
    _ifc81 = ('ifort81', '-parallel')
    _ifc81_64 = ('ifort81', '-parallel')
    _ifc91 = ('ifort91', '-parallel')
    _ifc91_64 = ('ifort91', '-parallel')
    _ifc101_17 = ('INTEL_PATHLVL=17 ifort101', '-parallel')
    _ifc101_17_64 = ('INTEL_PATHLVL=17 ifort101', '-parallel')
    _ifc111_69 = ('INTEL_PATHLVL=69 ifort111', '-parallel')
    _ifc111_69_64 = ('INTEL_PATHLVL=69 ifort111', '-parallel')
    _pgf52 = ('pgf90_52', '-Mextend')
    _pgf60 = ('pgf90_60', '-Mextend')
    _pgf61_5 = ('PGI_VERSION=61 PGI_PATHLVL=5 pgf90', '-Mextend')
    _gcc = ('gcc', '')
    _pgcc52 = ('pgcc_52', '')
    _pgcc61_5 = ('PGI_VERSION=61 PGI_PATHLVL=5 pgcc_61', '')

    # map ANSYS version/FORTRAN compiler to be used to actual compiler
    # name
    fctable = {
        ('70',  'LINUXIA32', 'ifc'): _notSupp,
        # doc says 7.0, but does not work on linking anymore
        ('71',  'LINUXIA32', 'ifc'): _ifc71,
        ('80',  'LINUXIA32', 'ifc'): _ifc71,
        ('81',  'LINUXIA32', 'ifc'): _ifc71,
        ('90',  'LINOP64',   'pgf90'): _pgf52,
        ('90',  'LINUXIA32', 'ifort'): _ifc80,
        ('100', 'LINEM64T',  'ifort'): _ifc81_64,
        ('100', 'LINOP64',   'pgf90'): _pgf52,
        ('100', 'LINUXIA32', 'ifort'): _ifc81,
        ('110', 'LINEM64T',  'ifort'): _ifc91_64,
        ('110', 'LINIA32',   'ifort'): _ifc91,
        ('110', 'LINOP64',   'pgf90'): _pgf61_5,
        ('120', 'LINIA32',   'ifort'): _ifc101_17,
        ('120', 'LINX64',    'ifort'): _ifc101_17_64,
        ('121', 'LINIA32',   'ifort'): _ifc101_17,
        ('121', 'LINX64',    'ifort'): _ifc101_17_64,
        ('130', 'LINIA32',   'ifort'): _ifc111_69,
        ('130', 'LINX64',    'ifort'): _ifc111_69_64,
        }

    # map ANSYS version/C compiler to be used to actual compiler name
    cctable = {
        ('70',  'LINUXIA32', 'icc'): _notSupp,
        ('71',  'LINUXIA32', 'icc'): _gcc,
        ('80',  'LINUXIA32', 'icc'): _gcc,
        ('81',  'LINUXIA32', 'icc'): _gcc,
        ('90',  'LINOP64',   'pgcc'): _pgcc52,
        ('90',  'LINUXIA32', 'icc'): _gcc,
        ('100', 'LINEM64T',  'icc'): _gcc,
        ('100', 'LINOP64',   'pgcc'): _pgcc52,
        ('100', 'LINUXIA32', 'icc'): _gcc,
        ('110', 'LINEM64T',  'icc'): _gcc,
        ('110', 'LINIA32',   'icc'): _gcc,
        ('110', 'LINOP64',   'pgcc'): _pgcc61_5,
        ('120', 'LINIA32',   'icc'): _gcc,
        ('120', 'LINX64',    'icc'): _gcc,
        ('121', 'LINIA32',   'icc'): _gcc,
        ('121', 'LINX64',    'icc'): _gcc,
        ('130', 'LINIA32',   'icc'): _gcc,
        ('130', 'LINX64',    'icc'): _gcc,
        }

    # map ANSYS version/linker to be used to actual linker name
    ldtable = {
        ('70',  'LINUXIA32', 'ld'): (_notSupp[0], ''),
        # doc says 7.0, but does not work on linking anymore
        ('71',  'LINUXIA32', 'ld'): (_ifc71[0], '-Vaxlib'),
        ('80',  'LINUXIA32', 'ld'): (_ifc71[0], '-Vaxlib'),
        ('81',  'LINUXIA32', 'ld'): (_ifc71[0], '-Vaxlib'),
        ('90',  'LINOP64',   'pgf90'): (_pgf52[0], ''),
        ('90',  'LINUXIA32', 'ld'): (_ifc80[0], ''),
        ('100', 'LINEM64T',  'ld'): (_ifc81_64[0], ''),
        ('100', 'LINOP64',   'pgf90'): (_pgf52[0], ''),
        ('100', 'LINUXIA32', 'ld'): (_ifc81[0], ''),
        ('110', 'LINEM64T',  'ld'): (_ifc91_64[0], ''),
        ('110', 'LINIA32',   'ld'): (_ifc91[0], ''),
        ('110', 'LINOP64',   'pgf90'): (_pgf61_5[0], ''),
        ('120', 'LINIA32',   'ld'): (_ifc101_17[0], ''),
        ('120', 'LINX64',    'ld'): (_ifc101_17_64[0], ''),
        ('121', 'LINIA32',   'ld'): (_ifc101_17[0], ''),
        ('121', 'LINX64',    'ld'): (_ifc101_17_64[0], ''),
        ('130', 'LINIA32',   'ld'): (_ifc101_17[0], ''),
        ('130', 'LINX64',    'ld'): (_ifc101_17_64[0], ''),
        }

    def __init__(self):
        self.ansys_revn = os.environ.get("ANSYS_REVN", "90")
        self.ansys_sys = subprocess.Popen(
            "source /ansys_inc/v%s/ansys/bin/anssh.ini ; echo $SYS" %
            (self.ansys_revn,),
            stdout=subprocess.PIPE, shell=True).communicate()[0].strip()

        self.CPPFLAGS = ""

    def __call__(self):
        self.gen_share()
        self.parse_share()
        self.gen_makefile()
        self.clean_up()

    def gen_share(self):
        os.system("/ansys_inc/v%s/ansys/customize/user/gen_share dummy"
                  % (self.ansys_revn,))

    def parse_share(self):
        makefile = open('Makefile')
        c_del = r"(^|\s+)-c($|\s+)"
        ld_del = r"(^|\s+)-o dummy($|\s+)"

        for l in makefile:
            h = l.split()
            if h:
                h = h[0]
            if h == "PP":
                self.CPPFLAGS += " ".join(l.split()[2:])
            elif h == "FC":
                if int(self.ansys_revn) < 90:
                    fcline = re.match(
                        r"FC = (?P<FC>\w+) (?P<FFLAGS>.+$)", l)
                else:
                    fcline = re.match(
                        r"FC = (?P<FC>\w+) \$\{debug\} \$\{ANSYS_INCPATH\} "
                        "(?P<FFLAGS>.+$)", l)
                (self.FC, self.FFLAGS
                 ) = self.fcfix(fcline.group("FC"))
                self.FFLAGS += ' ' + re.sub(c_del, " ",
                                     fcline.group("FFLAGS")).strip()
            elif h == "CC":
                ccline = re.match(
                    r"CC = (?P<CC>\w+) \$\{debug\} \$\{ANSYS_INCPATH\} "
                    "(?P<CFLAGS>.+$)", l)
                (self.CC, self.CFLAGS) = self.ccfix(ccline.group("CC"))
                self.CFLAGS += ' ' + re.sub(c_del, " ",
                                            ccline.group("CFLAGS")).strip()
            elif h == "LN":
                ldline = re.match(
                    r"LN = (?P<LD>\w+) (?P<LDFLAGS>.+$)", l)
                (self.LD, self.LDFLAGS) = self.ldfix(ldline.group("LD"))
                self.LDFLAGS += ' ' + re.sub(ld_del, " ",
                                             ldline.group("LDFLAGS")).strip()
            elif h == "IN":
                self.CPPFLAGS += " " + " ".join(l.split()[2:])

        self.CPPFLAGS = self.CPPFLAGS.strip()

    def fcfix(self, fc):
        return self.fctable[(self.ansys_revn, self.ansys_sys, fc)]

    def ccfix(self, cc):
        return self.cctable[(self.ansys_revn, self.ansys_sys, cc)]

    def ldfix(self, ld):
        return self.ldtable[(self.ansys_revn, self.ansys_sys, ld)]

    def gen_makefile(self):
        makefile = open("make_%s_ans%s.inc" %
                        (self.ansys_sys, self.ansys_revn), 'w')
        makefile.write("""# -*- makefile -*-
# make settings for ANSYS %s extensions on %s
# auto generated by %s
""" % (self.ansys_revn, self.ansys_sys, sys.argv[0]))
        makefile.write("CC = %s\n" % self.CC.strip())
        makefile.write("FC = %s\n" % self.FC.strip())
        makefile.write("LD = %s\n" % self.LD.strip())
        makefile.write("CPPFLAGS += %s\n" % self.CPPFLAGS.strip())
        makefile.write("CFLAGS   += %s\n" % self.CFLAGS.strip())
        makefile.write("FFLAGS   += %s -module $(DEST)\n" % self.FFLAGS.strip())
        makefile.write("FFLAGS   += $(CPPFLAGS)\n")
        makefile.write("LDFLAGS  += %s\n" % self.LDFLAGS.strip())
        makefile.close()

    def clean_up(self):
        os.remove('Makefile')

if __name__ == "__main__":
    GenMakeInc()()

# Local Variables:
# compile-command:"make -C .."
# End:
