#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Generate Makefile includes from ANSYS default Makefiles for shared
libraries.
"""

from __future__ import (
    division, print_function, absolute_import, unicode_literals)

# Standard libraries.
import os
import re
import sys
import functools
import subprocess

# ID: $Id$"
__date__ = "$Date$"[6:-1]
__version__ = "$Revision$"[10:-1]
__author__ = "`Berthold Höllmann <berthold.hoellmann@dnvgl.com>`__"
__copyright__ = "Copyright © 2005, 2014 by DNV GL SE"


class GenMakeInc(object):
    _notSupp = (None, None, None)

    def __init__(self):
        # map ANSYS version/FORTRAN compiler to be used to actual compiler
        # name
        self.fctable = {
            ('120', 'LINIA32', 'ifort'): ("_ifc101_17", "-parallel"),
            ('120', 'LINX64',  'ifort'): ("_ifc101_17_64", "-parallel"),
            ('121', 'LINIA32', 'ifort'): ("_ifc101_17", "-parallel"),
            ('121', 'LINX64',  'ifort'): ("_ifc101_17_64", "-parallel"),
            ('130', 'LINX64',  'ifort'): ("_ifc111_69_64", "-parallel"),
            ('140', 'LINX64',  'ifort'): ("_ifc111_69_64", "-parallel"),
            ('150', 'LINX64',  'ifort'): ("_ifc121_64", "-parallel"),
            ('161', 'LINX64',  'ifort'): ("_ifc140_64", "-parallel"),
            ('162', 'LINX64',  'ifort'): ("_ifc140_64", "-parallel"),
            ('170', 'LINX64',  'ifort'): ("_ifc150_64", "-parallel"),
            ('171', 'LINX64',  'ifort'): ("_ifc150_64", "-parallel"),
        }

        # map ANSYS version/C compiler to be used to actual compiler name
        self.cctable = {
            ('120', 'LINIA32', 'icc'): ("gcc", ""),
            ('120', 'LINX64',  'icc'): ("gcc", ""),
            ('121', 'LINIA32', 'icc'): ("gcc", ""),
            ('121', 'LINX64',  'icc'): ("gcc", ""),
            ('130', 'LINX64',  'icc'): ("gcc", ""),
            ('140', 'LINX64',  'icc'): ("gcc", ""),
            ('150', 'LINX64',  'icc'): ("gcc", ""),
            ('160', 'LINX64',  'icc'): ("gcc", ""),
            ('161', 'LINX64',  'icc'): ("gcc", ""),
            ('170', 'LINX64',  'icc'): ("gcc", ""),
            ('171', 'LINX64',  'icc'): ("gcc", ""),
        }

        # map ANSYS version/linker to be used to actual linker name
        self.ldtable = {
            ('120', 'LINIA32', 'ld'): ("_ifc101_17", ''),
            ('120', 'LINX64',  'ld'): ("_ifc101_17_64", ''),
            ('121', 'LINIA32', 'ld'): ("_ifc101_17", ''),
            ('121', 'LINX64',  'ld'): ("_ifc101_17_64", ''),
            ('130', 'LINX64',  'ld'): ("_ifc111_69_64", ''),
            ('140', 'LINX64',  'ld'): ("_ifc111_69_64", ''),
            ('150', 'LINX64',  'ld'): ("_ifc121_64", ''),
            ('161', 'LINX64',  'ld'): ("_ifc140_64", ''),
            ('162', 'LINX64',  'ld'): ("_ifc140_64", ''),
            ('170', 'LINX64',  'ld'): ("_ifc150_64", ''),
            ('171', 'LINX64',  'ld'): ("_ifc150_64", ''),
        }

        self.ansys_revn = os.environ.get("ANSYS_REVN", "90")
        self.ansys_sys = subprocess.Popen(
            ". /ansys_inc/v%s/ansys/bin/anssh.ini ; echo $SYS" %
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
        if int(self.ansys_revn) < 90:
            fcline_re = re.compile(
                r"FC = (?P<FC>\w+) (?P<FFLAGS>.+$)")
        else:
            fcline_re = re.compile(
                r"FC = (?P<FC>\w+) \$\{debug\} \$\{ANSYS_INCPATH\} "
                r"(?P<FFLAGS>.+$)")
        ccline_re = re.compile(
            r"CC = (?P<CC>\w+) \$\{debug\} \$\{ANSYS_INCPATH\} "
            r"(?P<CFLAGS>.+$)")
        ldline_re = re.compile(
            r"LN = (?P<LD>\w+) (?P<LDFLAGS>.+$)")
        c_del_re = re.compile(r"(^|\s+)-c($|\s+)")
        ld_del_re = re.compile(r"(^|\s+)-o dummy($|\s+)")
        with open('Makefile') as makefile:

            for l in makefile:
                h = l.split()
                if h:
                    h = h[0]
                if h == "PP":
                    self.CPPFLAGS += " ".join(l.split()[2:])
                elif h == "FC":
                    fcline = fcline_re.match(l)
                    (self.FC, self.FFLAGS) = self.fcfix(fcline.group("FC"))
                    self.FFLAGS = ' '.join((
                        self.FFLAGS, c_del_re.sub(
                            " ", fcline.group("FFLAGS")))).strip()
                elif h == "CC":
                    ccline = ccline_re.match(l)
                    (self.CC, self.CFLAGS) = self.ccfix(ccline.group("CC"))
                    self.CFLAGS += (' ' + c_del_re.sub(
                        " ", ccline.group("CFLAGS"))).strip()
                elif h == "LN":
                    ldline = ldline_re.match(l)
                    (self.LD, self.LDFLAGS) = self.ldfix(ldline.group("LD"))
                    self.LDFLAGS += ' ' + ld_del_re.sub(
                        " ", ldline.group("LDFLAGS")).strip()
                elif h == "IN":
                    self.CPPFLAGS += " " + " ".join(l.split()[2:])

        self.CPPFLAGS = self.CPPFLAGS.strip()

    def fcfix(self, fc):
        return (i.strip()
                for i in self.fctable[(self.ansys_revn, self.ansys_sys, fc)])

    def ccfix(self, cc):
        return (i.strip()
                for i in self.cctable[(self.ansys_revn, self.ansys_sys, cc)])

    def ldfix(self, ld):
        return (i.strip()
                for i in self.ldtable[(self.ansys_revn, self.ansys_sys, ld)])

    def gen_makefile(self):
        elems = {"argv": sys.argv[0],
                 "b_PATH": os.path.abspath(
                     os.path.join(
                         os.curdir, os.path.split(__file__)[0],  "bin"))}
        elems.update(self.__dict__)

        with open("make_{ansys_sys}_ans{ansys_revn}.inc".format(**elems),
                  'w') as makefile:
            makefile.write("""# -*- makefile -*-
# make settings for ANSYS {ansys_revn} extensions on {ansys_sys}
# auto generated by {argv}
CC = {CC}
FC = {b_PATH}/{FC}.sh
LD = {b_PATH}/{LD}.sh
CPPFLAGS += {CPPFLAGS}
CFLAGS   += {CFLAGS}
FFLAGS   += $(CPPFLAGS) {FFLAGS} -module $(DEST)
LDFLAGS  += {LDFLAGS}
""".format(**elems))

    def clean_up(self):
        os.remove('Makefile')

if __name__ == "__main__":
    GenMakeInc()()

# Local Variables:
# mode: python
# compile-command: "make -C .."
# End:
