#*************************************************************************
# Copyright (c) 2002 The University of Chicago, as Operator of Argonne
# National Laboratory.
# Copyright (c) 2002 The Regents of the University of California, as
# Operator of Los Alamos National Laboratory.
# This file is distributed subject to a Software License Agreement found
# in the file LICENSE that is included with this distribution. 
#*************************************************************************
#
# $Id$
#
TOP = ../..
include $(TOP)/configure/CONFIG

HTMLS = ezcaIDLGuide.html ezcaIDLRef.html

SCRIPTS_HOST = ezcaIDL.pro ezcaIDLWidgets.pro

SHARED_LIBRARIES=YES
LOADABLE_LIBRARY_HOST_Darwin = ezcaIDL
LIBRARY_HOST_DEFAULT = ezcaIDL
LIBRARY_HOST_Darwin = -nil-
ezcaIDL_SRCS = ezcaIDL.c
ezcaIDL_LIBS += ezca EzcaScan ca Com
ezcaIDL_SYS_LIBS_WIN32 = ws2_32 advapi32 user32

include $(TOP)/configure/RULES
