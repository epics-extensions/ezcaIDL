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
#If epics/extensions/configure directory exists, build with it.
#Otherwise use epics/extensions/config.
ifeq (0, $(words $(notdir $(wildcard $(TOP)/configure))))
include $(TOP)/config/CONFIG_EXTENSIONS
include $(TOP)/config/RULES_ARCHS
else
include $(TOP)/configure/CONFIG

HTMLS = ezcaIDLGuide.html ezcaIDLRef.html

SCRIPTS_HOST = ezcaIDL.pro ezcaIDLWidgets.pro

SHARED_LIBRARIES=YES
LOADABLE_LIBRARY_HOST_Darwin = ezcaIDL
LIBRARY_HOST_DEFAULT = ezcaIDL
LIBRARY_HOST_Darwin = -nil-
ezcaIDL_SRCS = ezcaIDL.c
ezcaIDL_LIBS += ezca EzcaScan ca Com

include $(TOP)/configure/RULES
endif
