EPICS_BASE = ---.base
CC_INCLUDES = ([], [$(EPICS_BASE).include], [--.include])
.ifdef __AXP__
CC_OPTS = /warn=disable=(IMPLICITFUNC, CVTDIFTYPES)/float=d_float
ARCH = axp
.else
CC_OPTS =
ARCH = vax
.endif
LINK_OPTS = $(ARCH)_options/opt
BASE_LIB = [$(EPICS_BASE).lib.$(ARCH)_vms]
EXT_LIB = [--.lib.$(ARCH)_vms]
BIN_DIR = [--.bin.$(ARCH)_vms]
LL1 = $(EXT_LIB)ezca/lib
LL2 = $(BASE_LIB)libca/lib
LL3 = $(BASE_LIB)libcom/lib


all : $(BIN_DIR)ezcaidl.exe
ezcaidl.obj : ezcaidl.c
   cc /include=$(CC_INCLUDES) /define=IDL $(CC_OPTS) $(MMS$SOURCE)

$(BIN_DIR)ezcaidl.exe : ezcaidl.obj, $(ARCH)_options.opt
    link /share /exe=$(MMS$TARGET) ezcaidl.obj, -
           $(LINK_OPTS),$(LL1),$(LL2),$(LL3)
