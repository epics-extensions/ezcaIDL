/*************************************************************************\
* Copyright (c) 2002 The University of Chicago, as Operator of Argonne
* National Laboratory.
* Copyright (c) 2002 The Regents of the University of California, as
* Operator of Los Alamos National Laboratory.
* This file is distributed subject to a Software License Agreement found
* in the file LICENSE that is included with this distribution. 
\*************************************************************************/
#include <tsDefs.h>
#include <cadef.h>
#include <epicsTypes.h>
#include <db_access.h>
#include <ezca.h>

#include <EzcaScan.h>

#define epicsExportSharedSymbols
#include <shareLib.h>

#define OK      0


/*      ezcaIDL.c
*
*       This file is an interface layer between IDL/PV-WAVE and EZCA.  It mainly
*   just converts parameter passing from the IDL/PV-WAVE mechanisms
*   (most parameters passed by reference) to the mechanism required by EZCA.
*   For a few the functions which are not supplied by EZCA this routine actually
*   does the work, using the ezcaPvToChid to fetch the channel descriptor.
*
*       Author: Mark Rivers
*       Date:   June 28, 1995
*
*       Modifications:
*       09-03-98  Mark Rivers.  Made work with WIN32 under IDL 5.1. It will not
*                               work with WIN32 on earlier versions of IDL
*                               because of the way IDL previously passed
*                               strings.
*       09-26-01  Mark Rivers   Changed code to no longer pass strings from IDL
*                               or PV-WAVE, only byte arrays.  This simplifies things
*                               and allows a single DLL to work with all versions
*                               of IDL and PVWAVE.
*/

static char* str_array_addr[1000];
#define BUILD_STR_ARRAY(len, s) \
    for (i=0; i<len; i++) str_array_addr[i]=s + MAX_STRING_SIZE*i
#define STR_ARRAY_ADDR(s) str_array_addr


/* The following macros allow this source file to be used with
   PV-WAVE and IDL on both Unix and VMS platforms. The difference
   is that on VMS platforms arguments are passed directly
   (by reference), while on Unix they are passed by the (argc, argp)
   mechanism. These macros also simplify the code for each routine. */
#if defined (VMS)
#   define WAVE_HEADER0(ftype, fname)\
      ftype fname() {
#   define WAVE_HEADER1(ftype, fname, type1, arg1)\
      ftype fname(type1 *arg1) {
#   define WAVE_HEADER2(ftype, fname, type1, arg1, type2, arg2)\
      ftype fname(type1 *arg1, type2 *arg2) {
#   define WAVE_HEADER3(ftype, fname, type1, arg1, type2, arg2, type3, arg3)\
      ftype fname(type1 *arg1, type2 *arg2, type3 *arg3) {
#   define WAVE_HEADER4(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4)\
      ftype fname(type1 *arg1, type2 *arg2, type3 *arg3, type4 *arg4) {
#   define WAVE_HEADER5(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4, type5, arg5)\
      ftype fname(type1 *arg1, type2 *arg2, type3 *arg3, type4 *arg4, type5 *arg5) {

#else
#   define WAVE_HEADER0(ftype, fname)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {
#   define WAVE_HEADER1(ftype, fname, type1, arg1)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {\
                  type1 *arg1 = (type1 *) argp[0];
#   define WAVE_HEADER2(ftype, fname, type1, arg1, type2, arg2)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {\
                  type1 *arg1 = (type1 *) argp[0];\
                  type2 *arg2 = (type2 *) argp[1];
#   define WAVE_HEADER3(ftype, fname, type1, arg1, type2, arg2, type3, arg3)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {\
                  type1 *arg1 = (type1 *) argp[0];\
                  type2 *arg2 = (type2 *) argp[1];\
                  type3 *arg3 = (type3 *) argp[2];
#   define WAVE_HEADER4(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {\
                  type1 *arg1 = (type1 *) argp[0];\
                  type2 *arg2 = (type2 *) argp[1];\
                  type3 *arg3 = (type3 *) argp[2];\
                  type4 *arg4 = (type4 *) argp[3];
#   define WAVE_HEADER5(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4, type5, arg5)\
          epicsShareFunc ftype epicsShareAPI fname(argc, argp)\
                  int argc;\
                  void *argp[];\
                  {\
                  type1 *arg1 = (type1 *) argp[0];\
                  type2 *arg2 = (type2 *) argp[1];\
                  type3 *arg3 = (type3 *) argp[2];\
                  type4 *arg4 = (type4 *) argp[3];\
                  type5 *arg5 = (type5 *) argp[4];
#endif


WAVE_HEADER4(int, ezcaIDLGet, char, pvname, char, type, int, nelem, void, buff)
   return(ezcaGet(pvname, *type, *nelem, buff));
}

WAVE_HEADER3(int, ezcaIDLGetControlLimits, char, pvname, double, low, double, high)
   return(ezcaGetControlLimits(pvname, low, high));
}

WAVE_HEADER3(int, ezcaIDLGetGraphicLimits, char, pvname, double, low, double, high)
   return(ezcaGetGraphicLimits(pvname, low, high));
}

WAVE_HEADER2(int, ezcaIDLGetPrecision, char, pvname, short, precision)
   return(ezcaGetPrecision(pvname, precision));
}

WAVE_HEADER4(int, ezcaIDLGetStatus, char, pvname, TS_STAMP, timestamp, short, status, short, severity)
   return(ezcaGetStatus(pvname, timestamp, status, severity));
}

WAVE_HEADER2(int, ezcaIDLGetUnits, char, pvname, char, units)
   return(ezcaGetUnits(pvname, units));
}

WAVE_HEADER1(int, ezcaIDLPerror, char, prefix)
   ezcaPerror(prefix);
   return EZCA_OK;
}

WAVE_HEADER2(int, ezcaIDLGetErrorString, char, prefix, char, err_string)
        int status;
        char *buff;

        status = ezcaGetErrorString(prefix, &buff);
        strcpy(err_string, buff);
        ezcaFree((void *)buff);
        return(status);
}

WAVE_HEADER4(int, ezcaIDLPut, char, pvname, char, type, int, nelem, void, buff)
   return(ezcaPut(pvname, *type, *nelem, buff));
}

WAVE_HEADER4(int, ezcaIDLPutOldCa, char, pvname, char, type, int, nelem, void, buff)
   return(ezcaPutOldCa(pvname, *type, *nelem, buff));
}

WAVE_HEADER3(int, ezcaIDLGetCountAndType, char, pvname, int, count, char, type)
   int status;
   chid *chid;

   status = ezcaPvToChid(pvname, &chid);
   if (status != EZCA_OK) return(status);
   *count = ca_element_count(*chid);
   *type = ca_field_type(*chid);
   return EZCA_OK;
}

WAVE_HEADER2(int, ezcaIDLSetMonitor, char, pvname, char, type)
   return(ezcaSetMonitor(pvname, *type));
}

WAVE_HEADER2(int, ezcaIDLClearMonitor, char, pvname, char, type)
   return(ezcaClearMonitor(pvname, *type));
}

WAVE_HEADER2(int, ezcaIDLNewMonitorValue, char, pvname, char, type)
   return(ezcaNewMonitorValue(pvname, *type));
}

WAVE_HEADER1(int, ezcaIDLSetTimeout, float, sec)
   return(ezcaSetTimeout(*sec));
}

WAVE_HEADER1(int, ezcaIDLSetRetryCount, int, retry)
   return(ezcaSetRetryCount(*retry));
}

WAVE_HEADER1(int, ezcaIDLGetTimeout, float, timeout)
   *timeout = ezcaGetTimeout();
   return EZCA_OK;
}

WAVE_HEADER1(int, ezcaIDLGetRetryCount, int, retrycount)
   *retrycount = ezcaGetRetryCount();
   return EZCA_OK;
}

WAVE_HEADER2(int, ezcaIDLEndGroupWithReport, int, nvals, int, status)
   int rc, nrcs;
   int *rcs;
   int i;

   rc = ezcaEndGroupWithReport(&rcs, &nrcs);
   if (nrcs > *nvals) nrcs = *nvals;
   if (*nvals > nrcs) *nvals = nrcs;
   for (i=0; i<*nvals; i++) status[i] = rcs[i];
   ezcaFree((void *) rcs);
   return(rc);
}


WAVE_HEADER3(long, ezcaIDLGetEnumStrings, char, pvname, int, s, char, strings)

   int status,i;
   struct dbr_gr_enum dbr;
   chid *chid;
   char *out;
   /* NOTE - this routines assumes that strings is
    * char[MAX_STRING_SIZE*MAX_ENUM_STATES] */

   status = ezcaPvToChid(pvname, &chid);
   if (status != EZCA_OK) return(status);

   status = ca_get(DBR_GR_ENUM, *chid, &dbr);
   if (status != ECA_NORMAL)
   {
      printf("ca_get failed on %s\n", pvname);
          return EZCA_CAFAILURE;
   }
        status = ca_pend_io(0.001);
        if (status != ECA_NORMAL)
        {
      printf("ca_pend_io failed on %s\n", pvname);
          return EZCA_CAFAILURE;
        }
        *s = dbr.no_str;
        if (*s>MAX_ENUM_STATES) *s=MAX_ENUM_STATES;
        out = strings;
        for (i=0;i<*s;i++) {
           strcpy(out, dbr.strs[i]);
           out += MAX_STRING_SIZE;
        }
        return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLStartGroup)
   return(ezcaStartGroup());
}

WAVE_HEADER0(int, ezcaIDLEndGroup)
   return(ezcaEndGroup());
}

WAVE_HEADER0(int, ezcaIDLDebugOn)
   ezcaDebugOn();
   return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLDebugOff)
   ezcaDebugOff();
   return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLTraceOn)
   ezcaTraceOn();
   return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLTraceOff)
   ezcaTraceOff();
   return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLAutoErrorMessageOn)
   ezcaAutoErrorMessageOn();
   return EZCA_OK;
}

WAVE_HEADER0(int, ezcaIDLAutoErrorMessageOff)
   ezcaAutoErrorMessageOff();
   return EZCA_OK;
}

/*************funciton from Ezca **********/
/***********************************************************/
WAVE_HEADER1(int, EzcaVersion, char, str)
        Ezca_version(str);
        return(OK);
}

WAVE_HEADER1(int, EzcaDebug, short, flag)
        return(Ezca_debug(*flag));
}

WAVE_HEADER1(int, EzcaInit, short, flag)
        return(Ezca_init(*flag));
}

WAVE_HEADER0(int, EzcaPendEvent)
    ca_pend_event(CA.PEND_EVENT_TIME);
    return (OK);
}

WAVE_HEADER2(int, EzcaSetPendTime, short, flag, float, time)
    return (Ezca_setPendTime(*flag, time));
}

WAVE_HEADER2(int, EzcaTimeStamp, char, str1, char, str2)
int st;
        st = Ezca_timeStamp(str1, str2);
        return(st);
}

WAVE_HEADER3(int, EzcaSearchList, short, s, char, str, chandata, list)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_search_list(*s,STR_ARRAY_ADDR(str),&(*list)));
}

WAVE_HEADER3(int, EzcaGetError, short, s, int, val, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_get_error_array(*s,STR_ARRAY_ADDR(str),val));
}


WAVE_HEADER5(int, EzcaGetArrayValues, int,type, int,count, short, s, void, d, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getArray(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER4(int, EzcaGetArrayTypeCount, short, s, int, type, int, count, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getTypeCount(*s,STR_ARRAY_ADDR(str),type,count));
}

WAVE_HEADER5(int, EzcaGetArrayEventValues, int,type, int,count, short, s, void, d, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getArrayEvent(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER5(int, EzcaPutArrayValues, int,type, int,count, short, s, void, d, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_putArray(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER5(int, EzcaPutArrayEventValues, int,type, int,count, short, s, void, d, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_putArrayEvent(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

/*********************************************************
*   trigger scan : add event monitors for the specified list
***********************************************************/
WAVE_HEADER4(int, EzcaMonitorScan_Add, short,npts,short,no,char,str,char, str2)
int i;
        BUILD_STR_ARRAY(*no, str2);
        return(Ezca_scanAdd(*npts,*no,str,STR_ARRAY_ADDR(str2)));
}

WAVE_HEADER2(int, EzcaMonitorScan_Clear, short, no, char, str2)
int i;
        BUILD_STR_ARRAY(*no, str2);
        return(Ezca_scanClearMonitor(*no,STR_ARRAY_ADDR(str2)));
}

WAVE_HEADER4(int, EzcaMonitorScan_Get,short,npts, short,no, double, d, char, str)
        *npts = Ezca_scanGetMonitor(str,d);
        return(*npts);
}

WAVE_HEADER2(int, EzcaMonitorScan_Zero, short, s, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_scanZeroMonitor(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER2(int, EzcaAddMonitorArray, short, s, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayAdd(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER2(int, EzcaClearMonitorArray, short, s, char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayClear(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER3(int, EzcaGetMonitorArray, short, s, double, vals,  char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayGet(*s,STR_ARRAY_ADDR(str),vals));
}

/***********************************************************/
WAVE_HEADER3(int, EzcaCheckMonitorArray, short, s, int, event,  char, str)
int i;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayCheck(*s,STR_ARRAY_ADDR(str),event));
}

WAVE_HEADER5(int, EzcaMonitorQueue_Get, short, mode, int, overflow, int, npts, double, d, char, str)
        return( Ezca_queueGet(str,d,overflow,*mode,npts));
}

WAVE_HEADER4(int, EzcaMonitorQueue_Add, short, mode, short, s, int, num, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueAdd(*mode,*num,*s,STR_ARRAY_ADDR(str)));
}

WAVE_HEADER2(int, EzcaMonitorQueue_Clear, short, s, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueClear(*s,STR_ARRAY_ADDR(str)));
}

WAVE_HEADER2(int, EzcaMonitorQueue_Zero, short, s, char, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueZero(*s,STR_ARRAY_ADDR(str)));
}

