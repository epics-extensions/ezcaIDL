#include <tsDefs.h>
#include <cadef.h>
#include <ezca.h>

#include <Ezca.h>
#define OK      0
#undef ERROR
#define ERROR   -1
extern struct caGlobals CA;

/* 	ezcaIDL.c
*
*	This file is an interface layer between IDL/PV-WAVE and EZCA.  It mainly
*   just converts parameter passing from the IDL/PV-WAVE mechanisms 
*   (most parameters passed by reference) to the mechanism required by EZCA.
*   For a few the functions which are not supplied by EZCA this routine actually
*   does the work, using the ezcaPvToChid to fetch the channel descriptor.
*
*	Author: Mark Rivers
*	Date:	June 28, 1995
*/

/* The following macros allow for the differences in the way PV-WAVE and
*  IDL pass character strings. PV-WAVE passes the address of the address
*  of the string in argp[]. IDL passes the address of a string descriptor
*  structure, which contains the address of the string as one of its elements */
#ifdef IDL
typedef struct {
   unsigned short length;
   short type;
   char *address;
} IDL_STR_DESCR;
#define STRARG IDL_STR_DESCR
#define STRADDR(s) s->address
#define STRFIXLEN(s) s->length = strlen(s->address)
static char* str_array_addr[1000];
#define BUILD_STR_ARRAY(len, s) for (i=0; i<len; i++) str_array_addr[i]=s[i].address
#define STR_ARRAY_ADDR(s) str_array_addr

#else /*(PV-WAVE)*/
#define STRARG char*
#define STRADDR(s) *s
#define STRFIXLEN(s) (*s)[strlen(*s)] = (char) 32
#define BUILD_STR_ARRAY(len, s)
#define STR_ARRAY_ADDR(s) s
#endif

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
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {
#   define WAVE_HEADER1(ftype, fname, type1, arg1)\
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {\
		  type1 *arg1 = (type1 *) argp[0];
#   define WAVE_HEADER2(ftype, fname, type1, arg1, type2, arg2)\
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {\
		  type1 *arg1 = (type1 *) argp[0];\
		  type2 *arg2 = (type2 *) argp[1];
#   define WAVE_HEADER3(ftype, fname, type1, arg1, type2, arg2, type3, arg3)\
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {\
		  type1 *arg1 = (type1 *) argp[0];\
		  type2 *arg2 = (type2 *) argp[1];\
		  type3 *arg3 = (type3 *) argp[2];
#   define WAVE_HEADER4(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4)\
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {\
		  type1 *arg1 = (type1 *) argp[0];\
		  type2 *arg2 = (type2 *) argp[1];\
		  type3 *arg3 = (type3 *) argp[2];\
		  type4 *arg4 = (type4 *) argp[3];
#   define WAVE_HEADER5(ftype, fname, type1, arg1, type2, arg2, type3, arg3, type4, arg4, type5, arg5)\
	  ftype fname(argc, argp)\
		  int argc;\
		  void *argp[];\
		  {\
		  type1 *arg1 = (type1 *) argp[0];\
		  type2 *arg2 = (type2 *) argp[1];\
		  type3 *arg3 = (type3 *) argp[2];\
		  type4 *arg4 = (type4 *) argp[3];\
		  type5 *arg5 = (type5 *) argp[4];
#endif


WAVE_HEADER4(int, ezcaIDLGet, STRARG, pvname, char, type, int, nelem, void, buff)
   return(ezcaGet(STRADDR(pvname), *type, *nelem, buff));
}

WAVE_HEADER3(int, ezcaIDLGetControlLimits, STRARG, pvname, double, low, double,	high)
   return(ezcaGetControlLimits(STRADDR(pvname), low, high));
}

WAVE_HEADER3(int, ezcaIDLGetGraphicLimits, STRARG, pvname, double, low, double,	high)
   return(ezcaGetGraphicLimits(STRADDR(pvname), low, high));
}

WAVE_HEADER2(int, ezcaIDLGetPrecision, STRARG, pvname, short, precision)
   return(ezcaGetPrecision(STRADDR(pvname), precision));
}

WAVE_HEADER4(int, ezcaIDLGetStatus, STRARG, pvname, TS_STAMP, timestamp, short,	status, short, severity)
   return(ezcaGetStatus(STRADDR(pvname), timestamp, status, severity));
}

WAVE_HEADER2(int, ezcaIDLGetUnits, STRARG, pvname, char, units)
   return(ezcaGetUnits(STRADDR(pvname), units));
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

WAVE_HEADER4(int, ezcaIDLPut, STRARG, pvname, char, type, int, nelem, void, buff)
   return(ezcaPut(STRADDR(pvname), *type, *nelem, buff));
}

WAVE_HEADER3(int, ezcaIDLGetCountAndType, STRARG, pvname, int, count, char,	type)
   int status;
   chid *chid;

   status = ezcaPvToChid(STRADDR(pvname), &chid);
   if (status != EZCA_OK) return(status);
   *count = ca_element_count(*chid);
   *type = ca_field_type(*chid);
   return EZCA_OK;
}

WAVE_HEADER2(int, ezcaIDLSetMonitor, STRARG, pvname, char, type)
   return(ezcaSetMonitor(STRADDR(pvname), *type));
}

WAVE_HEADER2(int, ezcaIDLClearMonitor, STRARG, pvname, char, type)
   return(ezcaClearMonitor(STRADDR(pvname), *type));
}

WAVE_HEADER2(int, ezcaIDLNewMonitorValue, STRARG, pvname, char, type)
   return(ezcaNewMonitorValue(STRADDR(pvname), *type));
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


WAVE_HEADER3(long, ezcaIDLGetEnumStrings, STRARG, pvname, int, s, char, strings)

   int status,i;
   struct dbr_gr_enum dbr;
   chid *chid;
   char *out;
   /* NOTE - this routines assumes that strings is char[16][40] */

   status = ezcaPvToChid(STRADDR(pvname), &chid);
   if (status != EZCA_OK) return(status);

   status = ca_get(DBR_GR_ENUM, *chid, &dbr);
   if (status != ECA_NORMAL) 
   {
      printf("ca_get failed on %s\n", STRADDR(pvname));
	  return EZCA_CAFAILURE;
   }
	status = ca_pend_io(0.001);
	if (status != ECA_NORMAL)
	{
      printf("ca_pend_io failed on %s\n", STRADDR(pvname));
	  return EZCA_CAFAILURE;
	}
	*s = dbr.no_str;
	if (*s>16) *s=16;
	out = strings;
	for (i=0;i<*s;i++) { 
	   strcpy(out, dbr.strs[i]);
	   out += 40;
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
WAVE_HEADER1(int, EzcaVersion, STRARG,str)
	Ezca_version(STRADDR(str));
/*	STRFIXLEN(str); */
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
    return (Ezca_setPendTime(*flag,time));
}

WAVE_HEADER2(int, EzcaTimeStamp, STRARG, str1, STRARG, str2)
int st;
	st = Ezca_timeStamp(STRADDR(str1),STRADDR(str2));
	return(st);
}

WAVE_HEADER2(int, EzcaSearchList, short, s, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_search_list(*s,STR_ARRAY_ADDR(str)));
}

WAVE_HEADER3(int, EzcaGetError, short, s, int, val, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_get_error_array(*s,STR_ARRAY_ADDR(str),val));
}


WAVE_HEADER5(int, EzcaGetArrayValues, int,type, int,count, short, s, void, d, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getArray(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER4(int, EzcaGetArrayTypeCount, int, s, int, type, int, count, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getTypeCount(*s,STR_ARRAY_ADDR(str),type,count));
}

WAVE_HEADER5(int, EzcaGetArrayEventValues, int,type, int,count, short, s, void, d, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_getArrayEvent(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER5(int, EzcaPutArrayValues, int,type, int,count, short, s, void, d, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_putArray(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

WAVE_HEADER5(int, EzcaPutArrayEventValues, int,type, int,count, short, s, void, d, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_putArrayEvent(*s,STR_ARRAY_ADDR(str),*type,*count,d));
}

/*********************************************************
*   trigger scan : add event monitors for the specified list
***********************************************************/
WAVE_HEADER4(int, EzcaMonitorScan_Add, short,npts,short,no,STRARG,str,STRARG, str2)
int i;
        BUILD_STR_ARRAY(*no, str2);
        return(Ezca_scanAdd(*npts,*no,STRADDR(str),STR_ARRAY_ADDR(str2)));
}

WAVE_HEADER2(int, EzcaMonitorScan_Clear, short, no, STRARG, str2)
int i;
        BUILD_STR_ARRAY(*no, str2);
	return(Ezca_scanClearMonitor(*no,STR_ARRAY_ADDR(str2)));
}

WAVE_HEADER4(int, EzcaMonitorScan_Get,short,npts, short,no, double, d, STRARG, str)
	*npts = Ezca_scanGetMonitor(STRADDR(str),d);
	return(*npts);
}

WAVE_HEADER2(int, EzcaMonitorScan_Zero, short, s, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
	return(Ezca_scanZeroMonitor(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER2(int, EzcaAddMonitorArray, short, s, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayAdd(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER2(int, EzcaClearMonitorArray, short, s, STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayClear(*s,STR_ARRAY_ADDR(str)));
}

/***********************************************************/
WAVE_HEADER3(int, EzcaGetMonitorArray, short, s, double, vals,  STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayGet(*s,STR_ARRAY_ADDR(str),vals));
}

/***********************************************************/
WAVE_HEADER3(int, EzcaCheckMonitorArray, short, s, int, event,  STRARG, str)
int i,no;

        BUILD_STR_ARRAY(*s, str);
        return(Ezca_monitorArrayCheck(*s,STR_ARRAY_ADDR(str),event));
}

WAVE_HEADER5(int, EzcaMonitorQueue_Get, short, mode, int, overflow, int, npts, double, d, STRARG, str)
        return( Ezca_queueGet(STRADDR(str),d,overflow,*mode,npts));
}

WAVE_HEADER4(int, EzcaMonitorQueue_Add, short, mode, short, s, int, num, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueAdd(*mode,*num,*s,STR_ARRAY_ADDR(str)));
}

WAVE_HEADER2(int, EzcaMonitorQueue_Clear, short, s, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueClear(*s,STR_ARRAY_ADDR(str)));
}

WAVE_HEADER2(int, EzcaMonitorQueue_Zero, short, s, STRARG, str)
int i;
        BUILD_STR_ARRAY(*s, str);
        return(Ezca_queueZero(*s,STR_ARRAY_ADDR(str)));
}

