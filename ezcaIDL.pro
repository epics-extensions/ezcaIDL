function call_Ezca, routine, p1, p2, p3, p4, p5
; NAME:
;	call_Ezca
;
; PURPOSE:
;	This function calls the shareable image which contains the EZCA
;       routines, and the short interface routines which call EZCA.
;       This routine is intended only to be called internally from the 
;       routines in this file. This routine is needed in order to make this 
;       package work with both IDL and PV-WAVE, and with both Unix and VMS.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	Status = call_Ezca(routine, p1, p2, p3, p4, p5)
;
; INPUTS:
;	routine:  The name of the external routine to be called
;
; OPTIONAL INPUT PARAMETERS:
;       p1:       The first parameter to be passed to the external routine
;       p2:       The second parameter to be passed to the external routine
;       p3:       The third parameter to be passed to the external routine
;       p4:       The fourth parameter to be passed to the external routine
;       p5:       The fifth parameter to be passed to the external routine
;
; OUTPUTS:
;   The function return value of call_Ezca is the value returned from the
;   external routine.
;
; COMMON BLOCKS:
;       EZCA_COMMON is used to store the name of the program being run,
;       the name of the shareable image, and a flag which indicates if we
;       are currently in an asynchronous group.
;
; PROCEDURE:
;	This routine calls call_external with IDL or linknload with PV-WAVE.
;       The name of the shareable image which it calls must defined with the
;       logical name (VMS) or environment variable (Unix) EZCA_IDL_SHARE (for
;       IDL) or EZCA_WAVE_SHARE (for PV-WAVE).
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	

; The first time through determine if we are running IDL or PV-WAVE and set
; things up accordingly

common ezca_common, program, IDL_object, WAVE_object, ingroup

if (n_elements(program) eq 0) then begin
    if (strpos(strupcase(!dir), 'IDL') ne -1) then begin
	program='IDL'
	if (n_elements(IDL_object) eq 0) then begin
	   IDL_object = getenv('EZCA_IDL_SHARE')
	   if IDL_object eq "" then IDL_object = 'ezcaIDL.so'
	endif
    endif else begin
	program='PV-WAVE'
	if (n_elements(WAVE_object) eq 0) then begin
	   WAVE_object = getenv('EZCA_WAVE_SHARE')
	   if WAVE_object eq "" then WAVE_object = 'ezcaWave.so'
	endif
    endelse
    ; Clear the ingroup flag if it doesn't exist yet
    if (n_elements(ingroup) eq 0) then ingroup=0
endif


if (program eq 'PV-WAVE') then goto, wave

; This is for calling IDL
	r = routine
	case n_params() of
1:	   return, call_external(IDL_object, r)
2:	   return, call_external(IDL_object, r, p1)
3:	   return, call_external(IDL_object, r, p1, p2)
4:	   return, call_external(IDL_object, r, p1, p2, p3)
5:	   return, call_external(IDL_object, r, p1, p2, p3, p4)
6:	   return, call_external(IDL_object, r, p1, p2, p3, p4, p5)
	endcase

wave:

; This is for calling PV-WAVE
	r = routine
	case n_params() of
1:	   return, linknload(WAVE_object, r)
2:	   return, linknload(WAVE_object, r, p1)
3:	   return, linknload(WAVE_object, r, p1, p2)
4:	   return, linknload(WAVE_object, r, p1, p2, p3)
5:	   return, linknload(WAVE_object, r, p1, p2, p3, p4)
6:	   return, linknload(WAVE_object, r, p1, p2, p3, p4, p5)
	endcase
end



function ezcaType, value
; NAME:
;	ezcaType
;
; PURPOSE:
;	This function returns the EZCA data type corresponding to an IDL
;       variable. It is called by caPut to determine the "type" parameter
;       to be passed to ezcaPut().
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	type = ezcaType(value)
;
; INPUTS:
;	value:	The IDL variable whose corresponding EZCA data type is to
;               be determined.
;
; OUTPUTS:
;       The function returns the EZCA data type (0-5) which corresponds to the
;       data type of the IDL variable. If the IDL variable is not of an allowed
;       type then ezcaType() returns -1.
;
; PROCEDURE:
;	Simple lookup table.
;
; RESTRICTIONS:
;       The IDL variable cannot be of data type complex (6), double precision
;       complex (9) or structure (8).  This is because channel access has no
;       equivalents for these data types.
;
; EXAMPLE:
;       IDL> test1 = 0.
;       IDL> type = ezcaType(test1)
;       IDL> print, type
;       4
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	

    type= size(value)
    type = type(type(0)+1)
    case type of
        1:  ezca_type = 0   ; Byte
        2:  ezca_type = 2   ; Short
        3:  ezca_type = 3   ; Long
        4:  ezca_type = 4   ; Float
        5:  ezca_type = 5   ; Double
        7:  ezca_type = 1   ; String
        else: ezca_type=-1  ; Anything else
    endcase
    return, ezca_type
end



function caGetCountAndType, pvname, count, type
;+
; NAME:
;	caGetCountAndType
;
; PURPOSE:
;	This function returns the number of elements and data type of a 
;       Channel Access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	Status = caGetCountAndType(pvname, count, type)
;
; INPUTS:
;	pvname:	The name of the process variable for which information is to
;               be returned.
;
; OUTPUTS:
;	count:  The number of elements in the process variable. This is 1 for
;               scalar process variables and more than 1 for arrays.
;
;       type:   This is a 3 element array containing information about the data
;               type of the process variable.
;               type(0) = Channel access data type as defined in "cadef.h"
;               type(1) = EZCA data type as defined in "ezca.h"
;               type(2) = IDL/PV-WAVE data type as defined in size()
;               These data types are as follows:
;
;               Name    Channel Access      EZCA        IDL/PVWAVE
;               String      0                 1             7
;               Short       1                 2             2
;               Float       2                 4             4
;               Enum        3                 2 (short)     2 (short)
;               Byte        4                 0             1
;               Long        5                 3             3
;               Double      6                 5             5
;
;   The function return value of caGetCountAndType is a status value.  The
;   status is 0 if the routine was successful (i.e. the process variable exists)
;   and non-zero if the routine failed.
;
; SIDE EFFECTS:
;	This routine will cause a Channel Access search to take place if this is
;       the first time this process variable has been referenced.
;
; RESTRICTIONS:
;       The channel access data type enum is mapped to EZCA and IDL short
;       data types.  However, application programs can use this routine to 
;       determine if the native channel access data type is enum, and then
;       use caGet(pvname, value, /string) to read the string equivalent of the 
;       process variable. Programs can also use 
;       caGetEnumStrings(pvname, strings) to read the strings for the all of
;       the possible values of an enum process variable.
;
; PROCEDURE:
;	This routine uses ezcaPvToChid() and then ca_element_count() and
;       ca_field_type().
;       Note that this routine always returns its values "immediately", even 
;       if it is called between a caStartGroup and caEndGroup.
;
; EXAMPLE:
;       IDL> status = caGetCountAndType('test_mca1.VAL', count, type)
;       IDL> print, status
;       0                       ; Status = success
;       IDL> print, count
;       2048                    ; Array with 2048 elements
;       IDL> print, type
;           5       3       3   ; Long data type
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-

    ca_type = 0B
    count = 0L
    status = call_ezca('ezcaIDLGetCountAndType', string(pvname), count, ca_type)
    if (status ne 0) then return, status
    case ca_type of 
        0: begin            ; String
            wave_type = 7
            ezca_type = 1
           end
        1: begin	    ; Short
            wave_type = 2
            ezca_type = 2
           end
        2: begin            ; Float
    	    wave_type = 4
            ezca_type = 4
           end
        3: begin            ; Enum
    	    wave_type = 2   ; Short
            ezca_type = 2   ; Short
           end
        4: begin            ; Byte
    	    wave_type = 1
            ezca_type = 0
           end
        5: begin            ; Long
	    wave_type = 3
            ezca_type = 3
           end
        6: begin            ; Double
	    wave_type = 5
            ezca_type = 5
           end
        else: begin
            wave_type = -1
            ezca_type = -1
            status = -1
	   end
	endcase
	type = [ca_type, wave_type, ezca_type]
        return, status
end



function caGet, pvname, val, string=string, max=n
;+
; NAME:
;	caGet
;
; PURPOSE:
;	This function reads the value of a Channel Access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	Status = caGet(pvname, value, /string, max=n)
;
; INPUTS:
;	pvname:	The name of the process variable for which the value is to
;               be returned.
;
; KEYWORD PARAMETERS:
;	STRING:	Set this flag to force caGet to return a string, rather than
;               a number.  This flag is particularly useful when the native
;               channel access data type is ENUM (3), since the string is
;               more descriptive than the number.
;
;	MAX:    This keyword parameter is used to limit the number of
;               values returned by caGet. caGet normally returns the native
;               element count for a process variable. Setting MAX to a
;               number less than this will cause caGet to return only the
;               first MAX values in the array.
;
; OUTPUTS:
;	value:  The value of the process variable. By default, caGet returns
;               "value" with the native data type and number of elements
;               of the process variable. It determines this information by
;               calling caGetCountAndType().  Note that if caGet is called
;               after calling caStartGroup but before calling caEndGroup then
;               the IDL variable "value" is created, but will not actually
;               contain the data until caEndGroup is called.
;
;       The function return value of caGet is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.  If caGet is called from
;       within an asynchronous group then the status return only indicates
;       whether the operation was successfully queued.
;
; COMMON BLOCKS:
;       EZCA_COMMON contains a flag (ingroup) which indicates if we
;       are currently in an asynchronous group. This routine tests that flag.
;
; SIDE EFFECTS:
;	This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced. It
;       performs a ca_get, unless called as part of an asynchronous group.
;
; RESTRICTIONS:
;       There are two important restrictions which must be kept in mind when
;       calling caGet from inside a "group", i.e. after calling caStartGroup 
;       and before calling caEndGroup.
;
;       1) The IDL "value" variable (i.e. the second parameter
;       passed to caGet) must not be "re-used" or deleted before the call to
;       caEndGroup. The reason for this is that EZCA has been passed the 
;       address of this variable as the location in which the data is to be 
;       copied when caEndGroup is called. Thus, this location must still 
;       point to a valid memory location when caEndGroup is called.  
;       If the "value" variable is re-used then IDL's behavior is 
;       unpredictable, and bus errors/access violations could occur.
;
;       2) When using caGet to read strings, the data type returned will be
;       a byte array, rather than a string.  The reason has to do with the 
;       manner in which IDL passes strings, which requires that EZCA actually
;       be passed pointers to byte arrays. When caGet is called outside of a
;       group it automatically converts the byte array to a string before
;       returning the value. However when caGet is called inside of a group 
;       it cannot perform this conversion, since it cannot be done until after 
;       the data is read, which does not occur until caEndGroup is called. 
;       Thus, it is the user's responsibility to convert the data from a byte 
;       array to a string after calling caEndGroup. This is done very simply 
;       with the string() function. For more information see the example below.
;
; PROCEDURE:
;	This routine uses ezcaGet().
;
; EXAMPLES:
;       IDL> ; The following is an example of a single caGet
;       IDL> status = caGet('test_mca1.VAL', value)
;
;       IDL> ; The following is an example of a valid grouped operation
;       IDL> ; It also shows how to handle strings.
;       IDL> caStartGroup
;       IDL> status = caGet('test_mca1.VAL', mca_value)
;       IDL> status = caGet('test_vme1.DESC', vme_desc) ; This is a string PV
;       IDL> status = caEndGroup()
;       IDL> vme_desc = string(vme_desc)    ; Convert from byte array to string
;
;       IDL> ; The following is an example of an INVALID grouped operation
;       IDL> caStartGroup
;       IDL> status = caGet('test_mca1.VAL', mca_value)
;       IDL> status = caGet('test_vme1.VAL', vme_value)
;       IDL> mca_value=0
;       IDL> ; We have redefined mca_value, so the previous location is
;       IDL> ; undefined. NO NOT DO THIS!
;       IDL> status = caEndGroup()
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-

    common ezca_common

    status = caGetCountAndType(pvname, nelem, type)
    if (status ne 0) then return, status
    type = type(2)  ; The EZCA data type
    if (n_elements(max) gt 0) then nelem = nelem < max
    if (nelem eq 1) then begin
        if (keyword_set(string)) then begin
            type = 1
            val = bytarr(132)
        endif else begin
            case type of
                0:   val = 0B           ; Byte
                1:   val = bytarr(132)  ; String
                2:   val = 0            ; Short
                3:   val = 0L           ; Long
                4:   val = 0.           ; Float
                5:   val = 0.D0         ; Double
            endcase
        endelse
    endif else begin
        case type of
            0: val = bytarr(nelem)  ; Byte
            1: val = bytarr(40, nelem)  ; String
            2: val = intarr(nelem)  ; Short
            3: val = lonarr(nelem)  ; Long
            4: val = fltarr(nelem)  ; Float
            5: val=  dblarr(nelem)  ; Double
        endcase
    endelse
    status = call_ezca('ezcaIDLGet', string(pvname), byte(type), $
                      long(nelem), val)
    if ((not ingroup) and (type eq 1)) then val = string(val)
    return, status
end



function caGetTimeout
;+
; NAME:
;	caGetTimeout
;
; PURPOSE:
;	This function returns the value of the EZCA Timeout parameter. This
;       value determines the time parameter passed to ca_pend_io() in EZCA.
;       In conjunction with the EZCA RetryCount parameter it determines how
;       long EZCA will try to connect to a process variable before giving up.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	timeout = caGetTimeout()
;
; INPUTS:
;	None.
;
; OUTPUTS:
;       The function return value of caGetTimeout is the floating point
;       value of the EZCA Timeout parameter, in seconds.
;
; PROCEDURE:
;	This routine uses ezcaGetTimeout().
;
; EXAMPLES:
;       IDL> print, caGetTimeout()
;       0.0500000
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    timeout = 0.
    status = call_ezca('ezcaIDLGetTimeout', timeout)
    return, timeout
end



pro caSetTimeout, timeout
;+
; NAME:
;	caSetTimeout
;
; PURPOSE:
;	This procedure sets the value of the EZCA Timeout parameter. This
;       value determines the time parameter passed to ca_pend_io() in EZCA.
;       In conjunction with the EZCA RetryCount parameter it determines how
;       long EZCA will try to connect to a process variable before giving up.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caSetTimeout, timeout
;
; INPUTS:
;	Timeout:  The timeout value in seconds (floating point).
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;	This routine uses ezcaSetTimeout().
;
; EXAMPLES:
;       IDL> caSetTimeout, .001
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    status = call_ezca('ezcaIDLSetTimeout', float(timeout))
end



function caGetRetryCount
;+
; NAME:
;	caGetRetryCount
;
; PURPOSE:
;	This function returns the value of the EZCA retry count parameter.
;       In conjunction with the EZCA Timeout parameter it determines how
;       long EZCA will try to connect to a process variable before giving up.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	RetryCount = caGetRetryCount()
;
; INPUTS:
;	None.
;
; OUTPUTS:
;       The function return value of caGetRetryCount is the integer
;       value of the EZCA RetryCount parameter.
;
; PROCEDURE:
;	This routine uses ezcaGetRetryCount().
;
; EXAMPLES:
;       IDL> print, caGetRetryCount()
;       599
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    count = 0L
    status = call_ezca('ezcaIDLGetRetryCount', count)
    return, count
end



pro caSetRetryCount, retryCount
;+
; NAME:
;	caSetRetryCount
;
; PURPOSE:
;	This procedure sets the value of the EZCA RetryCount parameter.
;       In conjunction with the EZCA Timeout parameter it determines how
;       long EZCA will try to connect to a process variable before giving up.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caSetRetryCount, retrycount
;
; INPUTS:
;	RetryCount: The integer retry count.
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;	This routine uses ezcaSetRetryCount().
;
; EXAMPLES:
;       IDL> caSetRetryCount, 100
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    status = call_ezca('ezcaIDLSetRetryCount', long(retryCount))
end



function caPut, name, value
;+
; NAME:
;	caPut
;
; PURPOSE:
;	This procedure writes a new value to a channel access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caPut(pvname, value)
;
; INPUTS:
;	pvname:	The name of the process variable for which the new value
;               is to be written
;
;       value:  The new value to be written. In general this can be a scalar
;               or array of any data type.  There are of course restrictions
;               in that certain strings cannot be written to certain process
;               variables, and some process variables cannot be passed arrays.
;
; OUTPUTS:
;       The function return value of caPut is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists and a valid value was written) and non-zero if the routine 
;       failed.
;
; PROCEDURE:
;	This routine uses ezcaPut(). The "nelem" and "type" parameters passed
;       to ezcaPut are determined from the IDL data type and number of elements
;       of the "value" parameter passed to caPut(). Strings are converted to
;       to byte arrays before being passed.
;
; RESTRICTIONS:
;       None.   caPut can be called inside a group, i.e. after calling
;               caStartGroup and before calling caEndGroup.  The "value"
;               variable passed to caPut can be immediately re-used when
;               inside a group, since EZCA copies it to a private location.
;
; EXAMPLES:
;       IDL> ; Put a linear ramp (findgen()) to a waveform process variable.
;       IDL> status = caPut('my_waveform.VAL', findgen(100))
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    nelem = n_elements(value)
    if (nelem eq 0) then return, -1  ; Variable not defined
    type = ezcatype(value)
    if (type eq 1) then begin  ; Convert strings to byte arrays
        temp = bytarr(40, nelem)
        for i=0, nelem-1 do begin
            temp(0,i) = byte(value(i))  ; Convert string to byte
            temp(39,i) = 0              ; Make sure there is a null at end
        endfor
        status = call_ezca('ezcaIDLPut', string(name), $
                          byte(type), long(nelem), temp)
    endif else begin
        status = call_ezca('ezcaIDLPut', string(name), $
                          byte(type), long(nelem), value)
    endelse
    return, status
end



pro caStartGroup
;+
; NAME:
;	caStartGroup
;
; PURPOSE:
;	This procedure starts an "asynchronous group".  Within an asynchronous
;       group all calls to caGet and caPut are asynchronous, i.e. they queue
;       a request and return immediately without waiting for a reply from
;       the channel access servers. Calling caEndGroup causes the queue to be 
;       flushed and waits for the replies. The use of asynchronous 
;       groups can greatly improve the efficiency of channel access. The user
;       must be aware of the restrictions on caGet outlined under the
;       description of that routine.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caStartGroup
;
; INPUTS:
;	None.
;
; OUTPUTS:
;       None
;
; COMMON BLOCKS:
;       EZCA_COMMON contains a flag (ingroup) which indicates if we
;       are currently in an asynchronous group. This routine sets that flag.
;
; PROCEDURE:
;	This routine uses ezcaStartGroup().
;
; EXAMPLES:
;       IDL> caStartGroup
;       IDL> status = caget('test_ao1.SCAN', scan)
;       IDL> status = caget('test_mca1.ERTM', ertm)
;       IDL> ; Print out values - they will be zero.
;       IDL> help, scan, ertm
;       IDL> status = caEndGroup()
;       IDL> ; Print out values after executing caEndGroup, they are non-zero
;       IDL> help, scan, ertm
;       Output:
;           SCAN            INT       =        0
;           ERTM            FLOAT     =      0.000000
;           SCAN            INT       =        6
;           ERTM            FLOAT     =       7.10000
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    common ezca_common
    ingroup = 1
    status = call_ezca('ezcaIDLStartGroup')
end



function caEndGroup, status
;+
; NAME:
;	caEndGroup
;
; PURPOSE:
;	This function ends an "asynchronous group". See caStartGroup for more
;       information on asynchronous groups.
;       caEndGroup flushes the queue of caGet and caPut calls and waits for
;       replies from the channel access servers.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	stat = caEndGroup(status)
;
; INPUTS:
;	None.
;
; OUTPUTS:
;       The function return value is 0 if the operation was successful,
;       otherwise it is the first encountered non-successful return code.
;       The optional status parameter can be used to return the status code 
;       of each operation in the group.
;
; OPTIONAL OUTPUT PARAMETERS:
;       status: If this optional parameter is present then it returns a
;               array of status information, one for each channel access
;               call in the group.
;
; COMMON BLOCKS:
;       EZCA_COMMON contains a flag (ingroup) which indicates if we
;       are currently in an asynchronous group. This routine clears that flag.
;
; PROCEDURE:
;	If the status parameter is present then this routine uses
;       ezcaEndGroupWithReport().  If the parameter is not present then
;       the routine calls ezcaEndGroup().
;
; RESTRICTIONS:
;       When the status parameter is present, and ezcaEndGroupWithReport() is
;       called, there is no way to know in advance how many status values
;       will be returned.  This routine passes a status array with 1024
;       elements, and then truncates it to the actual length.  The maximum
;       number of status values which can be retrieved is thus 1024. No errors
;       will occur if an asynchronous group has more than 1024 calls, but
;       only the first 1024 status values can be obtained. 
;       This is probably sufficient for most applications!
;
; EXAMPLES:
;       IDL> caStartGroup
;       IDL> status = caget('test_ao1.SCAN', scan)
;       IDL> status = caget('test_mca1.ERTM', ertm)
;       IDL> ; Print out values - they will be zero.
;       IDL> help, scan, ertm
;       IDL> status = caEndGroup()
;       IDL> ; Print out values after executing caEndGroup, they are non-zero
;       IDL> help, scan, ertm
;       Output:
;           SCAN            INT       =        0
;           ERTM            FLOAT     =      0.000000
;           SCAN            INT       =        6
;           ERTM            FLOAT     =       7.10000
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    common ezca_common
    ingroup = 0
    if (n_params() eq 0) then begin
        return, call_ezca('ezcaIDLEndGroup')
    endif else begin
        nvals = 1024L
        status = lonarr(nvals)
        stat = call_ezca('ezcaIDLEndGroupWithReport', nvals, status)
        status = status(0:nvals-1)
        return, stat
    endelse
end


function caSetMonitor, pvname
;+
; NAME:
;	caSetMonitor
;
; PURPOSE:
;	This procedure sets a monitor on the specified process variable.
;       This causes a channel access callback to execute whenever the value
;       of that process variable changes.  Subsequent calls to caGet() after
;       calling caSetMonitor will read the values provided by the callbacks,
;       rather than reading from the IOC.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caSetMonitor(pvname)
;
; INPUTS:
;	pvname: The name of the process variable on which to set the monitor.
;
; OUTPUTS:
;       The function return value of caSetMonitor is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaSetMonitor(). The "type" parameter required
;       by ezcaSetMonitor is the native EZCA data type as determined 
;       by caGetCountAndType().
;
; EXAMPLES:
;       IDL> status = caSetMonitor('test_ao1')
;       IDL> status = caGet('test_ao1', value)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    status = caGetCountAndType(pvname, count, type)
    if (status ne 0) then return, status
    status = call_ezca('ezcaIDLSetMonitor', string(pvname), byte(type(2)))
    return, status
end



function caClearMonitor, pvname
;+
; NAME:
;	caClearMonitor
;
; PURPOSE:
;	This procedure clears a monitor on the specified process variable.
;       It cancels the effect of caSetMonitor().
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caClearMonitor(pvname)
;
; INPUTS:
;	pvname: The name of the process variable on which to clear the monitor.
;
; OUTPUTS:
;       The function return value of caClearMonitor is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaClearMonitor(). The "type" parameter required
;       by ezcaClearMonitor is the native EZCA data type as determined 
;       by caGetCountAndType().
;
; EXAMPLES:
;       IDL> status = caClearMonitor('test_ao1')
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    status = caGetCountAndType(pvname, count, type)
    if (status ne 0) then return, status
    status = call_ezca('ezcaIDLClearMonitor', string(pvname), byte(type(2)))
    return, status
end



function caCheckMonitor, pvname
;+
; NAME:
;	caCheckMonitor
;
; PURPOSE:
;	This function returns a non-zero value if there is a new (unread)
;       monitor for this process variable, otherwise it returns zero.
;       This function is particularly useful when a caGet() operation is
;       expensive in time, e.g. reading large arrays.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	state = caCheckMonitor(pvname)
;
; INPUTS:
;	pvname: The name of the process variable on which to check the monitor.
;
; OUTPUTS:
;       The function return value is zero if no new monitor value is available,
;       and non-zero if a new monitor value is available.
;
; PROCEDURE:
;	This routine uses ezcaNewMonitorValue(). The "type" parameter required
;       by ezcaNewMonitorValue() is the native EZCA data type as determined 
;       by caGetCountAndType().
;
; EXAMPLES:
;       IDL> state = caCheckMonitor('test_ao1',event)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    status = caGetCountAndType(pvname, count, type)
    if (status ne 0) then return, status
    status = call_ezca('ezcaIDLNewMonitorValue', string(pvname), byte(type(2)))
    return, status
end


pro caDebug, state
;+
; NAME:
;	caDebug
;
; PURPOSE:
;	This procedure turns the EZCA debugging flag on or off. Turning on
;       the debugging flag prints lots of information, which is mainly
;       useful to developers.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caDebug, state
;
; INPUTS:
;	state:  state=1 turns debugging on, state=0 turns debugging off.
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;	This routine uses ezcaDebugOn() and ezcaDebugOff().
;
; EXAMPLES:
;       IDL> caDebug, 1     ; Turn on debugging
;       setting Debug
;       IDL> status = caGet('test_ao1', value)
;       ca_pend_event(0.000010)
;       --start end-of-prologue() report
;       ****** Start State:
;       AutoErrorMessage T InGroup F Debug T Trace F ErrorLocation LastOnly ListPrint
;       LastOnly TimeoutSeconds 0.050000
;       Workp : 9cf970 trashme F (nxt 0)
;       Channel_avail_hdr 0 :
;       ...
;       ...
;       IDL> caDebug, 0 ; Turn off debugging
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    if (state eq 0) then begin
        status = call_ezca('ezcaIDLDebugOff')
    endif else begin
        status = call_ezca('ezcaIDLDebugOn')
    endelse
	status = call_ezca('EzcaDebug',state)
end


pro caTrace, state
;+
; NAME:
;	caTrace
;
; PURPOSE:
;	This procedure turns the EZCA trace flag on or off. Turning on
;       the trace flag prints lots of information which is mainly useful 
;       to developers.  Setting the trace flag results in less
;       verbose output than setting the debug flag (see caDebug).
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caTrace, state
;
; INPUTS:
;	state:  state=1 turns trace on, state=0 turns trace off.
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;	This routine uses ezcaTraceOn() and ezcaTraceOff().
;
; EXAMPLES:
;       IDL> caTrace, 1     ;Turn on trace
;       setting Trace
;       IDL> status = caGet('test_ao1', value)
;       ca_pend_event(0.000010)
;       find_channel() found >test_ao1<
;       get_channel(): was able to find_channel()
;       ca_pend_event(0.000010)
;       ...
;       ...
;       IDL> caTrace, 0     ; Turn off trace
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    if (state eq 0) then begin
        status = call_ezca('ezcaIDLTraceOff')
    endif else begin
        status = call_ezca('ezcaIDLTraceOn')
    endelse
end



function caGetEnumStrings, pvname, strings
;+
; NAME:
;	caGetEnumStrings
;
; PURPOSE:
;	This function returns all of the choice strings associated with a 
;       Channel Access "enum" process variable. It is particularly useful
;       for building menus of options.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	Status = caGetEnumStrings(pvname, strings)
;
; INPUTS:
;	pvname:	The name of the process variable for which the enum strings
;               are to be returned. The native channel access data type of
;               this process variable must be enum (3).
;
; OUTPUTS:
;	strings: A string array containing the strings for each possible
;               value of the enum variable.
;
;       The function return value of caGetEnumStrings is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists and is of type enum) and non-zero if the routine failed.
;
; SIDE EFFECTS:
;	This routine causes a channel access read. It does not use the 
;       grouping mechanism of EZCA, i.e. it always executes immediately.
;
; RESTRICTIONS:
;       There must be less than 16 enum strings and they must each be less
;       than 26 characters.
;
; PROCEDURE:
;	This routine uses ezcaPvToChid and then ca_get() with a request type
;       of DBR_GR_ENUM.  The functionality required by this routine is not
;       presently provided directly in EZCA, although it should probably be 
;       added.
;
; EXAMPLES:
;       IDL> status = caGetEnumStrings('test_mca1.SCAN', strings)
;       IDL> for i=0, n_elements(strings)-1 do print, strings(i)
;       Passive
;       Event
;       I/O Intr
;       10 second
;       5 second
;       2 second
;       1 second
;       .5 second
;       .2 second
;       .1 second
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    ; Make sure the data type is DBF_ENUM
    status = caGetCountAndType(pvname, count, type)
    if (status ne 0) then return, status
    if (type(0) ne 3) then return, -1
    array = bytarr(40, 16)
    n = 0L
    status = call_ezca('ezcaIDLGetEnumStrings', string(pvname), n, array)
    if (status ne 0) then return, status
    strings = string(array(*, 0:n-1))
end



function caGetControlLimits, pvname, low, high
;+
; NAME:
;	caGetControlLimits
;
; PURPOSE:
;	This procedure reads the control limits for the specified channel
;       access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caGetControlLimits(pvname, low, high)
;
; INPUTS:
;	pvname: The name of the process variable from which to read the 
;               control limits.
;
; OUTPUTS:
;       low:    The low control limit (double).
;
;       high:   The high control limit (double).

;       The function return value of caGetControlLimits is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaGetControlLimits(). 
;
; EXAMPLE:
;       IDL> status = caGetControlLimits('test_ao1', low, high)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    low = 0.D0
    high = 0.D0
    status = call_ezca('ezcaIDLGetControlLimits', string(pvname), low, high)
    return, status
end


function caGetGraphicLimits, pvname, low, high
;+
; NAME:
;	caGetGraphicLimits
;
; PURPOSE:
;	This procedure reads the graphic limits for the specified channel
;       access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caGetGraphicLimits(pvname, low, high)
;
; INPUTS:
;	pvname: The name of the process variable from which to read the 
;               graphic limits.
;
; OUTPUTS:
;       low:    The low graphic limit (double).
;
;       high:   The high graphic limit (double).

;       The function return value of caGetGraphicLimits is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaGetGraphicLimits(). 
;
; EXAMPLE:
;       IDL> status = caGetGraphicLimits('test_ao1', low, high)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    low = 0.D0
    high = 0.D0
    status = call_ezca('ezcaIDLGetGraphicLimits', string(pvname), low, high)
    return, status
end


function caGetPrecision, pvname, precision
;+
; NAME:
;	caGetPrecision
;
; PURPOSE:
;	This procedure reads the precision for the specified channel
;       access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caGetPrecision(pvname, precision)
;
; INPUTS:
;	pvname: The name of the process variable from which to read the 
;               precision.
;
; OUTPUTS:
;       precision:  The precision (short).

;       The function return value of caGetPrecision is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaGetPrecision(). 
;
; EXAMPLE:
;       IDL> status = caGetPrecision('test_ao1', precision)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    precision = 0
    status = call_ezca('ezcaIDLGetPrecision', string(pvname), precision)
    return, status
end



function caGetStatus, pvname, timestamp, status, severity
;+
; NAME:
;	caGetStatus
;
; PURPOSE:
;	This procedure reads the status parameters for a channel access 
;       process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caGetStatus(pvname, timestamp, status, severity)
;
; INPUTS:
;	pvname: The name of the process variable from which to read the 
;               status parameters.
;
; OUTPUTS:
;       timestamp: The timestamp of the last time the record was processed
;               lonarr(2).
;
;       status: The status flag (int).
;
;       severity: The severity flag (int).

;       The function return value of caGetStatus is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; PROCEDURE:
;	This routine uses ezcaGetStatus(). 
;
; EXAMPLE:
;       IDL> status = caGetStatus('test_ao1', timestamp, status, severity)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    timestamp = lonarr(2)
    status = 0
    severity = 0
    status = call_ezca('ezcaIDLGetStatus', string(pvname), $
                        timestamp, status, severity)
    return, status
end



function caGetUnits, pvname, units
;+
; NAME:
;	caGetUnits
;
; PURPOSE:
;	This procedure reads the units string for the specified channel
;       access process variable.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	status = caGetUnits(pvname, units)
;
; INPUTS:
;	pvname: The name of the process variable from which to read the units.
;
; OUTPUTS:
;       units:  The units (string).
;
;       The function return value of caGetUnits is a status value.  The
;       status is 0 if the routine was successful (i.e. the process variable 
;       exists) and non-zero if the routine failed.
;
; COMMON BLOCKS:
;       EZCA_COMMON contains a flag (ingroup) which indicates if we
;       are currently in an asynchronous group. This routine tests that flag.
;
; PROCEDURE:
;	This routine uses ezcaGetUnits(). 
;
; EXAMPLE:
;       IDL> status = caGetUnits('test_ao1', units)
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    common ezca_common
    units = bytarr(40)
    status = call_ezca('ezcaIDLGetUnits', string(pvname), units)
    if (ingroup eq 0) then units = string(units)
    return, status
end



pro caError, err_string, on=on, off=off, print=print, prefix=prefix
;+
; NAME:
;	caError
;
; PURPOSE:
;	This procedure controls error printing and returns error strings.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caError, err_string, /ON, /OFF, /PRINT, prefix=prefix
;
; INPUTS:
;       None
;
; KEYWORD PARAMETERS:
;       /ON
;           Setting this switch turns on automatic error message printing
;           on stdout.  Automatic printing is initially enabled.
;
;       /OFF
;           Setting this switch turns off automatic error message printing
;           on stdout.
;
;       /PRINT
;           Setting this switch prints the last error message on stdout.
;
;       prefix=prefix
;           The prefix keyword can be used to pass a string which is prefixed
;           to error messages printed with /PRINT or fetched via the optional
;           output parameter.
;
; OPTIONAL OUTPUT PARAMETERS:
;	err_string:
;           If this parameter is present then it will contain the text of the
;           last error message.
;
; COMMON BLOCKS:
;       EZCA_COMMON contains a flag (ingroup) which indicates if we
;       are currently in an asynchronous group. This routine tests that flag.
;
; PROCEDURE:
;	This routine uses ezcaPerror(), ezcaAutoErrorMessageOn(), 
;       ezcaAutoErrorMessageOff(), and ezcaGetErrorString()
;
; EXAMPLE:
;       IDL> ; Define a prefix and turn on error messages
;       IDL> caError, prefix='My program', /ON  
;       IDL> ; Fetch the last error message
;       IDL> caError, err_string
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers
;	June 28, 1995	
;-
    common ezca_common

    if (n_elements(prefix) eq 0) then prefix=''

    if (keyword_set(on)) then begin
        t = call_ezca('ezcaIDLAutoErrorMessageOn')
    endif

    if (keyword_set(off)) then begin
        t = call_ezca('ezcaIDLAutoErrorMessageOff')
    endif

    if (keyword_set(print)) then begin
        p = [byte(prefix), 0B]
        t = call_ezca('ezcaIDLPerror', p)
    endif

    if (n_params() ne 0) then begin
        p = [byte(prefix), 0B]
        err_string = bytarr(132)
        t = call_ezca('ezcaIDLGetErrorString', p, err_string)
        if (ingroup eq 0) then err_string = string(err_string)
    endif

end


;******************************************************************
; added function by cha
;******************************************************************
;
;
FUNCTION caVersion
;+
; NAME:
;       caVersion
;
; PURPOSE:
;       This function returns the string of current version information 
;       about ezcaIDL 
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       string = caVersion()
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       Return the string which gives the version information about 
;       ezcaIDL, ezca, Ezca, and EPICS base verion number.
;
; PROCEDURE:
;	This routine uses Ezca_version() from the Ezca library.
;
; EXAMPLE:
;       IDL> print, caVersion()
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha   Dec, 1995
;-
;
	str='                                                                 '
        ln = call_ezca('EzcaVersion',str)
	str = 'ezcaIDL 2.0 '+ strmid(str,0,50)
        return,str
END

PRO caVersion, help=help
        print,' '
        print,"caVersion() - "
        print,'         to get ezcaIDL version information which includes 
	print,'         ezca, Ezca, and EPICS base version
        print,'  e.g.'
        print,'        str = caVersion()'
        print,' '
END

;
;
;
PRO caInit,flag, help=help , print=print
;+
; NAME:
;	caInit
;
; PURPOSE:
;	This routine sets the channel access timeout used by list array 
;       functions defined in Ezca library.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caInit [,flag] [,help=help]
;
; INPUTS:
;	flag:	Optional flag, if set to -1  Ezca library default timeout
;               settings will be used.
;
; KEYWORD PARAMETERS:
;     HELP:  If ,/HELP is set, on-line help will be displayed.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine set the channel access timeout values used in the 
;       Ezca library.  This routine set the timeout to 3 seconds for 
;       lists of process variables, and sets the timeout for 
;       ca_pend_event to 0.001 second. 
; 
;       If a value of -1 is specified for the flag, the default value
;       of 10 seconds for lists of process variables will be used.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;	This routine uses Ezca_init() from the Ezca library.
;
; EXAMPLES:
;       IDL> caInit
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha     Dec, 1995
;-
        if keyword_set(help) then goto, help1
        int = 0
        if keyword_set(print) then int = 1
	if n_elements(flag) then int = -1
        ln = call_ezca('EzcaInit',fix(int))
        return

; help on caInit
help1:
        print,' '
        print,"caInit [,flag]   - "
	print,'
	print,'   If flag is not set, the default timeout used by list 
	print,'   array  functions  
        print,'         set 1.0 second for single process variables'
        print,'         set 3.0 second for lists of process variables'
        print,'         set .001 second for ca_pend_event'
	print,'
	print,'   If flag is set to -1, the default timeout used by list 
	print,'   array  functions  
        print,'         set 5.0 second for single process variables'
        print,'         set 10.0 second for lists of process variables'
        print,'         set .001 second for ca_pend_event'
        print,'  e.g.'
        print,'         caInit'
        print,' '
END

;
;
;
PRO caPendEvent, time=time, help=help
;+
; NAME:
;	caPendEvent
;
; PURPOSE:
;	This function causes the Ezca to call channel access ca_pend_event
;       function.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	caPendEvent [,time=0.001] [,help=help]
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       TIME:  This keyword parameter is used to reset the timeout in seconds
;              used by ca_pend_event in Ezca library.
;
;       HELP:  If ,/HELP is set, on-line help will be displayed.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine sets the timeout for event monitor routines used in 
;       Ezca library and calls the ca_pend_event.
;
; RESTRICTIONS:
;	Positive time must be used.
;
; PROCEDURE:
;	This routine uses Ezca_setPendTime() from the Ezca library.
;
; EXAMPLES:
;       IDL> caPendEvent, time=0.0001 
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha      Dec, 1995	
;-
        if keyword_set(help) then goto, help1
        if (n_elements(time) ne 0) then begin
           ln = call_ezca('EzcaSetPendTime',3,float(time))
        endif
        ln = call_ezca('EzcaPendEvent')
        return

help1:
        print,' '
        print,"caPendEvent,[time=time]  - "
        print,'        to call channel access routine ca_pend_event which
        print,'        does the callbacks to event monitor routines'
        print,''
        print,'        Use the time=time keyword to set the time out for ca_pend_event'
	 print,'        This time will be used from then on in all ca_pend_event calls.'
        print,''
        print,' e.g.'
        print,"        caPendEvent, time=0.001"
        print,' '

END

;
;
;
PRO caPendIO, time=time, list_time=list_time, help=help
;+
; NAME:
;	caPendIO
;
; PURPOSE:
;	This routine sets the timeout used by ca_pend_io in array get/
;       put used in Ezca library.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       caPendIO, time=time, list_time=list_time
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;	TIME:   Use the TIME=time keyword to set the timeout waiting for
;               channel access I/O for single process variable name.
;
;	LIST_TIME: Use the LIST_TIME=list_time keyword to set the timeout 
;               waiting for channel access I/O for a list of PV names.
;
;       HELP:   If ,/HELP is specified, on line help will be displayed.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       These times will be used in array get/put from then on in all
;       ca_pend_io calls in Ezca library.
;
; RESTRICTIONS:
;       Positive real times should be used in those keywords.
;
; PROCEDURE:
;	This routine uses Ezca_setPendTime() from the Ezca library. 
;
; EXAMPLES:
;       IDL> caPendIO, time=0.1, list_time=3.
;       IDL> caPendIO, /help
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha      Dec, 1995	
;-
        if keyword_set(help) then goto, help1
        if (n_elements(time) ne 0) then begin
           ln = call_ezca('EzcaSetPendTime',1,float(time))
        endif
        if (n_elements(list_time) ne 0) then begin
           ln = call_ezca('EzcaSetPendTime',2,float(list_time))
        endif
;        ln = call_ezca('EzcaPendIO')
        return

; help on caPendIO
help1:
        print,' '
        print,"caPendIO, time=time, list_time=list_time  - "
        print,'        to call channel access routine ca_pend_io.
        print,''
        print,'        Use the time=time keyword to set the time out for
        print,'         ca_pend_io calls for single process variables.
        print,'         Use the list_time keyword to set the time out for
        print,'         lists of process variables.
        print, ''
        print,'        These times will be used in array get/put from then 
	print,'        on in all ca_pend_io calls.'
        print,''
        print,' e.g.'
        print,'        caPendIO, time=0.1, list_time=3.'
        print,' '

END


;
;
FUNCTION caTimeStamp, name 
;+
; NAME:
;	caTimeStamp
;
; PURPOSE:
;       This function returns the time stamp of corresponding value for
;       the specified record name.
;
; CATEGORY:
;	EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;	string = caTimeStamp(pvname)
;
; INPUTS:
;       pvname:	The name of the process variable for which the timestamp is to
;               be returned.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;	string:  The function returns the time stamp string for the requested
;                PV name. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;	This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced. 
;
; RESTRICTIONS:
;       Only single PV name is allowed in input.
;
; PROCEDURE:
;	This routine uses Ezca_timeStamp() from the Ezca library.
;
; EXAMPLES:
;       IDL> print,caTimeStamp('chademoai1')
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha      Dec, 1995	
;-
on_error,2              ; Return to caller IF an error occurs
        val = string(make_array(33, /byte, value=32))
        ln = call_ezca('EzcaTimeStamp',string(name),val)
        IF ln NE 0 THEN print,'Error: Irrelavent return value'
        val = strmid(val,0,21) + '        '
        return, val

END

; help on caTimeStamp
PRO caTimeStamp,help=help
        print,' '
        print,"caTimeStamp('name') - "
        print,'         this function returns the time stamp of corresponding value'
        print,'         for the specified record name.'
        print,''
        print,' e.g.'
        print,"         print,caTimeStamp('chademoai1')"
        print,' '
END
;
;
;
FUNCTION caSearch, name
;+
; NAME:
;	caSearch
;
; PURPOSE:
;       This function searches for a list of process variable names.
;       It returns 0 if successful, returns -1 if failed.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caSearch(pvname)
;
; INPUTS:
;      pvname: The variable for a list of process variables for which the 
;              channel access search to be done.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time pvnames has been referenced.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       This routine uses Ezca_search_list() from the Ezca library.
;
; EXAMPLES:
;       IDL> print,caSearch('chademoai1')
;       IDL> x = ['chademoai1','chademoai2']
;       IDL> status = caSearch(x)
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;      04-11-96   bkc   Fix typo error names to name
;-
on_error,2              ; Return to caller IF an error occurs
        no = n_elements(name)
; check for null string first
for i=0,no-1 do begin
	if strlen(name(i)) lt 1 then name(i) = ' '
	end
        ln = call_ezca('EzcaSearchList',fix(no),string(name))
;       IF ln NE 0 THEN print,'Error: Irrelavent return value'
        return, ln
END

; help on caSearch
PRO caSearch,help=help
        print,' '
        print,"caSearch(['name1','name2',...]) - "
        print,'         a channel access function searches for one or more'
        print,'         process variable names. It returns 0 if succeeded,'
	print,'         returns -1 if failed on any one from the list.'
        print,''
        print,' e.g.'
        print,"         x = ['chademoai1','chademoai2']"
        print,"         print,caSearch(x)"
        print,' '
END

;
;
FUNCTION caGetError, name, x
;+
; NAME:
;	caGetError
;
; PURPOSE:
;       This function get CA return codes for a list of process variable names.
;       Return code can be 0 or -1, 0 for success, -1 for failure.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caGetError(Pvname,Err)
;
; INPUTS:
;      Pvname: The variable for a list of process variables for which the 
;              channel access return code to be checked.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       Err:  The corresponding return code(s) for the Pvname(s) are returned.
;             Returns array of 0 or 1. 0 indicates success, 1 indicates failed. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time pvnames has been referenced.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       This routine uses Ezca_get_error_array() from the Ezca library.
;
; EXAMPLES:
;       IDL> print,caGetError('chademoai1')
;       IDL> x = ['chademoai1','chademoai2']
;       IDL> status = caGetError(x)
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;-
        no = n_elements(name)
; check for null string first
for i=0,no-1 do begin
	if strlen(name(i)) lt 1 then name(i) = ' '
	end
        x = make_array(no,/long)
        ln = call_ezca('EzcaGetError',fix(no),long(x),string(name))
        return,ln 
END

; help on caGetError
PRO caGetError,help=help
        print,' '
        print,"caGetError(name,err) - "
        print,'         this function returns the status of last channel access'
	print,'         call from Ezca library. 
	print,'         It returns 0 if OK, returns  -1 if error occured.'
	print,' INPUT:
	print,'      name -   single or a list of PV names
	print,'
	print,' OUTPUT:
	print,'      err  -   single or a list of error codes for the PV names
	print,'
        print,' e.g.'
        print,"         st = caGetError('idl_test:wf1',err)"
        print,' '
END

;
;
PRO caGetArray,help=help
print,' '
print,"status = caGetArray(names,pdata,max=no,type=i,/TYPE,/EVENT) "
print,'
print,'  This function returns 0 if everything went OK or -1 
print,'  if something went wrong during the get. 
print,'
print,'  The names can be a list of PV names or a single PV name.
print,'  The pdata argument returns the array of data obtained.
print,'  The keyword max= n is used to specify the maximum element of
print,'  data to be returned for each record in the list.'
print,'  The keyword type=i is used to specify the IDL type of
print,'  data to be returned for each record in the list.'
print,'
print,'  If both max and type are not specified, and single PV is
print,'  entered the native data array is returned for the PV. 
print,'
print,'  INPUT:    names      -  A list of PV names
print,'
print,'  OUTPUT:
print,'           pdata(max,noNames)
print,'
print,'                          Returns the composite data array
print,'                          of user specified type, noNames is
print,'                          the no of pvnames in the names list
print,'
print,'  KEYWORD:
print,'            max=no     -  (Default to 1)
print,'                          Maximum number of datas to be returned
print,'                          for each requested waveform record.
print,'                          If the no is greater than the native
print,'                          count, zeros will be padded.
print,'                          If not specified, only the first value
print,'                          is returned for a list of PVs.
print,''
print,'            type=i     -  (Default to 5)
print,'                          IDL data type to be returned if not
print,'                          specified the double is assumed
print,'                                 1 - byte    2 - short
print,'                                 3 - long    4 - float
print,'                                 5 - double  7 - string
print,'
print,'             /TYPE     -  Instead of type=i a user can use
print,'                          the type keyword directly, the type
print,'                          keyword supercedes the type=i specification
print,'
print,'                                 /double
print,'                                 /float
print,'                                 /string
print,'                                 /long
print,'                                 /short
print,'                                 /byte
print,'
print,'            /EVENT     - If specified use the ca_array_get_callback
print,'                         otherwise use the ca_array_get
print,'
print,'            /PRINT     - Only if this keyword is specified, then every 
print,'                         channel not found will be printed.
print,''
print,' e.g.'
print,"         names=['chademowf7','chademowf8']
print,"         st = caGetArray(names,pdata)"
print,"         st = caGetArray(names,pdata,max=10,/float)"
print,"         st = caGetArray(names,pdata,max=10,type=4)"
print,' '
END

PRO caGetTypeCount,help=help
print,''
print,"status = caGetTypeCount(names,types,counts,idl_types) "
print,'
print,'  This function return 0 if succeeds, -1 if failed. '
print,'
print,'  INPUT:   names      -   A list of pvnames
print,'
print,'  OUTPUT:
print,'           types      -   Returns the native data types
print,'                          for the PV names
print,'
print,'           counts     -   Returns the native element counts 
print,'                          for the PV names
print,'
print,'           idl_types -   Returns the corresponding IDL data types
print,'                          for the PV names
print,'
print,' e.g.'
print,"         names=['chademowf7','chademowf8']
print,"         st = caGetTypeCount(names,types,counts,idl_types)"
print,' '
END

;
; this routine gives type, and count array for the requested name array
;
FUNCTION caGetTypeCount, name,type,count,wave_type
on_error,2		; Return to caller IF an error occurs
	no = n_elements(name)
	type = lonarr(no)
	wave_type = lonarr(no)
	count= lonarr(no)
	ln = call_ezca('EzcaGetArrayTypeCount',no,type,count,string(name))

	for i=0,no-1 do begin
        ca_type = type(i)
        case ca_type of
        0: wave_type(i) = 7
;       0: wave_type(i) = 1
        1: wave_type(i) = 3
        2: wave_type(i) = 4
        3: wave_type(i) = 2
        4: wave_type(i) = 1
        5: wave_type(i) = 3
        6: wave_type(i) = 5
	else: print,'Warning: PV name "', name(i), '" not found!'
        endcase
	end

	return,ln
END

FUNCTION caGetArray,names,pdata,max_no=max_no,type=type, $
	event=event,double=double,string=string,float=float,long=long, $
	short=short,integer=integer,byte=byte , print=print
;+
; NAME:
;       caGetArray
;
; PURPOSE:
;       This function reads values for a list of Channel Access process 
;       variable. It returns 0 if successful, returns -1 if failed.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caGetArray(names,pdata,max=no,type=i,/TYPE,/EVENT)
;
; INPUTS:
;       names:	The variable for a list of channel access PV names for which 
;               the array of data is to be returned.
;
; KEYWORD PARAMETERS:
;      MAX:    Default to 1 for a list of PV names. If more than one values
;              to be returned for a list of array type PV names, this keyword
;              must be specified. If the `no' specified is greater than the
;              native count, zeros will be padded in the output array.
;
;              If only one PV name is input, then caGetArray returns 
;              the native element count for the process variable. Setting 
;              MAX to a number less than the native count this will cause 
;              caGetArray to return only the first MAX values for the PV.
;
;      TYPE:   This keyword specifies the IDL data type to be returned by 
;              the output array. If not specified, it defaults to 5, i.e. 
;              double precision type of data will be returned by the 
;              output array.
;
;                1 - byte      2 - short       3 - long      4 - float
;                5 - double    7 - string
;
;     /TYPE    Instead of type=i a user can use the IDL data type keyword
;              directly, the data type keyword supercedes the type=i 
;              specification. Valid types given below
;
;                            /double
;                            /float
;                            /string
;                            /long
;                            /short
;                            /byte
;
;    /EVENT    If specified use the ca_array_get_callback otherwise use
;              the ca_array_get
;
; OUTPUTS:
;     pdata:  The output variable,  pdata(max,noNames), returns the data 
;             array for the requested list of PV names. The `max' is the no
;             of values specified by the keyword MAX, the `noNames' is the
;             number of PV names in the input variable names.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced.
;
; RESTRICTIONS:
;       Only one type of data can be requested for a list of PV names. 
;
; PROCEDURE:
;	This routine uses Ezca_getArray() from the Ezca library.
;
; EXAMPLES:
;       Three examples are given below. 
;       The first caGetArray call returns only the first value for each PV 
;       name, the second and third caGetArray call both returns 10 float 
;       values for each PV name
;
;       IDL> names=['chademowf7','chademowf8']
;       IDL> st = caGetArray(names,pdata)
;       IDL> st = caGetArray(names,pdata,max=10,/float)
;       IDL> st = caGetArray(names,pdata,max=10,type=4)
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;      04-11-96     bkc    If array get failed, only the pvnames not found are 
;                          reported
;      04-22-96     bkc    Replace caError by caGetError 
;-
num = 1			; default to 1 value 
wave_type = 5		; default to double
if keyword_set(string) then type=7
if keyword_set(double) then type=5
if keyword_set(float) then type=4
if keyword_set(long) then type=3
if keyword_set(short) then type=2
if keyword_set(byte) then type=1

no = fix(n_elements(names))

; check for null string first
for i=0,no-1 do begin
	if strlen(names(i)) lt 1 then names(i) = ' '
	end
if no eq 1 then begin
;	st = caGetCountAndType(names, ct, ty)
	st = caGetTypeCount(names, ty, ct, wty)
	if st eq -1 then begin
		return,-1
		end
	num = ct(0)
	ca_type = ty(0)
	wave_type = wty(0)
	end

if n_elements(type) gt 0 then wave_type = type 
if n_elements(max_no) gt 0 then num = max_no

	case wave_type of             ; wave_type
1:	begin
	ca_type = 4L ; Byte or char
	pdata = make_array(num,no,/byte)
	end
;2:	ca_type = 3L ; Enum
2: begin
	ca_type = 3L ; Enum
	pdata = intarr(num,no)
	end
3:	begin
	ca_type = 5L ; Long
	pdata = make_array(num,no,/long)
	end
4: begin
	ca_type = 2L ; Float
	pdata = make_array(num,no,/float)
	end
5: begin
	ca_type = 6L ; Double
	pdata = make_array(num,no,/double)
	end
7: begin
	ca_type = 0L ; String
;	ca_type = 4L ; String
	pdata = make_array(40,num,no,/byte)
	end
else: begin
	print,'Not supported !!!'
	return,-1
	end ; Anything else
	endcase

if keyword_set(event) then $
	ln = call_ezca('EzcaGetArrayEventValues',ca_type,long(num),no, $
		pdata,string(names)) $
else $
	ln = call_ezca('EzcaGetArrayValues',ca_type,long(num),no, $
		pdata,string(names))
	IF ln NE 0 THEN begin
		if keyword_set(print) then begin
		ln = caGetError(names,p1)
		for i=0,no-1 do begin
		if p1(i) ne 0 then print,'Error: caGetArray failed on ',names(i)
		end
		endif else print,'Warning: some PV not found in caGetArray'
		end
	if wave_type eq 7 then pdata=string(pdata)
	return,ln
END

PRO caPutArray,help=help
print,' '
print,"status = caPutArray(names,pdata,/event) "
print,'
print,'  This function writes the pdata array for the requested names.
print,'  It returns 0 if succeed, else return -1.
print,'
print,'  INPUT:    names      -  A list of PV names
print,'            pdata      -  The input data array corresponding to the PV names
print,'                          to be written to IOC.
print,'
print,'  KEYWORD:
print,'            /EVENT     -  If specified use the ca_array_put_callback
print,'                          otherwise use the ca_array_put
print,' Examples:
print,'
print,'       In the following example write values [1,2,3] to two waveform records
print,'       chademowf2,  and chademowf5
print,'
print,"        x = ['chademowf2','chademowf5']
print,'        y = make_array(3, 2)
print,'        y(0,0) = [1,2,3]
print,'        y(0,1) = [1,2,3]
print,'        print,caPutArray(x,y)
print,'        print,caGetArray(x,pd,max=10)
print,'        print,pd
print,''
END

FUNCTION caPutArray,names,pdata,string=string,event=event 
;+
; NAME:
;       caPutArray
;
; PURPOSE:
;       This function writes an array of data to a list of Channel Access 
;       process variable. It returns 0 if successful, else retuns -1.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caPutArray(pvname, pdata, /event)
;
; INPUTS:
;       pvname:	The variable specifies a list of process variables for 
;               which the input array of data is to be written to IOC.
;       pdata:  Input data array. The data array must be consistant with 
;               the number of PV names defined in the pvname.
;
; KEYWORD PARAMETERS:
;      EVENT:   If specified use the ca_array_put_callback otherwise
;               use the ca_array_put.
;
; OUTPUTS:
;      None.
;
; COMMON BLOCKS:
;      None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced.
;
; RESTRICTIONS:
;       Thus, it is the user's responsibility to make sure the adequate
;       pdata is provided for the pvname.
;
; PROCEDURE:
;	This routine uses Ezca_putArray() from the Ezca library.
;
; EXAMPLES:
;       In the following example write a string value '11' to two PV
;       names: chademomask1.VAL  and chademoai2.VAL 
;
;       IDL> x = ['chademomask1', 'chademoai2']
;       IDL> y = make_array(1, 2, /string)
;       IDL> y(0) = '11'
;       IDL> y(1) = '11'
;       IDL> status = caPutArray(x,y)
;
;       In the following example write values [1,2,3] to two waveform records
;       names: chademowf2  and chademowf5
;
;       IDL> x = ['chademowf2','chademowf5']
;       IDL> y = make_array(3, 2)
;       IDL> y(0,0) = [1,2,3]
;       IDL> y(0,1) = [1,2,3]
;       IDL> print,caPutArray(x,y)
;       IDL> print,caGetArray(x,pd,max=10)
;       IDL> print,pd
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;-
no = fix(n_elements(names))
; check for null string first
for i=0,no-1 do begin
	if strlen(names(i)) lt 1 then names(i) = ' '
	end
if no eq 1 then begin
	st = caGetTypeCount(names, ty, ct, wty)
	num = ct(0)
	wave_type = wty(0)
end

; check name and pdata consistance

s = size(pdata)
ns = n_elements(s)
wave_type = s(ns-2)
if s(0) eq 2  and no eq 1 then begin
	print,'Error: caPutArray(names,pdata)  multiple name expected'
	return,-1
	end
if s(0) eq 2  and s(2) ne no then begin
	print,'Error: caPutArray(names,pdata)  names and pdata dimension error'
	return,-1
	end
if s(ns-1) gt 1 then num = s(1) $ ; array detected
else num = 1

	case wave_type of             ; wave_type
1:	begin
	ca_type = 4L ; Byte or char
	end
2: begin
	ca_type = 3L ; Enum
	end
3:	begin
	ca_type = 5L ; Long
	end
4: begin
	ca_type = 2L ; Float
	end
5: begin
	ca_type = 6L ; Double
	end
7: begin
;help,pdata,num,no
	pd= make_array(40,num,no,/byte)
	for i=0,no-1 do begin
	for j=0,num-1 do begin
	len = strlen(pdata(i))
	if len gt 0 then $
	pd(0:len-1,j,i)=byte(pdata(j+i*num))
	end
	end
	ca_type = 0L ; String
;help,pd
	pdata=pd
	end
else: begin
	print,'Not supported !!!'
	return,-1
	end ; Anything else
	endcase

if keyword_set(event) then $
	ln = call_ezca('EzcaPutArrayEventValues',ca_type,long(num),no, $
		pdata,string(names)) $
else $
	ln = call_ezca('EzcaPutArrayValues',ca_type,long(num),no, $
		pdata,string(names))
	return,ln
END




PRO caScan,help=help
	N=''
	print,''
	print,'caScan(name,pvnames,nonames,npts,vals,op_keyword,max=no) -
	print,'      Add/Get/Zero/Clear monitor of the specified name and pvnames
	print,'
	print,'      Input:    
	print,'       name    -   pvname for triggering scanning'
	print,'       pvnames -   a list of detector pvnames monitored by the trigger name'
	print,'
	print,'      Output:    
	print,'         nonames - no of detector pvnames triggered by name 
	print,'         npts    - no of points so far detected for each pvname
	print,'                   if /GET option is specified
	print,'         vals    - array holds the detected values
	print,'                   if /GET option is specified
	print,'
	print,'     op_keyword:
	print,'
	print,'        /ADD       add monitor
	print,'                      Return [npts nonames] for success
	print,'                      Return -1 if failed
	print,'                      Return 1 if old monitor already existed
	print,'        /CLEAR     clear monitor
	print,'                      Return 0 for success, -1 for failure
	print,'         /GET      get scan array of monitor values back
	print,'                      Return -1 for failure 
	print,'                      Return 1 if scan is not triggered yet
	print,'                      Return >1 if real data detected
	READ,'MORE... ',n
	print,'        /ZERO      zero allocated space before taking new scan data
	print,'                      Return 0 for success, -1 for failure
	print,'        max=no     specifies the max number of monitor values to be returned'
	print,'                      If the trigger name is not a scan record,
	print,'                      the max=no must be provided.
	print,' e.g.'
        print,"         print,caScan('name',pvnames,/add)"
        print,"         print,caScan('name',pvnames,nonames,npts,vals,/get)"
        print,"         print,caScan('name',pvnames,/zero)"
        print,"         print,caScan('name',pvnames,/clear)"
	print,'
	print,"   Non-Scan record type trigger"
	print,'
        print,"         print,caScan('name',pvnames,/add,max=100)"
        print,"         print,caScan('name',pvnames,nonames,npts,vals,/get,max=100)"
        print,"         print,caScan('name',pvnames,/zero,max=100)"
        print,"         print,caScan('name',pvnames,/clear,max=100)"
        print,' '
END
;
;
;
FUNCTION caScan, name, pvnames, nonames, npts, vals, add=add, get=get, clear=clear, $
		zero=zero, max=max
;+
; NAME:
;       caScan
;
; PURPOSE:
;       This function provides add/get/zero/clear monitor features on a 
;       scan record and a set of PV names.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caScan(name,pvnames,nonames,npts,vals,op_keyword,max=no)
;
; INPUTS:
;       name:   The name of the process variable which has control of 
;               triggering scan, e.g. the scan record name.
;       pvnames: A list of detector process variables for which the values
;                are to be monitored by the trigger name.
;
; KEYWORD PARAMETERS:
;      ADD:     Set this flag /ADD  to add a complete set of monitor for 
;               name and pvnames. 
;               Return 0 if successful, -1 if failed, 1 if old monitor 
;               already existed. If succeeds, the output variable npts
;               is set to the number of data to be detected, and the
;               nonames is set to the number of PVs in pvnames.
;
;      CLEAR:   Set this flag /CLEAR to clear the monitor set by add.
;               Return 0 if successful, -1 if failed.
;
;      GET:     Set this flag /GET to get scan array of monitor values back.
;               Return -1 if failed, return 1 if scan is properly set up but
;               not triggered yet, return >1 if real data detected. 
;               If succeeds, the npts is set to the number of data so far 
;               detected.
;
;      ZERO:    Set this flag /ZERO to zero the allocated space.
;               Return 0 for success, -1 for failure.
;
;      MAX:     Specifies the max number of monitor values to be returned.
;               If the trigger name is not a scan record, the max=no must
;               be provided for this function.
;
; OUTPUTS:
;     nonames:  This variable returns the number of PVs in pvnames.
;     npts:     This variable returns the current number of data points 
;               detected by the scan record.
;     vals:     This detector data array buff, vals(nonames,max), stores   
;               the detected data so far captured.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       This routine uses Ezca_scanAddMonitor(), Ezca_scanClearMonitor(),
;       Ezca_scanGetMonitor(), and Ezca_scanZeroMonitor() from the Ezca
;       library.
;
; EXAMPLES:
;      For scan record type triggered scan
;
;       IDL> print,caScan('name',pvnames,/add)
;       IDL> print,caScan('name',pvnames,nonames,npts,vals,/get)
;       IDL> print,caScan('name',pvnames,/zero)
;       IDL> print,caScan('name',pvnames,/clear)
;
;      For non-Scan record type triggered scan
;
;       IDL> print,caScan('name',pvnames,/add,max=100)
;       IDL> print,caScan('name',pvnames,nonames,npts,vals,/get,max=100)
;       IDL> print,caScan('name',pvnames,/zero,max=100)
;       IDL> print,caScan('name',pvnames,/clear,max=100)
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;
;-
on_error,2		; Return to caller IF an error occurs
	if n_elements(name) gt 1 then begin
		print,'Error: only one name string is allowed'
		return, -1
		end	
        add = keyword_set(add)
        get = keyword_set(get)
        clear = keyword_set(clear)
        zero = keyword_set(zero)
	if ((add or get or clear or zero) eq 0 ) then begin
		print,'Error: Keyword add/get/clear/zero is not specified.'
		return,-1
		end
; clear
	if (clear) then begin
	nonames=fix(n_elements(name))
	ln = call_ezca('EzcaMonitorScan_Clear',nonames,string(name))
;	IF ln NE 0 THEN print,'Error: caScan,/clear failed on ',name
	return,ln
	end
; zero 
	if (zero) then begin
	nonames=fix(n_elements(name))
	ln = call_ezca('EzcaMonitorScan_Zero',nonames,string(name))
	IF ln NE 0 THEN print,'Error: caScan,/Zero failed on ',name
	return,ln
	end

; set npts, nonames
	nonames=fix(n_elements(pvnames))
	if (n_elements(max) eq 0) then begin
		st = caget(name+'.NPTS',npts)
		if npts le 0 then return, -1
	endif else npts = max 
	if (n_elements(max) ne 0) then npts = fix(max > npts)
	npts= npts +1
; add
	if (add) then begin 
	ln = call_ezca('EzcaMonitorScan_Add',npts,nonames,string(name),string(pvnames))
	IF ln EQ -1 THEN print,'Error: caScan/add failed on ',name
	IF ln EQ 1 THEN print,'Error: old monitor on ',name
	return,ln
	end
; get
	if (get) then begin
	vals = make_array(nonames,npts,/double)
	ln = call_ezca('EzcaMonitorScan_Get',npts,nonames,vals,string(name))
	return,ln
	end

END

;
;
;
PRO caMonitor,help=help
; help on caMonitor
        N=''
        print,' '
        print,"caMonitor(names,vals,num,overflow,operation_keyword,type_keyword,maxqueue=no) - "
        print,'         Add/Get/Check/Clear monitor of the specified record name.'
        print,'         Currently this array monitor is targetted for scalar type of PVs.'
        print,''
	print,'  INPUT:   names      -   A single or a list of pvnames
	print,'
	print,'  OUTPUT:  vals       -   Returns the array of data if either keyword /GET 
	print,'                          or /CHECK is specified
	print,'           num        -   Returns the real number of data in the vals array
	print,'                          for the /QUEUE mode

	print,'           overflow   -   Returns the buffer full indicator for /the QUEUE mode
	print,'                          0 - vals queue buff is not full
	print,'                          1 - vals queue buff is full
	print,'
        print,'         Operation_keyword:'
        print,'             /ADD
        print,'                Add CA monitor for the specified name.'
        print,'                Return 0 for success, -1 for failure.'
        READ,'MORE... ',n
        print,'             /CLEAR
        print,'                Clear CA monitor and free space for the  specified name.'
        print,'                Return 0 for success, -1 for failure.'
        print,'             /CHECK
        print,'                Check whether any event happened for the list. 
        print,'                Return 0 for success, -1 for failure.'
	print,'                The output vals array variable contains event flags,
	print,'                0 -  no new event detected
	print,'                1 -  new value change event detected 
        print,'             /GET
        print,'                Get monitor values back for the specified name.'
        print,'                Different values are returned for different type of monitor.
        print,'                If no queue type specified, the vals array returns the current
	print,'                values for the PVs. 
        print,'                If the keyword /QUEUE is specified, in addition of
	print,'                vals array both the number of data and the buffer full
	print,'                indicator are returned. 
        print,''
        print,'         Type_keyword:'
        print,'             /QUEUE [, MODE=i ]
        print,'                Queue the value changes for the monitored channel until
        print,'                the user gets them. '
        READ,'MORE... ',n
        print,'                The keyword MODE is optional, it defaults to 1.'
        print,'                The MODE can be 1/2/3.  The MODE=1 or 2 will fill the '
        print,'                buffer with new values until it is fulled. '
        print,'                If MODE=1, the /GET will clear the QUEUE buff.
        print,'                If MODE=2, the /GET will not clear the QUEUE buff.
        print,'                The MODE=3 keeps the most current MAXQUEUE values'
        print,'                in the queue buffer.'
        print,'                The MAXQUEUE must be provided if /QUEUE mode is specified .'
        print,''
        print,'         MAXQUEUE=no
        print,'                This specifis the size of array for queue values.
        print,'                Data may be lost if size specified is too small, and the
        print,'                data coming too fast before a user is able to cash them.
        print,''
        print,' Examples'
        print,''
        print,"         print,caMonitor('chademoai1',/add)
        print,"         print,caMonitor('chademoai1',vals,/get)
        print,"         print,caMonitor('chademoai1',/clear)
        print,''
        print,"         print, caMonitor('chademoai1',/add,/queue,maxqueue=100)"
        print,"         print, caMonitor('chademoai1',vals,num,overflow,/get,/queue,maxqueue=100)"
        print,"         print, caMonitor('chademoai1',/clear,/queue)"
        print,' '
END



FUNCTION caMonitor, name, vals, num, overflow, $
	add=add, get=get, clear=clear, queue=queue, $
       check=check, zero=zero, maxqueue=maxqueue, mode=mode

;+
; NAME:
;       caMonitor
;
; PURPOSE:
;       This function provides Add/Get/Check/Clear monitor features on a 
;       single PV or a list of PV names.
;
; CATEGORY:
;       EPICS Channel Access Interface
;
; CALLING SEQUENCE:
;       Status = caMonitor(name, vals, num, overflow, op_keyword,type_keyword, max=no)
;
; INPUTS:
;       name:	The variable for a PV or a list of PV names.
;
; KEYWORD PARAMETERS:
;     ADD:      Set /ADD to add CA monitor for the specified name.
;               Return 0 for success, -1 for failure.
;
;     CLEAR:    Set /CLEAAR to clear CA monitor for the specified name.
;               Return 0 for success, -1 for failure.
;
;     CHECK:    Set /CHECK to check for new event for the specified name.
;               Return 0 for success, -1 for failure.
;               The ouput variable 'vals' contains the return event flags.
;                 0 - flags no new event detected
;                 1 - flags new event detected
;
;     GET:      Set /GET to get monitor values back for the specified name.
;               Different type of monitor returns different vals array.
;               If non queue type monitor set, the vals array returns a 
;               single value for each PV in the name variable.
;               If the kewword /QUEUE is specified, in addition of vals,
;               both num and overflow variables are also returned.
;
;     QUEUE:	Set /QUEUE flag to queue the value change event for the 
;               monitored channel until the user gets them. 
;
;     MAXQUEUE: The MAXQUEUE=no must be specified if /QUEUE is specified.
;
;     MODE:     This flag indicates what type of monitor queue is desired.
;               The MODE=i where i can be 1/2/3, it defaults to 1. The 
;               MODE=1 or 2 monitor fills the QUEUE buff until it is fulled. 
;               If MODE=1, the /GET will clear the QUEUE buff.
;               IF MODE=2, the /GET will not clear the QUEUE buff.
;               The MODE=3 uses the circulat buffer, it keeps the most 
;               current MAXQUEUE values in the queue buffer. 
;
; OUTPUTS:
;     vals:     Returns the array of data if either keyword /GET or /CHECK
;               is specified
;
;     num:      Returns the real number of data in the vals array for the 
;               /QUEUE mode
;
;     overflow: Returns the buffer full indicator for the /QUEUE mode
;               0 - vals queue buff is not full
;               1 - vals queue buff is full
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This routine will causes a channel access search to take place if 
;       this is the first time this process variable has been referenced.
;
; RESTRICTIONS:
;       All the PV are monitored as double precision in this function.
;       For getting the monitored queue array, only a single PV name can
;       be specified.  For non queue type monitor, only the first value
;       for a PV can be returned by this function.  Use caGet to get 
;       array type of values back.
;
; PROCEDURE:
;	This routine uses Ezca_monitorArrayAdd(), Ezca_monitorArrayGet(),
;       Ezca_monitorArrayCheck(), Ezca_monitorArrayClear(), Ezca_queueAdd(),
;       Ezca_queueGet(),Ezca_queueZero(), and Ezca_queueClear() 
;       from the Ezca library.
;
; EXAMPLES:
;    Single value monitor
;
;       IDL> print,caMonitor('chademoai1',/add)
;       IDL> print,caMonitor('chademoai1',vals,/get)
;       IDL> print,caMonitor('chademoai1',/clear)
;
;    Use queue array monitor with maxqueue=100
; 
;       IDL> print, caMonitor('chademoai1',/add,/queue,maxqueue=100)
;       IDL> print, caMonitor('chademoai1',vals,num,overflow,/get,/queue,maxqueue=100)
;       IDL> print, caMonitor('chademoai1',/clear,/queue)
;
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha      Dec, 1995	
;       04-12-96    bkc   Modified on line help syntax
;-
on_error,2              ; Return to caller IF an error occurs
        nvals = n_elements(name)
        queue = keyword_set(queue)
; clear
        clear = keyword_set(clear)
        if (clear) then begin
        if (queue) then begin
              ln =call_ezca('EzcaMonitorQueue_Clear',fix(nvals),name)
        endif else $
	      ln = call_ezca('EzcaClearMonitorArray',fix(nvals),name)
	return,ln
        end
;check 
	check = keyword_set(check)
        if (check) then begin
	vals = make_array(nvals,/long)
	ln = call_ezca('EzcaCheckMonitorArray',fix(nvals),vals,name)
	return,ln
	end
; zero
        zero = keyword_set(zero)
        if (zero) then begin
        nonames=fix(n_elements(name))
        ln = call_ezca('EzcaMonitorQueue_Zero',nonames,string(name))
;        IF ln NE 0 THEN print,'Error: caMonitor,/Queue,/Zero failed on ',name
        return,ln
        end

        add = keyword_set(add)
        get = keyword_set(get)
        wf = 0
        if ((add or get or clear or zero) eq 0 ) then begin
                print,'Error: Keyword add/get/clear is not specified.'
                print,'       Enter caMonitor to get help on this function'
                return,-1
                end
        if (nvals gt 1 and (queue )) then begin
                print,'Error: only one name string is allowed'
                return, -1
                end
        if queue and keyword_set(mode) eq 0 then mode = 1
;        nelems = caGetCount(name)
;        if nelems gt 1 then wf=1
; add
        if (add) then begin
;                if (wf) then begin
;                        ln = call_ezca('',fix(nvals),string(name))
;                        IF ln NE 0 THEN print,'Error: /add failed on waveform record',name
;                        return,ln
;                        end
        if (queue) then begin
        if maxqueue le 0 then begin
            print,'Error: maxqueue=n must be specified.'
            return, -1
            end
        ln = call_ezca('EzcaMonitorQueue_Add',mode,fix(nvals),long(maxqueue),string(name))
        endif else $
	ln = call_ezca('EzcaAddMonitorArray',fix(nvals),string(name))
	return,ln
        end
; get
        if (get) then begin
        if (queue) then begin
             if maxqueue lt 0 then begin
             print,'Error: maxqueue=n must be specified.'
             return, -1
             end
	vals = make_array(maxqueue,/double)
if nvals gt 1 then print,'Warning: Queue,/Get exceeds 1 pvname'
	overflow=0L
	num = long(maxqueue)
	ln = call_ezca('EzcaMonitorQueue_Get',mode,overflow,num,vals,string(name))
	endif else begin
	  vals = make_array(nvals,/double)
	  ln = call_ezca('EzcaGetMonitorArray',fix(nvals),vals,name)
	end
	return,ln
        end
END


