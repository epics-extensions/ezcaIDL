function call_Ezca, routine, p1, p2, p3, p4
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
;	Status = call_Ezca(routine, p1, p2, p3, p4)
;
; INPUTS:
;	routine:  The name of the external routine to be called
;
; OPTIONAL INPUT PARAMETERS:
;       p1:       The first parameter to be passed to the external routine
;       p2:       The second parameter to be passed to the external routine
;       p3:       The third parameter to be passed to the external routine
;       p4:       The fourth parameter to be passed to the external routine
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
;       IDL> state = caCheckMonitor('test_ao1')
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
