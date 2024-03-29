ezcaIDL is a library of routines for IDL and PV-WAVE which provides an
interface to EPICS Channel Access through the EZCA library.

ezcaIDL has been built and tested on WIN32, Linux, MacOS.

It is built as a DLL on Windows and a shareable library on Linux and MacOS.

When building on Windows the windows-x64-static architecture should be used.
This will create an ezcaIDL.dll file that is self-contained, i.e. it will contain
the code for ca and Com from EPICS base, and for ezca and EzcaScan from EPICS extensions.
This eliminates the need to set the PATH to include these directories.

The code library is structured from top to bottom as follows:
- ezcaIDL  (wrapper functions that use the calling conventions of the IDL call_external() function)
- ezca (implements Channel Access functions without requiring client callbacks)
- EzcaScan (implements some advanced Channel Access functions)
- EPICS base (Com and ca libraries implement channel access and OS-independent utility functions)

The following files are contained in the ezcaIDL directory.

- ezcaIDL.c
  - The source file for the wrapper functions between IDL/PV-WAVE and EZCA.
- ezcaIDL.pro
  - The IDL/PV-WAVE routines.
- ezcaIDLWidgets.pro
  - Routines for using channel access monitor with IDL widgets
- ezcaIDLGuide.html
  - ezcaIDL User's Guide
- ezcaIDLRef.html
  - ezcaIDL Reference Guide
- makeEzcaIDLRef.pro
  - An IDL procedure to build ezcaIDLRef.html from the documentation headers in
    ezcaIDL.PRO and ezcaIDLWidgets.pro.
- Makefile
  - File to build ezcaIDL. Used by the GNU make program.

IDL finds the shareable library at run time by one of two mechanisms.

1) First, it looks for the environment variable EZCA_IDL_SHARE.  If this envionment
variable exists it must point to the complete path to ezcaIDL.dll (Windows) or
libezcaIDL.so (all other architectures).  For example:
```
setenv EZCA_IDL_SHARE /usr/local/lib/libezcaIDL.so
```
2) If the shareable library is not found from the EZCA_IDL_SHARE environment variable
then it is searched for in the IDL search path, which is contained in the IDL system varable !IDL_PATH.
The file name searched for is the following:
```
base_!VERSION.OS_!VERSION.ARCH
```
In the above:
- base is either ezcaIDL.dll (Windows) or libezcaIDL.so (all other architectures). 
- !VERSION.OS can be "linux", "Win32", etc.  
- !VERSION.ARCH can be "x86", "x86_64", etc. 
- Execute the "help, !VERSION" command in IDL to see the values for your system. Example:
```
IDL> help, !VERSION
** Structure !VERSION, 8 tags, length=104, data length=100:
   ARCH            STRING    'x86_64'
   OS              STRING    'linux'
   OS_FAMILY       STRING    'unix'
   OS_NAME         STRING    'linux'
   RELEASE         STRING    '8.8.0'
   BUILD_DATE      STRING    'Jul 22 2020 (388929)'
   MEMORY_BITS     INT             64
   FILE_OFFSET_BITS
                   INT             64
```

Examples of file names in !IDL_PATH:
```
ezcaIDL.dll_Win32_x86_64
libezcaIDL.so_linux_x86_64
```
This mechanism can be more convenient than using EZCA_IDL_SHARE because it allows
easy switching between 32 and 64 bit versions of IDL, and does not require setting an
environment variable.  It does require either copying and renaming the files to a
location in the !IDL_PATH, or creating soft-links in the !IDL_PATH to point to the
locations of the actual shareable libraries.
