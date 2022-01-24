DATA: BEGIN OF ls_sysinfo,
        system  TYPE c LENGTH 3,
        mand    TYPE n LENGTH 3,
        aspra	  TYPE c LENGTH 1,
        sname	  TYPE c LENGTH 12,
        cattok  TYPE c LENGTH 1,
        saprl	  TYPE c LENGTH 4,
        host    TYPE c LENGTH 8,
        opsys	  TYPE c LENGTH 10,
        dbsys	  TYPE c LENGTH 10,
        datex	  TYPE c LENGTH 8,
        dcpfm	  TYPE c LENGTH 1,
        msname2	TYPE c LENGTH 40,
      END OF ls_sysinfo.

CALL FUNCTION 'CAT_PING'
  DESTINATION 'NONE'
  IMPORTING
    sysinfo = ls_sysinfo.
