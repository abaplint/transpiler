CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test.

    DATA: BEGIN OF ls_sysinfo,
            system  TYPE c LENGTH 3,
            mand    TYPE n LENGTH 3,
            aspra   TYPE c LENGTH 1,
            sname   TYPE c LENGTH 12,
            cattok  TYPE c LENGTH 1,
            saprl   TYPE c LENGTH 4,
            host    TYPE c LENGTH 8,
            opsys   TYPE c LENGTH 10,
            dbsys   TYPE c LENGTH 10,
            datex   TYPE c LENGTH 8,
            dcpfm   TYPE c LENGTH 1,
            msname2 TYPE c LENGTH 40,
          END OF ls_sysinfo.

    cl_abap_unit_assert=>assert_initial( ls_sysinfo ).

    CALL FUNCTION 'CAT_PING'
      DESTINATION 'MYTESTTEST'
      IMPORTING
        sysinfo = ls_sysinfo.

    cl_abap_unit_assert=>assert_not_initial( ls_sysinfo ).

  ENDMETHOD.
ENDCLASS.