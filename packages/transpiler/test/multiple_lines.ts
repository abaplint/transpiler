  it("interface attribute after method call in chain", async () => {
    const abap = `
INTERFACE lif_lang.
  DATA value TYPE c LENGTH 1 READ-ONLY.
ENDINTERFACE.

INTERFACE lif_sy.
  METHODS language RETURNING VALUE(ro_language) TYPE REF TO lif_lang.
ENDINTERFACE.

CLASS lcl_lang DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_lang.
    METHODS constructor IMPORTING iv_value TYPE c.
ENDCLASS.
CLASS lcl_lang IMPLEMENTATION.
  METHOD constructor.
    lif_lang~value = iv_value.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sy DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sy.
ENDCLASS.
CLASS lcl_sy IMPLEMENTATION.
  METHOD lif_sy~language.
    ro_language = NEW lcl_lang( 'E' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cp DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA sy TYPE REF TO lif_sy.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_cp IMPLEMENTATION.
  METHOD run.
    DATA lv_langu TYPE c LENGTH 1.
    lv_langu = lcl_cp=>sy->language( )->value.
    WRITE lv_langu.
  ENDMETHOD.
ENDCLASS.`;
    const js = await runSingle(abap);
    expect(js).to.include(`.get().lif_lang$value);`);
    expect(js).to.not.include(`.get().value);`);
  });
