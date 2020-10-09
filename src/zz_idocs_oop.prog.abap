*&---------------------------------------------------------------------*
*& Report zz_idocs_oop
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zz_idocs_oop.

CLASS lcx_idoc_exceptions DEFINITION INHERITING FROM cx_static_check  FINAL.

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      c_idoc_messages TYPE symsgid VALUE 'LCL_IDOC_MESSAGES',
      BEGIN OF segment_incorrect_for_idoc,
        msgid TYPE symsgid VALUE c_idoc_messages,
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF segment_incorrect_for_idoc .
    CONSTANTS:
      BEGIN OF parent_segment_not_found,
        msgid TYPE symsgid VALUE c_idoc_messages,
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parent_segment_not_found .
    CONSTANTS:
      BEGIN OF segment_child_of_segment,
        msgid TYPE symsgid VALUE c_idoc_messages,
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF segment_child_of_segment .
    DATA msgv1 TYPE syst_msgv READ-ONLY .
    DATA msgv2 TYPE syst_msgv READ-ONLY .
    DATA msgv3 TYPE syst_msgv READ-ONLY .
    DATA msgv4 TYPE syst_msgv READ-ONLY .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE clike OPTIONAL
        !msgv2    TYPE clike OPTIONAL
        !msgv3    TYPE clike OPTIONAL
        !msgv4    TYPE clike OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_idoc_exceptions IMPLEMENTATION.

  METHOD constructor.
    super->constructor( previous = previous ).

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.

    CLEAR me->textid.
    if_t100_message~t100key = COND #( WHEN textid IS INITIAL THEN if_t100_message=>default_textid
                                      ELSE textid ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_idoc_edidd_segment DEFINITION DEFERRED.

CLASS lcl_iterator DEFINITION.
  PUBLIC SECTION.
    TYPES tt_segment TYPE STANDARD TABLE OF REF TO lcl_idoc_edidd_segment.

    METHODS constructor IMPORTING it_segment TYPE tt_segment
                                  iv_start   TYPE sytabix DEFAULT 1
                                  iv_stop    TYPE sytabix OPTIONAL.

    METHODS add IMPORTING segment TYPE REF TO lcl_idoc_edidd_segment.
    METHODS remove IMPORTING segment TYPE REF TO lcl_idoc_edidd_segment.

    METHODS new_iterator RETURNING VALUE(ro_iterator) TYPE REF TO lcl_iterator.

    METHODS next RETURNING VALUE(rs_data) TYPE REF TO lcl_idoc_edidd_segment
                 RAISING   cx_sy_itab_error.

    METHODS has_next RETURNING VALUE(rv_flag) TYPE xsdboolean.
    METHODS is_empty RETURNING VALUE(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.

  PROTECTED SECTION.
    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_segment TYPE tt_segment.
ENDCLASS.

CLASS lcl_iterator IMPLEMENTATION.

  METHOD new_iterator.
    ro_iterator = NEW #( it_segment = mt_segment ).
  ENDMETHOD.

  METHOD add.
    APPEND segment TO mt_segment.
    mv_size = lines( mt_segment ).
  ENDMETHOD.

  METHOD remove.
    LOOP AT mt_segment TRANSPORTING NO FIELDS WHERE TABLE_LINE = segment.
      DELETE mt_segment.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    CLEAR mv_idx.
    mt_segment = it_segment. " shared, never changed
    mv_size = COND #( WHEN iv_stop IS INITIAL THEN lines( mt_segment )
                                              ELSE iv_stop ).
    skip( iv_start - 1 ).
  ENDMETHOD.

  METHOD next.
    skip( ).
    rs_data = mt_segment[ mv_idx ].
  ENDMETHOD.

  METHOD has_next.
    rv_flag = xsdbool( mv_idx < mv_size ).
  ENDMETHOD.

  METHOD is_empty.
    rv_flag = xsdbool( mv_size IS INITIAL ).
  ENDMETHOD.

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_idoc_edidd DEFINITION CREATE PROTECTED .

  PUBLIC SECTION.
    TYPES tt_edi_api TYPE STANDARD TABLE OF edi_iapi06.

    CLASS-METHODS create_with_data
      IMPORTING
                !iv_idoc_type      TYPE edi_idoctp
                !iv_idoc_extension TYPE edi_cimtyp
                !it_edidd          TYPE edidd_tt OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_idoc_edidd
      RAISING   lcx_idoc_exceptions.

    METHODS add
      IMPORTING is_edidd          TYPE edidd
      RETURNING VALUE(ro_segment) TYPE REF TO lcl_idoc_edidd_segment
      RAISING   lcx_idoc_exceptions.
    METHODS get_edidd
      RETURNING VALUE(rt_edidd) TYPE edidd_tt.
    METHODS get_segments
      IMPORTING iv_name            TYPE edilsegtyp OPTIONAL
      RETURNING VALUE(ro_segments) TYPE REF TO lcl_iterator.
    METHODS remove
      IMPORTING io_segment        TYPE REF TO lcl_idoc_edidd_segment
      RETURNING VALUE(rv_removed) TYPE flag.
  PROTECTED SECTION.

    TYPES:
      tt_idoc_structure_sorted TYPE SORTED TABLE OF edi_iapi06
        WITH UNIQUE KEY idoctyp cimtyp nr
        WITH UNIQUE SORTED KEY segtyp COMPONENTS segtyp.

    DATA ao_segments TYPE REF TO lcl_iterator.
    DATA at_idoc_structure TYPE tt_edi_api.
    DATA at_idoc_structure_sorted TYPE tt_idoc_structure_sorted.
    DATA av_idoc_extension TYPE edi_cimtyp.
    DATA av_idoc_type TYPE edi_idoctp.

    METHODS add_segment_do FINAL
      IMPORTING is_edidd          TYPE edidd
      RETURNING VALUE(ro_segment) TYPE REF TO lcl_idoc_edidd_segment
      RAISING   lcx_idoc_exceptions .
    METHODS constructor
      IMPORTING
                !iv_idoc_type      TYPE edi_idoctp
                !iv_idoc_extension TYPE edi_cimtyp
                !it_edidd          TYPE edidd_tt OPTIONAL
                !it_idoc_structure TYPE tt_edi_api
      RAISING   lcx_idoc_exceptions.

    CLASS-METHODS error IMPORTING
                                  textid   LIKE if_t100_message=>t100key OPTIONAL
                                  previous TYPE REF TO cx_root OPTIONAL
                                  msgv1    TYPE clike OPTIONAL
                                  msgv2    TYPE clike OPTIONAL
                                  msgv3    TYPE clike OPTIONAL
                                  msgv4    TYPE clike OPTIONAL
                        RAISING   lcx_idoc_exceptions.
  PRIVATE SECTION.

    METHODS add_segments_in_given_sequence
      IMPORTING it_edidd TYPE edidd_tt
      RAISING   lcx_idoc_exceptions.

ENDCLASS.

CLASS lcl_idoc_edidd_segment DEFINITION
  INHERITING FROM lcl_idoc_edidd FINAL
  FRIENDS lcl_idoc_edidd.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
                !iv_idoc_type      TYPE edi_idoctp
                !iv_idoc_extension TYPE edi_cimtyp
                !is_edidd          TYPE edidd
                !it_idoc_structure TYPE tt_edi_api
      RAISING   lcx_idoc_exceptions.
    METHODS get_name RETURNING VALUE(rv_name) TYPE edilsegtyp.
    METHODS get_sdata RETURNING VALUE(rs_sdata) TYPE edi_sdata.
    METHODS set_sdata IMPORTING is_sdata TYPE any.

    METHODS add REDEFINITION.
    METHODS get_edidd REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA as_edidd TYPE edidd.
ENDCLASS.


CLASS lcl_idoc_edidd_segment IMPLEMENTATION.

  METHOD add.
    ASSIGN at_idoc_structure_sorted[ KEY segtyp
           COMPONENTS segtyp = is_edidd-segnam ] TO FIELD-SYMBOL(<ls_idoc_structure>).
    IF sy-subrc <> 0.
      error( textid = lcx_idoc_exceptions=>segment_incorrect_for_idoc
             msgv1  = is_edidd-segnam
             msgv2  = av_idoc_type
             msgv3  = av_idoc_extension ).
    ENDIF.

    IF get_name( ) <> <ls_idoc_structure>-parseg.
      error( textid = lcx_idoc_exceptions=>segment_child_of_segment
             msgv1  = is_edidd-segnam
             msgv2  = <ls_idoc_structure>-parseg ).
    ENDIF.

    ro_segment = add_segment_do( is_edidd ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( iv_idoc_type = iv_idoc_type
                        iv_idoc_extension = iv_idoc_extension
                        it_idoc_structure = it_idoc_structure ).
    as_edidd = is_edidd.
  ENDMETHOD.

  METHOD get_edidd.
    APPEND as_edidd TO rt_edidd.

    DATA(lo_iterator) = ao_segments->new_iterator( ).
    WHILE lo_iterator->has_next( ).
      DATA(lt_edidd) = lo_iterator->next( )->get_edidd( ).
      APPEND LINES OF lt_edidd TO rt_edidd.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_name.
    rv_name = as_edidd-segnam.
  ENDMETHOD.

  METHOD get_sdata.
    rs_sdata = as_edidd-sdata.
  ENDMETHOD.

  METHOD set_sdata.
    MOVE is_sdata TO as_edidd-sdata.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_idoc_edidd IMPLEMENTATION.

  METHOD error.
    RAISE EXCEPTION TYPE lcx_idoc_exceptions
      EXPORTING
        textid   = textid
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
  ENDMETHOD.

  METHOD add.
    READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
      WITH KEY segtyp COMPONENTS segtyp = is_edidd-segnam.
    IF sy-subrc <> 0.
      error( textid = lcx_idoc_exceptions=>segment_incorrect_for_idoc
             msgv1  = is_edidd-segnam
             msgv2  = av_idoc_type
             msgv3  = av_idoc_extension ).
    ENDIF.

    IF ld_idoc_structure->parseg IS INITIAL.
      ro_segment = add_segment_do( is_edidd ).
    ELSE.
      DATA(lo_segments) = get_segments( ld_idoc_structure->parseg ).
      IF lo_segments->is_empty( ).
        error( textid = lcx_idoc_exceptions=>parent_segment_not_found
               msgv1  = ld_idoc_structure->parseg ).
      ENDIF.

      " add new segment to the last parent segment
      DATA(lo_iterator) = lo_segments->new_iterator( ).
      WHILE lo_iterator->has_next( ).
        DATA(lo_parent) = lo_iterator->next( ).
      ENDWHILE.

      ro_segment = lo_parent->add( is_edidd ).
    ENDIF.
  ENDMETHOD.

  METHOD add_segments_in_given_sequence.
    TYPES:  BEGIN OF ts_last_segment,
              number  TYPE posno,
              segment TYPE REF TO lcl_idoc_edidd_segment,
            END OF ts_last_segment,
            tt_last_segments TYPE SORTED TABLE OF ts_last_segment WITH UNIQUE KEY number.

    DATA lt_last_segments TYPE tt_last_segments.

    LOOP AT it_edidd ASSIGNING FIELD-SYMBOL(<ls_edidd>).
      READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
        WITH KEY segtyp COMPONENTS segtyp = <ls_edidd>-segnam.
      IF sy-subrc <> 0.
        error( textid = lcx_idoc_exceptions=>segment_incorrect_for_idoc
               msgv1  = <ls_edidd>-segnam
               msgv2  = av_idoc_type
               msgv3  = av_idoc_extension ).
      ENDIF.

      DATA(lo_parent) = me.
      IF ld_idoc_structure->parpno IS NOT INITIAL.
        ASSIGN lt_last_segments[ number = ld_idoc_structure->parpno ] TO FIELD-SYMBOL(<ls_last_segment>).
        IF sy-subrc <> 0.
          error( textid = lcx_idoc_exceptions=>parent_segment_not_found
                 msgv1  = ld_idoc_structure->parseg ).
        ENDIF.

        lo_parent = <ls_last_segment>-segment.
      ENDIF.

      DATA(lo_segment) = NEW lcl_idoc_edidd_segment( iv_idoc_type      = av_idoc_type
                                                     iv_idoc_extension = av_idoc_extension
                                                     is_edidd          = <ls_edidd>
                                                     it_idoc_structure = at_idoc_structure ).
      lo_parent->ao_segments->add( lo_segment ).

      ASSIGN lt_last_segments[ number = ld_idoc_structure->nr ] TO <ls_last_segment>.
      IF sy-subrc <> 0.
        INSERT VALUE #( number = ld_idoc_structure->nr ) INTO TABLE lt_last_segments ASSIGNING <ls_last_segment>.
      ENDIF.

      <ls_last_segment>-segment = lo_segment.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_segment_do.
    DATA lv_added    TYPE flag VALUE abap_false.
    DATA lt_segments TYPE TABLE OF REF TO lcl_idoc_edidd_segment.

    ro_segment = NEW #( iv_idoc_type      = av_idoc_type
                        iv_idoc_extension = av_idoc_extension
                        is_edidd          = is_edidd
                        it_idoc_structure = at_idoc_structure ).

    DATA(lo_iterator) = ao_segments->new_iterator( ).
    WHILE lo_iterator->has_next( ).
      APPEND lo_iterator->next( ) TO lt_segments.
    ENDWHILE.

    ao_segments = NEW #( it_segment = VALUE #( ) ).

    READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
      WITH KEY segtyp COMPONENTS segtyp = is_edidd-segnam.
    DATA(lv_new_segment_position) = ld_idoc_structure->nr.

    LOOP AT lt_segments INTO DATA(lo_segment).
      " new segments still needs to be added
      IF lv_added = abap_false.
        READ TABLE at_idoc_structure_sorted REFERENCE INTO ld_idoc_structure
          WITH KEY segtyp COMPONENTS segtyp = lo_segment->get_name( ).
        DATA(lv_old_segment_position) = ld_idoc_structure->nr.

        IF lv_old_segment_position > lv_new_segment_position.
          ao_segments->add( ro_segment ).
          lv_added = abap_true.
        ENDIF.
      ENDIF.

      ao_segments->add( lo_segment ).
    ENDLOOP.

    " if segment was not inserted between other segments
    " then add it to the end of the collection
    CHECK lv_added = abap_false.
    ao_segments->add( ro_segment ).
  ENDMETHOD.


  METHOD constructor.
    av_idoc_type = iv_idoc_type.
    av_idoc_extension = iv_idoc_extension.
    at_idoc_structure[] = it_idoc_structure[].
    at_idoc_structure_sorted[] = it_idoc_structure[].

    ao_segments = NEW #( it_segment = VALUE #( ) ).

    CHECK it_edidd[] IS NOT INITIAL.
    add_segments_in_given_sequence( it_edidd ).
  ENDMETHOD.


  METHOD create_with_data.
    DATA lt_idoc_structure TYPE tt_edi_api.

    CALL FUNCTION 'EDI_IDOC_SYNTAX_GET'
      EXPORTING
        pi_idoctyp       = iv_idoc_type
        pi_cimtyp        = iv_idoc_extension
      TABLES
        pt_syntax_table  = lt_idoc_structure
      EXCEPTIONS
        syntax_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      error( ).
    ENDIF.

    ro_instance = NEW #( iv_idoc_type      = iv_idoc_type
                         iv_idoc_extension = iv_idoc_extension
                         it_edidd          = it_edidd
                         it_idoc_structure = lt_idoc_structure ).
  ENDMETHOD.


  METHOD get_edidd.
    DATA(lo_iterator) = ao_segments->new_iterator( ).
    WHILE lo_iterator->has_next( ).
      APPEND LINES OF lo_iterator->next( )->get_edidd( ) TO rt_edidd.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_segments.
    DATA lv_found TYPE flag VALUE abap_false.

    ro_segments = NEW #( it_segment = VALUE #( ) ).

    DATA(lo_iterator) = ao_segments->new_iterator( ).
    WHILE lo_iterator->has_next( ).
      DATA(lo_segment) = lo_iterator->next( ).

      IF iv_name IS SUPPLIED.
        IF lo_segment->get_name( ) = iv_name.
          lv_found = abap_true.
          ro_segments->add( lo_segment ).
        ENDIF.
      ELSE.
        ro_segments->add( lo_segment ).
      ENDIF.

      " if segments with given name was found on this level then do not search deeper
      CHECK lv_found = abap_false.

      DATA(lo_child_segments) = lo_segment->get_segments( iv_name ).
      DATA(lo_child_iterator) = lo_child_segments->new_iterator( ).
      WHILE lo_child_iterator->has_next( ).
        ro_segments->add( lo_child_iterator->next( ) ).
      ENDWHILE.
    ENDWHILE.
  ENDMETHOD.

  METHOD remove.
    rv_removed = abap_false.

    DATA(lo_iterator) = ao_segments->new_iterator( ).
    WHILE lo_iterator->has_next( ).
      DATA(lo_segment) = lo_iterator->next( ).
      IF lo_segment = io_segment.
        ao_segments->remove( io_segment ).
        rv_removed = abap_true.
        RETURN.
      ENDIF.

      IF lo_segment->remove( io_segment ).
        rv_removed = abap_true.
        RETURN.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create.
    CLASS-METHODS add_child IMPORTING io_idoc TYPE REF TO lcl_idoc_edidd.
    CLASS-METHODS add_segment IMPORTING io_idoc TYPE REF TO lcl_idoc_edidd.
    CLASS-METHODS set IMPORTING io_idoc TYPE REF TO lcl_idoc_edidd.
    CLASS-METHODS remove IMPORTING io_idoc TYPE REF TO lcl_idoc_edidd.
    CLASS-METHODS partners IMPORTING io_idoc TYPE REF TO lcl_idoc_edidd.
  PROTECTED SECTION.
    "TYPES ts_e1edp01 TYPE e1edp01.
    TYPES: BEGIN OF ts_e1edp01,
             posex TYPE char6,
             action TYPE char3,
           END OF ts_e1edp01.

    TYPES: BEGIN OF ts_e1edp19,
            QUALF Type char3,
            IDTNR Type char35,
            KTEXT Type char70,
           END OF ts_e1edp19.
ENDCLASS.

START-OF-SELECTION.
  lcl_demo=>create( ).

CLASS lcl_demo IMPLEMENTATION.

  METHOD create.
    CONSTANTS:
      lc_type_orders05  TYPE edi_idoctp VALUE 'ORDERS05',
      lc_extension_none TYPE edi_cimtyp VALUE ''.

    DATA lt_edidd TYPE edidd_tt.

    TRY.
        DATA(lo_idoc) = lcl_idoc_edidd=>create_with_data(
                          iv_idoc_type = lc_type_orders05
                          iv_idoc_extension = lc_extension_none
                          it_edidd = lt_edidd ).
        partners( lo_idoc ).
        add_child( lo_idoc ).
        add_segment( lo_idoc ).
        set( lo_idoc ).
        remove( lo_idoc ).
        lt_edidd = lo_idoc->get_edidd( ).
      CATCH lcx_idoc_exceptions INTO DATA(lo_exception).
        MESSAGE lo_exception TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD partners.

    DATA(ls_e1edka1) = VALUE e1edka1( parvw = 'WE' partn = '1234567890' ).

    DATA(ls_edidd) = VALUE edidd( segnam = 'E1EDKA1' sdata = ls_e1edka1 ).

    TRY.
        io_idoc->add( ls_edidd ).
      CATCH lcx_idoc_exceptions INTO DATA(lo_exception).
        MESSAGE lo_exception TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD add_child.
    DATA:
      ls_e1edp01 TYPE ts_e1edp01,
      ls_e1edp19 TYPE ts_e1edp19.

    ls_e1edp01-posex = '10'.
    ls_e1edp19-qualf = '003'.

    TRY.
        DATA(lo_item) = io_idoc->add( VALUE #( segnam = 'E1EDP01' sdata = ls_e1edp01 ) ).
        lo_item->add( VALUE #( segnam = 'E1EDP19' sdata = ls_e1edp19 ) ).
      CATCH lcx_idoc_exceptions INTO DATA(lo_exception).
        MESSAGE lo_exception TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD add_segment.
    DATA:
      ls_edidd   TYPE edidd,
      ls_e1edp01 TYPE ts_e1edp01,
      ls_e1edp19 TYPE ts_e1edp19,
      lo_segment TYPE REF TO lcl_idoc_edidd_segment.

    ls_e1edp19-qualf = '003'.
    ls_edidd = VALUE #( segnam = 'E1EDP19' sdata = ls_e1edp19 ).

    TRY.
        DATA(lo_collection) = io_idoc->get_segments( 'E1EDP01' ).
        DATA(lo_iterator) = lo_collection->new_iterator( ).

        WHILE lo_iterator->has_next( ).
          lo_segment = lo_iterator->next( ).
          ls_e1edp01 = lo_segment->get_sdata( ).

          " add child under position 50
          CHECK ls_e1edp01-posex = '50'.
          lo_segment->add( ls_edidd ).
          EXIT.
        ENDWHILE.
      CATCH lcx_idoc_exceptions INTO DATA(lo_exception).
        MESSAGE lo_exception TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD set.
    " get the first E1EDKA1 segment
    DATA(lo_segment) = io_idoc->get_segments( 'E1EDKA1' )->next( ).
    IF lo_segment IS BOUND.
      lo_segment->set_sdata( VALUE e1edka1( BASE lo_segment->get_sdata( )
                                            partn = '0987654321' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD remove.
    " get the first E1EDKA1 segment
    DATA(lo_segment) = io_idoc->get_segments( 'E1EDKA1' )->next( ).
    IF lo_segment IS BOUND.
      io_idoc->remove( lo_segment ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
