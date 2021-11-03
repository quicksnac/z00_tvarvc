class ZCL_TVARVC_RANGE_DESC definition
  public
  create protected .

public section.

  types:
    BEGIN OF ts_sot,
        sign   TYPE ddsign,
        option TYPE ddoption,
        text   TYPE string,
      END OF ts_sot .
  types:
    tt_sot TYPE STANDARD TABLE OF ts_sot WITH KEY sign option .
  types TTR_ type ZCL_TVARVC=>TTR_ .
  types TSR_ type ZCL_TVARVC=>TSR_ .
  types:
    BEGIN OF ts_dtext,
        val  TYPE dd07v-domvalue_l,
        text TYPE dd07v-ddtext,
      END OF ts_dtext .
  types:
    tt_dtext TYPE STANDARD TABLE OF ts_dtext WITH KEY val .

  methods HUMANIZED_DESCRIPTION
    importing
      !IV_ENTITY type STRING optional
      !IV_DEL01 type STRING optional
      !IV_DEL02 type STRING optional
      !IV_DEL03 type STRING optional
      !IV_PREFIX01 type STRING default `: `
      !IV_INCLUDE type FLAG default 'X'
      !IV_EXCLUDE type FLAG default 'X'
    preferred parameter IV_ENTITY
    returning
      value(RV_) type STRING .
  methods HUMANIZED_DESCRIPTION_ONE_LINE
    importing
      !IV_ENTITY type STRING optional
    preferred parameter IV_ENTITY
    returning
      value(RV_) type STRING .
  class-methods NEW_RANGE_BY_NAME
    importing
      !IV_NAME type TVARVC-NAME
      !IV_DEFAULT type TVARVC-LOW optional
      !ITR_DEFAULT type TTR_ optional
      !IV_LANG type SY-LANGU default SY-LANGU
    preferred parameter IV_NAME
    returning
      value(RO_) type ref to ZCL_TVARVC_RANGE_DESC .
  class-methods NEW
    importing
      !ITR_ type TTR_
      !IV_LANG type SY-LANGU default SY-LANGU
    returning
      value(RO_) type ref to ZCL_TVARVC_RANGE_DESC .
  class-methods NEW_ANY_TABLE
    importing
      !ITR_ type ANY TABLE
      !IV_LANG type SY-LANGU default SY-LANGU
    returning
      value(RO_) type ref to ZCL_TVARVC_RANGE_DESC .
  class-methods INVERT_DOUBLE_NEGATION
    importing
      !ITR_ type TTR_
    returning
      value(RTR_) type TTR_ .
protected section.

  data MT_S type TT_DTEXT .
  data MT_O type TT_DTEXT .
  data MTR_ORIGINAL type TTR_ .
  data MV_LANG type SY-LANGU .
  data MTR_ type TTR_ .
  data MTR_FOLD type TTR_ .

  class-methods BT
    importing
      !IV_TEXT type STRING
      !IV_LOW type TSR_-LOW
      !IV_HIGH type TSR_-HIGH
    returning
      value(RV_) type STRING .
  methods GET_TEXT
    importing
      !IV_SIGN type DDSIGN
      !IV_OPTION type DDOPTION
      !IV_DEL01 type STRING optional
      !IV_DEL02 type STRING optional
      !IV_DEL03 type STRING optional
      !IV_PREFIX01 type STRING
    returning
      value(RV_) type STRING .
  methods CONSTRUCTOR
    importing
      !IV_LANG type SY-LANGU default SY-LANGU
      !ITR_ type TTR_ .
private section.

  methods CACHE_TEXTS .
ENDCLASS.



CLASS ZCL_TVARVC_RANGE_DESC IMPLEMENTATION.


METHOD bt.

    rv_ = iv_text.

    rv_ = replace( val  = rv_
              sub  = '...'
              with = iv_low
              occ  =   1 ).

    rv_ = replace( val  = rv_
              sub  = '...'
              with = iv_high
              occ  =   1 ).

  ENDMETHOD.


METHOD cache_texts.

    IF mt_s[] IS NOT INITIAL AND
       mt_o[] IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT domvalue_l AS val,
           ddtext     AS text
      FROM dd07v
      WHERE domname = 'DDSIGN' AND
            ddlanguage = @mv_lang
      INTO CORRESPONDING FIELDS OF TABLE @mt_s.

    SELECT domvalue_l AS val,
           ddtext     AS text
      FROM dd07v
      WHERE domname = 'DDOPTION' AND
            ddlanguage = @mv_lang
      INTO CORRESPONDING FIELDS OF TABLE @mt_o.

  ENDMETHOD.


METHOD constructor.
    mv_lang = iv_lang.
    mtr_original = itr_.
    mtr_ = invert_double_negation( itr_ ).
    me->cache_texts( ).

    SORT mtr_ BY sign option low high.
    mtr_fold = mtr_.
    DELETE ADJACENT DUPLICATES FROM mtr_fold COMPARING sign option.
    MODIFY mtr_fold FROM VALUE #( ) TRANSPORTING low high WHERE low IS NOT INITIAL OR high IS NOT INITIAL.
  ENDMETHOD.


METHOD get_text.

    DATA(lv_del01) = iv_del01.
    DATA(lv_del02) = iv_del02.
    DATA(lv_del03) = iv_del03.

    CASE iv_option.
      WHEN 'BT'.
        rv_ = REDUCE string( INIT lv_text = ||
                    FOR lsr_ IN mtr_ WHERE ( sign = iv_sign AND option = 'BT' )
                    NEXT lv_text = |{ lv_text }{ lv_del01 }{ bt( iv_text = |{ mt_o[ val = 'BT' ]-text }| iv_low = lsr_-low iv_high = lsr_-high ) }|
    ).
        REPLACE FIRST OCCURENCE OF lv_del01 IN rv_ WITH ''.
      WHEN 'EQ'.
        rv_ = REDUCE string( INIT lv_text = ||
                             FOR lsr_ IN mtr_ WHERE ( sign = iv_sign AND option = iv_option )
                             NEXT lv_text = |{ lv_text }{ lv_del01 }{ lsr_-low }|
          ).

        REPLACE FIRST OCCURENCE OF lv_del01 IN rv_ WITH ''.
      WHEN OTHERS.
        rv_ = REDUCE string( INIT lv_text = |{ mt_o[ val = iv_option ]-text }{ iv_prefix01 } |
                             FOR lsr_ IN mtr_ WHERE ( sign = iv_sign AND option = iv_option )
                             NEXT lv_text = |{ lv_text }{ lv_del01 }{ lsr_-low }|
          ).

        REPLACE FIRST OCCURENCE OF lv_del01 IN rv_ WITH ''.

    ENDCASE.

  ENDMETHOD.


METHOD humanized_description.
    DATA(lv_del01) = COND string( WHEN iv_del02 IS INITIAL THEN CONV #( |, | )
                            ELSE iv_del01 ).
    DATA(lv_del02) = COND string( WHEN iv_del02 IS INITIAL THEN CONV #( cl_abap_char_utilities=>cr_lf )
                            ELSE iv_del02 ).
    DATA(lv_del03) = COND string( WHEN iv_del02 IS INITIAL THEN CONV #( cl_abap_char_utilities=>cr_lf )
                            ELSE iv_del03 ).

    DATA: lt_sot TYPE tt_sot.

    DATA(lt_option_order) = VALUE tt_sot(
      ( option = 'EQ' )
      ( option = 'BT' )
      ( option = 'CP' )
      ( option = 'LE' )
      ( option = 'GE' )
      ( option = 'GT' )
      ( option = 'LT' )
*      ( option = 'NE' )
*      ( option = 'NB' )
*      ( option = 'NP' )
    ).

    LOOP AT lt_option_order REFERENCE INTO DATA(lr_order).
      LOOP AT mtr_fold REFERENCE INTO DATA(lr_fold) WHERE option = lr_order->option.
        DATA: ls_sot LIKE LINE OF lt_sot.
        ls_sot-sign = lr_fold->sign.
        ls_sot-option = lr_fold->option.
        ls_sot-text = condense( get_text( iv_sign = lr_fold->sign
                                          iv_option = lr_fold->option
                                          iv_del01 = lv_del01
                                          iv_del02 = lv_del02
                                          iv_del03 = lv_del03
                                          iv_prefix01 = iv_prefix01
                                )
                      ).
        IF ls_sot-text IS INITIAL.
          CONTINUE.
        ENDIF.
        APPEND ls_sot TO lt_sot.
      ENDLOOP.
    ENDLOOP.

    IF line_exists( lt_sot[ sign = 'I' ] ).
      IF iv_include = 'X'.
        MESSAGE s001 WITH iv_entity INTO DATA(lv_im).
        lv_im = |{ lv_im }{ iv_prefix01 }|.
      ENDIF.

      DATA(lv_i) = REDUCE string( INIT lv_text = | |
                    FOR ls_ IN lt_sot WHERE ( sign = 'I' )
                    NEXT lv_text = |{ lv_text }{ lv_del02 }{ ls_-text }| ).
      REPLACE FIRST OCCURRENCE OF lv_del02 IN lv_i WITH ''.
      CONDENSE lv_i.
      lv_i = |{ lv_im }{ lv_i }|.
    ENDIF.

    IF line_exists( lt_sot[ sign = 'E' ] ).
      IF iv_exclude = 'X'.
        MESSAGE s002 WITH iv_entity INTO DATA(lv_em).
        lv_em = |{ lv_em }{ iv_prefix01 }|.
      ENDIF.
      DATA(lv_e) = REDUCE string( INIT lv_text = | |
                    FOR ls_ IN lt_sot WHERE ( sign = 'E' )
                    NEXT lv_text = |{ lv_text }{ lv_del02 }{ ls_-text }| ).
      REPLACE FIRST OCCURRENCE OF lv_del02 IN lv_e WITH ''.
      CONDENSE lv_e.
      lv_e = |{ lv_em }{ lv_e }|.
    ENDIF.
*----------------------------------------------------------------------------
    IF lv_i IS NOT INITIAL AND
       lv_e IS NOT INITIAL.
      rv_ = lv_i && lv_del03 && lv_e.

    ELSEIF lv_i IS NOT INITIAL AND
           lv_e IS INITIAL.
      rv_ = lv_i .

    ELSEIF lv_i IS INITIAL AND
           lv_e IS NOT INITIAL.
      rv_ = lv_e .

    ENDIF.
  ENDMETHOD.


METHOD HUMANIZED_DESCRIPTION_ONE_LINE.
    rv_ = me->humanized_description(
             iv_entity   = iv_entity
             iv_del01    = |, |
             iv_del02    = |, |
             iv_del03    = |; |
             iv_prefix01 = |: |
             iv_include  = ' '
             iv_exclude  = 'X'
          ).
  ENDMETHOD.


METHOD invert_double_negation.
    rtr_ = itr_.
    LOOP AT rtr_ REFERENCE INTO DATA(lr_).
      IF lr_->sign = 'I'.
        CASE lr_->option.
          WHEN 'NE'.
            lr_->sign = 'E'. lr_->option = 'EQ'.
          WHEN 'NB'.
            lr_->sign = 'E'. lr_->option = 'BT'.
          WHEN 'NP'.
            lr_->sign = 'E'. lr_->option = 'CP'.
        ENDCASE.
      ELSE.
*        CASE lr_->option.
*          WHEN 'EQ'.
*            lr_->sign = 'I'. lr_->option = 'NE'.
*          WHEN 'BT'.
*            lr_->sign = 'I'. lr_->option = 'NB'.
*          WHEN 'CP'.
*            lr_->sign = 'I'. lr_->option = 'NP'.
*        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


METHOD new.
    ro_ = NEW #( itr_ = itr_ iv_lang = iv_lang ).
  ENDMETHOD.


METHOD new_any_table.
    DATA ltr_ TYPE ttr_.
    MOVE-CORRESPONDING itr_[] TO ltr_[].

    ro_ = new( itr_ = ltr_ iv_lang = iv_lang ).
  ENDMETHOD.


METHOD NEW_RANGE_BY_NAME.
    data(ltr_) = zcl_tvarvc=>get_range_by_name(
                   iv_name     = iv_name
                   iv_default  = iv_default
                   itr_default = itr_default
                 ).

    ro_ = new(
            itr_    = ltr_
            iv_lang = iv_lang
          ).
  ENDMETHOD.
ENDCLASS.
