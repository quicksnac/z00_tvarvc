*"* use this source file for your ABAP unit test classes
CLASS lcl_test DEFINITION DEFERRED.
CLASS zcl_tvarvc_range_desc DEFINITION LOCAL FRIENDS lcl_test.

CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_tvarvc_range_desc.  "class under test

    "METHODS: bt FOR TESTING.

    METHODS: hd_eq_single FOR TESTING.
    METHODS: hd_eq_multi FOR TESTING.
    METHODS: hd_eq_multi_material FOR TESTING.

    METHODS: hd_bt_single FOR TESTING.
    METHODS: hd_bt_multi FOR TESTING.

    METHODS: hd_ne_single FOR TESTING.
    METHODS: hd_ne_multi FOR TESTING.

ENDCLASS.       "lcl_Test


CLASS lcl_test IMPLEMENTATION.

  METHOD hd_eq_single.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'EQ' low = 'Low 01' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Include: Low 01'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_eq_multi.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'EQ' low = 'Low 01' )
                    ( sign = 'I' option = 'EQ' low = 'Low 02' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Include: Low 01, Low 02'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_eq_multi_material.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'EQ' low = 'Low 01' )
                    ( sign = 'I' option = 'EQ' low = 'Low 02' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( 'Material(s)' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Include Material(s): Low 01, Low 02'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_ne_single.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'NE' low = 'Low 01' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Exclude: Low 01'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_ne_multi.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'NE' low = 'Low 01' )
                    ( sign = 'E' option = 'EQ' low = 'Low 02' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Exclude: Low 01, Low 02'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_bt_single.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'BT' low = 'Low 01' high = 'High 01' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Include: Between Low 01 and High 01'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.
  METHOD hd_bt_multi.
    DATA(lo_) = zcl_tvarvc_range_desc=>new(
                  itr_    = VALUE #(
                    ( sign = 'I' option = 'BT' low = 'Low 01' high = 'High 01' )
                    ( sign = 'I' option = 'BT' low = 'Low 02' high = 'High 02' )
                   )
                ).
    DATA(lv_) = lo_->humanized_description( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_
      exp   = 'Include: Between Low 01 and High 01, Between Low 02 and High 02 '
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.

ENDCLASS.
