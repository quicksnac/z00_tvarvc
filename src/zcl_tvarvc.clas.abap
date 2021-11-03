class ZCL_TVARVC definition
  public
  create public .

public section.

  types:
    ttr_ TYPE RANGE OF tvarvc-low .
  types:
    tsr_ TYPE LINE OF ttr_ .

  constants MC_QM_120_BWART type RVARI_VNAM value 'ZZQM_120_BWART' ##NO_TEXT.
  constants MC_QM_120_CONSUMPTION type RVARI_VNAM value 'ZZQM_120_CONSUMPTION' ##NO_TEXT.
  constants MC_QM_120_POSTING type RVARI_VNAM value 'ZZQM_120_POSTING' ##NO_TEXT.
  constants MC_QM_0852_CAR_REPEAT type RVARI_VNAM value 'ZZQM_0852_CAR_REPEAT' ##NO_TEXT.
  constants MC_QM_0852_CAR_RREPEAT type RVARI_VNAM value 'ZZQM_0852_CAR_RREPEAT' ##NO_TEXT.
  constants MC_QM_0852_CAR_DUEDAYS type RVARI_VNAM value 'ZZQM_0852_CAR_DUEDAYS' ##NO_TEXT.
  constants MC_QM_0844_DELIVERY_TYPE type RVARI_VNAM value 'ZZQM_0844_DELIVERY_TYPE' ##NO_TEXT.
  constants MC_EWM_PACKMAT_EXCL type RVARI_VNAM value 'ZZEWM_PACKMAT_HUDELETE_EXCL' ##NO_TEXT.

  class-methods GET_PARAMETER_BY_NAME
    importing
      !IV_NAME type TVARVC-NAME
      !IV_DEFAULT type TVARVC-LOW optional
    preferred parameter IV_NAME
    returning
      value(RV_) type TVARVC-LOW .
  class-methods GET_RANGE_BY_NAME
    importing
      !IV_NAME type TVARVC-NAME
      !IV_DEFAULT type TVARVC-LOW optional
      !ITR_DEFAULT type TTR_ optional
    preferred parameter IV_NAME
    returning
      value(RTR_) type TTR_ .
  class-methods GET_IMPOSSIBLE_RANGE
    returning
      value(RTR_) type TTR_ .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TVARVC IMPLEMENTATION.


METHOD get_impossible_range.
    rtr_ = VALUE #( ( sign = 'E' option = 'CP' low = '*' ) ).
  ENDMETHOD.


METHOD get_parameter_by_name.
    rv_ = iv_default.

    SELECT SINGLE low FROM tvarvc INTO rv_ WHERE name = iv_name AND type = 'P'.
  ENDMETHOD.


METHOD get_range_by_name.
    SELECT sign, opti AS option, low, high FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE @rtr_
      WHERE name = @iv_name
        AND type = 'S'.
    "AND low NE '' AND high ne ''.

    IF rtr_ IS INITIAL.
      rtr_ = itr_default.
      IF iv_default IS SUPPLIED AND
         iv_default IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = iv_default ) TO rtr_.
      ENDIF.
    ENDIF.

    LOOP AT rtr_ ASSIGNING FIELD-SYMBOL(<fsr_>) WHERE sign IS INITIAL OR option IS INITIAL.
      IF <fsr_>-sign IS INITIAL.
        <fsr_>-sign = 'I'.
      ENDIF.
      IF <fsr_>-option IS INITIAL.
        <fsr_>-option = 'EQ'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
