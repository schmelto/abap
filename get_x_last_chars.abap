FORM get_x_last_chars USING iv_string
                            iv_num_last_chars TYPE i
                      CHANGING cv_result.
  IF strlen( iv_string ) < iv_num_last_chars.
    cv_result = iv_string.
  ELSE.
    cv_result = substring( val = iv_string
                           off = strlen( iv_string ) - iv_num_last_chars
                           len = iv_num_last_chars ).
  ENDIF.
ENDFORM.
