CLASS zcl_mdp_json_deserializer2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize
      IMPORTING
        !json TYPE string
      EXPORTING
        !node TYPE REF TO zcl_mdp_json_node
      RAISING
        zcx_mdp_json_invalid .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS co_debug_mode TYPE i VALUE 0 ##NO_TEXT.

    CLASS-METHODS deserialize_node
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !jsonnode      TYPE REF TO zcl_mdp_json_node
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS deserialize_object
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !jsonnode      TYPE REF TO zcl_mdp_json_node
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS deserialize_array
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !jsonnode      TYPE REF TO zcl_mdp_json_node
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS parse_string
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !parsed_string TYPE string
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS parse_number_node
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !jsonnode      TYPE REF TO zcl_mdp_json_node
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS deserialize_string_node
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !jsonnode      TYPE REF TO zcl_mdp_json_node
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
    CLASS-METHODS skip_whitespace
      IMPORTING
        !json          TYPE string
        !offset_before TYPE i
      EXPORTING
        !offset_after  TYPE i
      RAISING
        zcx_mdp_json_invalid .
ENDCLASS.



CLASS ZCL_MDP_JSON_DESERIALIZER2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MDP_JSON_DESERIALIZER2=>DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [<---] NODE                           TYPE REF TO ZCL_MDP_JSON_NODE
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize.

    DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.

    deserialize_node(
  EXPORTING
    json = json
    offset_before = 0
  IMPORTING
    jsonnode = l_jsonnode ) .

    node = l_jsonnode.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>DESERIALIZE_ARRAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] JSONNODE                       TYPE REF TO ZCL_MDP_JSON_NODE
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize_array.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i.

    DATA : l_array_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_array_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_array.

    IF co_debug_mode = 1.
      WRITE: / 'array:' .
    ENDIF.


    "we already assume first is [
    l_offset = l_offset + 1.

    DO.

      skip_whitespace( EXPORTING json = l_json offset_before = l_offset IMPORTING offset_after = l_offset ).

      CASE l_json+l_offset(1).
        WHEN ']'.
          l_offset = l_offset + 1 .
          offset_after = l_offset.
          EXIT.
        WHEN ','.
          l_offset = l_offset + 1 .
      ENDCASE.


      DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.


      DATA : wa_array_children TYPE zcl_mdp_json_node=>typ_array_children .

      deserialize_node( EXPORTING json = l_json offset_before = l_offset
        IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).

      wa_array_children-node = l_jsonnode .

      APPEND wa_array_children TO l_array_jsonnode->array_children.

      offset_after = l_offset.

    ENDDO.
    jsonnode = l_array_jsonnode .

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>DESERIALIZE_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] JSONNODE                       TYPE REF TO ZCL_MDP_JSON_NODE
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize_node.

    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    DATA l_offset_temp TYPE i.
    l_offset = offset_before.
    l_offset_temp = offset_before.

    DATA l_len TYPE i.

    DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.

    "    FIND REGEX '\{|\[|"|\d|t|f' IN SECTION OFFSET l_offset OF json
    "    MATCH OFFSET l_offset.
    skip_whitespace( EXPORTING json = l_json offset_before = l_offset IMPORTING offset_after = l_offset ).


    CASE l_json+l_offset(1).
      WHEN '{'.
        "l_offset = l_offset + 1.
        deserialize_object( EXPORTING json = l_json offset_before = l_offset IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).
        jsonnode = l_jsonnode.
        offset_after = l_offset.
      WHEN '['.
        "l_offset = l_offset + 1.
        deserialize_array( EXPORTING json = l_json offset_before = l_offset IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).
        jsonnode = l_jsonnode.
        offset_after = l_offset.
      WHEN '"'.
        deserialize_string_node( EXPORTING json = l_json offset_before = l_offset IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).

        offset_after = l_offset.
      WHEN 't'.
        IF l_json+l_offset(4) = 'true'.
          CREATE OBJECT l_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_true.
          l_jsonnode->value = l_json+l_offset(4).
          offset_after = l_offset + 4.

          IF co_debug_mode = 1.
            WRITE: / 'true'  .
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_mdp_json_invalid.
        ENDIF.

      WHEN 'n'.
        IF l_json+l_offset(4) = 'null'.
          CREATE OBJECT l_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_null.
          l_jsonnode->value = l_json+l_offset(4).
          offset_after = l_offset + 4.

          IF co_debug_mode = 1.
            WRITE: / 'null'  .
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_mdp_json_invalid.
        ENDIF.
      WHEN 'f'.
        IF l_json+l_offset(5) = 'false'.
          CREATE OBJECT l_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_false.
          l_jsonnode->value = l_json+l_offset(5).
          offset_after = l_offset + 5.

          IF co_debug_mode = 1.
            WRITE: / 'false'  .
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_mdp_json_invalid.
        ENDIF.
      WHEN OTHERS.
        parse_number_node( EXPORTING json = l_json offset_before = l_offset IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).
        jsonnode = l_jsonnode.
        offset_after = l_offset.

    ENDCASE.

    jsonnode = l_jsonnode.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>DESERIALIZE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] JSONNODE                       TYPE REF TO ZCL_MDP_JSON_NODE
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize_object.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i.

    IF l_json+l_offset(1) NE '{'.
      RETURN.
    ENDIF.
    l_offset = l_offset + 1.

    DATA : l_object_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_object_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_object.


    IF co_debug_mode = 1.
      WRITE: / 'object:' .
    ENDIF.

    DO.

*
*  } end

      skip_whitespace( EXPORTING json = l_json offset_before = l_offset IMPORTING offset_after = l_offset ).

      CASE l_json+l_offset(1).
        WHEN '}'.
          l_offset = l_offset + 1 .
          offset_after = l_offset.
          EXIT.
        WHEN ','.
          l_offset = l_offset + 1 .
          skip_whitespace( EXPORTING json = l_json offset_before = l_offset IMPORTING offset_after = l_offset ).
      ENDCASE.

* require a key
      DATA l_key_string TYPE string.
      DATA l_value_node TYPE REF TO zcl_mdp_json_node.

      parse_string( EXPORTING json = l_json offset_before = l_offset
               IMPORTING parsed_string = l_key_string offset_after = l_offset ).

      IF co_debug_mode = 1.
        WRITE: / 'key:' , l_key_string .
      ENDIF.

      skip_whitespace( EXPORTING json = l_json offset_before = l_offset IMPORTING offset_after = l_offset ).

      IF l_json+l_offset(1) NE ':'.
        RAISE EXCEPTION TYPE zcx_mdp_json_invalid.
      ENDIF.
      l_offset = l_offset + 1.

      " parse value for key
      deserialize_node( EXPORTING json = l_json offset_before = l_offset
             IMPORTING jsonnode = l_value_node offset_after = l_offset ).
      offset_after = l_offset.

      "create child object
      DATA : wa_object_child TYPE  zcl_mdp_json_node=>typ_object_children .
      wa_object_child-key = l_key_string .
      wa_object_child-node = l_value_node .

      INSERT wa_object_child INTO TABLE l_object_jsonnode->object_children.



    ENDDO.
    jsonnode = l_object_jsonnode .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>DESERIALIZE_STRING_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] JSONNODE                       TYPE REF TO ZCL_MDP_JSON_NODE
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize_string_node.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_parsed_string TYPE string.

    parse_string( EXPORTING json = l_json offset_before = l_offset
          IMPORTING parsed_string = l_parsed_string offset_after = l_offset ).

    DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_string.
    l_jsonnode->value = l_parsed_string .

    offset_after = l_offset.


    jsonnode = l_jsonnode .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>PARSE_NUMBER_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] JSONNODE                       TYPE REF TO ZCL_MDP_JSON_NODE
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_number_node.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i VALUE 0.

    "integer
    IF l_json+l_offset(1) EQ '-'.
      l_offset = l_offset + 1.
    ENDIF.

    CASE l_json+l_offset(1).
      WHEN '0'.
        l_offset = l_offset + 1.
      WHEN OTHERS.

        DO.
          IF l_json+l_offset(1) CO '1234567890'.
            l_offset = l_offset + 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

    ENDCASE.

    "fraction
    IF l_json+l_offset(1) = '.'.
      l_offset = l_offset + 1.
      DO.
        IF l_json+l_offset(1) CO '1234567890'.
          l_offset = l_offset + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    "exponent
    IF l_json+l_offset(1) CO 'eE'.
      l_offset = l_offset + 1.

      IF l_json+l_offset(1) CO '+-'.
        l_offset = l_offset + 1.
      ENDIF.

      DO.
        IF l_json+l_offset(1) CO '1234567890'.
          l_offset = l_offset + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.



    DATA l_match TYPE string.
    DATA l_match_start TYPE i.
    DATA l_match_len TYPE i.
    l_match_start = offset_before .
    l_match_len = l_offset - offset_before .
    l_match = l_json+l_match_start(l_match_len).


    DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_number.
    l_jsonnode->value = l_match .

    offset_after = l_offset.


    jsonnode = l_jsonnode .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>PARSE_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] PARSED_STRING                  TYPE        STRING
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_string.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i VALUE 0.


    IF l_json+l_offset(1) NE '"'.
      RETURN.
    ENDIF.
    "l_offset = l_offset + 1.

    DATA l_match TYPE string.

    DO.
      l_offset = l_offset + 1.

      CASE l_json+l_offset(1).
        WHEN '"' .
          EXIT.
        WHEN '\' .
          CASE l_json+l_offset(2).
            WHEN '\"'.
              l_offset = l_offset + 2.
            WHEN '\\'.
              l_offset = l_offset + 2.
            WHEN '\/'.
              l_offset = l_offset + 2.
            WHEN '\b'.
              l_offset = l_offset + 2.
            WHEN '\f'.
              l_offset = l_offset + 2.
            WHEN '\n'.
              l_offset = l_offset + 2.
            WHEN '\r'.
              l_offset = l_offset + 2.
            WHEN '\t'.
              l_offset = l_offset + 2.
*            WHEN '\uXXXX'.
*              l_offset = l_offset + 2.

          ENDCASE.
        WHEN OTHERS.


      ENDCASE.

    ENDDO.

    l_offset = l_offset + 1.


    DATA l_match_start TYPE i.
    DATA l_match_len TYPE i.
    l_match_start = offset_before + 1.
    l_match_len = l_offset - offset_before - 2.
    l_match = l_json+l_match_start(l_match_len).

    REPLACE ALL OCCURRENCES OF '\"' IN l_match WITH '"'.
    REPLACE ALL OCCURRENCES OF '\\' IN l_match WITH '\'.
    REPLACE ALL OCCURRENCES OF '\/' IN l_match WITH '/'.
    REPLACE ALL OCCURRENCES OF '\b' IN l_match WITH cl_abap_char_utilities=>backspace.
    REPLACE ALL OCCURRENCES OF '\f' IN l_match WITH cl_abap_char_utilities=>form_feed.
    REPLACE ALL OCCURRENCES OF '\n' IN l_match WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '\r' IN l_match WITH cl_abap_char_utilities=>cr_lf(1).
    REPLACE ALL OCCURRENCES OF '\t' IN l_match WITH cl_abap_char_utilities=>horizontal_tab .


    offset_after = l_offset.

    parsed_string = l_match .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_MDP_JSON_DESERIALIZER2=>SKIP_WHITESPACE
* +-------------------------------------------------------------------------------------------------+
* | [--->] JSON                           TYPE        STRING
* | [--->] OFFSET_BEFORE                  TYPE        I
* | [<---] OFFSET_AFTER                   TYPE        I
* | [!CX!] ZCX_MDP_JSON_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD skip_whitespace.
    DATA l_json TYPE string.
    l_json = json.
    DATA l_offset TYPE i.
    l_offset = offset_before.
    DATA l_offset_check TYPE i.
    l_offset_check = offset_before.



    DO.
      IF l_offset_check >= strlen( l_json ).
        EXIT.
      ENDIF.
      l_offset = l_offset_check.
      CASE l_json+l_offset_check(1).
        WHEN ` ` .
        WHEN cl_abap_char_utilities=>newline .
        WHEN cl_abap_char_utilities=>horizontal_tab .
        WHEN cl_abap_char_utilities=>cr_lf(1) .
        WHEN OTHERS.
          EXIT.
      ENDCASE.


      l_offset_check = l_offset_check + 1.

    ENDDO.

    IF co_debug_mode = 1.
      DATA : l_len     TYPE i VALUE 0,
             l_len_str TYPE string.

      l_len = l_offset - offset_before.
      l_len_str = l_len.
      CONCATENATE 'Skipped chars:' l_len_str INTO l_len_str.
      WRITE: / l_len_str  .
    ENDIF.

    offset_after = l_offset.
  ENDMETHOD.
ENDCLASS.