CLASS zcl_tasty_json_serializer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      serialize
        IMPORTING
          !node TYPE REF TO zcl_tasty_json_node
        EXPORTING
          !json TYPE string ,
      serialize_formatted
        IMPORTING
          !node TYPE REF TO zcl_tasty_json_node
        EXPORTING
          !json TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
    serialize_node
        IMPORTING jsonnode TYPE REF TO zcl_tasty_json_node
        EXPORTING json TYPE string

   ,serialize_object
        IMPORTING jsonnode TYPE REF TO zcl_tasty_json_node
        EXPORTING json TYPE string

   ,serialize_array
        IMPORTING jsonnode TYPE REF TO zcl_tasty_json_node
        EXPORTING json TYPE string
    ,get_escaped_value
            IMPORTING jsonnode TYPE REF TO zcl_tasty_json_node
        RETURNING VALUE(json) TYPE string
    ,get_formatted_json_string
      IMPORTING json_string_in         TYPE string
      RETURNING VALUE(json_string_out) TYPE string
      RAISING   cx_sxml_parse_error.

*        CHANGING  offset TYPE i .
    CONSTANTS: co_debug_mode TYPE i VALUE 1.

ENDCLASS.

CLASS zcl_tasty_json_serializer IMPLEMENTATION.

  METHOD serialize.

    serialize_node(
      EXPORTING
        jsonnode = node
      IMPORTING
        json = json
     ) .
  ENDMETHOD.

  METHOD serialize_formatted.

    serialize_node(
      EXPORTING
        jsonnode = node
      IMPORTING
        json = json
     ) .

    json = get_formatted_json_string( json ).
  ENDMETHOD.


  METHOD serialize_array.

  ENDMETHOD.


  METHOD serialize_node.
    DATA l_json TYPE string.
    DATA : l_index TYPE i VALUE 0.
    DATA : l_child_json TYPE string.

    CASE    jsonnode->json_type.
      WHEN  zcl_tasty_json_node=>co_json_string.
        DATA(escaped) = get_escaped_value( jsonnode ).
        CONCATENATE '"' escaped '"' INTO l_json.
      WHEN zcl_tasty_json_node=>co_json_number.
        l_json = jsonnode->value.
      WHEN zcl_tasty_json_node=>co_json_false.
        l_json = 'false'.
      WHEN zcl_tasty_json_node=>co_json_true.
        l_json = 'true'.
      WHEN zcl_tasty_json_node=>co_json_null.
        l_json = 'null'.
      WHEN zcl_tasty_json_node=>co_json_array.

        DATA : wa_array_children LIKE LINE OF jsonnode->array_children .


        l_json = '['.
        LOOP AT jsonnode->array_children INTO wa_array_children.
          IF l_index > 0.
            CONCATENATE l_json ',' INTO l_json.
          ENDIF.

          serialize_node(
            EXPORTING
              jsonnode = wa_array_children-node
            IMPORTING
              json = l_child_json
          ) .
          CONCATENATE l_json l_child_json INTO l_json.
          CLEAR wa_array_children.
          l_index = 1.
        ENDLOOP.
        CONCATENATE l_json ']' cl_abap_char_utilities=>cr_lf INTO l_json.
      WHEN zcl_tasty_json_node=>co_json_object.
        DATA : wa_object_children LIKE LINE OF jsonnode->object_children .


        l_json = '{'.

        l_index = 0 .
        LOOP AT jsonnode->object_children INTO wa_object_children.
          IF l_index > 0.
            CONCATENATE l_json ',' INTO l_json.
          ENDIF.

          CONCATENATE l_json '"' wa_object_children-key '":' INTO l_json.

          serialize_node(
            EXPORTING
              jsonnode = wa_object_children-node
            IMPORTING
              json = l_child_json
          ) .
          CONCATENATE l_json l_child_json INTO l_json.
          CLEAR wa_array_children.
          l_index = 1.
        ENDLOOP.
        CONCATENATE l_json '}' INTO l_json.
    ENDCASE.

    json = l_json.
  ENDMETHOD.


  METHOD serialize_object.

  ENDMETHOD.

  METHOD get_escaped_value.

    json =  jsonnode->value.

    REPLACE ALL OCCURRENCES OF '"'                                    IN json WITH '\"'.
    REPLACE ALL OCCURRENCES OF '\'                                    IN json WITH '\\'.
    REPLACE ALL OCCURRENCES OF '/'                                    IN json WITH '\/'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN json WITH '\b'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN json WITH '\f'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN json WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN json WITH '\r'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN json WITH '\t'.
  ENDMETHOD.

  METHOD get_formatted_json_string.

    "cloud
*    DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( json_string_in ).
    "on_premise
    DATA(json_xstring) = cl_abap_codepage=>convert_to( json_string_in ).

    "Check and pretty print JSON

    DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
    DATA(writer) = CAST if_sxml_writer(
                          cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).

    "cloud
    "DATA(json_formatted_string) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
    "on premise
    DATA(json_formatted_string) = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).

    json_string_out = escape( val = json_formatted_string format = cl_abap_format=>e_xml_text  ).

  ENDMETHOD.
ENDCLASS.
