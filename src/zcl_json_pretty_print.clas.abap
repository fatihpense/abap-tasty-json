CLASS zcl_json_pretty_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    CLASS-METHODS get_formatted_json_string
      IMPORTING json_string_in         TYPE string
      RETURNING VALUE(json_string_out) TYPE string
      RAISING   cx_sxml_parse_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_json_pretty_print IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA json_string  TYPE string .
    DATA formatted_json_string TYPE string.

    json_string = '{ "firstName": "John", "lastName": "Smith", "isAlive": true, "age": 27,' && |\r\n|  &&
                  '  "address": { "streetAddress": "21 2nd Street", "city": "New York", "state": "NY",' && |\r\n|  &&
                  '    "postalCode": "10021-3100" }, "phoneNumbers": [  { "type": "home",' && |\r\n|  &&
                  '      "number": "212 555-1234"  },  { "type": "office",  "number": "646 555-4567" }   ],' && |\r\n|  &&
                  '  "children": [],   "spouse": null }'.
    TRY.
        formatted_json_string = get_formatted_json_string( json_string ).
      CATCH cx_sxml_parse_error.
        out->write( 'parse error' ).
        EXIT.
    ENDTRY.

    out->write( formatted_json_string ).

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
