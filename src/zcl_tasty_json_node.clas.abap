CLASS zcl_tasty_json_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF typ_object_children,
        key  TYPE string,
        node TYPE REF TO zcl_tasty_json_node,
      END OF typ_object_children .
    TYPES:
      BEGIN OF typ_array_children,
        node TYPE REF TO  zcl_tasty_json_node,
      END OF typ_array_children .
    TYPES:
      tt_object_children TYPE HASHED TABLE OF zcl_tasty_json_node=>typ_object_children WITH UNIQUE KEY key .
    TYPES:
      tt_array_children  TYPE TABLE OF zcl_tasty_json_node=>typ_array_children .

    CONSTANTS co_json_object TYPE i VALUE 02 ##NO_TEXT.
    CONSTANTS co_json_array TYPE i VALUE 03 ##NO_TEXT.
    CONSTANTS co_json_string TYPE i VALUE 04 ##NO_TEXT.
    CONSTANTS co_json_number TYPE i VALUE 05 ##NO_TEXT.
    CONSTANTS co_json_true TYPE i VALUE 06 ##NO_TEXT.
    CONSTANTS co_json_false TYPE i VALUE 07 ##NO_TEXT.
    CONSTANTS co_json_null TYPE i VALUE 08 ##NO_TEXT.
    DATA json_type TYPE i .
    DATA value TYPE string .
    DATA object_children TYPE tt_object_children .
    DATA array_children TYPE tt_array_children .

    METHODS constructor
      IMPORTING
        !json_type TYPE i .
    METHODS object_get_child_node
      IMPORTING
        !key        TYPE string
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    METHODS array_get_child_node
      IMPORTING
        !index      TYPE i
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    METHODS string_set_value
      IMPORTING
        VALUE(value) TYPE string
      RETURNING
        VALUE(node)  TYPE REF TO zcl_tasty_json_node .
    METHODS array_add_child_node
      IMPORTING
        !child_node       TYPE REF TO zcl_tasty_json_node
      RETURNING
        VALUE(array_node) TYPE REF TO zcl_tasty_json_node .
    METHODS object_add_child_node
      IMPORTING
        !child_key         TYPE string
        !child_node        TYPE REF TO zcl_tasty_json_node
      RETURNING
        VALUE(object_node) TYPE REF TO zcl_tasty_json_node .
    METHODS serialize
      RETURNING
        VALUE(json_string) TYPE string .
    METHODS serialize_formatted
      RETURNING
        VALUE(json_string) TYPE string .
    CLASS-METHODS deserialize
      IMPORTING
        !json       TYPE string
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node
      RAISING
        zcx_tasty_json_invalid .
    CLASS-METHODS create_node
      IMPORTING
        !json_type  TYPE i
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_object_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_array_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_string_node
      IMPORTING i_value     TYPE string OPTIONAL
      RETURNING
                VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_number_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_true_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_false_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
    CLASS-METHODS create_null_node
      RETURNING
        VALUE(node) TYPE REF TO zcl_tasty_json_node .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_tasty_json_node IMPLEMENTATION.


  METHOD array_add_child_node.

    DATA : wa_array_children TYPE zcl_tasty_json_node=>typ_array_children .

    wa_array_children-node = child_node .

    APPEND wa_array_children TO me->array_children.
    array_node = me.
  ENDMETHOD.


  METHOD array_get_child_node.

    node = me->array_children[  index  ]-node .

  ENDMETHOD.


  METHOD constructor.
    me->json_type = json_type.

  ENDMETHOD.


  METHOD create_array_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_array .
  ENDMETHOD.


  METHOD create_false_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_false .
  ENDMETHOD.


  METHOD create_node.

*    DATA : l_json_node TYPE REF TO ZCL_TASTY_json_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = json_type .

*    node = l_json_node.
  ENDMETHOD.


  METHOD create_null_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_null .
  ENDMETHOD.


  METHOD create_number_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_number .
  ENDMETHOD.


  METHOD create_object_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_object .
  ENDMETHOD.


  METHOD create_string_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_string .
    NODE->value = i_value.
  ENDMETHOD.


  METHOD create_true_node.
    CREATE OBJECT node TYPE zcl_tasty_json_node EXPORTING json_type = zcl_tasty_json_node=>co_json_true .
  ENDMETHOD.


  METHOD deserialize.
*DATA: l_json_node TYPE REF TO ZCL_TASTY_json_node.

    zcl_tasty_json_deserializer=>deserialize(
      EXPORTING json = json
      IMPORTING node = node
   ).

  ENDMETHOD.


  METHOD object_add_child_node.

    DATA : wa_array_children TYPE zcl_tasty_json_node=>typ_array_children .

    wa_array_children-node = child_node .

    APPEND wa_array_children TO me->array_children.


    DATA : wa_object_children TYPE  zcl_tasty_json_node=>typ_object_children .
    wa_object_children-key = child_key .

    wa_object_children-node = child_node .

    INSERT wa_object_children INTO TABLE me->object_children.

    object_node = me.


  ENDMETHOD.


  METHOD object_get_child_node.

    node = me->object_children[ key = key  ]-node .

  ENDMETHOD.


  METHOD serialize.

    zcl_tasty_json_serializer=>serialize(
  EXPORTING node = me
  IMPORTING json = json_string ).

  ENDMETHOD.

  METHOD serialize_formatted.

    zcl_tasty_json_serializer=>serialize_formatted(
  EXPORTING node = me
  IMPORTING json = json_string ).

  ENDMETHOD.

  METHOD string_set_value.
    me->value = value.
    node = me.
  ENDMETHOD.
ENDCLASS.
