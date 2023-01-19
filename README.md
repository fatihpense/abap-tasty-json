# Fork of abap-tasty-json

## Imporvements:
- Pretty Print with method `serialize_formatted`
- new method `string_set_child_node` to create directly a sting node with key and value
- Fixed handilng of escaped characters `\"` `\n` `\t` `\r` `\f` `\b` `\/` `\\` in serialization

# From the original library documentation
This library fixes regex parsing bugs from the original library [zcl_mdp_json](https://github.com/fatihpense/zcl_mdp_json) It is still used by/found useful by some ABAP developers and it was hard to fix bugs without introducing new bugs.

Regex is never a complete solution for parsing. That is why, this library parses JSON string character by character. It is slower than the original library but even if we find new bugs, it will be easier to fix them.

Original Blog Post:
https://blogs.sap.com/2016/07/03/an-open-source-abap-json-library-zclmdpjson/

## Quick fix patch for the old library

If you are already using the old library and need quick fixes, you can use the parsing class [here](docs/old-library-patch/zcl_mdp_json_deserializer2.abap)
