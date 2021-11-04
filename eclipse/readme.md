# Eclipse

## ToDos

![image](https://user-images.githubusercontent.com/30869493/138055873-b134dfa6-36c4-4284-9b19-52c780899f67.png)

**Pattern:**
```abap
" ToDo: 
"   Date: ${date}
"   User: ${user}
"   Info: 
```

## eclipse-abap-keywordcolors

ABAP Keyword Colors -> Import -> .xml

## add missing method `str` + `1`

```abap
CLASS zcl_monster DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: main.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_monster IMPLEMENTATION.
  METHOD main.
    " Local Variables
    DATA: ld_monster_number  TYPE i,
          ld_number_of_heads TYPE i.

    create_monster( id_number_of_heads = ld_number_of_heads ).

  ENDMETHOD.

ENDCLASS.
```

![image](https://user-images.githubusercontent.com/30869493/137023157-2ef3fe0a-bd5e-4baf-b213-cf0e5275016b.png)


![image](https://user-images.githubusercontent.com/30869493/137021266-03e38fb7-6ddb-41a6-9bf3-3a13c81fb488.png)

```abap
METHODS create_monster
      IMPORTING
        number_of_heads       TYPE i
      RETURNING
        value(monster_number) TYPE i.
```

