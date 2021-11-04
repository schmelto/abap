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

## Maintain Text Elements

![image](https://user-images.githubusercontent.com/30869493/128341169-e12e0512-5fcb-44ac-bf31-e7143faa9876.png)

## eclipse-abap-keywordcolors

ABAP Keyword Colors -> Import -> .xml

## Shortcuts + usage

### add missing method -> `str` + `1`

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

### extracting a method -> `alt` + `shift` + `M`

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
    "Local Variables
    DATA: monster_madness1 TYPE i,
          monster_madness2 TYPE i,
          description1     TYPE string,
          description2     TYPE string.

    monster_madness1 = 25.
    monster_madness2 = 50.

    " Derive Monster Sanity
    IF monster_madness1 LT 30.
      description1 = 'Fairly sane'.
    ELSEIF monster_madness1 GT 90.
      description1 = 'BONKERS'.
    ENDIF.
    IF monster_madness2 LT 30.
      description1 = 'Fairly sane'.
    ELSEIF monster_madness2 GT 90.
      description1 = 'BONKERS'.
    ENDIF.

  ENDMETHOD.
  
ENDCLASS.
```

alt + shift + m

![image](https://user-images.githubusercontent.com/30869493/137021889-7c415633-27b1-427d-ba1e-ccfea7749b48.png)

![image](https://user-images.githubusercontent.com/30869493/137021968-7fc579ba-1bbc-4d69-a17e-502f4a3eb80b.png)
