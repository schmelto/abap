# Eclipse

## ABAP Development Tools (ADT)

https://tools.eu1.hana.ondemand.com/

## Shortcuts

| shortcut                                             | what it does                                              |
| ---------------------------------------------------- | --------------------------------------------------------- |
| <kbd>alt</kbd> + <kbd>shift</kbd> + <kbd>A</kbd>     | Eclipse Box Selection                                     |
| <kbd>alt</kbd> + <kbd>str</kbd> + <kbd>&#8595;</kbd> | duplicate lines                                           |
| <kbd>alt</kbd> + <kbd>U</kbd>                        | delete unused variables                                   |
| <kbd>shift</kbd> + <kbd>F1</kbd>                     | Pretty Printer                                            |
| <kbd>str</kbd> + <kbd>1</kbd>                        | Opens Quickfix/Quickassist Dialog on the selected element |
| <kbd>str</kbd> + <kbd>H</kbd>                        | Abap Source Text Search                                   |
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>A</kbd>   | Search                                                    |
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>L</kbd>     | List all available Keybord-Shortcuts                      |
| <kbd>str</kbd> + <kbd>space</kbd>                    | Auto complete                                             |
| <kbd>str</kbd> + <kbd><</kbd>                        | Comment                                                   |
| <kbd>str</kbd> + <kbd><</kbd>                        | Undo comment                                              |
| <kbd>str</kbd> + <kbd>E</kbd>                        | Show all open windows                                     |

&#8594; more shortcuts can be found [here](https://blogs.sap.com/2021/07/16/abap-adt-frequently-used-short-cuts/?utm_campaign=ABAPWeekly&utm_medium=email&utm_source=ABAPWeekly_50)

## Maintain Text Elements

![image](https://user-images.githubusercontent.com/30869493/128341169-e12e0512-5fcb-44ac-bf31-e7143faa9876.png)

## Copy Table lines as ABAP statement

![image](https://user-images.githubusercontent.com/30869493/134353052-51ebeecf-1b41-4afa-b6ed-deae8e12727d.png)

![image](https://user-images.githubusercontent.com/30869493/134353002-35a29244-95fb-4379-adb6-0401995ce109.png)

```abap
VALUE #( ( CLIENT ='100' ACCOUNT_NUMBER ='00000001' BANK_CUSTOMER_ID ='0000000000100001' BANK_NAME ='Volksbank' CITY ='Gaertringen' CUKY_FIELD ='' BALANCE ='200.00 ' CURRENCY ='EUR' ACCOUNT_CATEGORY ='01' LASTCHANGEDAT ='20210922132826.0000000 '  )
 ( CLIENT ='100' ACCOUNT_NUMBER ='00000002' BANK_CUSTOMER_ID ='0000000000200002' BANK_NAME ='Sparkasse' CITY ='Schwetzingen' CUKY_FIELD ='' BALANCE ='500.00 ' CURRENCY ='EUR' ACCOUNT_CATEGORY ='02' LASTCHANGEDAT ='20210922132826.0000000 '  )
 ( CLIENT ='100' ACCOUNT_NUMBER ='00000003' BANK_CUSTOMER_ID ='0000000000200003' BANK_NAME ='Commerzbank' CITY ='Nuernberg' CUKY_FIELD ='' BALANCE ='150.00 ' CURRENCY ='EUR' ACCOUNT_CATEGORY ='02' LASTCHANGEDAT ='20210922132826.0000000 '  )
 )
```

## Shortcuts + usage

### add missing method &#8594; <kbd>str</kbd> + <kbd>1</kbd>

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

### extracting a method &#8594; <kbd>alt</kbd> + <kbd>shift</kbd> + <kbd>M</kbd>

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

![image](https://user-images.githubusercontent.com/30869493/137021889-7c415633-27b1-427d-ba1e-ccfea7749b48.png)

![image](https://user-images.githubusercontent.com/30869493/137021968-7fc579ba-1bbc-4d69-a17e-502f4a3eb80b.png)

## ToDos

![image](https://user-images.githubusercontent.com/30869493/138055873-b134dfa6-36c4-4284-9b19-52c780899f67.png)

**Pattern:**

```abap
" ToDo:
"   Date: ${date}
"   User: ${user}
"   Info:
```

## Share Code

![image](https://user-images.githubusercontent.com/30869493/164220060-9a94673d-4627-4f36-8df1-25cb6d6c0ade.png)

![image](https://user-images.githubusercontent.com/30869493/164220003-b6210531-6323-48ec-aa84-bf16aa4932d2.png)
