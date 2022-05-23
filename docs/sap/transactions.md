## Transactions

### Transactions (prefix)

| shortcut | what it does |
|----|----|
| `/n` | execute transaction in current window |
| `/o` | execute transaction in a new window |
| `/i` | closes the current window |
| `/nex` | closses all windows and log off from the SAP System |
| `/*` | execute transaction with history data |
| `ANST` | Find all the Enhancements that are Implemented for a Transaction Code in SAP |
| `CODE_SCANNER` | Check for Code |
| `S_BCE_68001398` | Check wether a user have authorization to use a transaction or not |
| `SAT` | Runtime Analysis |
| `SCC4` | Open System for Customizing | 
| `SE93` | Maintain transactions |
| `SEARCH_SAP_MENU` | search though the SAP Menu |
| `SM04` | End User Sessions |
| `STMS` | Transport Management System (check if a transport went into the system) |

### Customizing transactions/reports

| shortcut | what it does |
|----|----|
| `OBB8` | Maintain Payment terms |
| `FBMP` | Dunning |
| `FBZP` | Payment run |
| `RFVITXBA` (Report) | Transport SO10 text |
| `VKOA` | Accountfinding |

### Sets

Transactions `GS01`, `GS02` and `GS03`.

![GS03](https://user-images.githubusercontent.com/30869493/169770738-bdf044b8-680b-446e-a907-32b399d7a5b4.png)

**Example calls in ABAP:**
```abap
CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
  EXPORTING
*    client                   = 
    shortname                = 
*    old_setid                = 
*    tabname                  = 
*    fieldname                = 
*    kokrs                    = 
*    ktopl                    = 
*    lib                      = 
*    rname                    = 
*    setclass                 = 
*    check_set_empty          = space
*    supress_popup            = space
*    no_dynamic_sets          = space
*  IMPORTING
*    new_setid                = 
*    set_info                 = 
*  TABLES
*    t_sets                   = 
*  EXCEPTIONS
*    no_set_found             = 1
*    no_set_picked_from_popup = 2
*    wrong_class              = 3
*    wrong_subclass           = 4
*    table_field_not_found    = 5
*    fields_dont_match        = 6
*    set_is_empty             = 7
*    formula_in_set           = 8
*    set_is_dynamic           = 9
*    others                   = 10
  .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
```

```abap
CALL FUNCTION 'G_SET_SEARCH_FOR_INTERVAL'
  EXPORTING
    set              = 
    from             = 
*    to               = 
*    tabelle          = 
*    include_subsets  = 'X'
*    internal_values  = space
*  TABLES
*    vallist          = 
*  EXCEPTIONS
*    value_not_found  = 1
*    set_not_found    = 2
*    conversion_error = 3
*    wrong_settype    = 4
*    wrong_interval   = 5
*    others           = 6
  .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
```

### Maintain Transactions

Transaction `SE93`

### Usage

1. Transaction `ST03N`
2. Select period
3. Transaction-Profile
4. Tasktype: DIALOG
5. Search for the transaction

### Post activation of open item management GL Account

Transaction: `FINS_ACTIVATE_OIM`
