# SAP

## SAP Gui Shortcuts

| shortcut | what it does |
|----|----|
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>7</kbd> | focus on execution bar |
| <kbd>str</kbd> + <kbd>N</kbd> | open new window |
| <kbd>str</kbd> + <kbd>Y</kbd> | mark in SAP Gui |

## Transactions (prefix)

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

## Customizing transactions/reports

| shortcut | what it does |
|----|----|
| `OBB8` | Maintain Payment terms |
| `FBMP` | Dunning |
| `FBZP` | Payment run |
| `RFVITXBA` (Report) | Transport SO10 text |
| `VKOA` | Accountfinding |

## SAP Tables
| table | what it does |
|----|----|
| ABLM_BLACKLIST | Blacklist Items of Executables from Applicaitons |
| ACDOCA | Universal Journal Entry Line Items |
| T001 | Company Codes |
| TSTC | SAP Transaction Codes |

## Edit SAP Table entries

### SE16

- run transaction `SE16`
- Enter Table
- Fill selection screen -> <kbd>F8</kbd>
- Select Table entries
- "View" with the glasses
- start debugger with `/h` in command line
- click in the relevant field and hit <kbd>ENTER</kbd>
- Aktivate Variables
  - EDIT -> Modify line
  - INSR -> Insert line
  - DELE -> Delete line

### SE16N

- run transaction `SE16N`
- Enter Table
- `/h`
- in the variables fill variables and change the value to `x` with the pen icon
  - gd-edit -> x
  - gd-sapedit -> x
  
  ![se16n debugger variables](https://user-images.githubusercontent.com/30869493/124892270-041ff580-dfda-11eb-81c8-fa1359b1bba4.png)

- <kbd>ENTER</kbd> -> <kbd>F8</kbd>

## Transport SAP Table entries

- run transaction `SE37`
- Execute Function Module `SE16N_INTERFACE`
  - I_TAB -> set table
	- I_EDIT -> x
	- I_SAPEDIT -> x
  - I_DISPLAY-> x
- <kbd>ENTER</kbd> -> <kbd>F8</kbd>

Select the enteries you want to transport
-> `Table Entry` -> `Tranport`
![image](https://user-images.githubusercontent.com/30869493/126965425-8a5e7477-4f25-46a8-ae55-2825a648a473.png)

## Debug Payment Run

1. Set a Break-Point in the program / Function module
2. Start the Payment Proposal Run in F110 without "Start immediately", for example using start time 12pm
3. go to the Job Overview page using transaction `SM37` / `SMX` -> mark the F110 Job -> enter `JDBG` in the execution bar, this will start the Job
4. It should stop at the Break-Point

## Find BADIs for an excuted transaction -> `CL_EXITHANDLER`

* Go to transaction `SE80` Class `CL_EXITHANDLER`
* navigate to method `get_instance` and set a Break-Point in the first line of `cl_exithandler=>get_class_name_by_interface`
* Use the transaction in which you are BAdI-hunting
* Examine the contents of the field exit_name whenever the processing stops at the breakpoint. I have found a case where exit_name was an unknown field. Then class_name gave a good clue to the name of the BAdI.