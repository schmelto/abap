# SAP

## SAP Gui Shortcuts

| shortcut | what it does |
|----|----|
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>7</kbd> | focus on execution bar |
| <kbd>str</kbd> + <kbd>N</kbd> | open new window |
| <kbd>str</kbd> + <kbd>Y</kbd> | mark in SAP Gui |

## SAP Tables
| table | what it does |
|----|----|
| ABLM_BLACKLIST | Blacklist Items of Executables from Applicaitons |
| ACDOCA | Universal Journal Entry Line Items |
| DD02L | SAP Tables data |
| T001 | Company Codes |
| TSTC | SAP Transaction Codes |
| TSTCT | SAP Transaction Codes Texts |

## Edit SAP Table entries

### SE16

- run transaction `SE16`
- Enter Table
- Fill selection screen &#8594; <kbd>F8</kbd>
- Select Table entries
- "View" with the glasses
- start debugger with `/h` in command line
- click in the relevant field and hit <kbd>ENTER</kbd>
- Aktivate Variables
  - EDIT &#8594; Modify line
  - INSR &#8594; Insert line
  - DELE &#8594; Delete line

### SE16N

- run transaction `SE16N`
- Enter Table
- `/h`
- in the variables fill variables and change the value to `x` with the pen icon
  - gd-edit &#8594; x
  - gd-sapedit &#8594; x
  
  ![se16n debugger variables](https://user-images.githubusercontent.com/30869493/124892270-041ff580-dfda-11eb-81c8-fa1359b1bba4.png)

- <kbd>ENTER</kbd> &#8594; <kbd>F8</kbd>

## Transport SAP Table entries

- run transaction `SE37`
- Execute Function Module `SE16N_INTERFACE`
  - I_TAB &#8594; set table
	- I_EDIT &#8594; x
	- I_SAPEDIT &#8594; x
  - I_DISPLAY&#8594; x
- <kbd>ENTER</kbd> &#8594; <kbd>F8</kbd>

Select the enteries you want to transport
&#8594; `Table Entry` &#8594; `Tranport`
![image](https://user-images.githubusercontent.com/30869493/126965425-8a5e7477-4f25-46a8-ae55-2825a648a473.png)

## Debug Payment Run

1. Set a Break-Point in the program / Function module
2. Start the Payment Proposal Run in F110 without "Start immediately", for example using start time 12pm
3. go to the Job Overview page using transaction `SM37` / `SMX` &#8594; mark the F110 Job &#8594; enter `JDBG` in the execution bar, this will start the Job
4. It should stop at the Break-Point

## Find BADIs for an executed transaction &#8594; `CL_EXITHANDLER`

* Go to transaction `SE80` Class `CL_EXITHANDLER`
* navigate to method `get_instance` and set a Break-Point in the first line of `cl_exithandler=>get_class_name_by_interface`
* Use the transaction in which you are BAdI-hunting
* Examine the contents of the field exit_name whenever the processing stops at the breakpoint. I have found a case where exit_name was an unknown field. Then class_name gave a good clue to the name of the BAdI.

### Configure tax codes that will be available for end-users

Transaction: `OBZT`

In this transaction you can configure the tax codes that will be available for end-users in FI-AP (FB60 / FB65) / FI-AR (FB70 / FB75) transactions as well as logistics invoice verification (MIRO).

You can also set some tax code as default one in these transactions. Screenshot of typical tax codes settings can be found below. Note: the visibility of input tax codes can be customized differently for FI-AP accounting and logistics invoice verification (in case, when FI-AP should be used only in certain cases that require specific tax codes). In this example the visibility of input tax codes was customized in the same way for FI-AP and MM-LIV areas.

![image](https://user-images.githubusercontent.com/30869493/140476010-9d073da7-d9d2-4cb0-9480-343c1534574a.png)