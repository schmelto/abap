## ABAP Docs

To make the method more readable/descriptive ABAP Doc expressions should be used:

```abap
"! <p class="shorttext synchronized">Modify payment IDOCs</p>
"! @exception dont_create_idoc | <p class="shorttext synchronized">Don't created a payment IDOC</p>
"! @parameter im_mestyp | <p class="shorttext synchronized">Message type</p>
"! @parameter im_reguh | <p class="shorttext synchronized">Payment data from the payment program</p>
"! @parameter im_regud | <p class="shorttext synchronized">Transfer data form printing</p>
"! @parameter im_flag_no_replace | <p class="shorttext synchronized">SPACE=Special characters conversion required in texts</p>
"! @parameter ex_fimsg | <p class="shorttext synchronized">FI-messages</p>
"! @parameter ch_xavis | <p class="shorttext synchronized">Flag: Advice required</p>
"! @parameter ch_edidc | <p class="shorttext synchronized">Control record (IDoc)</p>
"! @parameter t_regup | <p class="shorttext synchronized">Processed items from the payment program</p>
"! @parameter t_edidd | <p class="shorttext synchronized">Data record (IDoc)</p>
```

Types can be documented like:

```abap
TYPES:
   "! <p class="shorttext synchronized">Explanation text</p>
   type TYPE TABLE OF spfli.
```

Same for `BEGIN OF...`-Structures.

```abap
TYPES:
	"! <p class="shorttext synchronized">Explanation text</p>
	BEGIN OF structure,
   		field TYPE OF type.
	TYPES END OF structure.
```

### Short Texts and their Synchronization

The text between those bracets (`<p class="shorttext synchronized">Here some explanation text</p>`) is also shown in SAP GUI or in the ABAP Element Info in Eclipse.

### Documentation Links

In an ABAP Doc comment, the following syntax can be used to refer to the documentation of other repository objects:

```abap
... {@link [[[kind:]name.]...][kind:]name} ...
```

Example:

```abap
"! {@link PROG:z_test.class.METH:get_x_last_chars}
```

### Test Relations

The following syntax can be used to define so called test relations in front of the declaration of a test class or a test method:

```abap
"! @testing [kind:]name
```

Example:

```abap
"! @testing my_cds_view
```
