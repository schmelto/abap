### TDF - Test Double Framework

The Test Double Framework (TDF for short) provides tools to create so-called doubles for the objects class, table and core data service (CDS) at test runtime to test against them.

**General:**

The SQL Double has the task to replace the database layer and thus to replace all queries (CRUD) to one or more tables by a corresponding double. The tables can be specified during creation.

1. normal test scenario

`Test Class` &rarr; `Code Under Test` &rarr; `Database Table`

2. test scenario where table will be replaced by a double

`Test Class` &rarr; `Code Under Test` &rarr; `Double`

`Test Class` &rarr; `Double`

The double replaces the data in the table. All CRUD operations run against the double.

**Data exchange:**

However, we do not exchange the data directly on the database, these are available at any time and remain consistent. The generated test double is available to us for our test class and is removed again after execution.

The advantage of this technique is the stability of our test cases, because we cannot be sure that our test data on the database will not change. With this technique, data for our test always remain the same and you don't have to outsource or even adapt the accesses to the database to other classes.

**Example:**

In this example we want to replace the tables `LFB1` and `KNB1` with doubles.

**Definition:**
```abap
PRIVATE SECTION.
	CLASS-DATA:
		"! <p class="shorttext synchronized">Interface to use SQL fakes / Test doubles</p>
		go_environment TYPE REF TO if_osql_test_environment.
```

```abap
CLASS-METHODS: class_setup.
CLASS-METHODS: class_teardown.
```

**Implementation:**

```abap
METHOD class_setup.

	DATA: lt_lfb1 TYPE STANDARD TABLE OF lfb1 WITH EMPTY KEY,
	  lt_knb1 TYPE STANDARD TABLE OF knb1 WITH EMPTY KEY.

	go_environment = cl_osql_test_environment=>create( VALUE #( ( 'LFB1' )
								( 'KNB1' ) ) ).

	lt_lfb1 = VALUE #(
			( mandt = sy-mandt lifnr = '230002' bukrs = '0001' intad = 'test@test.com')
		     ).
	lt_knb1 = VALUE #(
			( mandt = sy-mandt kunnr = '220002' bukrs = '0001' intad = 'test@test.com')
		     ).

	go_environment->insert_test_data( lt_lfb1 ).
	go_environment->insert_test_data( lt_knb1 ).
ENDMETHOD.
```

```abap
METHOD class_teardown.

	go_environment->destroy( ).

ENDMETHOD.
```

* In the setup method, the SQL environment is built first and the tables to be exchanged are specified.
* Transfer of the test data to the environment
* Execution of the test cases against the test data
* Dismantling of the environment when the test class is dismantled

### How to implement Unit Tests to a global Class in Eclipse?

Declare Test Classes: 
![declare_test_class](https://user-images.githubusercontent.com/30869493/116395736-cc037600-a824-11eb-88cb-d40260edc36a.png)