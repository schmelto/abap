REPORT z_select_demo_delete.

SELECT * FROM spfli
INTO TABLE @DATA(result).

DELETE result WHERE carrid = 'LH'.
cl_demo_output=>display_data( result ).
