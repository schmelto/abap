## ABAP Daemon

A daemon is a utility program that runs continuously by itself and without the control of an interactive user on a multi-tasking operating system in the background to monitor and take care of certain subsystems or show immediate reaction to events. Daemons perform explicit actions at predefined times or in response to certain events.

Superclass: `CL_ABAP_DAEMON_EXT_BASE`

Full sample program: [zcl_tutorial_simple_daemon.abap](abap/samples/zcl_tutorial_simple_daemon.md)

**How to start the deamon in another program?**

```abap
REPORT z_tutorial_simple_daemon_start.

zcl_tutorial_simple_daemon=>start( iv_daemon_name = 'simple_daemon' iv_timeout = 10000 ).

DATA(lv_text) = `This is a simple ABAP Daemon message sent via PCP.`.
zcl_tutorial_simple_daemon=>send( iv_daemon_name = 'simple_daemon' iv_text = lv_text ).

zcl_tutorial_simple_daemon=>stop( iv_daemon_name = 'simple_daemon' ).
```

You can monitor all running ABAP Daemons using transaction `SMDAEMON` in SAPGUI. There you can see their state, check for errors, and also restart and terminate them.

To stop your daemon, select it from the list and go to `ABAP Daemon` &#8594; `Terminate Daemon`. Alternatively, you can also create a static STOP method.

