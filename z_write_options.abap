REPORT z_write_options.

DATA: text TYPE string VALUE '0123456789ABCDEF',
      col  TYPE i VALUE 25,
      len  TYPE i VALUE 5.

WRITE text.
WRITE /5(16) text.
WRITE AT col(len) text.
