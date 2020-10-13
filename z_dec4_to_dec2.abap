REPORT z_dec4_to_dec2.

DATA: dec2 TYPE p DECIMALS 2,
      dec4 TYPE p DECIMALS 4.

dec2 = 0.
dec4 = '23.1234'.

dec2 = dec4 - ( frac( dec4 * 100 ) / 100 ).

WRITE dec2.
