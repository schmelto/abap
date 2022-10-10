## SQL - Database selection

### Get different / distinct values of a table

Get only the different prices of `ckmlcr_single`.

| MANDT | KALNR        | PVPRS |
| ----- | ------------ | ----- |
| 0001  | 000100001200 | 15.00 |
| 0001  | 000100001200 | 16.50 |
| 0001  | 000100001200 | 15.00 |

**Example:**

```abap
SELECT DISTINCT pvprs FROM @ckmlcr_single AS ckmlcr_single INTO TABLE @DATA(unique_pvprs).
```

**Output:**

| PVPRS |
| ----- |
| 15.00 |
| 16.50 |

**Use Case:**

Check if there are differences in `PVPRS` further coding could be check if there are more than 1 line in `unique_pvprs`.

```abap
IF lines( unique_pvprs ) > 1.
```
