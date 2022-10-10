## DATA and Types

### Use inline declaration

This:

```abap
DATA(name) = 'something'.
```

is better than this:

```abap
" anti-pattern
DATA: name Type string.
name = 'something'.
```

but unfortunately not always applicable...
