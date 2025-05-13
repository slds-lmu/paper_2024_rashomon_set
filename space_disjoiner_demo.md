

``` r
# render with knitr::spin("space_disjoiner_demo.R", knit = TRUE, report = FALSE)

parset <- ps(
  x = p_dbl(0, 1),
  y = p_dbl(0, 1),
  a = p_fct(c("a", "b", "c")),
  b = p_lgl(depends = a == "b"),
  c = p_lgl(depends = a %in% c("b", "c") && b == TRUE),
  d = p_fct(c("x", "y"), depends = a == "c"),
  only.in.a.ab = p_dbl(0, 1, depends = a %in% c("a", "b")),
  only.in.c.true = p_dbl(0, 1, depends = c == TRUE)
)

parset
```

```
## <ParamSet(8)>
## Key: <id>
##                id    class lower upper nlevels        default parents  value
##            <char>   <char> <num> <num>   <num>         <list>  <list> <list>
## 1:              a ParamFct    NA    NA       3 <NoDefault[0]>  [NULL] [NULL]
## 2:              b ParamLgl    NA    NA       2 <NoDefault[0]>       a [NULL]
## 3:              c ParamLgl    NA    NA       2 <NoDefault[0]>     a,b [NULL]
## 4:              d ParamFct    NA    NA       2 <NoDefault[0]>       a [NULL]
## 5:   only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]>       a [NULL]
## 6: only.in.c.true ParamDbl     0     1     Inf <NoDefault[0]>       c [NULL]
## 7:              x ParamDbl     0     1     Inf <NoDefault[0]>  [NULL] [NULL]
## 8:              y ParamDbl     0     1     Inf <NoDefault[0]>  [NULL] [NULL]
```

``` r
sdx <- SpaceDisjoiner$new(parset)

sdx$param.set
```

```
## <ParamSet(8)>
## Key: <id>
##                id    class lower upper nlevels        default parents  value
##            <char>   <char> <num> <num>   <num>         <list>  <list> <list>
## 1:              a ParamFct    NA    NA       3 <NoDefault[0]>  [NULL] [NULL]
## 2:              b ParamLgl    NA    NA       2 <NoDefault[0]>       a [NULL]
## 3:              c ParamLgl    NA    NA       2 <NoDefault[0]>     a,b [NULL]
## 4:              d ParamFct    NA    NA       2 <NoDefault[0]>       a [NULL]
## 5:   only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]>       a [NULL]
## 6: only.in.c.true ParamDbl     0     1     Inf <NoDefault[0]>       c [NULL]
## 7:              x ParamDbl     0     1     Inf <NoDefault[0]>  [NULL] [NULL]
## 8:              y ParamDbl     0     1     Inf <NoDefault[0]>  [NULL] [NULL]
```

``` r
sdx$subspaces
```

```
## $`a=a,b=NA,c=NA`
## <ParamSet(3)>
##              id    class lower upper nlevels        default  value
##          <char>   <char> <num> <num>   <num>         <list> <list>
## 1: only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 2:            x ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 3:            y ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 
## $`a=b,b=TRUE,c=TRUE`
## <ParamSet(4)>
##                id    class lower upper nlevels        default  value
##            <char>   <char> <num> <num>   <num>         <list> <list>
## 1:   only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 2: only.in.c.true ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 3:              x ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 4:              y ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 
## $`a=c,b=NA,c=NA`
## <ParamSet(3)>
##        id    class lower upper nlevels        default  value
##    <char>   <char> <num> <num>   <num>         <list> <list>
## 1:      d ParamFct    NA    NA       2 <NoDefault[0]> [NULL]
## 2:      x ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 3:      y ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 
## $`a=b,b=FALSE,c=NA`
## <ParamSet(3)>
##              id    class lower upper nlevels        default  value
##          <char>   <char> <num> <num>   <num>         <list> <list>
## 1: only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 2:            x ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 3:            y ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 
## $`a=b,b=TRUE,c=FALSE`
## <ParamSet(3)>
##              id    class lower upper nlevels        default  value
##          <char>   <char> <num> <num>   <num>         <list> <list>
## 1: only.in.a.ab ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 2:            x ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
## 3:            y ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
```

``` r
grid <- generate_design_grid(parset, 2)

grid$data
```

```
##         x     y      a      b      c      d only.in.a.ab only.in.c.true
##     <num> <num> <char> <lgcl> <lgcl> <char>        <num>          <num>
##  1:     0     0      a     NA     NA   <NA>            0             NA
##  2:     0     0      a     NA     NA   <NA>            1             NA
##  3:     0     0      b   TRUE   TRUE   <NA>            0              0
##  4:     0     0      b   TRUE   TRUE   <NA>            0              1
##  5:     0     0      b   TRUE   TRUE   <NA>            1              0
##  6:     0     0      b   TRUE   TRUE   <NA>            1              1
##  7:     0     0      b   TRUE  FALSE   <NA>            0             NA
##  8:     0     0      b   TRUE  FALSE   <NA>            1             NA
##  9:     0     0      b  FALSE     NA   <NA>            0             NA
## 10:     0     0      b  FALSE     NA   <NA>            1             NA
## 11:     0     0      c     NA     NA      x           NA             NA
## 12:     0     0      c     NA     NA      y           NA             NA
## 13:     0     1      a     NA     NA   <NA>            0             NA
## 14:     0     1      a     NA     NA   <NA>            1             NA
## 15:     0     1      b   TRUE   TRUE   <NA>            0              0
## 16:     0     1      b   TRUE   TRUE   <NA>            0              1
## 17:     0     1      b   TRUE   TRUE   <NA>            1              0
## 18:     0     1      b   TRUE   TRUE   <NA>            1              1
## 19:     0     1      b   TRUE  FALSE   <NA>            0             NA
## 20:     0     1      b   TRUE  FALSE   <NA>            1             NA
## 21:     0     1      b  FALSE     NA   <NA>            0             NA
## 22:     0     1      b  FALSE     NA   <NA>            1             NA
## 23:     0     1      c     NA     NA      x           NA             NA
## 24:     0     1      c     NA     NA      y           NA             NA
## 25:     1     0      a     NA     NA   <NA>            0             NA
## 26:     1     0      a     NA     NA   <NA>            1             NA
## 27:     1     0      b   TRUE   TRUE   <NA>            0              0
## 28:     1     0      b   TRUE   TRUE   <NA>            0              1
## 29:     1     0      b   TRUE   TRUE   <NA>            1              0
## 30:     1     0      b   TRUE   TRUE   <NA>            1              1
## 31:     1     0      b   TRUE  FALSE   <NA>            0             NA
## 32:     1     0      b   TRUE  FALSE   <NA>            1             NA
## 33:     1     0      b  FALSE     NA   <NA>            0             NA
## 34:     1     0      b  FALSE     NA   <NA>            1             NA
## 35:     1     0      c     NA     NA      x           NA             NA
## 36:     1     0      c     NA     NA      y           NA             NA
## 37:     1     1      a     NA     NA   <NA>            0             NA
## 38:     1     1      a     NA     NA   <NA>            1             NA
## 39:     1     1      b   TRUE   TRUE   <NA>            0              0
## 40:     1     1      b   TRUE   TRUE   <NA>            0              1
## 41:     1     1      b   TRUE   TRUE   <NA>            1              0
## 42:     1     1      b   TRUE   TRUE   <NA>            1              1
## 43:     1     1      b   TRUE  FALSE   <NA>            0             NA
## 44:     1     1      b   TRUE  FALSE   <NA>            1             NA
## 45:     1     1      b  FALSE     NA   <NA>            0             NA
## 46:     1     1      b  FALSE     NA   <NA>            1             NA
## 47:     1     1      c     NA     NA      x           NA             NA
## 48:     1     1      c     NA     NA      y           NA             NA
##         x     y      a      b      c      d only.in.a.ab only.in.c.true
```

``` r
sdx$disjoinTable(grid$data)
```

```
## $`a=a,b=NA,c=NA`
##    only.in.a.ab     x     y
##           <num> <num> <num>
## 1:            0     0     0
## 2:            1     0     0
## 3:            0     0     1
## 4:            1     0     1
## 5:            0     1     0
## 6:            1     1     0
## 7:            0     1     1
## 8:            1     1     1
## 
## $`a=b,b=TRUE,c=TRUE`
##     only.in.a.ab only.in.c.true     x     y
##            <num>          <num> <num> <num>
##  1:            0              0     0     0
##  2:            0              1     0     0
##  3:            1              0     0     0
##  4:            1              1     0     0
##  5:            0              0     0     1
##  6:            0              1     0     1
##  7:            1              0     0     1
##  8:            1              1     0     1
##  9:            0              0     1     0
## 10:            0              1     1     0
## 11:            1              0     1     0
## 12:            1              1     1     0
## 13:            0              0     1     1
## 14:            0              1     1     1
## 15:            1              0     1     1
## 16:            1              1     1     1
## 
## $`a=c,b=NA,c=NA`
##         d     x     y
##    <char> <num> <num>
## 1:      x     0     0
## 2:      y     0     0
## 3:      x     0     1
## 4:      y     0     1
## 5:      x     1     0
## 6:      y     1     0
## 7:      x     1     1
## 8:      y     1     1
## 
## $`a=b,b=FALSE,c=NA`
##    only.in.a.ab     x     y
##           <num> <num> <num>
## 1:            0     0     0
## 2:            1     0     0
## 3:            0     0     1
## 4:            1     0     1
## 5:            0     1     0
## 6:            1     1     0
## 7:            0     1     1
## 8:            1     1     1
## 
## $`a=b,b=TRUE,c=FALSE`
##    only.in.a.ab     x     y
##           <num> <num> <num>
## 1:            0     0     0
## 2:            1     0     0
## 3:            0     0     1
## 4:            1     0     1
## 5:            0     1     0
## 6:            1     1     0
## 7:            0     1     1
## 8:            1     1     1
```

``` r
sdx$disjoinTable(grid$data[c(7, 5, 4, 1)])
```

```
## $`a=a,b=NA,c=NA`
##    only.in.a.ab     x     y
##           <num> <num> <num>
## 1:            0     0     0
## 
## $`a=b,b=TRUE,c=TRUE`
##    only.in.a.ab only.in.c.true     x     y
##           <num>          <num> <num> <num>
## 1:            1              0     0     0
## 2:            0              1     0     0
## 
## $`a=c,b=NA,c=NA`
## Empty data.table (0 rows and 3 cols): d,x,y
## 
## $`a=b,b=FALSE,c=NA`
## Empty data.table (0 rows and 3 cols): only.in.a.ab,x,y
## 
## $`a=b,b=TRUE,c=FALSE`
##    only.in.a.ab     x     y
##           <num> <num> <num>
## 1:            0     0     0
```

