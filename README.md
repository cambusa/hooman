# Hooman v1.0
=============

__Human-readable configuration language specification__



_Example for ETL configuration_

```    
HOOMAN
    VERSION 1

EXSTRACT
    DATA
        CODE
            OFFSET 1
            TYPE CHAR
            LENGTH 10
        DESCRIPTION
            OFFSET 1
            TYPE CHAR
            LENGTH 10
    ...        
        
TRANSFORM
    ...
LOAD<br>
    ...
```

_Example for texnical document_

```    
HOOMAN<br>
    VERSION 1<br>

DOCUMENT
    
    [LATEX]
        \int{\frac{Mx+N}{p(x)}dx}=\lambda \times ln(p)+\mu \times arctg\frac{p'}{\sqrt{-\Delta}}
```    
    

