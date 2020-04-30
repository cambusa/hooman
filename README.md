# Hooman v1.0

__Human-readable configuration language specification__

## Rules

New line + Indentation: belonging

Space: assignement

```[]```: change mode

Four space: one tabulation

```<<   >>```: text

```---```: remark

---

_Example for ETL configuration_

```    
HOOMAN
    VERSION 1

EXTRACT
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
    --- Script follows
    
LOAD
    --- Other stuffs
    
```

---

_Example for texnical document_

```    
HOOMAN
    VERSION 1

DOCUMENT
    <<
        Trick to solve integrals of rational functions
    >>
    [LATEX]
        \int{\frac{Mx+N}{p(x)}dx}=\lambda \times ln(p)+\mu \times arctg\frac{p'}{\sqrt{-\Delta}}
```    
    

