# Hooman v1.0 

__Human-readable configuration language specification - (cc) 2020 Rodolfo Calzetti__

## Rules

```New line + Indentation```: belonging

```Spaces```: assignement

```Four space```: a tabular key

```[]```: change mode

```<<   >>```: text

```***```: remark

```-->```: include file 

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
            OFFSET 11
            TYPE CHAR
            LENGTH 50
         
TRANSFORM
    *** Script follows
    [Javascript]
        --> sample.js
    
LOAD
    *** Other stuffs
    
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
    

