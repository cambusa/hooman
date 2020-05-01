# Hooman v1.0 

__Human-readable configuration language specification - (cc) 2020 Rodolfo Calzetti__


## Rules

```New line + Indentation```: belonging

```Four leading spaces```: a tabular key

```Spaces and tabs after a variable```: assignement

```+```: increment by one (of a sibling key)

```<<   >>```: text

```<<TAG  ...  TAG>>  ```: open/close mode

```***```: remark

```<--```: file include 

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
    <<Javascript
        <-- sample.js
    Javascript>>
    
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
    <<LATEX
        \int{\frac{Mx+N}{p(x)}dx}=\lambda \times ln(p)+\mu \times arctg\frac{p'}{\sqrt{-\Delta}}
    LATEX>>
    
```    
    

---

_Example for lists and arrays_

```    
HOOMAN
    VERSION 1

SHOPPING_LIST
    1 apples
    2 milk
    3 oatmeal
    
*** But also...
SHOPPING_LIST_2
    1 apples
    + milk
    + oatmeal

WORKSHEET
    ROWS 
        1
            COLOR WHITE
            COLS
                1
                    LABEL Amount
                    TYPE FLOAT
                    VALUE 1234.56
        +
            COLOR YELLOW
            COLS
                +
                    LABEL Name 
                    TYPE CHAR
                    VALUE Goofy
            
    
        
```    
    

