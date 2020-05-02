# Hooman v1.0 

__Specification for a new human-writable configuration language - (cc) 2020 Rodolfo Calzetti__


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
hooman
    version 1

extract
    data
        code
            offset 1
            type char
            length 10
        description
            offset 11
            type char
            length 50
         
transform
    *** Script follows
    <<javascript
        <-- sample.js
    javascript>>
    
load
    *** other stuffs
    
```

---

_Example for texnical document_

```    
hooman
    version 1

document
    <<
        Trick to solve integrals of rational functions
    >>
    <<latex
        \int{\frac{Mx+N}{p(x)}dx}=\lambda \times ln(p)+\mu \times arctg\frac{p'}{\sqrt{-\Delta}}
    latex>>
    
```    
    

---

_Example for lists and arrays_

```    
hooman
    version 1

shopping_list
    1 apples
    2 milk
    3 oatmeal
    
*** but also...
shopping_list_2
    1 apples
    + milk
    + oatmeal

worksheet
    rows 
        1
            color white
            cols
                1
                    label Amount
                    type float
                    value 1234.56
        +
            color yellow
            cols
                +
                    label Name 
                    type char
                    value Goofy
            
    
        
```    
    
_Example with syntax check_

```    
hooman
    version 1
    syntax
        data
            name
            address
                city
        worksheet
            rows 
                index
                    color \w+
                    cols
                        index
                            label \w+
                            type (float|char|date)
                            value .
            

data
    name Rodolfo
    address
        city Milan

worksheet
    rows 
        1
            color white
            cols
                1
                    label Amount
                    type float
                    value 1234.56
        +
            color yellow
            cols
                +
                    label Name 
                    type char
                    value Goofy

```    

_Example with external syntax check_

```    
hooman
    version 1
    syntax
        <<hooman
            <--     ..\syntax.hooman
        hooman>>
data
    name Rodolfo
    address
        city Milan

worksheet
    rows 
        1
            color white
            cols
                1
                    label Amount
                    type float
                    value 1234.56
        +
            color yellow
            cols
                +
                    label Name 
                    type char
                    value Goofy

```    

