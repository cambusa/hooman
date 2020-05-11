# Hooman v1.0 

__Specification for a new human-writable configuration language - (CC0) 2020 Rodolfo Calzetti__


## Rules

```Four leading spaces```: a tabular key

```Spaces and tabs after a variable```: simple assignement

```New line + Indentation```: complex assignement

```<<   >>```: text

```<<TAG  ...  TAG>>  ```: open/close mode

```***```: remark

```<--```: file include 

```@```: reset branch

```+```: increment by one (of a sibling key)

```!```: mandatory in the syntactic structure 

```==>```: entail in the syntactic rules 

---

## Todo

- Reader for file Hooman (the current vb.net project is to be completed)
- Editor for file Hooman (with intellisense deduced from the syntactic rules)

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

_Example with inclusion and branch redefinition_

```    
hooman
    version 1

document
    <<hooman
        <-- include.txt
    hooman>>

document @
document
    new_structure

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
    
_Example with syntax checking_

```    
hooman
    version 1
    syntax
        structure
            data !
                name !
                address
                    city

            worksheet
                rows 
                    *** starting index
                    1
                        color
                        cols
                            0
                                label
                                type
                                value
        rules
            1
                ==> 
                type (char|float|date)
            +
                type float
                ==>
                value \d+(\.\d+)?
            +
                type date
                ==>
                value \d{8}

data
    name Rodolfo
    address
        city Milan

worksheet
    rows 
        1
            color white
            cols
                0
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

_Example with external syntax checking_

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
                0
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

