# Hooman v1.0 <img align="right" height="150" src="https://raw.githubusercontent.com/cambusa/hooman/master/hooman04.png">


__Specification for a new human-writable declarative language - © 2020 Rodolfo Calzetti__

It's an alternative to XML, JSON and YAML if the document is to be written by a human.

Notable features:
1. Indentation is significant (like Python)
2. Files can be included
3. You can define a syntax structure and rules on values
4. It doesn't require any escaping


## The thirteen rules

```Spaces and tabs after a variable```: simple assignement

```New line + Indentation```: complex assignement

```<<   >>```: multiline text

```<<TAG  ...  TAG>>  ```: open/close mode

```***```: remark

```<--```: file include 

```@```: reset branch

```+```: increment by one (of a sibling key)

```!```: mandatory in the syntactic structure 

```*```: jollyname in the syntactic structure (for numerical/associative index)

```...```: iterable branch in the syntactic structure (for recursion) 

```==>```: entail in the syntactic rules 

```hooman/sintax/structure/.../variable defaultValue```: default value for syntactic rules 

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
                subdata ...
                    link
                        subdata

            worksheet
                rows 
                    rowindex *
                        color
                        cols
                            colindex *
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

---

## Why the name Hooman and the extension .fud??

```
the_cat_replies
    <<
        Look, i'm a hooman, 
        get off da counter, 
        don't eat mai fud, 
        dur dur dur dur
    >>
```

---

## License

```
Creative Commons Zero v1.0 Universal
```

