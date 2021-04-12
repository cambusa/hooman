# Hooman v1.3 <img align="right" height="150" src="https://raw.githubusercontent.com/cambusa/hooman/master/hooman04.png">


__Specification for a new human-writable metalanguage - © 2021 Rodolfo Calzetti__

It's an alternative to XML, JSON and YAML if the document is to be written by a human.

Notable features:
1. Indentation is significant (like Python)
2. Files and contents can be included
3. You can define a syntax structure and rules on values
4. It doesn't require any escaping
5. Structure and values can be overwritten (except for syntactic ones)

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

```*```: wildcard in the syntactic structure (for numerical/associative index)

```...```: recursive branch in the syntactic structure (for recursion) 

```==>```: entail in the syntactic rules 

```hooman/syntax/structure/.../variable defaultValue```: default value in the syntactic structure 

---

## Wiki

Read the [wiki](https://github.com/cambusa/hooman/wiki) to learn more.

---

## Example

*main.fud*

```    
hooman
    version 1
    *** includo la sintassi del linguaggio oggetto
    <-- syntax.fud
    
fields
    name
        value Johann Sebastian Bach
        
    amount
        type number
        value 23.45
        
    tradedate
        type date
        value 2021-03-31
        
    memo
        value 
        fields
            biography
                value something

text
    <<
    >>
```    

*syntax.fud*

```    
syntax
    structure
        fields ...
            id *
                type string
                value !
                length
                fields
        text

    rules
        1
            ==> 
            type (string|number|date)
        +
            type number
            ==>
            value \d+(\.\d+)?
        +
            type date
            ==>
            value {(\d{4})-(\d{2})-(\d{2})}
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

