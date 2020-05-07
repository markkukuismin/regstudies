---
output: 
  html_document: 
    keep_md: yes
  md_document:
    preserve_yaml: false
---


Register studies
======

<!-- README.md is generated from README.Rmd. Please edit that file -->

The data utilised in register studies, often called as register-based data or administrative data typically consists of time stamp variables and codes describing events. This package has been designed specifically for handling of such data. The packages is based on tidyverse packages and examples utilise widely tidyverse programming style. The functions are designed with ICD codes in mind but can be used with any kind of string formatted codes and are thus not necessarily limited to healthcare data. Users are allowed to make their own definitions of code classifiers, so the package suits needs of different countries having their own code definitions of disease codes for example.

Authors: [Juho Kopra](https://github.com/jukop), [Jani Miettinen](https://github.com/janikmiet), [Reijo Sund](https://github.com/rsund)


### Installation

```
devtools::install_github("https://github.com/jukop/regstudies/")
```

