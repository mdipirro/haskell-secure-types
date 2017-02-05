This repository contains the final project for the [**CNS**](http://www.math.unipd.it/~conti/teaching/CNS1617/index.html) (**C**omputer and **N**etwork **S**ecurity) course at the [University of Padua](http://www.unipd.it/). 

The lecture was held in the first semester of the Academic Year 2016/2017 as part of the "Realiable Systems" area of the [Master Degree] (http://informatica.math.unipd.it/laureamagistrale/index.html) in Computer Science.

In this project I define some Haskell monads for ensuring some properties of the final program. 
The main practical goal is to create some "secure" types which may be used by programmers and which may improve the quality, the safety and the security of the final software. 
On the other hand, the abstract goal is to explore how the functional programming paradigm could help programmers to write more safe and secure code. 
Three main golas are chased, as follows:

1. By using higher-order functions, I try to validate different inputs _before_ their use. If the input value is boxed into another type, the programmers have to provide a **validation function** in order to "extract" it. With functional programming this can be done in a very easy and pretty way. 
2. By using Haskell type system, I try to ensure two compile time properties. The first is **non-interference** and the second is a constrainted **computation on untainted data**. I also supply a declassification combinator for relaxing the notion of non-interference. 

[![Build Status](https://travis-ci.com/mdipirro/haskell-secure-types.svg?token=7LSQJy3DGkLbRMBT5MDM&branch=master)](https://travis-ci.com/mdipirro/haskell-secure-types)
