# currency-convert

Property    | Value
------------|----------------------
Module      | Data.Currency.Convert
Description | Typesafe currency coversion using exchange rates from various sources.
Copyright   | &copy; Tuomas Laakkonen 2016
License     | BSD3
Maintainer  | pigworts2@gmail.com
Stability   | Experimental
Portability | Non-portable (GHC extensions)

This module allows values of currency to be converted from one currency to another using exchange rates from various sources.

The basic operation for this module is as follows:

First, get a conversion function (with `getDefaultConverter` or `getConverter`):

```haskell
    >>> Converter convert <- getDefaultConverter
```

Then, construct some value of currency using the convenience functions, and convert it using the convert function you just got:

```haskell
    >>> convert (usd 100) :: EUR
    90.0 eur
```

The result type of `convert` defines what currency the value will be converted into.

`Currency` implements `Num`, `Fractional` and `Real` so the usual arithmetic operations are defined, and its value can be extracted with `toRational`.