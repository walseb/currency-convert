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

## Basic Operation

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

## Defining New Currencies

New currencies can be defined very simply:

```haskell
    type NEW = Currency "new"
    new :: Real a => a -> NEW
    new = unsafeCoerceCurrency . fromRational . toRational
```

For some hypothetical currency `new`. These can be used like any other currency.

## Defining New Rate Providers

A method for providing new sources of currency conversion rates is also provided:

Simply create a `RateProvider`. The arguments to `RateProvider` are as follows:

```haskell
	RateProvider (name :: String) (getRates :: IO RateDict)
```

Where `RateDict` is just a list of `(name :: String, rate :: Double)` where `rate` is **the conversion rate between 
the specified currency and the Euro**.

## Using Rate Providers

Different rate providers can be used by providing them as an argument to `getConverter`.
You can also chain these using the combinator `(<|-|>)` which tries one rate provider before trying another if that 
fails (e.g online api followed by a local cache if that fails followed by a hardcoded dictionary). 
