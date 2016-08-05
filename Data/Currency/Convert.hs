{-# LANGUAGE DataKinds, 
             KindSignatures,
             ScopedTypeVariables,
             RankNTypes,
             ImplicitParams,
             ExistentialQuantification,
             OverloadedStrings #-}

{-|
    Module      : Data.Currency.Convert
    Description : Typesafe currency coversion using exchange rates from various sources.
    Copyright   : (c) Tuomas Laakkonen 2016
    License     : BSD3
    Maintainer  : pigworts2@gmail.com
    Stability   : Experimental
    Portability : Non-portable (GHC extensions)

    This module allows values of currency to be converted from one currency to another using exchange rates from various sources.

    The basic operation for this module is as follows:

    First, get a conversion function (with @'getDefaultConverter'@ or @'getConverter'@):

    >>> Converter convert <- getDefaultConverter

    Then, construct some value of currency using the convenience functions, and convert it using the convert function you just got:

    >>> convert (usd 100) :: EUR
    90.0 eur

    The result type of @convert@ defines what currency the value will be converted into.
-}
module Data.Currency.Convert (
  Converter(..),
  getConverter,
  getDefaultConverter,
  unsafeCoerceCurrency,
  Currency(),
  RateProvider(..),
  RateDict(..),
  defaultProvider,
  backupProvider,
  dictProvider,
  localProvider,
  fixerIOProvider,
  (<|-|>),
  aud,
  bgn,
  brl,
  cad,
  chf,
  cny,
  czk,
  dkk,
  gbp,
  hkd,
  hrk,
  huf,
  idr,
  ils,
  inr,
  jpy,
  krw,
  mxn,
  myr,
  nok,
  nzd,
  php,
  pln,
  ron,
  rub,
  sek,
  sgd,
  thb,
  try,
  usd,
  zar,
  eur,
  AUD,
  BGN,
  BRL,
  CAD,
  CHF,
  CNY,
  CZK,
  DKK,
  GBP,
  HKD,
  HRK,
  HUF,
  IDR,
  ILS,
  INR,
  JPY,
  KRW,
  MXN,
  MYR,
  NOK,
  NZD,
  PHP,
  PLN,
  RON,
  RUB,
  SEK,
  SGD,
  THB,
  TRY,
  USD,
  ZAR,
  EUR
) where

import GHC.TypeLits
import Network.HTTP.Simple
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import Data.Aeson
import Control.Applicative
import qualified Control.Exception as E
import Debug.Trace

{-|
    @'Currency'@ is a wrapper around @Double@ that has a phantom symbol type, 
    allowing different currencies to be distinguished using type level literals.

    To extract the value from a @'Currency'@ value, use @toRational@ from @Real@.
-}
data Currency (s :: Symbol) = Currency Double
    deriving (Eq, Ord)

instance KnownSymbol s => Show (Currency s) where
    show (Currency v) = show v ++ " " ++ symbolVal (SProxy :: SProxy s)

instance Num (Currency a) where
    Currency a + Currency b = Currency $ a + b
    Currency a * Currency b = Currency $ a * b
    negate (Currency a) = Currency $ negate a
    signum (Currency a) = Currency $ signum a
    fromInteger i = Currency $ fromInteger i
    abs (Currency a) = Currency $ abs a

instance Fractional (Currency a) where
    Currency a / Currency b = Currency $ a / b
    fromRational r = Currency $ fromRational r

instance Real (Currency a) where
    toRational (Currency a) = toRational a

instance RealFrac (Currency a) where
    properFraction (Currency a) = let (a', b') = properFraction a in (a', Currency b')

data SProxy (s :: Symbol) = SProxy

-- | The Australian Dollar
type AUD = Currency "aud"

-- | The Australian Dollar
aud :: Double -> AUD
aud = Currency

-- | The Bulgarian Lev
type BGN = Currency "bgn"

-- | The Bulgarian Lev
bgn :: Double -> BGN
bgn = Currency

-- | The Brazillian Real
type BRL = Currency "brl"

-- | The Brazillian Real
brl :: Double -> BRL
brl = Currency

-- | The Canadian Dollar
type CAD = Currency "cad"

-- | The Canadian Dollar
cad :: Double -> CAD
cad = Currency

-- | The Swiss Franc
type CHF = Currency "chf"

-- | The Swiss Franc
chf :: Double -> CHF
chf = Currency

-- | The Yuan
type CNY = Currency "cny"

-- | The Yuan
cny :: Double -> CNY
cny = Currency

-- | The Czech Karuna
type CZK = Currency "czk"

-- | The Czech Karuna
czk :: Double -> CZK
czk = Currency

-- | The Danish Krone
type DKK = Currency "dkk"

-- | The Danish Krone
dkk :: Double -> DKK
dkk = Currency

-- | The Pound Sterling
type GBP = Currency "gbp"

-- | The Pound Sterling
gbp :: Double -> GBP
gbp = Currency

-- | The Hong Kong Dollar
type HKD = Currency "hkd"

-- | The Hong Kong Dollar
hkd :: Double -> HKD
hkd = Currency

-- | The Croatian Kuna
type HRK = Currency "hrk"

-- | The Croatian Kuna
hrk :: Double -> HRK
hrk = Currency

-- | The Forint
type HUF = Currency "huf"

-- | The Forint
huf :: Double -> HUF
huf = Currency

-- | The Rupiah
type IDR = Currency "idr"

-- | The Rupiah
idr :: Double -> IDR
idr = Currency

-- | The New Israeli Sheqel
type ILS = Currency "ils"

-- | The New Israeli Sheqel
ils :: Double -> ILS
ils = Currency

-- | The Indian Rupee
type INR = Currency "inr"

-- | The Indian Rupee
inr :: Double -> INR
inr = Currency

-- | The Yen
type JPY = Currency "jpy"

-- | The Yen
jpy :: Double -> JPY
jpy = Currency

-- | The Won
type KRW = Currency "krw"

-- | The Won
krw :: Double -> KRW
krw = Currency

-- | The Mexican Peso
type MXN = Currency "mxn"

-- | The Mexican Peso
mxn :: Double -> MXN
mxn = Currency

-- | The Malaysian Ringgit
type MYR = Currency "myr"

-- | The Malaysian Ringgit
myr :: Double -> MYR
myr = Currency

-- | The Norwegian Krone
type NOK = Currency "nok"

-- | The Norwegian Krone
nok :: Double -> NOK
nok = Currency

-- | The New Zealand Dollar
type NZD = Currency "nzd"

-- | The New Zealand Dollar
nzd :: Double -> NZD
nzd = Currency

-- | The Phillipine Peso
type PHP = Currency "php"

-- | The Phillipine Peso
php :: Double -> PHP
php = Currency

-- | The Zloty
type PLN = Currency "pln"

-- | The Zloty
pln :: Double -> PLN
pln = Currency

-- | The Romanian Leu
type RON = Currency "ron"

-- | The Romanian Leu
ron :: Double -> RON
ron = Currency

-- | The Russian Ruble
type RUB = Currency "rub"

-- | The Russian Ruble
rub :: Double -> RUB
rub = Currency

-- | The Swedish Krona
type SEK = Currency "sek"

-- | The Swedish Krona
sek :: Double -> SEK
sek = Currency

-- | The Singapore Dollar
type SGD = Currency "sgd"

-- | The Singapore Dollar
sgd :: Double -> SGD
sgd = Currency

-- | The Thai Baht
type THB = Currency "thb"

-- | The Thai Baht
thb :: Double -> THB
thb = Currency

-- | The Turkish Lira
type TRY = Currency "try"

-- | The Turkish Lira
try :: Double -> TRY
try = Currency

-- | The United States of America Dollar
type USD = Currency "usd"

-- | The United States of America Dollar
usd :: Double -> USD
usd = Currency

-- | The Rand
type ZAR = Currency "zar"

-- | The Rand
zar :: Double -> ZAR
zar = Currency

-- | The Euro
type EUR = Currency "eur"

-- | The Euro
eur :: Double -> EUR
eur = Currency


{-|
    @'RateProvider'@ is used by @'getConverter'@ to provide a conversion rate dictionary to the resulting conversion function.
    @'RateProvider'@ has two fields, the name (@:: String@), and the action (@:: IO RateDict@). 
    The name is used in error reporting, and the action is used to provide conversion rates when needed.
    
    There are several prebuilt @'RateProvider'@s - @'fixerIOProvider'@, @'backupProvider'@, @'localProvider'@, @'dictProvider'@ and @'defaultProvider'@.
-}
data RateProvider = RateProvider String (IO RateDict)

{-|
    @'RateDict'@ is the type provided by a @'RateProvider'@. 
    It is a mapping from currency codes to currency-to-euro conversion rates.
-}
newtype RateDict = RateDict [(String, Double)]

{-|
    @'(\<|-|\>)'@ combines two @'RateProvider'@s to create a third.
    This provider tries to return the first providers result, but if an exception is raised, it returns the second providers result.
-}
(<|-|>) :: RateProvider -> RateProvider -> RateProvider
(RateProvider sa a) <|-|> (RateProvider sb b) = RateProvider (sa ++ " | " ++ sb) $
    a `E.catch` (const b :: E.SomeException -> IO RateDict)
infixr <|-|>

newtype FixerIOResponse = FixerIOResponse [(String, Double)]

instance FromJSON FixerIOResponse where
    parseJSON = withObject "response" $ \r -> do
        o <- r .: "rates"
        let os = HS.toList o
        os' <- flip mapM os $ \(a, b) -> do
            b' <- parseJSON b
            return (T.unpack $ T.toLower a, b')
        return $ FixerIOResponse $ os' ++ [("eur", 1)]

{-|
    This @'RateProvider'@ uses the <fixer.io> API to return exchange rates updated daily by the European Central Bank.
-} 
fixerIOProvider :: RateProvider
fixerIOProvider = RateProvider "fixer.io" $ do
      resp <- httpJSON request
      let FixerIOResponse dict = getResponseBody resp
      return $ RateDict dict
    where request = setRequestQueryString [("base", Just "eur")] "http://api.fixer.io/latest"
    
{-|
    This @'RateProvider'@ takes a file path and returns a dictionary of rates from the file.
    The file must be in the form 
    
    @
     \<name1\> \<rate1\>
     \<name2\> \<rate2\>
     ...
    @
-}
localProvider :: FilePath -> RateProvider
localProvider path = RateProvider ("file " ++ path) $ do
    text <- readFile path
    let ls = map (\line -> let [a, b] = words line in (a, read b)) $ lines text
    return $ RateDict ls

{-|
    This @'RateProvider'@ allows a dictionary to be provided.
-}
dictProvider :: String -> [(String, Double)] -> RateProvider
dictProvider s = RateProvider (s ++ " dictionary") . return . RateDict

{-|
    This @'RateProvider'@ has a hardcoded dictionary.
-}
backupProvider :: RateProvider
backupProvider = dictProvider "backup" [("usd", 1.11), ("gbp", 0.83), ("eur", 1)]


{-|
    This @'RateProvider'@ is used by @'getDefaultConverter'@ it first tries <fixer.io> and then resorts to the backup dictionary.
-}
defaultProvider :: RateProvider
defaultProvider = fixerIOProvider <|-|> backupProvider

convert :: forall a b. (KnownSymbol a, KnownSymbol b, ?dict :: [(String, Double)], ?name :: String) 
           => Currency a -> Currency b
convert (Currency a) = let as = symbolVal (SProxy :: SProxy a) 
                           bs = symbolVal (SProxy :: SProxy b) in
                       case lookup as ?dict of
                           Just aRate -> let eurVal = a / aRate in
                               case lookup bs ?dict of
                                   Just bRate -> Currency $ eurVal * bRate
                                   Nothing -> 
                                       error $ "Unknown currency `" ++ bs ++ "` for provider `" ++ ?name ++ "`"
                           Nothing -> error $ "Unknown currency `" ++ as ++ "` for provider `" ++ ?name ++ "`"

{-|
    @'unsafeCoerceCurrency'@ can be used to convert between currencies directly without using exchange rates.
    Only use this if you know the resulting currency is supported by your @'RateProvider'@
-}   
unsafeCoerceCurrency :: Currency a -> Currency b
unsafeCoerceCurrency (Currency a) = Currency a

{-|
    @'Converter'@ is a newtype wrapper around the type of conversion functions to avoid @ImpredicativeTypes@ in @'getConverter'@
-}
newtype Converter = Converter (forall a b. (KnownSymbol a, KnownSymbol b) => Currency a -> Currency b)


{-|
    @'getConverter'@ takes a rate provider and returns a @'Converter'@ in the @IO@ monad.
    It is used like this:

    >>> Converter convert <- getConverter provider

    @convert@ can then be used to convert between currencies:

    >>> convert (usd 100) :: GBP
    75.0 gbp
-}
getConverter :: RateProvider -> IO Converter
getConverter (RateProvider name' prov) = do
    RateDict dict' <- prov
    return $ let ?dict = dict' 
                 ?name = name' in Converter $ convert

{-|
    @'getDefaultConverter'@ is like @'getConverter'@ except no provider needs to be specified (@'defaultProvider'@ is used).
-}
getDefaultConverter :: IO Converter
getDefaultConverter = getConverter defaultProvider
