{-# LANGUAGE DataKinds, 
             KindSignatures,
             ScopedTypeVariables,
             RankNTypes,
             ImplicitParams,
             ExistentialQuantification,
             OverloadedStrings #-}

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
import Control.Exception
import Debug.Trace

data Currency (s :: Symbol) = Currency Double

instance KnownSymbol s => Show (Currency s) where
    show (Currency v) = show v ++ " " ++ symbolVal (SProxy :: SProxy s)

data SProxy (s :: Symbol) = SProxy

type AUD = Currency "aud"

aud :: Double -> AUD
aud = Currency

type BGN = Currency "bgn"

bgn :: Double -> BGN
bgn = Currency

type BRL = Currency "brl"

brl :: Double -> BRL
brl = Currency

type CAD = Currency "cad"

cad :: Double -> CAD
cad = Currency

type CHF = Currency "chf"

chf :: Double -> CHF
chf = Currency

type CNY = Currency "cny"

cny :: Double -> CNY
cny = Currency

type CZK = Currency "czk"

czk :: Double -> CZK
czk = Currency

type DKK = Currency "dkk"

dkk :: Double -> DKK
dkk = Currency

type GBP = Currency "gbp"

gbp :: Double -> GBP
gbp = Currency

type HKD = Currency "hkd"

hkd :: Double -> HKD
hkd = Currency

type HRK = Currency "hrk"

hrk :: Double -> HRK
hrk = Currency

type HUF = Currency "huf"

huf :: Double -> HUF
huf = Currency

type IDR = Currency "idr"

idr :: Double -> IDR
idr = Currency

type ILS = Currency "ils"

ils :: Double -> ILS
ils = Currency

type INR = Currency "inr"

inr :: Double -> INR
inr = Currency

type JPY = Currency "jpy"

jpy :: Double -> JPY
jpy = Currency

type KRW = Currency "krw"

krw :: Double -> KRW
krw = Currency

type MXN = Currency "mxn"

mxn :: Double -> MXN
mxn = Currency

type MYR = Currency "myr"

myr :: Double -> MYR
myr = Currency

type NOK = Currency "nok"

nok :: Double -> NOK
nok = Currency

type NZD = Currency "nzd"

nzd :: Double -> NZD
nzd = Currency

type PHP = Currency "php"

php :: Double -> PHP
php = Currency

type PLN = Currency "pln"

pln :: Double -> PLN
pln = Currency

type RON = Currency "ron"

ron :: Double -> RON
ron = Currency

type RUB = Currency "rub"

rub :: Double -> RUB
rub = Currency

type SEK = Currency "sek"

sek :: Double -> SEK
sek = Currency

type SGD = Currency "sgd"

sgd :: Double -> SGD
sgd = Currency

type THB = Currency "thb"

thb :: Double -> THB
thb = Currency

type TRY = Currency "try"

try :: Double -> TRY
try = Currency

type USD = Currency "usd"

usd :: Double -> USD
usd = Currency

type ZAR = Currency "zar"

zar :: Double -> ZAR
zar = Currency

type EUR = Currency "eur"

eur :: Double -> EUR
eur = Currency

newtype RateDict = RateDict [(String, Double)]
data RateProvider = RateProvider String (IO RateDict)

(<|-|>) :: RateProvider -> RateProvider -> RateProvider
(RateProvider sa a) <|-|> (RateProvider sb b) = RateProvider (sa ++ " | " ++ sb) $
    a `catch` (const b :: SomeException -> IO RateDict)
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

fixerIOProvider :: RateProvider
fixerIOProvider = RateProvider "fixer.io" $ do
      resp <- httpJSON request
      let FixerIOResponse dict = getResponseBody resp
      return $ RateDict resp 
    where request = setRequestQueryString [("base", Just "eur")] "http://api.fixer.io/latest"
    
localProvider :: FilePath -> RateProvider
localProvider path = RateProvider ("file " ++ path) $ do
    text <- readFile path
    let ls = map (\line -> let [a, b] = words line in (a, read b)) $ lines text
    return $ RateDict ls

dictProvider :: String -> [(String, Double)] -> RateProvider
dictProvider s = RateProvider (s ++ " dictionary") . return . RateDict

backupProvider :: RateProvider
backupProvider = dictProvider "backup" [("usd", 1.11), ("gbp", 0.83), ("eur", 1)]

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
              
unsafeCoerceCurrency :: Currency a -> Currency b
unsafeCoerceCurrency (Currency a) = Currency a

newtype Converter = Converter (forall a b. (KnownSymbol a, KnownSymbol b) => Currency a -> Currency b)

getConverter :: RateProvider -> IO Converter
getConverter (RateProvider name' prov) = do
    RateDict dict' <- prov
    return $ let ?dict = dict' 
                 ?name = name' in Converter $ convert
    
getDefaultConverter :: IO Converter
getDefaultConverter = getConverter defaultProvider
