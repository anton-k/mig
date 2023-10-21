{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Derive standard HTTP-classes
module Mig.Core.Derive (
  deriveParam,
  deriveNewtypeParam,
  deriveBody,
  deriveParamBody,
  deriveNewtypeBody,
  deriveNewtypeParamBody,
  deriveHttp,
  deriveNewtypeHttp,
  deriveNewtypeForm,
  deriveForm,
  mapDerive,

  -- * useful with derive-topdown library
  paramClasses,
  bodyClasses,
  paramBodyClasses,
  httpClasses,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Web.FormUrlEncoded (FromForm, ToForm)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

paramClasses :: [Name]
paramClasses = [''Show, ''Eq, ''Ord, ''Generic, ''ToJSON, ''FromJSON, ''ToParamSchema, ''ToHttpApiData, ''FromHttpApiData]

bodyClasses :: [Name]
bodyClasses = [''Show, ''Eq, ''Ord, ''Generic, ''ToJSON, ''FromJSON, ''ToSchema, ''ToHttpApiData, ''FromHttpApiData]

paramBodyClasses :: [Name]
paramBodyClasses = [''Show, ''Eq, ''Ord, ''Generic, ''ToJSON, ''FromJSON, ''ToParamSchema, ''ToSchema, ''ToHttpApiData, ''FromHttpApiData]

httpClasses :: [Name]
httpClasses = [''Show, ''Eq, ''Ord, ''Generic, ''ToJSON, ''FromJSON, ''ToParamSchema, ''ToSchema, ''ToHttpApiData, ''FromHttpApiData]

mapDerive :: (Name -> Q [Dec]) -> [Name] -> Q [Dec]
mapDerive f types = fmap concat (mapM f types)

-- | Derives standard WEB-classes for a newtype suitable for request parameter
deriveNewtypeParam :: Name -> Q [Dec]
deriveNewtypeParam typeName = do
  let typeCon = conT typeName
  [d|
    deriving newtype instance Show $(typeCon)

    deriving newtype instance Eq $(typeCon)

    deriving newtype instance Ord $(typeCon)

    deriving newtype instance ToJSON $(typeCon)

    deriving newtype instance FromJSON $(typeCon)

    deriving newtype instance ToParamSchema $(typeCon)

    deriving newtype instance ToHttpApiData $(typeCon)

    deriving newtype instance FromHttpApiData $(typeCon)
    |]

-- | Derives standard WEB-classes for a type suitable for request parameter
deriveParam :: Name -> Q [Dec]
deriveParam typeName = do
  let typeCon = conT typeName
  [d|
    deriving stock instance Show $(typeCon)

    deriving stock instance Eq $(typeCon)

    deriving stock instance Ord $(typeCon)

    deriving instance Generic $(typeCon)

    deriving anyclass instance ToJSON $(typeCon)

    deriving anyclass instance FromJSON $(typeCon)

    deriving anyclass instance ToParamSchema $(typeCon)

    deriving anyclass instance ToHttpApiData $(typeCon)

    deriving anyclass instance FromHttpApiData $(typeCon)
    |]

-- | Derives standard WEB-classes for a newtype suitable for request body or response
deriveNewtypeBody :: Name -> Q [Dec]
deriveNewtypeBody typeName = do
  let typeCon = conT typeName
  [d|
    deriving newtype instance Show $(typeCon)

    deriving newtype instance Eq $(typeCon)

    deriving newtype instance Ord $(typeCon)

    deriving newtype instance ToJSON $(typeCon)

    deriving newtype instance FromJSON $(typeCon)

    deriving newtype instance ToSchema $(typeCon)
    |]

-- | Derives standard WEB-classes for a type suitable for request body or response
deriveBody :: Name -> Q [Dec]
deriveBody typeName = do
  let typeCon = conT typeName
  [d|
    deriving instance Show $(typeCon)

    deriving instance Eq $(typeCon)

    deriving instance Ord $(typeCon)

    deriving instance Generic $(typeCon)

    deriving instance ToJSON $(typeCon)

    deriving instance FromJSON $(typeCon)

    deriving instance ToSchema $(typeCon)
    |]

-- | Derives standard WEB-classes for a newtype suitable for request form
deriveNewtypeForm :: Name -> Q [Dec]
deriveNewtypeForm typeName = do
  let typeCon = conT typeName
  [d|
    deriving newtype instance Show $(typeCon)

    deriving newtype instance Eq $(typeCon)

    deriving newtype instance Ord $(typeCon)

    deriving newtype instance ToForm $(typeCon)

    deriving newtype instance FromForm $(typeCon)

    deriving newtype instance ToSchema $(typeCon)
    |]

-- | Derives standard WEB-classes for a type suitable for request form
deriveForm :: Name -> Q [Dec]
deriveForm typeName = do
  let typeCon = conT typeName
  [d|
    deriving instance Show $(typeCon)

    deriving instance Eq $(typeCon)

    deriving instance Ord $(typeCon)

    deriving instance Generic $(typeCon)

    deriving instance FromForm $(typeCon)

    deriving instance ToForm $(typeCon)

    deriving instance ToSchema $(typeCon)
    |]

deriveNewtypeHttp :: Name -> Q [Dec]
deriveNewtypeHttp = deriveNewtypeParamBody

deriveHttp :: Name -> Q [Dec]
deriveHttp = deriveParamBody

-- | Derives standard WEB-classes for a newtype which is both body and param
deriveNewtypeParamBody :: Name -> Q [Dec]
deriveNewtypeParamBody typeName = do
  let typeCon = conT typeName
  [d|
    deriving newtype instance Show $(typeCon)

    deriving newtype instance Eq $(typeCon)

    deriving newtype instance Ord $(typeCon)

    deriving newtype instance ToJSON $(typeCon)

    deriving newtype instance FromJSON $(typeCon)

    deriving newtype instance ToSchema $(typeCon)

    deriving newtype instance ToParamSchema $(typeCon)

    deriving newtype instance ToHttpApiData $(typeCon)

    deriving newtype instance FromHttpApiData $(typeCon)
    |]

-- | Derives standard WEB-classes for a type which is both body and param
deriveParamBody :: Name -> Q [Dec]
deriveParamBody typeName = do
  let typeCon = conT typeName
  [d|
    deriving stock instance Show $(typeCon)

    deriving stock instance Eq $(typeCon)

    deriving stock instance Ord $(typeCon)

    deriving stock instance Generic $(typeCon)

    deriving anyclass instance ToJSON $(typeCon)

    deriving anyclass instance FromJSON $(typeCon)

    deriving anyclass instance ToSchema $(typeCon)

    deriving anyclass instance ToParamSchema $(typeCon)

    deriving anyclass instance ToHttpApiData $(typeCon)

    deriving anyclass instance FromHttpApiData $(typeCon)
    |]
