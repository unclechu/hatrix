{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hatrix.Config
     ( Config (..)
     , readConfig
     , saveConfig
     , parseConfig
     ) where

import           Hatrix.Prelude

import           Data.Default
import           Data.Either.Combinators
import           Data.String (IsString (fromString))
import qualified Data.HashMap.Strict as HM
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS (writeFile)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Attoparsec.Text
import           Text.InterpolatedString.QM

import           System.Directory


-- | "Major" breaks backward compatibility
--   "Minor" increases when something new is added
--           but config still could be used by older version of the application
data ConfigVersion = ConfigVersion Word Word deriving (Show, Eq, Ord)

instance Default ConfigVersion where
  -- | Current version of config API implementation
  def = ConfigVersion 1 0

-- | This "show" implementation doesn't tell anything about it is "ConfigVersion", so keeping it
--   apart from deafult derived "Show" implementation which could be used for debugging purposes.
showConfigVersion ∷ (Monoid α, IsString α) ⇒ ConfigVersion → α
showConfigVersion (ConfigVersion major minor) = [qm| {major}.{minor} |]

instance FromJSON ConfigVersion where
  parseJSON (String str) =
    case parser `parseOnly` str of
         Right a → pure a
         Left  _ → fail [qms| ConfigVersion supposed to be represented as "N.N" (string)
                              where "N" is a decimal unsigned number, got this: "{str}" |]

    where parser = ConfigVersion <$> decimal <* char '.' <*> decimal

  parseJSON x = typeMismatch "ConfigVersion" x

instance ToJSON ConfigVersion where
  toJSON = String ∘ showConfigVersion


data Config
   = Config
   { configVersion ∷ ConfigVersion
   , accounts      ∷ [AccountConfig]
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Default Config where
  def
    = Config
    { configVersion = def
    , accounts      = mempty
    }


data AccountConfig
   = AccountConfig
   { accessToken ∷ Text
   , homeServer  ∷ Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- | Tries to read current @Config@ from a config file
readConfig ∷ IO (Either Text Config)
readConfig = do
  cfgFile ← getDefaultConfigFilePath
  doesConfigExist ← doesFileExist cfgFile

  -- If config file doesn't exist just returning default config
  if doesConfigExist
     then readFile cfgFile <&> parseConfig
     else pure $ Right def


-- | Stores provided @Config@ state to a config file
saveConfig ∷ Config → IO ()
saveConfig cfg = do
  cfgFile ← getDefaultConfigFilePath
  BS.writeFile cfgFile $ AesonPretty.encodePretty' encoderConfig cfg

  where encoderConfig
          = AesonPretty.defConfig
          { AesonPretty.confIndent = AesonPretty.Spaces 2
          , AesonPretty.confTrailingNewline = True
          }


-- | Constructs path to config file based on XDG paths
getDefaultConfigFilePath ∷ IO FilePath
getDefaultConfigFilePath = do
  -- Getting application config directory.
  -- It creates new if it not exists.
  -- E.g. ~/.config/Hatrix
  !dir ← do
    dir ← getXdgDirectory XdgConfig appName
    doesExist ← doesDirectoryExist dir
    dir <$ unless doesExist (createDirectory dir)

  -- Config file would be named as @config.MIDIHasKey@
  pure $ dir </> appName <.> "conf" <.> "json"

  where appName = "Hatrix"


-- | Parses raw JSON string to get @Config@ from it
parseConfig ∷ ByteString → Either Text Config
parseConfig src = do
  -- Raw JSON value to parse just config version
  (jsonConfig ∷ Value) ← eitherDecodeStrict' src & mapLeft fromString

  -- Parsing only config version from raw JSON, if config version is incompatible parsing of whole
  -- structure may fail but we need to show proper fail message about incompatible config version so
  -- that's why we parsing raw JSON first.
  parsedConfVer@(ConfigVersion parsedMajor _) ←
    case jsonConfig of
         (Object (HM.lookup "configVersion" → Just x@(String _))) →
           case fromJSON x of
                Success y → Right y
                Error msg → Left [qms| Parsing config version is failed with message: {msg} |]
         _ → Left "Parsing config version is failed"

  -- If major version of config is bigger than default version from current program it means we have
  -- breaking changes and it could not be read by current version of the application.
  if implMajor < parsedMajor
     then Left [qms| Version of config {showConfigVersion parsedConfVer ∷ Text} is
                     incompatible with currently implemented
                     {showConfigVersion implementedConfVer ∷ Text} |]

     else -- Now we're parsing real @Config@ after we ensured that version of config is compatible.
          -- Parsing @Config@ from already parsed @Value@ instead of just parsing again from source
          -- @ByteString@.
          case fromJSON jsonConfig of
               Success x → Right x
               Error msg → Left $ fromString msg

  where implementedConfVer@(ConfigVersion implMajor _) = def
