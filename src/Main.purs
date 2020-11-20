module Main where

import Prelude

import AWS.Gen.Metadata (MetadataElement, metadataFileRegex)
import AWS.Gen.MetadataReader (readService)
import AWS.Gen.Model (ServiceDef)
import AWS.Gen.Printer.PureScript (project)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (test)
import Data.Traversable (find, traverse_)
import Eff (liftExcept)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Generic (decodeJSON)
import Foreign.Object (values)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)
import Node.Path as Path

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath
apisPath = "./aws-sdk-js/apis/" :: FilePath

clientsPath = "aws-sdk-purs" :: FilePath

createClientProject :: FilePath -> ServiceDef -> Aff Unit
createClientProject path svc = project path svc

runProject :: Array FilePath -> FilePath -> MetadataElement -> Aff Unit
runProject apiFileNames clientsProject metadataElement = do
  log $ "Creating project - " <> name
  fileNameRegex <- mkApiFileNameRegex -- can be simplified later on not to use regex
  fileName <- findFileName fileNameRegex
  awsService <- readAwsService $ Path.concat [ apisPath, fileName ]
  serviceDef <- readServiceDef awsService
  createClientProject clientsPath serviceDef
--  createClientFiles clientsPath serviceDef
  where
    name = metadataElement.name

    mkApiFileNameRegex = case metadataFileRegex metadataElement of
      Right r -> pure r
      Left l -> throwError (error "Unable to form regex")

    findFileName pattern = case find (test pattern) apiFileNames of
      Just r -> pure r
      Nothing -> throwError (error $ "Unable to find file of pattern: " <> show pattern)

    readAwsService filePath = do
      jsonString <- readTextFile UTF8 filePath
      decodeJSON jsonString # liftExcept # liftEffect

    readServiceDef awsService =
      let s = readService metadataElement awsService
      in case s of
        Right r -> pure r
        Left l -> throwError $ error $ "Invalid metadata - " <> show l

main :: Effect Unit
main = launchAff_ do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEffect
  let metadataElements = values metadata
  apiFileNames <- readdir apisPath

  traverse_ (runProject apiFileNames clientsPath) metadataElements
  liftEffect $ log "Done!"
