module AWS.Gen.Printer.PureScript where

import Prelude

import AWS.Gen.Model (ServiceDef)
import AWS.Gen.Printer.PureScript.Requests (addRequestsModule)
import AWS.Gen.Printer.PureScript.Service (addServiceModule)
import AWS.Gen.Printer.PureScript.Types (addTypesModule)
import CST.Simple.Project (defaultProjectSettings, runProject)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll, toLower)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, apathize)
import FS (PartitionPaths(..), mkdirRecursive, partitionPaths, readdirRecursive)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, concat)

projectTemplatePath = "resources/templates/purescript/project" :: FilePath

projectPath :: FilePath -> ServiceDef -> FilePath
projectPath path { name } = concat [path, "purescript-aws-" <> (toLower name)]

filePath :: FilePath -> ServiceDef -> String -> FilePath
filePath path svc@({ name }) fileName = concat [projectPath path svc, "src", fileName <> ".purs"]

project :: FilePath -> ServiceDef -> Aff Unit
project path svc  = do
    paths <- readdirRecursive projectTemplatePath
    PartitionPaths { directoryPaths, filePaths } <- partitionPaths paths
    filePathsAndContent <- traverse (\f -> readTextFile UTF8 f # map (\c -> Tuple f c)) filePaths
    let filePathsAndNewContent = map (\(Tuple f c) -> Tuple f $ updateFileContent svc c) filePathsAndContent
    let newFilePathsAndNewContent = map (\(Tuple f c) -> Tuple (updateFilePath projectPath' f) c) filePathsAndNewContent
    let newDirectoryPaths = map (updateFilePath projectPath') directoryPaths

    _ <- apathize $ traverse (mkdirRecursive) newDirectoryPaths
    _ <- traverse (\(Tuple f c) -> writeTextFile UTF8 f c) newFilePathsAndNewContent

    runProject projectSettings do
      addServiceModule svc
      addTypesModule svc
      addRequestsModule svc

  where
    projectPath' = projectPath path svc
    srcPath = concat [ projectPath', "src" ]

    projectSettings =
      defaultProjectSettings
      { outputDirectory =
        concat [ projectPath', "src" ]
      , rmDirectoryFilesPreRun = true
      }

updateFileContent :: ServiceDef -> String -> String
updateFileContent { name } str = str
    # replaceAll (Pattern "{{MODULE_NAME_LOWER}}") (Replacement $ toLower name)

updateFilePath :: FilePath -> FilePath -> FilePath
updateFilePath path filePath' = replace (Pattern projectTemplatePath) (Replacement path) filePath'
