module BlogGenerator.Directory (buildIndex, convertDirectory) where

import BlogGenerator (confirm)
import BlogGenerator.Convert as Convert
import BlogGenerator.Html as Html
import BlogGenerator.Markup as Markup
import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Data.List (partition)
import Data.Maybe (listToMaybe)
import System.Directory (copyFile, createDirectory, doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))
import System.IO (hPutStrLn, stderr)

-- | Copy files from one directory to another, converting '.txt' files to
-- '.html' files in the process. Records unsuccessful reads and writes to
-- stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir =
  getDirFilesAndContent inputDir >>= \dirContents ->
    createOutputDirectoryOrExit outputDir
      *> copyFiles outputDir (dcFilesToCopy dirContents)
      *> let outputHtmls = txtsToRenderedHtml (dcFilesToProcess dirContents)
          in writeFiles outputDir outputHtmls
               *> putStrLn "Done"

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex input =
  let title = "Index"
      top = Html.h_ 1 (Html.txt_ title)
      summariseDoc structures =
        case listToMaybe structures of
          Nothing -> Html.h_ 2 (Html.txt_ "")
          Just structure ->
            case structure of
              Markup.Heading _ content -> Html.h_ 2 (Html.txt_ content)
              Markup.Paragraph content -> Html.h_ 2 (Html.txt_ $ (head . lines) content)
              _ -> Html.h_ 2 (Html.txt_ "")
      createLinkAndSummary (path, doc) =
        Html.p_ (Html.link_ path (Html.txt_ "")) <> summariseDoc doc
   in Html.html_ title (top <> foldMap createLinkAndSummary input)

-- | Relevant directory content for application
data DirContents
  = DirContents
  { -- | File paths and their content
    dcFilesToProcess :: [(FilePath, String)],
    -- | Other file paths, to be copied directly
    dcFilesToCopy :: [FilePath]
  }

-- | Return directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir =
  listDirectory inputDir >>= \filenames ->
    pure
      (map (inputDir </>) filenames)
      >>= \files ->
        let (txtFiles, otherFiles) = partition ((== ".txt") . takeExtension) files
         in applyIoOnList readFile txtFiles
              >>= filterAndReportFailures
              >>= \txtFilesAndContent ->
                pure $
                  DirContents
                    { dcFilesToProcess = txtFilesAndContent,
                      dcFilesToCopy = otherFiles
                    }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList func vals =
  sequenceA (map process vals)
  where
    process input =
      catch
        ( func input >>= \output ->
            pure (input, Right output)
        )
        (\e -> pure (input, Left $ displayException (e :: SomeException)))

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures vals =
  mconcat (map process vals)
  where
    process (first, second) =
      case second of
        Left e ->
          putStrLn ("IO error: " ++ e)
            *> pure []
        Right inner ->
          pure [(first, inner)]

-- | Transform a list of tuples containing a markup source filepath and string contents to a
-- list of tuples containing the filename of the output HTML file, and the string contents of
-- the output HTML file
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml tuples =
  indexInfo : map (transformBoth . convertFile) markupDocs
  where
    indexFileName = "index.html"
    markupDocs = map toOutputMarkupFile tuples
    indexInfo = (indexFileName, Html.render . buildIndex $ markupDocs)
    transformBoth (path, html) =
      (takeBaseName path <.> "html", Html.render html)

-- | Transform a tuple containing a markup source filepath and string contents to a
-- tuple containing the same markup source filepath, but the string contents has been
-- converted to a `Markup.Document`
toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (path, contents) = (path, Markup.parse contents)

-- | Transform a tuple containing a markup source filepath and a `Markup.document`
-- representation of the contents to a tuple containing the same markup source filepath,
-- but the `Markup.Document` has been converted to a `Html.Html`
convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (path, doc) = (path, Convert.convert (takeBaseName path) doc)

-- | Copy files to directory (assuming list contains filepaths, not filenames), recording
-- errors to stderr
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files =
  void (applyIoOnList copyFromTo files >>= filterAndReportFailures)
  where
    copyFromTo path = copyFile path (outputDir </> takeFileName path)

-- | Write files to directory (assuming list of tuples contain filenames, not filepaths),
-- recording errors to stderr
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir tuples =
  void (applyIoOnList writeFileContents tuples >>= filterAndReportFailures)
  where
    writeFileContents (fileName, contents) = writeFile (outputDir </> fileName) contents

-- | Create output directory or terminate program if directory creation failed
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  createOutputDirectory outputDir >>= \created ->
    when (not created) (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Create output directory.
-- Return whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  doesDirectoryExist dir >>= \dirExists ->
    ( if dirExists
        then
          confirm "Output directory exists. Override?" >>= \override ->
            when override (removeDirectoryRecursive dir)
              *> pure override
        else
          pure True
    )
      >>= \create ->
        when create (createDirectory dir)
          *> pure create
