{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Templates as PT
import Text.DocLayout (render) -- Used to render Doc type into Text type.
--import System.IO
import System.Directory (createDirectory, getDirectoryContents, doesDirectoryExist, copyFile)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Control.Arrow ((***))
import qualified Data.ByteString.Lazy as BS -- Using ByteStrings for efficiency since data file is large.
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (parseMaybe, parseEither, emptyObject)
import Data.HashMap.Strict as HM (empty, toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (groupBy)
import Data.Function (on)

data Issue = Issue
    { issueNumber :: Int --T.Text
    , issueId     :: Int --T.Text
    , issueUrl    :: T.Text
    , issueTitle  :: T.Text
    , issueAuthor :: T.Text
    , issueBody   :: Maybe T.Text
    } deriving (Show, Read)

data Comment = Comment
    { commentId       :: Int--T.Text
    , commentUrl      :: T.Text
    , commentIssueUrl :: T.Text
    , commentAuthor   :: T.Text
    , commentBody     :: Maybe T.Text
    } deriving (Show, Read)

instance J.FromJSON Issue where
  parseJSON = J.withObject "Issue" $ \obj -> do
      issueNumber <- obj J..: "number"
      issueId     <- obj J..: "id"
      issueUrl    <- obj J..: "url"
      issueTitle  <- obj J..: "title"
      userObj     <- obj J..: "user"
      issueAuthor <- userObj J..: "login"
      issueBody   <- obj J..: "body"
      return (Issue issueNumber issueId issueUrl issueTitle issueAuthor issueBody)

instance J.ToJSON Issue where
  toJSON (Issue issueNumber _ issueUrl issueTitle issueAuthor issueBody) =
      J.object [ "number" J..= issueNumber
               , "url"    J..= issueUrl
               , "title"  J..= issueTitle
               , "author" J..= issueAuthor
               , "body"   J..= issueBody
               ]

instance J.FromJSON Comment where
  parseJSON = J.withObject "Comment" $ \obj -> do
      commentId        <- obj J..: "id"
      commentUrl       <- obj J..: "url"
      commentIssueUrl  <- obj J..: "issue_url"
      userObj          <- obj J..: "user"
      commentAuthor    <- userObj J..: "login"
      commentBody      <- obj J..: "body"
      return (Comment commentId commentUrl commentIssueUrl commentAuthor commentBody)

instance J.ToJSON Comment where
  toJSON (Comment _ commentUrl commentIssueUrl commentAuthor commentBody) =
      J.object [ "url"        J..= commentUrl
               , "issue_url"  J..= commentIssueUrl
               , "author"     J..= commentAuthor
               , "body"       J..= commentBody
               ]

readJsonFile :: (J.FromJSON a) => FilePath -> IO a
readJsonFile filepath = handleFailure . J.eitherDecode <$> BS.readFile filepath
    where handleFailure result = case result of
                                   Left msg -> error msg
                                   Right val -> val

issuesIO :: IO [Issue]
issuesIO = readJsonFile $ sourceDir </> "issues.json" :: IO [Issue]

commentsIO :: IO [(T.Text, [Comment])]
commentsIO = do
    contents <- readJsonFile $ sourceDir </> "comments.json" :: IO [Comment]
    return . map (\cs -> (commentIssueUrl $ head cs, cs)) . groupBy ((==) `on` commentIssueUrl) $ contents

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectory dst
    dirContents <- getDirectoryContents src
    forM_ (filter (`notElem` [".", ".."]) dirContents) $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
           then copyDir srcPath dstPath
           else copyFile srcPath dstPath

assetsDir :: FilePath
assetsDir = "./assets/"

sourceDir :: FilePath
sourceDir = "./_data/"

templatesDir :: FilePath
templatesDir = "./_templates/"

targetDir :: FilePath
targetDir = "./_site/"

loadTemplate :: FilePath -> IO (PT.Template T.Text)
loadTemplate filepath = do
    templateIO <- P.runIO $ PT.getTemplate filepath
    templateText <- P.handleError templateIO
    Right template <- PT.compileTemplate filepath templateText
    return template

issueTemplateIO :: IO (PT.Template T.Text)
issueTemplateIO = loadTemplate $ templatesDir </> "issue.html"

issuesIndexTemplate :: IO (PT.Template T.Text)
issuesIndexTemplate = loadTemplate $ templatesDir </> "issuesIndex.html"

mdToHtml :: T.Text -> IO (T.Text)
mdToHtml md = do
    result <- P.runIO $ do
        doc <- P.readMarkdown P.def md
        P.writeHtml5String P.def doc
    P.handleError result

makeContext :: Issue -> [Comment] -> J.Value
makeContext issue comments = J.object [ T.pack "issue" J..= issue
                                      , T.pack "comments" J..= comments ]

copyAssets :: IO ()
copyAssets = copyDir assetsDir (targetDir </> "assets/")

makeIssuePage :: J.Value -> IO (T.Text)
makeIssuePage context = do
    template <- issueTemplateIO
    let page = render Nothing $ PT.renderTemplate template context
    TIO.putStrLn page
    return page
    --Right template <- compileTemplate templatesDir templateText
    --md <- TIO.readFile mdFile
    --body <- convertMarkdownToHtml md
    --let doc = renderTemplate template (makeContext body)
    --TIO.writeFile htmlFile (render Nothing doc)

main :: IO ()
main = do
    issues <- issuesIO
    comments <- commentsIO
    let firstIssue = last issues
        firstIssueUrl = issueUrl firstIssue :: T.Text
        firstComments = fromMaybe [] (lookup firstIssueUrl comments)

    makeIssuePage (makeContext firstIssue firstComments)
    return ()
    --Right template <- compileTemplate templatesDir templateText
    --md <- TIO.readFile mdFile
    --body <- convertMarkdownToHtml md
    --let doc = renderTemplate template (makeContext body)
    --TIO.writeFile htmlFile (render Nothing doc)