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

data Issue = Issue
    { issueNumber :: T.Text
    , issueId     :: T.Text
    , issueUrl    :: T.Text
    , issueTitle  :: T.Text
    , issueAuthor :: T.Text
    , issueBody   :: T.Text
    } deriving (Show, Read)

data Comment = Comment
    { commentId       :: T.Text
    , commentUrl      :: T.Text
    , commentIssueUrl :: T.Text
    , commentAuthor   :: T.Text
    , commentBody     :: T.Text
    } deriving (Show, Read)

instance J.FromJSON Issue where
  parseJSON = J.withObject "Issue" $ \obj -> do
      issueNumber <- obj J..: "number"
      issueId     <- obj J..: "id"
      issueUrl    <- obj J..: "url"
      issueTitle  <- obj J..: "title"
      issueAuthor <- obj J..: "author"
      issueBody   <- obj J..: "body"
      return (Issue issueNumber issueId issueUrl issueTitle issueAuthor issueBody)

instance J.FromJSON Comment where
  parseJSON = J.withObject "Comment" $ \obj -> do
      commentId        <- obj J..: "id"
      commentUrl       <- obj J..: "url"
      commentIssueUrl  <- obj J..: "issue_url"
      commentAuthor    <- obj J..: "author"
      commentBody      <- obj J..: "body"
      return (Comment commentId commentUrl commentIssueUrl commentAuthor commentBody)

readJsonFile :: (J.FromJSON a) => FilePath -> IO a
readJsonFile filepath = handleFailure . J.eitherDecode <$> BS.readFile filepath
    where handleFailure result = case result of
                                   Left msg -> error msg
                                   Right val -> val

issuesIO :: IO [Issue]
issuesIO = J.parseJSONList <$> readJsonFile $ sourceDir </> "issues.json" :: IO [Issue]

commentsIO :: IO [(T.Text, [Comment])]
commentsIO = do
    contents <- readJsonFile $ sourceDir </> "comments.json"
    return $ map (\c -> (fromJust $ J.parseMaybe (J..: "issue_url") c, fromJust . J.decode . J.encode $ c)) contents

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

makeContext :: J.Object -> J.Object -> J.Value
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
    print $ (J.parseEither (J..: (T.pack "id")) (last issues) :: Either String Int)
    let firstIssue = last issues
        Right firstIssueNumber = J.parseEither (J..: (T.pack "id")) firstIssue :: Either String Int
        firstComments = fromMaybe (HM.empty) (lookup firstIssueNumber comments)

    makeIssuePage (makeContext firstIssue firstComments)
    return ()
    --Right template <- compileTemplate templatesDir templateText
    --md <- TIO.readFile mdFile
    --body <- convertMarkdownToHtml md
    --let doc = renderTemplate template (makeContext body)
    --TIO.writeFile htmlFile (render Nothing doc)
