--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601Show)

config :: Configuration
config = defaultConfiguration { destinationDirectory = "docs" }

--------------------------------------------------------------------------------
main :: IO ()
main = getCurrentTime >>= site

site :: UTCTime -> IO ()
site start = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    bodyField "body"

            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html"   indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["feed.xml"] $ do
        route $ idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedCtx =
                    listField "posts" postCtx (return $ take 10 posts) `mappend`
                    constField "isomtime" (iso8601Show start) `mappend`
                    constField "root" root

            makeItem ""
                >>= loadAndApplyTemplate "templates/feed.xml" feedCtx

    match "templates/*" $ compile templateBodyCompiler

root :: String
root = "https://aji.github.io/blog"

postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    dateField "isodate" "%Y-%m-%d" `mappend`
    dateField "year" "%Y" `mappend`
    modificationTimeField "isomtime" "%Y-%m-%dT%H:%M:%SZ" `mappend`
    globalContext

globalContext :: Context String
globalContext =
    constField "root" root `mappend`
    defaultContext
