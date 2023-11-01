------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad ((>=>))
import           Data.Monoid (mappend)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Hakyll
import           Text.Pandoc.Definition
import           Text.Pandoc.Options (HTMLMathMethod(..), writerHTMLMathMethod)
import           Text.Pandoc.Walk (walk, walkM)
import qualified Data.Text as Text
import qualified Network.URI.Encode as URI (encode)
import qualified Text.Pandoc.Filter.Plot as PP
import qualified PanPipe as PanPipe
import qualified PanHandle as PanHandle

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "Lukas's Blog"
  , feedDescription = "Lukas's part of the blogosphere"
  , feedAuthorName = "Lukas Schmidt"
  , feedAuthorEmail = ""
  , feedRoot = "https://lugarun.github.io"
  }

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"


tikzFilter :: Block -> Compiler Block
tikzFilter (CodeBlock (id, "tikzpicture":extraClasses, namevals) contents) =
  (imageBlock . ("data:image/svg+xml;utf8," ++) . URI.encode . filter (/= '\n') . itemBody <$>) $
    makeItem (Text.unpack contents)
     >>= loadAndApplyTemplate (fromFilePath "templates/tikz.tex") (bodyField "body")
     >>= withItemBody (return . pack
                       >=> unixFilterLBS "rubber-pipe" ["--pdf"]
                       >=> unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
                       >=> return . unpack)
  where imageBlock fname = Para [Image (id, "tikzpicture":extraClasses, namevals) [] (Text.pack fname, "")]
tikzFilter x = return x

plotFilter' :: Pandoc -> IO Pandoc
plotFilter' = 
  PP.plotFilter (PP.defaultConfiguration
  { PP.defaultDirectory = "plots"
  }) (Just $ Format "html5")

main :: IO ()
main = hakyllWith config  $ do
    match ("images/*" ) $ do
        route   idRoute
        compile copyFileCompiler

    match ("plots/*" ) $ do
        route   $ customRoute (("posts/" <>) . toFilePath)
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/about.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    let pandocOptions = defaultHakyllWriterOptions {
        writerHTMLMathMethod = MathJax ""
    }
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ (pandocCompilerWithTransformM defaultHakyllReaderOptions pandocOptions ((unsafeCompiler . plotFilter') >=> (walkM tikzFilter) >=> (unsafeCompiler . PanPipe.transformDoc) >=> (return . PanHandle.transform)))
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext