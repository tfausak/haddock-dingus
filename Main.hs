{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified Database.SQLite.Simple as Sql
import qualified Documentation.Haddock.Markup as Haddock
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Environment as Environment
import qualified System.Random as Random
import qualified Text.Printf as Printf
import qualified Text.Read as Read

main :: IO ()
main = do
  db <- Maybe.fromMaybe ":memory:" <$> Environment.lookupEnv "DATABASE"
  port <- maybe 3000 read <$> Environment.lookupEnv "PORT"

  Sql.withConnection db $ \sql -> do
    Sql.execute_
      sql
      "create table Input (key integer primary key, contents text not null)"
    Sql.execute sql "insert into Input (key, contents) values (0, ?)"
      . Sql.Only
      $ unlines sample

    Warp.run port . loggingMiddleware $ \request respond ->
      case (Wai.requestMethod request, Wai.pathInfo request) of
        ("GET", []) -> respond $ Wai.responseLBS Http.found302 [(Http.hLocation, "/inputs/0")] LazyByteString.empty
        ("POST", ["inputs"]) -> do
          body <- Wai.strictRequestBody request
          let input = maybe Text.empty Encoding.decodeUtf8Lenient . Monad.join . lookup "input" . Http.parseQuery $ LazyByteString.toStrict body
          key <- Random.randomRIO (1, maxBound :: Int)
          Sql.execute sql "insert into Input (key, contents) values (?, ?)" (key, input :: Text.Text)
          respond $ Wai.responseLBS Http.found302 [(Http.hLocation, Encoding.encodeUtf8 . Text.pack $ "/inputs/" <> show key)] LazyByteString.empty
        ("GET", ["inputs", rawKey]) -> do
          let key = Maybe.fromMaybe 0 . Read.readMaybe $ Text.unpack rawKey :: Int
          rows <-
            Sql.query
              sql
              "select contents from Input where key = ?"
              [key]
          let contents = case rows of
                [] -> "Not found!"
                row : _ -> Sql.fromOnly row
          respond
            . Wai.responseLBS Http.ok200 [(Http.hContentType, "text/html;charset=utf-8")]
            . H.renderBS
            . H.doctypehtml_
            $ do
              H.head_ $ do
                H.title_ "Haddock Dingus"
                H.link_
                  [ H.crossorigin_ "anonymous",
                    H.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
                    H.integrity_ "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH",
                    H.rel_ "stylesheet"
                  ]
                H.script_
                  [ H.async_ "",
                    H.crossorigin_ "anonymous",
                    H.id_ "MathJax-script",
                    H.integrity_ "sha384-Wuix6BuhrWbjDBs24bXrjf4ZQ5aFeFWBuKkFekO2t8xFU0iNaLQfp2K6/1Nxveei",
                    H.src_ "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-mml-chtml.js"
                  ]
                  Text.empty
              H.body_ $ do
                H.header_ [H.class_ "bg-dark mb-3 navbar"] $ do
                  H.div_ [H.class_ "container"] $ do
                    H.a_ [H.class_ "navbar-brand text-light", H.href_ "/"] "Haddock Dingus"
                H.main_ [H.class_ "my-3"] $ do
                  H.div_ [H.class_ "container"] $ do
                    H.div_ [H.class_ "row"] $ do
                      H.div_ [H.class_ "col-lg mb-3"] $ do
                        H.h2_ "Input"
                        H.form_ [H.action_ "/inputs", H.method_ "post"] $ do
                          H.textarea_
                            [ H.class_ "font-monospace form-control mb-3",
                              H.name_ "input",
                              H.rows_ "10"
                            ]
                            $ H.toHtml contents
                          H.button_ [H.class_ "btn btn-primary", H.type_ "submit"] "Submit"
                      H.div_ [H.class_ "col-lg"] $ do
                        H.h2_ "Output"
                        H.div_ [H.class_ "card"]
                          . H.section_ [H.class_ "card-body"]
                          . Haddock.markup htmlMarkup
                          . Haddock.overIdentifier (curry Just)
                          . Haddock._doc
                          $ Haddock.parseParas Nothing contents
                H.footer_ [H.class_ "my-3 text-secondary"] $ do
                  H.div_ [H.class_ "border-top container pt-3"] $ do
                    H.a_
                      [H.class_ "link-secondary", H.href_ "https://haskell-haddock.readthedocs.io/latest/"]
                      "haskell-haddock.readthedocs.io"
        _ -> respond $ Wai.responseLBS Http.status404 [] "Not found"

loggingMiddleware :: Wai.Middleware
loggingMiddleware handle request respond = do
  handle request $ \response -> do
    Printf.printf
      "%d %s %s %s\n"
      (Http.statusCode $ Wai.responseStatus response)
      (Encoding.decodeUtf8Lenient $ Wai.requestMethod request)
      (Encoding.decodeUtf8Lenient $ Wai.rawPathInfo request)
      (Encoding.decodeUtf8Lenient $ Wai.rawQueryString request)
    respond response

htmlMarkup :: Haddock.DocMarkupH Void.Void (Haddock.Namespace, String) (H.Html ())
htmlMarkup =
  Haddock.Markup
    { Haddock.markupAName = \x -> H.a_ [H.name_ $ Text.pack x] mempty,
      Haddock.markupAppend = mappend,
      Haddock.markupBold = H.strong_,
      Haddock.markupCodeBlock = H.pre_ . H.code_,
      Haddock.markupDefList = H.dl_ . foldMap (\(t, d) -> H.dt_ t <> H.dd_ d),
      Haddock.markupEmphasis = H.em_,
      Haddock.markupEmpty = mempty,
      Haddock.markupExample = foldMap $ \x -> H.pre_ . H.code_ . H.toHtml . List.intercalate "\n" $ (">>> " <> Haddock.exampleExpression x) : Haddock.exampleResult x,
      Haddock.markupHeader = \x -> H.term (Text.pack $ "h" <> show (Haddock.headerLevel x)) $ Haddock.headerTitle x,
      Haddock.markupHyperlink = \x -> let url = Text.pack $ Haddock.hyperlinkUrl x in H.a_ [H.href_ url] . Maybe.fromMaybe (H.toHtml url) $ Haddock.hyperlinkLabel x,
      Haddock.markupIdentifier = H.code_ . H.toHtml . snd,
      Haddock.markupIdentifierUnchecked = Void.absurd,
      Haddock.markupMathDisplay = \x -> H.div_ . H.toHtml $ "\\[" <> x <> "\\]",
      Haddock.markupMathInline = \x -> H.span_ . H.toHtml $ "\\(" <> x <> "\\)",
      Haddock.markupModule = \x -> H.code_ . Maybe.fromMaybe (H.toHtml $ Haddock.modLinkName x) $ Haddock.modLinkLabel x,
      Haddock.markupMonospaced = H.code_,
      Haddock.markupOrderedList = H.ol_ . foldMap (\(i, x) -> H.li_ [H.value_ . Text.pack $ show i] x),
      Haddock.markupParagraph = H.p_,
      Haddock.markupPic = \x -> H.img_ [H.alt_ . maybe Text.empty Text.pack $ Haddock.pictureTitle x, H.src_ . Text.pack $ Haddock.pictureUri x],
      Haddock.markupProperty = H.pre_ . H.code_ . H.toHtml . mappend "prop> ",
      Haddock.markupString = H.toHtml,
      Haddock.markupTable = error "markupTable (impossible)",
      Haddock.markupUnorderedList = H.ul_ . foldMap H.li_,
      Haddock.markupWarning = error "markupWarning (impossible)"
    }

sample :: [String]
sample =
  [ "= Haddock Markup",
    "",
    "This sample is meant to showcase many of Haddock's common features. It is not meant to be exhaustive.",
    "",
    "Please edit this text and re-submit the form!",
    "",
    "== Inline Formatting",
    "",
    "Some /italic/ text.",
    "",
    "Some __bold__ text.",
    "",
    "Some @mono@ text.",
    "",
    "A <http://example.com> link.",
    "",
    "Another [hyper](http://example.com) link.",
    "",
    "An ![logo](https://haskell.foundation/assets/images/logos/hf-logo-100-alpha.png) image.",
    "",
    "== Block Formatting",
    "",
    "Headings are used throughout this example, from @= h1@ to @====== h6@.",
    "",
    "- unordered",
    "- list",
    "",
    "1. ordered",
    "2. list",
    "",
    "[d] definition",
    "[l] list",
    "",
    "@",
    "formatted",
    "  /code/ __block__",
    "@",
    "",
    "> unformatted",
    ">   /code/ __block__",
    "",
    "== Haskell",
    "",
    ">>> negate 1 -- GHCi example",
    "-1",
    "",
    "prop> a + b = b + a -- property",
    "",
    "Module \"Data.Maybe\" name.",
    "",
    "Qualified 'Data.Maybe.Maybe' type.",
    "",
    "Unqualified 'Maybe' type.",
    "",
    "Qualified 'Data.Maybe.fromMaybe' term.",
    "",
    "Unqualified 'maybe' term.",
    "",
    "(When using Haddock for real, the above modules, types, terms would be hyperlinked if they can be resolved.)",
    "",
    "== Math",
    "",
    "Inline \\( ax^2 + bx + c = 0 \\) math.",
    "",
    "Display math: \\[ x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a} \\]",
    "",
    "== Extra",
    "",
    "You won't be able to see it, but this will add an @\\<a name=\"anchor\"\\>@ element that can be used to link to specific pieces of content.",
    "",
    "#anchor#",
    "",
    "Anything can be \\'escaped\\' with backslashes, even \\ backslashes. Escaping works even when it's not \\\"necessary\\\".",
    "",
    "Most markup /can @be __nested__@/.",
    "",
    "Tables don't seem to work here."
  ]
