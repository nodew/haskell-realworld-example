module Test.Conduit.Api.StorySpec where

import Test.Hspec
import RIO
import qualified RIO.List as L
import qualified Test.Hspec.Wai as THW
import Test.Conduit.TestHelper
import Conduit.Api.Common
import Conduit.Api.User
import Conduit.Api.Article
import Conduit.Api.Comment
import Data.Maybe
import Data.Aeson
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- * popularism is defined by the number of comments
-- A user comes to the website and sees a list of most popular articles
-- A user reads a few articles
-- A user finds his favourite author and looks for a list of article from this author sorted by most recent
-- A user tries to comment on a article
-- A user cannot comment on a article until he logs in
-- A user signs in and comments on a article
-- A user publishes his own articles

spec :: Spec
spec = do
    let username = "test"
    context "Story APIs"
        $ beforeAll_ (setupTestUser username)
        $ beforeAll_ (setupTestArticle "1")
        $ afterAll_ cleanUpDb
        $ withApplication
        $ do
            it "should get a list of articles" $ do
                response <- THW.request methodGet "/api/articles" [] ""
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe ArticlesResponse
                liftIO $ mbBody `shouldNotBe` Nothing
                let (ArticlesResponse count [article]) = fromJust mbBody
                liftIO $ count `shouldBe` 1
                liftIO $ articleDataTitle article `shouldBe` "test article title"

            it "should get an article by slug" $ do
                response <- THW.request methodGet "/api/articles/test-article" [] ""
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (BoxedArticle ArticleData)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (BoxedArticle articleData) = fromJust mbBody
                liftIO $ articleDataBody articleData `shouldBe` "test article body"
                liftIO $ articleDataTitle articleData `shouldBe` "test article title"

            it "should get a list of articles from a specific author" $ do
                response <- THW.request methodGet "/api/articles?author=test" [] ""
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe ArticlesResponse
                liftIO $ mbBody `shouldNotBe` Nothing
                let (ArticlesResponse count [article]) = fromJust mbBody
                liftIO $ count `shouldBe` 1
                liftIO $ articleDataTitle article `shouldBe` "test article title"

            it "should fail to comment without authentication" $ do
                let comment = BoxedComment (NewCommentData "a good test article")
                response <- THW.request
                                methodPost
                                "/api/articles/test-article/comments"
                                [("Content-Type", "application/json")]
                                $ encode comment
                liftIO $ statusCode (simpleStatus response) `shouldBe` 401
                let mbBody = decode (simpleBody response) :: Maybe (BoxedComment CommentData)
                liftIO $ mbBody `shouldBe` Nothing

            it "should add a comment to an article" $ do
                let body = "a good test article"
                let comment = BoxedComment (NewCommentData body)
                token <- getAccessToken username
                response <-
                    THW.request
                        methodPost
                        "/api/articles/test-article/comments"
                        [ ("Content-Type", "application/json")
                        , ("Authorization", T.encodeUtf8 $ T.append "Token " token)]
                        $ encode comment
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (BoxedComment CommentData)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (BoxedComment commentData) = fromJust mbBody
                liftIO $ commentDataBody commentData `shouldBe` body
                liftIO $ (profileUsername $ commentDataAuthor commentData) `shouldBe` username

            it "should create an article" $ do
                let title = "test article title"
                let description = "test article description"
                let body = "test article body"
                let tagList = []
                let article = BoxedArticle (NewArticleData title description body tagList)
                token <- getAccessToken username
                response <-
                    THW.request
                        methodPost
                        "/api/articles"
                        [ ("Content-Type", "application/json")
                        , ("Authorization", T.encodeUtf8 $ T.append "Token " token)]
                        $ encode article
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (BoxedArticle ArticleData)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (BoxedArticle articleData) = fromJust mbBody
                liftIO $ articleDataBody articleData `shouldBe` body
                liftIO $ (profileUsername $ articleDataAuthor articleData) `shouldBe` username

