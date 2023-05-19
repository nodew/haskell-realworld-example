module Conduit.Api.Comment where

import Conduit.Api.Common
import Conduit.App
import Conduit.Core.Article
import Conduit.Core.Comment
import Conduit.Core.User
import qualified Conduit.Repository.Article as ArticleRepository
import qualified Conduit.Repository.Comment as CommentRepository
import Conduit.Util
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Time
import Data.UUID
import RIO
import Servant

data CommentData = CommentData
    { commentDataId :: UUID
    , commentDataBody :: Text
    , commentDataCreatedAt :: UTCTime
    , commentDataUpdatedAt :: UTCTime
    , commentDataAuthor :: UserProfile
    }
    deriving (Eq, Show, Generic)

instance ToJSON CommentData where
    toJSON = genericToJSON $ toJsonOptions 11

instance FromJSON CommentData where
    parseJSON = genericParseJSON $ toJsonOptions 11

newtype NewCommentData = NewCommentData
    {newCommentBody :: Text}
    deriving (Generic)

instance ToJSON NewCommentData where
    toJSON = genericToJSON $ toJsonOptions 10

instance FromJSON NewCommentData where
    parseJSON = genericParseJSON $ toJsonOptions 10

newtype BoxedComment a = BoxedComment
    {unBoxedComment :: a}
    deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BoxedComment a) where
    parseJSON = genericParseJSON $ toJsonOptions 7

instance ToJSON a => ToJSON (BoxedComment a) where
    toJSON = genericToJSON $ toJsonOptions 7

newtype CommentsResponse = CommentsResponse
    {cmtRespComments :: [CommentData]}
    deriving (Generic)

instance ToJSON CommentsResponse where
    toJSON = genericToJSON $ toJsonOptions 7

mapEnrichedCommentToCommentData :: (Comment, User, Bool) -> CommentData
mapEnrichedCommentToCommentData (comment, author, followingAuthor) =
    CommentData
        { commentDataId = commentUUID comment
        , commentDataBody = commentBody comment
        , commentDataCreatedAt = commentCreatedAt comment
        , commentDataUpdatedAt = commentUpdatedAt comment
        , commentDataAuthor = mapUserToUserProfile author followingAuthor
        }

type CommentApi =
    "articles"
        :> Capture "slug" Slug
        :> "comments"
        :> ( AuthProtect "Optional"
                :> Get '[JSON] CommentsResponse
                :<|> AuthProtect "Required"
                    :> ReqBody '[JSON] (BoxedComment NewCommentData)
                    :> Post '[JSON] (BoxedComment CommentData)
                :<|> AuthProtect "Required"
                    :> Capture "commentId" UUID
                    :> Delete '[JSON] NoContent
           )

getAllComments :: Slug -> Maybe User -> AppM CommentsResponse
getAllComments slug mbUser =
    ArticleRepository.getArticleBySlug slug
        >>= maybe
            (throwIO err404)
            ( \article -> do
                comments <- CommentRepository.getEnrichedCommentsByArticleId mbUser (articleId article)
                return $ CommentsResponse $ map mapEnrichedCommentToCommentData comments
            )

createComment ::
    Slug ->
    User ->
    BoxedComment NewCommentData ->
    AppM (BoxedComment CommentData)
createComment slug user (BoxedComment newComment) =
    ArticleRepository.getArticleBySlug slug >>= maybe (throwIO err404) createComment
    where
        createComment article = do
            currentTime <- liftIO getCurrentTime
            uuid <- liftIO newUUID
            comment <-
                CommentRepository.addComment $
                    Comment
                        { commentId = CommentId 0
                        , commentUUID = uuid
                        , commentBody = newCommentBody newComment
                        , commentArticleId = articleId article
                        , commentAuthorId = userId user
                        , commentCreatedAt = currentTime
                        , commentUpdatedAt = currentTime
                        }
            flipMaybe comment (throwIO err404) $ \comment' ->
                return $ BoxedComment $ mapEnrichedCommentToCommentData (comment', user, False)

deleteComment ::
    Slug ->
    User ->
    UUID ->
    AppM NoContent
deleteComment slug user uuid =
    ArticleRepository.getArticleBySlug slug
        >>= maybe
            (throwIO err404)
            ( \article -> do
                CommentRepository.getCommentByUUID uuid
                    >>= maybe
                        (throwIO err404)
                        ( \comment -> do
                            if (commentAuthorId comment /= userId user) || (commentArticleId comment /= articleId article)
                                then throwIO err403
                                else do
                                    success <- CommentRepository.deleteCommentById (commentId comment)
                                    if success
                                        then return NoContent
                                        else throwIO err400
                        )
            )

commentServer :: ServerT CommentApi AppM
commentServer slug =
    getAllComments slug
        :<|> createComment slug
        :<|> deleteComment slug
