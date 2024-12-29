-- | Environment for the blog.
module HsBlog.Env where

-- | Environment for the blog.
data Env
  = Env
    { eBlogName :: String -- ^ Name of the blog
    , eStylesheetPath :: FilePath -- ^ Path to the stylesheet
    }
  deriving Show

-- | Default environment for the blog.
defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css"
