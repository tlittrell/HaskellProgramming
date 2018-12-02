import Control.Monad (join)

andOne x = [x, 1]

myBind :: Monad m => (a -> m b) -> m a -> m b
myBind f xs = join $ fmap f xs