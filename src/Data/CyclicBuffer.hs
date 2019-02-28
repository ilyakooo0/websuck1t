module Data.CyclicBuffer
    (
        empty,
        insert,
        getFrom,
        CyclicBuffer,
        lastToken
    ) where

import Debug.Trace

data CyclicBuffer a = CyclicBuffer {
    lastId :: Int,
    limit :: Int,
    elements :: [a]
} deriving Show

empty :: Int -> CyclicBuffer a
empty l = CyclicBuffer 0 l []

insert :: CyclicBuffer a -> a -> (Int, CyclicBuffer a)
insert (CyclicBuffer last l ee) e = (last+1, CyclicBuffer (last+1) l $ take l (e:ee))

getFrom :: CyclicBuffer a -> Int -> Maybe [a]
getFrom (CyclicBuffer last limit es) req = 
    if index < limit && index >= 0
        then Just $ take index es
        else Nothing
    where index = traceShowId $ last - req

lastToken :: CyclicBuffer a -> Int
lastToken (CyclicBuffer l _ _) = l

-- instance Traversable CyclicBuffer where
--     traverse f (CyclicBuffer last left es) = CyclicBuffer last left <$> traverse f es

-- instance Functor CyclicBuffer where
--     fmap f (CyclicBuffer last left es) = CyclicBuffer last left $ map f es

-- instance Foldable CyclicBuffer where 