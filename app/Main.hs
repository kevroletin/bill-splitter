module Main where

import           Control.Monad
import           Control.Monad.Except (throwError, MonadError)
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Read       as T
import           GHC.Stack
import           System.Environment

newtype ParsingError = ParsingError String deriving (Eq, Show)

-- foldMapA helpers
newtype WrapApp f m = WrapApp { unWrapApp :: f m }

instance (Applicative f, Monoid m) => Monoid (WrapApp f m) where
  mempty                          = WrapApp $ pure mempty
  mappend (WrapApp a) (WrapApp b) = WrapApp $ mappend <$> a <*> b

foldMapA :: (Foldable t, Applicative f, Monoid m) => (a -> f m) -> t a -> f m
foldMapA f = unWrapApp . foldMap (WrapApp . f)

convertMap :: (Ord k1, Ord k2) => Map (k1,k2) a -> Map k1 (Map k2 a)
convertMap m = Map.fromListWith Map.union
               [ (i, Map.singleton j x) | ((i,j),x) <- Map.toList m ]

findMap :: (HasCallStack, Ord a, Show a) => a -> Map a b -> b
findMap x m = Map.findWithDefault err x m
  where err = error ("Provided map doesn't has requested key: " ++ show x)

newtype Name     = Name { unName :: Text } deriving (Eq, Ord, Show)
newtype Item     = Item { unItem :: Text } deriving (Eq, Ord, Show)
data Input       = Used | Unused deriving (Eq, Ord, Show)
data Interaction = Interaction { bIn :: Input, bOut :: Int } deriving (Eq, Ord, Show)

data TableF a =
  TableF
  { cells   :: Map Item (Map Name a)
  , persons :: [Name]
  , items   :: [Item]
  } deriving Show

type InTable  = TableF Interaction
type OutTable = TableF (Double, Int)

parsingFailure :: (MonadReader Pos m, MonadError ParsingError m) => String -> m a
parsingFailure msg = do
  pos <- ask
  throwError (ParsingError (msg ++ ": " ++ show pos))

newtype Pos = Pos { posLine :: Int } deriving (Eq, Show)

data Row = Row { rowItem  :: Item
               , rowLine  :: Int
               , rowCells :: [(Text, Text)]
               } deriving (Eq, Show)

summaryItem :: Item
summaryItem = Item "summary"

parseTable1 :: [(Int, Text)] -> Either ParsingError [Row]
parseTable1 [] = pure []
parseTable1 ((i, x):xs) = do
  header@(Row _ _ (length -> len)) <- runReaderT (eatLine x) (Pos i)

  let loop :: [(Int, Text)] -> Either ParsingError [Row]
      loop [] = pure []
      loop ((j, t):ts) =
        if "|-" `T.isPrefixOf` t then loop ts
        else do
          res <- flip runReaderT (Pos j) $ do
            res0@(Row _ _ cols) <- eatLine t
            when (length cols /= len) $
              parsingFailure "Row length differs from header length"
            pure res0
          (res :) <$> loop ts

  (header :) <$> loop xs

  where
    pairs :: (MonadReader Pos m, MonadError ParsingError m)
          => [Text] -> m [(Text, Text)]
    pairs [t] | T.null t   = pure []
              | otherwise = parsingFailure "Odd number of cells"
    pairs []  = pure []
    pairs (a:b:rest) =  ((a, b) :) <$> pairs rest

    eatLine :: (MonadReader Pos m, MonadError ParsingError m)
            => Text -> m Row
    eatLine (splitCells -> ("" : w : ws)) = Row (Item w) <$> asks posLine <*> pairs ws
    eatLine (splitCells -> (_  : _ : _ )) = parsingFailure "Line should start with a '|'"
    eatLine _ = parsingFailure "Empty table line"

    splitCells = fmap T.strip . T.split (== '|')

parseTable2 :: [Row] -> Either ParsingError InTable
parseTable2 [] = error "Empty table"
parseTable2 (Row _ line0 th : xs) = do
  people <- fmap Name <$> runReaderT (parseHeader th) (Pos line0)

  let parseRow (Row item line cols) =
        runReaderT (Map.singleton item <$> foldMapA eatCells (people `zip` cols))
                   (Pos line)

  cells <- foldMapA parseRow xs

  pure (TableF cells people (fmap rowItem xs))

  where
    eatTh (T.words -> [a, b]) str
      | str == T.toLower b = pure a
    eatTh a str = parsingFailure ("Expecting name + " ++ T.unpack str ++ " in " ++ show a)

    parseHeader :: (MonadReader Pos m, MonadError ParsingError m)
                => [(Text, Text)] -> m [Text]
    parseHeader [] = pure []
    parseHeader ((a, b) : rest) = do
      a' <- eatTh a "in"
      b' <- eatTh b "out"
      when (a' /= b') $ parsingFailure (show a' ++ " != " ++ show b')
      (a' :) <$> parseHeader rest

    eatCells :: (MonadReader Pos m, MonadError ParsingError m)
             => (Name, (Text, Text)) -> m (Map Name Interaction)
    eatCells (person, (inCell, outCell)) = do
      let inRes = if inCell == "-" || T.null inCell then Unused else Used
      outRes <-
        if T.null outCell
          then pure 0
          else either parsingFailure (pure . fst) $ T.decimal outCell
      pure . Map.singleton person $ Interaction inRes outRes

parseTable :: [(Int, Text)] -> Either ParsingError InTable
parseTable = parseTable2 <=< parseTable1

computeBalance :: InTable -> OutTable
computeBalance TableF{..} = TableF { cells = newCells, .. }
  where
    rowSumFn item f = getSum . foldMap (Sum . f) $ findMap item cells

    rowOut :: Item -> Int
    rowOut item = rowSumFn item bOut

    rowInCnt :: Item -> Int
    rowInCnt item = rowSumFn item (fromEnum . (== Used) . bIn)

    rowCost :: Item -> Double
    rowCost item = fromIntegral (rowOut item) / fromIntegral (rowInCnt item)

    row item = Map.mapWithKey f $ findMap item cells
      where cost = rowCost item
            f _  (Interaction Unused out) = (0.0 , out)
            f _  (Interaction Used   out) = (cost, out)

    newCells = Map.fromList (items `zip` fmap row items)

roundDig :: Int -> Double -> Double
roundDig n f = fromInteger (round $ f * (10^n)) / (10.0^^n)

showDouble :: Double -> String
showDouble x
  | a == 0             = ""
  | a == fromInteger b = show b
  | otherwise          = show a
  where a = roundDig 1 x
        b = round x

computeSummary :: OutTable -> [(Double, Int)]
computeSummary TableF{..} = fmap colSum persons
  where
    getCell :: Name -> Item -> (Double, Int)
    getCell person item = findMap person (findMap item cells)

    colSum :: Name -> (Double, Int)
    colSum person = bimap getSum getSum
                    . foldMap (f . getCell person)
                    . filter (/= summaryItem)
                    $ items
      where f (a, b) = (Sum a, Sum b)

addSummary :: OutTable -> OutTable
addSummary tbl@TableF{..} = TableF{ cells = Map.insert summaryItem summaryRow cells
                                  , items = filter (/= summaryItem) items ++ [summaryItem]
                                  , .. }
  where
    summaryList :: [(Double, Int)]
    summaryList = computeSummary $ tbl { cells = Map.delete summaryItem cells }

    summaryRow = Map.fromList (persons `zip` summaryList)

drawSummary' :: [Name] -> [(Double, Int)] -> [String]
drawSummary' names summary = fmap f (names `zip` summary)
  where f (n, (inp, outp)) = "+ " ++ (T.unpack . unName) n ++ " :: " ++ showDouble balance
            where balance = inp - fromIntegral outp

drawSummary :: OutTable -> [String]
drawSummary TableF{..} =
  case Map.lookup summaryItem cells of
    Nothing -> []
    Just row -> drawSummary' persons (fmap (`findMap` row) persons)

drawTable :: OutTable -> [String]
drawTable TableF{..} = header : "|-" : fmap row items
  where
    showPair (a , b) = [showDouble a, showNotZero b]

    showNotZero x = if x == 0 then "" else show x

    rowNums item = foldMap showPair $ fmap (`findMap` m) persons
      where m = findMap item cells

    wrap x = intercalate "|" ("" : x ++ [""])

    row item = wrap $ (T.unpack . unItem) item : rowNums item

    header =  wrap $ "" : foldMap (\(T.unpack . unName -> p) -> [p ++ " IN", p ++ " OUT"]) persons

throwEither :: (HasCallStack, Show a) => Either a b -> b
throwEither = either (error . show) id

main :: IO ()
main = do xs <- zip [1 .. ] . filter ("|" `T.isPrefixOf`) . T.lines <$> T.getContents
          let tbl = throwEither (addSummary . computeBalance <$> parseTable xs)
          mapM_ putStrLn (drawTable tbl ++ [" "] ++ drawSummary tbl)
