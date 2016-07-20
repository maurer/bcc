import Data.Char
import System.Environment
import Data.List
import Control.Applicative

main = do
  [tgt] <- getArgs
  newContents <- fmap (unlines . (map frobulate) . lines) $ readFile tgt
  writeFile (tgt ++ ".hexkip") newContents

readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing

cond f a = if f a then Just a else Nothing

printStr = and . (map isPrint)
ascStr = and . (map isAscii)
convert (x0:x1:x2:x3:x4:x5:x6:x7:[]) = do
  b0 <- readMaybe ('0':'x':x0:x1:[])
  b1 <- readMaybe ('0':'x':x2:x3:[])
  b2 <- readMaybe ('0':'x':x4:x5:[])
  b3 <- readMaybe ('0':'x':x6:x7:[])
  return $ reverse $ map chr $ filter (/= 0) [b0, b1, b2, b3]

ip s@(x0:x1:x2:x3:x4:x5:x6:x7:[]) = do
  b0 <- readMaybe ('0':'x':x0:x1:[])
  b1 <- readMaybe ('0':'x':x2:x3:[])
  b2 <- readMaybe ('0':'x':x4:x5:[])
  b3 <- readMaybe ('0':'x':x6:x7:[])
  let bs = [b0, b1, b2, b3]
  let str = intercalate "." $ reverse $ map show bs 
  if (or $ map (== 0) bs) || (or $ map (== 255) bs)
    then Nothing
    else case fullConv s of
           Just x -> return $ x ++ " | " ++ str
           Nothing -> return str

fullConv constAddr = ((convert constAddr) >>= cond printStr >>= cond ascStr)

frobulate line =
  case frobulate' line of
    Just x  -> x
    Nothing -> line

frobulate' line = do
  i <- elemIndex ':' line
  let (constAddr, _:_:_:rs) = splitAt i line
  if length constAddr == 8
    then do v <- (ip constAddr) <|> (fullConv constAddr)
            return $ "'" ++ v ++ "' (" ++ constAddr ++ ") " ++ rs
    else Nothing
