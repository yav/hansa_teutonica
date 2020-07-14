import Text.ParserCombinators.ReadP
import Data.Char
import System.Environment(getArgs)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

main :: IO ()
main =
  do [file] <- getArgs
     h <- openFile file ReadMode
     hSetEncoding h latin1
     txt <- hGetContents h
     case [ a | (a,"") <- readP_to_S pMap txt ] of
       [a] -> putStrLn (mapToLua a)


mapToLua (xs,ys) = unlines $
  [ "function talk()"
  , "  res = ''"
  ] ++ map cityToLua xs ++ map edgeToLua ys
  ++
  [ "  log(res)"
  , "end"
  ]

nameToLua x = '"' : x ++ "\""

cityToLua :: City -> String
cityToLua (n,loc,os) = unlines $
  ("  node(" ++ nameToLua n ++ ", " ++ show loc ++ ")")
  : [ "  office(" ++ show ty ++ ", " ++ show l ++ ")" | (ty,l) <- os ]

edgeToLua :: (String,[Dest]) -> String
edgeToLua (f,ts) = unlines $
  ("  from(" ++ nameToLua f ++ ")")
  : map destToLua ts

destToLua :: Dest -> String
destToLua (tgt,loc,rot,xs) = unlines $
  ("    to(" ++ nameToLua tgt ++ ", " ++ show loc ++ ", " ++ show rot ++ ")")
  : [ "    road(" ++ show x ++ ")" | x <- xs ]




pMap :: ReadP ([City], [(String,[Dest])])
pMap =
  do cities <- many1 pCity
     _ <- token (string "--")
     edges <- many1 pEdge
     pure (cities,edges)

type GUID = String

token :: ReadP a -> ReadP a
token p = do x <- p; skipSpaces; pure x

pGUID :: ReadP GUID
pGUID = token (char '#' >> munch1 isAlphaNum)

pCityName :: ReadP String
pCityName = token (between (char '"') (char '"') (munch1 ok))
  where ok c = isAlpha c || c == ' '

pInt :: ReadP Int
pInt = token (readS_to_P reads)

pOffice :: ReadP (String,Int)
pOffice =
  do ty <- token ((char 'T' >> pure "trader") +++ (char 'M' >> pure "merchant"))
     lvl <- pInt
     pure (ty,lvl)

type City = (String,GUID,[(String,Int)])

pCity :: ReadP City
pCity =
  do name <- pCityName
     loc <- pGUID
     _ <- token (char '-')
     offices <- pOffice `sepBy` token (char ',')
     pure (name,loc,offices)


pEdge :: ReadP (String,[Dest])
pEdge =
  do from <- pCityName
     tos <- many1 pDestination
     pure (from,tos)

type Dest = (String,GUID,Int,[GUID])

pDestination :: ReadP Dest
pDestination =
  do _ <- token (char '-')
     to <- pCityName
     loc <- pGUID
     rot <- pInt
     stops <- many1 pGUID
     pure (to,loc,rot,stops)


