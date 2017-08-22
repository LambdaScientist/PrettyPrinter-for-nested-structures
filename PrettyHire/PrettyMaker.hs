{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE InstanceSigs #-}

import Prelude

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))

import Text.PrettyPrint hiding (char,Str)

-- import Text.PrettyPrint.GenericPretty
-- data Tree = Leaf String | Node [Tree]

-- data Tree a = Leaf a | Node [Tree a] | StrList [Tree a] deriving (Generic, Show)
-- instance (Out a) => Out (Tree a)

-- parseTree :: Parser (Tree String)
-- parseTree = node <|> leaf <|> strList
--   where
--     node = Node <$> between (char '(') (char ')') (many parseTree)
--     strList = StrList <$> between (char '[') (char ']') strsList
--     leaf = Leaf <$> many1 (noneOf "()[],")

-- -- commaSep p  = p `sepBy` (char ",")

-- strsList :: Parser [(Tree String)]
-- strsList = sepBy parseTree (char ',')



-- main = do ast <- readFile "InputFile.txt"
--           print ast


-- main = do ast <- readFile "InputFile.txt"
--           case (parse parseTree "" ast) of
--             Left err  -> print err
--             Right xs  -> pp xs




data Atoms = Str [String]
           | List [[Atoms]] 
           | Tuple [Atoms] 
          --  | ListId String [Atoms] 
          --  | Par String [Atoms] 
           deriving (Show)

parseAtoms :: Parser Atoms
parseAtoms = do spaces
                tuple <|> str <|> list  --  try par <|> try listId <|> 
  where
    tuple = Tuple <$> between (char '(') (char ')') (many1 parseAtoms)
    -- par = Par <$> (many1 (noneOf "()[], ")) 
    --           <*> (between (char '(') (char ')') (many parseAtoms))
    -- listId = ListId <$> (many1 (noneOf " ()[],")) 
    --                 <*> (between (char '(') (char ')') (many parseAtoms))
    list = List <$> between (char '[') (char ']') atomList
    str = Str <$> strsList

strsList :: Parser [String]
strsList = sepEndBy1 (many1 (noneOf "()[], ")) (char ' ')

atomList :: Parser [[Atoms]]
atomList = sepBy1 (many parseAtoms) (char ',')

atomPretty :: Atoms -> Doc
-- atomPretty (Par strId as) = text strId 
--                             <> nest 2 (vcat (atomPretty <$> as))
-- atomPretty (ListId strId as) = text strId 
--                             <> nest 2 (vcat (atomPretty <$> as))
-- atomPretty (List atomList) = text "[" $+$ nest 2 (vcat (atomPretty <$> as)) $$ text "]" 
atomPretty (List atomList) = text "[" 
                             <+> nest 2 (vcat (map (vcat . map atomPretty) atomList)) 
                             $$ text "]" 
atomPretty (Tuple as) = text "(" <+> nest 2 (vcat (atomPretty <$> as)) $$ text ")" 
atomPretty (Str strs) = hcat $ map text strs 

main = do --ast <- readFile "InputFile.txt"
          ast <- readFile "/home/burhopja/Documents/burhopSrc/Joy/joy-hs/JoyBnfc/example.txt"
          case (parse (many parseAtoms) "" ast) of
            Left err  -> print err
            Right xs  -> writeFile "astPretty.txt" $ (render.vcat.map atomPretty) xs


-- instance (Show a) => Show (Tree a) where
--   -- show ::  => Tree a -> String
--   show = pretty 

-- parPrint :: String -> Doc
-- parPrint [] = []
-- parPrint (c:str) | c == "(" = 
--                  | c == ")" = 

-- tree1 :: Tree Int
-- tree1 = Node [Leaf 1, Leaf 1] 

-- barTest = pp tree1

-- fooText = parseTest parseTree "((a b c) a b c)"