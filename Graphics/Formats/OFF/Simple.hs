{-# LANGUAGE NamedFieldPuns #-}

-- | A simple datatype and parser for 3D OFF files. A single type is
-- used for OFF information with or without color. Loosely based on
-- <http://people.sc.fsu.edu/~jburkardt/data/off/off.html>, but uses the
-- file header to determine whether the object's faces contain color
-- values. 
module Graphics.Formats.OFF.Simple (
    OFF (..)
  , Face (..)
  , vertexCount
  , faceCount
  , hasColor
  , parseOFF
  , readOFFFile
) where

import Control.Applicative hiding ( (<|>), many, optional )

import qualified Data.Vector as V
import Data.Vector ( Vector )

import Numeric

import Text.Parsec
import Text.Parsec.String ( Parser )

-- | A vertex is just a triple of 'Double's.
type Vertex = (Double, Double, Double)

-- | A color is a 4-tuple of 'Double's representing RGB values.
type Color = (Double, Double, Double)

-- | A face is a vector of vertex indices and an optional color value.
data Face = Face (Vector Int) (Maybe Color)
            deriving (Show, Eq, Ord)

-- | Representation of an object in OFF format; a pair of vectors
-- containing the vertices and the faces of the object.
data OFF = OFF { 
                 vertices    :: Vector Vertex
               , faces       :: Vector Face
               }
           deriving (Show, Eq, Ord)

-- | The number of vertices in an 'OFF' object.
vertexCount :: OFF -> Int
vertexCount (OFF { vertices }) = V.length vertices

-- | The number of faces in an 'OFF' object.
faceCount :: OFF -> Int
faceCount (OFF { faces }) = V.length faces

-- | Returns 'True' if the 'OFF' object has color values associated
-- with its faces.
hasColor :: OFF -> Bool
hasColor (OFF { faces }) =
  case faces V.!? 0 of
    Just (Face _ (Just _)) -> True
    _ -> False

-- | Determines whether we're handling color files.
parseHeader :: Parser Bool
parseHeader = string "OFF" *> (char 'C' *> return True
                                       <|> return False)

-- | Comments span from @#@ to the end of the line.
parseComment :: Parser ()
parseComment = char '#' >> manyTill anyChar newline >> return ()

-- | Parse the vertex and face counts.
parseCounts :: Parser (Int, Int)
parseCounts = do
    vc <- parseInt
    spaces
    fc <- parseInt
    optional (many (oneOf "\t ") >> parseInt) -- edges ignored
    return (vc, fc)
  <?> "vertex, face, and edge count"

-- | Parse a line of vertex coordinates.
parseVertex :: Parser Vertex
parseVertex = do
    [x, y, z] <- count 3 (parseDouble <* spaces)
    return (x, y, z)
  <?> "x, y, z coordinates"

-- | Parse the given number of vertex lines
parseVertices :: Int -> Parser (Vector Vertex)
parseVertices n = V.replicateM n (parseVertex <* eatWhitespace)
  <?> show n ++ " vertices"

-- | Parse a line of non-colored vertex indices defining a face.
parseFace :: Parser Face
parseFace = do
    numVerts <- parseInt
    spaces
    verts <- V.replicateM numVerts (parseInt <* spaces)
    return $ Face verts Nothing
  <?> "vertex indices"

-- | First parse the indices of a face, then parse three additional
-- 'Double's representing the color value.
parseFaceC :: Parser Face
parseFaceC = do
    (Face verts Nothing) <- parseFace
    [r, g, b] <- count 3 (parseDouble <* spaces)
    return $ Face verts (Just (r, g, b))
  <?> "3 color components"

-- | Parse the given number of non-colored faces.
parseFaces :: Int -> Parser (Vector Face)
parseFaces n = V.replicateM n (parseFace <* eatWhitespace)
  <?> show n ++ " faces"

-- | Parse the given number of colored faces.
parseFacesC :: Int -> Parser (Vector Face)
parseFacesC n = V.replicateM n (parseFaceC <* eatWhitespace)
  <?> show n ++ " faces"

-- | Parse a 'Text' string representing an OFF object.
parseOFF :: Parser OFF
parseOFF = do
  eatWhitespace
  isColor <- parseHeader
  eatWhitespace
  (numVerts, numFaces) <- parseCounts
  eatWhitespace
  verts <- parseVertices numVerts
  eatWhitespace
  faces <- if isColor
           then parseFacesC numFaces
           else parseFaces numFaces
  eatWhitespace >> eof
  return $ OFF verts faces

-- | Read an OFF object from the given 'FilePath', returning either
-- the corresponding 'OFF' value or a 'ParseError'.
readOFFFile :: FilePath -> IO (Either ParseError OFF)
readOFFFile f = parse parseOFF f <$> readFile f

-- | Parse and discard whitespace and comments up until the next
-- non-comment, non-whitespace character.
eatWhitespace :: Parser ()
eatWhitespace =  try (spaces >> parseComment >> eatWhitespace) 
             <|> spaces 
             <|> return ()

-- | Parse an unsigned decimal 'Int'.
parseInt :: Parser Int
parseInt = do 
  s <- getInput
  case readDec s of
    [(n, s')] -> n <$ setInput s'
    _ -> empty

-- | Parse a signed 'Double'.
parseDouble :: Parser Double
parseDouble = do 
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _ -> empty

