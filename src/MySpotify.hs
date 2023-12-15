module MySpotify where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.List (groupBy)
import Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DataRow = DataRow
  { trackId :: String
  , trackName :: String
  , albumId :: String
  , albumName :: String
  , releaseDate :: String
  , duration :: Int
  }

data Album = Album
  { albumId :: String
  , albumName :: String
  , releaseDate :: String
  , songs :: [DataRow]
  }

transformData :: [DataRow] -> [Album]
transformData = map createAlbum . groupBy (\a b -> albumId a == albumId b)
  where
    createAlbum rows = Album
      { albumId = albumId (head rows)
      , albumName = albumName (head rows)
      , releaseDate = releaseDate (head rows)
      , songs = rows
      }

findLongestAlbum :: Int -> [Album] -> Maybe Album
findLongestAlbum year albums = maximumBy (comparing (sum . map duration . songs))
  $ filter (\album -> take 4 (releaseDate album) == show year) albums

findAlbumWithHighestAverage :: Int -> [Album] -> Maybe Album
findAlbumWithHighestAverage year albums = maximumBy (comparing (averageDuration . songs))
  $ filter (\album -> take 4 (releaseDate album) == show year) albums
  where
    averageDuration songs = sum (map duration songs) / fromIntegral (length songs)

findShortestAlbum :: Int -> [Album] -> Maybe Album
findShortestAlbum year albums = minimumBy (comparing (sum . map duration . songs))
  $ filter (\album -> take 4 (releaseDate album) == show year) albums
