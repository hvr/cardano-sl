{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, LambdaCase #-}

module Main where

import           Universum
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.FileEmbed (injectWith)

main :: IO ()
main = do
  (hash, progs) <- parseArgs
  mapM_ (setGitRev hash) progs

setGitRev :: ByteString -> FilePath -> IO ()
setGitRev hash prog = do
  putStrLn $ "Setting gitrev of " <> prog
  bs <- B8.readFile prog
  case injectWith "gitrev" hash bs of
    Just bs' -> do
      B8.writeFile prog bs'
      exitSuccess
    Nothing -> do
      B8.putStrLn $ "Failed setting gitrev to \"" <> hash <> "\""
      exitFailure

parseArgs :: IO (ByteString, [FilePath])
parseArgs = getArgs >>= \case
  (rev:prog:progs) -> pure (B8.pack rev, (prog:progs))
  _ -> die "usage: set-git-rev REV PROG [PROGS...]" >> exitFailure
