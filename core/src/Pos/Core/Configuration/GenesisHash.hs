{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisHash
       ( HasGenesisHash
       , withGenesisHash
       , GenesisHash (..)
       , getGenesisHeaderHash
       , genesisHash
       , genesisHeaderHash
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Reflection (Given (..), give)

import           Pos.Binary.Class (Raw)
import           Pos.Core.Block.Union.Types (HeaderHash, anyHeaderHash)
import           Pos.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }

getGenesisHeaderHash :: GenesisHash -> HeaderHash
getGenesisHeaderHash = anyHeaderHash . getGenesisHash

type HasGenesisHash = Given GenesisHash

withGenesisHash :: (Hash Raw) -> (HasGenesisHash => r) -> r
withGenesisHash gh = give (GenesisHash (coerce gh))

genesisHash :: HasGenesisHash => Hash a
genesisHash = getGenesisHash given

genesisHeaderHash :: HasGenesisHash => HeaderHash
genesisHeaderHash = anyHeaderHash $ getGenesisHash given
