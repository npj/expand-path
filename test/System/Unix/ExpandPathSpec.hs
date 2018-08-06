{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Unix.ExpandPathSpec where

import Test.Tasty.Hspec (Spec, it, shouldBe)

import System.Unix.ExpandPath.Monad
import Data.Functor.Identity (Identity (runIdentity))

newtype TestM a = TestM { runTestM :: Identity a }
  deriving (Functor, Applicative, Monad)

instance ExpandPathM TestM where
  homeDir = return "/homeDir"
  userHomeDir user = return $ "/home/" ++ user
  currentDir = return "/currentDir"

runTest :: TestM a -> a
runTest = runIdentity . runTestM

expandPath :: FilePath -> FilePath
expandPath = runTest . expandPathM

spec_expandPath :: Spec
spec_expandPath = do
  it "expands the current home directory" $ do
    expandPath "~" `shouldBe` "/homeDir"
    expandPath "~/" `shouldBe` "/homeDir"
    expandPath "~/Downloads" `shouldBe` "/homeDir/Downloads"

  it "expands a user home directory" $ do
    expandPath "~foobar" `shouldBe` "/home/foobar"
    expandPath "~foobar/" `shouldBe` "/home/foobar"
    expandPath "~foobar/Downloads" `shouldBe` "/home/foobar/Downloads"

  it "expands a relative path" $ do
    expandPath "projects/haskell" `shouldBe` "/currentDir/projects/haskell"
    expandPath "projects/~/haskell" `shouldBe` "/currentDir/projects/~/haskell"
    expandPath "projects/~foobar/*" `shouldBe` "/currentDir/projects/~foobar/*"

  it "leaves an absolute path alone" $ do
    expandPath "/usr/local/bin" `shouldBe` "/usr/local/bin"
    expandPath "/usr/local/bin/../share" `shouldBe` "/usr/local/bin/../share"
    expandPath "/home/~foobar/Downloads" `shouldBe` "/home/~foobar/Downloads"
    expandPath "/~/Downloads" `shouldBe` "/~/Downloads"

  it "normalizes paths" $ do
    expandPath "~/./" `shouldBe` "/homeDir"
    expandPath "~/////" `shouldBe` "/homeDir"
    expandPath "~/./Downloads/../" `shouldBe` "/homeDir/Downloads/../"

    expandPath "/./" `shouldBe` "/"
    expandPath "/////" `shouldBe` "/"
    expandPath "/./Downloads/../" `shouldBe` "/Downloads/../"
