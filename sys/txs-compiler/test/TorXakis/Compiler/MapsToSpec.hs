{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module TorXakis.Compiler.MapsToSpec where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Test.Hspec (Spec, it, pending, shouldBe)

import TorXakis.Compiler.Error

import TorXakis.Compiler.MapsTo

data Fruit = Orange | Pear | Apple deriving (Show, Eq)
data Vegetable = Cucumber | Carrot | Spinach deriving (Show, Eq)
data Legume = Lentils | Chickpeas | BlackEyedPeas deriving (Show, Eq)

fruitNames :: Map String Fruit
fruitNames = [("Orange", Orange), ("Pear", Pear), ("Apple", Apple)]

fruitNumbers :: Map Int Fruit
fruitNumbers = [(0, Orange), (1, Pear), (2, Apple)]

fruitWithName :: MapsTo String Fruit m => String -> m -> Either Error Fruit
fruitWithName n m =
    m .? n

spec :: Spec
spec = do
    it "It gets the right fruit in a map" $
       let Right res = fruitWithName "Orange" fruitNames in
           res `shouldBe` Orange
    it "It gets the right fruit in a composite map" $
       let Right res = fruitWithName "Orange" (fruitNames :& fruitNumbers) in
           res `shouldBe` Orange           
    it "It gets the right fruit in a composite map (second variant)" $
       let Right res = fruitWithName "Orange" (fruitNumbers :& fruitNames) in
           res `shouldBe` Orange
    -- Uncomment these to test for the type errors of the compiler:
    --
    -- it "Fails when no map is found)" $
    --    let Right res = fruitWithName "Orange" (fruitNumbers :& "Not here either") in
    --        res `shouldBe` Orange                      
    -- it "It gets the right fruit in a composite map (third variant)" $
    --    let Right res = fruitWithName "Orange" (fruitNumbers :& fruitNames :& fruitNumbers :& fruitNames) in
    --        res `shouldBe` Orange
    --
    -- We could test this by calling the ghc compiler, and checking the error message.

           
       
