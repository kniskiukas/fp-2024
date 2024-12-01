{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 and Lib2 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    
    testCase "Parsing AddRequest" $
      Lib2.parseQuery "add_request 1,type,origin,item1,item2" @?= (Right (Lib2.AddRequest (Lib2.Request 1 "type" "origin" (Lib2.Items ["item1","item2"])))),

    testCase "Parsing ListRequests" $
      Lib2.parseQuery "list_requests" @?= (Right Lib2.ListRequests),
    
    testCase "Parsing RemoveRequest" $
      Lib2.parseQuery "remove_request 1" @?= (Right (Lib2.RemoveRequest 1)),
    
    testCase "Parsing UpdateRequest" $
      Lib2.parseQuery "update_request 1 1,type,origin,item1,item2" @?= (Right (Lib2.UpdateRequest 1 (Lib2.Request 1 "type" "origin" (Lib2.Items ["item1","item2"])))),
    
    testCase "Parsing FindRequest" $
      Lib2.parseQuery "find_request 1" @?= (Right (Lib2.FindRequest 1)),

    testCase "Parsing invalid command" $
      Lib2.parseQuery "invalid" @?= (Left "Unexpected command: invalid")
  ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 tests"

    [ testCase "Test single" $
        let s = Lib3.Single (Lib2.AddRequest (Lib2.Request 1 "type" "origin" (Lib2.Items ["item1","item2"]))) 
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),

      testCase "Test batch" $
        let s1 = Lib2.AddRequest (Lib2.Request 1 "type" "origin" (Lib2.Items ["item1","item2"]))
            s2 = Lib2.UpdateRequest 1 (Lib2.Request 1 "type" "origin" (Lib2.Items ["item1","item2"]))
            s3 = Lib2.RemoveRequest 1
            b = Lib3.Batch [s1, s2, s3]
         in Lib3.parseStatements (Lib3.renderStatements b) @?= Right (b, ""),

      QC.testProperty "rendered and parsed" $
        \s -> Lib3.parseStatements (Lib3.renderStatements s) == Right (s, "")
    ]