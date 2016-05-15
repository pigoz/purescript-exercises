module Test.Main where

import Prelude
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as A

import Main (isEven, countEven, squares, filterNegative, cartesian, triples)

main = runTest do
  suite "4.4 - Recursion on Arrays" do
    test "isEven works" do
      A.assert "2 -> true" $ isEven 2 == true
      A.assert "1 -> true" $ isEven 1 == false
      A.assert "5 -> false" $ isEven 5 == false

    test "countEven works" do
      A.equal 2 $ countEven [1,2,3,4,5]
      A.equal 3 $ countEven [1,2,3,4,5,6]

  suite "4.9 – Filtering Arrays" do
    test "squares" do
      A.equal [4, 16, 64] $ squares [2, 4, 8]

    test "filterNegative" do
      A.equal [2, 3] $ filterNegative [2, -3, 3]

  suite "4.11 – Guards" do
    test "cartesian" do
      A.equal [[1,3], [1,4], [2, 3], [2, 4]] $ cartesian [1, 2] [3, 4]

    test "pitagorean triples" do
      A.equal [] $ triples 1
      A.equal [[3,4,5], [4, 3, 5]] $ triples 5
