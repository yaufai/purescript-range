module Test.Data.Range where

import Prelude

import Data.Range (RecordRange, elem, equivalent, include, intersect, isValid, mkRecordRange, overlap)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)


leftOpenNumberRange  = mkRecordRange (-1.0) true  3.5 false
rightOpenNumberRange = mkRecordRange (-2.5) true  3.0 false
openNumberRange      = mkRecordRange (-5.9) true  0.0 true
closedNumberRange    = mkRecordRange (-0.9) false 2.0 false

invalidNumberRange   = mkRecordRange 0.3 false (-2.0) false

pointOpenNumberRange      = mkRecordRange 0.0 true  0.0 true
pointClosedNumberRange    = mkRecordRange 0.0 false 0.0 true
pointLeftOpenNumberRange  = mkRecordRange 0.0 true  0.0 false
pointRightOpenNumberRange = mkRecordRange 0.0 false 0.0 true


specIsValid =
  describe "isValid" do
    it "positive" do
      isValid leftOpenNumberRange `shouldEqual` true
    it "right open" do
      isValid invalidNumberRange `shouldEqual` false
    it "open point range" do
      isValid pointOpenNumberRange `shouldEqual` true
    it "left point range" do
      isValid pointClosedNumberRange `shouldEqual` true
    it "right point range" do
      isValid pointLeftOpenNumberRange `shouldEqual` true
    it "clsed point range" do
      isValid pointRightOpenNumberRange `shouldEqual` true

specElem =
  describe "elem" do
    describe "range with one side open" do
      it "middle of the range" do
        (0.0 `elem` leftOpenNumberRange) `shouldEqual` true
      it "open edge" do
        ((-1.0) `elem` leftOpenNumberRange) `shouldEqual` false
      it "closed edge" do
        (3.5 `elem` leftOpenNumberRange) `shouldEqual` true
      it "smaller than its lower bound" do
        ((-2.0) `elem` leftOpenNumberRange) `shouldEqual` false
      it "larger than its upper bound" do
        (4.0 `elem` leftOpenNumberRange) `shouldEqual` false
    it "open point range" do
      (0.0 `elem` pointOpenNumberRange) `shouldEqual` false
    it "left point range" do
      (0.0 `elem` pointClosedNumberRange) `shouldEqual` true
    it "right point range" do
      (0.0 `elem` pointLeftOpenNumberRange) `shouldEqual` true
    it "clsed point range" do
      (0.0 `elem` pointRightOpenNumberRange) `shouldEqual` true

range01 :: RecordRange Number
range01 = mkRecordRange 0.0 false 1.0 true

specOverlap =
  describe "overlap" do
    describe "simple cases" do
      it "not overlaps from left" do
        let farLeft = mkRecordRange (-2.0) false (-1.0) true
        (farLeft `overlap` range01) `shouldEqual` false
      it "overlap left" do
        let leftSide = mkRecordRange (-2.0) false 0.5 true
        (leftSide `overlap` range01) `shouldEqual` true
      it "not overlaps from right" do
        let farRight = mkRecordRange (1.5) false (2.0) true
        (farRight `overlap` range01) `shouldEqual` false
      it "overlap left" do
        let rightSide = mkRecordRange 0.5 false 1.5 true
        (rightSide `overlap` range01) `shouldEqual` true

specInclude =
  describe "include" do
    it "positive" do
      let cover = mkRecordRange (-2.0) false (2.0) true
      (cover `include` range01) `shouldEqual` true
    it "negative" do
      let cover = mkRecordRange (-2.0) false (0.5) true
      (cover `include` range01) `shouldEqual` false

specIntersect =
  describe "intersect" do
    it "identity" do
      let result = range01 `intersect` range01
      let equivalence = result `equivalent` range01
      equivalence `shouldEqual` true
    it "edge openness" do
      let closed = mkRecordRange 0.0 false 1.0 false
      let open   = mkRecordRange 0.0 true  1.0 true
      let result = closed `intersect` open
      let equivalence = result `equivalent` open
      equivalence `shouldEqual` true
