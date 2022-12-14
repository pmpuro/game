(ns game.core-test
  (:require [clojure.test :as t]
            [game.core :as i]))

(t/deftest test-position-at
  (t/is (= [0 2] (i/position-at [1 1] [-1 1]))))

(t/deftest test-is-there-piece
  (t/is (= true (i/is-there-piece :x [10 20] [1 0] { [11 20] :x })))
  (t/is (= false (i/is-there-piece :x [10 20] [1 0] { [11 20] :o })))
  (t/is (= false (i/is-there-piece :x [10 20] [1 0] { [1 2] :x })))
  )

(t/deftest test-collect-into-direction
  (t/is (= [] (i/collect-to-direction :x [0 0] [1 0] { [0 0] :x })))
  (t/is (= [[1 0]] (i/collect-to-direction :x [0 0] [1 0] { [0 0] :x, [1 0] :x })))
  (t/is (= [[1 0] [2 0]] (i/collect-to-direction :x [0 0] [1 0] { [0 0] :x, [1 0] :x, [2 0] :x })))
  )

(t/deftest test-opponent-value-of
  (t/is (= 2 (i/opponent-value-of :o [1 1] { [1 0] :x, [1 2] :x})))
  (t/is (= 2 (i/opponent-value-of :o [1 1] { [1 2] :x, [1 3] :x})))
  (t/is (= 2 (i/opponent-value-of :o [1 2] { [1 0] :x, [1 1] :x}))))

(t/deftest test-value-of
  (t/testing "value-of"
    (t/testing "x already in the pos"
      (t/is (= 0 (i/value-of :x [0 0] { [0 0] :x})))
      (t/is (= 0 (i/value-of :x [0 0] { [0 0] :x, [0 1] :x})))
      (t/is (= 0 (i/value-of :x [0 0] { [0 0] :x, [1 0] :x}))))
    (t/testing "other piece already in the pos"
      (t/is (= 0 (i/value-of :x [0 0] { [0 0] :y}))))
    (t/testing "empty board"
      (t/is (= 0 (i/value-of :x [0 0] {}))))
    (t/testing "y"
      (t/is (= 2 (i/value-of :x [1 1] { [1 0] :x, [1 2] :x})))
      (t/is (= 2 (i/value-of :x [1 1] { [1 2] :x, [1 3] :x})))
      (t/is (= 2 (i/value-of :x [1 2] { [1 0] :x, [1 1] :x}))))
    (t/testing "x"
      (t/is (= 2 (i/value-of :x [1 1] { [0 1] :x, [2 1] :x})))
      (t/is (= 2 (i/value-of :x [1 1] { [2 1] :x, [3 1] :x})))
      (t/is (= 2 (i/value-of :x [2 1] { [0 1] :x, [1 1] :x}))))
    (t/testing "diagonal down"
      (t/is (= 2 (i/value-of :x [1 1] { [0 0] :x, [2 2] :x}))))
    (t/testing "diagonal up"
      (t/is (= 2 (i/value-of :x [1 1] { [0 2] :x, [2 0] :x}))))))

(t/deftest test-winner?
  (t/testing "test-winner?"
    (let [length 3]
      (t/testing "looser"
        (t/is (not (i/winner? length :x [0 0] {[1 0] :x}))))

      (t/testing "on x axis"
        (t/is (i/winner? length :x [0 0] {[1 0] :x, [2 0] :x}))
        (t/is (i/winner? length :x [2 0] {[1 0] :x, [3 0] :x})))

      (t/testing "on y axis"
        (t/is (i/winner? length :x [0 0] {[0 1] :x, [0 2] :x}))
        (t/is (i/winner? length :x [0 2] {[0 1] :x, [0 3] :x})))

      (t/testing "on diagonal down"
        (t/is (i/winner? length :x [0 0] {[1 1] :x, [2 2] :x})))

      (t/testing "on diagonal up"
        (t/is (i/winner? length :x [0 2] {[1 1] :x, [2 0] :x}))))))

(t/deftest test-winner-found?
  (t/is (i/winner-found? {:winner :x}))
  (t/is (i/winner-found? {:winner :o}))
  (t/is (not (i/winner-found? {})))
  (t/is (not (i/winner-found? {:winner false})))
  (t/is (not (i/winner-found? {:winner false})))
  (t/is (not (i/winner-found? {:winner true}))))

(t/deftest test-scan-board-for-empty-positions
  (let [size 2]
    (t/is (= 
            [[1 0] [2 2]] 
            (i/scan-board-for-empty-positions size { 
                                                    [0 0] :x,           [2 0] :x 
                                                    [0 1] :x, [1 1] :x, [2 1] :x 
                                                    [0 2] :x, [1 2] :x
                                                    })))))

(t/deftest test-position-value-of
  (let [
        board-size 2
        expected-value 1
        ]
    (t/is (= 0 (i/position-value-of :x [0 1] {})))
    (t/is (= 0 (i/position-value-of :x [1 0] {})))
    (t/is (= 0 (i/position-value-of :x [0 board-size] {})))
    (t/is (= 0 (i/position-value-of :x [board-size 0] {})))
    (t/is (= expected-value (i/position-value-of :x [1 1] {})))
    (t/is (= 0 (i/position-value-of :x [1 1] { [1 1] :x})))))

