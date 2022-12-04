(ns game.core)

;; movement could be a vector
(def direction-left [-1 0])
(def direction-right [1 0])
(def direction-up [0 -1])
(def direction-down [0 1])

(defn
  position-at
  "returns position at given direction"
  [direction position]
  (map #(+ %1 %2) direction position))

(defn 
  give-left
  "returns a position from left side of the given position"
  ([[x y]]
   [(dec x) y])
  ([[x y] diagonal?]
   (if diagonal?
     [(dec x) (dec y)]
     (give-left [x y]))))

(defn 
  give-right
  "returns a position from right side of the given position"
  ([[x y]]
   [(inc x) y])
  ([[x y] diagonal?]
   (if diagonal?
     [(inc x) (inc y)]
     (give-right [x y]))))

;; 1 2 3 4 5 6 7 8 9 10
;; . . . X . . . . . . 
;; 
;; lower and upper bound
;; distance from position

;; find how many pieces on right (and left and diagonal)
;; check if own piece on position



(defn 
  is-there-piece
  "checks if there is a piece in given direction"
  [piece position direction board]
  (let [checked-position (position-at position direction)]
    (= piece (board checked-position))))

(defn
  collect-to-direction
  "creates a coll of given pieces in given direction"
  [piece position direction board]
  (loop [result []
         current-position position
         next-position (position-at direction current-position)]
    (let [found (is-there-piece piece current-position direction board)]
      (if (not found)
        result
        (recur 
          (conj result next-position) 
          next-position 
          (position-at direction next-position))))))

;;  . . . . .
;;  . . X . .
;;  . . X . .
;;  . X . X .
;;  . . . . .

(defn 
  value-of 
  "calculates the value of position for given player"
  [piece position board]
  (if (not (nil? (board position))) 
    0
    (let [directions [direction-down direction-up direction-left direction-right]]
    (reduce +
     (map 
        (fn [direction] (count (collect-to-direction piece position direction board))) 
        directions)))))

(defn
  valid-move
  "boolean whether the position is valid"
  [board-size [x y]]
  (every? true? (map 
                  (fn [fun] (and (fun x) (fun y)))
                  [
                   (fn [v] (not (neg? v)))
                   (fn [v] (<= v board-size))
                   ])))

(defn
  winner?
  "returns boolean whether a move at the given position is winner"
  [winner-length piece position board]
  (let [directions [[direction-down direction-up] [direction-left direction-right]]]
    (some 
      true? 
      (map 
        (fn [[left right]]
          (= 
            (dec winner-length) 
            (reduce 
              +
              (map count [
                          (collect-to-direction piece position left board) 
                          (collect-to-direction piece position right board) 
                          ])))) 
        directions))))

(defn
  scan-board-for-empty-positions
  "scans empty positions"
  [board-size board]
    (for [x (range (inc board-size)) y (range (inc board-size)) :when (nil? (board [x y]))] 
      [x y]))

(defn
  value-positions
  "calculate given positions"
  [piece board free-positions-coll]
  (map 
    (fn [position] [(value-of piece position board) position]) 
    free-positions-coll))

(comment
  (value-positions :x {} [[0 0] [1 0]]) ; ([0 [0 0]] [0 [1 0]])
  (sort-by first [[1 [0 0]] [0 [1 0]]]) ; ([0 [1 0]] [1 [0 0]])
  (first (rest (first (take 1 (sort-by first [[1 [0 0]] [0 [1 0]]]))))) 
  ; ([0 [1 0]] [1 [0 0]])
  ; [1 0]
  )

(defn next-player [piece] ({:x :o, :o :x} piece))

(defn
  select-move
  "returns a new position for given player"
  [board-size piece board]
  (let [
        possible-positions (scan-board-for-empty-positions board-size board)
        valued-positions (value-positions piece board possible-positions)
        sorted-positions (sort-by first valued-positions)
        ]
    (first (rest (first (take 1 sorted-positions))))))

(defn
  move
  [state]
  (let [
        size (state :board-size)
        winner-size (state :winner-size)
        board (state :board)
        player (state :turn)
        new-move (select-move size player board)
        winner (winner? size player new-move board)
        new-board (assoc board new-move player)
        new-turn (next-player player)
        ]
    {:winner (if winner player nil)
     :turn new-turn
     :board new-board
     :winner-size winner-size 
     :board-size size
     }))

(comment
  (def juu (partial (comp nil? :winner)))
  (juu { :winner :x })
  (juu { :winner true })
  (juu { :winner false })
  (juu { :winner nil })

  (move {:turn :x, :board-size 2, :board {}}) ; {:winner nil, :turn :o, :board {[0 0] :x}, :board-size 2}
  )


;;
;; state:
;; { :winner nil,  :turn :x :board {} :board-size 2 }
;;

(defn
  game-loop
  "playes the game until draw or win"
  [board winner-size board-size]
  (take-while 
    (partial (comp nil? :winner)) 
    (iterate move {:turn :x 
                   :winner-size winner-size 
                   :board-size board-size 
                   :board board})))

(comment
  (take 4 (iterate move {:turn :x :board-size 2 :board {}}))
  )

(defn
  game
  []
  (let [
        winner-size 2 
        board-size 2 
        board {}
        ]
    (game-loop board winner-size board-size)))

(comment
  (take (* 3 3) (game))
  )

