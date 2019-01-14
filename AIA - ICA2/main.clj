;;Starting state for the 2 x 2 cube
(def goalState
    '#{
          (cube c1 (front down left))
          (cube c2 (back up right))
          (cube c3 (front down right))
          (cube c4 (back up left))

          (cube c5 (back down right))
          (cube c6 (front up  right))
          (cube c7 (back down left))
          (cube c8 (front up left))
          }
)


;;Map to define targets states based on movement type
(def groups '{
    :U up
    :L left
    :F front   
    :R right   
    :B back    
    :D down  
    :RU up
    :RL left
    :RF front   
    :RR right   
    :RB back    
    :RD down          
})
    
(def group-ops '{
    :U up
    :L left
    :F front   
    :R right   
    :B back    
    :D down           
})
    
;;A collection of the legal transformations that can be performed
(def Moves '[ F U D L R B RF RU RD RL RR RB ])
    
;;Function which generates a random transformation, from list valid ones
(defn Random-Move []
(first (repeatedly 1 #(rand-nth Moves))))

;;A function which applies the correct permutation rule to the state and returns the result
(defn apply-perm [state move]
    (if (contains? group-ops move)
      (permutate state (get permutations move))
      (reversePermutate state (get permutations move))
    ))
    
;;A Function which takes a starting state, applies a transformation to it and returns the resulting state
(defn apply-move
([state move]
(apply-move state 0 () move))
([state cnt perm move]
(if (< cnt 4)
(let [
        target-state (first (filter #(some (partial = (get groups move)) (last %)) state))
        next-pos (apply-perm (last target-state) move)
        tuple-body (butlast target-state)
        new-state (concat tuple-body (list next-pos))
      ]
    (apply-move (disj state target-state) (inc cnt)  (conj perm new-state) move))
    (clojure.set/union state (set perm))
    )))
    
;;Function which will scramble the cube by performing x number of random transformations
(defn Scrambler
    ([state moves]
    (Scrambler state moves 0))
    ([state moves cnt]
    (if (< cnt moves)
    (let [Move (keyword (Random-Move))]
      (println (str "--Transformation: " Move))
      (println (str "--State: " state))
       (recur (apply-move state Move) moves (inc cnt) ))
       state
       ))
)

;;Generates a per runtime startState, x moves away from the goal
(defn startStateGenerator [x] (Scrambler goalState x))
(def startState (startStateGenerator 5))

;; Randomly applies moves looking for the goal state, reaching a goal is pure luck
(defn Random-Search
([state startState goalState]
(if (empty? (clojure.set/difference state goalState))
state
(let [Move (keyword (Random-Move))]
   (println (str "--Transformation: " Move))
   (println (str "--State: " state))
   (recur (apply-move state Move) startState goalState)
   ))))


;;legal move generator 
(defn lmg [state]
(list (apply-move state :F) (apply-move state :U) (apply-move state :R) (apply-move state :B) (apply-move state :L) (apply-move state :D)             
(apply-move state :RF) (apply-move state :RU) (apply-move state :RR) (apply-move state :RB) (apply-move state :RL) (apply-move state :RD)))

;;legal move generator with costing heuristic 
(defn a*lmg [state]
    (let [n (:state state)
          c (:cost state)
          ]
      (list
             {:state (apply-move n :F), :cost (+ c (count (clojure.set/difference (apply-move n :F) goalState))) }
             {:state (apply-move n :U), :cost (+ c (count (clojure.set/difference (apply-move n :U) goalState))) }
             {:state (apply-move n :B), :cost (+ c (count (clojure.set/difference (apply-move n :B) goalState))) }
             {:state (apply-move n :D), :cost (+ c (count (clojure.set/difference (apply-move n :D) goalState))) }
             {:state (apply-move n :L), :cost (+ c (count (clojure.set/difference (apply-move n :L) goalState))) }
             {:state (apply-move n :R), :cost (+ c (count (clojure.set/difference (apply-move n :R) goalState))) }
             {:state (apply-move n :RF), :cost (+ c (count (clojure.set/difference (apply-move n :RF) goalState))) }
             {:state (apply-move n :RU), :cost (+ c (count (clojure.set/difference (apply-move n :RU) goalState))) }
             {:state (apply-move n :RB), :cost (+ c (count (clojure.set/difference (apply-move n :RB) goalState))) }
             {:state (apply-move n :RD), :cost (+ c (count (clojure.set/difference (apply-move n :RD) goalState))) }
             {:state (apply-move n :RL), :cost (+ c (count (clojure.set/difference (apply-move n :RL) goalState))) }
             {:state (apply-move n :RR), :cost (+ c (count (clojure.set/difference (apply-move n :RR) goalState))) }
        )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Testing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Rescramble the cube at desired complexity 
(def startState (startStateGenerator 2))

;;A random sequence of operators applied to reach a goal state
(Random-Search startState  (Scrambler startState 1) goalState)

;;Run a bfs 
(breadth-search startState goalState lmg)

;;An Implementation of A* path costing works by using clojure.set/difference
(A*search {:state startState, :cost 0} goalState a*lmg))

;;Run too compare the run times of the too approaches
(defn Time-Runtime []
    (println "bfs: " (time (breadth-search startState goalState lmg)))
    (println "a*: " (time (A*search {:state startState, :cost 0} goalState a*lmg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Test cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-cases []
    (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RF) :F) startState)) "Test1 fail,  Reversing the move supplied a state different to the start state")
     (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RL) :L) startState)) "Test2 fail,  Reversing the move supplied a state different to the start state")
      (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RR) :R) startState)) "Test3 fail,  Reversing the move supplied a state different to the start state")
      (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RU) :U) startState)) "Test4 fail,  Reversing the move supplied a state different to the start state")
      (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RB) :B) startState)) "Test5 fail,  Reversing the move supplied a state different to the start state")
     (assert  (empty?  (clojure.set/difference (apply-move (apply-move startState :RD) :D) startState)) "Test6 fail,  Reversing the move supplied a state different to the start state")
)
