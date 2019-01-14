;;This will take in a Permutation and state,
;;It will be built to deal with permutations over the top of it.

; Permutator
;===============================
; v(0.0l)

(ns ^{:doc "A matcher esc System to complete make moves on a puzzle
            using maps to tap through permutations"
      :author "Team 7"}
  Team7.Rubik.permutate)

(require '[cgsx.tools.matcher :refer :all])
(require '[clojure.pprint :refer :all])

(Declare end-State)

;;Takes in one state and one permutation and applies it.
;;Must be careful where a permutation has to be applied to all the whole world state
(defn permutate [state permutation]     
    (let [guard (get permutation :guard)
        moves (get permutation :moves)]  
        (if  (some #(= guard %) state)
            (for [x state
                :let [y (mfind [['?x x] moves] (? x))]
            ] y)
        )     
    )  
)        

;;Does the same as above but does the permutation in reverse
(defn reversePermutate [state permutation] 
    (let [guard (get permutation :guard)
        moves (get permutation :moves)]      
        (if  (some #(= guard %) state)
            (for [x state
                :let [y (mfind [[x '?x] moves] (? x))]
            ] y)
        ) 
    )      
) 


; ;;Takes in a world state and applies a permutation to each 
; (defn ApplyPermute [World permutation]
; (let [guard (get permutation :guard)
;         moves (get permutation :moves)]                           ;; to do a large world builder that applies to all states
; )
; )
(def permutations
    {
          :F {  :guard 'front
                :moves '([front front]
                      [up right]
                      [right down]
                      [down left]
                      [left up]
                )          
          }

          :U {  :guard 'up
                :moves '([front left]
                      [left back]
                      [up up]
                      [right front]
                      [back right]
                )
          }

          :R {  :guard 'right
                :moves '([front up]
                      [up back]
                      [right right]
                      [back down]
                      [down front]
                )           
          }

          :B { :guard 'back
          :moves '([up left]
                      [left down]
                      [right up]
                      [down right]
                      [back back]
                )
          }  

          :L {  :guard 'left
                :moves '([front down]
                      [up front]
                      [back up]
                      [down back]
                      [left left]
                )
          }

          :D {  :guard 'down
                :moves '([front right]
                      [back left]
                      [right back]
                      [down down]
                      [left front]
                )
          }
          :RF {  :guard 'front
          :moves '([front front]
                [up right]
                [right down]
                [down left]
                [left up]
          )          
    }

    :RU {  :guard 'up
          :moves '([front left]
                [left back]
                [up up]
                [right front]
                [back right]
          )
    }

    :RR {  :guard 'right
          :moves '([front up]
                [up back]
                [right right]
                [back down]
                [down front]
          )           
    }

    :RB { :guard 'back
    :moves '([up left]
                [left down]
                [right up]
                [down right]
                [back back]
          )
    }  

    :RL {  :guard 'left
          :moves '([front down]
                [up front]
                [back up]
                [down back]
                [left left]
          )
    }

    :RD {  :guard 'down
          :moves '([front right]
                [back left]
                [right back]
                [down down]
                [left front]
          )
    }
    }
)

