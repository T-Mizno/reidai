;;
;; Gaussの消去法
;;   水野 (of.mizno@gmail.com)
;;   2013/10/10
;;   2013/10/16
;;     gauss の forward のアルゴリズムを整理した。
;;     P の連想リストを更新していなかったので、Pを更新する関数全部で、新しいPを生成して返すようにした。
;;      (clojure の連想リストは immutable)
;;     powerMethod 固有値を計算する方法を変更: ベクトルの和を除算する
;;   2013/10/23
;;     テスト用の行列を生成する関数を作成
;;   2013/11/12
;;     名前空間 myzLinear に移動
;;   2013/11/13
;;     at と setVal の実装に型ヒント(^doubles ^objects)をつけて高速化(約10倍)
;;


;;
;; mkdir classes
;; java -cp  .\;... clojure.main  (Windows)
;; user=> (compile 'myzLinear)
;;         classes\ 以下に classファイルが作成される。
;;         利用側は (require 'myzLinear),  (myzLinear/... )

(ns myzLinear)

(def NumType Double/TYPE)
(def ^:const NumDefault 0.0)
(def ^:const NumFormat "%5.2f")

(def ^:const epsilon 1e-5)

(defn near? [x y] (< (Math/abs (- x y)) epsilon))
(defn isZero? [x] (near? x 0.0))

(defn hasElements? [ls] (not (or (nil? ls) (empty? ls))))
(defn notNil? [v]  (if (= nil v) false true))
(defn upto [start end]
  (if (> start end)
    '()
    (lazy-seq (range start (+ end 1)))))
(defn downto [start end]
  (if (< start end)
    '()
    (reverse (range end (+ start 1)))))
(defn in?
  ([v xs] (in? v xs =))
  ([v xs f2] (reduce (fn [acc x] (or acc (f2 v x))) false xs)))

(defrecord Ids [ids is js iStart iEnd jStart jEnd])
(defn newIds
  ([ids] 
     (let [is (sort (distinct (map #(first %) ids)))
           js (sort (distinct (map #(second %) ids)))]
       (->Ids ids is js (first is) (last is) (first js) (last js))))       
  ([m n] (newIds 0 (- m 1) 0 (- n 1)))
  ([iStart iEnd jStart jEnd] 
     (let [is (upto iStart iEnd)
           js (upto jStart jEnd)
           ids (for [i is, j js] (list i j))]
       (->Ids ids is js iStart iEnd jStart jEnd))))

(defrecord Matrix [ids vals])

(defn ids "list of indices" [matrix] (:ids (:ids matrix)))
(defn rowIds "list of 0..m" [matrix] (:is (:ids matrix)))
(defn colIds "list of 0..n" [matrix] (:js (:ids matrix)))

(defn id-i "(id@(i,j) -> i" [id] (first id))
(defn id-j "(id@(i,j) -> j" [id] (second id))

(defn m [matrix] (+ (- (:iEnd (:ids matrix)) (:iStart (:ids matrix))) 1))
(defn n [matrix] (+ (- (:jEnd (:ids matrix)) (:jStart (:ids matrix))) 1))

(defn id-sid 
  ([ids i j]
     (+ (* (- i (:iStart ids)) (+ (- (:jEnd ids) (:jStart ids)) 1)) (- j (:jStart ids))))
  ([ids id]
     (id-sid ids (id-i id) (id-j id))))
(defn sid-id [mat sid]
  (let [i (+ (quot sid (n mat)) (:iStart (:ids mat)))
        j (+ (rem sid (n mat))  (:jStart (:ids mat)))
        ]
    (list i j)))

(defn idInRange? 
 ([matrix i j]
    (let [ids (:ids matrix)]
      (and (<= (:iStart ids) i) (<= i (:iEnd ids)) (<= (:jStart ids) j) (<= j (:jEnd ids)))))
 ([matrix id] (idInRange? matrix (id-i id) (id-j id))))

(defn i-zi [matrix zi] (- zi (:iStart (:ids matrix))))
(defn j-zj [matrix zj] (- zj (:jStart (:ids matrix))))

(defn setVal "set val to ij"
  ([matrix i j v] 
    (if (idInRange? matrix i j)
      (let [^doubles arr (aget ^objects (:vals matrix) (i-zi matrix i))] (aset arr (j-zj matrix j) ^double v))))
  ([matrix id v] (setVal matrix (id-i id) (id-j id) v)))

(defn at
  ([mat i j] 
     (let [^doubles arr (aget ^objects (:vals mat) (i-zi mat i))] (aget arr (j-zj mat j))))
  ([mat id]  (at mat (id-i id) (id-j id))))

(defn fillList [mat avs]
  (loop [ijs (ids mat), vs avs]
    (when (and (hasElements? ijs) (hasElements? vs))
      (setVal mat (first ijs) (first vs))
      (recur (next ijs) (next vs))))
  mat)

(defn fill [matrix v] (fillList matrix (repeat (count (ids matrix)) v)))

(defn newMatrix 
  ([ids] (->Matrix ids (make-array NumType (count (:is ids)) (count (:js ids)))))
  ([m n] (newMatrix (newIds m n)))
  ([m n vals]  (fillList (newMatrix m n) vals))
  ([iStart iEnd jStart jEnd] (newMatrix (newIds iStart iEnd jStart jEnd)))
)

(defn listToMatrix 
  " make from list ( ((i j) v)* ) to matrix. "
  ([ivs]
     (let [iStart (apply min (map #(first (first %)) ivs))
           iEnd (apply max (map #(first (first %)) ivs))
           jStart (apply min (map #(second (first %)) ivs))
           jEnd (apply max (map #(second (first %)) ivs))]
       (listToMatrix  (newMatrix iStart iEnd jStart jEnd) ivs)))
  ([mat aivs]
     (loop [ivs aivs]
       (when (hasElements? ivs)
         (let [iv (first ivs)]
           (setVal mat (first iv) (second iv)))
         (recur (next ivs))))
       mat))

; express permutation matrix
(defn newP [matrix] (reduce (fn [acc i] (assoc acc i i)) {} (rowIds matrix)))
(defn swapP [P i j]
  (let [oldPi (P i), oldPj (P j)] 
    (assoc P i oldPj  j oldPi)
    ))

(defn rowVals "list of i-th row" [matrix i] (map #(at matrix i %) (colIds matrix)))
(defn colVals "list of j-th col" [matrix j] (map #(at matrix % j) (rowIds matrix)))

(defn stdout 
  ([P matrix]
     (print (reduce (fn [s1 s2] (str s1 " " (format "%5d" s2))) "     " (colIds matrix)))
     (println
      (reduce (fn [ss1 ss2] (str ss1 "\n" ss2) ) "" (map (fn [i] (reduce (fn [s1 s2] (str s1 " " s2)) (format "%4d:" (P i)) (map #(format NumFormat %) (rowVals matrix (P i))))) (rowIds matrix)))))
  ([matrix]
     (let [p (newP matrix)]
       (stdout p matrix)))
  )

(defn matMap [f fromMat]
  (listToMatrix (for [id (ids fromMat)]
                (list id (f (at fromMat id))))))

(defn copy [fromMat] (matMap (fn [x] x) fromMat))
(defn copyColumn [mat j]
  (fillList (newMatrix (:iStart (:ids mat)) (:iEnd (:ids mat)) 0 0)
            (colVals mat j)))
(defn copyRow [mat i]
  (fillList (newMatrix 0 0 (:jStart (:ids mat)) (:jEnd (:ids mat)))
            (rowVals mat i)))

(defn average [vs] (/ (apply + vs) (count vs)))
(defn geomMean [vs] (Math/pow (apply * vs) (/ 1.0 (count vs))))

(defn averageColumn [mat]
  (fillList (newMatrix (:iStart (:ids mat)) (:iEnd (:ids mat)) 0 0)
            (map #(average (rowVals mat %)) (rowIds mat))))
(defn geomMeanColumn [mat]
  (fillList (newMatrix (:iStart (:ids mat)) (:iEnd (:ids mat)) 0 0)
            (map #(geomMean (rowVals mat %)) (rowIds mat))))
(defn averageRow [mat]
  (fillList (newMatrix 0 0 (:jStart (:ids mat)) (:jEnd (:ids mat)))
            (map #(average (colVals mat %)) (colIds mat))))
(defn geomMeanRow [mat]
  (fillList (newMatrix 0 0 (:jStart (:ids mat)) (:jEnd (:ids mat)))
            (map #(geomMean (colVals mat %)) (colIds mat))))

(defn multiScalar [mat v] (matMap (fn [x] (* x v)) mat))
(defn divScalar [mat v] (matMap (fn [x] (/ x v)) mat))
(defn addScalar [mat v] (matMap (fn [x] (+ x v)) mat))
(defn subScalar [mat v] (matMap (fn [x] (- x v)) mat))

(defn matMapWithId [f2 fromMat]
  (listToMatrix (for [id (ids fromMat)]
                (list id (f2 id (at fromMat id))))))

(defn add [mat1 mat2] (matMapWithId (fn [id v] (if (idInRange? mat2 id) (+ v (at mat2 id)) v)) mat1))
(defn sub [mat1 mat2] (matMapWithId (fn [id v] (if (idInRange? mat2 id) (- v (at mat2 id)) v)) mat1))

(defn divColumn [mat j d]
  (matMapWithId (fn [id v] (if (and (in? j (colIds mat)) (= j (id-j id))) (/ v d) v)) mat))
(defn divRow [mat i d]
  (matMapWithId (fn [id v] (if (and (in? i (rowIds mat)) (= i (id-i id))) (/ v d) v)) mat))
(defn norm1OfColumn [mat j] (reduce (fn [acc v] (+ acc (Math/abs v))) 0 (colVals mat j)))
(defn sumColumn [mat j] (norm1OfColumn mat j))
(defn sumRow [mat i] (reduce (fn [acc v] (+ acc v)) 0 (rowVals mat i)))
(defn normalizeColumn [mat j]
  (divColumn mat j (sumColumn mat j)))
(defn normalizeRow [mat i]
  (divRow mat i (sumRow mat i)))


(defn transpose [mat]
  (listToMatrix (for [id (ids mat)]
                (list (list (second id) (first id)) (at mat (first id) (second id))))))

(defn multi [mat1 mat2]
  (if (not (= (colIds mat1) (rowIds mat2)))
      nil
      (listToMatrix (for [i (rowIds mat1), j (colIds mat2)]
                      (list (list i j)
                            (apply + (map #(* (at mat1 i %) (at mat2 % j)) (colIds mat1))))))))

(defn matDiff [mat1 mat2]
  (/
  (reduce (fn [acc id] (if (idInRange? mat2 id) (+ acc (Math/abs (- (at mat1 id) (at mat2 id)))) (+ acc 0) )) 0.0 (ids mat1))
  (* (m mat1) (n mat1))
  ))
(defn matEq? [mat1 mat2] (isZero? (matDiff mat1 mat2)))

(defn sumAllElements [mat]
  (reduce (fn [acc id] (+ acc (at mat id))) 0.0 (ids mat)))

(defn powerMethod 
  ([mat itrMax]
     (loop [x (averageColumn mat), itr 0]
       (let [newX (multi mat x)]
         (let [new-x (normalizeColumn newX 0)
               error (matDiff new-x x)]
           (if (or (matEq? x new-x) (> itr itrMax))
             {:itr itr
              :eigenValue (/ (sumColumn newX 0) (sumColumn x 0))
              :eigenVector x
              :error error}
             (recur  new-x (inc itr)))))))
  ([mat] (powerMethod mat 10)))

(defn searchMaxI [P A pi pj]
  (let [is (upto pi (:iEnd (:ids A)))]
    (loop [max-i (first is), max-v (at A (P (first is)) pj), ais is]
      (if (hasElements? ais)
        (if (> (Math/abs (at A (P (first ais)) pj)) (Math/abs max-v))
          (recur (first ais) (at A (P (first ais)) pj) (next ais))
          (recur max-i max-v (next ais)))
        max-i)
      )
    ))
(defn updateP [P A pi pj] (swapP P pi (searchMaxI P A pi pj)))

(defn underColumnsAreZero? [P A pi pj]
  (reduce (fn [acc i] (and acc (isZero? (at A (P i) pj)))) true (upto pi (:iEnd (:ids A)))))

(defn searchPivotj [aoldP aA api apj]
  (loop [oldP aoldP, A aA, pi api, pj apj]
    (if (not (idInRange? A pi pj))
      false
      (do
        (let [P (updateP oldP A pi pj)]
          (if (underColumnsAreZero? P A pi pj)
            (recur P A pi (+ pj 1))
            (list P pj)))))))

(defn forwardOneLine [P A b pi pj i]
  (when (not (isZero? (at A (P pi) pj)))
    (let [pvi (P pi)
          pci (P i)
          pvv (/ (at A pci pj) (at A pvi pj)) ]
;      (listToMatrix A  (for [j (upto (+ pj 1) (:jEnd (:ids A)))]
;                         (list (list pci j)
;                               (- (at A pci j) (* pvv (at A pvi j))))))
      (loop [j (+ pj 1)]
        (when (<= j (:jEnd (:ids A)))
          (setVal A  pci j (- (at A pci j) (* pvv (at A pvi j))))
          (recur (inc j))))
      (setVal b pci 0 (- (at b pci 0) (* pvv (at b pvi 0))))
      (setVal A pci pj pvv)
      )
    )
  )

(defn forwardRows [P A b pi pj]
  (loop [is (upto (+ pi 1) (:iEnd (:ids A)))]
    (when (hasElements? is)
      (forwardOneLine P A b pi pj (first is))
      (recur (next is)))))

(defn forward [aoldP aA ab api aj apivots]
  (loop [oldP aoldP, A aA, b ab, pi api, j aj, pivots apivots]
    (if (not (idInRange? A pi j))
      (list oldP pivots)
      (let [Ppj (searchPivotj oldP A pi j)]
        (if (not Ppj)
          (list oldP pivots)
          (do
            (let [P (first Ppj)
                  pj (second Ppj)]
              (forwardRows P A b pi pj)
              (recur P A b (+ pi 1) (+ pj 1) (cons (list pi pj) pivots)))))))))


(defn solvable? [P b pivots]
  (every? #(isZero? (at b (P %) 0))
          (filter #(not (in? % (map (fn [pv]  (first pv)) pivots))) (rowIds b))
          )
  )

(defn pivot-jToi [j pivots]
  (if (hasElements? pivots)
    (let [pv (first pivots)]
      (if (= j (second pv)) 
        (first pv)
        (pivot-jToi j (next pivots))))
    nil
    ))

(defn jInPivots? [j pivots]
  (reduce (fn [acc pv] (or acc (= (second pv) j))) false pivots))

(defn freeVarJs [A pivots]
  (filter #(not (jInPivots? % pivots)) (colIds A))
)

(defn baseColIds [pivots] (map #(id-j %) pivots))

(defn backwardOneLine [P A b i j x]
  (let [pci (P i)]
    (setVal x j 0
            (/
             (- (at b pci 0)
                (apply + (map #(* (at A pci %) (at x % 0)) (upto (+ j 1) (:jEnd (:ids A))))))
             (at A pci j))
            )))

(defn backward [P A b pivots x]
  (loop [js (reverse (sort (baseColIds pivots)))]
    (when (hasElements? js)
      (backwardOneLine P A b (pivot-jToi (first js) pivots) (first js) x)
      (recur (next js)))))

(defn backwardGaussFree [P A b pivots fJs]
     (if (>= (count fJs) 1)
       (cons
        (let [x (transpose (averageRow A))]
          (fill x 0)
          (setVal x (first fJs) 0 1.0)
          (backward P A b pivots x) 
          x)
        (backwardGaussFree P A b pivots (next fJs)))
       nil))

(defn backwardGaussSpecial [P A b pivots] 
  (let [x (transpose (averageRow A))]
    (fill x 0)
    (backward P A b pivots x)
    x))

(defn gauss [aA ab]
  (let [A (copy aA), b (copy ab), iniP (newP A)]
    (if (not (= (rowIds A) (rowIds b)))
      nil
      (let [Ppivots (forward iniP A b (:iStart (:ids A)) (:jStart (:ids A)) '())]
        (let [P (first Ppivots)
              pivots (second Ppivots)]
          (if (solvable? P b pivots)
            {:solvable true
             :P P
             :U A 
             :b b
             :pivots pivots
             :x (backwardGaussSpecial P A b pivots)
             :xs (backwardGaussFree   P A (fill (copy b) 0) pivots (freeVarJs A pivots))
             }
            {:solvable false
             :P P
             :U A 
             :b b
             :pivots pivots
             :x nil
             :xs nil
             }
            )
          )))))


;;
;; rank of matrix
;;
(defn rank [A]
  (let [result (gauss A (averageColumn A))]
    (count (:pivots result))))

;;
;; least square method by gaussian elimination
;;
(defn leastSquare [A b] (gauss (multi (transpose A) A) (multi (transpose A) b)))


;;
;; sample
;;
(def mat17-1 (newMatrix 2 2 '(2 3, 4 0)))
(def mat17-2 (newMatrix 2 1 '(1 5)))

(def mat23-1 (newMatrix 3 3 '(1 0 0, 2 1 0, 0 0 1)))
(def mat23-2 (newMatrix 3 3 '(1 0 0, 0 1 0, -1, 0 1)))
(def mat23-3 (newMatrix 3 3 '(1 0 0, 0 1 0, 0 -3 1)))

(def A58 (fillList (newMatrix -1 1 -2 1) '(1 3 3 2, 2 6 9 5, -1 -3 3 0)))
(def b58   (fillList (newMatrix -1 1 0 0) '(1 2 -1)))

(def A20 (fillList (newMatrix -1 1 -2 0) '(2 1 1, 4 1 0, -2 2 1)))
(def b20 (fillList (newMatrix -1 1 0 0) '(1 -2 7)))

(def A65 (newMatrix 4 2 '(0 0, 1 2, 4 8, 0 0)))
(def b65 (newMatrix 4 1 '(1 1 1 1)))

(def A126 (fillList (newMatrix -5 -3 -8 -7) '(1 2 1 5 0 0)))
(def b126 (fillList (newMatrix -5 -3 0 0) '( 4 3 9)))

(def A132 (fillList (newMatrix -2 1 3 4) '(1 0    1 1    1 3    1 4)))
(def b132 (fillList (newMatrix -2 1 0 0) '(0 1 2 5)))

(def matP (newMatrix 6 6 '(
 87  270 -12  -49 -276  40
-14 -45   6  10   46  -4
-50 -156  4  25  162 -25
94   294 -5  -47 -306 49
  1    1  3   1   0   2
 16   48  1  -6  -48  8
)))
(def matP2 (newMatrix 6 6 '(
 87.0  270.0 -12.0  -49.0 -276.0  40.0
-14.0 -45.0   6.0  10.0   46.0  -4.0
-50.0 -156.0  4.0  25.0  162.0 -25.0
94.0   294.0 -5.0  -47.0 -306.0 49.0
  1.0    1.0  3.0   1.0   0.0   2.0
 16.0   48.0  1.0  -6.0  -48.0  8.0
)))

;;
;; for test
;;
(def ^:const valMax 10000.0)
(def ^:const tolerantSize 50)

(defn randomList
  [randf]
  (lazy-seq (cons (apply randf '()) (randomList randf))))

(defn randListN
  [n]
  (take n (randomList (fn [] (* (rand-nth [-1 1]) (rand-int (int valMax)))))))

(defn randListR []
  (randomList (fn [] (* (rand-nth [-1.0 1.0]) (double (rand valMax))))))


(defn matDiffXs [a xs]
  (if (hasElements? xs)
    (+ (sumColumn (multi a (first xs)) 0) (matDiffXs a (next xs)))
    0.0))
      
(defn check_gauss 
  ([]
     (let [mns (randListN 2)]
       (let [m (+ 1 (rem (Math/abs (nth mns 0)) tolerantSize))
             n (+ 1 (rem (Math/abs (nth mns 1)) tolerantSize))]
         (check_gauss m n))))
  ([m n]
     (let [mns (randListN 4)
           iStart (nth mns 2)
           jStart (nth mns 3)
           vals (randListR)
           a (fillList (newMatrix iStart (- (+ iStart m) 1) jStart (- (+ jStart n) 1)) vals)
           realx  (fillList (newMatrix jStart (- (+ jStart n) 1) 0 0) (drop (* m n) vals))
           b (multi a realx)
           result (gauss a b)
           ax (multi a (:x result))
           diffX (matDiff b ax)
           diffXs (matDiffXs a (:xs result))
           ]
       (println "diffs is zero?" (isZero? (+ diffX diffXs)))
       (println "solvable?" (:solvable result))
       (println "diffs x, xs: " diffX diffXs)
       (println "m n freeVarNum" m, n, (- n (count (:pivots result))))
       (if (and (:solvable result) (isZero? (+ diffX diffXs)))
         true
         (do 
           (map #(stdout %) (:xs result))
           (stdout a)
           (stdout realx)
           (stdout b)
           (stdout (:x result))
           false
           )))))

(defn itr_gauss [itr]
  (let [flg (check_gauss)]
    (if (and flg (> itr 0)) 
      (itr_gauss (- itr 1))
      )))
