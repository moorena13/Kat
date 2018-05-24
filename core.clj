(ns chatbot.core)  
 
(def dictionary (atom {})) 
(def poslist ["noun" "pronoun" "verb" "adverb" "conjunction" "article" "interjection" "preposition" "adjective"])

(defn administrator []
  (println "Is this a protected session?")
  (read-line)) ;check whether to write to file at end
 
(defn makeword [w]
 (swap! dictionary  
        assoc (keyword w)  
        (atom {:partofspeech []  ;partofspeech is now a vector; can contain multiple parts of speech
               :connections {}})))

(defn removeword [w]
  (swap! dictionary
         dissoc (keyword w))
  (doseq [item (keys @dictionary)]
  (if (contains? (get @(get @dictionary item) :connections) (keyword w))
    (swap! (get @dictionary item)
           update-in [:connections] dissoc (keyword w)))))

(defn updateconnections [l] 
  (defn inner [one two] ;one is the word whose dictionary is being updated; two is the comparison
    (if (not= one two)
      (if (contains? (get @(get @dictionary (keyword one)) :connections) (keyword two))
        (swap! (get @dictionary (keyword one))
               update-in [:connections (keyword two)] inc)
        (swap! (get @dictionary (keyword one))
               assoc-in [:connections (keyword two)] 1))))
  (defn outer [word list]
    (doseq [item list]
      (inner word item)))
  (doseq [index l]
    (outer index l)))
 
(defn splitinput [s] 
 (clojure.string/split (clojure.string/lower-case s) #"\s+")) 

(defn strippunct [s]
  (clojure.string/replace s #"\.|,|\"|;|:|!|\?" ""))
 
(defn inputparser [s] 
  (if (not= s ()) 
    [(if (not (contains? @dictionary (keyword (first s))))
       (makeword (first s))) 
     (inputparser (rest s))]))

(defn fileout []
  (def outputstring "")
  (doseq [x (keys @dictionary)]
    (def outputstring (str outputstring x "\n"
                           (get @(get @dictionary x) :partofspeech) "\n"
                           (get @(get @dictionary x) :connections) "\n")))
  (spit "memory.txt" outputstring))

(defn filein []
  (defn word [a b c]
    (swap! dictionary  
        assoc (load-string a)  
        (atom {:partofspeech (load-string b)  
               :connections (load-string c)})))
  (defn destruct [v]
    (let [[a b c & remaining] v] 
      (word a b c)
      (if (not= remaining nil)
        (destruct remaining))))
  (destruct (clojure.string/split (slurp "memory.txt") #"\n")))

(defn getword [k p]
  (def connections (get @(get @dictionary k) :connections))
  (def counter 0)
  (def word "")
  (doseq [item connections]
    (let [[a b] item]
      (if (and (some (partial = p) (get @(get @dictionary a) :partofspeech)) 
               (> b counter))
        [(def word (name a))
        (def counter b)]))) ;fixed bug where connections value didn't matter
  word)

(defn reply [s]
  (def response "")
  (doseq [word s]
    (if (not= (get @(get @dictionary (keyword word)) :connections) {})
      (def response (str response " " (getword (keyword word) (first (get @(get @dictionary (keyword word)) :partofspeech)))))))
  (println response))

(defn updatepos [w p]
  (swap! (get @dictionary (keyword w))
         update-in [:partofspeech] conj p))

(defn getpos [w]
  (println "What part of speech is" w "?")
  (def input (strippunct (clojure.string/lower-case (read-line))))
  (if (not (some (partial = input) poslist))
    (getpos w)
    (updatepos w input))
  (println "Any other parts of speech?")
  (if (= (read-line) "YES") (getpos w)))

(defn masspos []
  (doseq [item (keys @dictionary)]
    (cond 
      (= (get @(get @dictionary item) :partofspeech) []) (getpos (name item)))))
 
(defn runbot []
  (filein)
  (def initial (read-line))
  (if (not= initial "TERMINATE")
    [(def input (splitinput (strippunct initial)))
    (inputparser input)
    (updateconnections input)
    (reply input)
    (runbot)]
    (if (= (administrator) "YES")
      (fileout)))) 

(runbot)


