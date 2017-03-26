(defprotocol APattern
  (recognize [this, url]))

;; Functions to parse a string representation of Pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pattern-part-regex 
    "Build a regular expression for given pattern part 
    (e.g. host, path or queryparam)"
    [partname]  (str partname "\\((\\S+)\\);") )
  
(defn extractparts
    "Extract values from all occurences of the given pattern part"
     [partname pattern] 
  (map #(nth % 1) (re-seq (re-pattern (pattern-part-regex partname)) pattern)) )

(defn extractpart 
    "Extract values from first (or single) occurence of the given pattern part"
    [partname pattern] 
    (first (extractparts partname pattern)) )

(defn parse-pattern 
    "Parse a string representation of pattern"
    [pattern]
    [ (extractpart "host" pattern)
      (extractpart "path" pattern)
      (extractparts "queryparam" pattern)]
)

(defn extractbinds 
  "Extract binds from the given string"
  [part] 
  (map #(nth % 1) (re-seq #"\?(\w+)" part)) )

(defn extract-qpname 
    "Extract the name from the queryparam (a string like 'paramname=?parambind')"
    [part] 
    (nth (re-find #"^(\w+)=" part) 1) )

    
;; Parse URL, bind values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn binds-to-regex 
    "Build a regular expression for the given binds.
    This regex will be user to build URL-matching pattern"
    [str binds] 
    (loop [s str bs binds]
        (if (empty? bs)
        s
        (recur (clojure.string/replace s (first bs) "([\\S&&[^\\?]]+)") 
                (next bs))))
)

(defn build-hostpath-regex
    "Build a regex for parsing the host + path part of the URL"
    [host path binds] 
    (re-pattern (binds-to-regex (str 
        "^\\w+://"
        host 
        "/" 
        path
        ) binds)
    )
)

(defn build-qp-pattern 
    "Build a pattern to parse the query parameter of the URL"
    [qpname] 
    (re-pattern (str 
        "[\\?\\&]" qpname "=" "([\\S&&[^\\?\\&]]+)"  
    ))
)

(defn parse-qp 
    "Parse and bind a query parameter from the URL"
    [url qpname] 
    (first (map #(nth % 1) 
        (re-seq (build-qp-pattern qpname) url)
    )  )
)

(defn parse-qps 
    "Parse all query parameters from the URL"
    [url qpnames qpbinds] 
    (for [[qpname qpbind] (map vector qpnames qpbinds)] 
        [qpbind (parse-qp url qpname)]
    )
)

(defn recognize-hostpath 
    "Extract all binds from host + path part of the URL"
    [url host path] 
    (let [
        hostbinds (extractbinds host)
        pathbinds (extractbinds path)
        hostpathbinds (concat hostbinds pathbinds)

        hostpathpattern (build-hostpath-regex host path (map #(str "?" %) hostpathbinds))
        
        [hostpathfound & hostpathbindvalues] (first (re-seq hostpathpattern url))
        hostpathbinded (map vector (map keyword hostpathbinds) hostpathbindvalues)
        ]

        [(some? hostpathfound) hostpathbinded]
    )
)

(defn recognize-qp [url queryparam] 
    "Extract all binds from query part of the URL"
  (let [
	  qpbinds (map #(keyword (first (extractbinds %))) queryparam)
	  qpnames (map #(extract-qpname %) queryparam)
	  
	  qpbinded (parse-qps url qpnames qpbinds)
	  qpmatches (every? some? (map #(second %) qpbinded) )
    ]
	  [qpmatches qpbinded]
  )
)

(deftype Pattern [pattern]
  APattern
  (recognize [this, url] 

	(let [
	  [host path queryparam] (parse-pattern pattern)
	  [hostpathmatches hostpathbinded] (recognize-hostpath url host path)
	  [qpmatches qpbinded] (recognize-qp url queryparam)
	  ]
    
      (if (and hostpathmatches qpmatches)
        (concat hostpathbinded qpbinded)
      )
	)
  )
)



(use 'clojure.test)
 
(def twitter (Pattern. "host(twitter.com); path(?user/status/?id);"))

(def dribbble (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))
  
(def livejournal (Pattern. "host(?username.livejournal.com); path(?postid.html);"))


(deftest tests
    (is (= 
        [[:user "bradfitz"] [:id "562360748727611392"]] 
        (recognize 
            twitter
            "http://twitter.com/bradfitz/status/562360748727611392"
        ) 
    ))
    
    (is (= 
        [[:id "1905065-Travel-Icons-pack"] [:offset "1"] [:type "users"] ] 
        (recognize 
            dribbble 
            "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
        )
    ))
    
    (is (= 
        nil 
        (recognize 
            dribbble 
            "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
        )
    ))
    
    (is (= 
        nil 
        (recognize 
            dribbble 
            "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"
        )
    ))

    (is (= 
        [[:username "nextbigfuture"] [:postid "4421103"]] 
        (recognize 
            livejournal
            "http://nextbigfuture.livejournal.com/4421103.html"
        ) 
    ))
)

(run-tests)
