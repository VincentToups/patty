(def '*Match-fail '("Match Fail"))

(de match1-symbol TERMS
   (let (@S (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS))
      (macro 
         (let (@S @Val)
            . @Body ) ) ) )

(de match1-number TERMS
   (let (@N (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS)
         @B (sym (pack "match-val-" (rand))) )
      (macro 
         (let (@B @Val)
            (if (and 
                   (num? @B)
                   (= @B @N) )
               (prog . @Body)
               *Match-fail ) ) ) ) )

(de match1-string TERMS
   (let (@N (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS)
         @B (sym (pack "match-val-" (rand))) )
      (macro 
         (let (@B @Val)
            (if (and 
                   (str? @B)
                   (= @B @N) )
               (prog . @Body)
               *Match-fail ) ) ) ) )


(de match1-list TERMS
   (let (@Pat (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS) )
      (cond
         ((= @Pat '())
          (macro 
             (if (= @Val '())
                (prog . @Body)
                *Match-fail ) ) )
         ((and 
             (= 1 (length @Pat))
             (cdr @Pat) )
          (let (@El-pat (car @Pat)
                @Tail-pat (cdr @Pat)
                @Val-name (sym (pack "match-val-" (rand))))
             (macro 
                (let (@Val-name @Val)                
                   (match1 @El-pat (car @Val-name)
                      (match1 @Tail-pat (cdr @Val-name) . @Body) ) ) ) ) )
         (T 
            (let (@Val-name (sym (pack "match-val-" (rand)))
                  @Ph (car @Pat)
                  @P-tail (cdr @Pat) )
                (macro 
                   (let (@Val-name @Val) 
                      (if (> (length @Val-name) 0) 
                         (match1 @Ph (car @Val-name) 
                            (match1-list @P-tail (cdr @Val-name) . @Body) )
                         *Match-fail ) ) ) ) ) ) ) )

(de match1-and TERMS
   (let (@Pats (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS))
      (cond 
         ((= 1 (length @Pats))
          (let (@Pat (car @Pats))
             (macro (match1 @Pat @Val . @Body)) ) )
         (T 
            (let (@Pat1 (car @Pats)
                  @Rest (cdr @Pats))
               (macro (match1 @Pat1 @Val
                         (match1-and @Rest @Val . @Body) ) ) ) ) ) ) )

(de match1-or TERMS
   (let (Pats (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS) )
      (recur (Pats)
         (cond 
            ((= '() Pats) *Match-fail)
            ((= 1 (length Pats))
             (let (@Pat (car Pats))
                (macro (match1 @Pat @Val . @Body)) ) )
            (T 
               (let (@Pat (car Pats)
                     Rest (cdr Pats)
                     Result (macro (match1 @Pat @Val . @Body)) )
                  (if (n== Result *Match-fail) Result
                     (recurse Rest) ) ) ) ) ) ) )

(de match1-? TERMS 
   (let (Args (car TERMS)
         F (eval (car Args))
         @Pat (cadr Args)
         @Val (eval (cadr TERMS))
         P (F @Val)
         @Body (cddr TERMS) )
      (if P 
         (if @Pat 
            (macro (match1 @Pat @Val . @Body))
            (macro (prog . @Body)) )
         *Match-fail ) ) )

(de match1-call TERMS 
   (let (Args (car TERMS)
         F (eval (car Args))
         @Pat (cadr Args)
         @Val (eval (cadr TERMS))
         @Val* (F @Val)
         @Body (cddr TERMS) )
      (if (n== @Val* *Match-fail) 
         (if @Pat 
            (macro (match1 @Pat @Val* . @Body))
            (macro (prog . @Body)) )
         *Match-fail ) ) )

(de match1 TERMS
   (let (@Pat (car TERMS)
         @Val (eval (cadr TERMS))
         @Body (cddr TERMS) )
      (cond 
         ((str? @Pat)
          (macro (match1-string @Pat @Val . @Body)) )
         ((sym? @Pat)
          (macro (match1-symbol @Pat @Val . @Body)) )
         ((num? @Pat)
          (macro (match1-number @Pat @Val . @Body)) )
         ((lst? @Pat)
          (let (Hd (car @Pat)) 
             (cond 
                ((= Hd 'list)
                 (let (@List-pats (cdr @Pat)) 
                    (macro (match1-list @List-pats @Val . @Body)) ) )
                ((= Hd 'and)
                 (let (@List-pats (cdr @Pat))
                    (macro (match1-and @List-pats @Val . @Body)) ) )
                ((= Hd 'or)
                 (let (@List-pats (cdr @Pat))
                    (macro (match1-or @List-pats @Val . @Body)) ) )
                ((= Hd '?)
                 (let (@List-pats (cdr @Pat))
                    (macro (match1-? @List-pats @Val . @Body)) ) )
                ((= Hd 'call)
                 (let (@List-pats (cdr @Pat))
                    (macro (match1-call @List-pats @Val . @Body)) ) )
                (T (quit "Unrecognized `match` pattern head." Hd)) ) ) )
         (T (quit "Unrecognized `match` pattern." @Pat)) ) ) )

(de patty TERMS
   (let (@Val (eval (car TERMS))
         Bodies (cdr TERMS) )
      (recur (Bodies)
         (cond 
            ((= Bodies '())
             (quit "`match` found no matching bodies" 
                (let (@TERMS TERMS) 
                   (macro '(match @TERMS)) ) ) )
            (T (let (Body (car Bodies)
                  Bodies (cdr Bodies)
                  @Pat (car Body)
                  @Success (cdr Body)
                  Result (macro (match1 @Pat @Val . @Success)) )
               (if (n== Result *Match-fail) Result 
                  (recurse Bodies) ) )) ) ) ) )



