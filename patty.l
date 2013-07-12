(def '*Match-fail '("Match Fail"))

(de patty-gensym (S)
   (sym (pack S (rand))) )
(de match1-symbol TERMS
   (let (@S (car TERMS)
         @Val (cadr TERMS) 
         @Body (cddr TERMS))
      (macro 
         (let (@S @Val)
            . @Body ) ) ) )

(de match1-quoted-symbol TERMS
   (let (@Qs (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS) )
      (macro (if (= @Qs @Val) (prog . @Body) *Match-fail)) ) )

(de match1-number TERMS
   (let (@N (car TERMS)
         @Val (cadr TERMS)
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
         @Val (cadr TERMS)
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
         @Val (cadr TERMS)
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

(de match1-con TERMS
   (let (@Pat (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS) )
      (cond
         ((= @Pat '())
          (macro 
             (if (= @Val '())
                (prog . @Body)
                *Match-fail ) ) )
         ((= 1 (length @Pat))
          (let (@Pat (car @Pat))
             (macro (match1 @Pat @Val . @Body)) ) )
         (T 
            (let (@Val-name (sym (pack "match-val-" (rand)))
                  @Ph (car @Pat)
                  @P-tail (cdr @Pat) )
                (macro 
                   (let (@Val-name @Val) 
                      (if (> (length @Val-name) 0) 
                         (match1 @Ph (car @Val-name) 
                            (match1-con @P-tail (cdr @Val-name) . @Body) )
                         *Match-fail ) ) ) ) ) ) ) )


(de match1-and TERMS
   (let (@Pats (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS))
      (cond 
         ((= 1 (length @Pats))
          (let (@Pat (car @Pats))
             (macro (match1 @Pat @Val . @Body)) ) )
         (T 
            (let (@Pat1 (car @Pats)
                  @Rest (cdr @Pats)
                  @Val-name  (sym (pack "match-val-" (rand))))
               (macro (let (@Val-name @Val) 
                         (match1 @Pat1 @Val
                            (match1-and-lit @Rest @Val-name . @Body) ) ) ) ) ) ) ) )

(de match1-and-lit TERMS
   (let (@Pats (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS))
      (cond 
         ((= 1 (length @Pats))
          (let (@Pat (car @Pats))
             (macro (match1 @Pat @Val . @Body)) ) )
         (T 
            (let (@Pat1 (car @Pats)
                  @Rest (cdr @Pats))
               (macro (match1 @Pat1 @Val
                            (match1-and-lit @Rest @Val-name . @Body) ) ) ) ) ) ) )

(de match1-or TERMS
   (let (Pats (car TERMS)
         @Val (cadr TERMS)
         @Val-Val___ (eval @Val)
         @Body (cddr TERMS) )
      (recur (Pats)
         (cond 
            ((= '() Pats) *Match-fail)
            ((= 1 (length Pats))
             (let (@Pat (car Pats))
                (macro (match1 @Pat @Val-Val___ . @Body)) ) )
            (T 
               (let (@Pat (car Pats)
                     Rest (cdr Pats)
                     Result (macro (match1 @Pat @Val-Val___ . @Body)) )
                  (if (n== Result *Match-fail) Result
                     (recurse Rest) ) ) ) ) ) ) )

(de match1-? TERMS 
   (let (Args (car TERMS)
         @F (car Args) 
         @Pat (cadr Args)
         @Val (cadr TERMS)
         @Val-name (patty-gensym "Val")
         @Body (cddr TERMS) )
      (if @Pat
         (macro 
            (let (@Val-name @Val)
               (if (@F @Val-name)
                  (match1 @Pat @Val-name . @Body)
                  *Match-fail ) ) )
         (macro 
            (let (@Val-name @Val)
               (if (@F @Val-name) (prog . @Body)
                  *Match-fail ) ) ) ) ) )

(de match1-call TERMS 
   (let (Args (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS))
      (cond 
         ((= 1 (length Args))
          (let (@F (car Args))
             (macro 
                (if (== (@F @Val) *Match-fail)
                   *Match-fail
                   (prog . @Body) ) ) ) )
         ((= 2 (length Args))
          (let (@F (car Args)
                @Sub-val (patty-gensym "Sub-val")
                @Sub-pat (cadr Args) )
             (macro 
                (let (@Sub-val (@F @Val))
                   (if (== *Match-fail @Sub-val)
                      *Match-fail
                      (match1 @Sub-pat @Sub-val . @Body) ) ) ) ) )
         (T (quit "Malformed match1-call")) ) ) )

(de match1-apply TERMS 
   (let (Args (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS))
      (cond 
         ((= 1 (length Args))
          (let (@F (car Args))
             (macro 
                (if (== (apply @F @Val) *Match-fail)
                   *Match-fail
                   (prog . @Body) ) ) ) )
         ((= 2 (length Args))
          (let (@F (car Args)
                @Sub-val (patty-gensym "Sub-val")
                @Sub-pat (cadr Args) )
             (macro 
                (let (@Sub-val (apply @F @Val))
                   (if (== *Match-fail @Sub-val)
                      *Match-fail
                      (match1 @Sub-pat @Sub-val . @Body) ) ) ) ) )
         (T (quit "Malformed match1-apply")) ) ) )


(de match1 TERMS
   (let (@Pat (car TERMS)
         @Val (cadr TERMS)
         @Body (cddr TERMS) )
      (cond
         ((== NIL @Pat)
          (if (== (eval @Val) NIL)
             (macro (prog . @Body))
             *Match-fail ) )
         ((== T @Pat)
          (if (== (eval @Val) T)
             (macro (prog . @Body))
             *Match-fail))
         ((str? @Pat)
          (macro (match1-string @Pat @Val . @Body)) )
         ((sym? @Pat)
          (macro (match1-symbol @Pat @Val . @Body)) )
         ((num? @Pat)
          (macro (match1-number @Pat @Val . @Body)) )
         ((lst? @Pat)
          (let (Hd (car @Pat)) 
             (cond 
                ((= Hd 'quote)
                 (macro (match1-quoted-symbol @Pat @Val . @Body)) )
                ((= Hd 'list)
                 (let (@List-pats (cdr @Pat)) 
                    (macro (match1-list @List-pats @Val . @Body)) ) )
                ((= Hd 'con)
                 (let (@Con-pats (cdr @Pat)) 
                    (macro (match1-con @Con-pats @Val . @Body)) ) )
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
                ((= Hd 'apply)
                 (let (@List-pats (cdr @Pat))
                    (macro (match1-apply @List-pats @Val . @Body)) ) )
                (T (quit "Unrecognized `match` pattern head." Hd)) ) ) )
         (T (quit "Unrecognized `match` pattern." @Pat)) ) ) )

(de patty TERMS
   (let (Patty-Val (eval (car TERMS))
         Patty-Bodies (cdr TERMS)
         Done NIL 
         Patty-Result NIL)
      (while (not Done)
         (cond 
            ((== NIL Patty-Bodies)
             (quit (pack "Patty failed to match " Patty-Val " against " TERMS)))
            (T 
               (let (Patty-Body (pop 'Patty-Bodies)
                     @Pat (car Patty-Body)
                     @Success (cdr Patty-Body)
                     Result  (macro (match1 @Pat Patty-Val . @Success)))
                  (when
                     (n== Result *Match-fail)
                     (setq Done T)
                        (setq Patty-Result Result) ) ) ) ) )
      Patty-Result ) )



