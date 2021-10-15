(define-syntax the
  (lambda (a)
    "the THING: this gives its NAME, TYPE, BINDING, TRANSFORMER, or ALL, or just some."
    (syntax-case a (name type binding transformer all)
      ((<1> a) #'(module-ref (current-module) (quote a)))
      ((<2> a name) #'(macro-name (the a)))
      ((<3> a type) #'(macro-type (the a)))
      ((<4> a binding) #'(macro-binding (the a)))
      ((<5> a transformer) #'(macro-transformer (the a)))
      ((<all> a all) #'(the a name type binding transformer))
      ((<some> a b b* ...) #'(cons (the a b) (the a b* ...)))
      ((usage a* ...) #'"Usage: (the [name|type|binding|transformer|all|]* A)"))))
