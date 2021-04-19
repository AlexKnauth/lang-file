(export interp comp)

;; An Expr is one of:
;;  - Symbol
;;  - ['lambda [Symbol] Expr]
;;  - [Expr Expr]

;; An Env is an (Assqof Symbol Any)

;; interp : Env Expr -> Any
(def (interp env expr)
  (match expr
    ((? symbol? x)   (assq-ref env x))
    (['lambda [a] b] (lambda (av) (interp (assq-put env a av) b)))
    ([f a]           ((interp env f) (interp env a)))))

;; comp : Expr -> (Env -> Any)
(def (comp expr)
  (match expr
    ((? symbol? x)
     (lambda (env) (assq-ref env x)))
    (['lambda [a] b]
     (let (b* (comp b))
       (lambda (env) (lambda (av) (b* (assq-put env a av))))))
    ([f a]
     (let ((f* (comp f)) (a* (comp a)))
       (lambda (env) ((f* env) (a* env)))))))

;; assq-ref : (Assqof K V) K -> V
(def (assq-ref a k) (cdr (assq k a)))

;; assq-put : (Assqof K V) K V -> (Assqof K V)
(def (assq-put a k v) [[k . v] . a])
