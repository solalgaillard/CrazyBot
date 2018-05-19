; utilisation de macro-caractères pour étendre un simple
; signe en une liste avec variable ou fonctions et arguments
; si nécessaire. "*" était utilisé pour les defparametres donc
; je n'ai pas pu en faire une macro et l'ai remplacé par "^".
; ce pattern-matcher a été largement inspiré par celui qu'il nous
; est demandé de construire au Chapitre 3 des fondements de l'IA.

; pour la variable d'élément:
(set-macro-character #\?
	#'(lambda (flux charactere)
		(cons '|?| (read flux t nil t)) ) )
		
; pour la variable de segment
(set-macro-character #\^
	#'(lambda (flux charactere)
		(cons '|^| (read flux t nil t)) ) )

; pour définir un membre d'une liste
; crée une macro avec une macro
; en utilisant l'évaluation de la fonction membre
; par la macro eval-pat.
(set-macro-character #\!
	#'(lambda (flux charactere)
		(cons '|?| `(m (member m ',(read flux t nil t)) ) ) ) )		


; fonction pilote du pattern-matcher
; distingue les cas où le pattern est terminé,
; ou l'input est terminé,
; et les deux jokers "?" et "^".	
(defun match (input-form pattern)
	"pattern-matcher prenant en compte él esseulé ou suite d'él"
	(cond
		((atom pattern)(eql pattern input-form)) ;si dernière récursion
		((atom (car pattern))
			(if (eq (car pattern)(car input-form)) ; tant que les él sont égaux.
				(match (cdr input-form) (cdr pattern)) 
				nil) )
		((eq (caar pattern) '|^|) ;si joker segment
			(let ((val (assoc (get-var (car pattern)) variables)))
			     (cond 
				(val ; est-ce que la variable a déjà une association ?
					(match input-form (append (cdr val)(cdr pattern))) ) ;récu avec subst
				((not (cdr pattern)) ; est-ce qu’on est à la fin du pattern ?
					; on met tout le reste dans var
					(push (append (list (get-var (car pattern))) input-form) variables)
					(if (consp (cdar pattern)) ;si arg il y a une funct, on évalue, sinon t.
						(eval 
							(cons 'eval-pat 
							(cons 'variables (cddar pattern))))
						t ) )
				(t (push (cons (get-var (car pattern)) nil) variables)
				   ; si la séquence valide, on dit 't, sinon on retire l'él de variables
				   (let ((res (match-sequence pattern input-form variables)))
					(if res t (progn (pop variables) nil) ) ) ) ) ) )
		((atom input-form) nil) ;plus d'input, c'est nil
		((eq (caar pattern) '|?|) ;él unique
			(when (match-element (car pattern) input-form) ;on le vérifie
				  (match (cdr input-form) (cdr pattern)) ) ) ; on récurse
		; match vérifie même les sous-listes
		((match (car input-form)(car pattern)) (match (cdr input-form)(cdr pattern))) ) )

; fonction spécifique au joker d'un seul él
(defun match-element (pattern input-form)
	(let ((val (assoc (get-var pattern) variables))) ; déjà une assoc?
	     (if val
			(equal (car input-form) (cdr val)) ; si oui on vérifie si ça marche avec prec val
			(progn  ; sinon on rajoute à variables
				(push (cons (get-var pattern) (car input-form)) variables)
				(if (consp (cdr pattern)) ; puis si funct dans args, on évalue
					(eval `(eval-pat variables ,@(cddr pattern)))
					t ) ) ) ) ) ;sinon 't

; fonction spécifique au joker de séquence
; qui regarde d'abord si on peut continuer 
; avec la définition de notre variable et qui
; sinon continue d'en affecter.
(defun match-sequence (pattern input-form var)
	(if input-form
		(if (match input-form (cdr pattern)) ; si l'affectation de l'input dans le pattern suffit.
			(if (consp (cdar pattern))
				(eval `(eval-pat variables ,@(cddar pattern)))
				t)
			(progn
				(setq variables var)
				(nconc (assoc (get-var (car pattern)) variables) 
				       (list (car input-form)) ) ; on concatenate et on continue
				(match-sequence pattern (cdr input-form) variables) ) ) ) )

; utilisé pour isoler la variable, que ce soit directement l'atome
; d'un doublet ou le car d'une sous-liste (dépend de si seul un symbole
; avait été passé à une macro-car ou si une fonction (donc une liste)
; avait été passé à la macro-car.)
(defmacro get-var (x) `(if (listp (cdr ,x)) (cadr ,x) (cdr ,x)))

; utilise l'opérateur spécial progv pour
; "binder" toutes les valeurs à la suite
; et ainsi exécuter la fonction en argument
(defmacro eval-pat (var &rest func)
`(progv
	(mapcar #'car ,var)
	(mapcar #'cdr ,var)
	(and ,@func) ) )
