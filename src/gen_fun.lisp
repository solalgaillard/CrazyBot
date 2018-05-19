; La variable memoire garde en mémoire des éléments 
; d'entrées précédentes pour pouvoir rebondir dessus
; en cas de creux dans la conversation.
; La fonction bismuth lit en boucle les données et 
; les traite.
(defun bismuth ()
	"répond à l’utilisateur utilisant un pattern-matcher"
	(setq memoire nil)
	(loop
		(setq variables nil)
		(format t "~%~a" "Paul Bismuth>")
		; variable locale avec réponse en liste
		(let ((réponse-en-liste (read-up (read-line)))) 
		      (if (equal réponse-en-liste '(au revoir))
			      	  (progn 
			      	  	(format t "~a" "Votez pour moi dimanche.")
			      	  	(return-from bismuth  '(fin du programme)) )
				  (write-up réponse-en-liste t) ) ) ) )

; Fonction qui lit la ligne, la nettoie,
; la transforme en liste, effectue les premières 
; substitutions et dèlegue le reste à
; (cherche-règles).
(defun read-up (line)
	"lit la ligne"
	(flatten
		(eval-inner ;évalue les sous-listes si pattern possède une fonction
			(cherche-règles 
				(flatten ;obligé d'aplatir à cause des inner listes dans les tables d'assoc
					(sublis ;remplace par des synonymes
						*syn-et-corr*
						(creer-liste 
							(nettoyer 
								(nstring-downcase ;enlève la 1ère maj.
									line 
										:start 0 
										:end 1 ) ) ) ) ) ) ) ) )

; Nettoie la chaîne en substituant 
; la ponctuation par des espaces.
(defun nettoyer (chaine)
	"Substitue la ponctuation par des espaces"
	(remove #\' 
		(substitute-if 
			#\space 
			#'ponctuation 
			chaine ) ) ) 
				
; valide si car fait partie de la chaîne.	
(defun ponctuation (char) 
	"valide quand l'arg correspondant à la ponctuation"
	(find char ".,;:`!\'?#()\\\"") )

; fonction qui crée la liste
; en faisant des appels récursifs
; avec read-from-string.
(defun creer-liste (chaine &optional (position 0))
	"crée une liste depuis une chaîne"
	(multiple-value-bind (atome position)
		(read-from-string chaine nil nil :start position)
		(cond
			((and (not atome) (eq position (length chaine))) nil)
			(t (cons atome (creer-liste chaine position)) ) ) ) )
			
; Evalue des fonctions sur les cons à l'intérieur de la liste
(defun eval-inner(L)
	"évalue les sous-listes"
	(cond 
		((not L) nil)
		((and (consp (car L)) (fboundp (caar L))) (cons (eval (car L)) (eval-inner (cdr L))))
		(t (cons (car L) (eval-inner (cdr L)))) ) )

; fonction qui dirige d'abord vers les règles courantes de substitution
; sinon vers la mémoire
; et enfin vers des règles spécifiques quand ni les règles courantes,
; ni la mémoire ont trouvé un pattern de substitution.
(defun cherche-règles (entree)
	"cherche une regle et transforme l’entree"
	(prog1
		(let ((prin_rules (puise-dans-regles entree)) (mem_rules (puise-dans-mem entree)))
			(if prin_rules
			   	prin_rules
			   	(if mem_rules
			    		mem_rules 
			    		(random-element *no_patterns* ) ) ) )		
		(write-in-mem entree) ) ) ;regarde si l'entrée peut être sauvegarder en mémoire

; prend un élément au hasard dans une liste	
(defun random-element (liste &optional (ct_el (length liste)))
	"choisir aléatoirement un élément d’une liste"
	(elt liste (random ct_el)) )

; utilise le pattern-matcher pour substituer une réponse
; à l'entrée de l'utilisateur
(defun puise-dans-regles (entree)
	"substitue l'entrée avec les pattern de pattern-regles"
	(let ((sub (cdr (assoc entree *pattern-regles* :test #'match))))
	     (when sub
		   (sublis (change-point-de-vue variables) ;substitions des éléments de la table d'assoc
		     	   (random-element sub) ) ) ) )

; permet de changer la perspective
; du discours
(defun change-point-de-vue (mots)
	"change je en tu et vice versa, etc"
	(sublis *change-perspective* mots) )	
			
; prend un élément sauvé dans la mémoire
(defun puise-dans-mem (entree)
	"prend él dans mémoire"
	(when (and memoire (< 1 (length memoire))) ;Ne relance pas sur la dernière entrée
	    	(let ((mem-rand (random-element memoire (-(length memoire) 1))))
		     (setq memoire (remove-element mem-rand memoire)) ;retire l'élément trouvé de la mémoire
		     (flatten ; obligé d'aplatir pour pas évaluer si plus d'un él dans table d'assoc
		     	(sublis (change-point-de-vue mem-rand)
				(random-element *from-mem*) ) ) ) ) )

; enlève él d'une liste ou sous-listes
(defun remove-element (mot liste)
	"enlève él d'une liste ou sous-liste"
	(cond
		((atom liste) liste) 
		((equal mot (car liste)) (remove-element mot (cdr liste)))
		(t (cons (remove-element mot (car liste)) 
			 (remove-element mot (cdr liste)) ) ) ) )

; fonction qui met un élément dans la mémoire
; et le prépare selon un pattern pour l'utiliser avec
; une table d'associations.
; Se débarasse des parties non utiles de la chaîne entrée
; et ne sauvegarde pas si déjà dans la liste.
(defun write-in-mem (entree)
	"sauvegarde pattern en mémoire"
	(setq variables nil) ;remet variables à nil pour prochain appel de match
	(some #'(lambda (mem_lis) ; pour chaque entrée tant que ça ne valide pas
			(let ((mem (match entree (append '(^x) mem_lis '(^y)))) (mem_ent entree) )
			     (when mem
			     	; retire à gauche et à droite de la chaîne
		      		(setq mem_ent 
		      			(remove (cadr (assoc 'x variables)) mem_ent :count 1))
		  		(setq mem_ent 
		      			(remove (cadr (assoc 'y variables)) mem_ent :count 1))
		      		; pour utiliser une table d'assoc
		      		(setq mem_ent (list (append (list 'x) mem_ent)))
		  		(unless (already-contained mem_ent memoire)
			  		(if (atom memoire) 
			  		    (setq memoire (list mem_ent))
		  	 		    (setq memoire (append memoire (list mem_ent))) )
		  	 		;ne garde pas plus de 5 mots clés en mémoire
		  	 		(when (> (length memoire) 5) (pop memoire)) ) ) ) ) 
		 *to-mem* ) ) 

; vérifie si le symbole fait déjà partie
; d'une liste ou de ses sous-listes.
(defun already-contained (mot liste)
	"le symbole est-il contenu dans une liste ou sous-liste"
	(cond
		((atom liste) nil) 
		((equal mot (car liste)) t)
		(t (if (already-contained mot (car liste)) 
				t 
				(already-contained mot (cdr liste)) ) ) ) )

; imprime à l'écran la liste retournée
(defun write-up (liste &optional (maj_el nil))
	"imprime à l'écran la liste"
	(cond
		((atom liste) (values))
		(t (format t 
			(if maj_el ; Si début de phrase, on capitalize
			    "~@(~a~) " 
			    (if (member (cadr liste) '(\. \, \)))
			    "~a"
			    "~a ") )
			    (car liste))
		   (write-up (cdr liste)) ) ) )

; fonctions de contrôle pour applatir une liste
; tirés des exemples du cours du fondements de l'IA
(defun flatten (une-liste)
	"append les sous-listes d'une-liste"
	(mappend #'mklist une-liste) )

(defun mklist (x)
	"retourne x si x est une liste, sinon (x)"
	(if (listp x) x (list x)) )
	
(defun mappend (fn list)
	"append les resultats de fn appliquée à chaque élément de list.
	comme mapcon, mais utilise append au lieu de nconc"
	(apply #'append (mapcar fn list)) )

; fonction qui permettent de changer le point
; de vue du locuteur
(defun +ez (sym)
	"transforme en 2nde pers du plur"
	(let ((new_conj (cdr (assoc sym *conj-irr*))))
	     (cond 
	     	(new_conj new_conj)
	     	(t (intern (concatenate 'string (symbol-name sym) "z"))) ) ) )
	     	

(defun -ez (sym)
	"transforme en 1ère pers du sing"
	(let ((new_conj (car (find sym *conj-irr* :key #'cdr))))
	     (cond 
	     	(new_conj new_conj)
	     	(t (intern (string-right-trim "z" (symbol-name sym)))) ) ) )
	     	

