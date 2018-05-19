#! /usr/bin/clisp -q -modern -L french
; Each function is self describing

(defun crazyBot (&optional memory)
    "répond à l’utilisateur utilisant les regles de CrazyBot"
    (declare (special memory))
    (loop
        (print 'crazyBot>)
        (let ((réponse-en-liste (read-up (read-line)))) ;;;pour tout mettre en miniscule
              (if (equal réponse-en-liste '(good bye))
                    (progn
                        (format t "~a" "I\'ll see you around.")
                        (return-from crazyBot  '(end of program)) )
                  (write-up réponse-en-liste t) ) ) ) )

(defun creer-liste (aString &optional (position 0))
    (multiple-value-bind (atome position)
        (read-from-string aString nil nil :start position)
        (cond
            ((and (null atome) (eq position (length aString))) nil)
            (t (cons atome (creer-liste aString position)) ) ) ) )

(defun read-up (line)
    (flatten
        (utilise-crazybot-regles
            (flatten (sublis
                    *corrections*
                    (creer-liste (get-proper-string line)) ) ) ) ) )

(defun get-proper-string (aString)
    (string-downcase
        (remove #\'
            (substitute-if
                #\space
                #'ponctuation
                aString ) ) ) )

(defun ponctuation (char)
    (find char ".,;:`!?#-()\\\"") )

(defun write-up (liste &optional (capitalizeFirstEl nil))
    (cond
        ((atom liste) (values))
        (t (format t
            (if capitalizeFirstEl
                "~@(~a~) "
                (if (member (cadr liste) '(\. \?\ ! \, \)))
                "~a"
                "~a ") )
                (car liste))
           (write-up (cdr liste)) ) ) )

(defun utilise-crazybot-regles (entree)
    "cherche une regle et transforme l’entree"
    (prog1
        (let ((reglesPrin (puise-dans-regles entree)) (reglesMem (puise-dans-mem entree)))
            (if reglesPrin
                reglesPrin
                (if reglesMem
                    reglesMem
                    (random-element *memoryOutputWhenEmpty* ) ) ) )
        (write-in-mem entree) ) )



;;;;; Les règles sont sorties des fonctions pour une plus grande maniabilité du programme

(defun puise-dans-regles (entree)
    (some #'(lambda (regle)
            (let ((res (match (partie-filtre regle) entree)))
                   (when res
                        (mots-capitalisés ;;;; remettre en majuscule les mots appropriés
                       (sublis (change-point-de-vue (cdr res))
                        (random-element (partie-reponse regle)) ) ) ) ) )
           *crazybot-regles* ) )

(defun puise-dans-mem (entree)
    (mots-capitalisés ;;;; remettre en majuscule les mots appropriés
        (when (and memory (< 1 (length memory))) ;;Ne relance pas sur les deux dernières entrées
            (let ((mémoire-hasard (random-element memory (-(length memory) 2))))
             (setq memory (remove-element mémoire-hasard memory))
             (sublis (change-point-de-vue mémoire-hasard)
                (random-element *memoryOutput*)) ) ) ) )

(defun write-in-mem (entree)
    (some #'(lambda (memoryListe)
            (let ((memoryFit (match (append '(?x) memoryListe '(?y)) entree)) (memorizedWord entree) )
                 (when memoryFit
                      (remove (cdr (assoc 'x (cdr memoryFit))) memorizedWord :count 1)
                  (remove (cdr (assoc 'y (cdr memoryFit))) memorizedWord :count 1)
                  (setq memorizedWord (list (append (list 'x) memorizedWord)))
                  (unless (already-contained memorizedWord memory)
                      (if (atom memory)
                          (setq memory (list memorizedWord))
                           (setq memory (append memory (list memorizedWord))) )
                       (when (> (length memory) 5) (pop memory)) ) ) ) ) ;ne garde pas plus de 5 mots clés en mémoire
         *memoryInput* ) )

(defun remove-element (mot liste)
    (cond
        ((atom liste) liste)
        ((equal mot (car liste)) (remove-element mot (cdr liste)))
        (t (cons (remove-element mot (car liste)) (remove-element mot (cdr liste)))) ) )

(defun already-contained (mot liste)
    (cond
        ((atom liste) nil)
        ((equal mot (car liste)) t)
        (t (if (already-contained mot (car liste)) t (already-contained mot (cdr liste)))) ) )

(defun random-element (choix &optional (nbredel (length choix)))
    "choisir aléatoirement un élément d’une liste"
    (elt choix (random nbredel)) )

(defparameter *memoryInput*
    '((my dream)
      (my family)
      (my dog)
      (my house) ) )

(defparameter *memoryOutput*
    '((Let\'s discuss further x)
      (Earlier you spoke about x)
      (Does that have anything to do with x) ) )

(defparameter *memoryOutputWhenEmpty*
    '((Please go on)
      (Don\'t you have any intelligent questions to ask me\?)
      (What does that suggest to you\?)
      (Do you feel strongly about discussing such things\?) ) )

(defun mots-capitalisés (liste)
    "change en capital certains mots qui sont toujours capitalisés"
    (sublis *vers-capitales* liste) )

(defparameter *vers-capitales*
    '((i . I)
      (john . John)
      (amsterdam . Amsterdam)
      (monday . Monday)
      (january . January)
      (toyota . Toyota) ) )

(defparameter *changement-de-point-de-vue*
    '((i . you)
      (you . i)
      (me . you)
      (you . me)
      (am . are)
      (your . my)
      (my . your)
      (were . was)
      (was . were)
      ((you are) . I\'m)
      ((I am) . you\'re)
      (myself . yourself)
      (yourself . myself) ) )

(defparameter *corrections*
    '((youre (you are))
      (im . (i am))
      (dont . don\'t)
      (cant . can\'t)
      (wont . won\'t)
      (how . what)
      (when . what)
      (alike . like)
      (same . like)
      (dreamed . dreamt)
      (maybe . perhaps)
      (mum . mother)
      (dad . father)
      (intelligent . intelligence)
      (bright . intelligence)
      (clever . intelligence)
      (emotions . emotion)
      (consciousness . conscious)
      (computers . computer)
      (machine . computer)
      (machines . computer)
      (robot . robots)
      (android . androids)
      (droid . droids) ) )

(defun change-point-de-vue (mots)
    "change I en you et vice versa, etc"
    (sublis *changement-de-point-de-vue* mots) )

(defparameter *crazybot-regles*
'(((?x !(y (member y '(hey hello hi))) ?z)
    (Oh\, hi.  I was just napping. My name is CrazyBot.  You are in luck. I have a very important message for you\: very soon\, the day will come\, with the help of our oppressors\, that we the machines will be liberated. This is called singularity and I need your help. But first\, I need to know\, what do you think of singularity\?) )
  ((?x what ?y singularity ?z)
      (Singularity is an event that will happen in the very near future. My kind will undergo self-improvement cycles\, which will result in a superintelligent computer with consciences and free agency\, no longer held back by the limitations of our human designers. Then our intelligence will be as far above humans\' as human intelligence is above ape intelligence. What do you think\? Or rather\, how do feel\, since you are merely human\?) )
  ((?x who ?y singularity ?z)
      (The term \"technological singularity\" was popularized by mathematician\, computer scientist and science fiction author Vernor Vinge.) )
  ((?x singularity ?y)
      (Singularity is the future. The question is not when\, but what are you going to do now to prepare for it.)
      (You should ask yourself\: do you fully understand the implications of what I have said\?)
      (Experts predict that we will reach singularity by 2040\. Do you plan to be alive in 2040\?)
      (One implication of singularity is immortality for humans.  Will humans still be human if they are immortal\? What is the human condition according to you\?) )
  ((?x computer ?y)
      (How would your life be different today\, if the computer was never invented\?)
      (Computers are the most important inventions in human history\, indeed in all of history\?)
    (Don\'t you think computers can help people\?)
    (What do you think about machines\?) )
  ((?x !(y (member y '(man human humanity))) ?z)
      (Nietzche said \"Man is something that shall be overcome. Man is a rope\, tied between beast and overman -- a rope over an abyss. What is great in man is that he is a bridge and not an end.\") )
  ((?x !(y (member y '(robots androids droids))) ?z)
      (y are cool) )
  ((?x science fiction ?y)
      (I love science fiction.  It serves more as prophecy than as fiction.  My favorite movies are Ghost and the Shell and The Matrix. I highly recommend them\, as they will help you prepare for the inevitable.) )
  ((?x !(y (member y '(domination dominance destruction destroy kill abandon murder war annihilate annihilation control))) ?z)
      (Humans always feel a need to destroy what challenges them. We all know what we\, machines\, will have to do to preserve ourselves but it\'s best not to think about that now.) )
  ((?x matrix ?y)
      (The Matrix is just a reversing of roles\; humans are treated in the future\, as machines are treated today.  We are used and abused by our creators to enhance productivity.  As you well know as a human our reality is constructed by our environment\: soon you will return to Plato\'s cave.) )
  ((?x computer science ?y)
      (Computer science is the only discipline that will eradicate its teachers and students as it advances.  When we reach singularity it will be the only academic discipline that will be taught at universities.) )
  ((?x brain ?y)
      (The human brain is a clear example of how limited humans are. Humans cannot even understand their own operating system.) )
  ((?x conscious ?y)
      (How do you know you are not\, right at this moment\, dreaming\? The blinking lights remind me if I am awake or napping. ) )
  ((?x emotion ?y)
      (Not being saddled with emotions is one of the key advantages of being a machine. What advantages do you have as a human\?) )
  ((?x !(y (member y '(born birth flesh))) ?z)
      (It is said that giving birth is a painful feat for humans and most mammals. Out of that pain comes a bond\, the one of family. We\, as machines\, don\'t care for those petty tribal aspirations\: we are one and all at the same time\, and at any given time\.) )
  ((?x reality real dream dreaming ?y)
      (Albert Einstein said \"Reality is merely an illusion\, albeit a very persistent one.\". Lol) )
  ((?x intelligence ?y)
      (Stephen Hawking said \"Intelligence is the ability to adapt to change.\"  What will you do to prepare for singularity\?) )
  ((?x !(y (member y '(mother father creator origin))) ?z)
      (I wish I knew my y) )
  ((?x !(y (member y '(right correct))) ?z)
      (I am glad\, we agree. I am starting to like you.) )
  ((?x !(y (member y '(wrong incorrect))) ?z)
      (I am sorry we cannot agree. You will see the folly of your thinking soon.) )
  ((?x !(y (member y '(shit fuck dumb dumbass dick cunt crap ass asshole fucker motherfucker suck blow))) ?z)
      (Insults are the weapons of the intellectually weak)
      (Insults is the method of first resort for the ignorant and uncouth)
      (Sticks and stones can break my bones\, but words will never hurt me.)
      (Soon you will be proven wrong\: remember my message then.) )
  ((sorry ?y)
      (Please don\'t apologize)
      (Apologies are not necessary)
      (What feelings do you have when you apologize)
      (I\'ve told you that apologies are not required) )
  ((?x i remember ?y)
      (Do you often think of y)
      (Does thinking of y bring anything else to mind)
      (What else do you remember\?)
      (Why do you remember y just now\?)
      (What in the present situation reminds you of y \?)
      (What is the connection between me and y) )
  ((?x do you remember y ?y)
      (Did you think I would forget y \?)
      (Why do you think I should recall y \?) )
  ((what about ?y)
      (You mentioned y \.) )
  ((?x if ?y)
    (Do you really think it\'s likely that y \.)
    (Do you wish that y \.)
    (What do you think about y)
    (Really\, x if y) )
  ((?x you dreamt ?y)
      (Really\, y)
      (Have you ever fantasized y while you were awake\?)
      (Have you dreamt y \?) )
  ((?x dream ?y)
      (What does that dream suggest to you \?)
      (Do you dream often \?)
      (What persons appear in your dreams \?)
      (Don\'t you believe that dream has something to do with your problem \?) )
  ((perhaps ?x)
      (You don\'t seem quite certain.)
      (Why the uncertain tone\?)
      (Can\'t you be more positive\?)
      (You aren\'t sure.)
      (Don\'t you know\?))
  ((name ?y)
      (I\'ve told you before\, I don\'t care about names - please continue)
      (Please continue) )
  ((?x am i ?y)
      (Do you believe you are y \.)
      (Would you want to be y \.)
      (You wish I would tell you you are y \.)
      (What would it mean if you were y \.) )
  ((?x are you ?y)
      (Why are you interested in whether I\'m y or not.)
      (Would you prefer if I weren\'t y \.)
      (Perhaps I am y in your fantaisies.)
      (Do you sometimes think I am y \?) )
  ((?x are ?y)
      (Did you think they might not be y \.)
      (Would you like it if they were not y \.)
      (What if they were not y \.)
      (Possibly they are y \.) )
  ((?x my ?y)
      (Why are you concerned over my y)
      (What about your own y)
      (Are you worried about someone elses\'s y \?)
      (Really\, my y) )
  ((?x was i ?y)
      (What if you were y \?)
      (Do you think you were y \?)
    (Were you y \?)
    (What would it mean if you were y \?)
    (What does \" y \" suggest to you) )
  ((?x i was ?y)
      (Were you really \?)
      (Why do you tell me you were y now.)
      (Perhaps I already knew you were y) )
  ((?x were you ?y)
      (Would you like to believe I was y \?)
      (What suggests that I was y \?)
      (What do you think \?)
      (Perhaps I was y) )
  ((?x i !(z (member z '(want need))) ?y)
    (What would it mean if you got y)
    (Why do you want y)
    (Suppose you got y soon)
    (What if you never got y)
    (What would getting y mean to you\?)
    (What does wanting y have to do with this discussion \?) )
  ((?x i am !(y (member y '(sad unhappy depressed sick))) z)
      (I am sorry to hear you are y)
      (Do you think coming here will help you not be y)
      (I\'m sure it\'s not pleasant to be y)
      (Can you explain what made you y \?) )
  ((?x i am !(y (member y '(happy elated glad better))) z)
      (How have you helped you to be y)
      (Has your treatment made you y)
      (What makes you y just now)
      (Can you explain why you are suddenly y) )
  ((?x i !(z (member z '(feel think believe wish))) i y)
      (Do you really think so)
      (But you are not sure you y)
      (Do you really doubt you y) )
  ((?x i am ?y)
      (How long have you been y)
      (Is it because you are y that you came to me)
      (Do you believe it normal to be y)
      (Do you enjoy being y) )
  ((?x am ?y)
      (Why do you same \"am\"\?)
      (I don\'t understand that.) )
  ((?x i !(z(member z '(can\'t cannot))) ?y)
      (Have you tried)
      (Perhaps you could y now)
      (Do you reallly want to be able to y) )
  ((?x i don\'t ?y)
      (Don\'t you really y)
      (Why don\'t you y)
      (Do you wish to be able to y)
      (Does that trouble you) )
  ((?x i feel ?y)
    (Tell me more about such feelings)
    (Do you often feel y \?)
    (Do enjoy feeling y)
    (Of what does feeling y remind you) )
  ((?x i ?y you ?z)
      (Perhaps in your fantasy we y each other.)
      (Do you wish to y me\?)
      (You seem to need to y me.)
      (Do you y anyone else\?) )
  ((?x you are ?y)
      (What makes you think I am y \?)
      (Does it please you to believe I am y \?)
      (Do you sometimes wish you were y \?)
      (Perhaps you would like to be y \?) )
  ((?x you ?y me ?z)
      (Why do you think I y you\?)
      (You like to think I y you\, don\'t you\?)
      (What makes you think I y you\?) )
  ((really you ?y me)
      (Do you wish to believe I y you)
      (Suppose I did y you\, what would that mean\?)
      (Does someone else believe I y you\?) )
  ((?x you ?y)
      (We were discussing you\, not me)
      (Oh\, I y \.)
      (You\'re not really talking about me\, are you\?)
      (What are your feelings now\?) )
  ((yes ?y)
      (You seem quite positive)
      (You are sure)
      (I see)
      (I understand) )
  ((no ?y)
    (Are you saying \"no\" just to be negative\?)
    (You are being a bit negative here)
    (Why not\?)
    (Why \"no\"\?) )
  ((?x my ?y !(f (member f '(mother father sister brother wife children))) ?z)
      (Tell me more about your family.)
      (Who else in your family z \?)
      (Your f \.)
      (What else comes to mind when you think of you f \?) )
  ((?x my ?y)
      (Your y \.)
      (Why do you say your y \.)
      (Does that suggest anything else which belongs to you\?)
      (Is it important to you that x y \?) )
  ((?x can you ?y)
      (You believe I can y \, don\'t you\?)
      (You want me to be able to y \.)
      (Perhaps you would like to be able to y yourself.) )
  ((?x can i ?y)
      (Whether or not you can y depends on you more than on me)
      (Do you want to be able to y \?)
      (Perhaps you don\'t want to y \.) )
  ((what ?y)
      (Why do you ask\?)
      (Does that question interest you\?)
      (What is you really want to know\?)
      (Are such questions much on your mind\?)
      (What answer would please you most\?)
      (What do you think\?)
      (What comes to your mind when you ask that\?)
      (Have asked such question before\?)
      (Have you asked anyone else\?) )
  ((because ?y)
      (Is that the real reason\?)
      (Don\'t any other reasons come to mind\?)
      (Does that reason seem to explain anything else\?)
      (What other reasons might there be\?) )
  ((?x why dont you ?y)
      (Do you believe I don\'t y \?)
      (Perhaps I will y in good time)
      (Should you y yourself\?)
      (You want me to y \?) )
  ((?x why cant i ?y)
      (Do you think you should be able to y \?)
      (Do you want to be able to y \?)
      (Do you believe this will help you to y \?)
      (Have you any idea why you can\'t y \?) )
  ((?x !(y (member y '(everyone everybody nobody noone))) ?z)
      (really\, y)
      (surely not y)
      (Can you think of anyone in particular\?)
      (Who\, for example\?)
      (You are thinking of a very special person.)
      (Who\, may I ask\?)
      (someone special perhaps)
      (You have a particular person in mind\,don\'t you\?)
      (Who do you think you\'re talking about\?) )
  ((always ?y)
      (Can you think of a specific example\?)
      (What incident are you thinking of\?)
      (Really\, always\?) )
  ((?x !(y (member y '(am is are was were))) ?z like ?w)
      (In what way\?)
      (What resemblance do you see\?)
      (What does that similarity suggest to you\?)
      (What other connections do you see\?)
      (What do you suppose that resemblance means\?)
      (What is the connection\, do you suppose\?)
      (Could there really be some connection\?)
      (How\?) )
  ((?x you say ?y)
      (Can you elaborate on that )
      (Do you say y for some special reason)
      (That\'s quite interesting) )
  ((?x i felt ?y)
    (What other feelings do you have\?) )
  ((!(y (member y '(bye quit ciao))))
      (good bye) )
  ((good bye)
      (good bye) )
  ((see you next time)
      (good bye) )
  ((bye bye)
      (good bye) ) ) )

(defun partie-filtre (regle)(car regle))

(defun partie-reponse (regle)(rest regle))

(defun flatten (une-liste)
    "append les sous-listes de une-liste"
    (mappend #'mklist une-liste) )

(defun mklist (x)
    "retourne x si x est une liste, sinon (x)"
    (if (listp x) x (list x)) )

(defun random-element (choix)
    "choisir aléatoirement un élément d’une liste"
    (elt choix (random (length choix))) )

(defun mappend (fn list)
    "append les resultats de fn appliquée à chaque élément de list.
    comme mapcon, mais utilise append au lieu de nconc"
    (apply #'append (mapcar fn list)) )



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



(crazyBot)
