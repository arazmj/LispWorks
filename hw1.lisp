;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;	Author: Amir Razmjou
;;	Student Id: 901928271
;;	Date: 9/4/2015
 ;;	Artificual Intelligence CSE 5290 Dr. Philip Chan
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positive-count (list)
  (if (or ; check if the list is not empty and 
        (null list) ; every element is a number
        (notevery #'numberp list))				
    'nill ; return nill if list list of numbers					
    (reduce ; sum the elements of the list
      #'+ (mapcar 
            (lambda (x) 
              (if (> x 0) 1 0)) list)))) ; map the list to 0 and 1s where 1 represent postives

(defun between (x pair) ; 'between' predicate helper
  (if (second pair) ; returns T if x is between first and second element
    (if (and ; of the pair list
          (> x (first pair)) 
          (< x (second pair))) T)			
    (if (> x (first pair)) T)))

(defconstant *categories*
  '((Tropical-Storm (39 73))  (Hurricane-Cat-1 (74 95))
	(Hurricane-Cat-2 (96 110)) (Hurricane-Cat-3 (111 130))
	(Hurricane-Cat-4 (131 155)) (Hurricane-Cat-5 (156))))

(defconstant *storms2004* 
  '((bonnie 65) (charley 150)
   (frances 145) (ivan 165) (jeanne 120)))

(defun storm-categories (storms)  
  (mapcar 
    (lambda(storm) 
      (list 
        (first storm) ; city name
        (first ; category name
          (find-if (lambda(category) 		; find first matching category
                     (between 
                       (second storm) 		; speed element in the storm list
                       (second category)))	; speed range in the categories list
            *categories*))))
    storms))

(defun storm-distribution (storms)
  (mapcar 
    (lambda (category) 
      (list 
        (first category)
        (count-if (lambda (storm) 
                    (between 
				     (second storm) 
				     (second category)))	
          storms)))
    *categories*))

(defun nested-member (key list)
  (some #'identity ; find some non-nil element
    (mapcar (lambda (e)						
              (if (listp e) ; if element is a list recall
                (nested-member key e)		 
                (eq key e)))
      list)))

; The first element of the the list is the parent
; and the second element is the left children tree and third element would be 
; right children. The children would be both a tree or an atom
; tree -> (tree (tree) (tree))
; tree -> atom

(defconstant *family-tree*
  '(John 
    (Mark 
      (James (GeorgeH) (Barbara)) 
      (Jane  (Bill) (Hillary))) 
    (Mary 
      (Peter (GeorgeW) (Laura)) 
      (Pat (Barack) (Michelle)))))

(defun find-node (tree p) ; finds the matching node 
  (if tree
    (if (eq (car tree) p) tree 
     (or ; if current root is not the node
        (find-node (second tree) p) ; recall the function on left tree and right tree
        (find-node (third tree) p)))))	

(defun parents (tree p) 
  (setq n (find-node tree p)) ; find the node
  (if (and (second n) (third n)) ; if left tree and right tree exists
    (list ; return them as a list
      (car (second n)) 
      (car (third n)))))	                  

(defun grandparents (tree p)   
  (setq  prnt (parents tree p))	
  (if prnt 
    (append 
     (parents tree (first prnt)) 
     (parents tree (second prnt)))))

(defun euclidean (a b)
  (sqrt 
    (reduce #'+ 
      (mapcar (lambda(x) (expt x 2)) 
        (mapcar #'- a b)))))

(defconstant *family-tree2* 
   '((a b) 
      (((c u) 
         (((m x)                       
            (((r))))    
           ((n y))   
           ((o))))
        ((d v))
        ((e w)
          (((p))
            ((q))))
        ((f)))))  

(defun first-person (tree) ; helper function return the first person in the node
      (if (listp tree)
        (first-person (car tree)) tree))

(defun find-node2 (tree p) ; returns a pair with first element as a list to the node
  (labels ( ; and second element as target node
    (find-node-rec (tree p h) ; h is used to keep track of the path to target node
	  (if tree 
	    (if (member p (first tree)) ; if p is member of the current node the current node is returned
         (cons h (list tree))				
	      (some #'identity
	        (mapcar 
	          (lambda(q) 
	            (find-node-rec q p 
	              (cons (first-person tree) h))) 
           (second tree)))))))
    (find-node-rec tree p ()))) ; start the helper function with empty path

(defun spouse (tree p)
  (car 
    (remove p 
      (first 
        (second 
          (find-node2 tree p))))))

(defun parents2 (tree p)
  (setq e 
    (first (first (find-node2 tree p))))	; parent is the first element of first element of find-node2
  (if e (cons e (list (spouse tree e)))))	; cons parent with the spouse of parent as a list

(defun grandparents2 (tree p)
  (setq e 
    (second (first (find-node2 tree p))))	; parent is the second element of first element of find-node2
  (if e (cons e (list (spouse tree e)))))

(defun children (tree p)
  (mapcar 
    (lambda (q) (first-person q)) 
    (second (second (find-node2 tree p)))))

(defun grandchildren (tree p)
  (setq c (children tree p))
  (apply #'append
	  (mapcar 
	    (lambda (q) (children tree q)) c)))

(defun siblings (tree p)
  (if (eq (first (first (second (find-node2 tree p)))) p)
  (remove p ; remove the child from the list of siblings
    (children tree								
	  (first (first (find-node2 tree p)))))))
  
