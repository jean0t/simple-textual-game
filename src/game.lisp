;;; Game constitutes of Scenarios and paths between them
;;; each scenario has items that you can keep in you inventory
;;; and interact with them (Not Developed yet)


;; Scenario related
(defparameter *scenario* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
			  (garden (you are in a beautiful garden. there is a well in front of you.))
			  (attic (you are in the attic. there is a giant welding torch in the corner.))))


;; Paths related
(defun describe-location (location scenario)
  "exhibits the description of the scenario"
  (cadr (assoc location scenario)))

(defparameter *paths* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (path)
  "exhibits the paths in a certain scenario"
  `(there is a ,(caddr path) going ,(cadr path) from here.))

(defun describe-paths (location path)
  (apply #'append (mapcar #'describe-path (cdr (assoc location path)))))

;; Object related
(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room) (bucket living-room)
				   (chain garden) (frog garden)))

(defun objects-at (location object object-location)
  (labels ((at-location-p (obj)
	     (eq (cadr (assoc obj object-location)) location)))
    (remove-if-not #'at-location-p object)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))



;; Game Abstraction
(defparameter *location* 'living-room) ; player starts in the Living Room

(defun look ()
  (append (describe-location *location* *scenario*)
	  (describe-paths *location* *paths*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *paths*))
		    :key #'cadr)))
    (if next (progn (setf *location* (car next))
		    (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
