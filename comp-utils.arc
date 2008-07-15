;;;;  Copyright (c) 2008 Dissegna Stefano
;;;;  Released under the terms of the GNU LGPL

(def read-from-file (file-name)
  nil)

(def listify (x)
  (if (consp x) x (list x)))

(def flatten-1 (l)
  (apply append (map listify l)))

(def mappend (f l)
  (if l 
    (let res (f (car l))
      (if (atom res)
        (cons res (mappend f (cdr l)))
	(append res (mappend f (cdr l))))) 
    nil))

(def atom (x) (not (consp x)))

(def union (l1 l2)
  (if (not l1) l2
      (not (mem (car l1) l2)) (cons (car l1) (union (cdr l1) l2))
      (union (cdr l1) l2)))

(def map-union (f l)
  (if l
      (with (res (f (car l))
	     tail (map-union f (cdr l)))
	(if (atom res)
	    (if (not (mem res tail))
		(cons res tail)
		tail)
	    (union res tail)))
      nil))

(def compose funcs
  (reduce (fn (x y) (fn (a) (x (y a)))) (cdr funcs) (car funcs)))

(def length=1 (l)
  (is (cdr l) nil))

(def keep (f l)
  (if l
    (if (f (car l))
      (cons (car l) (keep f (cdr l)))
      (keep f (cdr l)))))

(def some (f l)
  (if l
    (or (f (car l)) (some f (cdr l)))
    nil))

(def remove-duplicates (l)
  (if l
    (if (mem (car l) (cdr l)) 
      (remove-duplicates (cdr l))
      (cons (car l) (remove-duplicates (cdr l))))))
