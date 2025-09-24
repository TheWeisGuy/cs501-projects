#lang racket
(provide (struct-out bt-node) (struct-out bt-leaf) (struct-out bt-empty))
(provide (struct-out plus-node) (struct-out times-node) (struct-out int-leaf))
(provide palindrome-list eval-tree check-bt ml-split piles-insert)

; For each problem, you can find additional test cases and examples by looking in ps1-test.rkt.
; The general format of each test is of the form
;
;  (test-equal? "name" (f arg) expected)
;
; where f is a function you need to implement, arg is some test case arguments, and expected is what
; should be returned in that case. 
;
; If you don't understand what a problem is asking, look at the test cases for clarification.
; You can test your code by putting your ps1.rkt in the same directory as ps1-test.rkt and then
; running ps1-test.rkt in DrRacket or running "racket ps1-sol-test.rkt"
; from the command line.

; Submit your code by uploading the completed ps1.rkt to GradeScope.

;;;;;;;;; Problem 1 ;;;;;;;;;

; Recall that a palindrome or (palindromic word) is a word that is the
; same when read forwards and backwards.  For example, "kayak", "dad",
; and "radar", are all palindromes.

; Write a function palindrome-list which takes a string s as an argument
; and returns a list of all of the palindromic words that occur in s
; when s is converted to lower-case letters and all punctuation is
; removed.  The words should occur in the list in the order that they
; occur in the original string.  If a palindrome occurs multiple times,
; each occurrence should be in the list.

; Example: when s is "The gig was a gag, a joke.", (palindrome-list s) should return
; '("gig" "a" "gag" "a").


;helper function for palindrome-list
;@p s : list of strings to parse
;@p wordList : list of palindromes to return
(define (palindrome-helper s wordList)

  ;base case, we reached the end of the list so return s
  (if (empty? s)

      ;check if the current word is equal to its reverse
      (reverse  wordList )  
      (let ([reverseString (list->string (reverse (string->list (car s))))])
        (if (equal? (car s) reverseString)
            (palindrome-helper (rest s) (cons (car s) wordList))  
            (palindrome-helper (rest s) wordList)))))            
                                
(define (palindrome-list s)

  ;call palindrome helper
  ;remove all punctuation from the string and split it by spaces
  ;Chat gpt generated the regular expression to remove punctuation
  (remove "" (palindrome-helper
              (map string-downcase
                   (regexp-split #px"[\\s]+"
                                 (regexp-replace* #px"[.,!?;:'\"()\\[\\]{}<>-]+" s "")))  '())))


;;;;;;;;; Problem 2 ;;;;;;;;;

; In this problem we consider trees that are constructed using
; plus-node, times-node, or int-leaf.  The arg1 and arg2 fields of
; plus-node and times-node should be themselves trees constructed from
; these constructors. The val field of int-leaf should be an integer.

; We shall think of these trees as representing arithmetic expressions,
; Where, for example, (int-leaf i) represents the number i, plus-node e1
; e2, represents e1 + e2, after interpreting the trees e1 and e2 as
; arithmetic expressions, and similarly for times-node e1 e2.
; 
; Write a function eval-tree, which takes a tree t as input and returns
; the integer that results from evaluating the arithmetic expression
; corresponding to the tree.  For example (eval-tree (plus-node
; (int-leaf 1) (int-leaf 2))) should return 3.


(struct plus-node (arg1 arg2))
(struct times-node (arg1 arg2))
(struct int-leaf (val))


(define (eval-tree t) '()
  (match t

    ;if either a plus or a times, recurse on the individual child
    ;nodes and return the operation result
    
    [(plus-node b1 b2)
     (+(eval-tree b1)(eval-tree b2))]
    [(times-node b1 b2)
     (*(eval-tree b1)(eval-tree b2))]

    ;base case is leaf nodes, just return the number
    [(int-leaf num) num])
  )

;;;;;;;;; Problem 3 ;;;;;;;;;;;;

; In this problem we consider binary trees that are constructed using
; bt-node, bt-leaf, and bt-empty. int-leaf.

; The val fields of bt-node and bt-leaf are integers. The left and right
; fields of bt-node should be themselves trees constructed from these
; constructors.

; Recall that a binary search tree is a binary tree in which we have an
; invariant requiring that for a node of the form (bt-node i lt rt),
; every node value in the left child tree lt should be smaller than i,
; and every node in the right tree should be larger than i.

; Write a function check-bt which takes as an argument a binary tree t
; constructed using the above structs, and returns #t if t satisfies
; the binary tree invariant, and #f otherwise.

; Example: (check-bt (bt-node 5 (bt-leaf 1) (bt-leaf 6))) should return #t,
; but (check-bt (bt-node 5 (bt-leaf 6) (bt-leaf 6))) should return #f.

(struct bt-node (val left right))
(struct bt-leaf (val))
(struct bt-empty ())


;traverse the tree in order and return a list with each element
(define (check-bt-traverse t)
 (match t

   ;if bt-node append the result of the left recursive call, the current node
   ;and the right recursive call
    [(bt-node val left right)
     (append (check-bt-traverse left)
             (list val)
             (check-bt-traverse right))]
   
   ;base case add the leaf value to the list
   [(bt-leaf val)
     (list val)]
    [_ '()])

  )

;check if the given list is in ascending order
(define (is-bst? numList)
  (cond

    ;base case if numList is empty or we have reached the end return true
    ((or (empty? numList) (empty? (rest numList))) #t)

    ;if ever a value is out of order, return false
    ((>= (first numList) (second numList)) #f)         

    ;value is in order and not at the end of the list, keep goings
    (else
     (is-bst? (rest numList)))))

(define (check-bt t) 
  (is-bst? (check-bt-traverse t)))

;;;;;;;;; Problem 4 ;;;;;;;;;;;;

; Given a list of integers, '(i1 i2 ... ik), we say that the list is
; strictly monotone if either i1 < i2 < ... < ik or i1 > i2 > ... > ik.
; i.e. for either every element in the list is strictly smaller than the
; next element, or every element is strictly greater than the next
; element.
; 
; Write a function ml-split which takes a list l of integers and returns
; a list of lists obtained by breaking up l into strictly monotone
; lists. The returned lists should be maximal, meaning that there is no
; other splitting of l into strictly monotone lists in which any of the
; lists in the list could be larger while still being montone. In the
; case of ties, your solution should prefer to make the earlier lists
; larger.
; 
; Example: (ml-split '(1 2 3 4 3 2 1)) should return '((1 2 3 4) (3 2
; 1)).  Returning '((1 2) (3 4) (3 2 1)) would be wrong, because while
; each of the lists is strictly monotone, we could combine the first two
; to '(1 2 3 4), which is larger. Similarly, returning '((1 2 3) (4 3 2
; 1)) would be wrong because we should prefer to make the first list
; larger at the expense of making the second list shorter.


(define (ml-list-helper l current direction acc)
    (cond
      ;base case we get to the end of the list
      [(empty? l)
       (reverse (cons (reverse current) acc))]

      [else
       (let* ([currentVal (car l)]
              [prev (car current)])
         (cond
           ;if our values are ascending
           [(and (equal? direction 'inc) (< prev currentVal))
            (ml-list-helper (cdr l) (cons currentVal current) 'inc acc)]

           ;if our values are decreasing
           [(and (equal? direction 'dec) (> prev currentVal))
            (ml-list-helper (cdr l) (cons currentVal current) 'dec acc)]

           ;no current direction so we must determine it
           [(eq? direction 'none)
            (cond [
                  ;if increasing
                   (< prev currentVal) (ml-list-helper (cdr l) (cons currentVal current) 'inc acc)]
                  [
                   ;if decreasing
                   (> prev currentVal) (ml-list-helper (cdr l) (cons currentVal current) 'dec acc)]
                  [else
                    ;if two equal values
                   (ml-list-helper (cdr l) (list currentVal) 'none
                           (cons (reverse current) acc))])]

           ;direction change, start a new list
           [else
            (ml-list-helper (cdr l) (list currentVal) 'none
                    (cons (reverse current) acc))]))]))

(define (ml-split lst)

  

  (if (empty? lst)
      '()
      (ml-list-helper (cdr lst) (list (car lst)) 'none '())))



;;;;;;;;; Problem 5 ;;;;;;;;;;;;

; Write a function piles-insert which takes two arguments.  When running
; (piles-insert ls n), the first argument ls is a list of lists of
; integers. We call each list in ls a pile. You may assume that each
; pile is non-empty, and that each pile is sorted in ascending
; order. Finally, you may also assume that if ls is of the form '(p_1
; p_2 ... p_n), then the head of p_i is strictly less than the head of p_(i+1).
; 
; Evaluating (piles-insert ls n) should return a list of piles obtained from
; taking ls and inserting n so that either n has (1) been added to the
; head of the first pile in ls whose previous head is greater than or
; equal to n, or (2) if no such pile exists, then a new pile containing
; just n is added to the end of ls.
 
; Example: (piles-insert '((4) (5)) 3) should return '((3 4) (5))) and
; (piles-insert '((2) (6)) 4) should return '((2) (4 6)))

(define (piles-insert ls n)

  ;if we have no piles, make a pile with n
  (if (empty? ls)
      (list (list n))

      ;declare a variable to hold the current pile and the rest of the piles list
      (let ([pile (car ls)]
            [restOfPiles (cdr ls)])

        ;if n is less than or equal to the head of this pile
        ;make n the new head
        (if (>= (car pile) n)
            (cons (cons n pile) restOfPiles)

            ;else recurse on the rest of the piles
            (cons pile (piles-insert restOfPiles n))
        ))))
